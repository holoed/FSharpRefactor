// * **********************************************************************************************
// * Copyright (c) Edmondo Pentangelo. 
// *
// * This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// * copy of the license can be found in the License.html file at the root of this distribution. 
// * By using this source code in any fashion, you are agreeing to be bound 
// * by the terms of the Apache License, Version 2.0.
// *
// * You must not remove this notice, or any other, from this software.
// * **********************************************************************************************

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using FSharpRefactorAddin.VsPackage;
using FSharpRefactorVSAddIn.Common;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;

namespace FSharpRefactorAddin.Rename
{
    //TODO: This is a spike. Needs to be cleaned up and test it.
    //TODO: Remove duplication with HighlightTagger
    public class RenameCommandFilter : IOleCommandTarget
    {
        private readonly IWpfTextView _textView;
        private readonly ITextStructureNavigator _textStructureNavigator;
        private ASTAnalysis.SymbolTable _symbolTable;

        public RenameCommandFilter(IWpfTextView textView, ITextStructureNavigator textStructureNavigator)
        {
            _textView = textView;
            _textStructureNavigator = textStructureNavigator;
            _textView.LayoutChanged += ViewLayoutChanged;
            _textView.Caret.PositionChanged += CaretPositionChanged;
            _textView.TextBuffer.Changed += HandleTextChanged;
            RefreshSymbolTable(_textView.TextSnapshot.GetText());
        }
        
        public bool Added { get; set; }
        public IOleCommandTarget NextTarget { get; set; }

        int IOleCommandTarget.QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {            
            if (pguidCmdGroup == Guid.Parse("{19492BCB-32B3-4EC3-8826-D67CD5526653}") && 
                prgCmds.Any(x => x.cmdID == (uint)PkgCmdIDList.CmdidMyMenu))
            {
                prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_SUPPORTED | (uint)OLECMDF.OLECMDF_ENABLED;
                return VSConstants.S_OK;
            }

            if (pguidCmdGroup == Guid.Parse("{19492BCB-32B3-4EC3-8826-D67CD5526653}") && 
                prgCmds.Any(x => x.cmdID == (uint)PkgCmdIDList.CmdidMyCommand) &&
                CurrentWord.Success &&
                IsRenameableIdentifier) 
            {
                prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_SUPPORTED | (uint)OLECMDF.OLECMDF_ENABLED;
                return VSConstants.S_OK;
            }

            return NextTarget.QueryStatus(ref pguidCmdGroup, cCmds, prgCmds, pCmdText);
        }        

        int IOleCommandTarget.Exec(ref Guid pguidCmdGroup, uint nCmdId, uint nCmdexecopt, IntPtr pvaIn, IntPtr pvaOut)
        {
            if (pguidCmdGroup == Guid.Parse("{19492BCB-32B3-4EC3-8826-D67CD5526653}") && nCmdId == (uint)PkgCmdIDList.CmdidMyCommand)
                HandleRename();
            return NextTarget.Exec(ref pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }

        private void HandleRename()
        {
            if (CurrentWord.Success)
            {                
                var wnd = new RenameDialog {DataContext = this};
                var ret = wnd.ShowDialog();
                if (ret.HasValue && ret.Value)
                    RenameAllOccurences(CurrentWord, TextToRename);
            }
        }

        private void ViewLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            // If a new snapshot wasn't generated, then skip this layout
            if (e.NewViewState.EditSnapshot != e.OldViewState.EditSnapshot)
                UpdateAtCaretPosition(_textView.Caret.Position);
        }

        private void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        private void UpdateAtCaretPosition(CaretPosition caretPoisition)
        {
            var point = caretPoisition.Point.GetPoint(_textView.TextBuffer, caretPoisition.Affinity);

            if (!point.HasValue)
                return;

            CurrentWord = FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(point.Value);
            if (CurrentWord.Success)
                TextToRename = CurrentWord.Value.GetText();
        }

        private Maybe<SnapshotSpan> CurrentWord { get; set; }
        
        public string TextToRename { get; set; }

        private Maybe<SnapshotSpan> FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(SnapshotPoint currentRequest)
        {
            var word = _textStructureNavigator.GetExtentOfWord(currentRequest);

            var foundWord = true;

            // If we've selected something not worth highlighting, we might have
            // missed a "word" by a little bit
            if (!WordExtentIsValid(currentRequest, word))
            {
                // Before we retry, make sure it is worthwhile
                if (word.Span.Start != currentRequest ||
                    currentRequest == currentRequest.GetContainingLine().Start ||
                    char.IsWhiteSpace((currentRequest - 1).GetChar()))
                {
                    foundWord = false;
                }
                else
                {
                    // Try again, one character previous.  If the caret is at the end of a word, then
                    // this will pick up the word we are at the end of.
                    word = _textStructureNavigator.GetExtentOfWord(currentRequest - 1);

                    // If we still aren't valid the second time around, we're done
                    if (!WordExtentIsValid(currentRequest, word))
                        foundWord = false;
                }
            }

            return !foundWord ? new Maybe<SnapshotSpan> { Success = false } : 
                new Maybe<SnapshotSpan> { Value = word.Span, Success = true };
        }

        /// <summary>
        /// Determine if a given "word" should be highlighted
        /// </summary>
        private static bool WordExtentIsValid(SnapshotPoint currentRequest, TextExtent word)
        {
            return word.IsSignificant && currentRequest.Snapshot.GetText(word.Span).Any(c => char.IsLetter(c));
        }

        private void RenameAllOccurences(Maybe<SnapshotSpan> wordBefore, string newText)
        {
            var foundUsages = FindUsages(wordBefore);
            var afterSnapshot = _textView.TextSnapshot;
            foreach (var usage in foundUsages)
            {                
                var newUsage = usage.TranslateTo(afterSnapshot, SpanTrackingMode.EdgeExclusive);
                afterSnapshot = _textView.TextBuffer.Replace(newUsage, newText);                
            }
        }

        private IEnumerable<SnapshotSpan> FindUsages(Maybe<SnapshotSpan> word)
        {
            if (!word.Success)
                Enumerable.Empty<SnapshotSpan>();

            var position = GetPosition(word.Value);
            position = Tuple.Create(position.Item1, position.Item2, position.Item3, position.Item4);
            var usagesOfModifiedWord =
                FSharpRefactor.findAllReferencesInSymbolTable(_symbolTable, position);

            var spans = FindTheNewSpans(word.Value);
            return spans.Where(x => ReferencesContains(usagesOfModifiedWord, x)).ToList();
        }

        protected bool IsRenameableIdentifier
        {
            get { return FindUsages(CurrentWord).Any(); }
        }

        private static bool ReferencesContains(IEnumerable<Tuple<int, int, int, int>> references, SnapshotSpan currentWord)
        {
            return references.Any(x => Equals(x, GetPosition(currentWord)));
        }

        private static IEnumerable<SnapshotSpan> FindTheNewSpans(SnapshotSpan currentWord)
        {
            var txt = currentWord.Snapshot.GetText();
            var word = Regex.Match(currentWord.GetText(), "\\b\\w+\\b");
            var matches = Regex.Matches(txt, "\\b" + word + "\\b");
            var spans = matches.Cast<Match>().Select(m => new SnapshotSpan(currentWord.Snapshot, m.Index, m.Length));
            return spans.ToList();
        }

        private static Tuple<int, int, int, int> GetPosition(SnapshotSpan currentWord)
        {
            var lineStart = currentWord.Snapshot.GetLineNumberFromPosition(currentWord.Start.Position) + 1;
            var lineEnd = currentWord.Snapshot.GetLineNumberFromPosition(currentWord.End.Position) + 1;
            var startLine = currentWord.Snapshot.GetLineFromPosition(currentWord.Start.Position);
            var endLine = currentWord.Snapshot.GetLineFromPosition(currentWord.End.Position);
            var colStart = currentWord.Start.Position - startLine.Start.Position;
            var colEnd = currentWord.End.Position - endLine.Start.Position;
            return Tuple.Create(colStart, colEnd, lineStart, lineEnd);
        }

        private void HandleTextChanged(object sender, TextContentChangedEventArgs e)
        {
            RefreshSymbolTable(e.After.GetText());
        }

        private void RefreshSymbolTable(string allText)
        {
            _symbolTable = ASTAnalysis.buildSymbolTable(FSharpRefactor.parseWithPos(allText));
        }
    }
}
  