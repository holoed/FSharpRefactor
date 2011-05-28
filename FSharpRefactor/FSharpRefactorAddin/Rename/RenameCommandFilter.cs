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
using System.Reactive;
using System.Reactive.Linq;
using FSharpRefactorAddin.Common;
using FSharpRefactorAddin.VsPackage;
using FSharpRefactorVSAddIn.Common;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;

namespace FSharpRefactorAddin.Rename
{
    public class RenameCommandFilter : IOleCommandTarget, IDisposable
    {
        private readonly IWpfTextView _textView;
        private readonly ITextStructureNavigator _textStructureNavigator;
        private readonly ITextUndoHistory _textUndoHistory;
        private ASTAnalysis.SymbolTable _symbolTable;
        private readonly IDisposable _disposable;
        private const int ThrottlingTime = 500;

        public RenameCommandFilter(IWpfTextView textView, ITextStructureNavigator textStructureNavigator, ITextUndoHistory textUndoHistory)
        {
            _textView = textView;
            _textStructureNavigator = textStructureNavigator;
            _textUndoHistory = textUndoHistory;

            _disposable = new[]
                {
                    WireLayoutChangedEvent(),
                    WireCaretPositionChangedEvent(),
                    WireTextContentChangedEvent()
                }.Merge().Subscribe();

            RefreshSymbolTable(_textView.TextSnapshot.GetText());
        }

        private IObservable<Unit> WireTextContentChangedEvent()
        {
            return Observable
                .FromEventPattern<TextContentChangedEventArgs>(value => _textView.TextBuffer.Changed += value,
                                                               value => _textView.TextBuffer.Changed -= value)
                .Throttle(TimeSpan.FromMilliseconds(ThrottlingTime))
                .Do(x => HandleTextChanged(x.EventArgs))
                .Select(_ => Unit.Default);
        }

        private IObservable<Unit> WireCaretPositionChangedEvent()
        {
            return Observable
                .FromEventPattern<CaretPositionChangedEventArgs>(value => _textView.Caret.PositionChanged += value,
                                                                 value => _textView.Caret.PositionChanged -= value)
                .Throttle(TimeSpan.FromMilliseconds(ThrottlingTime))
                .Do(x => CaretPositionChanged(x.EventArgs))
                .Select(_ => Unit.Default);
        }

        private IObservable<Unit> WireLayoutChangedEvent()
        {
            return Observable
                .FromEventPattern<TextViewLayoutChangedEventArgs>(value => _textView.LayoutChanged += value,
                                                                  value => _textView.LayoutChanged -= value)
                .Throttle(TimeSpan.FromMilliseconds(ThrottlingTime))
                .Do(x => ViewLayoutChanged(x.EventArgs))
                .Select(_ => Unit.Default);
        }

        public bool Added { get; set; }
        public IOleCommandTarget NextTarget { get; set; }

        int IOleCommandTarget.QueryStatus(ref Guid pguidCmdGroup, uint cCmds, OLECMD[] prgCmds, IntPtr pCmdText)
        {
            const string guidMenuAndCommandsCmdSet = "{19492BCB-32B3-4EC3-8826-D67CD5526653}";

            if (pguidCmdGroup == Guid.Parse(guidMenuAndCommandsCmdSet) && 
                prgCmds.Any(x => x.cmdID == PkgCmdIDList.CmdidMyMenu))
            {
                prgCmds[0].cmdf = (uint)OLECMDF.OLECMDF_SUPPORTED | (uint)OLECMDF.OLECMDF_ENABLED;
                return VSConstants.S_OK;
            }

            if (pguidCmdGroup == Guid.Parse(guidMenuAndCommandsCmdSet) && 
                prgCmds.Any(x => x.cmdID == PkgCmdIDList.CmdidMyCommand) &&
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
            if (pguidCmdGroup == Guid.Parse("{19492BCB-32B3-4EC3-8826-D67CD5526653}") && nCmdId == PkgCmdIDList.CmdidMyCommand)
                HandleRename();
            return NextTarget.Exec(ref pguidCmdGroup, nCmdId, nCmdexecopt, pvaIn, pvaOut);
        }

        private void HandleRename()
        {
            if (CurrentWord.Success)
            {
                var foundUsages = FindUsages(CurrentWord);
                if (!foundUsages.Item1.Success)
                    return;
                TextToRename = foundUsages.Item1.Value.GetText();
                var wnd = new RenameDialog {DataContext = this};
                var ret = wnd.ShowDialog();
                if (ret.HasValue && ret.Value)
                    RenameAllOccurences(TextToRename, foundUsages.Item2);
            }
        }

        private void ViewLayoutChanged(TextViewLayoutChangedEventArgs e)
        {
            // If a new snapshot wasn't generated, then skip this layout
            if (e.NewViewState.EditSnapshot != e.OldViewState.EditSnapshot)
                UpdateAtCaretPosition(_textView.Caret.Position);
        }

        private void CaretPositionChanged(CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        private void UpdateAtCaretPosition(CaretPosition caretPoisition)
        {
            var point = caretPoisition.Point.GetPoint(_textView.TextBuffer, caretPoisition.Affinity);

            if (!point.HasValue)
                return;

            CurrentWord = _textStructureNavigator.FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(point.Value);
            if (CurrentWord.Success)
                TextToRename = CurrentWord.Value.GetText();
        }

        private Maybe<SnapshotSpan> CurrentWord { get; set; }
        
        public string TextToRename { get; set; }
                   
        private void RenameAllOccurences(string newText, IEnumerable<SnapshotSpan> foundUsages)
        {
            var afterSnapshot = _textView.TextSnapshot;
            
            var description = String.Format("Rename -> '{0}'", newText);
            using (var transaction = _textUndoHistory.CreateTransaction(description))
            {
                foreach (var usage in foundUsages)
                {
                    var newUsage = usage.TranslateTo(afterSnapshot, SpanTrackingMode.EdgeExclusive);
                    afterSnapshot = _textView.TextBuffer.Replace(newUsage, newText);
                }
                transaction.Complete();
            }
        }

        private Tuple<Maybe<SnapshotSpan>, List<SnapshotSpan>> FindUsages(Maybe<SnapshotSpan> word)
        {
            if (!word.Success)
                Tuple.Create(word, Enumerable.Empty<SnapshotSpan>());

            var ret = word.Value.FindTheNewSpans();

            var position = ret.Item1.GetPosition();
            position = Tuple.Create(position.Item1, position.Item2, position.Item3, position.Item4);
            var usagesOfModifiedWord =
                FSharpRefactor.findAllReferencesInSymbolTable(_symbolTable, position);
            
            return Tuple.Create(new Maybe<SnapshotSpan>{Value = ret.Item1, Success = true}, ret.Item2.Where(x => x.ReferencesContains(usagesOfModifiedWord)).ToList());
        }

        protected bool IsRenameableIdentifier
        {
            get { return FindUsages(CurrentWord).Item2.Any(); }
        }                      

        private void HandleTextChanged(TextContentChangedEventArgs e)
        {
            RefreshSymbolTable(e.After.GetText());
        }

        private void RefreshSymbolTable(string allText)
        {
            _symbolTable = ASTAnalysis.buildSymbolTable(FSharpRefactor.parseWithPos(allText));
        }

        public void Dispose()
        {
            _disposable.Dispose();
        }
    }
}
  