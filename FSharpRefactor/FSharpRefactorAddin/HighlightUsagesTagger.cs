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
using System.Threading;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;

namespace FSharpRefactorVSAddIn
{
    /// <summary>
    /// This tagger will provide tags for every word in the buffer that
    /// matches the word currently under the cursor.
    /// </summary>
    public class HighlightUsagesTagger : ITagger<HighlightUsagesTag>
    {
        private readonly object _updateLock = new object();
        private ASTAnalysis.SymbolTable _symbolTable;

        public HighlightUsagesTagger(ITextView view, ITextBuffer sourceBuffer, ITextSearchService textSearchService,
                                     ITextStructureNavigator textStructureNavigator)
        {
            View = view;
            SourceBuffer = sourceBuffer;
            TextSearchService = textSearchService;
            TextStructureNavigator = textStructureNavigator;

            WordSpans = new NormalizedSnapshotSpanCollection();
            CurrentWord = null;

            // Subscribe to both change events in the view - any time the view is updated
            // or the caret is moved, we refresh our list of highlighted words.
            View.Caret.PositionChanged += CaretPositionChanged;
            View.LayoutChanged += ViewLayoutChanged;
            View.TextBuffer.Changed += HandleTextChanged;
        }        

        private ITextView View { get; set; }
        private ITextBuffer SourceBuffer { get; set; }
        private ITextSearchService TextSearchService { get; set; }
        private ITextStructureNavigator TextStructureNavigator { get; set; }

        // The current set of words to highlight
        private NormalizedSnapshotSpanCollection WordSpans { get; set; }
        private SnapshotSpan? CurrentWord { get; set; }

        // The current request, from the last cursor movement or view render
        private SnapshotPoint RequestedPoint { get; set; }

        public IEnumerable<ITagSpan<HighlightUsagesTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (CurrentWord == null)
                yield break;

            // Hold on to a "snapshot" of the word spans and current word, so that we maintain the same
            // collection throughout
            var currentWord = CurrentWord.Value;
            var wordSpans = WordSpans;

            if (spans.Count == 0 || WordSpans.Count == 0)
                yield break;

            // If the requested snapshot isn't the same as the one our words are on, translate our spans
            // to the expected snapshot
            if (spans[0].Snapshot != wordSpans[0].Snapshot)
            {
                wordSpans = new NormalizedSnapshotSpanCollection(
                    wordSpans.Select(span => span.TranslateTo(spans[0].Snapshot, SpanTrackingMode.EdgeExclusive)));

                currentWord = currentWord.TranslateTo(spans[0].Snapshot, SpanTrackingMode.EdgeExclusive);
            }

            // First, yield back the word the cursor is under (if it overlaps)
            // Note that we'll yield back the same word again in the wordspans collection;
            // the duplication here is expected.
            if (spans.OverlapsWith(new NormalizedSnapshotSpanCollection(currentWord)))
                yield return new TagSpan<HighlightUsagesTag>(currentWord, new HighlightUsagesTag());

            // Second, yield all the other words in the file
            foreach (var span in NormalizedSnapshotSpanCollection.Overlap(spans, wordSpans))
                yield return new TagSpan<HighlightUsagesTag>(span, new HighlightUsagesTag());
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        private void ViewLayoutChanged(object sender, TextViewLayoutChangedEventArgs e)
        {
            // If a new snapshot wasn't generated, then skip this layout
            if (e.NewViewState.EditSnapshot != e.OldViewState.EditSnapshot)
                UpdateAtCaretPosition(View.Caret.Position);
        }

        private void CaretPositionChanged(object sender, CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        private void UpdateAtCaretPosition(CaretPosition caretPoisition)
        {
            var point = caretPoisition.Point.GetPoint(SourceBuffer, caretPoisition.Affinity);

            if (!point.HasValue)
                return;

            // If the new cursor position is still within the current word (and on the same snapshot),
            // we don't need to check it.
            if (CurrentWord.HasValue &&
                CurrentWord.Value.Snapshot == View.TextSnapshot &&
                point.Value >= CurrentWord.Value.Start &&
                point.Value <= CurrentWord.Value.End)
                return;

            RequestedPoint = point.Value;

            ThreadPool.QueueUserWorkItem(UpdateWordAdornments);
        }

        private void UpdateWordAdornments(object threadContext)
        {
            var currentRequest = RequestedPoint;            

            // Find all words in the buffer like the one the caret is on
            var currentWord = FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(currentRequest);

            if (!currentWord.Success)
                return;

            // If this is the same word we currently have, we're done (e.g. caret moved within a word).
            if (CurrentWord.HasValue && currentWord.Value == CurrentWord)
                return;

            // Find the new spans
            var wordSpans = FindTheNewSpans(currentWord.Value);


            // If we are still up-to-date (another change hasn't happened yet), do a real update))
            IfWeAreStillUpToDateDoARealUpdate(currentRequest, currentWord.Value, wordSpans);
        }

        private Maybe<SnapshotSpan> FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(SnapshotPoint currentRequest)
        {
            var word = TextStructureNavigator.GetExtentOfWord(currentRequest);

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
                    word = TextStructureNavigator.GetExtentOfWord(currentRequest - 1);

                    // If we still aren't valid the second time around, we're done
                    if (!WordExtentIsValid(currentRequest, word))
                        foundWord = false;
                }
            }

            if (!foundWord)
            {
                // If we couldn't find a word, just clear out the existing markers
                SynchronousUpdate(currentRequest, new NormalizedSnapshotSpanCollection(), null);
                return new Maybe<SnapshotSpan>{Success =  false};
            }

            return new Maybe<SnapshotSpan> {Value = word.Span, Success = true};
        }

        private static List<SnapshotSpan> FindTheNewSpans(SnapshotSpan currentWord)
        {
            var txt = currentWord.Snapshot.GetText();
            var matches = Regex.Matches(txt, "\\b" + currentWord.GetText().Trim() + "\\b");
            var spans = matches.Cast<Match>().Select(m => new SnapshotSpan(currentWord.Snapshot, m.Index, m.Length));           
            return spans.ToList();
        }

        private void IfWeAreStillUpToDateDoARealUpdate(SnapshotPoint currentRequest, SnapshotSpan currentWord, List<SnapshotSpan> wordSpans)
        {
            if (currentRequest == RequestedPoint)
            {
                lock (_updateLock)
                {
                    if (_symbolTable == null)
                        _symbolTable = ASTAnalysis.buildSymbolTable(FSharpRefactor.parseWithPos(currentRequest.Snapshot.GetText()));
                    var pos = GetPosition(currentWord);
                    var references = FSharpRefactor.findAllReferencesInSymbolTable(_symbolTable, pos);
                    var foundUsages = wordSpans.Where(x => ReferencesContains(references, x)).ToList();

                    SynchronousUpdate(currentRequest, new NormalizedSnapshotSpanCollection(foundUsages), currentWord);
                }
            }
        }

        private void HandleTextChanged(object sender, TextContentChangedEventArgs e)
        {
            var allText = e.After.GetText();
            var newSymbolTable = ASTAnalysis.buildSymbolTable(FSharpRefactor.parseWithPos(allText));
            if (e.Changes.Count == 0 || (e.Changes[0].NewText.Trim() == string.Empty && e.Changes[0].OldText.Trim() == string.Empty))
                return;

            var textChange = e.Changes[0];
            var wordBefore = FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(new SnapshotPoint(e.Before, textChange.OldPosition));
            var wordAfter = FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(new SnapshotPoint(e.After, textChange.NewPosition));
            if (wordBefore.Success && wordAfter.Success)
            {
                var oldText = Regex.Match(e.Before.GetText(wordBefore.Value), "\\b\\w+\\b");
                if (oldText.Success)
                {
                    var position = GetPosition(wordBefore.Value);
                    position = Tuple.Create(position.Item1, position.Item1 + oldText.Length, position.Item3, position.Item4);
                    var usagesOfModifiedWord =
                        FSharpRefactor.findAllReferencesInSymbolTable(_symbolTable, position).Where(p => !(p.Item1 == position.Item1 &&
                                                                                                           p.Item2 == position.Item2 &&
                                                                                                           p.Item3 == position.Item3 &&
                                                                                                           p.Item4 == position.Item4));

                    var spans = FindTheNewSpans(wordBefore.Value);
                    var foundUsages = spans.Where(x => ReferencesContains(usagesOfModifiedWord, x)).ToList();

                    var text = Regex.Match(e.After.GetText(wordAfter.Value), "\\b\\w+\\b").Value;
                    var afterSnapshot = e.After;
                    SourceBuffer.Changed -= HandleTextChanged;
                    foreach (var usage in foundUsages)
                    {                        
                        var newUsage = usage.TranslateTo(afterSnapshot, SpanTrackingMode.EdgeExclusive);
                        afterSnapshot = SourceBuffer.Replace(newUsage, text);
                    }
                    SourceBuffer.Changed += HandleTextChanged;
                }
            }

            _symbolTable = newSymbolTable;
        }        

        private static bool ReferencesContains(IEnumerable<Tuple<int, int, int, int>> references, SnapshotSpan currentWord)
        {
            return references.Any(x => Equals(x, GetPosition(currentWord)));
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

        /// <summary>
        /// Determine if a given "word" should be highlighted
        /// </summary>
        private static bool WordExtentIsValid(SnapshotPoint currentRequest, TextExtent word)
        {
            return word.IsSignificant && currentRequest.Snapshot.GetText(word.Span).Any(c => char.IsLetter(c));
        }

        /// <summary>
        /// Perform a synchronous update, in case multiple background threads are running
        /// </summary>
        private void SynchronousUpdate(SnapshotPoint currentRequest, NormalizedSnapshotSpanCollection newSpans,
                                       SnapshotSpan? newCurrentWord)
        {
            lock (_updateLock)
            {
                if (currentRequest != RequestedPoint)
                    return;

                WordSpans = newSpans;
                CurrentWord = newCurrentWord;

                var tempEvent = TagsChanged;
                if (tempEvent != null)
                    tempEvent(this,
                              new SnapshotSpanEventArgs(new SnapshotSpan(SourceBuffer.CurrentSnapshot, 0,
                                                                         SourceBuffer.CurrentSnapshot.Length)));
            }
        }
    }
}