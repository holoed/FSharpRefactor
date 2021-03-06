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
using System.Threading;
using FSharpRefactorAddin.Common;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Tagging;

namespace FSharpRefactorAddin.HighlightUsages
{
    /// <summary>
    /// This tagger will provide tags for every word in the buffer that
    /// matches the word currently under the cursor.
    /// </summary>
    public class HighlightUsagesTagger : ITagger<HighlightUsagesTag>, IDisposable
    {
        private readonly object _updateLock = new object();
        private readonly IDisposable _disposable;
        private const double ThrottlingTime = 500;

        public HighlightUsagesTagger(ITextView view, ITextBuffer sourceBuffer,
                                     ITextStructureNavigator textStructureNavigator)
        {
            View = view;
            SourceBuffer = sourceBuffer;
            TextStructureNavigator = textStructureNavigator;

            WordSpans = new NormalizedSnapshotSpanCollection();
            CurrentWord = null;

            // Subscribe to both change events in the view - any time the view is updated
            // or the caret is moved, we refresh our list of highlighted words.
            _disposable = new[]
                {
                    WireCaretPositionChangedEvent(), 
                    WireLayoutChangedEvent()
                }.Merge().Subscribe();
        }

        protected virtual IObservable<T> Throttle<T>(IObservable<T> xs)
        {
            return xs.Throttle(TimeSpan.FromMilliseconds(ThrottlingTime));
        }

        private IObservable<Unit> WireCaretPositionChangedEvent()
        {
            return Throttle(Observable
                                .FromEventPattern<CaretPositionChangedEventArgs>(value => View.Caret.PositionChanged += value,
                                                                                 value => View.Caret.PositionChanged -= value))
                .Do(x => CaretPositionChanged(x.EventArgs))
                .Select(_ => Unit.Default);
        }

        private IObservable<Unit> WireLayoutChangedEvent()
        {
            return Throttle(Observable
                                .FromEventPattern<TextViewLayoutChangedEventArgs>(value => View.LayoutChanged += value,
                                                                                  value => View.LayoutChanged -= value))
                .Do(x => ViewLayoutChanged(x.EventArgs))
                .Select(_ => Unit.Default);
        }

        private ITextView View { get; set; }
        private ITextBuffer SourceBuffer { get; set; }
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

        private void ViewLayoutChanged(TextViewLayoutChangedEventArgs e)
        {
            // If a new snapshot wasn't generated, then skip this layout
            if (e.NewViewState.EditSnapshot != e.OldViewState.EditSnapshot)
                UpdateAtCaretPosition(View.Caret.Position);
        }

        private void CaretPositionChanged(CaretPositionChangedEventArgs e)
        {
            UpdateAtCaretPosition(e.NewPosition);
        }

        private void UpdateAtCaretPosition(CaretPosition caretPosition)
        {
            var point = caretPosition.Point.GetPoint(SourceBuffer, caretPosition.Affinity);

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
            var currentWord = TextStructureNavigator.FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(currentRequest);

            if (!currentWord.Success)
            {
                // If we couldn't find a word, just clear out the existing markers
                SynchronousUpdate(currentRequest, new NormalizedSnapshotSpanCollection(), null);
                return;
            }

            // If this is the same word we currently have, we're done (e.g. caret moved within a word).
            if (CurrentWord.HasValue && currentWord.Value == CurrentWord)
                return;

            // Find the new spans
            var ret = currentWord.Value.FindTheNewSpans();


            // If we are still up-to-date (another change hasn't happened yet), do a real update))
            IfWeAreStillUpToDateDoARealUpdate(currentRequest, ret.Item1, ret.Item2);
        }              

        private void IfWeAreStillUpToDateDoARealUpdate(SnapshotPoint currentRequest, SnapshotSpan currentWord, List<SnapshotSpan> wordSpans)
        {
            if (currentRequest == RequestedPoint)
            {
                lock (_updateLock)
                {
                    var symbolTable = ASTAnalysis.buildSymbolTable(FSharpRefactor.parseWithPos(currentRequest.Snapshot.GetText()));
                    var pos = currentWord.GetPosition();
                    var references = FSharpRefactor.findAllReferencesInSymbolTable(symbolTable, pos);
                    var foundUsages = wordSpans.Where(x => x.ReferencesContains(references)).ToList();

                    SynchronousUpdate(currentRequest, new NormalizedSnapshotSpanCollection(foundUsages), currentWord);
                }
            }
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

        public void Dispose()
        {
            _disposable.Dispose();
        }
    }
}