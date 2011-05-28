using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using FSharpRefactorVSAddIn.Common;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Operations;

namespace FSharpRefactorAddin.Common
{
    public static class TextUtils
    {
        public static Maybe<SnapshotSpan> FindAllWordsInTheBufferLikeTheOneTheCaretIsOn(this ITextStructureNavigator textStructureNavigator, SnapshotPoint currentRequest)
        {
            var word = textStructureNavigator.GetExtentOfWord(currentRequest);

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
                    word = textStructureNavigator.GetExtentOfWord(currentRequest - 1);

                    // If we still aren't valid the second time around, we're done
                    if (!WordExtentIsValid(currentRequest, word))
                        foundWord = false;
                }
            }

            return !foundWord ? new Maybe<SnapshotSpan> { Success = false } :
                new Maybe<SnapshotSpan> { Value = word.Span, Success = true };
        }

        private static bool WordExtentIsValid(SnapshotPoint currentRequest, TextExtent word)
        {
            return word.IsSignificant && currentRequest.Snapshot.GetText(word.Span).Any(c => char.IsLetter(c));
        }

        public static Tuple<int, int, int, int> GetPosition(this SnapshotSpan currentWord)
        {
            var extraLenght = currentWord.GetWordIncludingQuotes().Item2.Length - currentWord.Length;
            var lineStart = currentWord.Snapshot.GetLineNumberFromPosition(currentWord.Start.Position) + 1;
            var lineEnd = currentWord.Snapshot.GetLineNumberFromPosition(currentWord.End.Position) + 1;
            var startLine = currentWord.Snapshot.GetLineFromPosition(currentWord.Start.Position);
            var endLine = currentWord.Snapshot.GetLineFromPosition(currentWord.End.Position);
            var colStart = currentWord.Start.Position - startLine.Start.Position;
            var colEnd = currentWord.End.Position - endLine.Start.Position;
            return Tuple.Create(colStart, colEnd + extraLenght, lineStart, lineEnd);
        }

        public static Tuple<SnapshotSpan, string> GetWordIncludingQuotes(this SnapshotSpan currentWord)
        {
            var endWordPos = currentWord.End.Position;
            var word = currentWord.GetText().Trim();

            while (endWordPos < currentWord.Snapshot.Length && currentWord.Snapshot.GetText(endWordPos, 1) == "\'")
            {
                word += "\'";
                endWordPos++;
            }

            if (word.EndsWith("\'"))
            {
                currentWord = new SnapshotSpan(currentWord.Snapshot, currentWord.Start.Position, word.Length);
            }

            while (endWordPos < currentWord.Snapshot.Length && currentWord.Snapshot.GetText(endWordPos, 1) == "`")
            {
                word += "`";
                endWordPos++;
            }

            if (word.EndsWith("``"))
            {
                string newWord;
                var startWordPos = currentWord.Start.Position;
                do
                {
                    newWord = currentWord.Snapshot.GetText(startWordPos, endWordPos - startWordPos);
                    startWordPos--;
                }
                while (!newWord.StartsWith("``") || startWordPos <= 0 || currentWord.Snapshot.GetText(startWordPos, 1) == "\n");
                word = newWord;
                currentWord = new SnapshotSpan(currentWord.Snapshot, startWordPos + 1, word.Length);
            }

            return Tuple.Create(currentWord, word);
        }

        public static Tuple<SnapshotSpan, List<SnapshotSpan>> FindTheNewSpans(this SnapshotSpan currentWord)
        {
            var txt = currentWord.Snapshot.GetText();
            var word = currentWord.GetWordIncludingQuotes();
            var matches = Regex.Matches(txt, word.Item2);
            var spans = matches.Cast<Match>().Select(m => new SnapshotSpan(currentWord.Snapshot, m.Index, m.Length));
            return Tuple.Create(word.Item1, spans.ToList());
        }

        public static bool ReferencesContains(this SnapshotSpan currentWord, IEnumerable<Tuple<int, int, int, int>> references)
        {
            return references.Any(x => Equals(x, currentWord.GetPosition()));
        }     
    }
}
