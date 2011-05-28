using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;

namespace FSharpRefactorAddinTests.Stubs
{
    public class TextSnapshotStub : ITextSnapshot
    {
        private readonly string _text;

        public TextSnapshotStub(string text)
        {
            _text = text;
        }

        public string GetText(Span span)
        {
            return _text.Substring(span.Start, span.Length);
        }

        public string GetText(int startIndex, int length)
        {
            return _text.Substring(startIndex, length);
        }

        public string GetText()
        {
            throw new NotImplementedException();
        }

        public char[] ToCharArray(int startIndex, int length)
        {
            throw new NotImplementedException();
        }

        public void CopyTo(int sourceIndex, char[] destination, int destinationIndex, int count)
        {
            throw new NotImplementedException();
        }

        public ITrackingPoint CreateTrackingPoint(int position, PointTrackingMode trackingMode)
        {
            throw new NotImplementedException();
        }

        public ITrackingPoint CreateTrackingPoint(int position, PointTrackingMode trackingMode, TrackingFidelityMode trackingFidelity)
        {
            throw new NotImplementedException();
        }

        public ITrackingSpan CreateTrackingSpan(Span span, SpanTrackingMode trackingMode)
        {
            throw new NotImplementedException();
        }

        public ITrackingSpan CreateTrackingSpan(Span span, SpanTrackingMode trackingMode, TrackingFidelityMode trackingFidelity)
        {
            throw new NotImplementedException();
        }

        public ITrackingSpan CreateTrackingSpan(int start, int length, SpanTrackingMode trackingMode)
        {
            throw new NotImplementedException();
        }

        public ITrackingSpan CreateTrackingSpan(int start, int length, SpanTrackingMode trackingMode, TrackingFidelityMode trackingFidelity)
        {
            throw new NotImplementedException();
        }

        public ITextSnapshotLine GetLineFromLineNumber(int lineNumber)
        {
            throw new NotImplementedException();
        }

        public ITextSnapshotLine GetLineFromPosition(int position)
        {
            throw new NotImplementedException();
        }

        public int GetLineNumberFromPosition(int position)
        {
            throw new NotImplementedException();
        }

        public void Write(TextWriter writer, Span span)
        {
            throw new NotImplementedException();
        }

        public void Write(TextWriter writer)
        {
            throw new NotImplementedException();
        }

        public ITextBuffer TextBuffer
        {
            get { return new TextBufferStub(_text); }
        }

        public IContentType ContentType
        {
            get { throw new NotImplementedException(); }
        }

        public ITextVersion Version
        {
            get { return new TextVersionStub(); }
        }

        public int Length
        {
            get { return _text.Length; }
        }

        public int LineCount
        {
            get { throw new NotImplementedException(); }
        }

        public char this[int position]
        {
            get { throw new NotImplementedException(); }
        }

        public IEnumerable<ITextSnapshotLine> Lines
        {
            get { throw new NotImplementedException(); }
        }
    }
}