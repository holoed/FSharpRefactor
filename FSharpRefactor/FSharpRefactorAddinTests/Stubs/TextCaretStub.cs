using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Formatting;

namespace FSharpRefactorAddinTests.Stubs
{
    public class TextCaretStub : ITextCaret 
    {
        public void EnsureVisible()
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(ITextViewLine textLine, double xCoordinate)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(ITextViewLine textLine, double xCoordinate, bool captureHorizontalPosition)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(ITextViewLine textLine)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(SnapshotPoint bufferPosition, PositionAffinity caretAffinity)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(SnapshotPoint bufferPosition, PositionAffinity caretAffinity, bool captureHorizontalPosition)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(VirtualSnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(VirtualSnapshotPoint bufferPosition, PositionAffinity caretAffinity)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveTo(VirtualSnapshotPoint bufferPosition, PositionAffinity caretAffinity, bool captureHorizontalPosition)
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveToPreferredCoordinates()
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveToNextCaretPosition()
        {
            throw new NotImplementedException();
        }

        public CaretPosition MoveToPreviousCaretPosition()
        {
            throw new NotImplementedException();
        }

        public ITextViewLine ContainingTextViewLine
        {
            get { throw new NotImplementedException(); }
        }

        public double Left
        {
            get { throw new NotImplementedException(); }
        }

        public double Width
        {
            get { throw new NotImplementedException(); }
        }

        public double Right
        {
            get { throw new NotImplementedException(); }
        }

        public double Top
        {
            get { throw new NotImplementedException(); }
        }

        public double Height
        {
            get { throw new NotImplementedException(); }
        }

        public double Bottom
        {
            get { throw new NotImplementedException(); }
        }

        public CaretPosition Position
        {
            get { throw new NotImplementedException(); }
        }

        public bool OverwriteMode
        {
            get { throw new NotImplementedException(); }
        }

        public bool InVirtualSpace
        {
            get { throw new NotImplementedException(); }
        }

        public bool IsHidden
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public event EventHandler<CaretPositionChangedEventArgs> PositionChanged;

        public void InvokePositionChanged(CaretPositionChangedEventArgs e)
        {
            EventHandler<CaretPositionChangedEventArgs> handler = PositionChanged;
            if (handler != null) handler(this, e);
        }
    }
}