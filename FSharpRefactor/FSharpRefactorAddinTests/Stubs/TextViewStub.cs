using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Formatting;
using Microsoft.VisualStudio.Text.Projection;
using Microsoft.VisualStudio.Utilities;

namespace FSharpRefactorAddinTests.Stubs
{
    public class TextViewStub : ITextView
    {
        private readonly TextCaretStub _caret = new TextCaretStub();

        public void OnCaretPositionChanged(CaretPosition oldPosition, CaretPosition newPosition)
        {
            _caret.InvokePositionChanged(new CaretPositionChangedEventArgs(this, oldPosition, newPosition));
        }

        public PropertyCollection Properties
        {
            get { throw new NotImplementedException(); }
        }

        public void DisplayTextLineContainingBufferPosition(SnapshotPoint bufferPosition, double verticalDistance, ViewRelativePosition relativeTo)
        {
            throw new NotImplementedException();
        }

        public void DisplayTextLineContainingBufferPosition(SnapshotPoint bufferPosition, double verticalDistance, ViewRelativePosition relativeTo, double? viewportWidthOverride, double? viewportHeightOverride)
        {
            throw new NotImplementedException();
        }

        public SnapshotSpan GetTextElementSpan(SnapshotPoint point)
        {
            throw new NotImplementedException();
        }

        public void Close()
        {
            throw new NotImplementedException();
        }

        public void QueueSpaceReservationStackRefresh()
        {
            throw new NotImplementedException();
        }

        public ITextViewLine GetTextViewLineContainingBufferPosition(SnapshotPoint bufferPosition)
        {
            throw new NotImplementedException();
        }

        public bool InLayout
        {
            get { throw new NotImplementedException(); }
        }

        public IViewScroller ViewScroller
        {
            get { throw new NotImplementedException(); }
        }

        public ITextViewLineCollection TextViewLines
        {
            get { throw new NotImplementedException(); }
        }

        public ITextCaret Caret
        {
            get { return _caret; }
        }

        public ITextSelection Selection
        {
            get { throw new NotImplementedException(); }
        }

        public ITrackingSpan ProvisionalTextHighlight
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public ITextViewRoleSet Roles
        {
            get { throw new NotImplementedException(); }
        }

        public ITextBuffer TextBuffer
        {
            get { throw new NotImplementedException(); }
        }

        public IBufferGraph BufferGraph
        {
            get { throw new NotImplementedException(); }
        }

        public ITextSnapshot TextSnapshot
        {
            get { throw new NotImplementedException(); }
        }

        public ITextSnapshot VisualSnapshot
        {
            get { throw new NotImplementedException(); }
        }

        public ITextViewModel TextViewModel
        {
            get { throw new NotImplementedException(); }
        }

        public ITextDataModel TextDataModel
        {
            get { throw new NotImplementedException(); }
        }

        public double MaxTextRightCoordinate
        {
            get { throw new NotImplementedException(); }
        }

        public double ViewportLeft
        {
            get { throw new NotImplementedException(); }
            set { throw new NotImplementedException(); }
        }

        public double ViewportTop
        {
            get { throw new NotImplementedException(); }
        }

        public double ViewportRight
        {
            get { throw new NotImplementedException(); }
        }

        public double ViewportBottom
        {
            get { throw new NotImplementedException(); }
        }

        public double ViewportWidth
        {
            get { throw new NotImplementedException(); }
        }

        public double ViewportHeight
        {
            get { throw new NotImplementedException(); }
        }

        public double LineHeight
        {
            get { throw new NotImplementedException(); }
        }

        public bool IsClosed
        {
            get { throw new NotImplementedException(); }
        }

        public IEditorOptions Options
        {
            get { throw new NotImplementedException(); }
        }

        public bool IsMouseOverViewOrAdornments
        {
            get { throw new NotImplementedException(); }
        }

        public bool HasAggregateFocus
        {
            get { throw new NotImplementedException(); }
        }

        public event EventHandler<TextViewLayoutChangedEventArgs> LayoutChanged;
        public event EventHandler ViewportLeftChanged;
        public event EventHandler ViewportHeightChanged;
        public event EventHandler ViewportWidthChanged;
        public event EventHandler<MouseHoverEventArgs> MouseHover;
        public event EventHandler Closed;
        public event EventHandler LostAggregateFocus;
        public event EventHandler GotAggregateFocus;
    }
}