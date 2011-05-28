using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Utilities;

namespace FSharpRefactorAddinTests.Stubs
{
    public class TextStructureNavigatorStub : ITextStructureNavigator
    {
        public TextExtent GetExtentOfWord(SnapshotPoint currentPosition)
        {
            throw new NotImplementedException();
        }

        public SnapshotSpan GetSpanOfEnclosing(SnapshotSpan activeSpan)
        {
            throw new NotImplementedException();
        }

        public SnapshotSpan GetSpanOfFirstChild(SnapshotSpan activeSpan)
        {
            throw new NotImplementedException();
        }

        public SnapshotSpan GetSpanOfNextSibling(SnapshotSpan activeSpan)
        {
            throw new NotImplementedException();
        }

        public SnapshotSpan GetSpanOfPreviousSibling(SnapshotSpan activeSpan)
        {
            throw new NotImplementedException();
        }

        public IContentType ContentType
        {
            get { throw new NotImplementedException(); }
        }
    }
}