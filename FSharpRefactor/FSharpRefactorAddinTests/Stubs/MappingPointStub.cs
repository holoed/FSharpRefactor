using System;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Projection;

namespace FSharpRefactorAddinTests.Stubs
{
    public class MappingPointStub : IMappingPoint
    {
        public SnapshotPoint? GetPoint(ITextBuffer targetBuffer, PositionAffinity affinity)
        {
            return new SnapshotPoint(targetBuffer.CurrentSnapshot, 0);
        }

        public SnapshotPoint? GetPoint(ITextSnapshot targetSnapshot, PositionAffinity affinity)
        {
            throw new NotImplementedException();
        }

        public SnapshotPoint? GetPoint(Predicate<ITextBuffer> match, PositionAffinity affinity)
        {
            throw new NotImplementedException();
        }

        public SnapshotPoint? GetInsertionPoint(Predicate<ITextBuffer> match)
        {
            throw new NotImplementedException();
        }

        public ITextBuffer AnchorBuffer
        {
            get { throw new NotImplementedException(); }
        }

        public IBufferGraph BufferGraph
        {
            get { throw new NotImplementedException(); }
        }
    }
}