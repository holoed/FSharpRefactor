using System;
using FSharpRefactorAddin.HighlightUsages;
using FSharpRefactorAddinTests.Stubs;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Operations;
using Microsoft.VisualStudio.Text.Projection;
using NUnit.Framework;

namespace FSharpRefactorAddinTests.HighlightUsages
{
    [TestFixture]
    public class HighlightUsagesTaggerTests
    {
        private HighlightUsagesTagger _tagger;
        private TextViewStub _textView;

        [SetUp]
        public void SetUp()
        {
            _textView = new TextViewStub();
            _tagger = new TestableHighlightUsagesTagger(_textView, new TextBufferStub(),
                                                new TextStructureNavigatorStub());
        }

        //[Test]
        //public void Test()
        //{
        //    _textView.OnCaretPositionChanged(
        //        new CaretPosition(new VirtualSnapshotPoint(), new MappingPointStub(), PositionAffinity.Successor), 
        //        new CaretPosition(new VirtualSnapshotPoint(), new MappingPointStub(), PositionAffinity.Successor));
        //}
    }
  
    public class TestableHighlightUsagesTagger : HighlightUsagesTagger
    {
        public TestableHighlightUsagesTagger(ITextView textView, ITextBuffer textBuffer, ITextStructureNavigator textStructureNavigator) 
            : base(textView, textBuffer, textStructureNavigator)
        {}

        protected override IObservable<T> Throttle<T>(IObservable<T> xs)
        {
            return xs;
        }
    }
}
