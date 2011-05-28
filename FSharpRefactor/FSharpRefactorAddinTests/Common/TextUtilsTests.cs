using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FSharpRefactorAddin.Common;
using FSharpRefactorAddinTests.Stubs;
using Microsoft.VisualStudio.Text;
using NUnit.Framework;

namespace FSharpRefactorAddinTests.Common
{
    [TestFixture]
    public class TextUtilsTests
    {
        [Test]
        public void GetSimpleWord()
        {
            var snapshotSpan = new SnapshotSpan(new TextSnapshotStub("Welcome to the real world."), 15, 4);
            var p = snapshotSpan.GetWordIncludingQuotes();
            Assert.AreEqual("real", p.Item2);
            Assert.AreEqual(snapshotSpan, p.Item1);
        }

        [Test]
        public void GetSimpleWordWithQuote()
        {
            var snapshot = new TextSnapshotStub("let f x' = x'");
            var snapshotSpan = new SnapshotSpan(snapshot, 6, 1);
            var p = snapshotSpan.GetWordIncludingQuotes();
            Assert.AreEqual("x'", p.Item2);
            Assert.AreEqual(new SnapshotSpan(snapshot, 6, 2), p.Item1);
        }

        [Test]
        public void GetSimpleWordWithTwoQuotes()
        {
            var snapshot = new TextSnapshotStub("let f x'' = x''");
            var snapshotSpan = new SnapshotSpan(snapshot, 6, 1);
            var p = snapshotSpan.GetWordIncludingQuotes();
            Assert.AreEqual("x''", p.Item2);
            Assert.AreEqual(new SnapshotSpan(snapshot, 6, 3), p.Item1);
        }

        [Test]
        public void GetLiteralIdentifiers()
        {
            var snapshot = new TextSnapshotStub("let f ``a value`` = ``a value``");
            var snapshotSpan = new SnapshotSpan(snapshot, 8, 7);
            var p = snapshotSpan.GetWordIncludingQuotes();
            Assert.AreEqual("``a value``", p.Item2);
            Assert.AreEqual(new SnapshotSpan(snapshot, 6, 11), p.Item1);
        }
    }
}
