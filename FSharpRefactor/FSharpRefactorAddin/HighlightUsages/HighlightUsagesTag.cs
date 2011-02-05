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

using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace FSharpRefactorVSAddIn.HighlightUsages
{
    public class HighlightUsagesTag : TextMarkerTag
    {
        public HighlightUsagesTag() : base("MarkerFormatDefinition/HighlightIdentifierFormatDefinition") { }
    }

    [Export(typeof(EditorFormatDefinition))]
    [Name("MarkerFormatDefinition/HighlightIdentifierFormatDefinition")]
    [UserVisible(true)]
    public class HighlightIdentifierFormatDefinition : MarkerFormatDefinition
    {
        public HighlightIdentifierFormatDefinition()
        {
            BackgroundColor = Colors.LightGreen;
            ForegroundColor = Colors.DarkGreen;
            DisplayName = "Highlight Identifier";
            ZOrder = 5;
        }
    }
}