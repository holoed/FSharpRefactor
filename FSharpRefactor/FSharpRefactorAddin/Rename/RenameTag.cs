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

namespace FSharpRefactorVSAddIn.Rename
{
    public class RenameTag : TextMarkerTag
    {
        public RenameTag() : base("MarkerFormatDefinition/RenameFormatDefinition") { }
    }

    [Export(typeof(EditorFormatDefinition))]
    [Name("MarkerFormatDefinition/RenameFormatDefinition")]
    [UserVisible(true)]
    public class RenameFormatDefinition : MarkerFormatDefinition
    {
        public RenameFormatDefinition()
        {
            ForegroundColor = Colors.Red;
            BackgroundColor = Colors.Transparent;
            DisplayName = "Rename identifier";
            ZOrder = 6;
            
        }
    }
}