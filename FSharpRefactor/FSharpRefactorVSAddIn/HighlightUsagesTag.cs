using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace FSharpRefactorVSAddIn
{
    public class HighlightUsagesTag : TextMarkerTag
    {
        public HighlightUsagesTag() : base("MarkerFormatDefinition/HighlightWordFormatDefinition") { }
    }

    [Export(typeof(EditorFormatDefinition))]
    [Name("MarkerFormatDefinition/HighlightWordFormatDefinition")]
    [UserVisible(true)]
    public class HighlightWordFormatDefinition : MarkerFormatDefinition
    {
        public HighlightWordFormatDefinition()
        {
            BackgroundColor = Colors.LightGreen;
            ForegroundColor = Colors.DarkGreen;
            DisplayName = "Highlight Word";
            ZOrder = 5;
        }
    }
}