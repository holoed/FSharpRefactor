using Microsoft.VisualStudio.Text.Tagging;

namespace FSharpRefactorVSAddIn
{
    public class HighlightUsagesTag : TextMarkerTag 
    { 
        public HighlightUsagesTag() : base("blue") { }
    }
}