using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Text;
using FSharpRefactorVSAddIn;

namespace FSharpRefactorAddin
{
    class RefactorImpl:IRefactor
    {

        public ITextBuffer CurrentTextBuffer { get; private set; }

        public HighlightUsagesTagger CurrentTagger { get; private set; }

        public RefactorImpl(ITextBuffer textBuffer, HighlightUsagesTagger tagger)
        {
            CurrentTagger = tagger;
            CurrentTextBuffer = textBuffer;
        }

        #region IRefactor Members



        public bool IsRenaming
        {
            get
            {
                return CurrentTagger == null ? false : CurrentTagger.Renaming;
            }
            set
            {
                if (CurrentTagger != null)
                    CurrentTagger.Renaming = value;
            }
        }

        public void Rename()
        {
            
        }

        #endregion
    }
}
