using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.OLE.Interop;
using Microsoft.VisualStudio.Shell;


namespace FSharpRefactorAddin
{
    [Guid(GuidList.guidFSharpRefactorAddinPkgString)]
    public class FSharpRefactorAddinPackage : FSharpRefactorAddinPackageBase
    {
    }
}