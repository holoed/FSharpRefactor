using System;
using System.ComponentModel.Design;
using System.Runtime.InteropServices;
using System.Security.Permissions;
using Microsoft.VisualStudio.Shell;

namespace FSharpRefactorAddin.VsPackage
{
	[PackageRegistration(UseManagedResourcesOnly = true)]	
	[ProvideMenuResource(1000, 1)]
    [Guid("3C7C5ABE-82AC-4A37-B077-0FF60E8B1FD3")]
    [InstalledProductRegistration("FSharpRefactor", "FSharpRefactor", "1.0", IconResourceID = 400)]
	[ComVisible(true)]
	public sealed class MenuCommandsPackage : Package
	{			
       
	}
}
