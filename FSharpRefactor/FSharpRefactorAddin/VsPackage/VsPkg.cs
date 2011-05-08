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

using System.Runtime.InteropServices;
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
