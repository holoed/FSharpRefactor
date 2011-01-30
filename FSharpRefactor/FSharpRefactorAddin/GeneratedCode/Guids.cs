using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FSharpRefactorAddin
{
    static class GuidList
    {
        public const string guidFSharpRefactorAddinPkgString = "e4119a0c-4387-41dc-8b4a-573a04e5e175";
        public const string guidCommandSetString = "7309f45b-35f6-4fa6-97d9-730779d9a1ea";

        public static readonly Guid guidFSharpRefactorAddinCmdSet = new Guid(guidCommandSetString);
    };
}
