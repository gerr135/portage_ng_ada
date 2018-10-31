--
--  Ada Spec: Portage.UseFlags
--
--  Description: IO routines for the UseFlag collections
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2006-2008
--
--  Copyright: See COPYING file that comes with this distribution
--

package Portage.UseFlags.IO is

    procedure AddFlags (Set : in out UseFlag_Set; UseStr : in String);
    --  Parse the string and add individual flags.
    --  Whitespace-separated list of flags, optionally prepended by + or -,
    --  is expected.

    procedure AppendEnvironment (Set : in out UseFlag_Set; EnvVar : in String);
    --  Read contents of the specified env var and pass it to AddFlags

    procedure AppendConfFile (Set : in out UseFlag_Set; FileName : in String);

    --  "high level" stuff
    procedure CollectFlags (Set : in out UseFlag_Set);
    --  Scans conf files in profiles, etc. in usual order
    --  appending stuff from environment.
    --  !FIXME!
    --  Right now simply calls AppendEnvironment.
    --  May need more parameters in the future.

end Portage.UseFlags.IO;
