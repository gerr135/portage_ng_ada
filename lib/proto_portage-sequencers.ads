-- Copyright (c) 2003 George Shapovalov <george@gentoo.org>.  All rights reserved.

-- # This program is free software; you can redistribute it and/or
-- # modify it under the terms of the GNU General Public License as
-- # published by the Free Software Foundation; either version 2 of the
-- # License, or (at your option) any later version.
-- #
-- # This program is distributed in the hope that it will be useful,
-- # but WITHOUT ANY WARRANTY; without even the implied warranty of
-- # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- # GNU General Public License for more details.
-- #
-- # You should have received a copy of the GNU General Public License
-- # along with this program; if not, write to the Free Software
-- # Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
-- # USA


-- This package defines "sequencer" types - stuff that has a linear
-- structure (such as lists and sets)

with Generic_Lists;
with BC.Containers.Sets.Unmanaged;
--with BC.Containers.Collections.Unmanaged;

with Proto_portage.Support;

package Proto_Portage.Sequencers is

-- all sequencers are controlled (i.e. they cleanly autodestruct upon leaving the scope)
-- sets do not keep positional info and do not provide iterators


package Ebuilds   is new Generic_Lists(Item=>Ebuild_Key);
package Use_Flags is new Generic_Lists(Item=>Useflag);

-- now we have two types: Ebuilds.List and Use_Flags.List,
-- both have the methods defined in generic package

-----------------------------
-- we also need a few set types for ebuilds and useflags
-- they serve different purposes and have different interfaces
-- therefore I am not bundling them together as lists

type Ebuild_Key_Set is private;
	-- this one is very basic. So far only necessary for some
	-- internal bookkeeping, but might be usefull somewhere else
	-- so making it visible

procedure Clear(TheSet : in out Ebuild_Key_Set);
procedure Add_Key(TheSet : in out Ebuild_Key_Set; key : Ebuild_Key);
procedure Remove_Key(TheSet : in out Ebuild_Key_Set; key : Ebuild_Key);

function  Contains(TheSet : Ebuild_Key_Set; key : Ebuild_Key) return Boolean;

----------------------------------------

type Inclusion_State is (Enabled, Disabled, Absent);
-- tristate result representing the +use/-use/absent
-- possible outcomes

type Useflag_Set is private;
	-- a single user flag and list of useflags.
	-- This is a real set, so no ordering is exposed
	-- I may expose a generic iterators (active and passive) later on,
	-- if this moves beyond prototype stage

procedure Clear(Set : in out Useflag_Set);
	--  Empty the set of all items.

function  Create_From_USE(From_String : String) return Useflag_Set;
procedure Append_From_USE(Use_Set : in out Useflag_Set; From_String : String);
	-- Init the set from USE= string (space separated list of useflags)
	-- both procedural and functional interface

function  Create_From_DEPEND(From_String : String) return Useflag_Set;
procedure Append_From_DEPEND(Use_Set : in out Useflag_Set; From_String : String);
	-- Init the set from individual dependency ( use1? ( pkg_name ) )
	-- supports only plain use? format for now

procedure Add(Set : in out Useflag_Set; Flag : Useflag; Enabled : Boolean := True);
procedure Remove (Set : in out Useflag_Set; Flag : Useflag);

function Is_Empty(Set : Useflag_Set) return Boolean;
	-- empty list in case of dependency indicated unconditional dep.

function Contains(Set : Useflag_Set; Flag : Useflag) return Inclusion_State;
function Is_aSubset(subSet, Set : Useflag_Set) return Boolean;
	-- last one is a portage-specific check
	-- returns true only if all enabled flags in sublist are present in list and
	-- if all explicitly disabled flags are absent (or disabled) in List
	-- If true, that arc may be followed, i.e. the dependency is active


private

-----------------
-- Ebuild_Key_Set

use Proto_portage.Support;

type Ebuild_Key_Set is record
	ums : UM_EbSets.Set;
end record;


-----------------
-- Usefllag_Set

type Useflag_Set is record
	ums : UM_FlagSets.Set;
end record;

end Proto_Portage.Sequencers;
