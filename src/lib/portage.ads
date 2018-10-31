--  Copyright (c) 2006 George Shapovalov <george@gentoo.org>.  All rights reserved.

--  # This program is free software; you can redistribute it and/or
--  # modify it under the terms of the GNU General Public License as
--  # published by the Free Software Foundation; either version 2 of the
--  # License, or (at your option) any later version.
--  #
--  # This program is distributed in the hope that it will be useful,
--  # but WITHOUT ANY WARRANTY; without even the implied warranty of
--  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  # GNU General Public License for more details.
--  #
--  # You should have received a copy of the GNU General Public License
--  # along with this program; if not, write to the Free Software
--  # Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
--  # USA

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Portage is

    --  In Ada, unlike many other languages, you may have hierarchial organization
    --  not only of types, but also packages. This is a top level - the (grand)parent.
    --  This module will contain the most common types which will be visible
    --  from within all child packages.

    --  As one might notice, I am using private types throughout.
    --  This is done first of all for illustration purposes,
    --  however this have saved me a bit of work already as at few occasions
    --  I realised that my original data representation "doesn't cut it" and
    --  had to redo some structures. Should I mention that I didn't have
    --  to change *any* of the downstream code (that is the code using
    --  these structures)?

    Not_Implemented : exception;
    --  A placeholder exception to mark stuff that needs more work
    --  (in a way so as to not forget that more work is necessary :))

    Inconsistent_Tree : exception;
    --  Raised if problems are found when building a Depgraph,
    --  for example a dependency referencing non-existant ebuild.

    USE_EnvVar_Name : constant String := "USE";

    Ebuild_Name_Pattern : constant String := "*.ebuild";

    PortageDir : constant String := ".";
    --  FIXME! pwd makes sense for prototyping,
    --  but should be fixed in real implementation
    Cache_Name : constant String := ".proto_pcache";
    --  FIXME!
    --  If this ever goes anywhere, it makes sense to change the
    --  file name to something which is less of a proto(type)


    -------------------------
    --  Ebuild "indexing"
    --
    --  It might be desirable to represent ebuilds by some index, rather than plane name
    --  The EHandle and Ebuild_key types provision this ability.
    --  The idea is that EHandle keeps track of key <-> name correspondence,
    --  So, the Ebuild_key is unique for every EHandle.
    --  Another possibility is to create a hash upon reading all the ebuild names
    --  and use that as an internal index.

    -- 	type EHandle is private;

    type Ebuild_Key is private;
    --  An "index" type - some immutable representation

    function Create   (Name : String)    return Ebuild_Key;
    function Get_Name (Key : Ebuild_Key) return String;
    function "=" (Left, Right : in Ebuild_Key) return Boolean;


	---------------------------------------------------------------
    --  This is a type to hold misc lists of the keys to be passed around.
	--  This has nothing to do with the "main tree", the list of "complete"
	--  ebuilds is defined in Portage.Ebuilds
    type EbuildKey_List is private;
	--  FIXME! Check that we actually use it at all, or that we do not
	--  need indexing/iteration.

    procedure Clear    (EList : in out EbuildKey_List);
    function  Is_Empty (EList : EbuildKey_List) return Boolean;

    --  As elsewhere, only "name matters". If the key is already on the list
    --  Add does nothing. If the key is absent Remove simply returns.
    procedure Add    (EList : in out EbuildKey_List; ebd : in Ebuild_Key);
    procedure Remove (EList : in out EbuildKey_List; ebd : in Ebuild_Key);
    function  HasKey (EList : EbuildKey_List; key : Ebuild_Key) return Boolean;


    --------------------------------------------------------------

    type UseFlag_States is (On, Off, Absent);
    --  Absent is used in situation when flag query returns empty

    type UseFlag is private;
    --  USE flags are supposed to be immutable.
    --  Therefore sticking to the simplest interface (i.e. using fixed strings) seems
    --  to make the most sense.

    function Create (Name : String; State : UseFlag_States := On) return UseFlag;

    function Get_Name  (Flag : UseFlag) return String;
    function Get_State (Flag : UseFlag) return UseFlag_States;

    type Traversal_Direction is (Direct, Reversed);
    --  indication of dependency traversal direction
    --  Note: "reverse" is a reserved word

private

    --  right now use actual names as keys. May uncomment EHandle and provide
    --  actually a non-trivial key.
    --  May be some kind of hash, may be just an index into some vector.

    use Ada.Strings.Unbounded;
    --  actually this may turn out to be space efficient as well,
    --  since Unbounded strings are reference-counted and controlled

    type Ebuild_Key is record
        key : Unbounded_String;
    end record;

    package EbuildKey_Vectors is new Ada.Containers.Vectors (
        Index_Type   => Positive,
        Element_Type => Ebuild_Key);

    type EbuildKey_List is tagged record
        list : EbuildKey_Vectors.Vector;
    end record;

    type UseFlag is record
        Name  : Unbounded_String;
        State : UseFlag_States;
    end record;

    --  NOTE  should these be exposed?
    function "<" (Left, Right : UseFlag) return Boolean;
    function "=" (Left, Right : UseFlag) return Boolean;
    --  flags are matched solely by name! Flag states are not considered!

end Portage;
