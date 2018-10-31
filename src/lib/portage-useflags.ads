--
--  Ada Spec: Portage.UseFlags
--
--  Description: UseFlag collections - top level (ADT) stuff
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2006-2008
--
--  Copyright: See COPYING file that comes with this distribution
--

with Ada.Containers.Ordered_Sets;

package Portage.UseFlags is

    --  Being a child of module Portage this one has a full visibility
    --  of its public part. In particular of the UseFlag type.
    --
    --  This is another "top-level" package. Here I'll define the ADT for
    --  UseFlag_Set and its immediate interface.
    --  Actual algorithms will live in a child module(s)

    type UseFlag_Set is private;
    --  A container for USE flags,
    --  As there can only be one use flag of the same name, this is naturally a set.
    --
    --  Flag matching is based on the name solely. Addition of the flag with the
    --  name already in collection overwrites its status. This provides an easy
    --  way to collate flags from multiple sources. Thus - the order is important!
    --
    --  It seems that a full (portage-wide) set will have semantics
    --  similar to any subset.
    --  May add another type if this does not hold though.

    ---  Construction stuff
    procedure Clear (Set : in out UseFlag_Set);
    --  empties the set

    procedure AddFlag (Set : in out UseFlag_Set; flag : in UseFlag);
    procedure AddFlag
       (Set        : in out UseFlag_Set;
        flag       : in String;
        flag_State : UseFlag_States := On);
    --  Addition of the flag by the same name overwrites its status

    function FlagStatus (Set : UseFlag_Set; flag : UseFlag) return UseFlag_States;
    function FlagStatus (Set : UseFlag_Set; flag : String)  return UseFlag_States;
    --  Absent is returned if either flag is not in collection
    --  or if it is, but has Status=Absent
    --  !NOTE! This may need to be changed if two above situations
    --  appear semantically different

    procedure RemoveFlag (Set : in out UseFlag_Set; flag : UseFlag);
    function  First_Flag (Set : UseFlag_Set) return UseFlag;
    function  Last_Flag  (Set : UseFlag_Set) return UseFlag;

    ------------------------------------
    --  Iteration

    type Cursor is private;
    --  encapsulates UseFlag_Set and "position"

    function Element (Position : Cursor) return UseFlag;
    function Find (Set : UseFlag_Set; Flag : UseFlag) return Cursor;

    function First (Set : UseFlag_Set) return Cursor;
    function Last  (Set : UseFlag_Set) return Cursor;

    function  Next (Position : Cursor) return Cursor;
    procedure Next (Position : in out Cursor);
    function  Previous (Position : Cursor) return Cursor;
    procedure Previous (Position : in out Cursor);

private

    --  The needed functionality seems to nicely match the abilities of
    --  Ada.Containers.Ordered_Sets. Will just use that one.
    --
    --  NOTE - should hashed sets be tried too?
    --  This is a prototyping stage after all :).

    package Use_Sets is new Ada.Containers.Ordered_Sets (Element_Type => UseFlag);

    type UseFlag_Set is record
        set : Use_Sets.Set;
    end record;

    type Cursor is record
        cur : Use_Sets.Cursor;
    end record;

end Portage.UseFlags;
