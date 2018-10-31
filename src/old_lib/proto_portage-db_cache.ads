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

-- This package defines the digraph structure and its associated methods

--with BC.Graphs.Directed.DFS_Traverse;
with BC.Graphs.Directed;
with BC.Containers.Collections.Unmanaged;
with BC.Containers.Queues.Unmanaged;

with Proto_Portage.Sequencers; use Proto_Portage.Sequencers;
with Proto_Portage.Support;

package Proto_Portage.DB_Cache is

Invalid_Path : Exception;
	-- raised if rebuild or read from non-existant path is requested
	-- (or passed/preset path is empty)

Broken_Cache : Exception;
	-- invalid header or otherwise broken data encountered
	-- while trying to read cache dump

type Dependency_Cache is limited private;
	-- a "root" class implementing the digraph of dependencies, cached in memory
	--
	-- limited means that ":=" and "=" are prohibited. They could have been overloaded,
	-- but they are not realy usefull for these objects anyway.
	--
	-- The type is controlled (i.e. it will autodestruct cleanly upon leaving the scope).
	--
	-- The real implementatio should also be made tagged (allowing dynamic polimorphism)
	-- or more precisely the root should be abstract tagged and
	-- specific types for Portage_Cache and World_Cache, etc. should be derived

-- base methods
-- remember - since the type is limited we cannot return it as a function result

procedure Clear(Cache : in out Dependency_Cache);
	-- empties the dependency info, other fields are left intact
procedure Clear_All(Cache : in out Dependency_Cache);
	-- reset all fields to empty values

procedure Regen_From_TextTree(Cache : in out Dependency_Cache; Tree_Location : String;
		Debug : Boolean := False);
procedure Regen_From_TextTree(Cache : in out Dependency_Cache;
		Debug : Boolean := False);
	-- read the portage tree and generate digraph.
	-- first routine also sets location of portage tree
	-- second uses preset one (raises Invalid_Path if empty or non-existant)

procedure Dump(Cache : in out Dependency_Cache; To_Path : String);
procedure Dump(Cache : in out Dependency_Cache);
	-- dump cache to given location (file),
	-- again second version uses preset path

procedure Suck(Cache : in out Dependency_Cache; From_Path : String);
procedure Suck(Cache : in out Dependency_Cache);
	-- restore cache from dump

-- access to helper parameters
procedure set_Cache_Location(Cache : in out Dependency_Cache; Path : String);
function  get_Cache_Location(Cache : Dependency_Cache) return String;
	-- access to location where cache dump is stored

procedure Set_Tree_Location(Cache : in out Dependency_Cache; Path : String);
function  Get_Tree_Location(Cache : Dependency_Cache) return String;
	-- access to location of textual tree


-- will limit prototype with just these atoms,
-- for real code it is probably worth defining a "Compound_Ebuild" type
-- that would combine Ebuild_Key and list of dependencies.
-- The type should probably reside in this package as well
--
-- correspondingly [Abstract_]Dependency_Cache should gain a few methods
-- dealing with addition/removal of this compound type


-- finally some usefull stuff :)
function Get_Dependency_List(Cache : Dependency_Cache;
	key : Ebuild_Key;       -- do search for this key
	Use_set : Useflag_Set;  -- against this set of useflags
	Direction : DepTraversal_Direction := Direct)
	return Ebuilds.List;
	-- the meat of the type. In fact quite simple (and very standard
	-- digraph traversal) routine.



private

Key_Not_Found : Exception;
	-- internal exception to be used with searches

-- You may want to skip these instantiations until you decide to learn more about ada
-- Basically these lines simply tune BC library (the famous Booch components, Ada variant)
-- to use appropriate types

package AG is new BC.Graphs(Vertex_Item => Ebuild_Key,
      Arc_Item => Useflag_Set,
      Storage => Support.Storage);
package DG is new AG.Directed;

-- we also need to mirror some instantiations for use
-- in traversal procedures, as they take Vertex types
-- instead of items directly
	package DG_EbKeys is new BC.Containers(Item=>DG.Vertex, "="=>DG."=");
	-- Lists
	package DG_EbColl is new DG_EbKeys.Collections;
	package DG_EbUMC  is new DG_EbColl.Unmanaged; -- Unmanaged ebuild collections
	-- Queues
	package DG_EbQueues is new DG_EbKeys.Queues;
	package DG_EbQUM is new DG_EbQueues.Unmanaged;


use Ada.Strings.Unbounded;

type Dependency_Cache is limited record
	-- the "real one" is probably gonna be tagged in addition
	graph : DG.Graph;
	Cache_Path, Tree_Path : Unbounded_String;
	-- pointers to the location of binary cache and portage tree
end record;

-- some private methods

-- elementary manipulations
procedure Add_Key(Cache : in out Dependency_Cache; key : Ebuild_Key;
	Outstanding_Keys : in out Ebuild_Key_Set);
	-- add sole ebuild (just a vertex) and
	-- clear it off the Outstanding_Keys set if it is there
	-- First the graph is checked whether the vertex with such key exists
	-- and if not, new vertex is created

procedure Add_Dependency(Cache : in out Dependency_Cache;
	From_Key, To_Key : Ebuild_Key; Condition : Useflag_Set;
	Outstanding_Keys : in out Ebuild_Key_Set);
	-- add/del dependency (arc) with set of useflags if defined.
	-- From_Key depends on To_Key
	-- if either of the keys is not yet added to the graph, add it
	-- and put it into the Outstanding_Keys set


end Proto_Portage.DB_Cache;
