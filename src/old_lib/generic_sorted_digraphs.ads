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


-- This package hierarchy defines few digraphs of a "special kind"
-- They are based around the collection of vertices which are unique,
-- and thus, rather than defining a container type Vertex all operations are performed
-- directly on vertex contents (Item type in generic header).
-- Thus, effectively, all vertices are "unique" and the whole structure has a more monolitic
-- interface.
-- Additionally the notion of the "key" is introduced, making vertices searchable/identifyible
-- by the key.
-- For the purpose of efficiency internal representations keep vertices in order, sorted
-- by the key. However it is possible to extend this to the general, non-sorted situation
-- if needed.
--
--
-- Two representations are provided in child packages
--
-- Static: "sorted comb" representation, consists of
-- ordered list of nodes, indexed by "ID's" (may just use node number as ID)
-- each node also has a starting index and Ndeps for incoming and outgoing deps,
-- pointing into array of ID's
--  Features:
-- Easy and quick IO on stream
-- Hard to add and very hard to remove nodes
--
-- Dynamic: "traditional" approach
-- ordered list of nodes (may export ID's in private part), or AVL tree (then, correspondingly, no ID's)
-- every node has lists of incoming/outgoing deps, as pointers or, if ID's are used, may be as ID's
--  Features:
-- faster node access by key as well as additions (and deletions)
-- more involved IO on streams


generic
	type Item is private;
	with function "<"(Left,Right:Item)return Boolean is <>;
package Generic_Sorted_Digraphs is

	Item_Not_Found : Exception;
		-- raised at the attempt to access or manipulate a non-existant item

	type Abstract_Sodigraph is abstract new Controlled with private;
		-- Sodi = Sorted_Directed

	procedure Clear(Graph : in out Abstract_Sodigraph) is abstract;

	procedure AddVertex(Graph : in out Abstract_Sodigraph; VItem : Item) is abstract;
	procedure DelVertex(Graph : in out Abstract_Sodigraph; VItem : Item) is abstract;
		-- construct and add vertex
		-- The concept is that


private


end Generic_Lists;
