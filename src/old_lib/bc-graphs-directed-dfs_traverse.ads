-- Ada Spec: bc-containers-trees-multiway-create_from_digraph
--
-- Description: an additional utility for Booch components
-- Traverses digraph starting at given vertex following
-- DFS (Depth-First search) algorithm.
-- Every vertex is visited exactly once.
--
--
-- Author: George Shapovalov <george@gentoo.org>, (C) 2003
--
-- Copyright: See COPYING file that comes with this distribution

with BC.Graphs.Directed;
with BC.Containers.Collections;

generic
	with procedure Apply(VElem : in Abstract_Vertex'Class;OK : out Boolean);
		-- the action on visited vertex
		-- Just as with other BC iterations there is Ok parameter:
		-- The iteration will terminate early if Apply sets OK to False.

	with function Arc_Valid(AElem : in Abstract_Arc'Class) return Boolean;
		-- a hook to support conditional arcs.
		-- True = arc is valid and is followed, otherwise skipped

	-- we need some temporary storage, to keep visited vertices
	-- the most efficient way seems to be the most generic one also
	-- i.e. to use set component from BC
	with package BCC is new BC.Containers(Item=>Vertex);
	with package B3C is new BCC.Collections;
procedure BC.Graphs.Directed.DFS_Traverse (G : Graph ; V : Vertex;
	Visited : in out B3C.Abstract_Collection'Class );

-- Visited is used for inner tracking of visited vertices, but is also
-- exposed so that user has easy access to the traversal order
