-- Ada Spec: bc-containers-trees-multiway-create_from_digraph
--
-- Description: an additional utility for Booch components
-- Traverses digraph starting at given vertex following
-- BFS (Breadth-First search) algorithm.
-- Every vertex is visited exactly once.
--
--
-- Author: George Shapovalov <george@gentoo.org>, (C) 2003
--
-- Copyright: See COPYING file that comes with this distribution

with BC.Graphs.Directed;
with BC.Containers.Collections;
with BC.Containers.Queues;

generic
	with procedure Apply(VElem : in Abstract_Vertex'Class;OK : out Boolean);
		-- the action on visited vertex

	with function Arc_Valid(AElem : in Abstract_Arc'Class) return Boolean;
		-- a hook to support conditional arcs.
		-- True = arc is valid and is followed, otherwise skipped

	-- we need some temporary storage, to keep visited vertices
	-- the most efficient way seems to be the most generic one also
	-- i.e. to use set component from BC
	with package BCC is new BC.Containers(Item=>Vertex);
	with package B3C is new BCC.Collections;

	-- for the Breadth-First search we also need a quiue for internal tracking.
	-- It is not meaningful outside, but in order to allow the most generic
	-- implementation - i.e. to choose whether to use bounded/unmanaged or say
	-- dynamic quiues, I'll expose the variable to which the user can pass
	-- the chosen type
	with package BCQ is new BCC.Queues;
procedure BC.Graphs.Directed.BFS_Traverse (G : Graph ; V : Vertex;
	Visited : in out B3C.Abstract_Collection'Class;
	Storage_Queue : in out BCQ.Abstract_Queue'Class;
	Visit_Incoming : Boolean := False );

-- Visited is used for inner tracking of visited vertices, but is also
-- exposed so that user has easy access to the traversal order
--
-- Visit_Incoming indicated whether to traverse on the incoming
-- arcs (in reverse direction) instead of outgoing.
