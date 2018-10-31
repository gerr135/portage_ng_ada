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


-- This module contains specification and definitions of two generic digraphs
-- static and dynamic.
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
		-- "usefull" info stored at the node
	type key  is private;
		-- key identifying the node
	with function Key_Of(nodeItem :Item) return Key;
	-- ordering items leads to faster access and modifications
	with function "<"(Left,Right:Key)return Boolean is <>;
package Generic_Digraphs is

	Item_Not_Found : Exception;
		-- raised at the attempt to remove a non-existant item

	type Abstract_Digraph is abstract tagged private;

	type Static_Digraph is private;
	-- a linear, ordered sequence of whatever,
	-- imitates dinamic array with first index=1
	-- It might be usefull to make it use an arbitrary discrete index,
	-- but this seems to be an overkill for its intended use

	-- general list manipulation routines
	procedure Clear(TheList : in out List);

	procedure Append(TheList : in out List; key : Item); -- adds the key at the end
	procedure Insert(TheList : in out List; key : Item;
		At_Index : Positive := 1);  -- like python's insert

	procedure Remove(TheList : in out List; At_Index : Positive);
	procedure Remove(TheList : in out List; key : Item;
		Report_Not_Found : Boolean := True);
		-- set Report_Not_Found to false to avoid Item_Not_Found being raised

	function Length(TheList : List) return Natural;
	function Get_At(TheList : List; At_Index : Positive) return Item;
	function Index_Of(key : Item;TheList : List) return Natural;
		-- 0 indicates that key wasn't found
		-- Alternatively this can be made into a procedure
		-- that returns positive parameter and additional Success of type Boolean


private

	-- for the purposes of prototyping BC.Collections.Unmanaged should be appropriate
	-- (they do not need any additional parameters, such as separate storage pool)
	-- but for the real code .Dynamic ones might be wort looking into
	-- (it grows in chunks and provides hashed searches)
	package BCC is new BC.Containers(Item);
	package B3C is new BCC.Collections;
	package UMCollections is new B3C.Unmanaged;

	-- I'll use encapsulation instead of type extension
	-- this way I won't need to provide realisation of some abstract functions
	-- and nothing except declared methods is accessible,
	-- but the type is still controlled!
	type List is record
		umc : UMCollections.Collection;
	end record;


end Generic_Lists;
