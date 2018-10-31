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


-- This is a generic specification of a linear sequence (think dynamic array)
-- abstraction, a.la python's list
-- Holds only one kind of item!

-- For the prototyping purposes I directly use Booch components' Collection,
-- but having this abstraction here will allow to painlessly change implementation
-- at any point in future.

with BC.Containers.Collections.Unmanaged;

generic
	type Item is private;
	-- plug it pretty much anything here
package Generic_Lists is

	Item_Not_Found : Exception;
		-- raised at the attempt to remove a non-existant item

	type List is private;
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
