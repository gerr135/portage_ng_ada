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

with BC.Support.Managed_Storage;
with BC.Containers.Collections.Unmanaged;
with BC.Containers.Sets.Unmanaged;
with BC.Containers.Queues.Unmanaged;
with System.Storage_Pools;

with Ada.Strings.Fixed;
With Ada.Strings.Unbounded;


package Proto_Portage.Support is

-- This package contains instantiations of Booch components and other necessary units
-- as well as helper types and routines

-- You may want to skip these instantiations until you decide to learn more about ada
-- Basically these lines simply set BC library (the famous Booch components, Ada variant)
-- to use appropriate types

	subtype Pool is BC.Support.Managed_Storage.Pool;
	Storage_Pool : Pool (Chunk_Size => 1024);

	Storage : System.Storage_Pools.Root_Storage_Pool'Class
		renames System.Storage_Pools.Root_Storage_Pool'Class (Storage_Pool);

	-- will use BC stuff for internal implementation of sequencers

	-- Ebuilds
	package BCC_EbKeys is new BC.Containers(Item=>Ebuild_Key);
	-- Lists
	package B3C_EbColl is new BCC_EbKeys.Collections;
	package UM_EbColls is new B3C_EbColl.Unmanaged; -- Unmanaged ebuild collections
	-- Sets
	package BC_EbSets is new BCC_EbKeys.Sets;
	function Trivial_Hash(V : Ebuild_Key) return Natural;
	package UM_EbSets is new BC_EbSets.Unmanaged(Hash=>Trivial_Hash,Buckets=>1);
	-- Queues
	package BC_EbQueues is new BCC_EbKeys.Queues;
	package BC_EbQUM is new BC_EbQueues.Unmanaged;

	-- Useflags
	package BCC_Flags is new BC.Containers(Item=>Statefull_Flag);
	-- Lists
	package B3C_FlagColl is new BCC_Flags.Collections;
	package UM_FlagColls is new B3C_FlagColl.Unmanaged;
	-- Sets
	package BC_FlagSets is new BCC_Flags.Sets;
	function Trivial_Hash(V : Statefull_Flag) return Natural;
	package UM_FlagSets is new BC_FlagSets.Unmanaged(Hash=>Trivial_Hash,Buckets=>1);

		-- Unmanaged in BC only means that the package uses global storage
		-- instead of type-specific storage pool.
		-- No need to be extra-fancy with the prototype..

-- some utility routines that did not fit in a particular category
use Ada.Strings;
use Ada.Strings.Unbounded;

function Index(S : String; C : Character; Going : in Direction := Forward) return Natural;
	-- return index of C in S, 0 if not found. S can be a slice (does not have
	-- to start at 1)
	-- The Ada.Strings.Fixed.Index is too heavy weight for what we need - it searches
	-- for patterns and does character mapping on the way.
	--
	-- Also, I don't usually include the in's for the in parameters, but I could not
	-- resist here. It's a pity I could not add "the" in between as well :)

procedure Skip_Whitespace(S : String; Position : in out Positive);
	-- starting at Position scans S until the first non-space/tab character
	-- is encountered. Returns Position pointing to this index (or S'Last+1)

function Get_DEPEND_String(For_the_key : Ebuild_Key) return String;
	-- reads ebuild file and returns contents of DEPEND variabe in a single string

end Proto_Portage.Support;
