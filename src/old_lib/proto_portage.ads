-- !!!THIS IS A PROTOTYPE CODE ONLY!!!
-- !!Do not use or expand it!!
-- It is gonna be rewritten!
-- (well, unless it turns out to be the most efficient implementation,
-- which is unlikely)
-- besides there isn't much here


-- Copyright (c) 2006 George Shapovalov <george@gentoo.org>.  All rights reserved.

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

with Ada.Strings.Unbounded;

package Proto_Portage is

-- For easier handling of the structure lets separate types.
-- I'll define the "basic" one top level and keep composite
-- in the corresponding modules

-- Also as you might have noticed, I am using private types throughout.
-- This is done first of all for illustration purposes,
-- however this have saved me a bit of work already as at few occasions
-- I realised that my original data representation "doesn't cut it" and
-- had to redo some structures. Should I mention that I didn't have
-- to change *any* of the downstream code (that is the code using
-- these structures)?


type Ebuild_Key is private;
	-- Only rarely an access to full ebuild contents is needed
	-- the "_Key" part of the name is there to emphasize this.
	-- this is basically a habdle that will sit in the vertices of the graph.

function Create_Key(Name : String)  return Ebuild_Key;
function Get_Name(Key : Ebuild_Key) return String;
--function "="  (Left, Right : in Ebuild_Key) return Boolean;

type Useflag is private;

function Create_Flag(Name : String) return Useflag;
function Get_Name (Flag : Useflag) return String;


type Statefull_Flag is private;
	-- combined flag with its state (enabled/disabled)
	-- will use for tracking "-use" flags


type DepTraversal_Direction is (Direct, Reversed);
	-- indication of dependency traversal direction
	-- Note: "reverse" is a reserved word


private

-- And the best part is I don't actually need to do much to implement this!

-- lets not be fancy and just use actual names as keys for this implementation
-- Actual code may use some optimizations, as described in design document

use Ada.Strings.Unbounded;
-- actually this may turn out to be space efficient as well,
-- since Unbounded strings are reference-counted and controlled


type Ebuild_Key is record
	key : Unbounded_String;
end record;

type Useflag is record
	flag : Unbounded_String;
end record;

-- Yes, they are identical structures, but they represent different entities
-- and we do not want to confuse them, do we?
--
-- Also, I might have saved some work by creating a common record with
-- Create and Get_name functions and defining these types with identical
-- interface in private part above (here I would just have to derive them
-- from that common record without redefining the methods),
-- but I chose to differentiate methods for readability.


type Statefull_Flag is record
	flag    : Useflag;
	Enabled : Boolean := True;
end record;


end Proto_Portage;
