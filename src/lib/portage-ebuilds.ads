--
--  Ada Spec: Portage.Ebuilds
--
--  Description: Module representing ebuilds.
--
--
--  Author: Geprge Shapovalov <george@gentoo.org>, (C) 2006-2008
--
--  Copyright: See COPYING file that comes with this distribution
--
--

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Text_IO;

--  limited with Portage.DepGraphs;
with Portage.UseFlags;

package Portage.Ebuilds is

    ---------------------------------------------------------------
    --  A singular dependency type - will represent dependency info given in ebuilds
    --  Will also serve as an arc type for the digraphs representing portage tree
    --
    --  Consists (principally) of the dependency key (the package) and the "condition"
    --  representing the relevant USE flag requirements.
    --  Condition is matched when all flags relevant to the given package match
    --  their states. Only On/Off states are matched.
    --
    --  The Ebuild type is unlikely to be modified, so no tagging is necessary, at least
    --  at present. However taggind will allow me to use dot notation, improving
    --  readability. Taggind Dependency_Type in addition seems to compilcate things
    --  though.

    type Dependency_Type is private;

    function Create
       (key       : Ebuild_Key;
        condition : UseFlags.UseFlag_Set)
        return      Dependency_Type;

    function Vertex    (dep : Dependency_Type) return Ebuild_Key;
    function Condition (dep : Dependency_Type) return UseFlags.UseFlag_Set;


	type    Dep_Count is new Natural;
	subtype Dep_Index is Dep_Count range 1 .. Dep_Count'Last;

    ----------------------------------------

    type Ebuild is tagged private;
    --  Groups relevant info from the ebuild file

    function ReadFromFile (FileName : String) return Ebuild;

    function Name (ebd : Ebuild) return String;
    function Key  (ebd : Ebuild) return Ebuild_Key;

    --  dependencies are numbered from 1 to NDeps
    --  NDeps = 0 means no deps..
    function NDeps      (ebd : Ebuild) return Dep_Count;
    function Dependency (ebd : Ebuild; i : Dep_Index) return Dependency_Type;

    Empty_Ebuild : constant Ebuild;


    ---------------------------------------------

    type Ebuild_List is tagged private;
    --  unlikely to be extended. Tagged mostly for using a (more readable) dot notation.

    procedure Clear    (EList : in out Ebuild_List);
    function  Is_Empty (EList : Ebuild_List) return Boolean;

    procedure Add (EList : in out Ebuild_List; ebd : in Ebuild'Class);
    --  As with Useflags or the graph - there can be only one!
    --  If ebuild is on the list already it replaces the one already there.

    function GetEbuild (EList : Ebuild_List; key : Ebuild_Key) return Ebuild'Class;

    function HasEbuild (EList : Ebuild_List; key : Ebuild_Key) return Boolean;

    procedure Print
       (EList : in Ebuild_List;
        F     : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);

	Empty_Ebuild_List : constant Ebuild_List;


private

    type Dependency_Type is record
        key       : Ebuild_Key;
        condition : UseFlags.UseFlag_Set;
    end record;

    package DepVectors is new Ada.Containers.Vectors (
        Index_Type => Positive,
        Element_Type => Dependency_Type);

    type DepList is record
        v : DepVectors.Vector;
    end record;

    type Ebuild is tagged record
        Name : Unbounded_String;
        deps : DepList;
    end record;
    --  or should this be abstracted even more? -
    --  Looks like vector may not be the optimal representation, so I may need to
    --  do the same dance as for the graphs.

    function "=" (Left, Right : Ebuild) return Boolean;
    --  Like with the flags, only the name matters.
    --  NOTE 1: This method *has* to be defined, otherwise matching is done
    --  on default "="'s of the fields, which does not make sense here.
    --  NOTE 2: This method should be defined before instantiating EbuildVectors.
    --  (as that one implicitly uses the predefined equality)

    Empty_Ebuild : constant Ebuild :=
       (Name => Null_Unbounded_String,
        deps => (v => DepVectors.Empty_Vector));

    package EbuildVectors is new Ada.Containers.Vectors (
        Index_Type => Positive,
        Element_Type => Ebuild);

    type Ebuild_List is tagged record
        list : EbuildVectors.Vector;
    end record;

	Empty_Ebuild_List : constant Ebuild_List :=
		(list => EbuildVectors.Empty_Vector);

end Portage.Ebuilds;
