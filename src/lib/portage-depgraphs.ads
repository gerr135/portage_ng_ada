--
--  Ada Spec: Portage.DepGraphs
--
--  Description: Dependency tracking types. This one contains the abstract top-level
--   with concrete realizations in child subunits.
--
--
--  Author: George Shapovalov <george@gentoo.org>, (C) 2006-2008
--
--  Copyright: See COPYING file that comes with this distribution
--

with Generic_Graphs;

with Portage.Ebuilds; use Portage.Ebuilds;
with Portage.UseFlags;

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
--  with Ada.Finalization;

package Portage.DepGraphs is

    ----------------------------------
    --  The purpose of this module is to provide a common interface to data and methods
    --  responsible for storing dependency info and resolving enquiries without going
    --  into details on how the data is stored.
    --    The approach is to have a "top-level" interface that only defines
    --  graph manipulation routines and then have necessary graph-like types
    --  that would only provide the algorithms described in terms of GeneralGraph
    --  iterface's primitive operations. The final types shall be constructed via
    --  generic packages providing implementations. See Ada Rationale 2005,
    --  part 1 - OOP, p17 for enlightment :).

    --  NOTE  Can the interface be made private? The representation childs will
    --  have to be private too. Will it be possible to provide visible "final" types
    --  then?

    package Graphs is new Generic_Graphs (
        Node_Type => Ebuild_Key,
        Arc_Type  => UseFlags.UseFlag_Set);

    --  type to pass selection of algorithm to use when traversing tree/graph
    type Traversal_Algorithm is (
        BFS,   -- Breadth First search
        DFS);  -- Depth FIrst search
    --  DFS is preferred for output (as it groups related packages better)
    --  BFS is preferred for actual dep resolution, as it fares better
    --   wrt incomplete dependencies.

    -------------------------------
    --  The "subtree" - this one is a real tree, not a graph
    --  The intended use is to hold the resolved dependencies.
    --  No cache, no building from portage tree, is supposed to be valid
    --  can be passed around (so, non-limited)
    type DepTree is abstract new Graphs.Graph_Type with private;

    --  A quick routine to list contents of the tree
    --  The resulting list only has naming information - the inserted ebuilds
    --  do not include deps, etc.
    function Spit_List
       (tree      : DepTree; -- 'Class?
        Algorithm : Traversal_Algorithm := DFS)
        return      Ebuild_List;

    type DepTree_Print_Style is (List, Tree);
    --  could use boolean for just these, but this way it is easier to change
    --  should more print styles be desired

    --  print the deps on sreen in a specified form
    procedure Print
       (deps      : in DepTree'Class;
        F         : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
        Style     : in DepTree_Print_Style   := List;
        Algorithm : in Traversal_Algorithm   := DFS);

    --------------------------------------------
    --  This is an abstract type -
    --  will contain the principal algorithms and will define necessary
    --  primitive operations as abstract (and mostly private).
    --  These will need to be overridden.
    --  No objects of this type can be declared. Use realizations defined
    --  in child units.

    type DepGraph is abstract new Graphs.Graph_Type with private;
    --  should still be limited

    --  Cache IO
    procedure Load  (graph : out DepGraph'Class; Cache_Location : in String := "");
    procedure Store (graph : in DepGraph'Class; Cache_Location : in String := "");
    --  if Cache_Location is not specified Load/Store is performed
    --  from/to internally kept location (switchable with SetCahceLocation)

    function Cache_is_Empty (graph : DepGraph) return Boolean;

    procedure SetCahceLocation
       (graph          : in out DepGraph; -- !NOTE! not class-wide (nor abstract)
        Cache_Location : in String := PortageDir & "/" & Cache_Name);

    function Timestamp (graph : DepGraph) return Ada.Calendar.Time;

    -----------------------
    --  Common algorithms
    --
    --   Using class-wide graph, to allow dynamic polimorphism

    --  Tree ops

    --  clear and then build depgraph anew
    procedure Build
       (graph         : out DepGraph'Class;
        Tree_location : in String := PortageDir);

    --  expand/update depgraph with the changes since the internal timestamp
    procedure Rebuild
       (graph         : in out DepGraph'Class;
        Tree_location : in String := PortageDir);

    --  above two rely on the following methods

    --  Adds "the whole" ebuild to the graph:
    --  First the corresponding Ebuild_Key is added, then for every dependency
    --  its key is checked and created if not yet in, then the corresponding arc is
    --  added..
    --
    --  To add just the Ebuild_Key use AddNode inherited from the interface
    procedure AddEbuild (graph : in out DepGraph; ebd : in Ebuild);

    --  The "tree walker" - principal algorithm
    --  If multiple to be tested should either make this a wrapper
    --  (and add a selection parameter), or add other FindDeps methods.
    procedure FindDeps
       (graph      : in DepGraph'Class;
        Use_Flags  : in UseFlags.UseFlag_Set;
        forPackage : in Ebuild_Key;
        direction  : in Traversal_Direction;
        deps       : in out DepTree'Class); -- 'Class?

private

    use Ada.Strings.Unbounded;

    type DepTree  is abstract new Graphs.Graph_Type with null record;

    type DepGraph is abstract new Graphs.Graph_Type with record
        tStamp : Ada.Calendar.Time;
        --  set by [Re]build to Clock (that is "now")
        Cache_Location : Unbounded_String;
        --
        Missing_Ebuilds : EbuildKey_List;
        --  an internal field used to track ebuilds that were added by being
        --  dependency of something, but not (yet) encountered "for real".
        --  The list is populated by (Re)Build method and then is kept upon return.
        --  Thus (Re)Build methods have a side-effect. However returning this
        --  explicitly looks ugly and the list may be needed for error reporting, etc.
        --  Thus a field is necessary anyway.
    end record;

    --  Inline pragmas need to be in the spec to have an effect
    pragma Inline (Timestamp);

end Portage.DepGraphs;
