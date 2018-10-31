with Ada.Directories;
with Ada.Text_IO;

package body Portage.DepGraphs is

    ------------------------------------------------------------------
    --  DepTree type

    function Spit_List (tree : DepTree;
        Algorithm : Traversal_Algorithm := DFS) return Ebuild_List is
		--
		EList : Ebuild_List;
    begin
        raise Not_Implemented;
        return EList;
    end Spit_List;

    ------------------------------------------------------------------
    --  DepGraph  type

    procedure Load (graph : out DepGraph'Class; Cache_Location : in String := "") is
    begin
        raise Not_Implemented;
    end Load;

    procedure Store (graph : in DepGraph'Class; Cache_Location : in String := "") is
    begin
        raise Not_Implemented;
    end Store;

    function Cache_is_Empty (graph : DepGraph) return Boolean is
        use Ada.Directories;
    begin
        return not Exists (To_String (graph.Cache_Location));
        --  FIXME
        --  Add some cache validity checking here
    end Cache_is_Empty;

    procedure SetCahceLocation
       (graph          : in out DepGraph;
        Cache_Location : in String := PortageDir & "/" & Cache_Name)
    is
    begin
        graph.Cache_Location := To_Unbounded_String (Cache_Location);
    end SetCahceLocation;

    --------------------------------------------------------
    --  Build method  - build DepGraph from "scratch"
    --
    --  The general idea is to read ebuilds one-by-one
    --  and add them to the graph as nodes.
    --  For every node (ebuild) get the list of deps, then,
    --  for every dep, check if it already exists in graph, if not - add its key,
    --  and then add the corresponding arc setting conditions (USE requirements) properly

    procedure Build
       (graph         : out DepGraph'Class;
        Tree_location : in String := PortageDir)
    is
        use Ada.Directories;
        -- 		procedure processEbuild(ebd : in Directory_Entry_Type) is
        -- 		  begin
        -- 			raise Not_Implemented;
        -- 		  end;

        EbuildSearch : Search_Type;
        ebdEntry     : Directory_Entry_Type;
        ebd          : Ebuild;

    begin  -- Build
        --  a simple Ada.Directories.Search invocation would do here, like below
        --  too bad it is not yet implemented (but it is in the standard already)
        -- 		Ada.Directories.Search(Directory => Tree_location,
        -- 			Pattern => Ebuild_Pattern,
        -- 			Filter  => (Ordinary_File => True, others => False),
        -- 			Process => processEbuild);
        --  So, we'll have to settle for an active iteration
        Start_Search
           (EbuildSearch,
            Directory => Tree_location,
            Pattern   => Ebuild_Name_Pattern,
            Filter    => (Ordinary_File => True, others => False));
        --
        while More_Entries (EbuildSearch) loop
            Get_Next_Entry (EbuildSearch, Directory_Entry => ebdEntry);
            ebd := ReadFromFile (Full_Name (ebdEntry));
            graph.AddEbuild (ebd);
        end loop;
        --
        End_Search (EbuildSearch);
        if not Is_Empty (graph.Missing_Ebuilds) then
            raise Inconsistent_Tree;
        end if;
    end Build;

    procedure Rebuild
       (graph         : in out DepGraph'Class;
        Tree_location : in String := PortageDir)
    is
        --
        use Ada.Directories;
        use type Ada.Calendar.Time;

        EbuildSearch : Search_Type;
        ebdEntry     : Directory_Entry_Type;
        ebd          : Ebuild;

    begin  -- Rebuild
        Start_Search (EbuildSearch,
            Directory => Tree_location,
            Pattern   => Ebuild_Name_Pattern,
            Filter    => (Ordinary_File => True, others => False));
        --
        while More_Entries (EbuildSearch) loop
            Get_Next_Entry (EbuildSearch, Directory_Entry => ebdEntry);
            if Modification_Time (ebdEntry) > graph.Timestamp then
                ebd := ReadFromFile (Full_Name (ebdEntry));
                graph.AddEbuild (ebd);
            end if;
        end loop;
        --
        End_Search (EbuildSearch);
        if not Is_Empty (graph.Missing_Ebuilds) then
            raise Inconsistent_Tree;
        end if;
    end Rebuild;

    procedure AddEbuild (graph : in out DepGraph; ebd : in Ebuild) is
    begin
        --  Since the primitive ops are implemented downstream
        --  we need to redispatch.
        --  (nice to have Ada here and use asbtract methods where proper,
        --  as this thing could have been nasty to track, have it not being caught
        --  at compile time :))
        --  Or, rather, should this method be made class-wide?
        DepGraph'Class (graph).AddNode (ebd.Key, Clear_Arcs => True);
        --  We are adding the ebuild anew - all the deps are specified explicitly.
        --  Since this method may be called by already existing graph,
        --  we need to clear first.
        Remove (graph.Missing_Ebuilds, ebd.Key);
        --
        for i in 1 .. ebd.NDeps loop
            declare
                dep  : Dependency_Type := ebd.Dependency (i);
                depV : Ebuild_Key      := Vertex (dep);
            begin
                if not DepGraph'Class (graph).HasNode (depV) then
                    Add (graph.Missing_Ebuilds, depV);
                end if;
                DepGraph'Class (graph).AddNode (depV);
                DepGraph'Class (graph).AddArc
                   (From    => ebd.Key,
                    To      => depV,
                    arcLoad => Condition (dep));
            end;
        end loop;
    end AddEbuild;

    procedure FindDeps (graph      : in DepGraph'Class;
        Use_Flags  : in UseFlags.UseFlag_Set;
        forPackage : in Ebuild_Key;
        direction  : in Traversal_Direction;
        deps       : in out DepTree'Class)
    is
    begin
        raise Not_Implemented;
    end FindDeps;

    procedure Print (deps      : in DepTree'Class;
        F         : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
        Style     : in DepTree_Print_Style   := List;
        Algorithm : in Traversal_Algorithm   := DFS)
    is
    begin
        case Style is
            when List =>
                --  we simply get a list of nodes (ebuilds) and call its print method
                declare
                    EList : Ebuild_List := deps.Spit_List (Algorithm);
                begin
                    Print (EList, F);
                end;
            --
            when Tree =>
                raise Not_Implemented;
        end case;
    end Print;

    function Timestamp (graph : DepGraph) return Ada.Calendar.Time is
    begin
        return graph.tStamp;
    end Timestamp;

end Portage.DepGraphs;
