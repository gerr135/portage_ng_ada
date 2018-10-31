--**************************************************************************
--   Copyright (C) 2006 by George Shapovalov  <gshapovalov@gmail.com>     --
--                                                                        --
--   This program is free software; you can redistribute it and/or modify --
--   it under the terms of the GNU Library General Public License as      --
--   published by the Free Software Foundation; either version 2 of the   --
--   License, or (at your option) any later version.                      --
--                                                                        --
--   This program is distributed in the hope that it will be useful,      --
--   but WITHOUT ANY WARRANTY; without even the implied warranty of       --
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        --
--   GNU General Public License for more details.                         --
--                                                                        --
--   You should have received a copy of the GNU Library General Public    --
--   License along with this program; if not, write to the                --
--   Free Software Foundation, Inc.,                                      --
--   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.            --
--**************************************************************************

--  A simplistic utility to list the dependencies.
--  Basically a frontend for the prototyping lib.

with GNAT.Command_Line;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;

with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Portage.UseFlags.IO;    use Portage.UseFlags;
with Portage.DepGraphs;      use Portage.DepGraphs;
with Generic_Graphs.Dynamic; use Portage;

procedure List_Deps is

    procedure printUsage is
        use Ada.Text_IO;
    begin
        Put_Line ("This program reads the generated 'ebuilds', creates cache");
        Put_Line ("and then reports list of dependencies either way (as requested).");
        Put_Line ("If no cache found it creates it. As this is just prototyping utility,");
        Put_Line ("I did not implement mtime checkes, so run it with '-f' option to update cache!");
        New_Line;
        Put_Line ("usage:");
        Put_Line ("   " & Ada.Command_Line.Command_Name & " [options]  name");
        Put_Line ("report list of dependencies (in right order) for name (singular)");
        New_Line;
        Put_Line ("options:");
        --  only short options for now
        Put_Line ("-h      print this help");
        Put_Line ("-g      turn on debug output (if any)");
        Put_Line ("-f      regenerate cache");
        Put_Line ("-r      track reverse dependencies (of full set, there are no 'installed' packages,");
        Put_Line ("         this is just a prototype!)");
    end printUsage;

    Finish : exception;  -- normal termination
    --  Ada doesn't have an analog of sys.exit() in the standard,
    --  mostly because of multiprocess(/thread) awareness - there are certain
    --  issues with process flow control...
    --  This exception is a way to get similar result in a standard-compliant
    --  and safe way.
    --
    --  If you really miss this function, OS_Exit/OS_Abort are defined in
    --  GNAT.OS_Lib (one of gnat extensions).  In addition that module defines few basic
    --  glibc bindings. However if you start using it a lot, DO NOT use Spawn and other
    --  process-controll functions!!
    --  Ada itself (i.e. the standard) provides a much better and flexible mechanism of
    --  dealing with multiprocess stuff.

    type ParamRec is record
		--  mostly the commandline params. DepGraph and USE flags will go as separate vars
        PkgName : Unbounded_String := Null_Unbounded_String;
        WorkDir : Unbounded_String := Null_Unbounded_String;
        --  directory where textual tree and the cache are stored
        --  gets assigned to 'pwd' if env var PORTAGE_DIR was not set
        --  should not contain the trailing '/'
        DepResolution    : Traversal_Direction := Direct;
        Regenerate_Cache : Boolean             := False;
        Debug            : Boolean             := False;
    end record;

    procedure processCommandLine (params : in out ParamRec) is
        use GNAT.Command_Line;
        use Ada.Command_Line;
        use Ada.Text_IO;
        Options : constant String := "g h f r";
    --  this works very similarly to GNU getopt, except this one uses single '-'
    --  for both short and long options, but lets not worry about it right now
    begin
        if Argument_Count < 1 then
            printUsage;
            raise Finish;
        end if;
        begin -- need to process local exceptions
            loop
                case Getopt (Options) is
                    when ASCII.NUL =>
                        exit;
                    --  end of option list

                    when 'g' =>
                        params.Debug := True;
                    when 'h' =>
                        printUsage;
                        raise Finish;

                    when 'f' =>
                        params.Regenerate_Cache := True;
                    when 'r' =>
                        params.DepResolution := Reversed;

                    when others =>
                        raise Program_Error;         -- should not get here!
                        --  basically serves to catch "damn, forgot to include that option
                        --  here"
                end case;
            end loop;
        exception
            when Invalid_Switch =>
                Put_Line ("Invalid Switch " & Full_Switch);
                raise Finish;
            when Invalid_Parameter =>
                Put_Line ("No parameter for " & Full_Switch);
                raise Finish;
            when Data_Error =>
                Put_Line ("Invalid numeric format for switch" & Full_Switch);
                raise Finish;
        end;
        params.PkgName := To_Unbounded_String (Get_Argument (Do_Expansion => True));
        --
        --  Set WorkDir
        declare -- just a visibility wrapper
            use Ada.Directories;
        begin
            if Ada.Environment_Variables.Exists (PortageDir) then
                params.WorkDir :=
                    To_Unbounded_String (Ada.Environment_Variables.Value (PortageDir));
            else
                params.WorkDir := To_Unbounded_String (Current_Directory);
            end if;
            --
            if not Exists (To_String (params.WorkDir))
               or else Kind (To_String (params.WorkDir)) /= Directory
            then
                Put_Line
                   ("The directory " & To_String (params.WorkDir) & " does not exist!");
                raise Finish;
            end if;
        end;
    end processCommandLine;

    params : ParamRec;
    UFlags : UseFlag_Set;

    package Dynamic_Graphs is new Graphs.Dynamic (DepGraph);
    type Dynamic_DepGraph is new Dynamic_Graphs.Dynamic_Graph with null record;
    PortageDeps : Dynamic_DepGraph;
	--  the whole tree, with conditionals (use flags, etc)

begin  -- main
    processCommandLine (params);
    UseFlags.IO.CollectFlags (UFlags);
    SetCahceLocation (PortageDeps,  -- view conversion
       Cache_Location => To_String (params.WorkDir));
    --
    if params.Regenerate_Cache
       or else -- order is significant here
       Cache_is_Empty (PortageDeps)
    then
        Rebuild (PortageDeps);
        Store (PortageDeps);
    else
        Load (PortageDeps);
    end if;
    --
    declare
        package Dynamic_Trees is new Graphs.Dynamic (DepTree);
        type Dynamic_DepTree is new Dynamic_Trees.Dynamic_Graph with null record;
        PackageDeps : Dynamic_DepTree;
    --  The tree of deps for the package, after resolution
    --  USE flags are already taken care of
    begin
        FindDeps
           (PortageDeps,
            UFlags,
            forPackage => Create (To_String (params.PkgName)),
            direction  => params.DepResolution,
            deps       => PackageDeps);
        Print (PackageDeps);
    end;
end List_Deps;
