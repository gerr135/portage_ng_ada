--
-- This is the "main" unit of prototyping utility
-- It performs dependency tracking (both ways)
--
-- As this is a first file likely to be looked at
-- I'll put some random comments in as I go..


-- with is analogous to Python's import
-- use adds previously withed stuff to local namespace
-- correspondingly use may be local
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada;
with GNAT.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.IO_Aux;
with GNAT.OS_Lib;
with GNAT.Directory_Operations;
with GNAT.Exception_Traces; use GNAT.Exception_Traces;

with Proto_Portage.Sequencers;
use Proto_Portage; use Proto_Portage.Sequencers;
with Proto_Portage.DB_Cache; use Proto_Portage.DB_Cache;

with Get_Environment;
	-- this is just a function, not a package;
	-- really just a (thick) binding to glibc's getenv
	-- Code taken directly from Lovelace online tutorial..

procedure Track_Deps is


Finish : Exception;
	-- immediate (normal) termination
	-- Ada doesn't have an analog of sys.exit() in the standard,
	-- mostly because of multiprocess(/thread) awareness - there are certain
	-- issues with process flow control...
	-- This exception is a way to get similar result in a standard-compliant
	-- and safe way.
	--
	-- OS_Exit/OS_Abort are defined in GNAT.OS_Lib (one of gnat extensions)
	-- if you really miss this function.
	-- In addition that module defines few basic glibc bindings. However if you
	-- start using it a lot, DO NOT use Spawn and other process-controll functions!!
	-- Ada itself (i.e. standard) provides a much more nice and flexible mechanism of
	-- dealing with multiprocess stuff.

	procedure printUsage is
		-- could be a local procedure (of processCommandLine),
		-- but this way I don't need to write a separate description,
		-- as it is quite close to the top :)
	begin
		Put_Line("This program reads the generated 'ebuilds', creates cache");
		Put_Line("and then reports list of dependencies either way (as requested).");
		Put_Line("If no cache found it creates it. As this is just prototyping utility,");
		Put_Line("I did not implement mtime checkes, so run it with '-f' option to update cache!");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options]  name");
		Put_Line("report list of dependencies (in right order) for name (singular)");
		New_Line;
		Put_Line("options:");
		-- only short options for now
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output (if any)");
		Put_Line("-f      regenerate cache");
		Put_Line("-r      track reverse dependencies (of full set, there are no 'installed' packages,");
		Put_Line("         this is just a prototype!)");
	end printUsage;


--------------------
-- Meaningfull stuff

type ComLineParamRec is record
	DoRegen : Boolean := False;
	DepTracking_Direction : DepTraversal_Direction := Direct;

	PkgName : Unbounded_String := Null_Unbounded_String;
		-- name for which to do tracking

	UseFlags : Useflag_Set;
		-- useflags set in environment

	WorkDir : Unbounded_String := Null_Unbounded_String;
		-- directory where textual tree and the cache are stored
		-- gets assigned to 'pwd' if env var PORTAGE_DIR was not set
		-- should not contain the trailing '/'

	CacheName : Unbounded_String := To_Unbounded_String(".db_cache");
		-- name of the file where to keep cache

	Debug : Boolean := False;
end record;

PortageDir_EnvName : constant String := "PORTAGE_DIR";
USE_EnvName        : constant String := "USE";
	-- should be made variable in real implementation perhaps..



	procedure processCommandLine(params : in out ComLineParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		Options : constant String := "g h f r";
		-- this works very similarly to GNU getopt, except this one uses single '-'
		-- for both short and long options, but lets not worry about it right now
	begin
		if Argument_Count < 1 then
			PrintUsage;
			raise Finish;
		end if;
		begin -- need to process local exceptions
			loop
			case Getopt (Options) is
				when ASCII.NUL => exit;
					-- end of option list

				when 'g' => Params.Debug := True;
				when 'h' => printUsage;

				when 'f' => params.DoRegen := True;
				when 'r' => params.DepTracking_Direction := Reversed;

				when others =>
					raise Program_Error;         -- should not get here!
					-- basically serves to catch "damn, forgot to include that option here"
			end case;
			end loop;
		exception
			when Invalid_Switch =>
				Put_Line ("Invalid Switch " & Full_Switch);raise Finish;
			when Invalid_Parameter =>
				Put_Line ("No parameter for " & Full_Switch);raise Finish;
			when Data_Error =>
				Put_Line ("Invalid numeric format for switch" & Full_Switch);raise Finish;
		end;

		params.pkgName := To_Unbounded_String(Get_Argument(Do_Expansion => True));
	end processCommandLine;


	procedure processEnvironment(params : in out ComLineParamRec) is
		use GNAT.OS_Lib; use GNAT.Directory_Operations;
	begin
		-- need to fill in UseFlags and WorkDir
		params.WorkDir := To_Unbounded_String(Get_Environment(PortageDir_EnvName));
		if params.WorkDir = "" then
			params.WorkDir := To_Unbounded_String(Get_Current_Dir);
		elsif not Is_Directory(To_String(params.WorkDir)) then
			Put_Line("The directory pointed to by "& PortageDir_EnvName & " does not exist!");
			raise Finish;
		end if;
		Append_From_USE(Use_Set=>params.Useflags, From_String=>Get_Environment(USE_EnvName));
	end;



-------------------------------

	procedure Create_DepCache(DepCache : out Dependency_Cache;
			params : ComLineParamRec) is
	begin
		-- attempt to read cache dump will generate exception if
		-- either cache does not exist or it is broken
		Suck(DepCache,
			From_Path => To_String(params.WorkDir & '/' & params.CacheName));
	exception
		when Invalid_Path | Broken_Cache =>
			-- cache is problemmatic, rebuild from text tree
			Regen_From_TextTree(DepCache,
				Tree_Location =>  To_String(params.WorkDir),
				Debug => params.Debug);
			-- and store the newbuilt info
			Dump(DepCache);
				-- To_Path was set by Suck, no need to repeat it here
	end;

	procedure Print_KeyList(TheList : Ebuilds.List; PkgName : String;
			params : ComLineParamRec) is
		use Ebuilds; use Ada.Integer_Text_IO;
	begin
		-- just print the names one per line
		Put("list of dependencies for "& PkgName);
		if params.DepTracking_Direction = Reversed then
			Put(" (reverse direction)");
		end if;
		Put_Line(":");

		for i in 1 .. Length(TheList) loop
			Put("#");Put(i, Width=>1);Put(":  ");
			Put_Line(Get_Name(Get_At(TheList,i)));
		end loop;
	end;

-------------------------------
-- "Main" block


params : ComLineParamRec;
depCache : Dependency_Cache;

begin
	Trace_On(Kind => Unhandled_Raise);

	if params.Debug then
		Put_Line("Starting...");
	end if;

	processCommandLine(params);
	processEnvironment(params);
		-- get useflags, working directory and other misc stuff

	if params.Debug then
		Put_Line("Processed command line and environment.");
	end if;

	Create_DepCache(DepCache, params);

	if params.Debug then
		Put_Line("Created cache.");
	end if;


	declare
		Dep_List : Ebuilds.List := Get_Dependency_List(
			Cache => DepCache,
			key=>Create_Key(To_String(params.PkgName)),
			Use_Set => params.UseFlags,
			Direction => params.DepTracking_Direction );
	begin
		Print_KeyList(TheList=>Dep_List, PkgName=>To_String(params.PkgName),
			params=>params);
	end;

exception
	when Finish => null;
end Track_Deps;
