--
-- This simple utility just generates bunch of files, just listing dependencies
-- to imitate portage tree.
-- In fact there is even no tree - it just dumps them all in current directory.
-- Writing directory tree walker is just a technical excersise and not the point
-- of this prototyping.


with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;  use Ada;
with GNAT.Command_Line;
with Ada.Numerics.Discrete_Random;with Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters;

with Proto_Portage.Sequencers;
use Proto_Portage; use Proto_Portage.Sequencers;

procedure Generate_Stubs is


Finish : Exception;
	-- immediate (normal) termination
	-- This ain't a multithreaded, real-time app, just a simple utility.
	-- The exception will work just fine..

	procedure printUsage is
		-- could be a local procedure
		-- but this way I don't need to write a separate description :)
	begin
		Put_Line("This program generates bunch of text files that will simply");
		Put_Line("contain DEPEND='...' lines, to serve as a test-bed for");
		Put_Line("prototype ""portage code""");
		Put_Line("Number of dependencies is poisson-distributed.");
		New_Line;
		Put_Line("usage:");
		Put_Line("   " & Command_Line.Command_Name & " [options]  file");
		New_Line;
		Put_Line("options:");
		-- only short options for now
		Put_Line("-h      print this help");
		Put_Line("-g      turn on debug output (if any)");
		Put_Line("-n      number of files to generate");
		Put_Line("-m      mean of (poisson) distribution of number of dependencies");
	end printUsage;


--------------------
-- Meaningfull stuff

type ComLineParamRec is record
	NFiles : Positive := 100; -- default-sized integer > 0
	DependsMean : Positive := 3;
	UseFraction : Positive := 10; -- P(having a use flag) = 1/UseFraction;
	Debug : Boolean := False;
end record;


	procedure processCommandLine(params : in out ComLineParamRec) is
		use GNAT.Command_Line; use Ada.Command_Line;
		Options : constant String := "g h m: n:";
		Last:Positive;
		-- this works very similarly to GNU getopt, except this one uses single '-'
		-- for both short and long options, but lets not worry about it right now
	begin
		if Argument_Count < 1 then
			-- we could go ahead and create the default number of ebuilds now,
			-- but lets protect the exploratory user from filling
			-- his local bin directory where he is likely in now :)
			Put_Line("Are you sure you want to create a bunch of files right here?");
			Put_Line("please give the -n option to set the number of files to generate");
			New_Line;
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

				when 'm' => Get(Parameter,Positive(params.DependsMean),Last);
				when 'n' => Get(Parameter,Positive(params.NFiles),Last);

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

		-- some consistency checks
	end processCommandLine;


-------------------------------
--

-- we will need a few random xxx generators, os here goes

-- file names (not gonna be fancy here - just use lowercase letters)
subtype AcceptedChars is Character range 'a'..'z';
package Char_Generators is new Discrete_Random(AcceptedChars);

-- udependency list generated for every ebuild
type String_List is array(Positive range <>) of Unbounded_String;
	-- for every ebuild we first generate numer of dependencies
	-- so we can just use an array here.
	-- Some dependencies are gonna have use flags on them, so there is no
	-- constant length - use unbouded_strings

type GeneratorRec is record
	Char : Char_Generators.Generator; -- ebuild names
	NDep, Useflag : Float_Random.Generator;
end record;

Name_Length : constant Positive := 10;

function generateName(Avoid : Ebuilds.List; Char_Generator : Char_Generators.Generator) return Ebuild_Key is
	-- generates a random file name and checks against "Avoid" list
	-- to make sure it is unique
	Name : String(1..Name_Length);
	use Char_Generators;
begin
	for i in Name'Range loop
		Name(i) := Random(Char_Generator);
	end loop;
	return Create_Key(Name);
end;


function generateDependency_Strings(Available : Ebuilds.List; Useflags : Use_Flags.List;
		 Generators : GeneratorRec; params : ComLineParamRec)
		 return String_List is
	-- this time they are called "Available" :), but that's the same list we need to consult
	use Float_Random;

	function generate_NDepends return Natural is
		-- returns random Natural distributed by poisson with specified mean
		use Elementary_Functions;
	begin
		return Natural(Float'Truncation( -Float(params.DependsMean)
			*Log(Random(Generators.NDep)) ));
	end;

	function generate_Useflag return String is
		-- empty string signals no flag is needed
		FG : Float_Random.Generator;
		NFlags : Natural := Use_Flags.Length(Useflags);
	begin
		declare
			FlagNum : Positive := Positive( 1.0 + Float'Truncation(
				Float(Random(Generators.Useflag))*Float(NFlags*params.UseFraction) ));
		begin
			if FlagNum > NFlags then
				return "";
			else
				return Get_Name(Use_Flags.Get_At(Useflags,FlagNum));
			end if;
		end;
	end generate_Useflag;

	subtype AvailableKeys is Positive range 1..Ebuilds.Length(Available);
	package Dep_Names is new Discrete_Random(AvailableKeys);
	DepName_Generator : Dep_Names.Generator;
	use Dep_Names;

	NDepends : Natural := generate_NDepends;

	DepStrings : String_List(1..NDepends);
		-- the result

begin
	-- the very first time this is envoked there are no dependencies to chose from
	-- just return an empty list..
	if Ebuilds.Length(Available) = 0 then
		return DepStrings;
	end if;
	-- otherwise carry on
	Reset(DepName_Generator);
	for i in 1..NDepends loop
		declare
			DepName  : String := Get_Name(Ebuilds.Get_At(Available, Random(DepName_Generator)));
			FlagName : String := generate_Useflag;
		begin
			-- check if we need a useflag
			if FlagName="" then
				DepStrings(i) := To_Unbounded_String(DepName);
			else
				DepStrings(i) := To_Unbounded_String( FlagName & "? ( " & DepName & " )" );
			end if;
		end;
	end loop;
	return DepStrings;
end;


procedure OutputEbuild(Name : Ebuild_Key; Dependencies : String_List) is
	-- use Ebuild_Key as the name
	F : File_Type;
	Last : Positive := 1;
begin
	Create(F, Name => Get_Name(Name)& ".ebuild");
	Put(F,"DEPEND="& '"');
	for i in 1 .. Dependencies'Last-1 loop
		Put_Line(F,To_String(Dependencies(i)));
	end loop;
	if Dependencies'Length > 0 then
		Put(F, To_String(Dependencies(Dependencies'Last)));
	end if;
	Put(F,'"');New_Line(F);
	Close(F);
end;


procedure Create_Flags(Flags : out Use_Flags.List) is
	-- creates a few useflags
	-- I thought about storing them somewhere and reading them,
	-- but this is just a prototyping utility! I'll just define a few right here

	use Use_Flags;

begin
	Clear(Flags);
	Append(Flags,Create_Flag("flag1"));
	Append(Flags,Create_Flag("flag2"));
	Append(Flags,Create_Flag("flag3"));
end;


procedure Reset_Generators(Generators : GeneratorRec) is
begin
	Char_Generators.Reset(Generators.Char);
	Float_Random.Reset(Generators.NDep);
	Float_Random.Reset(Generators.Useflag);
end;


-------------------------------
-- "Main" block

Generators : GeneratorRec;
-- There isn't enough entropy in the system to reset generators
-- for every new depepdency (as would be needed for local initialization
-- of random generators), so keeping them all global

params : ComLineParamRec;
EList : Ebuilds.List;
UList : Use_Flags.List;

begin
	processCommandLine(params);
	Create_Flags(Flags=>UList);
	Reset_Generators(Generators);

	-- general idea is to generate random name
	-- add DEPEND string that contains randomly chosen
	-- previously defined names and write that file out
	for CurNumber in 1 .. params.NFiles loop
		declare
			Name : Ebuild_Key := generateName(Avoid=>EList, Char_Generator=>Generators.Char);
			Dependencies : String_List := generateDependency_Strings(Available=>EList,
				Useflags=>UList, Generators=>Generators, params=>params);
		begin
			OutputEbuild(Name, Dependencies);
			Ebuilds.Append(TheList=>EList, key=>Name);
		end;
	end loop;
	-- that's it, we are done!
exception
	when Finish => null;
end;
