with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters;
-- with GNAT.IO_Aux; use GNAT.IO_Aux;

package body Proto_Portage.Support is

------------------------------------------
-- hashes

function Trivial_Hash(V : Ebuild_Key) return Natural is
	-- it is really trivial :)
begin
	return 1;
end;


function Trivial_Hash (V : Statefull_Flag) return Natural is
begin
	return 1;
end Trivial_Hash;


--------------------------------------------
-- Utility stuff

procedure Skip_Whitespace(S : String; Position : in out Positive) is
begin
	while Position <= S'Last and then
		( S(Position) = ' ' or S(Position) = Latin_1.HT )
	loop
		Position := Position+1;
	end loop;
end;


function Index(S : String; C : Character; Going : Direction := Forward)
		return Natural is
begin
	if Going = Forward then
		for i in S'Range loop
			if S(i) = C then return i; end if;
		end loop;
	else
		for i in reverse S'Range loop
			if S(i) = C then return i; end if;
		end loop;
	end if;
	-- C wasn't found
	return 0;
end;


function Get_DEPEND_String(For_the_key : Ebuild_Key) return String is
	-- It is possible to avoid dynamic storage completely here by using recursion
	-- but Unbounded_Strings are safe and iterative routine is more readable
	-- in this case

	F : File_Type;
	S : Unbounded_String := Null_Unbounded_String;
	DepStr : constant String := "DEPEND="& Latin_1.Quotation;
		-- I think this is more readable than "DEPEND="""
	DepStrLen : constant Positive := DepStr'Length;

begin
	Open(F, Mode=>In_File, Name=>Get_Name(For_the_key) & ".ebuild");

	-- skip until DEPEND string,
	loop
	  begin  -- need to protect Get_Line function
		declare
			SS : String := Get_Line(F);
			Pos : Positive := SS'First;
		begin
			Skip_Whitespace(SS,Pos);
			if Pos <= SS'Last-DepStrLen and then
				SS(Pos .. Pos + DepStrLen -1) = DepStr then
				-- initiate the storage string with the rest of the line after DEPEND="
				if SS(SS'Last)='"' then
					Close(F);
					return SS(Pos + DepStrLen .. SS'Last-1);
				else
					S := To_Unbounded_String( SS(Pos + DepStrLen .. SS'Last) );
				end if;
				exit; -- the loop;
			end if;
		end;
	  exception
	  	when End_Error => -- did not find DEPEND in file
			Close(F);
			return "";
			-- returning empty string for now, but should really raise
			-- informative exception!
	  end;
	end loop;

	-- now read the rest of DEPEND into S
	loop
	  begin  -- need to protect Get_Line function
		declare
			SS : String := Get_Line(F);
			QuotationPos : Natural := Index(SS, Latin_1.Quotation);
		begin
			if QuotationPos = 0 then
				S := S & " " & SS;
				-- substituting a space instead of newline
			else
				Close(F);
				return To_String(S & " " & SS(SS'First .. QuotationPos-1));
			end if;
		end;
	  exception
		when End_Error => -- no closing " either, who wrote this ebuild!
			Close(F);
			return To_String(S);
			-- the best that we can do,
			-- still raising an exception is better in real code
			-- as this might indicate broken transmission or any other problem
	  end;
	end loop;

	-- the compiler generates a warning due to a missing return here
	-- (which is not necessary as either quotation mark or End_Error exception
	-- will trigger returns), but I am too lazy to look up the correct suppression pragma
	-- to make the warning go away
end;

end Proto_Portage.Support;
