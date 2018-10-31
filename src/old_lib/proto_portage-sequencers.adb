with Ada.Characters.Latin_1;

package body Proto_Portage.Sequencers is

-----------------------------
-- Ebuild_Key_Set

procedure Clear(TheSet : in out Ebuild_Key_Set) is
begin
	UM_EbSets.Clear(TheSet.ums);
end;


procedure Add_Key(TheSet : in out Ebuild_Key_Set; key : Ebuild_Key) is
begin
	UM_EbSets.Add(TheSet.ums, key );
end;


procedure Remove_Key(TheSet : in out Ebuild_Key_Set; key : Ebuild_Key) is
begin
	UM_EbSets.Remove(TheSet.ums, key);
exception
	when BC.Not_Found =>
		null;
end;


function  Contains(TheSet : Ebuild_Key_Set; key : Ebuild_Key) return Boolean is
begin
	return UM_EbSets.Is_Member(S=>TheSet.ums, I=>key);
end;


------------------------------
-- Useflag_Set

procedure Add (Set : in out Useflag_Set;
	Flag : Useflag; Enabled : Boolean := True) is
begin
	UM_FlagSets.Add(Set.ums, (Flag,Enabled) );
end Add;


procedure Clear (Set : in out Useflag_Set) is
begin
	UM_FlagSets.Clear(Set.ums);
end Clear;


procedure Append_From_USE(Use_Set : in out Useflag_Set; From_String : String) is
	i, Last : Natural := From_String'First;
begin
	-- GNAT.Awk is an overkill for this situation and I don't like
	-- my Strings unit yet. Will simply parse it here..
	--
	-- skip to first non-space
	while i<=From_String'Last and then From_String(i) = ' ' loop i := i+1; end loop;
	Last := i; -- points to the first non-space char now
	-- now do the parsing
	loop
		i := i + 1;
		exit when i > From_String'Last;
			-- you can put exit condition pretty much anywhere in ada
		if From_String(i) = ' ' then
			Add(Use_Set, Create_Flag(From_String(Last..i-1)));
			-- loop over spaces, as there might be more than one
			while i<=From_String'Last and then From_String(i) = ' ' loop i := i+1; end loop;
			Last := i;
		end if;
	end loop;
end;


function  Create_From_Use(From_String : String) return Useflag_Set is
	UFSet : Useflag_Set;
begin
	Append_From_DEPEND(UFSet, From_String);
	return UFSet;
end;


procedure Append_From_DEPEND(Use_Set : in out Useflag_Set; From_String : String) is
	-- this is a tricky one, as there are many things that can go wrong
	-- and this function should gracefully handle any junk,
	-- but I'll limit the syntaxis for the purpose of prototyping..
	-- Also it should really raise informative exceptions on misformed strings
	-- but I'll skip on that as well for now..

	use Ada.Characters;

	function Is_a_Whitespace(i : Positive) return Boolean is
	begin
		return From_String(i) = ' ' or From_String(i) = Latin_1.HT;
	end;
	pragma Inline(Is_a_Whitespace);

	i, Last : Natural := From_String'First;

begin
	Outer:  -- loop lable
	loop
	  declare
		Flag_Enabled : Boolean := True;
	  begin
		-- skip the whitespace
		while Is_a_whitespace(i) loop
			-- space or tab
			i := i+1;
			exit Outer when i>From_String'Last;
		end loop;
		exit when From_String(i) = '(';

		Last := i;

		-- fetch the next non-whitespace block
		while not Is_a_whitespace(i) loop
			i := i+1;
			exit Outer when i>From_String'Last or else From_String(i) = '(';
		end loop;
		-- and check it it is a flag
		exit when From_String(i) /= '?';

		if From_String(Last) = '!' then
			Flag_Enabled := False;
			Last := Last + 1;
		end if;

		-- all is ready, add the flag.
		-- Note, i now points to the space after last char which is '?' (so it is i-2)
		Add(Set=>Use_Set, Flag=>Create_Flag(From_String(Last..i-2)), Enabled=>Flag_Enabled);
	  end;
	end loop outer;
end;


function  Create_From_DEPEND(From_String : String) return Useflag_Set is
	UFSet : Useflag_Set;
begin
	Append_From_DEPEND(UFSet, From_String);
	return UFSet;
end;
pragma Inline(Create_From_USE, Create_From_DEPEND);



function Contains (Set : Useflag_Set; Flag : Useflag)
		return Inclusion_State is
begin
	-- a very basic implementation here,
	-- may need twosearches internally, but thats just a prototype
	if    UM_FlagSets.Is_Member(S=>Set.ums, I=>(Flag,Enabled=>True)) then
		return Enabled;
	elsif UM_FlagSets.Is_Member(S=>Set.ums, I=>(Flag,Enabled=>False)) then
		return Disabled;
	else
		return Absent;
	end if;
end Contains;


function Is_aSubset (subSet, Set : Useflag_Set) return Boolean is
	-- use active iteration
	use BCC_Flags;
	use UM_FlagSets;
	Iter : Iterator'Class := New_Iterator(For_The_Set=>subSet.ums);
begin
	-- iterator is reset at creation
	while not Is_Done(Iter) loop
		declare
			CurItem : Statefull_Flag := Current_Item(Iter);
		begin
			if ( CurItem.Enabled and then not Is_Member(Set.ums, CurItem) )
			or else ( not CurItem.Enabled and then
				Is_Member(Set.ums, (CurItem.Flag,False)) )
			then
				return False;
			end if;
			Next(Iter);
		end;
	end loop;
	return True;
end Is_aSubset;


function Is_Empty (Set : Useflag_Set) return Boolean is
begin
	return UM_FlagSets.Is_Empty (Set.ums);
end Is_Empty;


procedure Remove (Set : in out Useflag_Set; Flag : Useflag) is
begin
	UM_FlagSets.Remove(Set.ums, (Flag,Enabled=>True));
		-- the enabled variant should appear more often
exception
	when BC.Not_Found =>
		UM_FlagSets.Remove(Set.ums, (Flag,Enabled=>False));
		-- if still not found the exception will be raised again
		-- and will propagate this time
end Remove;

pragma Inline(Add,Clear,Contains,Remove,Is_Empty);


end Proto_Portage.Sequencers;

