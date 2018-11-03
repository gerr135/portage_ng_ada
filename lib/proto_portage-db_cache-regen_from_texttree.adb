Separate (Proto_Portage.Db_Cache)
procedure Regen_From_TextTree (Cache : in out Dependency_Cache;
		Debug : Boolean := False) is
	-- this prototyping model is a bit restricted, so the code i quire dumb:
	-- just go sequientially over all available ebuilds and add them
	-- one-by-one. The Add_Dependency and Add_Key do the tracking
	-- of "missing" ends.
	-- TODO: We will just need to check that the missing
	-- list is empty at the end.
	--
	-- Also remember - we are just building digraph here, - not resolving dependencies!

	use GNAT.Directory_Operations, GNAT.Directory_Operations.Iteration;

	Missing_Keys : Ebuild_Key_Set;
		-- here we'll keep track of the keys that are "due" in order
		-- to resolve dependencies

	F : File_Type;

	procedure Add_Ebuild(
			Item : String;    -- here we get the name of the ebuild
			Index : Positive; -- the other two have no use for us,
			Quit : in out Boolean) is  -- Quit is preset to False, no need to worry about it
		-- the meat of the method
		-- The idea is to compose the string containing all in DEPEND (possibly multiline)
		-- and then to alternate Get_Useflags & Get_Key until the end.
		--
		-- It does not support multiple levels of () (but this is not that hard to add either,
		-- doesn't even require recursion), but it does support multiple use? before the dependency name

		use Ada.Characters;

		-- wrapping portions of code into routines
		procedure Get_Next_Flags (TheSet : in out Useflag_Set;
				S : String; Last : in out Natural) is
			-- read all immediate useflags and add them to TheSet
			-- set Last to the first witespace char after last '?',
			-- the position of '(' or S'Last+1
			-- TheSet is not cleared inside this procedure

		begin
			Outer: -- loop lable
			loop
			  declare
				i : Natural;
				Flag_Enabled : Boolean := True;
			  begin
				Skip_Whitespace(S,Last);
				exit Outer when Last>S'Last or else S(Last) = '(';

				i := Last;
				while not ( S(i) = ' ' or S(i) = Latin_1.HT ) loop
					i := i + 1;
					exit Outer when i > S'Last;
				end loop;
				-- now i points at the next whitespace or past the string

				-- check if this is a flag
				exit when S(i-1) /= '?';
					-- Last remains pointing to the beginning of the word

				-- otherwise add it
				if S(Last) = '!' then
					Flag_Enabled := False;
					Last := Last + 1;
				end if;

				-- all is ready, add the flag.
				-- Note, i now points to the space after last char
				-- which is '?' (so it is i-2)
				Add(Set=>TheSet, Flag=>Create_Flag(S(Last..i-2)),
					Enabled=>Flag_Enabled);
				Last := i;
			  end;
			end loop Outer;
		end Get_Next_Flags;

		procedure Get_Next_DepKey (TheKey : in out Ebuild_Key;
				S : String; Last : in out Natural) is
			-- Last points at the whitespace or the beginning of the word
			-- or '('. Also this routine is called right after Get_Flags
			-- so there is no need to check whether this is a flag or a dependency

			i : Natural;
			Bracketed : Boolean := False;
		begin
			Skip_Whitespace(S,Last);

			-- brackets need special processing
			if S(Last) = '(' then
				Bracketed := True;
				Last := Last + 1;
				Skip_Whitespace(S,Last);
			end if;

			i := Last;
			while not ( S(i) = ' ' or S(i) = Latin_1.HT ) loop
				i := i + 1;
				exit when i > S'Last;
			end loop;
			TheKey := Create_Key(S(Last .. i-1));
			Last := i;

			if Bracketed then
				-- skip until closing ')'
				while Last <= S'Last and then S(Last) /= ')' loop
					Last := Last + 1;
				end loop;
				Last := Last + 1;
			end if;
		end Get_Next_DepKey;
		--pragma Inline(Get_Next_Flags,Get_Next_DepKey);


		Pkg_Name : String := Base_Name(Item(Item'First .. Item'Last-7));
			-- get rid of the ".ebuild" part
		Pkg_Key  : Ebuild_Key := Create_Key(Pkg_Name);

		DEPEND_String : String := Get_DEPEND_String(For_the_key => Pkg_Key);
		Last : Natural := DEPEND_String'First;

	begin -- Add_Ebuild
		Add_Key(Cache, key=>Pkg_Key, Outstanding_Keys=>Missing_Keys);
		if Debug then
			Put("key: "& Pkg_Name & ", depStr='" & DEPEND_String &"'; deps:");
		end if;

		-- now process dependencies
		loop
		  declare
			Use_Set : Useflag_Set;
			Dep_Key : Ebuild_Key;
				-- I could have made them more global instead of declaring them here
				-- but this clearly marks them local to this part of code
				-- besides they would have to be cleared at the end of every step
				-- anyway, so it is not clear whether there is any performance benefit
		  begin
			Get_Next_Flags (Use_Set, S=>DEPEND_String, Last=>Last);
			exit when Last > DEPEND_String'Last;
				-- useflag should always be followed by dependency
				-- but we don't want to die on malformed DEPEND string
				-- Aborting in this way will just drop the hanging useflags
			Get_Next_DepKey(Dep_Key, S=>DEPEND_String, Last=>Last);

			Add_Dependency (Cache,
				From_Key  => Pkg_Key, To_Key => Dep_Key,
				Condition => Use_Set,
				Outstanding_Keys => Missing_Keys);
			if Debug then Put(Get_Name(Dep_Key)& ", "); end if;

			exit when Last > DEPEND_String'Last;
		  end;
		end loop;
		if Debug then New_Line; end if;
	end Add_Ebuild;

	procedure Find_Ebuilds is new
		GNAT.Directory_Operations.Iteration.Find(Action => Add_Ebuild );

	Tree_Dir : String := Get_Tree_Location(Cache);
begin
	Change_Dir(Tree_Dir);
		-- this might reduce seeking, as full path resolution might not
		-- be necessary for all files

	-- In fact tree walker isn't even necessary. The Find procedure
	-- in  GNAT.Directory_Operations.Iteration works just like find utility,
	-- except that it takes regexps instead of wildcards.
	-- Also by treating category(ies) as a part of name we a getting automatic
	-- support for identical package names under different categories
	-- and multilevel categories as well..
	Find_Ebuilds(Root_Directory=>Tree_Dir, File_Pattern=>".*ebuild");
		-- that's right - ".*", not "*."; that's a regexp, remember?

end Regen_From_TextTree;
