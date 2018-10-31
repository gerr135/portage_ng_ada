-- !!!THIS IS A PROTOTYPE CODE ONLY!!!
-- !!Do not use or expand it!!
-- It is gonna be rewritten!
-- (well, unless it turns out to be the most efficient implementation,
-- which is unlikely)
-- besides there isn't much here


package body Proto_Portage is

-- here I just need to remap the Unbounded_String methods
function Create_Key(Name : String)  return Ebuild_Key is
	item : Ebuild_Key := (key=>To_Unbounded_String(Name));
begin
	return item;
end;


function Get_Name(Key : Ebuild_Key) return String is
begin
	return To_String(Key.key);
end;
pragma Inline(Create_Key, Get_Name);
	-- making a real method remap, no need to produce additional calls


function Create_Flag(Name : String) return Useflag is
	item : Useflag := (flag=>To_Unbounded_String(Name));
begin
	return item;
end;


function Get_Name (Flag : Useflag) return String is
begin
	return To_String(Flag.flag);
end;
pragma Inline(Create_Flag, Get_Name);


-- function "="  (Left, Right : in Ebuild_Key) return Boolean is
-- begin
-- 	return Ada.Strings.Unbounded."="(Left.key,Right.key);
-- end;

end Proto_Portage;
