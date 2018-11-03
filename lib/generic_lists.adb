package body Generic_Lists is

use UMCollections;

-- with the selected implementation its just a bunch of remappings

procedure Clear(TheList : in out List) is
begin
	Clear(TheList.umc);
end;
pragma Inline(Clear);


procedure Append(TheList : in out List; key : Item) is
begin
	Append(TheList.umc, Elem=>key);
end;


procedure Insert(TheList : in out List; key : Item;
		At_Index : Positive := 1) is
begin
	Insert(TheList.umc, Elem=>key, Before=>At_Index);
end;


procedure Remove(TheList : in out List; key : Item;
		  Report_Not_Found : Boolean := True) is

	idx : Positive := Location(TheList.umc,Elem=>key);
begin
	if idx = 0 then -- not found
		if Report_Not_Found then raise Item_Not_Found;
		else return; end if;
	end if;
	Remove(TheList.umc,At_Index=>idx);
end;


procedure Remove(TheList : in out List; At_Index : Positive) is
begin
	Remove(TheList.umc, At_Index);
end;


function Length(TheList : List) return Natural is
begin
	return Length(TheLIst.umc);
end;


function Get_At(TheList : List; At_Index : Positive) return Item is
begin
	return Item_At(TheList.umc, At_Index);
end;


function Index_Of(key : Item;TheList : List) return Natural is
begin
	return Location(TheList.umc, Elem=>key);
end;

pragma Inline(Clear,Append,Insert,Length,Get_At,Index_Of);
-- making these a true remappings, no need for separate calls
-- for them. Might want to add Remove here too.


end Generic_Lists;