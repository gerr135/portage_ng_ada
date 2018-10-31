package body Portage.UseFlags is

    ----------------------------------
    -- UseFlag_Set

    procedure Clear (Set : in out UseFlag_Set) is
    begin
        raise Not_Implemented;
    end Clear;

    procedure AddFlag (Set : in out UseFlag_Set; flag : in UseFlag) is
    begin
		raise Not_Implemented;
    end AddFlag;

    procedure AddFlag (Set        : in out UseFlag_Set;
        flag       : in String; flag_State : UseFlag_States := On) is
    begin
		raise Not_Implemented;
    end AddFlag;

    function FlagStatus (Set : UseFlag_Set; flag : UseFlag) return UseFlag_States is
    begin
        return FlagStatus (Set, flag);
    end FlagStatus;

    function FlagStatus (Set : UseFlag_Set; flag : String) return UseFlag_States is
    begin
        return FlagStatus (Set, flag);
    end FlagStatus;

    procedure RemoveFlag (Set : in out UseFlag_Set; flag : UseFlag) is
    begin
        null;
    end RemoveFlag;

    function First_Flag (Set : UseFlag_Set) return UseFlag is
    begin
        return First_Flag (Set);
    end First_Flag;

    function Last_Flag (Set : UseFlag_Set) return UseFlag is
    begin
        return Last_Flag (Set);
    end Last_Flag;


    --------------------------------------------------
    --  Cursor

    function Element (Position : Cursor) return UseFlag is
    begin
        return Element (Position);
    end Element;

    function Find (Set : UseFlag_Set; Flag : UseFlag) return Cursor is
    begin
        return Find (Set, Flag);
    end Find;

    function First (Set : UseFlag_Set) return Cursor is
    begin
        return First (Set);
    end First;

    function Last (Set : UseFlag_Set) return Cursor is
    begin
        return Last (Set);
    end Last;

    function Next (Position : Cursor) return Cursor is
    begin
        return Next (Position);
    end Next;

    procedure Next (Position : in out Cursor) is
    begin
		raise Not_Implemented;
    end Next;

    function Previous (Position : Cursor) return Cursor is
    begin
        return Previous (Position);
    end Previous;

    procedure Previous (Position : in out Cursor) is
    begin
		raise Not_Implemented;
    end Previous;

end Portage.UseFlags;
