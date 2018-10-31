package body Portage is

    --------------------------------------------------
    --  Ebuild_Key

    function Create (Name : String) return Ebuild_Key is
        EKey : Ebuild_Key := (key => To_Unbounded_String (Name));
    begin
        return EKey;
    end Create;
    pragma Inline (Create);

    function Get_Name (Key : Ebuild_Key) return String is
    begin
        return To_String (Key.key);
    end Get_Name;
    pragma Inline (Get_Name);

    function "=" (Left, Right : in Ebuild_Key) return Boolean is
    begin
        return Left.key = Right.key;
    end "=";

    ------------------------------
    --  UseFlag

    function Create (Name : String; State : UseFlag_States := On) return UseFlag is
        Flag : UseFlag := (Name => To_Unbounded_String (Name), State => State);
    begin
        return Flag;
    end Create;
    pragma Inline (Create);

    function Get_Name (Flag : UseFlag) return String is
    begin
        return To_String (Flag.Name);
    end Get_Name;
    pragma Inline (Get_Name);

    function Get_State (Flag : UseFlag) return UseFlag_States is
    begin
        return Flag.State;
    end Get_State;
    pragma Inline (Get_State);

    function "<" (Left, Right : UseFlag) return Boolean is
    begin
        return Left.Name < Right.Name;
    end "<";
    pragma Inline ("<");

    function "=" (Left, Right : UseFlag) return Boolean is
    begin
        return Left.Name = Right.Name;
    end "=";
    pragma Inline ("=");

    -----------------------------------------------------
    --  EbuildKey_List

    procedure Clear (EList : in out EbuildKey_List) is
    begin
        raise Not_Implemented;
    end Clear;

    function Is_Empty (EList : EbuildKey_List) return Boolean is
    begin
        raise Not_Implemented;
        return True;
    end Is_Empty;

    procedure Add (EList : in out EbuildKey_List; ebd : in Ebuild_Key) is
    begin
        raise Not_Implemented;
    end Add;

    procedure Remove (EList : in out EbuildKey_List; ebd : in Ebuild_Key) is
    begin
        raise Not_Implemented;
    end Remove;

    function HasKey (EList : EbuildKey_List; key : Ebuild_Key) return Boolean is
    begin
        raise Not_Implemented;
        return True;
    end HasKey;

end Portage;
