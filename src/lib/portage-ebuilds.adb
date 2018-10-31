package body Portage.Ebuilds is

    ----------------------------------
    --  Dependency_Type

    function Create (key : Ebuild_Key; condition : UseFlags.UseFlag_Set) return Dependency_Type is
		dep : Dependency_Type;
    begin
		raise Not_Implemented;
        return dep;
    end Create;

    function Vertex (dep : Dependency_Type) return Ebuild_Key is
    begin
        return dep.key;
    end Vertex;

    function Condition (dep : Dependency_Type) return UseFlags.UseFlag_Set is
    begin
        return dep.condition;
    end Condition;



    --------------------------------------------------------------
    --  Ebuild

    function ReadFromFile (FileName : String) return Ebuild is
		ebd : Ebuild;
    begin
		raise Not_Implemented;
        return ebd;
    end ReadFromFile;

    function Name (ebd : Ebuild) return String is
    begin
        return To_String (ebd.Name);
    end Name;

    function Key (ebd : Ebuild) return Ebuild_Key is
    begin
        return Create (To_String (ebd.Name));
    end Key;


    function NDeps (ebd : Ebuild) return Dep_Count is
    begin
		--  FIXME! Introduce proper Dependency_Index type
        return DepVectors.Length (ebd.deps.v);
    end NDeps;


    function Dependency (ebd : Ebuild; i : Dep_Index) return Dependency_Type is
		dep : Dependency_Type;
    begin
		raise Not_Implemented;
		return dep;
    end Dependency;



    --------------------------------------------------
    --  Ebuild_List

    procedure Clear (EList : in out Ebuild_List) is
    begin
		raise Not_Implemented;
    end Clear;


    function Is_Empty (EList : Ebuild_List) return Boolean is
    begin
		raise Not_Implemented;
		return True;
    end Is_Empty;


    procedure Add (EList : in out Ebuild_List; ebd : in Ebuild'Class) is
    begin
		raise Not_Implemented;
    end Add;


    function GetEbuild (EList : Ebuild_List; key : Ebuild_Key) return Ebuild'Class is
		ebd : Ebuild'Class := Empty_Ebuild;
    begin
		raise Not_Implemented;
		return ebd;
    end GetEbuild;


    function HasEbuild (EList : Ebuild_List; key : Ebuild_Key) return Boolean is
    begin
		raise Not_Implemented;
		return False;
    end HasEbuild;


    procedure Print (EList : in Ebuild_List;
        F : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output) is
    begin
		raise Not_Implemented;
    end Print;


    ---------
    -- "=" --
    ---------

    function "=" (Left, Right : Ebuild) return Boolean is
    begin
        return Left.Name = Right.Name;
    end "=";

end Portage.Ebuilds;
