with Ada.Environment_Variables;

package body Portage.UseFlags.IO is

    procedure AddFlags (Set : in out UseFlag_Set; UseStr : in String) is
    begin
        raise Not_Implemented;
    end AddFlags;

    procedure AppendEnvironment (Set : in out UseFlag_Set; EnvVar : in String) is
        use Ada.Environment_Variables;
    begin
        if Exists (EnvVar) then
            AddFlags (Set, UseStr => Value (EnvVar));
        end if;
    end AppendEnvironment;

    procedure AppendConfFile (Set : in out UseFlag_Set; FileName : in String) is
    begin
        raise Not_Implemented;
    end AppendConfFile;

    procedure CollectFlags (Set : in out UseFlag_Set) is
    begin
        AppendEnvironment (Set, EnvVar => USE_EnvVar_Name);
    end CollectFlags;

end Portage.UseFlags.IO;
