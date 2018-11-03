with Proto_Portage.Support;
use Proto_Portage.Support; use Proto_Portage;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure test_Get_DEPEND is

begin
	Put_Line(Get_DEPEND_String(Create_Key(Argument(1))));
end;