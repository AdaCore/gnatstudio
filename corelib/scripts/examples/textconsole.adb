with GNAT.IO;  use GNAT.IO;
with Scripts;  use Scripts;

package body TextConsole is

   procedure Set_Data_Primitive
     (Instance : Class_Instance; Console  : access Text_Console) is
   begin
      Set (Console.Instances, Get_Script (Instance), Instance);
   end Set_Data_Primitive;

   function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Text_Console) return Class_Instance is
   begin
      return Get (Console.Instances, Script);
   end Get_Instance;

   procedure Insert_Text (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Text;

   procedure Insert_Prompt (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Txt);
   end Insert_Prompt;

   procedure Insert_Error (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put (Standard_Error, Txt);
   end Insert_Error;

   procedure Insert_Log (Console : access Text_Console; Txt : String) is
      pragma Unreferenced (Console);
   begin
      Put_Line ("-- log: " & Txt & "--");
   end Insert_Log;

end TextConsole;
