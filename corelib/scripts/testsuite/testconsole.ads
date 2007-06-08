with Scripts;

package TestConsole is

   type Test_Console is new Scripts.Virtual_Console_Record with private;
   overriding procedure Insert_Text
     (Console : access Test_Console; Txt : String);
   overriding procedure Insert_Prompt
     (Console : access Test_Console; Txt : String);
   overriding procedure Insert_Error
     (Console : access Test_Console; Txt : String);
   overriding procedure Insert_Log
     (Console : access Test_Console; Txt : String);
   overriding procedure Set_Data_Primitive
     (Instance : Scripts.Class_Instance; Console : access Test_Console);
   overriding function Get_Instance
     (Script  : access Scripts.Scripting_Language_Record'Class;
      Console : access Test_Console) return Scripts.Class_Instance;
   overriding function Read
     (Console    : access Test_Console;
      Size       : Integer;
      Whole_Line : Boolean) return String;

   procedure Free (Console : in out Test_Console);
   --  Free memory associated with Console

private
   type Test_Console is new Scripts.Virtual_Console_Record with record
      Instances : Scripts.Instance_List;
   end record;

end TestConsole;
