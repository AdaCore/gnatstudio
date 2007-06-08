with Scripts;

package TextConsole is

   type Text_Console is new Scripts.Virtual_Console_Record with private;
   overriding procedure Insert_Text
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Prompt
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Error
     (Console : access Text_Console; Txt : String);
   overriding procedure Insert_Log
     (Console : access Text_Console; Txt : String);
   overriding procedure Set_Data_Primitive
     (Instance : Scripts.Class_Instance; Console : access Text_Console);
   overriding function Get_Instance
     (Script  : access Scripts.Scripting_Language_Record'Class;
      Console : access Text_Console) return Scripts.Class_Instance;

private
   type Text_Console is new Scripts.Virtual_Console_Record with record
      Instances : Scripts.Instance_List;
   end record;

end TextConsole;
