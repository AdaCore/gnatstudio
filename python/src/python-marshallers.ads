
--  ??? Question: how do we have enough marshallers for all possible
--      commands: use generic ?
--  ??? How do we add marshallers for as yet unknown types (projects,...)


package Python.Marshallers is

   type Integer_Callback  is access procedure (Int : Integer);
   type Integer2_Callback is access procedure (Int1, Int2 : Integer);


   type Shell_Language is abstract tagged private;
   type Shell_Language_Access is access all Shell_Language_Record'Class;
   procedure Register_Command
     (Lang : access Shell_Language;
      Func : Integer_Callback;
      Name, Param1 : String) is abstract;
   procedure Register_Command
     (Lang : access Shell_Language;
      Func : Integer2_Callback;
      Name, Param1, Param2 : String) is abstract;


   type Python_Language is new Shell_Language with private;
   procedure Register_Command
     (Lang : access Python_Language;
      Func : Integer_Callback;
      Name, Param1 : String);
   procedure Register_Command
     (Lang : access Shell_Language;
      Func : Integer2_Callback;
      Name, Param1, Param2 : String);

end Python.Marshallers;
