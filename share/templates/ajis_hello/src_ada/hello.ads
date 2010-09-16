package Hello is

   type Printer is abstract tagged null record;

   procedure Print_On_Console (P : Printer; V : String) is abstract;

   type Ada_Printer is new Printer with null record;

   procedure Print_On_Console (P : Ada_Printer; V : String);

   procedure Print_Messages (P : Printer'Class);

end Hello;
