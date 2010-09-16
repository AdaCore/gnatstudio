with Ada.Text_IO; use Ada.Text_IO;

package body Hello is

   procedure Print_On_Console (P : Ada_Printer; V : String) is
   begin
      Put_Line ("[Ada ] " & V);
   end Print_On_Console;

   procedure Print_Messages (P : Printer'Class) is
   begin
      P.Print_On_Console ("Hello");
      P.Print_On_Console ("This subprograms makes cross dispatching calls");
      P.Print_On_Console ("Printer is either extended from Ada");
      P.Print_On_Console ("or from Java.");
   end Print_Messages;

end Hello;
