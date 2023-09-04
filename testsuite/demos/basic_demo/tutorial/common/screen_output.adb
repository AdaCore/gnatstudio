with Ada.Text_IO; use Ada.Text_IO;
with Input;

package body Screen_Output is

   ----------------
   -- Local Data --
   ----------------

   Debug_On : constant Boolean := False;
   --  When set, debugging information messages are output on the screen.

   ---------
   -- Msg --
   ---------

   procedure Msg
     (S1       : String;
      S2       : String  := "";
      End_Line : Boolean := True)
   is
   begin
      Put (S1);
      Put (S2);
      if End_Line then
         New_Line;
      end if;
   end Msg;

   ---------------
   -- Debug_Msg --
   ---------------

   procedure Debug_Msg (S : String) is
   begin
      if not Debug_On then
         return;
      end if;

      Put ("DEBUG: ");
      Put (S);
      New_Line;
   end Debug_Msg;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg (S1 : String; S2 : String := ""; S3 : String := "") is
   begin
      Put ("sdc error at line");
      Put (Natural'Image (Input.Line_Number) & ": ");
      Put (S1);
      Put (S2);
      Put (S3);
      New_Line;
   end Error_Msg;

   ------------------
   -- Syntax_Error --
   ------------------

   procedure Syntax_Error (S : String; Error_Pos : Natural := 0) is
      Pos : Natural := Error_Pos;

   begin
      if Pos = 0 then
         Pos := Input.Column_Number;
      end if;

      Put ("sdc:");
      Put_Line (Input.Current_Line);

      Put ("sdc:");
      for I in 1 .. Pos - 1 loop
         Put ("-");
      end loop;

      Put_Line ("!");
      Put ("sdc input error at line");
      Put (Natural'Image (Input.Line_Number) & ": " & S);
      New_Line;
   end Syntax_Error;

   -----------
   -- Pause --
   -----------

   procedure Pause is
   begin
      Put ("Press a key to continue...");
      Skip_Line;
   end Pause;

end Screen_Output;
