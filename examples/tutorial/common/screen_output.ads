--  Routines used to print messages on the screen

package Screen_Output is

   procedure Msg (S1 : String; S2 : String := ""; End_Line : Boolean := True);
   --  Prints message S1 followed by S2 on the screen. If End_Line is True
   --  appends to the message a carriage return (ie it ends a line).

   procedure Debug_Msg (S : String);
   --  Prints a debugging message if the flag Debug_On defined inside
   --  the body of this package is set to True;

   procedure Error_Msg (S1 : String; S2 : String := ""; S3 : String := "");
   --  Prints the error message S1 followed by S2 followed by S3 on the screen.

   procedure Syntax_Error (S : String; Error_Pos : Natural := 0);
   --  Prints the current input line that is being read indicating the
   --  character that caused the syntax error. The position of this
   --  character is given in Error_Pos. If Error_Pos = 0 then the position
   --  of the last character read is used. After printing the current input
   --  line prints message S preceded by the current line number.

   procedure Pause;
   --  Prints a pause message, and then wait for a carriage return.

end Screen_Output;
