with Except;
with Screen_Output;
with Stack;
with Values;

package body Instructions is

   ----------
   -- Read --
   ----------

   function Read (Word : String) return Instruction is
   begin
      --  Loop through each instruction asking if its string representation
      --  matches the word we have just encountered in the input.

      for I in Instruction loop
         --  Technical Note: Given a scalar type (such as an integer, an
         --  enumeration, etc), Type'Image (X) returns the value of X
         --  converted to a string.

         if Instruction'Image (I) = Word then
            return I;
         end if;
      end loop;

      --  If we have found an unrecognized instruction raise an exception.

      raise Except.User_Error;
   end Read;

   -------------
   -- Process --
   -------------

   procedure Process (I : Instruction) is
   begin
      case I is
         when Clear =>
            Stack.Clear;

         when Print =>
            Screen_Output.Msg (" -> ", End_Line => False);

            if Stack.Empty then
               Screen_Output.Msg ("stack is empty");
            else
               Screen_Output.Msg (Values.To_String (Stack.Top));
            end if;

         when Quit =>
            raise Except.Exit_SDC;

      end case;
   end Process;

end Instructions;


