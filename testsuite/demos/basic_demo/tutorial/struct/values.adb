with Except;
with Input;  use Input;
with Stack;

package body Values is

   -------------
   -- Process --
   -------------

   procedure Process (V : Value) is
   begin
      Stack.Push (V);
   end Process;

   ----------
   -- Read --
   ----------

   function Read (Word : String) return Value is
      Int_Val  : Integer;
      Real_Val : Float;
      Kind     : Input.Number_Kind;

   begin
      Input.Read_Number (Word, Int_Val, Real_Val, Kind);

      if Kind /= Int_Number then
         raise Except.User_Error;
      end if;

      return new Value_Info'(E => Int_Val);
      --  Allocate a new Value_Info (which is a record with one field)
      --  on the heap and initialize its only field "E" to be "Int_Val".
      --  NOTE: the ' in Value_Info'(...) must be there.
   end Read;

   ---------------
   -- To_String --
   ---------------

   function To_String (V : Value) return String is
   begin
      return Integer'Image (V.E);
      --  V is a pointer to a Value_Info record. V.all is the
      --  actual Value_Info record pointed by V. Thus, strictly speaking,
      --  we shoudl have written V.all.E above. However, Ada allows to
      --  remove the ".all" because the meaning of expression "V.E" is
      --  clear from its context.
   end To_String;

end Values;
