with Except;
with Screen_Output;

package body Stack is

   ----------------
   -- Local Data --
   ----------------

   Size : constant := 200;
   --  The stack size.

   Tab  : array (1 .. Size) of Value;
   --  The stack. We push and pop pointers to Values.

   Last : Natural := Tab'First - 1;
   --  Indicates the top of the stack. When 0 the stack is empty.

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Last := Tab'First - 1;
   end Clear;

   -----------
   -- Empty --
   -----------

   function Empty return Boolean is
   begin
      return Last < Tab'First;
   end Empty;

   ----------
   -- Push --
   ----------

   procedure Push (V : Value) is
   begin
      if Last = Tab'Last then
         raise Overflow;
      end if;

      Screen_Output.Debug_Msg ("Pushing -> " & Values.To_String (V));

      Last := Last - 1;
      Tab (Last) := V;
   end Push;

   ---------
   -- Pop --
   ---------

   function Pop return Value is
      V : Value;

   begin
      if Empty then
         raise Underflow;
      end if;

      V := Tab (Last);
      Last := Last - 1;

      Screen_Output.Debug_Msg ("Popping <- " & Values.To_String (V));

      return V;
   end Pop;

   ---------
   -- Top --
   ---------

   function Top return Value is
   begin
      if Empty then
         raise Underflow;
      end if;

      return Tab (Last);
   end Top;

   ----------
   -- View --
   ----------

   procedure View is
   begin
      for I in Tab'First .. Last loop
         Screen_Output.Msg (Values.To_String (Tab (I)));
      end loop;

      Screen_Output.Msg ("");
   end View;

end Stack;
