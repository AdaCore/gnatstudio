package body Generic_Stack is

   -----------
   -- Clear --
   -----------

   procedure Clear (Stack : in out Simple_Stack) is
      P : Simple_Stack;
   begin
      loop
         exit when Stack = null;
         P := Stack;
         Stack := Stack.Next;
         Free (P);
      end loop;
   end Clear;

   ----------
   -- Push --
   ----------

   procedure Push (Stack : in out Simple_Stack; Value : Generic_Type) is
   begin
      Stack := new Stack_Record' (Value, Stack);
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Stack : in out Simple_Stack; Value : out Generic_Type) is
      P : Simple_Stack;
   begin
      if Stack = null then
         raise Stack_Empty;
      else
         Value := Stack.Val;
         P     := Stack;
         Stack := Stack.Next;
         Free (P);
      end if;
   end Pop;

   procedure Pop (Stack : in out Simple_Stack) is
      Value : Generic_Type;
   begin
      Pop (Stack, Value);
   end Pop;

   ---------
   -- Top --
   ---------

   function Top (Stack : Simple_Stack) return Generic_Type_Access is
   begin
      if Stack = null then
         raise Stack_Empty;
      else
         return Stack.Val'Access;
      end if;
   end Top;

end Generic_Stack;
