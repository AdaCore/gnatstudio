-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

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
      Stack := new Stack_Record'(Value, Stack);
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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Stack : Simple_Stack) return Boolean is
   begin
      if Stack = null then
         return True;
      else
         return False;
      end if;
   end Is_Empty;

end Generic_Stack;
