------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Generic_Stack is

   procedure Unchecked_Free is new
     Ada.Unchecked_Deallocation (Stack_Record, Simple_Stack);

   -----------
   -- Clear --
   -----------

   procedure Clear (Stack : in out Simple_Stack) is
      P : Simple_Stack;
   begin
      while Stack /= null loop
         P := Stack;
         Stack := Stack.Next;
         Unchecked_Free (P);
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
         Unchecked_Free (P);
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

   ----------
   -- Next --
   ----------

   function Next (Stack : Simple_Stack) return Generic_Type_Access is
   begin
      if Stack = null or else Stack.Next = null then
         return null;
      else
         return Stack.Next.Val'Access;
      end if;
   end Next;

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
