------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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
     Ada.Unchecked_Deallocation (Type_Array, Type_Array_Access);

   -----------
   -- Clear --
   -----------

   procedure Clear (Stack : in out Simple_Stack) is
   begin
      Unchecked_Free (Stack.Values);
      Stack.Last := 0;
   end Clear;

   ----------
   -- Push --
   ----------

   Minimal_Array_Length : constant := 64;
   --  Minimal length to allocate on an array

   procedure Push (Stack : in out Simple_Stack; Value : Generic_Type) is
      Tmp : Type_Array_Access;
   begin
      if Stack.Values = null then
         Stack.Values := new Type_Array (1 .. Minimal_Array_Length);
      elsif Stack.Last >= Stack.Values'Last then
         Tmp := Stack.Values;
         Stack.Values := new Type_Array (1 .. Tmp'Length * 2);
         Stack.Values (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Stack.Last := Stack.Last + 1;
      Stack.Values (Stack.Last) := Value;
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop (Stack : in out Simple_Stack; Value : out Generic_Type) is
   begin
      if Stack.Last = 0 then
         raise Stack_Empty;
      else
         Value := Stack.Values (Stack.Last);
         Stack.Last := Stack.Last - 1;
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
      if Stack.Last = 0 then
         raise Stack_Empty;
      else
         return Stack.Values (Stack.Last)'Access;
      end if;
   end Top;

   ----------
   -- Next --
   ----------

   function Next (Stack : Simple_Stack) return Generic_Type_Access is
   begin
      if Stack.Last <= 1 then
         return null;
      else
         return Stack.Values (Stack.Last - 1)'Access;
      end if;
   end Next;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Stack : Simple_Stack) return Boolean is
   begin
      return Stack.Last = 0;
   end Is_Empty;

   --------------------
   -- Traverse_Stack --
   --------------------

   procedure Traverse_Stack
     (Stack    : Simple_Stack;
      Callback : access function (Obj : Generic_Type) return Boolean) is
   begin
      for J in 1 .. Stack.Last loop
         exit when not Callback (Stack.Values (J));
      end loop;
   end Traverse_Stack;

end Generic_Stack;
