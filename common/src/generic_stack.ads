-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2009, AdaCore             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

generic
   type Generic_Type is private;
package Generic_Stack is
   type Stack_Record;
   type Simple_Stack is access Stack_Record;
   type Stack_Record is record
      Val  : aliased Generic_Type;
      Next : Simple_Stack;
   end record;

   type Generic_Type_Access is access all Generic_Type;

   Stack_Empty : exception;
   --  Raised by the functions below when stack is empty.

   procedure Push (Stack : in out Simple_Stack; Value : Generic_Type);
   --  Push Value on top of Stack.

   procedure Pop (Stack : in out Simple_Stack; Value : out Generic_Type);
   --  Remove the value on top of Stack and return it in Value.
   --  Raise Stack_Empty if Stack is empty.

   procedure Pop (Stack : in out Simple_Stack);
   --  Remove the value on top of Stack.
   --  Raise Stack_Empty if Stack is empty.

   function Top (Stack : Simple_Stack) return Generic_Type_Access;
   --  Return a pointer to the top of the stack.
   --  Note that modifying the contents of the returned pointer will
   --  modify the contents of the stack.
   --  Raise Stack_Empty if Stack is empty.

   procedure Clear (Stack : in out Simple_Stack);
   --  Clear the contents of stack. This automatically frees memory for Stack
   --  as well.

   function Is_Empty (Stack : Simple_Stack) return Boolean;
   --  Returns True if the stack is empty.

private
   pragma Inline (Push);
   pragma Inline (Pop);
   pragma Inline (Top);
end Generic_Stack;
