-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

generic
   type Element_Type is private;
   --  with procedure Delete_Element (el : Element_Type);

package Simple_Vector is

   -------------------------------------------------------------------------

   type Node;
   type Node_Access is access Node;
   type Node is record
      Data : Element_Type;
      Next : Node_Access;
   end record;

   -------------------------------------------------------------------------

   procedure Append (Item : in Element_Type; Root : in out Node_Access);
   --  Appends new item at the beginning of the list. If root is null then
   --  a new list is created.

   procedure Append (Root : in out Node_Access; Item : in Element_Type);
   --  Appends new item to the end of the list. If root is null then a new
   --  list is created.

   function Get_Element_At (Root : in Node_Access; Index : in Integer)
         return Element_Type;
   --  Returns element with specified Index. If such element is not
   --  available then exception is raised. Starting index is 1 for the
   --  first element.

   function Size (Root : in Node_Access) return Integer;
   --  Returns size of the Vector

   procedure Release_Vector (Root : in out Node_Access);

   Element_Not_Found : exception;

   --  Other methods are welcome

   -------------------------------------------------------------------------

end Simple_Vector;
