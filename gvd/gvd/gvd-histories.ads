-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

--  This package handles all the history lists that are used throughout
--  odd, like for instance the command history.
--  All these histories can be imagined as lists, with a pointer to the
--  last item retrieved.
--
--  To traverse the whole history, you should do:
--     begin
--        loop
--           Move_To_Previous (History);
--           Put (Get_Current (History));
--        end loop
--     exception
--        when No_Such_Item => null;
--     end;

with Glib.Glist; use Glib.Glist;
with System;

generic
   type Data_Type (<>) is private;
package Odd.Histories is

   type History_List is private;

   type Direction is (Forward, Backward);

   procedure Append (History : in out History_List;
                     Data    : Data_Type);
   --  Append a new value to the history.
   --  The pointer to the current value now points to this new entry.

   function Get_Current (History : History_List) return Data_Type;
   --  Return the item currently pointed to.
   --  No_Such_Item is raised if the list is empty.

   procedure Move_To_Previous (History : in out History_List);
   --  Move the pointer to the value preceding the current one.
   --  Calling this function multiple times will traverse the whole list.
   --  If you are trying to move before the first item, nothing happens.

   procedure Move_To_Next (History : in out History_List);
   --  Move the pointer to the value following the current one.
   --  Calling this function multiple times will traverse the whole list.
   --  If you are trying to move after the last item, No_Such_Item is
   --  raised.

   function Get_Current_Repeat_Num (History : History_List) return Natural;
   --  Return the number of times the current item was repeated consequently
   --  in the history.
   --  If Collapse_Duplicates is False, this will always be 1.
   --  No_Such_Item is raised if the list is empty.

   procedure Wind  (History : in out History_List; D : Direction);
   --  Move forward or backward until end of history.

   function Length (History : in History_List) return Integer;
   --  Return the length of the history.

   procedure Free (History : in out History_List);
   --  Free memory used by the history.

   No_Such_Item : exception;

private

   type History_Position is (Inside_History, After_End, Before_Beginnning);

   type Data_Pointer is access Data_Type;

   type Data_Record is record
      Data        : Data_Pointer;
      Num_Repeats : Natural;
   end record;

   type Data_Access is access Data_Record;

   for Data_Access'Size use Standard'Address_Size;

   function Convert (Value : Data_Access) return System.Address;
   function Convert (Value : System.Address) return Data_Access;

   package Hlist is new Glib.Glist.Generic_List (Data_Access);

   type History_List is record
      Position : History_Position := Inside_History;
      Collapse : Boolean;
      List     : Hlist.Glist;
      Current  : Hlist.Glist;
   end record;

end Odd.Histories;
