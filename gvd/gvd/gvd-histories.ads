------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  This package handles all the history lists that are used throughout
--  the debugger, like for instance the command history.
--  All these histories can be viewed as lists, with a pointer to the
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

generic
   type Data_Type (<>) is private;
package GVD.Histories is

   type History_List is private;

   type Direction is (Forward, Backward);

   procedure Append
     (History : in out History_List;
      Data    : Data_Type);
   --  Append a new value to the history.
   --  The pointer to the current value now points to this new entry.

   function Get_Current (History : History_List) return Data_Type;
   --  Return the item currently pointed to.
   --  No_Such_Item is raised if the list is empty.

   procedure Set_Current (History : History_List; Data : Data_Type);
   --  Change the current item data.

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

   function Length (History : History_List) return Integer;
   --  Return the length of the history.

   procedure Free (History : in out History_List);
   --  Free memory used by the history.

   No_Such_Item : exception;

private

   type History_Position is (Inside_History, After_End, Before_Beginning);

   type Data_Pointer is access Data_Type;

   type Hlist;

   type Hlist_Link is access Hlist;

   type Hlist is record
      Data        : Data_Pointer;
      Previous    : Hlist_Link;
      Next        : Hlist_Link;
      Num_Repeats : Natural;
   end record;

   type History_List is record
      Position : History_Position := Inside_History;
      Collapse : Boolean;
      First    : Hlist_Link;
      Last     : Hlist_Link;
      Current  : Hlist_Link;
      Length   : Integer := 0;
   end record;

end GVD.Histories;
