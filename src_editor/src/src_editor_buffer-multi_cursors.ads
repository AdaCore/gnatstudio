------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

--  This package handles the public interface to multi cursors in the buffer,

with Gtk.Text_Iter; use Gtk.Text_Iter;

package Src_Editor_Buffer.Multi_Cursors is

   type Cursor is access all Multi_Cursor;

   package Cursors_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Cursor);

   function Get_Mark (C : Cursor) return Gtk_Text_Mark;
   function Get_Sel_Mark (C : Cursor) return Gtk_Text_Mark;

   function Get_Sel_Mark_Name (Cursor_Mark_Name : String) return String
   is
     (Cursor_Mark_Name & "_sel");

   function Get_Column_Memory (C : Cursor) return Gint;
   procedure Set_Column_Memory (C : Cursor; Offset : Gint);

   procedure Update_MC_Selection (B : Source_Buffer);

   procedure Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter);
   function Add_Multi_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter) return Cursor;
   --  Add a new multi cursor at the specified location
   --  in the specified buffer

   procedure Remove_All_Multi_Cursors (Buffer : Source_Buffer);
   --  Remove all multi cursors from the current buffer

   procedure Set_Multi_Cursors_Manual_Sync (Buffer : Source_Buffer);
   --  This sets the buffer in "main manual mode" regarding multi cursor
   --  insertion. It should be called before the main cursor action is done
   --  This basically means that in this mode, if any action is performed :
   --  - It wont impact any multi cursor
   --  - The main cursor will move accordingly to the action

   procedure Set_Multi_Cursors_Manual_Sync
     (Buffer  : Source_Buffer;
      MC_Mark : Gtk_Text_Mark);
   --  This sets the buffer in "slave manual mode" regarding multi cursor
   --  insertion, with the corresponding text mark as the multi-cursors mark.
   --  This should be called before the corresponding multi cursor's action is
   --  done. This basically means that in this mode, if any action is
   --  performed :
   --  - It wont impact any multi cursor
   --  - The main cursor will not move
   --  The action will be recorded as part of the same group as the main
   --  cursor's action regarding undo/redo groups.

   procedure Set_Multi_Cursors_Manual_Sync
     (Buffer : Source_Buffer;
      MC     : Multi_Cursor);

   procedure Set_Multi_Cursors_Auto_Sync (Buffer : Source_Buffer);
   --  This sets the buffer in auto mode regarding multi cursor insertion.
   --  This means that every insert/delete will impact every active cursors
   --  in the buffer. Do not forget to set that back after a manual multi
   --  cursor operation !

--     function Get_Multi_Cursors_Marks
--       (Buffer : Source_Buffer) return Marks_Lists.List;
   --  Return a full list of all multi cursor's marks.

   function Get_Multi_Cursors
     (Buffer : Source_Buffer) return Cursors_Lists.List;
   --  Return a full list of all multi cursor's marks.

   function Get_Multi_Cursors_Sync
     (Buffer : Source_Buffer) return Multi_Cursors_Sync_Type;

   procedure Set_Multi_Cursors_Sync
     (Buffer : Source_Buffer; Sync : Multi_Cursors_Sync_Type);

end Src_Editor_Buffer.Multi_Cursors;
