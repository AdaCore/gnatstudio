------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

   type Cursor is record
      Cursor_Id : Integer;
      Cursor    : Multi_Cursor_Access;
      Buffer    : Source_Buffer;
   end record;

   Nil_Cursor : Cursor := (-1, null, null);

   function Create
     (C : Multi_Cursor_Access; Buffer : Source_Buffer) return Cursor;

   function Is_Alive (C : Cursor) return Boolean;
   --  This function returns true when a cursor is alive and operations can be
   --  conducted on it. It returns false if the cursor has been deleted

   package Cursors_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Cursor);

   function Get_Mark (C : Cursor) return Gtk_Text_Mark
     with Pre => (Is_Alive (C));
   --  Return the insert mark of the cursor given in argument

   function Get_Sel_Mark (C : Cursor) return Gtk_Text_Mark
     with Pre => (Is_Alive (C));
   --  Return the Selection mark of the cursor given in argument

   function Get_Column_Memory (C : Cursor) return Gint
     with Pre => (Is_Alive (C));

   procedure Set_Column_Memory (C : Cursor; Offset : Gint)
     with Pre => (Is_Alive (C));

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
     (C       : Cursor)
     with Pre => (Is_Alive (C));
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
      MC     : Multi_Cursor_Access);

   procedure Set_Multi_Cursors_Auto_Sync (Buffer : Source_Buffer);
   --  This sets the buffer in auto mode regarding multi cursor insertion.
   --  This means that every insert/delete will impact every active cursors
   --  in the buffer. Do not forget to set that back after a manual multi
   --  cursor operation !

   function Get_Multi_Cursors
     (Buffer : Source_Buffer) return Cursors_Lists.List;
   --  Return a full list of all multi cursor's marks.

   function Get_Multi_Cursors_Sync
     (Buffer : Source_Buffer) return Multi_Cursors_Sync_Type;
   --  This function and its counter part setter are meant to save and restore
   --  the synchronization at a given point.

   procedure Set_Multi_Cursors_Sync
     (Buffer : Source_Buffer; Sync : Multi_Cursors_Sync_Type);
   --  This function and its counter part getter are meant to save and restore
   --  the synchronization at a given point.

end Src_Editor_Buffer.Multi_Cursors;
