------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Holders;

package Src_Editor_Buffer.Cursors is

   type Cursor (Is_Main_Cursor : Boolean) is record
      Buffer    : Source_Buffer;
      case Is_Main_Cursor is
         when True => null;
         when False =>
            Cursor_Id : Integer;
            Cursor    : Slave_Cursor_Access;
      end case;
   end record;

   package Cursors_Holders is new Ada.Containers.Indefinite_Holders (Cursor);
   use Cursors_Holders;
   function Holder
     (C : Cursor) return Holder renames Cursors_Holders.To_Holder;

   Nil_Cursor : Cursor := (False, null, -1, null);

   function Get_Main_Cursor (B : Source_Buffer) return Cursor is (True, B);

   function Create
     (C : Slave_Cursor_Access; Buffer : Source_Buffer) return Cursor;

   function Is_Alive (C : Cursor) return Boolean;
   --  This function returns true when a cursor is alive and operations can be
   --  conducted on it. It returns false if the cursor has been deleted

   package Cursors_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
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

   procedure Add_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter);
   function Add_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter) return Cursor;
   --  Add a new multi cursor at the specified location
   --  in the specified buffer

   procedure Delete_Cursor
     (Buffer : Source_Buffer; Location : Gtk_Text_Iter);
   --  Delete a multi cursor at the specified location in the specified buffer

   procedure Remove_All_Slave_Cursors (Buffer : Source_Buffer);
   --  Remove all multi cursors from the current buffer

   procedure Set_Manual_Sync
     (C : Cursor)
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

   procedure Set_Cursors_Auto_Sync (Buffer : Source_Buffer);
   --  This sets the buffer in auto mode regarding multi cursor insertion.
   --  This means that every insert/delete will impact every active cursors
   --  in the buffer. Do not forget to set that back after a manual multi
   --  cursor operation !

   function Get_Cursors
     (Buffer : Source_Buffer) return Cursors_Lists.List;
   --  Return a full list of all multi cursor's marks.

   function Has_Slave_Cursors
     (Buffer : Source_Buffer) return Boolean;

   procedure Move
     (C : Cursor; Loc : Gtk_Text_Iter; Extend_Selection : Boolean);

   function Get_Cursors_Sync
     (Buffer : Source_Buffer) return Cursors_Sync_Type;
   --  This function and its counter part setter are meant to save and restore
   --  the synchronization at a given point.

   procedure Set_Cursors_Sync
     (Buffer : Source_Buffer; Sync : Cursors_Sync_Type);
   --  This function and its counter part getter are meant to save and restore
   --  the synchronization at a given point.

end Src_Editor_Buffer.Cursors;
