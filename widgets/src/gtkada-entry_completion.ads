-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Basic_Types;
with Gtk.GEntry;

package Gtkada.Entry_Completion is

   type Gtkada_Entry_Record is new Gtk.GEntry.Gtk_Entry_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New (The_Entry : out Gtkada_Entry);
   --  Create a new entry.

   procedure Initialize (The_Entry : access Gtkada_Entry_Record'Class);
   --  Internal procedure

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : Basic_Types.String_Array_Access);
   --  Set the possible completions for The_Entry.
   --  Completions should not be freed by the caller, it is taken care of by
   --  this widget.

private
   type Gtkada_Entry_Record is new Gtk.GEntry.Gtk_Entry_Record with record
      Completions      : Basic_Types.String_Array_Access;
      Last_Position    : Integer;
      Completion_Index : Integer := Integer'First;
      --  The index, in Completions, of the last string inserted. 0 if no
      --  completion has been attempted yet.
   end record;

end Gtkada.Entry_Completion;
