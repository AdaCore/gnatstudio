-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
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
with Gtk.Box;
with Gtk.Combo;
with Gtk.GEntry;
with Gtk.Tree_Store;
with Gtk.Tree_View;

package Gtkada.Entry_Completion is

   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Gtkada_Entry is access all Gtkada_Entry_Record'Class;

   procedure Gtk_New
     (The_Entry : out Gtkada_Entry;
      Use_Combo : Boolean := True);
   --  Create a new entry. If Use_Combo is true, then the top field will be
   --  a combo box, to which you can associate a history. Otherwise, it will
   --  be a simple entry field.

   procedure Initialize
     (The_Entry : access Gtkada_Entry_Record'Class;
      Use_Combo : Boolean := True);
   --  Internal procedure

   function Get_Combo (The_Entry : access Gtkada_Entry_Record)
      return Gtk.Combo.Gtk_Combo;
   --  Return the combo box used to store the history of previously selected
   --  values through this completion entry. It is the responsability of the
   --  user to add new items in this history list.
   --  This returns null if the widget wasn't created as a combo.

   function Get_Entry (The_Entry : access Gtkada_Entry_Record)
      return Gtk.GEntry.Gtk_Entry;
   --  Return the top entry, possibly the one inside the combo if there is a
   --  combo

   procedure Set_Completions
     (The_Entry   : access Gtkada_Entry_Record;
      Completions : Basic_Types.String_Array_Access);
   --  Set the possible completions for The_Entry.
   --  Completions should not be freed by the caller, it is taken care of by
   --  this widget.

private
   type Gtkada_Entry_Record is new Gtk.Box.Gtk_Box_Record with record
      Combo            : Gtk.Combo.Gtk_Combo;
      GEntry           : Gtk.GEntry.Gtk_Entry;
         --  null if a combo was created.
      Completions      : Basic_Types.String_Array_Access;
      Last_Position    : Integer;
      Completion_Index : Integer := Integer'First;
      --  The index, in Completions, of the last string inserted. 0 if no
      --  completion has been attempted yet.

      View             : Gtk.Tree_View.Gtk_Tree_View;
      List             : Gtk.Tree_Store.Gtk_Tree_Store;
      --  The widget that displays the list of possible completions
   end record;

end Gtkada.Entry_Completion;
