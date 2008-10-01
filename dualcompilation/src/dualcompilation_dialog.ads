-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2008, AdaCore                  --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Glib.Object; use Glib.Object;
with Gtk.Dialog;  use Gtk.Dialog;
with Gtk.Frame;   use Gtk.Frame;
with Gtk.GEntry;  use Gtk.GEntry;

package Dualcompilation_Dialog is

   type Dualc_Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with private;
   type Dualc_Dialog is access all Dualc_Dialog_Record;

   procedure Gtk_New
     (Widget        : out Dualc_Dialog;
      Parent        : access GObject_Record'Class;
      Active        : Boolean;
      Tools_Path    : String;
      Compiler_Path : String);

   function Get_Active
     (Widget : access Dualc_Dialog_Record'Class) return Boolean;

   function Get_Tools_Path
     (Widget : access Dualc_Dialog_Record'Class) return String;

   function Get_Compiler_Path
     (Widget : access Dualc_Dialog_Record'Class) return String;

private

   type Dualc_Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Active         : Boolean;
      Frame          : Gtk.Frame.Gtk_Frame;
      Tools_Entry    : Gtk.GEntry.Gtk_Entry;
      Compiler_Entry : Gtk.GEntry.Gtk_Entry;
   end record;

end Dualcompilation_Dialog;
