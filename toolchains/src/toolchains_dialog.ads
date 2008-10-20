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

with Gtk.Button;  use Gtk.Button;
with Gtk.Dialog;  use Gtk.Dialog;
with Gtk.Frame;   use Gtk.Frame;
with Gtk.GEntry;  use Gtk.GEntry;

with GPS.Kernel;  use GPS.Kernel;

package Toolchains_Dialog is

   type Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with private;
   type Dialog is access all Dialog_Record;

   procedure Gtk_New
     (Widget            : out Dialog;
      Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Active            : Boolean;
      Tools_Path        : String;
      Use_Xrefs_Subdirs : Boolean;
      Compiler_Path     : String);

   function Get_Active
     (Widget : access Dialog_Record'Class) return Boolean;
   --  Whether the dual compilation mode should be activated

   function Get_Use_Xrefs_Subdir
     (Widget : access Dialog_Record'Class) return Boolean;
   --  Whether we should read ali files from a separate xrefs subdir

   function Get_Tools_Path
     (Widget : access Dialog_Record'Class) return String;
   --  Get the path where to find the Bleeding edge compiler

   function Get_Compiler_Path
     (Widget : access Dialog_Record'Class) return String;
   --  Get the path where to find the regular compiler

private

   type Dialog_Record is new Gtk.Dialog.Gtk_Dialog_Record with record
      Kernel         : Kernel_Handle;
      OK_Button      : Gtk_Button;
      Active         : Boolean;
      Xrefs_Subdir   : Boolean;
      Frame          : Gtk.Frame.Gtk_Frame;
      Tools_Entry    : Gtk.GEntry.Gtk_Entry;
      Compiler_Entry : Gtk.GEntry.Gtk_Entry;
   end record;

end Toolchains_Dialog;
