-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Button;           use Gtk.Button;
with Gtk.Check_Button;     use Gtk.Check_Button;
with Gtk.Clist;            use Gtk.Clist;
with Gtk.Dialog;           use Gtk.Dialog;
with Gtk.GEntry;           use Gtk.GEntry;
with Gtk.Label;            use Gtk.Label;

with GPS.Kernel;

package Make_Suite_Window_Pkg is

   type Make_Suite_Window_Record is new Gtk_Dialog_Record with record
      Kernel           : GPS.Kernel.Kernel_Handle;
      Directory_Entry  : Gtk_Entry;
      Name_Entry       : Gtk_Entry;
      Test_List        : Gtk_Clist;
      Browse_Directory : Gtk_Button;
      Add              : Gtk_Button;
      Remove           : Gtk_Button;
      Aunit1_Button    : Gtk_Check_Button;
      Label            : Gtk_Label;
   end record;
   type Make_Suite_Window_Access is access all Make_Suite_Window_Record'Class;

   procedure Gtk_New
     (Make_Suite_Window : out Make_Suite_Window_Access;
      Handle            : GPS.Kernel.Kernel_Handle);

   procedure Initialize
     (Make_Suite_Window : access Make_Suite_Window_Record'Class;
      Handle            : GPS.Kernel.Kernel_Handle);

end Make_Suite_Window_Pkg;
