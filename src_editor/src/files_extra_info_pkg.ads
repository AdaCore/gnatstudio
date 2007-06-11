-----------------------------------------------------------------------
--                               G P S                               --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Box;   use Gtk.Box;
with Gtk.Table; use Gtk.Table;
with Gtk.Label; use Gtk.Label;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.Button; use Gtk.Button;
with GPS.Kernel;
package Files_Extra_Info_Pkg is

   type Files_Extra_Info_Record is new Gtk_Box_Record with record
      --  Files_Frame : Gtk_Frame;
      Files_Table : Gtk_Table;
      Files_Label : Gtk_Label;
      Directory_Label : Gtk_Label;
      Files_Combo : Gtk_Combo;
      Files_Entry : Gtk_Entry;
      Directory_Combo : Gtk_Combo;
      Directory_Entry : Gtk_Entry;
      Browse_Button : Gtk_Button;
      Subdirs_Check : Gtk_Check_Button;
   end record;
   type Files_Extra_Info_Access is access all Files_Extra_Info_Record'Class;

   procedure Gtk_New
     (Files_Extra_Info : out Files_Extra_Info_Access;
      Handle : access GPS.Kernel.Kernel_Handle_Record'Class);
   procedure Initialize
     (Files_Extra_Info : access Files_Extra_Info_Record'Class;
      Handle : access GPS.Kernel.Kernel_Handle_Record'Class);

end Files_Extra_Info_Pkg;
