------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Glib;

with Gtk.Box;          use Gtk.Box;
with Gtk.Button;       use Gtk.Button;
with Gtk.Check_Button; use Gtk.Check_Button;
with Gtk.GEntry;       use Gtk.GEntry;
with Gtk.Label;        use Gtk.Label;
with Gtk.Table;        use Gtk.Table;
with GPS.Kernel;

package Files_Extra_Info_Pkg is

   type Files_Extra_Info_Record is new Gtk_Box_Record with record
      --  Files_Frame : Gtk_Frame;
      Files_Table     : Gtk_Table;
      Files_Label     : Gtk_Label;
      Directory_Label : Gtk_Label;
      Files_Entry     : Gtk_Entry;
      Directory_Entry : Gtk_Entry;
      Browse_Button   : Gtk_Button;
      Subdirs_Check   : Gtk_Check_Button;
   end record;
   type Files_Extra_Info_Access is access all Files_Extra_Info_Record'Class;

   procedure Gtk_New
     (Files_Extra_Info : out Files_Extra_Info_Access;
      Handle : access GPS.Kernel.Kernel_Handle_Record'Class);
   procedure Initialize
     (Files_Extra_Info : access Files_Extra_Info_Record'Class;
      Handle           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Start_Row_Number : Glib.Guint);

end Files_Extra_Info_Pkg;
