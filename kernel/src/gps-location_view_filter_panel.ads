-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
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

with GNAT.Strings;

with Glib;
with Gtk.Check_Button;
with Gtk.Combo;
with Gtk.Tool_Button;
with Gtk.Toolbar;

with GPS.Kernel;

package GPS.Location_View_Filter_Panel is

   type Locations_Filter_Panel_Record is
     new Gtk.Toolbar.Gtk_Toolbar_Record with private;

   type Locations_Filter_Panel is
     access all Locations_Filter_Panel_Record'Class;

   procedure Gtk_New
     (Panel  : in out Locations_Filter_Panel;
      Kernel : GPS.Kernel.Kernel_Handle);

   procedure Initialize
     (Self   : not null access Locations_Filter_Panel_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);

   function Get_Pattern
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return String;
   --  Returns current filter text or regular expression

   function Get_Is_Reg_Exp
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return Boolean;
   --  Returns True if current filter is a regular expression

   function Get_Hide_Matched
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return Boolean;
   --  Returns True if matched items must be hidden

   Signal_Apply_Filter       : constant Glib.Signal_Name;
   --  Emitted when user apply new filter parameters

   Signal_Cancel_Filter      : constant Glib.Signal_Name;
   --  Emitted then user cancel any filtering

private

   type Locations_Filter_Panel_Record is
     new Gtk.Toolbar.Gtk_Toolbar_Record with record
      Kernel       : GPS.Kernel.Kernel_Handle;

      Pattern      : Gtk.Combo.Gtk_Combo;
      Reg_Exp      : Gtk.Check_Button.Gtk_Check_Button;
      Show_Matched : Gtk.Tool_Button.Gtk_Tool_Button;
      Hide_Matched : Gtk.Tool_Button.Gtk_Tool_Button;
      Cancel       : Gtk.Tool_Button.Gtk_Tool_Button;

      Old_Pattern  : GNAT.Strings.String_Access;
      Old_Reg_Exp  : Boolean := False;
      --  Old parameters of the filter

      Show_Matched_Applied : Boolean := False;
      --  Filter has been applied
      Hide_Matched_Applied : Boolean := False;
      --  Reverse filter has been applied
      Modified             : Boolean := False;
      --  Filter parameters have been modified
   end record;

   Signal_Apply_Filter       : constant Glib.Signal_Name := "apply-filter";
   Signal_Cancel_Filter      : constant Glib.Signal_Name := "cancel-filter";

   procedure Apply_Filter
     (Self   : not null access Locations_Filter_Panel_Record'Class;
      Revert : Boolean);

   procedure Change_Pattern
     (Self : not null access Locations_Filter_Panel_Record'Class);

end GPS.Location_View_Filter_Panel;
