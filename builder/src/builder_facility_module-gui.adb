------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.Widget;                  use Gtk.Widget;
with Gtk.Window;                  use Gtk.Window;

with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;

with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;
with Default_Preferences;         use Default_Preferences;
with Default_Preferences.GUI;     use Default_Preferences.GUI;
with Dialog_Utils;                use Dialog_Utils;
with Informational_Popups;        use Informational_Popups;

package body Builder_Facility_Module.GUI is

   type Targets_Editor_Preferences_Page_Record is new Preferences_Page_Record
     with null record;
   type Targets_Editor_Preferences_Page is
     access all Targets_Editor_Preferences_Page_Record'Class;

   overriding function Get_Widget
     (Self    : not null access Targets_Editor_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;

   type Targets_Editor_Preferences_Page_View_Record is
     new Preferences_Page_View_Record with record
      Config_UI : Configuration_UI_Access;
   end record;
   type Targets_Editor_Preferences_Page_View is
     access all Targets_Editor_Preferences_Page_View_Record;

   overriding function Needs_Apply_Button
     (Self : not null access Targets_Editor_Preferences_Page_View_Record)
      return Boolean
   is
      (True);

   overriding procedure On_Apply_Button_Clicked
     (Self : not null access Targets_Editor_Preferences_Page_View_Record);

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Targets_Editor_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Self, Manager);
      Page_View : constant Targets_Editor_Preferences_Page_View :=
                    new Targets_Editor_Preferences_Page_View_Record;
   begin
      Dialog_Utils.Initialize (Page_View);

      Gtk_New (Page_View.Config_UI,
               Builder_Facility_Module.Registry,
               View_Fixed_Font.Get_Pref);
      Page_View.Append (Page_View.Config_UI);

      return Gtk_Widget (Page_View);
   end Get_Widget;

   -----------------------------
   -- On_Apply_Button_Clicked --
   -----------------------------

   overriding procedure On_Apply_Button_Clicked
     (Self : not null access Targets_Editor_Preferences_Page_View_Record) is
   begin
      Self.Config_UI.Apply_Changes;
      Refresh_All_Graphical_Elements;

      Display_Informational_Popup
        (Parent                => Gtk_Window (Self.Config_UI.Get_Toplevel),
         Icon_Name             => "vcs-up-to-date",
         Text                  => "Build Targets have been saved",
         No_Transparency_Color => Default_Style.Get_Pref_Bg);
   end On_Apply_Button_Clicked;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Manager             : constant Preferences_Manager :=
                              Kernel.Get_Preferences;
      Targets_Editor_Page : constant Targets_Editor_Preferences_Page :=
                              new Targets_Editor_Preferences_Page_Record;
   begin
      Manager.Register_Page
        (Name => Build_Targets_Page_Name,
         Page => Preferences_Page (Targets_Editor_Page));
   end Register_Module;

end Builder_Facility_Module.GUI;
