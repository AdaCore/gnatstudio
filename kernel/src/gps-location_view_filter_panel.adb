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
with Interfaces.C.Strings;

with Glib.Object;
with Gtk.Editable;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Image;
with Gtk.Stock;
with Gtk.Toggle_Button;
with Gtk.Tool_Item;

with GPS.Intl; use GPS.Intl;

package body GPS.Location_View_Filter_Panel is

   use type GNAT.Strings.String_Access;

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array (1 .. 3) :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Apply_Filter)),
      2 => Interfaces.C.Strings.New_String (String (Signal_Cancel_Filter)),
      3 => Interfaces.C.Strings.New_String
            (String (Signal_Visibility_Toggled)));

   Signals_Parameters : constant
     Glib.Object.Signal_Parameter_Types (1 .. 3, 1 .. 1) :=
       (1 => (others => Glib.GType_None),
        2 => (others => Glib.GType_None),
        3 => (others => Glib.GType_None));

   procedure On_Close
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on close button click

   procedure On_Cancel
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on Cancel button

   procedure On_Pattern_Changed
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on pattern entry change

   procedure On_Hide_Matched_Toggle
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on hide matched toggle

   package Gtk_Entry_Callbacks is
     new Gtk.Handlers.User_Callback
          (Gtk.GEntry.Gtk_Entry_Record, Locations_Filter_Panel);

   package Gtk_Check_Button_Callbacks is
     new Gtk.Handlers.User_Callback
          (Gtk.Check_Button.Gtk_Check_Button_Record, Locations_Filter_Panel);

   package Gtk_Tool_Button_Callbacks is
     new Gtk.Handlers.User_Callback
          (Gtk.Tool_Button.Gtk_Tool_Button_Record, Locations_Filter_Panel);

   package Locations_Filter_Panel_Callbacks is
     new Gtk.Handlers.Callback (Locations_Filter_Panel_Record);

   ------------------
   -- Apply_Filter --
   ------------------

   procedure Apply_Filter
     (Self : not null access Locations_Filter_Panel_Record'Class)
   is
      Pattern : constant String := Self.Pattern.Get_Text;
   begin
      if Pattern = "" then
         Locations_Filter_Panel_Callbacks.Emit_By_Name
           (Self, Signal_Cancel_Filter);
      else
         Locations_Filter_Panel_Callbacks.Emit_By_Name
           (Self, Signal_Apply_Filter);
      end if;
   end Apply_Filter;

   ----------------------
   -- Get_Hide_Matched --
   ----------------------

   function Get_Hide_Matched
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return Boolean is
   begin
      return Self.Hide_Matched.Get_Active;
   end Get_Hide_Matched;

   -------------------
   -- Get_Is_Regexp --
   -------------------

   function Get_Is_Regexp
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return Boolean is
   begin
      return Self.Regexp.Get_Active;
   end Get_Is_Regexp;

   -----------------
   -- Get_Pattern --
   -----------------

   function Get_Pattern
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return String
   is
   begin
      return Self.Pattern.Get_Text;
   end Get_Pattern;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Panel  : in out Locations_Filter_Panel;
      Kernel : GPS.Kernel.Kernel_Handle) is
   begin
      Panel := new Locations_Filter_Panel_Record;
      Initialize (Panel, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Locations_Filter_Panel_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      Close     : Gtk.Tool_Button.Gtk_Tool_Button;
      Item      : Gtk.Tool_Item.Gtk_Tool_Item;
      Find      : Gtk.Image.Gtk_Image;

   begin
      Gtk.Toolbar.Initialize (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "GPSLocationViewFilterPanel",
         Signals_Parameters);

      Self.Set_Icon_Size (Icon_Size_Small_Toolbar);
      Self.Set_Style (Toolbar_Icons);

      Self.Kernel := Kernel;

      --  Close button

      Gtk.Tool_Button.Gtk_New_From_Stock (Close, Gtk.Stock.Stock_Close);
      Close.Set_Tooltip_Text (-"Close filter panel");
      Gtk_Tool_Button_Callbacks.Connect
        (Close,
         Gtk.Tool_Button.Signal_Clicked,
         Gtk_Tool_Button_Callbacks.To_Marshaller (On_Close'Access),
         Locations_Filter_Panel (Self));
      Self.Insert (Close);

      --  Filter icon

      Gtk.Image.Gtk_New_From_Icon_Name
        (Find, Gtk.Stock.Stock_Find, Icon_Size_Button);
      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Find);
      Self.Insert (Item);

      --  Pattern entry

      Gtk.GEntry.Gtk_New (Self.Pattern);
      Self.Pattern.Set_Tooltip_Text
        (-"The text pattern or regular expression");
      Gtk_Entry_Callbacks.Connect
        (Self.Pattern,
         Gtk.Editable.Signal_Changed,
         Gtk_Entry_Callbacks.To_Marshaller (On_Pattern_Changed'Access),
         Locations_Filter_Panel (Self));

      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Self.Pattern);
      Self.Insert (Item);

      --  Regexp check button

      Gtk.Check_Button.Gtk_New (Self.Regexp, -"Regexp");
      Self.Regexp.Set_Tooltip_Text
        (-"Whether filter is a regular expression");

      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Self.Regexp);
      Self.Insert (Item);

      --  Hide matched check button

      Gtk.Check_Button.Gtk_New (Self.Hide_Matched, "Hide matches");
      Self.Hide_Matched.Set_Tooltip_Text
        (-"Revert filter: hide matching items");
      Gtk_Check_Button_Callbacks.Connect
        (Self.Hide_Matched,
         Gtk.Toggle_Button.Signal_Toggled,
         Gtk_Check_Button_Callbacks.To_Marshaller
           (On_Hide_Matched_Toggle'Access),
         Locations_Filter_Panel (Self));

      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Self.Hide_Matched);
      Self.Insert (Item);

      --  Cancel filter button

      Gtk.Tool_Button.Gtk_New_From_Stock (Self.Cancel, Gtk.Stock.Stock_Cancel);
      Self.Cancel.Set_Tooltip_Text (-"Cancel current filter");
      Gtk_Tool_Button_Callbacks.Connect
        (Self.Cancel,
         Gtk.Tool_Button.Signal_Clicked,
         Gtk_Tool_Button_Callbacks.To_Marshaller (On_Cancel'Access),
         Locations_Filter_Panel (Self));
      Self.Insert (Self.Cancel);
   end Initialize;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Pattern.Set_Text ("");
   end On_Cancel;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Pattern.Set_Text ("");
      Self.Hide;
   end On_Close;

   ----------------------------
   -- On_Hide_Matched_Toggle --
   ----------------------------

   procedure On_Hide_Matched_Toggle
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      if Self.Pattern.Get_Text /= "" then
         Locations_Filter_Panel_Callbacks.Emit_By_Name
           (Self, Signal_Visibility_Toggled);
      end if;
   end On_Hide_Matched_Toggle;

   ------------------------
   -- On_Pattern_Changed --
   ------------------------

   procedure On_Pattern_Changed
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Apply_Filter;
   end On_Pattern_Changed;

end GPS.Location_View_Filter_Panel;
