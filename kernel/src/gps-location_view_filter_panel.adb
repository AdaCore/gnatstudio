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

with GNAT.Regpat;
with Interfaces.C.Strings;

with Glib.Object;
with Gtk.Editable;
with Gtk.Enums; use Gtk.Enums;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Separator_Tool_Item;
with Gtk.Stock;
with Gtk.Toggle_Button;
with Gtk.Tool_Item;

with GPS.Intl; use GPS.Intl;
with GUI_Utils;
with Histories;

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

   Pattern_Key : constant Histories.History_Key := "locations_pattern";

   procedure On_Close
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on close button click

   procedure On_Apply
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on apply button click

   procedure On_Cancel
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on apply button click

   procedure On_Pattern_Activate
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on pattern entry activate

   procedure On_Pattern_Changed
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on pattern entry change

   procedure On_Reg_Exp_Toggle
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Locations_Filter_Panel);
   --  Called on regexp check button toggle

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
   begin
      if Self.Modified then
         GNAT.Strings.Free (Self.Old_Pattern);

         Self.Old_Pattern := new String'(Self.Pattern.Get_Entry.Get_Text);
         Self.Old_Reg_Exp := Self.Reg_Exp.Get_Active;

         GUI_Utils.Add_Unique_Combo_Entry
           (Self.Pattern, Self.Old_Pattern.all, Prepend => True);
         Histories.Add_To_History
           (Self.Kernel.Get_History.all, Pattern_Key, Self.Old_Pattern.all);

         Self.Applied := True;
         Self.Modified := False;

         Self.Apply.Set_Sensitive (Self.Modified);
         Self.Cancel.Set_Sensitive (Self.Applied);
         Self.Hide_Matched.Set_Sensitive (Self.Applied);

         Locations_Filter_Panel_Callbacks.Emit_By_Name
           (Self, Signal_Apply_Filter);
      end if;
   end Apply_Filter;

   --------------------
   -- Change_Pattern --
   --------------------

   procedure Change_Pattern
     (Self : not null access Locations_Filter_Panel_Record'Class)
   is
      Text  : constant String := Self.Pattern.Get_Entry.Get_Text;
      Valid : Boolean := True;

   begin
      if Self.Reg_Exp.Get_Active then
         begin
            declare
               Compiled : constant GNAT.Regpat.Pattern_Matcher :=
                 GNAT.Regpat.Compile (Text);
               pragma Unreferenced (Compiled);

            begin
               null;
            end;

         exception
            when GNAT.Regpat.Expression_Error =>
               Valid := False;
         end;
      end if;

      Self.Modified :=
        Valid
          and then Text /= ""
          and then (Self.Old_Pattern = null
                     or else Self.Old_Pattern.all /= Text
                     or else Self.Old_Reg_Exp /= Self.Reg_Exp.Get_Active);

      Self.Apply.Set_Sensitive (Self.Modified);
   end Change_Pattern;

   ----------------------
   -- Get_Hide_Matched --
   ----------------------

   function Get_Hide_Matched
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return Boolean is
   begin
      return Self.Hide_Matched.Get_Active;
   end Get_Hide_Matched;

   --------------------
   -- Get_Is_Reg_Exp --
   --------------------

   function Get_Is_Reg_Exp
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return Boolean is
   begin
      return Self.Old_Reg_Exp;
   end Get_Is_Reg_Exp;

   -----------------
   -- Get_Pattern --
   -----------------

   function Get_Pattern
     (Self : not null access Locations_Filter_Panel_Record'Class)
      return String
   is
   begin
      if Self.Old_Pattern /= null then
         return Self.Old_Pattern.all;

      else
         return "";
      end if;
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
      Separator : Gtk.Separator_Tool_Item.Gtk_Separator_Tool_Item;
      Label     : Gtk.Label.Gtk_Label;
      Item      : Gtk.Tool_Item.Gtk_Tool_Item;

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

      --  Filter label

      Gtk.Label.Gtk_New (Label, -"Filter:");
      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Label);
      Self.Insert (Item);

      --  Pattern combo box

      Gtk.Combo.Gtk_New (Self.Pattern);
      Self.Pattern.Set_Tooltip_Text
        (-"The text pattern or regular expression");
      Gtk_Entry_Callbacks.Connect
        (Self.Pattern.Get_Entry,
         Gtk.GEntry.Signal_Activate,
         Gtk_Entry_Callbacks.To_Marshaller (On_Pattern_Activate'Access),
         Locations_Filter_Panel (Self));
      Gtk_Entry_Callbacks.Connect
        (Self.Pattern.Get_Entry,
         Gtk.Editable.Signal_Changed,
         Gtk_Entry_Callbacks.To_Marshaller (On_Pattern_Changed'Access),
         Locations_Filter_Panel (Self));

      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Self.Pattern);
      Self.Insert (Item);

      --  RegExp check button

      Gtk.Check_Button.Gtk_New (Self.Reg_Exp, -"Regexp");
      Self.Reg_Exp.Set_Tooltip_Text (-"The filter is a regular expression");
      Gtk_Check_Button_Callbacks.Connect
        (Self.Reg_Exp,
         Gtk.Toggle_Button.Signal_Toggled,
         Gtk_Check_Button_Callbacks.To_Marshaller (On_Reg_Exp_Toggle'Access),
         Locations_Filter_Panel (Self));

      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Self.Reg_Exp);
      Self.Insert (Item);

      --  Apply filter button

      Gtk.Tool_Button.Gtk_New_From_Stock (Self.Apply, Gtk.Stock.Stock_Apply);
      Self.Apply.Set_Tooltip_Text (-"Apply filter");
      Gtk_Tool_Button_Callbacks.Connect
        (Self.Apply,
         Gtk.Tool_Button.Signal_Clicked,
         Gtk_Tool_Button_Callbacks.To_Marshaller (On_Apply'Access),
         Locations_Filter_Panel (Self));
      Self.Apply.Set_Sensitive (Self.Modified);
      Self.Insert (Self.Apply);

      --  Cancel filter button

      Gtk.Tool_Button.Gtk_New_From_Stock (Self.Cancel, Gtk.Stock.Stock_Cancel);
      Self.Cancel.Set_Tooltip_Text (-"Cancel currently applyed filter");
      Gtk_Tool_Button_Callbacks.Connect
        (Self.Cancel,
         Gtk.Tool_Button.Signal_Clicked,
         Gtk_Tool_Button_Callbacks.To_Marshaller (On_Cancel'Access),
         Locations_Filter_Panel (Self));
      Self.Cancel.Set_Sensitive (Self.Applied);
      Self.Insert (Self.Cancel);

      --  Separator

      Gtk.Separator_Tool_Item.Gtk_New (Separator);
      Self.Insert (Separator);

      --  Hide matched check button

      Gtk.Check_Button.Gtk_New (Self.Hide_Matched, "Hide matched");
      Self.Hide_Matched.Set_Tooltip_Text
        (-"Inverse filter: hide matched items");
      Gtk_Check_Button_Callbacks.Connect
        (Self.Hide_Matched,
         Gtk.Toggle_Button.Signal_Toggled,
         Gtk_Check_Button_Callbacks.To_Marshaller
           (On_Hide_Matched_Toggle'Access),
         Locations_Filter_Panel (Self));
      Self.Hide_Matched.Set_Sensitive (Self.Applied);

      Gtk.Tool_Item.Gtk_New (Item);
      Item.Add (Self.Hide_Matched);
      Self.Insert (Item);

      --  Initialize history

      Histories.Get_History
        (Kernel.Get_History.all, Pattern_Key, Self.Pattern);
   end Initialize;

   --------------
   -- On_Apply --
   --------------

   procedure On_Apply
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Apply_Filter;
   end On_Apply;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      if Self.Applied then
         Self.Applied := False;

         GNAT.Strings.Free (Self.Old_Pattern);
         Self.Change_Pattern;

         Self.Cancel.Set_Sensitive (Self.Applied);
         Self.Hide_Matched.Set_Sensitive (Self.Applied);

         Locations_Filter_Panel_Callbacks.Emit_By_Name
           (Self, Signal_Cancel_Filter);
      end if;
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
      if Self.Applied then
         Locations_Filter_Panel_Callbacks.Emit_By_Name
           (Self, Signal_Visibility_Toggled);
      end if;
   end On_Hide_Matched_Toggle;

   -------------------------
   -- On_Pattern_Activate --
   -------------------------

   procedure On_Pattern_Activate
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Apply_Filter;
   end On_Pattern_Activate;

   ------------------------
   -- On_Pattern_Changed --
   ------------------------

   procedure On_Pattern_Changed
     (Object : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Change_Pattern;
   end On_Pattern_Changed;

   -----------------------
   -- On_Reg_Exp_Toggle --
   -----------------------

   procedure On_Reg_Exp_Toggle
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Locations_Filter_Panel)
   is
      pragma Unreferenced (Object);

   begin
      Self.Change_Pattern;
   end On_Reg_Exp_Toggle;

end GPS.Location_View_Filter_Panel;
