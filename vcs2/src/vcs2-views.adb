------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Intl;                    use GPS.Intl;
with Gtkada.Combo_Tool_Button;    use Gtkada.Combo_Tool_Button;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Widget;                  use Gtk.Widget;
with Histories;                   use Histories;
with Pango.Layout;                use Pango.Layout;
with VCS2.Engines;                use VCS2.Engines;

package body VCS2.Views is

   type Kernel_Combo_Tool_Record is new Gtkada_Combo_Tool_Button_Record with
      record
         Kernel : Kernel_Handle;
      end record;
   type Kernel_Combo_Tool is access all Kernel_Combo_Tool_Record'Class;
   procedure On_Active_VCS_Selected (Widget : access Gtk_Widget_Record'Class);
   --  Called when a new active VCS is selected by the user in the toolbar.
   --  This is *not* the same as the VCS_Active_Changed_Hook, which should be
   --  monitored to react to actual changes

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : access Base_VCS_View_Record'Class;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   ----------------------------
   -- On_Active_VCS_Selected --
   ----------------------------

   procedure On_Active_VCS_Selected
     (Widget : access Gtk_Widget_Record'Class)
   is
      Combo    : constant Kernel_Combo_Tool := Kernel_Combo_Tool (Widget);
      Selected : constant String := Combo.Get_Selected_Item;

      procedure On_VCS (VCS : not null access VCS_Engine'Class);
      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
         N : constant String :=
           VCS.Name & " (" & VCS.Working_Directory.Display_Full_Name & ")";
      begin
         if N = Selected then
            Set_Active_VCS (Combo.Kernel, VCS);
         end if;
      end On_VCS;

   begin
      For_Each_VCS (Combo.Kernel, On_VCS'Access);
   end On_Active_VCS_Selected;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Base_VCS_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
      Combo : Kernel_Combo_Tool;

      procedure On_VCS (VCS : not null access VCS_Engine'Class);
      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
         N : constant String :=
           VCS.Name & " (" & VCS.Working_Directory.Display_Full_Name & ")";
      begin
         Combo.Add_Item (N, Short_Name => VCS.Name);
         if VCS = Active_VCS (View.Kernel) then
            Combo.Select_Item (N);
         end if;
      end On_VCS;

   begin
      if VCS_Count (View.Kernel) > 1 then
         Combo := new Kernel_Combo_Tool_Record;
         Combo.Kernel := View.Kernel;
         Initialize (Combo, Icon_Name => "");

         Combo.Set_Tooltip_Text (-"Right-click to select the repository");
         Toolbar.Insert (Combo, 0);

         For_Each_VCS (View.Kernel, On_VCS'Access);

         Widget_Callback.Connect
           (Combo, Gtkada.Combo_Tool_Button.Signal_Selection_Changed,
            On_Active_VCS_Selected'Access);
      end if;

      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => Histories.History_Key
           (To_String (View.Filter_Hist_Prefix)),
         Tooltip     => -"Filter the contents",
         Placeholder => -"filter",
         Options     => View.Filter_Options);
   end Create_Toolbar;

   ----------------------------
   -- On_Preferenced_Changed --
   ----------------------------

   procedure On_Preferenced_Changed
     (Self    : not null access Base_VCS_View_Record;
      Pref    : Preference)
   is
   begin
      Set_Font_And_Colors (Self.Tree, Fixed_Font => True, Pref => Pref);

      if Pref = null
        or else Pref = Preference (Show_Ellipsis)
      then
         Set_Property
           (Self.Text_Render,
            Gtk.Cell_Renderer_Text.Ellipsize_Property,
            (if Show_Ellipsis.Get_Pref
             then Ellipsize_Middle else Ellipsize_None));
         Self.Queue_Resize;
         Self.Tree.Queue_Draw;
      end if;
   end On_Preferenced_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
   begin
      Self.View.On_Preferenced_Changed (Pref);
   end Execute;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self    : not null access Base_VCS_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class)
   is
      pragma Unreferenced (Child);
      P : access On_Pref_Changed;
   begin
      P := new On_Pref_Changed;
      P.View := Self;
      Preferences_Changed_Hook.Add (P, Watch => Self);
      P.Execute (Self.Kernel, null);   --   initial setup
   end On_Create;

end VCS2.Views;
