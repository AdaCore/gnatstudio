------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

with Gdk.Device;
with Glib;                     use Glib;
with Glib.Main;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Tool_Button;

package Gtkada.Combo_Tool_Button is

   type Gtkada_Combo_Tool_Button_Record is new
     Gtk.Tool_Button.Gtk_Tool_Button_Record with private;
   type Gtkada_Combo_Tool_Button is
     access all Gtkada_Combo_Tool_Button_Record'Class;

   procedure Gtk_New
     (Self          : out Gtkada_Combo_Tool_Button;
      Icon_Name     : String;
      Click_Pops_Up : Boolean := False);
   procedure Initialize
     (Self          : access Gtkada_Combo_Tool_Button_Record'Class;
      Icon_Name     : String;
      Click_Pops_Up : Boolean := False);
   --  Create or initialize a button from a stock icon (see gtk-stock.ads).
   --  Leave Icon_Name to the empty string to display text instead.
   --
   --  The default behavior is to emit the Signal_Clicked signal when the user
   --  does a short click on the button, and popup the menu on long clicks.
   --  However, if you set Click_Pops_Up to True, then the signal
   --  Signal_Clicked is never executed, and instead the menu is displayed even
   --  on short clicks.

   type User_Data_Record is abstract tagged null record;
   type User_Data is access all User_Data_Record'Class;
   --  Some data that are attached to selectable items. These can be easily
   --  retrieved on the selected item.

   procedure Add_Item
     (Widget     : access Gtkada_Combo_Tool_Button_Record;
      Item       : String;
      Icon_Name  : String := "";
      Data       : User_Data := null;
      Short_Name : String := "");
   --  Add an item in the button items list.
   --
   --  Short_Name will be displayed in the button when no icon is defined for
   --  both the item and the button itself. It defaults to Item, but should be
   --  short.

   procedure Select_Item
     (Widget      : access Gtkada_Combo_Tool_Button_Record;
      Item        : String);
   --  Select Item from the list.
   --  Emits the Signal_Selection_Changed to let listeners
   --  know of the change.

   function Get_Selected_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record) return String;
   --  Get the currently selected item

   function Get_Selected_Item_Data
     (Widget : access Gtkada_Combo_Tool_Button_Record)
      return User_Data;
   --  Get the data attached to the selected item

   procedure Clear_Items
     (Widget : access Gtkada_Combo_Tool_Button_Record);
   --  Clear the Items list

   procedure Remove_If
     (Self : not null access Gtkada_Combo_Tool_Button_Record;
      Predicate : not null access function
        (Item : String; Data : User_Data) return Boolean);
   --  Remove the items for which the predicate is True

   function Has_Items
     (Self : not null access Gtkada_Combo_Tool_Button_Record)
      return Boolean;
   --  Whether at least one item is registered

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "clicked"
   --    procedure Handler
   --     (Button : access Gtkada_Combo_Tool_Button_Record'Class);
   --    Emitted when the button has been clicked on by the user. This is the
   --    signal you should use to start your own actions.
   --
   --  - "selection_changed"
   --    procedure Handler
   --     (Button : access Gtkada_Combo_Tool_Button_Record'Class);
   --    Emitted when the selected item has been changed by the user.
   --
   --  </signals>

   Signal_Clicked : constant Glib.Signal_Name := "clicked";
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection_changed";

private

   type Item_Record is record
      Icon_Name  : Unbounded_String;
      Full_Name  : Unbounded_String;
      Short_Name : Unbounded_String;
      Data       : User_Data;
   end record;

   package Item_Vector is new Ada.Containers.Vectors (Natural, Item_Record);

   type Gtkada_Combo_Tool_Button_Record is new
     Gtk.Tool_Button.Gtk_Tool_Button_Record
    with record
      Items       : Item_Vector.Vector;
      Selected    : Unbounded_String;

      Menu        : Gtk_Menu;
      --  null if the menu is not visible on the screen

      Click_Pops_Up : Boolean;

      Icon_Name    : Unbounded_String;
      --  The default icon name, when items do not provide one

      Popup_Timeout : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Popup_Device  : Gdk.Device.Gdk_Device;
      Popup_Time    : Glib.Guint32;
   end record;

end Gtkada.Combo_Tool_Button;
