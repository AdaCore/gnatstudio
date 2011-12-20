------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Glib;                     use Glib;
with Gtk.Arrow;                use Gtk.Arrow;
with Gtk.Button;               use Gtk.Button;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Fixed;                use Gtk.Fixed;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Tool_Item;

package Gtkada.Combo_Tool_Button is

   type Gtkada_Combo_Tool_Button_Record is new
     Gtk.Tool_Item.Gtk_Tool_Item_Record with private;
   type Gtkada_Combo_Tool_Button is
     access all Gtkada_Combo_Tool_Button_Record'Class;

   procedure Gtk_New
     (Button       : out Gtkada_Combo_Tool_Button;
      Stock_Id     : String;
      Default_Size : Gtk_Icon_Size := Icon_Size_Large_Toolbar);

   procedure Initialize
     (Button       : access Gtkada_Combo_Tool_Button_Record'Class;
      Stock_Id     : String;
      Default_Size : Gtk_Icon_Size := Icon_Size_Large_Toolbar);
   --  Create or initialize a button from a stock icon (see gtk-stock.ads)

   type User_Data_Record is abstract tagged null record;
   type User_Data is access all User_Data_Record'Class;
   --  Some data that are attached to selectable items. These can be easily
   --  retrieved on the selected item.

   procedure Add_Item
     (Widget   : access Gtkada_Combo_Tool_Button_Record;
      Item     : String;
      Stock_Id : String := "";
      Data     : User_Data := null);
   --  Add an item in the button items list.

   procedure Select_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record;
      Item   : String);
   --  Select Item from the list.

   function Get_Selected_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record) return String;
   --  Get the currently selected item

   function Get_Selected_Item_Data
     (Widget : access Gtkada_Combo_Tool_Button_Record)
      return User_Data;
   --  Get the data attached to the selected item.

   procedure Clear_Items
     (Widget : access Gtkada_Combo_Tool_Button_Record);
   --  Clear the Items list.

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

   Signal_Clicked           : constant Glib.Signal_Name := "clicked";
   Signal_Selection_Changed : constant Glib.Signal_Name := "selection_changed";

private

   package Strings_Vector is new Ada.Containers.Vectors
     (Natural, Ada.Strings.Unbounded.Unbounded_String);

   type Gtkada_Combo_Tool_Button_Record is new
     Gtk.Tool_Item.Gtk_Tool_Item_Record with record
      Items       : Strings_Vector.Vector;
      Selected    : Strings_Vector.Extended_Index;
      Arrow       : Gtk_Arrow;
      Fixed       : Gtk_Fixed;
      Menu_Button : Gtk_Toggle_Button;
      Icon_Button : Gtk_Button;
      Menu        : Gtk_Menu;
      Stock_Id    : Unbounded_String;
   end record;

end Gtkada.Combo_Tool_Button;
