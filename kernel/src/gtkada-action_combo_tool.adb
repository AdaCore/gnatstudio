------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtk.Widget;              use Gtk.Widget;

package body Gtkada.Action_Combo_Tool is

   type Action_User_Data is new Gtkada.Combo_Tool_Button.User_Data_Record with
      record
         Action : Unbounded_String;
      end record;
   type Action_User_Data_Access is access all Action_User_Data'Class;

   procedure On_Selection (Widget : access Gtk_Widget_Record'Class);
   procedure On_Clicked (Widget : access Gtk_Widget_Record'Class);
   --  Handling the signals of a Combo_Tool_Button

   ------------------
   -- On_Selection --
   ------------------

   procedure On_Selection (Widget : access Gtk_Widget_Record'Class) is
      Combo : constant Action_Combo_Tool := Action_Combo_Tool (Widget);
      Data  : constant Action_User_Data_Access :=
        Action_User_Data_Access (Get_Selected_Item_Data (Combo));
      Act   : constant Action_Record_Access :=
        Lookup_Action (Combo.Kernel, To_String (Data.Action));
   begin
      if Act = null then
         Combo.Set_Tooltip_Text ("");
      else
         Combo.Set_Tooltip_Markup (Get_Full_Description (Act, Combo.Kernel));
         Combo.Set_Icon_Name (Get_Icon_Name (Act));
      end if;
   end On_Selection;

   ----------------
   -- On_Clicked --
   ----------------

   procedure On_Clicked (Widget : access Gtk_Widget_Record'Class) is
      Combo : constant Action_Combo_Tool := Action_Combo_Tool (Widget);
      Data  : constant Action_User_Data_Access :=
        Action_User_Data_Access (Get_Selected_Item_Data (Combo));
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Success := Execute_Action
        (Combo.Kernel,
         Action => To_String (Data.Action));
   end On_Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self   : out Action_Combo_Tool;
      Kernel : not null access Kernel_Handle_Record'Class;
      Initial_Label  : String;
      Initial_Action : String)
   is
      Act   : constant Action_Record_Access :=
        Lookup_Action (Kernel, Initial_Action);
   begin
      Self := new Action_Combo_Tool_Record;
      Self.Kernel := Kernel;

      if Act = null then
         Gtkada.Combo_Tool_Button.Initialize (Self, Icon_Name => "");
      else
         Gtkada.Combo_Tool_Button.Initialize
           (Self, Icon_Name => Get_Icon_Name (Act));
      end if;

      Widget_Callback.Connect
        (Self, Signal_Selection_Changed, On_Selection'Access);
      Widget_Callback.Connect (Self, Signal_Clicked, On_Clicked'Access);

      Self.Add_Action (Initial_Label, Initial_Action);
      Self.Select_Item (Initial_Label);
   end Gtk_New;

   ----------------
   -- Add_Action --
   ----------------

   procedure Add_Action
     (Self   : not null access Action_Combo_Tool_Record'Class;
      Label  : String;
      Action : String)
   is
      Data : constant Gtkada.Combo_Tool_Button.User_Data :=
        new Action_User_Data'
          (User_Data_Record with Action => To_Unbounded_String (Action));
      Act   : constant Action_Record_Access :=
        Lookup_Action (Self.Kernel, Action);
   begin
      Self.Add_Item
        (Item      => (if Label = "" then Action else Label),
         Icon_Name => (if Act = null then "" else Get_Icon_Name (Act)),
         Data      => Data);
   end Add_Action;

   -------------------
   -- Remove_Action --
   -------------------

   procedure Remove_Action
     (Self    : not null access Action_Combo_Tool_Record'Class;
      Action  : String)
   is
      function Predicate
        (Dummy_Item : String; Data : User_Data) return Boolean
         is (Action_User_Data_Access (Data).Action = Action);
   begin
      Self.Remove_If (Predicate'Access);
   end Remove_Action;

end Gtkada.Action_Combo_Tool;
