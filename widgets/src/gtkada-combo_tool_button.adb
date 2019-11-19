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

with Ada.Numerics;
with Interfaces.C.Strings;     use Interfaces.C.Strings;

with Cairo;                    use Cairo;
with Glib.Convert;             use Glib.Convert;
with Glib.Main;                use Glib.Main;
with Glib.Object;              use Glib.Object;
with Gdk.Device;               use Gdk.Device;
with Gdk.Event;                use Gdk.Event;
with Gdk.Window;               use Gdk.Window;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Icon_Factory;         use Gtk.Icon_Factory;
with Gtk.Image;                use Gtk.Image;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with System;

package body Gtkada.Combo_Tool_Button is

   ----------------------
   -- Class definition --
   ----------------------

   Class_Record : aliased Ada_GObject_Class := Uninitialized_Class;
   Signals : constant chars_ptr_array :=
     (1 => New_String (String (Signal_Selection_Changed)));

   ---------------
   -- Menu_Item --
   ---------------

   type Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Full_Name : Unbounded_String;
      Data      : User_Data;
   end record;
   type Menu_Item is access all Menu_Item_Record'Class;

   --------------
   -- Handlers --
   --------------

   package Items_Callback is new Gtk.Handlers.User_Callback
     (Menu_Item_Record, Gtkada_Combo_Tool_Button);

   package Menu_Popup is new Popup_For_Device_User_Data
     (Gtkada_Combo_Tool_Button);

   ---------------------------
   -- Callback declarations --
   ---------------------------

   package Button_Sources is new Glib.Main.Generic_Sources
     (Gtkada_Combo_Tool_Button);

   function On_Long_Click (Self : Gtkada_Combo_Tool_Button) return Boolean;
   --  Called when the user had kept the button pressed for a long time

   function On_Button_Press
     (Button : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   function On_Button_Release
     (Button : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;
   function On_Menu_Button_Release
     (Button : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean;

   procedure Menu_Position
     (Menu    : not null access Gtk_Menu_Record'Class;
      X       : out Gint;
      Y       : out Gint;
      Push_In : out Boolean;
      Widget  : Gtkada_Combo_Tool_Button);

   procedure On_Menu_Item_Activated
     (Item   : access Menu_Item_Record'Class;
      Widget : Gtkada_Combo_Tool_Button);

   procedure Popup
     (Self : not null access Gtkada_Combo_Tool_Button_Record'Class);
   procedure Popdown
     (Self : not null access Gtkada_Combo_Tool_Button_Record'Class);
   --  Hide or Show the popup menu

   function On_Draw
     (Self : access GObject_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean;

   function Get_Button
     (Self : not null access Gtkada_Combo_Tool_Button_Record'Class)
      return Gtk_Widget;
   --  Return the internal button widget used by a GtkToolButton

   procedure Menu_Detacher
     (Attach_Widget : System.Address; Menu : System.Address);
   pragma Convention (C, Menu_Detacher);

   ----------------
   -- Get_Button --
   ----------------

   function Get_Button
     (Self : not null access Gtkada_Combo_Tool_Button_Record'Class)
      return Gtk_Widget is
   begin
      return Self.Get_Child;
   end Get_Button;

   -------------
   -- Popdown --
   -------------

   procedure Popdown
     (Self : not null access Gtkada_Combo_Tool_Button_Record'Class) is
   begin
      if Self.Menu /= null then
         Self.Menu.Deactivate;
         Self.Menu := null;
      end if;
   end Popdown;

   -----------
   -- Popup --
   -----------

   procedure Popup
     (Self : not null access Gtkada_Combo_Tool_Button_Record'Class)
   is
      M_Item, Active : Menu_Item;
      Label  : Gtk_Label;
      Icon   : Gtk_Image;
      Hbox   : Gtk_Hbox;
   begin
      if Self.Menu = null then
         Gtk_New (Self.Menu);

         --  Destroy the menu when Self is destroyed
         Self.Menu.Attach_To_Widget (Self, Menu_Detacher'Access);

         --  This is necessary because the menu gets a grab, and on OSX we
         --  cannot select the current item from the menu (on a long click)
         --  because the Enter/Leave events have not been propagated correctly.
         Self.Menu.On_Button_Release_Event
           (On_Menu_Button_Release'Access, Self);

         --  Fill the menu

         for Item of Self.Items loop
            M_Item := new Menu_Item_Record;
            M_Item.Data      := Item.Data;
            M_Item.Full_Name := Item.Full_Name;
            Gtk.Menu_Item.Initialize (M_Item);

            Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 5);
            M_Item.Add (Hbox);

            if Item.Icon_Name /= "" then
               Gtk_New_From_Icon_Name
                 (Icon, To_String (Item.Icon_Name), Icon_Size_Menu);
               Hbox.Pack_Start (Icon, False, False, 0);
            end if;

            if Item.Full_Name = Self.Selected then
               Gtk_New
                 (Label,
                  "<b>" & Escape_Text (To_String (Item.Full_Name)) & "</b>");
               Label.Set_Use_Markup (True);
               Active := M_Item;
            else
               Gtk_New (Label, To_String (Item.Full_Name));
            end if;

            Label.Set_Alignment (0.0, 0.5);
            Hbox.Pack_Start (Label, True, True, 0);

            Self.Menu.Add (M_Item);

            Add_Watch
              (Items_Callback.Connect
                 (M_Item, Gtk.Menu_Item.Signal_Activate,
                  Items_Callback.To_Marshaller
                    (On_Menu_Item_Activated'Access),
                  Gtkada_Combo_Tool_Button (Self)),
               Self);
         end loop;

         Self.Menu.Show_All;

         --  Show the menu on screen

         if Self.Popup_Timeout /= No_Source_Id then
            Remove (Self.Popup_Timeout);
            Self.Popup_Timeout := No_Source_Id;
         end if;

         Menu_Popup.Popup_For_Device
           (Self.Menu,
            Device            => Self.Popup_Device,
            Parent_Menu_Shell => null,
            Parent_Menu_Item  => null,
            Func              => Menu_Position'Access,
            Data              => Gtkada_Combo_Tool_Button (Self),
            Button            => 1,
            Activate_Time     => Self.Popup_Time);

         if Active /= null then
            Self.Menu.Select_Item (Active);
         end if;
      end if;
   end Popup;

   -------------
   -- On_Draw --
   -------------

   function On_Draw
     (Self : access GObject_Record'Class;
      Cr   : Cairo.Cairo_Context) return Boolean
   is
      B : constant Gtkada_Combo_Tool_Button :=
        Gtkada_Combo_Tool_Button (Self);
      W, H    : Gint;
      Result  : Boolean;
      Alloc   : Gtk_Allocation;
      Size : constant Gdouble := 6.0;
   begin
      if not B.Items.Is_Empty then
         Icon_Size_Lookup (B.Get_Icon_Size, W, H, Result);
         B.Get_Allocation (Alloc);
         Get_Style_Context (B).Render_Arrow
           (Cr    => Cr,
            Angle => Gdouble (Ada.Numerics.Pi),
            X     => Gdouble (Alloc.Width) - Size - 1.0,
            Y     => Gdouble ((Alloc.Height + H) / 2) - Size,
            Size  => Size);
      end if;
      return False;
   end On_Draw;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Button : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean
   is
      B : constant Gtkada_Combo_Tool_Button :=
        Gtkada_Combo_Tool_Button (Button);
      Stub : Gdk_Device_Record;
      pragma Unmodified (Stub);
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      Popdown (B);

      B.Popup_Time := Event.Time;
      B.Popup_Device := Gdk_Device (Get_User_Data (Event.Device, Stub));
      Ref (B.Popup_Device);

      if Event.Button = 3
        or else (Event.Button = 1 and then B.Click_Pops_Up)
      then
         --  Immediately popup the dialog.
         --  This is a workaround for a OSX-specific bug in gtk+ 3.8.2: if we
         --  popup the dialog later, there will be not "current event" at that
         --  point, so gtk+ will be waiting for the first ENTER_NOTIFY on a
         --  GtkMenuItem before taking into account a BUTTON_RELEASE_EVENT.
         --  But because of the way the grabs are handled, that ENTER_NOTIFY
         --  is never sent/received, so we cannot select an item in the menu at
         --  all.
         --  Because of the same bug, the current item in the menu might not be
         --  highlighted in any case on OSX, but at least with an immediate
         --  popup we can properly select any of the items.

         Tmp := On_Long_Click (B);
         return True;

      elsif Event.Button = 1 then
         if B.Popup_Timeout /= No_Source_Id then
            Remove (B.Popup_Timeout);
         end if;
         B.Popup_Timeout := Button_Sources.Timeout_Add
           (300, On_Long_Click'Access, B);
      end if;
      return False;
   end On_Button_Press;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Button : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean
   is
      pragma Unreferenced (Event);

      B : constant Gtkada_Combo_Tool_Button :=
        Gtkada_Combo_Tool_Button (Button);
   begin
      if B.Popup_Timeout /= No_Source_Id then
         Remove (B.Popup_Timeout);
         B.Popup_Timeout := No_Source_Id;

      else
         --  We already performed the long click, so don't do the default
         --  action in addition
         null;
      end if;

      if B.Popup_Device /= null then
         Unref (B.Popup_Device);
         B.Popup_Device := null;
      end if;

      return False;
   end On_Button_Release;

   ----------------------------
   -- On_Menu_Button_Release --
   ----------------------------

   function On_Menu_Button_Release
     (Button : access GObject_Record'Class;
      Event  : Gdk_Event_Button) return Boolean
   is
      B : constant Gtkada_Combo_Tool_Button :=
        Gtkada_Combo_Tool_Button (Button);
      Obj : GObject;
   begin
      Obj := Get_User_Data (Event.Window);
      if Obj /= null and then Obj.all in Menu_Item_Record'Class then
         B.Menu.Select_Item (Menu_Item (Obj));
      end if;

      if B.Popup_Device /= null then
         Unref (B.Popup_Device);
         B.Popup_Device := null;
      end if;

      if B.Popup_Timeout /= No_Source_Id then
         Remove (B.Popup_Timeout);
         B.Popup_Timeout := No_Source_Id;
      end if;

      --  Leave default behavior, which is to close the menu
      return False;
   end On_Menu_Button_Release;

   -------------------
   -- On_Long_Click --
   -------------------

   function On_Long_Click (Self : Gtkada_Combo_Tool_Button) return Boolean is
   begin
      Popup (Self);
      return False;
   end On_Long_Click;

   -------------------
   -- Menu_Detacher --
   -------------------

   procedure Menu_Detacher
     (Attach_Widget : System.Address; Menu : System.Address)
   is
      pragma Unreferenced (Menu);
      Stub : Gtkada_Combo_Tool_Button_Record;
      pragma Unmodified (Stub);
      Self : constant Gtkada_Combo_Tool_Button :=
        Gtkada_Combo_Tool_Button (Get_User_Data (Attach_Widget, Stub));
   begin
      Popdown (Self);
   end Menu_Detacher;

   -------------------
   -- Menu_Position --
   -------------------

   procedure Menu_Position
     (Menu    : not null access Gtk_Menu_Record'Class;
      X       : out Gint;
      Y       : out Gint;
      Push_In : out Boolean;
      Widget  : Gtkada_Combo_Tool_Button)
   is
      pragma Unreferenced (Menu);
      Menu_Req    : Gtk_Requisition;
      Allo : Gtk_Allocation;

   begin
      Size_Request (Widget.Menu, Menu_Req);
      Get_Origin (Get_Window (Widget), X, Y);
      Get_Allocation (Widget, Allo);

      X := X + Allo.X;
      Y := Y + Allo.Y + Allo.Height;

      Push_In := False;

      if Allo.Width > Menu_Req.Width then
         X := X + Allo.Width - Menu_Req.Width;
      end if;
   end Menu_Position;

   ----------------------------
   -- On_Menu_Item_Activated --
   ----------------------------

   procedure On_Menu_Item_Activated
     (Item   : access Menu_Item_Record'Class;
      Widget : Gtkada_Combo_Tool_Button)
   is
   begin
      Select_Item (Widget, To_String (Item.Full_Name));
      Widget_Callback.Emit_By_Name (Widget, Signal_Clicked);
   end On_Menu_Item_Activated;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self          : out Gtkada_Combo_Tool_Button;
      Icon_Name     : String;
      Click_Pops_Up : Boolean := False) is
   begin
      Self := new Gtkada_Combo_Tool_Button_Record;
      Initialize (Self, Icon_Name, Click_Pops_Up);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self          : access Gtkada_Combo_Tool_Button_Record'Class;
      Icon_Name     : String;
      Click_Pops_Up : Boolean := False) is
   begin
      Initialize_Class_Record
        (Ancestor     => Gtk.Tool_Button.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "GtkadaComboToolButton");
      Glib.Object.G_New (Self, Class_Record);

      Get_Style_Context (Self).Add_Class ("gps-combo-tool-button");

      Self.Set_Icon_Name (Icon_Name);
      Self.Set_Homogeneous (False);
      Self.Icon_Name := To_Unbounded_String (Icon_Name);
      Self.Click_Pops_Up := Click_Pops_Up;

      Get_Button (Self).On_Button_Press_Event (On_Button_Press'Access, Self);
      Get_Button (Self).On_Button_Release_Event
        (On_Button_Release'Access, Self);
      Self.On_Draw (On_Draw'Access, Self, After => True);
   end Initialize;

   --------------
   -- Add_Item --
   --------------

   procedure Add_Item
     (Widget     : access Gtkada_Combo_Tool_Button_Record;
      Item       : String;
      Icon_Name  : String := "";
      Data       : User_Data := null;
      Short_Name : String := "") is
   begin
      Widget.Items.Append
        ((Icon_Name   =>
            (if Icon_Name /= ""
             then To_Unbounded_String (Icon_Name)
             else Widget.Icon_Name),
          Full_Name   => To_Unbounded_String (Item),
          Short_Name  =>
            (if Short_Name /= ""
             then To_Unbounded_String (Short_Name)
             else To_Unbounded_String (Item)),
          Data        => Data));
   end Add_Item;

   ---------------
   -- Remove_If --
   ---------------

   procedure Remove_If
     (Self      : not null access Gtkada_Combo_Tool_Button_Record;
      Predicate : not null access function
        (Item : String; Data : User_Data) return Boolean)
   is
      Index : Natural := 0;
   begin
      Popdown (Self);
      while Index <= Self.Items.Last_Index loop
         declare
            It : constant Item_Record := Self.Items (Index);
         begin
            if Predicate (To_String (It.Full_Name), It.Data) then
               Self.Items.Delete (Index);
            else
               Index := Index + 1;
            end if;
         end;
      end loop;
   end Remove_If;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record;
      Item   : String) is
   begin
      --  Avoid loop when selected an item executes an action that refreshes
      --  the contents of the combo
      if Item /= Widget.Selected then
         Widget.Selected := To_Unbounded_String (Item);
         Popdown (Widget);

         for J of Widget.Items loop
            if J.Full_Name = Item then
               --  Change the toolbar icon

               if J.Icon_Name /= "" then
                  Widget.Set_Icon_Name (To_String (J.Icon_Name));
               else
                  Widget.Set_Label (To_String (J.Short_Name));
               end if;
               exit;
            end if;
         end loop;

         --  Emit the signal outside of the loop, in case listeners try
         --  to modify Widget.Items, to avoid tampering with cursors
         Widget_Callback.Emit_By_Name (Widget, Signal_Selection_Changed);
      end if;
   end Select_Item;

   -----------------
   -- Clear_Items --
   -----------------

   procedure Clear_Items (Widget : access Gtkada_Combo_Tool_Button_Record) is
   begin
      Widget.Items.Clear;
      Popdown (Widget);
   end Clear_Items;

   -----------------------
   -- Get_Selected_Item --
   -----------------------

   function Get_Selected_Item
     (Widget : access Gtkada_Combo_Tool_Button_Record) return String is
   begin
      return To_String (Widget.Selected);
   end Get_Selected_Item;

   ----------------------------
   -- Get_Selected_Item_Data --
   ----------------------------

   function Get_Selected_Item_Data
     (Widget : access Gtkada_Combo_Tool_Button_Record)
      return User_Data
   is
   begin
      for Item of Widget.Items loop
         if Item.Full_Name = Widget.Selected then
            return Item.Data;
         end if;
      end loop;
      return null;
   end Get_Selected_Item_Data;

   ---------------
   -- Has_Items --
   ---------------

   function Has_Items
     (Self : not null access Gtkada_Combo_Tool_Button_Record)
      return Boolean is
   begin
      return not Self.Items.Is_Empty;
   end Has_Items;

end Gtkada.Combo_Tool_Button;
