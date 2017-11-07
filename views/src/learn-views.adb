------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

with Ada.Containers.Doubly_Linked_Lists;

with Glib.Object;         use Glib.Object;

with Gtk.Box;             use Gtk.Box;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Flow_Box_Child;  use Gtk.Flow_Box_Child;
with Gtk.Label;           use Gtk.Label;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Style_Context;   use Gtk.Style_Context;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtkada.MDI;          use Gtkada.MDI;

with Dialog_Utils;        use Dialog_Utils;
with Generic_Views;       use Generic_Views;
with GPS.Kernel.Hooks;    use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;      use GPS.Kernel.MDI;

package body Learn.Views is

   package Group_Widget_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Dialog_Group_Widget,
         "="          => "=");

   type Learn_View_Record is new Generic_Views.View_Record with record
      Main_View     : Dialog_View;
      Group_Widgets : Group_Widget_Lists.List;
      Help_Label    : Gtk_Label;
   end record;
   type Learn_View is access all Learn_View_Record'Class;

   function Initialize
     (View : access Learn_View_Record'Class) return Gtk.Widget.Gtk_Widget;

   package Generic_Learn_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Learn_View",
      View_Name          => "Learn",
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Formal_View_Record => Learn_View_Record);

   function Filter_Learn_Item
     (Child : not null access Gtk_Flow_Box_Child_Record'Class) return Boolean;
   --  Called each time we want to refilter the learn items contained in the
   --  Learn view.

   procedure MDI_Child_Selected (Self : access Gtk_Widget_Record'Class);
   --  Called each time the selected MDI child changes

   procedure On_Learn_Item_Selected
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   --  Called when the user clicks on a learn item

   -----------------------
   -- Filter_Learn_Item --
   -----------------------

   function Filter_Learn_Item
     (Child : not null access Gtk_Flow_Box_Child_Record'Class) return Boolean
   is
      Item   : constant Learn_Item := Learn_Item (Child);
      Kernel : constant Kernel_Handle :=
                 Generic_Learn_Views.Get_Module.Get_Kernel;
   begin
      return Item.Is_Visible (Kernel.Get_Current_Context, Filter_Text => "");
   end Filter_Learn_Item;

   ------------------------
   -- MDI_Child_Selected --
   ------------------------

   procedure MDI_Child_Selected (Self : access Gtk_Widget_Record'Class) is
      View : constant Learn_View := Learn_View (Self);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (View.Kernel));
   begin
      --  Don't refresh the view according to the context if the Learn view
      --  gains the focus: the user probably wants to click on a learn item
      --  to display its help.

      if Child /= null and then Child.Get_Title /= "Learn" then
         for Group_Widget of View.Group_Widgets loop
            Group_Widget.Force_Refilter;
         end loop;
      end if;
   end MDI_Child_Selected;

   ----------------------------
   -- On_Learn_Item_Selected --
   ----------------------------

   procedure On_Learn_Item_Selected
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class)
   is
      View : constant Learn_View := Learn_View (Self);
      Item : constant Learn_Item := Learn_Item (Child);
   begin
      View.Help_Label.Set_Markup (Item.Get_Help);
   end On_Learn_Item_Selected;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View : access Learn_View_Record'Class) return Gtk.Widget.Gtk_Widget
   is
      Providers    : constant Learn_Provider_Maps.Map :=
                       Get_Registered_Providers;
      Help_View    : Dialog_View;
      Group_Widget : Dialog_Group_Widget;
      Pane         : Gtk_Paned;
   begin
      Initialize_Vbox (View);

      Gtk_New_Vpaned (Pane);
      Pane.Set_Position (500);
      View.Pack_Start (Pane);

      --  Connect to the Signal_Child_Selected signal to refilter all the
      --  learn intems contained in the view.

      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Child_Selected,
         Widget_Callback.To_Marshaller (MDI_Child_Selected'Access), View);

      --  Create the main view

      View.Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (View.Main_View);
      Pane.Pack1 (View.Main_View, Resize => True, Shrink => True);

      --  Create a group widget for all the registered providers

      for Provider of Providers loop
         Group_Widget := new Dialog_Group_Widget_Record;
         View.Group_Widgets.Append (Group_Widget);

         Initialize
           (Self                => Group_Widget,
            Parent_View         => View.Main_View,
            Group_Name          => Provider.Get_Name,
            Allow_Multi_Columns => False,
            Selection           => Selection_Single,
            Filtering_Function  => Filter_Learn_Item'Access);

         Group_Widget.On_Child_Selected
           (Call => On_Learn_Item_Selected'Access,
            Slot => View);

         Get_Style_Context (Group_Widget).Add_Class ("learn-groups");

         --  Add the provider's learn items in the group widget

         for Item of Provider.Get_Learn_Items loop
            Get_Style_Context (Item).Add_Class ("learn-items");
            Group_Widget.Append_Child
              (Widget    => Item,
               Expand    => False);
         end loop;
      end loop;

      --  Create the documentation view

      Help_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Help_View);
      Pane.Pack2 (Help_View, Resize => True, Shrink => True);

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Self                => Group_Widget,
         Parent_View         => Help_View,
         Allow_Multi_Columns => False);

      Gtk_New (View.Help_Label);
      View.Help_Label.Set_Alignment (0.0, 0.0);
      View.Help_Label.Set_Use_Markup (True);
      View.Help_Label.Set_Line_Wrap (True);
      View.Help_Label.Set_Justify (Justify_Fill);

      Group_Widget.Append_Child (View.Help_Label);

      return Gtk_Widget (View);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Generic_Learn_Views.Register_Module (Kernel);
   end Register_Module;

end Learn.Views;
