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

with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;

with Gtk.Box;                use Gtk.Box;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Flow_Box_Child;     use Gtk.Flow_Box_Child;
with Gtk.Label;              use Gtk.Label;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Paned;              use Gtk.Paned;
with Gtk.Style_Context;      use Gtk.Style_Context;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

with Dialog_Utils;           use Dialog_Utils;
with GUI_Utils;              use GUI_Utils;
with Generic_Views;          use Generic_Views;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;

package body Learn.Views is

   Learn_View_Module : Learn_View_Module_Access;

   package Group_Widget_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Dialog_Group_Widget,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "="             => "=");

   type Learn_View_Record is new Generic_Views.View_Record with record
      Main_View     : Dialog_View;
      Group_Widgets : Group_Widget_Maps.Map;
      Paned_View    : Gtk_Paned;
      Help_Label    : Gtk_Label;
   end record;
   type Learn_View is access all Learn_View_Record'Class;

   function Initialize
     (View : access Learn_View_Record'Class) return Gtk.Widget.Gtk_Widget;

   overriding procedure Create_Menu
     (View    : not null access Learn_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   package Generic_Learn_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Learn_View",
      View_Name          => "Learn",
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Formal_View_Record => Learn_View_Record);

   procedure Register_Preferences
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Register the Learn view's preferences (e.g: the preferences that allow
   --  the user to decide which providers should be displayed).

   ---------------
   -- Callbacks --
   ---------------

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed.

   function Filter_Learn_Item
     (Child : not null access Gtk_Flow_Box_Child_Record'Class) return Boolean;
   --  Called each time we want to refilter the learn items contained in the
   --  Learn view.

   procedure MDI_Child_Selected (Self : access Gtk_Widget_Record'Class);
   --  Called each time the selected MDI child changes

   procedure Change_Pane_Orientation (Self : access Gtk_Widget_Record'Class);
   --  Called each time the Learn view's MDI child has been reorganized
   --  (e.g: after a Drag'n'Drop).
   --  Switches the orientation of the paned view depeinding on the new
   --  position.

   procedure On_Learn_Item_Selected
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   --  Called when the user clicks on a learn item

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      use Boolean_Preference_Maps;
      use Generic_Learn_Views;

      View : constant Generic_Learn_Views.View_Access :=
               Generic_Learn_Views.Retrieve_View (Kernel);
   begin
      if View /= null
        and then Pref /= null
        and then not Learn_View_Module.Show_Providers_Preferences.Is_Empty
      then

         --  Try to find the changing preference in the registered ones.

         declare
            Cursor : Boolean_Preference_Maps.Cursor :=
                       Learn_View_Module.Show_Providers_Preferences.First;
         begin
            while Cursor /= No_Element loop
               if Preference (Element (Cursor)) = Pref then
                  exit;
               end if;

               Next (Cursor);
            end loop;

            --  If found, set the visibility of the group widget associated
            --  with the provider refered by the preference.

            if Cursor /= No_Element then
               declare
                  Provider_Name : constant String :=
                                    Boolean_Preference_Maps.Key (Cursor);
                  Group_Widget  : constant Dialog_Group_Widget :=
                                    View.Group_Widgets (Provider_Name);
               begin
                  if Boolean_Preference (Pref).Get_Pref then
                     Group_Widget.Set_No_Show_All (False);
                     Group_Widget.Show_All;
                  else
                     Group_Widget.Hide;
                  end if;
               end;
            end if;
         end;
      end if;
   end Execute;

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

   -----------------------------
   -- Change_Pane_Orientation --
   -----------------------------

   procedure Change_Pane_Orientation (Self : access Gtk_Widget_Record'Class) is
      View         : constant Learn_View := Learn_View (Self);
      Child        : constant MDI_Child :=
                       Generic_Learn_Views.Child_From_View (View);
      Tab_Orient   : constant Tab_Orientation_Type :=
                       Child.Get_Tab_Orientation;
      Current_Type : constant GType := View.Paned_View.Get_Type;

   begin
      --  Do nothing if the paned view is already well oriented
      if ((Tab_Orient = Bottom_To_Top or else Tab_Orient = Top_To_Bottom)
          and then Current_Type = Get_Type_Vpaned)
        or else
          ((Tab_Orient = Horizontal or else Tab_Orient = Automatic)
           and then Current_Type = Get_Type_Hpaned)
      then
         return;
      end if;

      --  Switch the paned orientation
      Switch_Paned_Orientation (View.Paned_View);

      View.Pack_Start (View.Paned_View);

      View.Show_All;
   end Change_Pane_Orientation;

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
   begin
      Initialize_Vbox (View);

      Gtk_New_Vpaned (View.Paned_View);
      View.Pack_Start (View.Paned_View);

      --  Connect to the Signal_Child_Selected signal to refilter all the
      --  learn intems contained in the view.

      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Child_Selected,
         Widget_Callback.To_Marshaller (MDI_Child_Selected'Access), View);

      --  Connect to the Signal_Child_Reorganized to change the paned view's
      --  orientation depending on the Learn view's orientation.

      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Children_Reorganized,
         Change_Pane_Orientation'Access, View);

      --  Create the main view

      View.Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (View.Main_View);
      View.Paned_View.Pack1 (View.Main_View, Resize => True, Shrink => True);

      --  Create a group widget for all the registered providers

      for Provider of Providers loop
         declare
            Provider_Name : constant String := Provider.Get_Name;
            Show_Pref     : constant Boolean_Preference :=
                              Learn_View_Module.Show_Providers_Preferences
                                (Provider_Name);
         begin
            Group_Widget := new Dialog_Group_Widget_Record;
            View.Group_Widgets.Insert
              (Key      => Provider.Get_Name,
               New_Item => Group_Widget);

            Initialize
              (Self                => Group_Widget,
               Parent_View         => View.Main_View,
               Group_Name          => Provider_Name,
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

            --  Set the group widget's visibility depending on the associated
            --  preference.

            Group_Widget.Set_No_Show_All (not Show_Pref.Get_Pref);
         end;
      end loop;

      --  Create the documentation view

      Help_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Help_View);
      View.Paned_View.Pack2 (Help_View, Resize => True, Shrink => True);

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

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Learn_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Kernel : constant Kernel_Handle := View.Kernel;
   begin
      for Pref of Learn_View_Module.Show_Providers_Preferences loop
         Append_Menu
           (Menu   => Menu,
            Kernel => Kernel,
            Pref   => Pref);
      end loop;
   end Create_Menu;

   --------------------------
   -- Register_Preferences --
   --------------------------

   procedure Register_Preferences
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Prefs_Manager      : constant Preferences_Manager :=
                             Kernel.Get_Preferences;
      Providers          : constant Learn_Provider_Maps.Map :=
                             Get_Registered_Providers;
      Show_Provider_Pref : Boolean_Preference;
   begin
      for Provider of Providers loop
         declare
            Provider_Name : constant String := Provider.Get_Name;
            Pref_Label    : constant String := "Show " & Provider_Name;
         begin
            Show_Provider_Pref := Create
              (Manager  => Prefs_Manager,
               Path     => ":Learn View",
               Name     => "learn-view-show" & Provider_Name,
               Label    => Pref_Label,
               Doc      => Pref_Label & " in Learn vieww",
               Default  => True);
            Learn_View_Module.Show_Providers_Preferences.Insert
              (Key      => Provider_Name,
               New_Item => Show_Provider_Pref);
         end;
      end loop;
   end Register_Preferences;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Learn_View_Module := new Learn_View_Module_Type;

      --  Register the Learn view's module
      Generic_Learn_Views.Register_Module
        (Kernel,
         ID => Module_ID (Learn_View_Module));

      --  Register its associated preferences
      Register_Preferences (Kernel);
      Preferences_Changed_Hook.Add (new On_Pref_Changed);
   end Register_Module;

end Learn.Views;
