------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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
with Glib.Values;

with Gdk.Event;              use Gdk.Event;
with Gtk.Box;                use Gtk.Box;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Flow_Box_Child;     use Gtk.Flow_Box_Child;
with Gtk.Handlers;           use Gtk.Handlers;
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
with XML_Utils;              use XML_Utils;

package body Learn.Views is

   Learn_View_Module : Learn_View_Module_Access;

   Default_Sep_Pos   : constant Float := 75.0;

   package Group_Widget_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Dialog_Group_Widget,
      "="          => "=");

   type Learn_Provider_Widgets_Type is record
      Provider_Label : Gtk_Label;
      Group_Widgets  : Group_Widget_Lists.List;
   end record;

   package Learn_Provider_Widgets_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Learn_Provider_Widgets_Type,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => "=",
        "="             => "=");

   type Learn_View_Record is new Generic_Views.View_Record with record
      Main_View             : Dialog_View;
      Provider_Widgets_Map  : Learn_Provider_Widgets_Maps.Map;
      Paned_View            : Gtk_Paned;
      Help_Label            : Gtk_Label;
      Stored_Pos            : Float := Default_Sep_Pos;
      Selected_Item         : Learn_Item;
      On_Realize_Handler_ID : Handler_Id;
      Previous_Context      : Selection_Context := No_Context;
   end record;
   type Learn_View is access all Learn_View_Record'Class;

   function Initialize
     (View : access Learn_View_Record'Class) return Gtk.Widget.Gtk_Widget;
   overriding procedure Save_To_XML
     (View : access Learn_View_Record; XML : in out XML_Utils.Node_Ptr);
   overriding procedure Load_From_XML
     (View : access Learn_View_Record; XML : XML_Utils.Node_Ptr);

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

   type On_Context_Changed is new Context_Hooks_Function with null record;
   overriding procedure Execute
     (Self    : On_Context_Changed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context);
   --  Called when the context changes

   function Filter_Learn_Item
     (Child : not null access Gtk_Flow_Box_Child_Record'Class) return Boolean;
   --  Called each time we want to refilter the learn items contained in the
   --  Learn view.

   procedure Filter_Learn_Items
     (Self    : not null access Learn_View_Record'Class;
      Context : Selection_Context);
   --  TODO

   procedure Change_Pane_Orientation (Self : access Gtk_Widget_Record'Class);
   --  Called each time the Learn view's MDI child has been reorganized
   --  (e.g: after a Drag'n'Drop).
   --  Switches the orientation of the paned view depeinding on the new
   --  position.

   procedure On_Learn_Item_Selected
     (Self  : access Glib.Object.GObject_Record'Class;
      Child : not null access Gtk_Flow_Box_Child_Record'Class);
   --  Called when the user clicks on a learn item

   procedure On_Realize
     (Self   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Used to restore the paned view's separator position when modified by the
   --  user.

   function On_Learn_Item_Button_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  Called each time the user clicks on a learn item.
   --  Execute the learn item's On_Double_Click callback, if any.

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

            --  If found, set the visibility of the widgets associated with the
            --  provider refered by the preference.

            if Cursor /= No_Element then
               declare
                  Provider_Name    : constant String :=
                                       Boolean_Preference_Maps.Key (Cursor);
                  Provider_Widgets : constant Learn_Provider_Widgets_Type :=
                                       View.Provider_Widgets_Map
                                         (Provider_Name);
               begin
                  if Boolean_Preference (Pref).Get_Pref then
                     Provider_Widgets.Provider_Label.Set_No_Show_All (False);
                     Provider_Widgets.Provider_Label.Show_All;

                     for Group_Widget of Provider_Widgets.Group_Widgets loop
                        Group_Widget.Set_No_Show_All (False);
                        Group_Widget.Show_All;
                        Group_Widget.Force_Refilter;
                     end loop;
                  else
                     Provider_Widgets.Provider_Label.Hide;

                     for Group_Widget of Provider_Widgets.Group_Widgets loop
                        Group_Widget.Hide;
                     end loop;
                  end if;
               end;
            end if;
         end;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_Context_Changed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Self);
      use Generic_Learn_Views;

      View  : constant Generic_Learn_Views.View_Access :=
                Generic_Learn_Views.Retrieve_View
                  (Kernel, Visible_Only => True);
   begin
      if View /= null then
         View.Filter_Learn_Items (Context);
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
   -- Filter_Learn_Items --
   ------------------------

   procedure Filter_Learn_Items
     (Self    : not null access Learn_View_Record'Class;
      Context : Selection_Context)
   is
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (Self.Kernel));
   begin
      --  Don't refresh the view according to the context if the Learn view
      --  gains the focus: the user probably wants to click on a learn item
      --  to display its help.

      if Child /= null and then Child.Get_Title /= "Learn" then
         for Provider_Widgets of Self.Provider_Widgets_Map loop
            for Group_Widget of Provider_Widgets.Group_Widgets loop
               Group_Widget.Force_Refilter;
            end loop;
         end loop;

         Self.Previous_Context := Context;
      end if;
   end Filter_Learn_Items;

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

      View.Filter_Learn_Items (View.Kernel.Get_Current_Context);
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
      --  Don't unselect the previously selected item if the user selects it
      --  again.
      if View.Selected_Item /= null
        and then View.Selected_Item.Get_Name /= Item.Get_Name
      then
         View.Main_View.Set_Child_Highlighted
           (Child_Key => View.Selected_Item.Get_Name,
            Highlight => False);
      end if;

      View.Help_Label.Set_Markup (Item.Get_Help);
      View.Selected_Item := Item;
   end On_Learn_Item_Selected;

   --------------------------------
   -- On_Learn_Item_Button_Press --
   --------------------------------

   function On_Learn_Item_Button_Press
     (Self  : access Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      Group_Widget : constant Dialog_Group_Widget :=
                       Dialog_Group_Widget (Self);
   begin
      if Event.The_Type = Gdk_2button_Press then
         declare
            use Gtk.Widget.Widget_List;
            List : constant Glist := Group_Widget.Get_Selected_Children;
            Item : Learn_Item;
            View : constant Generic_Learn_Views.View_Access :=
              Generic_Learn_Views.Retrieve_View (Learn_View_Module.Get_Kernel);
         begin
            if List /= Null_List then
               Item := Learn_Item (Get_Data (List));
            end if;

            if Item /= null then
               Item.On_Double_Click (Context => View.Previous_Context);
            end if;
         end;
      end if;

      return False;
   end On_Learn_Item_Button_Press;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize
     (Self   : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      pragma Unreferenced (Params);
      View : constant Learn_View := Learn_View (Self);
   begin
      Set_Position_Percent (View.Paned_View, View.Stored_Pos);
   end On_Realize;

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

      --  Connect to the On_Context_Changed hook to filter the learn items

      Context_Changed_Hook.Add_Debounce
        (new On_Context_Changed,
         Watch => View);

      --  Connect to the Signal_Child_Reorganized to change the paned view's
      --  orientation depending on the Learn view's orientation.

      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Children_Reorganized,
         Change_Pane_Orientation'Access, View);

      --  Restore the paned view's separator position when realizing the view
      --  for the first time.

      View.On_Realize_Handler_ID := Widget_Callback.Connect
        (View, Signal_Realize, On_Realize'Access);

      --  Create the main view

      View.Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (View.Main_View);
      View.Paned_View.Pack1 (View.Main_View, Resize => True, Shrink => True);

      --  Create group widgets for all the registered providers

      for Provider of Providers loop
         declare
            Provider_Name    : constant String := Provider.Get_Name;
            Provider_Widgets : Learn_Provider_Widgets_Type;
            Show_Pref        : constant Boolean_Preference :=
                                 Learn_View_Module.Show_Providers_Preferences
                                   (Provider_Name);
            Item_ID          : Positive := 1;
         begin
            Gtk_New (Provider_Widgets.Provider_Label, Provider_Name);
            Get_Style_Context (Provider_Widgets.Provider_Label).Add_Class
              ("learn-provider-labels");
            Provider_Widgets.Provider_Label.Set_No_Show_All
              (not Show_Pref.Get_Pref);
            View.Main_View.Append
              (Provider_Widgets.Provider_Label,
               Expand => False);

            for Item_Group of Provider.Get_Learn_Items loop
               Group_Widget := new Dialog_Group_Widget_Record;
               Provider_Widgets.Group_Widgets.Append (Group_Widget);

               Initialize
                 (Self                => Group_Widget,
                  Parent_View         => View.Main_View,
                  Group_Name          => Item_Group.Get_Name,
                  Allow_Multi_Columns => True,
                  Selection           => Selection_Single,
                  Filtering_Function  => Filter_Learn_Item'Access);

               Group_Widget.On_Child_Selected
                 (Call => On_Learn_Item_Selected'Access,
                  Slot => View);

               Group_Widget.Set_Column_Spacing (10);
               Group_Widget.Set_Row_Spacing (3);

               Get_Style_Context (Group_Widget).Add_Class ("learn-groups");

               --  Connect to the Button_Press_Event signal to detect
               --  double-clicks on learn items.

               Group_Widget.On_Button_Press_Event
                 (On_Learn_Item_Button_Press'Access);

               --  Add the group's learn items in the group widget

               for Item of Item_Group.Items loop
                  Get_Style_Context (Item).Add_Class ("learn-items");
                  Item.Set_Name ("learn-item-" & Item_ID'Img);
                  Group_Widget.Append_Child
                    (Widget      => Item,
                     Expand      => False,
                     Homogeneous => True,
                     Child_Key   => Item.Get_Name);

                  Item_ID := Item_ID + 1;
               end loop;

               --  Set the group widget's visibility depending on the
               --  associated  preference.

               Group_Widget.Set_No_Show_All (not Show_Pref.Get_Pref);
            end loop;

            View.Provider_Widgets_Map.Insert
              (Key      => Provider_Name,
               New_Item => Provider_Widgets);
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
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (View : access Learn_View_Record; XML : in out XML_Utils.Node_Ptr)
   is
      Root : Node_Ptr;
   begin
      Root := new Node;
      XML.Child := Root;
      Root.Tag := new String'("learn");
      Set_Attribute
        (Root, "position",
         Float'Image (Get_Position_Percent (View.Paned_View)) & "%");
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (View : access Learn_View_Record; XML : XML_Utils.Node_Ptr)
   is
      Learn_Node : constant Node_Ptr := XML.Child;
   begin
      if Learn_Node = null then
         return;
      end if;

      declare
         Pos_Str : constant String := Get_Attribute (Learn_Node, "position");
      begin
         if Pos_Str /= "" then
            View.Stored_Pos := Float'Value
              (Pos_Str (Pos_Str'First .. Pos_Str'Last - 1));
         end if;
      end;
   end Load_From_XML;

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
               Name     => "learn-view-show-" & Provider_Name,
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
