------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

with GNAT.OS_Lib;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Gdk.Drag_Contexts;         use Gdk.Drag_Contexts;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;                 use Gdk.Types;

with Glib.Action;               use Glib.Action;
with Glib.Convert;              use Glib.Convert;
with Glib.Main;                 use Glib.Main;
with Glib.Menu;                 use Glib.Menu;
with Glib.Menu_Model;           use Glib.Menu_Model;
with Glib.Object;               use Glib.Object;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Simple_Action;        use Glib.Simple_Action;
with Glib.Types;                use Glib.Types;
with Glib.Values;               use Glib.Values;
with Glib.Variant;              use Glib.Variant;

with Gtk.Container;             use Gtk.Container;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Tool_Item;             use Gtk.Tool_Item;

--  So that this type is correctly converted from C to Ada

with Gtk.Accel_Label;           use Gtk.Accel_Label;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Menu_Shell;            use Gtk.Menu_Shell;
with Gtk.Selection_Data;        use Gtk.Selection_Data;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Action_Combo_Tool;  use Gtkada.Action_Combo_Tool;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Main_Window;           use GPS.Main_Window;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with System;                    use System;
with Tooltips;                  use Tooltips;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Input_Sources.File;        use Input_Sources.File;
with DOM.Readers;               use DOM.Readers;
with DOM.Core.Nodes;            use DOM.Core, DOM.Core.Nodes;
with DOM.Core.Documents;        use DOM.Core.Documents;
with DOM.Core.Elements;         use DOM.Core.Elements;

with UTF8_Utils;                use UTF8_Utils;

package body GPS.Kernel.Modules.UI is
   Me : constant Trace_Handle :=
     Create ("GPS.Kernel.Modules.UI", GNATCOLL.Traces.Off);

   type GPS_Contextual_Menu_Record is new Gtk.Menu.Gtk_Menu_Record with record
      Kernel : access Kernel_Handle_Record'Class;
   end record;
   type GPS_Contextual_Menu is access all GPS_Contextual_Menu_Record'Class;
   --  A special type used for the contextual menu created by GPS. This is used
   --  to monitor whether a menu is opened.

   type Contextual_Menu_User_Data is record
      Context_Func : Contextual_Menu_Factory;
      Kernel       : Kernel_Handle;
      Event_Widget : Gtk_Widget;
   end record;

   type Contextual_Menu_Type
     is (Type_Action, Type_Submenu, Type_Separator);
   --  The type of the contextual menu

   type Contextual_Menu_Record;
   type Contextual_Menu_Access is access all Contextual_Menu_Record;
   type Contextual_Menu_Record
     (Menu_Type : Contextual_Menu_Type := Type_Separator)
      is record
         Kernel                : Kernel_Handle;
         Name                  : GNAT.Strings.String_Access;
         Label                 : Contextual_Menu_Label_Creator;
         Next                  : Contextual_Menu_Access;
         Group                 : Integer;
         Visible               : Boolean := True;
         Sensitive             : Boolean := True;
         Filter_Matched        : Boolean;
         --  Only valid while computing a contextual menu

         Label_For_Context     : Unbounded_String;
         --  Note: this field is only valid while computing the menu, in the
         --  body of Create

         case Menu_Type is
            when Type_Action =>
               Action           : GNAT.Strings.String_Access;

            when Type_Submenu =>
               Submenu          : Submenu_Factory;
               Submenu_Filter   : access Action_Filter_Record'Class := null;
               Submenu_Enable   : access Action_Filter_Record'Class := null;

            when Type_Separator =>
               Separator_Action : GNAT.Strings.String_Access;
               --  The action is only used for its filter
         end case;
      end record;
   --  A contextual menu entry declared by a user or GPS itself internally

   function Lookup_Action
     (Self : not null access Contextual_Menu_Record)
      return access Action_Record;
   --  Lookup the action for this contextual menu

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Contextual_Menu_Record, Contextual_Menu_Access);

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Kernel_Handle);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Contextual_Menu_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Contextual_Menu_Access, System.Address);

   package Action_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Contextual_Menu_Access);

   procedure Contextual_Menu_Destroyed
     (Data   : System.Address;
      Object : System.Address);
   pragma Convention (C, Contextual_Menu_Destroyed);
   --  Called when a contextual menu is destroyed and its context can be
   --  unrefed.

   type Contextual_Label_Parameters is
     new Contextual_Menu_Label_Creator_Record
   with record
      Label  : GNAT.Strings.String_Access;
      Custom : Custom_Expansion;
      Filter : Action_Filter;
   end record;

   type Contextual_Label_Param is access Contextual_Label_Parameters'Class;
   overriding function Get_Label
     (Creator : access Contextual_Label_Parameters;
      Context : Selection_Context) return String;
   --  Substitute %p, %f,... in the title to create a suitable contextual menu
   --  title.

   package Kernel_Contextuals is new GUI_Utils.User_Contextual_Menus
     (Contextual_Menu_User_Data);

   procedure On_Contextual_Menu_Hide
     (Self  : access Gtk_Widget_Record'Class);

   procedure Contextual_Action
     (Object : access GObject_Record'Class; Action : Contextual_Menu_Access);
   --  Execute action, in the context of a contextual menu

   type On_Context_Changed is new Context_Hooks_Function with null record;
   overriding procedure Execute
     (Self    : On_Context_Changed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context);
   --  Called when the context changes

   function Find_Menu_Item
     (Menubar : access Gtk_Menu_Bar_Record'Class;
      Path    : String)
      return Gtk.Menu_Item.Gtk_Menu_Item;
   --  Given an absolute path (see Register_Menu) for a menu item, return
   --  the underlying gtk menu item. Useful in particular to check or change
   --  the state of a menu item. Path is case insensitive.
   --  This function might return null when the item is not found.

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu;
   --  Create a contextual menu as a result of a mouse event
   --  Return null if no menu was created.

   procedure Add_Contextual_Menu
     (Kernel     : access Kernel_Handle_Record'Class;
      Menu       : Contextual_Menu_Access;
      Ref_Item   : String := "";
      Add_Before : Boolean := True);
   --  Add a new contextual menu in the list

   type Contextual_Menu_Reference is record
      Previous, Menu : Contextual_Menu_Access;
   end record;

   Null_Reference : constant Contextual_Menu_Reference := (null, null);

   function Find_Contextual_Menu_By_Name
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Contextual_Menu_Reference;
   --  Find a contextual menu by name

   type Toolbar_Button (Is_Separator : Boolean := False) is record
      case Is_Separator is
         when False =>
            Action    : Unbounded_String;
            Label     : Unbounded_String;
            Icon_Name : Unbounded_String;
            Group     : Unbounded_String;
            Hide      : Boolean := False;  --  Hide when filter fails
         when True =>
            Start_Of_Section : Unbounded_String;
      end case;
   end record;
   package Buttons_List is new Ada.Containers.Doubly_Linked_Lists
      (Toolbar_Button);

   type Toolbar_Description is record
      Inherit : Unbounded_String;
      Buttons : Buttons_List.List;
   end record;
   package Toolbar_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Toolbar_Description,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Case_Insensitive_Equal);
   --  Description of the toolbars

   procedure For_Each_Toolbar
     (Kernel : not null access Kernel_Handle_Record'Class;
      Name   : String;
      Callback : not null access procedure
        (Toolbar : not null access Gtk_Toolbar_Record'Class));
   --  Executes the Callback for each toolbar identified with the given id

   -------------------------
   -- proxies for actions --
   -------------------------

   function Get_Data
     (Self : not null access GObject_Record'Class)
     return access Action_Proxy'Class;
   function Lookup_Action
     (Self : not null access GObject_Record'Class)
     return access Action_Record;
   --  Lookup the action associated with the given widget.
   --  Self should be one of Action_Menu_Item or Action_Tool_Button.
   --
   --  Self.Looked_Up provides the action that was looked up previously. If the
   --  current action is different, various properties of the widget will be
   --  updated, and Looked_Up will be set to the new value.

   procedure Execute_Action
     (Self          : not null access GObject_Record'Class;
      Data          : Action_Proxy'Class;
      In_Foreground : Boolean := False);
   --  Execute the action associated with the proxy

   -------------
   -- GAction --
   -------------
   --  This type is used to create a link between gtk+ actions (GAction) which
   --  are used for menus in a GtkApplication, and the actions defined by GPS,
   --  which provide more advanced features (multi-key bindings, automatic
   --  filters, icons,...)

   type GPS_Action_Proxy is new Action_Proxy with record
      Active   : Boolean := True;
      --  Whether the action was enabled the last time we checked in the
      --  background
   end record;
   overriding procedure Set_Active
     (Self   : in out GPS_Action_Proxy;
      Active : Boolean;
      Object : not null access GObject_Record'Class);

   type GPS_Action_Record is new Gsimple_Action_Record with record
      --  ??? This is already stored by the Gsimple_Action
      CName : chars_ptr;
      Data  : aliased GPS_Action_Proxy;
   end record;
   type GPS_Action is access all GPS_Action_Record'Class;

   GPS_Action_CR : aliased Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   function GPS_Action_Get_Type return Glib.GType;
   --  Return the gtk+ type to use for actions that link GAction and GPS action

   procedure GPS_Action_Class_Init (Self : GObject_Class)
     with Convention => C;
   --  Initial properties for a GPS_Action class

   procedure Init_GAction_Iface
     (Iface : Action_Interface_Descr;
      Data  : System.Address)
     with Convention => C;
   --  Initialize the interface fields for GPS_Action

   procedure On_GPS_Action_Activate (Self : Gaction; P : System.Address);
   pragma Convention (C, On_GPS_Action_Activate);

   procedure On_GPS_Action_Change_State (Self : Gaction; Val : System.Address)
   is null;
   pragma Convention (C, On_GPS_Action_Change_State);

   function On_GPS_Action_Get_Enabled (Self : Gaction) return Gboolean;
   pragma Convention (C, On_GPS_Action_Get_Enabled);

   function On_GPS_Action_Get_Name (Self : Gaction) return chars_ptr;
   pragma Convention (C, On_GPS_Action_Get_Name);

   function On_GPS_Action_Get_Parameter_Type
     (Self : Gaction) return Gvariant_Type is (null);
   pragma Convention (C, On_GPS_Action_Get_Parameter_Type);

   function On_GPS_Action_Get_State
     (Self : Gaction) return System.Address is (System.Null_Address);
   pragma Convention (C, On_GPS_Action_Get_State);

   function On_GPS_Action_Get_State_Hint
     (Self : Gaction) return System.Address is (System.Null_Address);
   pragma Convention (C, On_GPS_Action_Get_State_Hint);

   function On_GPS_Action_Get_State_Type
     (Self : Gaction) return Gvariant_Type is (null);
   pragma Convention (C, On_GPS_Action_Get_State_Type);

   Property_Name           : constant Property_Id := 1;
   Property_Enabled        : constant Property_Id := 2;
   Property_Parameter_Type : constant Property_Id := 3;
   Property_State          : constant Property_Id := 4;
   Property_State_Type     : constant Property_Id := 5;

   procedure GPS_Action_Set_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec);
   procedure GPS_Action_Get_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec);
   --  Handling of properties for GPS_Action

   function Gtk_Name (Action : not null access GPS_Action_Record'Class)
      return String is ("app." & Value (Action.CName));
   --  The name by which the action can be refered in gtk+

   function Create_Or_Lookup_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String) return GPS_Action;
   --  Lookup whether there is already a GPS_Action associated with the
   --  given action. If there isn't, create a new one and register it in
   --  the application.

   ------------------------
   -- Action_Tool_Button --
   ------------------------

   type Widget_Action_Proxy is new Action_Proxy with null record;
   overriding procedure Set_Active
     (Self   : in out Widget_Action_Proxy;
      Active : Boolean;
      Object : not null access GObject_Record'Class);

   type Action_Tool_Button_Record is new Gtk_Tool_Button_Record with record
      Data            : aliased Widget_Action_Proxy;
      Forced_Stock    : Boolean := False;
      Focus_On_Action : Boolean := False;
   end record;
   type Action_Tool_Button is access all Action_Tool_Button_Record'Class;

   procedure Insert_Button
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Toolbar         : not null access Gtk_Toolbar_Record'Class;
      Descr           : Toolbar_Button;
      Section         : String;
      Toolbar_Id      : String);
   --  Create a new button so that it executes action when pressed.
   --  Hide and Optional are the same as for Data_Proxy
   --  Label can be used to override the label of the button (which
   --  by default is the name of the action)

   procedure On_Action_Button_Clicked
     (Button : access Gtk_Tool_Button_Record'Class);
   --  Called when an Action_Tool_Button has been activated.

   ----------------------
   -- Action_Menu_Item --
   ----------------------

   type Action_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Data : aliased Widget_Action_Proxy;
   end record;
   type Action_Menu_Item is access all Action_Menu_Item_Record'Class;

   function Gtk_New_Action_Item
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Full_Path     : String;
      Menu_Label    : String;
      Action        : String;
      Optional      : Boolean := False) return Gtk_Menu_Item;
   --  Create a new menu item that will execute Action.
   --
   --  Full_Path should have a form like "/main_main/submenu/label".
   --  Underscores are not used for mnemonics, and will be present in the
   --  final menu. Use String_Utils.Strip_Single_Underscores if needed.
   --  Full_Path must include the Menu_Label, although the latter is also
   --  provided separately for efficiency.
   --  Any special character like '/' should be quoted in Full_Path, using
   --  Escape_Menu_Name. Underscores must not be duplicated
   --
   --  This is considered as an absolute path, as if it always started with
   --  a '/'.

   procedure On_Activate_Action_Item
     (Item : access Gtk_Menu_Item_Record'Class);
   --  Called when an Action_Menu_Item is activated

   ----------------------------------------------
   --  Computing menu states in the background --
   ----------------------------------------------

   type Proxy_And_Filter is record
      Proxy  : access GObject_Record'Class;
      Filter : access Action_Filter_Record'Class;
   end record;
   package Proxy_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Proxy_And_Filter);
   use Proxy_Lists;
   --  List of all registered menu items or tool buttons for which the action
   --  has a filter. This is needed to dynamically deactivate menus and buttons
   --  whenever the current context changes

   procedure Add_To_Global_Proxies
     (Item   : not null access GObject_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class;
      Filter : access Action_Filter_Record'Class);
   --  Store items when they might need to be filtered when the context changes

   procedure Add_To_Unfiltered_Items (Item : Proxy_And_Filter);
   --  Add to the list of items that are not associated with a filter. We
   --  might still need to update them when an action is overridden or
   --  disabled by the user

   procedure On_Delete_Proxy
     (Kernel : System.Address;
      Item   : System.Address)
     with Convention => C;
   --  Called when a proxied item is deleted. This frees some data from
   --  the proxy's data, so must only be called when the item is destroyed.

   type Update_Menus_Data is record
      Context : Selection_Context;
      Current : Proxy_Lists.Cursor;
   end record;
   type Update_Menus_Data_Access is access all Update_Menus_Data;

   function Update_Menus_And_Buttons_Chunk
     (Data : Update_Menus_Data_Access) return Boolean;
   --  Recompute the state of menus and toolbar buttons based on the specified
   --  context. This will process as many items as possible, but will stop
   --  processing after a while and needs to be called again if it returns
   --  True.

   procedure Destroy (Data : in out Update_Menus_Data_Access);
   --  Free memory used by data

   package Update_Menus_Idle is new Glib.Main.Generic_Sources
     (Update_Menus_Data_Access);

   ----------------
   -- Menu_Model --
   ----------------

   procedure Parse_Menu_Model_From_XML
     (Kernel : not null access Kernel_Handle_Record'Class;
      Root   : Node);
   --  Create a GMenu_Model from the XML description used by GPS.
   --  This also stores enough information to create toolbars later on.

   function Create_Menubar_From_Model
     (Kernel  : not null access Kernel_Handle_Record'Class)
     return Gtk_Menu_Bar;
   --  Create a menu bar from a menu model.
   --  This is only needed when not using system menus, and will be removed as
   --  soon as we use those menus everywhere.

   ---------------
   -- Action_UI --
   ---------------
   --  Actions are associated to various UI element via this package. We keep
   --  track of these elements so that they can be removed when the action is
   --  destroyed.

   type UI_Element_Place is (In_Toolbar, In_Menu);
   type UI_Element (Place : UI_Element_Place := In_Menu) is record
      case Place is
         when In_Toolbar =>
            Toolbar_Name : Unbounded_String;
         when In_Menu =>
            Path : Unbounded_String;
      end case;
   end record;
   function Hash (Self : UI_Element) return Ada.Containers.Hash_Type;
   overriding function "=" (Left, Right : UI_Element) return Boolean;

   package UI_Elements is new Ada.Containers.Hashed_Sets
      (UI_Element, Hash, "=");
   use UI_Elements;

   package Action_Elements is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,              --  Action name
      Element_Type    => UI_Elements.Set,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => Case_Insensitive_Equal);
   use Action_Elements;
   --  Stores associations from actions to menu and toolbar items.

   procedure Add_Menu_To_Action (Action : String; Menu : String);
   procedure Add_Button_To_Action (Action : String; Toolbar : String);
   --  Associate a given menu and an action

   ---------------
   -- Utilities --
   ---------------

   procedure Propagate_Visibility
     (Widget : not null access Gtk_Widget_Record'Class;
      Parent : not null access Gtk_Container_Record'Class);
   --  Hide menu items for which all children are also hidden.
   --  Set menu items insensitive if all children are also insensitive
   --  Widget's visibility and sensitivity will be updated based on the
   --  list of children of Parent.

   procedure Recompute_State_And_Visibility (Shell : Gtk_Menu_Shell);
   --  Recompute the state of the menu and its submenus.

   procedure On_Menu_Map (Object : access Gtk_Widget_Record'Class);
   --  Called when Object (a Gtk_Menu) is mapped on the screen:
   --  recompute its sensitivitiy.

   procedure Connect_Submenu
     (Item    : Gtk_Menu_Item;
      Submenu : Gtk_Menu);
   --  Connect the submenu to the given menu item. This also takes care of
   --  connecting the menus to the proper signals, and compute the visibility.

   procedure Recompute_Object_State
     (Object : not null access GObject_Record'Class);
   --  Assuming Object is associated to an action, recompute the active
   --  state of Object according to the context and the filters that apply
   --  to this action.

   -------------
   -- Globals --
   -------------

   type Global_Data is record
      Menu_Model  : Gmenu;

      Actions_To_UI : Action_Elements.Map;
      --  Associations from actions to UI elements

      Toolbars : Toolbar_Maps.Map;

      Proxy_Items          : Proxy_Lists.List;
      --  The list of items whose sensitivity might need to be updated when the
      --  context changes.

      Unfiltered_Items     : Proxy_Lists.List;
      --  The list of all items associated with an action and that are not
      --  already part of Proxy_Items. These are generally ignored when the
      --  context changes.

      Update_Menus_Idle_Id : G_Source_Id := No_Source_Id;
   end record;

   Globals : Global_Data;
   --   ??? Should be stored in kernel

   ---------------
   -- Emphasize --
   ---------------

   function Emphasize (Name : String) return String is
   begin
      --  Another example would be:
      --    return "<span foreground=""blue"">" & Name & "</span>";
      return "<b>" & Glib.Convert.Escape_Text (Name) & "</b>";
   end Emphasize;

   ----------------------
   -- Substitute_Label --
   ----------------------

   function Substitute_Label
     (Text    : String;
      Context : Selection_Context;
      Custom  : Custom_Expansion := null) return String
   is
      Has_Error : Boolean := False;

      function Substitution (Param : String; Quoted : Boolean) return String;
      --  Substitute %P, %f,... as appropriate

      ------------------
      -- Substitution --
      ------------------

      function Substitution (Param : String; Quoted : Boolean) return String is
      begin
         if Param = "C" then
            if Custom /= null then
               declare
                  Result : constant String := Custom (Context);
               begin
                  if Result = "" then
                     Has_Error := True;
                     return "";
                  else
                     return Result;
                  end if;
               end;
            end if;

         else
            declare
               Done : aliased Boolean := False;
               Tmp  : constant String :=
                        Unknown_To_UTF8
                          (GPS.Kernel.Macros.Substitute
                             (Param, Context, Quoted, Done'Access));
            begin
               Has_Error := not Done;
               return Emphasize (Tmp);
            end;
         end if;

         Has_Error := True;
         return "";
      end Substitution;

      Tmp : constant String := Substitute
        (XML_Utils.Protect (Text),
         Delimiter => GPS.Kernel.Macros.Special_Character,
         Callback  => Substitution'Unrestricted_Access,
         Recursive => False);

   begin
      if Has_Error then
         return "";
      else
         return Tmp;
      end if;
   end Substitute_Label;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Creator : access Contextual_Label_Parameters;
      Context : Selection_Context) return String is
   begin
      if Filter_Matches (Creator.Filter, Context) then
         return Substitute_Label (Creator.Label.all, Context, Creator.Custom);
      else
         return "";
      end if;
   end Get_Label;

   ---------------------
   -- Compute_Tooltip --
   ---------------------

   function Compute_Tooltip
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget
   is
      use Abstract_Module_List;
      List    : constant Abstract_Module_List.List :=
        Kernel.Module_List (Module_ID_Record'Tag);
      Current : Abstract_Module_List.Cursor :=
        Abstract_Module_List.First (List);
      Module  : Module_ID;
      W       : Gtk_Widget;
   begin
      while Has_Element (Current) loop
         Module := Module_ID (Abstract_Module_List.Element (Current));
         if Module /= null then
            W := Tooltip_Handler (Module, Context);
            if W /= null then
               return W;
            end if;
         end if;

         Current := Abstract_Module_List.Next (Current);
      end loop;
      return null;
   end Compute_Tooltip;

   -------------------
   -- Create_Marker --
   -------------------

   function Create_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      Load   : XML_Utils.Node_Ptr := null) return Location_Marker
   is
      use Abstract_Module_List;
      use type XML_Utils.Node_Ptr;
      List    : constant Abstract_Module_List.List :=
        Kernel.Module_List (Module_ID_Record'Tag);
      Context : Selection_Context;
      Current : Abstract_Module_List.Cursor :=
        Abstract_Module_List.First (List);
      Module  : Module_ID;
      Marker  : Location_Marker;
   begin
      if Load = null then
         Context := Get_Current_Context (Kernel);
         Module := Module_ID (Get_Creator (Context));
         if Module /= null then
            return Bookmark_Handler (Module, null);
         end if;

      else
         while Has_Element (Current) loop
            Module := Module_ID (Abstract_Module_List.Element (Current));
            Marker := Bookmark_Handler (Module, Load);
            if not Marker.Is_Null then
               return Marker;
            end if;
            Current := Abstract_Module_List.Next (Current);
         end loop;
      end if;

      return No_Marker;
   end Create_Marker;

   -----------------------
   -- Contextual_Action --
   -----------------------

   procedure Contextual_Action
     (Object : access GObject_Record'Class;
      Action : Contextual_Menu_Access)
   is
      Success : Boolean;
      pragma Unreferenced (Object, Success);
   begin
      if Action.Menu_Type = Type_Action
         and then Action.Action /= null
      then
         Success := Execute_Action
           (Kernel  => Action.Kernel,
            Action  => Action.Action.all,
            Context => Action.Kernel.Last_Context_For_Contextual,
            Event   => GPS_Window
              (Action.Kernel.Get_Main_Window).Last_Event_For_Contextual,
            Show_Bar => True);
      end if;

   exception
      when E : others =>
         Trace (Me, E, Msg => "while executing " & Action.Name.all);
   end Contextual_Action;

   -------------------------------
   -- Contextual_Menu_Destroyed --
   -------------------------------

   procedure Contextual_Menu_Destroyed
     (Data   : System.Address;
      Object : System.Address)
   is
      pragma Unreferenced (Object);
      Kernel : constant Kernel_Handle := Convert (Data);
   begin
      Kernel.Contextual_Menu_Open := False;
      if Kernel.Last_Context_For_Contextual /= No_Context then
         Contextual_Menu_Close_Hook.Run (Kernel);
      end if;
   end Contextual_Menu_Destroyed;

   -----------------------------
   -- On_Contextual_Menu_Hide --
   -----------------------------

   procedure On_Contextual_Menu_Hide
     (Self  : access Gtk_Widget_Record'Class) is
   begin
      if Self.all in GPS_Contextual_Menu_Record'Class then
         GPS_Contextual_Menu (Self).Kernel.Contextual_Menu_Open := False;
      end if;
   end On_Contextual_Menu_Hide;

   ------------------------------------
   -- Add_Actions_To_Contextual_Menu --
   ------------------------------------

   procedure Add_Actions_To_Contextual_Menu
     (Context : Selection_Context;
      Menu    : in out Gtk.Menu.Gtk_Menu)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context);

      use type Gtk.Widget.Widget_List.Glist;

      function Menu_Is_Visible
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean;
      --  Whether the menu C should be made visible

      function Menu_Is_Sensitive
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean;
      --  Whether the menu C should be sensetive

      procedure Create_Item
        (C         : Contextual_Menu_Access;
         Context   : Selection_Context;
         Item      : out Gtk_Menu_Item;
         Full_Name : out GNAT.Strings.String_Access);
      --  Create the menu item to use when displaying C.
      --  Full_Name is the label of the menu.

      function Label_Name
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return String;
      --  Return the name of the label for C, including parent path

      function Has_Explicit_Parent
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean;
      --  Return True if the parent menu of C was explicitly registered by the
      --  user. In this case, C will be created only when that parent menu is
      --  created, not before.

      -----------------------
      -- Menu_Is_Sensitive --
      -----------------------

      function Menu_Is_Sensitive
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean is
      begin
         case C.Menu_Type is
            when Type_Separator =>
               return False;

            when Type_Action =>
               return True;

            when Type_Submenu =>
               return Filter_Matches (C.Submenu_Enable, Context);
         end case;
      end Menu_Is_Sensitive;

      ---------------------
      -- Menu_Is_Visible --
      ---------------------

      function Menu_Is_Visible
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean
      is
         Act : access Action_Record;
      begin
         if not C.Visible then
            return False;
         end if;

         case C.Menu_Type is
            when Type_Separator =>
               --  A separator is visible only:
               --     - If its own filter matches.
               --     - Or if it has no own filter, then only if the reference
               --       item is also visible in the case they are of the same
               --       group.
               --     - Always visible if there is no reference item
               --       and then only if it isn't the last entry the contextual
               --       menu.
               if C.Separator_Action /= null then
                  Act := Lookup_Action (C.Kernel, C.Separator_Action.all);
                  return Filter_Matches (Act, Context);
               end if;
               return True;

            when Type_Action =>
               Act := Lookup_Action (C);
               return Act /= null
                 and then Filter_Matches (Act, Context);

            when Type_Submenu =>
               return Filter_Matches (C.Submenu_Filter, Context);
         end case;
      end Menu_Is_Visible;

      -----------------
      -- Create_Item --
      -----------------

      procedure Create_Item
        (C         : Contextual_Menu_Access;
         Context   : Selection_Context;
         Item      : out Gtk_Menu_Item;
         Full_Name : out GNAT.Strings.String_Access)
      is
         use type GNAT.Strings.String_Access;
         Menu  : Gtk_Menu;
         Children : Widget_List.Glist;
      begin
         Full_Name := new String'(Label_Name (C, Context));

         case C.Menu_Type is
            when Type_Submenu =>
               declare
                  C2    : Contextual_Menu_Access;
                  Full2 : GNAT.Strings.String_Access;
                  Item  : Gtk_Menu_Item;
               begin
                  Gtk_New (Menu);

                  if C.Submenu /= null then
                     Append_To_Menu
                       (Factory => C.Submenu,
                        Context => Context,
                        Menu    => Menu);
                  end if;

                  --  Add all contextual menus that are children of C
                  C2 := Convert (Kernel.Contextual);
                  while C2 /= null loop
                     if C2.Filter_Matched then
                        if Parent_Menu_Name ('/' & Label_Name (C2, Context)) =
                          '/' & Full_Name.all & '/'
                        then
                           Create_Item (C2, Context, Item, Full2);
                           Add_Menu (Parent => Menu, Item => Item);
                           GNAT.OS_Lib.Free (Full2);
                        end if;
                     end if;
                     C2 := C2.Next;
                  end loop;
               end;

               Children := Get_Children (Menu);
               if Children = Gtk.Widget.Widget_List.Null_List then
                  Destroy (Menu);
                  Item := null;
               else
                  Gtk_New (Item, Base_Menu_Name (Full_Name.all));
                  Connect_Submenu (Item, Menu);
               end if;

               Widget_List.Free (Children);

            when Type_Separator =>
               declare
                  Sep : Gtk_Separator_Menu_Item;
               begin
                  Gtk_New (Sep);
                  Item := Gtk_Menu_Item (Sep);
               end;

            when Type_Action =>
               if Full_Name.all /= "" then
                  Gtk_New (Item, Base_Menu_Name (Full_Name.all));
                  Action_Callback.Connect
                    (Item, Gtk.Menu_Item.Signal_Activate,
                     Contextual_Action'Access,
                     User_Data   => C);
               else
                  Item := null;
               end if;
         end case;

         if Item /= null then
            Set_Sensitive (Item, C.Sensitive);

            declare
               Label : constant Gtk_Label :=
                         Gtk_Label (Get_Child (Item));
            begin
               if Label /= null then
                  Set_Use_Markup (Label, True);
               end if;
            end;
         end if;
      end Create_Item;

      ----------------
      -- Label_Name --
      ----------------

      function Label_Name
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return String
      is
         use type GNAT.Strings.String_Access;
      begin
         if C.Label = null then
            return C.Name.all;
         else
            --  Cache the expensive call to Get_Label in C.Label_For_Context.
            --  This name is used while building the menu to find parent
            --  items.
            if C.Label_For_Context = Null_Unbounded_String then
               C.Label_For_Context := To_Unbounded_String
                 (Get_Label (C.Label, Context));
            end if;
            return To_String (C.Label_For_Context);
         end if;
      end Label_Name;

      -------------------------
      -- Has_Explicit_Parent --
      -------------------------

      function Has_Explicit_Parent
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean
      is
         Label  : constant String := Label_Name (C, Context);
         Parent : constant String := Parent_Menu_Name ('/' & Label);
         C2     : Contextual_Menu_Access;
      begin
         if Parent /= "/" then
            C2 := Convert (Kernel.Contextual);
            while C2 /= null loop
               if C2.Filter_Matched
                 and then '/' & Label_Name (C2, Context) & '/' = Parent
               then
                  return True;
               end if;
               C2 := C2.Next;
            end loop;
         end if;
         return False;
      end Has_Explicit_Parent;

      Full_Name    : GNAT.Strings.String_Access;
      C            : Contextual_Menu_Access;
      Item         : Gtk_Menu_Item;
      Parent_Item  : Gtk_Menu_Item;
      Parent_Menu  : Gtk_Menu;
      Child        : Gtk_Widget;
      List         : Gtk.Widget.Widget_List.Glist;
      Is_Sensitive : Boolean;
      Key          : Gdk_Key_Type;
      Mods         : Gdk_Modifier_Type;
   begin
      Contextual_Menu_Open_Hook.Run (Kernel);

      --  Compute what items should be made visible, except for separators
      --  and submenus at the moment

      C := Convert (Kernel.Contextual);
      while C /= null loop
         if C.Menu_Type = Type_Action then
            C.Filter_Matched := Menu_Is_Visible (C, Context);
         end if;

         --  Reset the cache set in the previous contextual menu
         C.Label_For_Context := Null_Unbounded_String;
         C := C.Next;
      end loop;

      --  Same, but only for separators now, since their visibility might
      --  depend on the visibility of other items

      C := Convert (Kernel.Contextual);
      while C /= null loop
         if C.Menu_Type /= Type_Action then
            C.Filter_Matched := Menu_Is_Visible (C, Context);
         end if;
         C := C.Next;
      end loop;

      C := Convert (Kernel.Contextual);
      while C /= null loop
         Is_Sensitive := C.Filter_Matched
           and then not Has_Explicit_Parent (C, Context);

         if Is_Sensitive then
            C.Sensitive := Menu_Is_Sensitive (C, Context);

            Create_Item (C, Context, Item, Full_Name);

            if Item /= null then
               Item.Show_All;

               --  Do not force the creation of the parent menu for a
               --  separator: if it is a contextual submenu, it will have been
               --  created already if that menu should be visible (its
               --  Filtered_Matched is True).
               Parent_Item := Find_Or_Create_Menu_Tree
                 (Menu_Bar      => null,
                  Menu          => Menu,
                  Path          =>
                    Escape_Underscore (Parent_Menu_Name ('/' & Full_Name.all)),
                  Accelerators  => Get_Default_Accelerators (Kernel),
                  Allow_Create  => C.Menu_Type /= Type_Separator);

               if Parent_Item /= null then
                  Parent_Menu := Gtk_Menu (Get_Submenu (Parent_Item));
                  if Parent_Menu = null then
                     Gtk_New (Parent_Menu);
                     Set_Submenu (Parent_Item, Parent_Menu);
                  end if;

                  Add_Menu (Parent => Parent_Menu, Item => Item);
               elsif C.Menu_Type /= Type_Separator then
                  Add_Menu (Parent => Menu, Item => Item);
               end if;

               --  Display the key shortcut binded to the action if any
               if C.Menu_Type = Type_Action and then C.Action /= null then
                  Get_Shortcut_Simple
                    (Kernel,
                     Action => C.Action.all,
                     Key    => Key,
                     Mods   => Mods);

                  Child := Item.Get_Child;

                  if Child.all in Gtk_Accel_Label_Record'Class then
                     Gtk_Accel_Label (Child).Set_Accel (Key, Mods);
                  end if;
               end if;
            end if;

            GNAT.OS_Lib.Free (Full_Name);
         end if;

         C := C.Next;
      end loop;

      --  Do not Unref context, it will be automatically freed the next
      --  time a contextual menu is displayed.

      --  If the menu is empty, destroy it

      List := Get_Children (Menu);
      if List = Gtk.Widget.Widget_List.Null_List then
         Destroy (Menu);
         Menu := null;
      end if;

      Widget_List.Free (List);

      if Menu /= null then
         Menu.On_Hide (On_Contextual_Menu_Hide'Access, False);
         Menu.Weak_Ref
           (Contextual_Menu_Destroyed'Access, Data => Kernel.all'Address);
         Kernel.Contextual_Menu_Open := True;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         Destroy (Menu);
         Menu := null;
   end Add_Actions_To_Contextual_Menu;

   ----------------------------
   -- Create_Contextual_Menu --
   ----------------------------

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu
   is
      Dummy   : constant Block_Trace_Handle :=
         Create (Me, "Creating contextual menu");
      Context : Selection_Context;
      Menu    : Gtk_Menu := null;
      Win    : constant GPS_Window := GPS_Window (User.Kernel.Get_Main_Window);
      Child  : MDI_Child;
   begin
      --  Create the menu and add all the modules information
      Menu := new GPS_Contextual_Menu_Record;
      Gtk.Menu.Initialize (Menu);
      GPS_Contextual_Menu (Menu).Kernel := User.Kernel;

      --  Compute the context for the menu. We cannot reuse the current context
      --  stored in the kernel, since the user might be clicking anywhere in
      --  the widget (or even in another widget), and we need to use that
      --  location.

      Child := Find_MDI_Child_From_Widget (User.Event_Widget);
      if Child.all in GPS_MDI_Child_Record'Class then
         Context := GPS_MDI_Child (Child).Build_Context (Event);
      else
         Context := New_Context (User.Kernel);
      end if;

      User.Kernel.Last_Context_For_Contextual := Context;
      User.Kernel.Last_Context_From_Contextual := True;

      --  Do we need to add hand-coded items to the menu ?

      if User.Context_Func /= null then
         User.Context_Func (Context, Menu);
      end if;

      if Win.Last_Event_For_Contextual /= null then
         Free (Win.Last_Event_For_Contextual);
      end if;

      Win.Last_Event_For_Contextual := Copy (Event);

      --  Override the previous value. No Ref is taken explicitly, so we do not
      --  need to Unref either. This field is automatically reset to null when
      --  the last holder of a ref calls Unref.

      Add_Actions_To_Contextual_Menu (Context, Menu);

      return Menu;
   exception
      when E : others =>
         Trace (Me, E);
         return null;
   end Create_Contextual_Menu;

   ---------------------------
   -- Setup_Contextual_Menu --
   ---------------------------

   procedure Setup_Contextual_Menu
     (Kernel          : access Kernel_Handle_Record'Class;
      Event_On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context_Func    : Contextual_Menu_Factory := null)
   is
      User_Data : constant Contextual_Menu_User_Data :=
        (Context_Func => Context_Func,
         Event_Widget => Gtk_Widget (Event_On_Widget),
         Kernel       => Kernel_Handle (Kernel));
   begin
      Kernel_Contextuals.Register_Contextual_Menu
        (Event_On_Widget,
         User_Data,
         Menu_Create  => Create_Contextual_Menu'Access);
   end Setup_Contextual_Menu;

   --------------------
   -- Find_Menu_Item --
   --------------------

   function Find_Menu_Item
     (Menubar : access Gtk_Menu_Bar_Record'Class;
      Path    : String)
      return Gtk.Menu_Item.Gtk_Menu_Item is
   begin
      if Menubar = null then
         return null;
      end if;
      return Find_Or_Create_Menu_Tree
        (Menu_Bar      => Gtk_Menu_Bar (Menubar),
         Menu          => null,
         Path          => Escape_Underscore (Path),
         Accelerators  => null,
         Allow_Create  => False);
   end Find_Menu_Item;

   ----------------------
   -- Action_From_Menu --
   ----------------------

   function Action_From_Menu
     (Kernel : not null access Kernel_Handle_Record'Class;
      Path   : String) return String
   is
      Item : Gtk_Menu_Item;
   begin
      if Path /= "" and then Path (Path'First) = '/' then
         Item := Find_Menu_Item
            (GPS_Application_Window (Get_Main_Window (Kernel)).Menu_Bar,
             Path);
         if Item /= null
           and then Item.all in Action_Menu_Item_Record'Class
         then
            return Action_Menu_Item (Item).Data.Action.all;
         end if;
      end if;
      return Path;
   end Action_From_Menu;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Self : not null access GObject_Record'Class)
      return access Action_Proxy'Class is
   begin
      if Self.all in Action_Menu_Item_Record'Class then
         return Action_Menu_Item (Self).Data'Access;
      elsif Self.all in Action_Tool_Button_Record'Class then
         return Action_Tool_Button (Self).Data'Access;
      elsif Self.all in GPS_Action_Record'Class then
         return GPS_Action (Self).Data'Access;
      else
         return null;
      end if;
   end Get_Data;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Self : not null access Contextual_Menu_Record)
      return access Action_Record
   is
   begin
      case Self.Menu_Type is
         when Type_Action =>
            if Self.Action = null then
               return null;
            else
               return Lookup_Action (Self.Kernel, Self.Action.all);
            end if;
         when others =>
            return null;
      end case;
   end Lookup_Action;

   ----------
   -- Hash --
   ----------

   function Hash (Self : UI_Element) return Ada.Containers.Hash_Type is
   begin
      case Self.Place is
         when In_Menu =>
            return Ada.Strings.Hash (To_String (Self.Path));
         when In_Toolbar =>
            return Ada.Strings.Hash (To_String (Self.Toolbar_Name));
      end case;
   end Hash;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : UI_Element) return Boolean is
   begin
      if Left.Place /= Right.Place then
         return False;
      else
         case Left.Place is
            when In_Menu => return Left.Path = Right.Path;
            when In_Toolbar => return Left.Toolbar_Name = Right.Toolbar_Name;
         end case;
      end if;
   end "=";

   --------------------------
   -- Add_Button_To_Action --
   --------------------------

   procedure Add_Button_To_Action (Action : String; Toolbar : String) is
      E : constant Action_Elements.Cursor :=
        Globals.Actions_To_UI.Find (Action);
      Item : constant UI_Element :=
        (Place => In_Toolbar, Toolbar_Name => To_Unbounded_String (Toolbar));
   begin
      if Has_Element (E) then
         Globals.Actions_To_UI.Reference (E).Include (Item);
      else
         declare
            S : UI_Elements.Set;
         begin
            S.Include (Item);
            Globals.Actions_To_UI.Include (Action, S);
         end;
      end if;
   end Add_Button_To_Action;

   ------------------------
   -- Add_Menu_To_Action --
   ------------------------

   procedure Add_Menu_To_Action (Action, Menu : String) is
      E : constant Action_Elements.Cursor :=
        Globals.Actions_To_UI.Find (Action);
      Item : constant UI_Element :=
        (Place => In_Menu, Path => To_Unbounded_String (Menu));
   begin
      if Has_Element (E) then
         Globals.Actions_To_UI.Reference (E).Include (Item);
      else
         declare
            S : UI_Elements.Set;
         begin
            S.Include (Item);
            Globals.Actions_To_UI.Include (Action, S);
         end;
      end if;
   end Add_Menu_To_Action;

   ---------------------------------
   -- Update_Shortcuts_For_Action --
   ---------------------------------

   procedure Update_Shortcuts_For_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String)
   is
      Key    : Gdk_Key_Type;
      Mods   : Gdk_Modifier_Type;
      C      : constant Action_Elements.Cursor :=
        Globals.Actions_To_UI.Find (Action);

      procedure On_Button (C : not null access Gtk_Widget_Record'Class);
      procedure On_Button (C : not null access Gtk_Widget_Record'Class) is
         Action : constant access Action_Record := Lookup_Action (C);
      begin
         if Action /= null then
            Tooltips.Set_Static_Tooltip
              (Gtk_Widget (C),
               Text       => Get_Full_Description (Action, Kernel),
               Use_Markup => True);
         end if;
      end On_Button;

      procedure For_Toolbar
        (Toolbar : not null access Gtk_Toolbar_Record'Class);
      procedure For_Toolbar
        (Toolbar : not null access Gtk_Toolbar_Record'Class) is
      begin
         Toolbar.Foreach (On_Button'Unrestricted_Access);
      end For_Toolbar;

      procedure Internal
         (Win : not null access GPS_Application_Window_Record'Class);
      procedure Internal
         (Win : not null access GPS_Application_Window_Record'Class)
      is
         Item   : Gtk_Menu_Item;
         Child  : Gtk_Widget;
      begin
         if Win.Menu_Bar /= null then
            for M of Globals.Actions_To_UI.Reference (C) loop
               case M.Place is
                  when In_Menu =>
                     Item := Find_Menu_Item (Win.Menu_Bar, To_String (M.Path));
                     if Item /= null then
                        Child := Item.Get_Child;
                        if Child.all in Gtk_Accel_Label_Record'Class then
                           Gtk_Accel_Label (Child).Set_Accel (Key, Mods);
                        end if;
                     end if;

                  when In_Toolbar =>
                     For_Each_Toolbar
                       (Kernel,
                        To_String (M.Toolbar_Name),
                        For_Toolbar'Access);
               end case;
            end loop;
         end if;
      end Internal;

   begin
      if Has_Element (C) then
         Kernel.Get_Shortcut_Simple
           (Action => Action,
            Key    => Key,
            Mods   => Mods);
         For_All_Open_Windows (Kernel.Get_Application, Internal'Access);
      end if;
   end Update_Shortcuts_For_Action;

   --------------------------
   -- Menu_List_For_Action --
   --------------------------

   function Menu_List_For_Action (Action : String) return Unbounded_String
   is
      C : constant Action_Elements.Cursor :=
         Globals.Actions_To_UI.Find (Action);
      Result : Unbounded_String;
   begin
      if Has_Element (C) then
         for M of Globals.Actions_To_UI.Reference (C) loop
            if M.Place = In_Menu then
               Append (Result, ASCII.LF);
               Append (Result, M.Path);
            end if;
         end loop;
      end if;
      return Result;
   end Menu_List_For_Action;

   ----------------------
   -- For_Each_Toolbar --
   ----------------------

   procedure For_Each_Toolbar
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String;
      Callback : not null access procedure
        (Toolbar : not null access Gtk_Toolbar_Record'Class))
   is
      procedure Internal
        (W : not null access GPS_Application_Window_Record'Class);
      procedure Internal
        (W : not null access GPS_Application_Window_Record'Class)
      is
      begin
         if W.Toolbar /= null then
            Callback (W.Toolbar);
         end if;
      end Internal;

      C     : Child_Iterator;
      Child : MDI_Child;
      Toolbar : Gtk_Toolbar;
   begin
      if Name = "main" then
         For_All_Open_Windows (Kernel.Get_Application, Internal'Access);
      else
         C := Get_MDI (Kernel).First_Child;
         loop
            Child := Get (C);
            exit when Child = null;

            if Child.all in GPS_MDI_Child_Record'Class then
               Toolbar := GPS_MDI_Child (Child).Get_Toolbar;
               if Toolbar /= null
                 and then Toolbar.Get_Name = Name
               then
                  Callback (Toolbar);
               end if;
            end if;

            Next (C);
         end loop;
      end if;
   end For_Each_Toolbar;

   --------------------------
   -- Remove_UI_For_Action --
   --------------------------

   procedure Remove_UI_For_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String)
   is
      Path : Unbounded_String;

      procedure Remove_Menu
        (W : not null access GPS_Application_Window_Record'Class);
      --  Remove the menu Path from W's menubar

      procedure Remove_Button
        (Toolbar : not null access Gtk_Toolbar_Record'Class);
      --  Remove all buttons for Action from toolbar

      procedure Remove_Menu
        (W : not null access GPS_Application_Window_Record'Class)
      is
         M : Gtk_Menu_Item;
      begin
         if W.Menu_Bar /= null then
            M := Find_Menu_Item (W.Menu_Bar, To_String (Path));
            if M /= null then
               M.Destroy;
            end if;
         end if;
      end Remove_Menu;

      procedure Remove_Button
        (Toolbar : not null access Gtk_Toolbar_Record'Class)
      is
         package Widget_List is new Ada.Containers.Doubly_Linked_Lists
           (Gtk_Widget);
         To_Remove : Widget_List.List;

         procedure On_Child (C : not null access Gtk_Widget_Record'Class);
         procedure On_Child (C : not null access Gtk_Widget_Record'Class) is
         begin
            if C.all in Action_Tool_Button_Record'Class  then
               if Action_Tool_Button (C).Data.Action.all = Action then
                  Toolbar.Remove (C);
               end if;
            elsif C.all in Action_Combo_Tool_Record'Class then
               Action_Combo_Tool (C).Remove_Action (Action);
               if not Action_Combo_Tool (C).Has_Items then
                  --  Tampering risk: do not remove widgets in a Foreach on the
                  --  container that contains them.
                  To_Remove.Append (C);
               end if;
            end if;
         end On_Child;
      begin
         Toolbar.Foreach (On_Child'Unrestricted_Access);
         for C of To_Remove loop
            Toolbar.Remove (C);
         end loop;
      end Remove_Button;

      Item : Menu_Item_Info;
      C : Action_Elements.Cursor := Globals.Actions_To_UI.Find (Action);
   begin
      Trace (Me, "Remove all UI for action " & Action);
      if Has_Element (C) then
         for M of Globals.Actions_To_UI.Reference (C) loop
            case M.Place is
               when In_Menu =>
                  Path := M.Path;
                  Item := Find_Or_Create_Menu
                    (Globals.Menu_Model,
                     Escape_Underscore (To_String (Path)),
                     Allow_Create => False);
                  if Item /= No_Menu_Item then
                     Item.Model.Remove (Item.Position);
                     Unref (Item);
                  end if;

                  For_All_Open_Windows
                    (Kernel.Get_Application, Remove_Menu'Access);

               when In_Toolbar =>
                  --  Remove from live toolbars

                  For_Each_Toolbar
                    (Kernel,
                     To_String (M.Toolbar_Name),
                     Remove_Button'Access);

                  --  Remove from the model for future windows and views

                  declare
                     D : Toolbar_Description renames
                       Globals.Toolbars.Reference (To_String (M.Toolbar_Name));
                     C, C2 : Buttons_List.Cursor;
                  begin
                     C := D.Buttons.First;
                     while Buttons_List.Has_Element (C) loop
                        C2 := Buttons_List.Next (C);

                        if not Buttons_List.Element (C).Is_Separator
                          and then Buttons_List.Element (C).Action = Action
                        then
                           D.Buttons.Delete (C);
                        end if;

                        C := C2;
                     end loop;
                  end;
            end case;
         end loop;

         Globals.Actions_To_UI.Delete (C);
      end if;
   end Remove_UI_For_Action;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Self : not null access GObject_Record'Class)
     return access Action_Record
   is
      Label : Gtk_Accel_Label;
      Key   : Gdk_Key_Type;
      Mods  : Gdk_Modifier_Type;
      Action : access Action_Record;
      Data   : constant access Action_Proxy'Class := Get_Data (Self);

   begin
      if Data = null then
         return null;
      end if;

      Action := Lookup_Action (Data.Kernel, Data.Action.all);
      if Action /= Data.Looked_Up then
         Data.Looked_Up := Action;

         if Action /= null then
            if Self.all in Gtk_Widget_Record'Class then
               Tooltips.Set_Static_Tooltip
                 (Gtk_Widget (Self),
                  Text       => Get_Full_Description (Action, Data.Kernel),
                  Use_Markup => True);
               Get_Style_Context (Gtk_Widget (Self)).Remove_Class
                 ("nogpsaction");
            end if;

            --  Update the image if the action has one

            declare
               Icon : constant String :=  Get_Icon_Name (Action);
            begin
               if Icon /= ""
                 and then Self.all in Action_Tool_Button_Record'Class
                 and then not Action_Tool_Button (Self).Forced_Stock
               then
                  Action_Tool_Button (Self).Set_Icon_Name (Icon);
               end if;
            end;

            --  Lookup the keybinding. This is only done the first time we do
            --  the lookup to save time. Later on, this is updated via
            --  Update_Shortcuts_For_Action.

            if Self.all in Action_Menu_Item_Record'Class then
               Data.Kernel.Get_Shortcut_Simple
                 (Action => Data.Action.all,
                  Key    => Key,
                  Mods   => Mods);
               if Key /= 0 then
                  Label := Gtk_Accel_Label (Action_Menu_Item (Self).Get_Child);
                  Label.Set_Accel (Key, Mods);
               end if;
            end if;

         elsif Self.all in Gtk_Widget_Record'Class then
            Gtk_Widget (Self).Set_Tooltip_Text
              ("Action not found: " & Data.Action.all);
            Get_Style_Context (Gtk_Widget (Self)).Add_Class ("nogpsaction");
         end if;
      end if;

      return Action;
   end Lookup_Action;

   --------------------
   -- Execute_Action --
   --------------------

   procedure Execute_Action
     (Self          : not null access GObject_Record'Class;
      Data          : Action_Proxy'Class;
      In_Foreground : Boolean := False)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      --  Do not block GPS exit (only if the command itself spawns another
      --  command, like an external process, and that other command could
      --  block).

      Success := Execute_Action
        (Kernel      => Data.Kernel,
         Action      => Data.Action.all,
         Synchronous => In_Foreground,
         Via_Menu    => Self.all in Action_Menu_Item_Record'Class,
         Error_Msg_In_Console => True,
         Block_Exit  => False,
         Show_Bar    => True);
   end Execute_Action;

   -----------------------------
   -- On_Activate_Action_Item --
   -----------------------------

   procedure On_Activate_Action_Item
     (Item : access Gtk_Menu_Item_Record'Class)
   is
      Self : constant Action_Menu_Item := Action_Menu_Item (Item);
   begin
      Execute_Action (Self, Self.Data);
   end On_Activate_Action_Item;

   ---------------------------
   -- Add_To_Global_Proxies --
   ---------------------------

   procedure Add_To_Global_Proxies
     (Item   : not null access GObject_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class;
      Filter : access Action_Filter_Record'Class)
   is
   begin
      Weak_Ref
        (Item,
         Notify => On_Delete_Proxy'Access,
         Data   => Kernel.all'Address);

      if Item.all in Gtk_Tool_Item_Record'Class
        or else (Item.all in Gtk_Menu_Item_Record'Class
                 and System_Menus.Get_Pref)
      then
         --  This is a toolbar button or this is a menu item and we are
         --  not using system menus: we need to add this to the list of
         --  items that refreshed with every context change, since there
         --  isn't another specific point at which to monitor their
         --  status.

         Globals.Proxy_Items.Append (Proxy_And_Filter'(Item, Filter));

         --  If the background updating of menus was taking place, we need to
         --  restart it since its iterators are now invalid.

         if Globals.Update_Menus_Idle_Id /= No_Source_Id then
            Update_Menus_And_Buttons (Kernel);
         end if;

      else
         --  We are adding an action, or we are adding a menu item and we are
         --  not using system menus: we will refresh the state of this menu
         --  item when the menu is mapped, so we can add this directly to the
         --  list of unfiltered items.

         Add_To_Unfiltered_Items (Proxy_And_Filter'(Item, Filter));
      end if;
   end Add_To_Global_Proxies;

   -----------------------------
   -- Add_To_Unfiltered_Items --
   -----------------------------

   procedure Add_To_Unfiltered_Items (Item : Proxy_And_Filter) is
   begin
      Globals.Unfiltered_Items.Append (Item);
      --  This preserves the Weak_Ref we had on the item
   end Add_To_Unfiltered_Items;

   ---------------------
   -- On_Delete_Proxy --
   ---------------------

   procedure On_Delete_Proxy
     (Kernel : System.Address;
      Item   : System.Address)
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Kernel_Handle);
      K    : constant Kernel_Handle := Convert (Kernel);
      Obj  : constant GObject := Get_User_Data_Or_Null (Item);

      It   : Proxy_And_Filter;
      C    : Proxy_Lists.Cursor := Globals.Proxy_Items.First;
      Data : constant access Action_Proxy := Get_Data (Obj);
   begin
      if Data /= null then
         Free (Data.Action);
      end if;

      while Has_Element (C) loop
         It := Proxy_Lists.Element (C);
         if Get_Object (It.Proxy) = Item then
            Globals.Proxy_Items.Delete (C);

            --  Update cursor in the background updating, if needed

            if Globals.Update_Menus_Idle_Id /= No_Source_Id then
               Update_Menus_And_Buttons (K);
            end if;

            return;
         end if;
         Next (C);
      end loop;

      --  Not found in the Proxy_Items list, might be in the Unfiltered_Items
      --  list
      C := Globals.Unfiltered_Items.First;
      while Has_Element (C) loop
         It := Proxy_Lists.Element (C);
         if Get_Object (It.Proxy) = Item then
            Globals.Unfiltered_Items.Delete (C);
            return;
         end if;
         Next (C);
      end loop;
   end On_Delete_Proxy;

   -------------------
   -- Register_Menu --
   -------------------

   procedure Register_Menu
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Path          : String;
      Action        : String;
      Ref_Item      : String := "";
      Add_Before    : Boolean := True)
   is
      Full_Path   : constant String := Create_Menu_Path ("/", Path);
      Parent_Path : constant String := Parent_Menu_Name (Full_Path);
      Menu_Label  : constant String := Base_Menu_Name (Full_Path);

      procedure Add_To_Win
        (Win : not null access GPS_Application_Window_Record'Class);
      --  Add to the menubar for the specied window

      procedure Add_To_Win
        (Win : not null access GPS_Application_Window_Record'Class)
      is
         Parent, Pred, Item : Gtk_Menu_Item;
         Parent_Menu        : Gtk_Menu;
         Index              : Gint;
         Act                : access Action_Record;
         pragma Unreferenced (Act);
      begin
         if Win.Menu_Bar /= null then
            --  Find or create the parent menu

            if Parent_Path = "/" then
               --  ??? When creating from a <submenu> node in a plugin
               return;
            end if;

            Parent := Find_Or_Create_Menu_Tree
              (Menu_Bar     => Win.Menu_Bar,
               Menu         => null,
               Path         => Parent_Path,
               Accelerators => Get_Default_Accelerators (Kernel),
               Add_Before   => Add_Before,
               Ref_Item     => Ref_Item,
               Allow_Create => True);

            if Parent = null then
               Trace (Me, "Register_Menu: Parent menu not found for "
                      & Parent_Path & " (menu path is "
                      & Full_Path & ")");
               return;
            else
               Parent_Menu := Gtk_Menu (Get_Submenu (Parent));
               if Parent_Menu = null then
                  Gtk_New (Parent_Menu);
               end if;
               Connect_Submenu (Parent, Parent_Menu);
            end if;

            --  Find the reference menu item so that we can insert in proper
            --  place

            Find_Menu_Item_By_Name
              (Menu_Bar  => Win.Menu_Bar,
               Menu      => Parent_Menu,
               Name      => Ref_Item,
               Menu_Item => Pred,
               Index     => Index);

            --  Add the new item

            if Action = ""
               or else Menu_Label = ""
               or else Menu_Label (Menu_Label'First) = '-'
            then
               Item := Gtk_Menu_Item (Gtk_Separator_Menu_Item_New);
            else
               Item := Gtk_New_Action_Item
                 (Kernel      => Kernel,
                  Full_Path   => Parent_Path & Escape_Menu_Name (Menu_Label),
                  Menu_Label  => Menu_Label,
                  Action      => Action,
                  Optional    => False);
            end if;

            if Index = -1 then
               Parent_Menu.Append (Item);
            elsif Add_Before then
               Parent_Menu.Insert (Item, Index);
            else
               Parent_Menu.Insert (Item, Index + 1);
            end if;

            Item.Show_All;
         end if;
      end Add_To_Win;

      Item     : Menu_Item_Info;
      Ref      : Menu_Item_Info := No_Menu_Item;
      It       : Gmenu_Item;
      Act      : GPS_Action;
      Menu     : Gmenu;
   begin
      Add_Menu_To_Action (Action, Strip_Single_Underscores (Full_Path));

      Item := Find_Or_Create_Menu (Globals.Menu_Model, Parent_Path);
      if Item /= No_Menu_Item then
         if Action /= ""
            and then Menu_Label /= ""
            and then Menu_Label (Menu_Label'First) /= '-'
         then
            Act := Create_Or_Lookup_Action (Kernel, Action);
            G_New
               (It,
                Escape_Underscore (Strip_Single_Underscores (Menu_Label)),
                Detailed_Action => Act.Gtk_Name);
         else
            G_New_Section (It, "", Gmenu_New);
         end if;

         if Ref_Item /= "" then
            Ref := Find_Or_Create_Single_Level
               (Gmenu (Item.Item.Get_Link ("submenu")),
                Ref_Item, Allow_Create => False);
         end if;

         if Ref = No_Menu_Item then
            Menu := Gmenu (Item.Item.Get_Link ("submenu"));
            if Menu /= null then
               Menu.Append_Item (It);
            else
               Trace (Me, "Error adding menu " & Path & " ref=" & Ref_Item);
            end if;
         elsif Add_Before then
            Ref.Model.Insert_Item (Ref.Position, It);
         else
            Ref.Model.Insert_Item (Ref.Position + 1, It);
         end if;

         Unref (It);
         Unref (Ref);
         Unref (Item);
      end if;

      For_All_Open_Windows (Kernel.Get_Application, Add_To_Win'Access);
   end Register_Menu;

   -------------------------
   -- Gtk_New_Action_Item --
   -------------------------

   function Gtk_New_Action_Item
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Full_Path     : String;
      Menu_Label    : String;
      Action        : String;
      Optional      : Boolean := False) return Gtk_Menu_Item
   is
      Item : constant Action_Menu_Item := new Action_Menu_Item_Record;
   begin
      Gtk.Menu_Item.Initialize_With_Mnemonic (Item, Label => Menu_Label);
      Item.Data := (Action    => new String'(Action),
                    Kernel    => Kernel,
                    Optional  => Optional,
                    Hide      => False,
                    Looked_Up => null);

      --  The accel path is necessary to show the menu path in the
      --  Key Shortcuts editor, and tooltips for actions.
      Item.Set_Accel_Path ("<gps>" & Full_Path);

      Item.On_Activate (On_Activate_Action_Item'Access);
      Add_To_Global_Proxies (Item, Kernel, null);

      return Gtk_Menu_Item (Item);
   end Gtk_New_Action_Item;

   ------------------
   -- Execute_Menu --
   ------------------

   procedure Execute_Menu
     (Kernel    : Kernel_Handle;
      Menu_Name : String)
   is
      Menu : constant Gtk_Menu_Item := Find_Menu_Item
         (GPS_Application_Window (Get_Main_Window (Kernel)).Menu_Bar,
          Menu_Name);
   begin
      if Menu = null then
         Kernel.Insert (-"Can't execute " & Menu_Name, Mode => Error);
      elsif Menu.all in Action_Menu_Item_Record'Class then
         Execute_Action
           (Menu, Action_Menu_Item (Menu).Data, In_Foreground => True);
      else
         Activate (Menu);
      end if;
   end Execute_Menu;

   -------------------------
   -- Get_Toolbar_Section --
   -------------------------

   function Get_Toolbar_Section
     (Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Section : String;
      Last    : Boolean := True) return Glib.Gint
   is
      Count : Gint;
      Item  : Gtk_Tool_Item;
      In_Section : Boolean := False;
   begin
      Count := Toolbar.Get_N_Items;
      for J in 0 .. Count - 1 loop
         Item := Toolbar.Get_Nth_Item (J);

         if Item.all in Gtk_Separator_Tool_Item_Record'Class then
            if In_Section then
               --  We know that Last was set to True, necessarily
               return J;

            elsif Item.Get_Name = Section then
               if not Last then
                  return J + 1;  --  first item in following section
               else
                  In_Section := True;
               end if;
            end if;
         end if;
      end loop;
      return -1;
   end Get_Toolbar_Section;

   --------------------
   -- Create_Toolbar --
   --------------------

   procedure Create_Toolbar
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Toolbar         : in out Gtk.Toolbar.Gtk_Toolbar;
      Id              : String)
   is
      procedure Process_Toolbar (Descr : Toolbar_Description);
      --  Create a toolbar from its description

      ---------------------
      -- Process_Toolbar --
      ---------------------

      procedure Process_Toolbar (Descr : Toolbar_Description) is
         Sep     : Gtk_Separator_Tool_Item;
         Is_First : Boolean := True;
      begin
         for B of Descr.Buttons loop
            if B.Is_Separator then
               if not Is_First then
                  Gtk_New (Sep);
                  Sep.Set_Name (To_String (B.Start_Of_Section));
                  Toolbar.Insert (Sep);
               end if;
            else
               Insert_Button
                 (Kernel, Toolbar, B,
                  Section         => "",  --  always append
                  Toolbar_Id      => Id);
            end if;
            Is_First := False;
         end loop;

         if Descr.Inherit /= ""
            and then Globals.Toolbars.Contains (To_String (Descr.Inherit))
         then
            Process_Toolbar
               (Globals.Toolbars.Constant_Reference
                  (To_String (Descr.Inherit)));
         end if;
      end Process_Toolbar;

   begin
      if Toolbar = null then
         Gtk_New (Toolbar);
      else
         Remove_All_Children (Toolbar);
      end if;

      Toolbar.Set_Name (Id);  --  used in For_Each_Toolbar
      Toolbar.Set_Icon_Size (Icon_Size_Small_Toolbar);
      Toolbar.Set_Style (Toolbar_Icons);
      Toolbar.Set_Show_Arrow (True);

      if Globals.Toolbars.Contains (Id) then
         Process_Toolbar (Globals.Toolbars.Constant_Reference (Id));
      end if;
   end Create_Toolbar;

   ---------------------
   -- Register_Button --
   ---------------------

   procedure Register_Button
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Action          : String;
      Icon_Name       : String := "";
      Label           : String := "";
      Toolbar         : String := "main";
      Section         : String := "";
      Group           : String := "";
      Hide            : Boolean := False)
   is
      Descr : constant Toolbar_Button :=
        (Is_Separator    => False,
         Action          => To_Unbounded_String (Action),
         Icon_Name       => To_Unbounded_String (Icon_Name),
         Label           => To_Unbounded_String (Label),
         Group           => To_Unbounded_String (Group),
         Hide            => Hide);

      procedure For_Toolbar (Bar : not null access Gtk_Toolbar_Record'Class);
      procedure For_Toolbar (Bar : not null access Gtk_Toolbar_Record'Class) is
      begin
         Insert_Button
           (Kernel, Bar, Descr,
            Section         => Section,
            Toolbar_Id      => Toolbar);
      end For_Toolbar;

   begin
      --  Register in the model, so that future floating windows also
      --  get those buttons

      if not Globals.Toolbars.Contains (Toolbar) then
         Globals.Toolbars.Include (Toolbar, (others => <>));
      end if;

      Add_Button_To_Action (Action, Toolbar);

      declare
         D : Toolbar_Description renames
           Globals.Toolbars.Reference (Toolbar);
         C : Buttons_List.Cursor;
         In_Section : Boolean := False;
      begin
         if Section = "" then
            D.Buttons.Append (Descr);
         else
            C := D.Buttons.First;
            while Buttons_List.Has_Element (C) loop
               if Buttons_List.Element (C).Is_Separator then
                  if In_Section then
                     D.Buttons.Insert (Before => C, New_Item => Descr);
                     exit;
                  elsif Buttons_List.Element (C).Start_Of_Section = Section
                  then
                     In_Section := True;
                  end if;
               end if;
               Buttons_List.Next (C);
            end loop;

            if not In_Section then
               D.Buttons.Append (New_Item => Descr);
            end if;
         end if;
      end;

      --  Now add the button to all live toolbars

      For_Each_Toolbar (Kernel, Toolbar, For_Toolbar'Access);
   end Register_Button;

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   procedure Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Object);

      Context : constant Drag_Context :=
                  Drag_Context (Get_Object (Nth (Args, 1)));
      Data    : constant Gtk_Selection_Data :=
                  From_Object (Get_Address (Nth (Args, 4)));
      Time    : constant Guint32 := Guint32 (Get_Uint (Nth (Args, 6)));
   begin
      if Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
      then
         declare
            Uris : constant GNAT.Strings.String_List := Data.Get_Uris;
         begin
            for Url of Uris loop
               declare
                  File : constant Virtual_File :=
                    Create (+Filename_From_URI (Url.all, null));
               begin
                  if File_Extension (File) = Project_File_Extension then
                     Load_Project (Kernel, File);
                  else
                     Open_File_Action_Hook.Run
                       (Kernel, File,
                        Project  => No_Project,  --  will choose a random one
                        New_File => False);
                  end if;
               end;
            end loop;
         end;
         Gtk.Dnd.Finish
           (Context, Success => True, Del => False, Time => Time);
      else
         Gtk.Dnd.Finish
           (Context, Success => False, Del => False, Time => Time);
      end if;
   end Drag_Data_Received;

   -------------------------
   -- Add_Contextual_Menu --
   -------------------------

   procedure Add_Contextual_Menu
     (Kernel     : access Kernel_Handle_Record'Class;
      Menu       : Contextual_Menu_Access;
      Ref_Item   : String := "";
      Add_Before : Boolean := True)
   is
      C, Previous : Contextual_Menu_Access;
      Menu_Ref    : Contextual_Menu_Reference;
      Ref_Found   : Boolean := False;
   begin
      Menu_Ref := Find_Contextual_Menu_By_Name (Kernel, Menu.Name.all);

      if Menu.Name.all /= "" and then Menu_Ref /= Null_Reference then
         --  Menu already exists for this name we want to replace the existing
         --  one by the new one.
         Trace (Me, "Contextual menu already registered: " & Menu.Name.all);

         Menu.Next := Menu_Ref.Menu.Next;

         if Menu_Ref.Previous = null then
            Kernel.Contextual := Convert (Menu);
         else
            Menu_Ref.Previous.Next := Menu;
         end if;

         --  Now Menu_Ref.Menu is not pointed anymore, free associated memory

         GNAT.OS_Lib.Free (Menu_Ref.Menu.Name);
         Unchecked_Free (Menu_Ref.Menu);

      else
         if Kernel.Contextual /= System.Null_Address then
            C := Convert (Kernel.Contextual);

            --  Look for element C after which we should insert new menu item.
            --  If C = null insert at head of the list
            loop
               if C.Group > Menu.Group then
                  --  Insert before greater group
                  C := Previous;

                  exit;
               elsif C.Group = Menu.Group
                 and then Ref_Item /= ""
                 and then C.Name.all = Ref_Item
               then
                  --  We've found Ref_Item in given group
                  if Add_Before then
                     C := Previous;
                  end if;

                  Ref_Found := True;

                  exit;
               elsif C.Next = null then
                  --  End of list
                  exit;
               end if;

               Previous := C;
               C := C.Next;
            end loop;

            if Ref_Item /= "" and then not Ref_Found then
               Trace (Me, "Ref_Item not found (" & Ref_Item & ") when adding "
                      & Menu.Name.all);
            end if;

            if C = null then
               Menu.Next         := Convert (Kernel.Contextual);
               Kernel.Contextual := Convert (Menu);
            else
               Menu.Next := C.Next;
               C.Next := Menu;
            end if;

         else
            Kernel.Contextual := Convert (Menu);
         end if;
      end if;
   end Add_Contextual_Menu;

   ----------------------------------
   -- Find_Contextual_Menu_By_Name --
   ----------------------------------

   function Find_Contextual_Menu_By_Name
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Contextual_Menu_Reference
   is
      P, C : Contextual_Menu_Access;
   begin
      if Kernel.Contextual /= System.Null_Address then
         P := null;
         C := Convert (Kernel.Contextual);
         while C /= null loop
            if C.Name.all = Name then
               return (P, C);
            end if;
            P := C;
            C := C.Next;
         end loop;
      end if;
      return Null_Reference;
   end Find_Contextual_Menu_By_Name;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Action      : String;
      Name        : String := "";
      Label       : String := "";
      Custom      : Custom_Expansion := null;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Filter      : Action_Filter := null;
      Group       : Integer := Default_Contextual_Group)
   is
      N : constant String := (if Name = "" then Action else Name);
      T      : Contextual_Label_Param;
   begin
      if Label /= "" then
         T        := new Contextual_Label_Parameters;
         T.Label  := new String'(Label);
         T.Custom := Custom;

         if Filter /= null then
            T.Filter := Create_Filter (Kernel, Label) and Filter;
         else
            T.Filter := Action_Filter (Create_Filter (Kernel, Label));
         end if;
      elsif Filter /= null then
         T        := new Contextual_Label_Parameters;
         T.Label  := new String'(N);
         T.Filter := Filter;
      end if;

      Add_Contextual_Menu
         (Kernel,
          new Contextual_Menu_Record'
            (Kernel                => Kernel_Handle (Kernel),
             Menu_Type             => Type_Action,
             Name                  => new String'(N),
             Action                => new String'(Action),
             Next                  => null,
             Group                 => Group,
             Visible               => True,
             Sensitive             => True,
             Filter_Matched        => False,
             Label_For_Context     => Null_Unbounded_String,
             Label                 => Contextual_Menu_Label_Creator (T)),
         Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Action      : String;
      Name        : String := "";
      Label       : access Contextual_Menu_Label_Creator_Record'Class;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group)
   is
      N : constant String := (if Name = "" then Action else Name);
   begin
      Add_Contextual_Menu
         (Kernel,
          new Contextual_Menu_Record'
             (Kernel                => Kernel_Handle (Kernel),
              Menu_Type             => Type_Action,
              Name                  => new String'(N),
              Action                => new String'(Action),
              Next                  => null,
              Group                 => Group,
              Visible               => True,
              Sensitive             => True,
              Filter_Matched        => False,
              Label_For_Context     => Null_Unbounded_String,
              Label                 => Contextual_Menu_Label_Creator (Label)),
          Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   -----------------------------------
   -- Register_Contextual_Separator --
   -----------------------------------

   procedure Register_Contextual_Separator
     (Kernel      : access Kernel_Handle_Record'Class;
      Action      : String := "";   --  filter
      In_Submenu  : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group)
   is
      T      : Contextual_Label_Param;
   begin
      if In_Submenu /= "" then
         T        := new Contextual_Label_Parameters;
         T.Label  := new String'(In_Submenu & "/-");
      end if;

      Add_Contextual_Menu
         (Kernel,
          new Contextual_Menu_Record'
             (Kernel                => Kernel_Handle (Kernel),
              Menu_Type             => Type_Separator,
              Name                  => new String'(""),
              Separator_Action      =>
                 (if Action = "" then null else new String'(Action)),
              Next                  => null,
              Group                 => Group,
              Visible               => True,
              Sensitive             => True,
              Filter_Matched        => False,
              Label_For_Context     => Null_Unbounded_String,
              Label                 => Contextual_Menu_Label_Creator (T)),
          Ref_Item, Add_Before);
   end Register_Contextual_Separator;

   ---------------------------------
   -- Set_Contextual_Menu_Visible --
   ---------------------------------

   procedure Set_Contextual_Menu_Visible
     (Kernel  : access Kernel_Handle_Record'Class;
      Name    : String;
      Visible : Boolean)
   is
      Menu_Ref : Contextual_Menu_Reference :=
                   Find_Contextual_Menu_By_Name (Kernel, Name);
   begin
      if Menu_Ref /= Null_Reference then
         Register_Contextual_Menu
           (Kernel => Kernel,
            Name   => Name,
            Action => "");
         Menu_Ref := Find_Contextual_Menu_By_Name (Kernel, Name);
      end if;
      if Menu_Ref /= Null_Reference then
         Menu_Ref.Menu.Visible := Visible;
      end if;
   end Set_Contextual_Menu_Visible;

   -------------------------------------
   -- Set_Contextual_Menu_Sensitivity --
   -------------------------------------

   procedure Set_Contextual_Menu_Sensitivity
     (Kernel    : access Kernel_Handle_Record'Class;
      Name      : String;
      Sensitive : Boolean)
   is
      Menu_Ref : Contextual_Menu_Reference :=
                   Find_Contextual_Menu_By_Name (Kernel, Name);
   begin
      if Menu_Ref = Null_Reference then
         Register_Contextual_Menu
           (Kernel => Kernel,
            Name   => Name,
            Action => "");
         Menu_Ref := Find_Contextual_Menu_By_Name (Kernel, Name);
      end if;
      Menu_Ref.Menu.Sensitive := Sensitive;
   end Set_Contextual_Menu_Sensitivity;

   -------------------------------------
   -- Get_Registered_Contextual_Menus --
   -------------------------------------

   function Get_Registered_Contextual_Menus
     (Kernel : access Kernel_Handle_Record'Class)
      return GNAT.Strings.String_List_Access
   is
      Count  : Natural := 0;
      C      : Contextual_Menu_Access;
      Result : GNAT.Strings.String_List_Access;
   begin
      if Kernel.Contextual /= System.Null_Address then
         C := Convert (Kernel.Contextual);
         while C /= null loop
            Count := Count + 1;
            C := C.Next;
         end loop;

         Result := new GNAT.Strings.String_List (1 .. Count);
         Count := Result'First;
         C := Convert (Kernel.Contextual);
         while C /= null loop
            Result (Count) := new String'(C.Name.all);
            Count := Count + 1;
            C := C.Next;
         end loop;
      end if;

      return Result;
   end Get_Registered_Contextual_Menus;

   ---------------------------------
   -- Register_Contextual_Submenu --
   ---------------------------------

   procedure Register_Contextual_Submenu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Label             : String := "";
      Filter            : access Action_Filter_Record'Class := null;
      Enable_Filter     : access Action_Filter_Record'Class := null;
      Submenu           : Submenu_Factory := null;
      Ref_Item          : String := "";
      Add_Before        : Boolean := True;
      Group             : Integer := Default_Contextual_Group)
   is
      T : Contextual_Label_Param;
   begin
      if Label /= "" then
         T        := new Contextual_Label_Parameters;
         T.Label  := new String'(Label);
         T.Custom := null;
         T.Filter := Action_Filter (Create_Filter (Kernel, Label));
      end if;

      Add_Contextual_Menu
        (Kernel,
         new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Submenu,
            Name                  => new String'(Name),
            Submenu_Filter        => Filter,
            Submenu_Enable        => Enable_Filter,
            Next                  => null,
            Visible               => True,
            Filter_Matched        => False,
            Sensitive             => True,
            Group                 => Group,
            Submenu               => Submenu,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (T)),
         Ref_Item,
         Add_Before);
   end Register_Contextual_Submenu;

   ------------------------------
   -- On_Action_Button_Clicked --
   ------------------------------

   procedure On_Action_Button_Clicked
     (Button : access Gtk_Tool_Button_Record'Class)
   is
      B : constant Action_Tool_Button := Action_Tool_Button (Button);
   begin
      if B.Focus_On_Action then
         declare
            P : constant MDI_Child := Find_MDI_Child_From_Widget (B);
         begin
            if P /= null then
               Set_Focus_Child (P);
            end if;
         end;
      end if;

      Execute_Action (B, B.Data);
   end On_Action_Button_Clicked;

   -------------------
   -- Insert_Button --
   -------------------

   procedure Insert_Button
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Toolbar         : not null access Gtk_Toolbar_Record'Class;
      Descr           : Toolbar_Button;
      Section         : String;
      Toolbar_Id      : String)
   is
      Button : Action_Tool_Button;
      Combo  : Action_Combo_Tool;
      Item   : Gtk_Tool_Item;

      procedure On_Child (Widget : not null access Gtk_Widget_Record'Class);
      procedure On_Child (Widget : not null access Gtk_Widget_Record'Class) is
      begin
         if Widget.all in Action_Combo_Tool_Record'Class
           and then Widget.Get_Name = Descr.Group
         then
            Combo := Action_Combo_Tool (Widget);
         end if;
      end On_Child;

   begin
      if Descr.Group /= "" then
         --  Do we already have a button for this group ?

         Toolbar.Foreach (On_Child'Unrestricted_Access);
         if Combo = null then
            Gtk_New (Combo, Kernel => Kernel,
                     Initial_Label => To_String (Descr.Label),
                     Initial_Action => To_String (Descr.Action));
            Combo.Set_Name (To_String (Descr.Group));
            Combo.Set_Label (To_String (Descr.Group));
            Item := Gtk_Tool_Item (Combo);
         else
            Combo.Add_Action
              (Label  => To_String (Descr.Label),
               Action => To_String (Descr.Action));
         end if;

      else
         Button := new Action_Tool_Button_Record;
         Button.Focus_On_Action := Toolbar_Id /= "main";
         Button.Data := (Kernel    => Kernel,
                         Optional  => False,
                         Hide      => Descr.Hide,
                         Action    => new String'(To_String (Descr.Action)),
                         Looked_Up => null);

         if Descr.Label /= "" then
            Gtk.Tool_Button.Initialize
              (Button, Label => To_String (Descr.Label));
         else
            Gtk.Tool_Button.Initialize
              (Button, Label => To_String (Descr.Action));
         end if;

         if Descr.Icon_Name /= "" then
            Button.Forced_Stock := True;
            Button.Set_Icon_Name (To_String (Descr.Icon_Name));
         end if;

         --  The side effect is to set image, tooltip,... if the action already
         --  exists.
         --  If the action is unknown, or it has a filter, we will need to
         --  monitor this button when the context changes.

         Add_To_Global_Proxies (Button, Kernel, null);

         Button.On_Clicked (On_Action_Button_Clicked'Access);

         Item := Gtk_Tool_Item (Button);
      end if;

      if Item /= null then
         if Section = "" then
            Toolbar.Insert (Item);
         else
            Toolbar.Insert (Item, Get_Toolbar_Section (Toolbar, Section));
         end if;

         --  Do this to obtain the side effect of Lookup_Action that sets
         --  the icon for this item, if applicable.
         declare
            Dummy : access Action_Record := Lookup_Action (Item);
         begin
            null;
         end;

         Item.Show_All;
      end if;
   end Insert_Button;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Update_Menus_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Update_Menus_Data, Update_Menus_Data_Access);
   begin
      Unchecked_Free (Data);
      Globals.Update_Menus_Idle_Id := No_Source_Id;
   end Destroy;

   ----------------
   -- Set_Active --
   ----------------

   overriding procedure Set_Active
     (Self   : in out Widget_Action_Proxy;
      Active : Boolean;
      Object : not null access GObject_Record'Class)
   is
      W : constant Gtk_Widget := Gtk_Widget (Object);
   begin
      if Active then
         W.Show;  --  in case it was hidden earlier
         W.Set_Sensitive (True);
         W.Set_No_Show_All (False);

      else
         W.Set_Sensitive (False);
         if Self.Optional or else Self.Hide then
            W.Hide;
            W.Set_No_Show_All (True);  --  later Show_All should not impact
         end if;
      end if;
   end Set_Active;

   ------------------------------------
   -- Update_Menus_And_Buttons_Chunk --
   ------------------------------------

   function Update_Menus_And_Buttons_Chunk
     (Data : Update_Menus_Data_Access) return Boolean
   is
      procedure Cleanup_Window
        (Win : not null access GPS_Application_Window_Record'Class);
      --  Cleanup menubar and toolbar for a specific window

      --------------------
      -- Cleanup_Window --
      --------------------

      procedure Cleanup_Window
        (Win : not null access GPS_Application_Window_Record'Class)
      is
         procedure Cleanup_Toolbar_Separators
           (Toolbar : not null access Gtk_Toolbar_Record'Class);
         --  Cleanup separators in a toolbar

         --------------------------------
         -- Cleanup_Toolbar_Separators --
         --------------------------------

         procedure Cleanup_Toolbar_Separators
           (Toolbar : not null access Gtk_Toolbar_Record'Class)
         is
            Count       : constant Gint := Toolbar.Get_N_Items;
            Prev_Is_Sep : Boolean := True;
            Item        : Gtk_Tool_Item;
            Last_Visible_Sep : Gint := -1;
         begin
            for C in 0 .. Count - 1 loop
               Item := Toolbar.Get_Nth_Item (C);
               Item.Set_No_Show_All (True);

               if Item.all in Gtk_Separator_Tool_Item_Record'Class then
                  if Prev_Is_Sep then
                     Item.Hide;
                  else
                     Item.Show;
                     Last_Visible_Sep := C;
                  end if;
                  Prev_Is_Sep := True;

               else
                  Prev_Is_Sep := Prev_Is_Sep and then not Item.Get_Visible;
               end if;
            end loop;

            if Prev_Is_Sep and then Last_Visible_Sep /= -1 then
               Toolbar.Get_Nth_Item (Last_Visible_Sep).Hide;
            end if;
         end Cleanup_Toolbar_Separators;

      begin
         if Win.Menu_Bar /= null then
            Propagate_Visibility (Win.Menu_Bar, Win.Menu_Bar);
         end if;

         if Win.Toolbar /= null then
            Cleanup_Toolbar_Separators (Win.Toolbar);
         end if;
      end Cleanup_Window;

      Max_Idle_Duration : constant Duration := 0.05;
      A : Proxy_And_Filter;
      Action : access Action_Record;
      Start  : constant Time := Clock;
      Available : Boolean;

      D : access Action_Proxy'Class;
      The_Next : Proxy_Lists.Cursor;

   begin
      loop
         if not Has_Element (Data.Current) then
            --  no more items

            For_All_Open_Windows
               (Get_Kernel (Data.Context).Get_Application,
                Cleanup_Window'Access);
            Globals.Update_Menus_Idle_Id := No_Source_Id;
            return False;
         end if;

         if Clock - Start > Max_Idle_Duration then
            --  will try again
            return True;
         end if;

         The_Next := Next (Data.Current);

         A := Proxy_Lists.Element (Data.Current);
         D := Get_Data (A.Proxy);

         if A.Filter /= null then
            D.Set_Active (Filter_Matches (A.Filter, Data.Context), A.Proxy);
         else
            Action := Lookup_Action (A.Proxy);

            if Action = null then
               D.Set_Active (False, A.Proxy);

            else
               --  The context caches the filter, so there is limited
               --  cost in computing multiple times whether a given
               --  filter matches.
               --  Always compute with Filter_Matches, since the action might
               --  be explicitly disabled by the user.

               Available := Filter_Matches (Action, Data.Context);
               D.Set_Active (Available, A.Proxy);

               if not Has_Filter (Action) then
                  --  The item is already active, and will remain so, so
                  --  nothing to do here. We thus remove the item from the list
                  --  since there will be nothing to do with it anymore,
                  --  not worth wasting time
                  --  ??? If the action is overridden, we might need to review
                  --  the policy here, but that should not happen.

                  Add_To_Unfiltered_Items
                     (Proxy_Lists.Element (Data.Current));
                  Globals.Proxy_Items.Delete (Data.Current);
               end if;
            end if;
         end if;

         Data.Current := The_Next;
      end loop;
   end Update_Menus_And_Buttons_Chunk;

   ------------------------------
   -- Update_Menus_And_Buttons --
   ------------------------------

   procedure Update_Menus_And_Buttons
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Context : GPS.Kernel.Selection_Context := No_Context)
   is
      Ctxt : Selection_Context := Context;
      Data : Update_Menus_Data_Access;

      Was_Computing_Idle : Boolean := False;
      --  Whether the idle loop was computing

      Recomputation_Timed_Out : Boolean := False;
   begin
      if Kernel.Is_In_Destruction then
         return;
      end if;

      if Ctxt = No_Context then
         Ctxt := Get_Current_Context (Kernel);
      end if;

      if Ctxt = No_Context then
         return;
      end if;

      Data := new Update_Menus_Data'
        (Context => Ctxt,
         Current => Globals.Proxy_Items.First);

      if System_Menus.Get_Pref then

         --  The menus are handled outside of GPS: we are dealing with
         --  the asynchronous greying out of menus here.

         if Globals.Update_Menus_Idle_Id /= No_Source_Id then
            Was_Computing_Idle := True;
            Remove (Globals.Update_Menus_Idle_Id);
         end if;

         --  Do a first immediate pass, since it might look nicer. This is also
         --  needed on startup to avoid flickering the toolbars.
         --  Never do this first pass if we were already doing a recompute pass
         --  in the idle loop, to avoid performance locking in the following
         --  scenario:
         --      1- Update_Menus_And_Buttons_Chunk is called here
         --      2- the chunk takes too slow, so an idle is registered
         --      3- anywhere else in this package, a call to this function
         --         is made with the purpose to clean up the dangling pointers
         --         in the globals because the idle is registered
         --      4- back to step 1

         if Was_Computing_Idle
           or else Update_Menus_And_Buttons_Chunk (Data)
         then
            Globals.Update_Menus_Idle_Id := Update_Menus_Idle.Idle_Add
              (Update_Menus_And_Buttons_Chunk'Access,
               Data       => Data,
               Notify     => Destroy'Access);
         else
            Destroy (Data);
         end if;
      else
         --  The menus are handled by GPS: the state of the menus is going to
         --  be computed when the menus is mapped. So we can compute all the
         --  toolbar items directly, we know/hope it won't take a lot of time.

         while Update_Menus_And_Buttons_Chunk (Data) loop
            --  If we reached this, this means that the recomputation is taking
            --  longer than one allocated time slot: we'll want to trace this.
            Recomputation_Timed_Out := True;
         end loop;

         if Recomputation_Timed_Out then
            Trace (Me, "Refreshing toolbar items took longer than expected.");
         end if;

         Destroy (Data);
      end if;
   end Update_Menus_And_Buttons;

   ---------------------------
   -- Action_Status_Changed --
   ---------------------------

   procedure Action_Status_Changed
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String)
   is
      Data : access Action_Proxy'Class;
      C, N : Proxy_Lists.Cursor;
      P    : Proxy_And_Filter;
      Lower : constant String := To_Lower (Name);
   begin
      --  Put all items on the list to be checked. When they still do not
      --  have a filter, they will simply be put back on the list of unfiltered
      --  items.
      C := Globals.Unfiltered_Items.Last;
      while Has_Element (C) loop
         N := Previous (C);
         P := Proxy_Lists.Element (C);

         Data := Get_Data (P.Proxy);
         if Data /= null and then To_Lower (Data.Action.all) = Lower then
            --  This might end up putting the item back on Unfiltered_items,
            --  which is why we traverse the list in the reverse order here.
            Add_To_Global_Proxies (P.Proxy, Kernel, P.Filter);
            Globals.Unfiltered_Items.Delete (C);
         end if;

         C := N;
      end loop;

      Update_Menus_And_Buttons (Kernel);
   end Action_Status_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_Context_Changed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Self);
   begin
      Update_Menus_And_Buttons (Kernel, Context);
   end Execute;

   ----------------------------
   -- Start_Monitoring_Menus --
   ----------------------------

   procedure Start_Monitoring_Menus
     (Kernel      : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Context_Changed_Hook.Add_Debounce (new On_Context_Changed);
   end Start_Monitoring_Menus;

   -----------------
   -- Append_Menu --
   -----------------

   procedure Append_Menu
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Menu      : not null access Gtk_Menu_Record'Class;
      Label     : String;
      Action    : String)
   is
      Item      : Gtk_Menu_Item;
   begin
      Item := Gtk_New_Action_Item
        (Kernel      => Kernel,
         Menu_Label  => Label,
         Full_Path   => "/invisible/" & Label,
         Action      => Action,
         Optional    => False);
      Menu.Append (Item);
   end Append_Menu;

   -------------------------------
   -- Create_Menubar_From_Model --
   -------------------------------

   function Create_Menubar_From_Model
     (Kernel  : not null access Kernel_Handle_Record'Class)
     return Gtk_Menu_Bar
   is
      Block_Me : constant Block_Trace_Handle := Create (Me) with Unreferenced;

      procedure Process_Menu
        (Parent      : not null access Gtk_Menu_Shell_Record'Class;
         M           : Gmenu;
         Idx         : Gint;
         Parent_Path : Unbounded_String;
         Level       : Natural);
      --  Create a menu (and its submenu) from a menu model

      procedure Process_Menu
        (Parent      : not null access Gtk_Menu_Shell_Record'Class;
         M           : Gmenu;
         Idx         : Gint;
         Parent_Path : Unbounded_String;
         Level       : Natural)
      is
         Attr_Iter : Gmenu_Attribute_Iter;
         Links     : Gmenu_Link_Iter;
         Label     : Unbounded_String;
         Label_With_Mnemonic : Unbounded_String;
         Full_Path : Unbounded_String;
         Action    : Unbounded_String;
         Optional  : Boolean := False;
         Item      : Gtk_Menu_Item;
         Sep       : Gtk_Separator_Menu_Item;
         Menu      : Gtk_Menu;
         Attributes_Count : Natural := 0;
         Act       : Gaction;
      begin
         Attr_Iter := Iterate_Item_Attributes (M, Idx);
         while Next (Attr_Iter) loop
            Attributes_Count := Attributes_Count + 1;
            declare
               N   : constant String := Get_Name (Attr_Iter);
               Val : Gvariant;
            begin
               Val := Get_Value (Attr_Iter);
               if N = "label" then
                  Label_With_Mnemonic := To_Unbounded_String
                    (Get_String (Val, null));
                  Label := To_Unbounded_String
                    (Strip_Single_Underscores (Get_String (Val, null)));
               elsif N = "action" then
                  Action := To_Unbounded_String (Get_String (Val, null));
               elsif N = "hidden-when" then
                  Optional := Get_String (Val, null) = "action-disabled";
               elsif N = GPS_Id_Attribute then
                  Attributes_Count := Attributes_Count - 1;
               else
                  Trace (Me, "Unknown attribute " & N);
               end if;
               Unref (Val);
            end;
         end loop;
         Unref (Attr_Iter);

         if Attributes_Count > 0 then
            Full_Path := Parent_Path & '/' & Label;
            if Action = "" then
               Gtk_New_With_Mnemonic (Item, To_String (Label_With_Mnemonic));
            else
               --  Remove the "app." prefix for the action name. We then
               --  need to map from the gtk action name to the GPS name
               Act := Kernel.Get_Application.Lookup_Action
                  (Slice (Action, 5, Length (Action)));
               if Act = Gaction (Null_Interface) then
                  Item := null;
               elsif To_Object (Act).all in GPS_Action_Record'Class then
                  Item := Gtk_New_Action_Item
                    (Kernel      => Kernel,
                     Full_Path   => To_String (Full_Path),
                     Menu_Label  => To_String (Label_With_Mnemonic),
                     Action    => GPS_Action (To_Object (Act)).Data.Action.all,
                     Optional  => Optional);

               elsif Get_State_Type (Act) = null then
                  Gtk_New_With_Mnemonic
                    (Item, To_String (Label_With_Mnemonic));
                  Item.Set_Action_Name (To_String (Action));
               end if;
            end if;

            if Item /= null then
               Parent.Append (Item);
            end if;
         else
            Full_Path := Parent_Path;
         end if;

         Links := Iterate_Item_Links (M, Idx);
         while Next (Links) loop
            declare
               N   : constant String := Get_Name (Links);
               M2  : Gmenu_Model;
               P   : Gtk_Menu_Shell;
               Val : Gvariant;
            begin
               if N = "submenu" and then Item /= null then
                  Gtk_New (Menu);
                  Menu.Set_Accel_Group (Get_Default_Accelerators (Kernel));
                  Connect_Submenu (Item, Menu);
                  P := Gtk_Menu_Shell (Menu);
               elsif N = "section" then
                  if Idx /= 0 then

                     Gtk_New (Sep);

                     Val := M.Get_Item_Attribute_Value
                       (Item_Index    => Idx,
                        Attribute     => GPS_Id_Attribute,
                        Expected_Type => Gvariant_Type_String);
                     if Val /= Null_Gvariant then
                        Sep.Set_Name (Get_String (Val, null));
                        Unref (Val);
                     end if;

                     Parent.Append (Sep);
                  end if;
                  P := Gtk_Menu_Shell (Parent);
               else
                  Trace (Me, "Unknown attribute " & N);
               end if;

               if P /= null then
                  M2 := Get_Value (Links);
                  for Idx2 in 0 .. M2.Get_N_Items - 1 loop
                     Process_Menu (P, Gmenu (M2), Idx2, Full_Path, Level + 1);
                  end loop;
               end if;

               --  Do not Unref (M2)
            end;
         end loop;

         Unref (Links);
      end Process_Menu;

      Menubar : Gtk_Menu_Bar;

   begin
      Gtk_New (Menubar);

      for Idx in 0 .. Globals.Menu_Model.Get_N_Items - 1 loop
         Process_Menu
           (Menubar, Globals.Menu_Model, Idx, Null_Unbounded_String, 0);
      end loop;

      --  The pass above creates the menus from menus.xml: there can be empty
      --  menus, such as the SPARK or CodePeer menus. To remove those,
      --  recompute the visibility now.
      Recompute_State_And_Visibility (Gtk_Menu_Shell (Menubar));

      --  There are also menus that have no associated actions when this is
      --  created (such as the Navigate menu) - for these, we want to refresh
      --  the menu bar once the actions are created, otherwise the menu will
      --  appear as greyed out. Do this when mapping the menu bar.
      Menubar.On_Map (On_Menu_Map'Access);

      return Menubar;
   end Create_Menubar_From_Model;

   ---------------------
   -- Declare_Toolbar --
   ---------------------

   procedure Declare_Toolbar
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Id            : String;
      Inherits      : String := "")
   is
      pragma Unreferenced (Kernel);
      Descr : Toolbar_Description;
   begin
      if not Globals.Toolbars.Contains (Id) then
         Descr.Inherit := To_Unbounded_String (Inherits);
         Globals.Toolbars.Include (Id, Descr);
      end if;
   end Declare_Toolbar;

   -------------------------------
   -- Parse_Menu_Model_From_XML --
   -------------------------------

   procedure Parse_Menu_Model_From_XML
     (Kernel : not null access Kernel_Handle_Record'Class;
      Root   : Node)
   is
      procedure Process_Menu
         (Menu_Node : Node; Parent_Path : String; Model : Gmenu);
      --  Process a <menu> node. Parent will be null when using system menus.
      --  Parent_Path always ends up with a trailing '/'

      ------------------
      -- Process_Menu --
      ------------------

      procedure Process_Menu
         (Menu_Node : Node; Parent_Path : String; Model : Gmenu)
      is
         Label   : constant DOM_String := Get_Attribute (Menu_Node, "label");
         Clean_Label : constant String :=
            Parent_Path &
            Escape_Menu_Name                        --  protect '/' in the name
               (Strip_Single_Underscores (Label));  --  remove '_' mnemonics
         Action  : constant DOM_String := Get_Attribute (Menu_Node, "action");
         Optional_Str : constant DOM_String :=
           Get_Attribute (Menu_Node, "optional");
         Optional     : Boolean := False;
         N            : Node;
         Act          : GPS_Action;
         It           : Gmenu_Item;
         Val          : Gvariant;
         Menu, Section : Gmenu;
      begin
         if Optional_Str /= "" then   --  avoid raising exception if we can
            begin
               Optional    := Boolean'Value (Optional_Str);
            exception
               when Constraint_Error =>
                  Trace (Me, "Invalid value for 'optional': " & Optional_Str
                         & " for label=" & Label);
                  Optional := False;
            end;
         end if;

         if Action /= "" then
            Act := Create_Or_Lookup_Action (Kernel, Action);
            Add_Menu_To_Action (Action, Clean_Label);

            --  See possible attributes in gtkmenutrackeritem.c
            It := Gmenu_Item_New
               (Label           => Label,
                Detailed_Action => Act.Gtk_Name);
            if Optional then
               G_New_String (Val, "action-disabled");
            else
               G_New_String (Val, "action-missing");
            end if;
            It.Set_Attribute_Value
               (Attribute => "hidden-when",
                Value     => Val);

            Model.Append_Item (It);
            Unref (It);

         else
            Menu := Gmenu_New;
            It := Gmenu_Item_New_Submenu
               (Label   => Label,
                Submenu => Menu);
            Model.Append_Item (It);
            Unref (It);

            Section := Gmenu_New;
            It := Gmenu_Item_New_Section ("", Section);
            Menu.Append_Item (It);
            Unref (It);

            N := First_Child (Menu_Node);
            if N /= null then
               while N /= null loop
                  if Node_Name (N) = "menu" then
                     Process_Menu (N, Clean_Label & '/', Section);
                  elsif Node_Name (N) = "separator" then
                     Section := Gmenu_New;
                     It := Gmenu_Item_New_Section ("", Section);

                     declare
                        Id : constant String :=
                          Get_Attribute (N, "id");
                     begin
                        if Id /= "" then
                           G_New_String (Val, Id);
                           It.Set_Attribute_Value (GPS_Id_Attribute, Val);
                        end if;
                     end;

                     Menu.Append_Item (It);
                     Unref (It);
                  end if;
                  N := Next_Sibling (N);
               end loop;
            end if;
         end if;
      end Process_Menu;

      N, N2       : Node;

   begin
      Globals.Menu_Model := Gmenu_New;

      N := First_Child (Root);
      while N /= null loop
         if Node_Name (N) = "menubar" then
            N2 := First_Child (N);
            while N2 /= null loop
               if Node_Name (N2) = "menu" then
                  Process_Menu (N2, "/", Globals.Menu_Model);
               end if;
               N2 := Next_Sibling (N2);
            end loop;

         elsif Node_Name (N) = "toolbar" then
            declare
               Id : constant String := Get_Attribute (N, "id");
               Descr : Toolbar_Description;
               Hide  : Boolean;
            begin
               Descr.Inherit := To_Unbounded_String
                 (Get_Attribute (N, "inherit"));

               N2 := First_Child (N);
               while N2 /= null loop
                  if Node_Name (N2) = "button" then
                     declare
                        Attr : constant String := Get_Attribute (N2, "hide");
                     begin
                        if Attr = "" then
                           Hide := False;
                        else
                           Hide := Boolean'Value (Attr);
                        end if;
                     exception
                        when Constraint_Error =>
                           Hide := False;
                     end;

                     declare
                        Act : constant String := Get_Attribute (N2, "action");
                     begin
                        Descr.Buttons.Append
                          ((Is_Separator => False,
                            Action       => To_Unbounded_String (Act),
                            Group        => Null_Unbounded_String,
                            Label        => To_Unbounded_String
                              (Get_Attribute (N2, "label")),
                            Icon_Name    => To_Unbounded_String
                              (Get_Attribute (N2, "stock")),
                            Hide         => Hide));
                        Add_Button_To_Action (Act, Id);
                     end;

                  elsif Node_Name (N2) = "separator" then
                     Descr.Buttons.Append
                        ((Is_Separator => True,
                          Start_Of_Section => To_Unbounded_String
                             (Get_Attribute (N2, "id"))));
                  end if;

                  N2 := Next_Sibling (N2);
               end loop;

               Globals.Toolbars.Include (Id, Descr);
            end;
         end if;
         N := Next_Sibling (N);
      end loop;
   end Parse_Menu_Model_From_XML;

   -------------------
   -- Install_Menus --
   -------------------

   procedure Install_Menus
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Menubar   : out Gtk.Menu_Bar.Gtk_Menu_Bar)
   is
      Item : Menu_Item_Info;
      It   : Gtk_Menu_Item;
      Menu : Gtk_Menu;
   begin
      if Globals.Menu_Model = null then
         declare
            Description : constant GNATCOLL.VFS.Virtual_File :=
              Kernel.Get_Share_Dir / "menus.xml";
            Input       : File_Input;
            Reader : Tree_Reader;
         begin
            Trace (Me, "Load menus from " & Description.Display_Full_Name);
            Open (Description.Display_Full_Name, Input);
            Parse (Reader, Input);
            Close (Input);
            Parse_Menu_Model_From_XML
              (Kernel, Get_Element (Get_Tree (Reader)));
            Free (Reader);
         end;

         if System_Menus.Get_Pref then
            Item := Find_Or_Create_Menu
               (Globals.Menu_Model, "/Window", Allow_Create => False);
            if Item /= No_Menu_Item then
               Kernel_Desktop.Set_Menu_Model
                  (Get_MDI (Kernel),
                   Kernel.Get_Application,
                   Gmenu (Item.Item.Get_Link ("submenu")),
                   User => Kernel);
               Unref (Item);
            end if;
         end if;
      end if;

      if System_Menus.Get_Pref then
         Kernel.Get_Application.Set_Menubar (Globals.Menu_Model);
         Menubar := null;
      else
         Menubar := Create_Menubar_From_Model (Kernel);

         if Menubar /= null then
            --  This menu is handled by the MDI
            It := Find_Menu_Item (Menubar, -"/Window");
            if It /= null then
               Menu := Kernel_Desktop.Create_Menu
                 (Get_MDI (Kernel), User => Kernel);
               Connect_Submenu (It, Menu);
            end if;
         end if;
      end if;
   end Install_Menus;

   ----------------------------
   -- On_GPS_Action_Activate --
   ----------------------------

   procedure On_GPS_Action_Activate (Self : Gaction; P : System.Address) is
      pragma Unreferenced (P);
      S : constant GPS_Action := GPS_Action (To_Object (Self));
   begin
      Execute_Action (S, S.Data);
   end On_GPS_Action_Activate;

   ----------------------------
   -- On_GPS_Action_Get_Name --
   ----------------------------

   function On_GPS_Action_Get_Name (Self : Gaction) return chars_ptr is
      S : constant GPS_Action := GPS_Action (To_Object (Self));
   begin
      return S.CName;
   end On_GPS_Action_Get_Name;

   -------------------------------
   -- On_GPS_Action_Get_Enabled --
   -------------------------------

   function On_GPS_Action_Get_Enabled (Self : Gaction) return Gboolean is
      S : constant GPS_Action := GPS_Action (To_Object (Self));
   begin
      return (if S.Data.Active then 1 else 0);
   end On_GPS_Action_Get_Enabled;

   ------------------------
   -- Init_GAction_Iface --
   ------------------------

   procedure Init_GAction_Iface
     (Iface : Action_Interface_Descr;
      Data  : System.Address)
   is
      pragma Unreferenced (Data);
   begin
      Set_Activate (Iface, On_GPS_Action_Activate'Access);
      Set_Change_State (Iface, On_GPS_Action_Change_State'Access);
      Set_Get_Enabled (Iface, On_GPS_Action_Get_Enabled'Access);
      Set_Get_Name (Iface, On_GPS_Action_Get_Name'Access);
      Set_Get_Parameter_Type (Iface, On_GPS_Action_Get_Parameter_Type'Access);
      Set_Get_State (Iface, On_GPS_Action_Get_State'Access);
      Set_Get_State_Hint (Iface, On_GPS_Action_Get_State_Hint'Access);
      Set_Get_State_Type (Iface, On_GPS_Action_Get_State_Type'Access);
   end Init_GAction_Iface;

   -----------------------------
   -- GPS_Action_Set_Property --
   -----------------------------

   procedure GPS_Action_Set_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Object, Value, Property_Spec);
   begin
      Trace (Me, "Unimplemented: Set_Property " & Prop_Id'Img);
   end GPS_Action_Set_Property;

   -----------------------------
   -- GPS_Action_Get_Property --
   -----------------------------

   procedure GPS_Action_Get_Property
     (Object        : access Glib.Object.GObject_Record'Class;
      Prop_Id       : Property_Id;
      Value         : out Glib.Values.GValue;
      Property_Spec : Param_Spec)
   is
      pragma Unreferenced (Object, Value, Property_Spec);
   begin
      Trace (Me, "Unimplemented: Get_Property " & Prop_Id'Img);
   end GPS_Action_Get_Property;

   ---------------------------
   -- GPS_Action_Class_Init --
   ---------------------------

   procedure GPS_Action_Class_Init (Self : GObject_Class) is
   begin
      Set_Properties_Handlers
        (Self,
         Set_Property => GPS_Action_Set_Property'Access,
         Get_Property => GPS_Action_Get_Property'Access);

      Override_Property (Self, Property_Name, "name");
      Override_Property (Self, Property_Enabled, "enabled");
      Override_Property (Self, Property_Parameter_Type, "parameter-type");
      Override_Property (Self, Property_State, "state");
      Override_Property (Self, Property_State_Type, "state-type");
   end GPS_Action_Class_Init;

   -------------------------
   -- GPS_Action_Get_Type --
   -------------------------

   function GPS_Action_Get_Type return Glib.GType is
      Info : access GInterface_Info;
   begin
      if Glib.Object.Initialize_Class_Record
        (Ancestor     => GType_Object,
         Class_Record => GPS_Action_CR'Access,
         Type_Name    => "GPSAction",
         Class_Init   => GPS_Action_Class_Init'Access)
      then
         Info := new GInterface_Info'
           (Interface_Init     => Init_GAction_Iface'Access,
            Interface_Finalize => null,
            Interface_Data     => System.Null_Address);
         Glib.Object.Add_Interface
           (GPS_Action_CR,
            Iface  => Glib.Action.Get_Type,
            Info   => Info);
      end if;
      return GPS_Action_CR.The_Type;
   end GPS_Action_Get_Type;

   ----------------
   -- Set_Active --
   ----------------

   overriding procedure Set_Active
     (Self   : in out GPS_Action_Proxy;
      Active : Boolean;
      Object : not null access GObject_Record'Class)
   is
   begin
      if Self.Active /= Active then
         Self.Active := Active;
         Notify (Object, "enabled");
      end if;
   end Set_Active;

   -----------------------------
   -- Create_Or_Lookup_Action --
   -----------------------------

   function Create_Or_Lookup_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String) return GPS_Action
   is
      Act : GPS_Action;
      A   : Gaction;
      Clean : String := Action;
   begin
      --  Generate a valid name for gtk+

      for N in Clean'Range loop
         if not Is_Alphanumeric (Clean (N))
            and then Clean (N) /= '-'
            and then Clean (N) /= '.'
         then
            Clean (N) := '-';
         end if;
      end loop;

      --  One action per GPS action, so that they each have their own handling
      --  of enabled. We unfortunately can't use a generic action with a target
      --  name that would be the GPS action name.

      A := Kernel.Get_Application.Lookup_Action (Clean);
      if A = Gaction (Null_Interface) then
         Act := new GPS_Action_Record;
         G_New (Act, GPS_Action_Get_Type);

         Act.CName := New_String (Clean); --  ??? never freed
         Act.Data := (Action    => new String'(Action),
                      Kernel    => Kernel,
                      Optional  => False,
                      Hide      => False,
                      Active    => True,
                      Looked_Up => null);
         Add_To_Global_Proxies (Act, Kernel, Filter => null);
         Kernel.Get_Application.Add_Action (+Act);
      else
         Act := GPS_Action (To_Object (A));
      end if;

      return Act;
   end Create_Or_Lookup_Action;

   --------------------------
   -- Propagate_Visibility --
   --------------------------

   procedure Propagate_Visibility
     (Widget : not null access Gtk_Widget_Record'Class;
      Parent : not null access Gtk_Container_Record'Class)
   is
      use Widget_List;
      Children : Widget_List.Glist := Parent.Get_Children;
      Iter     : Widget_List.Glist := Children;
      S, V : Boolean := False;
      W    : Gtk_Widget;
      Last_Visible_Sep : Gtk_Widget;
      Prev_Is_Sep : Boolean := True;
   begin
      while Iter /= Null_List loop
         W := Widget_List.Get_Data (Iter);

         --  Separators should not impact the visibility of the parent.
         --  We also do not want to display a separator as the first or last
         --  item in the menu, nor display multiple separators next to
         --  each other.
         if W.all in Gtk_Separator_Menu_Item_Record'Class then
            if Prev_Is_Sep then
               W.Hide;
            else
               W.Show;
               Last_Visible_Sep := W;
            end if;
            Prev_Is_Sep := True;

         else
            if W.all in Gtk_Menu_Item_Record'Class
              and then Gtk_Menu_Item (W).Get_Submenu /= null
            then
               Propagate_Visibility
                 (W, Gtk_Container (Gtk_Menu_Item (W).Get_Submenu));
            end if;

            Prev_Is_Sep := Prev_Is_Sep and then not W.Get_Visible;
            S := S or else W.Get_Sensitive;
            V := V or else W.Get_Visible;  --  do not check parents
         end if;

         Iter := Next (Iter);
      end loop;

      if Prev_Is_Sep and then Last_Visible_Sep /= null then
         Last_Visible_Sep.Hide;
      end if;

      Widget_List.Free (Children);

      if not V then
         Widget.Hide;
      else
         Widget.Show;

         declare
            Is_Toplevel_Menu_Item : constant Boolean :=
                                     (Widget.Get_Parent /= null
                                        and then Widget.Get_Parent.all
                                        in Gtk_Menu_Bar_Record'Class);
            Is_Menu               : constant Boolean :=
                                      Widget.all in Gtk_Menu_Record'Class;
         begin

            --  Don't disable the sensitivity of toplevel menu items: otherwise
            --  we won't be able to recalculate the state of its children since
            --  it's done when the assocaited menu is mapped (i.e: after a
            --  click).
            --
            --  Don't disable the sensitivity of Gtk_Menu widgets too: it's a
            --  non sense and can lead to a display bug.

            if not (Is_Toplevel_Menu_Item or else Is_Menu) or else S then
               Widget.Set_Sensitive (S);
            end if;
         end;
      end if;
   end Propagate_Visibility;

   ----------------------------
   -- Recompute_Object_State --
   ----------------------------

   procedure Recompute_Object_State
     (Object : not null access GObject_Record'Class)
   is
      Data   : constant access Action_Proxy'Class := Get_Data (Object);
      Action : access Action_Record;
   begin
      if Data = null then
         return;
      end if;

      Action := Lookup_Action (Object);

      if Action = null then
         Data.Set_Active (False, Object);
      else
         Data.Set_Active
           (Filter_Matches (Action, Data.Kernel.Get_Current_Context),
            Object);
      end if;
   end Recompute_Object_State;

   -----------------
   -- On_Menu_Map --
   -----------------

   procedure On_Menu_Map (Object : access Gtk_Widget_Record'Class) is
   begin
      Recompute_State_And_Visibility (Gtk_Menu_Shell (Object));
   end On_Menu_Map;

   ------------------------------------
   -- Recompute_State_And_Visibility --
   ------------------------------------

   procedure Recompute_State_And_Visibility (Shell : Gtk_Menu_Shell) is
      use Widget_List;

      procedure Recompute_State (Menu : Gtk_Menu_Shell);
      --  Auxiliary recursion

      procedure Recompute_State (Menu : Gtk_Menu_Shell) is
         Children : Widget_List.Glist := Menu.Get_Children;
         Iter     : Widget_List.Glist := Children;
         Submenu  : Gtk_Widget;
         W        : Gtk_Widget;
      begin
         while Iter /= Null_List loop
            W := Widget_List.Get_Data (Iter);
            if W.all in Gtk_Menu_Item_Record'Class then
               Recompute_Object_State (W);
               Submenu := Gtk_Menu_Item (W).Get_Submenu;
               if Submenu /= null
                 and then Submenu.all in Gtk_Menu_Shell_Record'Class
               then
                  Recompute_State (Gtk_Menu_Shell (Submenu));
               end if;
            end if;
            Iter := Next (Iter);
         end loop;
         Widget_List.Free (Children);
      end Recompute_State;

   begin
      --  Recompute the state of each menu item in this menu and submenu
      Recompute_State (Shell);

      --  Refresh visibility of the menu, clean up duplicate separators
      Propagate_Visibility (Shell, Shell);
   end Recompute_State_And_Visibility;

   ---------------------
   -- Connect_Submenu --
   ---------------------

   procedure Connect_Submenu
     (Item    : Gtk_Menu_Item;
      Submenu : Gtk_Menu) is
   begin
      Item.Set_Submenu (Submenu);
      Submenu.On_Map (On_Menu_Map'Access);
   end Connect_Submenu;

end GPS.Kernel.Modules.UI;
