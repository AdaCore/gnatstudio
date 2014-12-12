------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

with GNAT.OS_Lib;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with Gdk.Drag_Contexts;         use Gdk.Drag_Contexts;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;                 use Gdk.Types;

with Glib.Action;               use Glib.Action;
with Glib.Convert;              use Glib.Convert;
with Glib.Error;                use Glib.Error;
with Glib.Main;                 use Glib.Main;
with Glib.Menu_Model;           use Glib.Menu_Model;
with Glib.Object;               use Glib.Object;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Simple_Action;        use Glib.Simple_Action;
with Glib.Values;               use Glib.Values;
with Glib.Variant;              use Glib.Variant;

with Gtk.Builder;               use Gtk.Builder;
with Gtk.Container;             use Gtk.Container;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Tool_Item;             use Gtk.Tool_Item;

--  So that this type is correctly converted from C to Ada
with Gtk.Image_Menu_Item;       use Gtk.Image_Menu_Item;
pragma Warnings (Off, Gtk.Image_Menu_Item);

with Gtk.Accel_Label;           use Gtk.Accel_Label;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Bar;              use Gtk.Menu_Bar;
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

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Main_Window;           use GPS.Main_Window;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;
with System;                    use System;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Input_Sources.File;        use Input_Sources.File;
with DOM.Readers;               use DOM.Readers;
with DOM.Core.Nodes;            use DOM.Core, DOM.Core.Nodes;
with DOM.Core.Documents;        use DOM.Core.Documents;
with DOM.Core.Elements;         use DOM.Core.Elements;
with Sax.Utils;                 use Sax.Utils;

with UTF8_Utils;                use UTF8_Utils;

package body GPS.Kernel.Modules.UI is
   use Sax.Utils.Symbol_Table_Pointers;

   Me : constant Trace_Handle :=
     Create ("GPS.Kernel.Modules.UI", GNATCOLL.Traces.Off);

   System_Menus : constant Trace_Handle :=
     Create ("SYSTEM_MENUS", GNATCOLL.Traces.Off);

   type Contextual_Menu_User_Data is record
      Object       : GObject;
      Context_Func : Context_Factory;
      Kernel       : Kernel_Handle;
      ID           : Module_ID;
      Event_Widget : Gtk_Widget;
   end record;

   type Contextual_Menu_Type
     is (Type_Command, Type_Action, Type_Submenu, Type_Separator);
   --  The type of the contextual menu

   type Contextual_Menu_Record;
   type Contextual_Menu_Access is access all Contextual_Menu_Record;
   type Contextual_Menu_Record
     (Menu_Type : Contextual_Menu_Type := Type_Separator)
      is record
         Kernel                : Kernel_Handle;
         Name                  : GNAT.Strings.String_Access;
         Label                 : Contextual_Menu_Label_Creator;
         Pix                   : GNAT.Strings.String_Access;
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
            when Type_Command =>
               Command       : Commands.Interactive.Interactive_Command_Access;
               Filter        : access Action_Filter_Record'Class := null;
               Enable_Filter : access Action_Filter_Record'Class := null;

            when Type_Action =>
               Action           : Action_Record_Access;

            when Type_Submenu =>
               Submenu          : Submenu_Factory;
               Submenu_Filter   : access Action_Filter_Record'Class := null;
               Submenu_Enable   : access Action_Filter_Record'Class := null;

            when Type_Separator =>
               Ref_Item         : Contextual_Menu_Access;
               Separator_Filter : access Action_Filter_Record'Class := null;
         end case;
      end record;
   --  A contextual menu entry declared by a user or GPS itself internally

   type Menu_Command_Record is new Interactive_Command with record
      Kernel    : Kernel_Handle;
      Menu_Name : GNAT.Strings.String_Access;
   end record;
   type Menu_Command is access all Menu_Command_Record'Class;
   overriding function Execute
     (Command : access Menu_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type;
   overriding procedure Free (X : in out Menu_Command_Record);
   --  See doc for interactive commands

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
      Filter : Macro_Filter;
   end record;

   type Contextual_Label_Param is access Contextual_Label_Parameters'Class;
   overriding function Get_Label
     (Creator : access Contextual_Label_Parameters;
      Context : Selection_Context) return String;
   --  Substitute %p, %f,... in the title to create a suitable contextual menu
   --  title.

   package Kernel_Contextuals is new GUI_Utils.User_Contextual_Menus
     (Contextual_Menu_User_Data);

   function Get_Focus_Widget return Gtk_Widget;
   --  Return the widget that currently has the keyboard focus

   procedure Contextual_Action
     (Object : access GObject_Record'Class; Action : Contextual_Menu_Access);
   --  Execute action, in the context of a contextual menu

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the context changes

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

   function Create_Command_For_Menu
     (Kernel    : Kernel_Handle;
      Full_Path : String) return Menu_Command;
   --  Utility function: create a command for a given menu

   package Node_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Node,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   procedure Add_Menu_To_List
     (Action : not null access Action_Record;
      Path   : String);
   --  Add a menu to the list of menus associated with Action.

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
      Data : aliased GPS_Action_Proxy;
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

   ------------------------
   -- Action_Tool_Button --
   ------------------------

   type Widget_Action_Proxy is new Action_Proxy with null record;
   overriding procedure Set_Active
     (Self   : in out Widget_Action_Proxy;
      Active : Boolean;
      Object : not null access GObject_Record'Class);

   type Action_Tool_Button_Record is new Gtk_Tool_Button_Record with record
      Data : aliased Widget_Action_Proxy;
      Forced_Stock : Boolean := False;
   end record;
   type Action_Tool_Button is access all Action_Tool_Button_Record'Class;

   procedure Gtk_New
     (Button   : out Action_Tool_Button;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Stock_Id : String := "";
      Action   : String;
      Optional : Boolean := False;
      Hide     : Boolean := False);
   --  Create a new button so that it executes action when pressed.
   --  Hide and Optional are the same as for Data_Proxy

   procedure On_Action_Button_Clicked
     (Button : access Gtk_Tool_Button_Record'Class);
   --  Called when an Action_Tool_Button has been activated.

   ----------------------
   -- Action_Menu_Item --
   ----------------------

   type Action_Menu_Item_Record is new Gtk_Image_Menu_Item_Record with record
      Data : aliased Widget_Action_Proxy;
   end record;
   type Action_Menu_Item is access all Action_Menu_Item_Record'Class;

   procedure On_Activate_Action_Item
     (Item : access Gtk_Menu_Item_Record'Class);
   --  Called when an Action_Menu_Item is mapped to screen

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

   type Global_Data is record
      Toolbar_Descriptions : Node_Maps.Map;
      Symbols              : Symbol_Table := No_Symbol_Table;
      Proxy_Items          : Proxy_Lists.List;
      Update_Menus_Idle_Id : G_Source_Id := No_Source_Id;
   end record;
   Globals : Global_Data;

   -----------------------------
   -- Create_Command_For_Menu --
   -----------------------------

   function Create_Command_For_Menu
     (Kernel    : Kernel_Handle;
      Full_Path : String) return Menu_Command
   is
      Command : Menu_Command;
   begin
      Command := new Menu_Command_Record;
      Command.Kernel := Kernel;
      Command.Menu_Name := new String'(Full_Path);
      Register_Perma_Command (Kernel, Command);
      return Command;
   end Create_Command_For_Menu;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Menu_Command_Record) is
   begin
      GNAT.Strings.Free (X.Menu_Name);
   end Free;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Menu_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Menu : constant Gtk_Menu_Item := Find_Menu_Item
        (Command.Kernel, Command.Menu_Name.all);
   begin
      if Menu /= null then
         Trace (Me, "Executing " & Command.Menu_Name.all);

         if Menu.all in Action_Menu_Item_Record'Class then
            Execute_Action
              (Menu, Action_Menu_Item (Menu).Data,
               In_Foreground => Context.Synchronous);
         else
            Activate (Menu);
         end if;

         return Success;
      else
         Command.Kernel.Insert
           (-"Can't execute " & Command.Menu_Name.all,
            Mode => Error);
         return Failure;
      end if;
   end Execute;

   ----------------------
   -- Get_Focus_Widget --
   ----------------------

   function Get_Focus_Widget return Gtk_Widget is
      use Widget_List;
      List : Widget_List.Glist := List_Toplevels;
      L    : Widget_List.Glist;
      W    : Gtk_Widget;
   begin
      L := First (List);
      while L /= Null_List loop
         W := Get_Data (L);
         --  ??? The first test here is a temporary workaround, we should
         --  investigate why Get_Object (W) can be null.
         --  This is to fix a fatal Storage_Error in the following scenario:
         --   starting from the default desktop, make a child floating, and
         --   close it using the keyboard shortcut for File->Close
         if Get_Object (W) /= System.Null_Address
           and then W.all in Gtk_Window_Record'Class
           and then Is_Active (Gtk_Window (W))
         then
            Free (List);
            return Get_Focus (Gtk_Window (W));
         end if;

         L := Next (L);
      end loop;

      Free (List);
      return null;
   end Get_Focus_Widget;

   ------------------------
   -- Get_Current_Module --
   ------------------------

   function Get_Current_Module
     (Kernel : access Kernel_Handle_Record'Class) return Module_ID
   is
      F : constant MDI_Child := Get_Focus_Child (Get_MDI (Kernel));
      C : MDI_Child;
      W : Gtk_Widget;
   begin
      if Is_In_Destruction (Kernel) then
         return null;
      end if;

      W := Get_Focus_Widget;

      if W = null
        or else W.In_Destruction
      then
         --  No valid window has the focus ? It is probably because we had a
         --  dialog like the Open From Project dialog, which is being closed.
         --  The focus is moved asynchronously, but we know it will go back to
         --  the MDI at this point, and thus the current MDI child is the focus
         --  one
         C := F;

      else
         --  We have an explicit widget with the keyboard focus. Check whether
         --  it belongs to an MDI child. If not, it is probably part of some
         --  popup dialog, and therefore There is no module.
         --  As a special case, if the focus widget's parent is a notebook,
         --  we check whether the associated page is a MDI child, and behave
         --  as if that child had the focus (EC19-008)

         C := Find_MDI_Child_From_Widget (W);
      end if;

      if C = null or else C /= F then
         return null;
      else
         return Get_Module_From_Child (C);
      end if;
   end Get_Current_Module;

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
      if Filter_Matches (Action_Filter (Creator.Filter), Context) then
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
         Module := Module_ID (Element (Current));
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
      Current : Abstract_Module_List.Cursor :=
        Abstract_Module_List.First (List);
      Module  : Module_ID;
      Marker  : Location_Marker;
   begin
      if Load = null then
         Module := Get_Current_Module (Kernel);
         if Module /= null then
            return Bookmark_Handler (Module, null);
         end if;

      else
         while Has_Element (Current) loop
            Module := Module_ID (Element (Current));
            Marker := Bookmark_Handler (Module, Load);
            if Marker /= null then
               return Marker;
            end if;
            Current := Abstract_Module_List.Next (Current);
         end loop;
      end if;

      return null;
   end Create_Marker;

   -----------------------
   -- Contextual_Action --
   -----------------------

   procedure Contextual_Action
     (Object : access GObject_Record'Class;
      Action : Contextual_Menu_Access)
   is
      pragma Unreferenced (Object);
      C       : Command_Access;
      Context : Interactive_Command_Context;
   begin
      Context.Context := Action.Kernel.Last_Context_For_Contextual;

      Assert (Me, Context.Context.Data.Data /= null,
              "Contextual_Action called on freed context");
      Context.Event :=
        GPS_Window
          (Action.Kernel.Get_Main_Window).Last_Event_For_Contextual;
      --   Event will be deep-copied in the call to Create_Proxy below

      case Action.Menu_Type is
         when Type_Action =>
            C := Create_Proxy (Action.Action.Command, Context);
         when Type_Command =>
            C := Create_Proxy (Action.Command, Context);
         when others =>
            null;
      end case;

      Launch_Background_Command
        (Kernel          => Action.Kernel,
         Command         => C,
         Active          => True,
         Show_Bar        => True,
         Destroy_On_Exit => True);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception while executing " & Action.Name.all);
         Trace (Me, E);
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
         Trace (Me, "Running Hook " & To_String (Contextual_Menu_Close_Hook));
         Run_Hook (Kernel, Contextual_Menu_Close_Hook);
         Trace (Me, "Destroying contextual menu and its context");
      end if;
   end Contextual_Menu_Destroyed;

   procedure On_Contextual_Menu_Hide
     (Self  : access Gtk_Widget_Record'Class);

   -----------------------------
   -- On_Contextual_Menu_Hide --
   -----------------------------

   procedure On_Contextual_Menu_Hide
     (Self  : access Gtk_Widget_Record'Class) is
   begin
      null;
      if Self.all in GPS_Contextual_Menu_Record'Class then
         GPS_Contextual_Menu (Self).Kernel.Contextual_Menu_Open := False;
      end if;
   end On_Contextual_Menu_Hide;

   ----------------------------
   -- Create_Contextual_Menu --
   ----------------------------

   procedure Create_Contextual_Menu
     (Kernel  : Kernel_Handle;
      Object  : Glib.Object.GObject;
      Context : Selection_Context;
      Menu    : in out Gtk.Menu.Gtk_Menu)
   is
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

            when Type_Command =>
               return Filter_Matches (C.Enable_Filter, Context);

            when Type_Submenu =>
               return Filter_Matches (C.Submenu_Enable, Context);
         end case;
      end Menu_Is_Sensitive;

      ---------------------
      -- Menu_Is_Visible --
      ---------------------

      function Menu_Is_Visible
        (C       : Contextual_Menu_Access;
         Context : Selection_Context) return Boolean is
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
               if C.Separator_Filter /= null then
                  return Filter_Matches (C.Separator_Filter, Context);
               else
                  return C.Ref_Item = null
                    or else C.Ref_Item.Group /= C.Group
                    or else C.Ref_Item.Filter_Matched;
               end if;

            when Type_Action =>
               return Filter_Matches (C.Action.Filter, Context);

            when Type_Command =>
               return Filter_Matches (C.Filter, Context);

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
         Image : Gtk_Image_Menu_Item;
         Pix   : Gtk_Image;
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
                        Object  => Object,
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
                  Set_Submenu (Item, Menu);
               end if;

               Widget_List.Free (Children);

            when Type_Separator =>
               declare
                  Sep : Gtk_Separator_Menu_Item;
               begin
                  Gtk_New (Sep);
                  Item := Gtk_Menu_Item (Sep);
               end;

            when Type_Action | Type_Command =>
               if Full_Name.all /= "" then
                  if C.Pix = null then
                     Gtk_New (Item, Base_Menu_Name (Full_Name.all));
                  else
                     Gtk_New (Image, Base_Menu_Name (Full_Name.all));
                     Gtk_New (Pix,
                              Stock_Id => C.Pix.all,
                              Size     => Icon_Size_Menu);
                     Set_Image (Image, Pix);
                     Item := Gtk_Menu_Item (Image);
                  end if;

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
            --  Cache the expensive call to Get_Label in C.Label_For_Context
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

      Full_Name   : GNAT.Strings.String_Access;
      C           : Contextual_Menu_Access;
      Item        : Gtk_Menu_Item;
      Parent_Item : Gtk_Menu_Item;
      Parent_Menu : Gtk_Menu;
      List        : Gtk.Widget.Widget_List.Glist;
      Is_Sensitive : Boolean;

   begin
      Run_Hook (Kernel, Contextual_Menu_Open_Hook);

      --  Compute what items should be made visible, except for separators
      --  for the moment

      C := Convert (Kernel.Contextual);
      while C /= null loop
         if C.Menu_Type /= Type_Separator then
            C.Filter_Matched := Menu_Is_Visible (C, Context);

            --  Reset the cache set in the previous contextual menu
            C.Label_For_Context := Null_Unbounded_String;
         end if;
         C := C.Next;
      end loop;

      --  Same, but only for separators now, since their visibility might
      --  depend on the visibility of other items

      C := Convert (Kernel.Contextual);
      while C /= null loop
         if C.Menu_Type = Type_Separator then
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
               Parent_Item := Find_Or_Create_Menu_Tree
                 (Menu_Bar      => null,
                  Menu          => Menu,
                  Path          => Parent_Menu_Name ('/' & Full_Name.all),
                  Accelerators  => Get_Default_Accelerators (Kernel),
                  Allow_Create  => True,
                  Use_Mnemonics => False);

               if Parent_Item /= null then
                  Parent_Menu := Gtk_Menu (Get_Submenu (Parent_Item));
                  if Parent_Menu = null then
                     Gtk_New (Parent_Menu);
                     Set_Submenu (Parent_Item, Parent_Menu);
                  end if;

               else
                  Parent_Menu := Menu;
               end if;

               Add_Menu (Parent => Parent_Menu, Item => Item);
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
   end Create_Contextual_Menu;

   ----------------------------
   -- Create_Contextual_Menu --
   ----------------------------

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu
   is
      Context : Selection_Context := New_Context;
      Menu    : Gtk_Menu := null;
      Win    : constant GPS_Window := GPS_Window (User.Kernel.Get_Main_Window);
   begin
      --  Create the menu and add all the modules information
      Menu := new GPS_Contextual_Menu_Record;
      Gtk.Menu.Initialize (Menu);
      GPS_Contextual_Menu (Menu).Kernel := User.Kernel;

      Set_Context_Information
        (Context,
         Kernel  => User.Kernel,
         Creator => Abstract_Module_ID (User.ID));

      if User.Context_Func /= null then
         User.Context_Func
           (Context      => Context,
            Kernel       => User.Kernel,
            Event_Widget => User.Event_Widget,
            Object       => User.Object,
            Event        => Event,
            Menu         => Menu);
      end if;

      User.Kernel.Last_Context_For_Contextual := Context;
      User.Kernel.Last_Context_From_Contextual := True;

      if Win.Last_Event_For_Contextual /= null then
         Free (Win.Last_Event_For_Contextual);
      end if;

      Win.Last_Event_For_Contextual := Copy (Event);

      --  Override the previous value. No Ref is taken explicitly, so we do not
      --  need to Unref either. This field is automatically reset to null when
      --  the last holder of a ref calls Unref.

      Create_Contextual_Menu (User.Kernel, User.Object, Context, Menu);

      return Menu;
   exception
      when E : others =>
         Trace (Me, E);
         return null;
   end Create_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel          : access Kernel_Handle_Record'Class;
      Event_On_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object          : access Glib.Object.GObject_Record'Class;
      ID              : Module_ID;
      Context_Func    : Context_Factory)
   is
      User_Data : Contextual_Menu_User_Data;
   begin
      Assert (Me, ID /= null, "Null module Id to Register_Contextual_Menu");

      User_Data := Contextual_Menu_User_Data'
        (Object       => GObject (Object),
         Context_Func => Context_Func,
         ID           => ID,
         Event_Widget => Gtk_Widget (Event_On_Widget),
         Kernel       => Kernel_Handle (Kernel));

      Kernel_Contextuals.Register_Contextual_Menu
        (Event_On_Widget,
         User_Data,
         Menu_Create  => Create_Contextual_Menu'Access);
   end Register_Contextual_Menu;

   --------------------
   -- Find_Menu_Item --
   --------------------

   function Find_Menu_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Win : constant GPS_Window := GPS_Window (Kernel.Get_Main_Window);
   begin
      if Win = null then
         return null;
      else
         return Find_Or_Create_Menu_Tree
           (Menu_Bar      => Win.Menu_Bar,
            Menu          => null,
            Path          => Path,
            Accelerators  => Get_Default_Accelerators (Kernel),
            Use_Mnemonics => False,
            Allow_Create  => False);
      end if;
   end Find_Menu_Item;

   -------------------
   -- Register_Menu --
   -------------------

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Item        : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class := null;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Filter      : Action_Filter  := null;
      Menubar     : access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class := null)
   is
      Parent, Pred : Gtk_Menu_Item;
      Parent_Menu  : Gtk_Menu;
      Index        : Gint;
      Win : constant GPS_Window := GPS_Window (Kernel.Get_Main_Window);
      Bar : Gtk_Menu_Bar;

   begin
      if Win = null and then Menubar = null then
         return;
      end if;

      Bar := (if Menubar = null then Win.Menu_Bar else Gtk_Menu_Bar (Menubar));

      Parent := Find_Or_Create_Menu_Tree
        (Menu_Bar     => Bar,
         Menu         => null,
         Path         => Parent_Path,
         Accelerators => Get_Default_Accelerators (Kernel),
         Add_Before   => Add_Before,
         Ref_Item     => Ref_Item,
         Allow_Create => True);

      if Parent = null then
         Trace (Me, "Register_Menu: Parent menu not found for " & Parent_Path);
         Parent_Menu := null;
      else
         Parent_Menu := Gtk_Menu (Get_Submenu (Parent));
         if Parent_Menu = null then
            Gtk_New (Parent_Menu);
            Set_Submenu (Parent, Parent_Menu);
         end if;
      end if;

      if Item /= null then
         Find_Menu_Item_By_Name
           (Menu_Bar  => Bar,
            Menu      => Parent_Menu,
            Name      => Ref_Item,
            Menu_Item => Pred,
            Index     => Index);

         Add_Menu (Parent     => Parent_Menu,
                   Menu_Bar   => Bar,
                   Item       => Item,
                   Index      => Index,
                   Add_Before => Add_Before);
         Item.Show_All;

         if Filter /= null then
            Add_To_Global_Proxies (Item, Kernel, Filter);
         end if;

         if Item.Get_Child /= null then
            declare
               Full : constant String := Create_Menu_Path
                 ('/' & Parent_Path, Item.Get_Label,
                  Remove_Underlines => Item.Get_Use_Underline);
            begin
               Register_Action
                 (Kernel      => Kernel,
                  Name        => Full,
                  Command     => Create_Command_For_Menu
                    (Kernel_Handle (Kernel), Full),
                  Description => "Menu " & Full,
                  Filter      => Filter,
                  Category    => "Menus");
            end;
         end if;
      end if;
   end Register_Menu;

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

   ----------------------
   -- Add_Menu_To_List --
   ----------------------

   procedure Add_Menu_To_List
     (Action : not null access Action_Record;
      Path   : String)
   is
   begin
      if Action.Menus /= null then
         for M in Action.Menus'Range loop
            if Action.Menus (M).all = Path then
               return;
            end if;
         end loop;
      end if;

      Append (Action.Menus, Path);
   end Add_Menu_To_List;

   -----------------------------
   -- Update_Shortcut_Display --
   -----------------------------

   procedure Update_Shortcut_Display
     (Kernel : access Kernel_Handle_Record'Class;
      Action : String)
   is
      Act    : constant access Action_Record := Lookup_Action (Kernel, Action);
      Item   : Gtk_Menu_Item;
      Key    : Gdk_Key_Type;
      Mods   : Gdk_Modifier_Type;
      Label  : Gtk_Accel_Label;
   begin
      if Act /= null and then Act.Menus /= null then
         Kernel.Get_Shortcut_Simple
           (Action => Action,
            Key    => Key,
            Mods   => Mods);
         for M in Act.Menus'Range loop
            Item := Find_Menu_Item (Kernel, Act.Menus (M).all);
            if Item = null then
               Trace (Me, "Not updating shortcut for " & Act.Menus (M).all
                  & " since menu not found");
            elsif Item.all in Action_Menu_Item_Record'Class then
               Label := Gtk_Accel_Label (Action_Menu_Item (Item).Get_Child);
               Label.Set_Accel (Guint (Key), Mods);
            end if;
         end loop;
      end if;
   end Update_Shortcut_Display;

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
      Pix   : Gtk_Image;
      Action : access Action_Record;
      Data   : constant access Action_Proxy'Class := Get_Data (Self);

   begin
      if Data = null then
         return null;
      end if;

      Action := Lookup_Action (Data.Kernel, Data.Action.all);

      if Action /= Data.Looked_Up then
         --  ??? Commented out because Data.Looked_Up might no longer be valid
--           if Data.Looked_Up /= null then
--              if Self.all in Gtk_Menu_Item_Record'Class then
--             Remove_Menu_From_List (Data.Looked_Up, Create_Menu_Path (Self));
--              end if;
--           end if;

         Data.Looked_Up := Action;

         if Action /= null then
            if Self.all in Gtk_Menu_Item_Record'Class then
               Add_Menu_To_List
                 (Action, Create_Menu_Path (Gtk_Menu_Item (Self)));
            end if;

            if Self.all in Gtk_Widget_Record'Class then
               Gtk_Widget (Self).Set_Tooltip_Markup
                 (Escape_Text (Action.Description.all)
                  & ASCII.LF & ASCII.LF
                  & "<b>Action:</b> "
                  & Escape_Text (Action.Name.all) & ASCII.LF
                  & "<b>Category:</b> "
                  & Escape_Text
                    ((if Action.Category = null then ""
                     else Action.Category.all))
                  & ASCII.LF
                  & "<b>Shortcut:</b> "
                  & Data.Kernel.Get_Shortcut
                    (Action          => Action.Name.all,
                     Use_Markup      => True,
                     Return_Multiple => True));
               Get_Style_Context (Gtk_Widget (Self)).Remove_Class
                 ("nogpsaction");
            end if;

            --  Update the image if the action has one

            if Action.Stock_Id /= null then
               if Self.all in Action_Menu_Item_Record'Class then
                  Gtk_New (Pix, Action.Stock_Id.all, Icon_Size_Menu);
                  Action_Menu_Item (Self).Set_Image (Pix);
                  Pix.Show;
               elsif Self.all in Action_Tool_Button_Record'Class
                 and then not Action_Tool_Button (Self).Forced_Stock
               then
                  Action_Tool_Button (Self).Set_Stock_Id (Action.Stock_Id.all);
               end if;
            end if;

            --  Lookup the keybinding. This is only done the first time we do
            --  the lookup to save time, but this means that after the user has
            --  edited the keyshortcuts, this will no longer be up-to-date.
            --  ??? We could use a timestamp somewhere to note we need a
            --  refresh.

            if Self.all in Action_Menu_Item_Record'Class then
               Data.Kernel.Get_Shortcut_Simple
                 (Action => Action.Name.all,
                  Key    => Key,
                  Mods   => Mods);
               if Key /= 0 then
                  Label := Gtk_Accel_Label (Action_Menu_Item (Self).Get_Child);
                  Label.Set_Accel (Guint (Key), Mods);
               end if;
            end if;

         elsif Self.all in Gtk_Widget_Record'Class then
            Gtk_Widget (Self).Set_Tooltip_Markup
              ("Action not found: " & Escape_Text (Data.Action.all));
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
      Context : constant Selection_Context :=
        Get_Current_Context (Data.Kernel);
      Action : constant access Action_Record := Lookup_Action (Self);
      Proxy  : Command_Access;
   begin
      Trace (Me, "Execute action " & Data.Action.all
             & " in foreground ? " & In_Foreground'Img);

      if Action = null then
         Data.Kernel.Insert
           ("Action not found: " & Data.Action.all, Mode => Error);

      elsif Filter_Matches (Action.Filter, Context) then
         --  Tests expect that using GPS.execute_action("/menu") will
         --  execute in the foreground, so we run Launch_Foreground_Command.
         --  However, when the user is using the GUI, it might make more
         --  sense to be in the background, not sure.

         Proxy := Create_Proxy
           (Action.Command,
            (Event            => null,
             Context          => Context,
             Synchronous      => True,
             Dir              => No_File,
             Via_Menu         => Self.all in Action_Menu_Item_Record'Class,
             Args             => null,
             Label            => new String'(Data.Action.all),
             Repeat_Count     => 1,
             Remaining_Repeat => 0));

         if In_Foreground then
            Launch_Foreground_Command
              (Data.Kernel, Proxy, Destroy_On_Exit => True);
         else
            --  Do not block GPS exit (only if the command itself spawns
            --  another command, like an external process, and that other
            --  command could block).

            Launch_Background_Command
              (Data.Kernel,
               Proxy,
               Active          => True,  --  start executing immediately
               Show_Bar        => True,
               Destroy_On_Exit => True,
               Block_Exit      => False);
         end if;

      elsif Get_Error_Message (Action.Filter) /= "" then
         Insert (Data.Kernel, Get_Error_Message (Action.Filter),
                 Mode => Error);
      else
         Insert (Data.Kernel,
                 -"Invalid context for this action", Mode => Error);
      end if;
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
      Globals.Proxy_Items.Append (Proxy_And_Filter'(Item, Filter));

      Weak_Ref
        (Item,
         Notify => On_Delete_Proxy'Access,
         Data   => Kernel.all'Address);

      --  If the background updating of menus was taking place, we need to
      --  restart it since its iterators are now invalid.

      if Globals.Update_Menus_Idle_Id /= No_Source_Id then
         Update_Menus_And_Buttons (Kernel);
      end if;
   end Add_To_Global_Proxies;

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
         It := Element (C);
         if Get_Object (It.Proxy) = Item then
            Globals.Proxy_Items.Delete (C);

            --  Update cursor in the background updating, if needed

            if Globals.Update_Menus_Idle_Id /= No_Source_Id then
               Update_Menus_And_Buttons (K);
            end if;

            exit;
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
      Add_Before    : Boolean := True;
      Use_Mnemonics : Boolean := True)
   is
      Ignored : Gtk_Menu_Item;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Register_Menu
        (Kernel, Path, Action, Ref_Item, Add_Before,
         Use_Mnemonics => Use_Mnemonics);
   end Register_Menu;

   -------------------
   -- Register_Menu --
   -------------------

   function Register_Menu
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Path          : String;
      Action        : String;
      Ref_Item      : String := "";
      Add_Before    : Boolean := True;
      Optional      : Boolean := False;
      Use_Mnemonics : Boolean := True;
      Menubar       : access Gtk.Menu_Bar.Gtk_Menu_Bar_Record'Class := null)
      return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Self : Action_Menu_Item;
      Full_Path : constant String := Create_Menu_Path ("/", Path);
      Accel_Path  : constant String := "<gps>" & Full_Path;
      Item : Gtk_Menu_Item;
   begin
      Item := Find_Menu_Item (Kernel, Full_Path);
      if Item /= null then
         Trace (Me, "Menu registered twice: " & Full_Path);
         return Item;
      end if;

      --  Create the menu item
      Self := new Action_Menu_Item_Record;

      if Use_Mnemonics then
         Gtk.Image_Menu_Item.Initialize_With_Mnemonic
           (Self, Label => Base_Menu_Name (Full_Path));
      else
         Gtk.Image_Menu_Item.Initialize
           (Self, Label => Base_Menu_Name (Full_Path));
      end if;

      Self.Data := (Action    => new String'(Action),
                    Kernel    => Kernel,
                    Optional  => Optional,
                    Hide      => False,
                    Looked_Up => null);
      Self.Set_Accel_Path (Accel_Path);
      Get_Style_Context (Self).Add_Class ("gpsaction");

      --  Add it to the menubar. We do not use Dir_Name, which would ignore
      --  escaping and would use '\' as a separator.

      Register_Menu
        (Kernel, Parent_Menu_Name (Full_Path), Self, Ref_Item, Add_Before,
         Menubar => Menubar);

      Add_To_Global_Proxies (Self, Kernel, null);

      --  We have modified Global_Proxy_Items: if the menu recomputer
      --  is running, its cursors might be invalid: reset it now.

      if Globals.Update_Menus_Idle_Id /= No_Source_Id then
         Update_Menus_And_Buttons (Kernel);
      end if;

      --  And now setup the dynamic behavior

      Self.On_Activate (On_Activate_Action_Item'Access);

      return Gtk_Menu_Item (Self);
   end Register_Menu;

   -----------------------
   -- Register_MDI_Menu --
   -----------------------

   procedure Register_MDI_Menu
     (Kernel     : Kernel_Handle;
      Item_Name  : String;
      Accel_Path : String)
   is
      pragma Unreferenced (Accel_Path);
      Full_Path : constant String := "/Window/" & Item_Name;
   begin
      Register_Action
        (Kernel      => Kernel,
         Name        => Full_Path,
         Command     => Create_Command_For_Menu (Kernel, Full_Path),
         Description => "Menu " & Full_Path,
         Filter      => null,
         Category    => "Menus");
   end Register_MDI_Menu;

   ------------------
   -- Execute_Menu --
   ------------------

   procedure Execute_Menu
     (Kernel    : Kernel_Handle;
      Menu_Name : String)
   is
      Command : Menu_Command;
   begin
      Command := Create_Command_For_Menu (Kernel, Menu_Name);
      Launch_Foreground_Command
        (Kernel, Command, Destroy_On_Exit => False);
   end Execute_Menu;

   -------------------------
   -- Get_Toolbar_Section --
   -------------------------

   function Get_Toolbar_Section
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Toolbar : access Gtk.Toolbar.Gtk_Toolbar_Record'Class := null;
      Section : String;
      Last    : Boolean := True) return Glib.Gint
   is
      T     : Gtk_Toolbar := Gtk_Toolbar (Toolbar);
      Count : Gint;
      Item  : Gtk_Tool_Item;
      In_Section : Boolean := False;
   begin
      if Toolbar = null then
         T := Get_Toolbar (Kernel);
      end if;

      if T = null then
         return -1;
      end if;

      Count := T.Get_N_Items;
      for J in 0 .. Count - 1 loop
         Item := T.Get_Nth_Item (J);

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

   function Create_Toolbar
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Id      : String)
      return Gtk.Toolbar.Gtk_Toolbar
   is
      Toolbar : Gtk_Toolbar;

      procedure Process_Toolbar (Toolbar_Node : Node);
      --  Process a <toolbar node>

      ---------------------
      -- Process_Toolbar --
      ---------------------

      procedure Process_Toolbar (Toolbar_Node : Node) is
         N       : Node;
         Sep     : Gtk_Separator_Tool_Item;
         Hide    : Boolean;
      begin
         N := First_Child (Toolbar_Node);
         while N /= null loop
            if Node_Name (N) = "button" then
               Hide := False;

               if Get_Attribute (N, "hide") /= "" then
                  begin
                     Hide := Boolean'Value (Get_Attribute (N, "hide"));
                  exception
                     when Constraint_Error =>
                        Hide := False;
                  end;
               end if;

               Register_Button
                 (Kernel,
                  Action   => Get_Attribute (N, "action"),
                  Stock_Id => Get_Attribute (N, "stock"),
                  Toolbar  => Toolbar,
                  Hide     => Hide);

            elsif Node_Name (N) = "separator" then
               Gtk_New (Sep);
               Sep.Set_Name (Get_Attribute (N, "id"));
               Toolbar.Insert (Sep);
            end if;

            N := Next_Sibling (N);
         end loop;
      end Process_Toolbar;

      N : Node;
   begin
      Gtk_New (Toolbar);
      Toolbar.Set_Name (Id);
      Toolbar.Set_Icon_Size (Icon_Size_Small_Toolbar);
      Toolbar.Set_Style (Toolbar_Icons);
      Toolbar.Set_Show_Arrow (True);

      if not Globals.Toolbar_Descriptions.Contains (Id) then
         return Toolbar;
      end if;

      N := Globals.Toolbar_Descriptions.Element (Id);
      if N /= null then
         Process_Toolbar (N);

         while N /= null loop
            declare
               Inh : constant String := Get_Attribute (N, "inherit");
            begin
               if Inh /= ""
                 and then Globals.Toolbar_Descriptions.Contains (Inh)
               then
                  N := Globals.Toolbar_Descriptions.Element (Inh);

                  if N /= null then
                     Process_Toolbar (N);
                  end if;
               else
                  N := null;
               end if;
            end;
         end loop;
      end if;

      return Toolbar;
   end Create_Toolbar;

   ---------------------
   -- Register_Button --
   ---------------------

   procedure Register_Button
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Action   : String;
      Stock_Id : String := "";
      Toolbar  : access Gtk.Toolbar.Gtk_Toolbar_Record'Class := null;
      Position : Glib.Gint := -1;
      Hide     : Boolean := False)
   is
      Button : Action_Tool_Button;
   begin
      Gtk_New (Button, Kernel, Stock_Id => Stock_Id, Action => Action,
               Hide => Hide);

      if Toolbar = null then
         Get_Toolbar (Kernel).Insert (Button, Position);
      else
         Toolbar.Insert (Button, Pos => Position);
      end if;

      Show_All (Button);
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
                     Open_File_Editor
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
         GNAT.OS_Lib.Free (Menu_Ref.Menu.Pix);
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

            if Menu.Menu_Type = Type_Separator then
               Menu.Ref_Item := C;
            end if;

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
      Name        : String;
      Action      : Action_Record_Access;
      Label       : String := "";
      Custom      : Custom_Expansion := null;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group)
   is
      T      : Contextual_Label_Param;
      Pix    : GNAT.Strings.String_Access;
      Menu   : Contextual_Menu_Access;
      Filter : access Action_Filter_Record'Class;

      Is_Separator : Boolean := False;
   begin
      if Label /= "" then
         T        := new Contextual_Label_Parameters;
         T.Label  := new String'(Label);
         T.Custom := Custom;
         T.Filter := Create_Filter (Label);
      end if;

      if Stock_Image /= "" then
         Pix := new String'(Stock_Image);
      end if;

      if Action = null then
         Is_Separator := True;
      else
         --  Look at the label to determine if this is a separator
         for J in reverse Name'Range loop
            if Name (J) = '/' then
               if J < Name'Last
                 and then Name (J + 1) = '-'
               then
                  Is_Separator := True;
               end if;
               exit;
            end if;
         end loop;
      end if;

      if Is_Separator then
         if Action /= null then
            Filter := Action.Filter;
         end if;

         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Separator,
            Name                  => new String'(Name),
            Separator_Filter      => Filter,
            Pix                   => Pix,
            Next                  => null,
            Ref_Item              => null,
            Group                 => Group,
            Visible               => True,
            Sensitive             => True,
            Filter_Matched        => False,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (T));
      else
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Action,
            Name                  => new String'(Name),
            Action                => Action,
            Pix                   => Pix,
            Next                  => null,
            Group                 => Group,
            Visible               => True,
            Sensitive             => True,
            Filter_Matched        => False,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (T));
      end if;

      Add_Contextual_Menu (Kernel, Menu, Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Action      : Action_Record_Access;
      Label       : access Contextual_Menu_Label_Creator_Record'Class;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Integer := Default_Contextual_Group)
   is
      Pix  : GNAT.Strings.String_Access;
      Menu : Contextual_Menu_Access;
   begin
      if Stock_Image /= "" then
         Pix := new String'(Stock_Image);
      end if;

      if Action = null then
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Separator,
            Name                  => new String'(Name),
            Separator_Filter      => null,
            Pix                   => Pix,
            Next                  => null,
            Group                 => Group,
            Ref_Item              => null,
            Visible               => True,
            Sensitive             => True,
            Filter_Matched        => False,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (Label));
      else
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Action,
            Name                  => new String'(Name),
            Action                => Action,
            Pix                   => Pix,
            Next                  => null,
            Group                 => Group,
            Visible               => True,
            Sensitive             => True,
            Filter_Matched        => False,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (Label));
      end if;

      Add_Contextual_Menu (Kernel, Menu, Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Action            : Commands.Interactive.Interactive_Command_Access;
      Filter            : access Action_Filter_Record'Class := null;
      Enable_Filter     : access Action_Filter_Record'Class := null;
      Label             : access Contextual_Menu_Label_Creator_Record'Class;
      Stock_Image       : String := "";
      Ref_Item          : String := "";
      Add_Before        : Boolean := True;
      Group             : Integer := Default_Contextual_Group)
   is
      Pix  : GNAT.Strings.String_Access;
      Menu : Contextual_Menu_Access;
   begin
      if Stock_Image /= "" then
         Pix := new String'(Stock_Image);
      end if;

      if Action = null then
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Separator,
            Name                  => new String'(Name),
            Separator_Filter      => Filter,
            Pix                   => Pix,
            Next                  => null,
            Ref_Item              => null,
            Group                 => Group,
            Visible               => True,
            Filter_Matched        => False,
            Sensitive             => True,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (Label));
      else
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Command,
            Name                  => new String'(Name),
            Command               => Action,
            Filter                => Filter,
            Enable_Filter         => Enable_Filter,
            Pix                   => Pix,
            Next                  => null,
            Group                 => Group,
            Visible               => True,
            Filter_Matched        => False,
            Sensitive             => True,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (Label));
         Register_Perma_Command (Kernel, Action);
      end if;

      Add_Contextual_Menu (Kernel, Menu, Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel            : access Kernel_Handle_Record'Class;
      Name              : String;
      Action            : Commands.Interactive.Interactive_Command_Access :=
                            null;
      Filter            : access Action_Filter_Record'Class := null;
      Enable_Filter     : access Action_Filter_Record'Class := null;
      Label             : String := "";
      Custom            : Custom_Expansion := null;
      Stock_Image       : String := "";
      Ref_Item          : String := "";
      Add_Before        : Boolean := True;
      Group             : Integer := Default_Contextual_Group)
   is
      T    : Contextual_Label_Param;
      Pix  : GNAT.Strings.String_Access;
      Menu : Contextual_Menu_Access;
   begin
      if Label /= "" then
         T        := new Contextual_Label_Parameters;
         T.Label  := new String'(Label);
         T.Custom := Custom;
         T.Filter := Create_Filter (Label);
      end if;

      if Stock_Image /= "" then
         Pix := new String'(Stock_Image);
      end if;

      if Action = null then
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Separator,
            Name                  => new String'(Name),
            Separator_Filter      => Filter,
            Pix                   => Pix,
            Next                  => null,
            Ref_Item              => null,
            Visible               => True,
            Group                 => Group,
            Sensitive             => True,
            Filter_Matched        => False,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (T));
      else
         Menu := new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Command,
            Name                  => new String'(Name),
            Command               => Action,
            Filter                => Filter,
            Enable_Filter         => Enable_Filter,
            Pix                   => Pix,
            Next                  => null,
            Visible               => True,
            Group                 => Group,
            Sensitive             => True,
            Filter_Matched        => False,
            Label_For_Context     => Null_Unbounded_String,
            Label                 => Contextual_Menu_Label_Creator (T));
         Register_Perma_Command (Kernel, Action);
      end if;

      Add_Contextual_Menu (Kernel, Menu, Ref_Item, Add_Before);
   end Register_Contextual_Menu;

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
      if Menu_Ref = Null_Reference then
         Register_Contextual_Menu
           (Kernel => Kernel,
            Name              => Name,
            Action            => null,
            Filter            => null,
            Label             => "");
         Menu_Ref := Find_Contextual_Menu_By_Name (Kernel, Name);
      end if;
      Menu_Ref.Menu.Visible := Visible;
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
           (Kernel            => Kernel,
            Name              => Name,
            Action            => null,
            Filter            => null,
            Label             => "");
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
         T.Filter := Create_Filter (Label);
      end if;

      Add_Contextual_Menu
        (Kernel,
         new Contextual_Menu_Record'
           (Kernel                => Kernel_Handle (Kernel),
            Menu_Type             => Type_Submenu,
            Name                  => new String'(Name),
            Submenu_Filter        => Filter,
            Submenu_Enable        => Enable_Filter,
            Pix                   => null,
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
      B      : constant Action_Tool_Button := Action_Tool_Button (Button);
   begin
      Execute_Action (B, B.Data);
   end On_Action_Button_Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button   : out Action_Tool_Button;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Stock_Id : String := "";
      Action   : String;
      Optional : Boolean := False;
      Hide     : Boolean := False)
   is
      Act : access Action_Record;
   begin
      --  ??? Should automatically grey out when the context does not match.
      Button := new Action_Tool_Button_Record;
      Button.Forced_Stock := Stock_Id /= "";
      Button.Data := (Kernel    => Kernel,
                      Optional  => Optional,
                      Hide      => Hide,
                      Action    => new String'(Action),
                      Looked_Up => null);

      if Stock_Id /= "" then
         Initialize_From_Stock (Button, Stock_Id);
      else
         Initialize (Button, Label => Action);
      end if;

      Get_Style_Context (Button).Add_Class ("gpsaction");

      --  The side effect is to set image, tooltip,... if the action already
      --  exists.
      --  If the action is unknown, or it has a filter, we will need to
      --  monitor this button when the context changes.

      Act := Lookup_Action (Button);
      if Act = null or else Act.Filter /= null then
         Add_To_Global_Proxies (Button, Kernel, null);
      end if;

      Button.On_Clicked (On_Action_Button_Clicked'Access);
   end Gtk_New;

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

      else
         if Self.Optional or else Self.Hide then
            W.Hide;
         else
            W.Set_Sensitive (False);
         end if;
      end if;
   end Set_Active;

   ------------------------------------
   -- Update_Menus_And_Buttons_Chunk --
   ------------------------------------

   function Update_Menus_And_Buttons_Chunk
     (Data : Update_Menus_Data_Access) return Boolean
   is
      Max_Idle_Duration : constant Duration := 0.05;
      A, Tmp_Elem : Proxy_And_Filter;
      Action : access Action_Record;
      Start  : constant Time := Clock;
      Tmp    : Proxy_Lists.Cursor;
      Menu_Bar : Gtk_Menu_Bar;
      Tool_Bar : Gtk_Toolbar;
      Available : Boolean;

      procedure Propagate_Visibility
        (Widget : not null access Gtk_Widget_Record'Class;
         Parent : not null access Gtk_Container_Record'Class);
      --  Hide menu items for which all children are also hidden.
      --  Set menu items insensitive if all children are also insensitive
      --  Widget's visibility and sensitivity will be updated based on the
      --  list of children of Parent.

      procedure Cleanup_Toolbar_Separators
        (Toolbar : not null access Gtk_Toolbar_Record'Class);
      --  Cleanup separators in a toolbar

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
            Widget.Set_Sensitive (S);
         end if;
      end Propagate_Visibility;

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

      D : access Action_Proxy'Class;
   begin
      loop
         if not Has_Element (Data.Current) then
            --  no more items

            Menu_Bar := GPS_Window
              (Get_Kernel (Data.Context).Get_Main_Window).Menu_Bar;
            Propagate_Visibility (Menu_Bar, Menu_Bar);

            Tool_Bar := Get_Toolbar (Get_Kernel (Data.Context));
            Cleanup_Toolbar_Separators (Tool_Bar);

            Globals.Update_Menus_Idle_Id := No_Source_Id;
            return False;
         end if;

         if Clock - Start > Max_Idle_Duration then
            --  will try again
            return True;
         end if;

         A := Element (Data.Current);
         D := Get_Data (A.Proxy);

         if A.Filter /= null then
            D.Set_Active (Filter_Matches (A.Filter, Data.Context), A.Proxy);
         else
            Action := Lookup_Action (A.Proxy);

            if Action = null then
               D.Set_Active (False, A.Proxy);

            else
               if Action.Filter = null then
                  D.Set_Active (True, A.Proxy);

                  --  The item is already active, and will remain so, so
                  --  nothing to do here. We thus remove the item from the list
                  --  since there will be nothing to do with it anymore,
                  --  not worth wasting time
                  --  ??? If the action is overridden, we might need to review
                  --  the policy here, but that should not happen.

                  Tmp := Previous (Data.Current);

                  --  Do this so that we don't hold a cursor on a container
                  --  which is being modified.

                  if Has_Element (Tmp) then
                     Tmp_Elem := Element (Tmp);

                     Globals.Proxy_Items.Delete (Data.Current);

                     Tmp := Globals.Proxy_Items.First;
                     while Has_Element (Tmp)
                       and then Element (Tmp) /= Tmp_Elem
                     loop
                        Next (Tmp);
                     end loop;
                     Data.Current := Tmp;
                  else
                     Globals.Proxy_Items.Delete (Data.Current);
                     Data.Current := Globals.Proxy_Items.First;
                  end if;

               else
                  --  The context caches the filter, so there is limited
                  --  cost in computing multiple times whether a given
                  --  filter matches.

                  Available := Filter_Matches (Action.Filter, Data.Context);
                  D.Set_Active (Available, A.Proxy);

                  --  If this is a tool button that we are just showing,
                  --  it is possible that the stock id was set at a time
                  --  when the actual stock was not defined: in this case,
                  --  force a refresh.
                  if Available
                    and then Action.Stock_Id /= null
                    and then A.Proxy.all in Action_Tool_Button_Record'Class
                    and then not Action_Tool_Button (A.Proxy).Forced_Stock
                  then
                     Action_Tool_Button (A.Proxy).Set_Stock_Id
                       (Action.Stock_Id.all);
                  end if;
               end if;
            end if;
         end if;

         Next (Data.Current);
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
   begin
      if Kernel.Is_In_Destruction then
         return;
      end if;

      if Ctxt = No_Context then
         Ctxt := Get_Current_Context (Kernel);
      end if;

      if Globals.Update_Menus_Idle_Id /= No_Source_Id then
         Was_Computing_Idle := True;
         Remove (Globals.Update_Menus_Idle_Id);
      end if;

      Data := new Update_Menus_Data'
        (Context => Ctxt,
         Current => Globals.Proxy_Items.First);

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
   end Update_Menus_And_Buttons;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      type Context_Args is access all Context_Hooks_Args'Class;
      D : constant Context_Args := Context_Args (Data);
   begin
      Update_Menus_And_Buttons (Kernel, D.Context);
   end On_Context_Changed;

   ----------------------------
   -- Start_Monitoring_Menus --
   ----------------------------

   procedure Start_Monitoring_Menus
     (Kernel      : not null access Kernel_Handle_Record'Class) is
   begin
      Add_Hook
        (Kernel,
         Context_Changed_Hook,
         Wrapper (On_Context_Changed'Access),
         Name => "monitor context for menus");
   end Start_Monitoring_Menus;

   -------------------
   -- Install_Menus --
   -------------------

   procedure Install_Menus
     (Kernel    : not null access Kernel_Handle_Record'Class;
      App       : not null access Gtk.Application.Gtk_Application_Record'Class;
      Description : GNATCOLL.VFS.Virtual_File;
      Menubar   : out Gtk.Menu_Bar.Gtk_Menu_Bar)
   is
      procedure Process_Menu_Bar (Menubar_Node : Node);
      --  Process a <menubar> node

      Builder_XML : Unbounded_String;

      procedure Process_Menu
        (Parent_Path : String;
         Parent      : not null access Gtk_Menu_Shell_Record'Class;
         Menu_Node   : Node);
      --  Process a <menu> node

      ------------------
      -- Process_Menu --
      ------------------

      procedure Process_Menu
        (Parent_Path : String;
         Parent      : not null access Gtk_Menu_Shell_Record'Class;
         Menu_Node   : Node)
      is
         Label  : constant DOM_String := Get_Attribute (Menu_Node, "label");
         Action : constant DOM_String := Get_Attribute (Menu_Node, "action");
         Optional_Str : constant DOM_String :=
           Get_Attribute (Menu_Node, "optional");
         Optional : Boolean := False;
         Item   : Gtk_Menu_Item;
         Sep    : Gtk_Separator_Menu_Item;
         Menu   : Gtk_Menu;
         N      : Node;
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
            Item := Register_Menu
              (Kernel, Parent_Path & "/" & Label,
               Action, Optional => Optional, Use_Mnemonics => True,
               Menubar => Menubar);

            if Active (System_Menus) then
               App.Add_Action (New_G_Action (Kernel, Action));
               Append (Builder_XML,
                       "<item><attribute name='label'>"
                       & XML_Utils.Protect (Label)
                       & "</attribute>"
                       & "<attribute name='action'>app."
                       & XML_Utils.Protect (Action)
                       & "</attribute></item>" & ASCII.LF);
            end if;
         else
            Gtk_New_With_Mnemonic (Item, Label);
            Parent.Append (Item);

            if Active (System_Menus) then
               Append (Builder_XML,
                       "<submenu>"
                       & "<attribute name='label'>" & XML_Utils.Protect (Label)
                       & "</attribute>" & ASCII.LF
                       & " <section>");
            end if;
         end if;

         N := First_Child (Menu_Node);

         if N /= null then
            Gtk_New (Menu);
            Menu.Set_Accel_Group (Get_Default_Accelerators (Kernel));
            Item.Set_Submenu (Menu);

            while N /= null loop
               if Node_Name (N) = "menu" then
                  Process_Menu
                    (Parent_Path & "/" & Strip_Single_Underscores (Label),
                     Menu, N);
               elsif Node_Name (N) = "separator" then
                  Gtk_New (Sep);
                  Menu.Append (Sep);

                  if Active (System_Menus) then
                     Append (Builder_XML, "</section><section>");
                  end if;
               end if;
               N := Next_Sibling (N);
            end loop;
         end if;

         if Active (System_Menus) and then Action = "" then
            Append (Builder_XML, "</section></submenu>" & ASCII.LF);
         end if;
      end Process_Menu;

      ----------------------
      -- Process_Menu_Bar --
      ----------------------

      procedure Process_Menu_Bar (Menubar_Node : Node) is
         N    : Node := First_Child (Menubar_Node);
      begin
         while N /= null loop
            if Node_Name (N) = "menu" then
               Process_Menu ("", Menubar, N);
            end if;

            N := Next_Sibling (N);
         end loop;
      end Process_Menu_Bar;

      Input  : File_Input;
      Reader : Tree_Reader;
      Doc    : Document;
      N      : Node;
   begin
      Trace (Me, "Install menus from " & Description.Display_Full_Name);
      if Globals.Symbols = No_Symbol_Table then
         Globals.Symbols := Allocate;
      end if;

      Gtk_New (Menubar);

      Builder_XML := To_Unbounded_String ("<interface><menu id='menubar'>");

      Open (Description.Display_Full_Name, Input);
      Reader.Set_Symbol_Table (Globals.Symbols);
      Parse (Reader, Input);
      Close (Input);

      Doc := Get_Tree (Reader);
      N := Get_Element (Doc);   --  name is irrelevant
      N := First_Child (N);

      while N /= null loop
         if Node_Name (N) = "menubar" then
            Process_Menu_Bar (N);
         elsif Node_Name (N) = "toolbar" then
            Globals.Toolbar_Descriptions.Include
              (Get_Attribute (N, "id"), Clone_Node (N, Deep => True));
         end if;

         N := Next_Sibling (N);
      end loop;

      Free (Reader);

      if Active (System_Menus) then
         Append (Builder_XML, "</menu></interface>");

         declare
            Menubar   : GObject;
            Builder   : Gtk_Builder;
            Tmp       : Guint;
            Error     : aliased GError;
         begin
            Gtk_New (Builder);
            Tmp := Builder.Add_From_String
              (To_String (Builder_XML), Error => Error'Access);
            if Tmp /= 0 then
               Menubar := Builder.Get_Object ("menubar");
               App.Set_Menubar (Gmenu_Model (Menubar));
            end if;
            Unref (Builder);
         end;
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
      return New_String (S.Data.Action.all);  --  ??? memory leak
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

   ------------------
   -- New_G_Action --
   ------------------

   function New_G_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      Action : String) return Glib.Action.Gaction
   is
      Act : GPS_Action;
   begin
      --  one action per GPS action, so that they each have their own handling
      --  of enabled. We unfortunately can't use a generic action with a target
      --  name that would be the GPS action name.

      Act := new GPS_Action_Record;
      G_New (Act, GPS_Action_Get_Type);

      Act.Data := (Action    => new String'(Action),
                   Kernel    => Kernel,
                   Optional  => False,
                   Hide      => False,
                   Active    => True,
                   Looked_Up => null);
      Add_To_Global_Proxies (Act, Kernel, Filter => null);

      return +Act;
   end New_G_Action;

end GPS.Kernel.Modules.UI;
