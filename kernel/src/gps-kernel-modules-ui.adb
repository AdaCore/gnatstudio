------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2013, AdaCore                     --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Traces;           use GNATCOLL.Traces;

with Gdk.Drag_Contexts;         use Gdk.Drag_Contexts;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types;                 use Gdk.Types;

with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;

with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;
with Gtk.Style_Context;         use Gtk.Style_Context;

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
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Main_Window;           use GPS.Main_Window;
with GUI_Utils;                 use GUI_Utils;
with System;                    use System;
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

   type Contextual_Menu_User_Data is record
      Object       : GObject;
      Context_Func : Context_Factory;
      Kernel       : Kernel_Handle;
      ID           : Module_ID;
      Event_Widget : Gtk_Widget;
   end record;

   type Menu_Factory_User_Data is record
      Kernel  : Kernel_Handle;
      Factory : Dynamic_Menu_Factory;
      Menu    : Gtk_Menu;
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
               Filter        : GPS.Kernel.Action_Filter;
               Enable_Filter : GPS.Kernel.Action_Filter;

            when Type_Action =>
               Action           : Action_Record_Access;

            when Type_Submenu =>
               Submenu          : Submenu_Factory;
               Submenu_Filter   : GPS.Kernel.Action_Filter;
               Submenu_Enable   : GPS.Kernel.Action_Filter;

            when Type_Separator =>
               Ref_Item         : Contextual_Menu_Access;
               Separator_Filter : GPS.Kernel.Action_Filter;
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

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu;
   --  Create a contextual menu as a result of a mouse event
   --  Return null if no menu was created.

   type Interactive_Action is record
      Kernel  : Kernel_Handle;
      Command : Interactive_Command_Access;
      Filter  : Action_Filter;
   end record;

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Interactive_Action);
   --  Execute a single command

   procedure Menu_Button_Press
     (Widget : access GObject_Record'Class;
      Data   : Menu_Factory_User_Data);
   --  Create a menu using the data in Factory

   package Command_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Interactive_Action);

   package Menu_Factory_Callback is
   new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Menu_Factory_User_Data);

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

   procedure Map_Menu
     (Item    : access GObject_Record'Class;
      Command : Interactive_Action);
   --  Called when a registered menu is displayed, so that we can check whether
   --  it should be made sensitive or not.

   procedure Unmap_Menu (Menu : access Gtk_Widget_Record'Class);
   --  Called when a menu is unmapped, so that the associated context can be
   --  destroyed.

   package Context_User_Data is new Glib.Object.User_Data (Selection_Context);
   package Integer_User_Data is new Glib.Object.User_Data (Integer);

   function Base_Menu_Name (Path : String) return String;
   --  Return the base name of a menu path. '/' is the menu separator. This
   --  subprogram handles pango markup correctly.

   function Dir_Menu_Name (Path : String) return String;
   --  Return the directory name of a menu path. '/' is the menu separator.
   --  This subprogram handles pango markup correctly.

   function Cleanup (Path : String) return String;
   --  Remove duplicate // in Path

   function Create_Command_For_Menu
     (Kernel    : Kernel_Handle;
      Full_Path : String) return Menu_Command;
   --  Utility function: create a command for a given menu

   ----------------------
   -- Action_Menu_Item --
   ----------------------

   type Action_Menu_Item_Record is new Gtk_Image_Menu_Item_Record with record
      Kernel   : Kernel_Handle;
      Name     : GNAT.Strings.String_Access;
      Optional : Boolean;

      Action   : Action_Record_Access;
      --  null until looked up
   end record;
   type Action_Menu_Item is access all Action_Menu_Item_Record'Class;

   procedure Lookup_Action
     (Self : not null access Action_Menu_Item_Record'Class);
   --  lookup the action if needed

   procedure On_Map_Action_Item (Item : access GObject_Record'Class);
   procedure On_Activate_Action_Item
     (Item : access Gtk_Menu_Item_Record'Class);
   procedure On_Destroy_Action_Item (Item : access Gtk_Widget_Record'Class);
   --  Called when an Action_Menu_Item is mapped to screen

   --------------
   -- Toolbars --
   --------------

   type Action_Tool_Button_Record is new Gtk_Tool_Button_Record with record
      Kernel : Kernel_Handle;
      Action : GNAT.Strings.String_Access;
   end record;
   type Action_Tool_Button is access all Action_Tool_Button_Record'Class;

   procedure Gtk_New
     (Button   : out Action_Tool_Button;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Stock_Id : String;
      Action   : String);
   --  Create a new button so that it executes action when pressed.

   procedure On_Action_Button_Clicked
     (Button : access Gtk_Widget_Record'Class);
   --  Called when an Action_Tool_Button has been activated.

   procedure On_Action_Button_Destroy
     (Button : access Gtk_Widget_Record'Class);
   --  Called when an Action_Tool_Button is destroyed.

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
      pragma Unreferenced (Context);
      Menu : constant Gtk_Menu_Item := Find_Menu_Item
        (Command.Kernel, Command.Menu_Name.all);
   begin
      if Menu /= null then
         Trace (Me, "Executing " & Command.Menu_Name.all);
         Activate (Menu);
         return Success;
      else
         Command.Kernel.Insert
           (-"Can't execute " & Command.Menu_Name.all,
            Mode => Error);
         return Failure;
      end if;
   end Execute;

   --------------------
   -- Base_Menu_Name --
   --------------------

   function Base_Menu_Name (Path : String) return String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = '/'
           and then J > Path'First
           and then Path (J - 1) /= '<'
         then
            return Path (J + 1 .. Path'Last);
         end if;
      end loop;

      return Path;
   end Base_Menu_Name;

   -------------------
   -- Dir_Menu_Name --
   -------------------

   function Dir_Menu_Name (Path : String) return String is
   begin
      for J in reverse Path'Range loop
         if Path (J) = '/'
           and then ((J > Path'First and then Path (J - 1) /= '<')
                     or else J = Path'First)
         then
            return Path (Path'First .. J);
         end if;
      end loop;

      return Path;
   end Dir_Menu_Name;

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
         C := Get_Focus_Child (Get_MDI (Kernel));

      else
         --  We have an explicit widget with the keyboard focus. Check whether
         --  it belongs to an MDI child. If not, it is probably part of some
         --  popup dialog, and therefore There is no module.
         --  As a special case, if the focus widget's parent is a notebook,
         --  we check whether the associated page is a MDI child, and behave
         --  as if that child had the focus (EC19-008)

         C := Find_MDI_Child_From_Widget (W);
      end if;

      if C = null
        or else C /= Get_Focus_Child (Get_MDI (Kernel))
      then
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
      Current : Cursor := Abstract_Module_List.First (List);
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
      Current : Cursor := Abstract_Module_List.First (List);
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
      Push_State (Action.Kernel, Busy);
      Context.Context := Action.Kernel.Last_Context_For_Contextual;

      Assert (Me, Context.Context.Data.Data /= null,
              "Contextual_Action called on freed context");
      Context.Event :=
        GPS_Window
          (Action.Kernel.Main_Window).Last_Event_For_Contextual;
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
      Pop_State (Action.Kernel);
   exception
      when E : others =>
         Pop_State (Action.Kernel);
         Trace (Me, "Unexpected exception while executing "
                & Action.Name.all & " " & Exception_Information (E));
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
      if Kernel.Last_Context_For_Contextual /= No_Context then
         Trace (Me, "Running Hook " & To_String (Contextual_Menu_Close_Hook));
         Run_Hook (Kernel, Contextual_Menu_Close_Hook);
         Trace (Me, "Destroying contextual menu and its context");
      end if;
   end Contextual_Menu_Destroyed;

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
                        if Dir_Menu_Name ('/' & Label_Name (C2, Context)) =
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
                    (Item, Signal_Activate,
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
         Parent : constant String := Dir_Menu_Name ('/' & Label);
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
                  Path          => Dir_Menu_Name ('/' & Full_Name.all),
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

      Pop_State (Kernel);

      --  If the menu is empty, destroy it

      List := Get_Children (Menu);
      if List = Gtk.Widget.Widget_List.Null_List then
         Destroy (Menu);
         Menu := null;
      end if;

      Widget_List.Free (List);

      if Menu /= null then
         Weak_Ref (Menu, Contextual_Menu_Destroyed'Access,
                   Data => Kernel.all'Address);
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
   begin
      --  Create the menu and add all the modules information
      Gtk_New (Menu);

      Push_State (User.Kernel, Busy);

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

      if GPS_Window (User.Kernel.Main_Window).Last_Event_For_Contextual
        /= null
      then
         Free (GPS_Window (User.Kernel.Main_Window).Last_Event_For_Contextual);
      end if;

      GPS_Window (User.Kernel.Main_Window).Last_Event_For_Contextual :=
        Copy (Event);

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
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item is
   begin
      if Kernel.Main_Window = null then
         return null;
      else
         return Find_Or_Create_Menu_Tree
           (Menu_Bar      => GPS_Window (Kernel.Main_Window).Menu_Bar,
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
      Filter      : Action_Filter  := null)
   is
      Parent, Pred : Gtk_Menu_Item;
      Parent_Menu  : Gtk_Menu;
      Index        : Gint;

   begin
      Parent := Find_Or_Create_Menu_Tree
        (Menu_Bar     => GPS_Window (Kernel.Main_Window).Menu_Bar,
         Menu         => null,
         Path         => Format (Parent_Path),
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
           (GPS_Window (Kernel.Main_Window).Menu_Bar,
            Parent_Menu, Ref_Item, Pred, Index);
         Add_Menu (Parent     => Parent_Menu,
                   Menu_Bar   => GPS_Window (Kernel.Main_Window).Menu_Bar,
                   Item       => Item,
                   Index      => Index,
                   Add_Before => Add_Before);
         Show_All (Item);

         if Filter /= null then
            Command_Callback.Object_Connect
              (Get_Toplevel (Item), Signal_Map, Map_Menu'Access,
               Slot_Object => Item,
               User_Data   => (Kernel_Handle (Kernel), null, Filter));
         end if;

         if Item.Get_Child /= null then
            declare
               Full : constant String :=
                 Cleanup ("/" & Parent_Path & "/" & Item.Get_Label);
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

   -------------------
   -- Register_Menu --
   -------------------

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      Command     : Interactive_Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True;
      Filter      : Action_Filter  := null;
      Mnemonics   : Boolean := True)
   is
      Ignore : Gtk_Menu_Item;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Register_Menu
        (Kernel, Parent_Path, Text, Stock_Image, Callback, Command,
         Accel_Key, Accel_Mods, Ref_Item, Add_Before, Sensitive,
         Filter, Mnemonics);
   end Register_Menu;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Interactive_Action)
   is
      pragma Unreferenced (Widget);
      Context : constant Selection_Context :=
                  Get_Current_Context (Command.Kernel);
   begin
      if Context /= No_Context
        and then Filter_Matches (Command.Filter, Context)
      then
         Launch_Background_Command
           (Command.Kernel,
            Create_Proxy
              (Command.Command,
               (null, Context, False, No_File, null, null, 1, 0)),
            Destroy_On_Exit => False,  --  ??? Should this be True
            Active          => True, Show_Bar => False, Queue_Id => "");

      elsif Get_Error_Message (Command.Filter) /= "" then
         Insert (Command.Kernel, Get_Error_Message (Command.Filter),
                 Mode => Error);

      else
         Insert (Command.Kernel,
                 -"Invalid context for this action", Mode => Error);
      end if;
   end Execute_Command;

   -------------
   -- Cleanup --
   -------------

   function Cleanup (Path : String) return String is
      Output : String (Path'Range);
      Index  : Natural := Output'First;
   begin
      for P in Path'Range loop
         if Path (P) /= '_'
           and then (Path (P) /= '/'
                     or else P + 1 > Path'Last
                     or else Path (P + 1) /= '/')
         then
            Output (Index) := Path (P);
            Index := Index + 1;
         end if;
      end loop;
      return Output (Output'First .. Index - 1);
   end Cleanup;

   -------------------
   -- Register_Menu --
   -------------------

   function Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Callback    : Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      Command     : Interactive_Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True;
      Filter      : Action_Filter  := null;
      Mnemonics   : Boolean := True) return Gtk_Menu_Item
   is
      use type Kernel_Callback.Marshallers.Void_Marshaller.Handler;

      Full_Path  : constant String := Cleanup ('/' & Parent_Path & '/' & Text);

      Accel_Path  : constant String := "<gps>" & Full_Path;
      Item        : Gtk_Menu_Item;
      Image       : Gtk_Image_Menu_Item;
      Pix         : Gtk_Image;
      The_Command : Interactive_Command_Access;

   begin
      if Stock_Image = "" then
         if Mnemonics then
            Gtk_New_With_Mnemonic (Item, Text);
         else
            Gtk_New (Item, Text);
         end if;
      else
         if Mnemonics then
            Gtk_New_With_Mnemonic (Image, Text);
         else
            Gtk_New (Image, Text);
         end if;

         Gtk_New (Pix, Stock_Image, Icon_Size_Menu);
         Set_Image (Image, Pix);
         Item := Gtk_Menu_Item (Image);
      end if;

      Set_Sensitive (Item, Sensitive);
      Set_Accel_Path (Item, Accel_Path);

      if Guint (Accel_Key) > 0 then
         Gtk.Accel_Map.Add_Entry
           (Accel_Path,
            Accel_Key  => Accel_Key,
            Accel_Mods => Accel_Mods);
      end if;

      Register_Menu (Kernel, Parent_Path, Item, Ref_Item, Add_Before);

      if Callback /= null then
         Kernel_Callback.Connect
           (Item, Signal_Activate,
            Kernel_Callback.To_Marshaller (Callback), Kernel_Handle (Kernel));
      end if;

      if Command /= null then
         Command_Callback.Connect
           (Item, Signal_Activate, Execute_Command'Access,
            User_Data   => (Kernel_Handle (Kernel), Command, Filter));
         Register_Perma_Command (Kernel, Command);
      end if;

      if Filter /= null then
         Register_Filter (Kernel, Filter, "");  --  Memory management only
         Command_Callback.Object_Connect
           (Get_Toplevel (Item), Signal_Map, Map_Menu'Access,
            Slot_Object => Item,
            User_Data   => (Kernel_Handle (Kernel), null, Filter));
      end if;

      --  For every menu that we create, register an action

      --  If we already have an action or an interactive command, simply
      --  reuse it.

      if Command /= null then
         The_Command := Command;
      else
         --  Otherwise, create the wrapper command which will launch the menu

         The_Command := Interactive_Command_Access
           (Create_Command_For_Menu (Kernel_Handle (Kernel), Full_Path));
      end if;

      Register_Action
        (Kernel      => Kernel,
         Name        => Full_Path,
         Command     => The_Command,
         Description => "Menu " & Full_Path,
         Filter      => Filter,
         Category    => "Menus");

      if Accel_Key /= 0
        or else Accel_Mods /= 0
      then
         Set_Default_Key (Kernel     => Kernel,
                          Action     => Full_Path,
                          Accel_Key  => Accel_Key,
                          Accel_Mods => Accel_Mods);
      end if;

      return Item;
   end Register_Menu;

   -------------------
   -- Lookup_Action --
   -------------------

   procedure Lookup_Action
     (Self : not null access Action_Menu_Item_Record'Class)
   is
      Label : Gtk_Accel_Label;
      Key   : Gdk_Key_Type;
      Mods  : Gdk_Modifier_Type;
      Pix   : Gtk_Image;
   begin
      if Self.Action = null then
         Self.Action := Lookup_Action (Self.Kernel, Self.Name.all);

         if Self.Action /= null then
            --  ??? Could free Self.Name, which duplicates Self.Action.Name

            Self.Set_Tooltip_Markup
              (Escape_Text (Self.Action.Description.all)
               & ASCII.LF & ASCII.LF
               & "<b>Action:</b> "
               & Escape_Text (Self.Action.Name.all) & ASCII.LF
               & "<b>Category:</b> "
               & Escape_Text (Self.Action.Category.all) & ASCII.LF
               & "<b>Shortcut:</b> "
               & Self.Kernel.Get_Shortcut
                 (Action          => Self.Action.Name.all,
                  Use_Markup      => True,
                  Return_Multiple => True));

            --  Update the image if the action has one

            if Self.Action.Stock_Id /= null then
               Gtk_New (Pix, Self.Action.Stock_Id.all, Icon_Size_Menu);
               Self.Set_Image (Pix);
               Pix.Show;
            end if;

            --  Lookup the keybinding. This is only done the first time we do
            --  the lookup to save time, but this means that after the user has
            --  edited the keyshortcuts, this will no longer be up-to-date.
            --  ??? We could use a timestamp somewhere to note we need a
            --  refresh.

            Self.Kernel.Get_Shortcut_Simple
              (Action    => Self.Action.Name.all,
               Key       => Key,
               Mods      => Mods);
            if Key /= 0 then
               Label := Gtk_Accel_Label (Self.Get_Child);
               Label.Set_Accel (Guint (Key), Mods);
            end if;

            Get_Style_Context (Self).Remove_Class ("nogpsaction");

         else
            Self.Set_Tooltip_Markup
              ("Action not found: " & Escape_Text (Self.Name.all));
            Get_Style_Context (Self).Add_Class ("nogpsaction");
         end if;
      end if;
   end Lookup_Action;

   -----------------------------
   -- On_Activate_Action_Item --
   -----------------------------

   procedure On_Activate_Action_Item
     (Item : access Gtk_Menu_Item_Record'Class)
   is
      Self : constant Action_Menu_Item := Action_Menu_Item (Item);
      Context : constant Selection_Context :=
        Get_Current_Context (Self.Kernel);
   begin
      Trace (Me, "Execute action " & Self.Name.all);
      Self.Lookup_Action;

      if Self.Action = null then
         Trace (Me, "Action not found: " & Self.Name.all);

      elsif Context /= No_Context
        and then Filter_Matches (Self.Action.Filter, Context)
      then
         --  Tests expect that using GPS.execute_action("/menu") will
         --  execute in the foreground, so we run Launch_Foreground_Command.
         --  However, when the user is using the GUI, it might make more
         --  sense to be in the background, not sure.

         Launch_Foreground_Command
           (Self.Kernel,
            Create_Proxy
              (Self.Action.Command,
               (null, Context, False, No_File, null, null, 1, 0)),
            Destroy_On_Exit => True);

      elsif Get_Error_Message (Self.Action.Filter) /= "" then
         Insert (Self.Kernel, Get_Error_Message (Self.Action.Filter),
                 Mode => Error);
      else
         Insert (Self.Kernel,
                 -"Invalid context for this action", Mode => Error);
      end if;
   end On_Activate_Action_Item;

   ----------------------------
   -- On_Destroy_Action_Item --
   ----------------------------

   procedure On_Destroy_Action_Item (Item : access Gtk_Widget_Record'Class) is
      Self : constant Action_Menu_Item := Action_Menu_Item (Item);
   begin
      Free (Self.Name);
      --  Do not free Self.Action, belongs to the kernel
   end On_Destroy_Action_Item;

   ------------------------
   -- On_Map_Action_Item --
   ------------------------

   procedure On_Map_Action_Item (Item : access GObject_Record'Class) is
      Self : constant Action_Menu_Item := Action_Menu_Item (Item);
   begin
      Self.Lookup_Action;

      if Self.Action = null then
         if Self.Optional then
            Self.Hide;
         else
            Self.Set_Sensitive (False);
         end if;
      elsif Self.Action.Filter = null then
         Self.Set_Sensitive (True);
      else
         Map_Menu (Self, Interactive_Action'
                     (Kernel  => Self.Kernel,
                      Command => Self.Action.Command,
                      Filter  => Self.Action.Filter));
      end if;
   end On_Map_Action_Item;

   -------------------
   -- Register_Menu --
   -------------------

   procedure Register_Menu
     (Kernel     : not null access Kernel_Handle_Record'Class;
      Path       : String;
      Action     : String;
      Ref_Item   : String := "";
      Add_Before : Boolean := True)
   is
      Ignored : Gtk_Menu_Item;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Register_Menu (Kernel, Path, Action, Ref_Item, Add_Before);
   end Register_Menu;

   -------------------
   -- Register_Menu --
   -------------------

   function Register_Menu
     (Kernel     : not null access Kernel_Handle_Record'Class;
      Path       : String;
      Action     : String;
      Ref_Item   : String := "";
      Add_Before : Boolean := True;
      Optional   : Boolean := False) return Gtk.Menu_Item.Gtk_Menu_Item
   is
      Self : Action_Menu_Item;
      Full_Path : constant String := Cleanup ('/' & Path);
      Accel_Path  : constant String := "<gps>" & Full_Path;
      Item : Gtk_Menu_Item;
   begin
      Item := Find_Menu_Item (Kernel, Path);
      if Item /= null then
         Trace (Me, "Menu registered twice: " & Path);
         return Item;
      end if;

      --  Create the menu item
      Self := new Action_Menu_Item_Record;
      Gtk.Image_Menu_Item.Initialize_With_Mnemonic
        (Self, Label => Base_Name (Path));
      Self.Name := new String'(Action);
      Self.Kernel := Kernel_Handle (Kernel);
      Self.Optional := Optional;
      Self.Set_Accel_Path (Accel_Path);
      Get_Style_Context (Self).Add_Class ("gpsaction");

      --  Add it to the menubar
      Register_Menu (Kernel, Dir_Name (Path), Self, Ref_Item, Add_Before);

      --  And now setup the dynamic behavior

      Self.Get_Toplevel.On_Map (On_Map_Action_Item'Access, Self);
      Self.On_Activate (On_Activate_Action_Item'Access);
      Self.On_Destroy (On_Destroy_Action_Item'Access);
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
      Command   : Menu_Command;
      Full_Path : constant String := "/Window/" & Item_Name;
   begin
      Command := Create_Command_For_Menu (Kernel, Full_Path);

      Register_Action
        (Kernel      => Kernel,
         Name        => Full_Path,
         Command     => Command,
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

   ----------------
   -- Unmap_Menu --
   ----------------

   procedure Unmap_Menu (Menu : access Gtk_Widget_Record'Class) is
   begin
      Trace (Me, "Unmap_Menu");
      Context_User_Data.Remove (Menu, "gpscontext");
   end Unmap_Menu;

   --------------
   -- Map_Menu --
   --------------

   procedure Map_Menu
     (Item    : access GObject_Record'Class;
      Command : Interactive_Action)
   is
      Menu : constant Gtk_Widget := Get_Toplevel (Gtk_Widget (Item));
      Ctxt : Selection_Context;
   begin
      --  Do not use the contextual menu here. Instead, we compute our own
      --  current context for the whole menu tree (not just the item), and
      --  cache it there so that it is only computed once when the user clicks
      --  on a menu.

      Ctxt := Context_User_Data.Get (Menu, "gpscontext", No_Context);
      if Ctxt = No_Context then
         Trace (Me, "Map_Menu: context does not exist yet, creating it");
         Ctxt := Get_Current_Context (Command.Kernel);
         Context_User_Data.Set (Menu, Ctxt, "gpscontext");

         --  Make sure the Unmap signal is set only once for this menu, for
         --  efficiency.
         if Integer_User_Data.Get (Menu, "gpsunmapid", -1) = -1 then
            Widget_Callback.Connect (Menu, Signal_Unmap, Unmap_Menu'Access);
            Integer_User_Data.Set (Menu, 1, "gpsunmapid");
         end if;
      end if;

      Set_Sensitive (Gtk_Widget (Item), Filter_Matches (Command.Filter, Ctxt));
   end Map_Menu;

   -----------------------
   -- Menu_Button_Press --
   -----------------------

   procedure Menu_Button_Press
     (Widget : access GObject_Record'Class;
      Data   : Menu_Factory_User_Data)
   is
      pragma Unreferenced (Widget);

      procedure Remove_Item
        (Item : not null access Gtk.Widget.Gtk_Widget_Record'Class);
      --  Remove one item from Data.Menu

      -----------------
      -- Remove_Item --
      -----------------

      procedure Remove_Item
        (Item : not null access Gtk.Widget.Gtk_Widget_Record'Class) is
      begin
         Remove (Data.Menu, Item);
      end Remove_Item;

   begin
      --  Remove all items in the menu
      Ref (Data.Menu);
      Forall (Data.Menu, Remove_Item'Unrestricted_Access);

      --  Override the previous value.
--        Trace (Me, "Menu_Button_Press: Setting Last_Context_For_Contextual");
--        Data.Kernel.Last_Context_For_Contextual :=
--          Get_Current_Context (Data.Kernel);

      Data.Factory
        (Data.Kernel, Data.Kernel.Last_Context_For_Contextual, Data.Menu);
      Show_All (Data.Menu);

   exception
      when E : others => Trace (Me, E);
   end Menu_Button_Press;

   ---------------------------
   -- Register_Dynamic_Menu --
   ---------------------------

   procedure Register_Dynamic_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Text        : String;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Factory     : Dynamic_Menu_Factory)
   is
      Item, Parent, Pred : Gtk_Menu_Item;
      Image              : Gtk_Image_Menu_Item;
      Pix                : Gtk_Image;
      Menu, Parent_Menu  : Gtk_Menu;
      Index              : Gint;

   begin
      if Stock_Image = "" then
         Gtk_New_With_Mnemonic (Item, Text);
      else
         Gtk_New_With_Mnemonic (Image, Text);
         Gtk_New (Pix, Stock_Image, Icon_Size_Menu);
         Set_Image (Image, Pix);
         Item := Gtk_Menu_Item (Image);
      end if;

      Gtk_New (Menu);
      Set_Submenu (Item, Menu);

      Parent := Find_Or_Create_Menu_Tree
        (Menu_Bar     => GPS_Window (Kernel.Main_Window).Menu_Bar,
         Menu         => null,
         Path         => Format (Parent_Path),
         Accelerators => Get_Default_Accelerators (Kernel),
         Add_Before   => Add_Before,
         Ref_Item     => Ref_Item,
         Allow_Create => True);

      if Parent = null then
         Trace (Me, "Register_Dynamic_Menu: Parent menu not found for " &
                Parent_Path);
         Parent_Menu := null;
      else
         Parent_Menu := Gtk_Menu (Get_Submenu (Parent));
         if Parent_Menu = null then
            Gtk_New (Parent_Menu);
            Set_Submenu (Parent, Parent_Menu);
         end if;
      end if;

      Find_Menu_Item_By_Name
        (GPS_Window (Kernel.Main_Window).Menu_Bar,
         Parent_Menu, Ref_Item, Pred, Index, False);
      Add_Menu (Parent     => Parent_Menu,
                Menu_Bar   => GPS_Window (Kernel.Main_Window).Menu_Bar,
                Item       => Item,
                Index      => Index,
                Add_Before => Add_Before);
      Show_All (Item);

      if Factory /= null then
         Menu_Factory_Callback.Connect
           (Get_Toplevel (Item), Signal_Map,
            Menu_Factory_Callback.To_Marshaller (Menu_Button_Press'Access),
            User_Data => (Kernel_Handle (Kernel), Factory, Menu));
      end if;
   end Register_Dynamic_Menu;

   ---------------------
   -- Register_Button --
   ---------------------

   procedure Register_Button
     (Kernel  : access Kernel_Handle_Record'Class;
      Text    : String;
      Command : Interactive_Command_Access := null;
      Image   : Gtk.Image.Gtk_Image := null;
      Tooltip : String := "")
   is
      Button  : Gtk_Tool_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
   begin
      if Image = null then
         Gtk_New (Button, null, Text);
         --  Set_Relief (Button, Relief_None);
      else
         Gtk_New (Button, Gtk_Widget (Image), Text);
      end if;

      Set_Homogeneous (Button, False);

      Insert (Toolbar, Button,
              Get_Toolbar_Separator_Position (Kernel, Before_Build));

      if Tooltip /= "" then
         Set_Tooltip_Text (Button, Tooltip);
      end if;

      Show_All (Button);

      if Command /= null then
         Register_Perma_Command (Kernel, Command);
      end if;

      Command_Callback.Connect
        (Button, Signal_Clicked, Execute_Command'Access,
         User_Data   => (Kernel_Handle (Kernel), Command, null));
   end Register_Button;

   ---------------------
   -- Register_Button --
   ---------------------

   procedure Register_Button
     (Kernel   : access Kernel_Handle_Record'Class;
      Stock_Id : String;
      Command  : Interactive_Command_Access := null;
      Tooltip  : String := "")
   is
      Button : Gtk_Tool_Button;
   begin
      Gtk_New_From_Stock (Button, Stock_Id);
      if Tooltip /= "" then
         Set_Tooltip_Text (Button, Tooltip);
      end if;
      Insert (Get_Toolbar (Kernel), Button,
              Get_Toolbar_Separator_Position (Kernel, Before_Build) - 1);

      Show_All (Button);

      if Command /= null then
         Register_Perma_Command (Kernel, Command);
      end if;

      Command_Callback.Connect
        (Button, Signal_Clicked, Execute_Command'Access,
         User_Data   => (Kernel_Handle (Kernel), Command, null));
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
                     Open_File_Editor (Kernel, File, New_File => False);
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
      Filter : Action_Filter;

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
      Filter            : GPS.Kernel.Action_Filter := null;
      Enable_Filter     : GPS.Kernel.Action_Filter := null;
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
      Filter            : GPS.Kernel.Action_Filter := null;
      Enable_Filter     : GPS.Kernel.Action_Filter := null;
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
      Filter            : GPS.Kernel.Action_Filter := null;
      Enable_Filter     : GPS.Kernel.Action_Filter := null;
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
     (Button : access Gtk_Widget_Record'Class)
   is
      B      : constant Action_Tool_Button := Action_Tool_Button (Button);
      Action : constant Action_Record_Access :=
        Lookup_Action (B.Kernel, B.Action.all);
   begin
      if Action = null then
         B.Kernel.Insert
           ("Command not found: '" & B.Action.all & "'",
            Mode => Error);

      else
         Execute_Command
           (Button,
            Command => (Kernel  => B.Kernel,
                        Command => Action.Command,
                        Filter  => Action.Filter));
      end if;
   end On_Action_Button_Clicked;

   ------------------------------
   -- On_Action_Button_Destroy --
   ------------------------------

   procedure On_Action_Button_Destroy
     (Button : access Gtk_Widget_Record'Class)
   is
      B : constant Action_Tool_Button := Action_Tool_Button (Button);
   begin
      GNAT.Strings.Free (B.Action);
   end On_Action_Button_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Button   : out Action_Tool_Button;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Stock_Id : String;
      Action   : String)
   is
   begin
      --  ??? Should automatically grey out when the context does not match.
      Button := new Action_Tool_Button_Record;
      Button.Kernel := Kernel_Handle (Kernel);
      Button.Action := new String'(Action);
      Gtk.Tool_Button.Initialize_From_Stock (Button, Stock_Id);
      Widget_Callback.Connect
        (Button, Gtk.Tool_Button.Signal_Clicked,
         On_Action_Button_Clicked'Access);
      Widget_Callback.Connect
        (Button, Gtk.Widget.Signal_Destroy, On_Action_Button_Destroy'Access);
   end Gtk_New;

   ----------------
   -- Add_Button --
   ----------------

   function Add_Button
     (Kernel   : access Kernel_Handle_Record'Class;
      Toolbar  : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Action   : String;
      Position : Glib.Gint := -1) return Gtk.Widget.Gtk_Widget
   is
      Button : Action_Tool_Button;
      A : constant Action_Record_Access :=
        Lookup_Action (Kernel, Action);
   begin
      if A /= null and then A.Stock_Id /= null then
         Gtk_New (Button, Kernel, A.Stock_Id.all, Action);
         Button.Set_Tooltip_Markup (A.Description.all);
      else
         Gtk_New (Button, Kernel, "", Action);
      end if;

      Toolbar.Insert (Button, Pos => Position);
      return Gtk_Widget (Button);
   end Add_Button;

   ----------------
   -- Add_Button --
   ----------------

   procedure Add_Button
     (Kernel   : access Kernel_Handle_Record'Class;
      Toolbar  : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Action   : String;
      Position : Glib.Gint := -1)
   is
      Ignored : Gtk_Widget;
      pragma Unreferenced (Ignored);
   begin
      Ignored := Add_Button (Kernel, Toolbar, Action, Position);
   end Add_Button;

   -------------------
   -- Install_Menus --
   -------------------

   procedure Install_Menus
      (Kernel      : not null access Kernel_Handle_Record'Class;
       Description : GNATCOLL.VFS.Virtual_File)
   is
      procedure Process_Menu_Bar (Menubar_Node : Node);
      --  Process a <menubar> node

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
            --  ??? Inefficient, we do not need to look for existing items
            --  here, since we are creating the menu bar from scratch.
            --  ??? Can we use this even if the action is ""
            Item := Register_Menu
              (Kernel, Parent_Path & "/" & Label, Action,
               Optional => Optional);
         else
            Gtk_New_With_Mnemonic (Item, Label);
            Parent.Append (Item);
         end if;

         N := First_Child (Menu_Node);

         if N /= null then
            Gtk_New (Menu);
            Menu.Set_Accel_Group (Get_Default_Accelerators (Kernel));
            Item.Set_Submenu (Menu);

            while N /= null loop
               if Node_Name (N) = "menu" then
                  Process_Menu (Parent_Path & "/" & Label, Menu, N);
               elsif Node_Name (N) = "separator" then
                  Gtk_New (Sep);
                  Menu.Append (Sep);
               end if;
               N := Next_Sibling (N);
            end loop;
         end if;
      end Process_Menu;

      ----------------------
      -- Process_Menu_Bar --
      ----------------------

      procedure Process_Menu_Bar (Menubar_Node : Node) is
         N    : Node := First_Child (Menubar_Node);
         Menubar : constant Gtk_Menu_Bar :=
           GPS_Window (Kernel.Main_Window).Menu_Bar;
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
      Open (Description.Display_Full_Name, Input);
      Parse (Reader, Input);
      Close (Input);

      Doc := Get_Tree (Reader);
      N := Get_Element (Doc);   --  name is irrelevant
      N := First_Child (N);

      while N /= null loop
         if Node_Name (N) = "menubar" then
            Process_Menu_Bar (N);
         end if;

         N := Next_Sibling (N);
      end loop;

      Free (Reader);
   end Install_Menus;
end GPS.Kernel.Modules.UI;
