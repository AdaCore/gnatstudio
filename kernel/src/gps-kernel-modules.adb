-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Gdk.Dnd;                   use Gdk.Dnd;
with Gdk.Event;
with Gdk.Types;                 use Gdk.Types;

with Glib.Convert;              use Glib.Convert;
with Glib.Module;               use Glib.Module;
with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Glib.Values;               use Glib.Values;

with Gtk.Accel_Map;             use Gtk.Accel_Map;
with Gtk.Button;                use Gtk.Button;
with Gtk.Dnd;                   use Gtk.Dnd;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Image;                 use Gtk.Image;

--  So that this type is correctly converted from C to Ada
with Gtk.Image_Menu_Item;       use Gtk.Image_Menu_Item;
pragma Warnings (Off, Gtk.Image_Menu_Item);

with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Bar;              use Gtk.Menu_Bar;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Selection;             use Gtk.Selection;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Window;                use Gtk.Window;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Interactive;
with File_Utils;                use File_Utils;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Macros;         use GPS.Kernel.Macros;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Main_Window;           use GPS.Main_Window;
with GUI_Utils;                 use GUI_Utils;
with Generic_List;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with System;                    use System;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body GPS.Kernel.Modules is

   Me : constant Debug_Handle := Create ("GPS.Kernel.Modules", Off);

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

   type Contextual_Menu_Type is (Type_Command,
                                 Type_Action,
                                 Type_Submenu,
                                 Type_Separator);
   --  The type of the contextual menu

   type Contextual_Menu_Record;
   type Contextual_Menu_Access is access all Contextual_Menu_Record;
   type Contextual_Menu_Record
     (Menu_Type : Contextual_Menu_Type := Type_Separator)
      is record
         Name           : GNAT.OS_Lib.String_Access;
         Label          : Contextual_Menu_Label_Creator;
         Pix            : GNAT.OS_Lib.String_Access;
         Next           : Contextual_Menu_Access;
         Group          : Natural;
         Visible        : Boolean := True;
         Sensitive      : Boolean := True;
         Filter_Matched : Boolean;
         --  Only valid while computing a contextual menu

         case Menu_Type is
            when Type_Command =>
               Command       : Commands.Interactive.Interactive_Command_Access;
               Filter        : GPS.Kernel.Action_Filter;

            when Type_Action =>
               Action           : Action_Record_Access;

            when Type_Submenu =>
               Submenu          : Submenu_Factory;
               Submenu_Filter   : GPS.Kernel.Action_Filter;

            when Type_Separator =>
               Ref_Item         : Contextual_Menu_Access;
               Separator_Filter : GPS.Kernel.Action_Filter;
         end case;
      end record;
   --  A contextual menu entry declared by a user or GPS itself internally

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
   --  unrefed

   type Contextual_Label_Parameters is new Contextual_Menu_Label_Creator_Record
      with record
      Label  : GNAT.OS_Lib.String_Access;
      Custom : Custom_Expansion;
      Filter : Macro_Filter;
      end record;
   type Contextual_Label_Param is access Contextual_Label_Parameters'Class;
   function Get_Label
     (Creator : access Contextual_Label_Parameters;
      Context : access Selection_Context'Class) return String;
   --  Substitute %p, %f,... in the title to create a suitable contextual menu
   --  title

   package Kernel_Contextuals is new GUI_Utils.User_Contextual_Menus
     (Contextual_Menu_User_Data);

   procedure Contextual_Action
     (Kernel : access GObject_Record'Class; Action : Contextual_Menu_Access);
   --  Execute action, in the context of a contextual menu

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu;
   --  Create a contextual menu as a result of a mouse event
   --  Return null if no menu was created.

   procedure Destroy_Contextual_Menu
     (User : Contextual_Menu_User_Data; Menu : Gtk_Menu);
   --  Destroy the contextual menu that was created before

   type Interactive_Action is record
      Kernel  : Kernel_Handle;
      Command : Interactive_Command_Access;
      Filter  : Action_Filter;
   end record;

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Interactive_Action);
   --  Execute a single command.

   procedure Menu_Button_Press
     (Widget  : access GObject_Record'Class;
      Data    : Menu_Factory_User_Data);
   --  Create a menu using the data in Factory.

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

   function Find_Contextual_Menu_By_Name
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Contextual_Menu_Access;
   --  Find a contextual menu by name

   procedure Map_Menu
     (Item : access GObject_Record'Class;
      Command : Interactive_Action);
   --  Called when a registered menu is displayed, so that we can check whether
   --  it should be made sensitive or not

   procedure Unmap_Menu
     (Item    : access GObject_Record'Class;
      Command : Interactive_Action);
   --  Called when a registered menu is hidden

   type Module_List_Access is access Module_List.List;
   function Convert is new Ada.Unchecked_Conversion
     (Module_List_Access, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Module_List_Access);

   ------------------
   -- Menu_Handler --
   ------------------

   procedure Menu_Handler
     (Module  : access Module_ID_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Module, Object, Context, Menu);
   begin
      null;
   end Menu_Handler;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   function Default_Context_Factory
     (Module : access Module_ID_Record;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Module, Child);
   begin
      return null;
   end Default_Context_Factory;

   -------------------
   -- Save_Function --
   -------------------

   function Save_Function
     (Module : access Module_ID_Record;
      Child  : Gtk.Widget.Gtk_Widget;
      Mode   : Save_Function_Mode) return Boolean
   is
      pragma Unreferenced (Module, Child, Mode);
   begin
      return False;
   end Save_Function;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   function Tooltip_Handler
     (Module  : access Module_ID_Record;
      Context : access Selection_Context'Class) return Gdk.Gdk_Pixmap
   is
      pragma Unreferenced (Module, Context);
   begin
      return null;
   end Tooltip_Handler;

   ----------------------
   -- Bookmark_Handler --
   ----------------------

   function Bookmark_Handler
     (Module : access Module_ID_Record;
      Load   : Xml_Int.Node_Ptr := null) return Location_Marker
   is
      pragma Unreferenced (Module, Load);
   begin
      return null;
   end Bookmark_Handler;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Module_ID_Record;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Module, File, Node, Level);
   begin
      null;
   end Customize;

   ----------
   -- Free --
   ----------

   procedure Free (Module : in out Module_ID) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_ID_Record'Class, Module_ID);
   begin
      Destroy (Module.all);
      Unchecked_Free (Module);
   end Free;

   ----------------------
   -- Get_Focus_Widget --
   ----------------------

   function Get_Focus_Widget return Gtk_Widget;

   function Get_Focus_Widget return Gtk_Widget is
      use Widget_List;
      List : Widget_List.Glist := List_Toplevels;
      L    : Widget_List.Glist;
      W    : Gtk_Widget;
   begin
      L := First (List);
      while L /= Null_List loop
         W := Get_Data (L);
         if Get_Property (W, Has_Toplevel_Focus_Property) then
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
      if Gtk.Object.In_Destruction_Is_Set (Get_MDI (Kernel)) then
         return null;
      end if;

      W := Get_Focus_Widget;

      if W = null
        or else Gtk.Object.In_Destruction_Is_Set (W)
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
         --  popup dialog, and therefore there is no module

         while W /= null loop
            if W.all in MDI_Child_Record'Class then
               C := MDI_Child (W);
               exit;
            end if;
            W := Get_Parent (W);
         end loop;
      end if;

      if C = null
        or else C /= Get_Focus_Child (Get_MDI (Kernel))
      then
         return null;
      else
         return Get_Module_From_Child (C);
      end if;
   end Get_Current_Module;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel (ID : Module_ID_Record'Class) return Kernel_Handle is
   begin
      return ID.Kernel;
   end Get_Kernel;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Module : Module_ID) return String is
   begin
      return Module.Name.all;
   end Get_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out Module_ID_Record) is
      pragma Unreferenced (Id);
   begin
      null;
   end Destroy;

   -------------------
   -- Create_Marker --
   -------------------

   function Create_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      Load   : Xml_Int.Node_Ptr := null) return Location_Marker
   is
      use type Module_List.List_Node;
      use type Xml_Int.Node_Ptr;
      Current : Module_List.List_Node :=
                  Module_List.First (List_Of_Modules (Kernel));
      Module  : Module_ID;
      Marker  : Location_Marker;
   begin
      if Load = null then
         Module := Get_Current_Module (Kernel);
         if Module /= null then
            return Bookmark_Handler (Module, null);
         end if;

      else
         while Current /= Module_List.Null_Node loop
            Module := Module_List.Data (Current);
            Marker := Bookmark_Handler (Module, Load);
            if Marker /= null then
               return Marker;
            end if;
            Current := Module_List.Next (Current);
         end loop;
      end if;
      return null;
   end Create_Marker;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Creator : access Contextual_Label_Parameters;
      Context : access Selection_Context'Class) return String
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
            if Creator.Custom /= null then
               declare
                  Result : constant String := Creator.Custom (Context);
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
               Tmp : constant String := GPS.Kernel.Macros.Substitute
                 (Param, Selection_Context_Access (Context), Quoted);
            begin
               Has_Error := Tmp = "";
               return Tmp;
            end;
         end if;

         Has_Error := True;
         return "";
      end Substitution;

   begin
      if Filter_Matches (Action_Filter (Creator.Filter), Context) then
         declare
            Tmp : constant String := Substitute
              (Creator.Label.all,
               Substitution_Char => GPS.Kernel.Macros.Special_Character,
               Callback          => Substitution'Unrestricted_Access,
               Recursive         => False);
         begin
            if Has_Error then
               return "";
            else
               return Tmp;
            end if;
         end;
      else
         return "";
      end if;
   end Get_Label;

   ---------------------
   -- Compute_Tooltip --
   ---------------------

   procedure Compute_Tooltip
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context_Access;
      Pixmap  : out Gdk.Gdk_Pixmap)
   is
      use type Module_List.List_Node;
      use type Gdk.Gdk_Pixmap;
      Current : Module_List.List_Node :=
                  Module_List.First (List_Of_Modules (Kernel));
      Module  : Module_ID;
   begin
      Pixmap := null;

      while Current /= Module_List.Null_Node loop
         Module := Module_List.Data (Current);
         if Module /= null then
            Pixmap := Tooltip_Handler (Module, Context);
            if Pixmap /= null then
               return;
            end if;
         end if;

         Current := Module_List.Next (Current);
      end loop;
   end Compute_Tooltip;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Module      : access Module_ID_Record;
      Kernel      : access Kernel_Handle_Record'Class;
      Module_Name : String;
      Priority    : Module_Priority := Default_Priority)
   is
      List : Module_List_Access := Convert (Kernel.Modules_List);
   begin
      Module.Name     := new String'(Module_Name);
      Module.Priority := Priority;
      Module.Kernel   := Kernel_Handle (Kernel);

      if List = null then
         List := new Module_List.List;
         Kernel.Modules_List := Convert (List);
      end if;

      Module_List.Append (List.all, Module_ID (Module));
   end Register_Module;

   -----------------------------
   -- Dynamic_Register_Module --
   -----------------------------

   procedure Dynamic_Register_Module
     (Kernel      : access Kernel_Handle_Record'Class;
      Shared_Lib  : String;
      Module_Name : String;
      Success     : out Boolean)
   is
      type Register_Module_Access is access procedure
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

      type Init_Proc is access procedure;

      Dyn_Module   : G_Module;
      Dyn_Register : Register_Module_Access;
      Init         : Init_Proc;

      procedure Get_Symbol is new
        Generic_Module_Symbol (Register_Module_Access);

      procedure Get_Symbol is new
        Generic_Module_Symbol (Init_Proc);

   begin
      Dyn_Module := Module_Open (Module_Build_Path ("", Shared_Lib));

      if Dyn_Module = null then
         Dyn_Module := Module_Open (Shared_Lib);
      end if;

      if Dyn_Module = null then
         Trace (Me, "Couldn't open shared lib: " & Shared_Lib);
         Success := False;
      else
         Get_Symbol (Dyn_Module, Module_Name & "_init", Init, Success);

         if Success then
            Init.all;

            Get_Symbol
              (Dyn_Module, Module_Name & "__register_module",
               Dyn_Register, Success);

            if Success then
               Trace (Me, "Registering module: " & Module_Name);
               Dyn_Register (Kernel);
            else
               Trace (Me, "Couldn't find register_module symbol");
            end if;

         else
            Trace (Me, "Couldn't find _init symbol");
         end if;
      end if;
   end Dynamic_Register_Module;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority
     (ID : access Module_ID_Record'Class) return Module_Priority is
   begin
      return ID.Priority;
   end Get_Priority;

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name (ID : access Module_ID_Record'Class) return String is
   begin
      return ID.Name.all;
   end Module_Name;

   -----------------------
   -- Contextual_Action --
   -----------------------

   procedure Contextual_Action
     (Kernel : access GObject_Record'Class;
      Action : Contextual_Menu_Access)
   is
      C       : Command_Access;
      Context : Interactive_Command_Context;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);
      Context.Context := Kernel_Handle (Kernel).Last_Context_For_Contextual;
      Context.Event   := Kernel_Handle (Kernel).Last_Event_For_Contextual;
      --   Get_Current_Event;

      --  This ensure that Context.Context will not be freed while it is needed
      --  and also that Kernel.Last_Context_For_Contextual is not reset to
      --  null while the action executes
      Ref (Context.Context);

      case Action.Menu_Type is
         when Type_Action =>
            C := Create_Proxy (Action.Action.Command, Context);
         when Type_Command =>
            C := Create_Proxy (Action.Command, Context);
         when others =>
            null;
      end case;

      Launch_Background_Command
        (Kernel          => Kernel_Handle (Kernel),
         Command         => C,
         Active          => True,
         Show_Bar        => True,
         Destroy_On_Exit => True);
      Pop_State (Kernel_Handle (Kernel));
   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle, "Unexpected exception while executing "
                & Action.Name.all & " " & Exception_Information (E));
   end Contextual_Action;

   -------------------------------
   -- Contextual_Menu_Destroyed --
   -------------------------------

   procedure Contextual_Menu_Destroyed
     (Data : System.Address;
      Object : System.Address)
   is
      pragma Unreferenced (Object);
      Kernel : constant Kernel_Handle := Convert (Data);
   begin
      if Kernel.Last_Context_For_Contextual /= null then
         Trace (Me, "Running Hook " & Contextual_Menu_Close_Hook);
         Run_Hook (Kernel, Contextual_Menu_Close_Hook);
         Trace (Me, "Destroying contextual menu and its context");

         --  This context might still be needed by the action itself, but a ref
         --  is already created from Contextual_Action and ensures this isn't
         --  reset to null in that case
         Unref (Kernel.Last_Context_For_Contextual);
      end if;
   end Contextual_Menu_Destroyed;

   ----------------------------
   -- Create_Contextual_Menu --
   ----------------------------

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu
   is
      use type Gtk.Widget.Widget_List.Glist;

      function Menu_Is_Visible
        (C : Contextual_Menu_Access;
         Context : Selection_Context_Access) return Boolean;
      --  Whether the menu C should be made visible

      procedure Create_Item
        (C         : Contextual_Menu_Access;
         Context   : Selection_Context_Access;
         Item      : out Gtk_Menu_Item;
         Full_Name : out GNAT.OS_Lib.String_Access);
      --  Create the menu item to use when displaying C.
      --  Full_Name is the label of the menu

      function Label_Name
        (C       : Contextual_Menu_Access;
         Context : Selection_Context_Access) return String;
      --  Return the name of the label for C, including parent path

      function Has_Explicit_Parent
        (C       : Contextual_Menu_Access;
         Context : Selection_Context_Access) return Boolean;
      --  Return True if the parent menu of C was explicitly registered by the
      --  user. In this case, C will be created only when that parent menu is
      --  created, not before.

      ---------------------
      -- Menu_Is_Visible --
      ---------------------

      function Menu_Is_Visible
        (C : Contextual_Menu_Access;
         Context : Selection_Context_Access) return Boolean is
      begin
         if not C.Visible then
            return False;
         end if;

         case C.Menu_Type is
            when Type_Separator =>
               --  A separator is visible only:
               --     - Its own filter matches
               --     - Or if it has no own filter, then only if the reference
               --       item is also visible
               --     - Always visible if there is no reference item
               --  and then only if it isn't the last entry the contextual
               --  menu

               if C.Separator_Filter /= null then
                  return Filter_Matches (C.Separator_Filter, Context);
               else
                  return C.Ref_Item = null
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
         Context   : Selection_Context_Access;
         Item      : out Gtk_Menu_Item;
         Full_Name : out GNAT.OS_Lib.String_Access)
      is
         use type GNAT.OS_Lib.String_Access;
         Image : Gtk_Image_Menu_Item;
         Pix   : Gtk_Image;
         Menu  : Gtk_Menu;
      begin
         Full_Name := new String'(Label_Name (C, Context));

         case C.Menu_Type is
            when Type_Submenu =>
               declare
                  C2    : Contextual_Menu_Access;
                  Full2 : GNAT.OS_Lib.String_Access;
                  Item  : Gtk_Menu_Item;
               begin
                  Gtk_New (Menu);

                  if C.Submenu /= null then
                     C.Submenu
                       (Object  => User.Object,
                        Context => Context,
                        Menu    => Menu);
                  end if;

                  --  Add all contextual menus that are children of C
                  C2 := Convert (User.Kernel.Contextual);
                  while C2 /= null loop
                     if C2.Filter_Matched then
                        if Dir_Name ('/' & Label_Name (C2, Context)) =
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

               if Children (Menu) = Gtk.Widget.Widget_List.Null_List then
                  Destroy (Menu);
                  Item := null;
               else
                  Gtk_New (Item, Base_Name (Full_Name.all));
                  Set_Submenu (Item, Menu);
               end if;

            when Type_Separator =>
               Gtk_New (Item, "");

            when Type_Action | Type_Command =>
               if Full_Name.all /= "" then
                  if C.Pix = null then
                     Gtk_New (Item, Base_Name (Full_Name.all));
                  else
                     Gtk_New (Image, Base_Name (Full_Name.all));
                     Gtk_New (Pix,
                              Stock_Id => C.Pix.all,
                              Size     => Icon_Size_Menu);
                     Set_Image (Image, Pix);
                     Item := Gtk_Menu_Item (Image);
                  end if;

                  Action_Callback.Object_Connect
                    (Item, "activate",
                     Contextual_Action'Access,
                     User_Data   => C,
                     Slot_Object => User.Kernel);
               else
                  Item := null;
               end if;
         end case;

         if Item /= null then
            Set_Sensitive (Item, C.Sensitive);
         end if;
      end Create_Item;

      ----------------
      -- Label_Name --
      ----------------

      function Label_Name
        (C       : Contextual_Menu_Access;
         Context : Selection_Context_Access) return String is
      begin
         if C.Label = null then
            return C.Name.all;
         else
            return Get_Label (C.Label, Context);
         end if;
      end Label_Name;

      -------------------------
      -- Has_Explicit_Parent --
      -------------------------

      function Has_Explicit_Parent
        (C       : Contextual_Menu_Access;
         Context : Selection_Context_Access) return Boolean
      is
         Label  : constant String := Label_Name (C, Context);
         Parent : constant String := Dir_Name ('/' & Label);
         C2     : Contextual_Menu_Access;
      begin
         if Parent /= "/" then
            C2 := Convert (User.Kernel.Contextual);
            while C2 /= null loop
               if '/' & Label_Name (C2, Context) & '/' = Parent then
                  return True;
               end if;
               C2 := C2.Next;
            end loop;
         end if;
         return False;
      end Has_Explicit_Parent;

      Context     : Selection_Context_Access;
      Menu        : Gtk_Menu := null;
      Full_Name   : GNAT.OS_Lib.String_Access;
      C           : Contextual_Menu_Access;
      Item        : Gtk_Menu_Item;
      Parent_Item : Gtk_Menu_Item;
      Parent_Menu : Gtk_Menu;
   begin
      --  Create the menu and add all the modules information
      Gtk_New (Menu);

      Push_State (User.Kernel, Busy);
      Context := User.Context_Func
        (Kernel       => User.Kernel,
         Event_Widget => User.Event_Widget,
         Object       => User.Object,
         Event        => Event,
         Menu         => Menu);

      if Context = null then
         Context := new Selection_Context;
      end if;

      --  Override the previous value. No Ref is taken explicitly, so we do not
      --  need to Unref either. This field is automatically reset to null when
      --  the last holder of a ref calls Unref.
      Trace (Me, "Set Last_Context_For_Contextual");
      User.Kernel.Last_Context_For_Contextual := Context;

      if User.Kernel.Last_Event_For_Contextual /= null then
         Free (User.Kernel.Last_Event_For_Contextual);
      end if;
      Deep_Copy (From => Event, To => User.Kernel.Last_Event_For_Contextual);

      Set_Context_Information
        (Context,
         Kernel  => User.Kernel,
         Creator => Abstract_Module_ID (User.ID));

      Run_Hook (User.Kernel, Contextual_Menu_Open_Hook);

      --  Compute what items should be made visible, except for separators
      --  for the moment

      C := Convert (User.Kernel.Contextual);
      while C /= null loop
         if C.Menu_Type /= Type_Separator then
            C.Filter_Matched := Menu_Is_Visible (C, Context);
         end if;
         C := C.Next;
      end loop;

      --  Same, but only for separators now, since their visibility might
      --  depend on the visibility of other items

      C := Convert (User.Kernel.Contextual);
      while C /= null loop
         if C.Menu_Type = Type_Separator then
            C.Filter_Matched := Menu_Is_Visible (C, Context);
         end if;
         C := C.Next;
      end loop;

      C := Convert (User.Kernel.Contextual);
      while C /= null loop
         if C.Filter_Matched
           and then not Has_Explicit_Parent (C, Context)
         then
            Create_Item (C, Context, Item, Full_Name);

            if Item /= null then
               Parent_Item := Find_Or_Create_Menu_Tree
                 (Menu_Bar      => null,
                  Menu          => Menu,
                  Path          => Dir_Name ('/' & Full_Name.all),
                  Accelerators  => Get_Default_Accelerators (User.Kernel),
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
      --  time a contextual menu is displayed

      Pop_State (User.Kernel);

      --  If the menu is empty, destroy it.

      if Children (Menu) = Gtk.Widget.Widget_List.Null_List then
         Destroy (Menu);
         Menu := null;
      end if;

      if Menu /= null then
         Weak_Ref (Menu, Contextual_Menu_Destroyed'Access,
                   Data => User.Kernel.all'Address);
      end if;

      return Menu;
   exception
      when E : others =>
         Trace (Exception_Handle, "Unhandled exception "
                & Exception_Information (E));
         return null;
   end Create_Contextual_Menu;

   -----------------------------
   -- Destroy_Contextual_Menu --
   -----------------------------

   procedure Destroy_Contextual_Menu
     (User : Contextual_Menu_User_Data;
      Menu : Gtk_Menu)
   is
      pragma Unreferenced (User);
   begin
      Destroy (Menu);
   end Destroy_Contextual_Menu;

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
         Menu_Create  => Create_Contextual_Menu'Access,
         Menu_Destroy => Destroy_Contextual_Menu'Access);
   end Register_Contextual_Menu;

   ------------------------------
   -- Add_Default_Desktop_Item --
   ------------------------------

   procedure Add_Default_Desktop_Item
     (Kernel   : access Kernel_Handle_Record'Class;
      Tag_Name : String;
      Position : Gtkada.MDI.Child_Position := Gtkada.MDI.Position_Default;
      Focus    : Boolean := False;
      Raised   : Boolean := False)
   is
      N        : Glib.Xml_Int.Node_Ptr;
   begin
      N := new Glib.Xml_Int.Node;
      N.Tag := new String'(Tag_Name);
      Add_To_Tree (Kernel.Default_Desktop, N, Position, Focus, Raised);
   end Add_Default_Desktop_Item;

   --------------------
   -- Find_Menu_Item --
   --------------------

   function Find_Menu_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item is
   begin
      return Find_Or_Create_Menu_Tree
        (Menu_Bar     => GPS_Window (Kernel.Main_Window).Menu_Bar,
         Menu         => null,
         Path         => Path,
         Accelerators => Get_Default_Accelerators (Kernel),
         Allow_Create => False);
   end Find_Menu_Item;

   -------------------
   -- Register_Menu --
   -------------------

   procedure Register_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Parent_Path : String;
      Item        : Gtk.Menu_Item.Gtk_Menu_Item := null;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True)
   is
      Parent, Pred : Gtk_Menu_Item;
      Parent_Menu  : Gtk_Menu;
      Index        : Gint;

   begin
      Parent := Find_Or_Create_Menu_Tree
        (Menu_Bar     => GPS_Window (Kernel.Main_Window).Menu_Bar,
         Menu         => null,
         Path         => Name_As_Directory (Parent_Path, UNIX),
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
      Action      : Action_Record_Access := null;
      Filter      : Action_Filter  := null)
   is
      Item  : Gtk_Menu_Item;
      pragma Unreferenced (Item);
   begin
      Item := Register_Menu
        (Kernel, Parent_Path, Text, Stock_Image, Callback, Command,
         Accel_Key, Accel_Mods, Ref_Item, Add_Before, Sensitive, Action,
         Filter);
   end Register_Menu;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Interactive_Action)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel_Handle (Widget));
   begin
      if Context /= null and then Filter_Matches (Command.Filter, Context) then
         Ref (Context);
         Launch_Background_Command
           (Kernel_Handle (Widget),
            Create_Proxy
              (Command.Command, (null, Context, null, null, null)),
            Destroy_On_Exit => False,
            Active => True, Show_Bar => True, Queue_Id => "");
      elsif Get_Error_Message (Command.Filter) /= "" then
         Insert (Kernel_Handle (Widget), Get_Error_Message (Command.Filter),
                 Mode => Error);
      else
         Insert (Kernel_Handle (Widget),
                 -"Invalid context for this action", Mode => Error);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Execute_Command;

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
      Action      : Action_Record_Access := null;
      Filter      : Action_Filter  := null) return Gtk_Menu_Item
   is
      use type Kernel_Callback.Marshallers.Void_Marshaller.Handler;
      function Cleanup (Path : String) return String;
      --  Remove duplicate // in Path

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

      Item        : Gtk_Menu_Item;
      Image       : Gtk_Image_Menu_Item;
      Pix         : Gtk_Image;
      Accel_Path  : constant String :=
                      Cleanup ("<gps>/" & Parent_Path & '/' & Text);
      Menu_Filter : Action_Filter := Filter;

   begin
      if Stock_Image = "" then
         Gtk_New_With_Mnemonic (Item, Text);
      else
         Gtk_New_With_Mnemonic (Image, Text);
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
           (Item, "activate",
            Kernel_Callback.To_Marshaller (Callback), Kernel_Handle (Kernel));
      end if;

      if Command /= null then
         Command_Callback.Object_Connect
           (Item, "activate", Execute_Command'Access,
            Slot_Object => Kernel_Handle (Kernel),
            User_Data   => (Kernel_Handle (Kernel),
                            Command,
                            Filter));
      end if;

      if Action /= null then
         Command_Callback.Object_Connect
           (Item, "activate", Execute_Command'Access,
            Slot_Object => Kernel_Handle (Kernel),
            User_Data   => (Kernel_Handle (Kernel),
                            Action.Command,
                            Action.Filter));
         if Action.Filter /= null then
            if Menu_Filter = null then
               Menu_Filter := Action.Filter;
            else
               Menu_Filter := Menu_Filter and Action.Filter;
            end if;
         end if;
      end if;

      if Menu_Filter /= null then
         Command_Callback.Object_Connect
           (Get_Toplevel (Item), "map", Map_Menu'Access,
            Slot_Object => Item,
            User_Data   => (Kernel_Handle (Kernel),
                            null,
                            Menu_Filter));
         Command_Callback.Object_Connect
           (Get_Toplevel (Item), "unmap", Unmap_Menu'Access,
            Slot_Object => Item,
            User_Data   => (Kernel_Handle (Kernel),
                            null,
                            null));
      end if;

      return Item;
   end Register_Menu;

   ----------------
   -- Unmap_Menu --
   ----------------

   procedure Unmap_Menu
     (Item    : access GObject_Record'Class;
      Command : Interactive_Action)
   is
      pragma Unreferenced (Item);
   begin
      if Command.Kernel.Last_Context_For_Contextual /= null then
         --  This removes the Ref we had taken in Map_Menu for each menu item
         --  that required a filter
         Unref (Command.Kernel.Last_Context_For_Contextual);
      end if;
   end Unmap_Menu;

   --------------
   -- Map_Menu --
   --------------

   procedure Map_Menu
     (Item    : access GObject_Record'Class;
      Command : Interactive_Action)
   is
   begin
      if Command.Kernel.Last_Context_For_Contextual = null then
         Trace (Me, "Creating last_context_for_contextual (Map_Menu)");
         Command.Kernel.Last_Context_For_Contextual :=
           Get_Current_Context (Command.Kernel);
      else
         --  For each call to Map_Menu, there is a call to Unmap_Menu.
         --  Therefore we take as many Refs as Unmap_Menu will be called.
         --  In the case above when we created the context, it already has a
         --  ref_count of 1
         Ref (Command.Kernel.Last_Context_For_Contextual);
      end if;

      Set_Sensitive
        (Gtk_Widget (Item),
         Filter_Matches (Command.Filter,
                         Command.Kernel.Last_Context_For_Contextual));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Map_Menu;

   -----------------------
   -- Menu_Button_Press --
   -----------------------

   procedure Menu_Button_Press
     (Widget  : access GObject_Record'Class;
      Data    : Menu_Factory_User_Data)
   is
      pragma Unreferenced (Widget);

      procedure Remove_Item
        (Item : access Gtk.Widget.Gtk_Widget_Record'Class);
      --  Remove one item from Data.Menu.

      -----------------
      -- Remove_Item --
      -----------------

      procedure Remove_Item
        (Item : access Gtk.Widget.Gtk_Widget_Record'Class) is
      begin
         Remove (Data.Menu, Item);
      end Remove_Item;

   begin
      --  Remove all items in the menu.
      Ref (Data.Menu);
      Forall (Data.Menu, Remove_Item'Unrestricted_Access);

      --  Override the previous value. Since no explicit Ref is taken, we also
      --  do not need to free it.
      Trace (Me, "Menu_Button_Press: Setting Last_Context_For_Contextual");
      Data.Kernel.Last_Context_For_Contextual :=
        Get_Current_Context (Data.Kernel);

      Ref (Data.Kernel.Last_Context_For_Contextual);
      Data.Factory
        (Data.Kernel, Data.Kernel.Last_Context_For_Contextual, Data.Menu);
      Show_All (Data.Menu);
      Unref (Data.Kernel.Last_Context_For_Contextual);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
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
      Item  : Gtk_Menu_Item;
      Image : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
      Menu  : Gtk_Menu;

   begin
      if Stock_Image = "" then
         Gtk_New_With_Mnemonic (Item, Text);
      else
         Gtk_New_With_Mnemonic (Image, Text);
         Gtk_New (Pix, Stock_Image, Icon_Size_Menu);
         Set_Image (Image, Pix);
         Item := Gtk_Menu_Item (Image);
      end if;

      Item := Find_Or_Create_Menu_Tree
        (Menu_Bar     => GPS_Window (Kernel.Main_Window).Menu_Bar,
         Menu         => null,
         Path         => Name_As_Directory (Parent_Path, UNIX),
         Accelerators => Get_Default_Accelerators (Kernel),
         Add_Before   => Add_Before,
         Ref_Item     => Ref_Item,
         Allow_Create => True);

      Gtk_New (Menu);
      Set_Submenu (Item, Menu);

      if Factory /= null then
         Menu_Factory_Callback.Connect
           (Get_Toplevel (Item), "map",
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
      Button  : Gtk_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
   begin
      if Image = null then
         Gtk_New (Button, Text);
         Set_Relief (Button, Relief_None);
         Append_Widget (Toolbar, Button);
      else
         Button := Append_Item
           (Toolbar, Text, Text, Tooltip, Gtk_Widget (Image));
      end if;

      Command_Callback.Object_Connect
        (Button, "clicked", Execute_Command'Access,
         Slot_Object => Kernel_Handle (Kernel),
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
      Button  : Gtk_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
   begin
      Button := Insert_Stock (Toolbar, Stock_Id, Tooltip);

      Command_Callback.Object_Connect
        (Button, "clicked", Execute_Command'Access,
         Slot_Object => Kernel_Handle (Kernel),
         User_Data   => (Kernel_Handle (Kernel), Command, null));
   end Register_Button;

   ------------------
   -- Free_Modules --
   ------------------

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Module_List.List, Module_List_Access);
      List : Module_List_Access := Convert (Kernel.Modules_List);
   begin
      --  Destroy the modules in the reverse order,
      --  otherwise, the scripts module is no longer available for the other
      --  modules, and some modules (e.g. editor) is freed too early.

      if List /= null then
         Module_List.Free (List.all, Reversed => True);
         Unchecked_Free (List);
         Kernel.Modules_List := System.Null_Address;
      end if;
   end Free_Modules;

   ---------------------
   -- List_Of_Modules --
   ---------------------

   function List_Of_Modules
     (Kernel : access Kernel_Handle_Record'Class) return Module_List.List is
   begin
      return Convert (Kernel.Modules_List).all;
   end List_Of_Modules;

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
                  Drag_Context (Get_Proxy (Nth (Args, 1)));
      Data    : constant Selection_Data :=
                  Selection_Data (Get_Proxy (Nth (Args, 4)));
      Time    : constant Guint32 := Guint32 (Get_Uint (Nth (Args, 6)));
      File    : Virtual_File;
      First   : Natural;
      Last    : Natural;

   begin
      if Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
      then
         declare
            Files : constant String := Strip_CR (Get_Data_As_String (Data));
         begin
            First := Files'First;
            Last  := First;

            loop
               exit when First > Files'Last;

               Skip_To_Char (Files, Last, ASCII.LF);

               if First + 7 < Last
                 and then Files (First .. First + 7) = "file:///"
               then
                  File := Create
                    (Locale_To_UTF8 (GNAT.OS_Lib.Normalize_Pathname
                       (Files (First + 8 .. Last - 1))));

                  if Is_Regular_File (File) then
                     if File_Extension (File) = Project_File_Extension then
                        Load_Project (Kernel, Full_Name (File).all);
                     else
                        Open_File_Editor (Kernel, File, New_File => False);
                     end if;
                  end if;
               end if;

               First := Last + 1;
               Last  := First;
            end loop;
         end;

         Gtk.Dnd.Finish (Context, Success => True, Del => False, Time => Time);

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
      C, Previous, Last_Group : Contextual_Menu_Access;
   begin
      Trace (Me, "Register_Contextual_Menu " & Menu.Name.all);
      C := Find_Contextual_Menu_By_Name (Kernel, Menu.Name.all);

      if Menu.Name.all /= "" and then C /= null then
         Trace (Me, "Contextual menu already registered: " & Menu.Name.all);
         GNAT.OS_Lib.Free (C.Name);
         --  ??? Can't free label for now.
         --  Unchecked_Free (C.Label);
         GNAT.OS_Lib.Free (C.Pix);
         Menu.Next := C.Next;
         C.all := Menu.all;
         Previous := Menu;
         Unchecked_Free (Previous);
      else
         if Kernel.Contextual /= System.Null_Address then
            C := Convert (Kernel.Contextual);
            while C /= null
              and then (Ref_Item = "" or else C.Name.all /= Ref_Item)
            loop
               if C.Group > Menu.Group then
                  Last_Group := Previous;
               end if;

               Previous := C;
               C := C.Next;
            end loop;

            if Menu.Menu_Type = Type_Separator then
               Menu.Ref_Item := C;
            end if;

            if Ref_Item /= "" and then C = null then
               Trace (Me, "Ref_Item not found (" & Ref_Item & ") when adding "
                      & Menu.Name.all);
            end if;

            if C = null then
               if Last_Group /= null then
                  Menu.Next       := Last_Group.Next;
                  Last_Group.Next := Menu;
               elsif Previous /= null then
                  Menu.Next     := Previous.Next;
                  Previous.Next := Menu;
               else
                  Menu.Next         := Convert (Kernel.Contextual);
                  Kernel.Contextual := Convert (Menu);
               end if;

            elsif Add_Before and then Ref_Item /= "" then
               if Previous = null then
                  Menu.Next := Convert (Kernel.Contextual);
                  Kernel.Contextual := Convert (Menu);
               else
                  Menu.Next := Previous.Next;
                  Previous.Next := Menu;
               end if;
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
      Name   : String) return Contextual_Menu_Access
   is
      C : Contextual_Menu_Access;
   begin
      if Kernel.Contextual /= System.Null_Address then
         C := Convert (Kernel.Contextual);
         while C /= null loop
            if C.Name.all = Name then
               return C;
            end if;
            C := C.Next;
         end loop;
      end if;
      return null;
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
      Group       : Natural := 0)
   is
      T : Contextual_Label_Param;
      Pix : GNAT.OS_Lib.String_Access;
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
           (Menu_Type        => Type_Separator,
            Name             => new String'(Name),
            Separator_Filter => null,
            Pix              => Pix,
            Next             => null,
            Ref_Item         => null,
            Group            => Group,
            Visible          => True,
            Sensitive        => True,
            Filter_Matched   => False,
            Label            => Contextual_Menu_Label_Creator (T));
      else
         Menu := new Contextual_Menu_Record'
           (Menu_Type      => Type_Action,
            Name           => new String'(Name),
            Action         => Action,
            Pix            => Pix,
            Next           => null,
            Group          => Group,
            Visible        => True,
            Sensitive      => True,
            Filter_Matched => False,
            Label          => Contextual_Menu_Label_Creator (T));
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
      Group       : Natural := 0)
   is
      Pix  : GNAT.OS_Lib.String_Access;
      Menu : Contextual_Menu_Access;
   begin
      if Stock_Image /= "" then
         Pix := new String'(Stock_Image);
      end if;

      if Action = null then
         Menu := new Contextual_Menu_Record'
           (Menu_Type        => Type_Separator,
            Name             => new String'(Name),
            Separator_Filter => null,
            Pix              => Pix,
            Next             => null,
            Group            => Group,
            Ref_Item         => null,
            Visible          => True,
            Sensitive        => True,
            Filter_Matched   => False,
            Label            => Contextual_Menu_Label_Creator (Label));
      else
         Menu := new Contextual_Menu_Record'
           (Menu_Type      => Type_Action,
            Name           => new String'(Name),
            Action         => Action,
            Pix            => Pix,
            Next           => null,
            Group          => Group,
            Visible        => True,
            Sensitive      => True,
            Filter_Matched => False,
            Label          => Contextual_Menu_Label_Creator (Label));
      end if;

      Add_Contextual_Menu (Kernel, Menu, Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Action      : Commands.Interactive.Interactive_Command_Access;
      Filter      : GPS.Kernel.Action_Filter := null;
      Label       : access Contextual_Menu_Label_Creator_Record'Class;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Natural := 0)
   is
      Pix  : GNAT.OS_Lib.String_Access;
      Menu : Contextual_Menu_Access;
   begin
      if Stock_Image /= "" then
         Pix := new String'(Stock_Image);
      end if;

      if Action = null then
         Menu := new Contextual_Menu_Record'
           (Menu_Type        => Type_Separator,
            Name             => new String'(Name),
            Separator_Filter => Filter,
            Pix              => Pix,
            Next             => null,
            Ref_Item         => null,
            Group            => Group,
            Visible          => True,
            Filter_Matched   => False,
            Sensitive        => True,
            Label            => Contextual_Menu_Label_Creator (Label));
      else
         Menu := new Contextual_Menu_Record'
           (Menu_Type      => Type_Command,
            Name           => new String'(Name),
            Command        => Action,
            Filter         => Filter,
            Pix            => Pix,
            Next           => null,
            Group          => Group,
            Visible        => True,
            Filter_Matched => False,
            Sensitive      => True,
            Label          => Contextual_Menu_Label_Creator (Label));
      end if;

      Add_Contextual_Menu (Kernel, Menu, Ref_Item, Add_Before);
   end Register_Contextual_Menu;

   ------------------------------
   -- Register_Contextual_Menu --
   ------------------------------

   procedure Register_Contextual_Menu
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Action      : Commands.Interactive.Interactive_Command_Access := null;
      Filter      : GPS.Kernel.Action_Filter := null;
      Label       : String := "";
      Custom      : Custom_Expansion := null;
      Stock_Image : String := "";
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Group       : Natural := 0)
   is
      T : Contextual_Label_Param;
      Pix : GNAT.OS_Lib.String_Access;
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
           (Menu_Type        => Type_Separator,
            Name             => new String'(Name),
            Separator_Filter => Filter,
            Pix              => Pix,
            Next             => null,
            Ref_Item         => null,
            Visible          => True,
            Group            => Group,
            Sensitive        => True,
            Filter_Matched   => False,
            Label            => Contextual_Menu_Label_Creator (T));
      else
         Menu := new Contextual_Menu_Record'
           (Menu_Type      => Type_Command,
            Name           => new String'(Name),
            Command        => Action,
            Filter         => Filter,
            Pix            => Pix,
            Next           => null,
            Visible        => True,
            Group          => Group,
            Sensitive      => True,
            Filter_Matched => False,
            Label          => Contextual_Menu_Label_Creator (T));
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
      C : Contextual_Menu_Access := Find_Contextual_Menu_By_Name
        (Kernel, Name);
   begin
      if C = null then
         Register_Contextual_Menu
           (Kernel => Kernel,
            Name   => Name,
            Action => null,
            Filter => null,
            Label  => "");
         C := Find_Contextual_Menu_By_Name (Kernel, Name);
      end if;
      C.Visible := Visible;
   end Set_Contextual_Menu_Visible;

   -------------------------------------
   -- Set_Contextual_Menu_Sensitivity --
   -------------------------------------

   procedure Set_Contextual_Menu_Sensitivity
     (Kernel    : access Kernel_Handle_Record'Class;
      Name      : String;
      Sensitive : Boolean)
   is
      C : Contextual_Menu_Access := Find_Contextual_Menu_By_Name
        (Kernel, Name);
   begin
      if C = null then
         Register_Contextual_Menu
           (Kernel => Kernel,
            Name   => Name,
            Action => null,
            Filter => null,
            Label  => "");
         C := Find_Contextual_Menu_By_Name (Kernel, Name);
      end if;
      C.Sensitive := Sensitive;
   end Set_Contextual_Menu_Sensitivity;

   -------------------------------------
   -- Get_Registered_Contextual_Menus --
   -------------------------------------

   function Get_Registered_Contextual_Menus
     (Kernel  : access Kernel_Handle_Record'Class)
      return GNAT.OS_Lib.String_List_Access
   is
      Count  : Natural := 0;
      C      : Contextual_Menu_Access;
      Result : GNAT.OS_Lib.String_List_Access;
   begin
      if Kernel.Contextual /= System.Null_Address then
         C := Convert (Kernel.Contextual);
         while C /= null loop
            Count := Count + 1;
            C := C.Next;
         end loop;

         Result := new GNAT.OS_Lib.String_List (1 .. Count);
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
     (Kernel        : access Kernel_Handle_Record'Class;
      Name          : String;
      Label         : String := "";
      Filter        : GPS.Kernel.Action_Filter := null;
      Submenu       : Submenu_Factory := null;
      Ref_Item      : String := "";
      Add_Before    : Boolean := True;
      Group         : Natural := 0)
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
           (Menu_Type      => Type_Submenu,
            Name           => new String'(Name),
            Submenu_Filter => Filter,
            Pix            => null,
            Next           => null,
            Visible        => True,
            Filter_Matched => False,
            Sensitive      => True,
            Group          => Group,
            Submenu        => Submenu,
            Label          => Contextual_Menu_Label_Creator (T)),
         Ref_Item,
         Add_Before);
   end Register_Contextual_Submenu;

end GPS.Kernel.Modules;
