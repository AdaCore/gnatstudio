-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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

--  So that this type is correctly converted from C to Ada
with Gtk.Image_Menu_Item;
pragma Warnings (Off, Gtk.Image_Menu_Item);

with GUI_Utils;         use GUI_Utils;
with Gdk.Dnd;           use Gdk.Dnd;
with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Glib;              use Glib;
with Glib.Convert;      use Glib.Convert;
with Glib.Module;       use Glib.Module;
with Glib.Object;       use Glib.Object;
with Glib.Values;       use Glib.Values;
with Glide_Main_Window; use Glide_Main_Window;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Accel_Map;     use Gtk.Accel_Map;
with Gtk.Button;        use Gtk.Button;
with Gtk.Dnd;           use Gtk.Dnd;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Image;         use Gtk.Image;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Menu_Bar;      use Gtk.Menu_Bar;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Selection;     use Gtk.Selection;
with Gtk.Toolbar;       use Gtk.Toolbar;
with Gtk.Widget;        use Gtk.Widget;
with Gtkada.MDI;        use Gtkada.MDI;
with Projects;          use Projects;
with String_Utils;      use String_Utils;
with Traces;            use Traces;
with Glide_Intl;        use Glide_Intl;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Ada.Exceptions;    use Ada.Exceptions;
with VFS;               use VFS;
with File_Utils;        use File_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Glide_Kernel.Modules is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Modules");

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

   package Kernel_Contextuals is new GUI_Utils.User_Contextual_Menus
     (Contextual_Menu_User_Data);

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu;
   --  Create a contextual menu as a result of a mouse event
   --  Return null if no menu was created.

   procedure Destroy_Contextual_Menu
     (User : Contextual_Menu_User_Data; Menu : Gtk_Menu);
   --  Destroy the contextual menu that was created before

   type Non_Interactive_Action is record
      Command : Command_Access;
      Filter  : Action_Filter;
   end record;

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Non_Interactive_Action);
   --  Execute a single command.

   procedure Create_Menu
     (Widget  : access GObject_Record'Class;
      Data    : Menu_Factory_User_Data);
   --  Create a menu using the data in Factory.

   package Command_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Non_Interactive_Action);

   package Menu_Factory_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Menu_Factory_User_Data);

   ---------------------
   -- Compute_Tooltip --
   ---------------------

   procedure Compute_Tooltip
     (Kernel  : access Kernel_Handle_Record'Class;
      Context : Selection_Context_Access;
      Pixmap  : out Gdk.Gdk_Pixmap;
      Width   : out Gint;
      Height  : out Gint)
   is
      Current : Module_List.List_Node :=
        Module_List.First (Kernel.Modules_List);

      use type Module_List.List_Node;
      use type Gdk.Gdk_Pixmap;

   begin
      Pixmap := null;
      Width  := 0;
      Height := 0;

      while Current /= Module_List.Null_Node loop
         if Module_List.Data (Current).Info.Tooltip_Handler /= null then
            Module_List.Data (Current).Info.Tooltip_Handler
              (Context => Context,
               Pixmap  => Pixmap,
               Width   => Width,
               Height  => Height);

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
     (Module                  : in out Module_ID;
      Kernel                  : access Kernel_Handle_Record'Class;
      Module_Name             : String;
      Priority                : Module_Priority     := Default_Priority;
      Contextual_Menu_Handler : Module_Menu_Handler := null;
      Default_Context_Factory : Module_Default_Context_Factory := null;
      Save_Function           : Module_Save_Function := null;
      Tooltip_Handler         : Module_Tooltip_Handler := null;
      Customization_Handler   : Module_Customization_Handler := null)
   is
      Prev    : Module_List.List_Node := Module_List.Null_Node;
      Current : Module_List.List_Node :=
        Module_List.First (Kernel.Modules_List);

      use type Module_List.List_Node;
   begin
      if Module = null then
         Module := new Module_ID_Record;
      end if;

      Module.Info := new Module_ID_Information'
        (Name_Length           => Module_Name'Length,
         Name                  => Module_Name,
         Priority              => Priority,
         Contextual_Menu       => Contextual_Menu_Handler,
         Default_Factory       => Default_Context_Factory,
         Save_Function         => Save_Function,
         Tooltip_Handler       => Tooltip_Handler,
         Customization_Handler => Customization_Handler);

      while Current /= Module_List.Null_Node loop
         if Module_List.Data (Current).Info.Name = Module_Name then
            Console.Insert
              (Kernel,
               (-"Module already registered: ") & Module_Name, Mode => Error);
            return;
         end if;

         if Module_List.Data (Current).Info.Priority < Priority then
            Module_List.Append (Kernel.Modules_List, Prev, Module);
            return;
         end if;

         Prev    := Current;
         Current := Module_List.Next (Current);
      end loop;

      Module_List.Append (Kernel.Modules_List, Module);
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
        (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);

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
      return ID.Info.Priority;
   end Get_Priority;

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name (ID : access Module_ID_Record'Class) return String is
   begin
      return ID.Info.Name;
   end Module_Name;

   ----------------------------
   -- Create_Contextual_Menu --
   ----------------------------

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu
   is
      Current : Module_List.List_Node :=
        Module_List.First (User.Kernel.Modules_List);
      Context : Selection_Context_Access;
      Menu    : Gtk_Menu := null;

      use type Module_List.List_Node;
      use type Gtk.Widget.Widget_List.Glist;
   begin
      if User.Kernel.Last_Context_For_Contextual /= null then
         Unref (User.Kernel.Last_Context_For_Contextual);
      end if;

      --  Create the menu and add all the modules information
      Gtk_New (Menu);

      Push_State (User.Kernel, Busy);
      Context := User.Context_Func
        (Kernel       => User.Kernel,
         Event_Widget => User.Event_Widget,
         Object       => User.Object,
         Event        => Event,
         Menu         => Menu);

      User.Kernel.Last_Context_For_Contextual := Context;

      if Context /= null then
         Set_Context_Information
           (Context,
            Kernel  => User.Kernel,
            Creator => User.ID);

         while Current /= Module_List.Null_Node loop
            if Module_List.Data (Current) /= User.ID
              and then Module_List.Data (Current).Info.Contextual_Menu /= null
            then
               Module_List.Data (Current).Info.Contextual_Menu
                 (Object  => User.Object,
                  Context => Context,
                  Menu    => Menu);
            end if;

            Current := Module_List.Next (Current);
         end loop;
      end if;

      Pop_State (User.Kernel);

      --  If the menu is empty, destroy it.

      if Children (Menu) = Gtk.Widget.Widget_List.Null_List then
         Destroy (Menu);
         Menu := null;
      end if;

      return Menu;
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
     (Kernel      : access Kernel_Handle_Record'Class;
      Node        : Glib.Xml_Int.Node_Ptr;
      X           : Integer := 100;
      Y           : Integer := 100;
      Width       : Integer := 100;
      Height      : Integer := 100;
      Short_Title : String := "";
      Title       : String := "";
      State       : Gtkada.MDI.State_Type := Gtkada.MDI.Normal;
      Dock        : Gtkada.MDI.Dock_Side := Gtkada.MDI.None;
      Focus       : Boolean := False;
      Raised      : Boolean := False)
   is
   begin
      Add_To_Tree
        (Get_MDI (Kernel),
         Kernel.Default_Desktop,
         Node,
         X, Y, Width, Height,
         Short_Title, Title,
         State, Dock, Focus, Raised);
   end Add_Default_Desktop_Item;

   --------------------
   -- Find_Menu_Item --
   --------------------

   function Find_Menu_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item is
   begin
      return Find_Or_Create_Menu_Tree
        (Menu_Bar     => Glide_Window (Kernel.Main_Window).Menu_Bar,
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
      Parent, Pred    : Gtk_Menu_Item;
      Parent_Menu     : Gtk_Menu;
      Index           : Gint;

   begin
      Parent := Find_Or_Create_Menu_Tree
        (Menu_Bar     => Glide_Window (Kernel.Main_Window).Menu_Bar,
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
      end if;

      if Item /= null then
         Find_Menu_Item_By_Name
           (Glide_Window (Kernel.Main_Window).Menu_Bar,
            Parent_Menu, Ref_Item, Pred, Index);
         Add_Menu (Parent     => Parent_Menu,
                   Menu_Bar   => Glide_Window (Kernel.Main_Window).Menu_Bar,
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
      Command     : Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True;
      Action      : Action_Record_Access := null)
   is
      Item  : Gtk_Menu_Item;
      pragma Unreferenced (Item);
   begin
      Item := Register_Menu
        (Kernel, Parent_Path, Text, Stock_Image, Callback, Command,
         Accel_Key, Accel_Mods, Ref_Item, Add_Before, Sensitive, Action);
   end Register_Menu;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Non_Interactive_Action)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel_Handle (Widget));
   begin
      if Filter_Matches (Command.Filter, Context, Kernel_Handle (Widget)) then
         Launch_Background_Command
           (Kernel_Handle (Widget), Command.Command, Destroy_On_Exit => False,
            Active => False, Show_Bar => False, Queue_Id => "");
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
      Command     : Command_Access := null;
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True;
      Sensitive   : Boolean := True;
      Action      : Action_Record_Access := null) return Gtk_Menu_Item
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


      Item  : Gtk_Menu_Item;
      Image : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
      Accel_Path : constant String :=
        Cleanup ("<gps>/" & Parent_Path & '/' & Text);

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
            Kernel_Callback.To_Marshaller (Callback),
            Kernel_Handle (Kernel));
      end if;

      if Command /= null then
         Command_Callback.Object_Connect
           (Item, "activate",
            Command_Callback.To_Marshaller (Execute_Command'Access),
            Slot_Object => Kernel_Handle (Kernel),
            User_Data   => (Command, null));
      end if;

      if Action /= null then
         Command_Callback.Object_Connect
           (Item, "activate",
            Command_Callback.To_Marshaller (Execute_Command'Access),
            Slot_Object => Kernel_Handle (Kernel),
            User_Data   => (Command_Access (Action.Command), Action.Filter));
      end if;

      return Item;
   end Register_Menu;

   -----------------
   -- Create_Menu --
   -----------------

   procedure Create_Menu
     (Widget  : access GObject_Record'Class;
      Data    : Menu_Factory_User_Data)
   is
      pragma Unreferenced (Widget);

      procedure Remove_Item
        (Item : access Gtk.Widget.Gtk_Widget_Record'Class);
      --  Remove one item from Data.Menu.

      procedure Remove_Item
        (Item : access Gtk.Widget.Gtk_Widget_Record'Class) is
      begin
         Remove (Data.Menu, Item);
      end Remove_Item;

   begin
      --  Remove all items in the menu.

      Forall (Data.Menu, Remove_Item'Unrestricted_Access);

      --  Unref the previous context used for a global or contextual menu,
      --  if any.

      if Data.Kernel.Last_Context_For_Contextual /= null then
         Unref (Data.Kernel.Last_Context_For_Contextual);
      end if;

      --  Append all items in the menu.

      Data.Kernel.Last_Context_For_Contextual :=
        Get_Current_Context (Data.Kernel);

      --  The context must live until the menu is unmapped, therefore
      --  we need to Ref it.
      Ref (Data.Kernel.Last_Context_For_Contextual);
      Data.Factory
        (Data.Kernel, Data.Kernel.Last_Context_For_Contextual, Data.Menu);
      Show_All (Data.Menu);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Create_Menu;

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
        (Menu_Bar     => Glide_Window (Kernel.Main_Window).Menu_Bar,
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
           (Menu, "map",
            Menu_Factory_Callback.To_Marshaller (Create_Menu'Access),
            User_Data => (Kernel_Handle (Kernel), Factory, Menu));
      end if;
   end Register_Dynamic_Menu;

   ---------------------
   -- Register_Button --
   ---------------------

   procedure Register_Button
     (Kernel  : access Kernel_Handle_Record'Class;
      Text    : String;
      Command : Command_Access := null;
      Image   : Gtk.Image.Gtk_Image := null;
      Tooltip : String := "")
   is
      Button  : Gtk_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
   begin
      Button := Append_Item (Toolbar, Text, Text, Tooltip, Gtk_Widget (Image));
      Show_All (Button);
      Command_Callback.Object_Connect
        (Button, "clicked",
         Command_Callback.To_Marshaller (Execute_Command'Access),
         Slot_Object => Kernel_Handle (Kernel),
         User_Data   => (Command, null));
   end Register_Button;

   ---------------------
   -- Register_Button --
   ---------------------

   procedure Register_Button
     (Kernel   : access Kernel_Handle_Record'Class;
      Stock_Id : String;
      Command  : Command_Access := null;
      Tooltip  : String := "")
   is
      Button  : Gtk_Button;
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);
   begin
      Button := Insert_Stock (Toolbar, Stock_Id, Tooltip);
      Show_All (Button);
      Command_Callback.Object_Connect
        (Button, "clicked",
         Command_Callback.To_Marshaller (Execute_Command'Access),
         Slot_Object => Kernel_Handle (Kernel),
         User_Data   => (Command, null));
   end Register_Button;

   ------------------
   -- Free_Modules --
   ------------------

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class) is
      use Module_List;
   begin
      --  ??? Problem: should destroy the modules in the reverse order.
      --  Otherwise, the scripts module is no longer available for the other
      --  modules.
      Free (Kernel.Modules_List);
   end Free_Modules;

   ---------------------
   -- List_Of_Modules --
   ---------------------

   function List_Of_Modules (Kernel : access Kernel_Handle_Record'Class)
      return Glide_Kernel.Module_List.List is
   begin
      return Kernel.Modules_List;
   end List_Of_Modules;

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   procedure Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : Glide_Kernel.Kernel_Handle)
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
                    (Locale_To_UTF8 (Files (First + 8 .. Last - 1)));

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

end Glide_Kernel.Modules;
