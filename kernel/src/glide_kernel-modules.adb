-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GUI_Utils;         use GUI_Utils;
with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Glib;              use Glib;
with Glib.Module;       use Glib.Module;
with Glib.Object;       use Glib.Object;
with Glib.Values;       use Glib.Values;
with Glide_Main_Window; use Glide_Main_Window;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Accel_Map;     use Gtk.Accel_Map;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Image;         use Gtk.Image;
with Gtk.Label;         use Gtk.Label;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Menu_Bar;      use Gtk.Menu_Bar;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Menu_Shell;    use Gtk.Menu_Shell;
with Gtk.Widget;        use Gtk.Widget;
with Gtkada.MDI;        use Gtkada.MDI;
with Language;          use Language;
with Prj;               use Prj;
with Prj_API;           use Prj_API;
with Src_Info;          use Src_Info;
with Src_Info.Queries;  use Src_Info.Queries;
with String_Utils;      use String_Utils;
with Traces;            use Traces;
with Glide_Intl;        use Glide_Intl;
with Glide_Kernel.Project; use Glide_Kernel.Project;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Ada.Exceptions;    use Ada.Exceptions;
with Unchecked_Deallocation;

package body Glide_Kernel.Modules is

   Me : constant Debug_Handle := Create ("Glide_Kernel.Modules");

   type Contextual_Menu_User_Data is record
      Object       : GObject;
      Context_Func : Context_Factory;
      Kernel       : Kernel_Handle;
      ID           : Module_ID;
      Event_Widget : Gtk_Widget;
   end record;

   package Kernel_Contextuals is new GUI_Utils.User_Contextual_Menus
     (Contextual_Menu_User_Data);

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu;
   --  Create a contextual menu as a result of a mouse event

   procedure Destroy_Contextual_Menu
     (User : Contextual_Menu_User_Data; Menu : Gtk_Menu);
   --  Destroy the contextual menu that was created before

   procedure Find_Menu_Item_By_Name
     (Menu_Bar  : Gtk_Menu_Bar;
      Menu      : Gtk_Menu;
      Name      : String;
      Menu_Item : out Gtk_Menu_Item;
      Index     : out Gint);
   --  Return the menu item with name Name, either from Menu, or from Menu_Bar
   --  if the latter is null.

   procedure General_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String;
      Info           : Line_Information_Data;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True);
   --  Create the Mime info for adding/creating/removing line information,
   --  and send it.

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Command_Access);
   --  Execute a single command.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Editor_Page_Array, Project_Editor_Page_Array_Access);
   --  Free the memory used by Pages

   procedure Unchecked_Free is new Unchecked_Deallocation
     (Search_Regexps_Array, Search_Regexps_Array_Access);

   function Module_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String;
   --  Command handler for the module commands.

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
      Mime_Handler            : Module_Mime_Handler := null;
      MDI_Child_Tag           : Ada.Tags.Tag := Kernel_Handle_Record'Tag;
      Default_Context_Factory : Module_Default_Context_Factory := null;
      Save_Function           : Module_Save_Function := null;
      Tooltip_Handler         : Module_Tooltip_Handler := null)
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
        (Name_Length     => Module_Name'Length,
         Name            => Module_Name,
         Priority        => Priority,
         Contextual_Menu => Contextual_Menu_Handler,
         Mime_Handler    => Mime_Handler,
         Default_Factory => Default_Context_Factory,
         Save_Function   => Save_Function,
         Tooltip_Handler => Tooltip_Handler,
         Child_Tag       => MDI_Child_Tag);

      while Current /= Module_List.Null_Node loop
         if Module_List.Data (Current).Info.Name = Module_Name then
            Console.Insert
              (Kernel,
               (-"module already registered: ") & Module_Name, Mode => Error);
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

   --------------------------
   -- Set_File_Information --
   --------------------------

   procedure Set_File_Information
     (Context : access File_Selection_Context;
      Directory : String := "";
      File_Name : String := "";
      Project_View      : Prj.Project_Id := Prj.No_Project;
      Importing_Project : Prj.Project_Id := Prj.No_Project) is
   begin
      Free (Context.Directory);
      Free (Context.File_Name);

      if Directory /= "" then
         Context.Directory := new String'(Directory);
      end if;

      if File_Name /= "" then
         Context.File_Name := new String'(File_Name);
      end if;

      Context.Creator_Provided_Project := Project_View /= No_Project;
      Context.Project_View := Project_View;
      Context.Importing_Project := Importing_Project;
   end Set_File_Information;

   -----------------------------
   -- Has_Project_Information --
   -----------------------------

   function Has_Project_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Creator_Provided_Project;
   end Has_Project_Information;

   -------------------------
   -- Project_Information --
   -------------------------

   function Project_Information (Context : access File_Selection_Context)
      return Prj.Project_Id is
   begin
      if Context.Project_View = No_Project
        and then Has_File_Information (Context)
      then
         Context.Project_View := Get_Project_From_File
           (Get_Project_View (Get_Kernel (Context)),
            File_Information (Context));
      end if;
      return Context.Project_View;
   end Project_Information;

   -------------------------------
   -- Has_Directory_Information --
   -------------------------------

   function Has_Directory_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Directory /= null;
   end Has_Directory_Information;

   ---------------------------
   -- Directory_Information --
   ---------------------------

   function Directory_Information
     (Context : access File_Selection_Context) return String is
   begin
      if Context.Directory = null then
         return "";
      else
         return Context.Directory.all;
      end if;
   end Directory_Information;

   --------------------------
   -- Has_File_Information --
   --------------------------

   function Has_File_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.File_Name /= null;
   end Has_File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information
     (Context : access File_Selection_Context) return String is
   begin
      if Context.File_Name = null then
         return "";
      else
         return Context.File_Name.all;
      end if;
   end File_Information;

   ---------------------------------------
   -- Has_Importing_Project_Information --
   ---------------------------------------

   function Has_Importing_Project_Information
     (Context : access File_Selection_Context) return Boolean is
   begin
      return Context.Importing_Project /= No_Project;
   end Has_Importing_Project_Information;

   -----------------------------------
   -- Importing_Project_Information --
   -----------------------------------

   function Importing_Project_Information
     (Context : access File_Selection_Context) return Prj.Project_Id is
   begin
      return Context.Importing_Project;
   end Importing_Project_Information;

   ------------------------------
   -- Set_Location_Information --
   ------------------------------

   procedure Set_Location_Information
     (Context     : access File_Location_Context;
      Category    : String := "";
      Message     : String := "";
      Line        : Integer := 0;
      Column      : Integer := 0) is
   begin
      Free (Context.Category_Name);
      if Category /= "" then
         Context.Category_Name := new String'(Category);
      end if;

      Free (Context.Message);
      if Message /= "" then
         Context.Message := new String'(Message);
      end if;

      Context.Line := Line;
      Context.Column := Column;
   end Set_Location_Information;

   --------------------------
   -- Has_Line_Information --
   --------------------------

   function Has_Line_Information
     (Context : access File_Location_Context) return Boolean is
   begin
      return Context.Line /= 0;
   end Has_Line_Information;

   ----------------------
   -- Line_Information --
   ----------------------

   function Line_Information
     (Context : access File_Location_Context) return Integer is
   begin
      return Context.Line;
   end Line_Information;

   ----------------------------
   -- Has_Column_Information --
   ----------------------------

   function Has_Column_Information
     (Context : access File_Location_Context) return Boolean is
   begin
      return Context.Column /= 0;
   end Has_Column_Information;

   ------------------------
   -- Column_Information --
   ------------------------

   function Column_Information
     (Context : access File_Location_Context) return Integer is
   begin
      return Context.Column;
   end Column_Information;

   ------------------------------
   -- Has_Category_Information --
   ------------------------------

   function Has_Category_Information
     (Context : access File_Location_Context) return Boolean is
   begin
      return Context.Category_Name /= null;
   end Has_Category_Information;

   --------------------------
   -- Category_Information --
   --------------------------

   function Category_Information
     (Context : access File_Location_Context) return String is
   begin
      return Context.Category_Name.all;
   end Category_Information;

   -----------------------------
   -- Has_Message_Information --
   -----------------------------

   function Has_Message_Information
     (Context : access File_Location_Context) return Boolean is
   begin
      return Context.Message /= null;
   end Has_Message_Information;

   -------------------------
   -- Message_Information --
   -------------------------

   function Message_Information
     (Context : access File_Location_Context) return String is
   begin
      return Context.Message.all;
   end Message_Information;

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context     : access Entity_Selection_Context;
      Entity_Name : String := "";
      Line        : Integer := 0;
      Column      : Integer := 0;
      Category    : Language.Language_Category := Language.Cat_Unknown) is
   begin
      Free (Context.Entity_Name);
      if Entity_Name /= "" then
         Context.Entity_Name := new String'(Entity_Name);
      end if;

      Context.Category := Category;
      Context.Line := Line;
      Context.Column := Column;
   end Set_Entity_Information;

   ---------------------------------
   -- Has_Entity_Name_Information --
   ---------------------------------

   function Has_Entity_Name_Information
     (Context : access Entity_Selection_Context) return Boolean is
   begin
      return Context.Entity_Name /= null;
   end Has_Entity_Name_Information;

   -----------------------------
   -- Entity_Name_Information --
   -----------------------------

   function Entity_Name_Information
     (Context : access Entity_Selection_Context) return String is
   begin
      if Context.Entity_Name = null then
         return "";
      else
         return Context.Entity_Name.all;
      end if;
   end Entity_Name_Information;

   --------------------------
   -- Has_Line_Information --
   --------------------------

   function Has_Line_Information
     (Context : access Entity_Selection_Context) return Boolean is
   begin
      return Context.Line /= 0;
   end Has_Line_Information;

   ----------------------
   -- Line_Information --
   ----------------------

   function Line_Information
     (Context : access Entity_Selection_Context) return Integer is
   begin
      return Context.Line;
   end Line_Information;

   ----------------------------
   -- Has_Column_Information --
   ----------------------------

   function Has_Column_Information
     (Context : access Entity_Selection_Context) return Boolean is
   begin
      return Context.Column /= 0;
   end Has_Column_Information;

   ------------------------
   -- Column_Information --
   ------------------------

   function Column_Information
     (Context : access Entity_Selection_Context) return Integer is
   begin
      return Context.Column;
   end Column_Information;

   ------------------------------
   -- Has_Category_Information --
   ------------------------------

   function Has_Category_Information
     (Context : access Entity_Selection_Context) return Boolean is
   begin
      return Context.Category /= Cat_Unknown;
   end Has_Category_Information;

   --------------------------
   -- Category_Information --
   --------------------------

   function Category_Information
     (Context : access Entity_Selection_Context)
      return Language.Language_Category is
   begin
      return Context.Category;
   end Category_Information;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out File_Selection_Context) is
   begin
      Free (Context.Directory);
      Free (Context.File_Name);
      Glide_Kernel.Destroy (Selection_Context (Context));
   end Destroy;

   --------------------------
   -- Set_Area_Information --
   --------------------------

   procedure Set_Area_Information
     (Context    : access File_Area_Context;
      Start_Line : Integer := 0;
      End_Line   : Integer := 0) is
   begin
      Context.Start_Line := Start_Line;
      Context.End_Line   := End_Line;
   end Set_Area_Information;

   --------------
   -- Get_Area --
   --------------

   procedure Get_Area
     (Context    : access File_Area_Context;
      Start_Line : out Integer;
      End_Line   : out Integer) is
   begin
      Start_Line := Context.Start_Line;
      End_Line   := Context.End_Line;
   end Get_Area;

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
   begin
      if User.Kernel.Last_Context_For_Contextual /= null then
         Free (User.Kernel.Last_Context_For_Contextual);
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
      pragma Assert (ID /= null);

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
      Focus       : Boolean := False)
   is
   begin
      Add_To_Tree
        (Get_MDI (Kernel),
         Kernel.Default_Desktop,
         Node,
         X, Y, Width, Height,
         Short_Title, Title,
         State, Dock, Focus);
   end Add_Default_Desktop_Item;

   --------------------
   -- Find_Menu_Item --
   --------------------

   function Find_Menu_Item
     (Kernel : access Kernel_Handle_Record'Class;
      Path   : String) return Gtk.Menu_Item.Gtk_Menu_Item
   is
      First, Last : Natural := Path'First + 1;
      Parent      : Gtk_Menu := null;
      Menu_Item   : Gtk_Menu_Item;
      Index       : Gint;

   begin
      pragma Assert (Path (Path'First) = '/');

      --  Find the existing parents

      loop
         Last := First + 1;
         Skip_To_Char (Path, Last, '/');

         Find_Menu_Item_By_Name
           (Glide_Window (Kernel.Main_Window).Menu_Bar,
            Parent,
            Path (First .. Last - 1),
            Menu_Item,
            Index);

         if Menu_Item = null then
            return null;
         end if;

         First := Last + 1;

         exit when First > Path'Last;

         if Get_Submenu (Menu_Item) = null then
            return null;
         end if;

         Parent := Gtk_Menu (Get_Submenu (Menu_Item));
      end loop;

      return Menu_Item;
   end Find_Menu_Item;

   ----------------------------
   -- Find_Menu_Item_By_Name --
   ----------------------------

   procedure Find_Menu_Item_By_Name
     (Menu_Bar  : Gtk_Menu_Bar;
      Menu      : Gtk_Menu;
      Name      : String;
      Menu_Item : out Gtk_Menu_Item;
      Index     : out Gint)
   is
      use type Widget_List.Glist;
      Children, Tmp : Widget_List.Glist;
      Label         : Gtk_Label;
      New_Name      : String := Name;
      Last          : Integer := New_Name'First;

   begin
      Menu_Item := null;

      if Name = "" then
         Index := -1;
         return;
      end if;

      for J in Name'Range loop
         if Name (J) = '_' then
            if J - 1 >= Name'First and then Name (J - 1) = '_' then
               New_Name (Last) := '_';
               Last := Last + 1;
            end if;
         else
            New_Name (Last) := Name (J);
            Last := Last + 1;
         end if;
      end loop;

      if Menu = null then
         Children := Get_Children (Menu_Bar);
      else
         Children := Get_Children (Menu);
      end if;

      Index := 0;
      Tmp := Children;

      while Tmp /= Widget_List.Null_List loop
         Menu_Item := Gtk_Menu_Item (Widget_List.Get_Data (Tmp));

         if Get_Child (Menu_Item) /= null
           and then Get_Child (Menu_Item).all in Gtk_Label_Record'Class
         then
            Label := Gtk_Label (Get_Child (Menu_Item));
            exit when Get_Text (Label) = New_Name (New_Name'First .. Last - 1);
         end if;

         Index := Index + 1;
         Tmp := Widget_List.Next (Tmp);
         Menu_Item := null;
      end loop;

      Widget_List.Free (Children);

      if Menu_Item = null then
         Index := -1;
      end if;
   end Find_Menu_Item_By_Name;

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
      procedure Add_Menu
        (Parent : Gtk_Menu; Item : Gtk_Menu_Item; Index : Gint);
      --  Append Item either to Parent, if not null, or directly to the menu
      --  bar

      --------------
      -- Add_Menu --
      --------------

      procedure Add_Menu
        (Parent : Gtk_Menu; Item : Gtk_Menu_Item; Index : Gint)
      is
         P : Gtk_Menu_Shell := Gtk_Menu_Shell (Parent);
      begin
         --  Insertion in the menu bar
         if Parent = null then
            P := Gtk_Menu_Shell (Glide_Window (Kernel.Main_Window).Menu_Bar);
         end if;

         if Index = -1 then
            Append (P, Item);
         elsif Add_Before then
            Insert (P, Item, Index);
         else
            Insert (P, Item, Index + 1);
         end if;
      end Add_Menu;

      First, Last     : Natural := Parent_Path'First + 1;
      Parent          : Gtk_Menu := null;
      Menu_Item, Pred : Gtk_Menu_Item;
      Menu            : Gtk_Menu;
      Index           : Gint;

   begin
      pragma Assert (Parent_Path (Parent_Path'First) = '/');

      --  Find the existing parents

      while First <= Parent_Path'Last loop
         Last := First + 1;
         Skip_To_Char (Parent_Path, Last, '/');

         Find_Menu_Item_By_Name
           (Glide_Window (Kernel.Main_Window).Menu_Bar,
            Parent,
            Parent_Path (First .. Last - 1),
            Menu_Item,
            Index);

         exit when Menu_Item = null;

         if Get_Submenu (Menu_Item) = null then
            Trace (Me, Parent_Path (First .. Last - 1)
                   & (-" doesn't have a submenu, can't create item in")
                   & Parent_Path);
            return;
         end if;

         Parent := Gtk_Menu (Get_Submenu (Menu_Item));
         First  := Last + 1;
      end loop;

      --  Create the missing parents

      while First <= Parent_Path'Last loop
         Last := First + 1;
         Skip_To_Char (Parent_Path, Last, '/');

         Gtk_New (Menu);
         Gtk_New_With_Mnemonic (Menu_Item, Parent_Path (First .. Last - 1));
         Set_Submenu (Menu_Item, Menu);

         Set_Accel_Group
           (Menu, Get_Default_Accelerators (Kernel));

         if Item = null
           and then Last >= Parent_Path'Last
         then
            Find_Menu_Item_By_Name
              (Glide_Window (Kernel.Main_Window).Menu_Bar,
               Parent, Ref_Item, Pred, Index);
            Add_Menu (Parent, Menu_Item, Index);
         else
            Add_Menu (Parent, Menu_Item, -1);
         end if;

         Show_All (Menu_Item);
         Parent := Menu;

         First := Last + 1;
      end loop;

      if Item /= null then
         Find_Menu_Item_By_Name
           (Glide_Window (Kernel.Main_Window).Menu_Bar,
            Parent, Ref_Item, Pred, Index);
         Add_Menu (Parent, Item, Index);

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
      Sensitive   : Boolean := True)
   is
      Item  : Gtk_Menu_Item;
   begin
      Item := Register_Menu
        (Kernel, Parent_Path, Text, Stock_Image, Callback, Command,
         Accel_Key, Accel_Mods, Ref_Item, Add_Before, Sensitive);
   end Register_Menu;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Widget  : access GObject_Record'Class;
      Command : Command_Access)
   is
      pragma Unreferenced (Widget);

      Dummy : Boolean;
   begin
      Dummy := Execute (Command);
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
      Sensitive   : Boolean := True)
     return Gtk_Menu_Item
   is

      function Cleanup (Path : String) return String;

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
        Cleanup ("<gps>" & Parent_Path & '/' & Text);

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
      Kernel_Callback.Connect
        (Item, "activate",
         Kernel_Callback.To_Marshaller (Callback),
         Kernel_Handle (Kernel));

      if Command /= null then
         Command_Callback.Connect
           (Item, "activate",
            Command_Callback.To_Marshaller (Execute_Command'Access),
            Command);
      end if;

      return Item;
   end Register_Menu;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write;
      Set_Busy  : Boolean := True) return Boolean
   is
      Current : Module_List.List_Node :=
        Module_List.First (Kernel.Modules_List);
      Result  : Boolean := False;

      use type Module_List.List_Node;
   begin
      if Set_Busy then
         Push_State (Kernel_Handle (Kernel), Busy);
      end if;

      while Current /= Module_List.Null_Node loop
         begin
            if Module_List.Data (Current).Info.Mime_Handler /= null then
               Result := Module_List.Data (Current).Info.Mime_Handler
                 (Kernel, Mime_Type, Data, Mode);
               exit when Result;
            end if;

         exception
            when Module_List.List_Empty =>
               null;

            when E : others =>
               Trace (Me, "Unexpected exception: " &
                      Exception_Information (E));
         end;

         Current := Module_List.Next (Current);
      end loop;

      if Set_Busy then
         Pop_State (Kernel_Handle (Kernel));
      end if;

      return Result;
   end Mime_Action;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Information_Record) is
   begin
      Free (X.Text);

      if X.Associated_Command /= null then
         Destroy (X.Associated_Command);
      end if;
   end Free;

   ------------------------------
   -- General_Line_Information --
   ------------------------------

   procedure General_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String;
      Info           : Line_Information_Data;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True)
   is
      Value         : GValue_Array (1 .. 5);
      Norm_Filename : constant String := Normalize_Pathname (File);
   begin
      Init (Value (1),  Glib.GType_String);
      Init (Value (2),  Glib.GType_String);
      Init (Value (3),  Glib.GType_Pointer);
      Init (Value (4),  Glib.GType_Boolean);
      Init (Value (5),  Glib.GType_Boolean);
      Set_String (Value (1), Norm_Filename);
      Set_String (Value (2), Identifier);
      Set_Address (Value (3), To_Address (Info));
      Set_Boolean (Value (4), Stick_To_Data);
      Set_Boolean (Value (5), Every_Line);

      if not Mime_Action
        (Kernel, Mime_File_Line_Info, Value, Set_Busy => False)
      then
         Trace (Me, "No file editor with line info display "
                & "capability was registered");
      end if;

      for J in Value'Range loop
         Unset (Value (J));
      end loop;
   end General_Line_Information;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String;
      Stick_To_Data  : Boolean := True;
      Every_Line     : Boolean := True)
   is
      A_Access : Line_Information_Data;
   begin
      A_Access := new Line_Information_Array (0 .. 0);
      General_Line_Information (Kernel,
                                File,
                                Identifier,
                                A_Access,
                                Stick_To_Data,
                                Every_Line);
      Unchecked_Free (A_Access);
   end Create_Line_Information_Column;


   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String)
   is
      A : Line_Information_Array (1 .. 0);
   begin
      General_Line_Information (Kernel, File, Identifier,
                                new Line_Information_Array'(A));
   end Remove_Line_Information_Column;

   --------------------------
   -- Add_Line_Information --
   --------------------------

   procedure Add_Line_Information
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : String;
      Identifier     : String;
      Info           : Line_Information_Data) is
   begin
      General_Line_Information (Kernel, File, Identifier, Info);
   end Add_Line_Information;

   ------------------------
   -- Clear_Highlighting --
   ------------------------

   procedure Clear_Highlighting
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : String) is
   begin
      if Is_Open (Kernel, Filename) then
         Open_File_Editor
           (Kernel,
            Filename,
            0, 0,
            Highlight_Line => False,
            Enable_Navigation => False);
      end if;
   end Clear_Highlighting;

   -----------------
   -- Create_Mark --
   -----------------

   function Create_Mark
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : String;
      Line              : Natural := 1;
      Column            : Natural := 1;
      Length            : Natural := 0) return String
   is
      File : Basic_Types.String_Access;
   begin
      if Is_Absolute_Path (Filename) then
         File := new String'(Normalize_Pathname (Filename));

      else
         declare
            F : constant String := Find_Source_File
              (Kernel, Filename, True);
         begin
            if Is_Absolute_Path (F) then
               File := new String'(F);

            else
               File := new String'(Filename);
            end if;
         end;
      end if;

      declare
         F : constant String := File.all;
      begin
         Free (File);

         return Interpret_Command
           (Kernel,
            "create_mark -l" & Line'Img
            & " -c" & Column'Img
            & " -L" & Length'Img
            & " " & F);
      end;
   end Create_Mark;

   ----------------------
   -- Open_File_Editor --
   ----------------------

   procedure Open_File_Editor
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : String;
      Line              : Natural := 1;
      Column            : Natural := 1;
      Column_End        : Natural := 0;
      Highlight_Line    : Boolean := False;
      Enable_Navigation : Boolean := True;
      New_File          : Boolean := True;
      From_Path         : Boolean := False)
   is
      Value      : GValue_Array (1 .. 7);
      File_Found : Boolean := False;
      Has_Dir    : Boolean;

   begin
      Init (Value (1), Glib.GType_String);

      if Is_Absolute_Path (Filename) then
         Set_String (Value (1), Normalize_Pathname (Filename));
         File_Found := True;

      elsif From_Path then
         Has_Dir := False;

         for S in Filename'Range loop
            if Filename (S) = '/'
              or else Filename (S) = Directory_Separator
            then
               Has_Dir := True;
               exit;
            end if;
         end loop;

         --  If we are editing a project file, check in the loaded tree first
         --  (in case an old copy is kept somewhere in the source or object
         --  path)

         if GNAT.Directory_Operations.File_Extension (Filename) =
           Prj.Project_File_Extension
         then
            declare
               Iterator : Imported_Project_Iterator := Start
                 (Get_Project (Kernel));
            begin
               while Current (Iterator) /= No_Project loop
                  if Project_Name (Current (Iterator)) & Project_File_Extension
                    = Filename
                  then
                     Set_String
                       (Value (1),
                        Project_Path (Project_Id'(Current (Iterator))));
                     File_Found := True;
                     Has_Dir := True;
                     exit;
                  end if;

                  Next (Iterator);
               end loop;
            end;
         end if;

         if not Has_Dir then
            declare
               F : GNAT.OS_Lib.String_Access := Locate_Regular_File
                 (Filename,
                  Include_Path (Get_Project_View (Kernel), Recursive => True));
            begin
               if F /= null then
                  Set_String (Value (1), Normalize_Pathname (F.all));
                  File_Found := True;
                  Free (F);
                  Has_Dir := True;
               end if;
            end;
         end if;

         --  Check the object directory too, for instance for the
         --  compiler-generated files (binder files, ...)
         if not Has_Dir then
            declare
               F : GNAT.OS_Lib.String_Access := Locate_Regular_File
                 (Filename,
                  Object_Path (Get_Project_View (Kernel), Recursive => True));
            begin
               if F /= null then
                  Set_String (Value (1), Normalize_Pathname (F.all));
                  File_Found := True;
                  Free (F);
                  Has_Dir := True;
               end if;
            end;
         end if;
      end if;

      if not File_Found then
         declare
            F : constant String := Find_Source_File
              (Kernel, Filename, True);
         begin
            --  If the file was found in one of the projects
            if Is_Absolute_Path (F) then
               Set_String (Value (1), F);

            --  Else just open the relative paths. This is mostly intended
            --  for files opened from the command line.
            else
               Set_String (Value (1), Normalize_Pathname (Filename));
            end if;
         end;
      end if;

      Init (Value (2), Glib.GType_Int);
      Set_Int (Value (2), Gint (Line));

      Init (Value (3), Glib.GType_Int);
      Set_Int (Value (3), Gint (Column));

      Init (Value (4), Glib.GType_Int);
      Set_Int (Value (4), Gint (Column_End));

      Init (Value (5), Glib.GType_Boolean);
      Set_Boolean (Value (5), Highlight_Line);

      Init (Value (6), Glib.GType_Boolean);
      Set_Boolean (Value (6), Enable_Navigation);

      Init (Value (7), Glib.GType_Boolean);
      Set_Boolean (Value (7), New_File);

      if not Mime_Action (Kernel, Mime_Source_File, Value) then
         Trace (Me, "No file editor was registered");
      end if;

      for J in Value'Range loop
         Unset (Value (J));
      end loop;
   end Open_File_Editor;

   ------------------------
   -- Close_File_Editors --
   ------------------------

   procedure Close_File_Editors
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : String)
   is
      Value : GValue_Array (1 .. 7);
   begin
      Init (Value (1), Glib.GType_String);
      Set_String (Value (1), Filename);

      Init (Value (2), Glib.GType_Int);
      Set_Int (Value (2), -1);

      Init (Value (3), Glib.GType_Int);
      Set_Int (Value (3), 0);

      Init (Value (4), Glib.GType_Int);
      Set_Int (Value (4), 0);

      Init (Value (5), Glib.GType_Boolean);
      Set_Boolean (Value (5), False);

      Init (Value (6), Glib.GType_Boolean);
      Set_Boolean (Value (6), False);

      Init (Value (7), Glib.GType_Boolean);
      Set_Boolean (Value (7), False);

      if not Mime_Action (Kernel, Mime_Source_File, Value) then
         Trace (Me, "No file editor was registered");
      end if;

      for J in Value'Range loop
         Unset (Value (J));
      end loop;
   end Close_File_Editors;

   ---------------
   -- Open_Html --
   ---------------

   procedure Open_Html
     (Kernel            : access Kernel_Handle_Record'Class;
      Filename          : String;
      Enable_Navigation : Boolean := True)
   is
      Value : GValue_Array (1 .. 2);
   begin
      Init (Value (1), Glib.GType_String);
      Set_String (Value (1), Filename);

      Init (Value (2), Glib.GType_Boolean);
      Set_Boolean (Value (2), Enable_Navigation);

      if not Mime_Action (Kernel, Mime_Html_File, Value) then
         Trace (Me, "No html viewer was registered");
      end if;

      for J in Value'Range loop
         Unset (Value (J));
      end loop;
   end Open_Html;

   -------------------------
   -- Display_Differences --
   -------------------------

   procedure Display_Differences
     (Kernel         : access Kernel_Handle_Record'Class;
      Orig_File      : String := "";
      New_File       : String := "";
      Diff_File      : String)
   is
      Value   : GValue_Array (1 .. 3);
      Success : Boolean;
   begin
      Init (Value (1), Glib.GType_String);
      Set_String (Value (1), Orig_File);

      Init (Value (2), Glib.GType_String);
      Set_String (Value (2), New_File);

      Init (Value (3), Glib.GType_String);
      Set_String (Value (3), Diff_File);

      Success := Mime_Action (Kernel, Mime_Diff_File, Value);

      for J in Value'Range loop
         Unset (Value (J));
      end loop;
   end Display_Differences;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Context : access Entity_Selection_Context)
      return Src_Info.Queries.Entity_Information
   is
      Lib_Info : LI_File_Ptr;
      Status   : Find_Decl_Or_Body_Query_Status;

   begin
      if Context.Entity = No_Entity_Information then
         Lib_Info := Locate_From_Source_And_Complete
           (Get_Kernel (Context), File_Information (Context));

         if Lib_Info = No_LI_File then
            Trace (Me, "Couldn't find LI file for "
                   & File_Information (Context));
         else
            Find_Declaration_Or_Overloaded
              (Kernel      => Get_Kernel (Context),
               Lib_Info    => Lib_Info,
               File_Name   => File_Information (Context),
               Entity_Name => Entity_Name_Information (Context),
               Line        => Line_Information (Context),
               Column      => Column_Information (Context),
               Entity      => Context.Entity,
               Status      => Status);

            if Status /= Success and then Status /= Fuzzy_Match then
               Destroy (Context.Entity);
               Context.Entity := No_Entity_Information;
            end if;
         end if;
      end if;

      return Context.Entity;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Entity_Selection_Context) is
   begin
      Destroy (File_Selection_Context (Context));
      Destroy (Context.Entity);
      Free (Context.Entity_Name);
   end Destroy;

   ------------------
   -- Free_Modules --
   ------------------

   procedure Free_Modules (Kernel : access Kernel_Handle_Record'Class) is
      use Module_List;

      Module_Node : List_Node;
   begin
      --  Unregister all mime handlers before freeing the modules list,
      --  to avoid the case when a module calls a mime action in its
      --  Destroy procedure.

      Module_Node := First (Kernel.Modules_List);

      while Module_Node /= Null_Node loop
         Data (Module_Node).Info.Mime_Handler := null;
         Module_Node := Next (Module_Node);
      end loop;

      Free (Kernel.Modules_List);
   end Free_Modules;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Data : in out Real_Kernel_Module_Data_Record) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Project_Editor_Page_Record'Class, Project_Editor_Page);
   begin
      if Data.Project_Editor_Pages /= null then
         for P in Data.Project_Editor_Pages'Range loop
            Destroy (Data.Project_Editor_Pages (P).all);
            Unchecked_Free (Data.Project_Editor_Pages (P));
         end loop;

         Unchecked_Free (Data.Project_Editor_Pages);
      end if;

      for S in Data.Search_Regexps'Range loop
         Free (Data.Search_Regexps (S).Name);
         Free (Data.Search_Regexps (S).Regexp);
      end loop;

      Unchecked_Free (Data.Search_Regexps);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Page : in out Project_Editor_Page_Record) is
   begin
      Free (Page.Label);
      Free (Page.Toc);
      Free (Page.Title);
   end Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Page         : access Project_Editor_Page_Record;
      Widget       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project_View : Prj.Project_Id := Prj.No_Project;
      Languages    : Argument_List)
   is
      pragma Unreferenced (Page, Widget, Project_View, Languages);
   begin
      null;
   end Refresh;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Page : access Project_Editor_Page_Record'Class)
      return String is
   begin
      return Page.Label.all;
   end Get_Label;

   -------------
   -- Get_Toc --
   -------------

   function Get_Toc (Page : access Project_Editor_Page_Record'Class)
      return String is
   begin
      return Page.Toc.all;
   end Get_Toc;

   ---------------
   -- Get_Title --
   ---------------

   function Get_Title (Page : access Project_Editor_Page_Record'Class)
      return String is
   begin
      return Page.Title.all;
   end Get_Title;

   ---------------
   -- Get_Flags --
   ---------------

   function Get_Flags (Page : access Project_Editor_Page_Record'Class)
      return Selector_Flags is
   begin
      return Page.Flags;
   end Get_Flags;

   ----------------------------------
   -- Register_Project_Editor_Page --
   ----------------------------------

   procedure Register_Project_Editor_Page
     (Kernel : access Kernel_Handle_Record'Class;
      Page   : Project_Editor_Page;
      Label  : String;
      Toc    : String;
      Title  : String;
      Flags     : Selector_Flags := Multiple_Projects or Multiple_Scenarios;
      Ref_Page  : String := "";
      Add_After : Boolean := True)
   is
      Tmp : Project_Editor_Page_Array_Access :=
        Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages;
      Pos : Natural;
   begin
      if Tmp = null then
         Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages :=
           new Project_Editor_Page_Array (1 .. 1);
      else
         Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages :=
           new Project_Editor_Page_Array (Tmp'First .. Tmp'Last + 1);
         Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages
           (Tmp'Range) := Tmp.all;
         Unchecked_Free (Tmp);
      end if;

      Page.Flags := Flags;
      Page.Label := new String'(Label);
      Page.Toc   := new String'(Toc);
      Page.Title := new String'(Title);

      Tmp := Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages;

      Pos := Tmp'Last;

      if Ref_Page /= "" then
         for J in Tmp'First .. Tmp'Last - 1 loop
            if Tmp (J).Label.all = Ref_Page then
               if Add_After then
                  Pos := J + 1;
               elsif J > Tmp'First then
                  Pos := J - 1;
               else
                  Pos := J;
               end if;
               exit;
            end if;
         end loop;
      end if;

      Tmp (Pos + 1 .. Tmp'Last) := Tmp (Pos .. Tmp'Last - 1);
      Tmp (Pos) := Page;
   end Register_Project_Editor_Page;

   --------------------------------
   -- Project_Editor_Pages_Count --
   --------------------------------

   function Project_Editor_Pages_Count
     (Kernel : access Kernel_Handle_Record'Class) return Natural is
   begin
      if Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages
        = null
      then
         return 0;
      else
         return
           Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages'Length;
      end if;
   end Project_Editor_Pages_Count;

   ---------------------------------
   -- Get_Nth_Project_Editor_Page --
   ---------------------------------

   function Get_Nth_Project_Editor_Page
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return Project_Editor_Page is
   begin
      if Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages /= null
        and then Num <=
          Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages'Length
      then
         return Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages
           (Real_Module_Data (Kernel.Modules_Data).Project_Editor_Pages'First
            + Num - 1);
      end if;
      return null;
   end Get_Nth_Project_Editor_Page;

   -----------------------------
   -- Register_Search_Pattern --
   -----------------------------

   procedure Register_Search_Pattern
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String;
      Regexp : String;
      Case_Sensitive : Boolean := False;
      Is_Regexp : Boolean := True)
   is
      Tmp : Search_Regexps_Array_Access :=
        Real_Module_Data (Kernel.Modules_Data).Search_Regexps;
   begin
      Real_Module_Data (Kernel.Modules_Data).Search_Regexps :=
        new Search_Regexps_Array (Tmp'First .. Tmp'Last + 1);
      Real_Module_Data (Kernel.Modules_Data).Search_Regexps
        (Tmp'Range) := Tmp.all;
      Unchecked_Free (Tmp);

      Tmp := Real_Module_Data (Kernel.Modules_Data).Search_Regexps;
      Tmp (Tmp'Last) :=
        (Name           => new String'(Name),
         Regexp         => new String'(Regexp),
         Case_Sensitive => Case_Sensitive,
         Is_Regexp      => Is_Regexp);

      Search_Regexps_Changed (Kernel);
   end Register_Search_Pattern;

   --------------------------
   -- Search_Regexps_Count --
   --------------------------

   function Search_Regexps_Count
     (Kernel : access Kernel_Handle_Record'Class) return Natural is
   begin
      return Real_Module_Data (Kernel.Modules_Data).Search_Regexps'Length;
   end Search_Regexps_Count;

   -----------------------------------
   -- Get_Nth_Search_Regexp_Options --
   -----------------------------------

   procedure Get_Nth_Search_Regexp_Options
     (Kernel         : access Kernel_Handle_Record'Class;
      Num            : Natural;
      Case_Sensitive : out Boolean;
      Is_Regexp      : out Boolean) is
   begin
      Case_Sensitive := Real_Module_Data
        (Kernel.Modules_Data).Search_Regexps (Num).Case_Sensitive;
      Is_Regexp := Real_Module_Data
        (Kernel.Modules_Data).Search_Regexps (Num).Is_Regexp;
   end Get_Nth_Search_Regexp_Options;

   --------------------------------
   -- Get_Nth_Search_Regexp_Name --
   --------------------------------

   function Get_Nth_Search_Regexp_Name
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return String is
   begin
      return Real_Module_Data
        (Kernel.Modules_Data).Search_Regexps (Num).Name.all;
   end Get_Nth_Search_Regexp_Name;

   ----------------------------------
   -- Get_Nth_Search_Regexp_Regexp --
   ----------------------------------

   function Get_Nth_Search_Regexp
     (Kernel : access Kernel_Handle_Record'Class; Num : Natural)
      return String is
   begin
      return Real_Module_Data
        (Kernel.Modules_Data).Search_Regexps (Num).Regexp.all;
   end Get_Nth_Search_Regexp;

   ----------------------------
   -- Module_Command_Handler --
   ----------------------------

   function Module_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;
      use type Command_List.List_Node;
      use type Module_List.List_Node;

      Command_Node : Command_List.List_Node;
      Args_Node    : List_Node;
      Success      : Boolean;
      Result       : GNAT.OS_Lib.String_Access;
      Current      : Module_List.List_Node;

      procedure Insert (S : String);
      --  Appends S & ASCII.LF to Result.
      --  Result must not be set to Null when calling this subprogram.

      procedure Insert (S : String) is
         R : constant String := Result.all & S & ASCII.LF;
      begin
         Free (Result);
         Result := new String'(R);
      end Insert;

   begin
      Result := new String'("");

      if Command = "help" then
         if Is_Empty (Args) then
            Insert (-"The following commands are defined:");
         end if;

         Command_Node := Command_List.First (Kernel.Commands_List);

         while Command_Node /= Command_List.Null_Node loop
            declare
               Data : constant Command_Information :=
                 Command_List.Data (Command_Node);
            begin
               if Is_Empty (Args) then
                  Insert ("  " & Data.Command.all);
               else
                  if Data.Command.all = Head (Args) then
                     Insert (Data.Help.all);
                  end if;
               end if;
            end;

            Command_Node := Command_List.Next (Command_Node);
         end loop;

         if Is_Empty (Args) then
            Insert (
              -"Type ""help <cmd>"" to get help about a specific command.");
         end if;

      elsif Command = "echo" then
         Args_Node := First (Args);

         while Args_Node /= Null_Node loop
            Insert (Data (Args_Node));
            Args_Node := Next (Args_Node);
         end loop;

      elsif Command = "insmod" then
         if Length (Args) /= 2 then
            return (-"Wrong number of arguments.") & ASCII.LF;
         end if;

         Dynamic_Register_Module
           (Kernel, Data (First (Args)), Data (Next (First (Args))), Success);

         if Success then
            return (-"Module successfully loaded.") & ASCII.LF;
         else
            return (-"Couldn't load module.") & ASCII.LF;
         end if;

      elsif Command = "lsmod" then
         Current := Module_List.First (Kernel.Modules_List);

         while Current /= Module_List.Null_Node loop
            Insert (Module_List.Data (Current).Info.Name);
            Current := Module_List.Next (Current);
         end loop;
      end if;

      declare
         R : constant String := Result.all;
      begin
         Free (Result);
         return R;
      end;
   end Module_Command_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Kernel.Modules_Data := new Real_Kernel_Module_Data_Record;
      Real_Module_Data (Kernel.Modules_Data).Search_Regexps :=
        new Search_Regexps_Array (1 .. 0);

      Register_Command
        (Kernel,
         "help",
         -"Lists recognized commands.",
         Module_Command_Handler'Access);

      Register_Command
        (Kernel,
         "echo",
         -"Display a line of text.",
         Module_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "insmod",
         Help    => -"Usage:" & ASCII.LF
           & "  insmod shared-lib module" & ASCII.LF
           & (-"Dynamically register from shared-lib a new module."),
         Handler => Module_Command_Handler'Access);

      Register_Command
        (Kernel,
         Command => "lsmod",
         Help    => -"List modules currently loaded.",
         Handler => Module_Command_Handler'Access);
   end Initialize;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Help    : String;
      Handler : Module_Command_Function)
   is
      use Command_List;

      Node : List_Node := First (Kernel.Commands_List);
   begin
      if Command = "" then
         return;
      end if;

      --  Check that the command is not already registered.

      while Node /= Null_Node loop
         if Data (Node).Command.all = Command then
            Trace
              (Me,
               "Interactive command " & Command & " is already registered");

            return;
         end if;

         Node := Next (Node);
      end loop;

      Append (Kernel.Commands_List,
                (Command => new String'(Command),
                 Help    => new String'(Help),
                 Command_Handler => Handler));
   end Register_Command;

   -----------------------
   -- Interpret_Command --
   -----------------------

   function Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List) return String
   is
      use String_List_Utils.String_List;
      use type Command_List.List_Node;

      Command_Node : Command_List.List_Node;

      Result : GNAT.OS_Lib.String_Access;

      Command_Found : Boolean := False;

   begin
      Result := new String'("");

      Command_Node := Command_List.First (Kernel.Commands_List);

      while Command_Node /= Command_List.Null_Node loop
         declare
            Data : constant Command_Information :=
              Command_List.Data (Command_Node);
         begin
            if Data.Command.all = Command then
               Free (Result);
               Result := new String'
                 (Data.Command_Handler
                    (Kernel,
                     Command,
                     Args));
               Command_Found := True;

               exit;
            end if;
         end;

         Command_Node := Command_List.Next (Command_Node);
      end loop;

      declare
         R : constant String := Result.all;
      begin
         Free (Result);

         if not Command_Found then
            return -"command not recognized";
         else
            return R;
         end if;
      end;
   end Interpret_Command;

   function Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String) return String
   is
      use String_List_Utils.String_List;
      use type Command_List.List_Node;

      Args         : Argument_List_Access;
      The_Command  : GNAT.OS_Lib.String_Access;
      The_Args     : String_List_Utils.String_List.List;

   begin
      if Command = "" then
         return "";
      end if;

      Trace (Me, "Launching interactive command: " & Command);

      Args := Argument_String_To_List (Command);

      The_Command := new String'(Args (Args'First).all);

      for J in Args'First + 1 .. Args'Last loop
         String_List_Utils.String_List.Append (The_Args, Args (J).all);
      end loop;

      declare
         R : constant String :=
           Interpret_Command (Kernel, The_Command.all, The_Args);
      begin
         Free (The_Command);
         Free (The_Args);
         Free (Args);

         return R;
      end;
   end Interpret_Command;

   procedure Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String) is
   begin
      Insert (Kernel, Interpret_Command (Kernel, Command), False);
   end Interpret_Command;

   procedure Interpret_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : String_List_Utils.String_List.List)
   is
   begin
      Insert (Kernel, Interpret_Command (Kernel, Command, Args), False);
   end Interpret_Command;

end Glide_Kernel.Modules;
