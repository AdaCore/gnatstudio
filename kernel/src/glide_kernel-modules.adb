-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  So that this type is correctly converted from C to Ada
with Gtk.Image_Menu_Item;
pragma Warnings (Off, Gtk.Image_Menu_Item);

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GUI_Utils;         use GUI_Utils;
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glib.Values;       use Glib.Values;
with Glide_Main_Window; use Glide_Main_Window;
with Gtk.Image_Menu_Item; use Gtk.Image_Menu_Item;
with Gtk.Accel_Group;   use Gtk.Accel_Group;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Image;         use Gtk.Image;
with Gtk.Label;         use Gtk.Label;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Menu_Bar;      use Gtk.Menu_Bar;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Menu_Shell;    use Gtk.Menu_Shell;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Prj;               use Prj;
with String_Utils;      use String_Utils;
with Traces;            use Traces;
with Glide_Intl;        use Glide_Intl;
with Glide_Kernel.Console; use Glide_Kernel.Console;

package body Glide_Kernel.Modules is

   Me : Debug_Handle := Create ("Glide_Kernel.Modules");

   type Contextual_Menu_User_Data is record
      Object       : GObject;
      Context_Func : Context_Factory;
      Kernel       : Kernel_Handle;
      ID           : Module_ID;
      Event_Widget : Gtk_Widget;
   end record;

   package Kernel_Contextuals is new GUI_Utils.User_Contextual_Menus
     (Contextual_Menu_User_Data);

   function Has_Lower_Priority (Module1, Module2 : Module_ID) return Boolean;
   --  Return True if Module1 has a lower priority than module2

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

   ------------------------
   -- Has_Lower_Priority --
   ------------------------

   function Has_Lower_Priority (Module1, Module2 : Module_ID) return Boolean is
   begin
      return Module1.Priority > Module2.Priority;
   end Has_Lower_Priority;

   ---------------------
   -- Register_Module --
   ---------------------

   function Register_Module
     (Module_Name             : String;
      Priority                : Module_Priority     := Default_Priority;
      Initializer             : Module_Initializer  := null;
      Contextual_Menu_Handler : Module_Menu_Handler := null;
      Mime_Handler            : Module_Mime_Handler := null)
      return Module_ID
   is
      ID : Module_ID := new Module_ID_Information'
        (Name_Length     => Module_Name'Length,
         Name            => Module_Name,
         Priority        => Priority,
         Initializer     => Initializer,
         Contextual_Menu => Contextual_Menu_Handler,
         Mime_Handler    => Mime_Handler,
         Was_Initialized => False);
   begin
      --  ??? Should check that the module isn't already in the list
      Module_List.Append (Global_Modules_List, ID);
      Module_List.Sort (Global_Modules_List, Has_Lower_Priority'Access);
      return ID;
   end Register_Module;

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name (ID : Module_ID) return String is
   begin
      return ID.Name;
   end Module_Name;

   -------------------------------
   -- Set_File_Name_Information --
   -------------------------------

   procedure Set_File_Name_Information
     (Context : access File_Name_Selection_Context;
      Directory : String := "";
      File_Name : String := "") is
   begin
      Free (Context.Directory);
      Free (Context.File_Name);

      if Directory /= "" then
         Context.Directory := new String' (Directory);
      end if;

      if File_Name /= "" then
         Context.File_Name := new String' (File_Name);
      end if;
   end Set_File_Name_Information;

   --------------------------
   -- Set_File_Information --
   --------------------------

   procedure Set_File_Information
     (Context           : access File_Selection_Context;
      Project_View      : Prj.Project_Id := Prj.No_Project;
      Importing_Project : Prj.Project_Id := Prj.No_Project) is
   begin
      Context.Project_View := Project_View;
      Context.Importing_Project := Importing_Project;
   end Set_File_Information;

   -----------------------------
   -- Has_Project_Information --
   -----------------------------

   function Has_Project_Information (Context : access File_Selection_Context)
      return Boolean is
   begin
      return Context.Project_View /= No_Project;
   end Has_Project_Information;

   -------------------------
   -- Project_Information --
   -------------------------

   function Project_Information (Context : access File_Selection_Context)
      return Prj.Project_Id is
   begin
      return Context.Project_View;
   end Project_Information;

   -------------------------------
   -- Has_Directory_Information --
   -------------------------------

   function Has_Directory_Information
     (Context : access File_Name_Selection_Context) return Boolean is
   begin
      return Context.Directory /= null;
   end Has_Directory_Information;

   ---------------------------
   -- Directory_Information --
   ---------------------------

   function Directory_Information
     (Context : access File_Name_Selection_Context) return String is
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
     (Context : access File_Name_Selection_Context) return Boolean is
   begin
      return Context.File_Name /= null;
   end Has_File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information
     (Context : access File_Name_Selection_Context) return String is
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

   ----------------------------
   -- Set_Entity_Information --
   ----------------------------

   procedure Set_Entity_Information
     (Context     : access Entity_Selection_Context;
      Entity_Name : String := "";
      Line        : Integer := 0;
      Column      : Integer := 0) is
   begin
      Free (Context.Entity_Name);
      if Entity_Name /= "" then
         Context.Entity_Name := new String' (Entity_Name);
      end if;

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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out File_Name_Selection_Context) is
   begin
      Free (Context.Directory);
      Free (Context.File_Name);
      Glide_Kernel.Destroy (Selection_Context (Context));
   end Destroy;

   ----------------------------
   -- Create_Contextual_Menu --
   ----------------------------

   function Create_Contextual_Menu
     (User  : Contextual_Menu_User_Data;
      Event : Gdk_Event) return Gtk_Menu
   is
      Current : Module_List.List := Global_Modules_List;
      Context : Selection_Context_Access;
      Menu    : Gtk_Menu := null;

   begin
      if User.Kernel.Last_Context_For_Contextual /= null then
         Free (User.Kernel.Last_Context_For_Contextual);
      end if;

      --  Create the menu and add all the modules information
      Gtk_New (Menu);

      Set_Busy (User.Kernel, True);
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

         while not Module_List.Is_Empty (Current) loop
            if Module_List.Head (Current) /= User.ID
              and then Module_List.Head (Current).Contextual_Menu /= null
            then
               Module_List.Head (Current).Contextual_Menu
                 (Object  => User.Object,
                  Context => Context,
                  Menu    => Menu);
            end if;

            Current := Module_List.Next (Current);
         end loop;
      end if;

      Set_Busy (User.Kernel, False);

      return Menu;
   end Create_Contextual_Menu;

   -----------------------------
   -- Destroy_Contextual_Menu --
   -----------------------------

   procedure Destroy_Contextual_Menu
     (User : Contextual_Menu_User_Data;
      Menu : Gtk_Menu) is
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

   begin
      Menu_Item := null;

      if Name = "" then
         Index := -1;
         return;
      end if;

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
            exit when Get_Text (Label) = Name;
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
      Accel_Key   : Gdk.Types.Gdk_Key_Type := 0;
      Accel_Mods  : Gdk.Types.Gdk_Modifier_Type := 0;
      Ref_Item    : String := "";
      Add_Before  : Boolean := True)
   is
      Item : Gtk_Menu_Item;
      Image : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
   begin
      if Stock_Image = "" then
         Gtk_New_With_Mnemonic (Item, Text);
      else
         Gtk_New_With_Mnemonic (Image, Text);
         Gtk_New (Pix, Stock_Image, Icon_Size_Menu);
         Set_Image (Image, Pix);
         Item := Gtk_Menu_Item (Image);
      end if;

      if Guint (Accel_Key) > 0 then
         Add_Accelerator
           (Item, "activate", Get_Default_Accelerators (Kernel),
            Accel_Key, Accel_Mods, Accel_Visible);
      end if;

      Register_Menu (Kernel, Parent_Path, Item, Ref_Item, Add_Before);
      Kernel_Callback.Connect
        (Item, "activate",
         Kernel_Callback.To_Marshaller (Callback),
         Kernel_Handle (Kernel));
   end Register_Menu;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      Current : Module_List.List := Global_Modules_List;
      Result  : Boolean := False;
   begin
      Set_Busy (Kernel_Handle (Kernel), True);

      while not Module_List.Is_Empty (Current) loop
         if Module_List.Head (Current).Mime_Handler /= null then
            Result := Module_List.Head (Current).Mime_Handler
              (Kernel, Mime_Type, Data, Mode);
            exit when Result;
         end if;

         Current := Module_List.Next (Current);
      end loop;

      Set_Busy (Kernel_Handle (Kernel), False);

      return Result;
   end Mime_Action;

   ----------------------
   -- Open_File_Editor --
   ----------------------

   procedure Open_File_Editor
     (Kernel   : access Kernel_Handle_Record'Class;
      Filename : String;
      Line     : Natural := 0;
      Column   : Natural := 0;
      Highlight_Line : Boolean := True)
   is
      Value : GValue_Array (1 .. 4);
   begin
      Init (Value (1), Glib.GType_String);
      Set_String (Value (1), Filename);

      Init (Value (2), Glib.GType_Int);
      Set_Int (Value (2), Gint (Line));

      Init (Value (3), Glib.GType_Int);
      Set_Int (Value (3), Gint (Column));

      Init (Value (4), Glib.GType_Boolean);
      Set_Boolean (Value (4), Highlight_Line);

      if not Mime_Action (Kernel, Mime_Source_File, Value) then
         Insert (Kernel, -"No file editor was registered");
      end if;

      for J in Value'Range loop
         Unset (Value (J));
      end loop;
   end Open_File_Editor;

   ---------------
   -- Open_Html --
   ---------------

   procedure Open_Html
     (Kernel         : access Kernel_Handle_Record'Class;
      Filename       : String)
   is
      Value : GValue_Array (1 .. 1);
   begin
      Init (Value (1), Glib.GType_String);
      Set_String (Value (1), Filename);

      if not Mime_Action (Kernel, Mime_Html_File, Value) then
         Insert (Kernel, -"No html viewer was registered");
      end if;

      Unset (Value (1));
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


end Glide_Kernel.Modules;
