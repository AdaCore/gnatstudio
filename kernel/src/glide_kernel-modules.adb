-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  So that this type is correctly converted from C to Ada
with Gtk.Image_Menu_Item;
pragma Warnings (Off, Gtk.Image_Menu_Item);

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GUI_Utils;         use GUI_Utils;
with Gdk.Event;         use Gdk.Event;
with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Glide_Main_Window; use Glide_Main_Window;
with Gtk.Label;         use Gtk.Label;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Menu_Bar;      use Gtk.Menu_Bar;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Prj;               use Prj;
with String_Utils;      use String_Utils;
with Traces;            use Traces;

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
      Priority                : Module_Priority := Default_Priority;
      Initializer             : Module_Initializer  := null;
      Contextual_Menu_Handler : Module_Menu_Handler := null)
      return Module_ID
   is
      ID : Module_ID := new Module_ID_Information'
        (Name_Length     => Module_Name'Length,
         Name            => Module_Name,
         Priority        => Priority,
         Initializer     => Initializer,
         Contextual_Menu => Contextual_Menu_Handler);
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

   --------------------------
   -- Set_File_Information --
   --------------------------

   procedure Set_File_Information
     (Context : access File_Selection_Context;
      Project_View : Prj.Project_Id;
      Directory : String := "";
      File_Name : String := "") is
   begin
      Context.Project_View := Project_View;

      if Directory /= "" then
         Context.Directory := new String' (Directory);
      end if;

      if File_Name /= "" then
         Context.File_Name := new String' (File_Name);
      end if;
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

   function Has_Directory_Information (Context : access File_Selection_Context)
      return Boolean is
   begin
      return Context.Directory /= null;
   end Has_Directory_Information;

   ---------------------------
   -- Directory_Information --
   ---------------------------

   function Directory_Information (Context : access File_Selection_Context)
      return String is
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

   function Has_File_Information (Context : access File_Selection_Context)
      return Boolean is
   begin
      return Context.File_Name /= null;
   end Has_File_Information;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information (Context : access File_Selection_Context)
      return String is
   begin
      if Context.File_Name = null then
         return "";
      else
         return Context.File_Name.all;
      end if;
   end File_Information;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out File_Selection_Context) is
   begin
      Free (Context.Directory);
      Free (Context.File_Name);
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
      Menu : Gtk_Menu := null;
   begin
      if User.Kernel.Last_Context_For_Contextual /= null then
         Free (User.Kernel.Last_Context_For_Contextual);
      end if;

      --  Create the menu and add all the modules information
      Gtk_New (Menu);

      Set_Busy_Cursor (Get_Window (User.Kernel.Main_Window), True, True);
      Context := User.Context_Func
        (Kernel       => User.Kernel,
         Event_Widget => User.Event_Widget,
         Object       => User.Object,
         Event        => Event,
         Menu         => Menu);
      Set_Context_Information
        (Context,
         Kernel  => User.Kernel,
         Creator => User.ID);

      User.Kernel.Last_Context_For_Contextual := Context;

      if Context /= null then
         while not Module_List.Is_Empty (Current) loop
            if Module_List.Head (Current) /= User.ID then
               Module_List.Head (Current).Contextual_Menu
                 (Context   => Context,
                  Menu      => Menu);
            end if;
            Current := Module_List.Next (Current);
         end loop;
      end if;

      Set_Busy_Cursor (Get_Window (User.Kernel.Main_Window), False, True);

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
                   & " doesn't have a submenu, can't create item in"
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
         Gtk_New (Menu_Item, Parent_Path (First .. Last - 1));
         Set_Submenu (Menu_Item, Menu);

         if Item = null
           and then Last >= Parent_Path'Last
         then
            Find_Menu_Item_By_Name
              (Glide_Window (Kernel.Main_Window).Menu_Bar,
               Parent, Ref_Item, Pred, Index);
            if Add_Before then
               Insert (Parent, Menu_Item, Index);
            elsif Index /= -1 then
               Insert (Parent, Menu_Item, Index + 1);
            else
               Append (Parent, Menu_Item);
            end if;
         else
            Append (Parent, Menu_Item);
         end if;

         Show_All (Menu_Item);
         Parent := Menu;

         First := Last + 1;
      end loop;

      if Item /= null then
         Find_Menu_Item_By_Name
           (Glide_Window (Kernel.Main_Window).Menu_Bar,
            Parent, Ref_Item, Pred, Index);
         if Add_Before then
            Insert (Parent, Item, Index);
         elsif Index /= -1 then
            Insert (Parent, Item, Index + 1);
         else
            Append (Parent, Item);
         end if;

         Show_All (Item);
      end if;
   end Register_Menu;

end Glide_Kernel.Modules;
