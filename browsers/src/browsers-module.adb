-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Browsers.Canvas;           use Browsers.Canvas;
with Browsers.Dependency_Items; use Browsers.Dependency_Items;
with Browsers.Projects;         use Browsers.Projects;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Intl;                use Glide_Intl;
with Src_Info.Queries;          use Src_Info.Queries;
with Src_Info;                  use Src_Info;
with Traces;                    use Traces;
with Prj_API;                   use Prj_API;

with Glib.Object;          use Glib.Object;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtkada.MDI;           use Gtkada.MDI;
with Gtk.Widget;           use Gtk.Widget;

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Browsers.Module is

   Dependency_Browser_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Browsers.Module");

   Default_Browser_Width  : constant := 400;
   Default_Browser_Height : constant := 400;
   --  <preference> Default size for the browsers

   Vertical_Layout : Boolean := True;
   --  <preference> Should the layout of the graph be vertical or horizontal ?

   function Find_File
     (In_Browser : access Glide_Browser_Record'Class; Filename : String)
      return Canvas_Item;
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Part   : Unit_Part;
      Dep : Dependency)
      return Boolean;
   --  A filter function that decides whether Dep should be displayed in the
   --  canvas. It should return false if Dep should not be displayed.
   --
   --  Part is the unit_part of the file whose dependencies we are examining.
   --
   --  ??? This obviously needs to be modifiable from the browser itself.

   function Is_System_File (Source : Internal_File) return Boolean;
   --  Return True if Source is a system file (runtime file for Ada).
   --  ??? This should be moved to a more general location, and perhaps be
   --  ??? implemented with support from the project files.

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   procedure On_Dependency_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Dependency Browser

   procedure Edit_Dependencies_From_Contextual
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access);
   --  Examine the dependencies of a specific file

   procedure On_Examine_Prj_Hierarchy
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access);
   --  Open the project hierarchy browser for a specific project

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the appropriate contextual menus

   ------------------
   -- Open_Browser --
   ------------------

   function Open_Browser
     (Kernel       : access Kernel_Handle_Record'Class;
      Browser_Type : Browser_Type_Mask := Any_Browser)
      return Gtkada.MDI.MDI_Child
   is
      --  Mask    : Browser_Type_Mask;
      Child   : MDI_Child;
      Browser : Glide_Browser;
   begin
      Child := Find_MDI_Child_By_Tag (Kernel, Glide_Browser_Record'Tag);

      --  ??? Should handle masks as well -- perhaps we need a more general
      --  ??? find function in the kernel

      --  Mask := Get_Mask (Glide_Browser (Get_Widget (Child)));
      --  if (Mask and Browser_Type) = Browser_Type then
      --         return Child;
      --  end if;

      if Child /= null then
         Raise_Child (Child);
      else
         Gtk_New (Browser, Browser_Type, Kernel);
         Register_Contextual_Menu
           (Kernel          => Kernel,
            Event_On_Widget => Browser,
            Object          => Browser,
            ID              => Dependency_Browser_Module_ID,
            Context_Func    => Browser_Context_Factory'Access);
         Child := Put (Get_MDI (Kernel), Browser);
         Set_Size_Request
           (Browser, Default_Browser_Width, Default_Browser_Height);
         Set_Title (Child, "<browser>");
      end if;

      return Child;
   end Open_Browser;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      In_Browser   : access Glide_Browser_Record'Class;
      File         : String)
   is
      F             : constant String := Base_Name (File);
      Item, Initial : File_Item;
      Link          : Dependency_Link;
      Dep, List     : Dependency_List;
      Lib_Info      : LI_File_Ptr;
      Status        : Dependencies_Query_Status;
      Intern        : Internal_File;
      New_Item      : Boolean;
      Must_Add_Link : Boolean;
      Part : Unit_Part;

      function Has_One
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Set Add_Link to False if there is at least one link returned.
      --  ??? Would be nicer if we had real iterators in the canvas

      -------------
      -- Has_One --
      -------------

      function Has_One
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean is
      begin
         Must_Add_Link := False;
         return False;
      end Has_One;

   begin
      Lib_Info := Locate_From_Source (Kernel, F);
      if Lib_Info = No_LI_File then
         --  ??? Should be displayed in the status bar.

         Trace (Me, "Examine_Dependencies: Couldn't find ALI file for "
                & File);

         --  ??? Should we put an item in the browser anyway, even if nothing
         --  ??? can be done with it anyway ? This might give more indication
         --  ??? to the user that we don't know anything about that file.
         return;
      end if;

      Complete_ALI_File_If_Needed (Kernel, Lib_Info);
      pragma Assert (Lib_Info /= No_LI_File);

      Initial := File_Item (Find_File (In_Browser, F));
      if Initial = null then
         Gtk_New (Initial, Get_Window (In_Browser), In_Browser, Kernel,  F);
         Put (Get_Canvas (In_Browser), Initial);

         --  ??? Should check if the item was already expanded, so as to avoid
         --  ??? useless work.
      end if;

      Find_Dependencies (Lib_Info, List, Status);

      Part := Get_Unit_Part (Get_Source_Info_List (Kernel), F);

      if Status = Success then
         Dep := List;
         while Dep /= null loop
            if Filter (Kernel, Part, Dep.Value) then
               Intern := File_Information (Dep.Value);
               Item := File_Item
                 (Find_File (In_Browser, Get_Source_Filename (Intern)));
               New_Item := Item = null;
               Must_Add_Link := True;

               if New_Item then
                  Gtk_New (Item, Get_Window (In_Browser), In_Browser,
                           Kernel, Intern);

               else
                  --  If the item already existed, chances are that the link
                  --  also existed. Don't duplicate it in that case.
                  For_Each_Link
                    (Get_Canvas (In_Browser), Has_One'Unrestricted_Access,
                     From => Canvas_Item (Initial), To => Canvas_Item (Item));
               end if;

               if Must_Add_Link then
                  Gtk_New (Link, Dependency_Information (Dep.Value));
                  Add_Link (Get_Canvas (In_Browser),
                            Link => Link,
                            Src  => Initial,
                            Dest => Item);
               end if;

               if New_Item then
                  Put (Get_Canvas (In_Browser), Item);
               end if;
            end if;

            Dep := Dep.Next;
         end loop;

         Destroy (List);
         Layout (Get_Canvas (In_Browser),
                 Force => False,
                 Vertical_Layout => Vertical_Layout);
         Refresh_Canvas (Get_Canvas (In_Browser));
      end if;
   end Examine_Dependencies;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File (Source : Internal_File) return Boolean is
      Name : constant String := Get_Source_Filename (Source);
      Ada_Runtime_File, Gtk_System_File : Boolean;
   begin
      --  ??? The implementation here is too GNAT specific. However, getting
      --  ??? the Unit_Name would be expensive.
      Ada_Runtime_File := Name'Length > 2
        and then (Name (Name'First .. Name'First + 1) = "a-"
                  or else Name (Name'First .. Name'First + 1) = "s-"
                  or else Name (Name'First .. Name'First + 1) = "i-"
                  or else Name (Name'First .. Name'First + 1) = "g-");

      Ada_Runtime_File := Ada_Runtime_File
        or else Name = "ada.ads"
        or else Name = "interfac.ads"
        or else Name = "system.ads"
        or else Name = "unchconv.ads"
        or else Name = "unchdeal.ads"
        or else Name = "text_io.ads"
        or else Name = "gnat.ads";

      Gtk_System_File := Name'Length > 4
        and then (Name (Name'First .. Name'First + 2) = "gtk"
                  or else Name (Name'First .. Name'First + 3) = "glib"
                  or else Name (Name'First .. Name'First + 2) = "gdk");

      return Ada_Runtime_File or else Gtk_System_File;
   end Is_System_File;

   ------------
   -- Filter --
   ------------

   function Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Part   : Unit_Part;
      Dep    : Dependency)
      return Boolean
   is
      Explicit_Dependency : Boolean;
      Info : constant Dependency_Info := Dependency_Information (Dep);
   begin
      --  ??? This must be configurable at the GUI level.

      --  Only show explicit dependencies, not implicit ones
      Explicit_Dependency :=
        (Part = Unit_Spec and then Get_Depends_From_Spec (Info))
        or else
        (Part = Unit_Body and then Get_Depends_From_Body (Info));

      --  Do not display dependencies on runtime files
      return Explicit_Dependency
        and then not Is_System_File (File_Information (Dep));
   end Filter;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (In_Browser : access Glide_Browser_Record'Class; Filename : String)
      return Canvas_Item
   is
      Found : Canvas_Item := null;

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Check whether Item contains File

      ----------------
      -- Check_Item --
      ----------------

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean is
      begin
         if Item.all in File_Item_Record'Class
           and then Get_Source_Filename (Get_Source (File_Item (Item))) =
           Filename
         then
            Found := Canvas_Item (Item);
            return False;
         end if;
         return True;
      end Check_Item;

   begin
      For_Each_Item (Get_Canvas (In_Browser), Check_Item'Unrestricted_Access);
      return Found;
   end Find_File;

   ---------------------------
   -- On_Dependency_Browser --
   ---------------------------

   procedure On_Dependency_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child : MDI_Child;
   begin
      Child := Open_Browser (Kernel, Dependency_Browser);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_Dependency_Browser "
                & Exception_Information (E));
   end On_Dependency_Browser;

   ---------------------------------------
   -- Edit_Dependencies_From_Contextual --
   ---------------------------------------

   procedure Edit_Dependencies_From_Contextual
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access)
   is
      Browser : MDI_Child;
   begin
      Browser := Open_Browser (Get_Kernel (Context), Dependency_Browser);
      Examine_Dependencies
        (Get_Kernel (Context),
         Glide_Browser (Get_Widget (Browser)),
         File_Information (File_Selection_Context_Access (Context)));
   end Edit_Dependencies_From_Contextual;

   ------------------------------
   -- On_Examine_Prj_Hierarchy --
   ------------------------------

   procedure On_Examine_Prj_Hierarchy
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access)
   is
      Browser : MDI_Child;
   begin
      Browser := Open_Browser (Get_Kernel (Context), Project_Browser);
      Examine_Project_Hierarchy
        (Get_Kernel (Context),
         Glide_Browser (Get_Widget (Browser)),
         Get_Project_From_View
         (Project_Information (File_Selection_Context_Access (Context))));
   end On_Examine_Prj_Hierarchy;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      Tools : constant String := '/' & (-"Tools");
   begin
      Gtk_New (Menu_Item, -"Dependency Browser");
      Register_Menu (Kernel, Tools, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Dependency_Browser'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Class Browser");
      Register_Menu (Kernel, Tools, Menu_Item);
   end Initialize_Module;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Browser  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access)
   is
      File : constant String := Select_File (Base_Directory => "");
      --  ??? Should set up filters to only open file from the current project.
   begin
      if File /= "" then
         Examine_Dependencies
           (Get_Kernel (Context), Glide_Browser (Browser), File);
      end if;
   end Open_File;

   -----------------------------
   -- Browser_Contextual_Menu --
   -----------------------------

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Item         : Gtk_Menu_Item;
      File_Context : File_Selection_Context_Access;
   begin
      --  File selection (for instance from the explorer)
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Gtk_New (Item, Label =>
                     -"Examine project hierarchy for "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Examine_Prj_Hierarchy'Access),
               Selection_Context_Access (Context));
         end if;

         if Has_File_Information (File_Context) then
            Gtk_New (Item, Label => File_Information (File_Context)
                     & (-" depends on"));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Dependencies_From_Contextual'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Browser_Contextual_Menu;

begin
   Dependency_Browser_Module_ID := Register_Module
     (Module_Name             => Dependency_Browser_Module_Name,
      Priority                => Default_Priority,
      Initializer             => Initialize_Module'Access,
      Contextual_Menu_Handler => Browser_Contextual_Menu'Access);
end Browsers.Module;
