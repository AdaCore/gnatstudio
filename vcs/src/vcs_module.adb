-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib.Object;               use Glib.Object;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Intl;                use Glide_Intl;

with Traces;                    use Traces;

with VCS_View_Pkg;              use VCS_View_Pkg;
with VCS_View_API;              use VCS_View_API;

with VCS;                       use VCS;
with Ada.Exceptions;            use Ada.Exceptions;
with String_List_Utils;         use String_List_Utils;

package body VCS_Module is

   VCS_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("VCS_Module");

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  ???

   procedure On_Update_All
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  ???

   procedure List_Open_Files
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  ???

   procedure Update_Files_In_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Updates all the files in the current directory.

   procedure Get_Status_For_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Show status for files in the current directory.

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   ---------------------------------
   -- Update_Files_In_Current_Dir --
   ---------------------------------

   procedure Update_Files_In_Current_Dir
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files    : String_List.List;
      Dirs     : String_List.List;
      Status   : File_Status_List.List;
      Ref      : VCS_Access := Get_Current_Ref (Kernel);
      Dir      : String := Get_Current_Dir (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);

   begin
      String_List.Append (Dirs, Dir);

      Status := Local_Get_Status (Ref, Dirs);

      while not  File_Status_List.Is_Empty (Status) loop
         String_List.Append
           (Files,
            String_List.Head (File_Status_List.Head (Status).File_Name));
         File_Status_List.Tail (Status);
      end loop;

      Update (Ref, Files);

      if Explorer /= null then
         Clear (Explorer);
         Get_Status (Ref, Files);
      end if;

      String_List.Free (Dirs);
      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Update_Files_In_Current_Dir;

   --------------------------------
   -- Get_Status_For_Current_Dir --
   --------------------------------

   procedure Get_Status_For_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_View_Access;
      Ref      : VCS_Access := Get_Current_Ref (Kernel);
      Dir      : String := Get_Current_Dir (Kernel);
      Files    : String_List.List;
      Dirs     : String_List.List;
      Status   : File_Status_List.List;
   begin
      Open_Explorer (Kernel);
      Explorer := Get_Explorer (Kernel);

      String_List.Append (Dirs, Dir);

      Status := Local_Get_Status (Ref, Dirs);

      while not  File_Status_List.Is_Empty (Status) loop
         String_List.Append
           (Files,
            String_List.Head (File_Status_List.Head (Status).File_Name));
         File_Status_List.Tail (Status);
      end loop;

      Clear (Explorer);
      Get_Status (Ref, Files);

      String_List.Free (Dirs);
      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Get_Status_For_Current_Dir;

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Explorer (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Interface;

   -------------------
   -- On_Update_All --
   -------------------

   procedure On_Update_All
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Dirs : String_List.List := Get_Dirs_In_Project (Kernel);
      Ref  : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Dirs);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Update_All;

   ---------------------
   -- List_Open_Files --
   ---------------------

   procedure List_Open_Files
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Ref      : VCS_Access := Get_Current_Ref (Kernel);
      Explorer : VCS_View_Access;
   begin
      Open_Explorer (Kernel);
      Explorer := Get_Explorer (Kernel);

      Clear (Explorer);
      Get_Status (Ref, Get_Files_In_Project (Get_Project (Kernel)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end List_Open_Files;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      VCS       : constant String := -"VCS";

   begin
      Register_Menu
        (Kernel, "/_" & VCS, Ref_Item => -"Navigate", Add_Before => False);

      Gtk_New (Menu_Item, -"Explorer");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Open_Interface'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Update all files in project");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Update_All'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"List open files in project");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (List_Open_Files'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, "/" & VCS, Menu_Item);

      Gtk_New (Menu_Item, -"Query status for current directory");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Get_Status_For_Current_Dir'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Update current directory");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Update_Files_In_Current_Dir'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, "/" & VCS, Menu_Item);

      Gtk_New (Menu_Item, -"Update");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Update'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Open");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Open'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"View Diff");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (View_Diff'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Edit log");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Edit_Log'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Commit");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Commit'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Annotate");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (View_Annotate'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"View Changelog");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (View_Log'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Revert");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Revert'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Add to repository");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Add'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Remove from repository");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (Remove'Access),
         Kernel_Handle (Kernel));
   end Initialize_Module;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Submenu      : Gtk_Menu;
      Menu_Item    : Gtk_Menu_Item;
      File_Context : File_Selection_Context_Access;
   begin
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_File_Information (File_Context)
           or else Has_Directory_Information (File_Context)
           or else Has_Project_Information (File_Context)
         then
            Gtk_New (Menu_Item, Label => -"VCS");
            Gtk_New (Submenu);
            VCS_View_API.VCS_Contextual_Menu (Object, Context, Submenu);
            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
            Append (Menu, Menu_Item);
         end if;
      end if;
   end VCS_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      VCS_Module_ID := Register_Module
        (Module_Name             => VCS_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => VCS_Contextual_Menu'Access);
   end Register_Module;

end VCS_Module;
