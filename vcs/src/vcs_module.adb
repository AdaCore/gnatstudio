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
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Main;                  use Gtk.Main;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Intl;                use Glide_Intl;

with Traces;                    use Traces;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;
with VCS_View_Pkg;              use VCS_View_Pkg;
with VCS_View_API;              use VCS_View_API;

with VCS;                       use VCS;
with String_List;

package body VCS_Module is

   VCS_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("VCS_Module");

   procedure Idle;
   --  This procedure will be called whenever the process in the background is
   --  waiting for something (command line output, network connection, etc)

   procedure Handle_VCS_Error
     (Message  : String;
      Kernel   : GObject);
   --  Handle the error message output by VCS operations.

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

   procedure List_Open_Files
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);

   procedure On_Context_Changed
     (Object  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args);
   --  ???

   procedure Update_Files_In_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Updates all the files in the current directory.

   procedure Get_Status_For_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Show status for files in the current directory.

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Object  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      pragma Unreferenced (Object);
      Context      : Selection_Context_Access :=
        To_Selection_Context_Access (To_Address (Args, 1));
      File         : File_Selection_Context_Access;
      Status       : File_Status_List.List;
      Dirs         : String_List.List;
      Explorer     : VCS_View_Access := Get_Explorer (Get_Kernel (Context));

      Ref          : VCS_Access := Get_Current_Ref (Get_Kernel (Context));
   begin
      if Context = null
        or else Explorer = null
      then
         return;
      end if;

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);
         if Has_Directory_Information (File) then
            String_List.Append (Dirs, Directory_Information (File));
            Status :=  Local_Get_Status (Ref, Dirs);
            String_List.Free (Dirs);

            Clear (Explorer);
            Display_File_Status (Get_Kernel (Context), Status, False);
            File_Status_List.Free (Status);
         end if;
      end if;
   end On_Context_Changed;

   ---------------------------------
   -- Update_Files_In_Current_Dir --
   ---------------------------------

   procedure Update_Files_In_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
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
   end Update_Files_In_Current_Dir;

   --------------------------------
   -- Get_Status_For_Current_Dir --
   --------------------------------

   procedure Get_Status_For_Current_Dir
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Ref      : VCS_Access := Get_Current_Ref (Kernel);
      Dir      : String := Get_Current_Dir (Kernel);
      Files    : String_List.List;
      Dirs     : String_List.List;
      Status   : File_Status_List.List;
   begin
      if Explorer = null then
         On_Open_Interface (Widget, Kernel);
         Explorer := Get_Explorer (Kernel);
      end if;

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
   end Get_Status_For_Current_Dir;

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      MDI      : MDI_Window := Get_MDI (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Child    : MDI_Child;
      Ref      : VCS_Access := Get_Current_Ref (Kernel);

      Dirs     : String_List.List;
      Status   : File_Status_List.List;

   begin
      if Explorer = null then
         Gtk_New (Explorer, Kernel);
         Set_Size_Request (Explorer, 400, 400);
         Child := Put (MDI, Explorer);
         Set_Title (Child, -"VCS Explorer");

         String_List.Append (Dirs, Get_Current_Dir (Kernel));
         Status :=  Local_Get_Status (Ref, Dirs);
         String_List.Free (Dirs);

         Clear (Explorer);
         Display_File_Status (Kernel, Status, False);
         File_Status_List.Free (Status);

         Widget_Callback.Object_Connect
           (Kernel,
            Context_Changed_Signal,
            On_Context_Changed'Access,
            Explorer);
      else
         Set_Focus_Child (Child);
      end if;
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
   end On_Update_All;

   -------------------
   -- List_Open_Files --
   -------------------

   procedure List_Open_Files
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Ref      : VCS_Access := Get_Current_Ref (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
   begin
      if Explorer = null then
         On_Open_Interface (Widget, Kernel);
         Explorer := Get_Explorer (Kernel);
      end if;

      Clear (Explorer);
      Get_Status (Ref, Get_Files_In_Project (Kernel));
   end List_Open_Files;

   ----------
   -- Idle --
   ----------

   procedure Idle is
      No_Main_Loop : Boolean;
      Count        : Natural := 0;
   begin
      while Gtk.Main.Events_Pending
        and then Count /= 30
      loop
         No_Main_Loop := Gtk.Main.Main_Iteration;
         Count := Count + 1;
      end loop;
   end Idle;

   ----------------------
   -- Handle_VCS_Error --
   ----------------------

   procedure Handle_VCS_Error
     (Message  : String;
      Kernel   : GObject) is
   begin
      Push_Message (Kernel_Handle (Kernel),
                    Glide_Kernel.Console.Error, Message);
   end Handle_VCS_Error;

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
--       Kernel_Callback.Connect
--         (Menu_Item, "activate",
--          Kernel_Callback.To_Marshaller (Update_Files_In_Current_Dir'Access),
--          Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Add to repository");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
--       Kernel_Callback.Connect
--         (Menu_Item, "activate",
--          Kernel_Callback.To_Marshaller (Update_Files_In_Current_Dir'Access),
--          Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Remove from repository");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
--       Kernel_Callback.Connect
--         (Menu_Item, "activate",
--          Kernel_Callback.To_Marshaller (Update_Files_In_Current_Dir'Access),
--          Kernel_Handle (Kernel));
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      VCS_Module_ID := Register_Module
        (Module_Name             => VCS_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => VCS_View_API.VCS_Contextual_Menu'Access);
   end Register_Module;

end VCS_Module;
