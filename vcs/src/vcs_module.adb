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

with Types;                   use Types;

with Glib.Object;             use Glib.Object;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Arguments;           use Gtk.Arguments;
with Gtk.Main;                use Gtk.Main;

with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Project;    use Glide_Kernel.Project;
with Glide_Intl;              use Glide_Intl;

with Traces;                  use Traces;

with Gtkada.MDI;              use Gtkada.MDI;
with Gtkada.Handlers;         use Gtkada.Handlers;
with VCS_View_Pkg;            use VCS_View_Pkg;

with Prj_API;                 use Prj_API;

with Ada.Text_IO; use Ada.Text_IO;

with VCS;                     use VCS;
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

   procedure On_Open
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_View_Diff
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_Edit_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_Commit
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   function Get_Current_Dir
     (Kernel : access Kernel_Handle_Record'Class)
     return String;
   --  Convenience function to get the current directory.

   function Get_Current_File (Kernel : Kernel_Handle) return String;
   --  Convenience function to get the current file.

   function Get_Current_Ref
     (Kernel : access Kernel_Handle_Record'Class)
     return VCS_Access;
   --  Convenience function to get the current file.

   function Get_Selected_Files
     (Kernel : Kernel_Handle)
     return String_List.List;
   --  Return the currently selected files, as a list.
   --  Caller must free this list afterwards.

   function Get_Explorer (Kernel : Kernel_Handle) return VCS_View_Access;
   --  Return the vcs explorer, if created, null otherwise.

   ------------------
   -- Get_Explorer --
   ------------------

   function Get_Explorer (Kernel : Kernel_Handle) return VCS_View_Access is
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag (Get_MDI (Kernel), VCS_View_Record'Tag);

      if Child = null then
         return null;
      else
         return VCS_View_Access (Get_Widget (Child));
      end if;
   end Get_Explorer;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Kernel : access Kernel_Handle_Record'Class)
     return VCS_Access
   is
   begin
      return Get_VCS_From_Id ("CVS");
      --  ??? should get this information from the project !!
   end Get_Current_Ref;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle)
     return String_List.List
   is
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Result   : String_List.List;
   begin
      if Explorer = null then
         if Get_Current_File (Kernel) = "" then
            return Result;
         end if;

         String_List.Append (Result, Get_Current_File (Kernel));
      else
         Result := Get_Selected_Files (Explorer);
      end if;

      return Result;
   end Get_Selected_Files;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Kernel : access Kernel_Handle_Record'Class)
     return String
   is
      Context : Selection_Context_Access :=
        Get_Current_Explorer_Context (Kernel);
      File    : File_Selection_Context_Access := null;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);
         if Has_Directory_Information (File) then
            return Directory_Information (File);
         end if;
      end if;

      return "";
   end Get_Current_Dir;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File (Kernel : Kernel_Handle) return String is
      Context : Selection_Context_Access :=
        Get_Current_Explorer_Context (Kernel);
      File    : File_Selection_Context_Access := null;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);
         if Has_File_Information (File) then
            return Directory_Information (File) & File_Information (File);
         end if;
      end if;

      return "";
   end Get_Current_File;

   -------------
   -- On_Open --
   -------------

   procedure On_Open
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Open_Files (null, Kernel, Files, Ref);
      String_List.Free (Files);
   end On_Open;

   ---------------
   -- On_Commit --
   ---------------

   procedure On_Commit
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      --  Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      --  Commit (null, Kernel, Files, Ref);
      String_List.Free (Files);
   end On_Commit;

   ------------------
   -- On_View_Diff --
   ------------------

   procedure On_View_Diff
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Diff_Files (null, Kernel, Files, Ref);
      String_List.Free (Files);
   end On_View_Diff;

   -----------------
   -- On_Edit_Log --
   -----------------

   procedure On_Edit_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Edit_Log (null, Kernel, Files, Ref);
      String_List.Free (Files);
   end On_Edit_Log;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Object  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      Context      : Selection_Context_Access :=
        To_Selection_Context_Access (To_Address (Args, 1));
      File         : File_Selection_Context_Access;

      Ref          : VCS_Access := Get_Current_Ref (Get_Kernel (Context));
      Explorer     : VCS_View_Access := VCS_View_Access (Object);
   begin
      if Context = null then
         return;
      end if;

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);
         if Has_Directory_Information (File) then
            Show_Files (Explorer, Directory_Information (File), Ref);
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
      Files   : String_List.List;
      Dirs    : String_List.List;
      Status  : File_Status_List.List;
      Ref     : VCS_Access := Get_Current_Ref (Kernel);
      Dir     : String := Get_Current_Dir (Kernel);

   begin
      if Dir /= "" then
         String_List.Append (Dirs, Dir);

         Status := Local_Get_Status (Ref, Dirs);

         while not  File_Status_List.Is_Empty (Status) loop
            String_List.Append
              (Files,
               String_List.Head (File_Status_List.Head (Status).File_Name));
            File_Status_List.Tail (Status);
         end loop;

         Update_File_List (Get_Explorer (Kernel), Kernel, Files, Ref);

         String_List.Free (Dirs);
         String_List.Free (Files);
      end if;
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

      if Dir /= "" then
         String_List.Append (Dirs, Dir);

         Status := Local_Get_Status (Ref, Dirs);

         while not  File_Status_List.Is_Empty (Status) loop
            String_List.Append
              (Files,
               String_List.Head (File_Status_List.Head (Status).File_Name));
            File_Status_List.Tail (Status);
         end loop;

         Get_Status (Explorer, Files);

         String_List.Free (Dirs);
         String_List.Free (Files);
      end if;
   end Get_Status_For_Current_Dir;

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      MDI      : MDI_Window := Get_MDI (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Child    : MDI_Child;
      Ref      : VCS_Access := Get_Current_Ref (Kernel);

   begin
      if Explorer = null then
         Gtk_New (Explorer, Kernel, Ref);
         Set_Size_Request (Explorer, 400, 400);
         Child := Put (MDI, Explorer);
         Set_Title (Child, -"VCS Explorer");
         Show_Files (Explorer, Get_Current_Dir (Kernel), Ref);

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
      Dirs : String_Id_Array := Source_Dirs (Get_Project_View (Kernel));
      --  ??? we need a project-recursive version of this function.
   begin
      for J in Dirs'Range loop
         if Dirs (J) /= No_String then
            --  String_To_Name_Buffer (Dirs (J));
            Put_Line (Get_String (Dirs (J)));
         end if;
      end loop;
   end On_Update_All;

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
      Push_Message (null, Kernel_Handle (Kernel),
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
      Register_Idle_Function (Idle'Access);
      Register_Error_Function (Handle_VCS_Error'Access, GObject (Kernel));

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
--       Kernel_Callback.Connect
--         (Menu_Item, "activate",
--          Kernel_Callback.To_Marshaller (List_Open_Files'Access),
--          Kernel_Handle (Kernel));

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

      Gtk_New (Menu_Item, -"Open");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Open'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"View Diff");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_View_Diff'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Edit log");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Edit_Log'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Commit");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_Commit'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"Annotate");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
--       Kernel_Callback.Connect
--         (Menu_Item, "activate",
--          Kernel_Callback.To_Marshaller (Update_Files_In_Current_Dir'Access),
--          Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"View Changelog");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
--       Kernel_Callback.Connect
--         (Menu_Item, "activate",
--          Kernel_Callback.To_Marshaller (Update_Files_In_Current_Dir'Access),
--          Kernel_Handle (Kernel));

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

--       Register_Contextual_Menu
--         (Kernel          => Kernel,
--          Event_On_Widget =>

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
         Contextual_Menu_Handler => null);
   end Register_Module;

end VCS_Module;
