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

with Glib.Object;               use Glib.Object;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Arguments;             use Gtk.Arguments;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Intl;                use Glide_Intl;

with Traces;                    use Traces;

with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Handlers;           use Gtkada.Handlers;
with VCS_View_Pkg;              use VCS_View_Pkg;

with Prj;                       use Prj;
with Prj_API;                   use Prj_API;
with Prj.Tree;                  use Prj.Tree;

with Basic_Types;               use Basic_Types;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

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

   function String_Array_To_String_List
     (S : String_Id_Array) return String_List.List;
   --  Convenience function to make a string_list out of a String_Id_Array.

   procedure On_Open
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_Update
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_View_Diff
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_View_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);

   procedure On_View_Annotate
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

   function Get_Files_In_Project
     (Kernel : Kernel_Handle) return String_List.List;

   function Get_Dirs_In_Project
     (Kernel : Kernel_Handle) return String_List.List;

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      On_Edit_Log (Widget, Get_Kernel (Context));
   end On_Menu_Edit_Log;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      On_Commit (Widget, Get_Kernel (Context));
   end On_Menu_Commit;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      On_Open (Widget, Get_Kernel (Context));
   end On_Menu_Open;

   --------------------
   -- On_Menu_Update --
   --------------------

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      On_Update (Widget, Get_Kernel (Context));
   end On_Menu_Update;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File_Context : File_Selection_Context_Access;
      Kernel       : Kernel_Handle := Get_Kernel (Context);
      Ref          : VCS_Access := Get_Current_Ref (Kernel);
   begin
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_File_Information (File_Context)
           and then Has_Directory_Information (File_Context)
         then
            Diff (Ref,
                  Directory_Information (File_Context)
                  & File_Information (File_Context));
         end if;
      end if;
   end On_Menu_Diff;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Item         : Gtk_Menu_Item;
      File_Context : File_Selection_Context_Access;
   begin
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         Gtk_New (Item, Label => -"VCS Update");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"VCS Open");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Open'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"VCS Diff");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Diff'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"VCS Update");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"VCS Edit log");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Edit_Log'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"VCS Commit");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Commit'Access),
            Selection_Context_Access (Context));
      end if;
   end VCS_Contextual_Menu;

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

      return Get_Current_Dir;
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
      Open (Ref, Files);

      declare
         L_Temp : String_List.List := Files;
      begin
         while not String_List.Is_Empty (L_Temp) loop
            Open_File_Editor (Kernel, String_List.Head (L_Temp));
            L_Temp := String_List.Next (L_Temp);
         end loop;
      end;
   end On_Open;

   ---------------
   -- On_Update --
   ---------------

   procedure On_Update
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Files);
   end On_Update;

   ---------------
   -- On_Commit --
   ---------------

   procedure On_Commit
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle) is
   begin
      --  ??? Right now, commit opens a log editor for the file.
      --  We should decide what the correct behavior should be.

      On_Edit_Log (Widget, Kernel);
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
      while not String_List.Is_Empty (Files) loop
         Diff (Ref, String_List.Head (Files));
         String_List.Tail (Files);
      end loop;
   end On_View_Diff;

   ------------------
   -- On_View_Log --
   ------------------

   procedure On_View_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      while not String_List.Is_Empty (Files) loop
         Log (Ref, String_List.Head (Files));
         String_List.Tail (Files);
      end loop;
   end On_View_Log;

   ------------------
   -- On_View_Annotate --
   ------------------

   procedure On_View_Annotate
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      while not String_List.Is_Empty (Files) loop
         Annotate (Ref, String_List.Head (Files));
         String_List.Tail (Files);
      end loop;
   end On_View_Annotate;

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
      MDI      : MDI_Window := Get_MDI (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Child    : MDI_Child;
      Ref      : VCS_Access := Get_Current_Ref (Kernel);

      Dirs     : String_List.List;
      Status   : File_Status_List.List;

   begin
      if Explorer = null then
         Gtk_New (Explorer, Kernel, Ref);
         Set_Size_Request (Explorer, 400, 400);
         Child := Put (MDI, Explorer);
         Set_Title (Child, -"VCS Explorer");

         String_List.Append (Dirs, Get_Current_Dir (Kernel));
         Status :=  Local_Get_Status (Ref, Dirs);
         String_List.Free (Dirs);

         Clear (Explorer);
         Display_File_Status (Kernel, Status, False);

         Widget_Callback.Object_Connect
           (Kernel,
            Context_Changed_Signal,
            On_Context_Changed'Access,
            Explorer);
      else
         Set_Focus_Child (Child);
      end if;
   end On_Open_Interface;

   ---------------------------------
   -- String_Array_To_String_List --
   ---------------------------------

   function String_Array_To_String_List
     (S : String_Id_Array) return String_List.List
   is
      Result : String_List.List;
   begin
      for J in reverse S'Range loop
         String_List.Prepend (Result, Get_String (S (J)));
      end loop;

      return Result;
   end String_Array_To_String_List;

   -------------------------
   -- Get_Dirs_In_Project --
   -------------------------

   function Get_Dirs_In_Project
     (Kernel : Kernel_Handle) return String_List.List
   is
      Result   : String_List.List;
      Project  : Project_Node_Id;
   begin
      Project := Get_Project (Kernel);

      declare
         Iterator : Imported_Project_Iterator := Start (Project, True);
      begin
         while Current (Iterator) /= Empty_Node loop
            String_List.Concat (Result,
                                String_Array_To_String_List
                                (Source_Dirs (Current (Iterator))));
            Next (Iterator);
         end loop;
      end;

      return Result;
   end Get_Dirs_In_Project;

   --------------------------
   -- Get_Files_In_Project --
   --------------------------

   function Get_Files_In_Project
     (Kernel : Kernel_Handle) return String_List.List
   is
      Result  : String_List.List;
      Project : Project_Node_Id;
      Files   : String_Array_Access;
   begin
      Project := Get_Project (Kernel);
      Files   := Get_Source_Files (Project, True);

      for J in reverse Files.all'Range loop
         String_List.Prepend (Result, Files.all (J).all);
      end loop;

      Free (Files);

      return Result;
   end Get_Files_In_Project;

   -------------------
   -- On_Update_All --
   -------------------

   procedure On_Update_All
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
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
         Kernel_Callback.To_Marshaller (On_Update'Access),
         Kernel_Handle (Kernel));

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
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_View_Annotate'Access),
         Kernel_Handle (Kernel));

      Gtk_New (Menu_Item, -"View Changelog");
      Register_Menu (Kernel, "/" & VCS, Menu_Item);
      Kernel_Callback.Connect
        (Menu_Item, "activate",
         Kernel_Callback.To_Marshaller (On_View_Log'Access),
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
         Contextual_Menu_Handler => VCS_Contextual_Menu'Access);
   end Register_Module;

end VCS_Module;
