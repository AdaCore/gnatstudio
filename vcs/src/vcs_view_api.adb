-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
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

with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Arguments;             use Gtk.Arguments;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with VCS;                       use VCS;
with VCS_View_Pkg;              use VCS_View_Pkg;

with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;

with Prj_API;                   use Prj_API;

with String_List_Utils;         use String_List_Utils;

with VCS_Module;                use VCS_Module;
with Log_Utils;                 use Log_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Commands;                  use Commands;
with Commands.VCS;              use Commands.VCS;
with Commands.External;         use Commands.External;

package body VCS_View_API is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff_Local
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Context_Changed
     (Object  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args);
   --  ???

   function Check_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display Head in the console, then return True if List is not
   --  empty, otherwise display List in the console and return False.

   procedure Commit_Files
     (Kernel : Kernel_Handle;
      Files  : String_List.List);
   --  ???

   -------------------
   -- Check_Handler --
   -------------------

   function Check_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      List_Temp : String_List.List_Node := First (List);
      Head_Temp : String_List.List_Node := First (Head);

   begin
      if not String_List.Is_Empty (List) then
         while Head_Temp /= Null_Node loop
            Push_Message (Kernel, Error, Data (Head_Temp));
            Head_Temp := Next (Head_Temp);
         end loop;
      end if;

      while List_Temp /= Null_Node loop
         Push_Message (Kernel, Error, Data (List_Temp));
         List_Temp := Next (List_Temp);
      end loop;

      return String_List.Is_Empty (List);
   end Check_Handler;

   ----------
   -- Open --
   ----------

   procedure Open
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Open (Ref, Files);

      declare
         use String_List;

         L_Temp : List_Node := First (Files);
      begin
         while L_Temp /= Null_Node loop
            Open_File_Editor (Kernel, Data (L_Temp));
            L_Temp := Next (L_Temp);
         end loop;
      end;

      String_List.Free (Files);
   end Open;

   ------------
   -- Update --
   ------------

   procedure Update
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Update (Ref, Files);
      Get_Status (Ref, Copy_String_List (Files));
      String_List.Free (Files);
   end Update;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Open_Explorer (Kernel);
      Get_Status (Ref, Files);
      String_List.Free (Files);
   end Get_Status;

   ---------
   -- Add --
   ---------

   procedure Add
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Add (Ref, Files);
      String_List.Free (Files);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Remove (Ref, Files);
      String_List.Free (Files);
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Revert (Ref, Files);
      String_List.Free (Files);
   end Revert;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) is
   begin
      --  ??? Right now, commit opens a log editor for the file.
      --  We should decide what the correct behavior should be.

      Edit_Log (Widget, Kernel);
   end Commit;

   ---------------
   -- View_Diff --
   ---------------

   procedure View_Diff
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      while not String_List.Is_Empty (Files) loop
         Diff (Ref, String_List.Head (Files));
         String_List.Next (Files);
      end loop;
   end View_Diff;

   --------------
   -- View_Log --
   --------------

   procedure View_Log
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      while not String_List.Is_Empty (Files) loop
         Log (Ref, String_List.Head (Files));
         String_List.Next (Files);
      end loop;
   end View_Log;

   -------------------
   -- View_Annotate --
   -------------------

   procedure View_Annotate
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      while not String_List.Is_Empty (Files) loop
         Annotate (Ref, String_List.Head (Files));
         String_List.Next (Files);
      end loop;
   end View_Annotate;

   --------------
   -- Edit_Log --
   --------------

   procedure Edit_Log
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
   begin
      while not String_List.Is_Empty (Files) loop
         Open_File_Editor
           (Kernel, Get_Log_From_File (Kernel, String_List.Head (Files)));
         String_List.Next (Files);
      end loop;
   end Edit_Log;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Item : Gtk_Menu_Item;

      File      : File_Selection_Context_Access;
      File_Name : File_Name_Selection_Context_Access;

      Kernel    : Kernel_Handle := Get_Kernel (Context);
   begin
      if Context.all in File_Name_Selection_Context'Class then
         File_Name := File_Name_Selection_Context_Access (Context);
      end if;

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);
      end if;

      if File_Name /= null and then
        Has_File_Information (File_Name)
      then
         declare
            File_S : String
              := Directory_Information (File_Name)
              & File_Information (File_Name);
         begin
            if File_S'Length > 4
              and then File_S (File_S'Last - 3 .. File_S'Last) = "_log"
            then
               declare
                  Original : String := Get_File_From_Log (Kernel, File_S);
               begin
                  Set_File_Name_Information
                    (File_Name,
                     Dir_Name (Original),
                     Base_Name (Original));

                  Gtk_New (Item, Label => -"Commit file " & Original);

                  Append (Menu, Item);
                  Context_Callback.Connect
                    (Item, "activate",
                     Context_Callback.To_Marshaller
                     (On_Menu_Commit'Access),
                     Selection_Context_Access (File_Name));
               end;
            else
               Gtk_New (Item, Label => -"Query Status");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Get_Status'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Update");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Update'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Start Editing");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Open'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Diff against head revision");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Diff'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Diff against working revision");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Diff_Local'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Edit changelog");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Edit_Log'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Commit");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (On_Menu_Commit'Access),
                  Selection_Context_Access (Context));
            end if;
         end;
      end if;

      if File_Name /= null
        and then Has_File_Information (File_Name)
        and then (Has_Directory_Information (File_Name)
                  or else (File /= null
                           and then Has_Project_Information (File)))
      then
         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      if File_Name /= null
        and then Has_Directory_Information (File_Name)
      then
         Gtk_New (Item, Label => -"Query status for directory");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Get_Status_Dir'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"Update directory");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update_Dir'Access),
            Selection_Context_Access (Context));
      end if;

      if File /= null
        and then Has_Project_Information (File)
        and then Has_Directory_Information (File_Name)
      then
         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      if File /= null
        and then Has_Project_Information (File)
      then
         Gtk_New (Item, Label => -"Query status for project");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Get_Status_Project'Access),
            Selection_Context_Access (Context));

         Gtk_New (Item, Label => -"Update project");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update_Project'Access),
            Selection_Context_Access (Context));
      end if;
   end VCS_Contextual_Menu;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Object : access Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
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
            Display_File_Status (Get_Kernel (Context), Status, False, True);
            File_Status_List.Free (Status);
         end if;
      end if;
   end On_Context_Changed;

   -------------------
   -- Open_Explorer --
   -------------------

   procedure Open_Explorer
     (Kernel : Kernel_Handle)
   is
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
         Display_File_Status (Kernel, Status, False, True);
         File_Status_List.Free (Status);

         Widget_Callback.Object_Connect
           (Kernel,
            Context_Changed_Signal,
            On_Context_Changed'Access,
            Explorer);
      end if;
   end Open_Explorer;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      File     : File_Name_Selection_Context_Access;
      List     : String_List.List;
      Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle := Get_Kernel (Context);
   begin
      if Context.all in File_Name_Selection_Context'Class then
         File := File_Name_Selection_Context_Access (Context);

         if Get_Creator (Context) = VCS_Module_ID then
            Explorer := Get_Explorer (Kernel);
            List := Get_Selected_Files (Explorer);
         else
            if Has_File_Information (File) then
               String_List.Append
                 (List,
                  Directory_Information (File) & File_Information (File));
            end if;
         end if;

         while not String_List.Is_Empty (List) loop
            Open_File_Editor
              (Kernel, Get_Log_From_File (Kernel, String_List.Head (List)));
            String_List.Next (List);
         end loop;
      end if;
   end On_Menu_Edit_Log;

   ------------------
   -- Commit_Files --
   ------------------

   procedure Commit_Files
     (Kernel : Kernel_Handle;
      Files  : String_List.List)
   is
      use String_List;

      Logs               : String_List.List;
      Files_Temp         : List_Node := First (Files);

      Commit_Command     : Commit_Command_Access;
      Get_Status_Command : Get_Status_Command_Access;

      Check_File         : External_Command_Access;
      Check_Log          : External_Command_Access;

      Command            : String_List.List;
      Args               : String_List.List;

      File_Check_Script  : constant String :=
        Get_Pref (Kernel, VCS_Commit_File_Check);

      Log_Check_Script   : constant String :=
        Get_Pref (Kernel, VCS_Commit_Log_Check);

      Ref                : VCS_Access := Get_Current_Ref (Kernel);
   begin
      while Files_Temp /= Null_Node loop
         Append (Logs, Get_Log (Kernel, Head (Files)));
         Files_Temp := Next (Files_Temp);
      end loop;

      Create (Commit_Command, Ref, Files, Logs);

      Create (Get_Status_Command, Ref, Files);

      if File_Check_Script /= "" then
         Append (Command, File_Check_Script);
         Append (Args, Head (Files));

         Create (Check_File,
                 Kernel,
                 Command,
                 Null_List,
                 Args,
                 Null_List,
                 Check_Handler'Access);
      end if;

      Files_Temp := First (Files);
      --  ??? Right now, we only commit the first file in the list.

      if Log_Check_Script /= "" then
         declare
            Log_File  : constant String
              := Get_Log_From_File (Kernel, Data (Files_Temp));
            Head_List : List;
         begin
            Free (Command);
            Append (Command, Log_Check_Script);

            Free (Args);
            Append (Args, Log_File);

            Append (Head_List, -"File: " & Head (Files));
            Append (Head_List,
                    -"The changelog provided does not pass the checks.");

            Create
              (Check_Log,
               Kernel,
               Command,
               Null_List,
               Args,
               Head_List,
               Check_Handler'Access);
         end;
      end if;

      if File_Check_Script = ""
        and then Log_Check_Script = ""
      then
         --  No log check, no file check.

         Enqueue (Get_Queue (Ref), Commit_Command);
      else
         if Log_Check_Script /= "" then
            Add_Consequence_Action
              (Command_Access (Check_Log), Command_Access (Commit_Command));
            if File_Check_Script /= "" then
               --  Log check and file check.

               Add_Consequence_Action
                 (Command_Access (Check_File), Command_Access (Check_Log));
               Enqueue (Get_Queue (Ref), Check_File);
            else
               --  Log check, no file check.

               Enqueue (Get_Queue (Ref), Check_Log);
            end if;
         else
            --  No log check, file check.

            Add_Consequence_Action
              (Command_Access (Check_File), Command_Access (Commit_Command));

            Enqueue (Get_Queue (Ref), Check_File);
         end if;
      end if;

      Enqueue (Get_Queue (Ref), Get_Status_Command);

      Free (Logs);
      Free (Command);
      Free (Args);
   end Commit_Files;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File     : File_Name_Selection_Context_Access;
      Kernel   : Kernel_Handle := Get_Kernel (Context);
      Files    : String_List.List;
   begin
      if Get_Creator (Context) = VCS_Module_ID then
         Commit (Widget, Kernel);

      elsif Context.all in File_Name_Selection_Context'Class then
         File := File_Name_Selection_Context_Access (Context);

         if Has_File_Information (File) then
            String_List.Append (Files,
                                Directory_Information (File)
                                & File_Information (File));

            Commit_Files (Kernel, Files);
         end if;
      end if;

      String_List.Free (Files);
   end On_Menu_Commit;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      File     : File_Name_Selection_Context_Access;
      List     : String_List.List;
      Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle := Get_Kernel (Context);
   begin
      if Context.all in File_Name_Selection_Context'Class then
         File := File_Name_Selection_Context_Access (Context);

         if Get_Creator (Context) = VCS_Module_ID then
            Explorer := Get_Explorer (Kernel);
            List := Get_Selected_Files (Explorer);
         else
            if Has_File_Information (File) then
               String_List.Append (List,
                                   Directory_Information (File)
                                   & File_Information (File));
            end if;
         end if;

         Open (Get_Current_Ref (Kernel), List);

         declare
            use String_List;

            L_Temp : List_Node := First (List);
         begin
            while L_Temp /= Null_Node loop
               Open_File_Editor (Kernel, Data (L_Temp));
               L_Temp := Next (L_Temp);
            end loop;
         end;

         String_List.Free (List);
      end if;
   end On_Menu_Open;

   --------------------
   -- On_Menu_Update --
   --------------------

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Update (Widget, Get_Kernel (Context));
   end On_Menu_Update;

   ------------------------
   -- On_Menu_Get_Status --
   ------------------------

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Get_Status (Widget, Get_Kernel (Context));
   end On_Menu_Get_Status;

   ------------------------
   -- On_Menu_Update_Dir --
   ------------------------

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;

   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File_Context) then
            String_List.Append (Files, Directory_Information (File_Context));
            Update (Get_Current_Ref (Get_Kernel (Context)), Files);
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Update_Dir;

   ----------------------------
   -- On_Menu_Get_Status_Dir --
   ----------------------------

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files        : String_List.List;
      File_Context : File_Name_Selection_Context_Access;
   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Name_Selection_Context'Class then
         File_Context := File_Name_Selection_Context_Access (Context);

         if Has_Directory_Information (File_Context) then
            String_List.Append (Files, Directory_Information (File_Context));
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Get_Status_Dir;

   ----------------------------
   -- On_Menu_Update_Project --
   ----------------------------

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;

   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Files := Get_Files_In_Project
              (Get_Project_From_View (Project_Information (File_Context)));
            Update (Get_Current_Ref (Get_Kernel (Context)), Files);
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Update_Project;

   --------------------------------
   -- On_Menu_Get_Status_Project --
   --------------------------------

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      use String_List;


      Files        : String_List.List;
      Files_Temp   : String_List.List_Node;
      File_Context : File_Selection_Context_Access;
      Blank_Status : File_Status_Record;
      Current_Status : File_Status_Record;

      Status       : File_Status_List.List;
      Kernel       : Kernel_Handle := Get_Kernel (Context);
   begin
      Open_Explorer (Get_Kernel (Context));
      Clear (Get_Explorer (Get_Kernel (Context)));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Files := Get_Files_In_Project
              (Get_Project_From_View (Project_Information
                                      (File_Context)));

            Files_Temp := String_List.First (Files);

            while Files_Temp /= String_List.Null_Node loop
               Current_Status := Blank_Status;
               Append (Current_Status.File_Name,
                       String_List.Data (Files_Temp));
               Files_Temp := String_List.Next (Files_Temp);
               File_Status_List.Append (Status, Current_Status);
            end loop;

            Display_File_Status (Kernel, Status, False, True);
            File_Status_List.Free (Status);

            Get_Status (Get_Current_Ref (Kernel), Files);
         end if;
      end if;
   end On_Menu_Get_Status_Project;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      View_Diff (Widget, Get_Kernel (Context));
   end On_Menu_Diff;

   ------------------------
   -- On_Menu_Diff_Local --
   ------------------------

   procedure On_Menu_Diff_Local
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      use File_Status_List;
      pragma Unreferenced (Widget);

      Files  : String_List.List := Get_Selected_Files (Get_Kernel (Context));
      Ref    : VCS_Access := Get_Current_Ref (Get_Kernel (Context));
      Status : File_Status_List.List := Local_Get_Status (Ref, Files);
      Status_Temp : List_Node := First (Status);

   begin
      while Status_Temp /= Null_Node loop
         if not String_List.Is_Empty (Data (Status_Temp).Working_Revision) then
            Diff (Ref,
                  String_List.Head (Data (Status_Temp).File_Name),
                  String_List.Head (Data (Status_Temp).Working_Revision));
         end if;

         Status_Temp := Next (Status_Temp);
      end loop;

      String_List.Free (Files);
   end On_Menu_Diff_Local;

end VCS_View_API;
