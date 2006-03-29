-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
--                              AdaCore                              --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;               use GNAT;

with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Menu_Item;             use Gtk.Menu_Item;

with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Commands;                  use Commands;
with Log_Utils;                 use Log_Utils;
with String_List_Utils;         use String_List_Utils;
with Traces;                    use Traces;
with VCS;                       use VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Module;                use VCS_Module;
with VCS_View.Explorer;
with VCS_View;                  use VCS_View;
with VFS;                       use VFS;

package body VCS_Activities_View_API is

   procedure On_Menu_Create_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Delete_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Commit_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Query_Status_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Update_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Diff_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Remove_From_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Toggle_Group_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Build_Patch_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   --  Action to open a file

   type Edit_Action_Command_Type is new Root_Command with record
      Kernel : Kernel_Handle;
      File   : Virtual_File;
   end record;
   type Edit_Action_Command_Access is access Edit_Action_Command_Type;

   function Execute
     (Command : access Edit_Action_Command_Type) return Command_Return_Type;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_Action_Command_Type) return Command_Return_Type
   is
      Filename : aliased String := Full_Name (Command.File).all;
   begin
      Execute_GPS_Shell_Command
        (Command.Kernel, "Editor.edit", (1 => Filename'Unchecked_Access));
      return Success;
   end Execute;

   ---------------------------------
   -- On_Menu_Toggle_Group_Commit --
   ---------------------------------

   procedure On_Menu_Toggle_Group_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      Activity  : Activity_Id;
   begin
      Activity := Value (Activity_Information (Context));
      Toggle_Group_Commit (Kernel, Activity);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Toggle_Group_Commit;

   -----------------------------
   -- On_Menu_Create_Activity --
   -----------------------------

   procedure On_Menu_Create_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      On_Create_Activity (Kernel);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Create_Activity;

   -----------------------------
   -- On_Menu_Delete_Activity --
   -----------------------------

   procedure On_Menu_Delete_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      On_Delete_Activity (Kernel, Value (Activity_Information (Context)));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Delete_Activity;

   -----------------------------
   -- On_Menu_Add_To_Activity --
   -----------------------------

   procedure On_Menu_Add_To_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      Activity  : Activity_Id;
   begin
      if Has_Activity_Information (Context) then
         Activity := Value (Activity_Information (Context));

         if Has_File_Information (Context)
           or else Has_Directory_Information (Context)
         then
            --  If we have a file information, then there is a single file to
            --  handle.
            Add_File (Kernel, Activity, File_Information (Context));

         else
            --  We have no file information, use the selected file in the VCS
            --  explorer.

            declare
               use type String_List.List_Node;
               Files    : String_List.List :=
                            VCS_View.Explorer.Get_Selected_Files (Kernel);
               File     : Virtual_File;
               Files_It : String_List.List_Node;
            begin
               Files_It := String_List.First (Files);

               while Files_It /= String_List.Null_Node loop
                  File := Create
                    (Full_Filename => String_List.Data (Files_It));
                  Add_File (Kernel, Activity, File);

                  Files_It := String_List.Next (Files_It);
               end loop;

               String_List.Free (Files);
            end;
         end if;

         Refresh (Get_Activities_Explorer (Kernel, False, False));
         Refresh (Get_Explorer (Kernel, False, False));
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Add_To_Activity;

   ----------------------------------
   -- On_Menu_Remove_From_Activity --
   ----------------------------------

   procedure On_Menu_Remove_From_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      use type String_List.List_Node;

      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      File     : Virtual_File;
      Files    : String_List.List;
      Files_It : String_List.List_Node;
   begin
      Files := Get_Selected_Files
        (VCS_View_Access (Get_Activities_Explorer (Kernel, False)));

      Files_It := String_List.First (Files);

      while Files_It /= String_List.Null_Node loop
         File := Create (Full_Filename => String_List.Data (Files_It));
         On_Remove_From_Activity (Kernel, Get_File_Activity (File), File);

         Files_It := String_List.Next (Files_It);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Remove_From_Activity;

   -----------------------------------
   -- On_Menu_Query_Status_Activity --
   -----------------------------------

   procedure On_Menu_Query_Status_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      Activity  : constant Activity_Id :=
                    Value (Activity_Information (Context));
      Files     : String_List.List;
   begin
      Files := Get_Files_In_Activity (Activity);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      Get_Status (Get_VCS_For_Activity (Kernel, Activity), Files);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Query_Status_Activity;

   -----------------------------
   -- On_Menu_Update_Activity --
   -----------------------------

   procedure On_Menu_Update_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      Activity  : constant Activity_Id :=
                    Value (Activity_Information (Context));
      Files     : String_List.List;
   begin
      Files := Get_Files_In_Activity (Activity);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      Update (Get_VCS_For_Activity (Kernel, Activity), Files);
      Get_Status (Get_VCS_For_Activity (Kernel, Activity), Files);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Update_Activity;

   -----------------------------
   -- On_Menu_Commit_Activity --
   -----------------------------

   procedure On_Menu_Commit_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel         : constant Kernel_Handle := Get_Kernel (Context);
      Activity       : constant Activity_Id :=
                         Value (Activity_Information (Context));
      Files          : String_List.List;
      All_Logs_Exist : Boolean := True;

      use String_List;
      use type String_List.List_Node;
   begin
      Files := Get_Files_In_Activity (Activity);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      --  Check if we have a log for the activity

      if not Has_Log (Kernel, Activity) then
         All_Logs_Exist := False;

         Open_File_Editor
           (Kernel,
            Get_Log_File (Kernel, Activity),
            Group            => Group_Consoles,
            Initial_Position => Position_Bottom);
      end if;

      --  If we have a log, commit the whole lot

      if All_Logs_Exist then
         Log_Action_Files
           (Kernel, Get_VCS_For_Activity (Kernel, Activity),
            Commit, Files, Activity);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Commit_Activity;

   ---------------------------
   -- On_Menu_Diff_Activity --
   ---------------------------

   procedure On_Menu_Diff_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      Activity  : constant Activity_Id :=
                    Value (Activity_Information (Context));
      VCS       : constant VCS_Access :=
                    Get_VCS_For_Activity (Kernel, Activity);
      Files     : constant String_List.List :=
                    Get_Files_In_Activity (Activity);
      Iter      : String_List.List_Node;
   begin
      --  For each file we get the diff

      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         Diff (VCS, Create (String_List.Data (Iter)));
         Iter := String_List.Next (Iter);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Activity;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      List      : String_List.List;
      Activity  : Activity_Id;

   begin
      if Context /= No_Context then
         if Has_File_Information (Context) then
            --  This is a file

            List := Get_Selected_Files (Kernel);

            while not String_List.Is_Empty (List) loop
               declare
                  File : constant Virtual_File :=
                           Create (String_List.Head (List));
               begin
                  Get_Log_From_ChangeLog (Kernel, File);

                  Open_File_Editor
                    (Kernel,
                     Get_Log_From_File (Kernel, File, True),
                     Group            => Group_Consoles,
                     Initial_Position => Position_Bottom);
               end;

               String_List.Next (List);
            end loop;

         else
            --  This is an activity line

            Activity := Value (Activity_Information (Context));

            Open_File_Editor
              (Kernel,
               Get_Log_File (Kernel, Activity),
               Group            => Group_Consoles,
               Initial_Position => Position_Bottom);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Edit_Log;

   ---------------------------------
   -- On_Menu_Close_Open_Activity --
   ---------------------------------

   procedure On_Menu_Close_Open_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      On_Close_Open_Activity (Kernel, Value (Activity_Information (Context)));
   end On_Menu_Close_Open_Activity;

   ------------------------------
   -- On_Menu_Build_Patch_File --
   ------------------------------

   procedure On_Menu_Build_Patch_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);

      procedure Append_Indent (Str : String);
      --  Append Str into the patch file content, add Tab before each new line

      Tab       : constant String := "        ";
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      Logs_Dir  : constant String := Get_Home_Dir (Kernel) & "log_files";
      Activity  : constant Activity_Id :=
                    Value (Activity_Information (Context));
      VCS       : constant VCS_Access :=
                    Get_VCS_For_Activity (Kernel, Activity);
      Files     : constant String_List.List :=
                    Get_Files_In_Activity (Activity);
      Filename  : constant String := Logs_Dir & OS_Lib.Directory_Separator &
                    Image (Activity) & ".dif";
      File      : Virtual_File;
      W_File    : Writable_File;
      Content   : Unbounded_String;
      Iter      : String_List.List_Node;

      Edit_File : Edit_Action_Command_Access;

      ------------
      -- Indent --
      ------------

      procedure Append_Indent (Str : String) is
      begin
         if Str'Length = 0 then
            return;
         end if;

         if Str (Str'First) /= ASCII.LF then
            Append (Content, Tab);
         end if;

         for K in Str'Range loop
            Append (Content, Str (K));

            if Str (K) = ASCII.LF then
               Append (Content, Tab);
            end if;
         end loop;
         Append (Content, ASCII.LF);
      end Append_Indent;

   begin
      File := Create (Filename);
      W_File := Write_File (File);

      --  Add activity name and log

      Append (Content, ASCII.LF);
      Append (Content,
              "This is a patch file for a GPS activity." & ASCII.LF);
      Append (Content,
              "It has been generated on " & Image (Clock, ISO_Date) &
              '.' & ASCII.LF & ASCII.LF);
      Append (Content,
              "Activity : " & Get_Name (Activity) & ASCII.LF & ASCII.LF);

      Append_Indent (Get_Log (Kernel, Activity));

      --  Add file logs

      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         declare
            use type String_List.List_Node;
            File : constant Virtual_File := Create (String_List.Data (Iter));
         begin
            Append (Content, Tab & "* " & Base_Name (File) & ASCII.LF);
            Append_Indent (Get_Log (Kernel, File));
            Append (Content, ASCII.LF);
            Iter := String_List.Next (Iter);
         end;
      end loop;

      Write (W_File, To_String (Content));
      Close (W_File);

      --  For each file we get the diff

      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         Diff_Patch (VCS, Create (String_List.Data (Iter)), File);
         Iter := String_List.Next (Iter);
      end loop;

      --  Add the last command that will open the file containing the patch

      Edit_File := new Edit_Action_Command_Type;
      Edit_File.Kernel := Kernel;
      Edit_File.File := File;

      Launch_Background_Command
        (Kernel, Edit_File, True, True, Name (VCS));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Build_Patch_File;

   -------------------
   -- Open_Explorer --
   -------------------

   procedure Open_Activities_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context)
   is
      pragma Unreferenced (Context);
      Explorer : VCS_Activities_View_Access;
   begin
      Explorer := Get_Activities_Explorer (Kernel, True, True);
      Clear (Explorer);
      Query_Activities_Files (Explorer, Kernel, Real_Query => False);
   end Open_Activities_Explorer;

   ----------------------------
   -- Query_Activities_Files --
   ----------------------------

   procedure Query_Activities_Files
     (Explorer   : VCS_Activities_View_Access;
      Kernel     : Kernel_Handle;
      Real_Query : Boolean)
   is
      pragma Unreferenced (Explorer);

      VCS      : VCS_Access;
      Activity : Activity_Id;
      Files    : String_List.List;
      Status   : File_Status_List.List;
   begin
      Activity := First;

      while Activity /= No_Activity loop
         Files := Get_Files_In_Activity (Activity);
         VCS   := Get_VCS_For_Activity (Kernel, Activity);

         if String_List.Is_Empty (Files) or else VCS = null then
            Display_File_Status
              (Kernel, Activity, File_Status_List.Null_List, VCS, True);

         else
            if Real_Query then
               Get_Status (VCS, Files);

            else
               Status := Local_Get_Status (VCS, Files);
               Display_File_Status
                 (Kernel, Activity, Status, VCS, False, True);
               File_Status_List.Free (Status);
            end if;
         end if;

         Activity := Next;
      end loop;
   end Query_Activities_Files;

   procedure Query_Activities_Files
     (Kernel     : Kernel_Handle;
      Real_Query : Boolean) is
   begin
      Query_Activities_Files
        (Get_Activities_Explorer (Kernel), Kernel, Real_Query);
   end Query_Activities_Files;

   ------------------
   -- Query_Status --
   ------------------

   procedure Query_Status
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_Activities_View_Access;
   begin
      Open_Activities_Explorer (Kernel, No_Context);
      Explorer := Get_Activities_Explorer (Kernel);
      Query_Activities_Files (Explorer, Kernel, True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Query_Status;

   ------------------------------------
   -- VCS_Activities_Contextual_Menu --
   ------------------------------------

   procedure VCS_Activities_Contextual_Menu
     (Kernel  : Kernel_Handle;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      File_Section     : Boolean;
      Activity_Section : Boolean;
      Active           : Boolean;
      Item             : Gtk_Menu_Item;
      Check            : Gtk_Check_Menu_Item;
      Activity         : Activity_Id;
      VCS              : VCS_Access;

   begin
      --  Determine which sections should be displayed

      if Has_Activity_Information (Context) then
         Activity_Section := not Has_File_Information (Context)
           and then not Has_Directory_Information (Context);
         Activity := Value (Activity_Information (Context));
         Active := not Is_Closed (Activity);
         File_Section     := Has_File_Information (Context)
           or else Has_Directory_Information (Context);
      else
         Activity_Section := False;
         File_Section     := False;
      end if;

      if Activity_Section then
         VCS := Get_VCS_For_Activity (Kernel, Activity);

         Gtk_New (Check, Label => -"Group commit");
         Set_Active (Check, Get_Group_Commit (Activity));
         Append (Menu, Check);
         Context_Callback.Connect
           (Check, "activate", On_Menu_Toggle_Group_Commit'Access, Context);
         Set_Sensitive
           (Check,
            VCS /= null
            and then Absolute_Filenames_Supported (VCS)
            and then Atomic_Commands_Supported (VCS)
            and then not Is_Closed (Activity));

         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      Gtk_New (Item, Label => -"Create new activity");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate", On_Menu_Create_Activity'Access, Context);
      Set_Sensitive (Item, True);

      if Activity_Section then
         if Is_Closed (Activity) then
            Gtk_New (Item, Label => -"Re-open activity");
         else
            Gtk_New (Item, Label => -"Close activity");
         end if;

         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Close_Open_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item, Label => -"Delete activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Delete_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item);
         Append (Menu, Item);

         Gtk_New (Item, Label => -"Commit activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Commit_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Query status");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Query_Status_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Update");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Update_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Compare against head revision");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Diff_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Build patch file");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Build_Patch_File'Access, Context);
         Set_Sensitive (Item, Active);
      end if;

      if Activity_Section or else File_Section then
         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      if File_Section then
         Gtk_New (Item, Label => -"Remove from activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Remove_From_Activity'Access, Context);
         Set_Sensitive (Item, Active);
      end if;

      if Activity_Section then
         --  Fill the section relative to files

         Gtk_New (Item, Label => -"Edit revision log");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Edit_Log'Access, Context);
         Set_Sensitive (Item, True);
      end if;
   end VCS_Activities_Contextual_Menu;

end VCS_Activities_View_API;
