------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Directories;           use Ada.Directories;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.Strings;              use GNAT;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

with Templates_Parser;          use Templates_Parser;

with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;

with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Commands;                  use Commands;
with Log_Utils;                 use Log_Utils;
with Projects;                  use Projects;
with String_List_Utils;         use String_List_Utils;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with UTF8_Utils;                use UTF8_Utils;
with VCS;                       use VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Module;                use VCS_Module;
with VCS_View;                  use VCS_View;

package body VCS_Activities_View_API is
   Me : constant Trace_Handle := Create ("GPS.VCS.VCS_ACTIVITIES_API");

   procedure Commit_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Commit the given activity

   procedure Query_Status_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Query status of the given activity

   procedure Update_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Update the given activity

   procedure Diff_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Diff the given activity against head

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

   procedure On_Menu_Toggle_Group_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   procedure On_Edit_Log
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Open activity log

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   procedure On_Remove_Log
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);
   --  Delete activity log file

   procedure On_Menu_Build_Patch_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   procedure On_Build_Patch_File
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id);

   procedure Populate_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id;
      Context  : Selection_Context);
   --  Add file in context or files selected on the VCS explorer into the given
   --  activity.

   procedure Apply
     (Context : Selection_Context;
      Action  : access
        procedure (Kernel   : not null access Kernel_Handle_Record'Class;
                   Activity : Activity_Id));
   --  Apply Action on the activity in Context or all activities selected on
   --  the Activity Explorer.

   --  Action to open a file

   type Edit_Action_Command_Type is new Root_Command with record
      Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File;
   end record;
   type Edit_Action_Command_Access is access Edit_Action_Command_Type;

   overriding function Execute
     (Command : access Edit_Action_Command_Type) return Command_Return_Type;

   --  Action adjust patch root

   type Adjust_Patch_Action_Command_Type is new Root_Command with record
      Kernel        : access Kernel_Handle_Record'Class;
      Patch_File    : Virtual_File;      -- patch file
      Files         : File_Array_Access; -- all files contained in the patch
      Root_Dir      : Virtual_File;
      --  The patch root directory to set
      Header_Length : Positive;         -- the character length of the header
   end record;
   type Adjust_Patch_Action_Command_Access is
     access Adjust_Patch_Action_Command_Type;

   overriding function Execute
     (Command : access Adjust_Patch_Action_Command_Type)
      return Command_Return_Type;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Context : Selection_Context;
      Action  : access
        procedure (Kernel   : not null access Kernel_Handle_Record'Class;
                   Activity : Activity_Id))
   is
   begin
      if Context /= No_Context then
         declare
            Kernel : constant Kernel_Handle := Get_Kernel (Context);
            List   : constant String_List.Vector :=
              Activity_Information (Context);
         begin
            for Item of List loop
               Action (Kernel, Value (Item));
            end loop;
         end;
      end if;
   end Apply;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Action_Command_Type) return Command_Return_Type
   is
      CL : Arg_List := Create ("Editor.edit");
   begin
      --  ??? We should use the Editors API
      Append_Argument (CL, +Full_Name (Command.File), One_Arg);
      Execute_GPS_Shell_Command (Command.Kernel, CL);
      return Success;
   end Execute;

   overriding function Execute
     (Command : access Adjust_Patch_Action_Command_Type)
      return Command_Return_Type
   is
      procedure Handle (Filename : Virtual_File);
      --  Handle this filename in the patch

      Root_Dir      : constant Filesystem_String :=
                        +To_Lower (+Command.Root_Dir.Full_Name (True));
      Patch         : Strings.String_Access := Read_File (Command.Patch_File);
      Patch_Content : Unbounded_String := To_Unbounded_String (Patch.all);

      ------------
      -- Handle --
      ------------

      procedure Handle (Filename : Virtual_File) is
         BF : constant String := Simple_Name (+Filename.Full_Name);
         LF : constant String := To_Lower (+Filename.Full_Name);
         NF : String := +Filename.Full_Name;

         procedure Replace_Token (Prefix, Token, By : String);
         --  Replaces the slice matching ^<prefix>.*<token> by By string

         -------------------
         -- Replace_Token --
         -------------------

         procedure Replace_Token (Prefix, Token, By : String) is
            First, Last : Natural := 1;
         begin
            First := Command.Header_Length;

            Do_Replace : loop
               Last := Index (Patch_Content, Token, From => First);

               if Last = 0 then
                  --  No more match, exit now, this should not append with a
                  --  properly formated patch as we exit when the first match
                  --  is found on the patch file.
                  exit Do_Replace;

               else
                  --  Look for the start of the line

                  First := Index
                    (Patch_Content, String'(1 => ASCII.LF),
                     From  => Last,
                     Going => Ada.Strings.Backward);

                  if First /= 0
                    and then Slice
                      (Patch_Content, First + 1, First + Prefix'Length)
                      = Prefix
                  then
                     First := First + Prefix'Length + 1;
                     Last := Last + Token'Length - 1;

                     Replace_Slice (Patch_Content, First, Last, By);
                     exit Do_Replace;

                  else
                     --  Do not macth, check for next macth
                     First := Last + Token'Length;
                  end if;
               end if;
            end loop Do_Replace;
         end Replace_Token;

      begin
         --  Use forward slash into the patch file, this is fine for UNIXes and
         --  Windows.

         for K in NF'Range loop
            if NF (K) = '\' then
               NF (K) := '/';
            end if;
         end loop;

         if Equal (+LF (LF'First .. LF'First + Root_Dir'Length - 1),
                   Root_Dir)
         then
            Replace_Token
              ("--- ", BF, NF (NF'First + Root_Dir'Length .. NF'Last));
            Replace_Token
              ("*** ", BF, NF (NF'First + Root_Dir'Length .. NF'Last));
         end if;
      end Handle;

   begin
      Strings.Free (Patch);

      for J in Command.Files'Range loop
         Handle (Command.Files (J));
      end loop;

      declare
         W_File : Writable_File := Write_File (Command.Patch_File);
      begin
         Write (W_File, To_String (Patch_Content));
         Close (W_File);
      end;

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
   begin
      Apply (Context, Toggle_Group_Commit'Access);
   exception
      when E : others => Trace (Me, E);
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
      when E : others => Trace (Me, E);
   end On_Menu_Create_Activity;

   -----------------------------
   -- On_Menu_Delete_Activity --
   -----------------------------

   procedure On_Menu_Delete_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, On_Delete_Activity'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Delete_Activity;

   -----------------------
   -- Populate_Activity --
   -----------------------

   procedure Populate_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id;
      Context  : Selection_Context) is
   begin
      if Has_File_Information (Context)
        or else Has_Directory_Information (Context)
      then
         declare
            Files : constant File_Array := File_Information (Context);
         begin
            for K in Files'Range loop
               Add_File (Kernel, Activity, Files (K));
            end loop;
         end;
      end if;
   end Populate_Activity;

   -----------------------------
   -- On_Menu_Add_To_Activity --
   -----------------------------

   procedure On_Menu_Add_To_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : Activity_Id;
   begin
      if Has_Activity_Information (Context) then
         Activity := Value (Activity_Information (Context).First_Element);

         Populate_Activity (Kernel, Activity, Context);

         Refresh (Get_Activities_Explorer (Kernel, False, False));
         Refresh (Get_Explorer (Kernel, False, False));
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Menu_Add_To_Activity;

   --------------------------------
   -- On_Menu_Commit_As_Activity --
   --------------------------------

   procedure On_Menu_Commit_As_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : constant Activity_Id := New_Activity (Kernel);
   begin
      Set_Name (Kernel, Activity, "*anonymous*");

      Populate_Activity (Kernel, Activity, Context);

      Open_Activities_Explorer (Kernel, Context);

      Refresh (Get_Activities_Explorer (Kernel, False, False));
      Refresh (Get_Explorer (Kernel, False, False));

      Commit_Activity (Kernel, Activity);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Commit_As_Activity;

   ----------------------------------
   -- On_Menu_Remove_From_Activity --
   ----------------------------------

   procedure On_Menu_Remove_From_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_File_Information (Context)
        or else Has_Directory_Information (Context)
      then
         declare
            Files : constant File_Array := File_Information (Context);
         begin
            for K in Files'Range loop
               On_Remove_From_Activity
                 (Kernel, Get_File_Activity (Files (K)), Files (K));
            end loop;
         end;
      end if;
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Remove_From_Activity;

   ---------------------------
   -- Query_Status_Activity --
   ---------------------------

   procedure Query_Status_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Files : constant File_Array := Get_Files_In_Activity (Activity);
   begin

      if Files'Length = 0 then
         Kernel.Insert
           (-"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      Get_Status (Get_VCS_For_Activity (Kernel, Activity), Files);
   end Query_Status_Activity;

   -----------------------------------
   -- On_Menu_Query_Status_Activity --
   -----------------------------------

   procedure On_Menu_Query_Status_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, Query_Status_Activity'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Query_Status_Activity;

   ---------------------
   -- Update_Activity --
   ---------------------

   procedure Update_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Files : constant File_Array := Get_Files_In_Activity (Activity);
   begin
      if Files'Length = 0 then
         Kernel.Insert
           (-"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      Update (Get_VCS_For_Activity (Kernel, Activity), Files);

      if Implicit_Status.Get_Pref then
         Get_Status (Get_VCS_For_Activity (Kernel, Activity), Files);
      end if;
   end Update_Activity;

   -----------------------------
   -- On_Menu_Update_Activity --
   -----------------------------

   procedure On_Menu_Update_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, Update_Activity'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Update_Activity;

   ---------------------
   -- Commit_Activity --
   ---------------------

   procedure Commit_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Files          : constant File_Array := Get_Files_In_Activity (Activity);
      All_Logs_Exist : Boolean := True;

   begin
      if Files'Length = 0 then
         Kernel.Insert
           (-"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      --  Check if we have a log for the activity

      if not Has_Log (Kernel, Activity) then
         All_Logs_Exist := False;
         Open_File_Action_Hook.Run
           (Kernel,
            Get_Log_File (Kernel, Activity),
            Project          => No_Project,
            Group            => Group_Consoles,
            Initial_Position => Position_Bottom);
      end if;

      --  If we have a log, commit the whole lot

      if All_Logs_Exist then
         Log_Action_Files
           (Kernel, Get_VCS_For_Activity (Kernel, Activity),
            Commit, Files, Activity);
      end if;
   end Commit_Activity;

   -----------------------------
   -- On_Menu_Commit_Activity --
   -----------------------------

   procedure On_Menu_Commit_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, Commit_Activity'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Commit_Activity;

   -------------------
   -- Diff_Activity --
   -------------------

   procedure Diff_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      VCS   : constant VCS_Access := Get_VCS_For_Activity (Kernel, Activity);
      Files : constant File_Array := Get_Files_In_Activity (Activity);
   begin
      --  For each file we get the diff
      for J in Files'Range loop
         Diff (VCS, Files (J));
      end loop;
   end Diff_Activity;

   ---------------------------
   -- On_Menu_Diff_Activity --
   ---------------------------

   procedure On_Menu_Diff_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, Diff_Activity'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Diff_Activity;

   -----------------
   -- On_Edit_Log --
   -----------------

   procedure On_Edit_Log
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id) is
   begin
      Open_File_Action_Hook.Run
        (Kernel,
         Get_Log_File (Kernel, Activity),
         Project          => No_Project,
         Group            => Group_Consoles,
         Initial_Position => Position_Bottom,
         Title            => Get_Name (Activity) & " [activity log]");
   end On_Edit_Log;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      List   : File_Array_Access;

   begin
      if Context /= No_Context then
         if Has_File_Information (Context) then
            --  This is a file

            List := Get_Selected_Files (Kernel);

            for J in List'Range loop
               declare
                  File : Virtual_File renames List (J);
               begin
                  Get_Log_From_ChangeLog (Kernel, File);

                  Open_File_Action_Hook.Run
                    (Kernel,
                     Get_Log_From_File (Kernel, File, True),
                     Project          => No_Project,
                     Group            => Group_Consoles,
                     Initial_Position => Position_Bottom);
               end;
            end loop;

            Unchecked_Free (List);

         else
            --  This is an activity line

            Apply (Context, On_Edit_Log'Access);
         end if;
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Menu_Edit_Log;

   -------------------
   -- On_Remove_Log --
   -------------------

   procedure On_Remove_Log
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Log_File : constant Virtual_File := Get_Log_File (Kernel, Activity);
      Success  : Boolean;
      pragma Unreferenced (Success);
   begin
      Open_File_Action_Hook.Run
         (Kernel, File => Log_File, Project => No_Project,
          Line => -1);  --  close all editors
      Delete (Log_File, Success);
      Query_Status (null, Kernel);
   end On_Remove_Log;

   ------------------------
   -- On_Menu_Remove_Log --
   ------------------------

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, On_Remove_Log'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Remove_Log;

   ---------------------------------
   -- On_Menu_Close_Open_Activity --
   ---------------------------------

   procedure On_Menu_Close_Open_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, On_Close_Open_Activity'Access);
   end On_Menu_Close_Open_Activity;

   -------------------------
   -- On_Build_Patch_File --
   -------------------------

   procedure On_Build_Patch_File
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Logs_Dir     : constant Virtual_File :=
                       Create_From_Dir (Get_Home_Dir (Kernel), "log_files");
      VCS          : constant VCS_Access :=
                       Get_VCS_For_Activity (Kernel, Activity);
      Files        : constant File_Array :=
                       Get_Files_In_Activity (Activity);
      File         : constant Virtual_File :=
                       Create_From_Dir (Logs_Dir, +Image (Activity) & ".dif");
      T_Set        : Translate_Set :=
                       Get_Activity_Template_Tags (Kernel, Activity);
      W_File       : Writable_File;
      Content      : Unbounded_String;

      Edit_File    : Edit_Action_Command_Access;
      Adjust_Patch : Adjust_Patch_Action_Command_Access;

   begin
      W_File := Write_File (File);

      --  Add activity name and log

      Insert (T_Set, Assoc ("IS_PATCH", True));
      Content := Parse (+Get_Activity_Log_Template (Kernel).Full_Name, T_Set);

      Write (W_File, UTF8_To_Locale (To_String (Content)));
      Close (W_File);

      --  For each file we get the diff

      for J in Files'Range loop
         Diff_Patch (VCS, Files (J), File);
      end loop;

      --  Adjust patch root

      declare
         Root_Project : constant Project_Type := Get_Project (Kernel);
         Root_Dir     : constant Filesystem_String :=
                          Format_Pathname
                            (+Root_Project.Attribute_Value
                               (VCS_Patch_Root,
                                Default => +Full_Name
                                  (Project_Directory (Root_Project))));
         Path         : constant Virtual_File := Create (Root_Dir);
      begin
         Adjust_Patch := new Adjust_Patch_Action_Command_Type;
         Adjust_Patch.Kernel := Kernel;
         Adjust_Patch.Patch_File := File;
         Adjust_Patch.Files := new File_Array'(Files);

         --  If Root_Dir is set use it and handle the case where it is a
         --  relative path, otherwise set it to the root project directory.

         Ensure_Directory (Path);

         if Is_Absolute_Path (Path) then
            Adjust_Patch.Root_Dir := Path;

         else
            Adjust_Patch.Root_Dir := Create_From_Base (Path.Full_Name);
            Ensure_Directory (Adjust_Patch.Root_Dir);
         end if;

         Adjust_Patch.Header_Length := Length (Content);

         Launch_Background_Command
           (Kernel, Adjust_Patch, True, True, Name (VCS));
      end;

      --  Add the last command that will open the file containing the patch

      Edit_File := new Edit_Action_Command_Type;
      Edit_File.Kernel := Kernel;
      Edit_File.File := File;

      Launch_Background_Command
        (Kernel, Edit_File, True, True, Name (VCS));
   end On_Build_Patch_File;

   ------------------------------
   -- On_Menu_Build_Patch_File --
   ------------------------------

   procedure On_Menu_Build_Patch_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
   begin
      Apply (Context, On_Build_Patch_File'Access);
   exception
      when E : others => Trace (Me, E);
   end On_Menu_Build_Patch_File;

   ------------------------------
   -- Open_Activities_Explorer --
   ------------------------------

   procedure Open_Activities_Explorer
     (Kernel  : not null access Kernel_Handle_Record'Class;
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
      Kernel     : not null access Kernel_Handle_Record'Class;
      Real_Query : Boolean)
   is
      pragma Unreferenced (Explorer);

      Root_Project : constant Virtual_File :=
                       Get_Registry (Kernel).Tree.Root_Project.Project_Path;
      VCS          : VCS_Access;
      Activity     : Activity_Id;
      Status       : File_Status_List.Vector;

   begin
      Activity := First;

      while Activity /= No_Activity loop
         declare
            Files : constant File_Array := Get_Files_In_Activity (Activity);
         begin
            if Get_Project_Path (Activity) = Root_Project then
               VCS := Get_VCS_For_Activity (Kernel, Activity);

               if Files'Length = 0 or else VCS = null then
                  Display_File_Status
                    (Kernel, Activity, File_Status_List.Empty_Vector,
                     VCS, True);

               else
                  if Real_Query then
                     Get_Status (VCS, Files);

                  else
                     Status := Local_Get_Status (VCS, Files);
                     Display_File_Status
                       (Kernel, Activity, Status, VCS, False, True);
                     Status.Clear;
                  end if;
               end if;
            end if;
         end;

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
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_Activities_View_Access;
   begin
      Open_Activities_Explorer (Kernel, No_Context);
      Explorer := Get_Activities_Explorer (Kernel);
      Query_Activities_Files (Explorer, Kernel, True);

   exception
      when E : others => Trace (Me, E);
   end Query_Status;

   ------------------------------------
   -- VCS_Activities_Contextual_Menu --
   ------------------------------------

   procedure VCS_Activities_Contextual_Menu
     (Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      use type GNAT.Strings.String_Access;

      Kernel : constant Kernel_Handle := Get_Kernel (Context);

      File_Section       : Boolean;
      Activity_Section   : Boolean;
      Item               : Gtk_Menu_Item;
      Check              : Gtk_Check_Menu_Item;
      Actions            : Action_Array;
      Atomic_Supported   : Boolean := True;
      Same_Closed_Status : Boolean := True;
      All_Opened         : Boolean := True;
      All_Has_Log        : Boolean := True;
      Group_Commit       : Boolean := True;
      Single             : Boolean := True; -- single activity selected
      Sep                : Gtk_Separator_Menu_Item;
   begin
      --  Determine which sections should be displayed

      if Has_Activity_Information (Context) then
         Activity_Section := not Has_File_Information (Context)
           and then not Has_Directory_Information (Context);
         File_Section     := Has_File_Information (Context)
           or else Has_Directory_Information (Context);
      else
         Activity_Section := False;
         File_Section     := False;
      end if;

      if Activity_Section then
         --  First setup the activity context, we need to know if all
         --  activities are compatible (using VCS supporting atomic command,
         --  all closed or opened, all have commit/status/... actions).

         declare
            VCS       : VCS_Access;
            Activity  : Activity_Id;
            L_Actions : Action_Array;
            First     : Boolean := True;
            Closed    : Boolean;
         begin
            for Item of Activity_Information (Context) loop
               Activity := Value (Item);

               VCS := Get_VCS_For_Activity (Kernel, Activity);
               L_Actions := Get_Identified_Actions (VCS);

               All_Opened := All_Opened and then not Is_Closed (Activity);
               All_Has_Log := All_Has_Log and then Has_Log (Kernel, Activity);

               Group_Commit := Group_Commit
                 and then Get_Group_Commit (Activity);

               if VCS = null then
                  Atomic_Supported := False;
               else
                  Atomic_Supported := Atomic_Supported
                    and then Atomic_Commands_Supported (VCS);
               end if;

               if First then
                  Actions := L_Actions;
                  Closed := Is_Closed (Activity);
                  Group_Commit := Get_Group_Commit (Activity);
                  First := False;

               else
                  Single := False;
                  for K in Actions'Range loop
                     if Actions (K) /= null and then L_Actions (K) = null then
                        Actions (K) := null;
                     end if;
                  end loop;

                  Same_Closed_Status := Same_Closed_Status
                    and then Closed = Is_Closed (Activity);
               end if;
            end loop;

            Gtk_New (Check, Label => -"Group commit");
            Set_Active (Check, Atomic_Supported);
            Append (Menu, Check);
            Context_Callback.Connect
              (Check, Signal_Activate,
               On_Menu_Toggle_Group_Commit'Access, Context);
            Set_Sensitive
              (Check,
               Actions (Commit) /= null
               and then Atomic_Supported
               and then All_Opened);

            Gtk_New (Sep);
            Append (Menu, Sep);
         end;
      end if;

      Gtk_New (Item, Label => -"Create new activity");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, Signal_Activate, On_Menu_Create_Activity'Access, Context);
      Set_Sensitive (Item, True);

      if Activity_Section then
         if All_Opened or else not Same_Closed_Status then
            if Single then
               Gtk_New (Item, Label => -"Close activity");
            else
               Gtk_New (Item, Label => -"Close activities");
            end if;
         else
            if Single then
               Gtk_New (Item, Label => -"Re-open activity");
            else
               Gtk_New (Item, Label => -"Re-open activities");
            end if;
         end if;

         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate,
            On_Menu_Close_Open_Activity'Access, Context);
         Set_Sensitive (Item, Same_Closed_Status);

         if Single then
            Gtk_New (Item, Label => -"Delete activity");
         else
            Gtk_New (Item, Label => -"Delete activities");
         end if;
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Delete_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Sep);
         Append (Menu, Sep);

         if Actions (Commit) /= null then
            if Single then
               Gtk_New (Item, Label => -"Commit activity");
            else
               Gtk_New (Item, Label => -"Commit activities");
            end if;
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Signal_Activate, On_Menu_Commit_Activity'Access, Context);
            Set_Sensitive (Item, All_Opened);
         end if;

         if Actions (Status_Files) /= null then
            Gtk_New (Item, Label => -"Query status");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Signal_Activate,
               On_Menu_Query_Status_Activity'Access, Context);
            Set_Sensitive (Item, All_Opened);
         end if;

         if Actions (Update) /= null then
            Gtk_New (Item, Label => -"Update");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Signal_Activate, On_Menu_Update_Activity'Access, Context);
            Set_Sensitive (Item, All_Opened);
         end if;

         if Actions (Diff_Head) /= null then
            Gtk_New (Item, Label => -"Compare against head revision");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Signal_Activate, On_Menu_Diff_Activity'Access, Context);
            Set_Sensitive (Item, All_Opened);
         end if;

         Gtk_New (Item, Label => -"Build patch file");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Build_Patch_File'Access, Context);
         Set_Sensitive (Item, All_Opened);
      end if;

      if File_Section or else Activity_Section then
         Gtk_New (Sep);
         Append (Menu, Sep);
      end if;

      if File_Section then
         Gtk_New (Item, Label => -"Remove from activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate,
            On_Menu_Remove_From_Activity'Access, Context);
         Set_Sensitive (Item, All_Opened);
      end if;

      if Activity_Section then
         if Single then
            Gtk_New (Item, Label => -"Edit revision log");
         else
            Gtk_New (Item, Label => -"Edit revision logs");
         end if;
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Edit_Log'Access, Context);
         Set_Sensitive (Item, True);

         if All_Has_Log then
            if Single then
               Gtk_New (Item, Label => -"Remove revision log");
            else
               Gtk_New (Item, Label => -"Remove revision logs");
            end if;
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, Signal_Activate, On_Menu_Remove_Log'Access, Context);
            Set_Sensitive (Item, True);
         end if;
      end if;
   end VCS_Activities_Contextual_Menu;

   ------------------------------------
   -- VCS_Activities_Command_Handler --
   ------------------------------------

   procedure VCS_Activities_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel               : constant Kernel_Handle := Get_Kernel (Data);
      VCS_Activities_Class : constant Class_Type :=
                               New_Class (Kernel, "Activities");
      Inst                 : constant Class_Instance :=
                               Nth_Arg (Data, 1, VCS_Activities_Class);
      Image_Id             : constant String :=
                               Get_Data (Inst, VCS_Activities_Class);
      Activity             : constant Activity_Id := Value (Image_Id);
      Refresh_Explorer     : Boolean := False;
   begin
      if Activity = No_Activity then
         Set_Error_Msg (Data, -("Cannot find Activity " & Image_Id));
         return;
      end if;

      if Command = "name" then
         Set_Return_Value (Data, Get_Name (Activity));

      elsif Command = "id" then
         Set_Return_Value (Data, Image (Activity));

      elsif Command = "has_log" then
         Set_Return_Value (Data, Has_Log (Kernel, Activity));

      elsif Command = "log_file" then
         Set_Return_Value
           (Data,
            Create_File (Get_Script (Data), Get_Log_File (Kernel, Activity)));

      elsif Command = "log" then
         Refresh_Explorer := True;
         Set_Return_Value (Data, Get_Log (Kernel, Activity));

      elsif Command = "is_closed" then
         Set_Return_Value (Data, Is_Closed (Activity));

      elsif Command = "set_closed" then
         Refresh_Explorer := True;
         Set_Closed (Kernel, Activity, Nth_Arg (Data, 2));

      elsif Command = "group_commit" then
         Set_Return_Value (Data, Get_Group_Commit (Activity));

      elsif Command = "toggle_group_commit" then
         Toggle_Group_Commit (Kernel, Activity);

      elsif Command = "files" then
         Set_Return_Value_As_List (Data);

         declare
            Files : File_Array renames Get_Files_In_Activity (Activity);
         begin
            for J in Files'Range loop
               Set_Return_Value
                 (Data, +Files (J).Full_Name);
               --  ??? We should return VFS instead of strings ... As in code
               --  below
               --  Set_Return_Value
               --    (Data, Create_File (Get_Script (Data), Files (J)));
            end loop;
         end;

      elsif Command = "add_file" then
         Refresh_Explorer := True;
         Add_File (Kernel, Activity, Nth_Arg (Data, 2));

      elsif Command = "remove_file" then
         Refresh_Explorer := True;
         Remove_File (Kernel, Activity, Nth_Arg (Data, 2));

      elsif Command = "vcs" then
         declare
            VCS : constant VCS_Access :=
                    Get_VCS_For_Activity (Kernel, Activity);
         begin
            if VCS = null then
               Set_Return_Value (Data, Name (Unknown_VCS_Reference));
            else
               Set_Return_Value (Data, Name (VCS));
            end if;
         end;

      elsif Command = "commit" then
         Commit_Activity (Kernel, Activity);

      elsif Command = "diff" then
         Diff_Activity (Kernel, Activity);

      elsif Command = "query_status" then
         Query_Status_Activity (Kernel, Activity);

      elsif Command = "update" then
         Update_Activity (Kernel, Activity);
      end if;

      if Refresh_Explorer then
         Refresh (Get_Activities_Explorer (Kernel));
      end if;
   end VCS_Activities_Command_Handler;

end VCS_Activities_View_API;
