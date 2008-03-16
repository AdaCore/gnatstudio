-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Directories;           use Ada.Directories;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.OS_Lib;               use GNAT;
with GNAT.Strings;
with Templates_Parser;          use Templates_Parser;

with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Commands;                  use Commands;
with Log_Utils;                 use Log_Utils;
with OS_Utils;                  use OS_Utils;
with Projects;                  use Projects;
with String_List_Utils;         use String_List_Utils;
with Traces;                    use Traces;
with UTF8_Utils;                use UTF8_Utils;
with VCS;                       use VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Module;                use VCS_Module;
with VCS_View.Explorer;
with VCS_View;                  use VCS_View;
with VFS;                       use VFS;

package body VCS_Activities_View_API is

   procedure Commit_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id);
   --  Commit the given activity

   procedure Query_Status_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id);
   --  Query status of the given activity

   procedure Update_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id);
   --  Update the given activity

   procedure Diff_Activity
     (Kernel   : Kernel_Handle;
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

   procedure On_Menu_Build_Patch_File
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);

   procedure Populate_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id;
      Context  : Selection_Context);
   --  Add file in context or files selected on the VCS explorer into the given
   --  activity.

   --  Action to open a file

   type Edit_Action_Command_Type is new Root_Command with record
      Kernel : Kernel_Handle;
      File   : Virtual_File;
   end record;
   type Edit_Action_Command_Access is access Edit_Action_Command_Type;

   function Execute
     (Command : access Edit_Action_Command_Type) return Command_Return_Type;

   --  Action adjust patch root

   type Adjust_Patch_Action_Command_Type is new Root_Command with record
      Kernel        : Kernel_Handle;
      Patch_File    : Virtual_File;     -- patch file
      Files         : String_List.List; -- all files contained in the patch
      Root_Dir      : String_Access;    -- the patch root directory to set
      Header_Length : Positive;         -- the character length of the header
   end record;
   type Adjust_Patch_Action_Command_Access is
     access Adjust_Patch_Action_Command_Type;

   function Execute
     (Command : access Adjust_Patch_Action_Command_Type)
      return Command_Return_Type;

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

   function Execute
     (Command : access Adjust_Patch_Action_Command_Type)
      return Command_Return_Type
   is
      procedure Handle (Filename : String);
      --  Handle this filename in the patch

      Root_Dir      : constant String := To_Lower (Command.Root_Dir.all);
      Iter          : String_List.List_Node;
      Patch         : Strings.String_Access := Read_File (Command.Patch_File);
      Patch_Content : Unbounded_String := To_Unbounded_String (Patch.all);

      ------------
      -- Handle --
      ------------

      procedure Handle (Filename : String) is
         BF : constant String := Simple_Name (Filename);
         LF : constant String := To_Lower (Filename);
         NF : String := Filename;

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

         if LF (LF'First .. LF'First + Root_Dir'Length - 1) = Root_Dir then
            Replace_Token
              ("--- ", BF, NF (NF'First + Root_Dir'Length .. NF'Last));
            Replace_Token
              ("*** ", BF, NF (NF'First + Root_Dir'Length .. NF'Last));
         end if;
      end Handle;

   begin
      Strings.Free (Patch);

      Iter := String_List.First (Command.Files);

      for K in 1 .. String_List.Length (Command.Files) loop
         Handle (String_List.Data (Iter));
         Iter := String_List.Next (Iter);
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
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : Activity_Id;
   begin
      Activity := Value (Activity_Information (Context));
      Toggle_Group_Commit (Kernel, Activity);
   exception
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Delete_Activity;

   -----------------------
   -- Populate_Activity --
   -----------------------

   procedure Populate_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id;
      Context  : Selection_Context) is
   begin
      if (Has_File_Information (Context)
          or else Has_Directory_Information (Context))
        and then
          not (Get_Creator (Context) = Abstract_Module_ID (VCS_Module_ID))
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
         Activity := Value (Activity_Information (Context));

         Populate_Activity (Kernel, Activity, Context);

         Refresh (Get_Activities_Explorer (Kernel, False, False));
         Refresh (Get_Explorer (Kernel, False, False));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Add_To_Activity;

   --------------------------------
   -- On_Menu_Commit_As_Activity --
   --------------------------------

   procedure On_Menu_Commit_As_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      use type String_List.List_Node;
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
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Commit_As_Activity;

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
      View     : VCS_View_Access;
   begin
      if (Has_File_Information (Context)
          or else Has_Directory_Information (Context))
        and then
          not (Get_Creator (Context) = Abstract_Module_ID (VCS_Module_ID))
      then
         --  If we have a file information, then there is a single file to
         --  handle.
         File := File_Information (Context);
         On_Remove_From_Activity (Kernel, Get_File_Activity (File), File);

      else
         if Get_Current_Focus_Widget (Kernel)
           = Gtk_Widget (Get_Activities_Explorer (Kernel, False))
         then
            View := VCS_View_Access (Get_Activities_Explorer (Kernel, False));
         else
            View := VCS_View_Access (Get_Explorer (Kernel, False));
         end if;

         Files := Get_Selected_Files (View);

         Files_It := String_List.First (Files);

         while Files_It /= String_List.Null_Node loop
            File := Create (Full_Filename => String_List.Data (Files_It));
            On_Remove_From_Activity (Kernel, Get_File_Activity (File), File);

            Files_It := String_List.Next (Files_It);
         end loop;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Remove_From_Activity;

   ---------------------------
   -- Query_Status_Activity --
   ---------------------------

   procedure Query_Status_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id)
   is
      Files : String_List.List;
   begin
      Files := Get_Files_In_Activity (Activity);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file in activity, cannot commit", Mode => Error);
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
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : constant Activity_Id :=
                   Value (Activity_Information (Context));
   begin
      Query_Status_Activity (Kernel, Activity);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Query_Status_Activity;

   ---------------------
   -- Update_Activity --
   ---------------------

   procedure Update_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id)
   is
      Files : String_List.List;
   begin
      Files := Get_Files_In_Activity (Activity);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      Update (Get_VCS_For_Activity (Kernel, Activity), Files);
      Get_Status (Get_VCS_For_Activity (Kernel, Activity), Files);
   end Update_Activity;

   -----------------------------
   -- On_Menu_Update_Activity --
   -----------------------------

   procedure On_Menu_Update_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : constant Activity_Id :=
                   Value (Activity_Information (Context));
   begin
      Update_Activity (Kernel, Activity);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Update_Activity;

   ---------------------
   -- Commit_Activity --
   ---------------------

   procedure Commit_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id)
   is
      use String_List;
      Files          : String_List.List;
      All_Logs_Exist : Boolean := True;
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
   end Commit_Activity;

   -----------------------------
   -- On_Menu_Commit_Activity --
   -----------------------------

   procedure On_Menu_Commit_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : constant Activity_Id :=
                   Value (Activity_Information (Context));
   begin
      Commit_Activity (Kernel, Activity);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Commit_Activity;

   -------------------
   -- Diff_Activity --
   -------------------

   procedure Diff_Activity
     (Kernel   : Kernel_Handle;
      Activity : Activity_Id)
   is
      VCS   : constant VCS_Access := Get_VCS_For_Activity (Kernel, Activity);
      Files : constant String_List.List := Get_Files_In_Activity (Activity);
      Iter  : String_List.List_Node;
   begin
      --  For each file we get the diff

      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         Diff (VCS, Create (String_List.Data (Iter)));
         Iter := String_List.Next (Iter);
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
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Activity : constant Activity_Id :=
                   Value (Activity_Information (Context));
   begin
      Diff_Activity (Kernel, Activity);
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Menu_Diff_Activity;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      List     : String_List.List;
      Activity : Activity_Id;

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
      when E : others => Trace (Exception_Handle, E);
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

      Kernel       : constant Kernel_Handle := Get_Kernel (Context);
      Logs_Dir     : constant String := Get_Home_Dir (Kernel) & "log_files";
      Activity     : constant Activity_Id :=
                       Value (Activity_Information (Context));
      VCS          : constant VCS_Access :=
                       Get_VCS_For_Activity (Kernel, Activity);
      Files        : constant String_List.List :=
                       Get_Files_In_Activity (Activity);
      Filename     : constant String :=
                       Logs_Dir & OS_Lib.Directory_Separator
                         & Image (Activity) & ".dif";
      T_Set        : Translate_Set :=
                       Get_Activity_Template_Tags (Kernel, Activity);
      File         : Virtual_File;
      W_File       : Writable_File;
      Content      : Unbounded_String;
      Iter         : String_List.List_Node;

      Edit_File    : Edit_Action_Command_Access;
      Adjust_Patch : Adjust_Patch_Action_Command_Access;

   begin
      File := Create (Filename);
      W_File := Write_File (File);

      --  Add activity name and log

      Insert (T_Set, Assoc ("IS_PATCH", True));
      Content := Parse (Get_Activity_Log_Template (Kernel), T_Set);

      Write (W_File, UTF8_To_Locale (To_String (Content)));
      Close (W_File);

      --  For each file we get the diff

      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         Diff_Patch (VCS, Create (String_List.Data (Iter)), File);
         Iter := String_List.Next (Iter);
      end loop;

      --  Adjust patch root

      declare
         Root_Project : constant Project_Type := Get_Project (Kernel);
         Root_Dir     : constant String :=
                          Format_Pathname
                            (Get_Attribute_Value
                               (Root_Project,
                                VCS_Patch_Root,
                                Default => Full_Name
                                  (Project_Directory (Root_Project)).all));
      begin
         Adjust_Patch := new Adjust_Patch_Action_Command_Type;
         Adjust_Patch.Kernel := Kernel;
         Adjust_Patch.Patch_File := File;
         Adjust_Patch.Files := Files;
         if Is_Directory_Separator (Root_Dir (Root_Dir'Last)) then
            Adjust_Patch.Root_Dir := new String'(Root_Dir);
         else
            Adjust_Patch.Root_Dir :=
              new String'(Root_Dir & OS_Lib.Directory_Separator);
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

   exception
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
         Activity         := Value (Activity_Information (Context));
         Active           := not Is_Closed (Activity);
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
           (Check, Signal_Activate,
            On_Menu_Toggle_Group_Commit'Access, Context);
         Set_Sensitive
           (Check,
            VCS /= null
            and then Atomic_Commands_Supported (VCS)
            and then not Is_Closed (Activity));

         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      Gtk_New (Item, Label => -"Create new activity");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, Signal_Activate, On_Menu_Create_Activity'Access, Context);
      Set_Sensitive (Item, True);

      if Activity_Section then
         if Is_Closed (Activity) then
            Gtk_New (Item, Label => -"Re-open activity");
         else
            Gtk_New (Item, Label => -"Close activity");
         end if;

         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate,
            On_Menu_Close_Open_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item, Label => -"Delete activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Delete_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item);
         Append (Menu, Item);

         Gtk_New (Item, Label => -"Commit activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Commit_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Query status");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate,
            On_Menu_Query_Status_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Update");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Update_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Compare against head revision");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Diff_Activity'Access, Context);
         Set_Sensitive (Item, Active);

         Gtk_New (Item, Label => -"Build patch file");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Build_Patch_File'Access, Context);
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
           (Item, Signal_Activate,
            On_Menu_Remove_From_Activity'Access, Context);
         Set_Sensitive (Item, Active);
      end if;

      if Activity_Section then
         --  Fill the section relative to files

         Gtk_New (Item, Label => -"Edit revision log");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, Signal_Activate, On_Menu_Edit_Log'Access, Context);
         Set_Sensitive (Item, True);
      end if;
   end VCS_Activities_Contextual_Menu;

   ------------------------------------
   -- VCS_Activities_Command_Handler --
   ------------------------------------

   procedure VCS_Activities_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      use type String_List.List_Node;

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
            Node : String_List.List_Node :=
                     String_List.First (Get_Files_In_Activity (Activity));
         begin
            while Node /= String_List.Null_Node loop
               Set_Return_Value (Data, String_List.Data (Node));
               Node := String_List.Next (Node);
            end loop;
         end;

      elsif Command = "add_file" then
         Refresh_Explorer := True;
         Add_File (Kernel, Activity, Get_Data (Data, 2));

      elsif Command = "remove_file" then
         Refresh_Explorer := True;
         Remove_File (Kernel, Activity, Get_Data (Data, 2));

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
