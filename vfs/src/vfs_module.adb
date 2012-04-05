------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off);
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On);

with Interfaces.C.Strings;      use Interfaces.C.Strings;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;

with Glib.Convert;              use Glib.Convert;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

with GUI_Utils;                 use GUI_Utils;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Projects;                  use Projects;
with Remote;                    use Remote;
with Traces;                    use Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;
with OS_Utils;
with Commands.Interactive;      use Commands, Commands.Interactive;

package body VFS_Module is

   VFS_Module_Name : constant String := "VFS_Module";
   Me              : constant Debug_Handle := Create (VFS_Module_Name);
   VFS_Module_Id   : Module_ID;

   Dir_Cst               : aliased constant String := "dir";
   Name_Cst              : aliased constant String := "name";
   Pattern_Cst           : aliased constant String := "pattern";
   Cd_Cmd_Parameters     : constant Cst_Argument_List := (1 => Dir_Cst'Access);
   Delete_Cmd_Parameters : constant Cst_Argument_List :=
                             (1 => Name_Cst'Access);
   Dir_Cmd_Parameters    : constant Cst_Argument_List :=
                             (1 => Pattern_Cst'Access);

   -----------------
   -- Subprograms --
   -----------------

   procedure VFS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the vfs module

   type Delete_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc from inherited subprogram

   type Rename_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Rename_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc from inherited subprogram

   type Create_Command is new Interactive_Command with record
      Create_Dir : Boolean;
   end record;
   overriding function Execute
     (Command : access Create_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc from inherited subprogram

   function Check_Prj
     (Tree     : access Project_Tree'Class;
      File_In  : GNATCOLL.VFS.Virtual_File) return Project_Type;
   --  Check if the file or directory File_In belongs to at least one of the
   --  projects in the tree (either as a source file, or as one of the source
   --  directories). If it doesn, returning the first matching project.
   --  Otherwise returns No_Project.

   --------------------------
   -- VFS_Command_Handler --
   --------------------------

   procedure VFS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Success : Boolean;

      procedure List_Files (Pattern : Filesystem_String);
      --  List files following Pattern
      --  Pattern may contain directory information and
      --  regular expressions.

      procedure Write
        (File : in out Writable_File; N : Positive; LF : Boolean);
      --  Write content of argument 1 into File. This procedure avoid as much
      --  as possible using the stack to write the content which could be
      --  large.

      ----------------
      -- List_Files --
      ----------------

      procedure List_Files (Pattern : Filesystem_String) is
         Directory_Name : constant Filesystem_String := Dir_Name (Pattern);
         Base           : constant Filesystem_String := Base_Name (Pattern);
         Is_Cur_Dir     : Boolean;
         File_Regexp    : Regexp;
         Directory      : Filesystem_String_Access;
         Dir            : Dir_Type;
         Buffer         : String (1 .. 4096);
         Last           : Natural;

      begin
         if Is_Directory (Pattern) then
            Directory := new Filesystem_String'(Name_As_Directory (Pattern));
            File_Regexp :=
              Compile ("*", Glob => True,
                       Case_Sensitive =>
                         Is_Case_Sensitive (Get_Nickname (Build_Server)));
         else
            Directory := new Filesystem_String'(Directory_Name);

            if Base'Length = 0 then
               File_Regexp :=
                 Compile ("*", Glob => True,
                          Case_Sensitive =>
                            Is_Case_Sensitive (Get_Nickname (Build_Server)));
            else
               File_Regexp :=
                 Compile (+Base, Glob => True,
                          Case_Sensitive =>
                            Is_Case_Sensitive (Get_Nickname (Build_Server)));
            end if;
         end if;

         Is_Cur_Dir := Equal
           (Normalize_Pathname (Directory.all, Resolve_Links => False),
            Get_Current_Dir);
         Open (Dir, Directory.all);

         loop
            Read (Dir, Buffer, Last);

            exit when Last = 0;

            if Match (Buffer (1 .. Last), File_Regexp) then
               if Is_Cur_Dir then
                  Set_Return_Value (Data, Buffer (1 .. Last));
               else
                  Set_Return_Value
                    (Data, +Directory.all & Buffer (1 .. Last));
               end if;
            end if;
         end loop;

         Close (Dir);
         Free (Directory);

      exception
         when Error_In_Regexp =>
            Free (Directory);
            raise;
      end List_Files;

      -----------
      -- Write --
      -----------

      procedure Write
        (File : in out Writable_File; N : Positive; LF : Boolean)
      is
         Local     : GNAT.OS_Lib.String_Access;
         Res       : chars_ptr;
         B_Read    : aliased Natural;
         B_Written : aliased Natural;
      begin
         declare
            Content : Unbounded_String := Nth_Arg (Data, N);
            USA     : Aux.Big_String_Access;
            Last    : Natural;
         begin
            if LF then
               Append (Content, ASCII.LF);
            end if;

            Aux.Get_String (Content, USA, Last);

            --  We copy the string on the heap again as Last can be less than
            --  the actual size taken by the underlying string access as we use
            --  a cache. This is also needed as we do not want to take a slice
            --  which will copy the string on the stack, so we want to avoid
            --  USA (1 .. Last).

            Local := new String (1 .. Last);
            Local.all := USA (1 .. Last);
         end;

         Res := Locale_From_UTF8 (Local.all, B_Read'Access, B_Written'Access);

         GNAT.OS_Lib.Free (Local);

         Write (File, Res);

         Free (Res);
      end Write;

   begin
      if Command = "pwd" then
         Set_Return_Value (Data, Filesystem_String'(Get_Current_Dir));

      elsif Command = "cd" then
         Name_Parameters (Data, Cd_Cmd_Parameters);

         begin
            Change_Dir (Filesystem_String'(Nth_Arg (Data, 1)));
         exception
            when Directory_Error =>
               Set_Error_Msg
                 (Data, Command & ": " & (-"cannot change current directory"));
               return;
         end;

      elsif Command = "delete" then
         Name_Parameters (Data, Delete_Cmd_Parameters);

         declare
            File : constant String := Nth_Arg (Data, 1);
         begin
            if Is_Directory (File) then
               Remove_Dir (File, True);
            else
               Delete_File (File, Success);

               if not Success then
                  Set_Error_Msg
                    (Data, Command & ": " & (-"cannot delete file"));
                  return;
               end if;
            end if;

         exception
            when Directory_Error =>
               Set_Error_Msg
                 (Data, Command & ": " & (-"cannot delete directory"));
               return;
         end;

      elsif Command = "dir" or else Command = "ls" then
         Name_Parameters (Data, Dir_Cmd_Parameters);
         Set_Return_Value_As_List (Data);

         begin
            List_Files (Nth_Arg (Data, 1, Default => "*"));
         exception
            when Error_In_Regexp =>
               Set_Error_Msg
                 (Data, -"error in regexp: " & Nth_Arg (Data, 1));
               return;
         end;

      elsif Command = "dump" then
         declare
            Temp_File : constant Virtual_File := OS_Utils.Create_Tmp_File;
            Writable  : Writable_File;
         begin
            Writable := Write_File (Temp_File);

            Write
              (Writable, N => 1, LF => Nth_Arg (Data, 2, Default => False));

            Close (Writable);
            Set_Return_Value (Data, Temp_File.Full_Name);
         end;

      elsif Command = "dump_file" then
         declare
            Filename : constant Filesystem_String := Nth_Arg (Data, 2);
            File     : Virtual_File;
            Writable : Writable_File;
            Tmp_Dir  : constant Virtual_File :=
                         Get_Tmp_Directory;
         begin
            File := Create (Filename);

            if not Is_Absolute_Path (File) then
               File := Create_From_Dir (Tmp_Dir, Full_Name (File));
            end if;

            Writable := Write_File (File, Append => True);

            Write
              (Writable, N => 1, LF => Nth_Arg (Data, 3, Default => False));

            Close (Writable);
            Set_Return_Value (Data, Full_Name (File));
         end;

      elsif Command = "base_name" then
         declare
            Filename : constant Filesystem_String := Nth_Arg (Data, 1);
            File     : Virtual_File;
         begin
            File := Create (Filename);
            Set_Return_Value (Data, Base_Name (File));
         end;

      elsif Command = "dir_name" then
         declare
            Filename : constant Filesystem_String := Nth_Arg (Data, 1);
            File     : Virtual_File;
         begin
            File := Create (Filename);
            Set_Return_Value (Data, Dir_Name (File));
         end;
      end if;
   end VFS_Command_Handler;

   ---------------
   -- Check_Prj --
   ---------------

   function Check_Prj
     (Tree    : access Project_Tree'Class;
      File_In : GNATCOLL.VFS.Virtual_File) return Project_Type
   is
      Iter : Project_Iterator;
      Prj  : Project_Type;

   begin
      if Is_Directory (File_In) then
         if Tree.Directory_Belongs_To_Project
           (File_In.Full_Name, Direct_Only => False)
         then
            return Tree.Root_Project;   --   ??? Not accurate
         else
            return No_Project;
         end if;

      else
         --  Do we have a source file ?
         Prj := Tree.Info (File_In).Project;
         if Prj /= No_Project then
            return Prj;
         end if;
      end if;

      Iter := Start (Tree.Root_Project, Include_Extended => False);
      loop
         Prj := Current (Iter);
         exit when Prj = No_Project;

         Next (Iter);

         --  First check if the file is a project file
         if File_In = Project_Path (Prj) then
            return Prj;
         end if;
      end loop;

      return No_Project;
   end Check_Prj;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Dir     : constant GNATCOLL.VFS.Virtual_File :=
                  Directory_Information (Context.Context);
      Success : Boolean;
      Res     : Gtkada.Dialogs.Message_Dialog_Buttons;
      File    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Project : Project_Type;

   begin
      Trace (Me, "deleting "
             & File_Information (Context.Context).Display_Full_Name);
      Push_State (Get_Kernel (Context.Context), Busy);

      if Has_File_Information (Context.Context) then
         File := File_Information (Context.Context);

         Res := Gtkada.Dialogs.Message_Dialog
           (-"Are you sure you want to delete " &
            Display_Full_Name (File) &
            " ?",
            Gtkada.Dialogs.Confirmation,
            Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No);

         if Res = Gtkada.Dialogs.Button_Yes then
            Delete (File, Success);

            if not Success then
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot remove file: ") &
                  Display_Full_Name (File),
                  Mode => Error);
            end if;
         else
            Success := False;
         end if;

      else
         --  Assign for further use
         File := Dir;

         Res := Gtkada.Dialogs.Message_Dialog
           (-"Are you sure you want to delete the directory " &
            Dir.Display_Full_Name & (-" and all its subdirectories ?"),
            Gtkada.Dialogs.Confirmation,
            Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No);

         if Res = Gtkada.Dialogs.Button_Yes then
            --  Is_Directory marks File as directory. This is important
            --  because File cannot be marked as such after deletion of the
            --  physical dir.

            if Dir.Is_Directory then
               Dir.Remove_Dir (True, Success);
            else
               Success :=  False;
            end if;

            if not Success then
               Success := False;
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot remove directory: ") & Dir.Display_Full_Name,
                  Mode => Error);
            end if;
         else
            Success := False;
         end if;
      end if;

      --  Need also to update project/file views
      if Success then
         Get_Kernel (Context.Context).File_Deleted (File);
         Project := Check_Prj
           (Get_Registry (Get_Kernel (Context.Context)).Tree, File);

         if Project /= No_Project then
            Res := Gtkada.Dialogs.Message_Dialog
              (-"A file or directory belonging to the project "
               & Project.Name
               & (-" has been deleted.") & ASCII.LF &
               (-"The project might require modifications."),
               Gtkada.Dialogs.Warning,
               Gtkada.Dialogs.Button_Yes);
            Reload_Project_If_Needed (Get_Kernel (Context.Context));
            Recompute_View (Get_Kernel (Context.Context));
         end if;
      end if;

      Pop_State (Get_Kernel (Context.Context));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Rename_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Dir     : constant Virtual_File :=
                  Directory_Information (Context.Context);
      Success : Boolean;
      Button  : Gtkada.Dialogs.Message_Dialog_Buttons;
      Is_Dir  : Boolean;

      procedure Actual_Rename
        (File_In     : GNATCOLL.VFS.Virtual_File;
         Success     : out Boolean;
         Prj_Changed : out Boolean);
      --  Performs the actual renaming

      procedure Rename_In_Prj
        (File_In, File_Out : Virtual_File; Success : out Boolean);
      --  Rename the path in the projects

      -------------------
      -- Rename_In_Prj --
      -------------------

      procedure Rename_In_Prj
        (File_In, File_Out : Virtual_File; Success : out Boolean)
      is
         Project  : constant Project_Type :=
                      Get_Project (Get_Kernel (Context.Context));
         Relative : Boolean;
         Iter     : Project_Iterator :=
                      Start (Project,
                                      Include_Extended => False);
         Prj      : Project_Type;
      begin
         Success     := False;

         loop
            Prj := Current (Iter);

            exit when Prj = No_Project;

            Next (Iter);
            Relative :=
              Get_Paths_Type (Prj) = Projects.Relative
              or else (Get_Paths_Type (Prj) = From_Pref
                       and then Generate_Relative_Paths.Get_Pref);

            Success := Success or else Rename_Path
              (Prj, File_In, File_Out, Relative);
         end loop;
      end Rename_In_Prj;

      -------------------
      -- Actual_Rename --
      -------------------

      procedure Actual_Rename
        (File_In     : GNATCOLL.VFS.Virtual_File;
         Success     : out Boolean;
         Prj_Changed : out Boolean)
      is
         Renamed : Virtual_File := No_File;
         Project : Project_Type;
      begin
         if Is_Directory (File_In) then
            declare
               Res : constant String :=
                       GUI_Utils.Query_User
                         (Get_Kernel (Context.Context).Get_Main_Window,
                          (-"Please enter the directory's new name:"),
                          Password_Mode => False,
                          Urgent        => False,
                          Default       => +File_In.Base_Dir_Name);
            begin
               if Res /= "" then
                  Renamed := Create_From_Dir (Get_Parent (File_In), +Res);
               end if;
            end;

         else
            declare
               Res : constant String :=
                       GUI_Utils.Query_User
                         (Get_Kernel (Context.Context).Get_Main_Window,
                          (-"Please enter the file's new name:"),
                          Password_Mode => False,
                          Urgent        => False,
                          Default       => +File_In.Base_Name);
            begin
               if Res /= "" then
                  Renamed := Create_From_Dir (File_In.Dir, +Res);
               end if;
            end;
         end if;

         if Renamed = File_In or else Renamed = No_File then
            Success     := True;
            Prj_Changed := False;

            return;
         end if;

         Rename (File_In, Renamed, Success);

         if not Success then
            Console.Insert
              (Get_Kernel (Context.Context),
               (-"Cannot rename ") &
               Display_Full_Name (File_In) &
               (-" into ") &
               Display_Full_Name (Renamed),
               Mode => Error);
            return;
         end if;

         --  Run the 'file_renamed' hook

         if Is_Directory (File_In) then
            Ensure_Directory (Renamed);
         end if;

         Get_Kernel (Context.Context).File_Renamed (File_In, Renamed);

         --  First check if file_in is defined in the projects
         Project := Check_Prj
           (Get_Registry (Get_Kernel (Context.Context)).Tree, File_In);

         if Project /= No_Project then
            if Is_Directory (File_In) then
               --  We need to change the paths defined in the projects

               Button := Gtkada.Dialogs.Message_Dialog
                 (-("The directory is referenced in the project ")
                  & Project.Name & ASCII.LF &
                  (-("Do you want GPS to modify these projects to " &
                     "reference its new name ?")),
                  Gtkada.Dialogs.Confirmation,
                  Button_Yes or Button_No);

               if Button = Button_Yes then
                  Rename_In_Prj
                    (File_In, Renamed, Prj_Changed);
               else
                  Prj_Changed := False;
               end if;
            else
               Button := Gtkada.Dialogs.Message_Dialog
                 (-("The file is referenced in the project ") &
                  Project.Name & ASCII.LF &
                  (-"The project(s) might require manual modifications."),
                  Gtkada.Dialogs.Warning,
                  Button_OK);
               Prj_Changed := True;
            end if;
         end if;
      end Actual_Rename;

      Prj_Changed : Boolean;

   begin
      Trace (Me, "renaming " &
             File_Information (Context.Context).Display_Full_Name);
      Push_State (Get_Kernel (Context.Context), Busy);

      Is_Dir := not Has_File_Information (Context.Context);

      if Is_Dir then
         Ensure_Directory (Dir);
         Actual_Rename (Dir, Success, Prj_Changed);

      else
         Actual_Rename
           (File_Information (Context.Context), Success, Prj_Changed);
      end if;

      --  Need also to update project/file view
      if Success and then Prj_Changed then
         Recompute_View (Get_Kernel (Context.Context));
      end if;

      Pop_State (Get_Kernel (Context.Context));

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Create_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Dir         : constant Virtual_File :=
                      Directory_Information (Context.Context);
      File        : GNATCOLL.VFS.Virtual_File;
      Project     : Project_Type;

   begin
      if Command.Create_Dir then
         declare
            Res : constant String :=
                    GUI_Utils.Query_User
                      (Get_Kernel (Context.Context).Get_Main_Window,
                       (-"Please enter the new directory's name:"),
                       Password_Mode => False,
                       Urgent        => False,
                       Default       => "");
         begin
            if Res /= "" then
               File := Create_From_Dir (Dir, +Res);
               Make_Dir (File);
            end if;
         exception
            when Directory_Error =>
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot create dir ") &
                  Display_Full_Name (File),
                  Mode => Error);
               return Commands.Failure;
         end;
      else
         declare
            Res : constant String :=
                    GUI_Utils.Query_User
                      (Get_Kernel (Context.Context).Get_Main_Window,
                       (-"Please enter the new file's name:"),
                       Password_Mode => False,
                       Urgent        => False,
                       Default       => "");
            W_File : GNATCOLL.VFS.Writable_File;
         begin
            if Res /= "" then
               File := Create_From_Dir (Dir, +Res);
               W_File := GNATCOLL.VFS.Write_File (File);
               GNATCOLL.VFS.Close (W_File);
            end if;
         exception
            when others =>
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot create file ") &
                  Display_Full_Name (File),
                  Mode => Error);
               return Commands.Failure;
         end;
      end if;

      GPS.Kernel.File_Saved (Get_Kernel (Context.Context), File);
      Project := Check_Prj
        (Get_Registry (Get_Kernel (Context.Context)).Tree, Dir);

      if Project /= No_Project then
         Recompute_View (Get_Kernel (Context.Context));
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Filters --
   -------------

   type File_Filter_Record is new Action_Filter_Record with null record;
   type Dir_Filter_Record is new Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Context : access File_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   overriding function Filter_Matches_Primitive
     (Context : access Dir_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access File_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Has_File_Information (Ctxt)
        and then not Has_Entity_Name_Information (Ctxt);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Dir_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Has_Directory_Information (Ctxt)
        and then not Has_File_Information (Ctxt);
   end Filter_Matches_Primitive;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File_View_Filter : constant Action_Filter :=
        Lookup_Filter (Kernel, "File_View");
      File_Filter      : constant Action_Filter := new File_Filter_Record;
      Dir_Filter       : constant Action_Filter := new Dir_Filter_Record;
      Command          : Interactive_Command_Access;
   begin
      VFS_Module_Id := new Module_ID_Record;

      Register_Module
        (Module      => VFS_Module_Id,
         Kernel      => Kernel,
         Module_Name => VFS_Module_Name,
         Priority    => Default_Priority);

      Command := new Create_Command;
      Create_Command (Command.all).Create_Dir := False;
      Register_Contextual_Menu
        (Kernel, "Create File",
         Action => Command,
         Filter => File_View_Filter and Dir_Filter,
         Label  => "File operations/Create a new file");
      Command := new Create_Command;
      Create_Command (Command.all).Create_Dir := True;
      Register_Contextual_Menu
        (Kernel, "Create dir",
         Action => Command,
         Filter => File_View_Filter and Dir_Filter,
         Label  => "File operations/Create a subdirectory");
      Command := new Rename_Command;
      Register_Contextual_Menu
        (Kernel, "Rename file",
         Action => Command,
         Filter => File_View_Filter and File_Filter,
         Label  => "File operations/Rename file %f");
      Register_Contextual_Menu
        (Kernel, "Rename directory",
         Action => Command,
         Filter => File_View_Filter and Dir_Filter,
         Label  => "File operations/Rename directory");
      Command := new Delete_Command;
      Register_Contextual_Menu
        (Kernel, "Delete file",
         Action => Command,
         Filter => File_View_Filter and File_Filter,
         Label  => "File operations/Delete file %f");
      Register_Contextual_Menu
        (Kernel, "Delete directory",
         Action => Command,
         Filter => File_View_Filter and Dir_Filter,
         Label  => "File operations/Delete directory recursively");

      Register_Command
        (Kernel, "pwd",
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "cd",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "delete",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "dir",
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "ls",
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "dump",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "dump_file",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "base_name",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel, "dir_name",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
   end Register_Module;

end VFS_Module;
