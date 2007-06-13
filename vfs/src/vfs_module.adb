---------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2007                      --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Strings;

with Gtkada.Dialogs;            use Gtkada.Dialogs;

with File_Utils;                use File_Utils;
with GUI_Utils;                 use GUI_Utils;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Projects;                  use Projects;
with Projects.Editor;           use Projects.Editor;
with Remote;                    use Remote;
with Traces;                    use Traces;
with VFS;                       use VFS;
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
   --  Interactive command handler for the vfs module.

   type Delete_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc from inherited subprogram

   type Rename_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Rename_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc from inherited subprogram

   type Create_Command is new Interactive_Command with record
      Create_Dir : Boolean;
   end record;
   function Execute
     (Command : access Create_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc from inherited subprogram

   procedure Check_Prj
     (Project  : Projects.Project_Type;
      File_In  : VFS.Virtual_File;
      Success  : out Boolean;
      Prj_List : out Unbounded_String);
   --  Check if the project contains File_In directory or file.
   --  If Success, then Prj_List will contain a printable list of (sub)projects
   --  referencing File_In.

   --------------------------
   -- VFS_Command_Handler --
   --------------------------

   procedure VFS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Success : Boolean;

      procedure List_Files (Pattern : String);
      --  List files following Pattern
      --  Pattern may contain directory information and
      --  regular expressions.

      ----------------
      -- List_Files --
      ----------------

      procedure List_Files (Pattern : String) is
         Directory_Name : constant String := Dir_Name (Pattern);
         Base           : constant String := Base_Name (Pattern);
         Is_Cur_Dir     : Boolean;
         File_Regexp    : Regexp;
         Directory      : GNAT.Strings.String_Access;
         Dir            : Dir_Type;
         Buffer         : String (1 .. 4096);
         Last           : Natural;

      begin
         if Is_Directory (Pattern) then
            Directory := new String'(Name_As_Directory (Pattern));
            File_Regexp :=
              Compile ("*", Glob => True,
                       Case_Sensitive => Is_Case_Sensitive (Build_Server));
         else
            Directory := new String'(Directory_Name);

            if Base = "" then
               File_Regexp :=
                 Compile ("*", Glob => True,
                          Case_Sensitive => Is_Case_Sensitive (Build_Server));
            else
               File_Regexp :=
                 Compile (Base, Glob => True,
                          Case_Sensitive => Is_Case_Sensitive (Build_Server));
            end if;
         end if;

         Is_Cur_Dir :=
           Normalize_Pathname (Directory.all, Resolve_Links => False) =
           Get_Current_Dir;
         Open (Dir, Directory.all);

         loop
            Read (Dir, Buffer, Last);

            exit when Last = 0;

            if Match (Buffer (1 .. Last), File_Regexp) then
               if Is_Cur_Dir then
                  Set_Return_Value (Data, Buffer (1 .. Last));
               else
                  Set_Return_Value
                    (Data, Directory.all & Buffer (1 .. Last));
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

   begin
      if Command = "pwd" then
         Set_Return_Value (Data, Get_Current_Dir);

      elsif Command = "cd" then
         Name_Parameters (Data, Cd_Cmd_Parameters);

         begin
            Change_Dir (Nth_Arg (Data, 1));
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
            Temp_File : constant String := OS_Utils.Create_Tmp_File;
            Writable  : Writable_File;
         begin
            Writable := Write_File (Create (Temp_File));

            if Nth_Arg (Data, 2, Default => False) then
               Write (Writable, Nth_Arg (Data, 1) & ASCII.LF);
            else
               Write (Writable, Nth_Arg (Data, 1));
            end if;

            Close (Writable);
            Set_Return_Value (Data, Temp_File);
         end;

      elsif Command = "dump_file" then
         declare
            Filename : constant String := Nth_Arg (Data, 2);
            File     : Virtual_File;
            Writable : Writable_File;
         begin
            if not Is_Absolute_Path (Filename) then
               File := Create (OS_Utils.Get_Tmp_Dir & Filename);
            else
               File := Create (Filename);
            end if;
            Writable := Write_File (File, Append => True);

            if Nth_Arg (Data, 3, Default => False) then
               Write (Writable, Nth_Arg (Data, 1) & ASCII.LF);
            else
               Write (Writable, Nth_Arg (Data, 1));
            end if;

            Close (Writable);
            Set_Return_Value (Data, Full_Name (File).all);
         end;
      end if;
   end VFS_Command_Handler;

   ---------------
   -- Check_Prj --
   ---------------

   procedure Check_Prj
     (Project  : Projects.Project_Type;
      File_In  : VFS.Virtual_File;
      Success  : out Boolean;
      Prj_List : out Unbounded_String)
   is
      Iter  : Projects.Imported_Project_Iterator :=
                Projects.Start (Project, Include_Extended => False);
      Prj   : Projects.Project_Type;
      First : Boolean := True;
      Found : Boolean;

   begin
      Prj_List := To_Unbounded_String ("");
      Success  := False;

      loop
         Prj := Projects.Current (Iter);

         exit when Prj = Projects.No_Project;

         Projects.Next (Iter);

         Found := False;

         --  Check directories.
         if Is_Directory (File_In)
           and then Projects.Editor.Contains_Path (Prj, File_In.Full_Name.all)
         then
            Success := True;
            Found := True;

         --  Check files
         elsif not Is_Directory (File_In) then

            --  First check if the file is a project file.
            if File_In = Project_Path (Prj) then
               Success := True;
               Found := True;

            --  Then check if the file is a source file.
            else
               declare
                  Files : File_Array_Access :=
                            Projects.Get_Source_Files
                              (Prj, Recursive => False);
               begin
                  for J in Files'Range loop
                     if Files (J) = File_In then
                        Success := True;
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  Unchecked_Free (Files);
               end;
            end if;
         end if;

         if Found then
            if not First then
               Prj_List := Prj_List & ", ";
            end if;

            First := False;
            Prj_List := Prj_List & "'" & Project_Name (Prj) & "'";
         end if;
      end loop;
   end Check_Prj;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Dir     : constant String := Directory_Information (Context.Context);
      Success : Boolean;
      Res     : Gtkada.Dialogs.Message_Dialog_Buttons;
      File    : VFS.Virtual_File := VFS.No_File;
      List    : Unbounded_String;

   begin
      Trace (Me, "deleting "
             & Full_Name (File_Information (Context.Context)).all);
      Push_State (Get_Kernel (Context.Context), Busy);

      if Has_File_Information (Context.Context) then
         File := File_Information (Context.Context);

         Res := Gtkada.Dialogs.Message_Dialog
           (-"Are you sure you want to delete " &
            File.Full_Name.all &
            " ?",
            Gtkada.Dialogs.Confirmation,
            Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No);

         if Res = Gtkada.Dialogs.Button_Yes then
            Delete (File, Success);

            if not Success then
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot remove file: ") &
                  File.Full_Name.all,
                  Mode => Error);
            end if;
         else
            Success := False;
         end if;

      else
         Res := Gtkada.Dialogs.Message_Dialog
           (-"Are you sure you want to delete the directory " &
            Dir & (-" and all its subdirectories ?"),
            Gtkada.Dialogs.Confirmation,
            Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No);

         if Res = Gtkada.Dialogs.Button_Yes then
            begin
               File := Create (Dir);
               --  Is_Directory marks File as directory. This is important
               --  because File cannot be marked as such after deletion of the
               --  physical dir.
               if Is_Directory (File) then
                  Remove_Dir (Dir, True);
                  Success := True;
               else
                  Success :=  False;
               end if;
            exception
               when Directory_Error =>
                  Success := False;
                  Console.Insert
                    (Get_Kernel (Context.Context),
                     (-"Cannot remove directory: ") & Dir,
                     Mode => Error);
            end;
         else
            Success := False;
         end if;
      end if;

      --  Need also to update project/file views
      if Success then
         Get_Kernel (Context.Context).File_Deleted (File);
         Check_Prj
           (Get_Project (Get_Kernel (Context.Context)),
            File, Success, List);

         if Success then
            Res := Gtkada.Dialogs.Message_Dialog
              (-"A file or directory belonging to the project(s) " &
               To_String (List) &
               (-" has been deleted.") & ASCII.LF &
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

   function Execute
     (Command : access Rename_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Dir      : constant String := Directory_Information (Context.Context);
      Success  : Boolean;
      Button   : Gtkada.Dialogs.Message_Dialog_Buttons;
      Is_Dir   : Boolean;
      Prj_List : Unbounded_String;

      procedure Actual_Rename
        (File_In     : VFS.Virtual_File;
         Success     : out Boolean;
         Prj_Changed : out Boolean);
      --  Performs the actual renaming

      procedure Rename_In_Prj
        (File_In, File_Out : String; Success : out Boolean);
      --  Rename the path in the projects

      -------------------
      -- Rename_In_Prj --
      -------------------

      procedure Rename_In_Prj
        (File_In, File_Out : String; Success : out Boolean)
      is
         Project  : constant Projects.Project_Type :=
                      Get_Project (Get_Kernel (Context.Context));
         Relative : Boolean;
         Iter     : Projects.Imported_Project_Iterator :=
                      Projects.Start (Project,
                                      Include_Extended => False);
         Prj      : Projects.Project_Type;
      begin
         Success     := False;

         loop
            Prj := Projects.Current (Iter);

            exit when Prj = Projects.No_Project;

            Projects.Next (Iter);
            Relative :=
              Get_Paths_Type (Prj) = Projects.Relative
              or else (Get_Paths_Type (Prj) = From_Pref
                       and then Get_Pref (Generate_Relative_Paths));

            Success := Success or else Projects.Editor.Rename_Path
              (Prj, File_In, File_Out, Relative);
         end loop;
      end Rename_In_Prj;

      -------------------
      -- Actual_Rename --
      -------------------

      procedure Actual_Rename
        (File_In     : VFS.Virtual_File;
         Success     : out Boolean;
         Prj_Changed : out Boolean)
      is
         Renamed : Unbounded_String;
         To_File : VFS.Virtual_File;
      begin
         if Is_Directory (File_In) then
            declare
               Res : constant String :=
                 GUI_Utils.Query_User
                   (Get_Kernel (Context.Context).Get_Main_Window,
                    (-"Please enter the directory's new name:"),
                    Password_Mode => False,
                    Urgent        => False,
                    Default       => File_In.Base_Dir_Name);
            begin
               Renamed :=
                 To_Unbounded_String (File_In.Get_Parent.Full_Name.all) & Res;
            end;

         else
            declare
               Res : constant String :=
                 GUI_Utils.Query_User
                   (Get_Kernel (Context.Context).Get_Main_Window,
                    (-"Please enter the file's new name:"),
                    Password_Mode => False,
                    Urgent        => False,
                    Default       => File_In.Base_Name);
            begin
               Renamed :=
                 To_Unbounded_String (File_In.Dir.Full_Name.all) & Res;
            end;
         end if;

         declare
            Res : constant String :=
                    To_String (Renamed);
         begin
            if Res = File_In.Full_Name.all or else Res = "" then
               Success     := True;
               Prj_Changed := False;

               return;
            end if;

            Rename (File_In, Res, Success);

            if not Success then
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot rename ") &
                  File_In.Full_Name.all &
                  (-" into ") &
                  Res,
                  Mode => Error);

               return;
            end if;

            --  Run the 'file_renamed' hook
            To_File := Create (Res);

            if Is_Directory (File_In) then
               Ensure_Directory (To_File);
            end if;

            Get_Kernel (Context.Context).File_Renamed (File_In, To_File);

            --  First check if file_in is defined in the projects.
            Check_Prj
              (Get_Project (Get_Kernel (Context.Context)),
               File_In, Prj_Changed, Prj_List);

            if Prj_Changed then
               if Is_Directory (File_In) then
                  --  We need to change the paths defined in the projects

                  Button := Gtkada.Dialogs.Message_Dialog
                    (-("The directory is referenced in the project(s) ") &
                     To_String (Prj_List) & ASCII.LF &
                     (-("Do you want GPS to modify these projects to " &
                        "reference its new name ?")),
                     Gtkada.Dialogs.Confirmation,
                     Button_Yes or Button_No);

                  if Button = Button_Yes then
                     Rename_In_Prj
                       (File_In.Full_Name.all, Res, Prj_Changed);
                  else
                     Prj_Changed := False;
                  end if;
               else
                  Button := Gtkada.Dialogs.Message_Dialog
                    (-("The file is referenced in the project(s) ") &
                     To_String (Prj_List) & ASCII.LF &
                     (-"The project(s) might require manual modifications."),
                     Gtkada.Dialogs.Warning,
                     Button_OK);
                  Prj_Changed := True;
               end if;
            end if;
         end;

      end Actual_Rename;

      Prj_Changed : Boolean;
      Dir_File    : VFS.Virtual_File;

   begin
      Trace (Me, "renaming "
             & Full_Name (File_Information (Context.Context)).all);
      Push_State (Get_Kernel (Context.Context), Busy);

      Is_Dir := not Has_File_Information (Context.Context);

      if Is_Dir then
         Dir_File := VFS.Create (Dir);
         Ensure_Directory (Dir_File);
         Actual_Rename (Dir_File, Success, Prj_Changed);

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

   function Execute
     (Command : access Create_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Dir         : constant String := Directory_Information (Context.Context);
      File        : VFS.Virtual_File;
      Prj_Changed : Boolean;
      Prj_List    : Unbounded_String;

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
               File := Create (Dir & Res);
               Make_Dir (File);
            end if;
         exception
            when Directory_Error =>
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot create dir ") &
                  File.Full_Name.all,
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
            W_File : VFS.Writable_File;
         begin
            if Res /= "" then
               File := Create (Dir & Res);
               W_File := VFS.Write_File (File);
               VFS.Close (W_File);
            end if;
         exception
            when others =>
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot create file ") &
                  File.Full_Name.all,
                  Mode => Error);
               return Commands.Failure;
         end;
      end if;

      GPS.Kernel.File_Saved (Get_Kernel (Context.Context), File);
      Check_Prj
        (Get_Project (Get_Kernel (Context.Context)),
         Create (Dir), Prj_Changed, Prj_List);

      if Prj_Changed then
         Recompute_View (Get_Kernel (Context.Context));
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Filters --
   -------------

   type File_Filter_Record is new Action_Filter_Record with null record;
   type Dir_Filter_Record is new Action_Filter_Record with null record;

   function Filter_Matches_Primitive
     (Context : access File_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   function Filter_Matches_Primitive
     (Context : access Dir_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
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

   function Filter_Matches_Primitive
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
      Explorer_Filter : constant Action_Filter :=
                          Lookup_Filter (Kernel, "Explorer_View")
                          or  Lookup_Filter (Kernel, "File_View");
      File_Filter     : constant Action_Filter :=
                          new File_Filter_Record;
      Dir_Filter      : constant Action_Filter :=
                          new Dir_Filter_Record;
      Command         : Interactive_Command_Access;
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
         Filter => Lookup_Filter (Kernel, "File_View") and Dir_Filter,
         Label  => "File operations/Create a new file");
      Command := new Create_Command;
      Create_Command (Command.all).Create_Dir := True;
      Register_Contextual_Menu
        (Kernel, "Create dir",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "File_View") and Dir_Filter,
         Label  => "File operations/Create a subdirectory");
      Command := new Rename_Command;
      Register_Contextual_Menu
        (Kernel, "Rename file",
         Action => Command,
         Filter => Explorer_Filter and File_Filter,
         Label  => "File operations/Rename file %f");
      Register_Contextual_Menu
        (Kernel, "Rename directory",
         Action => Command,
         Filter => Explorer_Filter and Dir_Filter,
         Label  => "File operations/Rename directory");
      Command := new Delete_Command;
      Register_Contextual_Menu
        (Kernel, "Delete file",
         Action => Command,
         Filter => Explorer_Filter and File_Filter,
         Label  => "File operations/Delete file %f");
      Register_Contextual_Menu
        (Kernel, "Delete directory",
         Action => Command,
         Filter => Explorer_Filter and Dir_Filter,
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
   end Register_Module;

end VFS_Module;
