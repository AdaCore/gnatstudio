------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2023, AdaCore                     --
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
with Ada.Unchecked_Conversion;
pragma Warnings (Off, ".*is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On, ".*is an internal GNAT unit");

with Interfaces.C.Strings;      use Interfaces.C.Strings;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;

with Glib.Convert;              use Glib.Convert;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Types;

with GPS.Editors;               use GPS.Editors;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with GUI_Utils;                 use GUI_Utils;
with Projects;                  use Projects;
with Remote;                    use Remote;
with GNATCOLL.Traces;                    use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;
with OS_Utils;
with Commands.Interactive;      use Commands, Commands.Interactive;

package body VFS_Module is

   VFS_Module_Name : constant String := "VFS_Module";
   Me              : constant Trace_Handle := Create ("GPS.OTHERS.VFS_MODULE");
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

   Open_Command_Name : constant String := "open file";
   type Open_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open a file selector dialog allowing the user to open a file from
   --  the current context's directory

   -------------
   -- Filters --
   -------------

   type File_Filter_Record is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Context : access File_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   type Dir_Filter_Record is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Dir_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   -----------
   -- Utils --
   -----------

   procedure Rename_In_Prj
     (Kernel : not null access Kernel_Handle_Record'Class;
      File_In, File_Out : Virtual_File;
      Success           : out Boolean);
   --  Rename the path in the projects

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
         Res       : Gtkada.Types.Chars_Ptr;
         B_Read    : aliased Natural;
         B_Written : aliased Natural;
         function Convert is new Ada.Unchecked_Conversion
           (Gtkada.Types.Chars_Ptr, Interfaces.C.Strings.chars_ptr);
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

         Write (File, Convert (Res));

         Gtkada.Types.g_free (Res);
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
            Tmp_Dir  : constant Virtual_File := Get_Tmp_Directory;
            File     : Virtual_File := Create (Filename);
            Writable : Writable_File;
         begin
            if not File.Is_Absolute_Path then
               File := Tmp_Dir / File;
            end if;

            Writable := Write_File (File, Append => True);
            Write
               (Writable, N => 1, LF => Nth_Arg (Data, 3, Default => False));
            Close (Writable);
            Set_Return_Value (Data, File.Full_Name);
         end;

      elsif Command = "base_name" then
         declare
            Filename : constant Filesystem_String := Nth_Arg (Data, 1);
            File     : constant Virtual_File := Create (Filename);
         begin
            Set_Return_Value (Data, File.Base_Name);
         end;

      elsif Command = "dir_name" then
         declare
            Filename : constant Filesystem_String := Nth_Arg (Data, 1);
            File     : constant Virtual_File := Create (Filename);
         begin
            Set_Return_Value (Data, File.Dir_Name);
         end;
      end if;
   end VFS_Command_Handler;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Delete_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel        : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dir           : constant GNATCOLL.VFS.Virtual_File :=
        Directory_Information (Context.Context);
      Success       : Boolean := False;
      Local_Success : Boolean;
      Res           : Gtkada.Dialogs.Message_Dialog_Buttons;
      File          : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Files         : GNATCOLL.VFS.File_Array_Access;
   begin
      Trace (Me, "deleting "
             & File_Information (Context.Context).Display_Full_Name);

      if Has_File_Information (Context.Context) then
         for File of File_Information (Context.Context) loop
            Res := GPS_Message_Dialog
              (-"Are you sure you want to delete " &
                 Display_Full_Name (File) &
                 " ?",
               Gtkada.Dialogs.Confirmation,
               Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No,
               Parent => Kernel.Get_Main_Window);

            if Res = Gtkada.Dialogs.Button_Yes then
               --  inform before deleting
               File_Deleting_Hook.Run (Get_Kernel (Context.Context), File);

               Delete (File, Local_Success);

               if not Local_Success then
                  Get_Kernel (Context.Context).Insert
                    ((-"Cannot remove file: ") &
                       Display_Full_Name (File),
                     Mode => Error);
               else
                  Vcs_Refresh_Hook.Run (Kernel, Is_File_Saved => False);
                  File_Deleted_Hook.Run (Get_Kernel (Context.Context), File);
               end if;
            else
               Local_Success := False;
            end if;

            Success := Local_Success or else Success;
         end loop;
      else
         --  Assign for further use
         File := Dir;

         Res := GPS_Message_Dialog
           (-"Are you sure you want to delete the directory " &
            Dir.Display_Full_Name & (-" and all its subdirectories ?"),
            Gtkada.Dialogs.Confirmation,
            Gtkada.Dialogs.Button_Yes or Gtkada.Dialogs.Button_No,
            Parent => Kernel.Get_Main_Window);

         if Res = Gtkada.Dialogs.Button_Yes then
            --  Is_Directory marks File as directory. This is important
            --  because File cannot be marked as such after deletion of the
            --  physical dir.

            if Dir.Is_Directory and then Dir.Is_Regular_File then
               --  close all editors associated with files
               Files := Read_Dir_Recursive (Dir);
               if Files /= null then
                  for F of Files.all loop
                     File_Deleting_Hook.Run
                       (Get_Kernel (Context.Context), File);
                  end loop;
                  GNATCOLL.VFS.Unchecked_Free (Files);
               end if;

               Dir.Remove_Dir (True, Success);
            else
               Success := False;
            end if;

            if not Success then
               Get_Kernel (Context.Context).Insert
                 ((-"Cannot remove directory: ") & Dir.Display_Full_Name,
                  Mode => Error);
            end if;
         else
            Success := False;
         end if;

         if Success then
            File_Deleted_Hook.Run (Get_Kernel (Context.Context), File);
         end if;
      end if;

      --  Need also to update project/file views
      if Success then
         Reload_Project_If_Needed (Get_Kernel (Context.Context));
         Recompute_View (Get_Kernel (Context.Context));
      end if;

      return Commands.Success;
   end Execute;

   -------------------
   -- Rename_In_Prj --
   -------------------

   procedure Rename_In_Prj
     (Kernel : not null access Kernel_Handle_Record'Class;
      File_In, File_Out : Virtual_File;
      Success           : out Boolean)
   is
      Project  : constant Project_Type :=
        Get_Project (Kernel);
      Relative : Boolean;
      Iter     : Project_Iterator :=
        Start (Project, Include_Extended => False);
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
      Is_Dir  : Boolean;

      procedure Actual_Rename
        (File        : GNATCOLL.VFS.Virtual_File;
         Success     : out Boolean;
         Prj_Changed : out Boolean);
      --  Performs the actual renaming

      -------------------
      -- Actual_Rename --
      -------------------

      procedure Actual_Rename
        (File        : GNATCOLL.VFS.Virtual_File;
         Success     : out Boolean;
         Prj_Changed : out Boolean)
      is
         New_File : Virtual_File := No_File;
      begin
         Prj_Changed := False;

         if Is_Directory (File) then
            declare
               Res : constant String :=
                       GUI_Utils.Query_User
                         (Get_Kernel (Context.Context).Get_Main_Window,
                          (-"Please enter the directory's new name:"),
                          Password_Mode => False,
                          Urgent        => False,
                          Default       => +File.Base_Dir_Name);
            begin
               if Res /= "" then
                  New_File := Create_From_Dir (Get_Parent (File), +Res);
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
                          Default       => +File.Base_Name);
            begin
               if Res /= "" then
                  New_File := Create_From_Dir (File.Dir, +Res);
               end if;
            end;
         end if;

         Rename_File
           (Kernel      => Get_Kernel (Context.Context),
            File        => File,
            New_File    => New_File,
            Success     => Success,
            Prj_Changed => Prj_Changed);
      end Actual_Rename;

      Prj_Changed : Boolean;

   begin
      Trace (Me, "renaming " &
             File_Information (Context.Context).Display_Full_Name);

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

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Create_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel        : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dir           : constant Virtual_File :=
        Directory_Information (Context.Context);
      File          : GNATCOLL.VFS.Virtual_File;
      Project       : Project_Type;
      Project_Saved : Boolean := False;
   begin
      if Command.Create_Dir then
         declare
            Options : Query_User_Option_Array_Type (1 .. 1) :=
              (1 => Query_User_Option_Type'
                 (Label    =>
                      To_Unbounded_String ("Add to source directories"),
                  Value    =>
                    True,
                  Hist_Key =>
                    To_Unbounded_String ("add-to-source-dirs")));
            Res     : constant String :=
              GUI_Utils.Query_User
                (Kernel.Get_Main_Window,
                 (-"Please enter the new directory's name:"),
                 Password_Mode => False,
                 Urgent        => False,
                 Default       => "",
                 Options       => Options'Unrestricted_Access,
                 History_Acc   => Kernel.Get_History);
         begin
            if Res /= "" then
               File := Create_From_Dir (Dir, +Res);
               Make_Dir (File);

               --  if the 'Add to source directories' option is checked, add
               --  the new directory to the project's source directories.

               if Options (Options'First).Value then
                  declare
                     Success : Boolean := False;
                  begin
                     Project := Get_Project_For_File
                       (Get_Registry (Kernel).Tree,
                        Dir);
                     GPS.Kernel.Project.Add_Source_Dir
                       (Project            => Project,
                        Dir                => File,
                        Success            => Success,
                        Use_Relative_Paths =>
                          Generate_Relative_Paths.Get_Pref);

                     if not Success then
                        Kernel.Insert
                          ("Cannot add '"
                           & File.Display_Base_Name
                           & "' to the project's source directories",
                          Mode => Error);
                     end if;
                  end;
               end if;
            end if;
         exception
            when Directory_Error =>
               Kernel.Insert
                 ((-"Cannot create dir ") &
                  Display_Full_Name (File),
                  Mode => Error);
               return Commands.Failure;
         end;
      else
         declare
            Res : constant String :=
                    GUI_Utils.Query_User
                      (Kernel.Get_Main_Window,
                       (-"Please enter the new file's name:"),
                       Password_Mode => False,
                       Urgent        => False,
                       Default       => "");
            W_File : GNATCOLL.VFS.Writable_File;
         begin
            if Res /= "" then
               --  ??? Should fill it with a template when appropriate
               File := Create_From_Dir (Dir, +Res);
               W_File := GNATCOLL.VFS.Write_File (File);
               GNATCOLL.VFS.Close (W_File);
            end if;
         exception
            when others =>
               Kernel.Insert
                 ((-"Cannot create file ") &
                  Display_Full_Name (File),
                  Mode => Error);
               return Commands.Failure;
         end;
      end if;

      File_Saved_Hook.Run (Kernel, File);
      Project := Get_Project_For_File
        (Get_Registry (Kernel).Tree, Dir);

      if Project /= No_Project then
         Recompute_View (Kernel);
         Project_Saved := Save_Project (Kernel, Project);

         if not Project_Saved then
            Kernel.Insert
              ("Could not save properly the project", Mode => Error);
         end if;
      end if;

      --  Now that we have recomputed the view, we can open the file, which
      --  will be associated with the right project. Otherwise, it would be
      --  associated with No_Project, and clicking on its name in the project
      --  view would open a new editor for it.

      if not Command.Create_Dir then
         declare
            Buf : constant Editor_Buffer'Class :=
              Get_Buffer_Factory (Kernel).Get
                (File => File);
            pragma Unreferenced (Buf);
         begin
            null;
         end;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle    := Get_Kernel (Context.Context);
      Dir    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
   begin
      if Has_Directory_Information (Context.Context) then
         Dir := Directory_Information (Context.Context);
      end if;

      declare
         Filename : constant Virtual_File :=
           Select_File
             (Title             => -"Open File",
              Base_Directory    => Dir,
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
              Kind              => Open_File,
              File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
              Pattern_Name      => -"All files;Ada files;C/C++ files",
              History           => Get_History (Kernel));
      begin
         if Filename /= GNATCOLL.VFS.No_File then
            --  Open with the first possible project, the user cannot choose
            --  which specific project to use (in the case of aggregates)
            Open_File_Action_Hook.Run
               (Kernel, Filename, Project => No_Project);
         end if;
      end;
      return Standard.Commands.Success;
   end Execute;

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

   -----------------
   -- Rename_File --
   -----------------

   procedure Rename_File
     (Kernel                  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File                    : GNATCOLL.VFS.Virtual_File;
      New_File                : GNATCOLL.VFS.Virtual_File;
      Success                 : out Boolean;
      Prj_Changed             : out Boolean;
      Display_Confirm_Dialogs : Boolean := True)
   is
      Project : Project_Type;
      Button  : Gtkada.Dialogs.Message_Dialog_Buttons;
   begin
      if New_File = File or else New_File = No_File then
         Success := True;
         return;
      end if;

      Rename (File, New_File, Success);

      if not Success then
         Kernel.Insert
           ((-"Cannot rename ") &
              Display_Full_Name (File) &
            (-" into ") &
              Display_Full_Name (New_File),
            Mode => Error);
         return;
      end if;

      --  Run the 'file_renamed' hook

      if Is_Directory (File) then
         Ensure_Directory (New_File);
      end if;

      File_Renamed_Hook.Run
        (Kernel, File, New_File);

      --  First check if file_in is defined in the projects
      Project := Get_Project_For_File
        (Get_Registry (Kernel).Tree, File);

      if Project /= No_Project then
         if Is_Directory (File) then
            --  We need to change the paths defined in the projects

            if Display_Confirm_Dialogs then
               Button := GPS_Message_Dialog
                 (-("The directory is referenced in the project ")
                  & Project.Name & ASCII.LF &
                  (-("Do you want GNAT Studio to modify these projects to " &
                       "reference its new name ?")),
                  Gtkada.Dialogs.Confirmation,
                  Button_Yes or Button_No,
                  Parent => Kernel.Get_Main_Window);
            end if;

            if not Display_Confirm_Dialogs or else Button = Button_Yes then
               Rename_In_Prj
                 (Kernel   => Kernel,
                  File_In  => File,
                  File_Out => New_File,
                  Success  => Prj_Changed);
            end if;

         else
            if Display_Confirm_Dialogs then
               Button := GPS_Message_Dialog
                 (-("The file is referenced in the project ") &
                    Project.Name & ASCII.LF &
                  (-"The project(s) might require manual modifications."),
                  Gtkada.Dialogs.Warning,
                  Button_OK,
                  Parent => Kernel.Get_Main_Window);
            end if;
            Prj_Changed := True;
         end if;
      end if;
   end Rename_File;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File_Explorer_Filter : constant Action_Filter :=
        Lookup_Filter (Kernel, "File_View")
        or (Lookup_Filter (Kernel, "Explorer_View")
            and not Lookup_Filter (Kernel, "Explorer_Toolbar_Filter"));
      --  The backspace key should go in the toolbar of the editor if focused
      File_Filter      : constant Action_Filter := new File_Filter_Record;
      Dir_Filter       : constant Action_Filter := new Dir_Filter_Record;
      Is_Dir           : constant Action_Filter :=
        File_Explorer_Filter and Dir_Filter;
      Is_File          : constant Action_Filter :=
        File_Explorer_Filter and File_Filter;
      Command          : Interactive_Command_Access;
   begin
      VFS_Module_Id := new Module_ID_Record;

      Register_Module
        (Module      => VFS_Module_Id,
         Kernel      => Kernel,
         Module_Name => VFS_Module_Name,
         Priority    => Default_Priority);

      --  Register these filters to make them accessible from other modules
      Register_Filter
        (Kernel,
         Filter => File_Filter,
         Name   => "File_Filter");
      Register_Filter
        (Kernel,
         Filter => Dir_Filter,
         Name   => "Dir_Filter");

      Register_Action
        (Kernel, Open_Command_Name, new Open_Command,
         Description => -"Open an existing file",
         Icon_Name   => "gps-open-file-symbolic");
      Register_Contextual_Menu
        (Kernel,
         Action => Open_Command_Name,
         Label  => "Open file from folder",
         Filter => File_Explorer_Filter and Dir_Filter);
      Register_Contextual_Menu
        (Kernel,
         Action => Open_Command_Name,
         Label  => "Open file from folder containing %f",
         Filter => File_Explorer_Filter and File_Filter);

      Register_Contextual_Submenu
        (Kernel,
         Name   => "File operations",
         Filter => Is_Dir or Is_File,
         Group  => Project_Contextual_Group);

      --  ??? Can we use the command 'new file' instead ?
      Command := new Create_Command;
      Create_Command (Command.all).Create_Dir := False;
      Register_Action
        (Kernel, "create new file",
         Command     => Command,
         Description => "Create a new file in the selected directory",
         Filter      => Is_Dir);
      Register_Contextual_Menu
        (Kernel,
         Action => "create new file",
         Label  => "File operations/Create a new file");

      Command := new Create_Command;
      Create_Command (Command.all).Create_Dir := True;
      Register_Action
        (Kernel, "create new directory",
         Command     => Command,
         Description => "Create a new subdirectory in the selected directory",
         Filter      => Is_Dir);
      Register_Contextual_Menu
        (Kernel,
         Action => "create new directory",
         Label  => "File operations/Create a subdirectory");

      Register_Action
        (Kernel, "rename file",
         Command     => new Rename_Command,
         Description => "Rename the selected file",
         Filter      => Is_File);
      Register_Contextual_Menu
        (Kernel,
         Action => "rename file",
         Label  => "File operations/Rename file %f");

      Register_Action
        (Kernel, "rename directory",
         Command     => new Rename_Command,
         Description => "Rename the selected directory",
         Filter      => Is_Dir);
      Register_Contextual_Menu
        (Kernel,
         Action => "rename directory",
         Label  => "File operations/Rename directory");

      Register_Action
        (Kernel, "delete file",
         Command     => new Delete_Command,
         Description => "Delete the selected file",
         Filter      => Is_File);
      Register_Contextual_Menu
        (Kernel,
         Action => "delete file",
         Label  => "File operations/Delete selected files");

      Register_Action
        (Kernel, "delete directory",
         Command     => new Delete_Command,
         Description => "Delete the selected directory and its subdirectories",
         Filter      => Is_Dir);
      Register_Contextual_Menu
        (Kernel,
         Action => "delete directory",
         Label  => "File operations/Delete directory recursively");

      Kernel.Scripts.Register_Command
        ("pwd",
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("cd",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("delete",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("dir",
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("ls",
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("dump",
         Minimum_Args => 1,
         Maximum_Args => 2,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("dump_file",
         Minimum_Args => 2,
         Maximum_Args => 3,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("base_name",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("dir_name",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
   end Register_Module;

end VFS_Module;
