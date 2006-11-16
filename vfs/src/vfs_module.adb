-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2006                      --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with GNAT.Strings;

with File_Utils;                use File_Utils;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
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

   --------------------------
   -- VFS_Command_Handler --
   --------------------------

   procedure VFS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Success  : Boolean;

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

   begin
      Push_State (Get_Kernel (Context.Context), Busy);
      Trace (Me, "deleting "
             & Full_Name (File_Information (Context.Context)).all);

      if Has_File_Information (Context.Context) then
         Delete (File_Information (Context.Context), Success);
      else
         begin
            Remove_Dir (Dir, True);
         exception
            when Directory_Error =>
               Console.Insert
                 (Get_Kernel (Context.Context),
                  (-"Cannot remove directory: ") & Dir,
                  Mode => Error);
         end;
      end if;

      --  ??? Need also to update project/file views
      Pop_State (Get_Kernel (Context.Context));
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      --  Command : Interactive_Command_Access;
   begin
      VFS_Module_Id := new Module_ID_Record;

      Register_Module
        (Module      => VFS_Module_Id,
         Kernel      => Kernel,
         Module_Name => VFS_Module_Name,
         Priority    => Default_Priority);

      --  ??? these commands are not integrated yet properly:
      --  missing confirmation dialog, as well as project recomputing
      --  Command := new Delete_Command;
      --  Register_Contextual_Menu
      --    (Kernel, "Delete file",
      --     Action => Command,
      --     Filter => Lookup_Filter (Kernel, "Explorer_File_Node"));
      --  Register_Contextual_Menu
      --    (Kernel, "Delete directory recursively",
      --     Action => Command,
      --     Filter => Lookup_Filter (Kernel, "Explorer_Directory_Node"));

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
