-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2003                    --
--                            ACT-Europe                             --
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

with Ada.Exceptions;            use Ada.Exceptions;
with File_Utils;                use File_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Widget;                use Gtk.Widget;
with Traces;                    use Traces;
with String_Utils;              use String_Utils;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;

package body VFS_Module is

   VFS_Module_Name : constant String := "VFS_Module";
   Me              : constant Debug_Handle := Create (VFS_Module_Name);
   VFS_Module_Id   : Module_ID;

   -----------------
   -- Subprograms --
   -----------------

   procedure VFS_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the conextual menu if necessary

   procedure VFS_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Interactive command handler for the vfs module.

   procedure On_Delete
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Delete recursively the file or directory described in the context

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
         Directory      : GNAT.OS_Lib.String_Access;
         Dir            : Dir_Type;
         Buffer         : String (1 .. 4096);
         Last           : Natural;

      begin
         if Is_Directory (Pattern) then
            Directory := new String'(Name_As_Directory (Pattern));
            File_Regexp :=
              Compile ("*", Glob => True,
                       Case_Sensitive => Filenames_Are_Case_Sensitive);
         else
            Directory := new String'(Directory_Name);

            if Base = "" then
               File_Regexp :=
                 Compile ("*", Glob => True,
                          Case_Sensitive => Filenames_Are_Case_Sensitive);
            else
               File_Regexp :=
                 Compile (Base, Glob => True,
                          Case_Sensitive => Filenames_Are_Case_Sensitive);
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

      elsif Command = "cd" or else Command = "delete" then
         declare
            File : constant String := Nth_Arg (Data, 1);
         begin
            if Command = "cd" then
               begin
                  Change_Dir (File);
               exception
                  when Directory_Error =>
                     Set_Error_Msg
                       (Data, Command & ": " &
                        (-"cannot change current directory"));
                     return;
               end;

            elsif Command = "delete" then
               if Is_Directory (File) then
                  begin
                     Remove_Dir (File, True);
                  exception
                     when Directory_Error =>
                        Set_Error_Msg
                          (Data, Command & ": " & "cannot delete directory");
                        return;
                  end;

               else
                  Delete_File (File, Success);

                  if not Success then
                     Set_Error_Msg
                       (Data, Command & ": " & (-"cannot delete file"));
                     return;
                  end if;
               end if;
            end if;
         end;

      elsif Command = "dir" or else Command = "ls" then
         Set_Return_Value_As_List (Data);

         if Number_Of_Arguments (Data) = 0 then
            List_Files ("*");
         else
            for A in 1 .. Number_Of_Arguments (Data) loop
               begin
                  List_Files (Nth_Arg (Data, A));
               exception
                  when Error_In_Regexp =>
                     Set_Error_Msg
                       (Data, -"error in regexp: " & Nth_Arg (Data, A));
                     return;
               end;
            end loop;
         end if;
      end if;
   end VFS_Command_Handler;

   ---------------
   -- On_Delete --
   ---------------

   procedure On_Delete
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      File    : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Dir     : constant String := Directory_Information (File);
      Success : Boolean;

   begin
      Push_State (Get_Kernel (File), Busy);
      Trace (Me, "deleting " & Directory_Information (File) &
             File_Information (File));

      if Has_File_Information (File) then
         Delete_File (Dir & File_Information (File), Success);
      else
         begin
            Remove_Dir (Dir, True);
         exception
            when Directory_Error =>
               Console.Insert
                 (Get_Kernel (Context),
                  (-"Cannot remove directory: ") & Dir,
                  Mode => Error);
         end;
      end if;

      --  ??? Need also to update project/file views
      Pop_State (Get_Kernel (File));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (File));
   end On_Delete;

   --------------------
   -- VFS_Contextual --
   --------------------

   procedure VFS_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      File    : File_Selection_Context_Access;
      Submenu : Gtk_Menu;
      Mitem   : Gtk_Menu_Item;

   begin
      return;

      pragma Warnings (Off);

      --  ??? Disabled for now, incomplete. In particular, delete should
      --  not be the first entry. Also, the graphical views are not
      --  resynchronized when a file is deleted.

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File) then
            Gtk_New (Mitem, Label => -"File System");
            Gtk_New (Submenu);
            Set_Submenu (Mitem, Gtk_Widget (Submenu));
            Append (Menu, Mitem);

            if Has_File_Information (File) then
               Gtk_New (Mitem, -"Delete file");
            else
               Gtk_New (Mitem, -"Delete directory recursively");
            end if;

            Append (Submenu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (On_Delete'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end VFS_Contextual;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      VFS_Module_Id := new Module_ID_Record;

      Register_Module
        (Module                  => VFS_Module_Id,
         Kernel                  => Kernel,
         Module_Name             => VFS_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => VFS_Contextual'Access);

      Register_Command
        (Kernel,
         Command      => "pwd",
         Usage        => "() -> None",
         Description  => -"Print name of current/working directory.",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "cd",
         Usage        => "(dir) -> None",
         Description  => -"Change the current directory to dir.",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "delete",
         Usage        => "(name) -> None",
         Description  => -"Delete file/directory name from the file system.",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "dir",
         Usage        => "([pattern]) -> None",
         Description  =>
           -"list files following pattern (all files by default).",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "ls",
         Usage        => "([pattern]) -> list of files",
         Description  =>
           -"list files following pattern (all files by default).",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => VFS_Command_Handler'Access);
   end Register_Module;

end VFS_Module;
