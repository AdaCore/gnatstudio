------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;      use GNATCOLL.VFS_Utils;
with OS_Utils;                use OS_Utils;
with Remote;                  use Remote;
with GPS.Core_Kernels;        use GPS.Core_Kernels;
with Language_Handlers;       use Language_Handlers;

package body GPS.Scripts.Files is

   type File_Properties_Record is new Instance_Property_Record with record
      File : Virtual_File;
   end record;

   procedure File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "File" commands

   function Get_File_Class (Data : Callback_Data'Class) return Class_Type;

   procedure Set_Data (Instance : Class_Instance; File : Virtual_File);

   File_Class_Name          : constant String := "File";

   Name_Cst       : aliased constant String := "name";
   Local_Cst      : aliased constant String := "local";
   Server_Cst     : aliased constant String := "remote_server";

   File_Cmd_Parameters   : constant Cst_Argument_List :=
                                (1 => Name_Cst'Access,
                                 2 => Local_Cst'Access);
   File_Name_Parameters  : constant Cst_Argument_List :=
                                (1 => Server_Cst'Access);
   -----------------
   -- Create_File --
   -----------------

   function Create_File
     (Script : access Scripting_Language_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script, New_Class (Get_Repository (Script), File_Class_Name));
   begin
      Set_Data (Instance, File);
      return Instance;
   end Create_File;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance) return GNATCOLL.VFS.Virtual_File
   is
      Data : Instance_Property;
   begin
      if Instance /= No_Class_Instance then
         Data := Get_Data (Instance, File_Class_Name);
      end if;

      if Data = null then
         return GNATCOLL.VFS.No_File;
      else
         return File_Properties_Record (Data.all).File;
      end if;
   end Get_Data;

   --------------------
   -- Get_File_Class --
   --------------------

   function Get_File_Class
     (Data : Callback_Data'Class)
      return Class_Type is
   begin
      return New_Class (Data.Get_Repository, File_Class_Name);
   end Get_File_Class;

   --------------------------
   -- File_Command_Handler --
   --------------------------

   procedure File_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Core_Kernel := Get_Kernel (Data);
      Info    : Virtual_File;
   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, File_Cmd_Parameters);

         declare
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_File_Class (Data));
            Name     : constant Filesystem_String := Nth_Arg (Data, 2);
            File     : Virtual_File;
         begin
            if Is_Absolute_Path (Name) then
               if Is_Cygwin_Path (Name) then
                  --  This is a cygwing PATH style, convert to standard DOS
                  Set_Data
                    (Instance,
                     Create (Format_Pathname (Name, DOS)));
               else
                  Set_Data (Instance, Create (Name));
               end if;
               return;
            end if;

            --  Base name case. Find full name using the following rules:
            --  1) If third argument is set to true, create from current dir
            --  else
            --  2) If Base Name can be found in project, use it
            --  else
            --  3) Create from current dir

            --  If we really want to create from current directory

            if Number_Of_Arguments (Data) > 2 then
               declare
                  From_Current : constant Boolean := Nth_Arg (Data, 3);
               begin
                  if From_Current then
                     Set_Data
                       (Instance,
                        Create_From_Dir (Get_Current_Dir, Name));
                     return;
                  end if;
               end;
            end if;

            --  Kernel's Create_From_Base will override File if needed

            File := Create_From_Base (Name);
            Set_Data
              (Instance, Kernel.Create_From_Base (Full_Name (File)));
         end;

      elsif Command = "name" then
         Name_Parameters (Data, File_Name_Parameters);
         Info := Nth_Arg (Data, 1);

         declare
            Server : Server_Type;
         begin
            --  Get the Server_Type value
            begin
               Server := Server_Type'Value (Nth_Arg (Data, 2, "GPS_Server"));
            exception
               when Constraint_Error =>
                  Server := GPS_Server;
            end;

            if Server = GPS_Server then
               Set_Return_Value (Data, Full_Name (Info));
            else
               Set_Return_Value
                 (Data,
                  Full_Name (To_Remote (Info, Get_Nickname (Server))));
            end if;
         end;

      elsif Command = "directory" then
         Info := Nth_Arg (Data, 1);
         Set_Return_Value (Data, Dir_Name (Info));

      elsif Command = "language" then
         Info := Nth_Arg (Data, 1);
         Set_Return_Value
           (Data, Get_Language_From_File
              (Kernel.Lang_Handler, Info));

      elsif Command = "other_file" then
         Info  := Nth_Arg (Data, 1);
         Set_Return_Value
           (Data,
            Create_File (Get_Script (Data),
                         Kernel.Registry.Tree.Other_File (Info)));
      end if;
   end File_Command_Handler;
   --------------------
   -- Get_File_Class --
   --------------------

   function Get_File_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, File_Class_Name);
   end Get_File_Class;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data'Class; N : Positive)
      return GNATCOLL.VFS.Virtual_File
   is
      Class : constant Class_Type := Get_File_Class (Data);
      Inst  : constant Class_Instance := Nth_Arg (Data, N, Class);
   begin
      return Get_Data (Inst);
   end Nth_Arg;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is
   begin
      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class        => Get_File_Class (Kernel),
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "language",
         Class        => Get_File_Class (Kernel),
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "name",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_File_Class (Kernel),
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "directory",
         Class        => Get_File_Class (Kernel),
         Handler      => File_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "other_file",
         Class        => Get_File_Class (Kernel),
         Handler      => File_Command_Handler'Access);
   end Register_Commands;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Instance : Class_Instance; File : Virtual_File) is
   begin
      if not Is_Subclass (Instance, File_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, File_Class_Name,
         File_Properties_Record'(File => File));
   end Set_Data;

   -----------------
   -- Set_Nth_Arg --
   -----------------

   procedure Set_Nth_Arg
     (Data : in out Callback_Data'Class;
      N    : Positive;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Inst  : constant Class_Instance := Create_File (Get_Script (Data), File);
   begin
      Set_Nth_Arg (Data, N, Inst);
   end Set_Nth_Arg;

end GPS.Scripts.Files;
