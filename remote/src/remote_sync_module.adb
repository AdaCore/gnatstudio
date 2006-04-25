-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.Regpat;            use GNAT.Regpat;
with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);

with Glib.Xml_Int;       use Glib.Xml_Int;

with GPS.Kernel.Console; use GPS.Kernel.Console;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.Kernel.Remote;  use GPS.Kernel.Remote;
with GPS.Kernel.Timeout; use GPS.Kernel.Timeout;

with Commands;           use Commands;
with Filesystem;         use Filesystem;
with Password_Manager;   use Password_Manager;
with String_Utils;       use String_Utils;
with Traces;             use Traces;
with VFS;                use VFS;

package body Remote_Sync_Module is

   Me : constant Debug_Handle := Create ("remote_sync_module");

   type Rsync_Module_Record is new Module_ID_Record with record
      Kernel     : Kernel_Handle;
      Rsync_Args : String_List_Access;
   end record;
   type Rsync_Module_ID is access all Rsync_Module_Record'Class;

   procedure Customize
     (Module : access Rsync_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   --  See doc for inherited subprogram

   Rsync_Module : Rsync_Module_ID := null;

   type Rsync_Callback_Data is new Callback_Data_Record with record
      Network_Name      : String_Access;
      User_Name         : String_Access;
      Nb_Password_Tries : Natural;
   end record;

   function On_Rsync_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean;
   --  run RSync hook

   procedure Parse_Rsync_Output
     (Data : Process_Data; Output : String);
   --  Called whenever new output from rsync is available

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      --  Register the module
      Rsync_Module := new Rsync_Module_Record;
      Rsync_Module.Kernel := Kernel;
      Register_Module
        (Rsync_Module, Kernel, "rsync");

      Add_Hook (Kernel, Rsync_Action_Hook,
                Wrapper (On_Rsync_Hook'Access),
                "rsync");
   end Register_Module;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Rsync_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Child   : Node_Ptr;
   begin
      if Node.Tag.all = "rsync_configuration" then
         Trace (Me, "Customize: 'rsync_configuration'");
         Child := Find_Tag (Node.Child, "arguments");
         if Child /= null then
            Module.Rsync_Args := Argument_String_To_List (Child.Value.all);
         end if;
      end if;
   end Customize;

   -------------------
   -- On_Rsync_Hook --
   -------------------

   function On_Rsync_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      Rsync_Data    : Rsync_Hooks_Args renames Rsync_Hooks_Args (Data.all);
      Src_Path      : String_Access;
      Dest_Path     : String_Access;
      Src_FS        : Filesystem_Access;
      Dest_FS       : Filesystem_Access;
      Machine       : Machine_Descriptor;
      Success       : Boolean;
      Transport_Arg : String_Access;
      Cb_Data       : Rsync_Callback_Data;

      function  Build_Arg return String_List;
      --  Build rsync arguments

      ---------------
      -- Build_Arg --
      ---------------

      function Build_Arg return String_List is
         Rsync_Args    : constant String_List
           := Clone (Rsync_Module.Rsync_Args.all);
      begin
         if Transport_Arg /= null then
            if Rsync_Data.Sync_Deleted then
               return (1 => new String'("--delete")) &
                      Rsync_Args & Transport_Arg & Src_Path & Dest_Path;
            else
               return (1 => new String'("--update")) &
                      Rsync_Args & Transport_Arg & Src_Path & Dest_Path;
            end if;
         else
            if Rsync_Data.Sync_Deleted then
               return (1 => new String'("--delete")) &
                      Rsync_Args & Src_Path & Dest_Path;
            else
               return (1 => new String'("--update")) &
                      Rsync_Args & Src_Path & Dest_Path;
            end if;
         end if;
      end Build_Arg;
   begin
      --  Check that we want to use rsync
      if Rsync_Data.Tool_Name /= "rsync" then
         return False;
      end if;

      --  Check the module configuration
      if Rsync_Module = null or else Rsync_Module.Rsync_Args = null then
         Insert (Kernel, "Invalid rsync configuration. Cannot use rsync.",
                 Mode => Error);
         return False;
      end if;

      if Rsync_Data.Src_Name = "" then
         --  Local src machine, remote dest machine
         Machine := Get_Machine_Descriptor (Rsync_Data.Dest_Name);
         Src_FS    := new Filesystem_Record'Class'(Get_Local_Filesystem);
         Src_Path  := new String'
           (To_Unix (Src_FS.all, Rsync_Data.Src_Path, True));
         Dest_FS   := new Filesystem_Record'Class'
           (Get_Filesystem (Rsync_Data.Dest_Name));
         if Machine.User_Name.all /= "" then
            Dest_Path := new String'
              (Machine.User_Name.all & "@" &
               Machine.Network_Name.all & ":" &
               To_Unix (Dest_FS.all, Rsync_Data.Dest_Path, True));
         else
            Dest_Path := new String'
              (Machine.Network_Name.all & ":" &
               To_Unix (Dest_FS.all, Rsync_Data.Dest_Path, True));
         end if;
      else
         --  Remote src machine, local dest machine
         Machine := Get_Machine_Descriptor (Rsync_Data.Src_Name);
         Src_FS    := new Filesystem_Record'Class'
           (Get_Filesystem (Rsync_Data.Src_Name));
         if Machine.User_Name.all /= "" then
            Src_Path  := new String'
              (Machine.User_Name.all & "@" &
               Machine.Network_Name.all & ":" &
               To_Unix (Src_FS.all, Rsync_Data.Src_Path, True));
         else
            Src_Path  := new String'
              (Machine.Network_Name.all & ":" &
               To_Unix (Src_FS.all, Rsync_Data.Src_Path, True));
         end if;
         Dest_FS   := new Filesystem_Record'Class'(Get_Local_Filesystem);
         Dest_Path := new String'
           (To_Unix (Dest_FS.all, Rsync_Data.Dest_Path, True));
      end if;

      if Machine = null then
         return False;
      end if;

      if Machine.Access_Name.all = "ssh" then
         Transport_Arg := new String'("--rsh=ssh");
      end if;

      Cb_Data := (Network_Name     => Machine.Network_Name,
                  User_Name        => Machine.User_Name,
                  Nb_Password_Tries => 0);

      Launch_Process
        (Kernel_Handle (Kernel),
         Command       => "rsync",
         Arguments     => Build_Arg,
         Console       => Get_Console (Kernel),
         Show_Command  => False,
         Show_Output   => False,
         Success       => Success,
         Line_By_Line  => True,
         Callback      => Parse_Rsync_Output'Access,
         Callback_Data => new Rsync_Callback_Data'(Cb_Data),
         Queue_Id      => Rsync_Data.Queue_Id,
         Synchronous   => Rsync_Data.Synchronous);
      Free (Src_Path);
      Free (Dest_Path);
      Free (Transport_Arg);
      return Success;
   end On_Rsync_Hook;

   ------------------------
   -- Parse_Rsync_Output --
   ------------------------

   procedure Parse_Rsync_Output
     (Data : Process_Data; Output : String)
   is
      Progress_Regexp : constant Pattern_Matcher := Compile
        ("^.*\(([0-9]*), [0-9.%]* of ([0-9]*)", Multiple_Lines);
      Matched         : Match_Array (0 .. 2);
      File_Nb         : Natural;
      Total_Files     : Natural;
      Force           : Boolean;
      Cb_Data         : Rsync_Callback_Data renames
        Rsync_Callback_Data (Data.Callback_Data.all);
   begin
      Trace (Me, "Parse_Rsync_Output: '" & Output & "'");
      if not Data.Process_Died then
         --  Retrieve password prompt if any
         Match (Get_Default_Password_Regexp,
                Output,
                Matched);
         if Matched (0) /= No_Match then
            Force := Cb_Data.Nb_Password_Tries > 0;
            Cb_Data.Nb_Password_Tries := Cb_Data.Nb_Password_Tries + 1;

            declare
               Password : constant String :=
                            Get_Password (Get_Main_Window (Data.Kernel),
                                          Cb_Data.Network_Name.all,
                                          Cb_Data.User_Name.all,
                                          Force);
            begin
               if Password = "" then
                  Interrupt (Data.Descriptor.all);
               else
                  Send (Data.Descriptor.all, Password);
               end if;
            end;

            return;
         end if;

         --  Retrieve passphrase prompt if any
         Match (Get_Default_Passphrase_Regexp,
                Output,
                Matched);
         if Matched (0) /= No_Match then
            Force := Cb_Data.Nb_Password_Tries > 0;
            Cb_Data.Nb_Password_Tries := Cb_Data.Nb_Password_Tries + 1;

            declare
               Password : constant String :=
                            Get_Passphrase
                              (Get_Main_Window (Data.Kernel),
                               Output (Matched (1).First .. Matched (1).Last),
                               Force);
            begin
               if Password = "" then
                  Interrupt (Data.Descriptor.all);
               else
                  Send (Data.Descriptor.all, Password);
               end if;
            end;

            return;
         end if;

         --  Retrieve progression.
         Match (Progress_Regexp,
                Output,
                Matched);
         if Matched (0) /= No_Match then
            File_Nb := Natural'Value
              (Output (Matched (1).First .. Matched (1).Last));
            Total_Files := Natural'Value
              (Output (Matched (2).First .. Matched (2).Last));
            Set_Progress (Data.Command,
              Progress => (Activity => Running,
                           Current  => File_Nb,
                           Total    => Total_Files));
         end if;
      end if;
   end Parse_Rsync_Output;

end Remote_Sync_Module;
