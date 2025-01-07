------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2023, AdaCore                     --
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

with GNAT.Strings;

with GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
pragma Warnings (Off, ".*is an internal GNAT unit");
with GNAT.Expect.TTY.Remote;
pragma Warnings (On, ".*is an internal GNAT unit");

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with GPS.Intl;                  use GPS.Intl;
with GPS.Customizable_Modules;  use GPS.Customizable_Modules;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;
with GPS.Kernel.Remote;         use GPS.Kernel.Remote;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with XML_Parsers;
with XML_Utils;                 use XML_Utils;

with Remote.View;

package body Remote_Module is

   Me : constant Trace_Handle := Create ("GPS.REMOTE.MODULE");

   type Remote_Module_Record is new Module_ID_Record with record
      Database : Remote.Db.Remote_Db_Type_Access;
   end record;

   overriding procedure Customize
     (Module : access Remote_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level);
   overriding procedure Destroy (Module : in out Remote_Module_Record);
   --  See doc for inherited subprogram

   Remote_Module_Id : Module_ID;
   Module_Name : constant String := "Remote_Module";

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been modified

   procedure Remote_Commands_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the "is_local_server" command

   type Open_From_Host_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Open_From_Host_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Project->Open remote menu

   type Open_Remote_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_Remote_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open a file selector allowing the user to open a file on a remote
   --  machine.

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Open_From_Host_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Filename : constant Virtual_File :=
        Select_File
          (-"Open Project",
           File_Pattern    => "*.gpr",
           Pattern_Name    => "Project files",
           Parent          => GPS.Kernel.MDI.Get_Current_Window (Kernel),
           Remote_Browsing => True,
           Kind            => Open_File,
           History         => Get_History (Kernel));
   begin
      if Filename /= GNATCOLL.VFS.No_File then
         GPS.Kernel.Project.Load_Project (Kernel, Filename);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_Remote_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      declare
         Filename : constant Virtual_File :=
           Select_File
             (Title             => -"Open Remote File",
              Parent            => GPS.Kernel.MDI.Get_Current_Window (Kernel),
              Remote_Browsing   => True,
              Use_Native_Dialog => False,
              Kind              => Open_File,
              File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
              Pattern_Name      => -"All files;Ada files;C/C++ files",
              History           => Get_History (Kernel));

      begin
         if Filename /= GNATCOLL.VFS.No_File then
            Open_File_Action_Hook.Run
               (Kernel, Filename, Project => GNATCOLL.Projects.No_Project);
         end if;
      end;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Do_Sync : Boolean;
   begin
      for J in Remote.Distant_Server_Type'Range loop
         if not Remote.Is_Local (J) then
            Do_Sync := True;

            for K in Remote.Distant_Server_Type'First
              .. Remote.Distant_Server_Type'Pred (J)
            loop
               if Remote.Get_Nickname (J) = Remote.Get_Nickname (K) then
                  --  Sync already done
                  Do_Sync := False;
                  exit;
               end if;
            end loop;

            if Do_Sync then
               Synchronize
                 (Kernel_Handle (Kernel),
                  From          => Remote.GPS_Server,
                  To            => J,
                  Blocking      => False,
                  Print_Command => False,
                  Print_Output  => False,
                  Force         => False,
                  Queue_Id      => "file_save_remote",
                  File          => File);
            end if;
         end if;
      end loop;
   end Execute;

   ---------------
   -- Customize --
   ---------------

   overriding procedure Customize
     (Module : access Remote_Module_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : XML_Utils.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (Level, File);

   begin
      Remote.Db.Read_From_XML
        (Db        => Module.Database,
         Kernel    => Module.Get_Kernel,
         Node      => Node,
         Is_System => True);
   end Customize;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Module : in out Remote_Module_Record) is
   begin
      GNAT.Expect.TTY.Remote.Close_All;
      Remote.Db.Free (Module.Database);
      Remote_Module_Id := null;
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Remote_Module_Id := new Remote_Module_Record;
      Register_Module
        (Module      => Remote_Module_Id,
         Kernel      => Kernel,
         Module_Name => Module_Name);
      Remote_Module_Record (Remote_Module_Id.all).Database :=
        Remote.Db.Initialize_Database;

      --  Load user specific machine list
      Load_Remote_Config (Kernel);

      File_Saved_Hook.Add (new On_File_Saved);

      Register_Command
        (Kernel,
         "is_server_local",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Handler      => Remote_Commands_Handler'Access);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "open remote project",
         new Open_From_Host_Command,
         Icon_Name   => "gps-open-project-symbolic",
         Description => -"Open remote project");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "open from host", new Open_Remote_Command,
         Description => -"Open a file from a remote host",
         Icon_Name   => "gps-open-file-symbolic");

      Remote.View.Register_Module (Kernel);
   end Register_Module;

   -----------------------------
   -- Remote_Commands_Handler --
   -----------------------------

   procedure Remote_Commands_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Server : Remote.Server_Type;
   begin
      if Command = "is_server_local" then
         Server := Remote.Server_Type'Value (Nth_Arg (Data, 1));
         GNATCOLL.Scripts.Set_Return_Value (Data, Remote.Is_Local (Server));
      end if;
   exception
      when others =>
         GNATCOLL.Scripts.Set_Return_Value (Data, True);
   end Remote_Commands_Handler;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database return access Remote.Db.Remote_Db_Type is
   begin
      return Remote_Module_Record (Remote_Module_Id.all).Database;
   end Get_Database;

   ------------------------
   -- Load_Remote_Config --
   ------------------------

   procedure Load_Remote_Config
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename    : constant Virtual_File :=
                      Create_From_Dir (Get_Home_Dir (Kernel), "remote.xml");
      File, Child : Node_Ptr;
      Err         : GNAT.Strings.String_Access;

   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);

         else
            Child := File.Child;
            while Child /= null loop
               Remote.Db.Read_From_XML
                 (Db        => Get_Database,
                  Kernel    => Kernel,
                  Node      => Child,
                  Is_System => False);
               Child := Child.Next;
            end loop;
         end if;

         Free (File);
      end if;
   end Load_Remote_Config;

   ------------------------
   -- Save_Remote_Config --
   ------------------------

   procedure Save_Remote_Config
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filename : constant Virtual_File :=
                   Create_From_Dir
                     (Get_Home_Dir (Kernel), "remote.xml");
      File     : Node_Ptr;
      Success  : Boolean;

   begin
      Trace (Me, "Saving " & Filename.Display_Full_Name);

      File := new Node;
      File.Tag := new String'("remote_config");

      Remote.Db.Save_To_XML (Get_Database, File);

      XML_Utils.Print (File, Filename, Success);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      elsif Active (Me) then
         Trace (Me, Filename.Display_Full_Name & " saved.");
      end if;
   end Save_Remote_Config;

end Remote_Module;
