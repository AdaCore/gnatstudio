------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2025, AdaCore                     --
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

--  Description of the system for synchronizing editors
--  ===================================================
--
--  Editors need to be kept in sync with the LSP servers: the server needs
--  to be notified with didOpen when editors open, didClose when they close,
--  and didChange when the contents change.
--
--  This is handled in two separate places: here in GS.LSP_Module by reacting
--  to hooks, and in GS.LSP_Client.Editors by implementing an editor
--  listener.
--
--    In GS.LSP_Module:
--       -  didOpen   ->  sent in reaction to File_Edited_Hook
--                        sent in reaction to File_Renamed_Hook
--                    ->  sent for all open editors, when a LSP server
--                        is started whilst editors are already open,
--                        or while reloading a project
--       -  didClose  ->  sent in reaction to File_Closed_Hook
--                        sent in reaction to File_Renamed_Hook
--
--    In GS.LSP_Client.Editors:
--       -  didChange ->  sent in reaction to After_Insert_Text

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Basic_Types;
with GNAT.Strings;
with GNAT.OS_Lib;  use GNAT.OS_Lib;

with Config;               use Config;
with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;

with Glib.Convert;

with GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.Utils;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with GPS.Kernel.Messages.Hyperlink;
with GPS.LSP_Client.Editors.Semantic_Tokens;
with VSS.Application;
with VSS.Regular_Expressions;
with VSS.Strings.Conversions;
with VSS.Unicode;

with Default_Preferences; use Default_Preferences;
with GPS.Core_Kernels;
with GPS.Default_Styles;
with GPS.Editors;
with GPS.Editors.Line_Information;      use GPS.Editors.Line_Information;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;               use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;        use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages.References;    use GPS.Kernel.Messages.References;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Task_Manager;           use GPS.Kernel.Task_Manager;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Project;

with GPS.LSP_Client.Call_Tree;
with GPS.LSP_Client.Configurations.ALS;
with GPS.LSP_Client.Configurations.Clangd;
with GPS.LSP_Client.Completion;
with GPS.LSP_Client.Dependency_Browers;
with GPS.LSP_Client.Editors;            use GPS.LSP_Client.Editors;
with GPS.LSP_Client.Outline;
with GPS.LSP_Client.Editors.Code_Actions;
with GPS.LSP_Client.Editors.Highlight;
with GPS.LSP_Client.Editors.Folding;
with GPS.LSP_Client.Editors.Formatting;
with GPS.LSP_Client.Editors.Navigation; use GPS.LSP_Client.Editors.Navigation;
with GPS.LSP_Client.Editors.Signature_Help;
with GPS.LSP_Client.Editors.Tooltips;   use GPS.LSP_Client.Editors.Tooltips;
with GPS.LSP_Client.Language_Servers;   use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Language_Servers.Interceptors;
with GPS.LSP_Client.Language_Servers.Real;
with GPS.LSP_Client.Language_Servers.Stub;
with GPS.LSP_Client.References;
with GPS.LSP_Client.Refactoring;
with GPS.LSP_Client.Requests.Execute_Command.Reload_Project;
with GPS.LSP_Client.Search;
with GPS.LSP_Client.Shell;
with GPS.LSP_Client.Tasks;
with GPS.LSP_Client.Utilities;
with GPS.LSP_Clients;
with GPS.Messages_Windows;              use GPS.Messages_Windows;
with GPS.Scripts.Commands;              use GPS.Scripts.Commands;
with Language;                          use Language;
with LSP.Client_Notification_Receivers;
with LSP.Messages;
with LSP.Types;                         use LSP.Types;
with Outline_View;                      use Outline_View;
with Src_Editor_Buffer;
with Src_Editor_Module;                 use Src_Editor_Module;
with Language.Cpp;
with Language.C;

package body GPS.LSP_Module is

   Me_Ada_Support : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.ADA_SUPPORT", GNATCOLL.Traces.On);
   --  General ALS support for Ada

   Me_GPR_Support : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.GPR_SUPPORT", GNATCOLL.Traces.On);
   --  General ALS support for GPR

   Me_Cpp_Support : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.CPP_SUPPORT", GNATCOLL.Traces.On);

   Me_Code_Actions : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.CODE_ACTIONS", GNATCOLL.Traces.On);
   --  Whether to support code actions

   Me_LSP_Logs  : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.LOGS", GNATCOLL.Traces.On);
   --  Whether to log the LSP notifications that arrive with the 'log' type

   Me_LSP_FORMATTING : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.FORMATTING", GNATCOLL.Traces.On);
   --  Enable/disable overall LSP formatting

   Me_LSP_FORMATTING_ADA : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("GPS.LSP.FORMATTING.ADA", GNATCOLL.Traces.Off);
   --  Enable/disable Ada LSP formatting

   Progress_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression
       ("([0-9][0-9]*)/([0-9][0-9]*)");  --  &1 - processed, &2 - total files

   Log_File_Pattern : constant VSS.Regular_Expressions.Regular_Expression :=
     VSS.Regular_Expressions.To_Regular_Expression
       ("Log file is: ([^\n]*)");  --  &1 - path

   type Listener_Factory is
     new GPS.Core_Kernels.Editor_Listener_Factory with null record;

   overriding function Create
     (Self    : Listener_Factory;
      Editor  : GPS.Editors.Editor_Buffer'Class;
      Factory : GPS.Editors.Editor_Buffer_Factory'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel)
      return GPS.Editors.Editor_Listener_Access;

   procedure Spawn_LSP_Server
     (Language : Standard.Language.Language_Access;
      Kernel   : not null access Kernel_Handle_Record'Class);
   --  Setup new language server for given language.

   function Share_Same_Server
     (A : Language.Language_Access; B : Language.Language_Access)
      return Boolean;
   --  Return True if both languages share the same language server.
   --  This is the case of C and C++ for instance.
   --  This function should be used when trying to find if a server is already
   --  running for a given language.

   function Hash (Value : Language_Access) return Ada.Containers.Hash_Type
   is
     (if Value = Language.Cpp.Cpp_Lang then
         Language.Hash (Language.C.C_Lang)
      else
         Language.Hash (Value));
   --  Use a custom hash function for our language server maps: C and C++
   --  should be considered as the same language for clangd.

   package Language_Server_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Language_Access,
      Element_Type    => Language_Server_Access,
      Hash            => Hash,
      Equivalent_Keys => Share_Same_Server,
      "="             => GPS.LSP_Client.Language_Servers."=");

   -------------------------
   -- Monitoring commands --
   -------------------------

   type Language_Server_Progress_Command is new Root_Command with record
      Title  : VSS.Strings.Virtual_String;
      --  The title that should show in the progress bar

      Key    : LSP_Number_Or_String;
      --  The key of the Command in Module.Token_To_Command

      Action : Command_Return_Type := Execute_Again;
      --  What to do at the next call to Execute
   end record;

   type Language_Server_Progress_Command_Access is access all
     Language_Server_Progress_Command;
   --  A command that does nothing except materialize the progress of a given
   --  progressToken in the language server.

   overriding function Execute
     (Command : access Language_Server_Progress_Command)
      return Command_Return_Type is (Command.Action);
   --  TODO: make the command self-destruct when it no longer finds itself
   --  in the associated array

   overriding procedure Interrupt
     (Command : in out Language_Server_Progress_Command);

   type Restart_Command is new Interactive_Command with record
      Kernel        : Kernel_Handle;
      Language_Name : Unbounded_String;
   end record;
   overriding function Execute
     (Self    : access Restart_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   overriding function Name
     (Command : access Language_Server_Progress_Command) return String is
      (VSS.Strings.Conversions.To_UTF_8_String (Command.Title));

   package Token_Command_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LSP_Number_Or_String,
      Element_Type    => Scheduled_Command_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   ------------
   -- Module --
   ------------

   type Module_Id_Record is
     new GPS.Kernel.Modules.Module_ID_Record
     and LSP.Client_Notification_Receivers.Client_Notification_Receiver
     and GPS.LSP_Client.Language_Servers.Interceptors.Server_Listener
   with record
      Language_Servers : Language_Server_Maps.Map;
      --  Map from language to LSP client

      Unknown_Server   : Language_Server_Access;
      --  Pseudo-server to handle managed text documents of language not
      --  supported by any configured language servers.

      Token_To_Command : Token_Command_Maps.Map;
      --  Associate the progress token to the Scheduled_Command monitoring it
   end record;

   type LSP_Module_Id is access all Module_Id_Record'Class;

   overriding procedure Destroy (Self : in out Module_Id_Record);
   --  Stops all language server processes and cleanup data structures.

   procedure Remove_Diagnostics
     (Self : in out Module_Id_Record'Class;
      File : GNATCOLL.VFS.Virtual_File);
   --  Remove the diagnostics corresponding to File from the interface

   function Get_Diagnostics_Message_Flags return Message_Flags
   is
     (case LSP_Diagnostics_Display_Policy'(LSP_Diagnostics_Display.Get_Pref)
      is
         when Editor_And_Locations => Side_And_Locations,
         when Editor_Only          => Sides_Only);
   --  Return the message flags for diagnostics, depending on the corresponding
   --  preference.

   overriding procedure On_Server_Started
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access);

   overriding procedure On_Server_Stopped
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access);

   overriding procedure On_Response_Processed
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Unbounded_String;
      Method : Unbounded_String);

   overriding procedure On_Response_Sent
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Unbounded_String);

   overriding procedure On_Publish_Diagnostics
     (Self   : access Module_Id_Record;
      Params : LSP.Messages.PublishDiagnosticsParams);

   overriding procedure On_Show_Message
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.ShowMessageParams);

   overriding procedure On_Log_Message
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.LogMessageParams);

   overriding function Get_Progress_Type
     (Self  : access Module_Id_Record;
      Token : LSP.Types.LSP_Number_Or_String)
      return LSP.Client_Notification_Receivers.Progress_Value_Kind;

   overriding procedure On_Progress
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.Progress_Params);

   overriding procedure On_Progress_SymbolInformation_Vector
     (Self   : access Module_Id_Record;
      Params : LSP.Messages.Progress_SymbolInformation_Vector);

   procedure Initiate_Server_Shutdown
     (Server         : not null
        GPS.LSP_Client.Language_Servers.Language_Server_Access;
      In_Destruction : Boolean);
   --  Initiate shutdown of the given language server for given language.
   --  In_Destruction should be set to True if the GS kernel is being
   --  destroyed.

   procedure Initiate_Servers_Shutdown
     (Self           : in out Module_Id_Record'Class;
      Servers        : in out Language_Server_Maps.Map'Class;
      In_Destruction : Boolean := False);
   --  Initiate shutdown of the given set of language servers and clears
   --  the set.

   function Lookup_Language
     (Self   : Module_Id_Record'Class;
      Server : not null Language_Server_Access) return Language_Access;
   --  Lookup for language associated with givent language server.

   Module : LSP_Module_Id;

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been opened

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called before closing a buffer.

   type On_File_Renamed is new File2_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      From   : GNATCOLL.VFS.Virtual_File;
      To     : GNATCOLL.VFS.Virtual_File);
   --  Called when file buffer is renamed.

   type On_Location_Changed is new File_Location_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type);
   --  React to a change in the editor location, to support requesting
   --  Code Actions at the point of the cursor.

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Project_Changing;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File);
   --  Called when project will be unloaded.

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Project_View_Changed;
       Kernel : not null access Kernel_Handle_Record'Class);

   type On_Variable_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Variable_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Send didConfigurationChanage notification to language servers when a
   --  scenario variable changes.

   type On_Preference_Changed is
     new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Preference_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Detect change of default encoding and send didConfigurationChanage
   --  notification to language servers.

   type On_Server_Stopped_Hook is
     new Language_Server_Lifecycle_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Server_Stopped_Hook;
       Kernel : not null access Kernel_Handle_Record'Class;
       Language : String);
   --  Called when the language server for the given language has stopped

   function Get_Server_For_File
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Language_Server_Access;
   --  Return the running server supporting File if it exists, or null

   Diagnostics_Messages_Category_Prefix :
     constant VSS.Strings.Virtual_String := "Diagnostics";

   -----------------------
   -- Share_Same_Server --
   -----------------------

   function Share_Same_Server
     (A : Language.Language_Access; B : Language.Language_Access)
      return Boolean is
   begin
      if A /= null and then B /= null then
         declare
            Lang_Name_A : constant String :=  Ada.Characters.Handling.To_Lower
              (A.Get_Name);
            Lang_Name_B : constant String := Ada.Characters.Handling.To_Lower
              (B.Get_Name);
         begin
            if Lang_Name_A in "c" | "cpp" | "c++"
              and then Lang_Name_B in "c" | "cpp" | "c++"
            then
               return True;
            else
               return Lang_Name_A = Lang_Name_B;
            end if;
         end;
      end if;

      return False;
   end Share_Same_Server;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self    : Listener_Factory;
      Editor  : GPS.Editors.Editor_Buffer'Class;
      Factory : GPS.Editors.Editor_Buffer_Factory'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel)
      return GPS.Editors.Editor_Listener_Access
   is
      pragma Unreferenced (Self, Factory);

   begin
      --  Create, initialize and return object to integrate source editor and
      --  language server.

      return Result : constant GPS.Editors.Editor_Listener_Access :=
        new GPS.LSP_Client.Editors.Src_Editor_Handler
          (GPS.Kernel.Kernel_Handle (Kernel))
      do
         declare
            Handler : GPS.LSP_Client.Editors.Src_Editor_Handler'Class
            renames GPS.LSP_Client.Editors.Src_Editor_Handler'Class
              (Result.all);
         begin
            Handler.Initialize (Editor.File);
         end;
      end return;
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Module_Id_Record) is
   begin
      Self.Initiate_Servers_Shutdown (Self.Language_Servers, True);
   end Destroy;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Server : constant Language_Server_Access :=
        Get_Server_For_File (Kernel, File);
   begin
      Module.Remove_Diagnostics (File);
      if Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (File);
      end if;
   end Execute;

   -------------------------
   -- Get_Server_For_File --
   -------------------------

   function Get_Server_For_File
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Language_Server_Access
   is
      Lang     : constant not null Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Cursor   : constant Language_Server_Maps.Cursor :=
                   Module.Language_Servers.Find (Lang);
   begin
      if Language_Server_Maps.Has_Element (Cursor) then
         return Language_Server_Maps.Element (Cursor);
      end if;
      return null;
   end Get_Server_For_File;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Server : constant Language_Server_Access :=
        Get_Server_For_File (Kernel, File);

   begin
      if Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Open (File);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      From   : GNATCOLL.VFS.Virtual_File;
      To     : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Self);

      Server : Language_Server_Access;
   begin
      --  Note: query the server twice to support renaming from one language
      --  to another.
      Server := Get_Server_For_File (Kernel, From);
      if Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Close (From);
      end if;

      Server := Get_Server_For_File (Kernel, To);
      if Server /= null then
         Server.Get_Client.Send_Text_Document_Did_Open (To);
      end if;
      Module.Remove_Diagnostics (From);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Preference_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Self, Kernel);

   begin
      --  Send the 'didChangeConfiguration' notitification to the running LSP
      --  servers when the changed preference can affect them.
      if Pref /= null
        and then (Pref.Get_Name = "General-Charset"
                  or Pref.Get_Name = "Doc-Search-Before-First"
                  or Pref.Get_Name = "Src-Editor-Fold-Comments"
                  or GNATCOLL.Utils.Starts_With
                    (Pref.Get_Name, "LSP-"))
      then
         for Server of Module.Language_Servers loop
            Server.Configuration_Changed;
         end loop;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Variable_Changed;
      Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      for Server of Module.Language_Servers loop
         Server.Configuration_Changed;
      end loop;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Project_Changing;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File) is
      pragma Unreferenced (Self, Kernel, File);
   begin
      --  Shutdown all running language servers.

      Module.Initiate_Servers_Shutdown (Module.Language_Servers);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type)
   is
      Server  : Language_Server_Access;
   begin
      --  Sanity check that we do have a language server for this language
      Server := Get_Server_For_File (Kernel, File);

      if Server = null then
         return;
      end if;

      --  After each location change, request whether there are CodeActions
      --  available for this location.
      declare
         Capabilities : constant LSP.Messages.ServerCapabilities :=
           Server.Get_Client.Capabilities;
      begin
         if Me_Code_Actions.Active
           and then Capabilities.codeActionProvider.Is_Set
         then
            GPS.LSP_Client.Editors.Code_Actions.Request_Code_Action
              (Kernel, File);
         end if;
      end;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Restart_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Server : constant Language_Server_Access :=
        Get_Language_Server
          (Language => Get_Language_Handler (Self.Kernel).Get_Language_By_Name
           (To_String (Self.Language_Name)));
   begin
      if Server /= null then
         Restart_Server (Server);
      end if;

      return Commands.Success;
   end Execute;

   ----------------------
   -- Spawn_LSP_Server --
   ----------------------

   procedure Spawn_LSP_Server
     (Language : Standard.Language.Language_Access;
      Kernel   : not null access Kernel_Handle_Record'Class)
   is
      Cmd_Line_Args_Pref : String_Preference;
      --  The prerence used to add custom arguments to the server's command
      --  line.

      procedure Create_Preferences
        (Server_Program : Virtual_File);
      --  Create the preferences for the given server.

      ------------------------
      -- Create_Preferences --
      ------------------------

      procedure Create_Preferences
        (Server_Program : Virtual_File)
      is
         Server_Base_Name   : constant String :=
           Server_Program.Display_Base_Name;
         Preferences        : constant Preferences_Manager :=
           Kernel.Get_Preferences;
      begin
         Cmd_Line_Args_Pref := Create
           (Manager    => Preferences,
            Path       =>
              "LSP:" & Server_Base_Name,
            Name       => "lsp-cmd_line-" & Server_Base_Name,
            Label      => "Command line arguments",
            Doc        => "Arguments for " & Server_Base_Name & " server. "
            & "You will need to restart GNAT Studio to take changes into "
            & "account.",
            Default    => "",
            Multi_Line => False);
      end Create_Preferences;

      Language_Name : constant String :=
                        Ada.Characters.Handling.To_Lower
                          (Language.Get_Name);
      Configuration :
        GPS.LSP_Client.Configurations.Server_Configuration_Access;
      Server        :
        GPS.LSP_Client.Language_Servers.Language_Server_Access;

      Libexec_GPS : constant Virtual_File :=
        Kernel.Get_System_Dir / "libexec" / "gnatstudio";

      On_Server_Capabilities : GPS.LSP_Clients.On_Server_Capabilities_Proc;

      procedure Setup_ALS (For_GPR : Boolean);
      --  Setup the configuration for the Ada Language Server.
      --  If For_GPR is set to True, the ALS will be configured for the GPR
      --  language instead of Ada.

      ---------------
      -- Setup_ALS --
      ---------------

      procedure Setup_ALS (For_GPR : Boolean) is
         --  Allow the environment variable "GPS_ALS" to override the
         --  location of the ada_language_server.
         --  Otherwise, check if the ada_language_server exe is available
         --  in GNAT Studio's install libexec directory.
         --  If not, fallback on the PATH.

         From_Env       : constant String :=
           VSS.Strings.Conversions.To_UTF_8_String
             (VSS.Application.System_Environment.Value ("GPS_ALS"));
         Libexec_ALS    : constant Virtual_File := Libexec_GPS
           / "als"
           / ("ada_language_server"
              & (if Host = Windows then ".exe" else ""));
         Language_Prefix : constant String :=
           (if For_GPR then "gpr" else "ada");
         Tracefile      : constant Virtual_File :=
           Kernel.Get_Home_Dir / Filesystem_String
             (Language_Prefix & "_ls_traces.cfg");
      begin
         if From_Env /= "" then
            Configuration.Server_Program := Create (+From_Env);
         elsif Libexec_ALS.Is_Regular_File then
            Configuration.Server_Program := Libexec_ALS;
         else
            Configuration.Server_Program :=
              Locate_On_Path ("ada_language_server");
         end if;

         if For_GPR then
            Configuration.Server_Arguments.Append ("--language-gpr");
         end if;

         if Tracefile.Is_Regular_File then
            Configuration.Server_Arguments.Append
              ("--tracefile=" & (+Tracefile.Full_Name.all));
         else
            Me_Ada_Support.Trace
              ("The tracefile for the Ada Language Server"
               & " could not be found");
         end if;
      end Setup_ALS;

   begin
      if Language_Name = "ada"
        and then Me_Ada_Support.Is_Active
      then
         Configuration :=
           new GPS.LSP_Client.Configurations.ALS.ALS_Configuration (Kernel);

         Setup_ALS (For_GPR => False);

      elsif Language_Name = "project file"
        and then Me_GPR_Support.Is_Active
      then
         Configuration :=
           new GPS.LSP_Client.Configurations.ALS.ALS_Configuration (Kernel);

         Setup_ALS (For_GPR => True);

      elsif Language_Name in "c" | "cpp" | "c++"
        and then Me_Cpp_Support.Is_Active
      then
         Configuration := new GPS.LSP_Client.Configurations.Clangd.
           Clangd_Configuration (Kernel);

         On_Server_Capabilities := GPS.LSP_Client.Configurations.Clangd.
           On_Server_Capabilities'Access;

         declare
            --  Allow the environment variable "GPS_CLANGD" to override
            --  the location of the clangd and add a fallback on the
            --  clangd found in the PATH env variable. For development.

            From_Env       : constant String :=
              VSS.Strings.Conversions.To_UTF_8_String
                (VSS.Application.System_Environment.Value ("GPS_CLANGD"));
            Libexec_Clangd : constant Virtual_File := Libexec_GPS
              / "clang" / "bin"
              / ("clangd" & (if Host = Windows then ".exe" else ""));
         begin
            if From_Env /= "" then
               Configuration.Server_Program := Create (+From_Env);
            elsif Libexec_Clangd.Is_Regular_File then
               Configuration.Server_Program := Libexec_Clangd;
            else
               Configuration.Server_Program :=
                 Locate_On_Path ("clangd");
            end if;
         end;

      else
         return;
      end if;

      if not Configuration.Is_Available then
         --  Server is not available, return.

         return;
      end if;

      Create_Preferences (Configuration.Server_Program);

      declare
         User_Cmd_Line_Args : constant String_List_Access :=
           GNATCOLL.Utils.Split
             (Cmd_Line_Args_Pref.Get_Pref, ' ');
      begin
         if User_Cmd_Line_Args /= null then
            for Arg of User_Cmd_Line_Args.all loop
               Configuration.Server_Arguments.Append (Arg.all);
            end loop;
         end if;
      end;

      Src_Editor_Module.Set_Editor_Tooltip_Handler_Factory
        (Tooltip_Factory =>
           Create_LSP_Client_Editor_Tooltip_Handler'Access);

      Configuration.Prepare_Configuration_Settings;

      Server :=
        GPS.LSP_Client.Language_Servers.Real.Create
          (Kernel              => Kernel,
           Configuration       => Configuration,
           Server_Interceptor  => Module,
           Request_Interceptor =>
             GPS.LSP_Client.Tasks.New_Task_Manager_Integration
             (Kernel, Language_Name),
           Language            => Language);

      declare
         S : GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
           renames
             GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
               (Server.all);
      begin
         if Language_Name in "c" | "cpp" | "c++" then
            --  clangd uses stderr for outputing log, redirect it to a file
            GPS.LSP_Client.Configurations.Clangd.Set_Standard_Errors_File
              (Kernel, S.Client);
         end if;

         S.Client.Set_On_Server_Capabilities (On_Server_Capabilities);
         S.Client.Set_Notification_Handler (Module);
         Module.Language_Servers.Insert (Language, Server);
         S.Start;
      end;
   end Spawn_LSP_Server;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      Languages       : GNAT.Strings.String_List :=
        GPS.Kernel.Project.Get_Root_Project_View
          (Kernel).Get_Project_Type.Languages (True)
        & new String'("project file");
      --  Append the 'project file' language (i.e: GPR files) to the project's
      --  own languages: we want to spawn the Ada Language Server for GPR files
      --  in any case.

      Running_Servers : Language_Server_Maps.Map := Module.Language_Servers;
   begin
      Module.Language_Servers.Clear;

      for Language_Name of Languages loop
         declare
            Lang     : constant not null Language.Language_Access :=
                         Kernel.Get_Language_Handler.Get_Language_By_Name
                           (Language_Name.all);
            Position : Language_Server_Maps.Cursor :=
                         Running_Servers.Find (Lang);
            Server   : Language_Server_Access;

         begin
            if Language_Server_Maps.Has_Element (Position)
              --  we should recreate compile_commands.json for clangd
              and then Lang.Get_Name not in "c" | "cpp" | "c++"
            then
               --  Language server for the giving language is configured and
               --  runing; move it to new set of language servers

               Server := Language_Server_Maps.Element (Position);
               Language_Server_Maps.Delete (Running_Servers, Position);
               Module.Language_Servers.Insert (Lang, Server);

               declare
                  Handle : constant GPS.Kernel.Kernel_Handle :=
                    GPS.Kernel.Kernel_Handle (Kernel);

                  Reload_Project : GPS.LSP_Client.Requests.Request_Access :=
                    new GPS.LSP_Client.Requests.Execute_Command.Reload_Project
                      .Reload_Project_Command_Request (Handle);
               begin
                  --  Ask ALS reload the project.

                  Server.Execute (Reload_Project);
               end;
            elsif not Module.Language_Servers.Contains (Lang) then

               --  Spawn a new language server for the given language
               Spawn_LSP_Server
                 (Language => Lang,
                  Kernel   => Kernel);
            end if;

            GNAT.Strings.Free (Language_Name);
         end;
      end loop;

      --  Shutdown running language servers for unused languages

      Module.Initiate_Servers_Shutdown (Running_Servers);
   end Execute;

   -------------------------
   -- Get_Language_Server --
   -------------------------

   function Get_Language_Server
     (Language : not null Standard.Language.Language_Access)
      return GPS.LSP_Client.Language_Servers.Language_Server_Access
   is
      Position : constant Language_Server_Maps.Cursor :=
                   Module.Language_Servers.Find (Language);

   begin
      if Language_Server_Maps.Has_Element (Position) then
         return Language_Server_Maps.Element (Position);

      else
         return null;
      end if;
   end Get_Language_Server;

   -------------------------
   -- Get_Running_Request --
   -------------------------

   function Get_Running_Request
     (Server : not null GPS.LSP_Client.Language_Servers.Language_Server_Access;
      Id     : LSP.Types.LSP_Number_Or_String)
      return GPS.LSP_Client.Requests.Request_Access
   is
      S :  GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
        renames GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
          (Server.all);
   begin
      return GPS.LSP_Client.Language_Servers.Real.Get_Running_Request (S, Id);
   end Get_Running_Request;

   --------------------
   -- Restart_Server --
   --------------------

   procedure Restart_Server
     (Server : not null
        GPS.LSP_Client.Language_Servers.Language_Server_Access)
   is
      S :  GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
        renames GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
          (Server.all);
   begin
      S.Restart;
   end Restart_Server;

   ------------------------------
   -- Initiate_Server_Shutdown --
   ------------------------------

   procedure Initiate_Server_Shutdown
     (Server         : not null
        GPS.LSP_Client.Language_Servers.Language_Server_Access;
      In_Destruction : Boolean)
   is
      S :  GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
        renames GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
          (Server.all);

   begin
      --  Initiate shutdown sequence.

      S.Shutdown (In_Destruction);
   end Initiate_Server_Shutdown;

   -------------------------------
   -- Initiate_Servers_Shutdown --
   -------------------------------

   procedure Initiate_Servers_Shutdown
     (Self           : in out Module_Id_Record'Class;
      Servers        : in out Language_Server_Maps.Map'Class;
      In_Destruction : Boolean := False)
   is
      pragma Unreferenced (Self);
   begin
      while not Servers.Is_Empty loop
         declare
            Position : Language_Server_Maps.Cursor := Servers.First;

         begin
            Initiate_Server_Shutdown
              (Language_Server_Maps.Element (Position), In_Destruction);
            Servers.Delete (Position);
         end;
      end loop;
   end Initiate_Servers_Shutdown;

   ---------------------
   -- Lookup_Language --
   ---------------------

   function Lookup_Language
     (Self   : Module_Id_Record'Class;
      Server : not null Language_Server_Access) return Language_Access
   is
      Position : Language_Server_Maps.Cursor := Self.Language_Servers.First;

   begin
      while Language_Server_Maps.Has_Element (Position) loop
         if Language_Server_Maps.Element (Position) = Server then
            return Language_Server_Maps.Key (Position);
         end if;

         Language_Server_Maps.Next (Position);
      end loop;

      return null;
   end Lookup_Language;

   --------------------
   -- LSP_Is_Enabled --
   --------------------

   function LSP_Is_Enabled
     (Language : not null Standard.Language.Language_Access) return Boolean is
   begin
      return Get_Language_Server (Language) /= null;
   end LSP_Is_Enabled;

   -------------------------------------
   -- LSP_Ada_Support_Trace_Is_Active --
   -------------------------------------

   function LSP_Ada_Support_Is_Active return Boolean is
   begin
      return Me_Ada_Support.Is_Active;
   end LSP_Ada_Support_Is_Active;

   ----------------------------------------
   -- LSP_Ada_Formatting_Trace_Is_Active --
   ----------------------------------------

   function LSP_Ada_Formatting_Is_Active return Boolean is
   begin
      return Me_LSP_FORMATTING.Is_Active
        and then Me_LSP_FORMATTING_ADA.Is_Active;
   end LSP_Ada_Formatting_Is_Active;

   -------------------------------------
   -- LSP_Cpp_Support_Trace_Is_Active --
   -------------------------------------

   function LSP_Cpp_Support_Is_Active return Boolean is
   begin
      return Me_Cpp_Support.Is_Active;
   end LSP_Cpp_Support_Is_Active;

   ------------------------
   -- Remove_Diagnostics --
   ------------------------

   procedure Remove_Diagnostics
     (Self : in out Module_Id_Record'Class;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Container : constant not null GPS.Kernel.Messages_Container_Access :=
        Self.Get_Kernel.Get_Messages_Container;
   begin
      for Category of Container.Get_Categories loop
         if Category.Starts_With (Diagnostics_Messages_Category_Prefix) then
            Container.Remove_File
              (Category => Category,
               File     => File,
               Flags    => Get_Diagnostics_Message_Flags);
         end if;
      end loop;
   end Remove_Diagnostics;

   ---------------------------
   -- On_Response_Processed --
   ---------------------------

   overriding procedure On_Response_Processed
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Unbounded_String;
      Method : Unbounded_String)
   is
      L : constant Language_Access := Self.Lookup_Language (Server);

   begin
      if L /= null then
         --  This is special case for "shutdown" request: module doesn't
         --  know language server at this point

         GPS.Kernel.Hooks.Language_Server_Response_Processed_Hook.Run
           (Kernel   => Self.Get_Kernel,
            Language => L.Get_Name,
            Contents => To_String (Data),
            Method   => To_String (Method));
      end if;
   end On_Response_Processed;

   ----------------------
   -- On_Response_Sent --
   ----------------------

   overriding procedure On_Response_Sent
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Unbounded_String)
   is
      L : constant Language_Access := Self.Lookup_Language (Server);

   begin
      if L /= null then
         GPS.Kernel.Hooks.Language_Client_Response_Sent_Hook.Run
           (Kernel   => Self.Get_Kernel,
            Language => L.Get_Name,
            Contents => To_String (Data),
            Method   => "");
      end if;
   end On_Response_Sent;

   -----------------------
   -- On_Server_Started --
   -----------------------

   overriding procedure On_Server_Started
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access)
   is
      Kernel   : constant Kernel_Handle := Self.Get_Kernel;
      Language : constant Language_Access := Self.Lookup_Language (Server);

      procedure Create_Restart_Action_If_Needed (Language_Name : String);
      --  Create an action to restart the language server

      -------------------------------------
      -- Create_Restart_Action_If_Needed --
      -------------------------------------

      procedure Create_Restart_Action_If_Needed (Language_Name : String)
      is
         Action_Name : constant String := "restart "
           & Language_Name & " language server";
         Action      : constant Action_Access := Lookup_Action
           (Kernel, Action_Name);
      begin
         if Action = null then
            Register_Action
              (Kernel      => Kernel,
               Name        => Action_Name,
               Command     => new Restart_Command'
                 (Interactive_Command with
                  Kernel        => Kernel,
                  Language_Name => To_Unbounded_String (Language_Name)),
               Description => "Restart the language server for "
               & Language_Name,
               Category    => "LSP");
            Register_Menu
              (Kernel,
               Action => Action_Name,
               Path   => "Navigate/Restart Language Server/" & Language_Name);
         end if;
      end Create_Restart_Action_If_Needed;

   begin
      --  When servers have been dissociated, we won't be able to find a
      --  language for the server. In this case, return immediately.
      if Language = null then
         return;
      end if;

      --  Enqueue "did open" for all the editors for this language
      for Buffer of Self.Get_Kernel.Get_Buffer_Factory.Buffers loop
         if Share_Same_Server (Buffer.Get_Language, Language) then
            Server.Get_Client.Send_Text_Document_Did_Open (Buffer.File);
         end if;
      end loop;

      Create_Restart_Action_If_Needed (Language.Get_Name);

      GPS.Kernel.Hooks.Language_Server_Started_Hook.Run
        (Kernel   => Self.Get_Kernel,
         Language => Language.Get_Name);
   end On_Server_Started;

   -----------------------
   -- On_Server_Stopped --
   -----------------------

   overriding procedure On_Server_Stopped
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access)
   is
      Language : constant Language_Access := Server.Get_Client.Language;
   begin
      if Language = null then
         return;
      end if;

      declare
         Language_Name : constant String := Language.Get_Name;
      begin
         GPS.Kernel.Hooks.Language_Server_Stopped_Hook.Run
           (Kernel   => Self.Get_Kernel,
            Language => Language_Name);

         if not Self.Language_Servers.Contains (Language) then
            Unregister_Action
              (Kernel                    => Self.Get_Kernel,
               Name                      => "restart "
               & Language_Name & " language server",
               Remove_Menus_And_Toolbars => True);
         end if;
      end;
   end On_Server_Stopped;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Server_Stopped_Hook;
       Kernel : not null access Kernel_Handle_Record'Class;
       Language : String) is
   begin
      --  A server has stopped: cancel all the progress bars.
      --  Note: we do this for all servers because at the moment we cannot
      --  identify which progress is emitted by which server.
      --  For the corner case that a language server will still report
      --  progress on a task, we will regenerate the task when receiving
      --  the report notification.
      for E of Module.Token_To_Command loop
         declare
            Command : constant Language_Server_Progress_Command_Access :=
              Language_Server_Progress_Command_Access
                (E.Get_Command);
         begin
            if Command /= null then
               Command.Action := Failure;
            end if;
         end;
      end loop;
      Module.Token_To_Command.Clear;
   end Execute;

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   overriding procedure On_Publish_Diagnostics
     (Self   : access Module_Id_Record;
      Params : LSP.Messages.PublishDiagnosticsParams)
   is
      function To_Importance
        (Item : LSP.Messages.Optional_DiagnosticSeverity)
         return GPS.Kernel.Messages.Message_Importance_Type;
      --  Convert LSP's DiagnosticSeverity type to GNAT Studio's
      --  Message_Importance_Type.

      function Get_Editor_Side_Icon
        (Importance : Message_Importance_Type) return String
      is
        (case Importance is
            when High          => "gps-emblem-build-error",
            when Medium        => "gps-emblem-build-warning",
            when Low           => "gps-emblem-build-style",
            when Informational => "gps-emblem-build-info-symbolic",
            when others        => "");
      --  Return the editor side icon displayed for diagnostics, according
      --  to their importance.

      function Get_Category
        (Diag : LSP.Messages.Diagnostic) return VSS.Strings.Virtual_String;
      --  Return the message category of Diag

      function Contains
        (Messages : Message_Array;
         Diag     : LSP.Messages.Diagnostic)
         return Boolean;
      --  Return True if Diag is already in messages

      function Contains
        (Messages : Message_Array;
         Info     : LSP.Messages.DiagnosticRelatedInformation)
         return Boolean;
      --  Return True if Info is already in messages

      function Compare_Diagnostics
        (Diagnostics : LSP.Messages.Diagnostic_Vector;
         File        : GNATCOLL.VFS.Virtual_File)
         return Boolean;
      --  Return True if the Diagnostics are still the same

      Container : constant not null GPS.Kernel.Messages_Container_Access :=
        Self.Get_Kernel.Get_Messages_Container;
      File      : constant GNATCOLL.VFS.Virtual_File :=
        GPS.LSP_Client.Utilities.To_Virtual_File (Params.uri);
      Holder : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
        Self.Get_Kernel.Get_Buffer_Factory.Get_Holder (File => File);

      -------------------
      -- To_Importance --
      -------------------

      function To_Importance
        (Item : LSP.Messages.Optional_DiagnosticSeverity)
         return GPS.Kernel.Messages.Message_Importance_Type is
      begin
         if not Item.Is_Set then
            return GPS.Kernel.Messages.High;

         else
            case Item.Value is
            when LSP.Messages.Error =>
               return GPS.Kernel.Messages.High;

            when LSP.Messages.Warning =>
               return GPS.Kernel.Messages.Medium;

            when LSP.Messages.Information =>
               return GPS.Kernel.Messages.Low;

            when LSP.Messages.Hint =>
               return GPS.Kernel.Messages.Informational;
            end case;
         end if;
      end  To_Importance;

      --------------
      -- Contains --
      --------------

      function Contains
        (Messages : Message_Array;
         Diag     : LSP.Messages.Diagnostic)
         return Boolean
      is
         use type Basic_Types.Visible_Column_Type;
         use type VSS.Strings.Virtual_String;
         Location : constant GPS.Editors.Editor_Location'Class :=
           GPS.LSP_Client.Utilities.LSP_Position_To_Location
             (Holder.Editor, Diag.span.first);
      begin
         for Msg of Messages loop
            --  The primary message must match a diagnostics
            if VSS.Strings.Conversions.To_Virtual_String (Msg.Get_Text)
              = Diag.message
              and then Msg.Get_Line = Location.Line
              and then Msg.Get_Column = Location.Column
            then
               declare
                  Children : constant Message_Array := Msg.Get_Children;
               begin
                  --  The secondary messages must match the relatedInformation
                  if Integer (Diag.relatedInformation.Length)
                    /= Msg.Get_Children'Length
                  then
                     --  Not the same number
                     return False;
                  else
                     for Related_Diag of Diag.relatedInformation loop
                        if not Contains (Children, Related_Diag) then
                           return False;
                        end if;
                     end loop;

                     return True;
                  end if;
               end;
            end if;
         end loop;

         return False;
      end Contains;

      --------------
      -- Contains --
      --------------

      function Contains
        (Messages : Message_Array;
         Info     : LSP.Messages.DiagnosticRelatedInformation)
         return Boolean
      is
         use GPS.Editors;
         use type Basic_Types.Visible_Column_Type;
         use type VSS.Strings.Virtual_String;

         Holder    : constant Controlled_Editor_Buffer_Holder
           := Self.Get_Kernel.Get_Buffer_Factory.Get_Holder (File => File);
         Start_Loc : constant GPS.Editors.Editor_Location'Class :=
           GPS.LSP_Client.Utilities.LSP_Position_To_Location
             (Holder.Editor, Info.location.span.first);
      begin
         for Msg of Messages loop
            if VSS.Strings.Conversions.To_Virtual_String (Msg.Get_Text)
              = Info.message
              and then Msg.Get_Line = Start_Loc.Line
              and then Msg.Get_Column = Start_Loc.Column
            then
               return True;
            end if;
         end loop;

         return False;
      end Contains;

      ------------------
      -- Get_Category --
      ------------------

      function Get_Category
        (Diag : LSP.Messages.Diagnostic) return VSS.Strings.Virtual_String
      is
         use type VSS.Strings.Virtual_String;
      begin
         return
           (if Diag.source.Is_Set
            then Diagnostics_Messages_Category_Prefix
            & ": " & Diag.source.Value
            else Diagnostics_Messages_Category_Prefix);
      end Get_Category;

      -------------------------
      -- Compare_Diagnostics --
      -------------------------

      function Compare_Diagnostics
        (Diagnostics : LSP.Messages.Diagnostic_Vector;
         File        : GNATCOLL.VFS.Virtual_File)
         return Boolean
      is
         use type VSS.Strings.Virtual_String;
         --  Count the number of diagnostics in the messages view
         Nb_Old_Diag : Integer := 0;
      begin
         for Category of Container.Get_Categories loop
            if Category.Starts_With (Diagnostics_Messages_Category_Prefix) then
               declare
                  Messages : constant Message_Array :=
                    Container.Get_Messages
                      (Category => Category, File => File);
               begin
                  Nb_Old_Diag := Nb_Old_Diag + Messages'Length;
                  for Diag of Diagnostics loop
                     if not Contains (Messages, Diag) then
                        return False;
                     end if;
                  end loop;
               end;
            end if;
         end loop;

         --  Refresh in case we don't have the same number of messages or
         --  this is the first time we are receiving diagnostics
         if (Nb_Old_Diag = 0 and then not Diagnostics.Is_Empty)
           or else Nb_Old_Diag /= Integer (Diagnostics.Length)
         then
            return False;
         else
            return True;
         end if;
      end Compare_Diagnostics;

   begin
      if Compare_Diagnostics (Params.diagnostics, File) then
         return;
      end if;

      Module.Remove_Diagnostics (File);

      for Diagnostic of Params.diagnostics loop
         declare
            use type VSS.Unicode.UTF16_Code_Unit_Count;

            Location : constant GPS.Editors.Editor_Location'Class :=
              GPS.LSP_Client.Utilities.LSP_Position_To_Location
                (Holder.Editor, Diagnostic.span.first);
            Importance : constant Message_Importance_Type :=
              To_Importance (Diagnostic.severity);
            Flags      : constant Message_Flags :=
              (if not Diagnostic.relatedInformation.Is_Empty then
                  Side_And_Locations
               else
                 Get_Diagnostics_Message_Flags);
            Category   : constant VSS.Strings.Virtual_String :=
              Get_Category (Diagnostic);
            M          : constant Simple_Message_Access :=
              GPS.Kernel.Messages.Simple.Create_Simple_Message
                (Container    => Container,
                 Category     => Category,
                 File         => File,
                 Line         => Location.Line,
                 Column       => Location.Column,
                 Text         => Diagnostic.message,
                 Importance   => Importance,
                 Flags        => Flags,
                 Allow_Auto_Jump_To_First => False);

            Action : constant GPS.Editors.Line_Information.
              Line_Information_Access :=
                new Line_Information_Record'
                 ((Text                     => Null_Unbounded_String,
                   Display_Popup_When_Alone => False,
                   Tooltip_Text             => To_Unbounded_String
                     (Glib.Convert.Escape_Text
                          (VSS.Strings.Conversions.To_UTF_8_String
                               (Diagnostic.message))),
                   Image                    =>
                     To_Unbounded_String (Get_Editor_Side_Icon (Importance)),
                   Message                  => Create (Message_Access (M)),
                   Category                 => <>,
                   Associated_Command       => null));

         begin
            M.Set_Action (Action);
            M.Set_Highlighting
              (Style  => GPS.Default_Styles.Messages_Styles
                    (To_Importance (Diagnostic.severity)),
               Length =>
                  (if Diagnostic.span.last.line = Diagnostic.span.first.line
                   then Highlight_Length'Min
                     (1, Highlight_Length (Diagnostic.span.last.character -
                          Diagnostic.span.first.character))
                   else Highlight_Whole_Line));

            --  For each related information, create a secondary message
            for Info of Diagnostic.relatedInformation loop
               declare
                  use GPS.Editors;

                  File      : constant GNATCOLL.VFS.Virtual_File :=
                    GPS.LSP_Client.Utilities.To_Virtual_File
                      (Info.location.uri);
                  Holder    : constant Controlled_Editor_Buffer_Holder :=
                    Self.Get_Kernel.Get_Buffer_Factory.Get_Holder
                      (File => File);
                  Start_Loc : constant GPS.Editors.Editor_Location'Class :=
                    GPS.LSP_Client.Utilities.LSP_Position_To_Location
                      (Holder.Editor, Info.location.span.first);

                  SLOC_Prefix : constant String :=
                    File.Display_Base_Name
                    & ":"
                    & GNATCOLL.Utils.Image (Start_Loc.Line, 1)
                    & ":"
                    & GNATCOLL.Utils.Image (Integer (Start_Loc.Column), 1)
                    & ": ";
                  --  Prefix of the message used to indicate the SLOC of the
                  --  relatedInformation, displayed as an hyperlink.

                  Info_Msg  : constant String :=
                    VSS.Strings.Conversions.To_UTF_8_String (Info.message);
                  Text      : constant String := SLOC_Prefix & Info_Msg;
               begin
                  GPS.Kernel.Messages.Hyperlink.Create_Hyperlink_Message
                    (Parent => GPS.Kernel.Messages.Message_Access (M),
                     File   => File,
                     Line   => Start_Loc.Line,
                     Column => Start_Loc.Column,
                     Text   => Text,
                     First  => Text'First,
                     Last   => Text'First + SLOC_Prefix'Length - 2,
                     Flags  => Flags);
               end;
            end loop;
         end;
      end loop;
   end On_Publish_Diagnostics;

   ------------------
   -- Show_Message --
   ------------------

   overriding procedure On_Show_Message
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.ShowMessageParams)
   is
      Mode   : GPS.Messages_Windows.Message_Type;
      Is_Log : Boolean := False;   --  Whether the message is a log
   begin
      --  Convert the message type to a GNAT STudio type
      case Value.a_type is
         when LSP.Messages.Error   => Mode := GPS.Messages_Windows.Error;
         when LSP.Messages.Warning => Mode := GPS.Messages_Windows.Info;
         when LSP.Messages.Info    => Mode := GPS.Messages_Windows.Info;
         when LSP.Messages.Log     => Is_Log := True;
      end case;

      if Is_Log then
         --  If it's a log, send this to the traces...
         Me_LSP_Logs.Trace
           (VSS.Strings.Conversions.To_UTF_8_String (Value.message));

      else
         --  ... otherwise send this to the Messages view.
         Self.Get_Kernel.Messages_Window.Insert_UTF8
           ("Language server: "
            & VSS.Strings.Conversions.To_UTF_8_String (Value.message),
            Mode => Mode);
      end if;
   end On_Show_Message;

   --------------------
   -- On_Log_Message --
   --------------------

   overriding procedure On_Log_Message
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.LogMessageParams) is
   begin
      case Value.a_type is
         when LSP.Messages.Log =>
            declare
               File  : GNATCOLL.VFS.Virtual_File;

               Match : constant
                 VSS.Regular_Expressions.Regular_Expression_Match :=
                   Log_File_Pattern.Match (Value.message);
            begin
               if Match.Has_Match then
                  File := GNATCOLL.VFS.Create_From_UTF8
                    (VSS.Strings.Conversions.To_UTF_8_String
                       (Match.Captured (1)));

                  for Server of Module.Language_Servers
                    when Server.Get_Client.Language.Get_Name = "Ada"
                  loop
                     Server.Get_Client.Set_Standard_Errors_File (File);
                  end loop;
               end if;
            end;
         when others =>
            null;
      end case;
   end On_Log_Message;

   -----------------------
   -- Get_Progress_Type --
   -----------------------

   overriding function Get_Progress_Type
     (Self  : access Module_Id_Record;
      Token : LSP.Types.LSP_Number_Or_String)
      return LSP.Client_Notification_Receivers.Progress_Value_Kind is
   begin
      return LSP.Client_Notification_Receivers.ProgressParams;
   end Get_Progress_Type;

   -----------------
   -- On_Progress --
   -----------------

   overriding procedure On_Progress
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.Progress_Params)
   is
      use LSP.Messages;

      S : Scheduled_Command_Access;

      function Get_Or_Create_Scheduled_Command
        (Key   : LSP_Number_Or_String;
         Title : VSS.Strings.Virtual_String) return Scheduled_Command_Access;
      --  Get the scheduled command for the given key, creating it if needed

      -------------------------------------
      -- Get_Or_Create_Scheduled_Command --
      -------------------------------------

      function Get_Or_Create_Scheduled_Command
        (Key   : LSP_Number_Or_String;
         Title : VSS.Strings.Virtual_String) return Scheduled_Command_Access
      is
         S      : Scheduled_Command_Access;
         C      : Language_Server_Progress_Command_Access;
         Cursor : constant Token_Command_Maps.Cursor :=
           Self.Token_To_Command.Find (Key);
      begin
         if Token_Command_Maps.Has_Element (Cursor) then
            return Token_Command_Maps.Element (Cursor);

         else
            --  Start a monitoring command...
            C := new Language_Server_Progress_Command;
            C.Title := Title;
            C.Key   := Key;
            S := Launch_Background_Command
              (Kernel            => Self.Get_Kernel,
               Command           => C,
               Active            => False,
               Show_Bar          => True,
               Queue_Id          =>
                 VSS.Strings.Conversions.To_UTF_8_String
                   (LSP.Types.To_Virtual_String (Key)),
               Block_Exit        => False);

            --  ... and store it by its token identifier
            Self.Token_To_Command.Insert (Key, S);
            return S;
         end if;
      end Get_Or_Create_Scheduled_Command;

      Default_Title : constant VSS.Strings.Virtual_String :=
        "Indexing";
      Progress_Set  : Boolean := False;

   begin
      case Value.Kind is
         when Progress_Begin =>
            S := Get_Or_Create_Scheduled_Command
              (Value.Begin_Param.token,
               Value.Begin_Param.value.title);

         when Progress_Report =>
            --  It could happen that the progress bar was cancelled when
            --  a language server died. If this happens, we create a
            --  scheduled command with a "fallback" value for the title.
            S := Get_Or_Create_Scheduled_Command
              (Value.Report_Param.token,
               Default_Title);

            if Value.Report_Param.value.message.Is_Set then
               --  processed/total files may be in a message
               declare

                  Message : constant VSS.Strings.Virtual_String :=
                    Value.Report_Param.value.message.Value;

                  Match : constant VSS.Regular_Expressions
                    .Regular_Expression_Match
                      := Progress_Pattern.Match (Message);

               begin
                  if Match.Has_Match then
                     declare
                        Processed : constant Integer := Integer'Wide_Wide_Value
                          (VSS.Strings.Conversions.To_Wide_Wide_String
                            (Match.Captured (1)));

                        Total : constant Integer := Integer'Wide_Wide_Value
                          (VSS.Strings.Conversions.To_Wide_Wide_String
                            (Match.Captured (2)));
                     begin
                        S.Set_Progress
                          ((Activity => Running,
                            Current  => Processed,
                            Total    => Total));

                        Progress_Set := True;
                     end;
                  end if;
               end;
            end if;

            if not Progress_Set then
               S.Set_Progress
                 ((Activity => Running,
                   --  The LSP supports giving the value as percentage, not
                   --  as current/total.
                   Current  => Integer
                     (Value.Report_Param.value.percentage.Value),
                   Total    => 100));
            end if;

         when Progress_End =>
            --  Make the action self-destruct at the next call to Execute,
            --  and remove it right now from the associating array.
            declare
               Cursor : Token_Command_Maps.Cursor :=
                 Self.Token_To_Command.Find (Value.End_Param.token);
            begin
               if Token_Command_Maps.Has_Element (Cursor) then
                  Language_Server_Progress_Command_Access
                    (Token_Command_Maps.Element
                       (Cursor).Get_Command).Action := Success;
                  Self.Token_To_Command.Delete (Cursor);

               else
                  GNATCOLL.Traces.Trace
                    (Me_LSP_Logs, "No command on progress end");
               end if;
            end;
      end case;
   end On_Progress;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt
     (Command : in out Language_Server_Progress_Command) is
   begin
      if Module = null then
         return;
      end if;

      if Module.Token_To_Command.Contains (Command.Key) then
         Module.Token_To_Command.Delete (Command.Key);
      end if;
   end Interrupt;

   ------------------------------------------
   -- On_Progress_SymbolInformation_Vector --
   ------------------------------------------

   overriding procedure On_Progress_SymbolInformation_Vector
     (Self   : access Module_Id_Record;
      Params : LSP.Messages.Progress_SymbolInformation_Vector) is
   begin
      raise Program_Error;
   end On_Progress_SymbolInformation_Vector;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      Module := new Module_Id_Record;
      Module.Unknown_Server :=
        new GPS.LSP_Client.Language_Servers.Stub.Stub_Language_Server;
      Register_Module (Module_ID (Module), Kernel, "LSP_Client");
      File_Edited_Hook.Add (new On_File_Edited);
      File_Closed_Hook.Add (new On_File_Closed);
      File_Renamed_Hook.Add (new On_File_Renamed);
      Project_Changing_Hook.Add (new On_Project_Changing);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
      Preferences_Changed_Hook.Add (new On_Preference_Changed);
      Variable_Changed_Hook.Add (new On_Variable_Changed);
      Language_Server_Stopped_Hook.Add (new On_Server_Stopped_Hook);

      Location_Changed_Hook.Add_Debounce (new On_Location_Changed);

      Src_Editor_Buffer.Add_Listener_Factory (new Listener_Factory);

      GPS.LSP_Client.Shell.Register_Commands (Kernel);

      GPS.LSP_Client.Editors.Highlight.Register (Kernel);
      GPS.LSP_Client.Editors.Navigation.Register_Module (Kernel);
      GPS.LSP_Client.Editors.Folding.Register_Module (Kernel);
      if Me_LSP_FORMATTING.Is_Active then
         GPS.LSP_Client.Editors.Formatting.Register_Module (Kernel);
      end if;
      GPS.LSP_Client.Editors.Signature_Help.Register_Module (Kernel);
      GPS.LSP_Client.Editors.Semantic_Tokens.Register (Kernel);

      GPS.LSP_Client.Call_Tree.Register_Module (Kernel);
      GPS.LSP_Client.Outline.Register_Module (Kernel);
      GPS.LSP_Client.References.Register (Kernel);
      GPS.LSP_Client.Refactoring.Register (Kernel, Module_ID (Module));
      GPS.LSP_Client.Completion.Register (Kernel);
      GPS.LSP_Client.Dependency_Browers.Register_Module (Kernel);
      GPS.LSP_Client.Search.Register_Module (Kernel);

      Set_Outline_Tooltip_Factory (LSP_Outline_Tooltip_Factory'Access);

      if Me_Cpp_Support.Active then
         GPS.LSP_Client.Configurations.Clangd.Register (Kernel);
      end if;
   end Register_Module;

end GPS.LSP_Module;
