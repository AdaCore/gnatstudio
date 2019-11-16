------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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
--  This is handled in two separate places: here in GPS.LSP_Module by reacting
--  to hooks, and in GPS.LSP_Client.Editors by implementing an editor
--  listener.
--
--    In GPS.LSP_Module:
--       -  didOpen   ->  sent in reaction to File_Edited_Hook
--                        sent in reaction to File_Renamed_Hook
--                    ->  sent for all open editors, when a LSP server
--                        is started whilst editors are already open,
--                        or while reloading a project
--       -  didClose  ->  sent in reaction to File_Closed_Hook
--                        sent in reaction to File_Renamed_Hook
--
--    In GPS.LSP_Client.Editors:
--       -  didChange ->  sent in reaction to After_Insert_Text

with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with System.Storage_Elements;

with Config;   use Config;
with Commands; use Commands;

with Glib.Convert;

with GNATCOLL.Traces;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with Default_Preferences;
with GPS.Core_Kernels;
with GPS.Default_Styles;
with GPS.Editors;
with GPS.Editors.Line_Information;      use GPS.Editors.Line_Information;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;               use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;        use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages.References;    use GPS.Kernel.Messages.References;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Task_Manager;           use GPS.Kernel.Task_Manager;
with GPS.Kernel.Project;
with GPS.LSP_Client.Configurations.ALS;
with GPS.LSP_Client.Editors;            use GPS.LSP_Client.Editors;
with GPS.LSP_Client.Editors.Navigation; use GPS.LSP_Client.Editors.Navigation;
with GPS.LSP_Client.Editors.Tooltips;   use GPS.LSP_Client.Editors.Tooltips;
with GPS.LSP_Client.Language_Servers;   use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Language_Servers.Interceptors;
with GPS.LSP_Client.Language_Servers.Real;
with GPS.LSP_Client.Language_Servers.Stub;
with GPS.LSP_Client.References;
with GPS.LSP_Client.Rename;
with GPS.LSP_Client.Shell;
with GPS.LSP_Client.Text_Documents;     use GPS.LSP_Client.Text_Documents;
with GPS.LSP_Client.Utilities;
with GPS.Messages_Windows;              use GPS.Messages_Windows;
with GPS.Scripts.Commands;              use GPS.Scripts.Commands;
with Language;                          use Language;
with LSP.Client_Notification_Receivers;
with LSP.Messages;
with LSP.Types;                         use LSP.Types;
with Outline_View;                      use Outline_View;
with Src_Editor_Buffer;
with Src_Editor_Module;                 use Src_Editor_Module;

package body GPS.LSP_Module is

   Me_Ada_Support : constant GNATCOLL.Traces.Trace_Handle :=
                      GNATCOLL.Traces.Create
                        ("GPS.LSP.ADA_SUPPORT", GNATCOLL.Traces.On);
   --  General ALS support

   Me_Cpp_Support : constant GNATCOLL.Traces.Trace_Handle :=
                      GNATCOLL.Traces.Create
                        ("GPS.LSP.CPP_SUPPORT", GNATCOLL.Traces.Off);

   Me_LSP_Logs  : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create ("GPS.LSP.LOGS", GNATCOLL.Traces.On);
   --  Whether to log the LSP notifications that arrive with the 'log' type

   type Listener_Factory is
     new GPS.Core_Kernels.Editor_Listener_Factory with null record;

   overriding function Create
     (Self    : Listener_Factory;
      Editor  : GPS.Editors.Editor_Buffer'Class;
      Factory : GPS.Editors.Editor_Buffer_Factory'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel)
      return GPS.Editors.Editor_Listener_Access;

   type Buffer_Handler_Record is record
      Buffer  : GPS.Editors.Editor_Buffer_Holders.Holder;
      Handler : Text_Document_Handler_Access;
   end record;

   package Buffer_Handler_Vectors is
     new Ada.Containers.Vectors (Positive, Buffer_Handler_Record);

   function Hash (Value : Language_Access) return Ada.Containers.Hash_Type;

   package Language_Server_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Language_Access,
      Element_Type    => Language_Server_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => GPS.LSP_Client.Language_Servers."=");

   package Text_Document_Handler_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Text_Document_Handler_Access,
        "="          => GPS.LSP_Client.Text_Documents."=");

   -------------------------
   -- Monitoring commands --
   -------------------------

   type Language_Server_Progress_Command is new Root_Command with record
      Title  : LSP_String;
      --  The title that should show in the progress bar

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

   overriding function Name
     (Command : access Language_Server_Progress_Command) return String is
      (To_UTF_8_String (Command.Title));

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
     and GPS.LSP_Client.Language_Servers.Interceptors.Interceptor_Listener
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

   overriding procedure On_Server_Started
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access);

   overriding procedure On_Server_Stopped
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access);

   overriding procedure On_Response_Processed
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
      Value : LSP.Messages.LogMessageParams) is null;

   overriding procedure On_Progress
     (Self  : access Module_Id_Record;
      Value : LSP.Messages.Progress_Params);

   procedure Initiate_Server_Shutdown
     (Self           : in out Module_Id_Record'Class;
      Lang           : not null Language.Language_Access;
      Server         : not null Language_Server_Access;
      In_Destruction : Boolean);
   --  Initiate shutdown of the given language server for given language.

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

   Diagnostics_Messages_Category : constant String := "Diagnostics";
   Diagnostics_Messages_Flags    : constant
     GPS.Kernel.Messages.Message_Flags :=
       GPS.Kernel.Messages.Sides_Only;

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

      use type Default_Preferences.Preference;

   begin
      if Pref /= null and then Pref.Get_Name = "General-Charset" then
         for Server of Module.Language_Servers loop
            Server.Configuration_Changed;
         end loop;
      end if;
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
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      procedure Setup_Server (Language : Standard.Language.Language_Access);
      --  Setup new language server for given language.

      ------------------
      -- Setup_Server --
      ------------------

      procedure Setup_Server (Language : Standard.Language.Language_Access) is
         function Getenv (Var : String) return String;
         --  Utility wrapper around Getenv

         ------------
         -- Getenv --
         ------------

         function Getenv (Var : String) return String is
            Str : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv (Var);
         begin
            return S : constant String := Str.all do
               GNAT.OS_Lib.Free (Str);
            end return;
         end Getenv;

         Language_Name : constant String :=
                           Ada.Characters.Handling.To_Lower
                             (Language.Get_Name);
         Configuration :
           GPS.LSP_Client.Configurations.Server_Configuration_Access;
         Server        :
         GPS.LSP_Client.Language_Servers.Language_Server_Access;

         Libexec_GPS : constant Virtual_File :=
           Kernel.Get_System_Dir / "libexec" / "gnatstudio";

      begin
         if Language_Name = "ada"
           and Me_Ada_Support.Is_Active
         then
            Configuration :=
              new GPS.LSP_Client.Configurations.ALS.ALS_Configuration (Kernel);

            declare
               --  Allow the environment variable "GPS_ALS" to override the
               --  location of the ada_language_server. For development.
               From_Env  : constant String := Getenv ("GPS_ALS");
               Tracefile : constant Virtual_File :=
                 Kernel.Get_Home_Dir / "ada_ls_traces.cfg";
            begin
               if From_Env /= "" then
                  Configuration.Server_Program := Create (+From_Env);
               else
                  Configuration.Server_Program := Libexec_GPS
                    / "als"
                    / ("ada_language_server"
                       & (if Host = Windows then ".exe" else ""));
               end if;

               if Tracefile.Is_Regular_File then
                  Configuration.Server_Arguments.Append
                    ("--tracefile=" & (+Tracefile.Full_Name.all));
               else
                  Me_Ada_Support.Trace
                    ("The tracefile for the Ada Language Server"
                     & " could not be found");
               end if;
            end;

            Src_Editor_Module.Set_Editor_Tooltip_Handler_Factory
              (Tooltip_Factory =>
                 Create_LSP_Client_Editor_Tooltip_Handler'Access);

         elsif Language_Name in "c" | "cpp"
           and Me_Cpp_Support.Is_Active
         then
            Configuration :=
              new GPS.LSP_Client.Configurations.Server_Configuration;

            Configuration.Server_Program := Libexec_GPS
              / "clangd" / "clangd";

         else
            return;
         end if;

         if not Configuration.Is_Available then
            --  Server is not available, return.

            return;
         end if;

         Server :=
           GPS.LSP_Client.Language_Servers.Real.Create
             (Kernel, Configuration, Module, Language);

         declare
            S : GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
              renames
                GPS.LSP_Client.Language_Servers.Real.Real_Language_Server'Class
                  (Server.all);
         begin
            S.Client.Set_Notification_Handler (Module);
            Module.Language_Servers.Insert (Language, Server);
            S.Start;
         end;
      end Setup_Server;

      Languages       : GNAT.Strings.String_List :=
                          GPS.Kernel.Project.Get_Root_Project_View
                            (Kernel).Get_Project_Type.Languages (True);
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
            if Language_Server_Maps.Has_Element (Position) then
               --  Language server for the giving language is configured and
               --  runing; move it to new set of language servers

               Server := Language_Server_Maps.Element (Position);
               Language_Server_Maps.Delete (Running_Servers, Position);
               Module.Language_Servers.Insert (Lang, Server);

               --  And notify about change of the configuration.

               Server.Configuration_Changed;
            else
               --  Start new language server if configured

               Setup_Server (Lang);
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

   ----------
   -- Hash --
   ----------

   function Hash (Value : Language_Access) return Ada.Containers.Hash_Type is
      X : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Value.all'Address);
   begin
      return Ada.Containers.Hash_Type'Mod (X);
   end Hash;

   ------------------------------
   -- Initiate_Server_Shutdown --
   ------------------------------

   procedure Initiate_Server_Shutdown
     (Self           : in out Module_Id_Record'Class;
      Lang           : not null Language.Language_Access;
      Server         : not null Language_Server_Access;
      In_Destruction : Boolean)
   is
      pragma Unreferenced (Self, Lang);

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
      In_Destruction : Boolean := False) is
   begin
      while not Servers.Is_Empty loop
         declare
            Position : Language_Server_Maps.Cursor := Servers.First;

         begin
            Self.Initiate_Server_Shutdown
              (Language_Server_Maps.Key (Position),
               Language_Server_Maps.Element (Position),
               In_Destruction);
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

   function LSP_Ada_Support_Trace_Is_Active return Boolean is
   begin
      return Me_Ada_Support.Is_Active;
   end LSP_Ada_Support_Trace_Is_Active;

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
      Container.Remove_File
        (Diagnostics_Messages_Category, File, Diagnostics_Messages_Flags);
   end Remove_Diagnostics;

   ---------------------------
   -- On_Response_Processed --
   ---------------------------

   overriding procedure On_Response_Processed
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Unbounded_String)
   is
      L : constant Language_Access := Self.Lookup_Language (Server);

   begin
      if L /= null then
         --  This is special case for "shutdown" request: module doesn't
         --  know language server at this point

         GPS.Kernel.Hooks.Language_Server_Response_Processed_Hook.Run
           (Kernel   => Self.Get_Kernel,
            Language => L.Get_Name,
            Data     => To_String (Data));
      end if;
   end On_Response_Processed;

   -----------------------
   -- On_Server_Started --
   -----------------------

   overriding procedure On_Server_Started
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access)
   is
      Language : constant Language_Access := Self.Lookup_Language (Server);
   begin
      --  When servers have been dissociated, we won't be able to find a
      --  language for the server. In this case, return immediately.
      if Language = null then
         return;
      end if;

      --  Enqueue "did open" for all the editors for this language
      for Buffer of Self.Get_Kernel.Get_Buffer_Factory.Buffers loop
         if Buffer.Get_Language = Language then
            Server.Get_Client.Send_Text_Document_Did_Open (Buffer.File);
         end if;
      end loop;

      GPS.Kernel.Hooks.Language_Server_Started_Hook.Run
        (Kernel   => Self.Get_Kernel,
         Language => Language.Get_Name);
   end On_Server_Started;

   -----------------------
   -- On_Server_Stopped --
   -----------------------

   overriding procedure On_Server_Stopped
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access) is
   begin
      GPS.Kernel.Hooks.Language_Server_Stopped_Hook.Run
        (Kernel   => Self.Get_Kernel,
         Language => Self.Lookup_Language (Server).Get_Name);
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
         Language_Server_Progress_Command_Access
           (E.Get_Command).Action := Failure;
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
      --  Convert LSP's DiagnosticSeverity type to GPS's
      --  Message_Importance_Type.

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

      Container : constant not null GPS.Kernel.Messages_Container_Access :=
                    Self.Get_Kernel.Get_Messages_Container;
      File      : constant GNATCOLL.VFS.Virtual_File :=
                    GPS.LSP_Client.Utilities.To_Virtual_File (Params.uri);

   begin
      Container.Remove_File
        (Diagnostics_Messages_Category, File, Diagnostics_Messages_Flags);

      for Diagnostic of Params.diagnostics loop
         declare
            M : constant Simple_Message_Access :=
              GPS.Kernel.Messages.Simple.Create_Simple_Message
                (Container    => Container,
                 Category     => Diagnostics_Messages_Category,
                 File         => File,
                 Line         => Integer (Diagnostic.span.first.line) + 1,
                 Column       =>
                   GPS.LSP_Client.Utilities.UTF_16_Offset_To_Visible_Column
                     (Diagnostic.span.first.character),
                 Text         => To_UTF_8_String (Diagnostic.message),
                 Importance   => To_Importance (Diagnostic.severity),
                 Flags        => Diagnostics_Messages_Flags,
                 Allow_Auto_Jump_To_First => False);
         begin
            M.Set_Action
              (new Line_Information_Record'
                 ((Text         => Null_Unbounded_String,
                   Tooltip_Text => To_Unbounded_String
                     (Glib.Convert.Escape_Text
                          (To_UTF_8_String (Diagnostic.message))),
                   Image        =>
                     To_Unbounded_String ("gps-emblem-build-warning"),
                   Message      => Create (Message_Access (M)),
                   Associated_Command => null)));
            M.Set_Highlighting
              (Style  => GPS.Default_Styles.Messages_Styles
                    (To_Importance (Diagnostic.severity)),
               Length =>
                  (if Diagnostic.span.last.line = Diagnostic.span.first.line
                   then Highlight_Length'Min
                     (1, Highlight_Length (Diagnostic.span.last.character -
                          Diagnostic.span.first.character))
                   else Highlight_Whole_Line));
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
      --  Convert the message type to a GPS type
      case Value.the_type is
         when LSP.Messages.Error   => Mode := GPS.Messages_Windows.Error;
         when LSP.Messages.Warning => Mode := GPS.Messages_Windows.Info;
         when LSP.Messages.Info    => Mode := GPS.Messages_Windows.Info;
         when LSP.Messages.Log     => Is_Log := True;
      end case;

      if Is_Log then
         --  If it's a log, send this to the traces...
         Me_LSP_Logs.Trace (To_UTF_8_String (Value.message));
      else
         --  ... otherwise send this to the Messages view.
         Self.Get_Kernel.Messages_Window.Insert_UTF8
           ("Language server: " & To_UTF_8_String (Value.message),
            Mode => Mode);
      end if;
   end On_Show_Message;

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
         Title : LSP_String) return Scheduled_Command_Access;
      --  Get the scheduled command for the given key, creating it if needed

      -------------------------------------
      -- Get_Or_Create_Scheduled_Command --
      -------------------------------------

      function Get_Or_Create_Scheduled_Command
        (Key   : LSP_Number_Or_String;
         Title : LSP_String) return Scheduled_Command_Access
      is
         S : Scheduled_Command_Access;
         C : Language_Server_Progress_Command_Access;
      begin
         if Self.Token_To_Command.Contains (Key) then
            return Self.Token_To_Command.Element (Key);
         else
            --  Start a monitoring command...
            C := new Language_Server_Progress_Command;
            C.Title := Title;
            S := Launch_Background_Command
              (Kernel            => Self.Get_Kernel,
               Command           => C,
               Active            => False,
               Show_Bar          => True,
               Queue_Id          => To_UTF_8_String
                 (Value.Begin_Param.token),
               Block_Exit        => False);

            --  ... and store it by its token identifier
            Self.Token_To_Command.Insert (Value.Begin_Param.token, S);
            return S;
         end if;
      end Get_Or_Create_Scheduled_Command;

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
               (if Value.Report_Param.value.message.Is_Set
                then Value.Report_Param.value.message.Value
                else To_LSP_String ("language server processing")));
            S.Set_Progress
              ((Activity => Running,
                --  The LSP supports giving the value as percentage, not
                --  as current/total.
                Current  => Value.Report_Param.value.percentage.Value,
                Total    => 100));

         when Progress_End =>
            --  Make the action self-destruct at the next call to Execute,
            --  and remove it right now from the associating array.
            Language_Server_Progress_Command_Access
              (Self.Token_To_Command.Element
                 (Value.End_Param.token).Get_Command).Action := Success;
            Self.Token_To_Command.Delete (Value.End_Param.token);
      end case;
   end On_Progress;

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
      Language_Server_Stopped_Hook.Add (new On_Server_Stopped_Hook);

      Src_Editor_Buffer.Add_Listener_Factory (new Listener_Factory);

      GPS.LSP_Client.Shell.Register_Commands (Kernel);
      GPS.LSP_Client.Editors.Navigation.Register_Module (Kernel);
      GPS.LSP_Client.References.Register (Kernel);
      GPS.LSP_Client.Rename.Register (Kernel, Module_ID (Module));

      Set_Outline_Tooltip_Factory (LSP_Outline_Tooltip_Factory'Access);
   end Register_Module;

end GPS.LSP_Module;
