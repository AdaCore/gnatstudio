------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with GNAT.Strings;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with System.Storage_Elements;

with Config; use Config;

with GNATCOLL.Traces;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with Default_Preferences;
with GPS.Core_Kernels;
with GPS.Editors;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
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
with GPS.LSP_Client.Shell;
with GPS.LSP_Client.Text_Documents;     use GPS.LSP_Client.Text_Documents;
with GPS.LSP_Client.Utilities;
with Language;                          use Language;
with LSP.Client_Notifications;
with LSP.Messages;
with LSP.Types;
with Outline_View;                      use Outline_View;
with Src_Editor_Buffer;
with Src_Editor_Module;                 use Src_Editor_Module;

package body GPS.LSP_Module is

   Me_Ada_Support : constant GNATCOLL.Traces.Trace_Handle :=
                      GNATCOLL.Traces.Create
                        ("GPS.LSP.ADA_SUPPORT", GNATCOLL.Traces.Off);
   --  General ALS support

   Me_Cpp_Support : constant GNATCOLL.Traces.Trace_Handle :=
                      GNATCOLL.Traces.Create
                        ("GPS.LSP.CPP_SUPPORT", GNATCOLL.Traces.Off);

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

   type Module_Id_Record is
     new GPS.Kernel.Modules.Module_ID_Record
     and LSP.Client_Notifications.Client_Notification_Handler
     and GPS.LSP_Client.Language_Servers.Interceptors.Interceptor_Listener
     and Text_Document_Manager with
      record
         Language_Servers : Language_Server_Maps.Map;
         --  Map from language to LSP client

         Unknown_Server   : Language_Server_Access;
         --  Pseudo-server to handle managed text documents of language not
         --  supported by any configured language servers.

         Unmanaged        : Text_Document_Handler_Vectors.Vector;
         --  List of unmanaged text documents (not associated with any
         --  language server). Unmanaged documents are:
         --   - editor buffer of which is under constuction (editor listener
         --     was created, but "file_edited" hook was not called)
         --   - new files
         --   - files under renaming or save as operations
         --   - files not closed during project reloading
         --
         --  Only few elements are expected to be in this container, thus
         --  vector container is fine.
      end record;

   type LSP_Module_Id is access all Module_Id_Record'Class;

   overriding procedure Destroy (Self : in out Module_Id_Record);
   --  Stops all language server processes and cleanup data structures.

   overriding procedure Register
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access);
   --  Register new text document handler.

   overriding procedure Unregister
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access);
   --  Unregister text document handler.

   overriding procedure Associated
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access);
   --  Remove text document from the list of unmanaged text documents.

   overriding procedure Dissociated
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access);
   --  Add text document to the list of unmanaged text documents.

   procedure On_File_Closed_Or_Renamed
     (Self : in out Module_Id_Record'Class;
      File : GNATCOLL.VFS.Virtual_File);
   --  Called on "file_closed" and "file_renamed" hooks to move text document
   --  from managed to unmanaged state.

   overriding procedure On_Server_Started
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access);

   overriding procedure On_Server_Stopped
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access);

   overriding procedure On_Response_Processed
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Ada.Strings.Unbounded.Unbounded_String);

   overriding procedure Publish_Diagnostics
     (Self   : in out Module_Id_Record;
      Params : LSP.Messages.PublishDiagnosticsParams);

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

   Diagnostics_Messages_Category : constant String := "Diagnostics";
   Diagnostics_Messages_Flags    : constant
     GPS.Kernel.Messages.Message_Flags :=
       GPS.Kernel.Messages.Side_And_Locations;

   ----------------
   -- Associated --
   ----------------

   overriding procedure Associated
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access)
   is
      Position : Text_Document_Handler_Vectors.Cursor
        := Self.Unmanaged.Find (Document);

   begin
      Self.Unmanaged.Delete (Position);
   end Associated;

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
          (GPS.Kernel.Kernel_Handle (Kernel), Module)
      do
         declare
            Handler : GPS.LSP_Client.Editors.Src_Editor_Handler'Class
            renames GPS.LSP_Client.Editors.Src_Editor_Handler'Class
              (Result.all);
         begin
            Handler.Initialize (Editor);
            Module.Register (Handler'Unchecked_Access);
         end;
      end return;
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Module_Id_Record) is
   begin
      Self.Initiate_Servers_Shutdown (Self.Language_Servers, True);

      while not Self.Unmanaged.Is_Empty loop
         Self.Unmanaged.First_Element.Destroy;
      end loop;
   end Destroy;

   -----------------
   -- Dissociated --
   -----------------

   overriding procedure Dissociated
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access) is
   begin
      Self.Unmanaged.Append (Document);
   end Dissociated;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self, Kernel);

   begin
      Module.On_File_Closed_Or_Renamed (File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);

      Lang     : constant not null Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Cursor   : constant Language_Server_Maps.Cursor :=
                   Module.Language_Servers.Find (Lang);
      Server   : Language_Server_Access;
      Position : Text_Document_Handler_Vectors.Cursor :=
                   Module.Unmanaged.First;

   begin
      if Language_Server_Maps.Has_Element (Cursor) then
         Server := Language_Server_Maps.Element (Cursor);

      else
         Server := Module.Unknown_Server;
      end if;

      while Text_Document_Handler_Vectors.Has_Element (Position) loop
         declare
            Document : Text_Document_Handler_Access
              renames Text_Document_Handler_Vectors.Element (Position);

         begin
            if Document.File = File then
               Server.Associate (Document);

               exit;
            end if;
         end;

         Text_Document_Handler_Vectors.Next (Position);
      end loop;
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
      pragma Unreferenced (Self, Kernel, To);

   begin
      Module.On_File_Closed_Or_Renamed (From);
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
           Kernel.Get_System_Dir / "libexec" / "gps";

      begin
         if Language_Name = "ada"
           and Me_Ada_Support.Is_Active
         then
            Configuration :=
              new GPS.LSP_Client.Configurations.ALS.ALS_Configuration (Kernel);

            declare
               --  Allow the environment variable "GPS_ALS" to override the
               --  location of the ada_language_server. For development.
               From_Env : constant String := Getenv ("GPS_ALS");
            begin
               if From_Env /= "" then
                  Configuration.Server_Program := Create (+From_Env);
               else
                  Configuration.Server_Program := Libexec_GPS
                    / "als"
                    / ("ada_language_server"
                       & (if Host = Windows then ".exe" else ""));
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
             (Kernel, Module, Configuration, Module);

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

         --  Do pass over unmanaged documents to associate them with new
         --  language server.

         declare
            Documents : constant Text_Document_Handler_Vectors.Vector :=
                          Module.Unmanaged;

         begin
            for Document of Documents loop
               if Kernel.Get_Language_Handler.Get_Language_From_File
                 (Document.File) = Language
               then
                  Server.Associate (Document);
               end if;
            end loop;
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

      --  Dissociate all files with the language server to be shutdown. It
      --  moves all associated files into 'unmanaged' state.

      S.Dissociate_All;
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

   -------------------------------
   -- On_File_Closed_Or_Renamed --
   -------------------------------

   procedure On_File_Closed_Or_Renamed
     (Self : in out Module_Id_Record'Class;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Container : constant not null GPS.Kernel.Messages_Container_Access :=
                    Self.Get_Kernel.Get_Messages_Container;
      Lang      : constant not null Language.Language_Access :=
                    Self.Get_Kernel.Get_Language_Handler.Get_Language_From_File
                      (File);
      Position  : constant Language_Server_Maps.Cursor :=
                    Self.Language_Servers.Find (Lang);
      Server    : Language_Server_Access;

   begin
      Container.Remove_File
        (Diagnostics_Messages_Category, File, Diagnostics_Messages_Flags);

      if Language_Server_Maps.Has_Element (Position) then
         Server := Language_Server_Maps.Element (Position);

      else
         Server := Self.Unknown_Server;
      end if;

      declare
         Document : Text_Document_Handler_Access
         renames Server.Text_Document (File);

      begin
         if Document /= null then
            --  Document is null then it represents new file without name. In
            --  this case it is not managed by any language server objects.

            Server.Dissociate (Server.Text_Document (File));
         end if;
      end;
   end On_File_Closed_Or_Renamed;

   ---------------------------
   -- On_Response_Processed --
   ---------------------------

   overriding procedure On_Response_Processed
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access;
      Data   : Ada.Strings.Unbounded.Unbounded_String)
   is
      L : constant Language_Access := Self.Lookup_Language (Server);

   begin
      if L /= null then
         --  This is special case for "shutdown" request: module doesn't
         --  know language server at this point

         GPS.Kernel.Hooks.Language_Server_Response_Processed_Hook.Run
           (Kernel   => Self.Get_Kernel,
            Language => L.Get_Name,
            Data     => Ada.Strings.Unbounded.To_String (Data));
      end if;
   end On_Response_Processed;

   -----------------------
   -- On_Server_Started --
   -----------------------

   overriding procedure On_Server_Started
     (Self   : in out Module_Id_Record;
      Server : not null Language_Server_Access) is
   begin
      GPS.Kernel.Hooks.Language_Server_Started_Hook.Run
        (Kernel   => Self.Get_Kernel,
         Language => Self.Lookup_Language (Server).Get_Name);
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

   -------------------------
   -- Publish_Diagnostics --
   -------------------------

   overriding procedure Publish_Diagnostics
     (Self   : in out Module_Id_Record;
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
         GPS.Kernel.Messages.Simple.Create_Simple_Message
           (Container                => Container,
            Category                 => Diagnostics_Messages_Category,
            File                     => File,
            Line                     =>
              Integer (Diagnostic.span.first.line) + 1,
            Column                   =>
              GPS.LSP_Client.Utilities.UTF_16_Offset_To_Visible_Column
                (Diagnostic.span.first.character),
            Text                     =>
              LSP.Types.To_UTF_8_String (Diagnostic.message),
            Importance               => To_Importance (Diagnostic.severity),
            Flags                    => Diagnostics_Messages_Flags,
            Allow_Auto_Jump_To_First => False);
      end loop;
   end Publish_Diagnostics;

   --------------
   -- Register --
   --------------

   overriding procedure Register
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access) is
   begin
      Self.Unmanaged.Prepend (Document);
   end Register;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      Module := new Module_Id_Record;
      Module.Unknown_Server :=
        new GPS.LSP_Client.Language_Servers.Stub.Stub_Language_Server (Module);
      Register_Module (Module_ID (Module), Kernel, "LSP_Client");
      File_Edited_Hook.Add (new On_File_Edited);
      File_Closed_Hook.Add (new On_File_Closed);
      File_Renamed_Hook.Add (new On_File_Renamed);
      Project_Changing_Hook.Add (new On_Project_Changing);
      Project_View_Changed_Hook.Add (new On_Project_View_Changed);
      Preferences_Changed_Hook.Add (new On_Preference_Changed);

      Src_Editor_Buffer.Add_Listener_Factory (new Listener_Factory);

      GPS.LSP_Client.Shell.Register_Commands (Kernel);
      GPS.LSP_Client.Editors.Navigation.Register_Module (Kernel);
      GPS.LSP_Client.References.Register (Kernel);

      Set_Outline_Tooltip_Factory (LSP_Outline_Tooltip_Factory'Access);
   end Register_Module;

   ----------------
   -- Unregister --
   ----------------

   overriding procedure Unregister
     (Self     : in out Module_Id_Record;
      Document : not null Text_Document_Handler_Access)
   is
      Position : Text_Document_Handler_Vectors.Cursor :=
                   Self.Unmanaged.Find (Document);

   begin
      if Text_Document_Handler_Vectors.Has_Element (Position) then
         Self.Unmanaged.Delete (Position);

      else
         raise Program_Error with "Managed or unknown text document";
      end if;
   end Unregister;

end GPS.LSP_Module;
