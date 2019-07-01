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

with Ada.Strings.UTF_Encoding.Wide_Strings;
with GNAT.OS_Lib;

with GNATCOLL.JSON;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

with LSP.JSON_Streams;

with GPS.Editors;
with GPS.Kernel.Project;
with GPS.LSP_Client.Utilities;
with Language;

package body GPS.LSP_Clients is

   Me : constant Trace_Handle := Create ("GPS.LSP_CLIENT");

   function "+" (Text : Ada.Strings.UTF_Encoding.UTF_8_String)
     return LSP.Types.LSP_String renames
       LSP.Types.To_LSP_String;

   procedure Process_Command_Queue (Self : in out LSP_Client'Class);

   procedure Reject_All_Requests (Self : in out LSP_Client'Class);
   --  Reject all ongoing (sent to the language server) and queued requests.
   --  Cleanup ongoing requests map and commands queue.

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self    : in out LSP_Client'Class;
      Request : in out GPS.LSP_Client.Requests.Request_Access) is
   begin
      Self.Enqueue ((Kind => GPS_Request, Request => Request));
      Request := null;
   end Enqueue;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self : in out LSP_Client'Class;
      Item : Command) is
   begin
      if Self.Is_Ready then
         if Self.Commands.Is_Empty then
            Self.Process_Command (Item);

         else
            Self.Commands.Append (Item);
         end if;

      else
         if Item.Kind = GPS_Request then
            declare
               Request : GPS.LSP_Client.Requests.Request_Access :=
                           Item.Request;

            begin
               Request.On_Rejected;
               GPS.LSP_Client.Requests.Destroy (Request);
            end;
         end if;
      end if;
   end Enqueue;

   -------------------------
   -- Initialize_Response --
   -------------------------

   overriding procedure Initialize_Response
     (Self     : not null access Response_Handler;
      Request  : LSP.Types.LSP_Number;
      Response : LSP.Messages.Initialize_Response)
   is
      pragma Unreferenced (Request);
   begin
      Self.Client.Server_Capabilities := Response.result.capabilities;

      if Response.result.capabilities.textDocumentSync.Is_Set then
         if Response.result.capabilities.textDocumentSync.Is_Number then
            case Response.result.capabilities.textDocumentSync.Value is
            when LSP.Messages.None =>
               Self.Client.Text_Document_Synchronization :=
                 GPS.LSP_Client.Text_Documents.Full;

            when LSP.Messages.Full =>
               Self.Client.Text_Document_Synchronization :=
                 GPS.LSP_Client.Text_Documents.Full;

            when LSP.Messages.Incremental =>
               Self.Client.Text_Document_Synchronization :=
                 GPS.LSP_Client.Text_Documents.Incremental;
            end case;

         else
            Self.Client.Text_Document_Synchronization :=
              GPS.LSP_Client.Text_Documents.Full;
         end if;

      else
         Self.Client.Text_Document_Synchronization :=
           GPS.LSP_Client.Text_Documents.Full;
      end if;

      Self.Client.Is_Ready := True;
      Self.Client.On_Initialized_Notification;

      Self.Client.Listener.On_Server_Started;

      Process_Command_Queue (Self.Client.all);
   end Initialize_Response;

   --------------
   -- Is_Ready --
   --------------

   function Is_Ready (Self : LSP_Client'Class) return Boolean is
   begin
      return Self.Is_Ready;
   end Is_Ready;

   --------------
   -- On_Error --
   --------------

   overriding procedure On_Error
     (Self  : in out LSP_Client;
      Error : String) is
   begin
      Me.Trace ("On_Error:" & Error);
      Self.Is_Ready := False;
      Self.Reject_All_Requests;
   end On_Error;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished (Self : in out LSP_Client) is
   begin
      Me.Trace ("On_Finished");
      Self.Is_Ready := False;
      Self.Reject_All_Requests;
   end On_Finished;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self : in out LSP_Client;
      Data : Ada.Strings.Unbounded.Unbounded_String)
   is

      function Get_Id
        (JSON : GNATCOLL.JSON.JSON_Array)
         return LSP.Types.LSP_Number_Or_String;

      ------------
      -- Get_Id --
      ------------

      function Get_Id
        (JSON : GNATCOLL.JSON.JSON_Array)
         return LSP.Types.LSP_Number_Or_String
      is
         Stream : aliased LSP.JSON_Streams.JSON_Stream;
         Result : LSP.Types.LSP_Number_Or_String;

      begin
         Stream.Set_JSON_Document (JSON);
         Stream.Start_Object;
         LSP.Types.Read_Number_Or_String (Stream, +"id", Result);

         return Result;
      end Get_Id;

      Value     : constant GNATCOLL.JSON.JSON_Value :=
                    GNATCOLL.JSON.Read (Data);
      JSON      : GNATCOLL.JSON.JSON_Array;
      Stream    : aliased LSP.JSON_Streams.JSON_Stream;
      Position  : Request_Maps.Cursor;
      Request   : GPS.LSP_Client.Requests.Request_Access;
      error     : LSP.Messages.ResponseError;
      Processed : Boolean := False;

   begin
      GNATCOLL.JSON.Append (JSON, Value);
      Stream.Set_JSON_Document (JSON);
      Stream.Start_Object;

      --  Report all error responses to Messages view.

      if Value.Has_Field ("id") and Value.Has_Field ("error") then
         --  This is a pure error

         Stream.Key ("error");
         LSP.Messages.ResponseError'Read (Stream'Access, error);

         declare
            S : constant String :=
                  "The language server has reported the following error:"
                  & ASCII.LF & "Code: " & error.code'Img & ASCII.LF
                  & LSP.Types.To_UTF_8_String (error.message);

         begin
            Trace (Me, S);
            Self.Kernel.Messages_Window.Insert_UTF8 (S);
         end;
      end if;

      if Value.Has_Field ("id") and not Value.Has_Field ("method") then
         --  Process response message when request was send by this object

         Position := Self.Requests.Find (Get_Id (JSON));

         if Request_Maps.Has_Element (Position) then
            Request := Request_Maps.Element (Position);
            Self.Requests.Delete (Position);

            if Value.Has_Field ("error") then
               Stream.Key ("error");
               LSP.Messages.ResponseError'Read (Stream'Access, error);

               begin
                  Request.On_Error_Message
                    (Code    => error.code,
                     Message => LSP.Types.To_UTF_8_String (error.message),
                     Data    => error.data);

               exception
                  when E : others =>
                     Trace (Me, E);
               end;

            elsif Value.Has_Field ("result") then
               Stream.Key ("result");

               begin
                  Request.On_Result_Message (Stream'Access);

               exception
                  when E : others =>
                     Trace (Me, E);
               end;

            else
               raise Program_Error;
            end if;

            GPS.LSP_Client.Requests.Destroy (Request);

            Processed := True;
         end if;
      end if;

      if not Processed then
         LSP.Clients.Client (Self).On_Raw_Message (Data);
      end if;

      if Value.Has_Field ("id") and not Value.Has_Field ("method") then
         --  Call response processed hook for all responses

         Self.Listener.On_Response_Processed (Data);
      end if;
   end On_Raw_Message;

   ----------------
   -- On_Started --
   ----------------

   overriding procedure On_Started (Self : in out LSP_Client) is
      Root    : constant GNATCOLL.VFS.Virtual_File :=
                  GPS.Kernel.Project.Get_Project
                    (Self.Kernel).Project_Path.Dir;
      --  ??? Root directory of the project is directoy where
      --  project file is stored.
      --  ??? Must be synchronized with ada.projectFile passed in
      --  WorkspaceDidChangeConfiguration notification.
      Id      : LSP.Types.LSP_Number;
      My_PID  : constant Integer :=
                  GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id);
      Request : constant LSP.Messages.InitializeParams :=
                  (processId    => (True, My_PID),
                   rootPath     => +Root.Display_Full_Name,
                   rootUri      =>
                     GPS.LSP_Client.Utilities.To_URI (Root),
                   capabilities =>
                     (workspace => (applyEdit => LSP.Types.False,
                                       others    => <>),
                      textDocument => <>),
                   trace        => LSP.Types.Unspecified);

   begin
      Self.Initialize_Request (Id, Request);
   end On_Started;

   ---------------------
   -- Process_Command --
   ---------------------

   procedure Process_Command
     (Self : in out LSP_Client'Class;
      Item : Command)
   is
      procedure Process_Open_File;
      procedure Process_Changed_File;
      procedure Process_Close_File;
      procedure Process_Request;

      --------------------------
      -- Process_Changed_File --
      --------------------------

      procedure Process_Changed_File is
      begin
         Self.On_DidChangeTextDocument_Notification
           (Item.Handler.Get_Did_Change_Message
              (Self.Text_Document_Synchronization));
      end Process_Changed_File;

      ------------------------
      -- Process_Close_File --
      ------------------------

      procedure Process_Close_File is
         Value : constant LSP.Messages.DidCloseTextDocumentParams :=
                   (textDocument =>
                      (uri        =>
                         GPS.LSP_Client.Utilities.To_URI (Item.File)));

      begin
         Self.On_DidCloseTextDocument_Notification (Value);
      end Process_Close_File;

      -----------------------
      -- Process_Open_File --
      -----------------------

      procedure Process_Open_File is
         Factory : constant GPS.Editors.Editor_Buffer_Factory_Access :=
                     Self.Kernel.Get_Buffer_Factory;
         Buffer  : constant GPS.Editors.Editor_Buffer'Class := Factory.Get
           (File        => Item.Handler.File,
            Open_Buffer => True,
            Open_View   => False);
         Lang    : constant not null Language.Language_Access :=
                     Buffer.Get_Language;
         Value   : constant LSP.Messages.DidOpenTextDocumentParams :=
                     (textDocument =>
                        (uri        =>
                           GPS.LSP_Client.Utilities.To_URI
                             (Item.Handler.File),
                         languageId => +Lang.Get_Name,
                         version    => 0,
                         text       => LSP.Types.To_LSP_String
                           (Buffer.Get_Chars_U)));

      begin
         Self.On_DidOpenTextDocument_Notification (Value);
      end Process_Open_File;

      ---------------------
      -- Process_Request --
      ---------------------

      procedure Process_Request is
         Id     : constant LSP.Types.LSP_Number_Or_String :=
                    Self.Allocate_Request_Id;
         Stream : aliased LSP.JSON_Streams.JSON_Stream;

      begin
         Stream.Start_Object;

         --  Serialize "jsonrpc" member

         Stream.Key ("jsonrpc");
         Stream.Write (GNATCOLL.JSON.Create ("2.0"));

         --  Serialize "id" memeber

         Stream.Key ("id");

         if Id.Is_Number then
            Stream.Write (GNATCOLL.JSON.Create (Id.Number));

         else
            Stream.Write
              (GNATCOLL.JSON.Create
                 (Ada.Strings.UTF_Encoding.Wide_Strings.Encode
                      (LSP.Types.To_Wide_String (Id.String))));
         end if;

         --  Serialize "method" member

         Stream.Key ("method");
         Stream.Write (GNATCOLL.JSON.Create (Item.Request.Method));

         --  Serialize "params" member

         Stream.Key ("params");
         Item.Request.Params (Stream'Access);

         Stream.End_Object;

         --  Send request's message

         Self.Send_Message
           (GNATCOLL.JSON.Get (Stream.Get_JSON_Document, 1).Write);

         --  Add request to the map

         Self.Requests.Insert (Id, Item.Request);
      end Process_Request;

   begin
      case Item.Kind is
         when Open_File =>
            Process_Open_File;

         when Changed_File =>
            Process_Changed_File;

         when Close_File =>
            Process_Close_File;

         when GPS_Request =>
            Process_Request;
      end case;
   end Process_Command;

   ---------------------------
   -- Process_Command_Queue --
   ---------------------------

   procedure Process_Command_Queue (Self : in out LSP_Client'Class) is
   begin
      --  ??? Must be rewritten for asynchronous execution.

      while not Self.Commands.Is_Empty loop
         Self.Process_Command (Self.Commands.First_Element);
         Self.Commands.Delete_First;
      end loop;
   end Process_Command_Queue;

   -------------------------
   -- Reject_All_Requests --
   -------------------------

   procedure Reject_All_Requests (Self : in out LSP_Client'Class) is
   begin
      Self.Listener.On_Server_Stopped;

      --  Reject all ongoing requests, results will be never received. Clean
      --  ongoing requests map.

      for Request of Self.Requests loop
         Request.On_Rejected;
         GPS.LSP_Client.Requests.Destroy (Request);
      end loop;

      Self.Requests.Clear;

      --  Reject all queued requests. Clean commands queue.

      for Command of Self.Commands loop
         if Command.Kind = GPS_Request then
            Command.Request.On_Rejected;
            GPS.LSP_Client.Requests.Destroy (Command.Request);
         end if;
      end loop;

      Self.Commands.Clear;
   end Reject_All_Requests;

   -----------------------------------
   -- Send_Text_Document_Did_Change --
   -----------------------------------

   overriding procedure Send_Text_Document_Did_Change
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access)
   is
      use type GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;

   begin
      for Command of Self.Commands loop
         if Command.Kind = Changed_File
           and then Command.Handler = Document
         then
            --  Nothing to do, DidChangeTextDocument notification has been
            --  requested.

            return;
         end if;
      end loop;

      Self.Enqueue ((Changed_File, Document));
   end Send_Text_Document_Did_Change;

   ----------------------------------
   -- Send_Text_Document_Did_Close --
   ----------------------------------

   overriding procedure Send_Text_Document_Did_Close
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access) is
   begin
      --  ??? Should check for incomplete Change_File command in queue and
      --  modify it properly.

      Self.Enqueue ((Close_File, Document.File));
   end Send_Text_Document_Did_Close;

   ---------------------------------
   -- Send_Text_Document_Did_Open --
   ---------------------------------

   overriding procedure Send_Text_Document_Did_Open
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access) is
   begin
      Self.Enqueue ((Open_File, Document));
   end Send_Text_Document_Did_Open;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self       : aliased in out LSP_Client;
      Executable : String;
      Arguments  : Spawn.String_Vectors.UTF_8_String_Vector) is
   begin
      Self.Set_Response_Handler (Self.Response_Handler'Unchecked_Access);

      Self.Set_Program (Executable);
      Self.Set_Arguments (Arguments);
      --  TODO: Self.Set_Environment
      --  TODO: Self.Set_Working_Directory
      Me.Trace ("Starting '" & Executable & ''');
      Self.Start;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Self               : in out LSP_Client'Class;
      Reject_Immediately : Boolean) is
   begin
      if Reject_Immediately then
         Self.Reject_All_Requests;
      end if;
   end Stop;

end GPS.LSP_Clients;
