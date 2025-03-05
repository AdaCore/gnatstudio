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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;

with GNAT.OS_Lib;

with VSS.JSON.Pull_Readers.Simple;
with VSS.JSON.Streams;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.String_Vectors;
with VSS.Strings.Conversions; use VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with GNATCOLL.JSON;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

with LSP.JSON_Streams;
with LSP.Messages;

with GPS.Editors;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;
with GPS.LSP_Client.Editors.Semantic_Tokens;
with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Partial_Results;
with GPS.LSP_Client.Utilities;
with GPS.LSP_Clients.Shutdowns;

package body GPS.LSP_Clients is

   use type VSS.Strings.Virtual_String;

   Me : constant Trace_Handle := Create ("GPS.LSP_CLIENT");
   --  General trace following the behavior of the LSP client

   Me_Exceptions : constant Trace_Handle :=
     Create ("GPS.LSP_CLIENT.EXCEPTIONS", On);
   --  Specific trace for exceptions when handling request

   Me_Errors : constant Trace_Handle := Create ("GPS.LSP_CLIENT.ERRORS", Off);
   --  Specific trace to log error code reported via LSP

   Throttle_Period : constant Duration := 60.0 * 3;  --  3 minutes
   Throttle_Max    : constant := 4;
   --  Handle throttling limits for relaunching the server: relaunch a
   --  maximum of Throttle_Max launches within a given Throttle_Period.

   procedure Process_Command_Queue (Self : in out LSP_Client'Class);

   procedure Reject_All_Requests (Self : in out LSP_Client'Class);
   --  Reject all ongoing (sent to the language server) and queued requests.
   --  Cleanup ongoing requests map and commands queue.

   procedure Auto_Cancel_Requests
     (Self    : in out LSP_Client'Class;
      Method  : VSS.Strings.Virtual_String);
   --  Invalidate all the requests queued for Method

   procedure Clear_Change_Requests
     (Self : in out LSP_Client'Class;
      File : Virtual_File);
   --  Remove any pending change request for the given file

   package LSP_Client_Sources is
     new Glib.Main.Generic_Sources (LSP_Client_Access);

   function On_Restart_Timer (Self : LSP_Client_Access) return Boolean;
   --  Process restart timer event: do startup of new language server process

   ------------
   -- Cancel --
   ------------

   procedure Cancel
     (Self    : in out LSP_Client'Class;
      Request : in out GPS.LSP_Client.Requests.Request_Access)
   is
      use type GPS.LSP_Client.Requests.Request_Access;

   begin
      --  First, lookup for enqueued request

      declare
         Position : Command_Lists.Cursor := Self.Commands.First;

      begin
         while Command_Lists.Has_Element (Position) loop
            declare
               Item : constant Command := Command_Lists.Element (Position);

            begin
               if Item.Kind = GPS_Request
                 and then Item.Request = Request
               then
                  Self.Commands.Delete (Position);
                  Request.On_Rejected (GPS.LSP_Client.Requests.Canceled);

                  --  Notify about cancelation of the request

                  begin
                     Self.Listener.On_Send_Cancel (Request);

                  exception
                     when E : others =>
                        Trace (Me_Exceptions, E);
                  end;

                  GPS.LSP_Client.Requests.Destroy
                    (Request, Is_Cancelled => True);

                  return;
               end if;

               Command_Lists.Next (Position);
            end;
         end loop;
      end;

      --  Lookup for request in progress

      declare
         Position      : Request_Maps.Cursor := Self.Requests.First;
         Request_Id    : LSP.Types.LSP_Number_Or_String;
         Partial_Token : LSP.Types.LSP_Number_Or_String;
         Item          : Command;

      begin
         while Request_Maps.Has_Element (Position) loop
            if Request_Maps.Element (Position) = Request then
               Request_Id := Request_Maps.Key (Position);

               Item := (Cancel_GPS_Request, Request_Id);
               Self.Enqueue (Item);

               Self.Canceled_Requests.Insert (Request_Id);
               Self.Requests.Delete (Position);

               if Request.all
                    in GPS.LSP_Client.Partial_Results
                         .LSP_Request_Partial_Result'Class
               then
                  Partial_Token :=
                    GPS.LSP_Client.Partial_Results
                      .LSP_Request_Partial_Result'Class
                         (Request.all).Partial_Result_Token;

                  Self.Canceled_Tokens.Insert (Request_Id, Partial_Token);
               end if;

               Request.On_Rejected (GPS.LSP_Client.Requests.Canceled);

               --  Notify about cancelation of the request

               begin
                  Self.Listener.On_Send_Cancel (Request);

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

               GPS.LSP_Client.Requests.Destroy
                 (Request, Is_Cancelled => True);

               return;
            end if;

            Request_Maps.Next (Position);
         end loop;
      end;

      raise Program_Error;
   end Cancel;

   -------------------------
   -- Get_Running_Request --
   -------------------------

   function Get_Running_Request
     (Self       : LSP_Client'Class;
      Request_Id : LSP.Types.LSP_Number_Or_String)
      return GPS.LSP_Client.Requests.Request_Access is
   begin
      if not Self.Requests.Contains (Request_Id) then
         return null;
      end if;
      return Self.Requests.Element (Request_Id);
   end Get_Running_Request;

   ------------------
   -- Get_Requests --
   ------------------

   function Get_Requests
     (Self : LSP_Client'Class)
      return GPS.LSP_Client.Requests.Requests_Lists.List
   is
      Res : GPS.LSP_Client.Requests.Requests_Lists.List;
   begin
      for R of Self.Requests loop
         Res.Append (R);
      end loop;

      return Res;
   end Get_Requests;

   ------------------
   -- Capabilities --
   ------------------

   function Capabilities
     (Self : LSP_Client'Class) return LSP.Messages.ServerCapabilities is
   begin
      return Self.Server_Capabilities;
   end Capabilities;

   ---------------------------
   -- Clear_Change_Requests --
   ---------------------------

   procedure Clear_Change_Requests
     (Self : in out LSP_Client'Class;
      File : Virtual_File)
   is
      New_Commands : Command_Lists.List;
   begin
      --  Remove any "Change_File" command pending for this file.
      for C of Self.Commands loop
         if not (C.Kind = Changed_File
                 and then C.Handler.File = File)
         then
            New_Commands.Append (C);
         end if;
      end loop;
      Self.Commands := New_Commands;
   end Clear_Change_Requests;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self    : in out LSP_Client'Class;
      Request : in out GPS.LSP_Client.Requests.Request_Access)
   is
      use GPS.LSP_Client.Requests;
      Item : Command;
   begin
      if Request.Auto_Cancel then
         Self.Auto_Cancel_Requests (Request.Method);
      end if;

      Item := (Kind => GPS_Request, Request => Request);

      --  Enqueue the request - do this before notifying listeners, since
      --  this call to Enqueue sets the Id of the request.

      Self.Enqueue (Item);

      --  Notify about send of the request
      if Item.Request /= null then
         begin
            Self.Listener.On_Send_Request (Request);

         exception
            when E : others =>
               Trace (Me_Exceptions, E);
         end;
      end if;

      Request := Item.Request;
   end Enqueue;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self : in out LSP_Client'Class;
      Item : in out Command) is
   begin
      if Self.Is_Ready then
         if Self.Commands.Is_Empty then
            Self.Process_Command (Item);

         else
            Self.Commands.Append (Item);
         end if;

      else
         if Item.Kind = GPS_Request then
            Item.Request.On_Rejected
              (GPS.LSP_Client.Requests.Server_Not_Ready);
            GPS.LSP_Client.Requests.Destroy (Item.Request);
         end if;
      end if;
   end Enqueue;

   -------------------------
   -- Initialize_Response --
   -------------------------

   overriding procedure Initialize_Response
     (Self     : not null access Response_Handler;
      Request  : LSP.Types.LSP_Number_Or_String;
      Response : LSP.Messages.Server_Responses.Initialize_Response)
   is
      pragma Unreferenced (Request);

      Capabilities : LSP.Messages.ServerCapabilities :=
        Response.result.capabilities;
   begin
      if Self.Client.On_Server_Capabilities /= null then
         Self.Client.On_Server_Capabilities (Capabilities);
      end if;

      Self.Client.Server_Capabilities := Capabilities;

      if Capabilities.textDocumentSync.Is_Set then
         if Capabilities.textDocumentSync.Is_Number then
            case Capabilities.textDocumentSync.Value is
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

      --  Retrieve the server's log file, if any
      if Response.result.serverInfo.Is_Set
        and then Response.result.serverInfo.Value.log_filename.Is_Set
      then
         Self.Client.Standard_Errors_File :=
           Create
             (+VSS.Strings.Conversions.To_UTF_8_String
                (Response.result.serverInfo.Value.log_filename.Value));
      end if;

      Self.Client.Is_Ready := True;
      Self.Client.On_Initialized_Notification;

      Self.Client.Listener.On_Server_Started;

      Process_Command_Queue (Self.Client.all);
   end Initialize_Response;

   --------------------------
   -- Workspace_Apply_Edit --
   --------------------------

   overriding procedure Workspace_Apply_Edit
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.ApplyWorkspaceEditParams)
   is
      On_Error : Boolean;
   begin
      GPS.LSP_Client.Edit_Workspace.Edit
        (Kernel                   => GPS.Kernel.Kernel_Handle
           (Self.Client.Kernel),
         Workspace_Edit           => Params.edit,
         Title                    => "Apply Workspace Edit",
         Make_Writable            => False,
         Auto_Save                => False,
         Allow_File_Renaming      => False,
         Locations_Message_Markup =>
           (if Params.label.Is_Set then Params.label.Value else ""),
         Error                    => On_Error);

      declare
         Failure : LSP.Types.Optional_Virtual_String (Is_Set => On_Error);

      begin
         if On_Error then
            Failure.Value := "Internal error";
         end if;

         Self.Client.Workspace_Apply_Edit (Request, Failure);
         Self.Client.Listener.On_Response_Sent
           (To_Unbounded_String ("ApplyWorkspaceEditResponse"));
      end;
   end Workspace_Apply_Edit;

   --------------------------------------
   -- Window_Work_Done_Progress_Create --
   --------------------------------------

   overriding procedure Window_Work_Done_Progress_Create
     (Self    : not null access Request_Handler;
      Request : LSP.Types.LSP_Number_Or_String;
      Params  : LSP.Messages.WorkDoneProgressCreateParams) is
   begin
      --  Do nothing here except send an empty response to the server
      Self.Client.Void_Response (Request);
   end Window_Work_Done_Progress_Create;

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
      --  A race condition could occur whereby the glib context poll
      --  occurs while the process is being terminated intentionally
      --  (in particular on Windows): in this case, this will create I/O
      --  errors that can be ignored.

      if not Self.Exiting then
         Me_Errors.Trace ("On_Error:" & Error);
         Self.Is_Ready := False;
         Self.Reject_All_Requests;
      end if;
   end On_Error;

   --------------------------
   -- On_Exit_Notification --
   --------------------------

   overriding procedure On_Exit_Notification (Self : access LSP_Client) is
   begin
      LSP.Clients.Client (Self.all).On_Exit_Notification;
      Self.Exiting := True;
   end On_Exit_Notification;

   -------------------------------
   -- On_Standard_Error_Message --
   -------------------------------

   overriding procedure On_Standard_Error_Message
     (Self : in out LSP_Client; Text : String) is
   begin
      if Self.Errors_Writable_File /= Invalid_File then
         GNATCOLL.VFS.Write (Self.Errors_Writable_File, Text);

      else
         LSP.Clients.Client (Self).On_Standard_Error_Message (Text);
      end if;
   end On_Standard_Error_Message;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished (Self : in out LSP_Client) is
   begin
      Me.Trace ("On_Finished");

      --  The underlying process has died. If this wasn't intentional,
      --  let's relaunch it.
      if not Self.Shutdown_Intentionally_Requested then
         Self.Restart_Timer :=
           LSP_Client_Sources.Timeout_Add
             (400, On_Restart_Timer'Access, Self'Unchecked_Access);
      end if;

      --  If we reach here, it means the shutdown is final, no
      --  relaunches are expected
      Self.Is_Ready := False;
      Self.Reject_All_Requests;
   end On_Finished;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self    : in out LSP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

      procedure Look_Ahead
        (Id         : out LSP.Types.LSP_Number_Or_String;
         Method     : out LSP.Types.Optional_Virtual_String;
         Token      : out LSP.Messages.Optional_ProgressToken;
         error      : out LSP.Messages.Optional_ResponseError;
         Has_Result : out Boolean);
      --  Parse message to find significant fields of the message: "id",
      --  "method", "error", and "result". First three are unparsed too.

      Text_Stream : aliased
        VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

      ----------------
      -- Look_Ahead --
      ----------------

      procedure Look_Ahead
        (Id         : out LSP.Types.LSP_Number_Or_String;
         Method     : out LSP.Types.Optional_Virtual_String;
         Token      : out LSP.Messages.Optional_ProgressToken;
         error      : out LSP.Messages.Optional_ResponseError;
         Has_Result : out Boolean)
      is
         use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

         Reader   : aliased
           VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         JS       : aliased LSP.JSON_Streams.JSON_Stream
           (False, Reader'Unchecked_Access);
         Id_Found : Boolean := False;

      begin
         Reader.Set_Stream (Text_Stream'Unchecked_Access);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Document);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         --  Implementation is optimized a bit to skip unnecessary processing
         --  of data when enough information to make decision is processed.
         --
         --  It is expected to work correctly for the messages with following
         --  minimal set of fields:
         --
         --  "id"/"method"                --  request
         --  "method"                     --  notification
         --  "method":"$/progress"/"params"."token"
         --                               --  partial result
         --  "id"/"result"                --  result response
         --  "id"/"error"                 --  error response

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);

            declare
               Key : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);

            begin
               JS.R.Read_Next;

               if Key = "id" then
                  Id_Found := True;

                  case JS.R.Element_Kind is
                     when String_Value =>
                        Id :=
                          (Is_Number => False,
                           String    => JS.R.String_Value);

                     when Number_Value =>
                        Id :=
                          (Is_Number => True,
                           Number    => LSP.Types.LSP_Number
                             (JS.R.Number_Value.Integer_Value));

                     when others =>
                        raise Constraint_Error;
                  end case;

                  JS.R.Read_Next;

               elsif Key = "method" then
                  pragma Assert (JS.R.Is_String_Value);

                  Method := (True, JS.R.String_Value);

                  exit when Id_Found;

                  JS.R.Read_Next;

               elsif Key = "error" then
                  LSP.Messages.Optional_ResponseError'Read (JS'Access, error);
                  --  Just need to report the error, stop here

                  exit when Id_Found;

               elsif Key = "result" then
                  Has_Result := True;
                  --  Don't care about the result, stop here

                  exit when Id_Found;

                  JS.Skip_Value;

               elsif Key = "params" then
                  --  Parse 'params' object to get 'token' value from a
                  --  notification if any

                  pragma Assert (JS.R.Is_Start_Object);

                  JS.R.Read_Next;

                  while not JS.R.Is_End_Object loop
                     pragma Assert (JS.R.Is_Key_Name);

                     declare
                        Key : constant String :=
                          VSS.Strings.Conversions.To_UTF_8_String
                            (JS.R.Key_Name);

                     begin
                        JS.R.Read_Next;

                        if Key = "token" then
                           case JS.R.Element_Kind is
                              when String_Value =>
                                 Token :=
                                   (Is_Set => True,
                                    Value =>
                                      (Is_Number => False,
                                       String    => JS.R.String_Value));

                              when Number_Value =>
                                 Token :=
                                   (Is_Set => True,
                                    Value =>
                                      (Is_Number => True,
                                       Number    => LSP.Types.LSP_Number
                                         (JS.R.Number_Value.Integer_Value)));

                              when others =>
                                 raise Constraint_Error;
                           end case;
                        end if;

                        JS.Skip_Value;
                     end;
                  end loop;

                  JS.R.Read_Next;

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         Text_Stream.Rewind;
      end Look_Ahead;

      Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Stream : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => Reader'Unchecked_Access);
      Id     : LSP.Types.LSP_Number_Or_String;
      Token  : LSP.Messages.Optional_ProgressToken;
      Method : LSP.Types.Optional_Virtual_String;

      Request_Position : Request_Maps.Cursor;
      Partial_Position : Request_Id_Maps.Cursor;
      Request          : GPS.LSP_Client.Requests.Request_Access := null;
      Req_Method       : VSS.Strings.Virtual_String;
      --  The method for the request to which this response corresponds, if any

      error      : LSP.Messages.Optional_ResponseError;
      Processed  : Boolean := False;
      Has_Result : Boolean := False;

   begin
      Text_Stream.Set_Data
        (VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
           (Data));

      Look_Ahead (Id, Method, Token, error, Has_Result);

      if LSP.Types.Assigned (Id) and not Method.Is_Set then
         --  Process response message when request was send by this object

         Request_Position := Self.Requests.Find (Id);

         if Self.Canceled_Requests.Contains (Id) then
            --  Request was canceled, reply message is required by protocol
            --  but not need to be processed.

            Self.Canceled_Requests.Delete (Id);

            if Self.Canceled_Tokens.Contains (Id) then
               Self.Partials.Delete (Self.Canceled_Tokens (Id));
               Self.Canceled_Tokens.Delete (Id);
            end if;

            Processed := True;

         elsif Request_Maps.Has_Element (Request_Position) then
            Request := Request_Maps.Element (Request_Position);
            Self.Requests.Delete (Request_Position);

            if Request.all
                 in GPS.LSP_Client.Partial_Results
                      .LSP_Request_Partial_Result'Class
            then
               Self.Partials.Delete
                 (GPS.LSP_Client.Partial_Results
                    .LSP_Request_Partial_Result'Class
                       (Request.all).Partial_Result_Token);
            end if;

            Req_Method := Request.Method;

            if error.Is_Set then
               begin
                  Request.On_Error_Message
                    (Code    => error.Value.code,
                     Message => error.Value.message,
                     Data    => GNATCOLL.JSON.JSON_Value (error.Value.data));

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

               begin
                  Self.Listener.On_Receive_Reply (Request);

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

            elsif Has_Result then
               Reader.Set_Stream (Text_Stream'Unchecked_Access);
               --  Rewind Stream to "result" rey
               loop
                  Stream.R.Read_Next;
                  exit when Stream.R.Is_Key_Name
                    and then VSS.Strings.Conversions.To_UTF_8_String
                      (Stream.R.Key_Name) = "result";
               end loop;

               Stream.R.Read_Next;

               declare
                  use type GPS.Kernel.Kernel_Handle;
               begin
                  if Request.Kernel = null
                    or else not Request.Kernel.Is_In_Destruction
                  then
                     Request.On_Result_Message (Stream'Access);
                  end if;

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

               begin
                  Self.Listener.On_Receive_Reply (Request);

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

            else
               raise Program_Error;
            end if;

            GPS.LSP_Client.Requests.Destroy (Request);

            Processed := True;
         end if;

      elsif Method.Is_Set
        and then Method.Value = "$/progress"
        and then Token.Is_Set
      then
         --  Partial result notification

         Partial_Position := Self.Partials.Find (Token.Value);

         if Request_Id_Maps.Has_Element (Partial_Position) then
            Request_Position :=
              Self.Requests.Find (Request_Id_Maps.Element (Partial_Position));

            if Request_Maps.Has_Element (Request_Position) then
               Request := Request_Maps.Element (Request_Position);

               Reader.Set_Stream (Text_Stream'Unchecked_Access);

               --  Rewind Stream to "params"."value" key

               Outer : loop
                  Stream.R.Read_Next;

                  if Stream.R.Is_Key_Name
                    and then VSS.Strings.Conversions.To_UTF_8_String
                               (Stream.R.Key_Name) = "params"
                  then
                     Stream.R.Read_Next;
                     pragma Assert (Stream.R.Is_Start_Object);

                     Stream.R.Read_Next;

                     loop
                        pragma Assert (Stream.R.Is_Key_Name);

                        exit Outer
                          when VSS.Strings.Conversions.To_UTF_8_String
                                 (Stream.R.Key_Name) = "value";

                        Stream.R.Read_Next;
                        Stream.Skip_Value;
                     end loop;
                  end if;
               end loop Outer;

               Stream.R.Read_Next;

               declare
                  use type GPS.Kernel.Kernel_Handle;

               begin
                  if Request.Kernel = null
                    or else not Request.Kernel.Is_In_Destruction
                  then
                     GPS.LSP_Client.Partial_Results
                       .LSP_Request_Partial_Result'Class
                          (Request.all).On_Partial_Result_Message
                             (Stream'Access);
                  end if;

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

            else
               pragma Assert
                 (Self.Canceled_Requests.Contains
                    (Request_Id_Maps.Element (Partial_Position)));
            end if;

            Processed := True;
         end if;
      end if;

      if error.Is_Set then
         declare
            use GPS.LSP_Client.Requests;
            S : constant String :=
              "The language server has reported the following error "
              & "for language:" & Self.Language.Get_Name
              & ASCII.LF & "Code: " & error.Value.code'Img & ASCII.LF
              & VSS.Strings.Conversions.To_UTF_8_String
              (error.Value.message);
            Request_Message : constant String :=
              (if Request /= null
               then (ASCII.LF
                 & "Request: "
                 & VSS.Strings.Conversions.To_UTF_8_String (Request.Method))
               else "");
         begin
            Trace (Me_Errors, S & Request_Message);
         end;
      end if;

      if not Processed then
         declare
            OK : Boolean := True;

         begin
            LSP.Clients.Client (Self).On_Raw_Message (Data, OK);

            if not OK then
               if Self.Is_Ready then
                  --  Report internal error when server is running in normal
                  --  mode. In case of server shutdown, when all requests was
                  --  rejected this internal error may be ignored silently.

                  Trace
                    (Me_Exceptions,
                     "Error: "
                     & VSS.Strings.Conversions.To_UTF_8_String
                       (Self.Error_Message));
               end if;
            end if;
         end;
      end if;

      --  Call response processed hook for all responses

      Self.Listener.On_Response_Processed
        (Data,
         VSS.Strings.Conversions.To_Unbounded_UTF_8_String (Req_Method));
   end On_Raw_Message;

   ----------------------
   -- On_Restart_Timer --
   ----------------------

   function On_Restart_Timer (Self : LSP_Client_Access) return Boolean is
      Now   : Time;
      Count : Natural := 0;

   begin
      Now := Clock;

      --  Count the number of launches that have occurred within the
      --  last throttle period

      for Launch_Time of Self.Launches loop
         exit when Now - Launch_Time > Throttle_Period;
         Count := Count + 1;
      end loop;

      --  If we haven't restarted too many times, relaunch now.
      if Count <= Throttle_Max then
         Me.Trace ("Restarting");
         Self.Launches.Prepend (Clock);

         Self.Start;

      else
         Self.Kernel.Insert
           ("The language server for " & Self.Language.Get_Name
            & " had to be restarted more than" & Throttle_Max'Img
            & " times in the past" & Integer (Throttle_Period)'Img
            & " seconds - aborting. Please report this.",
            Mode => GPS.Kernel.Error);
         Me.Trace ("Restarted too many times, aborting");

         --  Prevent further restart attempts
         Self.Shutdown_Intentionally_Requested := True;
      end if;

      Self.Restart_Timer := Glib.Main.No_Source_Id;

      return False;
   end On_Restart_Timer;

   ------------------
   -- On_Exception --
   ------------------

   overriding procedure On_Exception
     (Self       : in out LSP_Client;
      Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      pragma Unreferenced (Self);
   begin
      Trace (Me, Occurrence);
   end On_Exception;

   ----------------
   -- On_Started --
   ----------------

   overriding procedure On_Started (Self : in out LSP_Client) is

      function Get_Completion_Documentation_Formats
        return LSP.Messages.MarkupKind_Vector;

      function Get_CompletionItem_Resolve_Properties
        return VSS.String_Vectors.Virtual_String_Vector;

      function Get_Supported_ResourceOperations
        return LSP.Messages.ResourceOperationKindSet;

      function Get_Supported_CodeActionKinds
        return LSP.Messages.CodeActionKindSet;

      function Get_Experimental_Features return LSP.Types.LSP_Any;
      --  Return the selection of experimental features that are supported

      -------------------------------
      -- Get_Experimental_Features --
      -------------------------------

      function Get_Experimental_Features return LSP.Types.LSP_Any is
         --  Craft something like:
         --     {"advanced_refactorings":["add_parameter"
         use GNATCOLL.JSON;
         use LSP.Types;
         Result       : constant LSP_Any := Create_Object;
         Refactorings : JSON_Array;
      begin
         Refactorings.Append (Create ("add_parameter"));
         Result.Set_Field ("advanced_refactorings", Refactorings);
         return Result;
      end Get_Experimental_Features;

      ------------------------------------------
      -- Get_Completion_Documentation_Formats --
      ------------------------------------------

      function Get_Completion_Documentation_Formats
         return LSP.Messages.MarkupKind_Vector
      is
         Completion_Doc_Format : LSP.Messages.MarkupKind_Vector;
      begin
         Completion_Doc_Format.Append (LSP.Messages.plaintext);
         Completion_Doc_Format.Append (LSP.Messages.markdown);

         return Completion_Doc_Format;
      end Get_Completion_Documentation_Formats;

      -------------------------------------------
      -- Get_CompletionItem_Resolve_Properties --
      -------------------------------------------

      function Get_CompletionItem_Resolve_Properties
         return VSS.String_Vectors.Virtual_String_Vector
      is
         Properties : VSS.String_Vectors.Virtual_String_Vector;
      begin
         Properties.Append (To_Virtual_String ("detail"));
         Properties.Append (To_Virtual_String ("documentation"));

         return Properties;
      end Get_CompletionItem_Resolve_Properties;

      --------------------------------------
      -- Get_Supported_ResourceOperations --
      --------------------------------------

      function Get_Supported_ResourceOperations
         return LSP.Messages.ResourceOperationKindSet
      is
         Result : LSP.Messages.ResourceOperationKindSets.Set;
      begin
         LSP.Messages.ResourceOperationKindSets.Include
           (Result, LSP.Messages.ResourceOperationKind'(LSP.Messages.rename));

         return LSP.Messages.ResourceOperationKindSet (Result);
      end Get_Supported_ResourceOperations;

      -----------------------------------
      -- Get_Supported_CodeActionKinds --
      -----------------------------------

      function Get_Supported_CodeActionKinds
         return LSP.Messages.CodeActionKindSet
      is
         Result : LSP.Messages.CodeActionKindSets.Set;
      begin
         for Kind in LSP.Messages.CodeActionKind loop
            LSP.Messages.CodeActionKindSets.Include (Result, Kind);
         end loop;

         return LSP.Messages.CodeActionKindSet (Result);
      end Get_Supported_CodeActionKinds;

      Root   : constant GNATCOLL.VFS.Virtual_File :=
        GPS.Kernel.Project.Get_Project (Self.Kernel).Project_Path.Dir;
      --  ??? Root directory of the project is directoy where
      --  project file is stored.
      --  ??? Must be synchronized with ada.projectFile passed in
      --  WorkspaceDidChangeConfiguration notification.
      Id     : LSP.Types.LSP_Number_Or_String;
      My_PID : constant LSP.Types.LSP_Number :=
        LSP.Types.LSP_Number
          (GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id));

      Request : constant LSP.Messages.InitializeParams :=
        (processId             => (True, My_PID),
         rootPath              => (Is_Set => False),
         rootUri               =>
           (True,

              LSP.Types.To_Virtual_String
                (GPS.LSP_Client.Utilities.To_URI (Root))),
         initializationOptions => Self.Initialization_Options,
         capabilities          =>
           (workspace    =>
              (applyEdit      => LSP.Types.True,
               workspaceEdit  =>
                 (documentChanges    => (True, True),
                  resourceOperations =>
                    (True, Value => Get_Supported_ResourceOperations),
                  others             => <>),
               fileOperations =>
                 (Is_Set => True,
                  Value  => (didRename => (True, True), others => <>)),
               others         => <>),
            textDocument =>
              (hover              => (Is_Set => True, others => <>),
               signatureHelp      => (Is_Set => True, others => <>),
               declaration        => (Is_Set => True, others => <>),
               definition         => (Is_Set => True, others => <>),
               typeDefinition     => (Is_Set => True, others => <>),
               implementation     => (Is_Set => True, others => <>),
               publishDiagnostics =>
                 (Is_Set => True,
                  Value  =>
                    (relatedInformation => (True, True), others => <>)),
               codeAction         =>
                 (Is_Set => True,
                  Value  =>
                    (codeActionLiteralSupport =>
                       (Is_Set => True,
                        Value  =>
                          (codeActionKind =>
                             (valueSet => Get_Supported_CodeActionKinds))),
                     others                   => <>)),
               completion         =>
                 (dynamicRegistration => (Is_Set => True, Value => True),
                  completionItem      =>
                    (Is_Set => True,
                     Value  =>
                       (snippetSupport      =>
                          (Is_Set => True, Value => LSP_Use_Snippets.Get_Pref),
                        documentationFormat =>
                          Get_Completion_Documentation_Formats,
                        resolveSupport      =>
                          (True,

                             (properties =>
                                Get_CompletionItem_Resolve_Properties)),
                        others              => <>)),
                  completionItemKind  => <>,
                  contextSupport      => <>),
               --  Right now we support only whole line folding
               foldingRange       =>
                 (Is_Set => True,
                  Value  =>
                    (lineFoldingOnly => (Is_Set => True, Value => True),
                     others          => <>)),
               documentSymbol     =>
                 (Is_Set => True,
                  Value  =>
                    (hierarchicalDocumentSymbolSupport =>
                       (Is_Set => True, Value => True),
                     others                            => <>)),
               formatting         =>
                 (Is_Set => True,
                  Value  => (dynamicRegistration => LSP.Types.False)),
               rangeFormatting    =>
                 (Is_Set => True,
                  Value  => (dynamicRegistration => LSP.Types.False)),
               onTypeFormatting   =>
                 (Is_Set => True,
                  Value  => (dynamicRegistration => LSP.Types.False)),
               semanticTokens     =>
                 GPS.LSP_Client.Editors.Semantic_Tokens.Get_Supported_Options,
               others             => <>),
            window       => (Is_Set => False),
            general      => (Is_Set => False),
            experimental =>
              (Is_Set => True, Value => Get_Experimental_Features)),
         trace                 => (Is_Set => False),
         workspaceFolders      => (Is_Set => False),
         workDoneToken         => (Is_Set => False),
         clientInfo            => (Is_Set => False),
         locale                => (Is_Set => False));

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
      procedure Process_Rename_File;
      procedure Process_Request;
      procedure Process_Cancel_Request;

      ----------------------------
      -- Process_Cancel_Request --
      ----------------------------

      procedure Process_Cancel_Request is
      begin
         Self.On_Cancel_Notification ((id => Item.Id));
      end Process_Cancel_Request;

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
         use GPS.Editors;

         Value : constant LSP.Messages.DidCloseTextDocumentParams :=
                   (textDocument =>
                      (uri        =>
                         GPS.LSP_Client.Utilities.To_URI (Item.File)));

         Buffer  : constant GPS.Editors.Editor_Buffer'Class :=
           Self.Kernel.Get_Buffer_Factory.Get
             (File        => Item.File,
              Open_Buffer => False,
              Open_View   => False);
      begin
         if Buffer /= Nil_Editor_Buffer then
            Buffer.Set_Opened_On_LSP_Server (False);
         end if;
         Self.On_DidCloseTextDocument_Notification (Value);
      end Process_Close_File;

      -----------------------
      -- Process_Open_File --
      -----------------------

      procedure Process_Open_File is
         Factory : constant GPS.Editors.Editor_Buffer_Factory_Access :=
                     Self.Kernel.Get_Buffer_Factory;
         Buffer  : constant GPS.Editors.Editor_Buffer'Class := Factory.Get
           (File        => Item.File,
            Open_Buffer => True,
            Open_View   => False);
         Lang    : constant not null Language.Language_Access :=
                     Buffer.Get_Language;
         Value   : constant LSP.Messages.DidOpenTextDocumentParams :=
                     (textDocument =>
                        (uri        =>
                           GPS.LSP_Client.Utilities.To_URI
                             (Item.File),
                         languageId =>
                           VSS.Strings.Conversions.To_Virtual_String
                             (Lang.Get_Name),
                         version    => 0,
                         text       => Buffer.Get_Text));

      begin
         Self.On_DidOpenTextDocument_Notification (Value);
         Buffer.Set_Opened_On_LSP_Server (True);
      end Process_Open_File;

      -------------------------
      -- Process_Rename_File --
      -------------------------

      procedure Process_Rename_File is
         Value : LSP.Messages.RenameFilesParams;
      begin
         Value.files.Append
           (LSP.Messages.FileRename'(
            oldUri =>
              LSP.Types.To_Virtual_String
                (GPS.LSP_Client.Utilities.To_URI (Item.Old_URI)),
            newUri =>
              LSP.Types.To_Virtual_String
                (GPS.LSP_Client.Utilities.To_URI (Item.New_URI))));
         Self.On_DidRenameFiles_Notification (Value);
      end Process_Rename_File;

      ---------------------
      -- Process_Request --
      ---------------------

      procedure Process_Request is
         Id     : constant LSP.Types.LSP_Number_Or_String :=
           Self.Allocate_Request_Id;
         Stream : aliased LSP.JSON_Streams.JSON_Stream;
         Output : aliased
           VSS.Text_Streams.Memory_UTF8_Output.Memory_UTF8_Output_Stream;

      begin
         --  Allocate and set id of the request and token of the partial result
         --  when supported. Add request to the maps.

         Item.Request.Set_Id (Id);
         Self.Requests.Insert (Id, Item.Request);

         if Item.Request.all
           in GPS.LSP_Client.Partial_Results.LSP_Request_Partial_Result'Class
         then
            declare
               Token : constant LSP.Types.LSP_Number_Or_String :=
                 Self.Allocate_Request_Id;

            begin
               GPS.LSP_Client.Partial_Results.LSP_Request_Partial_Result'Class
                 (Item.Request.all).Set_Partial_Result_Token (Token);
               Self.Partials.Insert (Token, Id);
            end;
         end if;

         Stream.Set_Stream (Output'Unchecked_Access);
         Stream.Start_Object;

         --  Serialize "jsonrpc" member

         Stream.Key ("jsonrpc");
         Stream.Write_String ("2.0");

         --  Serialize "id" memeber

         Stream.Key ("id");

         if Id.Is_Number then
            Stream.Write_Integer (Interfaces.Integer_64 (Id.Number));

         else
            Stream.Write_String (Id.String);
         end if;

         --  Serialize "method" member

         Stream.Key ("method");
         Stream.Write_String (Item.Request.Method);

         --  Serialize "params" member

         Stream.Key ("params");
         Item.Request.Params (Stream'Access);

         Stream.End_Object;
         Stream.End_Document;

         --  Send request's message

         Self.Send_Buffer (Output.Buffer);
      end Process_Request;

   begin
      case Item.Kind is
         when Open_File =>
            Process_Open_File;

         when Changed_File =>
            Process_Changed_File;

         when Close_File =>
            Process_Close_File;

         when Rename_File =>
            Process_Rename_File;

         when GPS_Request =>
            Process_Request;

         when Cancel_GPS_Request =>
            Process_Cancel_Request;
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
         Request.On_Rejected (GPS.LSP_Client.Requests.Server_Died);

         begin
            Self.Listener.On_Reject_Request (Request);

         exception
            when E : others =>
               Trace (Me_Exceptions, E);
         end;

         GPS.LSP_Client.Requests.Destroy (Request);
      end loop;

      Self.Requests.Clear;
      Self.Partials.Clear;
      Self.Canceled_Requests.Clear;
      Self.Canceled_Tokens.Clear;

      --  Reject all queued requests. Clean commands queue.

      for Command of Self.Commands loop
         if Command.Kind = GPS_Request then
            Command.Request.On_Rejected (GPS.LSP_Client.Requests.Server_Died);

            begin
               Self.Listener.On_Reject_Request (Command.Request);

            exception
               when E : others =>
                  Trace (Me_Exceptions, E);
            end;

            GPS.LSP_Client.Requests.Destroy (Command.Request);
         end if;
      end loop;

      Self.Commands.Clear;
   end Reject_All_Requests;

   --------------------------
   -- Auto_Cancel_Requests --
   --------------------------

   procedure Auto_Cancel_Requests
     (Self    : in out LSP_Client'Class;
      Method  : VSS.Strings.Virtual_String)
   is
      use GPS.LSP_Client.Requests.Requests_Lists;
      --  Keep a copy of the requests list because we are tampering with the
      --  real list.
      Requests : constant GPS.LSP_Client.Requests.Requests_Lists.List :=
        Self.Get_Requests;
      Cursor   : GPS.LSP_Client.Requests.Requests_Lists.Cursor :=
        First (Requests);
   begin
      while Has_Element (Cursor) loop
         if Element (Cursor).Method = Method
           and then Element (Cursor).Auto_Cancel
         then
            declare
               Request : GPS.LSP_Client.Requests.Request_Access :=
                 Element (Cursor);
            begin
               Self.Cancel (Request);
            end;
         end if;
         Next (Cursor);
      end loop;
   end Auto_Cancel_Requests;

   -----------------------
   -- Request_Id_Prefix --
   -----------------------

   overriding function Request_Id_Prefix
     (Self : LSP_Client) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Ada.Characters.Handling.To_Lower (Self.Language.Get_Name));
   end Request_Id_Prefix;

   -----------------------------------
   -- Send_Text_Document_Did_Change --
   -----------------------------------

   overriding procedure Send_Text_Document_Did_Change
     (Self     : in out LSP_Client;
      Document : not null
        GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access)
   is
      use type GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;
      Item : Command;

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

      Item := (Changed_File, Document);

      Self.Enqueue (Item);
   end Send_Text_Document_Did_Change;

   ----------------------------------
   -- Send_Text_Document_Did_Close --
   ----------------------------------

   overriding procedure Send_Text_Document_Did_Close
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Item : Command := (Close_File, File);
   begin
      --  We want to close a file: no need to process change requests in
      --  between
      Self.Clear_Change_Requests (File);
      Self.Enqueue (Item);
   end Send_Text_Document_Did_Close;

   ---------------------------------
   -- Send_Text_Document_Did_Open --
   ---------------------------------

   overriding procedure Send_Text_Document_Did_Open
     (Self : in out LSP_Client;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Item : Command := (Open_File, File);
   begin
      Self.Enqueue (Item);
   end Send_Text_Document_Did_Open;

   --------------------------
   -- Send_Did_Rename_File --
   --------------------------

   overriding procedure Send_Did_Rename_File
     (Self    : in out LSP_Client;
      Old_URI : GNATCOLL.VFS.Virtual_File;
      New_URI : GNATCOLL.VFS.Virtual_File)
   is
      Item : Command := (Rename_File, Old_URI, New_URI);
   begin
      Self.Enqueue (Item);
   end Send_Did_Rename_File;

   --------------------------------
   -- Set_On_Server_Capabilities --
   --------------------------------

   procedure Set_On_Server_Capabilities
     (Self : in out LSP_Client'Class;
      Proc : On_Server_Capabilities_Proc) is
   begin
      Self.On_Server_Capabilities := Proc;
   end Set_On_Server_Capabilities;

   ------------------------------
   -- Set_Standard_Errors_File --
   ------------------------------

   procedure Set_Standard_Errors_File
     (Self : in out LSP_Client'Class;
      File : Virtual_File) is
   begin
      if Self.Standard_Errors_File /= File then
         if Self.Errors_Writable_File /= Invalid_File then
            GNATCOLL.VFS.Close (Self.Errors_Writable_File);
         end if;

         if not File.Is_Regular_File then
            --  Create an empty file. It is necessary for GNATCOLL to append
            --  to the file instead of use of temprorary file.

            declare
               Aux : GNATCOLL.VFS.Writable_File := File.Write_File;

            begin
               GNATCOLL.VFS.Close (Aux);
            end;
         end if;

         Self.Standard_Errors_File := File;
         Self.Errors_Writable_File := File.Write_File (Append => True);
         --  Open file with "Append => True" means that exactly given file
         --  will be used to write, and not a temporary file.
      end if;
   end Set_Standard_Errors_File;

   ------------------------------
   -- Get_Standard_Errors_File --
   ------------------------------

   function Get_Standard_Errors_File
     (Self : LSP_Client'Class) return Virtual_File
   is
   begin
      return Self.Standard_Errors_File;
   end Get_Standard_Errors_File;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self                   : aliased in out LSP_Client;
      Executable             : String;
      Arguments              : Spawn.String_Vectors.UTF_8_String_Vector;
      Initialization_Options : LSP.Types.Optional_LSP_Any)
   is

      function Get_Arguments_As_String
        (Arguments : Spawn.String_Vectors.UTF_8_String_Vector) return String;
      --  Return the list of arguments as a string.

      -----------------------------
      -- Get_Arguments_As_String --
      -----------------------------

      function Get_Arguments_As_String
        (Arguments : Spawn.String_Vectors.UTF_8_String_Vector) return String
      is
         Args : Unbounded_String;
      begin
         for Arg of Arguments loop
            Args := Args & To_Unbounded_String (Arg & " ");
         end loop;

         return To_String (Args);
      end Get_Arguments_As_String;

   begin
      Self.Set_Response_Handler (Self.Response_Handler'Unchecked_Access);
      Self.Set_Request_Handler  (Self.Request_Handler'Unchecked_Access);

      Self.Set_Program (Executable);
      Self.Set_Arguments (Arguments);
      Self.Set_Environment (Self.Kernel.Get_Original_Environment);
      Self.Initialization_Options := Initialization_Options;

      --  TODO: Self.Set_Working_Directory
      Me.Trace
        ("Starting '"
         & Executable
         & Get_Arguments_As_String (Arguments)
         & "'");
      Self.Exiting := False;
      Self.Launches.Prepend (Clock);
      Self.Start;
      Self.Shutdown_Intentionally_Requested := False;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop
     (Self               : in out LSP_Client'Class;
      Reject_Immediately : Boolean)
   is
      use type Glib.Main.G_Source_Id;

      Request : GPS.LSP_Client.Requests.Request_Access :=
                  new GPS.LSP_Clients.Shutdowns.Shutdown_Request
                    (Client => Self'Unchecked_Access);

   begin
      if Self.Restart_Timer /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Self.Restart_Timer);
         Self.Restart_Timer := Glib.Main.No_Source_Id;
      end if;

      Self.Shutdown_Intentionally_Requested := True;
      Self.Enqueue (Request);

      --  Disable acceptance of new requests
      Self.Is_Ready := False;

      if Reject_Immediately then
         Self.Reject_All_Requests;

         --  Disable reporting of any errors
         Self.Exiting := True;
      end if;
   end Stop;

   -------------
   -- Restart --
   -------------

   procedure Restart
     (Self                   : in out LSP_Client'Class;
      Initialization_Options : LSP.Types.Optional_LSP_Any)
   is
   begin
      --  Reset the initialization options
      Self.Initialization_Options := Initialization_Options;

      --  Initiate normal server shutdown sequence
      Self.Stop (Reject_Immediately => False);

      --  The relaunch is being requested by the user: clear the list
      --  of automatic relaunches so that the restart does not get
      --  stopped by the throttling mechanism.
      Self.Launches.Clear;

      --  Set this flag to False so that the relaunch mechanism gets enabled
      --  once the server process dies (see On_Finished).
      Self.Shutdown_Intentionally_Requested := False;
   end Restart;

end GPS.LSP_Clients;
