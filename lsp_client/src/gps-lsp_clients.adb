------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2026, AdaCore                     --
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
with VSS.JSON.Push_Writers;
with VSS.JSON.Streams;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.String_Vectors;
with VSS.Strings.Conversions; use VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with GNATCOLL.JSON;
with GNATCOLL.Traces; use GNATCOLL.Traces;

with LSP.Constants;
with LSP.Enumerations;
with LSP.Errors;
with LSP.JSON_Streams;
with LSP.Progress_Report_Readers;
with LSP.Progress_Reports;

with GPS.Editors;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Kernel.Project;
with GPS.LSP_Client.Editors.Semantic_Tokens;
with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Partial_Results;
with GPS.LSP_Client.Requests.Symbols;
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
      Request : GPS.LSP_Client.Requests.Request_Access);
   --  Invalidate all the queued requests and cancel the requests for which
   --  Queued.Auto_Cancel (Request) returns True

   procedure Clear_Change_Requests
     (Self : in out LSP_Client'Class; File : Virtual_File);
   --  Remove any pending change request for the given file

   function To_Token
     (Id : LSP.Structures.Integer_Or_Virtual_String)
      return LSP.Structures.ProgressToken
   is (case Id.Is_Integer is
         when True  => (Is_Integer => True, Integer => Id.Integer),
         when False =>
           (Is_Integer => False, Virtual_String => Id.Virtual_String));
   --  LSP.Structures.Integer_Or_Virtual_String and LSP.Structures.
   --  ProgressToken have the same shape but are distinct types.

   package LSP_Client_Sources is new
     Glib.Main.Generic_Sources (LSP_Client_Access);

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
               if Item.Kind = GPS_Request and then Item.Request = Request then
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
         Request_Id    : LSP.Structures.Integer_Or_Virtual_String;
         Partial_Token : LSP.Structures.ProgressToken;
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
                  in GPS
                       .LSP_Client
                       .Partial_Results
                       .LSP_Request_Partial_Result'Class
               then
                  Partial_Token :=
                    GPS
                      .LSP_Client
                      .Partial_Results
                      .LSP_Request_Partial_Result'Class (Request.all)
                      .Partial_Result_Token;

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

               GPS.LSP_Client.Requests.Destroy (Request, Is_Cancelled => True);

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
      Request_Id : LSP.Structures.Integer_Or_Virtual_String)
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
     (Self : LSP_Client'Class) return LSP.Structures.ServerCapabilities is
   begin
      return Self.Server_Capabilities;
   end Capabilities;

   ---------------------------
   -- Clear_Change_Requests --
   ---------------------------

   procedure Clear_Change_Requests
     (Self : in out LSP_Client'Class; File : Virtual_File)
   is
      New_Commands : Command_Lists.List;
   begin
      --  Remove any "Change_File" command pending for this file.
      for C of Self.Commands loop
         if not (C.Kind = Changed_File and then C.Handler.File = File) then
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
      if Request = null then
         return;
      end if;

      if Request.Auto_Cancel (Request) then
         Self.Auto_Cancel_Requests (Request);
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

   procedure Enqueue (Self : in out LSP_Client'Class; Item : in out Command) is
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

   -----------------------------
   -- On_Initialize_Response --
   -----------------------------

   overriding
   procedure On_Initialize_Response
     (Self  : in out Response_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.InitializeResult)
   is
      pragma Unreferenced (Id);

      Capabilities : LSP.Structures.ServerCapabilities := Value.capabilities;
   begin
      if Self.Client.On_Server_Capabilities /= null then
         Self.Client.On_Server_Capabilities (Capabilities);
      end if;

      Self.Client.Server_Capabilities := Capabilities;

      if Capabilities.textDocumentSync.Is_Set then
         if not Capabilities.textDocumentSync.Value.Is_TextDocumentSyncOptions
         then
            case Capabilities.textDocumentSync.Value.TextDocumentSyncKind is
               when LSP.Enumerations.None        =>
                  Self.Client.Text_Document_Synchronization :=
                    GPS.LSP_Client.Text_Documents.Full;

               when LSP.Enumerations.Full        =>
                  Self.Client.Text_Document_Synchronization :=
                    GPS.LSP_Client.Text_Documents.Full;

               when LSP.Enumerations.Incremental =>
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
      Self.Client.Send_Notification.On_Initialized_Notification
        ((null record));

      Self.Client.Listener.On_Server_Started;

      Process_Command_Queue (Self.Client.all);
   end On_Initialize_Response;

   ---------------------------
   -- On_ApplyEdit_Request --
   ---------------------------

   function To_Kernel_Handle
     (Self : not null access GPS.Kernel.Kernel_Handle_Record'Class)
      return GPS.Kernel.Kernel_Handle
   is (GPS.Kernel.Kernel_Handle (Self));
   --  Self.Client.Kernel (an access discriminant) cannot be converted to
   --  the named Kernel_Handle type directly, even though it was itself
   --  built from a Kernel_Handle when the LSP_Client was constructed and
   --  is guaranteed to outlive this handler. Routing the value through a
   --  regular anonymous-access parameter first makes the conversion legal.

   overriding
   procedure On_ApplyEdit_Request
     (Self  : in out Request_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.ApplyWorkspaceEditParams)
   is
      On_Error : Boolean;
   begin
      GPS.LSP_Client.Edit_Workspace.Edit
        (Kernel                   => To_Kernel_Handle (Self.Client.Kernel),
         Workspace_Edit           => Value.edit,
         Title                    => "Apply Workspace Edit",
         Make_Writable            => False,
         Auto_Save                => False,
         Allow_File_Renaming      => False,
         Locations_Message_Markup => Value.label,
         Error                    => On_Error);

      declare
         Result : LSP.Structures.ApplyWorkspaceEditResult;

      begin
         Result.applied := not On_Error;

         if On_Error then
            Result.failureReason := "Internal error";
         end if;

         Self.Client.Send_Response.On_ApplyEdit_Response (Id, Result);
         Self.Client.Listener.On_Response_Sent
           (To_Unbounded_String ("ApplyWorkspaceEditResponse"));
      end;
   end On_ApplyEdit_Request;

   --------------------------------
   -- On_Progress_Create_Request --
   --------------------------------

   overriding
   procedure On_Progress_Create_Request
     (Self  : in out Request_Handler;
      Id    : LSP.Structures.Integer_Or_Virtual_String;
      Value : LSP.Structures.WorkDoneProgressCreateParams)
   is
      pragma Unreferenced (Value);
   begin
      --  Do nothing here except send an empty response to the server
      Self.Client.Send_Response.On_Progress_Create_Response
        (Id, (null record));
   end On_Progress_Create_Request;

   ------------------------------
   -- On_Symbol_Partial_Result --
   ------------------------------

   overriding
   procedure On_Symbol_Partial_Result
     (Self  : in out Progress_Handler;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.Symbol_Progress_Report)
   is
      use type GPS.LSP_Client.Requests.Request_Access;
      use type LSP.Structures.Symbol_Progress_Report_Variant;

      Position : constant Partial_Token_Maps.Cursor :=
        Self.Client.Partials.Find (Token);
      Request  : GPS.LSP_Client.Requests.Request_Access;

   begin
      if not Partial_Token_Maps.Has_Element (Position)
        or else Value.Kind /= LSP.Structures.Variant_1
      then
         return;
      end if;

      Request :=
        Self.Client.Get_Running_Request
          (Partial_Token_Maps.Element (Position));

      if Request /= null
        and then
          Request.all
          in GPS.LSP_Client.Requests.Symbols.Abstract_Symbol_Request'Class
      then
         GPS.LSP_Client.Requests.Symbols.Abstract_Symbol_Request'Class
           (Request.all)
           .On_Partial_Result_Message (Value.Variant_1);
      end if;
   end On_Symbol_Partial_Result;

   --------------------------------
   -- On_ProgressBegin_Work_Done --
   --------------------------------

   overriding
   procedure On_ProgressBegin_Work_Done
     (Self  : in out Progress_Handler;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressBegin) is
   begin
      Self.Client.Listener.On_Progress_Begin (Token, Value);
   end On_ProgressBegin_Work_Done;

   ---------------------------------
   -- On_ProgressReport_Work_Done --
   ---------------------------------

   overriding
   procedure On_ProgressReport_Work_Done
     (Self  : in out Progress_Handler;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressReport) is
   begin
      Self.Client.Listener.On_Progress_Report (Token, Value);
   end On_ProgressReport_Work_Done;

   ------------------------------
   -- On_ProgressEnd_Work_Done --
   ------------------------------

   overriding
   procedure On_ProgressEnd_Work_Done
     (Self  : in out Progress_Handler;
      Token : LSP.Structures.ProgressToken;
      Value : LSP.Structures.WorkDoneProgressEnd) is
   begin
      Self.Client.Listener.On_Progress_End (Token, Value);
   end On_ProgressEnd_Work_Done;

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

   overriding
   procedure On_Error (Self : in out LSP_Client; Error : String) is
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

   procedure On_Exit_Notification (Self : in out LSP_Client'Class) is
   begin
      Self.Send_Notification.On_Exits_Notification;
      Self.Exiting := True;
   end On_Exit_Notification;

   -------------------------------
   -- On_Standard_Error_Message --
   -------------------------------

   overriding
   procedure On_Standard_Error_Message
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

   overriding
   procedure On_Finished (Self : in out LSP_Client) is
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

   overriding
   procedure On_Raw_Message
     (Self    : in out LSP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);

      procedure Look_Ahead
        (Id         : out LSP.Structures.Integer_Or_Virtual_String;
         Id_Found   : out Boolean;
         Method     : out VSS.Strings.Virtual_String;
         Token      : out LSP.Structures.ProgressToken_Optional;
         error      : out LSP.Errors.ResponseError_Optional;
         Has_Result : out Boolean);
      --  Parse message to find significant fields of the message: "id",
      --  "method", "error", and "result". First three are unparsed too.

      Text_Stream :
        aliased VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

      ----------------
      -- Look_Ahead --
      ----------------

      procedure Look_Ahead
        (Id         : out LSP.Structures.Integer_Or_Virtual_String;
         Id_Found   : out Boolean;
         Method     : out VSS.Strings.Virtual_String;
         Token      : out LSP.Structures.ProgressToken_Optional;
         error      : out LSP.Errors.ResponseError_Optional;
         Has_Result : out Boolean)
      is
         use all type VSS.JSON.Streams.JSON_Stream_Element_Kind;

         Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         JS     :
           aliased LSP.JSON_Streams.JSON_Stream
                     (False, Reader'Unchecked_Access);

      begin
         Id_Found := False;
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
                          (Is_Integer     => False,
                           Virtual_String => JS.R.String_Value);

                     when Number_Value =>
                        Id :=
                          (Is_Integer => True,
                           Integer    =>
                             Standard.Integer
                               (JS.R.Number_Value.Integer_Value));

                     when others       =>
                        raise Constraint_Error;
                  end case;

                  JS.R.Read_Next;

               elsif Key = "method" then
                  pragma Assert (JS.R.Is_String_Value);

                  Method := JS.R.String_Value;

                  exit when Id_Found;

                  JS.R.Read_Next;

               elsif Key = "error" then
                  pragma Assert (JS.R.Is_Start_Object);

                  JS.R.Read_Next;

                  declare
                     Code    : LSP.Enumerations.ErrorCodes := 0;
                     Message : VSS.Strings.Virtual_String;

                  begin
                     while not JS.R.Is_End_Object loop
                        pragma Assert (JS.R.Is_Key_Name);

                        declare
                           Error_Key : constant String :=
                             VSS.Strings.Conversions.To_UTF_8_String
                               (JS.R.Key_Name);

                        begin
                           JS.R.Read_Next;

                           if Error_Key = "code" then
                              Code :=
                                LSP.Enumerations.ErrorCodes
                                  (JS.R.Number_Value.Integer_Value);
                              JS.R.Read_Next;

                           elsif Error_Key = "message" then
                              Message := JS.R.String_Value;
                              JS.R.Read_Next;

                           else
                              JS.Skip_Value;
                           end if;
                        end;
                     end loop;

                     error :=
                       (Is_Set => True,
                        Value  => (code => Code, message => Message));
                  end;

                  JS.R.Read_Next;
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
                                    Value  =>
                                      (Is_Integer     => False,
                                       Virtual_String => JS.R.String_Value));

                              when Number_Value =>
                                 Token :=
                                   (Is_Set => True,
                                    Value  =>
                                      (Is_Integer => True,
                                       Integer    =>
                                         Standard.Integer
                                           (JS.R.Number_Value.Integer_Value)));

                              when others       =>
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

      Reader   : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Stream   :
        aliased LSP.JSON_Streams.JSON_Stream
                  (Is_Server_Side => False, R => Reader'Unchecked_Access);
      Id       : LSP.Structures.Integer_Or_Virtual_String;
      Id_Found : Boolean;
      Token    : LSP.Structures.ProgressToken_Optional;
      Method   : VSS.Strings.Virtual_String;

      Request_Position : Request_Maps.Cursor;
      Partial_Position : Partial_Token_Maps.Cursor;
      Request          : GPS.LSP_Client.Requests.Request_Access := null;
      Req_Method       : VSS.Strings.Virtual_String;
      --  The method for the request to which this response corresponds, if any

      error      : LSP.Errors.ResponseError_Optional;
      Processed  : Boolean := False;
      Has_Result : Boolean := False;

   begin
      Text_Stream.Set_Data
        (VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
           (Data));

      Look_Ahead (Id, Id_Found, Method, Token, error, Has_Result);

      if Id_Found and Method.Is_Empty then
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
               in GPS
                    .LSP_Client
                    .Partial_Results
                    .LSP_Request_Partial_Result'Class
            then
               Self.Partials.Delete
                 (GPS
                    .LSP_Client
                    .Partial_Results
                    .LSP_Request_Partial_Result'Class (Request.all)
                    .Partial_Result_Token);
            end if;

            Req_Method := Request.Method;

            if error.Is_Set then
               begin
                  Request.On_Error_Message
                    (Code => error.Value.code, Message => error.Value.message);

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
                  exit when
                    Stream.R.Is_Key_Name
                    and then
                      VSS.Strings.Conversions.To_UTF_8_String
                        (Stream.R.Key_Name)
                      = "result";
               end loop;

               Stream.R.Read_Next;

               declare
                  use type GPS.Kernel.Kernel_Handle;
               begin
                  if Request.Kernel = null
                    or else not Request.Kernel.Is_In_Destruction
                  then
                     Request.On_Result_Message (Reader);
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

      elsif not Method.Is_Empty
        and then Method = "$/progress"
        and then Token.Is_Set
      then
         --  Partial result notification, or generic work-done progress
         --  notification not tied to any of our own pending requests.

         Partial_Position := Self.Partials.Find (Token.Value);

         if Partial_Token_Maps.Has_Element (Partial_Position) then
            Request_Position :=
              Self.Requests.Find
                (Partial_Token_Maps.Element (Partial_Position));

            if Request_Maps.Has_Element (Request_Position) then
               Request := Request_Maps.Element (Request_Position);

               Reader.Set_Stream (Text_Stream'Unchecked_Access);

               --  Rewind Reader to the start of the "params" value

               loop
                  Stream.R.Read_Next;

                  exit when
                    Stream.R.Is_Key_Name
                    and then
                      VSS.Strings.Conversions.To_UTF_8_String
                        (Stream.R.Key_Name)
                      = "params";
               end loop;

               Stream.R.Read_Next;

               declare
                  use type GPS.Kernel.Kernel_Handle;

               begin
                  if Request.Kernel = null
                    or else not Request.Kernel.Is_In_Destruction
                  then
                     LSP.Progress_Report_Readers.Read_Progress_Report
                       (Reader, Request.Method)
                       .Visit_Receiver (Self.Progress_Handler);
                  end if;

               exception
                  when E : others =>
                     Trace (Me_Exceptions, E);
               end;

            else
               pragma
                 Assert
                   (Self.Canceled_Requests.Contains
                      (Partial_Token_Maps.Element (Partial_Position)));
            end if;

            Processed := True;

         else
            --  Not one of our own pending requests: this is a generic
            --  work-done progress notification (e.g. background indexing).

            declare
               Kind_Name : VSS.Strings.Virtual_String;

            begin
               Reader.Set_Stream (Text_Stream'Unchecked_Access);

               loop
                  Stream.R.Read_Next;

                  exit when
                    Stream.R.Is_Key_Name
                    and then
                      VSS.Strings.Conversions.To_UTF_8_String
                        (Stream.R.Key_Name)
                      = "params";
               end loop;

               Stream.R.Read_Next;
               pragma Assert (Stream.R.Is_Start_Object);
               Stream.R.Read_Next;

               while not Stream.R.Is_End_Object loop
                  pragma Assert (Stream.R.Is_Key_Name);

                  declare
                     Param_Key : constant String :=
                       VSS.Strings.Conversions.To_UTF_8_String
                         (Stream.R.Key_Name);

                  begin
                     Stream.R.Read_Next;

                     if Param_Key = "value" then
                        pragma Assert (Stream.R.Is_Start_Object);
                        Stream.R.Read_Next;

                        while not Stream.R.Is_End_Object loop
                           pragma Assert (Stream.R.Is_Key_Name);

                           declare
                              Value_Key : constant String :=
                                VSS.Strings.Conversions.To_UTF_8_String
                                  (Stream.R.Key_Name);

                           begin
                              Stream.R.Read_Next;

                              if Value_Key = "kind" then
                                 Kind_Name := Stream.R.String_Value;
                              end if;

                              Stream.Skip_Value;
                           end;
                        end loop;

                     else
                        Stream.Skip_Value;
                     end if;
                  end;
               end loop;

               declare
                  Method_Name : VSS.Strings.Virtual_String;

               begin
                  if Kind_Name = "begin" then
                     Method_Name := "WorkDoneProgressBegin";
                  elsif Kind_Name = "report" then
                     Method_Name := "WorkDoneProgressReport";
                  elsif Kind_Name = "end" then
                     Method_Name := "WorkDoneProgressEnd";
                  end if;

                  if not Method_Name.Is_Empty then
                     Reader.Set_Stream (Text_Stream'Unchecked_Access);

                     loop
                        Stream.R.Read_Next;

                        exit when
                          Stream.R.Is_Key_Name
                          and then
                            VSS.Strings.Conversions.To_UTF_8_String
                              (Stream.R.Key_Name)
                            = "params";
                     end loop;

                     Stream.R.Read_Next;

                     LSP.Progress_Report_Readers.Read_Progress_Report
                       (Reader, Method_Name)
                       .Visit_Receiver (Self.Progress_Handler);
                  end if;
               end;
            end;

            Processed := True;
         end if;
      end if;

      if error.Is_Set then
         declare
            use GPS.LSP_Client.Requests;
            S               : constant String :=
              "The language server has reported the following error "
              & "for language:"
              & Self.Language.Get_Name
              & ASCII.LF
              & "Code: "
              & error.Value.code'Img
              & ASCII.LF
              & VSS.Strings.Conversions.To_UTF_8_String (error.Value.message);
            Request_Message : constant String :=
              (if Request /= null
               then
                 (ASCII.LF
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
        (Data, VSS.Strings.Conversions.To_Unbounded_UTF_8_String (Req_Method));
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
           ("The language server for "
            & Self.Language.Get_Name
            & " had to be restarted more than"
            & Throttle_Max'Img
            & " times in the past"
            & Integer (Throttle_Period)'Img
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

   overriding
   procedure On_Exception
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

   overriding
   procedure On_Started (Self : in out LSP_Client) is

      function Get_Completion_Documentation_Formats
         return LSP.Structures.MarkupKind_Vector;

      function Get_CompletionItem_Resolve_Properties
         return VSS.String_Vectors.Virtual_String_Vector;

      function Get_Supported_ResourceOperations
         return LSP.Structures.ResourceOperationKind_Set;

      function Get_Supported_CodeActionKinds
         return LSP.Structures.CodeActionKind_Set;

      function Get_Supported_Symbols return LSP.Structures.SymbolKind_Set;

      function Get_Experimental_Features return LSP.Structures.LSPAny;
      --  Return the selection of experimental features that are supported

      -------------------------------
      -- Get_Experimental_Features --
      -------------------------------

      function Get_Experimental_Features return LSP.Structures.LSPAny is
         --  Craft something like:
         --     {"advanced_refactorings":["add_parameter"
         use GNATCOLL.JSON;
         Result       : constant JSON_Value := Create_Object;
         Refactorings : JSON_Array;
      begin
         Refactorings.Append (Create ("add_parameter"));
         Result.Set_Field ("advanced_refactorings", Refactorings);
         return GPS.LSP_Client.Utilities.To_LSP_Any (Result);
      end Get_Experimental_Features;

      ------------------------------------------
      -- Get_Completion_Documentation_Formats --
      ------------------------------------------

      function Get_Completion_Documentation_Formats
         return LSP.Structures.MarkupKind_Vector
      is
         Completion_Doc_Format : LSP.Structures.MarkupKind_Vector;
      begin
         Completion_Doc_Format.Append (LSP.Enumerations.PlainText);
         Completion_Doc_Format.Append (LSP.Enumerations.Markdown);

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
         return LSP.Structures.ResourceOperationKind_Set is
      begin
         return (others => True);
      end Get_Supported_ResourceOperations;

      -----------------------------------
      -- Get_Supported_CodeActionKinds --
      -----------------------------------

      function Get_Supported_CodeActionKinds
         return LSP.Structures.CodeActionKind_Set
      is
         Result : LSP.Structures.CodeActionKind_Set;
      begin
         Result.Append (LSP.Enumerations.QuickFix);
         Result.Append (LSP.Enumerations.Refactor);
         Result.Append (LSP.Enumerations.RefactorExtract);
         Result.Append (LSP.Enumerations.RefactorInline);
         Result.Append (LSP.Enumerations.RefactorRewrite);
         Result.Append (LSP.Enumerations.Source);
         Result.Append (LSP.Enumerations.SourceOrganizeImports);

         return Result;
      end Get_Supported_CodeActionKinds;

      ---------------------------
      -- Get_Supported_Symbols --
      ---------------------------

      function Get_Supported_Symbols return LSP.Structures.SymbolKind_Set is
      begin
         return (others => True);
      end Get_Supported_Symbols;

      Root   : constant GNATCOLL.VFS.Virtual_File :=
        GPS.Kernel.Project.Get_Project (Self.Kernel).Project_Path.Dir;
      --  ??? Root directory of the project is directoy where
      --  project file is stored.
      --  ??? Must be synchronized with ada.projectFile passed in
      --  WorkspaceDidChangeConfiguration notification.
      Id     : LSP.Structures.Integer_Or_Virtual_String;
      My_PID : constant Standard.Integer :=
        GNAT.OS_Lib.Pid_To_Integer (GNAT.OS_Lib.Current_Process_Id);

      Base : constant LSP.Structures.An_InitializeParams :=
        (workDoneToken         => (Is_Set => False),
         processId             => (Is_Null => False, Value => My_PID),
         clientInfo            => (Is_Set => False),
         locale                => VSS.Strings.Empty_Virtual_String,
         rootPath              => (Is_Set => False),
         rootUri               =>
           (Is_Null => False, Value => GPS.LSP_Client.Utilities.To_URI (Root)),
         capabilities          =>
           (workspace    =>
              (Is_Set => True,
               Value  =>
                 (applyEdit      => LSP.Constants.True,
                  workspaceEdit  =>
                    (Is_Set => True,
                     Value  =>
                       (documentChanges    => LSP.Constants.True,
                        resourceOperations => Get_Supported_ResourceOperations,
                        others             => <>)),
                  fileOperations =>
                    (Is_Set => True,
                     Value  =>
                       (didRename => LSP.Constants.True, others => <>)),
                  symbol         =>
                    (Is_Set => True,
                     Value  =>
                       (symbolKind =>
                          (Is_Set => True,
                           Value  => (valueSet => Get_Supported_Symbols)),
                        others     => <>)),
                  others         => <>)),
            textDocument =>
              (Is_Set => True,
               Value  =>
                 (hover              => (Is_Set => True, others => <>),
                  signatureHelp      => (Is_Set => True, others => <>),
                  declaration        => (Is_Set => True, others => <>),
                  definition         => (Is_Set => True, others => <>),
                  typeDefinition     => (Is_Set => True, others => <>),
                  implementation     => (Is_Set => True, others => <>),
                  publishDiagnostics =>
                    (Is_Set => True,
                     Value  =>
                       (relatedInformation => LSP.Constants.True,
                        others             => <>)),
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
                    (Is_Set => True,
                     Value  =>
                       (dynamicRegistration => LSP.Constants.True,
                        completionItem      =>
                          (Is_Set => True,
                           Value  =>
                             (snippetSupport      =>
                                (Is_Set => True,
                                 Value  => LSP_Use_Snippets.Get_Pref),
                              documentationFormat =>
                                Get_Completion_Documentation_Formats,
                              resolveSupport      =>
                                (Is_Set => True,
                                 Value  =>
                                   (properties =>
                                      Get_CompletionItem_Resolve_Properties)),
                              others              => <>)),
                        others              => <>)),
                  --  Right now we support only whole line folding
                  foldingRange       =>
                    (Is_Set => True,
                     Value  =>
                       (lineFoldingOnly => LSP.Constants.True, others => <>)),
                  documentSymbol     =>
                    (Is_Set => True,
                     Value  =>
                       (hierarchicalDocumentSymbolSupport =>
                          LSP.Constants.True,
                        others                            => <>)),
                  formatting         =>
                    (Is_Set => True,
                     Value  => (dynamicRegistration => LSP.Constants.False)),
                  rangeFormatting    =>
                    (Is_Set => True,
                     Value  => (dynamicRegistration => LSP.Constants.False)),
                  onTypeFormatting   =>
                    (Is_Set => True,
                     Value  => (dynamicRegistration => LSP.Constants.False)),
                  semanticTokens     =>
                    GPS
                      .LSP_Client
                      .Editors
                      .Semantic_Tokens
                      .Get_Supported_Options,
                  others             => <>)),
            window       => (Is_Set => False),
            general      => (Is_Set => False),
            experimental => Get_Experimental_Features,
            others       => <>),
         initializationOptions => Self.Initialization_Options,
         trace                 => (Is_Set => False));

      Request : constant LSP.Structures.InitializeParams :=
        (Base with Parent => (workspaceFolders => (Is_Set => False)));

   begin
      Id := Self.Allocate_Request_Id;
      Self.Send_Request.On_Initialize_Request (Id, Request);
   end On_Started;

   ---------------------
   -- Process_Command --
   ---------------------

   procedure Process_Command (Self : in out LSP_Client'Class; Item : Command)
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
         Self.Send_Notification.On_CancelRequest_Notification
           ((id => Item.Id));
      end Process_Cancel_Request;

      --------------------------
      -- Process_Changed_File --
      --------------------------

      procedure Process_Changed_File is
      begin
         Self.Send_Notification.On_DidChange_Notification
           (Item.Handler.Get_Did_Change_Message
              (Self.Text_Document_Synchronization));
      end Process_Changed_File;

      ------------------------
      -- Process_Close_File --
      ------------------------

      procedure Process_Close_File is
         use GPS.Editors;

         Value : constant LSP.Structures.DidCloseTextDocumentParams :=
           (textDocument =>
              (uri => GPS.LSP_Client.Utilities.To_URI (Item.File)));

         Buffer : constant GPS.Editors.Editor_Buffer'Class :=
           Self.Kernel.Get_Buffer_Factory.Get
             (File => Item.File, Open_Buffer => False, Open_View => False);
      begin
         if Buffer /= Nil_Editor_Buffer then
            Buffer.Set_Opened_On_LSP_Server (False);
         end if;
         Self.Send_Notification.On_DidClose_Notification (Value);
      end Process_Close_File;

      -----------------------
      -- Process_Open_File --
      -----------------------

      procedure Process_Open_File is
         Factory : constant GPS.Editors.Editor_Buffer_Factory_Access :=
           Self.Kernel.Get_Buffer_Factory;
         Buffer  : constant GPS.Editors.Editor_Buffer'Class :=
           Factory.Get
             (File => Item.File, Open_Buffer => True, Open_View => False);
         Lang    : constant not null Language.Language_Access :=
           Buffer.Get_Language;
         Value   : constant LSP.Structures.DidOpenTextDocumentParams :=
           (textDocument =>
              (uri        => GPS.LSP_Client.Utilities.To_URI (Item.File),
               languageId =>
                 VSS.Strings.Conversions.To_Virtual_String (Lang.Get_Name),
               version    => 0,
               text       => Buffer.Get_Text));

      begin
         Self.Send_Notification.On_DidOpen_Notification (Value);
         Buffer.Set_Opened_On_LSP_Server (True);
      end Process_Open_File;

      -------------------------
      -- Process_Rename_File --
      -------------------------

      procedure Process_Rename_File is
         Value : LSP.Structures.RenameFilesParams;
      begin
         Value.files.Append
           (LSP.Structures.FileRename'
              (oldUri =>
                 LSP.Structures.Virtual_String
                   (GPS.LSP_Client.Utilities.To_URI (Item.Old_URI)),
               newUri =>
                 LSP.Structures.Virtual_String
                   (GPS.LSP_Client.Utilities.To_URI (Item.New_URI))));
         Self.Send_Notification.On_DidRenameFiles_Notification (Value);
      end Process_Rename_File;

      ---------------------
      -- Process_Request --
      ---------------------

      procedure Process_Request is
         Id     : constant LSP.Structures.Integer_Or_Virtual_String :=
           Self.Allocate_Request_Id;
         Writer : aliased VSS.JSON.Push_Writers.JSON_Simple_Push_Writer;
         Output :
           aliased VSS
                     .Text_Streams
                     .Memory_UTF8_Output
                     .Memory_UTF8_Output_Stream;

      begin
         --  Allocate and set id of the request and token of the partial result
         --  when supported. Add request to the maps.

         Item.Request.Set_Id (Id);
         Self.Requests.Insert (Id, Item.Request);

         if Item.Request.all
            in GPS.LSP_Client.Partial_Results.LSP_Request_Partial_Result'Class
         then
            declare
               Token : constant LSP.Structures.ProgressToken :=
                 To_Token (Self.Allocate_Request_Id);

            begin
               GPS.LSP_Client.Partial_Results.LSP_Request_Partial_Result'Class
                 (Item.Request.all)
                 .Set_Partial_Result_Token (Token);
               Self.Partials.Insert (Token, Id);
            end;
         end if;

         Writer.Set_Stream (Output'Unchecked_Access);
         Writer.Start_Document;
         Writer.Start_Object;

         --  Serialize "jsonrpc" member

         Writer.Key_Name ("jsonrpc");
         Writer.String_Value ("2.0");

         --  Serialize "id" memeber

         Writer.Key_Name ("id");

         if Id.Is_Integer then
            Writer.Integer_Value (Interfaces.Integer_64 (Id.Integer));

         else
            Writer.String_Value (Id.Virtual_String);
         end if;

         --  Serialize "method" member

         Writer.Key_Name ("method");
         Writer.String_Value (Item.Request.Method);

         --  Serialize "params" member

         Writer.Key_Name ("params");
         Item.Request.Params (Writer);

         Writer.End_Object;
         Writer.End_Document;

         --  Send request's message

         Self.Send_Buffer (Output.Buffer);
      end Process_Request;

   begin
      case Item.Kind is
         when Open_File          =>
            Process_Open_File;

         when Changed_File       =>
            Process_Changed_File;

         when Close_File         =>
            Process_Close_File;

         when Rename_File        =>
            Process_Rename_File;

         when GPS_Request        =>
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
      Request : GPS.LSP_Client.Requests.Request_Access)
   is
      use GPS.LSP_Client.Requests.Requests_Lists;
      use type GPS.LSP_Client.Requests.Request_Access;

      --  Keep a copy of the requests list because we are tampering with the
      --  real list.
      Requests : constant GPS.LSP_Client.Requests.Requests_Lists.List :=
        Self.Get_Requests;
      Cursor   : GPS.LSP_Client.Requests.Requests_Lists.Cursor :=
        First (Requests);
   begin
      if Request /= null then
         while Has_Element (Cursor) loop
            if Element (Cursor).Method = Request.Method
              and then Element (Cursor).Auto_Cancel (Request)
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
      end if;
   end Auto_Cancel_Requests;

   -----------------------
   -- Request_Id_Prefix --
   -----------------------

   overriding
   function Request_Id_Prefix
     (Self : LSP_Client) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String
          (Ada.Characters.Handling.To_Lower (Self.Language.Get_Name));
   end Request_Id_Prefix;

   ---------------------
   -- Server_Language --
   ---------------------

   overriding
   function Server_Language
     (Self : LSP_Client) return VSS.Strings.Virtual_String is
   begin
      return
        VSS.Strings.Conversions.To_Virtual_String (Self.Language.Get_Name);
   end Server_Language;

   -----------------------------------
   -- Send_Text_Document_Did_Change --
   -----------------------------------

   overriding
   procedure Send_Text_Document_Did_Change
     (Self     : in out LSP_Client;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access)
   is
      use type GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access;
      Item : Command;

   begin
      for Command of Self.Commands loop
         if Command.Kind = Changed_File and then Command.Handler = Document
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

   overriding
   procedure Send_Text_Document_Did_Close
     (Self : in out LSP_Client; File : GNATCOLL.VFS.Virtual_File)
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

   overriding
   procedure Send_Text_Document_Did_Open
     (Self : in out LSP_Client; File : GNATCOLL.VFS.Virtual_File)
   is
      Item : Command := (Open_File, File);
   begin
      Self.Enqueue (Item);
   end Send_Text_Document_Did_Open;

   --------------------------
   -- Send_Did_Rename_File --
   --------------------------

   overriding
   procedure Send_Did_Rename_File
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
     (Self : in out LSP_Client'Class; Proc : On_Server_Capabilities_Proc) is
   begin
      Self.On_Server_Capabilities := Proc;
   end Set_On_Server_Capabilities;

   ------------------------------
   -- Set_Standard_Errors_File --
   ------------------------------

   procedure Set_Standard_Errors_File
     (Self : in out LSP_Client'Class; File : Virtual_File) is
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
     (Self : LSP_Client'Class) return Virtual_File is
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
      Initialization_Options : LSP.Structures.LSPAny)
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
      Self.Set_Request_Handler (Self.Request_Handler'Unchecked_Access);

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
     (Self : in out LSP_Client'Class; Reject_Immediately : Boolean)
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
      Initialization_Options : LSP.Structures.LSPAny) is
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
