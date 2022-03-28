------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;       use GNATCOLL.Traces;
with Spawn.String_Vectors;

with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with GPS.Kernel.Hooks;

with DAP.Persistent_Breakpoints;
with DAP.Module;
with DAP.Requests.Initialize;
with DAP.Requests.Disconnects;
with DAP.Requests.StackTraces;
with DAP.Tools;
with DAP.Views;

package body DAP.Clients is

   Me : constant Trace_Handle := Create ("GPS.DAP.Clients", On);

   Node_Binary : constant String := "/usr/bin/node";

   procedure Free is new Ada.Unchecked_Deallocation
     (DAP.Modules.Breakpoint_Managers.DAP_Client_Breakpoint_Manager'Class,
      DAP.Modules.Breakpoint_Managers.DAP_Client_Breakpoint_Manager_Access);

   -- StackTrace_Request --

   type StackTrace_Request is
     new DAP.Requests.StackTraces.StackTrace_DAP_Request
   with record
      Client : DAP_Client_Access;
   end record;

   type StackTrace_Request_Access is access all StackTrace_Request;

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   ------------------
   -- Break_Source --
   ------------------

   procedure Break_Source
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type;
      Temporary : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Sorce (Self.Breakpoints, File, Line, Temporary);
      end if;
   end Break_Source;

   ----------------------
   -- Break_Subprogram --
   ----------------------

   procedure Break_Subprogram
     (Self       : in out DAP_Client;
      Subprogram : String;
      Temporary  : Boolean := False)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Break_Subprogram (Self.Breakpoints, Subprogram, Temporary);
      end if;
   end Break_Subprogram;

   ------------------
   -- Current_File --
   ------------------

   function Current_File
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Stopped_File;
   end Current_File;

   ------------------
   -- Current_Line --
   ------------------

   function Current_Line
     (Self : in out DAP_Client) return Integer is
   begin
      return Self.Stopped_Line;
   end Current_Line;

   --------------------------
   -- Remove_Breakpoint_At --
   --------------------------

   procedure Remove_Breakpoint_At
     (Self      : in out DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_Breakpoint_At (Self.Breakpoints, File, Line);
      end if;
   end Remove_Breakpoint_At;

   ------------------------
   -- Remove_Breakpoints --
   ------------------------

   procedure Remove_Breakpoints
     (Self : in out DAP_Client;
      List : DAP.Types.Breakpoint_Identifier_Lists.List)
   is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_Breakpoints (Self.Breakpoints, List);
      end if;
   end Remove_Breakpoints;

   ----------------------------
   -- Remove_All_Breakpoints --
   ----------------------------

   procedure Remove_All_Breakpoints (Self : in out DAP_Client) is
      use DAP.Modules.Breakpoint_Managers;
   begin
      if Self.Breakpoints /= null then
         Remove_All_Breakpoints (Self.Breakpoints);
      end if;
   end Remove_All_Breakpoints;

   --------------------
   -- Has_Breakpoint --
   --------------------

   function Has_Breakpoint
     (Self      : DAP_Client;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Editable_Line_Type)
      return Boolean is
   begin
      return False;
   end Has_Breakpoint;

   -----------------
   -- Can_Enqueue --
   -----------------

   function Can_Enqueue (Self : DAP_Client) return Boolean is
   begin
      return Self.Status in Initialization .. Stopped;
   end Can_Enqueue;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status
     (Self   : in out DAP_Client;
      Status : Debugger_Status_Kind)
   is
      use DAP.Modules.Breakpoint_Managers;
      use type Generic_Views.Abstract_View_Access;
   begin
      if Self.Status /= Terminating then
         Self.Status := Status;

         if Self.Breakpoints /= null then
            Self.Breakpoints.Status_Changed (Self.Status);
         end if;

         if Self.Breakpoints_View /= null then
            DAP.Views.View_Access (Self.Breakpoints_View).On_Status_Changed
              (Self.Status);
         end if;
      end if;
   end Set_Status;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access) is
   begin
      if Self.Can_Enqueue then
         Self.Process (Request);

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;

      Request := null;
   end Enqueue;

   ---------------------
   -- Get_Breakpoints --
   ---------------------

   function Get_Breakpoints
     (Self : DAP_Client)
      return DAP.Breakpoint_Maps.All_Breakpoints
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;

      Empty : DAP.Breakpoint_Maps.All_Breakpoints;
   begin
      if Self.Breakpoints /= null then
         return DAP.Modules.Breakpoint_Managers.Get_Breakpoints
           (Self.Breakpoints);
      else
         return Empty;
      end if;
   end Get_Breakpoints;

   --------------------------
   -- Get_Breakpoints_View --
   --------------------------

   function Get_Breakpoints_View
     (Self : DAP_Client)
      return Generic_Views.Abstract_View_Access is
   begin
      return Self.Breakpoints_View;
   end Get_Breakpoints_View;

   --------------------
   -- Get_Executable --
   --------------------

   function Get_Executable
     (Self : in out DAP_Client) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end Get_Executable;

   ---------------------------
   -- Set_Breakpoints_State --
   ---------------------------

   procedure Set_Breakpoints_State
     (Self  : in out DAP_Client;
      List  : Breakpoint_Identifier_Lists.List;
      State : Boolean)
   is
      use type DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager_Access;
   begin
      if Self.Breakpoints /= null then
         DAP.Modules.Breakpoint_Managers.Set_Breakpoints_State
           (Self.Breakpoints, List, State);
      end if;
   end Set_Breakpoints_State;

   --------------------------
   -- Set_Breakpoints_View --
   --------------------------

   procedure Set_Breakpoints_View
     (Self : in out DAP_Client;
      View : Generic_Views.Abstract_View_Access) is
   begin
      Self.Breakpoints_View := View;
   end Set_Breakpoints_View;

   --------------------
   -- Get_Request_ID --
   --------------------

   function Get_Request_ID
     (Self : in out DAP_Client) return LSP.Types.LSP_Number
   is
      use type LSP.Types.LSP_Number;
      ID : constant LSP.Types.LSP_Number := Self.Request_Id;
   begin
      if Self.Request_Id < LSP.Types.LSP_Number'Last then
         Self.Request_Id := Self.Request_Id + 1;
      else
         Self.Request_Id := 1;
      end if;

      return ID;
   end Get_Request_ID;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Self : in out DAP_Client) return Debugger_Status_Kind is
   begin
      return Self.Status;
   end Get_Status;

   -------------------
   -- Error_Message --
   -------------------

   overriding function Error_Message
     (Self : DAP_Client) return VSS.Strings.Virtual_String is
   begin
      return Self.Error_Msg;
   end Error_Message;

   -------------------
   -- On_Configured --
   -------------------

   procedure On_Configured (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Running);
   end On_Configured;

   --------------
   -- On_Ready --
   --------------

   procedure On_Ready (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Ready);
   end On_Ready;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue (Self : in out DAP_Client) is
   begin
      Self.Set_Status (Running);
   end On_Continue;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished (Self : in out DAP_Client)
   is
      use type Generic_Views.Abstract_View_Access;
   begin
      Self.Reject_All_Requests;
      begin
         if Self.Breakpoints_View /= null then
            DAP.Views.View_Access
              (Self.Breakpoints_View).On_Process_Terminated;
            Self.Breakpoints_View := null;
         end if;
      exception
         when E : others =>
            Trace (Me, E);
      end;

      DAP.Modules.Breakpoint_Managers.On_Finished (Self.Breakpoints);
      Free (Self.Breakpoints);
      DAP.Persistent_Breakpoints.Show_Breakpoints_In_All_Editors (Self.Kernel);
      DAP.Module.Finished (Self.Id);
   end On_Finished;

   -----------------
   -- On_Launched --
   -----------------

   procedure On_Launched (Self : in out DAP_Client) is
   begin
      Self.Breakpoints := new DAP.Modules.Breakpoint_Managers.
        DAP_Client_Breakpoint_Manager (Self.Kernel, Self.This);
      DAP.Modules.Breakpoint_Managers.Initialize (Self.Breakpoints);
   end On_Launched;

   --------------------
   -- On_Raw_Message --
   --------------------

   overriding procedure On_Raw_Message
     (Self    : in out DAP_Client;
      Data    : Ada.Strings.Unbounded.Unbounded_String;
      Success : in out Boolean)
   is
      pragma Unreferenced (Success);
      use type VSS.Strings.Virtual_String;
      use type LSP.Types.LSP_Number;
      use type DAP.Requests.DAP_Request_Access;

      procedure Look_Ahead
        (Seq         : out LSP.Types.LSP_Number;
         Request_Seq : out LSP.Types.LSP_Number;
         A_Type      : out VSS.Strings.Virtual_String;
         Success     : out LSP.Types.Optional_Boolean;
         Message     : in out VSS.Strings.Virtual_String;
         Event       : in out VSS.Strings.Virtual_String);

      Memory : aliased
        VSS.Text_Streams.Memory_UTF8_Input.Memory_UTF8_Input_Stream;

      ----------------
      -- Look_Ahead --
      ----------------

      procedure Look_Ahead
        (Seq         : out LSP.Types.LSP_Number;
         Request_Seq : out LSP.Types.LSP_Number;
         A_Type      : out VSS.Strings.Virtual_String;
         Success     : out LSP.Types.Optional_Boolean;
         Message     : in out VSS.Strings.Virtual_String;
         Event       : in out VSS.Strings.Virtual_String)
      is

         Reader : aliased
           VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
         JS     : aliased LSP.JSON_Streams.JSON_Stream
           (False, Reader'Access);

      begin
         Seq         := 0;
         Request_Seq := 0;
         A_Type      := VSS.Strings.Empty_Virtual_String;
         Success     := (Is_Set => False);

         Reader.Set_Stream (Memory'Unchecked_Access);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Document);
         JS.R.Read_Next;
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               Key : constant String :=
                 VSS.Strings.Conversions.To_UTF_8_String (JS.R.Key_Name);

            begin
               JS.R.Read_Next;

               if Key = "seq" then
                  pragma Assert (JS.R.Is_Number_Value);
                  Seq := LSP.Types.LSP_Number
                    (JS.R.Number_Value.Integer_Value);

                  JS.R.Read_Next;

               elsif Key = "request_seq" then
                  pragma Assert (JS.R.Is_Number_Value);
                  Request_Seq := LSP.Types.LSP_Number
                    (JS.R.Number_Value.Integer_Value);

                  JS.R.Read_Next;

               elsif Key = "type" then
                  pragma Assert (JS.R.Is_String_Value);

                  A_Type := JS.R.String_Value;

                  JS.R.Read_Next;

               elsif Key = "success" then
                  pragma Assert (JS.R.Is_Boolean_Value);

                  Success := (True, JS.R.Boolean_Value);

                  JS.R.Read_Next;

               elsif Key = "message" then
                  pragma Assert (JS.R.Is_String_Value);

                  Message := JS.R.String_Value;

                  JS.R.Read_Next;

               elsif Key = "event" then
                  pragma Assert (JS.R.Is_String_Value);

                  Event := JS.R.String_Value;

                  JS.R.Read_Next;

               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;

         Memory.Rewind;
      end Look_Ahead;

      Reader : aliased VSS.JSON.Pull_Readers.Simple.JSON_Simple_Pull_Reader;
      Stream : aliased LSP.JSON_Streams.JSON_Stream
        (Is_Server_Side => False, R => Reader'Access);

      Seq         : LSP.Types.LSP_Number;
      A_Type      : VSS.Strings.Virtual_String;
      Request_Seq : LSP.Types.LSP_Number;
      R_Success   : LSP.Types.Optional_Boolean;
      Message     : VSS.Strings.Virtual_String;
      Event       : VSS.Strings.Virtual_String;

      Position    : Requests_Maps.Cursor;
      Request     : DAP.Requests.DAP_Request_Access;
      New_Request : DAP.Requests.DAP_Request_Access := null;

   begin
      Memory.Set_Data
        (VSS.Stream_Element_Vectors.Conversions.Unchecked_From_Unbounded_String
           (Data));

      Look_Ahead (Seq, Request_Seq, A_Type, R_Success, Message, Event);

      Reader.Set_Stream (Memory'Unchecked_Access);
      Stream.R.Read_Next;
      pragma Assert (Stream.R.Is_Start_Document);
      Stream.R.Read_Next;

      if A_Type = "response" then
         if Request_Seq /= 0 then
            Position := Self.Sent.Find (Request_Seq);

            if Requests_Maps.Has_Element (Position) then
               Request := Requests_Maps.Element (Position);
               Self.Sent.Delete (Position);

               if R_Success.Is_Set
                 and then not R_Success.Value
               then
                  begin
                     Request.On_Error_Message (Message);
                  exception
                     when E : others =>
                        Trace (Me, E);
                  end;

               else
                  declare
                     use type GPS.Kernel.Kernel_Handle;
                  begin
                     if Request.Kernel = null
                       or else not Request.Kernel.Is_In_Destruction
                     then
                        Request.On_Result_Message (Stream'Access, New_Request);

                        GPS.Kernel.Hooks.Dap_Response_Processed_Hook.Run
                          (Kernel   => Request.Kernel,
                           Method   => Request.Method);
                     end if;

                  exception
                     when E : others =>
                        Trace (Me, E);
                  end;
               end if;

               DAP.Requests.Destroy (Request);
            end if;
         end if;

         if New_Request /= null then
            Self.Process (New_Request);
         end if;

      elsif A_Type = "event" then
         Self.Process_Event (Stream'Access, Event);
      end if;
   end On_Raw_Message;

   -------------------
   -- Process_Event --
   -------------------

   procedure Process_Event
     (Self   : in out DAP_Client;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class;
      Event  : VSS.Strings.Virtual_String)
   is
      use VSS.Strings;
   begin
      if Event = "output" then
         declare
            output : DAP.Tools.OutputEvent;
         begin
            DAP.Tools.OutputEvent'Read (Stream, output);
            if output.body_category = "console" then
               Self.Kernel.Get_Messages_Window.Insert_Text
                 ("Debugger adapter" & Self.Id'Img & ":"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));

            elsif output.body_category = "important" then
               Self.Kernel.Get_Messages_Window.Insert_Error
                 ("Dbg adapter" & Self.Id'Img & " [important]:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));

               Trace (Me, "Dbg adapter [important]:" &
                        VSS.Strings.Conversions.To_UTF_8_String
                        (output.body_output));

            elsif output.body_category = "stdout" then
               --  Adapter writes debuggee output in a log file
               null;

            elsif output.body_category = "stderr" then
               Self.Kernel.Get_Messages_Window.Insert_Error
                 ("Debugger" & Self.Id'Img & " [stderr]:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));
               Trace (Me, "Debugger" & Self.Id'Img & " [stderr]:" &
                        VSS.Strings.Conversions.To_UTF_8_String
                        (output.body_output));
            end if;
         end;

      elsif Event = "initialized" then
         Self.Set_Status (Initialized);

      elsif Event = "stopped" then
         declare
            stop : DAP.Tools.StoppedEvent;
         begin
            DAP.Tools.StoppedEvent'Read (Stream, stop);
            Self.Set_Status (Stopped);

            if stop.body_reason = "breakpoint" then
               Self.Breakpoints.Stopped
                 (stop, Self.Stopped_File, Self.Stopped_Line);

               if Self.Stopped_Line = 0 then
                  declare
                     Req : StackTrace_Request_Access :=
                       new StackTrace_Request (Self.Kernel);
                  begin
                     Req.Client := Self.This;
                     Req.Parameters.arguments.threadId := stop.body_threadId;
                     Self.Enqueue (DAP.Requests.DAP_Request_Access (Req));
                  end;
               end if;

            else
               Self.Kernel.Get_Messages_Window.Insert_Error
                 ("Debugger" & Self.Id'Img & " stopped:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (stop.body_reason));

               Trace (Me, "Debugger" & Self.Id'Img & " stopped:" &
                        VSS.Strings.Conversions.To_UTF_8_String
                        (stop.body_reason));
            end if;
         end;

      else
         Self.Kernel.Get_Messages_Window.Insert_Error
           ("Event:" & VSS.Strings.Conversions.To_UTF_8_String (Event));

         Trace (Me, "Event:" &
                  VSS.Strings.Conversions.To_UTF_8_String (Event));
      end if;
   end Process_Event;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Result      : DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use GNATCOLL.VFS;
      pragma Unreferenced (New_Request);
      use type Generic_Views.Abstract_View_Access;

      Frame : DAP.Tools.StackFrame;
   begin
      Frame := Result.body_stackFrames.First_Element;

      Self.Client.Stopped_File := Create
        (+(VSS.Strings.Conversions.To_UTF_8_String (Frame.a_source.path)));
      Self.Client.Stopped_Line := Integer (Frame.line);

      Self.Client.Breakpoints.On_Location_Changed
        (Self.Client.Stopped_File, Self.Client.Stopped_Line);

      if Self.Client.Breakpoints_View /= null then
         DAP.Views.View_Access
           (Self.Client.Breakpoints_View).On_Location_Changed
           (Self.Client.Stopped_File, Self.Client.Stopped_Line);
      end if;
   end On_Result_Message;

   ----------------
   -- On_Started --
   ----------------

   overriding procedure On_Started (Self : in out DAP_Client) is
      Init : DAP.Requests.Initialize.Initialize_DAP_Request_Access :=
        new DAP.Requests.Initialize.Initialize_DAP_Request (Self.Kernel);
   begin
      Init.Initialize
        (Self.Project, Self.File,
         Ada.Strings.Unbounded.To_String (Self.Args));

      Self.Process (DAP.Requests.DAP_Request_Access (Init));
   end On_Started;

   -------------------
   -- On_Terminated --
   -------------------

   procedure On_Terminated (Self : in out DAP_Client) is
   begin
      Self.Stop;
   end On_Terminated;

   -------------
   -- Process --
   -------------

   procedure Process
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access)
   is
      Id     : constant LSP.Types.LSP_Number :=
        Self.Get_Request_ID;
      Stream : aliased LSP.JSON_Streams.JSON_Stream;
      Output : aliased VSS.Text_Streams.Memory_UTF8_Output.
        Memory_UTF8_Output_Stream;

   begin
      if Self.Status /= Terminating then
         Request.Set_Seq (Id);
         Request.Set_Client (Self.This);
         Stream.Set_Stream (Output'Unchecked_Access);
         Request.Write (Stream'Access);
         Stream.End_Document;

         --  Send request's message

         Self.Send_Buffer (Output.Buffer);

         --  Add request to the map

         Self.Sent.Insert (Id, Request);

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;
   end Process;

   -------------------------
   -- Reject_All_Requests --
   -------------------------

   procedure Reject_All_Requests (Self : in out DAP_Client) is
   begin
      --  Reject all queued requests. Clean commands queue.

      for Request of Self.Sent loop
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end loop;

      Self.Sent.Clear;
   end Reject_All_Requests;

   -----------
   -- Start --
   -----------

   procedure Start
     (Self    : in out DAP_Client;
      Adapter : String;
      Project : GNATCOLL.Projects.Project_Type;
      File    : GNATCOLL.VFS.Virtual_File;
      Args    : String)
   is
      Node_Args : Spawn.String_Vectors.UTF_8_String_Vector;

   begin
      Trace
        (Me, "Launching the debug adapter: " & Node_Binary & " " & Adapter);

      Self.Project := Project;
      Self.File    := File;
      Self.Args    := Ada.Strings.Unbounded.To_Unbounded_String (Args);

      Self.Set_Program (Node_Binary);
      Node_Args.Append (Adapter);
      Self.Set_Arguments (Node_Args);
      Self.Start;
   end Start;

   ----------
   -- Quit --
   ----------

   procedure Quit (Self : in out DAP_Client) is
      Disconnect : DAP.Requests.Disconnects.
        Disconnect_DAP_Request_Access := new DAP.Requests.Disconnects.
          Disconnect_DAP_Request (Self.Kernel);
   begin
      if Self.Status /= Terminating then
         Self.Process (DAP.Requests.DAP_Request_Access (Disconnect));
         Self.Set_Status (Terminating);
      end if;
   end Quit;

end DAP.Clients;
