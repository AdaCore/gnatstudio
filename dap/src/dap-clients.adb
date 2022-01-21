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

with GNATCOLL.Traces;       use GNATCOLL.Traces;
with Spawn.String_Vectors;

with VSS.JSON.Pull_Readers.Simple;
with VSS.Stream_Element_Vectors.Conversions;
with VSS.Strings.Conversions;
with VSS.Text_Streams.Memory_UTF8_Input;
with VSS.Text_Streams.Memory_UTF8_Output;

with DAP.Module;
with DAP.Requests.Initialize;
with DAP.Requests.Disconnects;
with DAP.Tools;

package body DAP.Clients is

   Me : constant Trace_Handle := Create ("DAP.Clients", On);

   Node_Binary : constant String := "/usr/bin/node";

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Self    : in out DAP_Client;
      Request : in out DAP.Requests.DAP_Request_Access) is
   begin
      if Self.Is_Ready then
         if Self.Queue.Is_Empty then
            Self.Process (Request);

         else
            Self.Queue.Append (Request);
         end if;

      else
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end if;

      Request := null;
   end Enqueue;

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
      Self.Is_Ready := True;
   end On_Configured;

   -----------------
   -- On_Finished --
   -----------------

   overriding procedure On_Finished (Self : in out DAP_Client) is
   begin
      Self.Is_Ready := False;
      Self.Reject_All_Requests;
      DAP.Module.Finished (Self.Id);
   end On_Finished;

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
                 ("Debugger" & Self.Id'Img & ":"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));

            elsif output.body_category = "important" then
               Self.Kernel.Get_Messages_Window.Insert_Text
                 ("Debugger" & Self.Id'Img & " [important]:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));

               Trace (Me, "Event [important]:" &
                        VSS.Strings.Conversions.To_UTF_8_String
                        (output.body_output));

               --  debuggee output
            elsif output.body_category = "stdout" then
               Self.Kernel.Get_Messages_Window.Insert_Text
                 ("Debugger" & Self.Id'Img & "[stdout]:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));

            elsif output.body_category = "stderr" then
               Self.Kernel.Get_Messages_Window.Insert_Text
                 ("Debugger" & Self.Id'Img & "[stderr]:"  &
                    VSS.Strings.Conversions.To_UTF_8_String
                    (output.body_output));

            end if;
         end;
      else
         Trace (Me, "Event:" &
                  VSS.Strings.Conversions.To_UTF_8_String (Event));
      end if;
   end Process_Event;

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
      if not Self.Is_Terminating then
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
      --  Reject all ongoing requests, results will be never received. Clean
      --  ongoing requests map.

      for Request of Self.Queue loop
         Request.On_Rejected;
         DAP.Requests.Destroy (Request);
      end loop;

      Self.Queue.Clear;

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
      if not Self.Is_Terminating then
         Self.Process (DAP.Requests.DAP_Request_Access (Disconnect));
         Self.Is_Terminating := True;
      end if;
   end Quit;

end DAP.Clients;
