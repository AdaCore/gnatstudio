------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with VSS.Strings.Conversions;

with GPS.Kernel;
with DAP.Views.Call_Stack;
with DAP.Utils;               use DAP.Utils;

package body DAP.Clients.Stack_Trace.StackTrace is

   Me : constant Trace_Handle := Create
     ("GPS.DAP.STACKTRACE_REQUEST_IMPL", On);

   ------------
   -- Create --
   ------------

   function Create
     (Client : access DAP.Clients.DAP_Client'Class;
      From   : Integer := 0;
      Limit  : Integer := 0)
      return StackTrace_Request_Access
   is
      Self : constant StackTrace_Request_Access :=
        new StackTrace_Request (GPS.Kernel.Kernel_Handle (Client.Kernel));
   begin
      Self.Parameters.arguments.threadId := Client.Get_Current_Thread;
      if Limit /= 0 then
         Self.Parameters.arguments.startFrame := (True, From);
         Self.Parameters.arguments.levels     := (True, Limit);
      end if;

      return Self;
   end Create;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out StackTrace_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.StackTraceResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      use GNATCOLL.VFS;
      use DAP.Tools;

   begin
      New_Request := null;

      if Result.a_body.totalFrames.Is_Set then
         Client.Get_Stack_Trace.Total_Count :=
           Result.a_body.totalFrames.Value;

      elsif Length (Result.a_body.stackFrames) = 0 then
         --  We do not have more frames, set Total_Count > 0 to
         --  disable sending more requests.
         Client.Get_Stack_Trace.Total_Count := 1;
      end if;

      for Index in 1 .. Length (Result.a_body.stackFrames) loop
         declare
            Frame : constant StackFrame_Variable_Reference :=
              Get_StackFrame_Variable_Reference
                (Result.a_body.stackFrames, Index);
            Bt : Frame_Record;
         begin
            Bt.Id := Frame.id;
            Bt.Name     := VSS.Strings.Conversions.
              To_Unbounded_UTF_8_String (Frame.name);

            if not Frame.instructionPointerReference.Is_Empty then
               Bt.Address := String_To_Address
                 (UTF8 (Frame.instructionPointerReference));
            end if;

            if Frame.source.Is_Set then
               Bt.File := To_File (Frame.source.Value.path);
               Bt.Line := Frame.line;
            end if;

            if Client.Get_Stack_Trace.Frames.Is_Empty then
               Client.Get_Stack_Trace.Selected_Frame := Bt;

               DAP.Utils.Highlight_Current_File_And_Line
                 (Client.Kernel, Bt.File, Bt.Line);
            end if;

            Client.Get_Stack_Trace.Frames.Append (Bt);
         end;
      end loop;

      Self.On_Response (Client, True);

   exception
      when E : others =>
         Trace (Me, E);
         Self.On_Response (Client, True);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out StackTrace_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      DAP.Requests.StackTrace.StackTrace_DAP_Request
        (Self).On_Error_Message (Client, Message);

      Self.On_Response (Client, False);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out StackTrace_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      DAP.Requests.StackTrace.StackTrace_DAP_Request
        (Self).On_Rejected (Client);

      Self.On_Response (Client, False);
   end On_Rejected;

   -----------------
   -- On_Response --
   -----------------

   procedure On_Response
     (Self       : in out StackTrace_Request;
      Client     : not null access DAP.Clients.DAP_Client'Class;
      Has_Result : Boolean) is
   begin
      if Client.Get_Status = Running then
         --  We uploaded the frames so can set the status to stopped to
         --  start updating the views.
         Client.Set_Status (Stopped);

      elsif Has_Result and then Client.Get_Status = Stopped then
         --  We uploaded the next part of the frames, update the view.
         DAP.Views.Call_Stack.Update (Self.Kernel);

         --  Update context to refresh actions' availability,
         --  mostly for "debug callstack fetch"
         Self.Kernel.Context_Changed (GPS.Kernel.No_Context);
      end if;
   end On_Response;

end DAP.Clients.Stack_Trace.StackTrace;
