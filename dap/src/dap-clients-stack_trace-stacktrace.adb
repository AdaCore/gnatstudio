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
      pragma Unreferenced (Client);
      Start_Frame : constant Natural :=
        (if Self.Parameters.arguments.startFrame.Is_Set then
            Self.Parameters.arguments.startFrame.Value
         else 0);
      Thread_Id : constant Integer := Self.Parameters.arguments.threadId;
      Append    : Boolean := False;
   begin
      New_Request := null;

      Self.Callbacks.On_Stacktrace_Frames
        (Thread_Id   => Thread_Id,
         Start_Frame => Start_Frame,
         Response    => Result,
         Append_More => Append);

      Self.Callbacks.On_Stacktrace_Fetch_Complete
        (Thread_Id     => Thread_Id,
         Success       => True,
         Initial_Fetch => Start_Frame = 0);

   exception
      when E : others =>
         Trace (Me, E);
         Self.Callbacks.On_Stacktrace_Fetch_Complete
           (Thread_Id     => Thread_Id,
            Success       => False,
            Initial_Fetch => Start_Frame = 0);
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

      Self.Callbacks.On_Stacktrace_Fetch_Complete
        (Thread_Id     => Self.Parameters.arguments.threadId,
         Success       => False,
         Initial_Fetch => not Self.Parameters.arguments.startFrame.Is_Set
           or else Self.Parameters.arguments.startFrame.Value = 0);
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

      Self.Callbacks.On_Stacktrace_Fetch_Complete
        (Thread_Id     => Self.Parameters.arguments.threadId,
         Success       => False,
         Initial_Fetch => not Self.Parameters.arguments.startFrame.Is_Set
           or else Self.Parameters.arguments.startFrame.Value = 0);
   end On_Rejected;

end DAP.Clients.Stack_Trace.StackTrace;
