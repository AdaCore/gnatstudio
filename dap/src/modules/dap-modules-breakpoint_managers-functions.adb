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

with GPS.Kernel.Hooks;
with DAP.Clients;

package body DAP.Modules.Breakpoint_Managers.Functions is

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Function_Breakpoint_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Function_Breakpoints.On_Error_Message
        (DAP.Requests.Function_Breakpoints.
           Function_Breakpoint_DAP_Request (Self), Client, Message);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Function_Breakpoint_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Function_Breakpoints.On_Rejected
        (DAP.Requests.Function_Breakpoints.
           Function_Breakpoint_DAP_Request (Self), Client);
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Function_Breakpoint_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetFunctionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Update  : Boolean := False;
      Actual  : Breakpoint_Vectors.Vector;
      Enabled : Breakpoint_Vectors.Vector;
      Num     : Breakpoint_Identifier;
   begin
      New_Request := null;

      case Self.Action is
         when Init =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Initialized_For_Subprograms
              (Actual, Self.Last);

            if Self.Last then
               for Data of Actual loop
                  Self.Manager.Send_Commands (Data);
               end loop;
            end if;

         when Add =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Added_Subprogram
              (Self.Sent.Last_Element, Actual, Num);

            Self.Manager.Send_Commands (Self.Sent.Last_Element);

            GPS.Kernel.Hooks.Debugger_Breakpoint_Added_Hook.Run
              (Kernel   => Self.Kernel,
               Debugger => Client.Get_Visual,
               Id       => Integer (Num));

            Update := True;

         when Delete =>
            --  Do nothing because we delete breakpoints by notification
            null;

         when Enable =>
            for Index in 1 .. Length (Result.a_body.breakpoints) loop
               Actual.Append
                 (Convert (Self.Kernel, Result.a_body.breakpoints (Index)));
            end loop;

            Self.Manager.Holder.Subprogram_Status_Changed
              (Actual, Self.Last, Enabled);

            for Data of Enabled loop
               Self.Manager.Send_Commands (Data);
            end loop;

            Update := Self.Last;

         when Disable =>
            --  Do nothing because we delete BP by notifications
            Update := True;

         when Sync =>
            --  Do nothing because we already have all data
            null;
      end case;

      if Update then
         Self.Manager.Show_Breakpoints;
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Manager.Client.Get_Visual);
      end if;

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

end DAP.Modules.Breakpoint_Managers.Functions;
