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

package body DAP.Modules.Breakpoint_Managers.SetExceptionBreakpoints is

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Exception_Breakpoint_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.SetExceptionBreakpoints.On_Error_Message
        (DAP.Requests.SetExceptionBreakpoints.
           Exception_Breakpoints_DAP_Request (Self), Client, Message);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Exception_Breakpoint_Request;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.SetExceptionBreakpoints.On_Rejected
        (DAP.Requests.SetExceptionBreakpoints.
           Exception_Breakpoints_DAP_Request (Self), Client);
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Exception_Breakpoint_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.SetExceptionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Actual  : Breakpoint_Vectors.Vector;
      Data    : Breakpoint_Data;
      Changed : Breakpoint_Vectors.Vector;
      Update  : Boolean;
   begin
      New_Request := null;

      case Self.Action is
         when Delete | Disable | Sync =>
            --  Do nothing
            null;

         when Init =>
            for Index in 1 .. Length (Result.a_body.Value.breakpoints) loop
               Data := Self.Sent.Element (Index);
               Convert
                 (Self.Kernel, Data, Result.a_body.Value.breakpoints (Index));
               Actual.Append (Data);
            end loop;

            Self.Manager.Done_For_Exceptions (Actual);

         when Add =>
            if not Self.Sent.Is_Empty
              and then Result.a_body.Is_Set
            then
               Data := Self.Sent.Last_Element;
               Convert
                 (Self.Kernel,
                  Data,
                  Result.a_body.Value.breakpoints
                    (Length (Result.a_body.Value.breakpoints)));

               Self.Manager.Holder.Added
                 (Data    => Data,
                  Changed => Changed,
                  Check   => False,
                  Update  => Update);

               if Update then
                  --  BP is not verified and should be deleted, update the list
                  New_Request := Self.Manager.Send_Exception (Changed, Sync);
               else
                  --  BP is added, set commands and trigger the hook
                  Self.Manager.Send_Commands (Data);

                  GPS.Kernel.Hooks.Debugger_Breakpoint_Added_Hook.Run
                    (Kernel   => Self.Kernel,
                     Debugger => Client.Get_Visual,
                     Id       => Integer (Data.Num));
               end if;
            end if;

         when Enable =>
            if Result.a_body.Is_Set then
               for Index in 1 .. Length (Result.a_body.Value.breakpoints) loop
                  Actual.Append
                    (Convert
                       (Self.Kernel, Result.a_body.Value.breakpoints (Index)));
               end loop;

               Self.Manager.Holder.Address_Exception_Status_Changed
                 (On_Exception, Actual, Changed, Update);

               if Update then
                  --  Update breakpoints after deleting (pending) bps
                  New_Request := Self.Manager.Send_Exception (Changed, Sync);
               end if;

               --  Set commands for the enabled bps
               Self.Manager.Send_Commands (Changed);
            end if;
      end case;

      Self.Manager.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Self.Kernel, Self.Manager.Client.Get_Visual);

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

end DAP.Modules.Breakpoint_Managers.SetExceptionBreakpoints;
