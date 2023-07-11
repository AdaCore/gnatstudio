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

package body DAP.Modules.Breakpoint_Managers.Exceptions is

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Exception_Breakpoint_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Exception_Breakpoints.On_Error_Message
        (DAP.Requests.Exception_Breakpoints.
           Exception_Breakpoints_DAP_Request (Self), Message);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out Exception_Breakpoint_Request) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Exception_Breakpoints.On_Rejected
        (DAP.Requests.Exception_Breakpoints.
           Exception_Breakpoints_DAP_Request (Self));
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Exception_Breakpoint_Request;
      Result      : in out DAP.Tools.SetExceptionBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Actual  : Breakpoint_Vectors.Vector;
      Data    : Breakpoint_Data;
      Enabled : Breakpoint_Vectors.Vector;
   begin
      New_Request := null;

      case Self.Action is
         when Delete | Disable | Synch =>
            --  Do nothing
            null;

         when Init =>
            for Data of Self.Sent loop
               Self.Manager.Send_Commands (Data);
            end loop;

         when Add =>
            if not Self.Sent.Is_Empty
              and then Result.a_body.Is_Set
            then
               Data := Self.Sent.Last_Element;
               Data.Locations := Convert
                 (Self.Kernel,
                  Result.a_body.Value.breakpoints
                    (Length (Result.a_body.Value.breakpoints))).Locations;
               Data.Num := Data.Locations.First_Element.Num;

               Self.Manager.Holder.Add_BP_From_Response (Data);
               Self.Manager.Send_Commands (Data);
            end if;

         when Enable =>
            if Result.a_body.Is_Set then
               for Index in 1 .. Length (Result.a_body.Value.breakpoints) loop
                  Actual.Append
                    (Convert
                       (Self.Kernel, Result.a_body.Value.breakpoints (Index)));
               end loop;

               Self.Manager.Holder.Status_Changed
                 (On_Exception, Actual, Enabled);
               for Data of Enabled loop
                  Self.Manager.Send_Commands (Data);
               end loop;
            end if;
      end case;

      Self.Manager.Show_Breakpoints;
      GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
        (Self.Kernel, Self.Manager.Client.Get_Visual);

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

end DAP.Modules.Breakpoint_Managers.Exceptions;
