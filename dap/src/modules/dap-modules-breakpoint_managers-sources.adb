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

package body DAP.Modules.Breakpoint_Managers.Sources is

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Source_Line_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Breakpoints.On_Error_Message
        (DAP.Requests.Breakpoints.Breakpoint_DAP_Request (Self), Message);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Source_Line_Request) is
   begin
      Self.Manager.Dec_Response (Self.Action);
      DAP.Requests.Breakpoints.On_Rejected
        (DAP.Requests.Breakpoints.Breakpoint_DAP_Request (Self));
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Source_Line_Request;
      Result      : in out DAP.Tools.SetBreakpointsResponse;
      New_Request : in out DAP_Request_Access)
   is
      use DAP.Tools;

      Holder  : constant GPS.Editors.Controlled_Editor_Buffer_Holder'Class :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder (File => Self.File);
      Data    : Breakpoint_Data;
      Actual  : Breakpoint_Vectors.Vector;
      Update  : Boolean := False;
      Warning : Boolean := False;

   begin
      New_Request := null;

      case Self.Action is
         when Init =>
            declare
               Changed : Breakpoint_Hash_Maps.Map;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Data := Self.Sent.Element (Index);
                  Convert (Self.Kernel, Self.File, Holder, Data,
                           Result.a_body.breakpoints (Index));
                  Actual.Append (Data);

                  if not Result.a_body.breakpoints (Index).verified then
                     Warning := True;
                  end if;
               end loop;

               if Warning then
                  Self.Manager.Client.Display_In_Debugger_Console
                    ("Some breakpoints set graphically are not "
                     & "recognized by the debugger and, thus, will be lost "
                     & "when running it. "
                     & ASCII.LF
                     & "This can happen when the executable "
                     & "being debugged has not been compiled with the debug "
                     & "flags or when the breakpoint's source file is not"
                     & " found in the symbols table. This also can happen for "
                     & "catchpoints."
                     & ASCII.LF
                     & "You should try to set them after a start command."
                     & ASCII.LF
                     & "Breakpoints and/or catchpoints that could not be set: "
                     & ASCII.LF
                     & ASCII.LF);
               end if;

               --  Update breakpoints data like numbers and locations
               Self.Manager.Holder.Initialized_For_File
                 (Self.File, Actual, Changed);

               if not Changed.Is_Empty then
                  for Data of Changed.Element (Self.File) loop
                     Self.Manager.Send_Commands (Data);
                  end loop;

                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed.Element (Self.File), Sync);
               else
                  for Data of Actual loop
                     Self.Manager.Send_Commands (Data);
                  end loop;
               end if;
            end;
            Update := True;

         when Add =>
            declare
               Changed : Breakpoint_Vectors.Vector;
            begin
               Data := Self.Sent.Last_Element;
               Convert
                 (Self.Kernel, Self.File, Holder, Data,
                  Result.a_body.breakpoints
                    (Length (Result.a_body.breakpoints)));

               --  Update the breakpoint data because in notifications we
               --  don't have full information like disposition and so on
               Self.Manager.Holder.Added
                 (Data => Data, Changed => Changed, Check => True);

               if not Changed.Is_Empty then
                  if Changed.Contains (Data) then
                     Self.Manager.Send_Commands (Data);
                  end if;

                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed, Sync);
               else
                  Update := True;
                  Self.Manager.Send_Commands (Data);

                  GPS.Kernel.Hooks.Debugger_Breakpoint_Added_Hook.Run
                    (Kernel   => Self.Kernel,
                     Debugger => Self.Get_Client.Get_Visual,
                     Id       => Integer (Data.Num));
               end if;
            end;

         when Delete =>
            --  Do nothing because we delete breakpoints by notification
            null;

         when Enable =>
            declare
               Changed : Breakpoint_Hash_Maps.Map;
               Id      : Integer;
               Enabled : Breakpoint_Vectors.Vector;
            begin
               for Index in 1 .. Length (Result.a_body.breakpoints) loop
                  Data := Self.Sent.Element (Index);
                  Convert (Self.Kernel, Self.File, Holder, Data,
                           Result.a_body.breakpoints (Index));
                  Actual.Append (Data);
               end loop;

               Self.Manager.Holder.Status_Changed
                 (Self.File, Actual, Changed, Enabled, Id);

               for Data of Enabled loop
                  Self.Manager.Send_Commands (Data);
               end loop;

               if not Changed.Is_Empty then
                  New_Request := Self.Manager.Send_Line
                    (Self.File, Changed.Element (Self.File), Sync);

               elsif Id > 0 then
                  GPS.Kernel.Hooks.Debugger_Breakpoint_Changed_Hook.Run
                    (Self.Kernel, Self.Manager.Client.Get_Visual, Id);
                  Self.Manager.Show_Breakpoints;

               else
                  Update := True;
               end if;
            end;

         when Disable =>
            --  Do nothing because we delete BP by notifications
            Update := True;

         when Sync =>
            --  Do nothing because we already have all data
            null;
      end case;

      --  Update visual representation
      if Update then
         GPS.Kernel.Hooks.Debugger_Breakpoints_Changed_Hook.Run
           (Self.Kernel, Self.Manager.Client.Get_Visual);
         Self.Manager.Show_Breakpoints;
      end if;

      Self.Manager.Dec_Response (Self.Action);
   end On_Result_Message;

end DAP.Modules.Breakpoint_Managers.Sources;
