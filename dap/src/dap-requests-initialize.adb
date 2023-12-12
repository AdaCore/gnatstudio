------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with VSS.Strings.Conversions;

with DAP.Clients;
with DAP.Tools.Inputs;
with DAP.Tools.Outputs;
with DAP.Requests.Launch;

package body DAP.Requests.Initialize is

   Me : constant Trace_Handle := Create ("GPS.DAP.Requests_Initialize", On);

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Initialize_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_InitializeRequest (Stream, Self.Parameters);
   end Write;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Initialize_DAP_Request;
      Client : not null access DAP.Clients.DAP_Client'Class)
   is
   begin
      Trace (Me, "Rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Initialize_DAP_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Kernel.Get_Messages_Window.Insert_Error
        ("[Debug]:" &
           VSS.Strings.Conversions.To_UTF_8_String (Message));

      Trace (Me, VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Initialize_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      Success     : in out Boolean;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.InitializeResponse;
   begin
      DAP.Tools.Inputs.Input_InitializeResponse (Stream, Response, Success);

      if Success then
         Initialize_DAP_Request'Class
           (Self).On_Result_Message (Client, Response, New_Request);
      end if;
   end On_Result_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Initialize_DAP_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.InitializeResponse;
      New_Request : in out DAP_Request_Access)
   is

      Launch : constant DAP.Requests.Launch.Launch_DAP_Request_Access :=
        new DAP.Requests.Launch.Launch_DAP_Request (Self.Kernel);
   begin
      Client.Set_Capabilities (Result.a_body);
      Launch.Initialize (Client);
      New_Request := DAP_Request_Access (Launch);
   end On_Result_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Initialize_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.Initialize;
