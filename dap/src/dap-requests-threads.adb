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

with DAP.Tools.Inputs;
with DAP.Tools.Outputs;

package body DAP.Requests.Threads is

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Threads_DAP_Request;
      Stream : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      DAP.Tools.Outputs.Output_ThreadsRequest (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Threads_DAP_Request;
      Stream      : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.ThreadsResponse;
      Success  : Boolean := True;
   begin
      DAP.Tools.Inputs.Input_ThreadsResponse (Stream, Response, Success);
      if Success then
         Threads_DAP_Request'Class
           (Self).On_Result_Message (Response, New_Request);
      end if;
   end On_Result_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Threads_DAP_Request;
      Id   : Integer) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

end DAP.Requests.Threads;
