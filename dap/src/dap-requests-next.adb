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

with DAP.Module;

package body DAP.Requests.Next is

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Self   : Next_DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      DAP.Tools.NextRequest'Write (Stream, Self.Parameters);
   end Write;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Next_DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access)
   is
      Response : DAP.Tools.NextResponse;
   begin
      DAP.Tools.NextResponse'Read (Stream, Response);
      Next_DAP_Request'Class
        (Self).On_Result_Message (Response, New_Request);
   end On_Result_Message;

   -------------
   -- Set_Seq --
   -------------

   overriding procedure Set_Seq
     (Self : in out Next_DAP_Request;
      Id   : LSP.Types.LSP_Number) is
   begin
      Self.Parameters.seq := Id;
   end Set_Seq;

   -----------------------
   -- On_Result_Message --
   -----------------------

   procedure On_Result_Message
     (Self        : in out Next_DAP_Request;
      Result      : DAP.Tools.NextResponse;
      New_Request : in out DAP_Request_Access) is
   begin
      New_Request := null;
      DAP.Module.Get_Current_Debugger.On_Continue;
   end On_Result_Message;

end DAP.Requests.Next;
