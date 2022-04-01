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

--  "continue" request and Debugger/Continue action

with DAP.Tools;

package DAP.Requests.Continue is

   -- Continue_DAP_Request --

   type Continue_DAP_Request is new DAP_Request with record
      Parameters : aliased DAP.Tools.ContinueRequest :=
        DAP.Tools.ContinueRequest'
          (seq       => 0,
           a_type    => "request",
           command   => "continue",
           arguments => (threadId => 0));
   end record;

   type Continue_DAP_Request_Access is access all Continue_DAP_Request;

   overriding procedure Write
     (Self   : Continue_DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Result_Message
     (Self        : in out Continue_DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access);

   procedure On_Result_Message
     (Self        : in out Continue_DAP_Request;
      Result      : DAP.Tools.ContinueResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure Set_Seq
     (Self : in out Continue_DAP_Request;
      Id   : LSP.Types.LSP_Number);

   overriding function Method
     (Self : in out Continue_DAP_Request)
      return String is ("continue");

end DAP.Requests.Continue;
