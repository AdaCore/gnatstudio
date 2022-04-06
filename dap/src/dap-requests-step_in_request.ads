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

--  "stepIn" request

with DAP.Tools;

package DAP.Requests.Step_In_Request is

   -- Step_In_DAP_Request --

   type Step_In_DAP_Request is new DAP_Request with record
      Parameters : aliased DAP.Tools.StepInRequest :=
        DAP.Tools.StepInRequest'
          (seq       => 0,
           a_type    => "request",
           command   => "stepIn",
           arguments => (granularity => DAP.Tools.Enums.line,
                         targetId    => 0,
                         threadId    => 0));
   end record;

   type Step_In_DAP_Request_Access is access all Step_In_DAP_Request;

   overriding procedure Write
     (Self   : Step_In_DAP_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Result_Message
     (Self        : in out Step_In_DAP_Request;
      Stream      : not null access LSP.JSON_Streams.JSON_Stream'Class;
      New_Request : in out DAP_Request_Access);

   procedure On_Result_Message
     (Self        : in out Step_In_DAP_Request;
      Result      : DAP.Tools.StepInResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure Set_Seq
     (Self : in out Step_In_DAP_Request;
      Id   : LSP.Types.LSP_Number);

   overriding function Method
     (Self : in out Step_In_DAP_Request)
      return String is ("stepIn");

end DAP.Requests.Step_In_Request;
