------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2024-2026, AdaCore                   --
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

--  Interface to handle partial results of the LSP request.

with LSP.JSON_Streams;
with LSP.Types;

package GPS.LSP_Client.Partial_Results is

   type LSP_Request_Partial_Result is limited interface;

   procedure On_Partial_Result_Message
     (Self   : in out LSP_Request_Partial_Result;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is null;
   --  Called when partial result is received from the server.

   procedure Set_Partial_Result_Token
     (Self : in out LSP_Request_Partial_Result;
      To   : LSP.Types.ProgressToken) is abstract;
   --  Set token to return partial requests. Request must store it and return
   --  by Partial_Result_Token function.

   function Partial_Result_Token
     (Self : LSP_Request_Partial_Result)
      return LSP.Types.ProgressToken is abstract;
   --  Returns partial result token allocated for the given request.

end GPS.LSP_Client.Partial_Results;
