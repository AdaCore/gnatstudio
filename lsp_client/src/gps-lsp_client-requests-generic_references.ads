------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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
--  This generic package provides weak-reference for request for user
--  specified request type to simplify code.

generic
   type Request_Type is abstract new LSP_Request with private;

package GPS.LSP_Client.Requests.Generic_References is

   type Request_Access is access all Request_Type'Class;

   type Reference is tagged private;

   function To_Reference
     (Self : Reference) return GPS.LSP_Client.Requests.Reference;
   --  Convert request type specific reference to general reference.

   function To_Reference
     (Self : GPS.LSP_Client.Requests.Reference) return Reference;
   --  Convert general reference to request type specific reference.
   --  Return empty reference when type of request is not compatible.

   function Request (Self : Reference) return Request_Access;
   --  Return associated request object or null.

   function Has_Request (Self : Reference) return Boolean;
   --  Return True when associated request object is alive.

   function Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access) return Reference;
   --  Execute request using language server for the given language. Request
   --  parameter is set to null and reference is returned.

private

   type Reference is new Abstract_Reference with null record;

end GPS.LSP_Client.Requests.Generic_References;
