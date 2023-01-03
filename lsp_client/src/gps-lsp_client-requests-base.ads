------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2023, AdaCore                  --
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

--  Base types for requests with common set of members

package GPS.LSP_Client.Requests.Base is

   --  Text_Document_Request provides field to store associated
   --  virtual file, and implementation of Text_Document function.

   type Text_Document_Request is
     abstract new GPS.LSP_Client.Requests.LSP_Request with
      record
         File : GNATCOLL.VFS.Virtual_File;
      end record;

   overriding function Text_Document
     (Self : Text_Document_Request) return GNATCOLL.VFS.Virtual_File;

end GPS.LSP_Client.Requests.Base;
