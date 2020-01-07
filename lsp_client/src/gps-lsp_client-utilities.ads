------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2020, AdaCore                  --
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

with GNATCOLL.VFS;

with Basic_Types;
with LSP.Messages;
with LSP.Types;

package GPS.LSP_Client.Utilities is

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri;
   --  Converts Virtual_File to DocumentUri.

   function To_Virtual_File
     (Item : LSP.Messages.DocumentUri) return GNATCOLL.VFS.Virtual_File;
   --  Converts DocumentUri to Virtual_File.

   function UTF_16_Offset_To_Visible_Column
     (Item : LSP.Types.UTF_16_Index) return Basic_Types.Visible_Column_Type;
   --  Converts UTF16 index to visible column type.

   function Visible_Column_To_UTF_16_Offset
     (Item : Basic_Types.Visible_Column_Type) return LSP.Types.UTF_16_Index;
   --  Converts visible column to UTF16 index

end GPS.LSP_Client.Utilities;
