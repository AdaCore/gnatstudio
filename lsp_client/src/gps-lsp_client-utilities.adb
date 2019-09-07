------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with URIs;

package body GPS.LSP_Client.Utilities is

   ------------
   -- To_URI --
   ------------

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri is
   begin
      return LSP.Types.To_LSP_String
        (URIs.Conversions.From_File (Item.Display_Full_Name));
   end To_URI;

   ---------------------
   -- To_Virtual_File --
   ---------------------

   function To_Virtual_File
     (Item : LSP.Messages.DocumentUri) return GNATCOLL.VFS.Virtual_File
   is
      File : constant String :=
        URIs.Conversions.To_File (LSP.Types.To_UTF_8_String (Item));
   begin
      --  Call Create_From_UTF8 to guess the proper filesystem encoding
      --  from the UTF8 string returned by the protocol.
      return GNATCOLL.VFS.Create_From_UTF8 (File);
   end To_Virtual_File;

   -------------------------------------
   -- UTF_16_Offset_To_Visible_Column --
   -------------------------------------

   function UTF_16_Offset_To_Visible_Column
     (Item : LSP.Types.UTF_16_Index) return Basic_Types.Visible_Column_Type
   is
      use type LSP.Types.UTF_16_Index;

   begin
      return Basic_Types.Visible_Column_Type (Item + 1);
      --  ??? Dummy implementation
   end UTF_16_Offset_To_Visible_Column;

   -------------------------------------
   -- Visible_Column_To_UTF_16_Offset --
   -------------------------------------

   function Visible_Column_To_UTF_16_Offset
     (Item : Basic_Types.Visible_Column_Type) return LSP.Types.UTF_16_Index
   is
      use type Basic_Types.Visible_Column_Type;

   begin
      return LSP.Types.UTF_16_Index (Item - 1);
   end Visible_Column_To_UTF_16_Offset;

end GPS.LSP_Client.Utilities;
