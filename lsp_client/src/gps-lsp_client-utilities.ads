------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2021, AdaCore                  --
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
with Language;
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
   --  Converts visible column to UTF16 index.

   function To_Language_Category
     (K            : LSP.Messages.SymbolKind;
      Is_Procedure : Boolean := False)
      return Language.Language_Category;
   --  Converts SymbolKind to an appropriate Language_Category.
   --  Is_Procedure should be True if the Symbol is a function without a return
   --  statement (The LSP doesn't have the concept of Procedure).

   function To_Construct_Visibility
     (V : LSP.Messages.Als_Visibility)
      return Language.Construct_Visibility;
   --  Converts AlsVisibility to Construct_Visibility.

end GPS.LSP_Client.Utilities;
