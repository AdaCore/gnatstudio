------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2026, AdaCore                  --
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

with GNATCOLL.JSON;
with GNATCOLL.VFS;

with Language;
with LSP.Structures;
with LSP.Enumerations;

with GPS.Editors;
with GPS.Kernel;

package GPS.LSP_Client.Utilities is

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Structures.DocumentUri;
   --  Converts Virtual_File to DocumentUri.

   function To_Virtual_File
     (Item : LSP.Structures.DocumentUri) return GNATCOLL.VFS.Virtual_File;
   --  Converts DocumentUri to Virtual_File.

   function LSP_Position_To_Location
     (Editor   : GPS.Editors.Editor_Buffer'Class;
      Position : LSP.Structures.Position)
      return GPS.Editors.Editor_Location'Class;
   --  Converts the given LSP position to an editor location.

   function Location_To_LSP_Position
     (Location : GPS.Editors.Editor_Location'Class)
      return LSP.Structures.Position;
   --  Converts the given editor location to a LSP position.

   function To_Language_Category
     (K : LSP.Enumerations.SymbolKind; Is_Procedure : Boolean := False)
      return Language.Language_Category;
   --  Converts SymbolKind to an appropriate Language_Category.
   --  Is_Procedure should be True if the Symbol is a function without a return
   --  statement (The LSP doesn't have the concept of Procedure).

   function To_Construct_Visibility
     (V : LSP.Enumerations.AlsVisibility) return Language.Construct_Visibility;
   --  Converts AlsVisibility to Construct_Visibility.

   function Get_Formatting_Options
     (Kernel : GPS.Kernel.Kernel_Handle; File : GNATCOLL.VFS.Virtual_File)
      return LSP.Structures.FormattingOptions;
   --  Return the formatting options used for LSP Formatting for Document

   function To_LSP_Any
     (Value : GNATCOLL.JSON.JSON_Value) return LSP.Structures.LSPAny;
   --  Converts a GNATCOLL.JSON value into the LSP "any" wire representation.

   function From_LSP_Any
     (Value : LSP.Structures.LSPAny) return GNATCOLL.JSON.JSON_Value;
   --  Converts the LSP "any" wire representation into a GNATCOLL.JSON value.

end GPS.LSP_Client.Utilities;
