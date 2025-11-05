------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2025, AdaCore                  --
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

--  GPS.LSP_Client.Utilities - Minimal Protocol Conversions
--
--  This is a MINIMAL subset of the original utilities, containing only
--  the 4 pure functions used by lsp_client_core (no GPS.Editors dependency).
--
--  Excluded from this minimal version:
--    - LSP_Position_To_Location (needs GPS.Editors.Editor_Buffer)
--    - Location_To_LSP_Position (needs GPS.Editors.Editor_Location)
--    - Get_Formatting_Options (needs GPS.Kernel.Preferences callback)
--
--  These will be reimplemented for TUI or added via Callbacks in Phase 5.

with GNATCOLL.VFS;
with Language;
with LSP.Messages;

package GPS.LSP_Client.Utilities is

   function To_URI
     (Item : GNATCOLL.VFS.Virtual_File) return LSP.Messages.DocumentUri;
   --  Converts Virtual_File to DocumentUri.

   function To_Virtual_File
     (Item : LSP.Messages.DocumentUri) return GNATCOLL.VFS.Virtual_File;
   --  Converts DocumentUri to Virtual_File.
   --  Always normalizes paths for security (hardcoded, was preference).

   function To_Language_Category
     (K            : LSP.Messages.SymbolKind;
      Is_Procedure : Boolean := False)
      return Language.Language_Category;
   --  Converts LSP SymbolKind to GNAT Studio Language_Category.

   function To_Construct_Visibility
     (V : LSP.Messages.Als_Visibility)
      return Language.Construct_Visibility;
   --  Converts ALS visibility to GNAT Studio Construct_Visibility.

end GPS.LSP_Client.Utilities;
