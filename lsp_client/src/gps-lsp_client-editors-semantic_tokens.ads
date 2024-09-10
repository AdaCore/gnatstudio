------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

--  Integration of LSP semantic tokens capabilities with GNAT Studio's source
--  editor.

with GPS.Kernel;               use GPS.Kernel;
with LSP.Messages;             use LSP.Messages;

package GPS.LSP_Client.Editors.Semantic_Tokens is

   procedure Register (Kernel : Kernel_Handle);
   --  Register requests

   function Get_Supported_Options
     return LSP.Messages.Optional_SemanticTokensClientCapabilities;
   --  Returns supported by client options

end GPS.LSP_Client.Editors.Semantic_Tokens;
