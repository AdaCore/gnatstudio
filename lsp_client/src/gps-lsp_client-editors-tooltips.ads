------------------------------------------------------------------------------
--                                  G P S                                   --
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

--  Integration with GPS's source editor tooltips

with Src_Editor_Box;           use Src_Editor_Box;
with Src_Editor_Box.Tooltips;  use Src_Editor_Box.Tooltips;

package GPS.LSP_Client.Editors.Tooltips is

   function Create_LSP_Client_Editor_Tooltip_Handler
     (Box : not null access Source_Editor_Box_Record'Class)
      return Editor_Tooltip_Handler_Access;
   --  LSP-based editor tooltips factory.

end GPS.LSP_Client.Editors.Tooltips;
