------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

package Src_Editor_Buffer.Blocks is

   procedure Compute_Blocks
     (Buffer    : access Source_Buffer_Record'Class;
      Immediate : Boolean);
   --  Fill the buffer information with the data necessary to handle block
   --  folding.
   --  If Immediate is True, do the computing immediately. Otherwise, do it
   --  only if the semantic tree for this buffer is ready.

   procedure Calculate_Screen_Offset
     (Buffer : access Source_Buffer_Record'Class;
      Block  : in out Block_Record);
   --  Return the screen position, after TAB expansion, of the block

end Src_Editor_Buffer.Blocks;
