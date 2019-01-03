------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

--  This package contains the hooks that are used by the editors

with GPS.Kernel; use GPS.Kernel;

package Src_Editor_Buffer.Hooks is

   procedure Location_Changed (Buffer : Source_Buffer);
   --  Emit the hook Cursor_Stopped_Hook

   procedure Word_Added (Buffer : Source_Buffer);
   --  Emit the hook Word_Added_Hook

   procedure Character_Added
     (Buffer      : Source_Buffer;
      Character   : Gunichar;
      Interactive : Boolean);
   --  Emit the Character_Added_Hook. Character should be 8 (control-H) when a
   --  character was removed from the buffer.
   --  Interactive indicates whether the character is a result of user
   --  interaction.

   procedure Buffer_Modified (Buffer : Source_Buffer);
   --  Emit the Buffer_Modified hook

end Src_Editor_Buffer.Hooks;
