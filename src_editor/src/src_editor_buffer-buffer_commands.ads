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

--  This package handles commands related to the editor buffer, such
--  as completion commands or delimiter jump commands.

with Commands;
with Commands.Interactive; use Commands.Interactive;

package Src_Editor_Buffer.Buffer_Commands is

   type Jump_To_Delimiter_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Jump_To_Delimiter_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Jump to the next delimiter for the one currently under the cursor.

end Src_Editor_Buffer.Buffer_Commands;
