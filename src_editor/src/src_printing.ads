-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides printing for Source_Buffers.
--  Currently only implemented under Windows, provides a no-op under
--  unix.

with Src_Editor_Box; use Src_Editor_Box;

package Src_Printing is

   procedure Print
     (Editor     : Source_Editor_Box;
      Font_Name  : String;
      Font_Size  : Positive;
      Bold       : Boolean := False;
      Italicized : Boolean := False);
   --  Print the contents of the buffer associated with the Source_Editor_Box
   --  using the indicated font.  Has no effect if the buffer is empty.
   --  Under Windows will ask user for printer selection via dialog box, the
   --  cancellation of which will return without printing.
   --  Font_Size is the size of the font, in points.

end Src_Printing;
