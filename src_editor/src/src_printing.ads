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

--  This package provides printing for Source_Buffers.

with Basic_Types;         use Basic_Types;
with Src_Editor_Box;

package Src_Printing is

   type Abstract_Printer is abstract tagged null record;

   procedure Print
     (This       : Abstract_Printer;
      Editor     : Src_Editor_Box.Source_Editor_Box;
      From       : Editable_Line_Type := 1;
      To         : Editable_Line_Type := Editable_Line_Type'Last) is abstract;
   --  Print the contents of the buffer associated with the Source_Editor_Box
   --  using the indicated font.  Has no effect if the buffer is empty.
   --  Limit line range to From .. To if specified.
   --  Under Windows will ask user for printer selection via dialog box, the
   --  cancellation of which will return without printing.

end Src_Printing;
