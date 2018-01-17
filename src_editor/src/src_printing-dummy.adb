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

--  Dummy version of this file

pragma Warnings (Off);

package body Src_Printing.Win32_Printer is

   overriding procedure Print
     (This       : Printer;
      Editor     : Src_Editor_Box.Source_Editor_Box;
      From       : Editable_Line_Type := 1;
      To         : Editable_Line_Type := Editable_Line_Type'Last)
   is null;

   function Create return Printer is
      Dummy : Printer;
   begin
      return Dummy;
   end Create;

end Src_Printing.Win32_Printer;
