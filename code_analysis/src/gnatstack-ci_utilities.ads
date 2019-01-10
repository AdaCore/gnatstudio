------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with GNATStack.Data_Model;

package GNATStack.CI_Utilities is

   procedure Write
     (File_Name : String;
      Data      :
        GNATStack.Data_Model.Subprogram_Information_Ordered_Sets.Set);
   --  Writes 'nodes' into the specified CI file.

   procedure Merge
     (Data      : in out GNATStack.Data_Model.Analysis_Information;
      File_Name : String);
   --  Reads 'nodes' from the specified CI file and merges them with existing
   --  information by adding new nodes.

end GNATStack.CI_Utilities;
