-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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
