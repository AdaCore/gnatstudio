-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                             AdaCore                               --
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

--  This package is the part of the Docgen tool responsable for the
--  processing of the program structure information for the source file
--  list passed by the procedure Docgen.

with GPS.Kernel;
with Docgen.Backend;

package Docgen.Work_On_File is

   procedure Process_Files
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Source_File_List : in out Type_Source_File_Table.HTable;
      Nb_Files         : Natural;
      Options          : Docgen.All_Options);
   --  Process the Nb_Files files from Source_File_List, and generate their
   --  documentation.

end Docgen.Work_On_File;
