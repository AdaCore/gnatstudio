-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glide_Kernel;   use Glide_Kernel;

package Log_Utils is

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class);
   --  This subprogram must be called before calling any subprogram
   --  from this package.

   function Get_Log_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) return String;
   --  Return the absolute name for the log file corresponding to
   --  File_Name.

   function Get_File_From_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Log_Name : String) return String;
   --  Return the absolute name for the file corresponding to
   --  Log_Name;

   function Get_Log
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) return String;
   --   Return the log for the given file.

   procedure Remove_File_From_Mapping
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String);
   --  Remove the entry File_Name from the logs mapping,
   --  if such an entry exists.

end Log_Utils;
