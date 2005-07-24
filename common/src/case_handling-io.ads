-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2005                      --
--                              AdaCore                              --
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

--  Support for saving/loading case exception files

package Case_Handling.IO is

   procedure Load_Exceptions
     (C         : in out Casing_Exceptions;
      Filename  : String;
      Read_Only : Boolean);
   --  Load case exceptions file and set the in memory container

   procedure Save_Exceptions (C : in Casing_Exceptions; Filename : String);
   --  Save the case exceptions container into Filename. The container still
   --  remains in memory. The read-only case exceptions are not saved.

end Case_Handling.IO;
