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

--  Support for saving/loading case exception files

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Case_Handling.IO is

   procedure Load_Exceptions
     (C         : in out Casing_Exceptions;
      Filename  : Virtual_File;
      Read_Only : Boolean);
   --  Load case exceptions file and set the in memory container

   procedure Save_Exceptions
     (C        : Casing_Exceptions;
      Filename : Virtual_File;
      Success  : out Boolean);
   --  Save the case exceptions container into Filename. The container still
   --  remains in memory. The read-only case exceptions are not saved.

end Case_Handling.IO;
