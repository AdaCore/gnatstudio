------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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

--  This package provides utilities for JSON format

with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package JSON_Utils is

   function Save (File : Virtual_File) return JSON_Value;
   --  Store Virtual_File as a JSON_Value.

   function Load (Value : JSON_Value) return Virtual_File;
   --  Restore Virtual_File from the JSON_Value.

end JSON_Utils;
