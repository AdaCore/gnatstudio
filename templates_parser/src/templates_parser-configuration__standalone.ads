------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with GNAT.OS_Lib;

package Templates_Parser.Configuration is

   subtype Time_Stamp is GNAT.OS_Lib.OS_Time;

   pragma Style_Checks (Off);
   --  Kill bogus style check on missing overriding keyword with old GNAT
   --  versions.
   function "="
     (T1, T2 : Time_Stamp)
      return Boolean
      renames GNAT.OS_Lib."=";
   pragma Style_Checks (All_Checks);

   function Is_Regular_File
     (Filename : String)
      return Boolean
      renames GNAT.OS_Lib.Is_Regular_File;

   function File_Time_Stamp
     (Filename : String)
      return Time_Stamp
      renames GNAT.OS_Lib.File_Time_Stamp;

end Templates_Parser.Configuration;
