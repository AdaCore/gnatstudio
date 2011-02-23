------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                      Copyright (C) 2005-2009, AdaCore                    --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
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
