-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package groups all the functions related to file manipulation
--  in GVD, including remote file access and caching for information
--  that is displayed in the source window.

with GVD.Types;
with Main_Debug_Window_Pkg;

package GVD.Files is

   function Find_In_Cache
     (Window    : access Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class;
      File_Name : String) return GVD.Types.File_Cache_List;
   --  Return the cached data for a given file.
   --  If no data was previously cached for that file, then a new File_Cache
   --  is returned.

   procedure Load_File
     (Contents    : out GVD.Types.String_Access;
      Error_Msg   : out GVD.Types.String_Access;
      Cache       : GVD.Types.File_Cache_List;
      Remote_Host : GVD.Types.String_Access := null);
   --  Load the contents of the file associated with Cache, and return a
   --  pointer to it in Contents.
   --  It is your responsability to free Contents and Error_Msg
   --  Control-M characters have already been stripped.
   --
   --  If the file could not be found, Contents is set to null and Error_Msg
   --  contains the exact text of the error message.
   --  Error_Msg is left to null if the file could be found.

   procedure Clear_Cache
     (Window : access Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class;
      Force : Boolean := True);
   --  Clear the contents of the cached data for Window.
   --  if Force is False, then only the entries for which the Carriage return
   --  were not stripped are deleted, otherwise all entries are removed

end GVD.Files;
