------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

--  Package containing utility subprograms used throughout Docgen3

with GNATCOLL.VFS;

private package Docgen3.Utils is

   function Filter (S : String) return String;
   --  Returns an string without the spaces and line terminators at the
   --  beginning/end of the string.

   function Image
     (Db     : General_Xref_Database;
      Entity : General_Entity) return String;
   --  Return the location formated the gnat way: "file:line:col"

   function Image
     (Loc           : General_Location;
      With_Filename : Boolean := True) return String;
   --  Return the location formated the gnat way: "[file:]line:col". If
   --  With_Filename is false then the name of the file is not added to the
   --  returned string.

   function Is_Spec_File
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Whether File is a spec file

   function Spaces_Only (Text : String) return Boolean;
   --  Returns true if the string is not empty and all its characters are ' '

   function To_String (N : Integer) return String;
   --  Convert Number to String removing spaces

end Docgen3.Utils;
