------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               String_Utils                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--                Copyright (C) 2001 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with Unchecked_Deallocation;

package String_Utils is
   --  String utilities used in source file generation

   type String_Access is access String;
   procedure Free is new Unchecked_Deallocation (String, String_Access);

   procedure Skip_To_String
     (Type_Str  : String;
      Index     : in out Natural;
      Substring : String);
   --  Skip every character until an occurence of Substring is found.
   --  Index is set to the first character of the occurence.

   function To_File_Name (Name : in String) return String;
   --  Returns a file name from an ada subprogram/package name.

   procedure Ada_Case (S : in out String);
   --  Returns S with a casing matching Ada style.

   function Strip_Quotes (S : in String) return String;
   --  Removes the quotes and the spaces at the beginning and end of S.

end String_Utils;
