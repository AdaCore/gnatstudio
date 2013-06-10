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

--  Package containing utility routines for Virtual Files

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNATCOLL.Xref;           use GNATCOLL.Xref;

private package Docgen3.Files is

   ----------------
   -- Files_List --
   ----------------

   package Files_List is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => GNATCOLL.VFS.Virtual_File);

   function Less_Than (Left, Right : GNATCOLL.VFS.Virtual_File) return Boolean;
   package Files_Vector_Sort is new Files_List.Generic_Sorting
     ("<" => Less_Than);

   procedure Remove_Element
     (List   : in out Files_List.Vector;
      Cursor : in out Files_List.Cursor);
   --  Remove element located at Cursor and place the cursor just after its
   --  current position

   procedure Write_To_File
     (Context   : access constant Docgen_Context;
      Directory : Virtual_File;
      Filename  : Filesystem_String;
      Text      : access Unbounded_String);
   --  Write the contents of Printout in the specified file

   procedure Write_To_File
     (Context   : access constant Docgen_Context;
      Directory : Virtual_File;
      Filename  : Filesystem_String;
      Text      : String);
   --  Write the contents of Printout in the specified file

end Docgen3.Files;
