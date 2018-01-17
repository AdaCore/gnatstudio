------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

with GNAT.Strings;
with Language_Handlers;
with Xref;                use Xref;

package Xref.Docgen is

   type Comment_Result is record
      Text       : Unbounded_String;
      Start_Line : Integer := -1;
   end record;

   No_Comment_Result : constant Comment_Result :=
     (Text       => Null_Unbounded_String,
      Start_Line => -1);

   function No (Comment : Comment_Result) return Boolean;
   --  Return true if Comment = No_Comment_Result

   function Present (Comment : Comment_Result) return Boolean;
   --  Return true if Comment /= No_Comment_Result

   function Get_Location
     (Entity : Root_Entity'Class) return General_Location;
   --  Return the location of Entity

   function Get_Docgen_Documentation
     (Self     : access General_Xref_Database_Record;
      Handler  : Language_Handlers.Language_Handler;
      Buffer   : GNAT.Strings.String_Access;
      Location : General_Location;
      End_Loc  : General_Location := No_Location) return Comment_Result;
   --  Retrieve the documentation associated with Entity. This is a variant of
   --  the routine provided by Xref since GNATdoc needs the location associated
   --  with the returned documentation (functionality not available in Xref).
   --  In addition, most of this routine receives as parameter the pointer to
   --  the buffer containing the sources; this avoids repeatedly loading the
   --  same file to retrieve all their documentation.
   --
   --  This routine is currently used only to retrieve documentation of C/C++
   --  entities.

private
   pragma Inline (No);
   pragma Inline (Present);
   pragma Inline (Get_Location);
end Xref.Docgen;
