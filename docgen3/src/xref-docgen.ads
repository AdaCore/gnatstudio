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

--  Temporary package used to develop new Xref routines since Docgen3 needs
--  the location associatd with the returned documentation (functionality not
--  available in the Xref routines). In addition, most of these routines
--  receive as parameter the pointer to the buffer containing the sources;
--  this avoids repeatedly loading the same file to retrieve all their
--  documentation.

--  In order to develop this package avoiding conflicts with the Xref routines
--  used by the Wavefronts, this package duplicates routines needed to provide
--  the new functionality. As soon as the new functionality is agreed and
--  incorporated this package will be removed or moved to its definitive
--  location in the repository.

with GNAT.Strings;
with Language_Handlers;
with Xref;                use Xref;

package Xref.Docgen is

   function No (E : General_Entity) return Boolean;
   --  Return true if E = No_General_Entity

   function No (L : General_Location) return Boolean;
   --  Return true if L = No_Location

   function Present (E : General_Entity) return Boolean;
   --  Return true if E /= No_General_Entity

   function Present (L : General_Location) return Boolean;
   --  Return true if L /= No_Location

   function Get_Location
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location;
   --  Return the location of Entity

   --  This type declaration should be located in GNATCOLL.Xref
   type Comment_Result is record
      Text       : Unbounded_String;
      Start_Line : Integer := -1;
   end record;

   No_Comment_Result : constant Comment_Result :=
     (Text       => Null_Unbounded_String,
      Start_Line => -1);

   function Documentation
     (Self             : access General_Xref_Database_Record;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : General_Entity;
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String;
   --  Retrieve the documentation associated with Entity

   function Get_Docgen_Documentation
     (Self    : access General_Xref_Database_Record;
      Handler : Language_Handlers.Language_Handler;
      Buffer  : GNAT.Strings.String_Access;
      Entity  : General_Entity) return Comment_Result;
   --  Retrieve the documentation associated with Entity

   function Get_Docgen_Documentation
     (Self     : access General_Xref_Database_Record;
      Handler  : Language_Handlers.Language_Handler;
      Buffer   : GNAT.Strings.String_Access;
      Location : General_Location) return Comment_Result;
   --  Retrieve the documentation associated with Entity

   function Get_Docgen_Src
     (Self    : access General_Xref_Database_Record;
      Handler : Language_Handlers.Language_Handler;
      Buffer  : GNAT.Strings.String_Access;
      Entity  : General_Entity) return String;
   --  Retrieve the sources associated with Entity

private
   pragma Inline (Present);
   pragma Inline (Get_Location);
end Xref.Docgen;
