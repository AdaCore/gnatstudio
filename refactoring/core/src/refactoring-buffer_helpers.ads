------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

--  This package contains high level text buffer manipulation function used
--  by the refactoring.

--  ??? need to decide between Erase, Delete or Remove for code deletion.

with GPS.Editors;
with Basic_Types;  use Basic_Types;

private package Refactoring.Buffer_Helpers is

   ------------------------
   -- Conversion helpers --
   ------------------------

   function To_Location
     (Context   : not null access Factory_Context_Record'Class;
      Location  : access Universal_Location)
      return GPS.Editors.Editor_Location'Class;

   function To_Location
     (Context   : not null access Factory_Context_Record'Class;
      Location  : GPS.Editors.Editor_Location'Class)
      return Universal_Location;

   --------------------------------
   -- Helpers for buffer reading --
   --------------------------------

   function Get_Line
     (Context   : not null access Factory_Context_Record'Class;
      Location  : access Universal_Location;
      Start_Col : Visible_Column_Type := 0) return String;
   --  Get all character from the column specified by the cursor to the end of
   --  the line, or beginning by Start_Col if not 0. The String resulting must
   --  have parameter 'First equal to Cursor.Col.

   function Get
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location) return String;
   --  Return a text slice between the two locations given in parameter

   --------------------------------
   -- Helpers for buffer writing --
   --------------------------------

   --  Note - each of these subprograms first lock construct updates, then
   --  perform the modifications, and then updated the construct updates. Since
   --  the lock is reentrand, only one update will be performed per call. If
   --  construct database updates have to be further delayed, then locks on
   --  the file must be taken before calling this.
   --
   --  See Language.Tree.Database.Update_Lock for more information

   procedure Remove_Code
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location);
   --  Removes the code between Start and Stop, removing the remaining blank
   --  line if any.

   procedure Comment_Code
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location);
   --  Comment code from Start to Stop

   procedure Add_Line
     (Context  : not null access Factory_Context_Record'Class;
      Location : access Universal_Location;
      New_Line : String;
      Indent   : Boolean := False);
   --  Add a line AFTER the position specified by the cursor. To add a line at
   --  the begining of the text, set cursor line = 0. If Indent is true then
   --  the new line will get automatically indented.

   procedure Indent_Line
     (Context  : not null access Factory_Context_Record'Class;
      Location : access Universal_Location);
   --  Indent the line pointed by the location

   procedure Remove_Line
     (Context  : not null access Factory_Context_Record'Class;
      Location : access Universal_Location);
   --  Delete the line where the location is

   procedure Replace
     (Context     : not null access Factory_Context_Record'Class;
      Start, Stop : access Universal_Location;
      New_Value   : String);
   --  Replace the characters between Start_Cursor and End_Cursor by New_Value.

   procedure Replace
     (Context   : not null access Factory_Context_Record'Class;
      Location  : access Universal_Location;
      Len       : Natural;
      New_Value : String);
   --  Replace the Len characters, from the position designed by the location,
   --  by New_Value

end Refactoring.Buffer_Helpers;
