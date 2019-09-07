------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

--  This package contains functionalities to access or modify the
--  editable text (ie corresponding to editable lines, with opposition
--  to buffer text).

with Case_Handling; use Case_Handling;

package Src_Editor_Buffer.Text_Handling is

   function Get_Chars
     (Buffer               : access Source_Buffer_Record'Class;
      Line                 : Editable_Line_Type := 0;
      Column               : Character_Offset_Type := 0;
      Before               : Integer := -1;
      After                : Integer := -1;
      Include_Hidden_Chars : Boolean := True) return Basic_Types.UTF8_String;
   --  Return the characters around given position.
   --  If Line is 0, then the contents of the current selection is returned

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record'Class;
      Text         : String;
      Line_Begin   : Editable_Line_Type;
      Column_Begin : Character_Offset_Type;
      Line_End     : Editable_Line_Type;
      Column_End   : Character_Offset_Type);
   --  Replace the characters between given positions

   procedure Replace_Slice
     (Buffer : access Source_Buffer_Record'Class;
      Text   : String;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type := 0;
      Before : Integer := -1;
      After  : Integer := -1);
   --  Replace the characters around given position

   procedure Autocase_Text
     (Buffer : access Source_Buffer_Record'Class;
      Casing : Casing_Policy);
   --  Apply automatic casing to the text before the cursor,
   --  if the language preferences allow it. Otherwise, do nothing.

end Src_Editor_Buffer.Text_Handling;
