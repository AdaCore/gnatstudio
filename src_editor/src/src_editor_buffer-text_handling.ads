-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package contains functionalities to access or modify the
--  editable text (ie corresponding to editable lines, with opposition
--  to buffer text).

package Src_Editor_Buffer.Text_Handling is

   function Get_Chars
     (Buffer       : access Source_Buffer_Record'Class;
      Line_Begin   : Editable_Line_Type;
      Column_Begin : Natural;
      Line_End     : Editable_Line_Type;
      Column_End   : Natural) return UTF8_String;
   --  Return the characters between given positions.

   function Get_Chars
     (Buffer     : access Source_Buffer_Record'Class;
      Line       : Editable_Line_Type;
      Column     : Natural := 0;
      Before     : Integer := -1;
      After      : Integer := -1) return UTF8_String;
   --  Return the characters around given position.

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record'Class;
      Text         : String;
      Line_Begin   : Editable_Line_Type;
      Column_Begin : Natural;
      Line_End     : Editable_Line_Type;
      Column_End   : Natural);
   --  Replace the characters between given positions.

   procedure Replace_Slice
     (Buffer     : access Source_Buffer_Record'Class;
      Text       : String;
      Line       : Editable_Line_Type;
      Column     : Natural := 0;
      Before     : Integer := -1;
      After      : Integer := -1);
   --  Replace the characters around given position.

end Src_Editor_Buffer.Text_Handling;
