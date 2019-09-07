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

--  This package provides Text utility.

package Vdiff2_Module.Utils.Text is

   procedure New_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural);
   --  Add a blank line at line Line of a given file editor,.
   pragma Inline (New_Line);

   function Line_Length
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural) return Natural;
   --  Returns le length of a line from the position of the cursor.
   pragma Inline (Line_Length);

   procedure Delete
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Len    : Natural := 0);
   pragma Inline (Delete);

   procedure Delete_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural);
   pragma Inline (Delete_Line);

   procedure Insert
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Text   : String);
   pragma Inline (Insert);

   procedure Insert_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Text   : String);
   pragma Inline (Insert_Line);

   function Get
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Len    : Natural) return String;
   pragma Inline (Get);

   function Get_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural) return String;
   pragma Inline (Get_Line);

   procedure Move_Block
     (Kernel       : Kernel_Handle;
      Source_File  : Virtual_File;
      Dest_File    : Virtual_File;
      Source_Range : in out Diff_Range;
      Dest_Range   : in out Diff_Range);
   --  Copy the text in the range Source_Range to Dest_Range in Dest_File

   procedure Delete_Block
     (Kernel       : Kernel_Handle;
      Dest_File    : Virtual_File;
      Source_Range : in out Diff_Range;
      Dest_Range   : in out Diff_Range);

   procedure Replace
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Len    : Natural;
      Text   : String);
   pragma Inline (Replace);

   procedure Replace_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Text   : String);
   pragma Inline (Replace_Line);

end Vdiff2_Module.Utils.Text;
