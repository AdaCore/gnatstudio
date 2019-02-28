------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;

package body Vdiff2_Module.Utils.Text is

   --------------
   -- New_Line --
   --------------

   procedure New_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural) is
   begin
      Insert_Line (Kernel, File, Line, "");
   end New_Line;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural) return Natural is
   begin
      return Get_Line (Kernel, File, Line)'Length;
   end Line_Length;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Len    : Natural := 0) is
   begin
      Replace_Text (Kernel, File, Line, Column, "", 0, Len);
   end Delete;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural) is
   begin
      Replace_Text (Kernel, File, Line, 1, "");
   end Delete_Line;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Text   : String) is
   begin
      Replace_Text (Kernel, File, Line, Column, Text, 0, 0);
   end Insert;

   -----------------
   -- Insert_Line --
   -----------------

   procedure Insert_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Text   : String) is
   begin
      Replace_Text (Kernel, File, Line, 1, Text & ASCII.LF, 0, 0);
   end Insert_Line;

   ---------
   -- Get --
   ---------

   function Get
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Len    : Natural) return String is
   begin
      return Get_Chars (Kernel, File, Line, Column, 0, Len);
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural) return String is
   begin
      return Get_Chars (Kernel, File, Line, 1);
   end Get_Line;

   ----------------
   -- Move_Block --
   ----------------

   procedure Move_Block
     (Kernel       : Kernel_Handle;
      Source_File  : Virtual_File;
      Dest_File    : Virtual_File;
      Source_Range : in out Diff_Range;
      Dest_Range   : in out Diff_Range)
   is
      Offset_Dest       : constant Natural :=
        Dest_Range.Last - Dest_Range.First;
      Offset_Source     : constant Natural :=
        Source_Range.Last - Source_Range.First;
      Offset_Min        : Natural := Offset_Source;

      First_Dest        : Natural;
      First_Source      : Natural;

      Current_Line      : String_Access;

   begin
      Remove_Blank_Lines (Kernel, Dest_Range.Blank_Lines_Mark.Element);
      Remove_Blank_Lines (Kernel, Source_Range.Blank_Lines_Mark.Element);

      First_Dest := Dest_Range.First;
      First_Source := Source_Range.First;

      Trace (Me, "VALEUR DE FIRST DEST : " & Integer'Image (First_Dest));
      Trace (Me, "VALEUR DE FIRST SOURCE : " & Integer'Image (First_Source));

      if Offset_Source > Offset_Dest then
         Offset_Min := Offset_Dest;
      end if;

      if Offset_Source > 0 and Offset_Dest > 0 then
         for J in 1 .. Offset_Min loop
            Current_Line := new String'
              (Get_Line (Kernel, Source_File, (First_Source + J - 1)));

            Replace_Line (Kernel, Dest_File, (First_Dest + J - 1),
                          Current_Line.all);
         end loop;

         if Offset_Source /= Offset_Min then

            for J in Offset_Source .. Offset_Dest loop
               Delete_Line (Kernel, Dest_File, First_Dest + J - 1);
            end loop;
         end if;

         if Offset_Dest /= Offset_Min then

            for J in Offset_Min .. Offset_Source loop
               Current_Line := new String'
                 (Get_Line
                    (Kernel, Source_File,
                     First_Source + J));
               Insert_Line
                 (Kernel, Dest_File,
                  First_Dest + J,
                  Current_Line.all);
            end loop;
         end if;

      elsif Offset_Dest <= 0 then

         for J in 1 .. Offset_Source loop
            Current_Line := new String'
              (Get_Line
                 (Kernel, Source_File,
                  First_Source + J - 1));
            Insert
              (Kernel, Dest_File,
               First_Dest + J - 1, 1,
               Current_Line.all);
         end loop;
      end if;
   end Move_Block;

   ------------------
   -- Delete_Block --
   ------------------

   procedure Delete_Block
     (Kernel       : Kernel_Handle;
      Dest_File    : Virtual_File;
      Source_Range : in out Diff_Range;
      Dest_Range   : in out Diff_Range)
   is
      Offset_Dest       : constant Natural :=
        Dest_Range.Last - Dest_Range.First;
      First_Dest        : Natural;

   begin
      Remove_Blank_Lines (Kernel, Dest_Range.Blank_Lines_Mark.Element);
      Remove_Blank_Lines (Kernel, Source_Range.Blank_Lines_Mark.Element);
      First_Dest := Dest_Range.First;

      for J in 1 .. Offset_Dest loop
         Delete_Line (Kernel, Dest_File, (First_Dest));
      end loop;
   end Delete_Block;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Len    : Natural;
      Text   : String) is
   begin
      Replace_Text (Kernel, File, Line, Column, Text, 0, Len);
   end Replace;

   ------------------
   -- Replace_Line --
   ------------------

   procedure Replace_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Text   : String) is
   begin
      Replace_Text (Kernel, File, Line, 1, Text);
   end Replace_Line;

end Vdiff2_Module.Utils.Text;
