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

with Gdk.RGBA;                 use Gdk.RGBA;
with GNAT.OS_Lib;              use GNAT.OS_Lib;

with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with GPS.Kernel.Scripts;       use GPS.Kernel.Scripts;
with GPS.Kernel.Style_Manager; use GPS.Kernel.Style_Manager;
with Gtkada.Style;             use Gtkada.Style;
with String_Utils;             use String_Utils;

package body Vdiff2_Module.Utils.Shell_Command is

   --------------
   -- Add_Line --
   --------------

   function Add_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Buffer : GPS_Editor_Buffer'Class;
      Pos    : Editable_Line_Type;
      Style  : String := "";
      Number : Natural := 1) return Editor_Mark'Class is
   begin
      return Add_Special_Line
        (This       => Buffer,
         Start_Line => Integer (Pos),
         Text       => (1 .. Number - 1 => ASCII.LF),
         Style      => Get_Style_Manager (Kernel_Handle (Kernel)).Get (Style),
         Name       => "",
         Column_Id  => "",
         Info       => null);
   end Add_Line;

   ------------------------
   -- Remove_Blank_Lines --
   ------------------------

   procedure Remove_Blank_Lines
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Mark   : Editor_Mark'Class)
   is
      pragma Unreferenced (Kernel);
      Buffer : constant Editor_Buffer'Class := Mark.Location (False).Buffer;
   begin
      Remove_Special_Lines
         (GPS_Editor_Buffer'Class (Buffer), Mark, Integer'Last);
   end Remove_Blank_Lines;

   ----------
   -- Edit --
   ----------

   procedure Edit
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      CL : Arg_List := Create ("Editor.edit");
   begin
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, "0", One_Arg);
      Append_Argument (CL, "0", One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);
   end Edit;

   ---------------------------
   -- Synchronize_Scrolling --
   ---------------------------

   procedure Synchronize_Scrolling
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File1  : Virtual_File;
      File2  : Virtual_File;
      File3  : Virtual_File := GNATCOLL.VFS.No_File)
   is
      CL : Arg_List := Create ("Editor.set_synchronized_scrolling");
   begin
      Append_Argument (CL, +Full_Name (File1), One_Arg);
      Append_Argument (CL, +Full_Name (File2), One_Arg);

      if File3 /= GNATCOLL.VFS.No_File then
         Append_Argument (CL, +Full_Name (File3), One_Arg);
      end if;

      Execute_GPS_Shell_Command (Kernel, CL);
   end Synchronize_Scrolling;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Before : Integer := -1;
      After  : Integer := -1) return String
   is
      CL : Arg_List := Create ("Editor.get_chars");
   begin
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, Image (Line), One_Arg);
      Append_Argument (CL, Image (Column), One_Arg);
      Append_Argument (CL, Image (Before), One_Arg);
      Append_Argument (CL, Image (After), One_Arg);
      return Execute_GPS_Shell_Command (Kernel, CL);
   end Get_Chars;

   ------------------------
   -- Get_File_Last_Line --
   ------------------------

   function Get_File_Last_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File) return Natural
   is
      CL : Arg_List := Create ("Editor.get_last_line");
   begin
      Append_Argument (CL, +Full_Name (File), One_Arg);
      return Natural'Value (Execute_GPS_Shell_Command (Kernel, CL));
   end Get_File_Last_Line;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1)
   is
      CL : Arg_List;
   begin
      for J in 1 .. Number loop
         CL := Create ("Editor.highlight");
         Append_Argument (CL, +Full_Name (File), One_Arg);
         Append_Argument (CL, Style, One_Arg);
         Append_Argument (CL, Image (Pos + J - 1), One_Arg);
         Execute_GPS_Shell_Command (Kernel, CL);
      end loop;
   end Highlight_Line;

   ---------------------
   -- Highlight_Range --
   ---------------------

   procedure Highlight_Range
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1)
   is
      CL : Arg_List;
      Args_Highlight_Range : GNAT.OS_Lib.Argument_List :=
                               (1 => new String'(+Full_Name (File)),
                                2 => new String'(Style),
                                3 => new String'(Image (Line)),
                                4 => new String'("-1"),
                                5 => new String'("-1"));

   begin
      CL := Create ("Editor.highlight_range");
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, Style, One_Arg);
      Append_Argument (CL, Image (Line), One_Arg);

      if Line /= 0 then
         if Start_C >= 0 then
            Append_Argument (CL, Image (Start_C), One_Arg);
         else
            Append_Argument (CL, "-1", One_Arg);
         end if;

         if End_C >= 0  and then Start_C >= 0 then
            Append_Argument (CL, Image (End_C), One_Arg);
         else
            Append_Argument (CL, "-1", One_Arg);
         end if;
      end if;

      Execute_GPS_Shell_Command (Kernel, CL);
      Free (Args_Highlight_Range);
   end Highlight_Range;

   ---------------------------
   -- Register_Highlighting --
   ---------------------------

   procedure Register_Highlighting
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      CL : Arg_List;
      Default_Color      : constant String  := Side_Default_Color.Get_Pref;
      Append_Color       : constant String  := Side_Append_Color.Get_Pref;
      Remove_Color       : constant String  := Side_Remove_Color.Get_Pref;
      Change_Color       : constant String  := Side_Change_Color.Get_Pref;
      Change_Fine_Color  : constant String  :=
        To_String (Shade_Or_Lighten (Side_Change_Color.Get_Pref));

   begin
      --  <preferences>

      CL := Create ("Editor.register_highlighting");
      Append_Argument (CL, Default_Style, One_Arg);
      Append_Argument (CL, Default_Color, One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);

      CL := Create ("Editor.register_highlighting");
      Append_Argument (CL, Append_Style, One_Arg);
      Append_Argument (CL, Append_Color, One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);

      --  This is defined as the same as Default_Color, but kept for
      --  backward compatibility for now, while Old_Style is referenced
      --  in the Ada code.

      CL := Create ("Editor.register_highlighting");
      Append_Argument (CL, Old_Style, One_Arg);
      Append_Argument (CL, Default_Color, One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);

      CL := Create ("Editor.register_highlighting");
      Append_Argument (CL, Remove_Style, One_Arg);
      Append_Argument (CL, Remove_Color, One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);

      CL := Create ("Editor.register_highlighting");
      Append_Argument (CL, Change_Style, One_Arg);
      Append_Argument (CL, Change_Color, One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);

      CL := Create ("Editor.register_highlighting");
      Append_Argument (CL, Fine_Change_Style, One_Arg);
      Append_Argument (CL, Change_Fine_Color, One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);
   end Register_Highlighting;

   ------------------
   -- Replace_Text --
   ------------------

   procedure Replace_Text
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Text   : String;
      Before : Integer := -1;
      After  : Integer := -1)
   is
      CL : Arg_List := Create ("Editor.replace_text");
   begin
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, Image (Line), One_Arg);
      Append_Argument (CL, Image (Column), One_Arg);
      Append_Argument (CL, Text, One_Arg);
      Append_Argument (CL, Image (Before), One_Arg);
      Append_Argument (CL, Image (After), One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);
   end Replace_Text;

   -----------------
   -- Unhighlight --
   -----------------

   procedure Unhighlight
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "")
   is
      CL : Arg_List := Create ("Editor.unhighlight");
   begin
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, Style, One_Arg);
      Append_Argument (CL, Image (Pos), One_Arg);
      Execute_GPS_Shell_Command (Kernel, CL);
   end Unhighlight;

   ----------------------
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "") is
   begin
      Unhighlight (Kernel, File, Pos, Style);
      Unhighlight_Range (Kernel, File, Fine_Change_Style, Pos);
   end Unhighlight_Line;

   -----------------------
   -- Unhighlight_Range --
   -----------------------

   procedure Unhighlight_Range
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1)
   is
      CL : Arg_List := Create ("Editor.unhighlight_range");
   begin
      Append_Argument (CL, +Full_Name (File), One_Arg);
      Append_Argument (CL, Style, One_Arg);
      Append_Argument (CL, Image (Line), One_Arg);

      if Line /= 0 then
         if Start_C >= 0 then
            Append_Argument (CL, Image (Start_C), One_Arg);
         else
            Append_Argument (CL, "-1", One_Arg);
         end if;

         if End_C >= 0  and then Start_C >= 0 then
            Append_Argument (CL, Image (End_C), One_Arg);
         else
            Append_Argument (CL, "-1", One_Arg);
         end if;
      end if;

      Execute_GPS_Shell_Command (Kernel, CL);
   end Unhighlight_Range;

end Vdiff2_Module.Utils.Shell_Command;
