-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Glide_Kernel;                      use Glide_Kernel;
with Glide_Kernel.Scripts;              use Glide_Kernel.Scripts;
with Glide_Kernel.Preferences;          use Glide_Kernel.Preferences;

with String_Utils;                      use String_Utils;
with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with Basic_Types;

with Gdk.Color;                         use Gdk.Color;

package body Vdiff2_Module.Utils.Shell_Command is

   --------------
   -- Add_Line --
   --------------

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String
   is
      Args_Line : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Image (Pos)),
         3 => new String'(Image (Number)),
         4 => new String'(Style));
      Res : constant String :=  Execute_GPS_Shell_Command
        (Kernel, "add_blank_lines", Args_Line);

   begin
      Basic_Types.Free (Args_Line);
      return Res;
   end Add_Line;

   -----------------
   -- Delete_Mark --
   -----------------

   procedure Delete_Mark
     (Kernel : Kernel_Handle;
      Link   : String) is
      pragma Unreferenced (Kernel, Link);
   begin
      null; -- ??? corrected when nico add delete_mark command
   end Delete_Mark;

   ----------
   -- Edit --
   ----------

   procedure Edit
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      Args_edit           : Argument_List :=
        (1 => new String'(Full_Name (File).all));

   begin
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);
   end Edit;

   ---------------------------
   -- Synchronize_Scrolling --
   ---------------------------

   procedure Synchronize_Scrolling
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File1  : Virtual_File;
      File2  : Virtual_File;
      File3  : Virtual_File := VFS.No_File)
   is
      Args : Argument_List_Access;
   begin
      if File3 = VFS.No_File then
         Args := new Argument_List'
           (1 => new String'(Full_Name (File1).all),
            2 => new String'(Full_Name (File2).all));
      else
         Args := new Argument_List'
           (1 => new String'(Full_Name (File1).all),
            2 => new String'(Full_Name (File2).all),
            3 => new String'(Full_Name (File3).all));
      end if;

      Execute_GPS_Shell_Command
        (Kernel, "set_synchronized_scrolling", Args.all);
      Basic_Types.Free (Args.all);
      Free (Args);
   end Synchronize_Scrolling;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Before : Integer := -1;
      After  : Integer := -1) return String
   is
      Args_Replace_Text : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Image (Line)),
         3 => new String'(Image (Column)),
         4 => new String'(Image (Before)),
         5 => new String'(Image (After)));
      Res               : constant String := Execute_GPS_Shell_Command
        (Kernel, "get_chars", Args_Replace_Text);

   begin
      Basic_Types.Free (Args_Replace_Text);
      return Res;
   end Get_Chars;

   --------------------------
   --  Get_File_Last_Line  --
   --------------------------

   function Get_File_Last_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File : Virtual_File) return Natural
   is
      Args_Line : Argument_List :=
        (1 => new String'(Full_Name (File).all));
      Res       : constant String :=  Execute_GPS_Shell_Command
        (Kernel, "get_last_line", Args_Line);

   begin
      Basic_Types.Free (Args_Line);
      return Natural'Value (Res);
   end Get_File_Last_Line;

   ----------------------
   -- Get_Line_Number  --
   ----------------------

   function Get_Line_Number
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Mark   : String) return Natural
   is
      Args_Line : Argument_List :=
        (1 => new String'(Mark));
      Res       : constant String :=  Execute_GPS_Shell_Command
        (Kernel, "get_line", Args_Line);

   begin
      Basic_Types.Free (Args_Line);
      return Natural'Value (Res);
   end Get_Line_Number;

   ---------------------
   -- Goto_Difference --
   ---------------------

   procedure Goto_Difference
     (Kernel : Kernel_Handle;
      Link : Diff_Chunk_Access)
   is
      Args : Argument_List (1 .. 1);
   begin

      if Link.Range1.Mark /= null then
         Args := (1 => new String'(Link.Range1.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
      end if;

      if Link.Range2.Mark /= null then
         Args := (1 => new String'(Link.Range2.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
      end if;

      if Link.Range3.Mark /= null then
         Args := (1 => new String'(Link.Range3.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
      end if;
   end Goto_Difference;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "";
      Number : Natural := 1)
   is
      Args_Highlight : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Style),
         3 => null);

   begin

      for J in 1 .. Number loop
         Args_Highlight (3) := new String'(Image (Pos + J - 1));
         Execute_GPS_Shell_Command (Kernel, "highlight", Args_Highlight);
         Free (Args_Highlight (3));
      end loop;

      Basic_Types.Free (Args_Highlight);
   end Highlight_Line;

   ---------------------
   -- Highlight_Range --
   ---------------------

   procedure Highlight_Range
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1)
   is
      Args_Highlight_Range : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Style),
         3 => new String'(Image (Line)),
         4 => new String'("-1"),
         5 => new String'("-1"));

   begin

      if Line /= 0 then

         if End_C >= 0  and then Start_C >= 0 then
            Free (Args_Highlight_Range (5));
            Args_Highlight_Range (5) := new String'(Image (End_C));
         end if;

         if Start_C >= 0 then
            Free (Args_Highlight_Range (4));
            Args_Highlight_Range (4) := new String'(Image (Start_C));
         end if;
      end if;

      Execute_GPS_Shell_Command
        (Kernel, "highlight_range", Args_Highlight_Range);
      Basic_Types.Free (Args_Highlight_Range);
   end Highlight_Range;

   ---------------------
   -- Mark_Diff_Block --
   ---------------------

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural) return String
   is
      Args : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Image (Pos)),
         3 => new String'("1"));
      Res : constant String := Execute_GPS_Shell_Command
        (Kernel, "create_mark", Args);

   begin
      Basic_Types.Free (Args);
      return Res;
   end Mark_Diff_Block;

   ---------------------------
   -- Register_Highlighting --
   ---------------------------

   procedure Register_Highlighting
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Default_Color : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Default_Color));
      Old_Color     : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Old_Color));
      Append_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Append_Color));
      Remove_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Remove_Color));
      Change_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Change_Color));
      Change_Fine_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Fine_Change_Color));
      Args          : Argument_List :=
        (1 => new String'(Default_Style),
         2 => new String'(Default_Color));

   begin
      --  <preferences>

      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Append_Style),
               2 => new String'(Append_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Old_Style),
               2 => new String'(Old_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Remove_Style),
               2 => new String'(Remove_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Change_Style),
               2 => new String'(Change_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Fine_Change_Style),
               2 => new String'(Change_Fine_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);

      VDiff2_Module (Vdiff_Module_ID).Enable_Fine_Diff
        := (Change_Fine_Color /= Change_Color);
   end Register_Highlighting;

   --------------------------
   --  Remove_Blank_Lines  --
   --------------------------

   procedure Remove_Blank_Lines
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Mark   : in out String_Access)
   is
      Args : Argument_List (1 .. 1);
   begin

      if Mark /= null then
         Args (1) := Mark;
         Execute_GPS_Shell_Command
           (Kernel, "remove_blank_lines", Args);
         Free (Mark);
      end if;
   end Remove_Blank_Lines;

   ------------------
   -- Replace_Text --
   ------------------

   procedure Replace_Text
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Natural;
      Column : Natural;
      Text   : String;
      Before : Integer := -1;
      After  : Integer := -1)
   is
      Args_Replace_Text : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Image (Line)),
         3 => new String'(Image (Column)),
         4 => new String'(Text),
         5 => new String'(Image (Before)),
         6 => new String'(Image (After)));

   begin
      Execute_GPS_Shell_Command
        (Kernel, "replace_text", Args_Replace_Text);
      Basic_Types.Free (Args_Replace_Text);
   end Replace_Text;

   -----------------
   -- Unhighlight --
   -----------------

   procedure Unhighlight
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "")
   is
      Args_Highlight : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Style),
         3 => new String'(Image (Pos)));

   begin
      Execute_GPS_Shell_Command (Kernel, "unhighlight", Args_Highlight);
      Basic_Types.Free (Args_Highlight);
   end Unhighlight;

   ----------------------
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "")
   is

   begin
      Unhighlight (Kernel, File, Pos, Style);
      Unhighlight_Range (Kernel, File, Fine_Change_Style, Pos);
   end Unhighlight_Line;

   -----------------------
   -- Unhighlight_Range --
   -----------------------

   procedure Unhighlight_Range
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Style   : String;
      Line    : Natural := 0;
      Start_C : Integer := -1;
      End_C   : Integer := -1)
   is
      Args_Highlight_Range : Argument_List :=
        (1 => new String'(Full_Name (File).all),
         2 => new String'(Style),
         3 => new String'(Image (Line)),
         4 => new String'("-1"),
         5 => new String'("-1"));

   begin

      if Line /= 0 then

         if End_C >= 0  and then Start_C >= 0 then
            Free (Args_Highlight_Range (5));
            Args_Highlight_Range (5) := new String'(Image (End_C));
         end if;

         if Start_C >= 0 then
            Free (Args_Highlight_Range (4));
            Args_Highlight_Range (4) := new String'(Image (Start_C));
         end if;
      end if;

      Execute_GPS_Shell_Command
        (Kernel, "unhighlight_range", Args_Highlight_Range);
      Basic_Types.Free (Args_Highlight_Range);
   end Unhighlight_Range;

end Vdiff2_Module.Utils.Shell_Command;
