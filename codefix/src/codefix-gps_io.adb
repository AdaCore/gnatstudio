-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2008, AdaCore               --
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

with GNATCOLL.Utils;     use GNATCOLL.Utils;
with GPS.Kernel.Console; use GPS.Kernel.Console;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;
with String_Utils;       use String_Utils;
with Traces;             use Traces;
with VFS;                use VFS;

package body Codefix.GPS_Io is

   Me : constant Debug_Handle := Create ("Codefix.GPS_IO");

   ------------
   -- Get_Id --
   ------------

   function Get_Id (This : GPS_Mark) return String is
   begin
      return This.Id.all;
   end Get_Id;

   ------------------
   -- Get_New_Mark --
   ------------------

   function Get_New_Mark
     (Current_Text : Console_Interface;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class
   is
      Result : GPS_Mark;
      Args   : GNAT.Strings.String_List :=
        (1 => new String'(Full_Name (Get_File (Cursor)).all),
         2 => new String'(Image (Get_Line (Cursor))),
         3 => new String'(Image (Natural (Get_Column (Cursor)))),
         4 => new String'("0"));

   begin
      Result.Id := new String'
        (Execute_GPS_Shell_Command
           (Current_Text.Kernel, "Editor.create_mark", Args));
      Free (Args);
      return Result;
   end Get_New_Mark;

   ------------------------
   -- Get_Current_Cursor --
   ------------------------

   function Get_Current_Cursor
     (Current_Text : Console_Interface;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class
   is
      New_Cursor : File_Cursor;
      Args       : GNAT.Strings.String_List (1 .. 1);
   begin
      Set_File (New_Cursor, Get_File_Name (Current_Text));
      Args (1) := new String'(GPS_Mark (Mark).Id.all);

      declare
         Column : constant String := Execute_GPS_Shell_Command
           (Current_Text.Kernel, "Editor.get_column", Args);
         Line : constant String := Execute_GPS_Shell_Command
           (Current_Text.Kernel, "Editor.get_line", Args);
      begin
         Set_Location
           (New_Cursor,
            Natural'Value (Line),
            Column_Index'Value (Column));

      exception
         when Constraint_Error =>
            Trace (Me, "unexpected result from get_column/line: " &
                   Column & ":" & Line);
      end;

      Free (Args);
      return New_Cursor;
   end Get_Current_Cursor;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out GPS_Mark) is
   begin
      Free (This.Id);
      Free (Mark_Abstr (This));
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Console_Interface) is
   begin
      Free (Text_Interface (This));

      if This.Lines /= null then
         Free (This.Lines.all);
      end if;

      Free (This.Lines);
      Free (This.Lines_Number);
      Free (This.File_Modified);
   end Free;

   ----------
   -- Undo --
   ----------

   procedure Undo (This : in out Console_Interface) is
      Args : GNAT.Strings.String_List :=
        (1 => new String'(Full_Name (Get_File_Name (This)).all));
      Ignore : constant String := Execute_GPS_Shell_Command
        (This.Kernel, "Editor.undo", Args);
      pragma Unreferenced (Ignore);
   begin
      Free (Args);
   end Undo;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String
   is
      Line : constant String := Get_Recorded_Line (This, Get_Line (Cursor));
      Char_Ind : constant Char_Index :=
        To_Char_Index (Get_Column (Cursor), Line);
   begin
      return Line
        (Natural (Char_Ind) .. Natural (Char_Ind) + Len - 1);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class) return Character
   is
      Line : constant String := Get_Recorded_Line (This, Get_Line (Cursor));
      Char_Ind : constant Char_Index :=
        To_Char_Index (Get_Column (Cursor), Line);
   begin
      return Line (Natural (Char_Ind));
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This      : Console_Interface;
      Cursor    : Text_Cursor'Class;
      Start_Col : Column_Index := 0) return String
   is
      Line : constant String := Get_Recorded_Line (This, Get_Line (Cursor));
      Char_Ind : Char_Index;
   begin
      if Start_Col = 0 then
         Char_Ind := To_Char_Index (Get_Column (Cursor), Line);
      else
         Char_Ind := To_Char_Index (Start_Col, Line);
      end if;

      return Line (Natural (Char_Ind) .. Line'Last);
   end Get_Line;

   -----------------------
   -- Get_Recorded_Line --
   -----------------------

   function Get_Recorded_Line
     (This   : Console_Interface;
      Number : Natural) return String
   is
      Node : String_List.List_Node;
   begin
      Update (This);

      Node := First (This.Lines.all);

      for J in 1 .. Number - 1 loop
         if Node = String_List.Null_Node then
            return "";
         end if;

         Node := Next (Node);
      end loop;

      return Data (Node).all;
   end Get_Recorded_Line;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This      : in out Console_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String) is
   begin
      This.File_Modified.all := True;
      Text_Has_Changed (This);

      if Get_Line (Cursor) /= 0 then
         declare
            Args : GNAT.Strings.String_List :=
              (1 => new String'(Full_Name (Get_File_Name (This)).all),
               2 => new String'(Image (Get_Line (Cursor))),
               3 => new String'(Image (Natural (Get_Column (Cursor)))),
               4 => new String'(New_Value),
               5 => new String'("0"),           --  before
               6 => new String'(Image (Len)));  --  after
            S : constant String := Execute_GPS_Shell_Command
              (This.Kernel, "Editor.replace_text", Args);
         begin
            if S /= "" then
               Insert (This.Kernel, S, True, Error);
            end if;

            Free (Args);
         end;

      else
         declare
            Args : GNAT.Strings.String_List :=
              (1 => new String'(Full_Name (Get_File_Name (This)).all),
               2 => new String'("1"),  --  line
               3 => new String'("1"),  --  column
               4 => new String'(New_Value),
               5 => new String'("0"),  --  before
               6 => new String'("0")); --  after
            S : constant String := Execute_GPS_Shell_Command
              (This.Kernel, "Editor.replace_text", Args);
         begin
            if S /= "" then
               Insert (This.Kernel, S, True, Error);
            end if;

            Free (Args);
         end;
      end if;
   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This        : in out Console_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String)
   is
      Line_Str        : GNAT.Strings.String_Access;
      Insert_Position : Text_Cursor := Text_Cursor (Cursor);
   begin
      This.File_Modified.all := True;
      Text_Has_Changed (This);

      Set_Location (Insert_Position, Get_Line (Insert_Position), 1);

      if Get_Line (Cursor) = 0 then
         Replace (This, Insert_Position, 0, New_Line & EOL_Str);
      else
         Line_Str := new String'(Get_Line (This, Insert_Position));
         Set_Location
           (Insert_Position,
            Get_Line (Insert_Position),
            To_Column_Index (Char_Index (Line_Str'Last), Line_Str.all) + 1);
         Replace (This, Insert_Position, 0, EOL_Str & New_Line);
         Free (Line_Str);
      end if;
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This   : in out Console_Interface;
      Cursor : Text_Cursor'Class)
   is
      Args : GNAT.Strings.String_List :=
        (1 => new String'(Full_Name (Get_File_Name (This)).all),
         2 => new String'(Image (Get_Line (Cursor))),
         3 => new String'(Image (Natural (Get_Column (Cursor)))),
         4 => new String'(""));  --  replacement text
      S : constant String :=
        Execute_GPS_Shell_Command (This.Kernel, "Editor.replace_text", Args);
   begin
      This.File_Modified.all := True;
      Text_Has_Changed (This);

      if S /= "" then
         Insert (This.Kernel, S, True, Error);
      end if;

      Free (Args);
   end Delete_Line;

   -----------------
   -- Indent_Line --
   -----------------

   procedure Indent_Line
     (This : in out Console_Interface;
      Cursor : Text_Cursor'Class)
   is
      ArgsFile : GNAT.Strings.String_List :=
        (1 => new String'(Full_Name (Get_File_Name (This)).all));

      ArgsEditorBuffer : GNAT.Strings.String_List :=
        (1 => new String'("%1"),
         2 => new String'("False"),
         3 => new String'("False"));

      ArgsEditorLocation : GNAT.Strings.String_List :=
        (1 => new String'("%1"),
         2 => new String'(Integer'Image (Cursor.Get_Line)),
         3 => new String'(Column_Index'Image (Cursor.Get_Column)));

      ArgsIndent : GNAT.Strings.String_List :=
        (1 => new String'("%2"),
         2 => new String'("%1"),
         3 => new String'("%1"));

      ResultFile : constant String := Execute_GPS_Shell_Command
        (This.Kernel, "File", ArgsFile);
      pragma Unreferenced (ResultFile);

      ResultEditorBuffer : constant String := Execute_GPS_Shell_Command
        (This.Kernel, "EditorBuffer.get", ArgsEditorBuffer);
      pragma Unreferenced (ResultEditorBuffer);

      ResultEditorLocation : constant String := Execute_GPS_Shell_Command
        (This.Kernel, "EditorLocation", ArgsEditorLocation);
      pragma Unreferenced (ResultEditorLocation);

      ResultIndent : constant String := Execute_GPS_Shell_Command
        (This.Kernel, "EditorBuffer.indent", ArgsIndent);
      pragma Unreferenced (ResultIndent);

   begin
      This.File_Modified.all := True;
      Text_Has_Changed (This);

      Free (ArgsFile);
      Free (ArgsEditorBuffer);
      Free (ArgsEditorLocation);
      Free (ArgsIndent);
   end Indent_Line;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out Console_Interface;
      Path : VFS.Virtual_File)
   is
      pragma Unreferenced (This, Path);
   begin
      null;
   end Initialize;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (This : Console_Interface)
      return GNAT.Strings.String_Access
   is
      Args : GNAT.Strings.String_List :=
        (1 => new String'(Full_Name (Get_File_Name (This)).all));
      S    : constant GNAT.Strings.String_Access := new String'
        (Execute_GPS_Shell_Command (This.Kernel, "Editor.get_buffer", Args));
   begin
      Free (Args);
      return S;
   end Read_File;

   --------------
   -- Line_Max --
   --------------

   function Line_Max (This : Console_Interface) return Natural is
   begin
      Update (This);

      return This.Lines_Number.all;
   end Line_Max;

   ------------
   -- Update --
   ------------

   procedure Update (This : Console_Interface) is
      File          : GNAT.Strings.String_Access;
      Current_Index : Natural := 0;
      Old_Index     : Natural := 0;
      Last_Line     : GNAT.Strings.String_Access;

   begin
      if not This.File_Modified.all then
         return;
      end if;

      Free (This.Lines.all);

      File := Read_File (This);

      Current_Index := File'First;
      Old_Index := Current_Index;

      while Current_Index <= File'Last loop
         Skip_To_Char (File.all, Current_Index, ASCII.LF);

         if Current_Index > File'Last then
            Current_Index := File'Last;
         end if;

         if File (Current_Index) = ASCII.LF then
            Last_Line := new String (1 .. Current_Index - Old_Index);
            Last_Line.all := File (Old_Index .. Current_Index - 1);
         else
            Last_Line := new String (1 .. Current_Index - Old_Index + 1);
            Last_Line.all := File (Old_Index .. Current_Index);
         end if;

         Append (This.Lines.all, Last_Line);
         Current_Index := Current_Index + 1;
         Old_Index     := Current_Index;
      end loop;

      declare
         Args : GNAT.Strings.String_List :=
           (1 => new String'(Full_Name (Get_File_Name (This)).all));
         Result : constant String := Execute_GPS_Shell_Command
           (This.Kernel, "Editor.get_last_line", Args);

      begin
         This.Lines_Number.all := Natural'Value (Result);
         Free (Args);

      exception
         when Constraint_Error =>
            Trace (Me, "unexpected result from get_last_line: " & Result);
      end;

      This.File_Modified.all := False;
      Free (File);
   end Update;

   ----------------
   -- Set_Kernel --
   ----------------

   procedure Set_Kernel
     (This : in out Console_Interface; Kernel : Kernel_Handle) is
   begin
      This.Kernel := Kernel;
   end Set_Kernel;

   ----------------------
   -- Constrain_Update --
   ----------------------

   procedure Constrain_Update (This : in out Console_Interface) is
   begin
      This.File_Modified.all := True;
   end Constrain_Update;

end Codefix.GPS_Io;
