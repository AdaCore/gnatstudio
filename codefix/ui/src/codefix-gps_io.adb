------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GNATCOLL.VFS;    use GNATCOLL.VFS;
with GNATCOLL.Xref;

package body Codefix.GPS_Io is
   use type GNATCOLL.Xref.Visible_Column;

   Me : constant Trace_Handle := Create ("GPS.CODEFIX.GPS_IO");

   ------------------
   -- Get_New_Mark --
   ------------------

   overriding function Get_New_Mark
     (Current_Text : Console_Interface;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class
   is
      Result : GPS_Mark;
   begin
      Result.Mark.Replace_Element
        (Current_Text.Kernel.Get_Buffer_Factory.New_Mark
           (Cursor.Get_File,
            Get_Line (Cursor),
            Natural (Get_Column (Cursor))));

      return Result;
   end Get_New_Mark;

   ------------------------
   -- Get_Current_Cursor --
   ------------------------

   overriding function Get_Current_Cursor
     (Current_Text : Console_Interface;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class
   is
      New_Cursor : File_Cursor;
   begin
      Set_File (New_Cursor, Get_File_Name (Current_Text));

      begin
         Set_Location
           (New_Cursor,
            GPS_Mark'Class (Mark).Mark.Element.Line,
            GPS_Mark'Class (Mark).Mark.Element.Column);

      exception
         when Constraint_Error =>
            Trace (Me, "unexpected result from get_column/line: "
                   & GPS_Mark'Class (Mark).Mark.Element.Line'Img & ":"
                   & GPS_Mark'Class (Mark).Mark.Element.Column'Img);
      end;

      return New_Cursor;
   end Get_Current_Cursor;

   ----------
   -- Undo --
   ----------

   overriding procedure Undo (This : in out Console_Interface) is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (This.Get_File_Name);
   begin
      Editor.Undo;
   end Undo;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class;
      Len    : Natural) return String
   is
      Line : constant String := Get_Line (This, Cursor, 1);
      Char_Ind : constant String_Index_Type :=
        To_Char_Index (Get_Column (Cursor), Line);
   begin
      return Line
        (Natural (Char_Ind) .. Natural (Char_Ind) + Len - 1);
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class) return Character
   is
      Line : constant String := Get_Line (This, Cursor, 1);
      Char_Ind : constant String_Index_Type :=
        To_Char_Index (Get_Column (Cursor), Line);
   begin
      return Line (Natural (Char_Ind));
   end Get;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (This      : Console_Interface;
      Cursor    : Text_Cursor'Class;
      Start_Col : Visible_Column_Type := 0) return String
   is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (This.Get_File_Name);
      Loc_Start : constant Editor_Location'CLass :=
        Editor.New_Location_At_Line (Cursor.Get_Line);
      Loc_End   : constant Editor_Location'CLass := Loc_Start.End_Of_Line;

      Line : constant String := Editor.Get_Chars (Loc_Start, Loc_End);
      Char_Ind : String_Index_Type;

      Last_Ind : Integer := Line'Last;
   begin
      if Start_Col = 0 then
         Char_Ind := To_Char_Index (Get_Column (Cursor), Line);
      else
         Char_Ind := To_Char_Index (Start_Col, Line);
      end if;

      while Last_Ind >= Line'First and then Line (Last_Ind) = ASCII.LF loop
         Last_Ind := Last_Ind - 1;
      end loop;

      return Line (Natural (Char_Ind) .. Last_Ind);
   end Get_Line;

   -------------
   -- Replace --
   -------------

   overriding procedure Replace
     (This      : in out Console_Interface;
      Cursor    : Text_Cursor'Class;
      Len       : Natural;
      New_Value : String)
   is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (Get_File_Name (This));

      Actual_Start_Line : Integer;
      Actual_Start_Column : Visible_Column_Type;
   begin
      Text_Has_Changed (This);

      if Get_Line (Cursor) /= 0 then
         Actual_Start_Line := Cursor.Get_Line;
         Actual_Start_Column := Cursor.Get_Column;
      else
         Actual_Start_Line := 1;
         Actual_Start_Column := 1;
      end if;

      declare
         Loc_Start : constant Editor_Location'Class :=
           Editor.New_Location
             (Actual_Start_Line, Actual_Start_Column);
      begin
         if Len /= 0 then
            declare
               Loc_End : constant Editor_Location'Class :=
                 Loc_Start.Forward_Char (Len - 1);
            begin
               Editor.Delete (Loc_Start, Loc_End);
            end;
         end if;

         Editor.Insert (Loc_Start, New_Value);
      end;
   end Replace;

   -------------
   -- Replace --
   -------------

   overriding procedure Replace
     (This         : in out Console_Interface;
      Start_Cursor : Text_Cursor'Class;
      End_Cursor   : Text_Cursor'Class;
      New_Value    : String)
   is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (Get_File_Name (This));
      Loc_Start : constant Editor_Location'Class :=
        Editor.New_Location
          (Start_Cursor.Get_Line, Start_Cursor.Get_Column);
      Loc_End : constant Editor_Location'Class :=
        Editor.New_Location
          (End_Cursor.Get_Line, End_Cursor.Get_Column);
   begin
      if not
        (Loc_Start.Line > Loc_End.Line
         or else
           (Loc_Start.Line = Loc_End.Line
            and then Loc_Start.Column > Loc_End.Column))
      then
         --  Loc start must be after Loc end, we don't delete null ranges.

         Editor.Delete (Loc_Start, Loc_End);
      end if;

      Editor.Insert (Loc_Start, New_Value);
      Text_Has_Changed (This);
   end Replace;

   --------------
   -- Add_Line --
   --------------

   overriding procedure Add_Line
     (This     : in out Console_Interface;
      Cursor   : Text_Cursor'Class;
      New_Line : String;
      Indent   : Boolean := False)
   is
      Insert_Position : Text_Cursor := Text_Cursor (Cursor);
   begin
      Text_Has_Changed (This);

      Set_Location (Insert_Position, Get_Line (Insert_Position), 1);

      if Get_Line (Cursor) = 0 then
         Replace (This, Insert_Position, 0, New_Line & EOL_Str);
      else
         declare
            Line_Str : constant String := Get_Line (This, Insert_Position);
         begin
            Set_Location
              (Insert_Position,
               Get_Line (Insert_Position),
               To_Column_Index
                 (String_Index_Type (Line_Str'Last), Line_Str) + 1);
            Replace (This, Insert_Position, 0, EOL_Str & New_Line);
         end;
      end if;

      if Indent then
         declare
            Line_Cursor : Text_Cursor := Text_Cursor (Cursor);
         begin
            Line_Cursor.Set_Location
              (Line_Cursor.Get_Line + 1, 1);

            This.Indent_Line (Line_Cursor);
         end;
      end if;
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   overriding procedure Delete_Line
     (This   : in out Console_Interface;
      Cursor : Text_Cursor'Class)
   is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (Get_File_Name (This));
      Loc_Start : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Cursor.Get_Line);
      Loc_End : constant Editor_Location'Class := Loc_Start.End_Of_Line;
   begin
      Editor.Delete (Loc_Start, Loc_End);
      Text_Has_Changed (This);
   end Delete_Line;

   -----------------
   -- Indent_Line --
   -----------------

   overriding procedure Indent_Line
     (This : in out Console_Interface;
      Cursor : Text_Cursor'Class)
   is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (Get_File_Name (This));
      Loc : constant Editor_Location'Class :=
        Editor.New_Location_At_Line (Cursor.Get_Line);
   begin
      Editor.Indent (Loc, Loc);
      Text_Has_Changed (This);
   end Indent_Line;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (This : in out Console_Interface;
      Path : GNATCOLL.VFS.Virtual_File)
   is
   begin
      Initialize (Text_Interface (This), Path);
   end Initialize;

   ---------------
   -- Read_File --
   ---------------

   overriding function Read_File
     (This : Console_Interface) return Unbounded_String
   is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (Get_File_Name (This));

   begin
      return To_Unbounded_String (Editor.Get_Chars);
   end Read_File;

   --------------
   -- Line_Max --
   --------------

   overriding function Line_Max (This : Console_Interface) return Natural is
      Editor : constant Editor_Buffer'Class :=
        This.Kernel.Get_Buffer_Factory.Get (Get_File_Name (This));
   begin
      return Editor.Lines_Count;
   end Line_Max;

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

   overriding procedure Constrain_Update (This : in out Console_Interface) is
   begin
      null;
   end Constrain_Update;

end Codefix.GPS_Io;
