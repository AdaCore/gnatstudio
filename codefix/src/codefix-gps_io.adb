-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2003                    --
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

with Glide_Kernel.Project;  use Glide_Kernel.Project;
with Glide_Kernel.Scripts;  use Glide_Kernel.Scripts;
with Glide_Kernel.Console;  use Glide_Kernel.Console;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with String_List_Utils;
with String_Utils;          use String_Utils;
with Projects.Registry;     use Projects.Registry;
with Basic_Types;           use Basic_Types;
with Traces;                use Traces;

package body Codefix.GPS_Io is

   Me : constant Debug_Handle := Create ("Codefix.GPS_IO");

   package SL renames String_List_Utils.String_List;

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
      Cursor       : Text_Cursor'Class) return Mark_Abstr'Class
   is
      Result : GPS_Mark;
      Args   : Argument_List :=
        (1 => new String'
           (Get_Full_Path_From_File
            (Registry        => Get_Registry (Current_Text.Kernel),
             Filename        => Get_File_Name (Current_Text),
             Use_Source_Path => True,
             Use_Object_Path => False)),
         2 => new String'(Image (Cursor.Line)),
         3 => new String'(Image (Cursor.Col)),
         4 => new String'("0"));
   begin
      Result.Id := new String'
        (Execute_GPS_Shell_Command (Current_Text.Kernel, "create_mark", Args));
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
      Args : Argument_List (1 .. 1);

   begin
      Assign (New_Cursor.File_Name, Get_File_Name (Current_Text));
      Args (1) := new String'(GPS_Mark (Mark).Id.all);

      declare
         Column : constant String :=
           Execute_GPS_Shell_Command (Current_Text.Kernel, "get_column", Args);
         Line : constant String :=
           Execute_GPS_Shell_Command (Current_Text.Kernel, "get_line", Args);

      begin
         New_Cursor.Col := Natural'Value (Column);
         New_Cursor.Col := Natural'Value (Line);

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
      Args : Argument_List := (1 => new String'(Get_File_Name (This)));
      Ignore : constant String := Execute_GPS_Shell_Command
        (This.Kernel, "undo", Args);
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
      Len    : Natural) return String is
   begin
      return Get_Recorded_Line (This, Cursor.Line)
        (Cursor.Col .. Cursor.Col + Len - 1);
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This   : Console_Interface;
      Cursor : Text_Cursor'Class) return String
   is
      Line : constant String := Get_Recorded_Line (This, Cursor.Line);
   begin
      return Line (Cursor.Col .. Line'Last);
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
         if Node = Null_Node then
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

      if Cursor.Line /= 0 then
         declare
            Args : Argument_List :=
              (1 => new String'
                 (Get_Full_Path_From_File
                  (Registry        => Get_Registry (This.Kernel),
                   Filename        => Get_File_Name (This),
                   Use_Source_Path => True,
                   Use_Object_Path => False)),
               2 => new String'(Image (Cursor.Line)),
               3 => new String'(Image (Cursor.Col)),
               4 => new String'(New_Value),
               5 => new String'("0"),           --  before
               6 => new String'(Image (Len)));  --  after
            S : constant String :=
              Execute_GPS_Shell_Command (This.Kernel, "replace_text", Args);
         begin
            if S /= "" then
               Insert (This.Kernel, S, True, Error);
            end if;

            Free (Args);
         end;

      else
         declare
            Args : Argument_List :=
              (1 => new String'
                 (Get_Full_Path_From_File
                  (Registry        => Get_Registry (This.Kernel),
                   Filename        => Get_File_Name (This),
                   Use_Source_Path => True,
                   Use_Object_Path => False)),
               2 => new String'("1"),  --  line
               3 => new String'("1"),  --  column
               4 => new String'(New_Value),
               5 => new String'("0"),  --  before
               6 => new String'("0")); --  after
            S : constant String :=
              Execute_GPS_Shell_Command (This.Kernel, "replace_text", Args);
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
      Line_Str        : GNAT.OS_Lib.String_Access;
      Insert_Position : Text_Cursor := Text_Cursor (Cursor);
   begin
      This.File_Modified.all := True;
      Text_Has_Changed (This);

      Insert_Position.Col := 1;

      if Cursor.Line = 0 then
         Replace (This, Insert_Position, 0, New_Line & EOL_Str);
      else
         Line_Str := new String'(Get_Line (This, Insert_Position));
         Insert_Position.Col := Line_Str'Last + 1;
         Replace (This, Insert_Position, 0, EOL_Str & New_Line);
         Free (Line_Str);
      end if;
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This : in out Console_Interface;
      Cursor : Text_Cursor'Class)
   is
      Args : Argument_List :=
        (1 => new String'
           (Get_Full_Path_From_File
            (Registry        => Get_Registry (This.Kernel),
             Filename        => Get_File_Name (This),
             Use_Source_Path => True,
             Use_Object_Path => False)),
         2 => new String'(Image (Cursor.Line)),
         3 => new String'(Image (Cursor.Col)),
         4 => new String'(""));  --  replacement text
      S : constant String :=
        Execute_GPS_Shell_Command (This.Kernel, "replace_text", Args);
   begin
      This.File_Modified.all := True;
      Text_Has_Changed (This);

      if S /= "" then
         Insert (This.Kernel, S, True, Error);
      end if;

      Free (Args);
   end Delete_Line;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : in out Console_Interface;
      Path : String)
   is
      pragma Unreferenced (This, Path);
   begin
      null;
   end Initialize;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (This : Console_Interface)
      return GNAT.OS_Lib.String_Access
   is
      Args : Argument_List :=
        (1 => new String'(Get_Full_Path_From_File
           (Registry        => Get_Registry (This.Kernel),
            Filename        => Get_File_Name (This),
            Use_Source_Path => True,
            Use_Object_Path => False)));
      S    : constant GNAT.OS_Lib.String_Access := new String'
        (Execute_GPS_Shell_Command (This.Kernel, "get_buffer", Args));
   begin
      Free (Args);
      return S;
   end Read_File;

   ------------
   -- Commit --
   ------------

   procedure Commit (This : Console_Interface) is
      pragma Unreferenced (This);
   begin
      null;
   end Commit;

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
      File          : GNAT.OS_Lib.String_Access;
      Current_Index : Natural := 0;
      Old_Index     : Natural := 0;
      Last_Line     : GNAT.OS_Lib.String_Access;

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
         Args : Argument_List :=
           (1 => new String'
              (Get_Full_Path_From_File
               (Registry        => Get_Registry (This.Kernel),
                Filename        => Get_File_Name (This),
                Use_Source_Path => True,
                Use_Object_Path => False)));
         Result : constant String :=
           Execute_GPS_Shell_Command (This.Kernel, "get_last_line", Args);

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

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Compilation_Output) is
   begin
      Free (This.Errors_Buffer);
   end Free;

   ------------------------
   -- Get_Direct_Message --
   ------------------------

   procedure Get_Direct_Message
     (This    : in out Compilation_Output;
      Current : out Error_Message)
   is
      Last_Index : constant Natural := This.Current_Index;
   begin
      Skip_To_Char
        (This.Errors_Buffer.all,
         This.Current_Index,
         ASCII.LF);

      Initialize
        (Current, This.Errors_Buffer (Last_Index .. This.Current_Index - 1));

      if Current /= Invalid_Error_Message then
         Assign
           (Current.File_Name, Get_Full_Path_From_File
            (Registry        => Get_Registry (This.Kernel),
             Filename        => Current.File_Name.all,
             Use_Source_Path => True,
             Use_Object_Path => False));
      end if;

      This.Current_Index := This.Current_Index + 1;
   end Get_Direct_Message;

   ----------------------
   -- No_More_Messages --
   ----------------------

   function No_More_Messages (This : Compilation_Output) return Boolean is
   begin
      if This.Errors_Buffer /= null then
         return This.Current_Index >= This.Errors_Buffer'Last;
      else
         return True;
      end if;
   end No_More_Messages;

   ---------------------
   -- Get_Last_Output --
   ---------------------

   procedure Get_Last_Output
     (This : in out Compilation_Output; Kernel : Kernel_Handle) is
   begin
      Assign
        (This.Errors_Buffer,
         Execute_GPS_Shell_Command (Kernel, "get_build_output"));
      This.Kernel := Kernel;
   end Get_Last_Output;

end Codefix.GPS_Io;
