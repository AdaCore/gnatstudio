-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Glide_Kernel.Modules;  use Glide_Kernel.Modules;
with Glide_Kernel.Project;  use Glide_Kernel.Project;

with String_Utils; use String_Utils;

package body Codefix.GPS_Io is

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
   begin
      Result.Id := new String'
        (Interpret_Command
           (Current_Text.Kernel,
            "create_mark -l" & Natural'Image (Cursor.Line)
              & " -c" & Natural'Image (Cursor.Col) & " -L 1 "
              & Find_Source_File
                (Current_Text.Kernel, Get_File_Name (Current_Text))));

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
   begin
      Assign (New_Cursor.File_Name, Get_File_Name (Current_Text));
      New_Cursor.Col := Natural'Value
        (Interpret_Command
           (Current_Text.Kernel,
            "get_column " & GPS_Mark (Mark).Id.all));

      New_Cursor.Line := Natural'Value
        (Interpret_Command
           (Current_Text.Kernel,
            "get_line " & GPS_Mark (Mark).Id.all));

      return New_Cursor;
   end Get_Current_Cursor;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out GPS_Mark) is
   begin
      Free (This.Id);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Console_Interface) is
   begin
      Free (Text_Interface (This));
   end Free;

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
      Number : Positive) return String
   is
      Node : String_List.List_Node;
   begin
      Update (This);

      Node := First (This.Lines.all);

      for J in 1 .. Number - 1 loop
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
      New_Value : String)
   is
      Garbage : Dynamic_String;
   begin
      This.File_Modified.all := True; --  ??? To be deleted with the callback

      if Cursor.Line /= 0 then
         Garbage := new String'
           (Interpret_Command
              (This.Kernel,
               "replace_text -l" & Natural'Image (Cursor.Line)
               & " -c" & Natural'Image (Cursor.Col) & " "
               & Find_Source_File
                   (This.Kernel, Get_File_Name (This))
               & " -a" & Natural'Image (Len)
               & " -b 0"
               & " """ & New_Value & """"));
      else
         Garbage := new String'
           (Interpret_Command
              (This.Kernel,
               "replace_text -l 1"
               & " -c 1 "
               & Find_Source_File
                   (This.Kernel, Get_File_Name (This))
               & " -a 0"
               & " -b 0"
               & " """ & New_Value & """"));
      end if;
      --  ??? Be carefull !!! In New_Value, if there is any cote,
      --  the command will be bad interpreted !!! Waiting to know the
      --  escape character before fixing this bug.
      Free (Garbage);
   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This        : in out Console_Interface;
      Cursor      : Text_Cursor'Class;
      New_Line    : String)
   is
      Line_Str        : Dynamic_String;
      Insert_Position : Text_Cursor := Text_Cursor (Cursor);
   begin
      This.File_Modified.all := True; --  ??? To be deleted with the callback

      Insert_Position.Col := 1;

      if Cursor.Line = 0 then
         Replace (This, Insert_Position, 0, New_Line & EOL_Str);
      else
         Line_Str := new String'(Get_Line (This, Insert_Position));
         Insert_Position.Col := Line_Str'Last + 1;
         Replace (This, Insert_Position, 0, EOL_Str & New_Line);
      end if;
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This : in out Console_Interface;
      Cursor : Text_Cursor'Class)
   is
      Garbage : Dynamic_String;
   begin
      This.File_Modified.all := True; --  ??? To be deleted with the callback

      Garbage := new String'
        (Interpret_Command
           (This.Kernel,
            "replace_text -l" & Natural'Image (Cursor.Line)
            & " -c" & Natural'Image (Cursor.Col) & " "
            & Find_Source_File
                (This.Kernel, Get_File_Name (This))
            & " """""));
      Free (Garbage);
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

   function Read_File (This : Console_Interface) return Dynamic_String is
   begin

      Put_Line (Find_Source_File
                  (This.Kernel, Get_File_Name (This))); --  ??? Debug ?

      return new String'
        (Interpret_Command
           (This.Kernel, "get_buffer " & Find_Source_File
              (This.Kernel, Get_File_Name (This))));
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
      File          : Dynamic_String;
      Current_Index : Natural := 0;
      Old_Index     : Natural := 0;
      Last_Line     : Dynamic_String;
   begin
      if not This.File_Modified.all then
         return;
      end if;

      Free (This.Lines.all);

      File := Read_File (This);

      Current_Index := File'First;
      Old_Index := Current_Index;

      --  ??? In future, this operation should be made by diff utils

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

      This.Lines_Number.all := Natural'Value
        (Interpret_Command
           (This.Kernel,
            "get_last_line "
            & Find_Source_File
                (This.Kernel, Get_File_Name (This))));


      This.File_Modified.all := False;

   end Update;

   ----------------
   -- Set_Kernel --
   ----------------

   procedure Set_Kernel
     (This : in out Console_Interface; Kernel : Kernel_Handle) is
   begin
      This.Kernel := Kernel;
   end Set_Kernel;

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
        (This.Errors_Buffer, Interpret_Command (Kernel, "get_build_output"));
   end Get_Last_Output;


end Codefix.GPS_Io;
