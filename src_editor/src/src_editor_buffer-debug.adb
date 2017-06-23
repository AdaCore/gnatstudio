------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2017, AdaCore                     --
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

with Ada.Tags;

with GNAT.Strings;            use GNAT.Strings;

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Glib.Object;             use Glib.Object;

with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;

with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.Messages;     use GPS.Kernel.Messages;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;      use Gtk.Text_Tag_Table;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Language;                use Language;
with Commands.Editor;         use Commands.Editor;

with Src_Editor_Module.Line_Highlighting;
use Src_Editor_Module.Line_Highlighting;

package body Src_Editor_Buffer.Debug is

   Me : constant Trace_Handle := Create ("buffer_debug", GNATCOLL.Traces.Off);

   procedure Buffer_Cmds (Data : in out Callback_Data'Class; Command : String);
   --  Command handler for the EditorBuffer class

   procedure Get_Buffer
     (Buffer : in out Source_Buffer;
      Data   : in out Callback_Data'Class;
      Arg    : Positive);
   --  Set the Buffer variable appropriately, or null if the buffer could
   --  not be found or is no longer valid.
   --  If the buffer is no longer valid, null is returned and an error msg is
   --  set in Data.

   function Dump_Text_For_Tag
     (Buffer : Source_Buffer; Tag : Gtk_Text_Tag) return String;
   --  Dump text in buffer, replacing graphic characters with "." unless they
   --  are in tag Tag, in which case replace them with "#".

   function To_String (Action : Line_Information_Access) return String;
   function To_String (Info : Line_Info_Width_Array_Access) return String;
   function To_String (Info : Line_Data_Record) return String;
   function To_String (X : GNAT.Strings.String_Access) return String;
   function To_String (X : Message_Access) return String;
   function To_String (X : Command_Access) return String;
   --  Utility functions

   function I (X : Integer) return String;
   function I (X : Editable_Line_Type) return String;
   function I (X : Buffer_Line_Type) return String;
   --  Image functions

   -------
   -- I --
   -------

   function I (X : Integer) return String is
      B : constant String := X'Img;
   begin
      return B (B'First + 1 .. B'Last);
   end I;

   function I (X : Editable_Line_Type) return String is
   begin
      return I (Integer (X));
   end I;

   function I (X : Buffer_Line_Type) return String is
   begin
      return I (Integer (X));
   end I;

   ---------------
   -- To_String --
   ---------------

   function To_String (Action : Line_Information_Access) return String is
   begin
      if Action = null then
         return "";
      else
         if Action.Associated_Command = null then
            return To_String (Action.Text) &
              To_String (Action.Tooltip_Text);
         else
            return Ada.Tags.External_Tag (Action.Associated_Command.all'Tag);
         end if;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Command_Access) return String is
   begin
      if X = null then
         return "NULL";
      else
         return Name (X);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Message_Access) return String is
   begin
      if X = null or X.Get_Action = null then
         return "NULL";
      else
         return "Text: " & To_String (X.Get_Action.Text) & ", "
           & "Tooltip: " & To_String (X.Get_Action.Tooltip_Text) & ", "
           & "Image: " & To_String (X.Get_Action.Image) & ", "
           & "Command: " & To_String (X.Get_Action.Associated_Command);
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : GNAT.Strings.String_Access) return String is
   begin
      if X = null then
         return "NULL";
      else
         return "'" & X.all & "'";
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Info : Line_Info_Width_Array_Access) return String is
      Res : Unbounded_String;

   begin
      if Info = null then
         return "NULL";
      end if;

      for J in Info'Range loop
         if Info (J).Messages.Is_Empty then
            Res := Res & "#"
              & ", "
              & To_String (Info (J).Action)
              & "#";
         else
            Res := Res & "#"
              & To_String (Info (J).Messages.First_Element.Message)
              & ", "
              & "#";
         end if;
      end loop;

      return To_String (Res);
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Info : Line_Data_Record) return String is
   begin
      return "el:" & I (Info.Editable_Line);
   end To_String;

   function Dump_Text_For_Tag
     (Buffer : Source_Buffer; Tag : Gtk_Text_Tag) return String
   is
      Iter : Gtk_Text_Iter;
      R    : Unbounded_String;
      C    : Character;
      Success : Boolean;
   begin
      Get_Start_Iter (Buffer, Iter);

      while not Is_End (Iter) loop
         C := Get_Char (Iter);

         if Is_Graphic (C) then
            if Has_Tag (Iter, Tag) then
               Append (R, '#');
            else
               Append (R, '.');
            end if;
         else
            Append (R, C);
         end if;

         Forward_Char (Iter, Success);
         exit when not Success;
      end loop;

      return To_String (R);
   end Dump_Text_For_Tag;

   -----------------
   -- Buffer_Cmds --
   -----------------

   procedure Buffer_Cmds (Data : in out Callback_Data'Class; Command : String)
   is
      --  Kernel      : constant Kernel_Handle := Get_Kernel (Data);
      Buffer      : Source_Buffer;

      function Recurse_Explore
        (L : Editable_Line_Type; Level : Natural) return Editable_Line_Type;
      --  Return the last line that has actually been explored.

      function Recurse_Explore
        (L : Editable_Line_Type; Level : Natural) return Editable_Line_Type
      is
         use Lines_List;
         C        : Lines_List.Cursor;
         U        : Universal_Line;
         Prev     : Editable_Line_Type;
         R        : Unbounded_String;
      begin
         R := To_Unbounded_String ("[" & I (Level) & "] el:" & I (L));

         if Buffer.Editable_Lines (L).Stored_Editable_Lines /= 0 then
            R := R & " (" &
              I (Buffer.Editable_Lines (L).Stored_Editable_Lines) & ")";
         end if;

         Set_Return_Value (Data, To_String (R));

         Prev := L;

         C := Buffer.Editable_Lines (L).Stored_Lines.First;

         while Has_Element (C) loop
            U := Element (C);

            if U.Nature = Editable then
               Prev := Recurse_Explore (Prev + 1, Level + 1);
            else
               Set_Return_Value
                 (Data, "[" & I (Level + 1) & "] special: " & U.Text.all);
            end if;

            Next (C);
         end loop;

         return Prev;
      end Recurse_Explore;

      Dummy : Editable_Line_Type;
      pragma Unreferenced (Dummy);

   begin
      Get_Buffer (Buffer, Data, 1);

      if Command = "debug_dump_editable_lines" then
         Set_Return_Value_As_List (Data);

         for Line in 1 .. Buffer.Last_Editable_Line loop
            if Buffer.Editable_Lines (Line).Where = In_Buffer then
               Set_Return_Value
                 (Data, "bl:" & I (Buffer.Editable_Lines (Line).Buffer_Line));
            else
               Set_Return_Value (Data, String'("[hidden]"));
            end if;
         end loop;

      elsif Command = "debug_dump_buffer_lines" then
         Set_Return_Value_As_List (Data);

         for Line in Buffer.Line_Data'First .. Buffer.Line_Data'First +
           Buffer_Line_Type (Get_Line_Count (Buffer) - 1)
         loop
            Set_Return_Value
              (Data, "[" & I (Line) & "] " &
               (if Buffer.Line_Data (Line).Side_Info_Data = null
                  then "null"
                  else "<>") &
                 ":" & I (Buffer.Line_Data (Line).Editable_Line) &
                 ":" & (if Buffer.Line_Data (Line).Line_Mark = null
                        then "null"
                        else "<>") &
                 ":" & I (Integer (Buffer.Line_Data (Line).File_Line)) &
                 ":" & I (Integer
                          (Buffer.Line_Data (Line).Highlighting'Length)));
         end loop;

      elsif Command = "debug_dump_side_info" then
         Set_Return_Value_As_List (Data);

         for Line in Buffer.Line_Data'Range loop
            Set_Return_Value
              (Data, To_String (Buffer.Line_Data (Line).Side_Info_Data));
         end loop;

      elsif Command = "debug_dump_line_highlighting" then
         Set_Return_Value_As_List (Data);

         for Line in Buffer.Line_Data'First .. Buffer.Line_Data'First +
           Buffer_Line_Type (Get_Line_Count (Buffer) - 1)
         loop
            Set_Return_Value
              (Data,
               Get_Name
                 (Buffer.Line_Data (Line).Highlighting
                    (Highlight_Editor).Active));
         end loop;

      elsif Command = "debug_dump_all_lines" then
         Set_Return_Value_As_List (Data);

         for Line in Buffer.Line_Data'First .. Buffer.Line_Data'First +
           Buffer_Line_Type (Get_Line_Count (Buffer) - 1)
         loop
            if Buffer.Line_Data (Line).Editable_Line /= 0 then
               Dummy := Recurse_Explore
                 (Buffer.Line_Data (Line).Editable_Line, 0);
            else
               Set_Return_Value
                 (Data, "[0] special:" & To_String (Buffer.Line_Data (Line)));
            end if;
         end loop;

      elsif Command = "debug_dump_syntax_highlighting" then
         declare
            Tag : Gtk_Text_Tag;
         begin
            declare
            begin
               --  Check whether the parameter is a syntax tag
               Tag := Buffer.Syntax_Tags
                 (Language_Entity'Value (Nth_Arg (Data, 2)));
            exception
               when Constraint_Error =>
                  --  Try to find the parameter in the syntax highlighting tags
                  Tag := Lookup (Get_Tag_Table (Buffer), Nth_Arg (Data, 2));
            end;

            if Tag /= null then
               Set_Return_Value (Data, Dump_Text_For_Tag (Buffer, Tag));
            end if;
         end;

      elsif Command = "debug_dump_side_info_config" then
         Set_Return_Value_As_List (Data);

         if Buffer.Editable_Line_Info_Columns /= null
           and then Buffer.Editable_Line_Info_Columns.all /= null
         then
            for L in Buffer.Editable_Line_Info_Columns.all'Range loop
               Set_Return_Value
                 (Data,
                  To_String
                    (Buffer.Editable_Line_Info_Columns.all (L).Identifier));
            end loop;
         end if;

      elsif Command = "debug_dump_undo_queue"
        or else Command = "debug_dump_redo_queue"
      then
         Set_Return_Value_As_List (Data);

         declare
            use Command_Lists;
            Q : List;
            N : Command_Lists.Cursor;
            C : Command_Access;
         begin
            if Command = "debug_dump_undo_queue" then
               Q := Debug_Get_Undo_Queue (Buffer.Queue);
            else
               Q := Debug_Get_Redo_Queue (Buffer.Queue);
            end if;

            N := First (Q);

            while Has_Element (N) loop
               C := Element (N);

               if C.all in Base_Editor_Command_Type'Class then
                  Set_Return_Value
                    (Data,
                     "[" & I (Debug_Get_Group (C)) & "]" &
                     Debug_String (Base_Editor_Command_Type'Class (C.all)));
               else
                  Set_Return_Value (Data, String'("not an editor command!"));
               end if;

               N := Next (N);
            end loop;
         end;
      end if;
   end Buffer_Cmds;

   ----------------
   -- Get_Buffer --
   ----------------

   procedure Get_Buffer
     (Buffer : in out Source_Buffer;
      Data   : in out Callback_Data'Class;
      Arg    : Positive)
   is
      EditorBuffer : constant Class_Type :=
                       New_Class (Get_Kernel (Data), "EditorBuffer");
      Inst : constant Class_Instance := Nth_Arg (Data, Arg, EditorBuffer);
   begin
      Buffer := Source_Buffer (GObject'(Get_Data (Inst)));
      if Buffer = null then
         Set_Error_Msg (Data, "No associated buffer");
      end if;
   end Get_Buffer;

   --------------
   -- Register --
   --------------

   procedure Register
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      EditorBuffer : constant Class_Type := New_Class (Kernel, "EditorBuffer");
   begin
      if not Active (Me) then
         return;
      end if;

      Register_Command
        (Kernel, "debug_dump_editable_lines",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_side_info",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_side_info_config",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_all_lines",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_buffer_lines",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_syntax_highlighting",
         1, 1, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_line_highlighting",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_undo_queue",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_redo_queue",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
   end Register;

end Src_Editor_Buffer.Debug;
