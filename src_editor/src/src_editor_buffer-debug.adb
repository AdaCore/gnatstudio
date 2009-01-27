-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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

with GNAT.Strings; use GNAT.Strings;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib.Object; use Glib.Object;
with Gdk.Pixbuf;  use Gdk.Pixbuf;

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts.Gtkada;

with GPS.Kernel;         use GPS.Kernel;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;

package body Src_Editor_Buffer.Debug is

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

   function To_String (Info : Line_Info_Width_Array_Access) return String;
   function To_String (Info : Line_Data_Record) return String;
   function To_String (X : GNAT.Strings.String_Access) return String;
   function To_String (X : Line_Information_Access) return String;
   function To_String (X : Gdk_Pixbuf) return String;
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

   function To_String (X : Gdk_Pixbuf) return String is
   begin
      if X = null then
         return "NULL";
      else
         return "<image>";
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (X : Line_Information_Access) return String is
   begin
      if X = null then
         return "NULL";
      else
         return "Text: " & To_String (X.Text) & ", "
           & "Tooltip: " & To_String (X.Tooltip_Text) & ", "
           & "Image: " & To_String (X.Image) & ", "
           & "Command: " & To_String (X.Associated_Command);
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
         Res := Res & "#"
           & To_String (Info (J).Info) & ", " & I (Info (J).Width)
           & "#";
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
         C        : Cursor;
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
               Set_Return_Value (Data, "[hidden]");
            end if;
         end loop;

      elsif Command = "debug_dump_buffer_lines" then
         Set_Return_Value_As_List (Data);

         for Line in Buffer.Line_Data'First .. Buffer.Line_Data'First +
           Buffer_Line_Type (Get_Line_Count (Buffer) - 1)
         loop
            Set_Return_Value
              (Data, To_String (Buffer.Line_Data (Line)));
         end loop;

      elsif Command = "debug_dump_side_info" then
         Set_Return_Value_As_List (Data);

         for Line in 1 .. Buffer.Last_Editable_Line loop
            Set_Return_Value
              (Data, To_String (Buffer.Editable_Lines (Line).Side_Info_Data));
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
      Register_Command
        (Kernel, "debug_dump_editable_lines",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_side_info",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_all_lines",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
      Register_Command
        (Kernel, "debug_dump_buffer_lines",
         0, 0, Buffer_Cmds'Access, EditorBuffer);
   end Register;

end Src_Editor_Buffer.Debug;
