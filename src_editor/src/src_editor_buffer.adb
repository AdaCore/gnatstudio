-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Glib.Unicode;              use Glib.Unicode;
with Glib.Values;               use Glib.Values;
with Gdk.Color;                 use Gdk.Color;
with Gtk;                       use Gtk;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_Tag;              use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;        use Gtk.Text_Tag_Table;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Types;              use Gtkada.Types;

with Basic_Types;               use Basic_Types;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Src_Highlighting;          use Src_Highlighting;

with Interfaces.C.Strings;      use Interfaces.C.Strings;
with System;
with File_Utils;                use File_Utils;
with Src_Info;                  use Src_Info;
with Glide_Intl;                use Glide_Intl;

with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Commands.Editor;           use Commands.Editor;
with Commands.Controls;         use Commands.Controls;
with Src_Editor_Module;         use Src_Editor_Module;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Calendar;              use Ada.Calendar;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with Src_Editor_View;           use Src_Editor_View;

with Pango.Font;                use Pango.Font;

with String_List_Utils;         use String_List_Utils;
with Ada.Unchecked_Deallocation;
with VFS;                       use VFS;

with Src_Editor_Module.Line_Highlighting;

with Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Line_Information;

package body Src_Editor_Buffer is

   use Src_Editor_Buffer.Blocks;
   use Src_Editor_Module.Line_Highlighting;
   use Src_Editor_Buffer.Line_Information;
   use type System.Address;

   Me : constant Debug_Handle := Create ("Source_Editor_Buffer");

   Buffer_Recompute_Interval : constant Guint32 := 200;
   --  The interval at which to check whether the buffer should be reparsed,
   --  in milliseconds.

   Buffer_Recompute_Delay    : constant Duration := 1.0;
   --  The delay between the last edit and the re-parsing of the buffer,
   --  in seconds.

   package Buffer_Timeout is new Gtk.Main.Timeout (Source_Buffer);

   type Delimiter_Type is (Opening, Closing);

   function Strlen
     (Str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.size_t;
   pragma Import (C, Strlen);

   Delimiters : constant array (1 .. 3, Delimiter_Type'Range) of Character
     := (('(', ')'),
         ('[', ']'),
         ('{', '}'));
   --  ??? Should we get that from the language ?

   --------------------
   -- Signal Support --
   --------------------

   Class_Record : GObject_Class := Uninitialized_Class;
   --  A pointer to the 'class record'.

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String ("cursor_position_changed"),
      2 => New_String ("side_column_changed"),
      3 => New_String ("side_column_configuration_changed"),
      4 => New_String ("line_highlights_changed"),
      5 => New_String ("status_changed"),
      6 => New_String ("buffer_information_changed"));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (GType_Int, GType_Int),
      2 => (GType_None, GType_None),
      3 => (GType_None, GType_None),
      4 => (GType_None, GType_None),
      5 => (GType_None, GType_None),
      6 => (GType_None, GType_None));
   --  The parameters associated to each new signal

   package Buffer_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Source_Buffer_Record);

   --------------------------
   -- Forward declarations --
   --------------------------

   procedure Changed_Handler
     (Buffer : access Source_Buffer_Record'Class);
   --  This procedure is used to signal to the clients that the insert
   --  cursor position may have changed by emitting the
   --  "cursor_position_changed" signal.

   procedure Line_Highlights_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "Line_Highlights_Changed" signal.

   procedure Mark_Set_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is used to signal to the clients that the insert
   --  cursor position has changed by emitting the "cursor_position_changed"
   --  signal. This signal is emitted only when the mark changed is the
   --  Insert_Mark.

   procedure Insert_Text_Cb
     (Buffer          : access Source_Buffer_Record'Class;
      End_Insert_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      Text            : String);
   --  This procedure recomputes the syntax-highlighting of the buffer
   --  in a semi-optimized manor, based on syntax-highlighting already
   --  done before the insertion and the text added.
   --
   --  This procedure assumes that the language has been set.

   procedure Insert_Text_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is just a proxy between the "insert_text" signal
   --  and the Insert_Text_Cb callback. It extracts the parameters from
   --  Params and then call Insert_Text_Cb. For efficiency reasons, the
   --  signal is processed only when Lang is not null and the language
   --  supports syntax highlighting.
   --
   --  Note that this handler is designed to be connected "after", in which
   --  case the Insert_Iter iterator is located at the end of the inserted
   --  text.

   procedure First_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  First handler connected to the "insert_text" signal.

   procedure Delete_Range_Cb
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter);
   --  This procedure recomputes the syntax-highlighting of the buffer
   --  in a semi-optimized manor, based on syntax-highlighting already
   --  done before the deletion and the location of deletion.
   --
   --  This procedure assumes that the language has been set.

   procedure Delete_Range_Before_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler connected to the "delete_range" signal, but which occurs before
   --  the actual deletion.

   procedure Delete_Range_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is just a proxy between the "delete_range" signal
   --  and the Delete_Range_Cb callback. It extracts the parameters from
   --  Params and then call Delete_Range_Cb. For efficiency reasons, the
   --  signal is processed only when Lang is not null and the language
   --  supports syntax highlighting.
   --
   --  Note that this handler is designed to be connected "after", in which
   --  case the Start and End iterators are equal, since the text between
   --  these iterators have already been deleted.

   procedure Emit_New_Cursor_Position
     (Buffer : access Source_Buffer_Record'Class);
   --  Signal the new cursor position by emitting the "cursor_position_changed"
   --  signal.

   procedure Highlight_Slice
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter);
   --  Re-compute the highlighting for at least the given region.
   --  If the text creates non-closed comments or string regions, then
   --  the re-highlighted area is automatically extended to the right.
   --  When the re-highlighted area is extended to the right, the extension
   --  is computed in a semi-intelligent fashion.

   procedure Kill_Highlighting
     (Buffer : access Source_Buffer_Record'Class;
      From   : Gtk_Text_Iter;
      To     : Gtk_Text_Iter);
   --  Remove all highlighting tags for the given region.

   procedure Buffer_Destroy (Buffer : access Source_Buffer_Record'Class);
   --  Free memory associated to Buffer.

   function Automatic_Save (Buffer : Source_Buffer) return Boolean;
   --  Handle automatic save of the buffer, using a timeout.

   procedure Internal_Save_To_File
     (Buffer   : Source_Buffer;
      Filename : VFS.Virtual_File;
      Internal : Boolean;
      Success  : out Boolean);
   --  Low level save function. Only writes the buffer contents on disk,
   --  with no modification on the buffer's settings.
   --  If Internal is True, do not emit kernel signals. This is used notably
   --  for automatic saves.

   procedure Preferences_Changed
     (Buffer : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

   procedure Cursor_Move_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the cursor moves.

   procedure User_Edit_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the user inserts or deletes text.

   procedure Lines_Remove_Hook_Before
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type);
   --  Actions that must be executed whenever a line is removed.
   --  This is called before the lines are actually removed from the buffer.

   procedure Lines_Remove_Hook_After
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type);
   --  Same as above, but occurs after the lines have been removed.

   procedure Lines_Add_Hook
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type);
   --  Actions that must be executed whenever a line is added.
   --  Start is the number of the line just before the lines were inserted,
   --  Number is the number of lines that were inserted.

   procedure End_Action_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever an action is ended.

   procedure Destroy_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed when the buffer is destroyed.

   procedure Initialize_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed after initialization.

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk_Text_Iter;
      Line   : Gint;
      Column : Gint);
   --  Return the iter at position (Line, Column), tab expansion included.

   procedure Clear (Buffer : access Source_Buffer_Record);
   --  Delete all characters from the given buffer, leaving an empty buffer.

   procedure Set_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Buffer_Line_Type;
      Id     : String;
      Set    : Boolean);
   --  Common function for [Add|Remove]_Line_Highlighting.

   procedure Request_Blocks (Buffer : access Source_Buffer_Record'Class);
   --  Request that the blocks be recomputed whenever there is time.

   function Check_Blocks (Buffer : Source_Buffer) return Boolean;
   --  Timeout that recomputes the blocks if needed.

   type Src_String is record
      Contents  : Unchecked_String_Access;
      Length    : Natural := 0;
      Read_Only : Boolean := False;
   end record;
   --  Special purpose string type to avoid extra copies and string allocation
   --  as much as possible.
   --  The actual contents of a Src_String is represented by
   --  Contents (1 .. Length) (as utf8-encoded string)
   --  Never use Free (Contents) directly, use the Free procedure below.

   procedure Free (S : in out Src_String);
   --  Free the memory associated with S.

   function Get_String
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return Src_String;
   --  Return the string at line Line, without the line terminator.
   --  Return null if the Line is not a valid line or there is no contents
   --  associated with the line.
   --  The caller is responsible for freeing the returned value..
   --  The returned string is UTF8-encoded

   function Get_First_Lines
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return GNAT.OS_Lib.String_Access;
   --  Return the text up to line Line.

   ----------
   -- Free --
   ----------

   procedure Free (S : in out Src_String) is
   begin
      if S.Read_Only then
         S.Contents := null;
      else
         Free (S.Contents);
      end if;
   end Free;

   ----------------
   -- Get_String --
   ----------------

   --  ??? See whether these two functions could be optimized further.

   function Get_String
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return Src_String
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
      Success              : Boolean;
      Result               : Src_String;
      Chars                : Interfaces.C.Strings.chars_ptr;

   begin
      if Line not in Buffer.Editable_Lines'Range then
         return Result;
      end if;

      case Buffer.Editable_Lines (Line).Where is
         when In_Buffer =>
            Get_Iter_At_Line
              (Buffer,
               Start_Iter,
               Gint (Buffer.Editable_Lines (Line).Buffer_Line - 1));
            Copy (Start_Iter, End_Iter);
            Forward_To_Line_End (End_Iter, Success);

            if Get_Line (Start_Iter) /= Get_Line (End_Iter) then
               return Result;
            end if;

            Chars := Get_Text (Buffer, Start_Iter, End_Iter, True);
            Result.Contents := To_Unchecked_String (Chars);
            Result.Length := Integer (Strlen (Chars));

         when In_Mark =>
            if Buffer.Editable_Lines (Line).Text /= null then
               Result.Read_Only := True;
               Result.Contents  := To_Unchecked_String
                 (Buffer.Editable_Lines (Line).Text.all'Address);
               Result.Length    := Result.Contents'Length;
            end if;
      end case;

      return Result;
   end Get_String;

   function Get_String
     (Buffer : Source_Buffer) return GNAT.OS_Lib.String_Access
   is
      Start, The_End : Gtk_Text_Iter;
      Result         : GNAT.OS_Lib.String_Access;
      Chars          : Interfaces.C.Strings.chars_ptr;

   begin
      if Lines_Are_Real (Buffer) then
         Get_Bounds (Buffer, Start, The_End);
         Chars := Get_Text (Buffer, Start, The_End, True);

         --  ??? Need to find a way to avoid the call to Value which is
         --  inneficient.

         Result := new String'(Value (Chars));
         Free (Chars);
         return Result;

      else
         return Get_First_Lines (Buffer, Buffer.Last_Editable_Line);
      end if;
   end Get_String;

   ---------------------
   -- Get_First_Lines --
   ---------------------

   function Get_First_Lines
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return GNAT.OS_Lib.String_Access
   is
      A      : array (Buffer.Editable_Lines'First .. Line) of Src_String;
      Len    : Integer := 0;
      Index  : Integer := 1;
      Output : GNAT.OS_Lib.String_Access;

   begin
      for J in A'Range loop
         A (J) := Get_String (Buffer, Editable_Line_Type (J));
         Len := Len + A (J).Length;
      end loop;

      Output := new String (1 .. Len + A'Length);

      for J in A'Range loop
         Len := A (J).Length;

         if Len /= 0 then
            Output (Index .. Index + Len - 1) := A (J).Contents (1 .. Len);
         end if;

         Output (Index + Len) := ASCII.LF;
         Index := Index + Len + 1;
         Free (A (J));
      end loop;

      return Output;
   end Get_First_Lines;

   ------------------
   -- Check_Blocks --
   ------------------

   function Check_Blocks (Buffer : Source_Buffer) return Boolean is
   begin
      if Clock < Buffer.Blocks_Request_Timestamp + Buffer_Recompute_Delay then
         return True;
      end if;

      Compute_Blocks (Buffer);
      Emit_New_Cursor_Position (Buffer);

      --  Unregister the timeout.
      Buffer.Blocks_Timeout_Registered := False;
      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Check_Blocks;

   --------------------
   -- Request_Blocks --
   --------------------

   procedure Request_Blocks (Buffer : access Source_Buffer_Record'Class) is
   begin
      Buffer.Blocks_Request_Timestamp := Clock;

      if not Buffer.Blocks_Timeout_Registered then
         Buffer.Blocks_Timeout_Registered := True;
         Buffer.Blocks_Timeout := Buffer_Timeout.Add
           (Buffer_Recompute_Interval,
            Check_Blocks'Access,
            Source_Buffer (Buffer));
      end if;
   end Request_Blocks;

   ---------------------
   -- Get_Buffer_Line --
   ---------------------

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type) return Buffer_Line_Type is
   begin
      if Line in Buffer.Editable_Lines'Range then
         if Buffer.Editable_Lines (Line).Where = In_Buffer then
            return Buffer.Editable_Lines (Line).Buffer_Line;
         end if;
      end if;

      return 0;
   end Get_Buffer_Line;

   -----------------------
   -- Get_Editable_Line --
   -----------------------

   function Get_Editable_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type) return Editable_Line_Type is
   begin
      if Buffer.Line_Data /= null
        and then Line in Buffer.Line_Data'Range
      then
         return Buffer.Line_Data (Line).Editable_Line;
      end if;

      return 0;
   end Get_Editable_Line;

   -----------------------
   -- Get_Editable_Line --
   -----------------------

   function Get_Editable_Line
     (Buffer : access Source_Buffer_Record;
      Line   : File_Line_Type) return Editable_Line_Type is
   begin
      for J in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (J).File_Line = Line then
            return Buffer.Line_Data (J).Editable_Line;
         end if;

         --  ??? This optimization assumes that the lines are ordered, is it
         --  true ?
         if Buffer.Line_Data (J).File_Line > Line then
            return 0;
         end if;
      end loop;

      return 0;
   end Get_Editable_Line;

   ---------------------
   -- Get_Buffer_Line --
   ---------------------

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : File_Line_Type) return Buffer_Line_Type
   is
   begin
      for J in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (J).File_Line = Line then
            return J;
         end if;

         --  ??? This optimization assumes that the lines are ordered, is it
         --  true ?
         if Buffer.Line_Data (J).File_Line > Line then
            return 0;
         end if;
      end loop;

      return 0;
   end Get_Buffer_Line;

   --------------------
   -- Automatic_Save --
   --------------------

   function Automatic_Save (Buffer : Source_Buffer) return Boolean is
      Success : Boolean;
   begin
      if not Buffer.Modified_Auto or else Buffer.Filename = VFS.No_File then
         return True;
      end if;

      Internal_Save_To_File
        (Buffer,
         Create
           (Full_Filename =>
              Dir_Name (Buffer.Filename) & ".#" & Base_Name (Buffer.Filename)),
         True,
         Success);
      Buffer.Modified_Auto := False;

      return True;
   end Automatic_Save;

   ----------------------
   -- Cursor_Move_Hook --
   ----------------------

   procedure Cursor_Move_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Clear the completion data.

      Clear (Buffer.Completion);
   end Cursor_Move_Hook;

   --------------------
   -- User_Edit_Hook --
   --------------------

   procedure User_Edit_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Clear the completion data.

      Clear (Buffer.Completion);

      --  Request re-parsing of the blocks.

      Request_Blocks (Buffer);
   end User_Edit_Hook;

   ------------------
   -- Destroy_Hook --
   ------------------

   procedure Destroy_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      --  Remove the completion data.

      String_List_Utils.String_List.Free (Buffer.Completion.List);
      GNAT.OS_Lib.Free (Buffer.Completion.Prefix);

      --  ??? Must remove the line information column

      --  Unregister the blocks timeout.

      if Buffer.Blocks_Timeout_Registered then
         Timeout_Remove (Buffer.Blocks_Timeout);
      end if;
   end Destroy_Hook;

   --------------------
   -- Buffer_Destroy --
   --------------------

   procedure Buffer_Destroy (Buffer : access Source_Buffer_Record'Class) is
      Result : Boolean;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Line_Info_Display_Array, Line_Info_Display_Array_Access);
   begin

      --  We do not free memory associated to Buffer.Current_Command, since
      --  this command is already freed when freeing Buffer.Queue.

      if Buffer.Filename /= VFS.No_File then
         File_Closed (Buffer.Kernel, Buffer.Filename);
      end if;

      Destroy_Hook (Buffer);

      if Buffer.Timeout_Id /= 0 then
         Timeout_Remove (Buffer.Timeout_Id);
         Buffer.Timeout_Id := 0;

         if Buffer.Filename /= VFS.No_File then
            Delete_File
              (Dir_Name (Buffer.Filename) & ".#" &
               Base_Name (Buffer.Filename), Result);
         end if;
      end if;

      if Buffer.Controls_Set then
         Remove_Controls (Buffer);
      end if;

      Free_Queue (Buffer.Queue);

      if Buffer.Buffer_Line_Info_Columns.all /= null then
         for J in Buffer.Buffer_Line_Info_Columns.all'Range loop
            Free (Buffer.Buffer_Line_Info_Columns.all (J).Identifier);
         end loop;

         Unchecked_Free (Buffer.Buffer_Line_Info_Columns.all);
      end if;

      Unchecked_Free (Buffer.Buffer_Line_Info_Columns);

      if Buffer.Editable_Line_Info_Columns.all /= null then
         for J in Buffer.Editable_Line_Info_Columns.all'Range loop
            Free (Buffer.Editable_Line_Info_Columns.all (J).Identifier);
         end loop;

         Unchecked_Free (Buffer.Editable_Line_Info_Columns.all);
      end if;

      Unchecked_Free (Buffer.Editable_Line_Info_Columns);

      if Buffer.Editable_Lines /= null then
         for J in Buffer.Editable_Lines'Range loop
            if Buffer.Editable_Lines (J).Where = In_Mark then
               Free (Buffer.Editable_Lines (J).Text);
            end if;
         end loop;

         Unchecked_Free (Buffer.Editable_Lines);
      end if;

      for J in Buffer.Line_Data'Range loop
         if Buffer.Line_Data (J).Enabled_Highlights /= null then
            Unchecked_Free (Buffer.Line_Data (J).Enabled_Highlights);
         end if;
      end loop;

      Unchecked_Free (Buffer.Line_Data);

      Block_List.Free (Buffer.Blocks);
   end Buffer_Destroy;

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler (Buffer : access Source_Buffer_Record'Class) is
   begin
      Emit_New_Cursor_Position (Buffer);
      Buffer.Modified_Auto := True;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Changed_Handler;

   ----------------------
   -- Mark_Set_Handler --
   ----------------------

   procedure Mark_Set_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Mark : constant Gtk_Text_Mark :=
        Get_Text_Mark (Glib.Values.Nth (Params, 2));

   begin
      --  Emit the new cursor position if it is the Insert_Mark that was
      --  changed.

      if Buffer.Setting_Mark then
         return;
      end if;

      Buffer.Setting_Mark := True;

      declare
         Mark_Name : constant String := Get_Name (Mark);
      begin
         if Mark_Name = "insert"
           or else Mark_Name = "gtk_drag_target"
         then
            if Get_Object (Mark) /= Get_Object (Buffer.Insert_Mark) then
               --  If the mark corresponds to a cursor position, set the stored
               --  Insert_Mark accordingly.

               Buffer.Insert_Mark := Mark;
            end if;

            Emit_New_Cursor_Position (Buffer);

            Cursor_Move_Hook (Buffer);
         end if;
      end;

      Buffer.Setting_Mark := False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Mark_Set_Handler;

   --------------------
   -- Insert_Text_Cb --
   --------------------

   procedure Insert_Text_Cb
     (Buffer          : access Source_Buffer_Record'Class;
      End_Insert_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      Text            : String)
   is
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Entity_Kind : Language_Entity;
      Ignored     : Boolean;

      Tags : Highlighting_Tags renames Buffer.Syntax_Tags;
   begin
      --  Set the Start_Iter to the begining of the inserted text,
      --  and set the End_Iter.

      Copy (Source => End_Insert_Iter, Dest => Start_Iter);
      Backward_Chars (Start_Iter, Text'Length, Ignored);
      Copy (Source => End_Insert_Iter, Dest => End_Iter);

      --  Search the initial minimum area to re-highlight...

      Entity_Kind := Normal_Text;

      Entity_Kind_Search_Loop :
      for Current_Entity in Standout_Language_Entity loop
         if Has_Tag (Start_Iter, Tags (Current_Entity)) then
            --  This means that we are in a highlighted region. The minimum
            --  region to re-highlight starts from the begining of the current
            --  region to the end of the following region.

            Entity_Kind := Current_Entity;
            Backward_To_Tag_Toggle
              (Start_Iter, Tags (Current_Entity), Result => Ignored);
            Forward_To_Tag_Toggle (End_Iter, Tags (Entity_Kind), Ignored);
            Forward_To_Tag_Toggle (End_Iter, Result => Ignored);

            exit Entity_Kind_Search_Loop;

         elsif Begins_Tag (End_Iter, Tags (Current_Entity)) or else
           Ends_Tag (Start_Iter, Tags (Current_Entity))
         then
            --  Case Begins_Tag:
            --    This means that we inserted right at the begining of
            --    a highlighted region... The minimum region to re-highlight
            --    starts from the begining of the previous region to the
            --    end of the current region.
            --  Case Ends_Tag:
            --    This means that we inserted right at the end of a highlighted
            --    region. In this case, the minimum region to re-highlight
            --    starts from the begining of the previous region to the end of
            --    the current region.
            --  In both cases, the processing is the same...

            Entity_Kind := Current_Entity;
            Backward_To_Tag_Toggle (Start_Iter, Result => Ignored);
            Forward_To_Tag_Toggle (End_Iter, Result => Ignored);

            exit Entity_Kind_Search_Loop;
         end if;
      end loop Entity_Kind_Search_Loop;

      if Entity_Kind = Normal_Text then
         --  We are inside a normal text region. Just re-highlight this region.

         Backward_To_Tag_Toggle (Start_Iter, Result => Ignored);
         Forward_To_Tag_Toggle (End_Iter, Result => Ignored);
      end if;

      Highlight_Slice (Buffer, Start_Iter, End_Iter);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Insert_Text_Cb;

   -------------------------
   -- Insert_Text_Handler --
   -------------------------

   procedure Insert_Text_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Length : constant Gint := Get_Int (Nth (Params, 3));
      Dummy  : Boolean;
      Start  : Buffer_Line_Type;
      Iter   : Gtk_Text_Iter;
      Number : Buffer_Line_Type;

   begin
      Get_Text_Iter (Nth (Params, 1), Iter);

      if Buffer.Lang /= null
        and then Get_Language_Context (Buffer.Lang).Syntax_Highlighting
      then
         declare
            Text : constant String :=
              Get_String (Nth (Params, 2), Length => Length);
         begin
            Insert_Text_Cb (Buffer, Iter, Text);
         end;
      end if;

      --  Call Add_Lines, to compute added lines for the side column.

      Start := Buffer_Line_Type (Get_Line (Iter) + 1);
      Backward_Chars (Iter, Length, Dummy);
      Number := Start - Buffer_Line_Type (Get_Line (Iter) + 1);

      if Number > 0 then
         Lines_Add_Hook (Buffer, Start - Number, Number);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Insert_Text_Handler;

   -----------------------
   -- First_Insert_Text --
   -----------------------

   procedure First_Insert_Text
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Pos     : Gtk_Text_Iter;
      Length  : constant Gint := Get_Int (Nth (Params, 3));
      Text    : constant String :=
        Get_String (Nth (Params, 2), Length => Length);
      Command : Editor_Command := Editor_Command (Buffer.Current_Command);

   begin
      if Buffer.Inserting then
         return;
      end if;

      User_Edit_Hook (Buffer);

      Get_Text_Iter (Nth (Params, 1), Pos);

      if Is_Null_Command (Command) then
         Create
           (Command,
            Insertion,
            Source_Buffer (Buffer),
            False,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1)),
            Natural (Get_Line_Offset (Pos) + 1));

         Buffer.Inserting := True;
         Enqueue (Buffer, Command_Access (Command));
         Buffer.Inserting := False;

         Add_Text (Command, Text);
         Buffer.Current_Command := Command_Access (Command);

      elsif Get_Mode (Command) = Insertion then
         if Length = 1
           and then (Text (1) = ASCII.LF or else Text (1) = ' ')
         then
            End_Action (Buffer);
            Create
              (Command,
               Insertion,
               Source_Buffer (Buffer),
               False,
               Get_Editable_Line
                 (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1)),
               Natural (Get_Line_Offset (Pos) + 1));

            Buffer.Inserting := True;
            Enqueue (Buffer, Command_Access (Command));
            Buffer.Inserting := False;
         end if;

         Add_Text (Command, Text);
         Buffer.Current_Command := Command_Access (Command);

      else
         End_Action (Buffer);
         Create
           (Command,
            Insertion,
            Source_Buffer (Buffer),
            False,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Get_Line (Pos) + 1)),
            Natural (Get_Line_Offset (Pos) + 1));
         Enqueue (Buffer, Command_Access (Command));
         Add_Text (Command, Text);
         Buffer.Current_Command := Command_Access (Command);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end First_Insert_Text;

   ---------------------
   -- Delete_Range_Cb --
   ---------------------

   procedure Delete_Range_Cb
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Ignored    : Boolean;

   begin
      --  Search the initial minimum area to re-highlight:
      --    - case Has_Tag:
      --        This means that we are inside a highlighted region. In that
      --        case, we just re-highlight this region.
      --    - case Begins_Tag:
      --        We are at the begining of a highlighted region. We re-highlight
      --        from the begining of the previous region to the end of this
      --        region.
      --    - case Ends_Tag:
      --        We are at the end of a highlighted region. We re-highlight from
      --        the begining of this region to the end of the next region.
      --  I all three cases, the processing is the same...

      Copy (Source => Iter, Dest => Start_Iter);
      Copy (Source => Iter, Dest => End_Iter);
      Backward_To_Tag_Toggle (Start_Iter, Result => Ignored);
      Forward_To_Tag_Toggle (End_Iter, Result => Ignored);
      Highlight_Slice (Buffer, Start_Iter, End_Iter);
   end Delete_Range_Cb;

   --------------------------
   -- Delete_Range_Handler --
   --------------------------

   procedure Delete_Range_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start_Iter : Gtk_Text_Iter;
   begin
      Get_Text_Iter (Nth (Params, 1), Start_Iter);

      if Buffer.Lang /= null
        and then Get_Language_Context (Buffer.Lang).Syntax_Highlighting
      then
         Delete_Range_Cb (Buffer, Start_Iter);
      end if;

      if Buffer.First_Removed_Line > 0 then
         Lines_Remove_Hook_After
           (Buffer, Buffer.First_Removed_Line, Buffer.Last_Removed_Line);
         Buffer.First_Removed_Line := 0;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Delete_Range_Handler;

   ---------------------------------
   -- Delete_Range_Before_Handler --
   ---------------------------------

   procedure Delete_Range_Before_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Start_Iter   : Gtk_Text_Iter;
      End_Iter     : Gtk_Text_Iter;
      Command      : Editor_Command := Editor_Command (Buffer.Current_Command);
      Direction    : Direction_Type := Extended;
      Line, Column : Gint;
      Line_Start   : Gint;
      Column_Start : Gint;
      Line_End     : Gint;
      Column_End   : Gint;

   begin
      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      --  Determine the direction mode for the delete action.

      Get_Cursor_Position (Buffer, Line, Column);

      Line_Start   := Get_Line (Start_Iter);
      Column_Start := Get_Line_Offset (Start_Iter);
      Line_End     := Get_Line (End_Iter);
      Column_End   := Get_Line_Offset (End_Iter);

      if Line = Line_Start
        and then Column = Column_Start
      then
         Direction := Backward;
      end if;

      if Line = Line_End
        and then Column = Column_End
      then
         Direction := Forward;
      end if;

      --  Remove the lines in the side information column.

      if Line_Start /= Line_End then
         Lines_Remove_Hook_Before
           (Buffer,
            Buffer_Line_Type (Line_Start + 1),
            Buffer_Line_Type (Line_End + 1));
      end if;

      if Buffer.Inserting then
         return;
      end if;

      User_Edit_Hook (Buffer);

      if not Is_Null_Command (Command)
        and then (Get_Mode (Command) /= Deletion
                  or else (Get_Direction (Command) /= Direction))
      then
         End_Action (Buffer);
         Command := Editor_Command (Buffer.Current_Command);

         pragma Assert (Is_Null_Command (Command));
      end if;

      if Is_Null_Command (Command) then
         Create
           (Command,
            Deletion,
            Source_Buffer (Buffer),
            True,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1)),
            Natural (Column_Start + 1),
            Direction,
            Get_Editable_Line (Buffer, Buffer_Line_Type (Line + 1)),
            Natural (Column + 1));

         Buffer.Inserting := True;
         Enqueue (Buffer, Command_Access (Command));
         Buffer.Inserting := False;

         Add_Text
           (Command,
            Get_Slice (Buffer, Start_Iter, End_Iter),
            Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1)),
            Natural (Column_Start + 1));
      else
         if Direction = Forward then
            Add_Text
              (Command,
               Get_Slice (Buffer, Start_Iter, End_Iter),
               Get_Editable_Line (Buffer, Buffer_Line_Type (Line_Start + 1)),
               Natural (Column_Start + 1));
         else
            Add_Text
              (Command,
               Get_Slice (Buffer, Start_Iter, End_Iter));
         end if;
      end if;

      Buffer.Current_Command := Command_Access (Command);

      if Direction = Extended
        or else Line_Start /= Line_End
      then
         End_Action (Buffer);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Delete_Range_Before_Handler;

   -----------------------------
   -- Line_Highlights_Changed --
   -----------------------------

   procedure Line_Highlights_Changed
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");
   begin
      Emit_By_Name
        (Get_Object (Buffer), "line_highlights_changed"
         & ASCII.NUL);
   end Line_Highlights_Changed;

   --------------------------------
   -- Buffer_Information_Changed --
   --------------------------------

   procedure Buffer_Information_Changed
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");
   begin
      Emit_By_Name
        (Get_Object (Buffer), "buffer_information_changed" & ASCII.NUL);
   end Buffer_Information_Changed;

   --------------------
   -- Status_Changed --
   --------------------

   procedure Status_Changed
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");
   begin
      Emit_By_Name (Get_Object (Buffer), "status_changed" & ASCII.NUL);
   end Status_Changed;

   ---------------------
   -- Get_Last_Status --
   ---------------------

   function Get_Last_Status
     (Buffer : access Source_Buffer_Record'Class)
      return Status_Type is
   begin
      return Buffer.Current_Status;
   end Get_Last_Status;

   ---------------------
   -- Set_Last_Status --
   ---------------------

   procedure Set_Last_Status
     (Buffer : access Source_Buffer_Record'Class;
      Status : Status_Type) is
   begin
      Buffer.Current_Status := Status;
   end Set_Last_Status;

   ------------------------------
   -- Emit_New_Cursor_Position --
   ------------------------------

   procedure Emit_New_Cursor_Position
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String;
         Line   : Gint;
         Column : Gint);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");

      L, C : Gint;
   begin
      Get_Screen_Position (Buffer, L, C);

      Emit_By_Name
        (Get_Object (Buffer), "cursor_position_changed" & ASCII.NUL,
         Line   => Gint (Get_Editable_Line (Buffer, Buffer_Line_Type (L + 1))),
         Column => C + 1);

      --  Remove delimiters highlight.

      if Buffer.Has_Delimiters_Highlight then
         declare
            From : Gtk_Text_Iter;
            To   : Gtk_Text_Iter;
         begin
            Get_Iter_At_Mark
              (Buffer, From, Buffer.Start_Delimiters_Highlight);
            Get_Iter_At_Mark (Buffer, To, Buffer.End_Delimiters_Highlight);

            Delete_Mark (Buffer, Buffer.Start_Delimiters_Highlight);
            Delete_Mark (Buffer, Buffer.End_Delimiters_Highlight);

            Remove_Tag (Buffer, Buffer.HL_Line_Tag, From, To);
         end;

         Buffer.Has_Delimiters_Highlight := False;
      end if;

      if not Get_Pref (Buffer.Kernel, Highlight_Delimiters) then
         return;
      end if;

      --  Highlight brackets if necessary.

      declare
         Current              : Gtk_Text_Iter;
         On_Cursor_Iter       : Gtk_Text_Iter;
         First_Highlight_Iter : Gtk_Text_Iter;
         Last_Highlight_Iter  : Gtk_Text_Iter;
         Highlight_Necessary  : Boolean := False;

         Both_Highlights      : Boolean := False;
         --  Indicate whether highlighting occurs before and after the cursor.

         Success              : Boolean;
         Counter              : Natural;
         Counter_Max          : constant := 4096;
         --  ??? Should that be a preference ?

         Stack                : Natural;
         C                    : Character;

         Delimiter            : Integer;

      begin
         Get_Iter_At_Mark (Buffer, On_Cursor_Iter, Buffer.Insert_Mark);

         --  Highlight previous parenthesis, if necessary.

         --  Find a closing delimiter.

         Delimiter := -1;
         Copy (On_Cursor_Iter, Current);

         Backward_Char (Current, Success);

         if Success then
            C := Get_Char (Current);

            for J in Delimiters'Range loop
               if Delimiters (J, Closing) = C then
                  Delimiter := J;
                  exit;
               end if;
            end loop;
         end if;

         if Delimiter in Delimiters'Range then
            Counter := 0;
            Stack := 1;
            Backward_Char (Current, Success);

            while Success and then Counter < Counter_Max loop
               C := Get_Char (Current);

               if C = Delimiters (Delimiter, Closing) then
                  Stack := Stack + 1;

               elsif C = Delimiters (Delimiter, Opening) then
                  Stack := Stack - 1;
               end if;

               if Stack = 0 then
                  Copy (Current, First_Highlight_Iter);
                  Copy (On_Cursor_Iter, Last_Highlight_Iter);

                  Highlight_Necessary := True;
                  exit;
               end if;

               Counter := Counter + 1;
               Backward_Char (Current, Success);
            end loop;
         end if;

         --  Highlight next parenthesis, if necessary.

         Delimiter := -1;
         Copy (On_Cursor_Iter, Current);
         C := Get_Char (On_Cursor_Iter);

         for J in Delimiters'Range loop
            if Delimiters (J, Opening) = C then
               Delimiter := J;
               exit;
            end if;
         end loop;

         if Delimiter in Delimiters'Range then
            Counter := 0;
            Stack := 1;
            Forward_Char (Current, Success);

            while Success and then Counter < Counter_Max loop
               C := Get_Char (Current);

               if C = Delimiters (Delimiter, Opening) then
                  Stack := Stack + 1;

               elsif C = Delimiters (Delimiter, Closing) then
                  Stack := Stack - 1;

               end if;

               if Stack = 0 then
                  if not Highlight_Necessary then
                     Copy (On_Cursor_Iter, First_Highlight_Iter);
                  else
                     Both_Highlights := True;
                  end if;

                  Forward_Char (Current, Success);
                  Copy (Current, Last_Highlight_Iter);

                  Highlight_Necessary := True;
                  exit;
               end if;

               Counter := Counter + 1;
               Forward_Char (Current, Success);
            end loop;

         end if;

         --  Highlight next parenthesis, if necessary.

         Delimiter := -1;
         C := Get_Char (On_Cursor_Iter);

         for J in Delimiters'Range loop
            if Delimiters (J, Opening) = C then
               Delimiter := J;
               exit;
            end if;
         end loop;

         if Delimiter in Delimiters'Range then
            Counter := 0;
            Stack := 1;
            Copy (On_Cursor_Iter, Current);
            Forward_Char (Current, Success);

            while Success and then Counter < Counter_Max loop
               C := Get_Char (Current);

               if C = Delimiters (Delimiter, Opening) then
                  Stack := Stack + 1;

               elsif C = Delimiters (Delimiter, Closing) then
                  Stack := Stack - 1;

               end if;

               if Stack = 0 then
                  if not Highlight_Necessary then
                     Copy (On_Cursor_Iter, First_Highlight_Iter);
                  end if;

                  Forward_Char (Current, Success);
                  Copy (Current, Last_Highlight_Iter);

                  Highlight_Necessary := True;
                  exit;
               end if;

               Counter := Counter + 1;
               Forward_Char (Current, Success);
            end loop;
         end if;

         if Highlight_Necessary then
            Copy (First_Highlight_Iter, Current);
            Forward_Char (Current, Success);
            Apply_Tag
              (Buffer,
               Buffer.HL_Line_Tag,
               First_Highlight_Iter,
               Current);

            Copy (Last_Highlight_Iter, Current);
            Backward_Char (Current, Success);
            Apply_Tag
              (Buffer,
               Buffer.HL_Line_Tag,
               Current,
               Last_Highlight_Iter);

            if Both_Highlights then
               Copy (On_Cursor_Iter, Current);
               Backward_Char (Current, Success);
               Forward_Char (On_Cursor_Iter, Success);
               Apply_Tag
                 (Buffer,
                  Buffer.HL_Line_Tag,
                  Current,
                  On_Cursor_Iter);
            end if;

            Buffer.Start_Delimiters_Highlight := Create_Mark
              (Buffer, "", First_Highlight_Iter);
            Buffer.End_Delimiters_Highlight := Create_Mark
              (Buffer, "", Last_Highlight_Iter);

            Forward_To_Line_End (Last_Highlight_Iter, Success);
            Set_Line_Offset (First_Highlight_Iter, 0);

            Highlight_Slice
              (Buffer, First_Highlight_Iter, Last_Highlight_Iter);

            Buffer.Has_Delimiters_Highlight := True;
         end if;
      end;
   end Emit_New_Cursor_Position;

   ---------------------
   -- Highlight_Slice --
   ---------------------

   procedure Highlight_Slice
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter)
   is
      Highlight_Complete : Boolean := False;
      Entity_Start       : Gtk_Text_Iter;
      Entity_End         : Gtk_Text_Iter;
      Tags               : Highlighting_Tags renames Buffer.Syntax_Tags;
      Slice_Offset_Line  : Gint;
      Slice_Offset_Column : Gint;
      Result             : Boolean;

      function Highlight_Cb
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Function called by Language.Parse_Entities for each entity found

      procedure Local_Highlight;
      --  Highlight the region exactly located between Entity_Start and
      --  Entity_End.
      --  After this procedure is run, some variables are positioned to
      --  the following values:
      --    - Last_Entity is equal to the incomplete entity kind found inside
      --      the given region, or to Normal_Text if all entities were complete
      --    - Entity_Start is set to the begining of the incomplete region
      --      found in the given buffer slice, if any.

      ------------------
      -- Highlight_Cb --
      ------------------

      function Highlight_Cb
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         Success : Boolean;
         Col     : Gint;
      begin
         --  Some parsers currently leave line numbers to 0. Don't highlight in
         --  this case, since we cannot use from the byte index due to
         --  limitations in gtk+

         if Sloc_Start.Line = 0 then
            return False;
         end if;

         --  Don't need to take into account the offset column, unless we are
         --  still on the same line that we started at.

         if Gint (Sloc_Start.Line) = 1 then
            Col := Gint (Sloc_Start.Column) + Slice_Offset_Column - 1;
         else
            Col := Gint (Sloc_Start.Column) - 1;
         end if;

         Get_Iter_At_Line_Index
           (Buffer, Entity_Start,
            Gint (Sloc_Start.Line) + Slice_Offset_Line - 1,
            Col);

         --  If the column is 0, the entity really ended on the end of the
         --  previous line.

         if Sloc_End.Column = 0 then
            Get_Iter_At_Line_Index
              (Buffer, Entity_End,
               Gint (Sloc_End.Line) + Slice_Offset_Line,
               0);
            Backward_Char (Entity_End, Success);

         else
            if Gint (Sloc_End.Line) = 1 then
               Col := Gint (Sloc_End.Column) + Slice_Offset_Column - 1;
            else
               Col := Gint (Sloc_End.Column) - 1;
            end if;

            Get_Iter_At_Line_Index
              (Buffer, Entity_End,
               Gint (Sloc_End.Line) + Slice_Offset_Line - 1,
               Col);
            Forward_Char (Entity_End, Success);
         end if;

         if Partial_Entity then
            Highlight_Complete := False;
         end if;

         if Entity in Standout_Language_Entity then
            Apply_Tag (Buffer, Tags (Entity), Entity_Start, Entity_End);
         end if;

         return False;
      end Highlight_Cb;

      ---------------------
      -- Local_Highlight --
      ---------------------

      procedure Local_Highlight is
         UTF8   : constant Interfaces.C.Strings.chars_ptr :=
           Get_Slice (Entity_Start, Entity_End);
         Slice : constant Unchecked_String_Access :=
           To_Unchecked_String (UTF8);

         --  ??? Could it be more efficient to just substract
         --     Get_Offset (Entity_End) - Get_Offset (Entity_Start)
         Length : constant Integer := Integer (Strlen (UTF8));
         pragma Suppress (Access_Check, Slice);

      begin
         Highlight_Complete := True;
         Slice_Offset_Line   := Get_Line (Entity_Start);
         Slice_Offset_Column := Get_Line_Index (Entity_Start);

         --  First, un-apply all the style tags...

         Kill_Highlighting (Buffer, Entity_Start, Entity_End);

         --  Now re-highlight the text...

         Parse_Entities
           (Buffer.Lang,
            Slice (1 .. Length),
            Highlight_Cb'Unrestricted_Access);
         g_free (UTF8);
      end Local_Highlight;

   begin
      Copy (Source => Start_Iter, Dest => Entity_Start);
      Copy (Source => End_Iter, Dest => Entity_End);

      --  Highlight to the end of line, to avoid missing most of the
      --  partial entities (strings, characters, ...)

      Forward_Line (Entity_End, Result);
      Local_Highlight;

      if not Highlight_Complete then
         --  In this case, we are in the middle of e.g a multi-line comment,
         --  and we re-highlight the whole buffer since we do not know where
         --  the comment started.
         --  ??? would be nice to optimize here

         Set_Offset (Entity_Start, 0);
         Forward_To_End (Entity_End);
         Local_Highlight;
      end if;
   end Highlight_Slice;

   -----------------------
   -- Kill_Highlighting --
   -----------------------

   procedure Kill_Highlighting
     (Buffer : access Source_Buffer_Record'Class;
      From   : Gtk_Text_Iter;
      To     : Gtk_Text_Iter) is
   begin
      for Entity_Kind in Standout_Language_Entity loop
         Remove_Tag (Buffer, Buffer.Syntax_Tags (Entity_Kind), From, To);
      end loop;
   end Kill_Highlighting;

   -------------------------
   -- Forward_To_Line_End --
   -------------------------

   procedure Forward_To_Line_End (Iter : in out Gtk.Text_Iter.Gtk_Text_Iter) is
      Result_Ignored : Boolean;
   begin
      --  ??? Can we use Gtk.Text_Iter.Forward_To_Line_End, and if not why not
      while not Is_End (Iter) and then not Ends_Line (Iter) loop
         Forward_Char (Iter, Result_Ignored);
      end loop;
   end Forward_To_Line_End;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null) is
   begin
      Buffer := new Source_Buffer_Record;
      Initialize (Buffer, Kernel, Lang);
   end Gtk_New;

   ---------------------
   -- Initialize_Hook --
   ---------------------

   procedure Initialize_Hook (Buffer : access Source_Buffer_Record'Class) is
      Iter : Gtk_Text_Iter;
   begin
      --  Initialize the completion.
      Clear (Buffer.Completion);

      Get_Start_Iter (Buffer, Iter);

      Buffer.Completion.Mark := Create_Mark (Buffer, "", Iter);
      Buffer.Completion.Previous_Mark := Create_Mark (Buffer, "", Iter);
      Buffer.Completion.Next_Mark := Create_Mark (Buffer, "", Iter);

      Buffer.Completion.Buffer := Gtk.Text_Buffer.Gtk_Text_Buffer (Buffer);

      --  Initialize the line info.

      Buffer.Editable_Lines := new Editable_Line_Array (1 .. 1);
      Buffer.Editable_Lines (1) :=
        (Where          => In_Buffer,
         Buffer_Line    => 1,
         Side_Info_Data => null);

      --  ??? create line info (above)

      Buffer.Line_Data := new Line_Data_Array (1 .. 1);
      Buffer.Line_Data (1) := New_Line_Data;
      Buffer.Line_Data (1).Editable_Line := 1;

      --  Compute the block information.

      Request_Blocks (Buffer);
   end Initialize_Hook;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null)
   is
      Tags    : Gtk_Text_Tag_Table;
      Command : Check_Modified_State;
   begin
      Gtk.Text_Buffer.Initialize (Buffer);

      Glib.Object.Initialize_Class_Record
        (Buffer, Signals, Class_Record, "GlideSourceBuffer",
         Signal_Parameters);

      Buffer.Lang := Lang;
      Buffer.Kernel := Kernel;

      --  Save the newly created highlighting tags into the source buffer
      --  tag table.

      Tags := Get_Tag_Table (Buffer);

      Preferences_Changed (Buffer, Kernel);

      for Entity_Kind in Standout_Language_Entity'Range loop
         Text_Tag_Table.Add (Tags, Buffer.Syntax_Tags (Entity_Kind));
      end loop;

      --  Create HL_Line_Tag and save it into the source buffer tag table.
      Create_Highlight_Line_Tag
        (Buffer.HL_Line_Tag, Get_Pref (Kernel, Default_HL_Line_Color));
      Text_Tag_Table.Add (Tags, Buffer.HL_Line_Tag);

      --  Create HL_Region_Tag and save it into the source buffer tag table.
      Create_Highlight_Region_Tag
        (Buffer.HL_Region_Tag, Get_Pref (Kernel, Default_HL_Region_Color));
      Text_Tag_Table.Add (Tags, Buffer.HL_Region_Tag);

      Gtk_New (Buffer.Non_Editable_Tag);
      Set_Property (Buffer.Non_Editable_Tag, Editable_Property, False);
      Add (Tags, Buffer.Non_Editable_Tag);

      --  Save the insert mark for fast retrievals, since we will need to
      --  access it very often.
      Buffer.Insert_Mark := Get_Insert (Buffer);

      --  Initialize the queue for editor commands

      Buffer.Queue := New_Queue;

      --  And finally, connect ourselves to the interesting signals

      Buffer_Callback.Connect
        (Buffer, "changed",
         Buffer_Callback.To_Marshaller (Changed_Handler'Access),
         After => True);
      Buffer_Callback.Connect
        (Buffer, "mark_set", Cb => Mark_Set_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, "insert_text",
         Cb => First_Insert_Text'Access);
      Buffer_Callback.Connect
        (Buffer, "insert_text",
         Cb => Insert_Text_Handler'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, "delete_range",
         Cb => Delete_Range_Handler'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, "delete_range",
         Cb => Delete_Range_Before_Handler'Access,
         After => False);

      Kernel_Callback.Object_Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         Slot_Object => Buffer,
         User_Data   => Kernel);

      Initialize_Hook (Buffer);

      --  Create the queue change hook that will be called every
      --  time the state of the queue associated to the buffer changes.

      Create (Command, Source_Buffer (Buffer), Buffer.Queue);
      Add_Queue_Change_Hook
        (Buffer.Queue,
         Command_Access (Command),
         "State_Check");

      Buffer.First_Removed_Line := 0;

      Buffer.Editable_Line_Info_Columns := new Line_Info_Display_Array_Access'
        (null);
      Buffer.Buffer_Line_Info_Columns   := new Line_Info_Display_Array_Access'
        (null);
   end Initialize;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Buffer : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      B       : Source_Buffer := Source_Buffer (Buffer);
      Timeout : Gint;
      Prev    : Boolean;
   begin
      --  Since we update the tags directly, gtk+ will automatically refresh
      --  the source view, we don't need to do anything for this.

      Create_Syntax_Tags
        (B.Syntax_Tags,
         Keyword_Color       => Get_Pref_Fg   (Kernel, Keywords_Style),
         Keyword_Color_Bg    => Get_Pref_Bg   (Kernel, Keywords_Style),
         Keyword_Font_Desc   => Get_Pref_Font (Kernel, Keywords_Style),
         Comment_Color       => Get_Pref_Fg   (Kernel, Comments_Style),
         Comment_Color_Bg    => Get_Pref_Bg   (Kernel, Comments_Style),
         Comment_Font_Desc   => Get_Pref_Font (Kernel, Comments_Style),
         Character_Color     => Get_Pref_Fg   (Kernel, Strings_Style),
         Character_Color_Bg  => Get_Pref_Bg   (Kernel, Strings_Style),
         Character_Font_Desc => Get_Pref_Font (Kernel, Strings_Style),
         String_Color        => Get_Pref_Fg   (Kernel, Strings_Style),
         String_Color_Bg     => Get_Pref_Bg   (Kernel, Strings_Style),
         String_Font_Desc    => Get_Pref_Font (Kernel, Strings_Style));

      --  Connect timeout, to handle automatic saving of buffer

      if B.Timeout_Id /= 0 then
         Timeout_Remove (B.Timeout_Id);
         B.Timeout_Id := 0;
      end if;

      Timeout := Get_Pref (Kernel, Periodic_Save);

      if Timeout > 0 then
         B.Timeout_Id := Buffer_Timeout.Add
           (Guint32 (Timeout) * 1000,  Automatic_Save'Access, B.all'Access);
      end if;

      --  ??? Should improve this when the preference for source highlightings
      --  becomes more sophisticated.
      Prev := B.Parse_Blocks;
      B.Parse_Blocks :=
        not Equal (Get_Pref (Kernel, Current_Block_Color),
                   White (Get_Default_Colormap));

      if Prev /= B.Parse_Blocks then
         Buffer_Information_Changed (B);
      end if;

      if not Prev and then B.Parse_Blocks then
         Request_Blocks (B);
      end if;

      Prev := B.Block_Folding;
      B.Block_Folding := Get_Pref (Kernel, Block_Folding);

      if not B.Block_Folding and then Prev then
         Unfold_All (B);
         Remove_Block_Folding_Commands (B);
      end if;

      if not Prev and then B.Block_Folding then
         Request_Blocks (B);
      end if;
   end Preferences_Changed;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean)
   is
      Contents : GNAT.OS_Lib.String_Access;
      UTF8     : Gtkada.Types.Chars_Ptr;
      Ignore   : aliased Natural;
      Length   : aliased Natural;
      Last     : Natural;
      CR_Found : Boolean := False;

   begin
      Success := True;
      Contents := Read_File (Filename);

      if Contents = null then
         Trace (Me, "Load_File: Couldn't read contents of "
                & Full_Name (Filename));
         Success := False;
         return;
      end if;

      Clear (Buffer);
      Buffer.Inserting := True;

      if Lang_Autodetect then
         Set_Language
           (Buffer, Get_Language_From_File
            (Get_Language_Handler (Buffer.Kernel), Filename));
      end if;

      --  ??? This is not compatible with files with "CR" as line terminator.

      Strip_CR (Contents.all, Last, CR_Found);

      UTF8 := Glib.Convert.Convert
        (Contents.all (Contents'First .. Last), "UTF-8",
         Get_Pref (Buffer.Kernel, Default_Charset),
         Ignore'Unchecked_Access, Length'Unchecked_Access);

      if UTF8 = Gtkada.Types.Null_Ptr then
         --  In case conversion failed, use a default encoding so that we can
         --  at least show something in the editor
         UTF8 := Glib.Convert.Convert
           (Contents.all (Contents'First .. Last), "UTF-8", "ISO-8859-1",
            Ignore'Unchecked_Access, Length'Unchecked_Access);
      end if;

      Insert_At_Cursor (Buffer, UTF8, Gint (Length));

      g_free (UTF8);
      Free (Contents);

      if CR_Found then
         Buffer.Line_Terminator := CR_LF;
      else
         Buffer.Line_Terminator := LF;
      end if;

      Set_Modified (Buffer, False);

      Buffer.Inserting := False;
      Buffer.Modified_Auto := False;

      Buffer.Timestamp := To_Timestamp (File_Time_Stamp (Filename));

      Empty_Queue (Buffer.Queue);
      Buffer.Current_Command := null;

   exception
      when E : others =>
         Success := False;
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Load_File;

   ---------------------------
   -- Internal_Save_To_File --
   ---------------------------

   procedure Internal_Save_To_File
     (Buffer   : Source_Buffer;
      Filename : VFS.Virtual_File;
      Internal : Boolean;
      Success  : out Boolean)
   is
      FD         : File_Descriptor := Invalid_FD;
      Terminator : Line_Terminator_Style := Buffer.Line_Terminator;
      Buffer_Line : Buffer_Line_Type;
      Locale_Filename : constant String := Locale_Full_Name (Filename);

      Force_Write : Boolean := False;
      --  Whether the file mode has been forced to writable.

      procedure New_Line (FD : File_Descriptor);
      --  Write a new line on FD.

      procedure New_Line (FD : File_Descriptor) is
         NL            : aliased constant String := ASCII.CR & ASCII.LF;
         Bytes_Written : Integer;
         pragma Unreferenced (Bytes_Written);

      begin
         case Terminator is
            when CR_LF =>
               Bytes_Written := Write (FD, NL (1)'Address, 2);
            when CR =>
               Bytes_Written := Write (FD, NL (1)'Address, 1);
            when Unknown | LF =>
               Bytes_Written := Write (FD, NL (2)'Address, 1);
         end case;
      end New_Line;

   begin
      Success := True;

      FD := Create_File (Locale_Filename, Fmode => Binary);

      --  The file could not be opened, check whether it is read-only.

      if FD = Invalid_FD
        and then Is_Regular_File (Filename)
        and then not Is_Writable (Filename)
      then
         declare
            Buttons : Message_Dialog_Buttons;
         begin
            Buttons := Message_Dialog
              (Msg            => -"The file "
                 & Base_Name (Filename) & ASCII.LF
                 & (-"is read-only. Do you want to overwrite it ?"),
               Dialog_Type    => Confirmation,
               Buttons        => Button_Yes or Button_No,
               Default_Button => Button_No,
               Title          => -"File is read-only",
               Justification  => Justify_Left,
               Parent         => Get_Main_Window (Buffer.Kernel));

            if Buttons = Button_Yes then
               Force_Write := True;
               Set_Writable (Filename, True);
               FD := Create_File (Locale_Filename, Fmode => Binary);
            end if;
         end;
      end if;

      if FD = Invalid_FD then
         Insert
           (Buffer.Kernel,
            -"Could not open file for writing: " & Full_Name (Filename),
            Mode => Error);
         Success := False;
         return;
      end if;

      declare
         Terminator_Pref : constant Line_Terminators :=
           Line_Terminators'Val (Get_Pref (Buffer.Kernel, Line_Terminator));
         Bytes_Written   : Integer;
         pragma Unreferenced (Bytes_Written);

         Strip_Blank     : Boolean;
         Index           : Natural;

      begin
         case Terminator_Pref is
            when Unix =>
               Terminator := LF;
            when Windows =>
               Terminator := CR_LF;
            when Unchanged =>
               null;
         end case;

         Strip_Blank := Get_Pref (Buffer.Kernel, Strip_Blanks);

         for Line in
           Buffer.Editable_Lines'First .. Buffer.Last_Editable_Line
         loop
            declare
               Str      : Src_String := Get_String (Buffer, Line);
               C        : Gunichar;

            begin
               if Str.Length > 0 then
                  declare
                     Contents : constant String :=
                       Locale_From_UTF8 (Str.Contents (1 .. Str.Length));
                  begin
                     if Strip_Blank then
                        Index := Contents'Length;

                        while Index >= Contents'First loop
                           C := UTF8_Get_Char
                             (Contents (Index .. Contents'First));
                           exit when C /= Character'Pos (' ')
                             and then C /= Character'Pos (ASCII.HT);

                           Index := UTF8_Find_Prev_Char (Contents, Index);
                        end loop;

                        Bytes_Written := Write
                          (FD, Contents (Contents'First)'Address, Index);

                     else
                        Bytes_Written := Write
                          (FD, Contents (Contents'First)'Address,
                           Contents'Length);
                     end if;
                  end;
               end if;

               Free (Str);

               if Line /= Buffer.Last_Editable_Line then
                  New_Line (FD);
               end if;
            end;
         end loop;
      end;

      Close (FD);

      --  If the file could be saved, emit the corresponding signal.
      --  Emit the signal only if we are really saving to the buffer's file,
      --  not to another filename (which happens for example when doing
      --  automatic saves.

      if Success
        and then Filename = Buffer.Filename
      then
         File_Saved (Buffer.Kernel, Filename);

         for J in Buffer.Editable_Lines'Range loop
            Buffer_Line := Get_Buffer_Line (Buffer, J);

            if Buffer_Line /= 0
              and then Buffer_Line in Buffer.Line_Data'Range
            then
               Buffer.Line_Data (Buffer_Line).File_Line := File_Line_Type (J);
            end if;
         end loop;

         Buffer.Saved_Position := Get_Position (Buffer.Queue);
         Buffer.Current_Status := Saved;
         Status_Changed (Buffer);

      elsif Success
        and then Filename /= Buffer.Filename
        and then not Internal
      then
         if Buffer.Filename /= VFS.No_File then
            File_Closed (Buffer.Kernel, Buffer.Filename);
         end if;

         File_Edited (Buffer.Kernel, Filename);
      end if;

      --  If the file mode was forced to writable, reset it to read-only.

      if Force_Write then
         Set_Writable (Filename, False);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         --  To avoid consuming up all File Descriptors, we catch all
         --  exceptions here, and close the current file descriptor.

         if FD /= Invalid_FD then
            Close (FD);
         end if;
   end Internal_Save_To_File;

   ------------------
   -- Save_To_File --
   ------------------

   procedure Save_To_File
     (Buffer   : access Source_Buffer_Record;
      Filename : VFS.Virtual_File;
      Success  : out Boolean)
   is
      Name_Changed : constant Boolean := Buffer.Filename /= Filename;
   begin
      if Name_Changed then
         Buffer.Filename := Filename;
      end if;

      if Name_Changed then
         Set_Language
           (Buffer,
            Get_Language_From_File
              (Glide_Language_Handler (Get_Language_Handler (Buffer.Kernel)),
               Buffer.Filename));

         --  ??? The following is expensive, it would be nice to have a
         --  simpler way to report a possible change in the list of sources
         --  of a project.
         Recompute_View (Buffer.Kernel);
      end if;

      Internal_Save_To_File (Source_Buffer (Buffer), Filename, False, Success);

      if not Success then
         return;
      end if;

      Buffer.Timestamp := To_Timestamp
        (File_Time_Stamp (Get_Filename (Buffer)));

      if Buffer.Filename /= VFS.No_File then
         Delete (Create (Full_Filename =>
                           Dir_Name (Buffer.Filename)
                           & ".#" & Base_Name (Buffer.Filename)));
      end if;

      Set_Modified (Buffer, False);
      Buffer.Modified_Auto := False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Save_To_File;

   -----------
   -- Clear --
   -----------

   procedure Clear (Buffer : access Source_Buffer_Record) is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if Get_Char_Count (Buffer) > 0 then
         Get_Bounds (Buffer, Start_Iter, End_Iter);
         Delete (Buffer, Start_Iter, End_Iter);
      end if;
   end Clear;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Buffer : access Source_Buffer_Record;
      Lang   : Language.Language_Access)
   is
      Buffer_Start_Iter : Gtk_Text_Iter;
      Buffer_End_Iter   : Gtk_Text_Iter;
   begin
      if Buffer.Lang /= Lang then
         Buffer.Lang := Lang;
         Get_Bounds (Buffer, Buffer_Start_Iter, Buffer_End_Iter);

         --  Do not try to highlight an empty buffer
         if not Is_End (Buffer_Start_Iter) then
            if Buffer.Lang /= null
              and then Get_Language_Context (Buffer.Lang).Syntax_Highlighting
            then
               Highlight_Slice (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
            else
               Kill_Highlighting (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
            end if;
         end if;

         Buffer.Parse_Blocks :=
            not Equal (Get_Pref (Buffer.Kernel, Current_Block_Color),
                       White (Get_Default_Colormap));

         Compute_Blocks (Buffer);

         Buffer_Information_Changed (Buffer);
      end if;
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Buffer : access Source_Buffer_Record) return Language.Language_Access is
   begin
      return Buffer.Lang;
   end Get_Language;

   -----------------------
   -- Is_Valid_Position --
   -----------------------

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Gint;
      Column : Gint := 0) return Boolean
   is
      Iter : Gtk_Text_Iter;
   begin
      --  First check that Line does not exceed the number of lines
      --  in the buffer.

      if Line >= Get_Line_Count (Buffer) then
         return False;
      end if;

      --  At this point, we know that the line number is valid. If the column
      --  number is 0, then no need to verify the column number as well.
      --  Column 0 always exists and this speeds up the query.

      if Column = 0 then
         return True;
      end if;

      --  Get a text iterator at the begining of the line number Line.
      --  Then, move it to the end of the line to get the number of
      --  characters in this line.

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, 0);
      Forward_To_Line_End (Iter);

      --  Check that Column does not exceed the number of character in
      --  in the current line.

      if Column > Get_Line_Offset (Iter) then
         return False;
      end if;

      --  At this point, we passed all the checks, so the position is valid.
      return True;
   end Is_Valid_Position;

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Natural := 1) return Boolean
   is
      Buffer_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Buffer, Line);
   begin
      if Buffer_Line = 0 then
         Trace (Me, "invalid buffer line");
         return False;
      end if;

      return Is_Valid_Position
        (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1));
   end Is_Valid_Position;

   -------------------------
   -- Set_Cursor_Position --
   -------------------------

   procedure Set_Cursor_Position
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Column  : Gint)
   is
      Iter : Gtk_Text_Iter;
   begin
      Assert (Me, Is_Valid_Position (Buffer, Line, Column),
              "Invalid position for Set_Cursor_Position "
              & Full_Name (Get_Filename (Buffer)) & Line'Img & Column'Img);

      if not Buffer.Inserting then
         --  At this point, we know that the (Line, Column) position is
         --  valid, so we can safely get the iterator at this position.
         Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
         Place_Cursor (Buffer, Iter);
      end if;
   end Set_Cursor_Position;

   procedure Set_Cursor_Position
     (Buffer  : access Source_Buffer_Record;
      Line    : Editable_Line_Type;
      Column  : Natural)
   is
      Buffer_Line : Buffer_Line_Type :=
        Get_Buffer_Line (Buffer, Line);
   begin
      --  If the line is in a non-visible line, make the line visible.

      if Buffer_Line = 0 then
         Unfold_Line (Buffer, Line);
         Buffer_Line := Get_Buffer_Line (Buffer, Line);
      end if;

      Set_Cursor_Position (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1));
   end Set_Cursor_Position;

   ---------------------------------
   -- Get_Iter_At_Screen_Position --
   ---------------------------------

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk_Text_Iter;
      Line   : Gint;
      Column : Gint)
   is
      Result  : Boolean := True;
      Current : Gint := 0;
      Tab_Len : constant Gint := Get_Pref (Buffer.Kernel, Tab_Width);
   begin
      Assert (Me, Is_Valid_Position (Buffer, Line, 0),
              "Invalid position for Set_Screen_Position "
              & Full_Name (Get_Filename (Buffer)) & Line'Img);

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, 0);

      --  We have to test Result, in case Iter was pointing after the end of
      --  the buffer.

      while Result and then Current < Column loop
         if Get_Char (Iter) = ASCII.HT then
            Current := Current + Tab_Len - (Current mod Tab_Len);
         else
            Current := Current + 1;
         end if;

         Forward_Char (Iter, Result);
      end loop;
   end Get_Iter_At_Screen_Position;

   -------------------------
   -- Set_Screen_Position --
   -------------------------

   procedure Set_Screen_Position
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Column  : Gint)
   is
      Start   : Gtk_Text_Iter;
   begin
      Get_Iter_At_Screen_Position (Buffer, Start, Line, Column);
      Place_Cursor (Buffer, Start);
   end Set_Screen_Position;

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk_Text_Iter;
      Line   : out Gint;
      Column : out Gint)
   is
      Start   : Gtk_Text_Iter;
      Result  : Boolean := True;
      Tab_Len : constant Gint := Get_Pref (Buffer.Kernel, Tab_Width);

   begin
      Line   := Get_Line (Iter);
      Column := 0;
      Get_Iter_At_Line_Offset (Buffer, Start, Line, 0);

      --  We have to test Result, in case Iter was pointing after the end of
      --  the buffer.

      while Result and then not Equal (Start, Iter) loop
         if Get_Char (Start) = ASCII.HT then
            Column := Column + Tab_Len - (Column mod Tab_Len);
         else
            Column := Column + 1;
         end if;

         Forward_Char (Start, Result);
      end loop;
   end Get_Screen_Position;

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Get_Screen_Position (Buffer, Iter, Line, Column);
   end Get_Screen_Position;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Line   := Get_Line (Iter);
      Column := Get_Line_Offset (Iter);
   end Get_Cursor_Position;

   --------------------------
   -- Get_Selection_Bounds --
   --------------------------

   procedure Get_Selection_Bounds
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : out Gint;
      Start_Column : out Gint;
      End_Line     : out Gint;
      End_Column   : out Gint;
      Found        : out Boolean)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Found);

      if Found then
         Start_Line   := Get_Line (Start_Iter);
         Start_Column := Get_Line_Offset (Start_Iter);
         End_Line     := Get_Line (End_Iter);
         End_Column   := Get_Line_Offset (End_Iter);
      else
         Start_Line   := 0;
         Start_Column := 0;
         End_Line     := 0;
         End_Column   := 0;
      end if;
   end Get_Selection_Bounds;

   -------------------
   -- Get_Selection --
   -------------------

   function Get_Selection
     (Buffer : access Source_Buffer_Record) return String
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Found      : Boolean;

   begin
      Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Found);

      if Found then
         return Get_Slice
           (Buffer, Get_Line (Start_Iter), Get_Line_Offset (Start_Iter),
            Get_Line (End_Iter), Get_Line_Offset (End_Iter));
      else
         return "";
      end if;
   end Get_Selection;

   ------------
   -- Search --
   ------------

   procedure Search
     (Buffer             : access Source_Buffer_Record;
      Pattern            : String;
      Case_Sensitive     : Boolean := True;
      Whole_Word         : Boolean := False;
      Search_Forward     : Boolean := True;
      From_Line          : Gint := 0;
      From_Column        : Gint := 0;
      Found              : out Boolean;
      Match_Start_Line   : out Gint;
      Match_Start_Column : out Gint;
      Match_End_Line     : out Gint;
      Match_End_Column   : out Gint)
   is
      pragma Unreferenced
        (Buffer, Pattern, Case_Sensitive, Whole_Word,
         Search_Forward, From_Line, From_Column);

   begin
      --  ??? Unimplemented
      Found := False;
      Match_Start_Line   := 0;
      Match_Start_Column := 0;
      Match_End_Line     := 0;
      Match_End_Column   := 0;
   end Search;

   ---------------
   -- Get_Slice --
   ---------------

   function Get_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return Gtkada.Types.Chars_Ptr
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;

   begin
      pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);

      if End_Line = -1 then
         Get_End_Iter (Buffer, End_Iter);
      else
         pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));
         Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
      end if;

      return Get_Text (Buffer, Start_Iter, End_Iter, True);
   end Get_Slice;

   function Get_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return String
   is
      Str : constant Gtkada.Types.Chars_Ptr :=
        Get_Slice (Buffer, Start_Line, Start_Column, End_Line, End_Column);
      S   : constant String := Value (Str);

   begin
      g_free (Str);
      return S;
   end Get_Slice;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
      Text        : String;
      Enable_Undo : Boolean := True)
   is
      Iter                     : Gtk_Text_Iter;
      Previous_Inserting_Value : constant Boolean := Buffer.Inserting;
   begin
      pragma Assert (Is_Valid_Position (Buffer, Line, Column));

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Inserting := True;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
      Insert (Buffer, Iter, Text);

      if not Enable_Undo then
         Buffer.Inserting := Previous_Inserting_Value;
      end if;

      Request_Blocks (Buffer);
   end Insert;

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Natural;
      Text        : String;
      Enable_Undo : Boolean := True)
   is
      Buffer_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Buffer, Line);
   begin
      if Buffer_Line = 0 then
         Trace (Me, "invalid buffer line");
         return;
      end if;

      Insert (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1), Text,
              Enable_Undo);
   end Insert;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
      Length      : Gint;
      Enable_Undo : Boolean := True)
   is
      Iter                     : Gtk_Text_Iter;
      End_Iter                 : Gtk_Text_Iter;
      Result                   : Boolean;
      Previous_Inserting_Value : constant Boolean := Buffer.Inserting;
   begin
      pragma Assert (Is_Valid_Position (Buffer, Line, Column));

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Inserting := True;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
      Copy (Iter, End_Iter);
      Forward_Chars (End_Iter, Length, Result);
      Delete_Interactive (Buffer, Iter, End_Iter, True, Result);

      if not Enable_Undo then
         Buffer.Inserting := Previous_Inserting_Value;
      end if;

      Request_Blocks (Buffer);
   end Delete;

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Natural;
      Length      : Natural;
      Enable_Undo : Boolean := True)
   is
      Buffer_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Buffer, Line);

   begin
      if Buffer_Line = 0 then
         Trace (Me, "invalid buffer line");
         return;
      end if;

      Delete (Buffer, Gint (Buffer_Line - 1), Gint (Column - 1), Gint (Length),
              Enable_Undo);
   end Delete;

   -------------------
   -- Replace_Slice --
   -------------------

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Text         : String;
      Enable_Undo  : Boolean := True)
   is
      Start_Iter               : Gtk_Text_Iter;
      End_Iter                 : Gtk_Text_Iter;
      Previous_Inserting_Value : constant Boolean := Buffer.Inserting;

   begin
      pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
      pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));

      if not Buffer.Inserting then
         End_Action (Buffer);
      end if;

      if not Enable_Undo then
         Buffer.Inserting := True;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);
      Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);

      --  Currently, Gtk_Text_Buffer does not export a service to replace
      --  some text, so we delete the slice first, then insert the text...
      Delete (Buffer, Start_Iter, End_Iter);

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);
      Insert (Buffer, Start_Iter, Text);

      if not Enable_Undo then
         Buffer.Inserting := Previous_Inserting_Value;
      end if;

      Request_Blocks (Buffer);
   end Replace_Slice;

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      End_Line     : Editable_Line_Type;
      End_Column   : Natural;
      Text         : String;
      Enable_Undo  : Boolean := True)
   is
      Buffer_Start_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Buffer, Start_Line);
      Buffer_End_Line : constant Buffer_Line_Type :=
        Get_Buffer_Line (Buffer, End_Line);
   begin
      if Buffer_Start_Line = 0 or else Buffer_End_Line = 0 then
         Trace (Me, "invalid buffer line");
         return;
      end if;

      Replace_Slice
        (Buffer,
         Gint (Buffer_Start_Line - 1),
         Gint (Start_Column - 1),
         Gint (Buffer_End_Line - 1),
         Gint (End_Column - 1),
         Text,
         Enable_Undo);
   end Replace_Slice;

   ----------------
   -- Select_All --
   ----------------

   procedure Select_All (Buffer : access Source_Buffer_Record) is
      Insert_Mark    : constant Gtk_Text_Mark := Get_Insert (Buffer);
      Selection_Mark : constant Gtk_Text_Mark := Get_Selection_Bound (Buffer);
      Start_Iter     : Gtk_Text_Iter;
      End_Iter       : Gtk_Text_Iter;
   begin
      Get_Start_Iter (Buffer, Start_Iter);
      Get_End_Iter (Buffer, End_Iter);
      --  Move the selection_bound mark to the begining of the buffer, and
      --  the insert mark at the end, thus creating the selection on the
      --  entire buffer
      Move_Mark (Buffer, Selection_Mark, Start_Iter);
      Move_Mark (Buffer, Insert_Mark, End_Iter);
   end Select_All;

   -------------------
   -- Select_Region --
   -------------------

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Expand_Tabs  : Boolean := True)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if Expand_Tabs then
         Get_Iter_At_Screen_Position
           (Buffer, Start_Iter, Start_Line, Start_Column);
         Get_Iter_At_Screen_Position (Buffer, End_Iter, End_Line, End_Column);

      else
         if not Is_Valid_Position (Buffer, Start_Line, Start_Column)
           or else not Is_Valid_Position (Buffer, End_Line, End_Column)
         then
            Trace (Me, "invalid position in Select_Region, aborting.");
            return;
         end if;

         Get_Iter_At_Line_Offset
           (Buffer, Start_Iter, Start_Line, Start_Column);
         Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
      end if;

      Move_Mark_By_Name (Buffer, "selection_bound", Start_Iter);
      Move_Mark_By_Name (Buffer, "insert", End_Iter);
   end Select_Region;

   ----------------------
   -- Highlight_Region --
   ----------------------

   procedure Highlight_Region
     (Buffer : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if not Is_Valid_Position (Buffer, Start_Line, Start_Column)
        or else not Is_Valid_Position (Buffer, End_Line, End_Column)
      then
         Trace (Me, "invalid position in Highlight_Region, aborting.");
         return;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);
      Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
      Apply_Tag (Buffer, Buffer.HL_Region_Tag, Start_Iter, End_Iter);
   end Highlight_Region;

   ------------------------
   -- Unhighlight_Region --
   ------------------------

   procedure Unhighlight_Region
     (Buffer : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if not Is_Valid_Position (Buffer, Start_Line, Start_Column)
        or else not Is_Valid_Position (Buffer, End_Line, End_Column)
      then
         Trace (Me, "invalid position in Unhighlight_Region, aborting.");
         return;
      end if;

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);
      Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
      Remove_Tag (Buffer, Buffer.HL_Region_Tag, Start_Iter, End_Iter);
   end Unhighlight_Region;

   ---------------------
   -- Unhighlight_All --
   ---------------------

   procedure Unhighlight_All (Buffer : access Source_Buffer_Record) is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Start_Iter (Buffer, Start_Iter);
      Get_End_Iter (Buffer, End_Iter);
      Remove_Tag (Buffer, Buffer.HL_Region_Tag, Start_Iter, End_Iter);
   end Unhighlight_All;

   --------------------
   -- Lines_Add_Hook --
   --------------------

   procedure Lines_Add_Hook
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type) is
   begin
      --  Resynchronize the arrays that need to be synchronized with line
      --  numbers.
      Add_Lines (Buffer, Start, Number);

      --  Parse the block information.
      Request_Blocks (Buffer);
   end Lines_Add_Hook;

   ------------------------------
   -- Lines_Remove_Hook_Before --
   ------------------------------

   procedure Lines_Remove_Hook_Before
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type) is
   begin
      --  Resynchronize the arrays that need to be synchronized with line
      --  numbers.
      Remove_Lines (Buffer, Start_Line, End_Line);

      --  It is necessary to set the fields Buffer.First_Removed_Line and
      --  Buffer.Last_Removed_Line because the parameters for the
      --  "delete_range" signal are invalid when we connect after the actual
      --  deletion has been done.

      Buffer.First_Removed_Line := Start_Line;
      Buffer.Last_Removed_Line  := End_Line;
   end Lines_Remove_Hook_Before;

   -----------------------------
   -- Lines_Remove_Hook_After --
   -----------------------------

   procedure Lines_Remove_Hook_After
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type)
   is
      pragma Unreferenced (Start_Line, End_Line);
   begin
      --  Parse the block information.
      Request_Blocks (Buffer);
   end Lines_Remove_Hook_After;

   ---------------------
   -- End_Action_Hook --
   ---------------------

   procedure End_Action_Hook (Buffer : access Source_Buffer_Record'Class) is
   begin
      if not Is_Empty (Buffer.Completion) then
         Clear (Buffer.Completion);
      end if;
   end End_Action_Hook;

   ----------------
   -- End_Action --
   ----------------

   procedure End_Action (Buffer : access Source_Buffer_Record'Class) is
      Command : constant Editor_Command :=
        Editor_Command (Buffer.Current_Command);

   begin
      End_Action_Hook (Buffer);

      if not Is_Null_Command (Command) then
         Buffer.Current_Command := null;
      end if;
   end End_Action;

   -------------------------
   -- External_End_Action --
   -------------------------

   procedure External_End_Action (Buffer : access Source_Buffer_Record) is
      Command : constant Editor_Command :=
        Editor_Command (Buffer.Current_Command);

   begin
      if not Is_Null_Command (Command) then
         End_Action (Buffer);
      end if;
   end External_End_Action;

   ----------
   -- Redo --
   ----------

   procedure Redo (Buffer : access Source_Buffer_Record) is
   begin
      Redo (Buffer.Queue);
   end Redo;

   ----------
   -- Undo --
   ----------

   procedure Undo (Buffer : access Source_Buffer_Record) is
      Command : constant Editor_Command :=
        Editor_Command (Buffer.Current_Command);

   begin
      if not Is_Null_Command (Command) then
         End_Action (Buffer);
      end if;

      Undo (Buffer.Queue);
   end Undo;

   -------------
   -- Enqueue --
   -------------

   procedure Enqueue
     (Buffer  : access Source_Buffer_Record;
      Command : Command_Access) is
   begin
      if Buffer.Saved_Position > Get_Position (Buffer.Queue) then
         Buffer.Saved_Position := -1;
      end if;

      Enqueue (Buffer.Queue, Command);
   end Enqueue;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Buffer : access Source_Buffer_Record)
     return Glide_Kernel.Kernel_Handle is
   begin
      return Buffer.Kernel;
   end Get_Kernel;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename
     (Buffer : access Source_Buffer_Record) return VFS.Virtual_File is
   begin
      return Buffer.Filename;
   end Get_Filename;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Buffer : access Source_Buffer_Record; Name : VFS.Virtual_File) is
   begin
      Buffer.Filename := Name;
   end Set_Filename;

   -------------------------
   -- Get_File_Identifier --
   -------------------------

   function Get_File_Identifier
     (Buffer : access Source_Buffer_Record) return VFS.Virtual_File is
   begin
      return Buffer.File_Identifier;
   end Get_File_Identifier;

   -------------------------
   -- Set_File_Identifier --
   -------------------------

   procedure Set_File_Identifier
     (Buffer : access Source_Buffer_Record; Name : VFS.Virtual_File) is
   begin
      Buffer.File_Identifier := Name;
   end Set_File_Identifier;

   ---------------------------
   -- Source_Lines_Revealed --
   ---------------------------

   procedure Source_Lines_Revealed
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type)
   is
      Context     : File_Area_Context_Access;
      First, Last : Editable_Line_Type;
   begin
      Context := new File_Area_Context;
      Set_Context_Information
        (Context,
         Buffer.Kernel,
         Src_Editor_Module_Id);

      if Buffer.Filename /= VFS.No_File then
         Set_File_Information (Context, Buffer.Filename);

      elsif Buffer.File_Identifier /= VFS.No_File then
         Set_File_Information (Context, Buffer.File_Identifier);
      end if;

      --  Find the editable boundaries.

      for J in reverse 1 .. Start_Line loop
         First := Get_Editable_Line (Buffer, J);
         exit when First /= 0;
      end loop;

      for J in End_Line .. Buffer.Line_Data'Last loop
         Last := Get_Editable_Line (Buffer, J);
         exit when Last /= 0;
      end loop;

      if First = 0 then
         First := 1;
      end if;

      if Last = 0 then
         Last := Buffer.Last_Editable_Line;
      end if;

      Set_Area_Information (Context, Integer (First), Integer (Last));
      Glide_Kernel.Source_Lines_Revealed (Buffer.Kernel, Context);
      Unref (Selection_Context_Access (Context));
   end Source_Lines_Revealed;

   ---------------------
   -- Check_Timestamp --
   ---------------------

   function Check_Timestamp
     (Buffer   : access Source_Buffer_Record;
      Ask_User : Boolean := False;
      Force    : Boolean := False) return Boolean
   is
      New_Timestamp : Timestamp;
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);

      Response : Gtk_Response_Type;
      Success : Boolean;
      Line, Column : Gint;
   begin
      if Buffer.Filename /= VFS.No_File
        and then Is_Regular_File (Buffer.Filename)
      then
         New_Timestamp := To_Timestamp (File_Time_Stamp (Buffer.Filename));

         if New_Timestamp > Buffer.Timestamp then
            if Force then
               Response := Gtk_Response_Accept;
            else
               if not Ask_User then
                  return False;
               end if;

               Dialog := Create_Gtk_Dialog
                 (Msg         => Base_Name (Buffer.Filename)
                  & (-" changed on disk. Really edit ?")
                  & ASCII.LF & ASCII.LF
                  & (-"Clicking on Revert will reload the file from disk.")
                  & ASCII.LF
                  & (-"Clicking on Yes will keep the current file in memory."),
                  Dialog_Type   => Confirmation,
                  Title         => -"File changed on disk",
                  Justification => Justify_Left,
                  Parent        => Get_Main_Window (Buffer.Kernel));

               Button := Add_Button (Dialog, Stock_Yes, Gtk_Response_Yes);
               Button := Add_Button
                 (Dialog, Stock_Revert_To_Saved, Gtk_Response_Accept);

               Show_All (Dialog);
               Response := Run (Dialog);
               Destroy (Dialog);
            end if;

            case Response is
               when Gtk_Response_Yes =>
                  Buffer.Timestamp := New_Timestamp;

               when Gtk_Response_Accept =>
                  Get_Cursor_Position (Buffer, Line, Column);
                  Load_File
                    (Buffer,
                     Filename        => Buffer.Filename,
                     Lang_Autodetect => True,
                     Success         => Success);

                  if Is_Valid_Position (Buffer, Line, Column) then
                     Set_Cursor_Position (Buffer, Line, Column);
                  elsif Is_Valid_Position (Buffer, Line, 0) then
                     Set_Cursor_Position (Buffer, Line, 0);
                  end if;

               when others =>
                  null;
            end case;
         end if;
      end if;

      return True;
   end Check_Timestamp;

   ---------
   -- Ref --
   ---------

   procedure Ref (Buffer : access Source_Buffer_Record) is
   begin
      Buffer.References := Buffer.References + 1;
      Buffer.Total_References := Buffer.Total_References + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Buffer : access Source_Buffer_Record) is
   begin
      Buffer.References := Buffer.References - 1;

      if Buffer.References = 0 then
         Buffer_Destroy (Buffer);
      end if;
   end Unref;

   -------------------------
   -- Get_Total_Ref_Count --
   -------------------------

   function Get_Total_Ref_Count
     (Buffer : access Source_Buffer_Record)
      return Integer
   is
   begin
      return Buffer.Total_References;
   end Get_Total_Ref_Count;

   -------------------
   -- Get_Ref_Count --
   -------------------

   function Get_Ref_Count
     (Buffer : access Source_Buffer_Record)
      return Integer is
   begin
      return Buffer.References;
   end Get_Ref_Count;

   -----------
   -- Clear --
   -----------

   procedure Clear (Data : in out Completion_Data) is
   begin
      GNAT.OS_Lib.Free (Data.Prefix);

      String_List_Utils.String_List.Free (Data.List);
      Data.Node := String_List_Utils.String_List.Null_Node;

      Data.Top_Reached := False;
      Data.Bottom_Reached := False;
      Data.Complete := False;
      Data.Backwards := False;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Data : Completion_Data) return Boolean is
   begin
      return (Data.Prefix = null);
   end Is_Empty;

   ------------------
   -- Add_Controls --
   ------------------

   procedure Add_Controls (Buffer : access Source_Buffer_Record) is
      Undo_Redo : Undo_Redo_Information;
   begin
      Undo_Redo := Undo_Redo_Data.Get (Buffer.Kernel, Undo_Redo_Id);

      Set_Controls
        (Buffer.Queue,
         Undo_Redo.Undo_Button,
         Undo_Redo.Redo_Button,
         Undo_Redo.Undo_Menu_Item,
         Undo_Redo.Redo_Menu_Item);
      Buffer.Controls_Set := True;
   end Add_Controls;

   ---------------------
   -- Remove_Controls --
   ---------------------

   procedure Remove_Controls (Buffer : access Source_Buffer_Record) is
   begin
      Unset_Controls (Buffer.Queue);
      Buffer.Controls_Set := False;
   end Remove_Controls;

   ----------------------------
   -- Get_Total_Column_Width --
   ----------------------------

   function Get_Total_Column_Width
     (Buffer : access Source_Buffer_Record) return Natural is
   begin
      return Buffer.Total_Column_Width;
   end Get_Total_Column_Width;

   ------------------------
   -- Line_Needs_Refresh --
   ------------------------

   function Line_Needs_Refresh
     (Buffer : access Source_Buffer_Record;
      Line   : Buffer_Line_Type) return Boolean
   is
      Editable_Line : Editable_Line_Type;
   begin
      if Buffer.Line_Data (Line).Side_Info_Data /= null then
         for J in Buffer.Line_Data (Line).Side_Info_Data'Range loop
            if not Buffer.Line_Data (Line).Side_Info_Data (J).Set then
               return True;
            end if;
         end loop;
      end if;

      Editable_Line := Get_Editable_Line (Buffer, Line);

      if Editable_Line = 0 then
         return False;
      end if;

      if Buffer.Editable_Lines (Editable_Line).Side_Info_Data /= null then
         for J in Buffer.Editable_Lines
           (Editable_Line).Side_Info_Data'Range
         loop
            if not Buffer.Editable_Lines
              (Editable_Line).Side_Info_Data (J).Set
            then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Line_Needs_Refresh;

   ----------------------
   -- Create_Side_Info --
   ----------------------

   procedure Create_Side_Info
     (Buffer : access Source_Buffer_Record;
      Line   : Buffer_Line_Type)
   is
      Buffer_Lines   : Line_Data_Array_Access renames Buffer.Line_Data;
      Columns_Config : Line_Info_Display_Array_Access
        renames Buffer.Buffer_Line_Info_Columns.all;
   begin
      if Columns_Config /= null then
         if Buffer_Lines (Line).Side_Info_Data = null then
            Buffer_Lines (Line).Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config'Range);

            for K in Columns_Config'Range loop
               Buffer_Lines (Line).Side_Info_Data (K) :=
                 (null,
                  Width => -1,
                  Set   => not Columns_Config (K).Every_Line);
            end loop;
         end if;
      end if;
   end Create_Side_Info;

   ----------------------
   -- Create_Side_Info --
   ----------------------

   procedure Create_Side_Info
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type)
   is
      Editable_Lines   : Editable_Line_Array_Access
        renames Buffer.Editable_Lines;
      Columns_Config : Line_Info_Display_Array_Access
        renames Buffer.Editable_Line_Info_Columns.all;
   begin
      if Columns_Config /= null then
         if Editable_Lines (Line).Side_Info_Data = null then
            Editable_Lines (Line).Side_Info_Data := new
              Line_Info_Width_Array (Columns_Config'Range);

            for K in Columns_Config'Range loop
               Editable_Lines (Line).Side_Info_Data (K) :=
                 (null,
                  Width => -1,
                  Set   => not Columns_Config (K).Every_Line);
            end loop;
         end if;
      end if;
   end Create_Side_Info;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   function Needs_To_Be_Saved
     (Buffer : access Source_Buffer_Record'Class)
      return Boolean is
   begin
      case Get_Status (Buffer) is
         when Unmodified | Saved =>
            return False;

         when Modified =>
            if Buffer.Last_Saved_Position /= Get_Position (Buffer.Queue) then
               return True;

            else
               return False;
            end if;
      end case;
   end Needs_To_Be_Saved;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Buffer : access Source_Buffer_Record)
      return Status_Type is
   begin
      if (Undo_Queue_Empty (Buffer.Queue)
          and then Redo_Queue_Empty (Buffer.Queue))
        or else
          (Buffer.Saved_Position = Get_Position (Buffer.Queue)
           and then Buffer.Saved_Position = 0)
      then
         return Unmodified;
      else
         if Buffer.Saved_Position = Get_Position (Buffer.Queue) then
            return Saved;
         else
            return Modified;
         end if;
      end if;
   end Get_Status;

   ---------------------------
   -- Get_Extra_Information --
   ---------------------------

   function Get_Extra_Information
     (Buffer : Source_Buffer)
      return Extra_Information_Array_Access is
   begin
      return Buffer.Extra_Information;
   end Get_Extra_Information;

   ---------------------------
   -- Set_Line_Highlighting --
   ---------------------------

   procedure Set_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Buffer_Line_Type;
      Id     : String;
      Set    : Boolean)
   is
      Category   : Natural;
      Last_Index : Natural;
   begin
      Category := Lookup_Category (Id);

      if Category = 0 then
         --  Could not identify highlighting category
         return;
      end if;

      if Line = 0 then
         return;
      end if;

      if Line < Editor.Line_Data'First
        or else Line > Editor.Line_Data'Last
      then
         Trace (Me, "Wrong line number: " & Image (Integer (Line)));
         return;
      end if;

      Last_Index := Get_Last_Index;

      if Editor.Line_Data (Line).Enabled_Highlights = null then
         --  If we are removing a highlight where no highlight is defined,
         --  we can exit immediately.

         if not Set then
            return;
         end if;

         Editor.Line_Data (Line).Enabled_Highlights :=
           new Boolean_Array (1 .. Last_Index);
         Editor.Line_Data (Line).Enabled_Highlights.all :=
           (others   => False);
         Editor.Line_Data (Line).Enabled_Highlights (Category) := Set;

         if Set then
            Editor.Line_Data (Line).Current_Highlight := Get_GC (Category);
         else
            for J in Editor.Line_Data (Line).Enabled_Highlights'Range loop
               if Editor.Line_Data (Line).Enabled_Highlights (J) then
                  Editor.Line_Data (Line).Current_Highlight := Get_GC (J);
               end if;
            end loop;
         end if;

         return;
      end if;

      if Editor.Line_Data (Line).Enabled_Highlights'Last < Last_Index then
         declare
            A : Boolean_Array_Access;
         begin
            A := new Boolean_Array (1 .. Last_Index);
            A (1 .. Editor.Line_Data (Line).Enabled_Highlights'Last)
              := Editor.Line_Data (Line).Enabled_Highlights.all;
            A (Editor.Line_Data (Line).Enabled_Highlights'Last + 1
                 .. Last_Index) := (others => False);
            Unchecked_Free (Editor.Line_Data (Line).Enabled_Highlights);
            Editor.Line_Data (Line).Enabled_Highlights := A;
         end;
      end if;

      Editor.Line_Data (Line).Enabled_Highlights (Category) := Set;

      for J in Editor.Line_Data (Line).Enabled_Highlights'Range loop
         if Editor.Line_Data (Line).Enabled_Highlights (J) then
            Editor.Line_Data (Line).Current_Highlight := Get_GC (J);
            return;
         end if;
      end loop;

      --  If we reach this stage, no highlighting was found, therefore we
      --  remove the current GC.

      Editor.Line_Data (Line).Current_Highlight := null;
   end Set_Line_Highlighting;

   --------------------------
   -- Add_Line_Higlighting --
   --------------------------

   procedure Add_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Id     : String)
   is
      The_Line : Buffer_Line_Type;
   begin
      if Line = 0 then
         for J in Editor.Line_Data'Range loop
            Set_Line_Highlighting (Editor, J, Id, True);
         end loop;
      else
         The_Line := Get_Buffer_Line (Editor, Line);

         if The_Line /= 0 then
            Set_Line_Highlighting (Editor, The_Line, Id, True);
         end if;
      end if;

      Line_Highlights_Changed (Editor);
   end Add_Line_Highlighting;

   ------------------------------
   -- Remove_Line_Highlighting --
   ------------------------------

   procedure Remove_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Id     : String)
   is
      The_Line : Buffer_Line_Type;
   begin
      if Line = 0 then
         for J in Editor.Line_Data'Range loop
            Set_Line_Highlighting (Editor, J, Id, False);
         end loop;
      else
         The_Line := Get_Buffer_Line (Editor, Line);

         if The_Line /= 0 then
            Set_Line_Highlighting (Editor, The_Line, Id, False);
         end if;
      end if;

      Line_Highlights_Changed (Editor);
   end Remove_Line_Highlighting;

   ----------------------
   -- Get_Highlight_GC --
   ----------------------

   function Get_Highlight_GC
     (Editor : access Source_Buffer_Record;
      Line   : Buffer_Line_Type) return Gdk_GC is
   begin
      if Line = 0 then
         return null;
      end if;

      if Editor.Line_Data /= null
        and then Line <= Editor.Line_Data'Last
      then
         return Editor.Line_Data (Line).Current_Highlight;
      end if;

      return null;
   end Get_Highlight_GC;

   ---------------
   -- Get_Block --
   ---------------

   function Get_Block
     (Editor : access Source_Buffer_Record;
      Line   : Buffer_Line_Type) return Block_Record is
   begin
      if Line = 0 then
         return New_Block;
      end if;

      if Editor.Line_Data /= null
        and then Line <= Editor.Line_Data'Last
      then
         if Editor.Line_Data (Line).Block = null then
            return New_Block;
         else
            return Editor.Line_Data (Line).Block.all;
         end if;
      end if;

      return New_Block;
   end Get_Block;

   ---------------------------
   -- Has_Block_Information --
   ---------------------------

   function Has_Block_Information
     (Editor : access Source_Buffer_Record) return Boolean is
   begin
      return Editor.Parse_Blocks;
   end Has_Block_Information;

   -------------------
   -- Should_Indent --
   -------------------

   function Should_Indent (Buffer : Source_Buffer) return Boolean is
      Lang          : constant Language_Access := Get_Language (Buffer);
      Indent_Params : Indent_Parameters;
      Use_Tabs      : Boolean := False;
      Indent_Style  : Indentation_Kind;
   begin
      if Lang = null
        or else not Get_Language_Context (Lang).Can_Indent
      then
         return False;
      end if;

      Get_Indentation_Parameters
        (Lang         => Lang,
         Use_Tabs     => Use_Tabs,
         Params       => Indent_Params,
         Indent_Style => Indent_Style);

      return Indent_Style /= None;
   end Should_Indent;

   --------------------
   -- Do_Indentation --
   --------------------

   function Do_Indentation (Buffer : Source_Buffer) return Boolean is
      Iter, End_Pos : Gtk_Text_Iter;
      Result : Boolean;
   begin
      Get_Selection_Bounds (Buffer, Iter, End_Pos, Result);

      if Result then
         --  Do not consider a line selected if only the first character
         --  is selected.

         if Get_Line_Offset (End_Pos) = 0 then
            Backward_Char (End_Pos, Result);
         end if;

         --  Do not consider a line selected if only the last character is
         --  selected.

         if Ends_Line (Iter) then
            Forward_Char (Iter, Result);
         end if;

      else
         Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
         Copy (Iter, Dest => End_Pos);
      end if;

      return Do_Indentation (Buffer, Iter, End_Pos);
   end Do_Indentation;

   --------------------
   -- Do_Indentation --
   --------------------

   function Do_Indentation
     (Buffer      : Source_Buffer;
      From, To    : Gtk_Text_Iter) return Boolean
   is
      Lang          : constant Language_Access := Get_Language (Buffer);
      Indent_Style  : Indentation_Kind;
      End_Pos       : Gtk_Text_Iter;
      Iter          : Gtk_Text_Iter;
      Indent        : Natural;
      C_Str         : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Slice         : Unchecked_String_Access;
      pragma Suppress (Access_Check, Slice);
      Line          : Gint;
      Current_Line  : Gint;
      Offset        : Integer;
      Cursor_Line, Cursor_Offset : Gint;
      Editable_Line : Editable_Line_Type;
      Global_Offset : Integer := 0;

      Line_Start    : Natural := 0;
      Line_End      : Natural := 0;
      Result        : Boolean;
      Buffer_Text   : GNAT.OS_Lib.String_Access;
      Index         : Integer;
      Replace_Cmd   : Editor_Replace_Slice;
      Tabs_Used     : Boolean := False;
      Indent_Params : Indent_Parameters;
      Use_Tabs      : Boolean := False;

      function Blank_Slice (Count : Natural; Use_Tabs : Boolean) return String;
      --  Return a string representing count blanks.
      --  If Use_Tabs is True, use ASCII.HT characters as much as possible,
      --  otherwise use only spaces.

      procedure Find_Non_Blank (Last : Natural);
      --  Set Index to the first non blank character in Slice (Index .. Last)
      --  Also set Tabs_Used to True if any tab character is found.

      procedure Local_Next_Indentation
        (Lang          : Language_Access;
         Buffer        : String;
         Indent        : out Natural;
         Indent_Params : Indent_Parameters);
      --  Wrapper around Next_Indentation to take into account Indent_Style.

      procedure Skip_To_First_Non_Blank (Iter : in out Gtk_Text_Iter);
      --  Move Iter to the first non-blank character on the line

      procedure Create_Command;
      --  Create the replace slice command.

      -----------------------------
      -- Skip_To_First_Non_Blank --
      -----------------------------

      procedure Skip_To_First_Non_Blank (Iter : in out Gtk_Text_Iter) is
         Result : Boolean := True;
      begin
         Set_Line_Offset (Iter, 0);
         while Result
           and then not Ends_Line (Iter)
           and then Is_Space (Get_Char (Iter))
         loop
            Forward_Char (Iter, Result);
         end loop;
      end Skip_To_First_Non_Blank;

      -----------------
      -- Blank_Slice --
      -----------------

      function Blank_Slice
        (Count : Natural; Use_Tabs : Boolean) return String is
      begin
         if Use_Tabs then
            return (1 .. Count / Indent_Params.Tab_Width => ASCII.HT) &
              (1 .. Count mod Indent_Params.Tab_Width => ' ');
         else
            return (1 .. Count => ' ');
         end if;
      end Blank_Slice;

      --------------------
      -- Find_Non_Blank --
      --------------------

      procedure Find_Non_Blank (Last : Natural) is
      begin
         while Index <= Last
           and then (Buffer_Text (Index) = ' '
                     or else Buffer_Text (Index) = ASCII.HT)
         loop
            if Buffer_Text (Index) = ASCII.HT then
               Tabs_Used := True;
            end if;

            Index := Index + 1;
         end loop;
      end Find_Non_Blank;

      ----------------------------
      -- Local_Next_Indentation --
      ----------------------------

      procedure Local_Next_Indentation
        (Lang          : Language_Access;
         Buffer        : String;
         Indent        : out Natural;
         Indent_Params : Indent_Parameters)
      is
         Next_Indent   : Natural;
      begin
         if Indent_Style = Simple then
            Next_Indentation
              (Language_Root (Lang.all)'Access,
               Buffer, Indent, Next_Indent, Indent_Params);

         else
            Next_Indentation
              (Lang, Buffer, Indent, Next_Indent, Indent_Params);
         end if;
      end Local_Next_Indentation;

      --------------------
      -- Create_Command --
      --------------------

      procedure Create_Command is
      begin
         --  Only indent if the current indentation is wrong
         --  ??? Would be nice to indent the whole selection at once,
         --  this would make the undo/redo behavior more intuitive.
         if Tabs_Used or else Offset /= Indent then
            Editable_Line := Get_Editable_Line
              (Buffer, Buffer_Line_Type (Current_Line + 1));
            Create
              (Replace_Cmd,
               Buffer,
               Editable_Line,
               1,
               Editable_Line,
               Natural (Offset + 1),
               Blank_Slice (Indent, Use_Tabs));

            Enqueue (Buffer, Command_Access (Replace_Cmd));
            Global_Offset := Global_Offset - Offset + Indent;
         end if;
      end Create_Command;

   begin  --  Do_Indentation
      if Lang = null
        or else not Get_Language_Context (Lang).Can_Indent
      then
         return False;
      end if;

      Get_Indentation_Parameters
        (Lang         => Lang,
         Use_Tabs     => Use_Tabs,
         Params       => Indent_Params,
         Indent_Style => Indent_Style);

      if Indent_Style = None then
         return False;
      end if;

      --  Where is the cursor (so we can keep it on the same line)
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Cursor_Line   := Get_Line (Iter);
      Cursor_Offset := Get_Line_Offset (Iter);

      --  What should we indent (current line or current selection) ?

      Copy (From, Dest => Iter);
      Set_Line_Offset (Iter, 0);
      Current_Line := Get_Line (Iter);

      Copy (To, Dest => End_Pos);

      if not Ends_Line (End_Pos) then
         Forward_To_Line_End (End_Pos, Result);
      end if;

      Line := Get_Line (End_Pos);

      if Lines_Are_Real (Buffer) then
         --  We're spending most of our time getting this string.
         --  Consider saving the current line, indentation level and
         --  the stacks used by Next_Indentation to avoid parsing
         --  the buffer from scratch each time.

         C_Str := Get_Slice (Buffer, 0, 0, Line, Get_Line_Offset (End_Pos));
         Slice := To_Unchecked_String (C_Str);

         --  In the loop below, Global_Offset contains the offset between
         --  the modified buffer, and the original buffer, caused by the
         --  buffer replacements.

         loop
            Line_Start := Natural (Get_Offset (Iter)) + 1 - Global_Offset;
            Index      := Line_Start;

            if Current_Line = Cursor_Line then
               Skip_To_First_Non_Blank (Iter);
               Cursor_Offset := Cursor_Offset - Get_Line_Offset (Iter);
            end if;

            if not Ends_Line (Iter) then
               Forward_To_Line_End (Iter, Result);
            end if;

            Line_End := Natural (Get_Offset (Iter)) + 1 - Global_Offset;

            Local_Next_Indentation
              (Lang, Slice (1 .. Line_End), Indent, Indent_Params);

            while Index <= Line_End
              and then (Slice (Index) = ' '
                     or else Slice (Index) = ASCII.HT)
            loop
               if Slice (Index) = ASCII.HT then
                  Tabs_Used := True;
               end if;

               Index := Index + 1;
            end loop;

            Offset := Index - Line_Start;

            Create_Command;

            --  If the cursor was located before the first non-blank character,
            --  move it to that character. This is more usual for Emacs users,
            --  and more user friendly generally.

            if Current_Line = Cursor_Line then
               Get_Iter_At_Line (Buffer, Iter, Current_Line);
               Skip_To_First_Non_Blank (Iter);
               if Cursor_Offset > 0 then
                  Forward_Chars (Iter, Cursor_Offset, Result);
               end if;
               Place_Cursor (Buffer, Iter);
            end if;

            exit when Current_Line >= Line;

            Current_Line := Current_Line + 1;
            Get_Iter_At_Line_Offset (Buffer, Iter, Current_Line);
         end loop;

         g_free (C_Str);

      else
         --  We're spending most of our time getting this string.
         --  Consider saving the current line, indentation level and
         --  the stacks used by Next_Indentation to avoid parsing
         --  the buffer from scratch each time.

         Buffer_Text := Get_First_Lines
           (Buffer, Get_Editable_Line (Buffer, Buffer_Line_Type (Line + 1)));


         --  Initialize Line_End at the line before Iter, so that Line_Start
         --  is well computed in the first iteration of the loop below.

         Line_End := Buffer_Text'Length
           - Natural (Get_Offset (End_Pos) - Get_Offset (Iter)) - 1;

         --  In the loop below, Global_Offset contains the offset between
         --  the modified buffer, and the original buffer, caused by the
         --  buffer replacements.

         loop
            Line_Start := Line_End + 1;
            Index      := Line_Start;

            if Current_Line = Cursor_Line then
               Skip_To_First_Non_Blank (Iter);
               Cursor_Offset := Cursor_Offset - Get_Line_Offset (Iter);
            end if;

            if not Ends_Line (Iter) then
               Forward_To_Line_End (Iter, Result);
            end if;

            Line_End := Line_Start + Natural (Get_Line_Offset (Iter));

            Local_Next_Indentation
              (Lang,
               Buffer_Text
                 (Buffer_Text'First .. Buffer_Text'First + Line_End - 1),
               Indent, Indent_Params);

            Find_Non_Blank (Line_End);
            Offset := Index - Line_Start;

            Create_Command;

            --  If the cursor was located before the first non-blank character,
            --  move it to that character. This is more usual for Emacs users,
            --  and more user friendly generally.

            if Current_Line = Cursor_Line then
               Get_Iter_At_Line (Buffer, Iter, Current_Line);
               Skip_To_First_Non_Blank (Iter);
               if Cursor_Offset > 0 then
                  Forward_Chars (Iter, Cursor_Offset, Result);
               end if;
               Place_Cursor (Buffer, Iter);
            end if;

            exit when Current_Line >= Line;

            Current_Line := Current_Line + 1;
            Get_Iter_At_Line_Offset (Buffer, Iter, Current_Line);
         end loop;

         Free (Buffer_Text);
      end if;

      return True;

   exception
      when E : others =>
         --  Stop propagation of exception, since doing nothing
         --  in this callback is harmless.

         if Buffer_Text /= null then
            Free (Buffer_Text);
         end if;

         if C_Str /= Gtkada.Types.Null_Ptr then
            g_free (C_Str);
         end if;

         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Do_Indentation;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Context : access Src_Editor_Action_Context) return String
   is
      pragma Unreferenced (Context);
   begin
      return "Source editor";
   end Get_Name;

   ---------------------
   -- Context_Matches --
   ---------------------

   function Context_Matches
     (Context : access Src_Editor_Action_Context;
      Kernel  : access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Context);
      Widget : constant Gtk_Widget := Get_Current_Focus_Widget (Kernel);
   begin
      return Widget.all in Source_View_Record'Class;
   end Context_Matches;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Info_Width) is
   begin
      if X.Info /= null then
         Free (X.Info.all);
         Unchecked_Free (X.Info);
      end if;
   end Free;

   ----------------------
   -- Forward_Position --
   ----------------------

   procedure Forward_Position
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      Length       : Integer;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Natural)
   is
      Iter      : Gtk_Text_Iter;
      Success   : Boolean;
   begin
      Get_Iter_At_Line_Offset
        (Buffer,
         Iter,
         Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
         Gint (Start_Column - 1));

      for J in 1 .. Length loop
         Forward_Char (Iter, Success);
         exit when not Success;

         while Buffer.Line_Data
           (Buffer_Line_Type (Get_Line (Iter) + 1)).Editable_Line = 0
         loop
            Forward_Char (Iter, Success);
            exit when not Success;
         end loop;
      end loop;

      End_Line := Get_Editable_Line
        (Buffer, Buffer_Line_Type (Get_Line (Iter) + 1));

      End_Column := Natural (Get_Line_Offset (Iter) + 1);
   end Forward_Position;

   --------------
   -- Get_Text --
   --------------

   function Get_Text
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      End_Line     : Editable_Line_Type;
      End_Column   : Natural) return String
   is
      Start_Iter, End_Iter : Gtk_Text_Iter;
   begin
      --  ??? Should remove non-editable lines / include hidden lines ?
      --  ??? Should we provide a Get_Iter_At_Line_Offset function
      --  generalized to Editable_Line_Type ?

      Get_Iter_At_Line_Offset
        (Buffer,
         Start_Iter,
         Gint (Get_Buffer_Line (Buffer, Start_Line) - 1),
         Gint (Start_Column - 1));

      Get_Iter_At_Line_Offset
        (Buffer,
         End_Iter,
         Gint (Get_Buffer_Line (Buffer, End_Line) - 1),
         Gint (End_Column - 1));

      return Get_Text (Buffer, Start_Iter, End_Iter, True);
   end Get_Text;

   ------------------
   -- Blocks_Valid --
   ------------------

   function Blocks_Valid
     (Buffer : access Source_Buffer_Record) return Boolean is
   begin
      return not Buffer.Blocks_Timeout_Registered;
   end Blocks_Valid;

end Src_Editor_Buffer;
