-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with Glib.Values;               use Glib.Values;
with Gtk;                       use Gtk;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with OS_Utils;                  use OS_Utils;
with Src_Info;                  use Src_Info;
with Glide_Intl;                use Glide_Intl;

with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Commands.Editor;           use Commands.Editor;
with Commands.Controls;         use Commands.Controls;
with Src_Editor_Module;         use Src_Editor_Module;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Ada.Exceptions;            use Ada.Exceptions;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;

with Pango.Font;                  use Pango.Font;
with Pango.Layout;                use Pango.Layout;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;

with String_List_Utils;         use String_List_Utils;
with Ada.Unchecked_Deallocation;

package body Src_Editor_Buffer is

   use type System.Address;

   Me : constant Debug_Handle := Create ("Source_Editor_Buffer");

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
      3 => New_String ("status_changed"));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (GType_Int, GType_Int),
      2 => (GType_None, GType_None),
      3 => (GType_None, GType_None));
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

   procedure Side_Column_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "side_column_changed" signal.

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
   --  signal is processed only when Lang is not null.
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
   --  signal is processed only when Lang is not null.
   --
   --  Note that this handler is designed to be connected "after", in which
   --  case the Start and End iterators are equal, since the text between
   --  these iterators have already been deleted.

   procedure Emit_New_Cursor_Position
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Gint;
      Column : Gint);
   --  Signal the new cursor position by emitting the "cursor_position_changed"
   --  signal. Line and Column are the new line and column number of the
   --  cursor. Note that line numbers start from 1.

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

   procedure Strip_Ending_Line_Terminator
     (Buffer : access Source_Buffer_Record'Class);
   --  Delete the last character of the buffer if it is an ASCII.LF.

   procedure Buffer_Destroy (Buffer : access Source_Buffer_Record'Class);
   --  Free memory associated to Buffer.

   function Automatic_Save (Buffer : Source_Buffer) return Boolean;
   --  Handle automatic save of the buffer, using a timeout.

   procedure Internal_Save_To_File
     (Buffer   : Source_Buffer;
      Filename : String;
      Success  : out Boolean);
   --  Low level save function. Only writes the buffer contents on disk,
   --  with no modification on the buffer's settings.

   procedure Preferences_Changed
     (Buffer : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed.

   procedure End_Action (Buffer : access Source_Buffer_Record'Class);
   --  This procedure should be called every time that an internal
   --  event should cancel the current user action: focus switching
   --  to another window, cursor moved, etc.

   procedure Cursor_Move_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the cursor moves.

   procedure User_Edit_Hook (Buffer : access Source_Buffer_Record'Class);
   --  Actions that must be executed whenever the user inserts or deletes text.

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

   procedure Get_Column_For_Identifier
     (Buffer     : access Source_Buffer_Record;
      Identifier : String;
      Width      : Integer;
      Column     : out Integer);
   --  Return the index of the column corresponding to the identifier.
   --  Create such a column if necessary.

   procedure Add_Lines
     (Buffer   : access Source_Buffer_Record'Class;
      Start  : Integer;
      Number : Integer);
   --  Add Number blank lines to the column info, after Start.

   procedure Remove_Lines
     (Buffer       : access Source_Buffer_Record'Class;
      Start_Line : Integer;
      End_Line   : Integer);
   --  Remove lines from the column info.

   procedure Insert_At_Position
     (Buffer   : access Source_Buffer_Record;
      Info   : Line_Information_Record;
      Column : Integer;
      Line   : Integer;
      Width  : Integer);
   --  Insert Info at the correct line position in L.

   procedure Remove_Line_Information_Column
     (Buffer : access Source_Buffer_Record'Class;
      Column : Integer);
   --  Remove the column from the side window information in Buffer.

   procedure Free (X : in out Line_Info_Width);
   --  Free memory associated to X.

   --------------------
   -- Automatic_Save --
   --------------------

   function Automatic_Save (Buffer : Source_Buffer) return Boolean is
      Success : Boolean;
   begin
      if not Buffer.Modified_Auto or else Buffer.Filename = null then
         return True;
      end if;

      Internal_Save_To_File
        (Buffer,
         Dir_Name (Buffer.Filename.all) & ".#" &
         Base_Name (Buffer.Filename.all), Success);
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
   end Destroy_Hook;

   --------------------
   -- Buffer_Destroy --
   --------------------

   procedure Buffer_Destroy (Buffer : access Source_Buffer_Record'Class) is
      Result : Boolean;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Line_Info_Display_Array, Line_Info_Display_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Line_Info_Width_Array, Line_Info_Width_Array_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Line_Info_Display_Record, Line_Info_Display_Access);
   begin
      --  We do not free memory associated to Buffer.Current_Command, since
      --  this command is already freed when freeing Buffer.Queue.

      if Buffer.Filename /= null then
         File_Closed (Buffer.Kernel, Buffer.Filename.all);
      end if;

      Destroy_Hook (Buffer);

      if Buffer.Timeout_Id /= 0 then
         Timeout_Remove (Buffer.Timeout_Id);
         Buffer.Timeout_Id := 0;

         if Buffer.Filename /= null then
            Delete_File
              (Dir_Name (Buffer.Filename.all) & ".#" &
               Base_Name (Buffer.Filename.all), Result);
         end if;
      end if;

      if Buffer.Controls_Set then
         Remove_Controls (Buffer);
      end if;

      Free_Queue (Buffer.Queue);
      Free (Buffer.Filename);
      Unchecked_Free (Buffer.Real_Lines);

      for J in Buffer.Line_Info'Range loop
         for K in Buffer.Line_Info (J).Column_Info'Range loop
            Free (Buffer.Line_Info (J).Column_Info (K));
         end loop;

         Free (Buffer.Line_Info (J).Identifier);
         Unchecked_Free (Buffer.Line_Info (J).Column_Info);
         Unchecked_Free (Buffer.Line_Info (J));
      end loop;

      Unchecked_Free (Buffer.Line_Info);
   end Buffer_Destroy;

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler (Buffer : access Source_Buffer_Record'Class) is
      Line : Gint;
      Col  : Gint;
   begin
      Get_Screen_Position (Buffer, Line => Line, Column => Col);
      Emit_New_Cursor_Position (Buffer, Line => Line, Column => Col);
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
      Line : Gint;
      Col  : Gint;

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

            Get_Screen_Position (Buffer, Line, Col);
            Emit_New_Cursor_Position (Buffer, Line => Line, Column => Col);

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
            Backward_To_Tag_Toggle (Start_Iter, Result => Ignored);
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
      Pos    : Gtk_Text_Iter;
      Length : constant Gint := Get_Int (Nth (Params, 3));
      Dummy  : Boolean;
      Start  : Integer;
      Iter   : Gtk_Text_Iter;

   begin
      if Buffer.Lang /= null then
         declare
            Pos    : Gtk_Text_Iter;
            Length : constant Gint := Get_Int (Nth (Params, 3));
            Text   : constant String :=
              Get_String (Nth (Params, 2), Length => Length);

         begin
            Get_Text_Iter (Nth (Params, 1), Pos);
            Insert_Text_Cb (Buffer, Pos, Text);
         end;
      end if;

      --  Call Add_Lines, to compute added lines for the side column.
      --  ??? We could create a hook for added lines.

      Get_Text_Iter (Nth (Params, 1), Pos);
      Copy (Pos, Iter);
      Start := Integer (Get_Line (Pos));
      Backward_Chars (Pos, Length, Dummy);
      Add_Lines (Buffer, Start, Start - Integer (Get_Line (Pos)));

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
            Natural (Get_Line (Pos)),
            Natural (Get_Line_Offset (Pos)));

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
               Natural (Get_Line (Pos)),
               Natural (Get_Line_Offset (Pos)));

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
            Natural (Get_Line (Pos)),
            Natural (Get_Line_Offset (Pos)));
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
      End_Iter   : Gtk_Text_Iter;

      Start_Line : Integer;
      End_Line   : Integer;
   begin
      if Buffer.Lang /= null then
         declare
            Start_Iter : Gtk_Text_Iter;
         begin
            Get_Text_Iter (Nth (Params, 1), Start_Iter);
            Delete_Range_Cb (Buffer, Start_Iter);
         end;
      end if;

      --  Remove the lines in the side information column.
      --  ??? We could add a hook for removed lines.

      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);
      Start_Line := Integer (Get_Line (Start_Iter));
      End_Line   := Integer (Get_Line (End_Iter));

      if Start_Line /= End_Line then
         Remove_Lines (Buffer, Start_Line + 1, End_Line + 1);
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
      Direction    : Direction_Type;
      Line, Column : Gint;
      Line_Start   : Gint;
      Column_Start : Gint;

   begin
      if Buffer.Inserting then
         return;
      end if;

      User_Edit_Hook (Buffer);

      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      if not Is_Null_Command (Command)
        and then Get_Mode (Command) /= Deletion
      then
         End_Action (Buffer);
         Command := Editor_Command (Buffer.Current_Command);

         pragma Assert (Is_Null_Command (Command));
      end if;

      Line_Start   := Get_Line (Start_Iter);
      Column_Start := Get_Line_Offset (Start_Iter);

      if Is_Null_Command (Command) then
         Get_Cursor_Position (Buffer, Line, Column);

         if Line = Line_Start
           and then Column = Column_Start
         then
            Direction := Backward;
         else
            Direction := Forward;
         end if;

         Create
           (Command,
            Deletion,
            Source_Buffer (Buffer),
            True,
            Integer (Line_Start),
            Integer (Column_Start),
            Direction);

         Buffer.Inserting := True;
         Enqueue (Buffer, Command_Access (Command));
         Buffer.Inserting := False;

      else
         Direction := Get_Direction (Command);
      end if;

      Add_Text
        (Command,
         Get_Slice (Buffer, Start_Iter, End_Iter),
         Integer (Line_Start),
         Integer (Column_Start));
      Buffer.Current_Command := Command_Access (Command);

      if Direction = Backward then
         End_Action (Buffer);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Delete_Range_Before_Handler;

   -----------------------
   -- Jump_To_Delimiter --
   -----------------------

   procedure Jump_To_Delimiter (Buffer : access Source_Buffer_Record) is
      On_Cursor_Iter       : Gtk_Text_Iter;
      First_Highlight_Iter : Gtk_Text_Iter;
      Last_Highlight_Iter  : Gtk_Text_Iter;
   begin
      if not Buffer.Has_Delimiters_Highlight then
         return;
      end if;

      Get_Iter_At_Mark
        (Buffer, First_Highlight_Iter, Buffer.Start_Delimiters_Highlight);
      Get_Iter_At_Mark
        (Buffer, Last_Highlight_Iter, Buffer.End_Delimiters_Highlight);
      Get_Iter_At_Mark (Buffer, On_Cursor_Iter, Buffer.Insert_Mark);

      if Equal (First_Highlight_Iter, On_Cursor_Iter) then
         Place_Cursor (Buffer, Last_Highlight_Iter);
      else
         Place_Cursor (Buffer, First_Highlight_Iter);
      end if;
   end Jump_To_Delimiter;

   -------------------------
   -- Side_Column_Changed --
   -------------------------

   procedure Side_Column_Changed
     (Buffer : access Source_Buffer_Record'Class)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");
   begin
      Emit_By_Name (Get_Object (Buffer), "side_column_changed" & ASCII.NUL);
   end Side_Column_Changed;

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
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Gint;
      Column : Gint)
   is
      procedure Emit_By_Name
        (Object : System.Address;
         Name   : String;
         Line   : Gint;
         Column : Gint);
      pragma Import (C, Emit_By_Name, "g_signal_emit_by_name");

   begin
      Emit_By_Name
        (Get_Object (Buffer), "cursor_position_changed" & ASCII.NUL,
         Line => Line, Column => Column);

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
      Slice_Offset       : Gint;
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
         Partial_Entity : Boolean) return Boolean is
      begin
         Get_Iter_At_Offset
           (Buffer, Entity_Start,
            Gint (Sloc_Start.Index) + Slice_Offset - 1);
         Get_Iter_At_Offset
           (Buffer, Entity_End, Gint (Sloc_End.Index) + Slice_Offset);

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
         Ignore : aliased Natural;
         Length : aliased Natural;
         C_Str  : constant Interfaces.C.Strings.chars_ptr :=
           Glib.Convert.Convert
             (UTF8, Integer (Strlen (UTF8)),
              Get_Pref (Buffer.Kernel, Default_Charset), "UTF-8",
              Ignore'Unchecked_Access, Length'Unchecked_Access);
         Slice : constant Unchecked_String_Access :=
           To_Unchecked_String (C_Str);
         pragma Suppress (Access_Check, Slice);

      begin
         g_free (UTF8);
         Highlight_Complete := True;
         Slice_Offset := Get_Offset (Entity_Start);

         --  First, un-apply all the style tags...

         Kill_Highlighting (Buffer, Entity_Start, Entity_End);

         --  Now re-highlight the text...

         Parse_Entities
           (Buffer.Lang,
            Slice (1 .. Length),
            Highlight_Cb'Unrestricted_Access);
         g_free (C_Str);
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
      while not Is_End (Iter) and then not Ends_Line (Iter) loop
         Forward_Char (Iter, Result_Ignored);
      end loop;
   end Forward_To_Line_End;

   ----------------------------------
   -- Strip_Ending_Line_Terminator --
   ----------------------------------

   procedure Strip_Ending_Line_Terminator
     (Buffer : access Source_Buffer_Record'Class)
   is
      Iter      : Gtk_Text_Iter;
      End_Iter  : Gtk_Text_Iter;
      Ignored   : Boolean;
   begin
      --  Does the buffer end with CR & LF?

      if Get_Char_Count (Buffer) > 1 then
         Get_End_Iter (Buffer, End_Iter);
         Copy (Source => End_Iter, Dest => Iter);
         Backward_Chars (Iter, Count => 2, Result => Ignored);

         if Get_Slice (Iter, End_Iter) = ASCII.CR & ASCII.LF then
            Delete (Buffer, Iter, End_Iter);
            return;
         end if;
      end if;

      --  Or does it end with a CR or LF?

      if Get_Char_Count (Buffer) > 0 then
         Get_End_Iter (Buffer, Iter);
         Backward_Char (Iter, Ignored);

         case Character'(Get_Char (Iter)) is
            when ASCII.LF | ASCII.CR =>
               Get_End_Iter (Buffer, End_Iter);
               Delete (Buffer, Iter, End_Iter);
            when others =>
               null;
         end case;
      end if;
   end Strip_Ending_Line_Terminator;

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

      Buffer.Line_Info := new Line_Info_Display_Array (1 .. 0);
      Buffer.Real_Lines := new Natural_Array (1 .. 1);
      Buffer.Real_Lines (1) := 0;
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
   end Initialize;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Buffer : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      B       : Source_Buffer := Source_Buffer (Buffer);
      Timeout : Gint;
   begin
      --  Since we update the tags directly, gtk+ will automatically refresh
      --  the source view, we don't need to do anything for this.

      Create_Syntax_Tags
        (B.Syntax_Tags,
         Keyword_Color       => Get_Pref (Kernel, Default_Keyword_Color),
         Keyword_Font_Desc   => Get_Pref (Kernel, Keyword_Font),
         Comment_Color       => Get_Pref (Kernel, Default_Comment_Color),
         Comment_Font_Desc   => Get_Pref (Kernel, Comment_Font),
         Character_Color     => Get_Pref (Kernel, Default_String_Color),
         Character_Font_Desc => Get_Pref (Kernel, String_Font),
         String_Color        => Get_Pref (Kernel, Default_String_Color),
         String_Font_Desc    => Get_Pref (Kernel, String_Font));

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
   end Preferences_Changed;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : String;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean)
   is
      Contents : GNAT.OS_Lib.String_Access;
      UTF8     : Gtkada.Types.Chars_Ptr;
      Ignore   : aliased Natural;
      Length   : aliased Natural;

   begin
      Success := True;
      Contents := Read_File (Filename);

      if Contents = null then
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

      UTF8 := Glib.Convert.Convert
        (Contents.all, "UTF-8", Get_Pref (Buffer.Kernel, Default_Charset),
         Ignore'Unchecked_Access, Length'Unchecked_Access);

      if UTF8 = Gtkada.Types.Null_Ptr then
         --  In case conversion failed
         Insert_At_Cursor (Buffer, Contents.all);
      else
         Insert_At_Cursor (Buffer, UTF8, Gint (Length));
      end if;

      g_free (UTF8);
      Free (Contents);
      Strip_Ending_Line_Terminator (Buffer);
      Set_Modified (Buffer, False);

      Buffer.Inserting := False;
      Buffer.Modified_Auto := False;

      Buffer.Timestamp := To_Timestamp (File_Time_Stamp (Filename));

      Empty_Queue (Buffer.Queue);
      Buffer.Current_Command := null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Load_File;

   ---------------------------
   -- Internal_Save_To_File --
   ---------------------------

   procedure Internal_Save_To_File
     (Buffer   : Source_Buffer;
      Filename : String;
      Success  : out Boolean)
   is
      FD         : File_Descriptor := Invalid_FD;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      CR_Found   : Boolean := False;

      procedure New_Line (FD : File_Descriptor);
      --  Write a new line on FD.

      procedure New_Line (FD : File_Descriptor) is
         NL            : constant String := ASCII.CR & ASCII.LF;
         Bytes_Written : Integer;
         pragma Unreferenced (Bytes_Written);

      begin
         if CR_Found then
            Bytes_Written := Write (FD, NL (1)'Address, 2);
         else
            Bytes_Written := Write (FD, NL (2)'Address, 1);
         end if;
      end New_Line;

   begin
      Success := True;

      FD := Create_File (Filename, Fmode => Binary);

      if FD = Invalid_FD then
         Success := False;
         return;
      end if;

      Get_Bounds (Buffer, Start_Iter, End_Iter);

      declare
         UTF8           : constant Gtkada.Types.Chars_Ptr :=
           Get_Text (Buffer, Start_Iter, End_Iter, True);
         Contents       : GNAT.OS_Lib.String_Access;
         Bytes_Written  : Integer;
         pragma Unreferenced (Bytes_Written);

         First, Current : Natural := 1;
         Blanks         : Natural := 0;
         Ignore, Length : Natural;

      begin
         Length := Integer (Strlen (UTF8));
         Contents := new String (1 .. Length);
         Glib.Convert.Convert
           (UTF8, Length, Get_Pref (Buffer.Kernel, Default_Charset), "UTF-8",
            Ignore, Length, Result => Contents.all);
         g_free (UTF8);

         if not Get_Pref (Buffer.Kernel, Strip_Blanks) then
            Current := Length + 1;

            --  Check whether first line terminator is LF or CR/LF and set
            --  CR_Found accordingly.

            for J in 1 .. Length loop
               if Contents (J) = ASCII.CR then
                  CR_Found := True;
                  exit;

               elsif Contents (J) = ASCII.LF then
                  exit;
               end if;
            end loop;

         else
            while Current <= Length loop
               case Contents (Current) is
                  when ASCII.CR =>
                     CR_Found := True;

                  when ASCII.LF =>
                     if Blanks /= 0 then
                        Bytes_Written := Write
                          (FD, Contents (First)'Address, Blanks - First);
                        New_Line (FD);
                        Blanks := 0;
                        First := Current + 1;
                     end if;

                  when ' ' | ASCII.HT =>
                     if Blanks = 0 then
                        Blanks := Current;
                     end if;

                  when others =>
                     Blanks := 0;
               end case;

               Current := Current + 1;
            end loop;
         end if;

         if First < Length then
            if Blanks /= 0 then
               Bytes_Written :=
                 Write (FD, Contents (First)'Address, Blanks - First);
               New_Line (FD);

            else
               Bytes_Written :=
                 Write (FD, Contents (First)'Address, Current - First);

               if Contents (Length) /= ASCII.LF
                 and then Contents (Length) /= ASCII.CR
               then
                  New_Line (FD);
               end if;
            end if;
         end if;

         Free (Contents);
      end;

      Close (FD);

      --  If the file could be saved, emit the corresponding signal.

      if Success then
         File_Saved (Buffer.Kernel, Filename);
         Buffer.Saved_Position := Get_Position (Buffer.Queue);
         Buffer.Current_Status := Saved;
         Status_Changed (Buffer);
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
      Filename : String;
      Success  : out Boolean)
   is
      Name_Changed : Boolean;
      Result       : Boolean;
   begin
      Internal_Save_To_File (Buffer.all'Access, Filename, Success);

      if not Success then
         return;
      end if;

      if Buffer.Filename /= null then
         Delete_File
           (Dir_Name (Buffer.Filename.all) & ".#" &
            Base_Name (Buffer.Filename.all), Result);
      end if;

      Set_Modified (Buffer, False);
      Buffer.Modified_Auto := False;
      Name_Changed := Buffer.Filename = null
        or else Buffer.Filename.all /= Filename;

      if Name_Changed then
         Free (Buffer.Filename);
         Buffer.Filename := new String'(Filename);
      end if;

      Buffer.Timestamp := To_Timestamp
        (File_Time_Stamp (Get_Filename (Buffer)));

      if Name_Changed then
         Set_Language
           (Buffer,
            Get_Language_From_File
              (Glide_Language_Handler (Get_Language_Handler (Buffer.Kernel)),
               Buffer.Filename.all));

         --  ??? The following is expensive, it would be nice to have a
         --  simpler way to report a possible change in the list of sources
         --  of a project.
         Recompute_View (Buffer.Kernel);
      end if;

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
            if Buffer.Lang /= null then
               Highlight_Slice (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
            else
               Kill_Highlighting (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
            end if;
         end if;
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

      --  At this point, we passed all the checks, so the position is licit.
      return True;
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
              & Get_Filename (Buffer) & Line'Img & Column'Img);

      if not Buffer.Inserting then
         --  At this point, we know that the (Line, Column) position is
         --  valid, so we can safely get the iterator at this position.
         Get_Iter_At_Line_Offset (Buffer, Iter, Line, Column);
         Place_Cursor (Buffer, Iter);
      end if;
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
                & Get_Filename (Buffer) & Line'Img);

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
      UTF8       : Interfaces.C.Strings.chars_ptr;
      Ignore     : aliased Natural;
      Length     : aliased Natural;
      Contents   : Interfaces.C.Strings.chars_ptr;

   begin
      pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Start_Line, Start_Column);

      if End_Line = -1 then
         Get_End_Iter (Buffer, End_Iter);
      else
         pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));
         Get_Iter_At_Line_Offset (Buffer, End_Iter, End_Line, End_Column);
      end if;

      UTF8     := Get_Text (Buffer, Start_Iter, End_Iter, True);
      Length   := Natural (Strlen (UTF8));
      Contents := Glib.Convert.Convert
        (UTF8, Length, Get_Pref (Buffer.Kernel, Default_Charset), "UTF-8",
         Ignore'Unchecked_Access, Length'Unchecked_Access);
      g_free (UTF8);

      return Contents;
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
      Delete (Buffer, Iter, End_Iter);

      if not Enable_Undo then
         Buffer.Inserting := Previous_Inserting_Value;
      end if;
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

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      pragma Assert (Line < Get_Line_Count (Buffer));

      --  Search for a highlighte line, if any, and unhighlight it.
      Cancel_Highlight_Line (Buffer);

      --  And finally highlight the given line
      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Line, 0);
      Copy (Source => Start_Iter, Dest => End_Iter);
      Forward_To_Line_End (End_Iter);
      Apply_Tag (Buffer, Buffer.HL_Line_Tag, Start_Iter, End_Iter);
   end Highlight_Line;

   ----------------------
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      pragma Assert (Line < Get_Line_Count (Buffer));

      Get_Iter_At_Line_Offset (Buffer, Start_Iter, Line, 0);
      Copy (Source => Start_Iter, Dest => End_Iter);
      Forward_To_Line_End (End_Iter);
      Remove_Tag (Buffer, Buffer.HL_Line_Tag, Start_Iter, End_Iter);
   end Unhighlight_Line;

   ---------------------------
   -- Cancel_Highlight_Line --
   ---------------------------

   procedure Cancel_Highlight_Line
     (Buffer : access Source_Buffer_Record)
   is
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Found      : Boolean := True;
   begin
      Get_Start_Iter (Buffer, Start_Iter);

      if not Begins_Tag (Start_Iter, Buffer.HL_Line_Tag) then
         Forward_To_Tag_Toggle (Start_Iter, Buffer.HL_Line_Tag, Found);
      end if;

      if Found then
         Copy (Source => Start_Iter, Dest => End_Iter);
         Forward_To_Line_End (End_Iter);
         Remove_Tag (Buffer, Buffer.HL_Line_Tag, Start_Iter, End_Iter);
      end if;
   end Cancel_Highlight_Line;

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
         pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
         pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));

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
      pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
      pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));

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
      pragma Assert (Is_Valid_Position (Buffer, Start_Line, Start_Column));
      pragma Assert (Is_Valid_Position (Buffer, End_Line, End_Column));

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
     (Buffer : access Source_Buffer_Record)
     return String
   is
   begin
      if Buffer.Filename = null then
         return "";
      else
         return Buffer.Filename.all;
      end if;
   end Get_Filename;

   ------------------
   -- Set_Filename --
   ------------------

   procedure Set_Filename
     (Buffer : access Source_Buffer_Record;
      Name   : String)
   is
   begin
      Free (Buffer.Filename);
      Buffer.Filename := new String'(Name);
   end Set_Filename;

   ---------------------------
   -- Source_Lines_Revealed --
   ---------------------------

   procedure Source_Lines_Revealed
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Integer;
      End_Line   : Integer)
   is
      Context : File_Area_Context_Access;
   begin
      Context := new File_Area_Context;
      Set_Context_Information
        (Context,
         Buffer.Kernel,
         Src_Editor_Module_Id);

      if Buffer.Filename /= null then
         if Base_Name (Buffer.Filename.all) = Buffer.Filename.all then
            Set_File_Information
              (Context, "", Base_Name (Buffer.Filename.all));

         else
            Set_File_Information
              (Context,
               Dir_Name (Buffer.Filename.all),
               Base_Name (Buffer.Filename.all));
         end if;
      end if;

      Set_Area_Information (Context, Start_Line, End_Line);
      Glide_Kernel.Source_Lines_Revealed (Buffer.Kernel, Context);
      Free (Selection_Context_Access (Context));
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
      if Buffer.Filename /= null
        and then Buffer.Filename.all /= ""
        and then Is_Regular_File (Buffer.Filename.all)
      then
         New_Timestamp := To_Timestamp (File_Time_Stamp (Buffer.Filename.all));

         if New_Timestamp > Buffer.Timestamp then
            if Force then
               Response := Gtk_Response_Accept;
            else
               if not Ask_User then
                  return False;
               end if;

               Dialog := Create_Gtk_Dialog
                 (Msg         => Base_Name (Buffer.Filename.all)
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
                     Filename        => Buffer.Filename.all,
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

   -------------------
   -- Get_Ref_Count --
   -------------------

   function Get_Ref_Count
     (Buffer : access Source_Buffer_Record)
      return Integer is
   begin
      return Buffer.Total_References;
   end Get_Ref_Count;

   -------------------
   -- Do_Completion --
   -------------------

   procedure Do_Completion (Buffer : access Source_Buffer_Record) is

      use String_List_Utils.String_List;

      procedure Extend_Completions_List;
      --  Add an item to the buffer's completion, or mark it as
      --  complete. Place the completion node to the newly added
      --  item, or to Null if the completion was finished.

      -----------------------------
      -- Extend_Completions_List --
      -----------------------------

      procedure Extend_Completions_List is
         Data       : Completion_Data renames Buffer.Completion;
         Word_Begin : Gtk_Text_Iter;
         Word_End   : Gtk_Text_Iter;
         Iter_Back  : Gtk_Text_Iter;
         Iter_Forward : Gtk_Text_Iter;

         Aux        : Gtk_Text_Iter;
         Success    : Boolean := True;
         Found      : Boolean := False;
         Word_Found : Boolean := False;

         Count      : Gint := 1;
      begin
         if Data.Complete then
            if Data.Node = Null_Node then
               Data.Node := First (Data.List);
            else
               Data.Node := Next (Data.Node);
            end if;

            return;
         end if;

         --  Loop until a new word with the right prefix is found.

         Get_Iter_At_Mark (Buffer, Iter_Back, Data.Previous_Mark);
         Get_Iter_At_Mark (Buffer, Iter_Forward, Data.Next_Mark);

         while not Found loop
            --  If a boundary is reached, force the search in the other
            --  direction, otherwise extend search in the opposite direction

            if Data.Top_Reached then
               Data.Backwards := False;
            elsif Data.Bottom_Reached then
               Data.Backwards := True;
            else
               Data.Backwards := not Data.Backwards;
            end if;

            --  Find a word and examine it.

            Count := 0;

            while not Word_Found loop
               if Data.Backwards then
                  --  Find the previous real word, if it exists.

                  Backward_Word_Start (Iter_Back, Success);
                  Count := Count + 1;

                  if Success then
                     Copy (Iter_Back, Aux);
                     Backward_Char (Aux, Success);

                     if not Success or else Get_Char (Aux) /= '_' then
                        Copy (Iter_Back, Word_Begin);
                        Copy (Iter_Back, Word_End);
                        Forward_Word_Ends (Word_End, Count, Success);

                        Word_Found := True;
                     end if;
                  else
                     exit;
                  end if;

               else
                  --  Find the next real word.

                  Forward_Word_End (Iter_Forward, Success);
                  Count := Count + 1;

                  if Success then
                     if Get_Char (Iter_Forward) /= '_' then
                        Copy (Iter_Forward, Word_End);
                        Copy (Iter_Forward, Word_Begin);
                        Backward_Word_Starts (Word_Begin, Count, Success);

                        Word_Found := True;
                     end if;
                  else
                     exit;
                  end if;
               end if;
            end loop;

            if Word_Found then
               --  We have a valid word between Word_Begin and Word_End.

               declare
                  S : constant String := Get_Slice (Word_Begin, Word_End);
               begin
                  --  If the word has the right prefix, and is not already
                  --  in the list, then add it to the list and point to it,
                  --  otherwise continue extending the search.
                  --
                  --  The string comparison below is correct, since both
                  --  strings are UTF-8.

                  if S'Length > Data.Prefix'Length
                    and then S
                      (S'First .. S'First - 1 + Data.Prefix'Length)
                      = Data.Prefix.all
                    and then not Is_In_List
                      (Data.List,
                       S (S'First + Data.Prefix'Length .. S'Last))
                  then
                     Found := True;

                     if Data.Backwards then
                        Move_Mark (Buffer, Data.Previous_Mark, Word_Begin);
                     else
                        Move_Mark (Buffer, Data.Next_Mark, Word_End);
                     end if;

                     Append
                       (Data.List,
                        S (S'First + Data.Prefix'Length .. S'Last));

                     Data.Node := Last (Data.List);
                  end if;

                  Word_Found := False;
               end;
            else
               if Data.Backwards then
                  Data.Top_Reached := True;
               else
                  Data.Bottom_Reached := True;
               end if;

               if Data.Top_Reached and then Data.Bottom_Reached then
                  Data.Complete := True;

                  if Data.Node /= Null_Node then
                     Data.Node := Next (Data.Node);
                  end if;

                  return;
               else
                  null;
               end if;
            end if;
         end loop;
      end Extend_Completions_List;

      Command : Editor_Replace_Slice;
      Delete  : Editor_Command;

      Iter    : Gtk_Text_Iter;
      Prev    : Gtk_Text_Iter;
      Success : Boolean;

      Text    : GNAT.OS_Lib.String_Access;

   begin
      if Is_Empty (Buffer.Completion) then
         Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);

         --  If the completions list is empty, that means we have to
         --  initiate the mark data and launch the first search.

         End_Action (Buffer);

         --  At this point the completion data is reset.
         --  Get the completion suffix.

         Copy (Iter, Prev);
         Backward_Char (Prev, Success);

         if not Success then
            return;
         end if;

         while Is_Entity_Letter (Get_Char (Prev)) loop
            Backward_Char (Prev, Success);

            exit when not Success;
         end loop;

         if Success then
            Forward_Char (Prev, Success);
         end if;

         declare
            P : constant String := Get_Slice (Prev, Iter);
         begin
            if P /= "" then
               Buffer.Completion.Prefix := new String'(P);

               Move_Mark (Buffer, Buffer.Completion.Mark, Iter);
               Move_Mark (Buffer, Buffer.Completion.Previous_Mark, Iter);
               Move_Mark (Buffer, Buffer.Completion.Next_Mark, Iter);

               Extend_Completions_List;
            else
               Clear (Buffer.Completion);
               return;

            end if;
         end;
      else
         Extend_Completions_List;
      end if;

      if Buffer.Completion.Node /= Null_Node then
         Text := new String'(Data (Buffer.Completion.Node));
      else
         Text := new String'("");
      end if;

      Get_Iter_At_Mark (Buffer, Prev, Buffer.Completion.Mark);
      Get_Iter_At_Mark (Buffer, Iter, Buffer.Insert_Mark);
      Buffer.Inserting := True;

      if Text.all = ""
        and then
          (Get_Line (Prev) /= Get_Line (Iter)
           or else Get_Line_Offset (Prev) /= Get_Line_Offset (Iter))
      then
         Create (Delete,
                 Deletion,
                 Source_Buffer (Buffer),
                 False,
                 Integer (Get_Line (Prev)),
                 Integer (Get_Line_Offset (Prev)),
                 Forward);

         Set_Text (Delete, Get_Slice (Prev, Iter));
         Enqueue (Buffer, Command_Access (Delete));

      else
         Create
           (Command,
            Source_Buffer (Buffer),
            Natural (Get_Line (Prev)),
            Natural (Get_Line_Offset (Prev)),
            Natural (Get_Line (Iter)),
            Natural (Get_Line_Offset (Iter)),
            Text.all,
            True);
         Enqueue (Buffer, Command_Access (Command));
      end if;

      Buffer.Current_Command := null;

      GNAT.OS_Lib.Free (Text);

      Buffer.Inserting := False;
   end Do_Completion;

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

   -------------------
   -- Get_Line_Info --
   -------------------

   function Get_Line_Info
     (Buffer : access Source_Buffer_Record)
      return Line_Info_Display_Array_Access is
   begin
      return Buffer.Line_Info;
   end Get_Line_Info;

   -------------------
   -- Set_Line_Info --
   -------------------

   procedure Set_Line_Info
     (Buffer    : access Source_Buffer_Record;
      Line_Info : Line_Info_Display_Array_Access) is
   begin
      Buffer.Line_Info := Line_Info;
   end Set_Line_Info;

   --------------------
   -- Get_Real_Lines --
   --------------------

   function Get_Real_Lines
     (Buffer : access Source_Buffer_Record)
      return Natural_Array_Access is
   begin
      return Buffer.Real_Lines;
   end Get_Real_Lines;

   --------------------
   -- Set_Real_Lines --
   --------------------

   procedure Set_Real_Lines
     (Buffer     : access Source_Buffer_Record;
      Real_Lines : Natural_Array_Access) is
   begin
      Buffer.Real_Lines := Real_Lines;
   end Set_Real_Lines;

   ------------------------
   -- Insert_At_Position --
   ------------------------

   procedure Insert_At_Position
     (Buffer   : access Source_Buffer_Record;
      Info   : Line_Information_Record;
      Column : Integer;
      Line   : Integer;
      Width  : Integer)
   is
      Line_Info  : Line_Info_Display_Array_Access renames Buffer.Line_Info;
      Real_Lines : Natural_Array_Access renames Buffer.Real_Lines;
   begin
      if Line not in Real_Lines'Range then
         declare
            A : constant Natural_Array := Real_Lines.all;
         begin
            Unchecked_Free (Real_Lines);
            Real_Lines := new Natural_Array
              (1 .. Line * 2);

            Real_Lines (A'Range) := A;
            Real_Lines (A'Last + 1 .. Real_Lines'Last) :=
              (others => 0);
         end;
      end if;

      --  If needed, increase the size of the target array

      if (not Line_Info (Column).Stick_To_Data)
        and then Line > Line_Info (Column).Column_Info'Last
      then
         declare
            A : Line_Info_Width_Array (1 .. Line * 2);
         begin
            A (1 .. Line_Info (Column).Column_Info'Last) :=
              Line_Info (Column).Column_Info.all;
            Line_Info (Column).Column_Info :=
              new Line_Info_Width_Array'(A);
         end;
      end if;

      --  Insert the data in the array

      if not (Line_Info (Column).Stick_To_Data
              and then Line > Buffer.Original_Lines_Number)
      then
         if Line_Info (Column).Column_Info (Line).Info /= null then
            Free (Line_Info (Column).Column_Info (Line));
         end if;

         Line_Info (Column).Column_Info (Line) :=
           Line_Info_Width'(new Line_Information_Record'(Info), Width);
      end if;
   end Insert_At_Position;

   -------------------------------
   -- Get_Column_For_Identifier --
   -------------------------------

   procedure Get_Column_For_Identifier
     (Buffer     : access Source_Buffer_Record;
      Identifier : String;
      Width      : Integer;
      Column     : out Integer)
   is
      Line_Info  : Line_Info_Display_Array_Access renames Buffer.Line_Info;
   begin
      --  Browse through existing columns and try to match Identifier

      for J in Line_Info'Range loop
         if Line_Info (J).Identifier.all = Identifier then
            Column := J;

            --  Set the new width of the column

            if Line_Info (J).Width < Width then
               for K in (J + 1) .. Line_Info.all'Last loop
                  Line_Info (K).Starting_X :=
                    Line_Info (K).Starting_X + Width
                    - Line_Info (J).Width;
               end loop;

               Buffer.Total_Column_Width :=
                 Buffer.Total_Column_Width + Width
                   - Line_Info (J).Width;

               Line_Info (J).Width := Width;
            end if;

            return;
         end if;
      end loop;

      --  If we reach this point, that means no column was found that
      --  corresponds to Identifier. Therefore we create one.

      declare
         A : Line_Info_Display_Array
           (Line_Info.all'First .. Line_Info.all'Last + 1);
         New_Column : Line_Info_Width_Array
           (1 .. Buffer.Original_Lines_Number);
      begin
         A (Line_Info'First .. Line_Info'Last) := Line_Info.all;

         A (A'Last) := new Line_Info_Display_Record'
           (Identifier  => new String'(Identifier),
            Starting_X  => Buffer.Total_Column_Width + 2,
            Width       => Width,
            Column_Info => new Line_Info_Width_Array'(New_Column),
            Stick_To_Data => False,
            Every_Line    => False);
         Unchecked_Free (Line_Info);
         Line_Info := new Line_Info_Display_Array'(A);

         Column := Line_Info.all'Last;

         Buffer.Total_Column_Width := Buffer.Total_Column_Width + Width + 2;
      end;
   end Get_Column_For_Identifier;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (Buffer     : access Source_Buffer_Record;
      Identifier : String;
      Box        : Gtk_Widget;
      Info       : Glide_Kernel.Modules.Line_Information_Data)
   is
      Column : Integer;
      Num    : Gint := 1;
      Height : Gint;
      Width  : Gint := -1;
      Widths : array (Info'Range) of Gint;
      Layout : Pango_Layout;

   begin
      Layout := Create_Pango_Layout (Box);
      Set_Font_Description
        (Layout,
         Get_Pref (Buffer.Kernel, Source_Editor_Font));

      --  Compute the maximum width of the items to add.
      --  We compute this width once and for all and in advance,
      --  because is is quite expensive, and we don't want to do it
      --  in Src_Editor_View.Redraw_Columns, since that function is
      --  called a great number of times.

      for J in Info'Range loop
         Widths (J) := -1;
         if Info (J).Text /= null then
            Set_Text (Layout, String'(Info (J).Text.all));
            Get_Pixel_Size (Layout, Num, Height);

            if Num = 0 then
               Num := 1;
            end if;

            Widths (J) := Num;

            if Num > Width then
               Width := Num;
            end if;
         end if;

         if Info (J).Image /= Null_Pixbuf then
            Num := Get_Width (Info (J).Image);

            if Num > Width then
               Widths (J) := Num;
               Width := Num;
            end if;
         end if;
      end loop;

      --  Get the column that corresponds to Identifier,
      --  create it if necessary.
      Get_Column_For_Identifier
        (Buffer,
         Identifier,
         Integer (Width),
         Column);

      --  Update the stored data.
      for J in Info'Range loop
         Insert_At_Position
           (Buffer, Info (J), Column, J, Integer (Widths (J)));
      end loop;

      Unref (Layout);

      Side_Column_Changed (Buffer);
   end Add_File_Information;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (Buffer          : access Source_Buffer_Record;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean)
   is
      Col : Integer;

   begin
      Get_Column_For_Identifier
        (Buffer, Identifier, -1, Col);

      Buffer.Line_Info (Col).Stick_To_Data := Stick_To_Data;
      Buffer.Line_Info (Col).Every_Line    := Every_Line;
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Buffer : access Source_Buffer_Record'Class;
      Column : Integer)
   is
      Width : Integer;

      procedure Free is new Ada.Unchecked_Deallocation
        (Line_Info_Width_Array, Line_Info_Width_Array_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Line_Info_Display_Record, Line_Info_Display_Access);

      Line_Info  : Line_Info_Display_Array_Access
        renames Buffer.Line_Info;
   begin
      Width := Line_Info (Column).Width;

      for J in Line_Info (Column).Column_Info'Range loop
         Free (Line_Info (Column).Column_Info (J));
      end loop;

      GNAT.OS_Lib.Free (Line_Info (Column).Identifier);
      Free (Line_Info (Column).Column_Info);
      Free (Line_Info (Column));

      declare
         A : Line_Info_Display_Array
           (Line_Info.all'First .. Line_Info.all'Last - 1);
      begin
         A (Line_Info.all'First .. Column - 1) :=
           Line_Info (Line_Info.all'First .. Column - 1);
         A (Column .. Line_Info.all'Last - 1) :=
           Line_Info (Column + 1 .. Line_Info.all'Last);
         Unchecked_Free (Line_Info);
         Line_Info := new Line_Info_Display_Array'(A);
      end;

      for J in Column .. Line_Info.all'Last loop
         Line_Info (J).Starting_X :=
           Line_Info (J).Starting_X - Width - 2;
      end loop;

      Buffer.Total_Column_Width := Buffer.Total_Column_Width - Width - 2;
   end Remove_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (Buffer     : access Source_Buffer_Record;
      Identifier : String)
   is
      Col : Integer;
   begin
      Get_Column_For_Identifier (Buffer, Identifier, -1, Col);
      Remove_Line_Information_Column (Buffer, Col);

      Side_Column_Changed (Buffer);
   end Remove_Line_Information_Column;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Integer;
      Number : Integer)
   is
      Real_Lines : Natural_Array_Access renames Buffer.Real_Lines;
   begin
      if Number <= 0 then
         return;
      end if;

      if not Buffer.Original_Text_Inserted then
         Buffer.Original_Lines_Number := Number;

         if Buffer.Original_Lines_Number > Real_Lines'Last then
            declare
               A : constant Natural_Array := Real_Lines.all;
            begin
               Real_Lines := new Natural_Array
                 (1 .. Number * 2);
               Real_Lines (A'Range) := A;
               Real_Lines (A'Last + 1 .. Real_Lines'Last)
                 := (others => 0);
            end;
         end if;

         for J in 1 .. Number loop
            Real_Lines (J) := J;
         end loop;

         Buffer.Original_Text_Inserted := True;

      else
         --  Shift down the existing lines.

         for J in reverse Start + 1 .. Real_Lines'Last loop
            Real_Lines (J) := Real_Lines (J - Number);
         end loop;

         --  Reset the newly inserted lines.

         Real_Lines
           (Integer'Max (Start - Number + 1, Real_Lines'First) .. Start)
           := (others => 0);
      end if;
   end Add_Lines;

   ------------------
   -- Remove_Lines --
   ------------------

   procedure Remove_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Integer;
      End_Line   : Integer)
   is
      Real_Lines : Natural_Array_Access renames Buffer.Real_Lines;
   begin
      if End_Line <= Start_Line then
         return;
      end if;

      Real_Lines
        (Start_Line + 1 .. Real_Lines'Last + Start_Line - End_Line) :=
        Real_Lines (End_Line + 1 .. Real_Lines'Last);

      Real_Lines
        (Real_Lines'Last + Start_Line - End_Line + 1
           .. Real_Lines'Last) := (others => 0);
   end Remove_Lines;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Info_Width) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Line_Information_Record, Line_Information_Access);
   begin
      if X.Info /= null then
         Free (X.Info.all);
      end if;

      Free (X.Info);
   end Free;

   -----------------------
   -- Needs_To_Be_Saved --
   -----------------------

   function Needs_To_Be_Saved
     (Buffer : access Source_Buffer_Record'Class)
      return Boolean is
   begin
      case Get_Status (Buffer) is
         when Unmodified | Saved =>
            Buffer.Last_Saved_Position := Get_Position (Buffer.Queue);
            return False;

         when Modified =>
            if Buffer.Last_Saved_Position /= Get_Position (Buffer.Queue) then
               Buffer.Last_Saved_Position := Get_Position (Buffer.Queue);
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

end Src_Editor_Buffer;
