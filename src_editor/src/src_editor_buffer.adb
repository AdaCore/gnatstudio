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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Glib.Convert;              use Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Gtk;                       use Gtk;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Text_Iter;             use Gtk.Text_Iter;
with Gtk.Text_Mark;             use Gtk.Text_Mark;
with Gtk.Text_Tag_Table;        use Gtk.Text_Tag_Table;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.Types;              use Gtkada.Types;
with Pango.Enums;

with Basic_Types;               use Basic_Types;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Src_Highlighting;          use Src_Highlighting;

with Interfaces.C.Strings;      use Interfaces.C.Strings;
with System;
with String_Utils;              use String_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with OS_Utils;                  use OS_Utils;
with Src_Info;                  use Src_Info;
with Glide_Intl;                use Glide_Intl;

with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Commands.Editor;           use Commands.Editor;
with Src_Editor_Module;         use Src_Editor_Module;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Ada.Exceptions;            use Ada.Exceptions;
with Traces;                    use Traces;

package body Src_Editor_Buffer is

   use type System.Address;

   Me : constant Debug_Handle := Create ("Source_Editor_Buffer");

   package Buffer_Timeout is new Gtk.Main.Timeout (Source_Buffer);

   -----------------
   -- Preferences --
   -----------------

   Default_Keyword_Font_Attr : constant Font_Attributes :=
     To_Font_Attributes (Weight => Pango.Enums.Pango_Weight_Bold);
   Default_Comment_Font_Attr : constant Font_Attributes :=
     To_Font_Attributes (Style => Pango.Enums.Pango_Style_Italic);
   Default_String_Font_Attr  : constant Font_Attributes := To_Font_Attributes;
   Default_Character_Font_Attr : constant Font_Attributes :=
     To_Font_Attributes;
   --  <preferences>

   --------------------
   -- Signal Support --
   --------------------

   Class_Record : GObject_Class := Uninitialized_Class;
   --  A pointer to the 'class record'.

   Signals : Interfaces.C.Strings.chars_ptr_array :=
     (1 => New_String ("cursor_position_changed"));
   --  The list of new signals supported by this GObject

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (GType_Int, GType_Int));
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

   --------------------
   -- Buffer_Destroy --
   --------------------

   procedure Buffer_Destroy (Buffer : access Source_Buffer_Record'Class) is
   begin
      if Buffer.Timeout_Id /= 0 then
         Timeout_Remove (Buffer.Timeout_Id);
         Buffer.Timeout_Id := 0;
      end if;

      Free_Queue (Buffer.Queue);

      if Buffer.Current_Command /= null then
         Destroy (Buffer.Current_Command);
      end if;

      Free (Buffer.Filename);
   end Buffer_Destroy;

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler (Buffer : access Source_Buffer_Record'Class) is
      Line : Gint;
      Col  : Gint;
   begin
      Get_Cursor_Position (Buffer, Line => Line, Column => Col);
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

      if Get_Object (Mark) /= Get_Object (Buffer.Insert_Mark) then

         --  If the mark corresponds to a cursor position, set the stored
         --  Insert_Mark accordingly.
         declare
            Mark_Name : String := Get_Name (Mark);
         begin
            if Mark_Name = "insert"
              or else Mark_Name = "gtk_drag_target"
            then
               Buffer.Insert_Mark := Mark;
            end if;
         end;

         Get_Cursor_Position (Buffer, Line, Col);
         Emit_New_Cursor_Position (Buffer, Line => Line, Column => Col);
      end if;

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
      Params : Glib.Values.GValues) is
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
      Pos         : Gtk_Text_Iter;
      Length      : constant Gint := Get_Int (Nth (Params, 3));
      Text        : constant String :=
        Get_String (Nth (Params, 2), Length => Length);
      Command     : Editor_Command := Editor_Command (Buffer.Current_Command);

   begin
      if Buffer.Inserting then
         return;
      end if;

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
         Enqueue (Buffer.Queue, Command);
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
            Enqueue (Buffer.Queue, Command);
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
         Enqueue (Buffer.Queue, Command);
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
      Params : Glib.Values.GValues) is
   begin
      if Buffer.Lang /= null then
         declare
            Start_Iter : Gtk_Text_Iter;
         begin
            Get_Text_Iter (Nth (Params, 1), Start_Iter);
            Delete_Range_Cb (Buffer, Start_Iter);
         end;
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

   begin
      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);

      if not Buffer.Inserting then
         if not Is_Null_Command (Command)
           and then Get_Mode (Command) /= Deletion
         then
            End_Action (Buffer);
            Command := Editor_Command (Buffer.Current_Command);
         end if;

         if Is_Null_Command (Command) then
            Get_Cursor_Position (Buffer, Line, Column);

            if Line = Get_Line (Start_Iter)
              and then Column = Get_Line_Offset (Start_Iter)
            then
               Direction := Backward;
            else
               Direction := Forward;
            end if;

            Create (Command,
                    Deletion,
                    Source_Buffer (Buffer),
                    True,
                    Natural (Get_Line (Start_Iter)),
                    Natural (Get_Line_Offset (Start_Iter)),
                    Direction);

            Buffer.Inserting := True;
            Enqueue (Buffer.Queue, Command);
            Buffer.Inserting := False;

         else
            Direction := Get_Direction (Command);
         end if;

         Add_Text (Command,
                   Get_Slice (Buffer, Start_Iter, End_Iter, True),
                   Natural (Get_Line (Start_Iter)),
                   Natural (Get_Line_Offset (Start_Iter)));
         Buffer.Current_Command := Command_Access (Command);

         if Direction = Backward then
            End_Action (Buffer);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Delete_Range_Before_Handler;

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
         C_Str : constant Interfaces.C.Strings.chars_ptr :=
           Get_Slice (Entity_Start, Entity_End);
         Len   : constant Natural := Natural (Strlen (C_Str));
         Slice : constant Unchecked_String_Access :=
           To_Unchecked_String (C_Str);
         pragma Suppress (Access_Check, Slice);

      begin
         Highlight_Complete := True;
         Slice_Offset := Get_Offset (Entity_Start);

         --  First, un-apply all the style tags...
         Kill_Highlighting (Buffer, Entity_Start, Entity_End);

         --  Now re-highlight the text...

         Parse_Entities
           (Buffer.Lang,
            Slice (1 .. Len),
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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null)
   is
      Tags    : Gtk_Text_Tag_Table;
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
         User_Data   => Kernel_Handle (Kernel));
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
         Keyword_Font_Attr   => Default_Keyword_Font_Attr,
         Comment_Color       => Get_Pref (Kernel, Default_Comment_Color),
         Comment_Font_Attr   => Default_Comment_Font_Attr,
         Character_Color     => Get_Pref (Kernel, Default_Character_Color),
         Character_Font_Attr => Default_Character_Font_Attr,
         String_Color        => Get_Pref (Kernel, Default_String_Color),
         String_Font_Attr    => Default_String_Font_Attr);
      --  ??? Use preferences for the font attributes...


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
        (Contents.all, "UTF-8", "ISO-8859-1",
         Ignore'Unchecked_Access, Length'Unchecked_Access);

      --  --  ??? This does not seem to work, but should
      --  UTF8 := Locale_To_UTF8
      --    (Contents.all, Ignore'Unchecked_Access, Length'Unchecked_Access);

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

   begin
      Success := True;

      FD := Create_File (Filename, Fmode => Binary);

      if FD = Invalid_FD then
         Success := False;
         return;
      end if;

      Get_Bounds (Buffer, Start_Iter, End_Iter);

      declare
         New_Line       : constant String := (1 => ASCII.LF);
         File_Buffer    : constant String :=
           Get_Text (Buffer, Start_Iter, End_Iter);
         Bytes_Written  : Integer;
         First, Current : Natural := File_Buffer'First;
         Blanks         : Natural := 0;

      begin
         if not Get_Pref (Buffer.Kernel, Strip_Blanks) then
            Current := File_Buffer'Last + 1;
         else
            while Current <= File_Buffer'Last loop
               case File_Buffer (Current) is
                  when ASCII.LF | ASCII.CR =>
                     if Blanks /= 0 then
                        Bytes_Written := Write
                          (FD, File_Buffer (First)'Address, Blanks - First);
                        Bytes_Written := Write
                          (FD, New_Line'Address, New_Line'Length);
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

         if First < File_Buffer'Last then
            if Blanks /= 0 then
               Bytes_Written :=
                 Write (FD, File_Buffer (First)'Address, Blanks - First);
               Bytes_Written :=
                 Write (FD, New_Line'Address, New_Line'Length);

            else
               Bytes_Written :=
                 Write (FD, File_Buffer (First)'Address, Current - First);

               if File_Buffer (File_Buffer'Last) /= ASCII.LF
                 and then File_Buffer (File_Buffer'Last) /= ASCII.CR
               then
                  Bytes_Written :=
                    Write (FD, New_Line'Address, New_Line'Length);
               end if;
            end if;
         end if;
      end;

      Close (FD);

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
         Buffer.Filename := new String' (Filename);
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

   -------------------------
   -- Get_Screen_Position --
   -------------------------

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk_Text_Iter;
      Line   : out Gint;
      Column : out Gint)
   is
      Start : Gtk_Text_Iter;
   begin
      Line := Get_Line (Iter);
      --  Column := Get_Line_Offset (Iter);

      --  ??? Important: the solution below is definitely not the best one,
      --  since it will only handle ASCII (and Latin-1) characters, but not
      --  other characters, since we are converting to a string. Probably we
      --  should use pango to compute the width, but it only seems to return
      --  the position in pixels, which isn't very useful to us.
      Get_Iter_At_Line_Offset (Buffer, Start, Line, 0);
      declare
         S : constant String := Do_Tab_Expansion
           (Get_Text (Buffer, Start, Iter),
            Tab_Size => Integer (Get_Pref (Buffer.Kernel, Tab_Width)));
      begin
         Column := S'Length;
      end;
   end Get_Screen_Position;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint)
   is
      Insert_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Insert_Iter, Buffer.Insert_Mark);
      Get_Screen_Position (Buffer, Insert_Iter, Line, Column);
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
         return Get_Slice (Buffer, Start_Iter, End_Iter);
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

      return Get_Text (Buffer, Start_Iter, End_Iter);
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

      End_Action (Buffer);

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

      End_Action (Buffer);

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

      End_Action (Buffer);

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

   ------------------
   -- Delete_Slice --
   ------------------

   procedure Delete_Slice
     (Buffer       : access Source_Buffer_Record;
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

      Delete (Buffer, Start_Iter, End_Iter);
   end Delete_Slice;

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

   ----------------
   -- End_Action --
   ----------------

   procedure End_Action (Buffer : access Source_Buffer_Record) is
      Command : constant Editor_Command :=
        Editor_Command (Buffer.Current_Command);

   begin
      if not Is_Null_Command (Command) then
         Buffer.Current_Command := null;
      end if;
   end End_Action;

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

   ---------------
   -- Get_Queue --
   ---------------

   function Get_Queue
     (Buffer : access Source_Buffer_Record) return Command_Queue is
   begin
      return Buffer.Queue;
   end Get_Queue;

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
      Buffer.Filename := new String' (Name);
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
      Set_File_Information
        (Context,
         Dir_Name (Buffer.Filename.all),
         Base_Name (Buffer.Filename.all));
      Set_Area_Information (Context, Start_Line, End_Line);
      Glide_Kernel.Source_Lines_Revealed (Buffer.Kernel, Context);
      Free (Selection_Context_Access (Context));
   end Source_Lines_Revealed;

   ---------------------
   -- Check_Timestamp --
   ---------------------

   function Check_Timestamp
     (Buffer : access Source_Buffer_Record;
      Ask_User : Boolean := False) return Boolean
   is
      New_Timestamp : Timestamp;
   begin
      if Buffer.Filename /= null
        and then Buffer.Filename.all /= ""
        and then Is_Regular_File (Buffer.Filename.all)
      then
         New_Timestamp := To_Timestamp (File_Time_Stamp (Buffer.Filename.all));

         if New_Timestamp > Buffer.Timestamp then
            if not Ask_User
              or else Message_Dialog
              (Msg         => Base_Name (Buffer.Filename.all)
                 & (-" changed on disk. Really edit ?"),
               Dialog_Type => Confirmation,
               Buttons     => Button_Yes or Button_No,
               Title       => -"File changed on disk",
               Parent      => Get_Main_Window (Buffer.Kernel)) /= Button_Yes
            then
               return False;
            end if;

            Buffer.Timestamp := New_Timestamp;
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

end Src_Editor_Buffer;
