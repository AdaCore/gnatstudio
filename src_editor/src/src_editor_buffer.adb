-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Glib.Values;            use Glib.Values;
with Gtk;                    use Gtk;
with Gtk.Handlers;           use Gtk.Handlers;
with Gtk.Text_Iter;          use Gtk.Text_Iter;
with Gtk.Text_Mark;          use Gtk.Text_Mark;
with Gtk.Text_Tag_Table;     use Gtk.Text_Tag_Table;

with Language;               use Language;
with Src_Highlighting;       use Src_Highlighting;

with Interfaces.C.Strings;   use Interfaces.C.Strings;
with System;

package body Src_Editor_Buffer is

   use type System.Address;

   Default_Keyword_Color : constant String := "red";
   Default_Comment_Color : constant String := "blue";
   Default_String_Color  : constant String := "brown";
   --  ??? As soon as we have defined a uniform GLIDE handling of
   --  ??? defaults/preferences, these constants should move there.

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
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
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
   --  done before the insertiong and the text added.
   --
   --  This procedure assumes that the language has been set.

   procedure Insert_Text_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues);
   --  This procedure is just a proxy between the "insert_text" signal
   --  and the Insert_Text_Cb callback. It extracts the parameters from
   --  Params and then call Insert_Text_Cb. For efficiency reasons, the
   --  signal is processed only  when Lang is not null.
   --
   --  Note that this handler is designed to be connected "after", in which
   --  case the Insert_Iter iterator is located at the end of the inserted
   --  text.

   procedure Delete_Range_Cb
     (Buffer : access Source_Buffer_Record'Class;
      Iter   : Gtk_Text_Iter);
   --  This procedure recomputes the syntax-highlighting of the buffer
   --  in a semi-optimized manor, based on syntax-highlighting already
   --  done before the deletion and the location of deletion.
   --
   --  This procedure assumes that the language has been set.

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
   --  Re-compute the highlighting for at the least the given region.
   --  If the text creates non-closed comments or string regions, then
   --  the re-highlighted area is automatically extended to the right.
   --  When the re-highlighted area is extended to the right, the extension
   --  is computed in a semi-intelligent fashion.

   procedure Kill_Highlighting
     (Buffer : access Source_Buffer_Record'Class;
      From   : Gtk_Text_Iter;
      To     : Gtk_Text_Iter);
   --  Remove all highlighting tags for the given region.

   procedure Forward_To_Line_End (Iter : in out Gtk_Text_Iter);
   --  This is a temporary implementation of Gtk.Text_Iter.Forward_To_Line_End
   --  because the gtk+ one is broken at the moment, and causes Critical
   --  warnings.
   --  ??? Remove this procedure when the problem is fixed.

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues)
   is
      Line : Gint;
      Col  : Gint;
   begin
      Get_Cursor_Position (Buffer, Line => Line, Column => Col);
      Emit_New_Cursor_Position (Buffer, Line => Line, Column => Col);
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
      if Get_Object (Mark) /= Get_Object (Buffer.Insert_Mark) then
         declare
            Iter : Gtk_Text_Iter;
            Line : Gint;
            Col  : Gint;
         begin
            Get_Text_Iter (Glib.Values.Nth (Params, 1), Iter);
            Line := Get_Line (Iter);
            Col := Get_Line_Offset (Iter);
            Emit_New_Cursor_Position (Buffer, Line => Line, Column => Col);
         end;
      end if;
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

      Tags : Highlighting_Tags renames Buffer.Highlight_Tags;
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
   end Insert_Text_Handler;

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
      --        We are at the end of a highlighed region. We re-highlight from
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
   end Delete_Range_Handler;

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
      Lang_Context        : constant Language_Context :=
        Get_Language_Context (Buffer.Lang);
      Entity              : Language_Entity;
      Entity_Start        : Gtk_Text_Iter;
      Entity_End          : Gtk_Text_Iter;
      An_Iter             : Gtk_Text_Iter;
      Ignored             : Boolean;
      Tags                : Highlighting_Tags renames Buffer.Highlight_Tags;

      procedure Local_Highlight (From : Gtk_Text_Iter; To : Gtk_Text_Iter);
      --  Highlight the region exactly located between From and To.
      --  After this procedure is run, some variables are positioned to
      --  the following values:
      --    - Entity is equal to the last entity kind found inside the
      --      given region
      --    - Entity_Start is set to the begining of the last region found
      --      in the given buffer slice
      --    - Entity_End is set to the end of the given buffer slice.

      procedure Local_Highlight (From : Gtk_Text_Iter; To : Gtk_Text_Iter) is
         Slice               : constant String := Get_Slice (From, To);
         Slice_Offset        : constant Gint := Get_Offset (From);
         Entity_Start_Offset : Gint;
         Entity_End_Offset   : Gint;
         Entity_Length       : Gint;
         Index               : Natural := Slice'First;
         Next_Char           : Positive;
      begin
         --  First, un-apply all the style tags...
         Kill_Highlighting (Buffer, From, To);

         --  Now re-highlight the text...
         Copy (Source => From, Dest => Entity_Start);
         Entity_Start_Offset := Slice_Offset;

         Highlight_Loop :
         loop
            Looking_At
              (Buffer.Lang, Slice (Index .. Slice'Last), Entity, Next_Char);

            Entity_Length := Gint (Next_Char - Index);
            Entity_End_Offset := Entity_Start_Offset + Entity_Length;
            Get_Iter_At_Offset (Buffer, Entity_End, Entity_End_Offset);

            if Entity in Standout_Language_Entity then
               Apply_Tag (Buffer, Tags (Entity), Entity_Start, Entity_End);
            end if;

            exit Highlight_Loop when Next_Char > Slice'Last;

            Index := Next_Char;
            Entity_Start_Offset := Entity_End_Offset;
            Copy (Source => Entity_End, Dest => Entity_Start);
         end loop Highlight_Loop;
      end Local_Highlight;

   begin
      Local_Highlight (From => Start_Iter, To => End_Iter);

      --  Check the last entity found during the highlighting loop
      case Entity is

         when Normal_Text |
              Keyword_Text =>
            --  Nothing to do in that case, the current highlighting is already
            --  perfect.
            null;

         when Comment_Text =>

            if Lang_Context.New_Line_Comment_Start_Length /= 0 then
               --  Apply the Comment_Text tag from the end of the current
               --  highlighted area to the end of the current line.
               Copy (Source => Entity_End, Dest => Entity_Start);
               Forward_To_Line_End (Entity_End);
               Kill_Highlighting (Buffer, Entity_Start, Entity_End);
               Apply_Tag (Buffer, Tags (Entity), Entity_Start, Entity_End);

            elsif Lang_Context.Comment_End_Length /= 0 then
               --  In this case, comments end with a defined string. check the
               --  last characters if they match the end-of-comment string. If
               --  they match, then verify that it is indeed an end-of-comment
               --  string when the start-of-comment and end-of-comment strings
               --  are identical (this is done by checking if the comments
               --  tag is applied at the begining of the suposed end-of-comment
               --  string). If the comment area is not closed, then
               --  re-highlight up to the end of the buffer.

               Copy (Source => Entity_End, Dest => An_Iter);
               Backward_Chars
                 (An_Iter, Gint (Lang_Context.Comment_End_Length), Ignored);

               if Get_Slice (An_Iter, Entity_End) /=
                    Lang_Context.Comment_End or else
                  not (Lang_Context.Comment_Start =
                         Lang_Context.Comment_End and then
                       Has_Tag (An_Iter, Tags (Comment_Text)))
               then
                  Forward_To_End (Entity_End);
                  Local_Highlight (From => Entity_Start, To => Entity_End);
               end if;

            end if;

         when String_Text =>
            --  First, check that we don't have a constant character...
            --  We have a constant Character region if the length of the
            --  region is exactly 3 (2 delimiters + the character), and
            --  the first and last characters are equal to
            --  Lang_Context.Constant_Character.

            if Get_Offset (Entity_Start) - Get_Offset (Entity_End) = 3
               and then
               Get_Char (Entity_Start) = Lang_Context.Constant_Character
            then
               Copy (Source => Entity_End, Dest => An_Iter);
               Backward_Char (An_Iter, Ignored);
               if Get_Char (An_Iter) = Lang_Context.Constant_Character then
                  --  In this case, the text following this area is not
                  --  affected, so there is no more text to re-highlight.
                  return;
               end if;
            end if;

            --  Now, verify that the string is closed by checking the last
            --  character against the string delimiter. If we find it,
            --  then make sure that we are indeed closing the string
            --  (as opposed to opening it) by checking whether the String_Text
            --  tag is applied at the position before the string delimiter.

            Copy (Source => Entity_End, Dest => An_Iter);
            Backward_Char (An_Iter, Ignored);

            if Get_Char (An_Iter) /= Lang_Context.String_Delimiter or else
               not Has_Tag (An_Iter, Tags (String_Text))
            then
               Forward_To_Line_End (Entity_End);
               Local_Highlight (From => Entity_Start, To => Entity_End);
            end if;
      end case;
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
         Remove_Tag (Buffer, Buffer.Highlight_Tags (Entity_Kind), From, To);
      end loop;
   end Kill_Highlighting;

   -------------------------
   -- Forward_To_Line_End --
   -------------------------

   procedure Forward_To_Line_End (Iter : in out Gtk_Text_Iter) is
      Result_Ignored : Boolean;
   begin
      while not Is_End (Iter) and then not Ends_Line (Iter) loop
         Forward_Char (Iter, Result_Ignored);
      end loop;
   end Forward_To_Line_End;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Lang   : Language.Language_Access := null) is
   begin
      Buffer := new Source_Buffer_Record;
      Initialize (Buffer, Lang);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Lang   : Language.Language_Access := null)
   is
      Tags : Gtk_Text_Tag_Table;
   begin
      Gtk.Text_Buffer.Initialize (Buffer);
      Glib.Object.Initialize_Class_Record
        (Buffer, Signals, Class_Record, "GlideSourceBuffer",
         Signal_Parameters);

      Buffer.Lang := Lang;

      Buffer.Highlight_Tags :=
        Create_Tags
          (Keyword_Color => Default_Keyword_Color,
           Comment_Color => Default_Comment_Color,
           String_Color => Default_String_Color);
      --  ??? Use preferences for the colors...

      --  Save the newly created highlighting tags into the source buffer
      --  tag table.

      Tags := Get_Tag_Table (Buffer);

      for Entity_Kind in Standout_Language_Entity'Range loop
         Text_Tag_Table.Add (Tags, Buffer.Highlight_Tags (Entity_Kind));
      end loop;

      --  Save the insert mark for fast retrievals, since we will need to
      --  access it very often.

      Buffer.Insert_Mark := Get_Insert (Buffer);

      --  And finally, connect ourselves to the interestings signals

      Buffer_Callback.Connect
        (Buffer, "changed", Cb => Changed_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, "mark_set", Cb => Mark_Set_Handler'Access, After => True);
      Buffer_Callback.Connect
        (Buffer, "insert_text",
         Cb => Insert_Text_Handler'Access,
         After => True);
      Buffer_Callback.Connect
        (Buffer, "delete_range",
         Cb => Delete_Range_Handler'Access,
         After => True);
   end Initialize;

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
         if Buffer.Lang /= null then
            Highlight_Slice (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
         else
            Kill_Highlighting (Buffer, Buffer_Start_Iter, Buffer_End_Iter);
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
      Line := Get_Line (Insert_Iter);
      Column := Get_Line_Offset (Insert_Iter);
   end Get_Cursor_Position;

end Src_Editor_Buffer;
