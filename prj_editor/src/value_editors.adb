-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
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

with Gdk.Color;       use Gdk.Color;
with Gdk.Font;        use Gdk.Font;
with Glib;            use Glib;
with Gtk.Arguments;   use Gtk.Arguments;
with Gtk.Handlers;    use Gtk.Handlers;
with Gtk.Style;       use Gtk.Style;
with Gtk.Text;        use Gtk.Text;
with Gtk.Widget;      use Gtk.Widget;
with Gtkada.Handlers; use Gtkada.Handlers;

with System;          use System;
with Unchecked_Conversion;

with Prj.Tree;        use Prj.Tree;
with Prj;             use Prj;
with Types;           use Types;
with Stringt;         use Stringt;
with Namet;           use Namet;

with Prj_API;                  use Prj_API;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;

package body Value_Editors is

   procedure Insert_Text
     (Widget : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  Called when some text is inserted in the widget

   procedure Delete_Text
     (Widget : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);
   --  Called when some text is removed from the widget

   procedure Highlight_Line
     (Editor : access Value_Editor_Record'Class;
      Buffer : String;
      From, To : Natural);
   --  Properly highlight the current line (grey-out variable references)

   procedure Highlight_Region
     (Editor : access Value_Editor_Record'Class; From, To : Natural);
   --  Highlight the lines that contain the region From .. To.
   --  This is needed since Highlight_Line must be called for whole lines,
   --  not part of them.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Editor : out Value_Editor) is
   begin
      Editor := new Value_Editor_Record;
      Value_Editors.Initialize (Editor);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Editor : access Value_Editor_Record'Class) is
   begin
      Gtk.Text.Initialize (Editor);

      Editor.Insert_Id := Widget_Callback.Connect
        (Editor, "insert_text", Insert_Text'Access, After => True);
      Editor.Delete_Id := Widget_Callback.Connect
        (Editor, "delete_text", Delete_Text'Access, After => True);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Editor : access Value_Editor_Record'Class;
      Kernel : access Kernel_Handle_Record'Class) is
   begin
      Editor.Var_Ref := Get_Pref (Kernel, Variable_Ref_Background);
      Editor.Invalid_Ref := Get_Pref (Kernel, Invalid_Variable_Ref_Background);
   end Configure;

   -----------------
   -- Delete_Text --
   -----------------

   procedure Delete_Text
     (Widget : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Editor : Value_Editor := Value_Editor (Widget);
      Start_Pos : Gint := To_Gint (Params, 1);
      End_Pos   : Gint := To_Gint (Params, 2);
   begin
      if End_Pos >= Start_Pos then
         Highlight_Region (Editor, Natural (Start_Pos), Natural (End_Pos));
      end if;
   end Delete_Text;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Editor : access Value_Editor_Record'Class;
      Buffer : String;
      From, To : Natural)
   is
      Index : Natural := From;
      Var_Start : Natural := 0;
      Var_End : Natural := From;
      Position : constant Gint := Get_Position (Editor);
   begin
      --  We have to delete the whole line, in case some part of it was
      --  greyed-out, but should now be on white background.
      --  We also want to avoid calling recursively the insert_text and
      --  delete_text handlers
      Handler_Block (Editor, Editor.Insert_Id);
      Handler_Block (Editor, Editor.Delete_Id);
      Delete_Text
        (Editor, Gint (From - Buffer'First), Gint (To - Buffer'First + 1));
      Set_Point (Editor, Guint (From - Buffer'First));

      while Index <= To loop
         if Index < To
           and then Buffer (Index) = '$'
           and then Buffer (Index + 1) = '{'
         then
            Insert (Editor, Chars => Buffer (Var_End .. Index - 1));
            Var_Start := Index;
            Index := Index + 2;

         elsif Buffer (Index) = '}' and then Var_Start /= 0 then
            if Editor.Allow_Ref then
               Insert (Editor,
                       Back => Editor.Var_Ref,
                       Chars => Buffer (Var_Start .. Index));
            else
               Insert (Editor,
                       Fore => Editor.Invalid_Ref,
                       Chars => Buffer (Var_Start .. Index));
            end if;
            Var_Start := 0;
            Index := Index + 1;
            Var_End := Index;

         else
            Index := Index + 1;
         end if;
      end loop;

      if Var_Start /= 0 then
         Insert (Editor, Chars => Buffer (Var_Start .. Index - 1));
      elsif Var_End /= Index then
         Insert (Editor, Chars => Buffer (Var_End .. Index - 1));
      end if;

      Handler_Unblock (Editor, Editor.Insert_Id);
      Handler_Unblock (Editor, Editor.Delete_Id);
      Set_Position (Editor, Position);
   end Highlight_Line;

   ----------------------
   -- Highlight_Region --
   ----------------------

   procedure Highlight_Region
     (Editor : access Value_Editor_Record'Class; From, To : Natural)
   is
      Buffer : constant String := Get_Chars (Editor);
      Index : Natural := From;
      Line_Start : Natural;
   begin
      --  We rehighlight the whole current line, since the user might have
      --  inserted a '$', a '{' or a '}' in the middle that changes completely
      --  the meaning of the line.

      while Index >= Buffer'First
        and then Buffer (Index) /= ASCII.LF
      loop
         Index := Index - 1;
      end loop;
      Index := Index + 1;
      Line_Start := Index;

      while Index <= Buffer'Last loop
         if Buffer (Index) = ASCII.LF then
            Highlight_Line (Editor, Buffer, Line_Start, Index - 1);
            Line_Start := Index + 1;
            exit when Index > To;
         end if;
         Index := Index + 1;
      end loop;
      if Line_Start < Index then
         Highlight_Line (Editor, Buffer, Line_Start, Index - 1);
      end if;
   end Highlight_Region;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Widget : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      type Guint_Ptr is access all Guint;
      function To_Guint_Ptr is new Unchecked_Conversion (Address, Guint_Ptr);

      Editor : Value_Editor := Value_Editor (Widget);
      Str : constant String := To_String (Params, 1);
      Length : constant Gint := To_Gint (Params, 2);
      Position : Address := To_Address (Params, 3);
      Pos : constant Guint := To_Guint_Ptr (Position).all;
      Current_Position : constant Gint := Get_Position (Editor);
   begin
      --  Inserting a new line isn't authorized when editing single values
      if Editor.Expr_Kind = Prj.Single then
         for J in Str'Range loop
            if Str (J) = ASCII.LF then
               Handler_Block (Editor, Editor.Insert_Id);
               Handler_Block (Editor, Editor.Delete_Id);
               Delete_Text (Editor,
                            Gint (Pos) - Length + Gint (J - Str'First),
                            Gint (Pos) - Length + Gint (J - Str'First) + 1);
               Set_Point
                 (Editor, Pos - Guint (Length) + Guint (J - Str'First));
               Insert (Editor, Chars => " ");
               Handler_Unblock (Editor, Editor.Insert_Id);
               Handler_Unblock (Editor, Editor.Delete_Id);
               Set_Position (Editor, Current_Position);
            end if;
         end loop;
      end if;

      --  Note that there might be several lines impacted in case of a
      --  copy-paste.
      Highlight_Region
        (Editor, Natural (Pos) - Natural (Length), Natural (Pos));
   end Insert_Text;

   -----------------------
   -- Set_Visible_Lines --
   -----------------------

   procedure Set_Visible_Lines
     (Editor : access Value_Editor_Record; Lines : Natural)
   is
      F : Gdk_Font;
      Text_Border_Room : constant := 1;  --  See definition in gtktext.c
   begin
      Realize (Editor);
      F := Get_Font (Get_Style (Editor));
      Set_Size_Request
        (Editor, -1,
         Gint (Lines) * (Get_Ascent (F) + Get_Descent (F))
         + 2 * (Y_Thickness (Get_Style (Editor)) + Text_Border_Room));
   end Set_Visible_Lines;

   ----------------------------
   -- Add_Variable_Reference --
   ----------------------------

   procedure Add_Variable_Reference
     (Editor : access Value_Editor_Record; Var_Name : String) is
   begin
      Insert (Editor, Chars => "${" & Var_Name & "}");
   end Add_Variable_Reference;

   ----------------------
   -- Allow_References --
   ----------------------

   procedure Allow_References
     (Editor : access Value_Editor_Record; Allow : Boolean) is
   begin
      Editor.Allow_Ref := Allow;
   end Allow_References;

   --------------------
   -- Check_Validity --
   --------------------

   function Check_Validity
     (Editor : access Value_Editor_Record) return Validity
   is
      Buffer : constant String := Get_Chars (Editor);
      Var_Start : Natural := 0;
      Var_End : Natural := Buffer'First;
      Index : Natural := Buffer'First;
   begin
      while Index <= Buffer'Last loop
         if Index < Buffer'Last
           and then Buffer (Index) = '$'
           and then Buffer (Index + 1) = '{'
         then
            Var_Start := Index;
            Index := Index + 2;

         elsif Buffer (Index) = '}' and then Var_Start /= 0 then
            if not Editor.Allow_Ref then
               return Unexpected_Ref;
            end if;

            --  ??? Must check for circular dependencies

            Var_Start := 0;
            Index := Index + 1;
            Var_End := Index;

         elsif Buffer (Index) = ASCII.LF
           and then Var_Start /= 0
           and then Editor.Allow_Ref
         then
            --  ??? Should check for empty lines
            return Unterminated_Ref;

         else
            Index := Index + 1;
         end if;
      end loop;

      if Var_Start /= 0 and then Editor.Allow_Ref then
         return Unterminated_Ref;
      end if;

      return Valid;
   end Check_Validity;

   ---------------
   -- Set_Value --
   ---------------

   function Get_Value
     (Editor : access Value_Editor_Record;
      Project : Project_Node_Id) return Prj.Tree.Project_Node_Id
   is
      Buffer : constant String := Get_Chars (Editor);
      Var_Start : Natural := 0;
      Var_End : Natural := Buffer'First;
      Index : Natural := Buffer'First;
      Str : Project_Node_Id;

      Is_Enumeration_Type : constant Boolean := not Editor.Allow_Ref;
      --  True if we are processing an enumeration type.
      --  ??? Should this be a parameter to Get_Value instead.

      List_Expr : Project_Node_Id := Empty_Node;
      --  Global expression (handles lists)

      Line_Expr  : Project_Node_Id := Empty_Node;
      --  Expression for the current line

      procedure Add_String (From, To : Natural);
      --  Add a new literal string to the current line expression.
      --  If we are processing a type declaration, append the value to the
      --  list of possible values.

      ----------------
      -- Add_String --
      ----------------

      procedure Add_String (From, To : Natural) is
         Ignore : String_Id;
      begin
         if Is_Enumeration_Type then
            Ignore := Add_Possible_Value (Line_Expr, Buffer (From .. To));
         else
            --  ??? Should reuse existing strings if possible
            --  ??? Should be done in Prj_Api
            Str := Default_Project_Node (N_Literal_String);
            Start_String;
            Store_String_Chars (Buffer (From .. To));
            Set_String_Value_Of (Str, End_String);
            Concatenate (Line_Expr, Str);
         end if;
      end Add_String;

   begin
      pragma Assert (Check_Validity (Editor) = Valid);

      if Is_Enumeration_Type then
         --  ??? should reuse an existing type
         Line_Expr := Get_Or_Create_Type (Project, "???");
      end if;

      while Index <= Buffer'Last loop
         if Index < Buffer'Last
           and then Buffer (Index) = '$'
           and then Buffer (Index + 1) = '{'
         then
            Add_String (Var_End, Index - 1);
            Var_Start := Index;
            Index := Index + 2;

         elsif Buffer (Index) = '}' and then Var_Start /= 0 then
            Str := Default_Project_Node (N_Variable_Reference);

            --  ??? Should get the record for the actual variable, to
            --  initialize the rest.
            Name_Len := Index - Var_Start - 2;
            Name_Buffer (1 .. Name_Len) := Buffer (Var_Start + 2 .. Index - 1);
            Set_Name_Of (Str, Name_Find);
            Concatenate (Line_Expr, Str);

            Var_Start := 0;
            Index := Index + 1;
            Var_End := Index;

         elsif Buffer (Index) = ASCII.LF then
            if Var_End <= Index - 1 then
               Add_String (Var_End, Index - 1);
            end if;
            if not Is_Enumeration_Type then
               Concatenate_List (List_Expr, Line_Expr);
               Line_Expr := Empty_Node;
            end if;
            Index := Index + 1;
            Var_End := Index;

         else
            Index := Index + 1;
         end if;
      end loop;

      if Var_End <= Index - 1 then
         Add_String (Var_End, Index - 1);
         if not Is_Enumeration_Type and then Editor.Expr_Kind = List then
            Concatenate_List (List_Expr, Line_Expr);
         end if;
      end if;

      if List_Expr = Empty_Node then
         return Line_Expr;
      else
         return List_Expr;
      end if;
   end Get_Value;

   -----------------------
   -- Set_Variable_Kind --
   -----------------------

   procedure Set_Variable_Kind
     (Editor : access Value_Editor_Record; Expr_Kind : Prj.Variable_Kind) is
   begin
      Editor.Expr_Kind := Expr_Kind;
   end Set_Variable_Kind;

end Value_Editors;
