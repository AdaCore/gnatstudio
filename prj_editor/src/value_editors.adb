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

package body Value_Editors is

   Ref_Background : constant String := "#AAAAAA";
   --  <preference> Color to use for the background of variable references

   procedure Realized (Widget : access Gtk_Widget_Record'Class);
   --  Called when the widget is realized.

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
      Widget_Callback.Connect
        (Editor, "realize", Widget_Callback.To_Marshaller (Realized'Access));
      Editor.Insert_Id := Widget_Callback.Connect
        (Editor, "insert_text", Insert_Text'Access, After => True);
      Editor.Delete_Id := Widget_Callback.Connect
        (Editor, "delete_text", Delete_Text'Access, After => True);
   end Initialize;

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
      Highlight_Region (Editor, Natural (Start_Pos), Natural (End_Pos));
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
            Insert (Editor,
                    Back => Editor.Grey,
                    Chars => Buffer (Var_Start .. Index));
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
      Length : constant Gint := To_Gint (Params, 2);
      Position : Address := To_Address (Params, 3);
      Pos : constant Guint := To_Guint_Ptr (Position).all;
   begin
      --  Note that there might be several lines impacted in case of a
      --  copy-paste.
      Highlight_Region
        (Editor, Natural (Pos) - Natural (Length), Natural (Pos));
   end Insert_Text;

   --------------
   -- Realized --
   --------------

   procedure Realized (Widget : access Gtk_Widget_Record'Class) is
      Editor : Value_Editor := Value_Editor (Widget);
   begin
      Editor.Grey := Parse (Ref_Background);
      Alloc (Get_Default_Colormap, Editor.Grey);
   end Realized;

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
      Set_USize (Editor, -1,
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
end Value_Editors;
