-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                use Glib;
with Gtk.Box;             use Gtk.Box;
with Gtk.Text;            use Gtk.Text;
with Gtk.Layout;          use Gtk.Layout;
with Gtk.Extra.PsFont;    use Gtk.Extra.PsFont;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Color;           use Gdk.Color;
with Gdk.Font;            use Gdk.Font;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Scrollbar;       use Gtk.Scrollbar;
with Language;            use Language;
with Debugger;            use Debugger;
with Ada.Direct_IO;
with Gtkada.Types;        use Gtkada.Types;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Widget;          use Gtk.Widget;

with Unchecked_Deallocation;

with Gtk.Handlers; use Gtk.Handlers;

with Ada.Text_IO; use Ada.Text_IO;

package body Gtkada.Code_Editors is

   Do_Color_Highlighting : constant Boolean := True;
   --  Indicate whether the editor should provide color highlighting.

   Layout_Width : constant := 10;
   --  Width for the area reserved for the buttons.

   package Editor_Cb is new Callback (Code_Editor_Record);

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   -------------------
   -- Scroll_Layout --
   -------------------
   --  We can not make both the Gtk_Text and the Gtk_Layout use the same
   --  Gtk_Adjustment, since they both try to modify it when they are resized,
   --  resulting in an infinite loop and a Storage_Error. Instead, they both
   --  have their own adjustment, and we synchronize the Gtk_Layout ones with
   --  the Gtk_Text ones whenever it is needed.

   procedure Scroll_Layout (Editor : access Code_Editor_Record'Class) is
   begin
      Set_Value (Get_Vadjustment (Editor.Buttons),
                 Get_Value (Get_Vadj (Editor.Text)));
   end Scroll_Layout;

   procedure Scroll_Layout_Changed (Editor : access Code_Editor_Record'Class)
   is
   begin
      Set_Upper (Get_Vadjustment (Editor.Buttons),
                 Get_Upper (Get_Vadj (Editor.Text)));
      Set_Lower (Get_Vadjustment (Editor.Buttons),
                 Get_Lower (Get_Vadj (Editor.Text)));

      --  Also set the value, since "value_changed" is not changed when the
      --  Gtk_Text is resized, and thus the Gtk_Layout is temporarily
      --  desynchronized.
      if Get_Value (Get_Vadjustment (Editor.Buttons))
        /= Get_Value (Get_Vadj (Editor.Text))
      then
         Set_Value (Get_Vadjustment (Editor.Buttons),
                    Get_Value (Get_Vadj (Editor.Text)));
      end if;
   end Scroll_Layout_Changed;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Editor : access Code_Editor_Record'Class) is
   begin
      Unref (Editor.Font);
      Gdk.Pixmap.Unref (Editor.Default_Pixmap);
      Gdk.Bitmap.Unref (Editor.Default_Mask);
   end Destroy_Cb;

   ------------------
   -- Gtk_New_Hbox --
   ------------------

   procedure Gtk_New_Hbox
     (Editor : out Code_Editor;
      Homogeneous : Boolean := False;
      Spacing     : Glib.Gint := 0)
   is
   begin
      Editor := new Code_Editor_Record;
      Initialize (Editor, Homogeneous, Spacing);
   end Gtk_New_Hbox;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor      : access Code_Editor_Record'Class;
      Homogeneous : in  Boolean := False;
      Spacing     : in  Gint := 0)
   is
      Scrollbar : Gtk_Vscrollbar;
   begin
      Gtk.Box.Initialize_Hbox (Editor, Homogeneous => False);

      Gtk_New_Vscrollbar (Scrollbar, Null_Adjustment);
      Gtk_New (Editor.Text, Vadj => Get_Adjustment (Scrollbar));
      Set_Editable (Editor.Text, False);

      --  Set a minimal size for the layout, so that the buttons are visible.
      --  Note that this widget is resized vertically dynamically if needed,
      --  so we can just set a size of 0.
      Gtk_New (Editor.Buttons);
      Set_Usize (Editor.Buttons, Layout_Width, 0);

      Editor_Cb.Object_Connect
        (Get_Vadj (Editor.Text), "value_changed",
         Editor_Cb.To_Marshaller (Scroll_Layout'Access),
         Slot_Object => Editor);
      Editor_Cb.Object_Connect
        (Get_Vadj (Editor.Text), "changed",
         Editor_Cb.To_Marshaller (Scroll_Layout_Changed'Access),
         Slot_Object => Editor);
      Editor_Cb.Object_Connect
        (Get_Vadj (Editor.Text), "destroy",
         Editor_Cb.To_Marshaller (Destroy_Cb'Access),
         Slot_Object => Editor);

      Pack_Start (Editor, Editor.Buttons, Expand => False, Fill => False);
      Pack_Start (Editor, Editor.Text, Expand => True, Fill => True);
      Pack_Start (Editor, Scrollbar, Expand => False, Fill => False);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Editor         : access Code_Editor_Record;
                        Ps_Font_Name   : String;
                        Font_Size      : Gint;
                        Default_Icon   : chars_ptr_array;
                        Comments_Color : String;
                        Strings_Color  : String;
                        Keywords_Color : String)
   is
   begin
      Editor.Font := Get_Gdkfont (Ps_Font_Name, Font_Size);

      Realize (Editor.Text);
      Create_From_Xpm_D (Editor.Default_Pixmap,
                         Get_Window (Editor.Text),
                         Editor.Default_Mask,
                         White (Get_System),
                         Default_Icon);
      Editor.Colors (Comment_Text) := Parse (Comments_Color);
      Alloc (Get_System, Editor.Colors (Comment_Text));
      Editor.Colors (String_Text)  := Parse (Strings_Color);
      Alloc (Get_System, Editor.Colors (String_Text));
      Editor.Colors (Keyword_Text) := Parse (Keywords_Color);
      Alloc (Get_System, Editor.Colors (Keyword_Text));

      --  ???Unfortunately, it is not possible currently to specify the
      --  step_increment for the adjustments, since this is overriden in
      --  several places in the text widget.
      --    Set_Step_Increment
      --     (Get_Vadj (Editor.Text),
      --      Gfloat (Get_Ascent (Editor.Font) + Get_Descent (Editor.Font)));

   end Configure;

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language
     (Editor : access Code_Editor_Record;
      Lang   : access Language.Language_Root'Class)
   is
   begin
      Free (Editor.Lang);
      Editor.Lang := new Language_Root'Class'(Lang.all);
   end Set_Current_Language;

   -----------
   -- Clear --
   -----------

   procedure Clear (Editor : access Code_Editor_Record) is
   begin
      Delete_Text (Editor.Text);
   end Clear;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor    : access Code_Editor_Record;
      File_Name : String;
      Debug     : access Debugger.Debugger_Root'Class)
   is
      function Func (File : String; Line : Positive) return Boolean;

      ----------
      -- Func --
      ----------

      function Func (File : String; Line : Positive) return Boolean is
      begin
         return Line_Contains_Code (Debug, File, Line);
      end Func;

   begin
      Load_File (Editor, File_Name, Func'Unrestricted_Access,
                 Editor.Default_Pixmap, Editor.Default_Mask);
   end Load_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor    : access Code_Editor_Record;
      File_Name : String;
      Icon_Func : Icon_Function;
      Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap)
   is
      package Char_Direct_IO is new Ada.Direct_IO (Character);
      F : Char_Direct_IO.File_Type;
      Length : Char_Direct_IO.Count;
      Buffer : String_Access;
      Line        : Positive := 1;
      Index       : Positive := 1;
      Line_Height : Gint;
      Line_Start  : Positive := 1;
      Entity      : Language_Entity;
      Next_Char   : Positive;

   begin

      --  Avoid reloading a file twice.
      --  This also solve the problem of recursive loops ("info line" in gdb,
      --  with annotation level set to 1 will print a file reference as well).

      if Editor.Current_File /= null
        and then Editor.Current_File.all = File_Name
      then
         return;
      else
         Free (Editor.Current_File);
         Editor.Current_File := new String'(File_Name);
      end if;

      --  Clear the old file and the old icons.
      Freeze (Editor.Buttons);
      Forall (Editor.Buttons, Gtk.Widget.Destroy_Cb'Access);
      Thaw (Editor.Buttons);

      Freeze (Editor.Text);
      Delete_Text (Editor.Text);
      Thaw (Editor.Text);

      --  Read the size of the file
      Char_Direct_IO.Open (F, Char_Direct_IO.In_File, File_Name);
      Length := Char_Direct_IO.Size (F);
      Char_Direct_IO.Close (F);

      --  Allocate the buffer
      Buffer := new String (1 .. Positive (Length));

      declare
         type Fixed_String is new String (1 .. Positive (Length));
         package String_Direct_IO is new Ada.Direct_IO (Fixed_String);
         F : String_Direct_IO.File_Type;
      begin
         String_Direct_IO.Open (F, String_Direct_IO.In_File, File_Name);
         String_Direct_IO.Read (F, Fixed_String (Buffer.all));
         String_Direct_IO.Close (F);
      end;

      --  Insert the contents of the buffer in the text area.

      Line_Height := Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);

      Freeze (Editor.Text);
      Freeze (Editor.Buttons);

      while Index <= Buffer'Last loop

         case Buffer (Index) is

            when ASCII.CR =>  --  ignore, this is processed as ASCII.LF
               Index := Index + 1;

            when ASCII.LF =>
               if Do_Color_Highlighting then
                  Insert (Editor.Text, Editor.Font, Null_Color, Null_Color,
                          Buffer (Index .. Index));
               else
                  Insert (Editor.Text, Editor.Font, Null_Color, Null_Color,
                          Buffer (Line_Start .. Index));
               end if;

               if Icon_Func (File_Name, Line) then
                  declare
                     Pix : Gtk_Pixmap;
                  begin
                     Gtk_New (Pix, Pixmap, Mask);
                     Put (Editor.Buttons, Pix,
                          X => 0,
                          Y => Gint (Line - 1) * Line_Height + 3);
                  end;
               end if;

               Line := Line + 1;
               Index := Index + 1;
               Line_Start := Index;

            when others =>
               if Do_Color_Highlighting then
                  if Editor.Lang /= null then
                     Looking_At (Editor.Lang, Buffer (Index .. Buffer'Last),
                                 Entity, Next_Char);
                  else
                     Next_Char := Index + 1;
                     Entity := Normal_Text;
                  end if;
                  Insert (Editor.Text, Editor.Font, Editor.Colors (Entity),
                          Null_Color, Buffer (Index .. Next_Char - 1));

                  --  Count the number of lines
                  for J in Index .. Next_Char - 1 loop
                     if Buffer (J) = ASCII.LF then
                        Line := Line + 1;
                     end if;
                  end loop;

                  Index := Next_Char;
               else
                  Index := Index + 1;
               end if;
         end case;
      end loop;

      Thaw (Editor.Text);

      --  For the buttons to become visible again, we have to hide the layout,
      --  and then show it again... Don't know why !

      Hide (Editor.Buttons);
      Show_All (Editor.Buttons);
      Thaw (Editor.Buttons);

      Free (Buffer);

   exception

      --  File not found
      when Name_Error =>
         Put_Line (Standard_Error, "WARNING: File not found: " & File_Name);
   end Load_File;
end Gtkada.Code_Editors;
