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
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Color;           use Gdk.Color;
with Gdk.Font;            use Gdk.Font;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Box;             use Gtk.Box;
with Gtk.Ctree;           use Gtk.Ctree;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Extra.PsFont;    use Gtk.Extra.PsFont;
with Gtk.Layout;          use Gtk.Layout;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Scrollbar;       use Gtk.Scrollbar;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text;            use Gtk.Text;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Types;        use Gtkada.Types;
with Language;            use Language;
with Debugger;            use Debugger;
with Explorer;            use Explorer;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Ada.Direct_IO;

with Unchecked_Deallocation;

with Gtk.Handlers; use Gtk.Handlers;

with Ada.Text_IO; use Ada.Text_IO;

package body Gtkada.Code_Editors is

   Do_Color_Highlighting : constant Boolean := True;
   --  Indicate whether the editor should provide color highlighting.

   Display_Explorer : constant Boolean := True;
   --  True if we should associate an explorer tree to each editor.

   Explorer_Width : constant := 150;
   --  Width for the area reserved for the explorer.

   Layout_Width : constant := 20;
   --  Width for the area reserved for the buttons.

   Line_Numbers_Width : constant Positive := 5;
   --  Number of characters reserved on the left for line numbers.

   subtype Line_Number is String (1 .. Line_Numbers_Width);
   --  Type of strings used to display line numbers.

   package Editor_Cb is new Callback (Code_Editor_Record);

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   procedure Jump_To
     (Widget : access Gtk_Widget_Record'Class; Position : Position_Type);
   --  Called by Explorer when an item in the buffer is selected.
   --  This will select the word starting at Index.

   -------------
   -- Jump_To --
   -------------

   procedure Jump_To
     (Widget : access Gtk_Widget_Record'Class; Position : Position_Type)
   is
      Editor : Code_Editor := Code_Editor (Widget);
      Last   : Positive;
      Pos    : Positive :=
        Position.Index + (Position.Line + 1) * (Line_Numbers_Width + 1) - 1;
      Buffer : String := Get_Chars (Editor.Text, Gint (Pos));

   begin
      Last := Buffer'First;

      while Last < Buffer'Last
        and then Buffer (Last) /= ' '
        and then Buffer (Last) /= '('
        and then Buffer (Last) /= ';'
      loop
         Last := Last + 1;
      end loop;

      Freeze (Editor.Text);
      Set_Position (Editor.Text, Gint (Pos));
      Select_Region
        (Editor.Text, Gint (Pos), Gint (Last + Pos - Buffer'First));
      Thaw (Editor.Text);
   end Jump_To;

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
      Set_Value
        (Get_Vadjustment (Editor.Buttons),
         Get_Value (Get_Vadj (Editor.Text)));
   end Scroll_Layout;

   procedure Scroll_Layout_Changed
     (Editor : access Code_Editor_Record'Class) is
   begin
      Set_Upper
        (Get_Vadjustment (Editor.Buttons),
         Gfloat'Max
           (Get_Upper (Get_Vadj (Editor.Text)),
            Get_Value (Get_Vadj (Editor.Text))));
      Set_Lower
        (Get_Vadjustment (Editor.Buttons),
         Get_Lower (Get_Vadj (Editor.Text)));
      Set_Page_Size
        (Get_Vadjustment (Editor.Buttons),
         Get_Page_Size (Get_Vadj (Editor.Text)));

      --  Also set the value, since "value_changed" is not changed when the
      --  Gtk_Text is resized, and thus the Gtk_Layout is temporarily
      --  desynchronized. This should not be done if the two values are
      --  already equal, do nothing to prevent loops.
      --
      --  To work around a bug in gtk+ (when adjusting the value of the
      --  adjustment when we are resing the code editor beyond the last line),
      --  we first hide it, and then show it again.

      if Get_Value (Get_Vadjustment (Editor.Buttons)) /=
        Get_Value (Get_Vadj (Editor.Text))
      then
         Hide (Editor.Buttons);
         Set_Value
           (Get_Vadjustment (Editor.Buttons),
            Get_Value (Get_Vadj (Editor.Text)));
         Show (Editor.Buttons);
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
     (Editor      : out Code_Editor;
      Homogeneous : Boolean := False;
      Spacing     : Glib.Gint := 0) is
   begin
      Editor := new Code_Editor_Record;
      Initialize (Editor, Homogeneous, Spacing);
   end Gtk_New_Hbox;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor      : access Code_Editor_Record'Class;
      Homogeneous : in Boolean := False;
      Spacing     : in Gint := 0)
   is
      Scrollbar : Gtk_Vscrollbar;
      Box       : Gtk_Hbox;
      Paned     : Gtk_Hpaned;

   begin
      Gtk.Box.Initialize_Hbox (Editor, Homogeneous => False);

      Gtk_New_Hpaned (Paned);
      Gtk_New_Hbox (Box, Homogeneous => False);
      Gtk_New_Vscrollbar (Scrollbar, Null_Adjustment);
      Gtk_New (Editor.Text, Vadj => Get_Adjustment (Scrollbar));
      Set_Editable (Editor.Text, False);
      Set_Line_Wrap (Editor.Text, False);

      --  Set a minimal size for the layout, so that the buttons are visible.
      --  Note that this widget is resized vertically dynamically if needed,
      --  so we can just set a size of 0.
      Gtk_New (Editor.Buttons);
      Set_Usize (Editor.Buttons, Layout_Width, 0);

      Gtk_New (Editor.Explorer_Scroll);
      Set_Policy (Editor.Explorer_Scroll, Policy_Automatic, Policy_Automatic);
      Set_Usize (Editor.Explorer_Scroll, Explorer_Width, -1);

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

      Pack_Start (Editor, Paned, Expand => True, Fill => True);
      Add1 (Paned, Editor.Explorer_Scroll);
      Pack_Start (Box, Editor.Buttons, Expand => False, Fill => False);
      Pack_Start (Box, Editor.Text, Expand => True, Fill => True);
      Pack_Start (Box, Scrollbar, Expand => False, Fill => False);
      Add2 (Paned, Box);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Editor         : access Code_Editor_Record;
      Ps_Font_Name   : String;
      Font_Size      : Gint;
      Default_Icon   : Chars_Ptr_Array;
      Current_Line_Icon : Chars_Ptr_Array;
      Comments_Color : String;
      Strings_Color  : String;
      Keywords_Color : String;
      Show_Line_Numbers : Boolean := False)
   is
      Current_Line_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Current_Line_Mask   : Gdk.Bitmap.Gdk_Bitmap;
   begin
      Editor.Font := Get_Gdkfont (Ps_Font_Name, Font_Size);
      Editor.Show_Line_Nums := Show_Line_Numbers;

      Realize (Editor.Text);
      Create_From_Xpm_D
        (Editor.Default_Pixmap,
         Get_Window (Editor.Text),
         Editor.Default_Mask,
         White (Get_System),
         Default_Icon);
      Create_From_Xpm_D
        (Current_Line_Pixmap,
         Get_Window (Editor.Text),
         Current_Line_Mask,
         White (Get_System),
         Current_Line_Icon);

      --  Create the current line icon, and make sure it is never destroyed.
      Gtk_New (Editor.Current_Line_Button,
               Current_Line_Pixmap, Current_Line_Mask);
      Ref (Editor.Current_Line_Button);

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
      Lang   : access Language.Language_Root'Class) is
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
      --  Local wrapper for Line_Contains_Code.

      ----------
      -- Func --
      ----------

      function Func (File : String; Line : Positive) return Boolean is
      begin
         return Line_Contains_Code (Debug, File, Line);
      end Func;

   begin
      Load_File
        (Editor, File_Name, Func'Unrestricted_Access,
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
      function Line_Number_String (Line : Positive) return String;
      --  Return a string that contains the line number.
      --  The number is aligned to the right, and the string as a length of
      --  Line_Numbers_Width.

      function Line_Number_String (Line : Positive) return String is
         S : String        := Positive'Image (Line);
         N : Line_Number   := (others => ' ');
         Length : Positive := Positive'Min (S'Length - 1, N'Length);
      begin
         N (N'Last - Length + 1 .. N'Last) := S (2 .. Length + 1);
         return N & " ";
      end Line_Number_String;

      F           : File_Descriptor;
      Length      : Positive;
      Buffer      : String_Access;
      Line        : Positive := 1;
      Index       : Positive := 1;
      Line_Height : Gint;
      Line_Start  : Positive := 1;
      Entity      : Language_Entity;
      Next_Char   : Positive;
      Name        : aliased String := File_Name & ASCII.NUL;

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
         Editor.Current_File := new String' (File_Name);
      end if;

      --  Clear the old file and the old icons.
      Freeze (Editor.Buttons);

      if Get_Parent (Editor.Current_Line_Button) /= null then
         Remove (Editor.Buttons, Editor.Current_Line_Button);
      end if;

      Forall (Editor.Buttons, Gtk.Widget.Destroy_Cb'Access);
      Thaw (Editor.Buttons);

      Freeze (Editor.Text);
      Delete_Text (Editor.Text);
      Thaw (Editor.Text);

      --  Read the size of the file
      F      := Open_Read (Name'Address, Text);
      Length := Positive (File_Length (F));
      Close (F);

      --  Allocate the buffer
      Buffer := new String (1 .. Length);

      declare
         type Fixed_String is new String (1 .. Positive (Length));
         package String_Direct_IO is new Ada.Direct_IO (Fixed_String);
         F : String_Direct_IO.File_Type;
      begin
         String_Direct_IO.Open (F, String_Direct_IO.In_File, File_Name);
         String_Direct_IO.Read (F, Fixed_String (Buffer.all));
         String_Direct_IO.Close (F);
      end;

      --  Create the explorer tree.

      if Display_Explorer then
         if Editor.Explorer /= null then
            Remove (Editor.Explorer_Scroll, Editor.Explorer);
         end if;

         Editor.Explorer := Explore (Editor, Buffer.all, Jump_To'Access);
         Show_All (Editor.Explorer);
         Add (Editor.Explorer_Scroll, Editor.Explorer);
      end if;

      --  Insert the contents of the buffer in the text area.

      Line_Height := Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);

      Freeze (Editor.Text);
      Freeze (Editor.Buttons);

      if Editor.Show_Line_Nums then
         Insert (Editor.Text, Editor.Font, Chars => Line_Number_String (1));
      end if;

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

               if Editor.Show_Line_Nums then
                  Insert (Editor.Text, Editor.Font,
                          Chars => Line_Number_String (Line));
               end if;

            when others =>
               if Do_Color_Highlighting then
                  if Editor.Lang /= null then
                     Looking_At (Editor.Lang, Buffer (Index .. Buffer'Last),
                                 Entity, Next_Char);
                  else
                     Next_Char := Index + 1;
                     Entity := Normal_Text;
                  end if;

                  --  Print every line separately, so that we can add line
                  --  numbers as well.

                  declare
                     J          : Positive := Index;
                     Line_Start : Positive := Index;
                  begin
                     while J < Next_Char loop
                        if Buffer (J) = ASCII.LF then
                           Insert (Editor.Text, Editor.Font,
                                   Editor.Colors (Entity),
                                   Null_Color,
                                   Buffer (Line_Start .. J));
                           Line := Line + 1;
                           if Editor.Show_Line_Nums then
                              Insert (Editor.Text, Editor.Font,
                                      Chars => Line_Number_String (Line));
                           end if;
                           Line_Start := J + 1;
                        end if;
                        J := J + 1;
                     end loop;
                     Insert (Editor.Text, Editor.Font,
                             Editor.Colors (Entity),
                             Null_Color,
                             Buffer (Line_Start .. Next_Char - 1));
                  end;
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

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor    : access Code_Editor_Record;
      Line      : Natural)
   is
      Line_Height : Gint;
      Y : Gint;
   begin
      Line_Height := Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);
      Y := Gint (Line - 1) * Line_Height + 3;

      --  Display the current line icon
      --  ??? Would be nice if it was replacing (or at least displayed above)
      --  the breakpoint icon.
      --  Note that we start by hiding everything, and then show everything
      --  at the end, so that the layout is correctly refreshed. This is not
      --  done otherwise.

      Freeze (Editor.Buttons);
      Hide_All (Editor.Buttons);

      if Get_Parent (Editor.Current_Line_Button) /= null then
         Move (Editor.Buttons, Editor.Current_Line_Button,
               X => 10, Y => Y);
      else
         Put (Editor.Buttons, Editor.Current_Line_Button,
              X => 10, Y => Y);
      end if;

      Thaw (Editor.Buttons);
      Show_All (Editor.Buttons);

      --  Scroll the code editor to make sure the line is visible on screen.

      Clamp_Page (Get_Vadj (Editor.Text),
                  Lower => Gfloat (Y - Line_Height),
                  Upper => Gfloat (Y + 4 * Line_Height));

   end Set_Line;

end Gtkada.Code_Editors;
