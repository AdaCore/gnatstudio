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
with Gdk.Event;           use Gdk.Event;
with Gdk.Types;           use Gdk.Types;
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
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Style;           use Gtk.Style;
with Gtkada.Types;        use Gtkada.Types;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtk.Arguments;       use Gtk.Arguments;
with Language;            use Language;
with Debugger;            use Debugger;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Odd.Explorer;        use Odd.Explorer;
with Odd.Menus;           use Odd.Menus;
with Odd.Process;         use Odd.Process;
with Odd_Intl;            use Odd_Intl;
with Process_Proxies;     use Process_Proxies;
with Odd.Strings;         use Odd.Strings;
with Odd.Types;           use Odd.Types;

with Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Gtk.Handlers; use Gtk.Handlers;

with Ada.Text_IO; use Ada.Text_IO;

package body Odd.Code_Editors is

   use Odd;

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

   File_Name_Bg_Color : constant String := "darkgrey";
   --  Color used for the background of the file name in the editor.

   No_Breakpoint : Breakpoint_Array (1 .. 0);
   --  Array used to reset the breakpoint list

   subtype Line_Number is String (1 .. Line_Numbers_Width);
   --  Type of strings used to display line numbers.

   package Editor_Cb is new Callback (Code_Editor_Record);
   package Editor_Event_Cb is new Gtk.Handlers.Return_Callback
     (Code_Editor_Record, Boolean);

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   procedure Jump_To
     (Widget : access Gtk_Widget_Record'Class; Position : Position_Type);
   --  Called by Explorer when an item in the buffer is selected.
   --  This will select the word starting at Index.

   function Button_Press_Cb (Editor : access Code_Editor_Record'Class;
                             Event  : Gdk.Event.Gdk_Event)
                            return Boolean;
   --  Handle button press events in the text editor.

   procedure Print_Buffer (Editor : access Code_Editor_Record'Class);
   --  Insert the contents of the buffer in the editor. Color highlighting is
   --  provided, and line numbers may or may not be added.
   --  ??? Need some caching for lines with breakpoints.

   procedure Update_Buttons (Editor : access Code_Editor_Record'Class);
   --  Update the display of line-breaks buttons.
   --  If this feature is disabled for the editor, then they are all removed.

   procedure Expand_Explorer_Tree
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args);
   --  Compute the contents of the explorer if needed. This is not done
   --  systematically, so as to speed things up.

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

      --  Set the adjustment directly, so that the text is not scrolled
      --  on the screen (which is too slow for big files)
      Set_Value
        (Get_Vadj (Editor.Text),
         Gfloat (Position.Line + 1) *
         Gfloat ((Get_Ascent (Editor.Font) + Get_Descent (Editor.Font))));
      Changed (Get_Vadj (Editor.Text));

      --  Change the cursor position, and highlight the entity.
      --  We claim the selection so that the selected entity always has the
      --  same color (if we don't, the first selection has a different color
      --  than the following ones).
      Claim_Selection (Editor.Text, True, 0);
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
      Free (Editor.Buffer);
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

      Add_Events (Editor.Text, Button_Press_Mask or Button_Release_Mask);

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
      Editor_Event_Cb.Object_Connect
        (Editor.Text, "button_press_event",
         Editor_Event_Cb.To_Marshaller (Button_Press_Cb'Access),
         Slot_Object => Editor);

      Pack_Start (Editor, Paned, Expand => True, Fill => True);
      Add1 (Paned, Editor.Explorer_Scroll);
      Pack_Start (Box, Editor.Buttons, Expand => False, Fill => False);
      Pack_Start (Box, Editor.Text, Expand => True, Fill => True);
      Pack_Start (Box, Scrollbar, Expand => False, Fill => False);
      Add2 (Paned, Box);
      Show_All (Paned);
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
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Comments_Color : String;
      Strings_Color  : String;
      Keywords_Color : String)
   is
      Current_Line_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Current_Line_Mask   : Gdk.Bitmap.Gdk_Bitmap;
      Color               : Gdk.Color.Gdk_Color;
   begin
      Editor.Font := Get_Gdkfont (Ps_Font_Name, Font_Size);

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
      Create_From_Xpm_D
        (Editor.Stop_Pixmap,
         Get_Window (Editor.Text),
         Editor.Stop_Mask,
         White (Get_System),
         Stop_Icon);

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

      Color := Parse (File_Name_Bg_Color);
      Alloc (Get_System, Color);
      Editor.File_Name_Style := Copy (Get_Style (Editor.Text));
      Set_Base (Editor.File_Name_Style, State_Normal, Color);
      Set_Base (Editor.File_Name_Style, State_Selected, Color);
      Set_Foreground (Editor.File_Name_Style, State_Active,
                      Black (Get_System));

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

   ---------------------
   -- Button_Press_Cb --
   ---------------------

   function Button_Press_Cb (Editor : access Code_Editor_Record'Class;
                             Event  : Gdk.Event.Gdk_Event)
                            return Boolean
   is
      Menu    : Gtk_Menu;
      X, Y    : Gint;
      Line    : Gint := 0;
      Index   : Integer;
      Current_Line : Gint := 1;
      Start_Index : Integer;
      Min : Gint := Gint (Guint'Min (Get_Selection_Start_Pos (Editor.Text),
                                     Get_Selection_End_Pos (Editor.Text)));
      Max : Gint := Gint (Guint'Max (Get_Selection_Start_Pos (Editor.Text),
                                     Get_Selection_End_Pos (Editor.Text)));

   begin
      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
        and then Editor.Buffer /= null
      then
         Index := Editor.Buffer'First;

         --  Take advantage of the fact that all lines and characters have the
         --  same size.
         Y := Gint (Get_Y (Event)) + Gint (Get_Value (Get_Vadj (Editor.Text)));
         Line := Y / (Get_Ascent (Editor.Font) + Get_Descent (Editor.Font))
           + 1;
         X := Gint (Get_X (Event)) / Char_Width (Editor.Font, Character'('m'));

         --  Do not count the space used for line numbers
         if Editor.Show_Line_Nums then
            X := X - Gint (Line_Numbers_Width);
         end if;

         --  Get the index of the item
         if X < 0 then
            Index := -1;
         else
            while Index <= Editor.Buffer'Last
              and then Current_Line < Line
            loop
               if Editor.Buffer (Index) = ASCII.LF then
                  Current_Line := Current_Line + 1;
               end if;
               Index := Index + 1;
            end loop;
            Index := Index + Integer (X) - Editor.Buffer'First;
         end if;

         Start_Index := Index;
         if Editor.Show_Line_Nums then
            Start_Index := Index + Integer (Line) * (Line_Numbers_Width + 1);
         end if;

         --  Only take the selection into account if it is under the cursor.
         --  Otherwise, the behavior is somewhat unexpected.

         if Get_Has_Selection (Editor.Text)
           and then Min <= Gint (Start_Index)
           and then Gint (Start_Index) <= Max
         then
            --  Keep only the first line of the selection. This avoids having
            --  too long menus, and since the debugger can not handle multiple
            --  line commands anyway is not a big problem.
            --  We do not use Editor.Buffer directly, so that we don't have to
            --  take into account the presence of line numbers.

            declare
               S : String := Get_Chars (Editor.Text, Min, Max);
            begin
               for J in S'Range loop
                  if S (J) = ASCII.LF then
                     Max := Gint (J - S'First) + Min;
                     exit;
                  end if;
               end loop;

               --  Use the selection...
               Menu := Editor_Contextual_Menu
                 (Editor, Line, Get_Chars (Editor.Text, Min, Max));
            end;
         else
            if Index < 0 then
               Menu := Editor_Contextual_Menu (Editor, Line, "");
            else
               --  Find the beginning of the entity
               Start_Index := Index;
               while Start_Index >= Editor.Buffer'First
                 and then (Is_Letter (Editor.Buffer (Start_Index))
                           or else
                           Is_Digit (Editor.Buffer (Start_Index))
                           or else
                           Editor.Buffer (Start_Index) = '_')
               loop
                  Start_Index := Start_Index - 1;
               end loop;

               --  Find the end of the entity
               while Index <= Editor.Buffer'Last
                 and then (Is_Letter (Editor.Buffer (Index))
                           or else
                           Is_Digit (Editor.Buffer (Index))
                           or else
                           Editor.Buffer (Index) = '_')
               loop
                  Index := Index + 1;
               end loop;

               Menu := Editor_Contextual_Menu
                 (Editor, Line, Editor.Buffer (Start_Index + 1 .. Index - 1));
            end if;
         end if;

         Popup (Menu,
                Button            => Gdk.Event.Get_Button (Event),
                Activate_Time     => Gdk.Event.Get_Time (Event));
         --  Stop the event so that the contextual menu is handled correctly
         --  (ie hidden when the mouse button is releases, and the selection
         --  is not unselected).
         Emit_Stop_By_Name (Editor.Text, "button_press_event");
         return True;
      end if;
      return False;
   end Button_Press_Cb;

   -----------
   -- Clear --
   -----------

   procedure Clear (Editor : access Code_Editor_Record) is
   begin
      Delete_Text (Editor.Text);
   end Clear;

   ------------------
   -- Print_Buffer --
   ------------------

   procedure Print_Buffer (Editor : access Code_Editor_Record'Class) is
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

      Index       : Positive := 1;
      Line        : Positive := 1;
      Line_Height : Gint;
      Line_Start  : Positive := 1;
      Entity      : Language_Entity;
      Next_Char   : Positive;
   begin
      if Editor.Buffer = null then
         return;
      end if;

      --  Clear the old file
      Freeze (Editor.Text);
      Delete_Text (Editor.Text);
      Thaw (Editor.Text);

      --  Insert the contents of the buffer in the text area.

      Line_Height := Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);

      Freeze (Editor.Text);
      Freeze (Editor.Buttons);

      if Editor.Show_Line_Nums then
         Insert (Editor.Text, Editor.Font, Chars => Line_Number_String (1));
      end if;

      while Index <= Editor.Buffer'Last loop

         case Editor.Buffer (Index) is

            when ASCII.CR =>  --  ignore, this is processed as ASCII.LF
               Index := Index + 1;

            when ASCII.LF =>
               if Do_Color_Highlighting then
                  Insert (Editor.Text, Editor.Font, Null_Color, Null_Color,
                          Editor.Buffer (Index .. Index));
               else
                  Insert (Editor.Text, Editor.Font, Null_Color, Null_Color,
                          Editor.Buffer (Line_Start .. Index));
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
                     Looking_At (Editor.Lang,
                                 Editor.Buffer (Index .. Editor.Buffer'Last),
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
                        if Editor.Buffer (J) = ASCII.LF then
                           Insert (Editor.Text, Editor.Font,
                                   Editor.Colors (Entity),
                                   Null_Color,
                                   Editor.Buffer (Line_Start .. J));
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
                             Editor.Buffer (Line_Start .. Next_Char - 1));
                  end;
                  Index := Next_Char;
               else
                  Index := Index + 1;
               end if;
         end case;
      end loop;

      Thaw (Editor.Text);
      Thaw (Editor.Buttons);
   end Print_Buffer;

   --------------------
   -- Update_Buttons --
   --------------------

   procedure Update_Buttons (Editor : access Code_Editor_Record'Class) is
      use Gtk.Widget.Widget_List;
      Tmp : Glist := Editor.Possible_Breakpoint_Buttons;

      Line_Height : constant Gint :=
        Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);
      Index : Positive;
      Line  : Positive := 1;
      Pix   : Gtk_Pixmap;
      Debug : Debugger_Access;
      Kind  : Line_Kind;

   begin
      if Editor.Buffer = null then
         return;
      end if;

      --  Clear the existing buttons.

      Freeze (Editor.Buttons);

      Hide_All (Editor.Buttons);

      if Get_Parent (Editor.Current_Line_Button) /= null then
         Hide (Editor.Current_Line_Button);
         Remove (Editor.Buttons, Editor.Current_Line_Button);
      end if;

      while Tmp /= Null_List loop
         Destroy (Get_Data (Tmp));
         Tmp := Next (Tmp);
      end loop;
      Free (Editor.Possible_Breakpoint_Buttons);
      Editor.Possible_Breakpoint_Buttons := Null_List;

      --  Display the breakpoint icons

      if Editor.Show_Lines_With_Code then
         Index := Editor.Buffer'First;
         Debug := Convert (Editor).Debugger;

         --  Do not emit the debugger output, and do not parse for file
         --  patterns, so as to avoid scrolling.

         Push_Internal_Command_Status (Get_Process (Debug), True);
         Set_Parse_File_Name (Get_Process (Debug), False);
         while Index <= Editor.Buffer'Last loop
            if Editor.Buffer (Index) = ASCII.LF then
               Kind := Line_Contains_Code
                 (Debug, Editor.Current_File.all, Line);

               exit when Kind = No_More_Code;

               if Kind = Have_Code then
                  Gtk_New (Pix, Editor.Default_Pixmap, Editor.Default_Mask);
                  Put (Editor.Buttons, Pix,
                       X => 0,
                       Y => Gint (Line - 1) * Line_Height + 3);
                  Prepend (Editor.Possible_Breakpoint_Buttons,
                           Gtk_Widget (Pix));
               end if;

               Line := Line + 1;
            end if;
            Index := Index + 1;
         end loop;
         Set_Parse_File_Name (Get_Process (Debug), True);
         Pop_Internal_Command_Status (Get_Process (Debug));
      end if;

      Set_Line (Editor, Editor.Current_Line);
      Show_All (Editor.Buttons);

      Thaw (Editor.Buttons);
   end Update_Buttons;

   --------------------------
   -- Expand_Explorer_Tree --
   --------------------------

   procedure Expand_Explorer_Tree
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Args   : Gtk_Args)
   is
      Editor : Code_Editor := Code_Editor (Widget);
      Node   : Gtk_Ctree_Node := Gtk_Ctree_Node (To_C_Proxy (Args, 1));
   begin
      if Node = Editor.Explorer_Root
        and then not Editor.Explorer_Is_Computed
      then
         Remove_Node (Editor.Explorer, Editor.Explorer_Dummy_Node);

         Editor.Explorer_Is_Computed := True;
         Explore
           (Editor.Explorer, Editor.Explorer_Root,
            Editor, Editor.Buffer.all, Editor.Lang,
            Base_File_Name (Editor.Current_File.all), Jump_To'Access);
         Show_All (Editor.Explorer);
         Gtk.Ctree.Expand (Editor.Explorer, Editor.Explorer_Root);
      end if;
   end Expand_Explorer_Tree;

   --------------------
   -- File_Not_Found --
   --------------------

   procedure File_Not_Found
     (Editor    : access Code_Editor_Record;
      File_Name : String)
   is
   begin
      --  Clear the old file
      Delete_Text (Editor.Text);
      if Get_Parent (Editor.Current_Line_Button) /= null then
         Remove (Editor.Buttons, Editor.Current_Line_Button);
      end if;
      Forall (Editor.Buttons, Gtk.Widget.Destroy_Cb'Access);

      --  Print a warning message
      Insert (Editor.Text, Editor.Font, Null_Color, Null_Color,
              File_Name & (-": File not found"));
   end File_Not_Found;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor    : access Code_Editor_Record;
      File_Name : String)
   is
      F           : File_Descriptor;
      Length      : Positive;
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
         Free (Editor.Buffer);
         Editor.Current_File := new String' (File_Name);
      end if;

      --  Read the size of the file
      F      := Open_Read (Name'Address, Text);

      if F = Invalid_FD then
         File_Not_Found (Editor, File_Name);
         return;
      else
         Length := Positive (File_Length (F));

         --  Allocate the buffer
         Editor.Buffer := new String (1 .. Length);
         Length := Read (F, Editor.Buffer (1)'Address, Length);
         Close (F);
      end if;

      --  Create the explorer tree.

      if Display_Explorer then
         if Editor.Explorer /= null then
            Remove (Editor.Explorer_Scroll, Editor.Explorer);
         end if;

         Gtk_New (Editor.Explorer, 1);

         Editor.Explorer_Root := Insert_Node
           (Editor.Explorer, null, null,
            Null_Array + Base_File_Name (File_Name),
            5, Null_Pixmap, Null_Bitmap,
            Null_Pixmap, Null_Bitmap, False, False);
         Editor.Explorer_Is_Computed := False;

         --  Insert a dummy node so that the ctree displays a [+] button on
         --  the left side.
         Editor.Explorer_Dummy_Node := Insert_Node
           (Editor.Explorer, Editor.Explorer_Root, null,
            Null_Array + "",
            5, Null_Pixmap, Null_Bitmap, null, null, True, False);

         Widget_Callback.Object_Connect
           (Editor.Explorer, "tree_expand",
            Expand_Explorer_Tree'Access, Editor);

         Set_Cell_Style (Editor.Explorer, 0, 0, Editor.File_Name_Style);
         Set_Column_Auto_Resize (Editor.Explorer, 0, True);

         Show_All (Editor.Explorer);
         Add (Editor.Explorer_Scroll, Editor.Explorer);
      end if;

      Print_Buffer (Editor);
      Update_Buttons (Editor);
      Update_Breakpoints (Editor, No_Breakpoint);

      --  For the buttons to become visible again, we have to hide the layout,
      --  and then show it again... Don't know why !

      Hide (Editor.Buttons);
      Show_All (Editor.Buttons);

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
      Show (Editor.Buttons);

      Thaw (Editor.Buttons);
      Show_All (Editor.Buttons);

      --  Scroll the code editor to make sure the line is visible on screen.

      Clamp_Page (Get_Vadj (Editor.Text),
                  Lower => Gfloat (Y - Line_Height),
                  Upper => Gfloat (Y + 4 * Line_Height));

      Editor.Current_Line := Line;

      --  Make sure the arrow that indicated the previous line is no longer
      --  visible.
      Queue_Draw (Editor.Buttons);
   end Set_Line;

   ------------------------
   -- Set_Show_Line_Nums --
   ------------------------

   procedure Set_Show_Line_Nums (Editor : access Code_Editor_Record;
                                 Show   : Boolean := False)
   is
      --  Save the currently displayed line
      Value : constant Gfloat := Get_Value (Get_Vadj (Editor.Text));
   begin
      if Show /= Editor.Show_Line_Nums then
         Editor.Show_Line_Nums := Show;
         Print_Buffer (Editor);
         Set_Value (Get_Vadj (Editor.Text), Value);
      end if;
   end Set_Show_Line_Nums;

   ------------------------
   -- Get_Show_Line_Nums --
   ------------------------

   function Get_Show_Line_Nums (Editor : access Code_Editor_Record)
                               return Boolean
   is
   begin
      return Editor.Show_Line_Nums;
   end Get_Show_Line_Nums;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File (Editor : access Code_Editor_Record)
                             return String
   is
   begin
      if Editor.Current_File = null then
         return "";
      else
         return Editor.Current_File.all;
      end if;
   end Get_Current_File;

   ------------------------------
   -- Set_Show_Lines_With_Code --
   ------------------------------

   procedure Set_Show_Lines_With_Code (Editor : access Code_Editor_Record;
                                       Show   : Boolean)
   is
   begin
      if Show /= Editor.Show_Lines_With_Code then
         Editor.Show_Lines_With_Code := Show;
         Update_Buttons (Editor);
      end if;
   end Set_Show_Lines_With_Code;

   ------------------------------
   -- Get_Show_Lines_With_Code --
   ------------------------------

   function Get_Show_Lines_With_Code (Editor : access Code_Editor_Record)
                                     return Boolean
   is
   begin
      return Editor.Show_Lines_With_Code;
   end Get_Show_Lines_With_Code;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor    : access Code_Editor_Record;
      Br        : Odd.Types.Breakpoint_Array)
   is
      use Gtk.Widget.Widget_List;
      Tmp : Glist := Editor.Breakpoint_Buttons;
      Pix : Gtk_Pixmap;
      Line_Height : constant Gint :=
        Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);

   begin
      if Editor.Current_File = null then
         return;
      end if;

      declare
         Base_File : String := Base_File_Name (Editor.Current_File.all);
      begin
         Hide_All (Editor.Buttons);
         Freeze (Editor.Buttons);

         --  Remove all existing breakpoints

         while Tmp /= Null_List loop
            Destroy (Get_Data (Tmp));
            Tmp := Next (Tmp);
         end loop;

         Free (Editor.Breakpoint_Buttons);
         Editor.Breakpoint_Buttons := Null_List;

         --  Add the new ones
         for B in Br'Range loop
            if Br (B).File /= null
              and then Br (B).File.all = Base_File
            then
               Gtk_New (Pix, Editor.Stop_Pixmap, Editor.Stop_Mask);
               Put (Editor.Buttons, Pix,
                    X => 0,
                    Y => Gint (Br (B).Line - 1) * Line_Height + 3);
               Prepend (Editor.Breakpoint_Buttons, Gtk_Widget (Pix));
            end if;
         end loop;

         Show_All (Editor.Buttons);
         Thaw (Editor.Buttons);
      end;
   end Update_Breakpoints;

end Odd.Code_Editors;
