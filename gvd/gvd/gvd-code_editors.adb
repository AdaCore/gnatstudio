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
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Extra.PsFont;    use Gtk.Extra.PsFont;
with Gtk.Layout;          use Gtk.Layout;
with Gtk.Paned;           use Gtk.Paned;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Scrollbar;       use Gtk.Scrollbar;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text;            use Gtk.Text;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Main;            use Gtk.Main;
with Gtk.Menu;            use Gtk.Menu;
with Gtkada.Types;        use Gtkada.Types;
with Language;            use Language;
with Debugger;            use Debugger;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Menu_Item;       use Gtk.Menu_Item;

with Odd.Explorer;          use Odd.Explorer;
with Odd.Menus;             use Odd.Menus;
with Odd.Process;           use Odd.Process;
with Odd_Intl;              use Odd_Intl;
with Process_Proxies;       use Process_Proxies;
with Odd.Strings;           use Odd.Strings;
with Odd.Types;             use Odd.Types;

with Unchecked_Deallocation;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Main_Debug_Window_Pkg;   use Main_Debug_Window_Pkg;

with Gtk.Handlers; use Gtk.Handlers;

with Ada.Text_IO; use Ada.Text_IO;

package body Odd.Code_Editors is

   use Odd;

   ---------------------
   -- Local constants --
   ---------------------

   Do_Color_Highlighting : constant Boolean := True;
   --  Indicate whether the editor should provide color highlighting.

   Display_Explorer : constant Boolean := True;
   --  True if we should associate an explorer tree to each editor.

   Explorer_Width : constant := 200;
   --  Width for the area reserved for the explorer.

   Layout_Width : constant := 20;
   --  Width for the area reserved for the buttons.

   Line_Numbers_Width : constant Positive := 5;
   --  Number of characters reserved on the left for line numbers.

   No_Breakpoint : Breakpoint_Array (1 .. 0);
   --  Array used to reset the breakpoint list

   subtype Line_Number is String (1 .. Line_Numbers_Width);
   --  Type of strings used to display line numbers.

   Editor_Contextual_Menu_Name : constant String := "odd_editor_context";
   --  String used to store the editor contextual menu as a user data

   --------------------
   -- Local packages --
   --------------------

   type Breakpoint_Record (File_Length : Natural) is record
      Process : Debugger_Process_Tab;
      File    : String (1 .. File_Length);
      Line    : Integer;
   end record;

   type Variable_Record (Name_Length : Natural) is record
      Process      : Debugger_Process_Tab;
      Name         : String (1 .. Name_Length);
      Auto_Refresh : Boolean;
   end record;

   package Editor_Cb is new Callback (Code_Editor_Record);
   package Editor_Event_Cb is new Gtk.Handlers.Return_Callback
     (Code_Editor_Record, Boolean);
   package Check_Editor_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, Odd.Code_Editors.Code_Editor);
   package Widget_Breakpoint_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Breakpoint_Record);
   package Widget_Variable_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Variable_Record);
   package Editor_Idle is new Gtk.Main.Idle (Code_Editor);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Free is new Unchecked_Deallocation (String, String_Access);

   function Button_Press_Cb
     (Editor : access Code_Editor_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle button press events in the text editor.

   function Pixmap_Clicked_Cb
     (Editor : access Code_Editor_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle button press events in the buttons layout.

   procedure Print_Buffer (Editor : access Code_Editor_Record'Class);
   --  Insert the contents of the buffer in the editor. Color highlighting is
   --  provided, and line numbers may or may not be added.
   --  ??? Need some caching for lines with breakpoints.

   procedure Update_Buttons
     (Editor : access Code_Editor_Record'Class;
      Reset_Line : Boolean := True);
   --  Update the display of line-breaks buttons.
   --  If this feature is disabled for the editor, then they are all removed.
   --  If Reset_Line is True, then the editor is scrolled so as to show the
   --  the current line.

   procedure Scroll_Layout (Editor : access Code_Editor_Record'Class);
   --  Synchronize the new position of the buttons layout after the user has
   --  scrolled the editor.

   procedure Scroll_Layout_Changed
     (Editor : access Code_Editor_Record'Class);
   --  Synchronize the new values of the buttons layout after the user has
   --  scrolled the editor. This procedure is mainly called on resize events.

   procedure Destroy_Cb (Editor : access Code_Editor_Record'Class);
   --  Free the memory occupied by the editor and the buttons layout, as well
   --  as all the associated pixmaps.

   function Editor_Contextual_Menu
     (Editor   : access Odd.Code_Editors.Code_Editor_Record'Class;
      Line     : Glib.Gint;
      Entity   : String)
     return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when a specific
   --  entity is selected in the code editor.

   procedure Set_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record);
   --  Set a breakpoint on a specific line.

   procedure Till_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record);
   --  Set a temporary breakpoint on a line, and continue execution.

   procedure Change_Line_Nums
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor);
   --  Callback for the "show line numbers" contextual menu item.

   procedure Print_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Var    : Variable_Record);
   --  Callback for the "print variable" or "display variable" contextual menu
   --  items.

   procedure Change_Lines_With_Code
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor);
   --  Callback for the "show lines with code" contextual menu item.

   procedure Show_Current_Line_Menu (Editor : access Code_Editor_Record'Class);
   --  Display the current file and current line in the editor.

   procedure Is_Breakpoint
     (Editor : access Code_Editor_Record'Class;
      Line   : Integer;
      Result : out Boolean;
      Num    : out Integer);
   --  Tell if a breakpoint is set at a specific line.
   --  If it is the case, return the number of the breakpoint.

   function Idle_Compute_Lines (Editor : Code_Editor) return Boolean;
   --  Idle function called to compute the lines with code in the editor

   function Check_Single_Line
     (Editor     : access Code_Editor_Record'Class;
      Line       : Natural)
     return Boolean;
   --  Check whether Line contains executable code, and put an icon for it
   --  in the button toolbar if needed.
   --  Returns False if no line after Line contains code.

   -------------------
   -- Is_Breakpoint --
   -------------------

   procedure Is_Breakpoint
     (Editor : access Code_Editor_Record'Class;
      Line   : Integer;
      Result : out Boolean;
      Num    : out Integer)
   is
      Breakpoints_Array : Odd.Types.Breakpoint_Array_Ptr :=
        Convert (Editor).Breakpoints;
   begin
      for Index in Breakpoints_Array'Range loop
         if Breakpoints_Array (Index).Line = Line
           and then Breakpoints_Array (Index).File.all =
             Base_File_Name (Editor.Current_File.all)
         then
            Num := Breakpoints_Array (Index).Num;
            Result := True;
            return;
         end if;
      end loop;

      Result := False;
   end Is_Breakpoint;

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
      Process     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Homogeneous : Boolean := False;
      Spacing     : Glib.Gint := 0) is
   begin
      Editor := new Code_Editor_Record;
      Initialize (Editor, Process, Homogeneous, Spacing);
   end Gtk_New_Hbox;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor      : access Code_Editor_Record'Class;
      Process     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Homogeneous : in Boolean := False;
      Spacing     : in Gint := 0)
   is
      Scrollbar : Gtk_Vscrollbar;
      Box       : Gtk_Hbox;
      Paned     : Gtk_Hpaned;

   begin
      Gtk.Box.Initialize_Hbox (Editor, Homogeneous => False);
      Editor.Process := Gtk_Widget (Process);

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
      Set_USize (Editor.Buttons, Layout_Width, 0);
      Add_Events (Editor.Buttons, Button_Press_Mask or Button_Release_Mask);

      Gtk_New (Editor.Explorer_Scroll);
      Set_Policy (Editor.Explorer_Scroll, Policy_Automatic, Policy_Automatic);
      Set_USize (Editor.Explorer_Scroll, Explorer_Width, -1);

      Gtk_New (Editor.Explorer, Editor);
      Add (Editor.Explorer_Scroll, Editor.Explorer);

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
      Editor_Event_Cb.Object_Connect
        (Editor.Buttons, "button_press_event",
         Editor_Event_Cb.To_Marshaller (Pixmap_Clicked_Cb'Access),
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

      --  ???Unfortunately, it is not possible currently to specify the
      --  step_increment for the adjustments, since this is overriden in
      --  several places in the text widget.
      --    Set_Step_Increment
      --     (Get_Vadj (Editor.Text),
      --      Gfloat (Get_Ascent (Editor.Font) + Get_Descent (Editor.Font)));

      Editor.Line_Height :=
        Get_Ascent (Editor.Font) + Get_Descent (Editor.Font);
   end Configure;

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language
     (Editor : access Code_Editor_Record;
      Lang   : Language.Language_Access) is
   begin
      Free (Editor.Lang);
      if Lang /= null then
         Editor.Lang := new Language_Root'Class'(Lang.all);
      end if;
   end Set_Current_Language;

   -----------------------
   -- Pixmap_Clicked_Cb --
   -----------------------

   function Pixmap_Clicked_Cb
     (Editor : access Code_Editor_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      Num     : Integer;
      Result  : Boolean;
      Line    : Gint := 0;

   begin
      case Get_Button (Event) is
         when 1 | 3 =>
            if Get_Event_Type (Event) = Button_Press
              and then Editor.Buffer /= null
            then
               Line :=
                 (Gint (Get_Y (Event)) +
                   Gint (Get_Value (Get_Vadj (Editor.Text)))) /
                 (Get_Ascent (Editor.Font) + Get_Descent (Editor.Font)) + 1;

               case Get_Button (Event) is
                  when 3 =>
                     --
                     --  ??? Should figure what to put here later
                     --

                     return True;

                  when 1 =>
                     Is_Breakpoint (Editor, Integer (Line), Result, Num);

                     if Result then
                        Remove_Breakpoint
                          (Convert (Editor).Debugger, Num, Display => True);
                     else
                        Break_Source (Convert (Editor).Debugger,
                                      Editor.Current_File.all,
                                      Integer (Line),
                                      Display => True);
                     end if;

                     return True;

                  when others =>
                     return False;
               end case;
            end if;

            return False;

         when 4 =>
            Set_Value
              (Get_Vadj (Editor.Text),
               Get_Value (Get_Vadj (Editor.Text)) -
                 Get_Page_Increment (Get_Vadj (Editor.Text)));
            return True;

         when  5 =>
            Set_Value
              (Get_Vadj (Editor.Text),
               Get_Value (Get_Vadj (Editor.Text)) +
                 Get_Page_Increment (Get_Vadj (Editor.Text)));
            return True;

         when others => return False;
      end case;
   end Pixmap_Clicked_Cb;

   ---------------------
   -- Button_Press_Cb --
   ---------------------

   function Button_Press_Cb
     (Editor : access Code_Editor_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
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
      case Get_Button (Event) is
         when 3 =>
            if Get_Event_Type (Event) = Button_Press
              and then Editor.Buffer /= null
            then
               Index := Editor.Buffer'First;

               --  Take advantage of the fact that all lines and characters
               --  have the same size.
               Y := Gint (Get_Y (Event))
                 + Gint (Get_Value (Get_Vadj (Editor.Text)));
               Line := Y / (Get_Ascent (Editor.Font)
                            + Get_Descent (Editor.Font))
                 + 1;
               X := Gint (Get_X (Event))
                 / Char_Width (Editor.Font, Character'('m'));

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
                  Start_Index := Index
                    + Integer (Line) * (Line_Numbers_Width + 1);
               end if;

               --  Only take the selection into account if it is under
               --  the cursor.
               --  Otherwise, the behavior is somewhat unexpected.

               if Get_Has_Selection (Editor.Text)
                 and then Min <= Gint (Start_Index)
                 and then Gint (Start_Index) <= Max
               then
                  --  Keep only the first line of the selection.
                  --  This avoids having too long menus, and since the debugger
                  --  can not handle multiple line commands anyway is not a big
                  --  problem.
                  --  We do not use Editor.Buffer directly, so that we
                  --  don't have to take into account the presence of line
                  --  numbers.

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
                       (Editor, Line, Editor.Buffer (Start_Index
                                                     + 1 .. Index - 1));
                  end if;
               end if;

               Popup (Menu,
                   Button            => Gdk.Event.Get_Button (Event),
                      Activate_Time     => Gdk.Event.Get_Time (Event));
               --  Stop the event so that the contextual menu is handled
               --  correctly (ie hidden when the mouse button is
               --  released, and the selection is not unselected).

               Emit_Stop_By_Name (Editor.Text, "button_press_event");

               return True;
            end if;
            return False;

         when 4 =>
            Set_Value (Get_Vadj (Editor.Text),
                             Get_Value (Get_Vadj
                                        (Editor.Text))
                       - Get_Page_Increment
                       (Get_Vadj (Editor.Text)));
            return True;
         when  5 =>
            Set_Value (Get_Vadj (Editor.Text),
                       Get_Value (Get_Vadj
                                  (Editor.Text))
                       + Get_Page_Increment
                       (Get_Vadj (Editor.Text)));
            return True;
         when others => return False;
      end case;

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

   procedure Update_Buttons
     (Editor     : access Code_Editor_Record'Class;
      Reset_Line : Boolean := True)
   is
      use Gtk.Widget.Widget_List;
      Tmp         : Glist;
      Pix         : Gtk_Pixmap;
      Index       : Positive;
      Num_Lines   : Natural := 0;
      Value       : Gfloat;
   begin
      if Editor.Buffer = null then
         return;
      end if;

      --  Clear the existing buttons.

      Freeze (Editor.Buttons);

      if Get_Parent (Editor.Current_Line_Button) /= null then
         Hide (Editor.Current_Line_Button);
         Remove (Editor.Buttons, Editor.Current_Line_Button);
      end if;

      --  Remove all existing buttons
      Tmp := Children (Editor.Buttons);
      while Tmp /= Null_List loop
         Remove (Editor.Buttons, Get_Data (Tmp));
         Tmp := Next (Tmp);
      end loop;

      --  Display the breakpoint icons

      Editor.Current_File_Cache := Find_In_Cache
        (Convert (Editor).Window, Editor.Current_File.all);
      if Editor.Idle_Id /= 0 then
         Idle_Remove (Editor.Idle_Id);
      end if;
      if Editor.Show_Lines_With_Code then
         Editor.Idle_Id := Editor_Idle.Add
           (Idle_Compute_Lines'Access, Code_Editor (Editor));

         --  Show the breakpoints we already know about
         if Editor.Current_File_Cache.Line_Has_Code /= null then
            for Line in Editor.Current_File_Cache.Line_Has_Code'Range loop
               if Editor.Current_File_Cache.Line_Has_Code (Line) then
                  Gtk_New (Pix, Editor.Default_Pixmap, Editor.Default_Mask);
                  Put (Editor.Buttons, Pix,
                       X => 0,
                       Y => Gint (Line - 1) * Editor.Line_Height + 3);
               end if;
            end loop;
         end if;

         --  Allocate the arrays if required
         if Editor.Current_File_Cache.Current_Line = 0
           and then Editor.Current_File_Cache.Line_Has_Code = null
         then
            Index := Editor.Buffer'First;
            while Index <= Editor.Buffer'Last loop
               if Editor.Buffer (Index) = ASCII.LF then
                  Num_Lines := Num_Lines + 1;
               end if;
               Index := Index + 1;
            end loop;

            Editor.Current_File_Cache.Line_Has_Code :=
              new Packed_Boolean_Array (1 .. Num_Lines);
            Editor.Current_File_Cache.Line_Has_Code.all := (others => False);
            Editor.Current_File_Cache.Line_Parsed :=
              new Packed_Boolean_Array (1 .. Num_Lines);
            Editor.Current_File_Cache.Line_Parsed.all := (others => False);
         end if;
      end if;

      Value := Get_Value (Get_Vadj (Editor.Text));
      Set_Line (Editor, Editor.Current_Line, Set_Current => False);

      if not Reset_Line then
         Set_Value (Get_Vadj (Editor.Text), Value);
         Value_Changed (Get_Vadj (Editor.Text));
      end if;

      Show_All (Editor.Buttons);
      Thaw (Editor.Buttons);
   end Update_Buttons;

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
     (Editor      : access Code_Editor_Record;
      File_Name   : String;
      Set_Current : Boolean := True)
   is
      F      : File_Descriptor;
      Length : Positive;
      Name   : aliased String := File_Name & ASCII.NUL;
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
         Editor.Current_File := new String'(File_Name);
      end if;

      --  Read the size of the file
      F      := Open_Read (Name'Address, Text);

      if F = Invalid_FD then
         File_Not_Found (Editor, File_Name);
         return;
      else
         Length := Positive (File_Length (F));

         --  Allocate the buffer
         --  and strip the ^Ms from the string
         declare
            S : String (1 .. Length);
         begin
            Length := Read (F, S'Address, Length);
            Editor.Buffer := new String'(Strip_Control_M (S));
         end;

         Close (F);
      end if;

      --  Create the explorer tree.

      if Set_Current then
         Set_Current_File (Editor.Explorer, File_Name);
      end if;

      Print_Buffer (Editor);
      Update_Buttons (Editor, True);
      if Debugger_Process_Tab (Editor.Process).Breakpoints /= null then
         Update_Breakpoints
           (Editor, Debugger_Process_Tab (Editor.Process).Breakpoints.all);
      else
         Update_Breakpoints (Editor, No_Breakpoint);
      end if;

      if not Display_Explorer then
         Hide (Editor.Explorer_Scroll);
      end if;

   exception

      --  File not found
      when Name_Error =>
         Put_Line (Standard_Error, "WARNING: File not found: " & File_Name);
   end Load_File;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Editor      : access Code_Editor_Record;
      Line        : Natural;
      Set_Current : Boolean := True)
   is
      Y : Gint;
   begin
      Y := Gint (Line - 1) * Editor.Line_Height + 3;

      --  Display the current line icon
      --  Note that we start by hiding everything, and then show everything
      --  at the end, so that the layout is correctly refreshed. This is not
      --  done otherwise.

      Freeze (Editor.Buttons);
      Hide_All (Editor.Buttons);

      if Set_Current then
         if Get_Parent (Editor.Current_Line_Button) /= null then
            Move (Editor.Buttons, Editor.Current_Line_Button,
                  X => 10, Y => Y);
         else
            Put (Editor.Buttons, Editor.Current_Line_Button,
                 X => 10, Y => Y);
         end if;
      else
         if Get_Parent (Editor.Current_Line_Button) /= null then
            Remove (Editor.Buttons, Editor.Current_Line_Button);
         end if;
      end if;
      Show (Editor.Buttons);

      Thaw (Editor.Buttons);
      Show_All (Editor.Buttons);

      --  Scroll the code editor to make sure the line is visible on screen.

      Clamp_Page (Get_Vadj (Editor.Text),
                  Lower => Gfloat (Y - Editor.Line_Height),
                  Upper => Gfloat (Y + 4 * Editor.Line_Height));

      Editor.Current_Line := Line;

      if Set_Current then
         Set_Current_Line (Editor.Explorer, Line);
      end if;

      --  Make sure the arrow that indicated the previous line is no longer
      --  visible.
      Queue_Draw (Editor.Buttons);
   end Set_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Editor : access Code_Editor_Record) return Natural is
   begin
      return Editor.Current_Line;
   end Get_Line;

   ------------------------
   -- Set_Show_Line_Nums --
   ------------------------

   procedure Set_Show_Line_Nums
     (Editor : access Code_Editor_Record;
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

   function Get_Show_Line_Nums
     (Editor : access Code_Editor_Record) return Boolean is
   begin
      return Editor.Show_Line_Nums;
   end Get_Show_Line_Nums;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Code_Editor_Record) return String is
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

   procedure Set_Show_Lines_With_Code
     (Editor : access Code_Editor_Record; Show : Boolean) is
   begin
      if Show /= Editor.Show_Lines_With_Code then
         Editor.Show_Lines_With_Code := Show;
         Update_Buttons (Editor, False);
      end if;
   end Set_Show_Lines_With_Code;

   ------------------------------
   -- Get_Show_Lines_With_Code --
   ------------------------------

   function Get_Show_Lines_With_Code
     (Editor : access Code_Editor_Record) return Boolean is
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
                    Y => Gint (Br (B).Line - 1) * Editor.Line_Height + 3);
               Prepend (Editor.Breakpoint_Buttons, Gtk_Widget (Pix));
            end if;
         end loop;

         Show_All (Editor.Buttons);
         Thaw (Editor.Buttons);
      end;
   end Update_Breakpoints;

   --------------------
   -- Highlight_Word --
   --------------------

   procedure Highlight_Word
     (Editor   : access Code_Editor_Record;
      Position : Odd.Explorer.Position_Type)
   is
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
   end Highlight_Word;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Editor : access Code_Editor_Record'Class) return Gtk.Widget.Gtk_Widget is
   begin
      return Editor.Process;
   end Get_Process;

   ----------------------------
   -- Editor_Contextual_Menu --
   ----------------------------

   function Editor_Contextual_Menu
     (Editor   : access Odd.Code_Editors.Code_Editor_Record'Class;
      Line     : Glib.Gint;
      Entity   : String)
     return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
      Check : Gtk_Check_Menu_Item;
   begin
      --  Destroy the previous menu (which we couldn't do earlier because
      --  of the call to popup. We will change every item anyway.

      begin
         Menu := Menu_User_Data.Get (Editor, Editor_Contextual_Menu_Name);
         Destroy (Menu);
      exception
         when Gtkada.Types.Data_Error => null;
      end;

      --  Create a new menu

      Gtk_New (Menu);

      Gtk_New (Mitem, Label => -"Print " & Entity);
      Append (Menu, Mitem);
      Widget_Variable_Handler.Connect
        (Mitem, "activate",
         Widget_Variable_Handler.To_Marshaller (Print_Variable'Access),
         Variable_Record'(Name_Length  => Entity'Length,
                          Name         => Entity,
                          Auto_Refresh => False,
                          Process      => Convert (Editor)));
      if Entity'Length = 0 then
         Set_State (Mitem, State_Insensitive);
      end if;

      Gtk_New (Mitem, Label => -"Display " & Entity);
      Append (Menu, Mitem);
      Widget_Variable_Handler.Connect
        (Mitem, "activate",
         Widget_Variable_Handler.To_Marshaller (Print_Variable'Access),
         Variable_Record'(Name_Length  => Entity'Length,
                          Name         => Entity,
                          Auto_Refresh => True,
                          Process      => Convert (Editor)));
      if Entity'Length = 0 then
         Set_State (Mitem, State_Insensitive);
      end if;

      --  Display a separator

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      --  Line specific items

      Gtk_New (Mitem, Label => -"Set Breakpoint on Line" & Gint'Image (Line));
      Append (Menu, Mitem);
      Widget_Breakpoint_Handler.Connect
        (Mitem, "activate",
         Widget_Breakpoint_Handler.To_Marshaller (Set_Breakpoint'Access),
         Breakpoint_Record'(File_Length  => Get_Current_File (Editor)'Length,
                            Process      => Convert (Editor),
                            File         => Get_Current_File (Editor),
                            Line         => Integer (Line)));

      Gtk_New (Mitem, Label => -"Continue Until Line" & Gint'Image (Line));
      Append (Menu, Mitem);
      Widget_Breakpoint_Handler.Connect
        (Mitem, "activate",
         Widget_Breakpoint_Handler.To_Marshaller (Till_Breakpoint'Access),
         Breakpoint_Record'(File_Length  => Get_Current_File (Editor)'Length,
                            Process      => Convert (Editor),
                            File         => Get_Current_File (Editor),
                            Line         => Integer (Line)));

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show Current Location");
      Append (Menu, Mitem);
      Editor_Cb.Object_Connect
        (Mitem, "activate",
         Editor_Cb.To_Marshaller (Show_Current_Line_Menu'Access),
         Editor);
      Set_Sensitive (Mitem, Get_Current_File (Editor.Explorer) /= "");

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      --  Editor specific items

      Gtk_New (Check, Label => -"Display Line Numbers");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Show_Line_Nums (Editor));
      Append (Menu, Check);
      Check_Editor_Handler.Connect
        (Check, "activate",
         Check_Editor_Handler.To_Marshaller (Change_Line_Nums'Access),
         Code_Editor (Editor));

      Gtk_New (Check, Label => -"Show lines with code");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Show_Lines_With_Code (Editor));
      Append (Menu, Check);
      Check_Editor_Handler.Connect
        (Check, "activate",
         Check_Editor_Handler.To_Marshaller (Change_Lines_With_Code'Access),
         Code_Editor (Editor));

      Show_All (Menu);
      Menu_User_Data.Set (Editor, Menu, Editor_Contextual_Menu_Name);
      return Menu;
   end Editor_Contextual_Menu;

   ----------------------------
   -- Change_Lines_With_Code --
   ----------------------------

   procedure Change_Lines_With_Code
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor) is
   begin
      Set_Show_Lines_With_Code (Editor, Get_Active (Item));
   end Change_Lines_With_Code;

   --------------------
   -- Set_Breakpoint --
   --------------------

   procedure Set_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record) is
   begin
      Break_Source (Br.Process.Debugger, Br.File, Br.Line, Display => True);
   end Set_Breakpoint;

   ---------------------
   -- Till_Breakpoint --
   ---------------------

   procedure Till_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record) is
   begin
      Break_Source (Br.Process.Debugger, Br.File, Br.Line, Temporary => True);
      Continue (Br.Process.Debugger, Display => True);
   end Till_Breakpoint;

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Var    : Variable_Record)
   is
      pragma Warnings (Off, Widget);
   begin
      if Var.Auto_Refresh then
         Process_User_Command
           (Var.Process, "graph display " & Var.Name,
            Output_Command => True);
      else
         Process_User_Command
           (Var.Process, "graph print " & Var.Name,
            Output_Command => True);
      end if;
   end Print_Variable;

   ----------------------
   -- Change_Line_Nums --
   ----------------------

   procedure Change_Line_Nums
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Code_Editor) is
   begin
      Set_Show_Line_Nums (Editor, Get_Active (Item));
   end Change_Line_Nums;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu (Editor : access Code_Editor_Record'Class)
   is
      Name : constant String := Get_Current_File (Editor.Explorer);
      Lang : Language_Access;
   begin
      if Name /= "" then
         Lang := Get_Language_From_File (Name);
         Set_Current_Language (Editor, Lang);
         Load_File (Editor,
                    Find_File
                    (Debugger_Process_Tab (Editor.Process).Debugger, Name),
                    Set_Current => False);
         Set_Line (Editor, Get_Current_Line (Editor.Explorer),
                   Set_Current => False);
      end if;
   end Show_Current_Line_Menu;

   ------------------------
   -- Idle_Compute_Lines --
   ------------------------

   function Idle_Compute_Lines (Editor : Code_Editor) return Boolean
   is
      Process  : Debugger_Process_Tab := Convert (Editor);
      Debug    : Debugger_Access := Process.Debugger;
      Line     : Integer;
      Line_Max : Integer;
      Found    : Boolean := False;
   begin
      --  If we already reached the end, cancel the Idle loop
      if Editor.Current_File_Cache.Line_Parsed = null then
         Editor.Idle_Id := 0;
         return False;
      end if;

      if Command_In_Process (Get_Process (Debug)) then
         return True;
      end if;

      --  Priority is given to computing the visible lines on the screen.

      Line := Integer (Get_Value (Get_Vadj (Editor.Text)))
        / Integer (Editor.Line_Height);
      if Line <= Editor.Current_File_Cache.Line_Parsed'First then
         Line := Editor.Current_File_Cache.Line_Parsed'First;
      end if;

      Line_Max := Line + Integer (Get_Allocation_Height (Editor.Text))
        / Integer (Editor.Line_Height);
      while Line <= Line_Max loop
         if Line > Editor.Current_File_Cache.Line_Parsed'Last then
            exit;
         end if;

         if not Editor.Current_File_Cache.Line_Parsed (Line) then
            Found := True;
            exit;
         end if;

         Line := Line + 1;
      end loop;

      --  Else find the first line we did not parse
      if not Found then
         loop
            Editor.Current_File_Cache.Current_Line :=
              Editor.Current_File_Cache.Current_Line + 1;
            if Editor.Current_File_Cache.Current_Line >
              Editor.Current_File_Cache.Line_Has_Code'Last
            then
               Free (Editor.Current_File_Cache.Line_Parsed);
               Editor.Idle_Id := 0;
               return False;
            end if;

            exit when not Editor.Current_File_Cache.Line_Parsed
              (Editor.Current_File_Cache.Current_Line);
         end loop;
         Line := Editor.Current_File_Cache.Current_Line;
      end if;

      --  Check whether the line contains some code

      if not Check_Single_Line (Editor, Line)
        and then Line = Editor.Current_File_Cache.Current_Line
      then
         Free (Editor.Current_File_Cache.Line_Parsed);
         Editor.Idle_Id := 0;
         return False;
      end if;
      return True;
   end Idle_Compute_Lines;

   -----------------------
   -- Check_Single_Line --
   -----------------------

   function Check_Single_Line
     (Editor     : access Code_Editor_Record'Class;
      Line       : Natural)
     return Boolean
   is
      Kind        : Line_Kind;
      Pix         : Gtk_Pixmap;
      Process : Debugger_Process_Tab := Convert (Editor);
      Debug   : Debugger_Access := Process.Debugger;
   begin
      Push_Internal_Command_Status (Get_Process (Debug), True);
      Set_Parse_File_Name (Get_Process (Debug), False);

      --  Check whether the line contains code

      Kind := Line_Contains_Code (Debug, Editor.Current_File.all, Line);
      Editor.Current_File_Cache.Line_Parsed (Line) := True;

      Set_Parse_File_Name (Get_Process (Debug), True);
      Pop_Internal_Command_Status (Get_Process (Debug));

      --  Deactivate the idle callback if we have finished
      if Kind = No_More_Code then
         return False;
      end if;

      if Kind = Have_Code then
         Freeze (Editor.Buttons);
         Hide_All (Editor.Buttons);

         Editor.Current_File_Cache.Line_Has_Code (Line) := True;
         Gtk_New (Pix, Editor.Default_Pixmap, Editor.Default_Mask);
         Put
           (Editor.Buttons, Pix,
            X => 0,
            Y => Gint (Line - 1) * Editor.Line_Height + 3);

         Show_All (Editor.Buttons);
         Thaw (Editor.Buttons);
      end if;

      return True;
   end Check_Single_Line;

end Odd.Code_Editors;
