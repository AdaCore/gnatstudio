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
with Gdk.Color;           use Gdk.Color;
with Gdk.Event;           use Gdk.Event;
with Gdk.Types;           use Gdk.Types;
with Gdk.Window;          use Gdk.Window;
with Gtk.Box;
with Gtk.Pixmap;          use Gtk.Pixmap;
with Gtk.Scrollbar;       use Gtk.Scrollbar;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Layout;          use Gtk.Layout;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Text;            use Gtk.Text;
with Gdk.Pixmap;          use Gdk.Pixmap;
with Gdk.Bitmap;          use Gdk.Bitmap;
with Gdk.Font;            use Gdk.Font;
with Gtk.Extra.PsFont;    use Gtk.Extra.PsFont;
with Gtk.Adjustment;      use Gtk.Adjustment;
with Gtk.Widget;          use Gtk.Widget;
with Odd.Types;           use Odd.Types;
with Odd.Preferences;     use Odd.Preferences;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Odd.Text_Boxes is

   Layout_Width : constant := 20;
   --  Width for the area reserved for the buttons.

   package Box_Cb is new Callback (Odd_Text_Box_Record);
   package Box_Event_Cb is new Return_Callback (Odd_Text_Box_Record, Boolean);

   procedure Scroll_Layout (Box : access Odd_Text_Box_Record'Class);
   --  Synchronize the new position of the buttons layout after the user has
   --  scrolled the box

   procedure Scroll_Layout_Changed
     (Box : access Odd_Text_Box_Record'Class);
   --  Synchronize the new values of the buttons layout after the user has
   --  scrolled the Box. This procedure is mainly called on resize events.

   procedure Destroy_Cb (Box : access Odd_Text_Box_Record'Class);
   --  Free the memory occupied by the editor and the buttons layout, as well
   --  as all the associated pixmaps.

   function Pixmap_Clicked_Cb
     (Box   : access Odd_Text_Box_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback for button_press events in the buttons layout.

   function Button_Press_Cb
     (Box   : access Odd_Text_Box_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle button press events in the text child.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Box : out Odd_Text_Box) is
   begin
      Box := new Odd_Text_Box_Record;
      Initialize (Box);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Box   : access Odd_Text_Box_Record'Class) is
      Scrollbar : Gtk_Vscrollbar;
   begin
      Gtk.Box.Initialize_Hbox (Box, Homogeneous => False);

      Gtk_New (Box.Child, Vadj => Null_Adjustment);
      Set_Editable (Box.Child, False);
      Set_Line_Wrap (Box.Child, False);

      Gtk_New_Vscrollbar (Scrollbar, Get_Vadj (Box.Child));

      --  Set a minimal size for the layout, so that the buttons are visible.
      --  Note that this widget is resized vertically dynamically if needed,
      --  so we can just set a size of 0.
      Gtk_New (Box.Buttons);
      Set_USize (Box.Buttons, Layout_Width, 0);
      Add_Events (Box.Buttons, Button_Press_Mask or Button_Release_Mask);

      Set_Line_Wrap (Box.Child, False);

      Box_Cb.Object_Connect
        (Get_Vadj (Box.Child), "value_changed",
         Box_Cb.To_Marshaller (Scroll_Layout'Access),
         Slot_Object => Box);
      Box_Cb.Object_Connect
        (Get_Vadj (Box.Child), "changed",
         Box_Cb.To_Marshaller (Scroll_Layout_Changed'Access),
         Slot_Object => Box);
      Box_Cb.Object_Connect
        (Get_Vadj (Box.Child), "destroy",
         Box_Cb.To_Marshaller (Destroy_Cb'Access),
         Slot_Object => Box);
      Box_Event_Cb.Object_Connect
        (Box.Buttons, "button_press_event",
         Box_Event_Cb.To_Marshaller (Pixmap_Clicked_Cb'Access),
         Slot_Object => Box);
      Box_Event_Cb.Object_Connect
        (Box.Child, "button_press_event",
         Box_Event_Cb.To_Marshaller (Button_Press_Cb'Access),
         Slot_Object => Box);

      Pack_Start (Box, Box.Buttons, Expand => False, Fill => False);
      Pack_Start (Box, Box.Child, Expand => True, Fill => True);
      Pack_Start (Box, Scrollbar, Expand => False, Fill => False);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Box               : access Odd_Text_Box_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array)
   is
      Current_Line_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Current_Line_Mask   : Gdk.Bitmap.Gdk_Bitmap;

   begin
      Box.Font := Get_Gdkfont (Ps_Font_Name, Font_Size);

      --  Realize (Box.Child);
      Create_From_Xpm_D
        (Current_Line_Pixmap,
         Null_Window,
         Get_System,
         Current_Line_Mask,
         White (Get_System),
         Current_Line_Icon);

      --  Create the current line icon, and make sure it is never destroyed.

      Gtk_New
        (Box.Current_Line_Button, Current_Line_Pixmap, Current_Line_Mask);
      Ref (Box.Current_Line_Button);

      --  ??? Unfortunately, it is not possible currently to specify the
      --  step_increment for the adjustments, since this is overridden in
      --  several places in the text widget.
      --    Set_Step_Increment
      --     (Get_Vadj (Editor.Text),
      --      Gfloat (Get_Ascent (Editor.Font) + Get_Descent (Editor.Font)));

      Box.Line_Height := Get_Ascent (Box.Font) + Get_Descent (Box.Font);
   end Configure;

   -------------------
   -- Scroll_Layout --
   -------------------

   --  We can not make both the Gtk_Text and the Gtk_Layout use the same
   --  Gtk_Adjustment, since they both try to modify it when they are resized,
   --  resulting in an infinite loop and a Storage_Error. Instead, they both
   --  have their own adjustment, and we synchronize the Gtk_Layout ones with
   --  the Gtk_Text ones whenever it is needed.

   procedure Scroll_Layout (Box : access Odd_Text_Box_Record'Class) is
   begin
      Set_Value
        (Get_Vadjustment (Box.Buttons), Get_Value (Get_Vadj (Box.Child)));
      --  ??? Need to queue a draw event explicitely under Win32
      Queue_Draw (Box.Buttons);
   end Scroll_Layout;

   ---------------------------
   -- Scroll_Layout_Changed --
   ---------------------------

   procedure Scroll_Layout_Changed (Box : access Odd_Text_Box_Record'Class) is
   begin
      Set_Upper
        (Get_Vadjustment (Box.Buttons),
         Gfloat'Max
           (Get_Upper (Get_Vadj (Box.Child)),
            Get_Value (Get_Vadj (Box.Child))));
      Set_Lower
        (Get_Vadjustment (Box.Buttons), Get_Lower (Get_Vadj (Box.Child)));
      Set_Page_Size
        (Get_Vadjustment (Box.Buttons), Get_Page_Size (Get_Vadj (Box.Child)));

      --  Also set the value, since "value_changed" is not changed when the
      --  Gtk_Text is resized, and thus the Gtk_Layout is temporarily
      --  desynchronized. This should not be done if the two values are
      --  already equal, do nothing to prevent loops.
      --
      --  To work around a bug in gtk+ (when adjusting the value of the
      --  adjustment when we are resizing the code editor beyond the last
      --  line), we first hide it, and then show it again.

      if Get_Value (Get_Vadjustment (Box.Buttons)) /=
        Get_Value (Get_Vadj (Box.Child))
      then
         Hide (Box.Buttons);
         Set_Value
           (Get_Vadjustment (Box.Buttons),
            Get_Value (Get_Vadj (Box.Child)));
         --  ??? Need to queue a draw event explicitely under Win32
         Queue_Draw (Box.Buttons);
         Show (Box.Buttons);
      end if;
   end Scroll_Layout_Changed;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Box : access Odd_Text_Box_Record'Class) is
   begin
      Free (Box.Buffer);
      Unref (Box.Font);
      Destroy (Box.Current_Line_Button);
   end Destroy_Cb;

   --------------
   -- Set_Line --
   --------------

   procedure Set_Line
     (Box         : access Odd_Text_Box_Record;
      Line        : Natural;
      Set_Current : Boolean := True)
   is
      Y : Gint := Gint (Line - 1) * Box.Line_Height + 3;
   begin
      --  Display the current line icon
      --  Note that we start by hiding everything, and then show everything
      --  at the end, so that the layout is correctly refreshed. This is not
      --  done otherwise.

      Freeze (Box.Buttons);
      Hide_All (Box.Buttons);

      if Set_Current then
         if Get_Parent (Box.Current_Line_Button) /= null then
            Move (Box.Buttons, Box.Current_Line_Button, X => 10, Y => Y);
         else
            Put (Box.Buttons, Box.Current_Line_Button, X => 10, Y => Y);
         end if;
      else
         if Get_Parent (Box.Current_Line_Button) /= null then
            Remove (Box.Buttons, Box.Current_Line_Button);
         end if;
      end if;

      --  Scroll the code editor to make sure the line is visible on screen.

      Freeze (Box.Child);
      Clamp_Page
        (Get_Vadj (Box.Child),
         Lower => Gfloat (Y - Box.Line_Height),
         Upper => Gfloat (Y + 2 * Box.Line_Height));
      Thaw (Box.Child);

      if Set_Current then
         Box.Current_Line := Line;
      end if;

      Show_All (Box.Buttons);
      Thaw (Box.Buttons);

      --  Make sure the arrow that indicated the previous line is no longer
      --  visible.
      Queue_Draw (Box.Buttons);
   end Set_Line;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Box : access Odd_Text_Box_Record) return Natural is
   begin
      return Box.Current_Line;
   end Get_Line;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (Box : access Odd_Text_Box_Record) return Gtk.Text.Gtk_Text is
   begin
      return Box.Child;
   end Get_Child;

   -----------------
   -- Get_Buttons --
   -----------------

   function Get_Buttons
     (Box : access Odd_Text_Box_Record) return Gtk.Layout.Gtk_Layout is
   begin
      return Box.Buttons;
   end Get_Buttons;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Box    : access Odd_Text_Box_Record;
      Fore   : in Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Back   : in Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Chars  : in String := "") is
   begin
      Insert (Box.Child, Box.Font, Fore, Back, Chars);
   end Insert;

   ------------------------------
   -- Hide_Current_Line_Button --
   ------------------------------

   procedure Hide_Current_Line_Button (Box : access Odd_Text_Box_Record) is
   begin
      if Get_Parent (Box.Current_Line_Button) /= null then
         Freeze (Box.Buttons);
         Remove (Box.Buttons, Box.Current_Line_Button);
         Thaw (Box.Buttons);
      end if;
   end Hide_Current_Line_Button;

   ----------------------
   -- Pixels_From_Line --
   ----------------------

   function Pixels_From_Line
     (Box  : access Odd_Text_Box_Record;
      Line : Natural) return Gint is
   begin
      return Gint (Line - 1) * Box.Line_Height + 3;
   end Pixels_From_Line;

   ----------------------
   -- Line_From_Pixels --
   ----------------------

   function Line_From_Pixels
     (Box  : access Odd_Text_Box_Record;
      Y    : Gint) return Natural is
   begin
      return Natural (Y / Box.Line_Height + 1);
   end Line_From_Pixels;

   -----------------------
   -- Pixmap_Clicked_Cb --
   -----------------------

   function Pixmap_Clicked_Cb
     (Box   : access Odd_Text_Box_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      Line    : Natural := 0;
   begin
      case Get_Button (Event) is
         when 1 | 3 =>
            if Get_Event_Type (Event) = Button_Press then
               Line := Line_From_Pixels
                 (Box,
                  Gint (Get_Y (Event))
                  + Gint (Get_Value (Get_Vadj (Box.Child))));
               return On_Pixmap_Clicked
                 (Box, Natural (Get_Button (Event)), Line);
            end if;

         when 4 =>
            Set_Value
              (Get_Vadj (Box.Child),
               Get_Value (Get_Vadj (Box.Child)) -
                 Get_Page_Increment (Get_Vadj (Box.Child)));

         when  5 =>
            Set_Value
              (Get_Vadj (Box.Child),
               Get_Value (Get_Vadj (Box.Child)) +
                 Get_Page_Increment (Get_Vadj (Box.Child)));

         when others => return False;
      end case;
      return True;
   end Pixmap_Clicked_Cb;

   ---------------------
   -- Button_Press_Cb --
   ---------------------

   function Button_Press_Cb
     (Box   : access Odd_Text_Box_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      Menu    : Gtk_Menu;
      Line    : Natural := 0;
      Y       : Gint;
   begin
      case Get_Button (Event) is
         when 3 =>
            if Get_Event_Type (Event) = Button_Press then

               Y := Gint (Get_Y (Event)) - 1
                 + Gint (Get_Value (Get_Vadj (Box.Child)));
               Line := Line_From_Pixels (Box, Y);
               Menu := Child_Contextual_Menu
                 (Box, Line, Get_Entity
                  (Box, Gint (Get_X (Event)), Gint (Get_Y (Event))));

               Popup (Menu,
                      Button        => Gdk.Event.Get_Button (Event),
                      Activate_Time => Gdk.Event.Get_Time (Event));
               --  Stop the event so that the contextual menu is handled
               --  correctly (ie hidden when the mouse button is
               --  released, and the selection is not unselected).

               Emit_Stop_By_Name (Box.Child, "button_press_event");
            end if;
            return True;
         when 4 =>
            Set_Value (Get_Vadj (Box.Child), Get_Value (Get_Vadj (Box.Child))
                       - Get_Page_Increment (Get_Vadj (Box.Child)));
            return True;
         when  5 =>
            Set_Value (Get_Vadj (Box.Child), Get_Value (Get_Vadj (Box.Child))
                       + Get_Page_Increment  (Get_Vadj (Box.Child)));
            return True;
         when others => return False;
      end case;
   end Button_Press_Cb;

   ----------------
   -- Set_Buffer --
   ----------------

   procedure Set_Buffer
     (Box            : access Odd_Text_Box_Record;
      Buffer         : Odd.Types.String_Access := null;
      Clear_Previous : Boolean := True) is
   begin
      if Clear_Previous then
         Free (Box.Buffer);
      end if;

      Box.Buffer := Buffer;
      Delete_Text (Box.Child);
   end Set_Buffer;

   -----------------
   -- Lines_Count --
   -----------------

   function Lines_Count (Box : access Odd_Text_Box_Record) return Natural is
      Lines : Natural := 1;
   begin
      if Box.Buffer /= null then
         for Index in Box.Buffer'Range loop
            if Box.Buffer (Index) = ASCII.LF then
               Lines := Lines + 1;
            end if;
         end loop;
      end if;

      return Lines;
   end Lines_Count;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Box : access Odd_Text_Box_Record) return Boolean is
   begin
      return Box.Buffer = null;
   end Is_Empty;

   ------------------
   -- Update_Child --
   ------------------

   procedure Update_Child (Box : access Odd_Text_Box_Record'Class) is
   begin
      --  Clear the old text
      Freeze (Box.Child);
      Delete_Text (Box.Child);
      if Box.Buffer /= null then
         --  Insert the contents of the buffer in the text area.
         Insert_Buffer (Box, Box.Buffer.all);
      end if;
      Thaw (Box.Child);
   end Update_Child;

   ------------------
   -- Print_Buffer --
   ------------------

   procedure Insert_Buffer
     (Box    : access Odd_Text_Box_Record;
      Buffer : String) is
   begin
      Insert (Box, Chars => Buffer);
   end Insert_Buffer;

   -----------------------
   -- On_Pixmap_Clicked --
   -----------------------

   function On_Pixmap_Clicked
     (Box    : access Odd_Text_Box_Record;
      Button : Natural;
      Line   : Natural) return Boolean
   is
      pragma Warnings (Off, Box);
      pragma Warnings (Off, Button);
      pragma Warnings (Off, Line);
   begin
      return True;
   end On_Pixmap_Clicked;

   ----------------------------
   -- Invisible_Column_Width --
   ----------------------------

   function Invisible_Column_Width
     (Box : access Odd_Text_Box_Record) return Glib.Gint is
   begin
      return 0;
   end Invisible_Column_Width;

   ---------------------------
   -- Child_Contextual_Menu --
   ---------------------------

   function Child_Contextual_Menu
     (Box    : access Odd_Text_Box_Record;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu
   is
      pragma Warnings (Off, Box);
      pragma Warnings (Off, Line);
      pragma Warnings (Off, Entity);
   begin
      return null;
   end Child_Contextual_Menu;

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Box : access Odd_Text_Box_Record) return Odd.Types.String_Access is
   begin
      return Box.Buffer;
   end Get_Buffer;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Box : access Odd_Text_Box_Record'Class;
      X, Y : Gint) return String
   is
      Line    : Natural := 0;
      Index   : Integer;
      Current_Line : Natural := 1;
      Start_Index : Integer;
      Min : Gint := Gint (Guint'Min (Get_Selection_Start_Pos (Box.Child),
                                     Get_Selection_End_Pos (Box.Child)));
      Max : Gint := Gint (Guint'Max (Get_Selection_Start_Pos (Box.Child),
                                     Get_Selection_End_Pos (Box.Child)));
      X2 : Gint;
   begin
      if Box.Buffer /= null then
         Index := Box.Buffer'First;

         Line := Line_From_Pixels
           (Box, Y - 1 + Gint (Get_Value (Get_Vadj (Box.Child))));
         X2 := X / Char_Width (Box.Font, Character' ('m'))
           - Invisible_Column_Width (Box) + 1;

         if X2 <= 0 then
            Index := -1;
         else
            while Index <= Box.Buffer'Last
              and then Current_Line < Line
            loop
               if Box.Buffer (Index) = ASCII.LF then
                  Current_Line := Current_Line + 1;
               end if;

               Index := Index + 1;
            end loop;

            --  Go to the right column, but make sure we are still on
            --  the current line.
            --  Index is the index in the buffer, while J is the current
            --  column number (after processing horizontal tabs).

            Index := Index - Box.Buffer'First;
            declare
               J : Integer := 1;
            begin
               while J <= Integer (X2) loop

                  Index := Index + 1;
                  exit when Box.Buffer'Last < Index
                    or else Box.Buffer (Index) = ASCII.LF;

                  if Box.Buffer (Index) = ASCII.HT
                    and then J mod Tab_Size /= 0
                  then
                     --  Go to the next column that is a multiple of Tab_Size
                     J := (1 + J / Tab_Size) * Tab_Size + 1;
                  else
                     J := J + 1;
                  end if;
               end loop;
            end;
         end if;

         Start_Index := Index +
           Line * Integer (Invisible_Column_Width (Box));

         if Get_Has_Selection (Box.Child)
           and then Min <= Gint (Start_Index)
           and then Gint (Start_Index) <= Max
         then
            declare
               S : String := Get_Chars (Box.Child, Min, Max);
            begin
               for J in S'Range loop

                  if S (J) = ASCII.LF then
                     Max := Gint (J - S'First) + Min;
                     exit;
                  end if;
               end loop;

               return Get_Chars (Box.Child, Min, Max);
            end;
         else
            if Index < 0 or Index > Box.Buffer'Last then
               return "";
            else
               Start_Index := Index;
               while Start_Index >= Box.Buffer'First
                 and then (Is_Letter (Box.Buffer (Start_Index))
                           or else
                 Is_Digit (Box.Buffer (Start_Index))
                 or else
                 Box.Buffer (Start_Index) = '_')
               loop
                  Start_Index := Start_Index - 1;
               end loop;
               while Index <= Box.Buffer'Last
                 and then (Is_Letter (Box.Buffer (Index))
                           or else
                 Is_Digit (Box.Buffer (Index))
                 or else
                 Box.Buffer (Index) = '_')
               loop
                  Index := Index + 1;
               end loop;
               return Box.Buffer (Start_Index + 1 .. Index - 1);
            end if;
         end if;

      end if;
      return "";

   end Get_Entity;

   ---------------------
   -- Get_Entity_Area --
   ---------------------

   procedure Get_Entity_Area
     (Box    : access Odd_Text_Box_Record'Class;
      X, Y   : in Glib.Gint;
      Area   : out Gdk.Rectangle.Gdk_Rectangle;
      Entity : in out Odd.Types.String_Access)
   is
      Line    : Natural := 0;
      Index   : Integer;
      Line_Index : Integer;
      Current_Line : Natural := 1;
      Start_Index : Integer;
      X2 : Gint;

   begin
      Entity := null;
      if Box.Buffer /= null then
         Index := Box.Buffer'First;

         Line := Line_From_Pixels
           (Box, Y - 1 + Gint (Get_Value (Get_Vadj (Box.Child))));
         X2 := X / Char_Width (Box.Font, Character' ('m'))
           - Invisible_Column_Width (Box) + 1;

         if X2 <= 0 then
            Index := -1;
         else
            while Index <= Box.Buffer'Last
              and then Current_Line < Line
            loop
               if Box.Buffer (Index) = ASCII.LF then
                  Current_Line := Current_Line + 1;
                  Line_Index := Index;
               end if;

               Index := Index + 1;
            end loop;

            --  Go to the right column, but make sure we are still on
            --  the current line.
            --  Index is the index in the buffer, while J is the current
            --  column number (after processing horizontal tabs).

            Index := Index - Box.Buffer'First;
            declare
               J : Integer := 1;
            begin
               while J <= Integer (X2) loop

                  Index := Index + 1;
                  exit when Box.Buffer'Last < Index
                    or else Box.Buffer (Index) = ASCII.LF;

                  if Box.Buffer (Index) = ASCII.HT
                    and then J mod Tab_Size /= 0
                  then
                     --  Go to the next column that is a multiple of Tab_Size
                     J := (1 + J / Tab_Size) * Tab_Size + 1;
                  else
                     J := J + 1;
                  end if;
               end loop;
            end;
         end if;

         Start_Index := Index +
           Line * Integer (Invisible_Column_Width (Box));

         if Index < 0 or Index > Box.Buffer'Last then
            Entity := null;
         else
            Start_Index := Index;
            while Start_Index >= Box.Buffer'First
              and then (Is_Letter (Box.Buffer (Start_Index))
                        or else
                        Is_Digit (Box.Buffer (Start_Index))
                        or else
                        Box.Buffer (Start_Index) = '_')
            loop
               Start_Index := Start_Index - 1;
            end loop;
               while Index <= Box.Buffer'Last
                 and then (Is_Letter (Box.Buffer (Index))
                           or else
                           Is_Digit (Box.Buffer (Index))
                           or else
                           Box.Buffer (Index) = '_')
               loop
                  Index := Index + 1;
               end loop;

               Area.X :=
                 Gint16 (Integer (-X)
                         + (Start_Index - Line_Index
                            + Integer (Invisible_Column_Width (Box)))
                         * Integer (Char_Width (Box.Font, Character' ('m'))));
               Area.Width :=
                 Guint16 (Gint ((Index - Start_Index - 1))
                          * (Char_Width (Box.Font, Character' ('m'))));

               Area.Y :=
                 -Gint16 ((Y mod (Get_Ascent (Box.Font)
                                   + Get_Descent (Box.Font))));

               Area.Height :=
                 Guint16 (Get_Ascent (Box.Font) + Get_Descent (Box.Font));

               Entity := new String'
                 (Box.Buffer (Start_Index + 1 .. Index - 1));
         end if;
      end if;
   end Get_Entity_Area;

end Odd.Text_Boxes;
