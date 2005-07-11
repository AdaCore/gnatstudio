-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2005                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Regpat;             use GNAT.Regpat;

with Glib;                    use Glib;
with Gdk.Color;               use Gdk.Color;
with Gdk.Event;               use Gdk.Event;
with Gdk.Font;                use Gdk.Font;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Window;              use Gdk.Window;
with Gtk.Box;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Container;           use Gtk.Container;
with Gtk.Pixmap;              use Gtk.Pixmap;
with Gtk.Handlers;            use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Layout;              use Gtk.Layout;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Mark;           use Gtk.Text_Mark;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;      use Gtk.Text_Tag_Table;
with Gtk.Text_View;           use Gtk.Text_View;
with Gdk.Pixmap;              use Gdk.Pixmap;
with Gdk.Bitmap;              use Gdk.Bitmap;
with Gdk.Font;                use Gdk.Font;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Widget;              use Gtk.Widget;
with Pango.Font;              use Pango.Font;

with Basic_Types;             use Basic_Types;
with Config;                  use Config;
with Debugger;                use Debugger;
with Default_Preferences;     use Default_Preferences;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Types;               use GVD.Types;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;

package body GVD.Assembly_View is

   Me : constant Debug_Handle := Create ("GVD.Assembly_View", On);

   package Assembly_View_Cb is new Callback (GVD_Assembly_View_Record);
   package Assembly_View_Event_Cb is
     new Return_Callback (GVD_Assembly_View_Record, Boolean);

   procedure Scroll_Layout (Assembly_View : GVD_Assembly_View);
   pragma Unreferenced (Scroll_Layout);
   --  Synchronize the new position of the buttons layout after the user has
   --  scrolled the box

   procedure Scroll_Layout_Changed (Assembly_View : GVD_Assembly_View);
   pragma Unreferenced (Scroll_Layout_Changed);
   --  Synchronize the new values of the buttons layout after the user has
   --  scrolled the Box. This procedure is mainly called on resize events.

   procedure Destroy_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   --  Free the memory occupied by the editor and the buttons layout, as well
   --  as all the associated pixmaps.

   function Pixmap_Clicked_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback for button_press events in the buttons layout.

   function Button_Press_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle button press events in the text view.

   function Key_Press_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the child (handling of meta-scrolling)

   procedure Show_Current_Line_Menu
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   --  Display the current line in the editor.

   procedure Iter_From_Address
     (Assembly_View : GVD_Assembly_View;
      Address       : Address_Type;
      Iter          : out Gtk_Text_Iter;
      Found         : out Boolean);
   --  Return an iterator pointing at the beginning of the Address in the
   --  text buffer. Addresses are search at the begginning of lines in the
   --  buffer.
   --  Found tells whether the address was found.

   function Address_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Address_Type;
   --  Return the address associated with a given line in the text widget.
   --  "" is returned if no address was found.

   procedure Is_Breakpoint_Address
     (Assembly_View : GVD_Assembly_View;
      Addr          : Address_Type;
      Result        : out Boolean;
      Num           : out Breakpoint_Identifier);
   --  Result is set to True if a breakpoint is set at address Addr

   procedure Reset_Highlighting (Assembly_View : GVD_Assembly_View);
   --  Reset the buffer hightlighting

   procedure On_Frame_Changed
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type;
      End_Pc        : Address_Type);
   --  Called when the assembly code for the address PC needs to be loaded.
   --  This gets the assembly source code for a range starting at PC, and
   --  going up to End_Pc.
   --  A minimal range of Assembly_Range_Size is displayed, unless End_Pc is
   --  "-1", in which case the assembly code for the whole current function is
   --  displayed (??? To be updated).

   function In_Range
     (Address : Address_Type;
      R       : Cache_Data_Access) return Boolean;
   --  Return True if Address is in the range of addresses described by R.

   function Find_In_Cache
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type) return Cache_Data_Access;
   --  Return the cached data that contains PC.
   --  null is returned if none is found.

   function Add_Address (Addr : String; Offset : Integer) return String;
   --  Add the value of Offset to the hexadecimal number Addr.
   --  Addr is coded in C (0x....), and so is the returned string

   procedure Meta_Scroll
     (Assembly_View : GVD_Assembly_View;
      Down          : Boolean);
   --  The user has asked to see the assembly range outside what is currently
   --  displayed in the assembly editor.

   procedure Meta_Scroll_Down
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   procedure Meta_Scroll_Up
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   --  The user has asked for the previous or next undisplayed assembly page

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Assembly_View : out GVD_Assembly_View;
      Process       : access Glib.Object.GObject_Record'Class) is
   begin
      Assembly_View := new GVD_Assembly_View_Record;
      Initialize (Assembly_View, Process);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Assembly_View : GVD_Assembly_View;
      Process       : access Glib.Object.GObject_Record'Class)
   is
      Scrolling_Area : Gtk_Scrolled_Window;
   begin
      Gtk.Box.Initialize_Hbox (Assembly_View, Homogeneous => False);

      Gtk_New (Assembly_View.View);
      Set_Editable (Assembly_View.View, False);
      Set_Wrap_Mode (Assembly_View.View, Wrap_None);

      Gtk_New (Scrolling_Area);
      Set_Policy
        (Scrolling_Area,
         H_Scrollbar_Policy => Gtk.Enums.Policy_Automatic,
         V_Scrollbar_Policy => Gtk.Enums.Policy_Automatic);
      Add (Container => Scrolling_Area, Widget => Assembly_View.View);

      --  Set a minimal size for the layout, so that the buttons are visible.
      --  Starting with GtkAda 2.0, the layout is not resized dynamically
      --  vertically, and if we don't set a big enough size, then the children
      --  won't be visible.
      --  However, the size is modified appropriately when adding new children,
      --  so we can have an initial height of 0 (ie the size actually occupied
      --  on the screen).
      Gtk_New (Assembly_View.Buttons);
      Set_USize (Assembly_View.Buttons, Layout_Width, 0);
      Add_Events
        (Assembly_View.Buttons, Button_Press_Mask or Button_Release_Mask);

--        Box_Cb.Object_Connect
--          (Get_Vadj (Box.Child), "value_changed",
--           Box_Cb.To_Marshaller (Scroll_Layout'Access),
--           Slot_Object => Box);
--        Box_Cb.Object_Connect
--          (Get_Vadj (Box.Child), "changed",
--           Box_Cb.To_Marshaller (Scroll_Layout_Changed'Access),
--           Slot_Object => Box);

      Assembly_View_Cb.Connect
        (Assembly_View, "destroy",
         Assembly_View_Cb.To_Marshaller (Destroy_Cb'Access));
      Assembly_View_Event_Cb.Object_Connect
        (Assembly_View.Buttons, "button_press_event",
         Assembly_View_Event_Cb.To_Marshaller (Pixmap_Clicked_Cb'Access),
         Slot_Object => Assembly_View);
      Assembly_View_Event_Cb.Object_Connect
        (Assembly_View.View, "button_press_event",
         Assembly_View_Event_Cb.To_Marshaller (Button_Press_Cb'Access),
         Slot_Object => Assembly_View);
      Assembly_View_Event_Cb.Object_Connect
        (Assembly_View.View, "key_press_event",
         Assembly_View_Event_Cb.To_Marshaller (Key_Press_Cb'Access),
         Assembly_View);

      Pack_Start
        (Assembly_View, Assembly_View.Buttons, Expand => False, Fill => False);
      Pack_Start (Assembly_View, Scrolling_Area, Expand => True, Fill => True);

      Assembly_View.Process := Glib.Object.GObject (Process);
      Show_All (Assembly_View);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Assembly_View     : GVD_Assembly_View;
      Font              : Pango.Font.Pango_Font_Description;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array)
   is
      Current_Line_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Current_Line_Mask   : Gdk.Bitmap.Gdk_Bitmap;
      Tag_Table           : constant Gtk_Text_Tag_Table :=
                              Get_Tag_Table (Get_Buffer (Assembly_View.View));
   begin
      --  Font

      Set_Font (Assembly_View, Font);

      --  Pixmaps

      Create_From_Xpm_D
        (Current_Line_Pixmap,
         Null_Window,
         Get_Default_Colormap,
         Current_Line_Mask,
         White (Get_Default_Colormap),
         Current_Line_Icon);

      Create_From_Xpm_D
        (Assembly_View.Stop_Pixmap,
         Null_Window,
         Get_Default_Colormap,
         Assembly_View.Stop_Mask,
         White (Get_Default_Colormap),
         Stop_Icon);

      --  Create the current line icon, and make sure it is never destroyed.

--        Gtk_New
--          (Assembly_View.Current_Line_Button,
--           Current_Line_Pixmap,
--           Current_Line_Mask);
--        Ref (Assembly_View.Current_Line_Button);

      --  Highlighting

      Gtk_New (Assembly_View.Highlight_Tag);
      Set_Property
        (Assembly_View.Highlight_Tag, Foreground_Gdk_Property,
         Get_Pref (GVD_Prefs, Asm_Highlight_Color));
      Add (Tag_Table, Assembly_View.Highlight_Tag);

      --  Pc
      --  ??? This a temporrary solution used to materialized the program
      --  counter at the Gtk_Text_Buffer level.

      Gtk_New (Assembly_View.Pc_Tag);
      Set_Property
        (Assembly_View.Pc_Tag, Background_Gdk_Property,
         Get_Pref (GVD_Prefs, Memory_Highlighted_Color));
      Add (Tag_Table, Assembly_View.Pc_Tag);
   end Configure;

   -------------------
   -- Scroll_Layout --
   -------------------

   --  We can not make both the Gtk_Text and the Gtk_Layout use the same
   --  Gtk_Adjustment, since they both try to modify it when they are resized,
   --  resulting in an infinite loop and a Storage_Error. Instead, they both
   --  have their own adjustment, and we synchronize the Gtk_Layout ones with
   --  the Gtk_Text ones whenever it is needed.

   procedure Scroll_Layout (Assembly_View : GVD_Assembly_View) is
   begin
--        Set_Value
--          (Get_Vadjustment (Box.Buttons), Get_Value (Get_Vadj (Box.Child)));
      --  ??? Need to queue a draw event explicitely under Win32
      Queue_Draw (Assembly_View.Buttons);
   end Scroll_Layout;

   ---------------------------
   -- Scroll_Layout_Changed --
   ---------------------------

   procedure Scroll_Layout_Changed (Assembly_View : GVD_Assembly_View) is
      pragma Unreferenced (Assembly_View);
   begin
--        Set_Upper
--          (Get_Vadjustment (Box.Buttons),
--           Grange_Float'Max
--             (Get_Upper (Get_Vadj (Box.Child)),
--              Get_Value (Get_Vadj (Box.Child))));
--        Set_Lower
--          (Get_Vadjustment (Box.Buttons), Get_Lower (Get_Vadj (Box.Child)));
--        Set_Page_Size
--       (Get_Vadjustment (Box.Buttons), Get_Page_Size (Get_Vadj (Box.Child)));

      --  Also set the value, since "value_changed" is not changed when the
      --  Gtk_Text is resized, and thus the Gtk_Layout is temporarily
      --  desynchronized. This should not be done if the two values are
      --  already equal, do nothing to prevent loops.
      --
      --  To work around a bug in gtk+ (when adjusting the value of the
      --  adjustment when we are resizing the code editor beyond the last
      --  line), we first hide it, and then show it again.

--        if Get_Value (Get_Vadjustment (Box.Buttons)) /=
--          Get_Value (Get_Vadj (Box.Child))
--        then
--           Hide (Box.Buttons);
--           Set_Value
--             (Get_Vadjustment (Box.Buttons),
--              Get_Value (Get_Vadj (Box.Child)));
--           --  ??? Need to queue a draw event explicitely under Win32
--           Queue_Draw (Box.Buttons);
--           Show (Box.Buttons);
--        end if;
      null;
   end Scroll_Layout_Changed;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class) is
   begin
      Unref (Assembly_View.Font);

--        Destroy (Assembly_View.Current_Line_Button);
   end Destroy_Cb;

   ---------------------
   -- Set_Source_Line --
   ---------------------

   procedure Set_Source_Line
     (Assembly_View : GVD_Assembly_View;
      Source_Line   : Natural)
   is
   begin
      Get_Line_Address
        (Visual_Debugger (Assembly_View.Process).Debugger,
         Source_Line,
         Assembly_View.Source_Line_Start,
         Assembly_View.Source_Line_End);

      Trace (Me, "[Set_Source_Line] Source_Line = " & Source_Line'Img &
             ASCII.LF &
             "Range_Start = " &
             Address_To_String (Assembly_View.Source_Line_Start) &
             ASCII.LF &
             "Range_End   = " &
             Address_To_String (Assembly_View.Source_Line_End));
   end Set_Source_Line;

   ---------------------
   -- Index_From_Line --
   ---------------------

   function Index_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural)
      return Natural
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Iter   : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Buffer, Iter, Gint (Line));
      return Natural (Get_Offset (Iter));
   end Index_From_Line;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Assembly_View : GVD_Assembly_View;
      Chars         : String := "")
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
   begin
      Trace (Me,
             "[Insert_At_Cursor] Chars = " & ASCII.LF &
             Chars);
      Insert_At_Cursor (Buffer, Chars);
   end Insert_At_Cursor;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Assembly_View : GVD_Assembly_View;
      Tag           : Gtk.Text_Tag.Gtk_Text_Tag;
      Chars         : String := "")
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Start_Mark : Gtk_Text_Mark;
   begin
      Trace (Me,
             "[Insert_At_Cursor (Tag)] Chars = " & ASCII.LF &
             Chars);
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Start_Mark := Create_Mark (Buffer, Where => Start_Iter);
      Insert_At_Cursor (Buffer, Chars);
      Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
      Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
      Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
      Delete_Mark (Buffer, Start_Mark);
   end Insert_At_Cursor;

   ----------------------
   -- Pixels_From_Line --
   ----------------------

   function Pixels_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Gint is
   begin
      return Gint (Line - 1) * Assembly_View.Line_Height + 3;
   end Pixels_From_Line;

   ----------------------
   -- Line_From_Pixels --
   ----------------------

   function Line_From_Pixels
     (Assembly_View : GVD_Assembly_View;
      Y             : Gint) return Natural is
   begin
      return Natural (Y / Assembly_View.Line_Height + 1);
   end Line_From_Pixels;

   --------------------
   -- Move_N_Columns --
   --------------------

   procedure Move_N_Columns
     (Assembly_View : GVD_Assembly_View;
      Index         : in out Natural;
      Columns       : Integer)
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Text       : String_Access;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      J          : Integer := 1;
      Tab_Size   : constant Integer := 8;

   begin
      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Text := new String'(Get_Text (Buffer, Start_Iter, End_Iter));

      --  Go to the right column, but make sure we are still on
      --  the current line.
      --  Index is the index in the buffer, while J is the current
      --  column number (after processing horizontal tabs).

      while J <= Columns loop
         Index := Index + 1;

         exit when Index > Text'Last
           or else Text (Index) = ASCII.LF;

         if Text (Index) = ASCII.HT
           and then J mod Tab_Size /= 0
         then
            --  Go to the next column that is a multiple of Tab_Size
            J := (1 + J / Tab_Size) * Tab_Size + 1;
         else
            J := J + 1;
         end if;
      end loop;

      Free (Text);
   end Move_N_Columns;

   -----------------------
   -- Pixmap_Clicked_Cb --
   -----------------------

   function Pixmap_Clicked_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Assembly_View);
      --        Line : Natural := 0;
   begin
      case Get_Button (Event) is
--           when 1 | 3 =>
--              if Get_Event_Type (Event) = Button_Press then
--                 Line := Line_From_Pixels (Box, Gint (Get_Y (Event)) - 1);
--                 return On_Pixmap_Clicked
--                   (Box, Natural (Get_Button (Event)), Line);
--              end if;
--
--           when 4 =>
--              Set_Value
--                (Get_Vadj (Box.Child),
--                 Get_Value (Get_Vadj (Box.Child)) -
--                   Get_Page_Increment (Get_Vadj (Box.Child)) / 2.0);
--
--           when  5 =>
--              Set_Value
--                (Get_Vadj (Box.Child),
--                 Get_Value (Get_Vadj (Box.Child)) +
--                   Get_Page_Increment (Get_Vadj (Box.Child)) / 2.0);

         when others =>
            return False;
      end case;

--        return True;
   end Pixmap_Clicked_Cb;

   ---------------------
   -- Button_Press_Cb --
   ---------------------

   function Button_Press_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk.Event.Gdk_Event) return Boolean
   is
      Menu       : Gtk_Menu;
      Line       : Natural := 0;
--        Y          : Gint;
      Area       : Gdk.Rectangle.Gdk_Rectangle;
      Entity     : String_Access;
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean;
      Text       : String_Access;

   begin
      case Get_Button (Event) is
         when 3 =>
            if Get_Event_Type (Event) = Button_Press then
               Get_Bounds (Buffer, Start_Iter, End_Iter);
               Text := new String'(Get_Text (Buffer, Start_Iter, End_Iter));

--                    Y := Gint (Get_Y (Event)) - 1
--                      + Gint (Get_Value (Get_Vadj (Box.Child)));
--                    Line := Line_From_Pixels (Box, Y);
               Line := 1;
               --  ??? Temporary the time the above gets implemented

               --  Take the selection into account if it is exists

               Get_Selection_Bounds (Buffer, Start_Iter, End_Iter, Result);

               if Result then
                  --  The selection has a nonzero length.
                  --  Keep only the first line of the selection. This avoids
                  --  having too long menus, and since the debugger can not
                  --  handle multiple line commands anyway is not a big
                  --  problem.
                  --  We do not use Editor.Buffer directly, so that we don't
                  --  have to take into account the presence of line
                  --  numbers.

                  if Get_Line (Start_Iter) /= Get_Line (End_Iter) then
                     Forward_To_Line_End (End_Iter, Result);
                     --  ??? Not sure the following is needed. Is end_iter
                     --  before or after LF?
--                       if Result then
--                          Backward_Cursor_Position (End_Iter, Result);
--                       end if;
                  end if;

                  declare
                     S : constant String :=
                           Get_Text (Buffer, Start_Iter, End_Iter);
                  begin
                     Menu :=
                       Child_Contextual_Menu
                         (GVD_Assembly_View (Assembly_View), Line, S);
                  end;

               else
                  Get_Entity_Area
                    (GVD_Assembly_View (Assembly_View),
                     Gint (Get_X (Event)), Gint (Get_Y (Event)),
                     Area, Entity);

                  if Entity /= null then
                     Menu := Child_Contextual_Menu
                       (GVD_Assembly_View (Assembly_View), Line, Entity.all);
                     Free (Entity);
                  else
                     Menu := Child_Contextual_Menu
                       (GVD_Assembly_View (Assembly_View), Line, "");
                  end if;
               end if;

               Popup
                 (Menu,
                  Button        => Gdk.Event.Get_Button (Event),
                  Activate_Time => Gdk.Event.Get_Time (Event));
               --  Stop the event so that the contextual menu is handled
               --  correctly (ie hidden when the mouse button is
               --  released, and the selection is not unselected).

               Emit_Stop_By_Name (Assembly_View.View, "button_press_event");
            end if;

            Free (Text);

            return True;

         when 4 =>
--           Set_Value (Get_Vadj (Box.Child), Get_Value (Get_Vadj (Box.Child))
--                         - Get_Page_Increment (Get_Vadj (Box.Child)));
            return True;

         when  5 =>
--           Set_Value (Get_Vadj (Box.Child), Get_Value (Get_Vadj (Box.Child))
--                         + Get_Page_Increment  (Get_Vadj (Box.Child)));
            return True;

         when others =>
            return False;
      end case;
   end Button_Press_Cb;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Assembly_View : GVD_Assembly_View;
      Text          : String)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
   begin
      Begin_User_Action (Buffer);
      Set_Text (Buffer, Text);
      End_User_Action (Buffer);
   end Set_Text;

   -----------------------
   -- On_Pixmap_Clicked --
   -----------------------

   function On_Pixmap_Clicked
     (Assembly_View : GVD_Assembly_View;
      Button : Natural;
      Line   : Natural) return Boolean
   is
      Result : Boolean;
      Num    : Breakpoint_Identifier;
   begin
      if Button = 1 then
         declare
            Address : constant Address_Type :=
                        Address_From_Line (Assembly_View, Line);
            Process : constant Visual_Debugger :=
                        Visual_Debugger (Assembly_View.Process);
         begin
            Is_Breakpoint_Address (Assembly_View, Address, Result, Num);

            if Result then
               Remove_Breakpoint
                 (Process.Debugger, Num, Mode => GVD.Types.Visible);
            else
               if Address /= Invalid_Address then
                  Break_Address
                    (Process.Debugger, Address, Mode => GVD.Types.Visible);
               end if;
            end if;
         end;
      end if;

      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return True;
   end On_Pixmap_Clicked;

   ----------------------------
   -- Invisible_Column_Width --
   ----------------------------

   function Invisible_Column_Width
     (Assembly_View : GVD_Assembly_View) return Glib.Gint
   is
      pragma Unreferenced (Assembly_View);
   begin
      return 0;
   end Invisible_Column_Width;

   ---------------------------
   -- Child_Contextual_Menu --
   ---------------------------

   function Child_Contextual_Menu
     (Assembly_View : GVD_Assembly_View;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu
   is
      pragma Unreferenced (Line, Entity);

      Menu  : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, Label => -"Show Current Location");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, "activate",
         Assembly_View_Cb.To_Marshaller (Show_Current_Line_Menu'Access),
         Assembly_View);

      Gtk_New (Menu_Item, Label => -"Show Previous Page");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, "activate",
         Assembly_View_Cb.To_Marshaller (Meta_Scroll_Up'Access),
         Assembly_View);

      Gtk_New (Menu_Item, Label => -"Show Next Page");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, "activate",
         Assembly_View_Cb.To_Marshaller (Meta_Scroll_Down'Access),
         Assembly_View);

      Show_All (Menu);
      return Menu;
   end Child_Contextual_Menu;

   ---------------------
   -- Get_Entity_Area --
   ---------------------

   procedure Get_Entity_Area
     (Assembly_View : GVD_Assembly_View;
      X, Y          : in Glib.Gint;
      Area          : out Gdk.Rectangle.Gdk_Rectangle;
      Entity        : in out String_Access)
   is
      Buffer       : constant Gtk_Text_Buffer :=
                       Get_Buffer (Assembly_View.View);
      Start_Iter   : Gtk_Text_Iter;
      End_Iter     : Gtk_Text_Iter;
      Line         : constant Natural := 0;
      Text         : String_Access;
      Index        : Integer;
      Line_Index   : Integer;
      Start_Index  : Integer;
      X2           : Gint;

   begin
      Entity := null;

      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Text := new String'(Get_Text (Buffer, Start_Iter, End_Iter));

      if Text /= null then
         Index := Text'First;
--           Line := Line_From_Pixels
--             (Box, Y - 1 + Gint (Get_Value (Get_Vadj (Box.Child))));
         X2 := X / Char_Width (Assembly_View.Font, Character'('m')) -
           Invisible_Column_Width (Assembly_View) + 1;

         if X2 <= 0 then
            Index := -1;
         else
            Index := Index_From_Line (Assembly_View, Line);
            Line_Index := Index;
            Move_N_Columns (Assembly_View, Index, Integer (X2));
         end if;

         Start_Index := Index +
           Line * Integer (Invisible_Column_Width (Assembly_View));

         if Index < 0 or Index > Text'Last then
            Entity := null;
         else
            Start_Index := Index;

            while Start_Index >= Text'First
              and then
                (Is_Letter (Text (Start_Index))
                 or else Is_Digit (Text (Start_Index))
                 or else Text (Start_Index) = '_')
            loop
               Start_Index := Start_Index - 1;
            end loop;

            while Index <= Text'Last
              and then
                (Is_Letter (Text (Index))
                 or else Is_Digit (Text (Index))
                 or else Text (Index) = '_')
            loop
               Index := Index + 1;
            end loop;

            if Index >= Start_Index + 2 then
               Entity := new String'
                 (Text (Start_Index + 1 .. Index - 1));

               Area.X := GRectangle_Coord
                 (Integer (-X) +
                    (Start_Index - Line_Index +
                       Integer (Invisible_Column_Width (Assembly_View))) *
                    Integer
                      (Char_Width (Assembly_View.Font, Character'('m'))));

               Area.Width := GRectangle_Length
                 (Gint ((Index - Start_Index - 1)) *
                    (Char_Width (Assembly_View.Font, Character'('m'))));

               Area.Y := -GRectangle_Coord
                 ((Y mod (Get_Ascent (Assembly_View.Font) +
                            Get_Descent (Assembly_View.Font))));

               Area.Height := GRectangle_Length
                 (Get_Ascent (Assembly_View.Font) +
                    Get_Descent (Assembly_View.Font));
            end if;
         end if;
      end if;

      Free (Text);
   end Get_Entity_Area;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Assembly_View : GVD_Assembly_View;
      Font          : Pango_Font_Description)
   is
      F : Gdk_Font;
      use Gdk;
   begin
      F := From_Description (Font);

      if F /= Assembly_View.Font then
         Assembly_View.Font := F;
         --  ??? Unfortunately, it is not possible currently to specify the
         --  step_increment for the adjustments, since this is overridden in
         --  several places in the text widget.
         --  Set_Step_Increment
         --   (Get_Vadj (Editor.Text),
         --    Gfloat (Get_Ascent (Editor.Font) + Get_Descent (Editor.Font)));
         Assembly_View.Line_Height :=
           Get_Ascent (Assembly_View.Font) + Get_Descent (Assembly_View.Font);
      end if;

      Modify_Font (Assembly_View.View, Font);
   end Set_Font;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Assembly_View : GVD_Assembly_View;
      Pc            : GVD.Types.Address_Type) is
   begin
      Trace (Me, "*** [Set_Address] *** Pc = " & Address_To_String (Pc));

      Assembly_View.Pc := Pc;
   end Set_Address;

   ------------------------
   -- Reset_Highlighting --
   ------------------------

   procedure Reset_Highlighting (Assembly_View : GVD_Assembly_View) is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Trace (Me, "[Reset_Highlighting]");
      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Remove_Tag (Buffer, Assembly_View.Highlight_Tag, Start_Iter, End_Iter);
      Remove_Tag (Buffer, Assembly_View.Pc_Tag, Start_Iter, End_Iter);
   end Reset_Highlighting;

   -----------------------------
   -- Highlight_Address_Range --
   -----------------------------

   procedure Highlight_Address_Range (Assembly_View : GVD_Assembly_View) is
      Buffer      : constant Gtk_Text_Buffer :=
                      Get_Buffer (Assembly_View.View);
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Found       : Boolean := False;

   begin
      Trace (Me, "[Highlight_Address_Range] Source_Line = " &
             Visual_Debugger (Assembly_View.Process).Current_Line'Img);

--        if not In_Range
--          (Assembly_View.Source_Line_Start, Assembly_View.Current_Range)
--          or else not In_Range
--            (Assembly_View.Source_Line_End, Assembly_View.Current_Range)
--        then
--           On_Frame_Changed
--             (Assembly_View,
--              Assembly_View.Source_Line_Start,
--              Assembly_View.Source_Line_End);
--        end if;

      Freeze (Assembly_View.Buttons);

      if Assembly_View.Source_Line_Start /= Invalid_Address
        and then Assembly_View.Source_Line_End /= Invalid_Address
      then
         while not Found loop
            Iter_From_Address
              (Assembly_View, Assembly_View.Source_Line_Start, Start_Iter,
               Found);

            Iter_From_Address
              (Assembly_View, Assembly_View.Source_Line_End, End_Iter, Found);
         end loop;

         --  Highlight the new range
         Begin_User_Action (Buffer);
         Trace (Me, "[Highlight_Address_Range] " &
                "From Address = " &
                Address_To_String (Assembly_View.Source_Line_Start) &
                ", Line = " & Gint'Image (Get_Line (Start_Iter)) &
                " at Offset = " &
                Gint'Image (Get_Line_Offset (Start_Iter)) & ASCII.LF &
                "To Address = " &
                Address_To_String (Assembly_View.Source_Line_End) &
                ", Line = " & Gint'Image (Get_Line (End_Iter)) &
                " at Offset = " &
                Gint'Image (Get_Line_Offset (End_Iter)));

         Apply_Tag (Buffer, Assembly_View.Highlight_Tag, Start_Iter, End_Iter);
         End_User_Action (Buffer);
      end if;

      Thaw (Assembly_View.Buttons);
   end Highlight_Address_Range;

   -----------------------
   -- Iter_From_Address --
   -----------------------

   procedure Iter_From_Address
     (Assembly_View : GVD_Assembly_View;
      Address       : Address_Type;
      Iter          : out Gtk_Text_Iter;
      Found         : out Boolean)
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean := True;
   begin
      Get_Start_Iter (Buffer, Iter);

      while Result loop
         Copy (Iter, End_Iter);
         Forward_To_Line_End (End_Iter, Result);

         declare
            Line  : constant String := Get_Text (Buffer, Iter, End_Iter);
            Index : Natural := Line'First;
         begin
            Skip_Hexa_Digit (Line, Index);

            if
              String_To_Address (Line (Line'First .. Index - 1)) = Address
            then
               Trace (Me, "[Iter_From_Address] Address = """ &
                      Address_To_String (Address) & """ => " &
                      Get_Line (Iter)'Img);
               Found := True;
               return;
            end if;
         end;

         Forward_Line (Iter, Result);
      end loop;

      Result := False;
   end Iter_From_Address;

   -----------------------
   -- Address_From_Line --
   -----------------------

   function Address_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Address_Type
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean;
      Matched    : Match_Array (0 .. 1);

   begin
      Get_Iter_At_Line (Buffer, Start_Iter, Gint (Line));
      Copy (Start_Iter, End_Iter);
      Forward_To_Line_End (End_Iter, Result);

      declare
         Line_Text : constant String :=
                       Get_Text (Buffer, Start_Iter, End_Iter);
      begin
         --  ??? Regexp should depend on the debugger.
         --  It does not include any 0 right after "0x"
         Match ("^0x0*([0-9a-f]+)", Line_Text, Matched);

         if Matched (1) /= No_Match then
            return String_To_Address
              ("0x" & Line_Text (Matched (1).First .. Matched (1).Last));
         end if;
      end;

      return Invalid_Address;
   end Address_From_Line;

   ---------------------------
   -- Is_Breakpoint_Address --
   ---------------------------

   procedure Is_Breakpoint_Address
     (Assembly_View : GVD_Assembly_View;
      Addr          : Address_Type;
      Result        : out Boolean;
      Num           : out Breakpoint_Identifier)
   is
      Breakpoints_Array : constant GVD.Types.Breakpoint_Array_Ptr :=
                            Visual_Debugger
                              (Assembly_View.Process).Breakpoints;
   begin
      for Index in Breakpoints_Array'Range loop
         if Breakpoints_Array (Index).Address = Addr then
            Num := Breakpoints_Array (Index).Num;
            Result := True;
            return;
         end if;
      end loop;

      Result := False;
   end Is_Breakpoint_Address;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Assembly_View : GVD_Assembly_View;
      Br            : GVD.Types.Breakpoint_Array)
   is
      use Gtk.Widget.Widget_List;

      Iter  : Gtk_Text_Iter;
      Found : Boolean;
      Line  : Natural;
      Pix   : Gtk_Pixmap;
      First : Glist := Children (Assembly_View.Buttons);
      Tmp   : Glist := First;
   begin
      Freeze (Assembly_View.Buttons);

      --  Remove all existing breakpoints
      while Tmp /= Null_List loop
         if Get_Data (Tmp) /=
           Gtk_Widget (Assembly_View.Current_Line_Button)
         then
            Destroy (Get_Data (Tmp));
         end if;

         Tmp := Next (Tmp);
      end loop;

      Free (First);

      --  Add the new ones
      for B in Br'Range loop
         if Br (B).Address /= Invalid_Address then
            Iter_From_Address (Assembly_View, Br (B).Address, Iter, Found);

            if Found then
               Line := Natural (Get_Line (Iter));
               Gtk_New
                 (Pix, Assembly_View.Stop_Pixmap, Assembly_View.Stop_Mask);
               Put
                 (Assembly_View.Buttons, Pix,
                  0, Pixels_From_Line (Assembly_View, Line));
            end if;
         end if;
      end loop;

      Show_All (Assembly_View.Buttons);
      Thaw (Assembly_View.Buttons);
   end Update_Breakpoints;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Address : Address_Type;
      R       : Cache_Data_Access) return Boolean is
   begin
      return R /= null
        and then R.Low /= Invalid_Address
        and then R.High /= Invalid_Address
        and then Address >= R.Low
        and then Address <= R.High;
   end In_Range;

   -------------------
   -- Find_In_Cache --
   -------------------

   function Find_In_Cache
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type) return Cache_Data_Access
   is
      Tmp : Cache_Data_Access := Assembly_View.Cache;
   begin
      while Tmp /= null loop
         if In_Range (Pc, Tmp) then
            return Tmp;
         end if;

         Tmp := Tmp.Next;
      end loop;

      return null;
   end Find_In_Cache;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed (Assembly_View : GVD_Assembly_View) is
      Tmp : Cache_Data_Access;
   begin
      --  Clear the cache, since it is no longer valid.

      while Assembly_View.Cache /= null loop
         Tmp := Assembly_View.Cache.Next;
         Free (Assembly_View.Cache.Data);
         Assembly_View.Cache := Tmp;
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Executable_Changed;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu
     (Assembly_View : access GVD_Assembly_View_Record'Class)
   is
      pragma Unreferenced (Assembly_View);
   begin
      null;
   end Show_Current_Line_Menu;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Assembly_View : GVD_Assembly_View) is
   begin
      Set_Property
        (Assembly_View.Highlight_Tag,
         Foreground_Gdk_Property,
         Get_Pref (GVD_Prefs, Asm_Highlight_Color));
      Set_Font (Assembly_View, Get_Pref_Font (GVD_Prefs, Default_Style));

      Update_Display (Assembly_View);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Preferences_Changed;

   -----------------
   -- Meta_Scroll --
   -----------------

   procedure Meta_Scroll
     (Assembly_View : GVD_Assembly_View;
      Down          : Boolean)
   is
--        Pos      : Grange_Float;
   begin
      Trace (Me, "[Meta_Scroll]");
      if Assembly_View.Current_Range /= null
        and then Get_Pref (GVD_Prefs, Assembly_Range_Size) /= 0
      then
         Set_Busy
           (Visual_Debugger (Assembly_View.Process), True,
            Force_Refresh => True);

         if Down then
            declare
               Addr : constant Address_Type :=
                        Address_From_Line
                          (Assembly_View, Assembly_View.Current_Line);
--                 F    : constant Gdk_Font := From_Description
--                   (Get_Pref_Font (GVD_Prefs, Default_Style));
--                 Line  : Natural;
               Iter  : Gtk_Text_Iter;
               Found : Boolean;
            begin
               On_Frame_Changed
                 (Assembly_View, Invalid_Address, Invalid_Address);

               Iter_From_Address (Assembly_View, Addr, Iter, Found);

--                 Line := Natural (Get_Line (Iter));
--                 Set_Line (Assembly_View, Line);
--                 Pos := Grange_Float
--                   (Gint (Line) * (Get_Ascent (F) + Get_Descent (F)));
            end;
         elsif Assembly_View.Current_Range.High /= Invalid_Address then
--              Pos := Get_Upper (Get_Vadj (Get_Child (Box)))
--                - Get_Page_Size (Get_Vadj (Get_Child (Box)));
            On_Frame_Changed
              (Assembly_View,
               Assembly_View.Current_Range.High,
               String_To_Address
                 (Add_Address
                    (Address_To_String (Assembly_View.Current_Range.High),
                     Integer (Get_Pref (GVD_Prefs, Assembly_Range_Size)))));
         end if;

         if Down or else Assembly_View.Current_Range.High
           /= Invalid_Address
         then
            --  Scroll to the right position
            --              Set_Value (Get_Vadj (Get_Child (Box)), Pos);
            null;
         end if;

         --  Re-highlight the current range
         Highlight_Address_Range (Assembly_View);

         Set_Busy (Visual_Debugger (Assembly_View.Process), False);
      end if;
   end Meta_Scroll;

   -----------------
   -- Add_Address --
   -----------------

   function Add_Address
     (Addr   : String;
      Offset : Integer) return String
   is
      Convert : constant String := "0123456789abcdef";
      Buffer  : String (1 .. 32);
      Pos     : Natural := Buffer'Last;

   begin
      if Addr'Length < 2
        or else Addr (Addr'First .. Addr'First + 1) /= "0x"
      then
         return "0x0";
      end if;

      declare
         Str     : constant String :=
           "16#" & Addr (Addr'First + 2 .. Addr'Last) & '#';
         Value   : Long_Long_Integer := Long_Long_Integer'Value (Str) +
           Long_Long_Integer (Offset);
      begin
         while Value > 0 loop
            Buffer (Pos) := Convert (Integer (Value mod 16) + Convert'First);
            Pos := Pos - 1;
            Value := Value / 16;
         end loop;

         return "0x" & Buffer (Pos + 1 .. Buffer'Last);
      end;
   end Add_Address;

   ----------------------
   -- Meta_Scroll_Down --
   ----------------------

   procedure Meta_Scroll_Down
     (Assembly_View : access GVD_Assembly_View_Record'Class) is
   begin
      Meta_Scroll (GVD_Assembly_View (Assembly_View), Down => True);
   end Meta_Scroll_Down;

   --------------------
   -- Meta_Scroll_Up --
   --------------------

   procedure Meta_Scroll_Up
     (Assembly_View : access GVD_Assembly_View_Record'Class) is
   begin
      Meta_Scroll (GVD_Assembly_View (Assembly_View), Down => False);
   end Meta_Scroll_Up;

   ------------------
   -- Key_Press_Cb --
   ------------------

   function Key_Press_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Assembly_View);
--        Scroll : constant Gtk_Adjustment := Get_Vadj (Get_Child (Box));
   begin
      case Get_Key_Val (Event) is
--           when GDK_Page_Down =>
--              --  Only scroll if we are on the last page
--
--              if Get_Value (Scroll)
--                >= Get_Upper (Scroll) - Get_Page_Size (Scroll)
--              then
--                 Meta_Scroll (Box, Down => True);
--                 return True;
--              end if;
--
--           when GDK_Page_Up =>
--              --  Only scroll if we are on the first page
--
--              if Get_Value (Scroll) = 0.0 then
--                 Meta_Scroll (Box, Down => False);
--                 return True;
--              end if;

         when others => null;
      end case;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_Cb;

   ----------------------
   -- On_Frame_Changed --
   ----------------------

   procedure On_Frame_Changed
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type;
      End_Pc        : Address_Type)
   is
      Process               : constant Visual_Debugger :=
                                Visual_Debugger (Assembly_View.Process);
      S                     : String_Access;
      S2                    : String_Access;
      S3                    : String_Access;
      Start                 : Address_Type;
      Last                  : Address_Type;
      Low_Range, High_Range : Address_Type;
      Pc_In_Range           : constant Boolean :=
                                In_Range (Pc, Assembly_View.Current_Range);
      Pc_End_In_Range       : constant Boolean :=
                                In_Range (End_Pc, Assembly_View.Current_Range);
      S_First               : Natural;

   begin
      Trace (Me,
             "*** [On_Frame_Changed] *** " & ASCII.LF &
             "Pc = " & Address_To_String (Pc) & ", In_Range => " &
             Pc_In_Range'Img & ASCII.LF &
             "End_Pc = " & Address_To_String (End_Pc) & ", In_Range => " &
             Pc_End_In_Range'Img);

      --  Is the range already visible ?

      if Pc_In_Range and then Pc_End_In_Range then
         return;
      end if;

      Set_Busy (Process, True);

      --  Should we prepend to the current buffer ?
      if not Pc_In_Range and then Pc_End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => Pc,
            End_Address     => Assembly_View.Current_Range.Low);

         Assembly_View.Current_Range.Low := Pc;

         S2 := Assembly_View.Current_Range.Data;
         Assembly_View.Current_Range.Data := new String'
           (Do_Tab_Expansion (S.all, 8) & ASCII.LF & S2.all);
         Free (S2);

      --  Should we append to the current buffer ?
      elsif Pc_In_Range and then not Pc_End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => Assembly_View.Current_Range.High,
            End_Address     => Set_Offset (End_Pc, 1));

         Assembly_View.Current_Range.High := End_Pc;

         --  Avoid duplicating the first assembly line since it was already
         --  displayed.
         S_First := S'First;
         Skip_To_Char (S.all, S_First, ASCII.LF);
         S_First := S_First + 1;

         S2 := Assembly_View.Current_Range.Data;
         Assembly_View.Current_Range.Data := new String'
           (S2.all & ASCII.LF &
            Do_Tab_Expansion (S (S_First .. S'Last), 8));
         Free (S2);

      --  Else get a whole new range (minimum size Assembly_Range_Size)
      else
         Assembly_View.Current_Range := Find_In_Cache (Assembly_View, Pc);
         if Assembly_View.Current_Range = null then
            if Get_Pref (GVD_Prefs, Assembly_Range_Size) = 0
              or else End_Pc = Invalid_Address
            then
               Get_Machine_Code
                 (Process.Debugger,
                  Range_Start     => Start,
                  Range_End       => Last,
                  Code            => S);
            else
               Get_Machine_Code
                 (Process.Debugger,
                  Range_Start     => Start,
                  Range_End       => Last,
                  Code            => S,
                  Start_Address   => Pc,
                  End_Address     => Set_Offset
                    (Pc, Integer (Get_Pref (GVD_Prefs, Assembly_Range_Size))));
            end if;

            if Start /= Invalid_Address then
               Low_Range := Start;
            end if;

            if Last /= Invalid_Address then
               High_Range := Last;
            end if;

            --  If both are null, this means that gdb couldn't get the assembly
            --  at all, and there's no point in trying again afterwards.
            --  We just pretend things worked....

            if Start = Invalid_Address and then Last = Invalid_Address then
               Assembly_View.Cache := new Cache_Data'
                 (Low  => Pc,
                  High => Pc,
                  Data => new String'(-"Couldn't get assembly code"),
                  Next => Assembly_View.Cache);
            else

               --  If the end address is not visible, disassemble a little
               --  bit more...

               if High_Range /= Invalid_Address
                 and then End_Pc > High_Range
               then
                  Get_Machine_Code
                    (Process.Debugger,
                     Range_Start     => Start,
                     Range_End       => Last,
                     Code            => S2,
                     Start_Address   => High_Range,
                     End_Address     => Set_Offset (End_Pc, 1));
                  S3 := new String'(S.all & S2.all);
                  Free (S);
                  Free (S2);
                  S := S3;

                  if Last /= Invalid_Address then
                     High_Range := Last;
                  end if;
               end if;

               Assembly_View.Cache := new Cache_Data'
                 (Low  => Low_Range,
                  High => High_Range,
                  Data => new String'(Do_Tab_Expansion (S.all, 8)),
                  Next => Assembly_View.Cache);
            end if;
            Free (S);
            Assembly_View.Current_Range := Assembly_View.Cache;
         end if;
      end if;

      Set_Text (Assembly_View, Assembly_View.Current_Range.Data.all);
      Set_Busy (Process, False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Set_Busy (Process, False);
   end On_Frame_Changed;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Assembly_View : GVD_Assembly_View) is
      Buffer        : constant Gtk_Text_Buffer :=
                        Get_Buffer (Assembly_View.View);
      Start_Iter    : Gtk_Text_Iter;
      End_Iter      : Gtk_Text_Iter;
      Dummy_Boolean : Boolean;

      Address_Low  : Address_Type := Assembly_View.Source_Line_Start;
      Address_High : Address_Type := Assembly_View.Source_Line_End;
   begin
      Trace (Me, "[Update_Display] Pc => " &
             Address_To_String (Assembly_View.Pc) & ASCII.LF &
             "Line_Start => " &
             Address_To_String (Assembly_View.Source_Line_Start) &
             ASCII.LF &
             "Line_End => " &
             Address_To_String (Assembly_View.Source_Line_End));

      --  Restore the previous range to the default color
      Reset_Highlighting (Assembly_View);

      if Assembly_View.Pc /= Invalid_Address
        and then (Assembly_View.Source_Line_End = Invalid_Address
                  or else Assembly_View.Pc > Assembly_View.Source_Line_End)
      then
         Address_High := Assembly_View.Pc;
      elsif Assembly_View.Pc /= Invalid_Address
        and then (Assembly_View.Source_Line_Start /= Invalid_Address
                  or else Assembly_View.Pc < Assembly_View.Source_Line_Start)
      then
         Address_Low := Assembly_View.Pc;
      end if;

      Trace (Me, "[Update_Display] Address_Low => " &
             Address_To_String (Address_Low) & ASCII.LF &
             "Address_High => " & Address_To_String (Address_High));

      if not In_Range (Address_Low, Assembly_View.Current_Range)
        or else not In_Range (Address_High, Assembly_View.Current_Range)
      then
         On_Frame_Changed
           (Assembly_View,
            Address_Low,
            Address_High);
      end if;

      --  Highlight the range of addresses correponding to the current source
      --  line.

      Highlight_Address_Range (Assembly_View);

      --  Display the arrow showing the location of the program counter.

      Iter_From_Address
        (Assembly_View, Assembly_View.Pc, Start_Iter, Dummy_Boolean);
      Copy (Start_Iter, Dest => End_Iter);
      Forward_To_Line_End (End_Iter, Dummy_Boolean);
      Apply_Tag (Buffer, Assembly_View.Pc_Tag, Start_Iter, End_Iter);

      --  Make sure that the Pc line is visible
      Place_Cursor (Buffer, Start_Iter);
      Scroll_Mark_Onscreen (Assembly_View.View, Get_Insert (Buffer));

   end Update_Display;

end GVD.Assembly_View;
