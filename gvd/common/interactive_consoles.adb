-----------------------------------------------------------------------
--                               G P S                               --
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

with Glib;                use Glib;
with Glib.Convert;
with Glib.Values;         use Glib.Values;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;

with Gdk.Types;           use Gdk.Types;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gdk.Event;           use Gdk.Event;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Main;            use Gtk.Main;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Text_Tag;        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;  use Gtk.Text_Tag_Table;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Pango.Font;          use Pango.Font;
with Pango.Enums;         use Pango.Enums;

with System;               use System;
with Histories;            use Histories;

with Traces;               use Traces;
with Ada.Exceptions;       use Ada.Exceptions;
with GNAT.OS_Lib;          use GNAT.OS_Lib;

package body Interactive_Consoles is

   Me : constant Debug_Handle := Create ("Interactive_Consoles");
   package Console_Idle is new Gtk.Main.Idle (Interactive_Console);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Mark_Set_Handler
     (Console : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues);
   --  Prevent cursor movements before the prompt.

   function Button_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handler for the "button_press_event" signal.

   function Button_Release_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handler for the "button_release_event" signal.

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "key_press_event" signal.

   function Delete_Event_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "delete_event" signal.

   procedure Replace_Cursor (Console : Interactive_Console);
   --  If the cursor is in a forbidden zone, place it at the prompt.

   function Place_Cursor_At_Prompt
     (Console : Interactive_Console) return Boolean;
   --  Place the cursor at the prompt mark.

   procedure Destroy_Idle (Console : in out Interactive_Console);
   --  Destroy handler for idle callbacks.

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class);
   --  Called when the console is destroyed.

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Console : in out Interactive_Console) is
   begin
      Console.Idle_Registered := False;
   end Destroy_Idle;

   ---------------------------
   -- Enable_Prompt_Display --
   ---------------------------

   procedure Enable_Prompt_Display
     (Console : access Interactive_Console_Record;
      Enable  : Boolean)
   is
      Last_Iter : Gtk_Text_Iter;
   begin
      Set_Editable (Console.View, Enable);
      Set_Cursor_Visible (Console.View, Enable);
      Console.Input_Blocked := not Enable;

      if Enable and then Console.Message_Was_Displayed then
         Display_Prompt (Console);

         if Console.User_Input /= null then
            Get_End_Iter (Console.Buffer, Last_Iter);
            Insert (Console.Buffer, Last_Iter, Console.User_Input.all);

            Free (Console.User_Input);
         end if;

         Console.Message_Was_Displayed := False;
      end if;
   end Enable_Prompt_Display;

   ----------------
   -- Grab_Focus --
   ----------------

   procedure Grab_Focus (Console : access Interactive_Console_Record) is
   begin
      Grab_Focus (Console.View);
   end Grab_Focus;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Console        : access Interactive_Console_Record;
      Text           : String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Add_To_History : Boolean := False)
   is
      UTF8        : constant String := Glib.Convert.Locale_To_UTF8 (Text);
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;
      Success     : Boolean;
      Internal    : constant Boolean := Console.Internal_Insert;

   begin
      Console.Internal_Insert := True;

      Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Console.Buffer, Last_Iter);

      if Console.User_Input = null then
         Console.User_Input := new String'
           (Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter));
      end if;

      if Add_LF then
         Insert (Console.Buffer, Last_Iter, UTF8 & ASCII.LF);
      else
         Insert (Console.Buffer, Last_Iter, UTF8);
      end if;

      if Add_To_History and then Console.History /= null then
         if UTF8 (UTF8'Last) = ASCII.LF then
            Histories.Add_To_History
              (Console.History.all, History_Key (Console.Key.all),
               UTF8 (UTF8'First .. UTF8'Last - 1));
         else
            Histories.Add_To_History
              (Console.History.all, History_Key (Console.Key.all), UTF8);
         end if;
      end if;

      Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Console.Buffer, Last_Iter);

      Apply_Tag
        (Console.Buffer, Console.Uneditable_Tag, Prompt_Iter, Last_Iter);

      Forward_Chars (Prompt_Iter, Console.User_Input'Length, Success);
      Apply_Tag
        (Console.Buffer,
         Console.External_Messages_Tag,
         Prompt_Iter, Last_Iter);

      if Highlight then
         Apply_Tag
           (Console.Buffer,
            Console.Highlight_Tag,
            Prompt_Iter, Last_Iter);
      end if;

      Display_Prompt (Console);

      Console.Message_Was_Displayed := True;
      Console.Internal_Insert := Internal;
   end Insert;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable
     (Console : access Interactive_Console_Record) return Boolean is
   begin
      return not Console.Input_Blocked;
   end Is_Editable;

   --------------------------
   -- Button_Press_Handler --
   --------------------------

   function Button_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);

      Console : constant Interactive_Console := Interactive_Console (Object);
   begin
      Console.Button_Press := True;
      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press_Handler;

   ----------------------------
   -- Button_Release_Handler --
   ----------------------------

   function Button_Release_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      pragma Unreferenced (Params);

      Console : constant Interactive_Console := Interactive_Console (Object);

   begin
      Console.Button_Press := False;
      Replace_Cursor (Console);

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Release_Handler;

   --------------------------
   -- Delete_Event_Handler --
   --------------------------

   function Delete_Event_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);

      Console : constant Interactive_Console := Interactive_Console (Object);
   begin
      if Console.Idle_Registered then
         Idle_Remove (Console.Idle_Id);
      end if;

      --  When a Gtk_Text_View is deleted, a "mark_set" signal is sent
      --  internally, which causes an additional idle callback to be
      --  registered through Mark_Set_Handler.
      --  The following two lines are used to prevent adding unnecessary
      --  idle callbacks when we are deleting the console.
      Console.Internal_Insert := True;
      Console.Idle_Registered := True;

      return False;
   end Delete_Event_Handler;

   -----------------------
   -- Key_Press_Handler --
   -----------------------

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Console : constant Interactive_Console := Interactive_Console (Object);

      Key         : constant Gdk_Key_Type  := Get_Key_Val (Event);
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;
      Success     : Boolean;

   begin
      case Key is
         when GDK_Up =>
            if Console.History /= null then
               declare
                  Hist : constant GNAT.OS_Lib.String_List_Access := Get_History
                    (Console.History.all, History_Key (Console.Key.all));
               begin
                  if Hist /= null
                    and then Console.Current_Position + Hist'First < Hist'Last
                  then
                     Console.Current_Position := Console.Current_Position + 1;

                     Get_Iter_At_Mark
                       (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                     Get_End_Iter (Console.Buffer, Last_Iter);
                     Delete (Console.Buffer, Prompt_Iter, Last_Iter);

                     Get_Iter_At_Mark
                       (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                     Insert
                       (Console.Buffer,
                        Prompt_Iter,
                        Hist (Hist'First + Console.Current_Position).all);
                     Get_End_Iter (Console.Buffer, Last_Iter);
                     Place_Cursor (Console.Buffer, Last_Iter);
                  end if;
               end;
            end if;
            return True;

         when GDK_Down =>
            if Console.History /= null
              and then Console.Current_Position /= -1
            then
               declare
                  Hist : constant GNAT.OS_Lib.String_List_Access := Get_History
                    (Console.History.all, History_Key (Console.Key.all));
               begin
                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Get_End_Iter (Console.Buffer, Last_Iter);
                  Delete (Console.Buffer, Prompt_Iter, Last_Iter);

                  Console.Current_Position := Console.Current_Position - 1;

                  if Console.Current_Position /= -1 then
                     Get_Iter_At_Mark
                       (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                     Insert
                       (Console.Buffer,
                        Prompt_Iter,
                        Hist (Hist'First + Console.Current_Position).all);
                     Get_End_Iter (Console.Buffer, Last_Iter);
                     Place_Cursor (Console.Buffer, Last_Iter);
                  end if;
               end;
            end if;
            return True;

         when GDK_Tab | GDK_KP_Tab =>
            if Console.Completion = null then
               return False;
            end if;

            Get_Iter_At_Mark
              (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
            Get_End_Iter (Console.Buffer, Last_Iter);

            declare
               use String_List_Utils.String_List;
               Text        : constant String :=
                 Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter);
               Completions : List :=
                 Console.Completion (Text, Console.User_Data);
               Prefix      : constant String := Longest_Prefix (Completions);
               Node        : List_Node;
               Line        : Gint;
               Offset      : Gint;
               More_Than_One : constant Boolean :=
                 Completions /= Null_List
                 and then Next (First (Completions)) /= Null_Node;
               Success     : Boolean;
               Prompt_Iter : Gtk_Text_Iter;
               Prev_Begin  : Gtk_Text_Iter;
               Prev_Last   : Gtk_Text_Iter;
               Pos         : Gtk_Text_Iter;
            begin
               if More_Than_One then
                  Node := First (Completions);

                  --  Get the range copy the current line.

                  Get_End_Iter (Console.Buffer, Pos);
                  Line := Get_Line (Pos);

                  --  Get the offset of the prompt.

                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Offset := Get_Line_Offset (Prompt_Iter);

                  Get_End_Iter (Console.Buffer, Pos);
                  Insert (Console.Buffer, Pos, "" & ASCII.LF);

                  while Node /= Null_Node loop
                     Get_End_Iter (Console.Buffer, Pos);
                     Set_Line_Offset (Pos, 0);
                     Insert (Console.Buffer, Pos, Data (Node) & ASCII.LF);
                     Node := Next (Node);
                  end loop;

                  Get_Iter_At_Line_Offset
                    (Console.Buffer, Prev_Begin, Line, 0);
                  Copy (Prev_Begin, Prev_Last);
                  Forward_To_Line_End (Prev_Last, Success);

                  Get_End_Iter (Console.Buffer, Pos);
                  Insert_Range (Console.Buffer, Pos, Prev_Begin, Prev_Last);

                  Get_End_Iter (Console.Buffer, Pos);
                  Set_Line_Offset (Pos, 0);

                  Get_Iter_At_Line_Offset
                    (Console.Buffer, Prev_Begin, Line, 0);
                  Apply_Tag
                    (Console.Buffer, Console.Uneditable_Tag, Prev_Begin, Pos);

                  --  Restore the prompt

                  Get_End_Iter (Console.Buffer, Prompt_Iter);
                  Set_Line_Offset (Prompt_Iter, Offset);

                  Move_Mark (Console.Buffer, Console.Prompt_Mark, Prompt_Iter);
                  Scroll_Mark_Onscreen (Console.View, Console.Prompt_Mark);
               end if;

               --  Insert the completion, if any.
               Get_End_Iter (Console.Buffer, Pos);

               if Prefix'Length > Text'Length then
                  Insert (Console.Buffer, Pos,
                          Prefix (Prefix'First + Text'Length .. Prefix'Last));
               end if;

               if not More_Than_One then
                  Insert (Console.Buffer, Pos, " ");
               end if;

               Console.Internal_Insert := False;

               Free (Completions);
            end;

            return True;

         when GDK_Return =>
            if Console.Input_Blocked then
               return True;
            end if;

            Get_End_Iter (Console.Buffer, Last_Iter);
            Insert (Console.Buffer, Last_Iter, ASCII.LF & "");

            if Console.Handler = null then
               return True;
            end if;

            Get_End_Iter (Console.Buffer, Last_Iter);
            Get_Iter_At_Mark
              (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);

            Backward_Char (Last_Iter, Success);

            declare
               Command : GNAT.OS_Lib.String_Access := new String'
                 (Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter));

               Output  : GNAT.OS_Lib.String_Access;
            begin
               if Command.all = ""
                 and then Console.Empty_Equals_Repeat
                 and then Console.History /= null
               then
                  declare
                     H : constant String_List_Access :=
                       Get_History
                         (Console.History.all, History_Key (Console.Key.all));
                  begin
                     if H /= null
                       and then H (H'First) /= null
                     then
                        Free (Command);
                        Command := new String'
                          (H (H'First + Console.Current_Position + 1).all);
                     end if;
                  end;

                  Insert (Console.Buffer, Last_Iter, Command.all);
               end if;

               Output := new String'
                 (Console.Handler (Command.all, Console.User_Data));

               Get_End_Iter (Console.Buffer, Last_Iter);

               Insert (Console.Buffer, Last_Iter, Output.all);

               if Command.all /= "" and then Console.History /= null then
                  Add_To_History
                    (Console.History.all,
                     History_Key (Console.Key.all), Command.all);
                  Console.Current_Position := -1;
               end if;

               Get_Iter_At_Mark
                 (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
               Get_End_Iter (Console.Buffer, Last_Iter);

               Apply_Tag
                 (Console.Buffer,
                  Console.Uneditable_Tag, Prompt_Iter, Last_Iter);

               Display_Prompt (Console);

               Free (Command);
               Free (Output);
            end;

            return True;

         when others =>
            return False;
      end case;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_Handler;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Console : access Interactive_Console_Record'Class)
   is
      First_Iter  : Gtk_Text_Iter;
      Prompt_Iter : Gtk_Text_Iter;

      Offset : Gint;
   begin
      Get_End_Iter (Console.Buffer, First_Iter);
      Offset := Get_Offset (First_Iter);

      Insert (Console.Buffer, First_Iter, Console.Prompt.all);

      Get_End_Iter (Console.Buffer, Prompt_Iter);
      Get_Iter_At_Offset (Console.Buffer, First_Iter, Offset);

      Apply_Tag
        (Console.Buffer, Console.Uneditable_Tag, First_Iter, Prompt_Iter);
      Apply_Tag
        (Console.Buffer, Console.Prompt_Tag, First_Iter, Prompt_Iter);

      Move_Mark (Console.Buffer, Console.Prompt_Mark, Prompt_Iter);

      Scroll_Mark_Onscreen (Console.View, Console.Prompt_Mark);
   end Display_Prompt;

   ----------------------------
   -- Place_Cursor_At_Prompt --
   ----------------------------

   function Place_Cursor_At_Prompt
     (Console : Interactive_Console) return Boolean
   is
      Prompt_Iter : Gtk_Text_Iter;
      Cursor_Iter : Gtk_Text_Iter;

   begin
      if Selection_Exists (Console.Buffer) then
         return False;
      end if;

      Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_Iter_At_Mark (Console.Buffer, Cursor_Iter, Console.Insert_Mark);

      if Compare (Cursor_Iter, Prompt_Iter) < 0 then
         Place_Cursor (Console.Buffer, Prompt_Iter);
         Console.Insert_Mark := Get_Insert (Console.Buffer);
      end if;

      return False;
   end Place_Cursor_At_Prompt;

   --------------------
   -- Replace_Cursor --
   --------------------

   procedure Replace_Cursor (Console : Interactive_Console) is
   begin
      if Console.Idle_Registered then
         return;
      end if;

      Console.Idle_Registered := True;

      Console.Idle_Id :=
        Console_Idle.Add
          (Place_Cursor_At_Prompt'Access,
           Console,
           Destroy => Destroy_Idle'Access);
   end Replace_Cursor;

   ----------------------
   -- Mark_Set_Handler --
   ----------------------

   procedure Mark_Set_Handler
     (Console : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues)
   is
      C         : constant Interactive_Console :=
        Interactive_Console (Console);
      Mark      : constant Gtk_Text_Mark :=
        Get_Text_Mark (Glib.Values.Nth (Params, 2));
      Mark_Name : constant String := Get_Name (Mark);

   begin
      --  Prevent recursion

      if C.Internal_Insert then
         return;
      end if;

      C.Internal_Insert := True;

      if C.Insert_Mark = null
        or else Get_Object (Mark) /= Get_Object (C.Insert_Mark)
      then
         --  If the mark corresponds to a cursor position, set the stored
         --  Insert_Mark accordingly.

         if Mark_Name = "insert"
           or else Mark_Name = "gtk_drag_target"
         then
            C.Insert_Mark := Mark;
         end if;
      end if;

      if not C.Button_Press then
         Replace_Cursor (C);
      end if;

      C.Internal_Insert := False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Mark_Set_Handler;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class) is
      C : constant Interactive_Console := Interactive_Console (Console);
   begin
      Unref (C.Uneditable_Tag);
      Unref (C.Prompt_Tag);
      Unref (C.Highlight_Tag);
      Unref (C.External_Messages_Tag);
      Free (C.Prompt);
      Free (C.User_Input);
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Console   : out Interactive_Console;
      Prompt    : String;
      Handler   : Command_Handler;
      User_Data : GObject;
      Font      : Pango.Font.Pango_Font_Description;
      History_List : Histories.History;
      Key          : Histories.History_Key;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode := Gtk.Enums.Wrap_None;
      Empty_Equals_Repeat : Boolean := False) is
   begin
      Console := new Interactive_Console_Record;
      Initialize (Console, Prompt, Handler, User_Data, Font,
                  History_List, Key, Wrap_Mode,
                  Empty_Equals_Repeat);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console   : access Interactive_Console_Record'Class;
      Prompt    : String;
      Handler   : Command_Handler;
      User_Data : GObject;
      Font      : Pango.Font.Pango_Font_Description;
      History_List : Histories.History;
      Key          : Histories.History_Key;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode;
      Empty_Equals_Repeat : Boolean := False)
   is
      Iter : Gtk_Text_Iter;
   begin
      --  Initialize the text buffer and the text view.

      Console.Prompt := new String'(Prompt);
      Console.Handler := Handler;
      Console.User_Data := User_Data;
      Console.Key := new String'(String (Key));
      Console.History := History_List;

      Gtk.Scrolled_Window.Initialize (Console);
      Set_Policy
        (Console,
         H_Scrollbar_Policy => Policy_Automatic,
         V_Scrollbar_Policy => Policy_Automatic);

      Gtk_New (Console.Buffer);
      Gtk_New (Console.View, Console.Buffer);
      Set_Wrap_Mode (Console.View, Wrap_Mode);

      --  The buffer should be destroyed when the view is destroyed
      --  ??? Perhaps we should store it in the module_id, and always reuse it
      --  when the console is created. This allows the user to destroy the
      --  console without losing its contents
      Unref (Console.Buffer);

      Gtk_New (Console.Uneditable_Tag);
      Set_Property (Console.Uneditable_Tag, Editable_Property, False);
      Add (Get_Tag_Table (Console.Buffer), Console.Uneditable_Tag);

      Gtk_New (Console.External_Messages_Tag);
      Set_Property
        (Console.External_Messages_Tag,
         Gtk.Text_Tag.Style_Property,
         Pango_Style_Normal);
      Add (Get_Tag_Table (Console.Buffer), Console.External_Messages_Tag);

      Gtk_New (Console.Highlight_Tag);
      Add (Get_Tag_Table (Console.Buffer), Console.Highlight_Tag);

      Gtk_New (Console.Prompt_Tag);
      Set_Property
        (Console.Prompt_Tag,
         Gtk.Text_Tag.Font_Desc_Property,
         Font);

      Add (Get_Tag_Table (Console.Buffer), Console.Prompt_Tag);

      Add (Console, Console.View);

      Console.Internal_Insert := True;

      Set_Size_Request (Console, -1, 100);

      Modify_Font (Console.View, Font);

      Widget_Callback.Connect
        (Console, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));

      Widget_Callback.Object_Connect
        (Console.Buffer, "mark_set",
         Cb => Mark_Set_Handler'Access,
         Slot_Object => Console);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Console.View, "button_release_event",
         Button_Release_Handler'Access,
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Console.View, "button_press_event",
         Button_Press_Handler'Access,
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Console.View, "key_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Key_Press_Handler'Access),
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Return_Callback.Connect
        (Console, "delete_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Delete_Event_Handler'Access),
         After => False);

      Get_End_Iter (Console.Buffer, Iter);

      Console.Prompt_Mark := Create_Mark (Console.Buffer, "", Iter);
      Console.Insert_Mark := Get_Insert (Console.Buffer);

      Console.Internal_Insert := False;

      Display_Prompt (Console);
      Console.Empty_Equals_Repeat := Empty_Equals_Repeat;
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear (Console : access Interactive_Console_Record) is
      Start, The_End : Gtk_Text_Iter;
   begin
      Get_Bounds (Console.Buffer, Start, The_End);
      Delete (Console.Buffer, Start, The_End);
   end Clear;

   ----------------------------
   -- Set_Completion_Handler --
   ----------------------------

   procedure Set_Completion_Handler
     (Console : access Interactive_Console_Record'Class;
      Handler : Completion_Handler) is
   begin
      Console.Completion := Handler;
   end Set_Completion_Handler;

   -------------------------
   -- Set_Highlight_Color --
   -------------------------

   procedure Set_Highlight_Color
     (Console : access Interactive_Console_Record'Class;
      Color   : Gdk_Color) is
   begin
      Set_Property (Console.Highlight_Tag, Foreground_Gdk_Property, Color);
   end Set_Highlight_Color;

   ---------------
   -- Get_Chars --
   ---------------

   function Get_Chars
     (Console : access Interactive_Console_Record)
     return String
   is
      Start, The_End : Gtk_Text_Iter;
   begin
      Get_Bounds (Console.Buffer, Start, The_End);
      return Get_Text (Start, The_End);
   end Get_Chars;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Console : access Interactive_Console_Record)
      return GNAT.OS_Lib.String_List_Access is
   begin
      if Console.History = null then
         return null;
      else
         return Get_History
           (Console.History.all, History_Key (Console.Key.all));
      end if;
   end Get_History;

end Interactive_Consoles;
