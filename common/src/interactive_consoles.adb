-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                              AdaCore                              --
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

with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with System;              use System;

with GNAT.OS_Lib;         use GNAT.OS_Lib;

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
with Gtk.Object;          use Gtk.Object;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Text_Tag;        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;  use Gtk.Text_Tag_Table;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;
with Gtk.Selection;       use Gtk.Selection;
with Gtk.Arguments;       use Gtk.Arguments;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Pango.Font;          use Pango.Font;
with Pango.Enums;         use Pango.Enums;

with Traces;              use Traces;
with Histories;           use Histories;
with String_Utils;        use String_Utils;
with GUI_Utils;           use GUI_Utils;

package body Interactive_Consoles is

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
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "button_press_event" signal.

   function Button_Release_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handler for the "button_release_event" signal.

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "key_press_event" signal.

   procedure Selection_Received_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler for the "selection_received" signal.

   procedure Size_Allocate_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler for the "size_allocate" signal.

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

   procedure Insert_And_Execute
     (Object : access Gtk_Widget_Record'Class;
      Text   : String);
   --  Interpret "Text", an ASCII.LF separated list of commands that are
   --  sequentially displayed, executed and stored in the console's history

   procedure Execute_Command
     (Console : Interactive_Console;
      Command : String);
   --  Execute Command, store it in the history, and display its result
   --  in the console

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
   begin
      Insert_UTF8 (Console, UTF8, Add_LF, Highlight, Add_To_History);
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   procedure Insert_UTF8
     (Console        : access Interactive_Console_Record;
      UTF8           : Glib.UTF8_String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Add_To_History : Boolean := False)
   is
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
   end Insert_UTF8;

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
      Event  : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Console : constant Interactive_Console := Interactive_Console (Object);
   begin
      Console.Button_Press := True;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
         return False;
   end Button_Release_Handler;

   ---------------------------
   -- Size_Allocate_Handler --
   ---------------------------

   procedure Size_Allocate_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Alloc   : Gtk_Allocation_Access;
      Console : constant Interactive_Console := Interactive_Console (Widget);
   begin
      --  The purpose of this callback is to workaround a bug in Gtk+: when
      --  the size of this console is too small, an infinite loop can occur,
      --  with Gtk+ hesitating between displaying either the horizontal or the
      --  vertical scrollbar, the presence of one causing the other one to be
      --  removed, causing it to be also removed, and back again.
      --
      --  To fix this, we change the policy of the scrollbars to Always when
      --  the width is too small.
      Alloc := Get_Allocation (Nth (Params, 1));

      --  The value 25 here is hoped to be large enough to be greater than the
      --  width of one scrollbar in any theme.

      if Alloc.Width < 25 then
         Set_Policy (Console, Policy_Always, Policy_Always);
      else
         Set_Policy (Console, Policy_Automatic, Policy_Automatic);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Size_Allocate_Handler;

   --------------------------------
   -- Selection_Received_Handler --
   --------------------------------

   procedure Selection_Received_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Args : constant Gtk_Args := Gtk_Args (Params);
      Data : constant Selection_Data := Selection_Data (To_C_Proxy (Args, 1));
   begin
      if Get_Length (Data) > 0 then
         Insert_And_Execute (Widget, Strip_CR (Get_Data_As_String (Data)));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Selection_Received_Handler;

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

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Delete_Event_Handler;

   -----------------------
   -- Key_Press_Handler --
   -----------------------

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Console     : constant Interactive_Console :=
                      Interactive_Console (Object);
      Key         : constant Gdk_Key_Type  := Get_Key_Val (Event);
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;
      Success     : Boolean;

   begin
      case Key is
         when GDK_Up | GDK_Down =>
            if Console.Input_Blocked or else Console.Waiting_For_Input then
               return True;
            end if;

            declare
               Hist : constant String_List_Access := Get_History
                 (Console.History.all, History_Key (Console.Key.all));
            begin
               if Hist /= null then
                  if Key = GDK_Up
                    and then
                    Console.Current_Position + Hist'First < Hist'Last
                  then
                     Console.Current_Position := Console.Current_Position + 1;

                  elsif Key = GDK_Down
                    and then Console.Current_Position /= -1
                  then
                     Console.Current_Position :=
                       Console.Current_Position - 1;
                  end if;

                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Get_End_Iter (Console.Buffer, Last_Iter);
                  Delete (Console.Buffer, Prompt_Iter, Last_Iter);
                  if Console.Current_Position /= -1 then
                     Insert
                       (Console.Buffer, Prompt_Iter,
                        Hist (Hist'First + Console.Current_Position).all);
                  end if;

                  Get_End_Iter (Console.Buffer, Prompt_Iter);
                  Place_Cursor (Console.Buffer, Prompt_Iter);
                  Success := Scroll_To_Iter
                    (Console.View,
                     Iter          => Prompt_Iter,
                     Within_Margin => 0.0,
                     Use_Align     => False,
                     Xalign        => 0.0,
                     Yalign        => 0.0);
               end if;
            end;
            return True;

         when GDK_Tab | GDK_KP_Tab =>
            if Console.Completion = null or else Console.Waiting_For_Input then
               return False;
            else
               Do_Completion
                 (View            => Console.View,
                  Completion      => Console.Completion,
                  Prompt_End_Mark => Console.Prompt_Mark,
                  Uneditable_Tag  => Console.Uneditable_Tag,
                  User_Data       => Console.User_Data);
               Console.Internal_Insert := False;
               return True;
            end if;

         when GDK_C =>
            if not Console.Waiting_For_Input
              and then Console.Interrupt /= null
              and then Get_State (Event) = (Control_Mask or Shift_Mask)
            then
               return Console.Interrupt (Console, Console.User_Data);
            else
               return False;
            end if;

         when GDK_Return | GDK_KP_Enter =>
            if Console.Input_Blocked then
               return True;
            end if;

            Get_End_Iter (Console.Buffer, Last_Iter);
            Insert (Console.Buffer, Last_Iter, (1 => ASCII.LF));

            if Console.Waiting_For_Input then
               Gtk.Main.Main_Quit;
               return True;
            end if;

            if Console.Handler = null then
               return True;
            end if;

            Get_Iter_At_Mark
              (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
            Get_End_Iter (Console.Buffer, Last_Iter);
            Backward_Char (Last_Iter, Success);

            declare
               Command : constant String :=
                           Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter);
               H       : String_List_Access;
            begin
               if Command = ""
                 and then Console.Empty_Equals_Repeat
                 and then Console.History /= null
               then
                  if not Console.Command_Received then
                     Display_Prompt (Console);
                     return True;
                  end if;

                  H := Get_History
                    (Console.History.all, History_Key (Console.Key.all));

                  if H /= null
                    and then H (H'First) /= null
                  then
                     Insert
                       (Console.Buffer, Last_Iter,
                        H (H'First + Console.Current_Position + 1).all);
                     Execute_Command
                       (Console,
                        H (H'First + Console.Current_Position + 1).all);
                     return True;
                  end if;
               end if;

               Execute_Command (Console, Command);
               Console.Command_Received := True;
            end;

            return True;

         when others =>
            return False;
      end case;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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

      if Gtk.Object.In_Destruction_Is_Set (Console) then
         return;
      end if;

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
      when E : others => Trace (Exception_Handle, E);
   end Mark_Set_Handler;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class) is
      C : constant Interactive_Console := Interactive_Console (Console);
   begin
      if C.Idle_Registered then
         Idle_Remove (C.Idle_Id);
      end if;

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
     (Console             : out Interactive_Console;
      Prompt              : String;
      Handler             : Command_Handler;
      User_Data           : System.Address;
      Font                : Pango.Font.Pango_Font_Description;
      History_List        : Histories.History;
      Key                 : Histories.History_Key;
      Highlight           : Gdk_Color := Null_Color;
      Wrap_Mode           : Gtk.Enums.Gtk_Wrap_Mode := Gtk.Enums.Wrap_None;
      Empty_Equals_Repeat : Boolean := False) is
   begin
      Console := new Interactive_Console_Record;
      Initialize (Console, Prompt, Handler, User_Data, Font,
                  History_List, Key, Highlight, Wrap_Mode,
                  Empty_Equals_Repeat);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console             : access Interactive_Console_Record'Class;
      Prompt              : String;
      Handler             : Command_Handler;
      User_Data           : System.Address;
      Font                : Pango.Font.Pango_Font_Description;
      History_List        : Histories.History;
      Key                 : Histories.History_Key;
      Highlight           : Gdk_Color := Null_Color;
      Wrap_Mode           : Gtk.Enums.Gtk_Wrap_Mode;
      Empty_Equals_Repeat : Boolean := False)
   is
      Iter : Gtk_Text_Iter;
   begin
      --  Initialize the text buffer and the text view

      Console.Prompt := new String'(Prompt);
      Set_Command_Handler (Console, Handler, User_Data);
      Console.Key := new String'(String (Key));
      Console.History := History_List;
      Console.Highlight := Highlight;

      Gtk.Scrolled_Window.Initialize (Console);
      Set_Policy
        (Console,
         H_Scrollbar_Policy => Policy_Automatic,
         V_Scrollbar_Policy => Policy_Automatic);

      Gtk_New (Console.Buffer);
      Gtk_New (Console.View, Console.Buffer);
      Set_Wrap_Mode (Console.View, Wrap_Mode);

      Set_Left_Margin (Console.View, 4);

      --  The buffer should be destroyed when the view is destroyed
      --  ??? Perhaps we should store it in the module_id, and always reuse it
      --  when the console is created. This allows the user to destroy the
      --  console without losing its contents
      Unref (Console.Buffer);

      Gtk_New (Console.Uneditable_Tag);
      Set_Property
        (Console.Uneditable_Tag, Gtk.Text_Tag.Editable_Property, False);
      Add (Get_Tag_Table (Console.Buffer), Console.Uneditable_Tag);

      Gtk_New (Console.External_Messages_Tag);
      Set_Property
        (Console.External_Messages_Tag,
         Gtk.Text_Tag.Style_Property,
         Pango_Style_Normal);
      Add (Get_Tag_Table (Console.Buffer), Console.External_Messages_Tag);

      Gtk_New (Console.Highlight_Tag);

      if Console.Highlight /= Null_Color then
         Set_Property
           (Console.Highlight_Tag, Foreground_Gdk_Property,
            Console.Highlight);
      end if;

      Add (Get_Tag_Table (Console.Buffer), Console.Highlight_Tag);

      Gtk_New (Console.Prompt_Tag);
      Set_Property
        (Console.Prompt_Tag,
         Gtk.Text_Tag.Font_Desc_Property,
         Font);

      Add (Get_Tag_Table (Console.Buffer), Console.Prompt_Tag);

      Add (Console, Console.View);

      Console.Internal_Insert := True;

      Modify_Font (Console.View, Font);

      Widget_Callback.Connect (Console, Signal_Destroy, On_Destroy'Access);

      Widget_Callback.Object_Connect
        (Console.Buffer, Signal_Mark_Set,
         Cb          => Mark_Set_Handler'Access,
         Slot_Object => Console);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Console.View, Signal_Button_Release_Event,
         Button_Release_Handler'Access,
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Console.View, Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Button_Press_Handler'Access),
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Console.View, Signal_Selection_Received,
         Selection_Received_Handler'Access,
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Console.View, Signal_Size_Allocate,
         Size_Allocate_Handler'Access,
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Console.View, Signal_Key_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Key_Press_Handler'Access),
         Gtk_Widget (Console),
         After => False);

      Gtkada.Handlers.Return_Callback.Connect
        (Console, Signal_Delete_Event,
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

   -------------------------
   -- Set_Command_Handler --
   -------------------------

   procedure Set_Command_Handler
     (Console   : access Interactive_Console_Record'Class;
      Handler   : Command_Handler;
      User_Data : System.Address) is
   begin
      Console.Handler := Handler;
      Console.User_Data := User_Data;
   end Set_Command_Handler;

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

   ---------------------------
   -- Set_Interrupt_Handler --
   ---------------------------

   procedure Set_Interrupt_Handler
     (Console : access Interactive_Console_Record'Class;
      Handler : Interrupt_Handler) is
   begin
      Console.Interrupt := Handler;
   end Set_Interrupt_Handler;

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
     (Console : access Interactive_Console_Record) return String
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
      return GNAT.Strings.String_List_Access is
   begin
      if Console.History = null then
         return null;
      else
         return Get_History
           (Console.History.all, History_Key (Console.Key.all));
      end if;
   end Get_History;

   ------------------------
   -- Insert_And_Execute --
   ------------------------

   procedure Insert_And_Execute
     (Object : access Gtk_Widget_Record'Class;
      Text   : String)
   is
      procedure Get_Next_Command
        (Text     : String;
         Start_At : in out Natural;
         Command  : in out GNAT.Strings.String_Access);
      --  Look into Text, starting at Start_At, for the next command
      --  Commands are separated by ASCII.LF and trimmed

      Console   : constant Interactive_Console := Interactive_Console (Object);
      Last_Iter : Gtk_Text_Iter;
      Command   : GNAT.Strings.String_Access;
      Start_At  : Natural := Text'First;

      ----------------------
      -- Get_Next_Command --
      ----------------------

      procedure Get_Next_Command
        (Text     : String;
         Start_At : in out Natural;
         Command  : in out GNAT.Strings.String_Access)
      is
         End_At : Natural := Start_At - 1;
      begin
         --  Look for end-of-line

         for J in Start_At .. Text'Last loop
            if Text (J) = ASCII.LF then
               End_At := J;
               exit;
            end if;
         end loop;

         if End_At < Start_At then
            End_At := Text'Last;
         end if;

         --  Do not trim blank spaces at the beginning of the line, since
         --  they might be relevant in some contexts (for instance python)
         Command := new String'
           (Trim (Text (Start_At .. End_At), Ada.Strings.Right));

         Start_At := End_At + 1;
      end Get_Next_Command;

   begin
      if Console.Input_Blocked then
         return;
      end if;

      while Start_At <= Text'Last loop
         Get_Next_Command (Text, Start_At, Command);

         --  We also execute empty commands, since they might be relevant
         --  in some contexts (python for instance)
         if Command /= null and then Command.all /= "" then
            Get_End_Iter (Console.Buffer, Last_Iter);
            Insert (Console.Buffer, Last_Iter, Command.all);

            --  Execute only if Command ends with a Line Feed.
            --  ??? Should take into account input that might already be in
            --  the console

            if Console.Handler /= null
              and then Command (Command'Last) = ASCII.LF
            then
               Execute_Command
                 (Console,
                  Command (Command'First .. Command'Last - 1));
            end if;
         end if;

         Free (Command);
      end loop;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Insert_And_Execute;

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Console : Interactive_Console;
      Command : String)
   is
      Output      : GNAT.Strings.String_Access;
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;

   begin
      Output := new String'
        (Console.Handler (Console, Command, Console.User_Data));

      Get_End_Iter (Console.Buffer, Last_Iter);
      Insert (Console.Buffer, Last_Iter, Output.all);

      if Command /= "" and then Console.History /= null then
         Add_To_History
           (Console.History.all,
            History_Key (Console.Key.all), Command);
         Console.Current_Position := -1;
      end if;

      Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Console.Buffer, Last_Iter);

      Apply_Tag
        (Console.Buffer,
         Console.Uneditable_Tag, Prompt_Iter, Last_Iter);

      Display_Prompt (Console);
      Free (Output);
   end Execute_Command;

   ----------------
   -- Set_Prompt --
   ----------------

   procedure Set_Prompt
     (Console : access Interactive_Console_Record; Prompt : String) is
   begin
      Free (Console.Prompt);
      Console.Prompt := new String'(Prompt);
   end Set_Prompt;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Console : access Interactive_Console_Record)
      return Gtk.Text_View.Gtk_Text_View is
   begin
      return Console.View;
   end Get_View;

   ----------
   -- Read --
   ----------

   function Read
     (Console    : access Interactive_Console_Record;
      Whole_Line : Boolean) return String
   is
      Last_Iter, Prompt_Iter : Gtk_Text_Iter;
      End_Mark               : Gtk_Text_Mark;
      Success                : Boolean;
   begin
      if Whole_Line then
         Get_End_Iter (Console.Buffer, Last_Iter);
         End_Mark := Create_Mark (Console.Buffer, "", Last_Iter);

         Console.Waiting_For_Input := True;
         Grab_Focus (Get_View (Console));
         Gtk.Main.Main;
         Console.Waiting_For_Input := False;

         Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, End_Mark);
         Delete_Mark (Console.Buffer, End_Mark);
         Get_End_Iter (Console.Buffer, Last_Iter);
         Backward_Char (Last_Iter, Success);
         return Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter);
      else
         --  Since we do not bufferize
         return "";
      end if;
   end Read;

end Interactive_Consoles;
