-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;              use System;

with GNAT.Expect;         use GNAT.Expect;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with GNAT.Regpat;         use GNAT.Regpat;
with GNATCOLL.Scripts.Gtkada; use GNATCOLL.Scripts, GNATCOLL.Scripts.Gtkada;

with Glib;                use Glib;
with Glib.Convert;
with Glib.Values;         use Glib.Values;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;
with Gdk.Types;           use Gdk, Gdk.Types;
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
with Gtkada.MDI;          use Gtkada.MDI;
with Pango.Font;          use Pango.Font;
with Pango.Enums;         use Pango.Enums;

with Traces;              use Traces;
with Histories;           use Histories;
with String_Utils;        use String_Utils;
with String_List_Utils;   use String_List_Utils;
with GUI_Utils;           use GUI_Utils;

package body Interactive_Consoles is
   Me : constant Debug_Handle := Create ("Console");

   package Console_Idle is new Gtk.Main.Idle (Interactive_Console);
   function UC is new Ada.Unchecked_Conversion
     (System.Address, Interactive_Console);
   function UC is new Ada.Unchecked_Conversion
     (Interactive_Console, System.Address);

   ---------------------------------
   -- Interactive_Virtual_Console --
   ---------------------------------

   type Interactive_Virtual_Console_Record is new Virtual_Console_Record with
      record
         Console   : Interactive_Console;
         Script    : Scripting_Language;
         Took_Grab : Boolean := False;
         Child     : MDI_Child := null;
         --  MDI_Child cached, used in Insert_Error
      end record;
   type Interactive_Virtual_Console
     is access all Interactive_Virtual_Console_Record'Class;

   overriding procedure Ref
     (Console : access Interactive_Virtual_Console_Record);
   overriding procedure Unref
     (Console : access Interactive_Virtual_Console_Record);
   overriding procedure Insert_Text
     (Console : access Interactive_Virtual_Console_Record; Txt : String);
   overriding procedure Insert_Log
     (Console : access Interactive_Virtual_Console_Record; Txt : String);
   overriding procedure Insert_Prompt
     (Console : access Interactive_Virtual_Console_Record; Txt : String);
   overriding procedure Insert_Error
     (Console : access Interactive_Virtual_Console_Record; Txt : String);
   overriding procedure Grab_Events
     (Console : access Interactive_Virtual_Console_Record; Grab : Boolean);
   overriding procedure Set_As_Default_Console
     (Console     : access Interactive_Virtual_Console_Record;
      Script      : GNATCOLL.Scripts.Scripting_Language);
   overriding procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access Interactive_Virtual_Console_Record);
   overriding function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Interactive_Virtual_Console_Record)
      return Class_Instance;
   overriding procedure Process_Pending_Events_Primitive
     (Console : access Interactive_Virtual_Console_Record);
   overriding function Read
     (Console    : access Interactive_Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String;
   overriding procedure Clear
     (Console    : access Interactive_Virtual_Console_Record);
   --  See inherited subprograms

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

   procedure Insert_UTF8_With_Tag
     (Console        : access Interactive_Console_Record;
      UTF8           : Glib.UTF8_String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Highlight_Tag  : Gtk_Text_Tag;
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True);
   --  Same as Insert_UTF8, but the tag to use for highlighting is specified

   procedure Prepare_For_Output
     (Console     : access Interactive_Console_Record'Class;
      Internal    : out Boolean;
      Last_Iter   : out Gtk_Text_Iter);
   procedure Terminate_Output
     (Console       : access Interactive_Console_Record'Class;
      Internal      : Boolean;
      Show_Prompt   : Boolean);
   --  Prepare or terminate text insertion in the console. This properly takes
   --  care of the prompt, making the text read-only,... Any highlighting of
   --  the text must be done separately.

   ---------
   -- Ref --
   ---------

   overriding procedure Ref
     (Console : access Interactive_Virtual_Console_Record) is
   begin
      Ref (Console.Console);
   end Ref;

   -----------
   -- Unref --
   -----------

   overriding procedure Unref
     (Console : access Interactive_Virtual_Console_Record) is
   begin
      Unref (Console.Console);
   end Unref;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Console : access Interactive_Virtual_Console_Record) is
   begin
      Clear (Console.Console);
   end Clear;

   -----------------
   -- Insert_Text --
   -----------------

   overriding procedure Insert_Text
     (Console : access Interactive_Virtual_Console_Record; Txt : String) is
   begin
      Insert (Console.Console, Txt, Add_LF => False, Show_Prompt => False);
   end Insert_Text;

   ----------------
   -- Insert_Log --
   ----------------

   overriding procedure Insert_Log
     (Console : access Interactive_Virtual_Console_Record; Txt : String)
   is
      pragma Unreferenced (Console);
   begin
      Trace (Me, Txt);
   end Insert_Log;

   -------------------
   -- Insert_Prompt --
   -------------------

   overriding procedure Insert_Prompt
     (Console : access Interactive_Virtual_Console_Record; Txt : String) is
   begin
      --  If the console has its own prompt, so ignore the one passed in
      --  parameter
      if Console.Console.Prompt.all /= "" then
         Display_Prompt (Console.Console);
      else
         Set_Prompt (Console.Console, Txt);
         Display_Prompt (Console.Console);
         Set_Prompt (Console.Console, "");
      end if;
   end Insert_Prompt;

   ------------------
   -- Insert_Error --
   ------------------

   overriding procedure Insert_Error
     (Console : access Interactive_Virtual_Console_Record; Txt : String) is
   begin
      Insert (Console.Console, Txt,
              Add_LF      => True,
              Highlight   => True,
              Show_Prompt => False);

      if Console.Child = null then
         Console.Child := Find_MDI_Child_From_Widget (Console.Console.View);
      end if;
      Raise_Child (Console.Child);
   end Insert_Error;

   -----------------
   -- Grab_Events --
   -----------------

   overriding procedure Grab_Events
     (Console : access Interactive_Virtual_Console_Record; Grab : Boolean) is
   begin
      if Grab then
         Console.Took_Grab := False;

         --  Grab the mouse, keyboard,... so as to avoid recursive loops in
         --  GPS (user selecting a menu while python is running)
         Ref (Console.Console);

         if Get_Window (Console.Console) /= null then
            --  If we already have a grab (for instance when the user is
            --  displaying a menu and we are running the python command as a
            --  filter for that menu), no need to take another. In fact,
            --  taking another would break the above scenario, since in
            --  gtkmenu.c the handler for grab_notify cancels the menu when
            --  another grab is taken (G305-005)

            if Gtk.Main.Grab_Get_Current = null then
               Gtk.Main.Grab_Add (Console.Console);
               Console.Took_Grab := True;
            end if;
         end if;

      else
         --  Note: the widget might have been destroyed by the python command,
         --  we need to check that it still exists.

         if Console.Took_Grab then
            Gtk.Main.Grab_Remove (Console.Console);
            Unref (Console.Console);
         end if;
      end if;
   end Grab_Events;

   ----------------------------
   -- Set_As_Default_Console --
   ----------------------------

   overriding procedure Set_As_Default_Console
     (Console : access Interactive_Virtual_Console_Record;
      Script  : GNATCOLL.Scripts.Scripting_Language) is
   begin
      Console.Script := Script;
   end Set_As_Default_Console;

   ------------------------
   -- Set_Data_Primitive --
   ------------------------

   overriding procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access Interactive_Virtual_Console_Record) is
   begin
      GNATCOLL.Scripts.Gtkada.Set_Data (Instance, GObject (Console.Console));
   end Set_Data_Primitive;

   ------------------
   -- Get_Instance --
   ------------------

   overriding function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Interactive_Virtual_Console_Record)
      return Class_Instance is
   begin
      return GNATCOLL.Scripts.Gtkada.Get_Instance
        (Script, GObject (Console.Console));
   end Get_Instance;

   --------------------------------------
   -- Process_Pending_Events_Primitive --
   --------------------------------------

   overriding procedure Process_Pending_Events_Primitive
     (Console : access Interactive_Virtual_Console_Record)
   is
      Dead : Boolean;
      pragma Unreferenced (Console, Dead);
   begin
      --  Process all gtk+ events, so that the text becomes visible
      --  immediately, even if the python program hasn't finished executing

      --  Note: since we have grabed the mouse and keyboards, events will only
      --  be sent to the python console, thus avoiding recursive loops inside
      --  GPS.

      while Gtk.Main.Events_Pending loop
         Dead := Gtk.Main.Main_Iteration;
      end loop;
   end Process_Pending_Events_Primitive;

   ----------
   -- Read --
   ----------

   overriding function Read
     (Console    : access Interactive_Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String
   is
      pragma Unreferenced (Size);
   begin
      return Read (Console.Console, Whole_Line);
   end Read;

   -----------------------------------
   -- Get_Or_Create_Virtual_Console --
   -----------------------------------

   function Get_Or_Create_Virtual_Console
     (Console : Interactive_Console) return Virtual_Console is
   begin
      if Console = null then
         return null;

      elsif Console.Virtual = null then
         Console.Virtual := new Interactive_Virtual_Console_Record;
         Interactive_Virtual_Console (Console.Virtual).Console := Console;
      end if;

      return Console.Virtual;
   end Get_Or_Create_Virtual_Console;

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Virtual : GNATCOLL.Scripts.Virtual_Console) return Interactive_Console is
   begin
      return Interactive_Virtual_Console (Virtual).Console;
   end Get_Console;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Console : in out Interactive_Console) is
      pragma Warnings (Off, Console);
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
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True)
   is
      UTF8 : constant String := Glib.Convert.Locale_To_UTF8 (Text);
   begin
      Insert_UTF8
        (Console, UTF8, Add_LF, Highlight, Add_To_History, Show_Prompt);
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   procedure Insert_UTF8
     (Console        : access Interactive_Console_Record;
      UTF8           : Glib.UTF8_String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True)
   is
   begin
      Insert_UTF8_With_Tag
        (Console        => Console,
         UTF8           => UTF8,
         Add_LF         => Add_LF,
         Highlight      => Highlight,
         Highlight_Tag  => Console.Highlight_Tag,
         Add_To_History => Add_To_History,
         Show_Prompt    => Show_Prompt);
   end Insert_UTF8;

   ------------------------
   -- Prepare_For_Output --
   ------------------------

   procedure Prepare_For_Output
     (Console   : access Interactive_Console_Record'Class;
      Internal  : out Boolean;
      Last_Iter : out Gtk_Text_Iter)
   is
      Prompt_Iter : Gtk_Text_Iter;
   begin
      Internal := Console.Internal_Insert;
      Get_End_Iter (Console.Buffer, Last_Iter);

      if Console.User_Input = null then
         Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
         Console.User_Input := new String'
           (Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter));
      end if;
   end Prepare_For_Output;

   ----------------------
   -- Terminate_Output --
   ----------------------

   procedure Terminate_Output
     (Console     : access Interactive_Console_Record'Class;
      Internal    : Boolean;
      Show_Prompt : Boolean)
   is
      Last_Iter   : Gtk_Text_Iter;
      Success     : Boolean;
      Prompt_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Console.Buffer, Last_Iter);
      Apply_Tag
        (Console.Buffer, Console.Uneditable_Tag, Prompt_Iter, Last_Iter);

      Forward_Chars (Prompt_Iter, Console.User_Input'Length, Success);
      Apply_Tag
        (Console.Buffer,
         Console.External_Messages_Tag,
         Prompt_Iter, Last_Iter);

      if Show_Prompt then
         Display_Prompt (Console);
      end if;

      Console.Message_Was_Displayed := True;
      Console.Internal_Insert := Internal;
   end Terminate_Output;

   --------------------------
   -- Insert_UTF8_With_Tag --
   --------------------------

   procedure Insert_UTF8_With_Tag
     (Console        : access Interactive_Console_Record;
      UTF8           : Glib.UTF8_String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Highlight_Tag  : Gtk_Text_Tag;
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True)
   is
      Last_Iter : Gtk_Text_Iter;
      Internal  : Boolean;

   begin
      Prepare_For_Output (Console, Internal, Last_Iter);

      if Add_LF then
         if Highlight then
            Insert_With_Tags
              (Console.Buffer, Last_Iter, UTF8 & ASCII.LF,
               Highlight_Tag);
         else
            Insert (Console.Buffer, Last_Iter, UTF8 & ASCII.LF);
         end if;

      elsif Highlight then
         Insert_With_Tags (Console.Buffer, Last_Iter, UTF8, Highlight_Tag);
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

      Terminate_Output (Console, Internal, Show_Prompt);
   end Insert_UTF8_With_Tag;

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
      Cursor_Iter, Start_Iter : Gtk_Text_Iter;
      Link    : Hyper_Links := Console.Links;
      Success : Boolean;
   begin
      Console.Button_Press := False;

      if Link /= null then
         Get_Iter_At_Mark (Console.Buffer, Cursor_Iter, Console.Insert_Mark);
         while Link /= null loop
            if Has_Tag (Cursor_Iter, Link.Tag) then
               Copy (Source => Cursor_Iter, Dest => Start_Iter);
               Success := Begins_Tag (Start_Iter, Link.Tag);
               if not Success then
                  Backward_To_Tag_Toggle (Start_Iter, Link.Tag, Success);
               end if;

               if Success then
                  Success := Ends_Tag (Cursor_Iter, Link.Tag);
                  if not Success then
                     Forward_To_Tag_Toggle (Cursor_Iter, Link.Tag, Success);
                  end if;

                  if Success then
                     On_Click
                       (Link.Callback,
                        Get_Text (Console.Buffer, Start_Iter, Cursor_Iter));
                  end if;
               end if;
               exit;
            end if;

            Link := Link.Next;
         end loop;
      end if;

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

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Virtual_Console_Record'Class, Virtual_Console);

      Console : constant Interactive_Console := Interactive_Console (Object);
   begin
      if Console.Idle_Registered
        and then Console.Idle_Id /= 0
      then
         Idle_Remove (Console.Idle_Id);
         Console.Idle_Id := 0;
      end if;

      --  When a Gtk_Text_View is deleted, a "mark_set" signal is sent
      --  internally, which causes an additional idle callback to be
      --  registered through Mark_Set_Handler.
      --  The following two lines are used to prevent adding unnecessary
      --  idle callbacks when we are deleting the console.
      Console.Internal_Insert := True;
      Console.Idle_Registered := True;

      if Console.Virtual /= null then
         if Interactive_Virtual_Console (Console.Virtual).Script /= null then
            Set_Default_Console
              (Interactive_Virtual_Console (Console.Virtual).Script, null);
         end if;
         Unchecked_Free (Console.Virtual);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Delete_Event_Handler;

   --------------------------------
   -- Default_Completion_Handler --
   --------------------------------

   function Default_Completion_Handler
     (Input     : String;
      User_Data : System.Address) return String_List_Utils.String_List.List
   is
      Console     : constant Interactive_Console := UC (User_Data);
      Completions : String_Lists.List;
      C           : String_Lists.Cursor;
      Result      : String_List_Utils.String_List.List :=
                      String_List_Utils.String_List.Null_List;
   begin
      if Console.Virtual /= null
        and then Interactive_Virtual_Console (Console.Virtual).Script /= null
      then
         Complete (Interactive_Virtual_Console (Console.Virtual).Script,
                   Input, Completions);
         C := String_Lists.First (Completions);
         while String_Lists.Has_Element (C) loop
            String_List_Utils.String_List.Append
              (Result, String_Lists.Element (C));
            String_Lists.Next (C);
         end loop;
      end if;

      return Result;
   end Default_Completion_Handler;

   -------------------------------
   -- Default_Interrupt_Handler --
   -------------------------------

   function Default_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address) return Boolean
   is
      pragma Unreferenced (User_Data);
   begin
      if Console.Virtual /= null
        and then Interactive_Virtual_Console (Console.Virtual).Script /= null
      then
         return Interrupt
           (Interactive_Virtual_Console (Console.Virtual).Script);
      end if;
      return False;
   end Default_Interrupt_Handler;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   function Default_Command_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String
   is
      pragma Unreferenced (User_Data);
   begin
      if Console.Virtual /= null
        and then Interactive_Virtual_Console (Console.Virtual).Script /= null
      then
         declare
            Errors : aliased Boolean;
            S : constant String := Execute_Command
              (Script  => Interactive_Virtual_Console (Console.Virtual).Script,
               Command      => Input,
               Show_Command => False,
               Hide_Output  => False,
               Errors       => Errors'Unchecked_Access);
         begin
            --  Preserve the focus on the console after an interactive
            --  execution

            Grab_Focus (Console.View);

            if S = ""
              or else S (S'Last) = ASCII.LF
              or else S (S'Last) = ASCII.CR
            then
               return S;
            else
               return S & ASCII.LF;
            end if;
         end;
      end if;
      return "";

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return "";
   end Default_Command_Handler;

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
               if Console.Completion = Default_Completion_Handler'Access then
                  Do_Completion
                    (View            => Console.View,
                     Completion      => Default_Completion_Handler'Access,
                     Prompt_End_Mark => Console.Prompt_Mark,
                     Uneditable_Tag  => Console.Uneditable_Tag,
                     User_Data       => UC (Console));

               else
                  Do_Completion
                    (View            => Console.View,
                     Completion      => Console.Completion,
                     Prompt_End_Mark => Console.Prompt_Mark,
                     Uneditable_Tag  => Console.Uneditable_Tag,
                     User_Data       => Console.User_Data);
               end if;
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
      if not Console.Input_Blocked then
         Get_End_Iter (Console.Buffer, First_Iter);
         Offset := Get_Offset (First_Iter);

         Insert (Console.Buffer, First_Iter, Console.Prompt.all);

         Get_End_Iter (Console.Buffer, Prompt_Iter);
         Get_Iter_At_Offset (Console.Buffer, First_Iter, Offset);

         Apply_Tag
           (Console.Buffer, Console.Uneditable_Tag, First_Iter, Prompt_Iter);
         Apply_Tag
           (Console.Buffer, Console.Prompt_Tag, First_Iter, Prompt_Iter);
      else
         Get_End_Iter (Console.Buffer, Prompt_Iter);
      end if;

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
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Pattern_Matcher, Pattern_Matcher_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hyper_Link_Callback_Record'Class, Hyper_Link_Callback);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hyper_Link_Record, Hyper_Links);
      C     : constant Interactive_Console := Interactive_Console (Console);
      L, L2 : Hyper_Links;
   begin
      if C.Idle_Registered
        and then C.Idle_Id /= 0
      then
         Idle_Remove (C.Idle_Id);
         C.Idle_Id := 0;
      end if;

      L := C.Links;
      while L /= null loop
         L2 := L.Next;
         On_Destroy (L.Callback.all);
         Unchecked_Free (L.Callback);
         Unchecked_Free (L.Pattern);
         Unchecked_Free (L);
         L := L2;
      end loop;

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
      Handler             : Command_Handler := Default_Command_Handler'Access;
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
      Handler             : Command_Handler := Default_Command_Handler'Access;
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
        (Console, Gtk.Widget.Signal_Delete_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (Delete_Event_Handler'Access),
         After => False);

      Get_End_Iter (Console.Buffer, Iter);

      Console.Prompt_Mark := Create_Mark (Console.Buffer, "", Iter);
      Console.Insert_Mark := Get_Insert (Console.Buffer);
      Console.Completion  := Default_Completion_Handler'Access;
      Console.Interrupt   := Default_Interrupt_Handler'Access;

      Console.Internal_Insert := False;

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

      if Console.Handler /= Default_Command_Handler'Access then
         Get_End_Iter (Console.Buffer, Last_Iter);
         Insert (Console.Buffer, Last_Iter, Output.all);
      end if;

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

      if Console.Handler /= Default_Command_Handler'Access then
         Display_Prompt (Console);
      end if;

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

   -----------------------
   -- Create_Hyper_Link --
   -----------------------

   procedure Create_Hyper_Link
     (Console  : access Interactive_Console_Record;
      Regexp   : GNAT.Regpat.Pattern_Matcher;
      Callback : not null access Hyper_Link_Callback_Record'Class)
   is
      Tag : Gtk_Text_Tag;
   begin
      Gtk_New (Tag);
      Add (Get_Tag_Table (Console.Buffer), Tag);
      Set_Property (Tag, Gtk.Text_Tag.Foreground_Property, "blue");
      Set_Property
        (Tag, Gtk.Text_Tag.Underline_Property, Pango_Underline_Single);

      Console.Links := new Hyper_Link_Record'
        (Pattern  => new Pattern_Matcher'(Regexp),
         Callback => Hyper_Link_Callback (Callback),
         Tag      => Tag,
         Next     => Console.Links);
      Console.Links_Count := Console.Links_Count + 1;
   end Create_Hyper_Link;

   -----------------------
   -- Insert_With_Links --
   -----------------------

   procedure Insert_With_Links
     (Console   : access Interactive_Console_Record;
      Text      : String;
      Add_LF    : Boolean := True;
      Highlight : Boolean := False)
   is
      type Link_And_Location is record
         Link        : Hyper_Links;
         First, Last : Natural;
      end record;
      Locs : array (1 .. Console.Links_Count) of Link_And_Location;
      --  Index in Text of the first occurrence of each regexp

      Index : Natural := Text'First;

      procedure Update_Pattern_Loc (L : Natural; Link : Hyper_Links);
      --  Update the next location of the pattern after Index

      ------------------------
      -- Update_Pattern_Loc --
      ------------------------

      procedure Update_Pattern_Loc (L : Natural; Link : Hyper_Links) is
         Matches : Match_Array (0 .. 1);
      begin
         Match (Link.Pattern.all, Text, Matches, Data_First => Index);

         if Matches (0) = No_Match then
            Locs (L) := (Link  => Link,
                         First => Integer'Last,
                         Last  => Integer'Last);

         elsif Paren_Count (Link.Pattern.all) = 0 then
            Locs (L) := (Link  => Link,
                         First => Matches (0).First,
                         Last  => Matches (0).Last);

         else
            Locs (L) := (Link  => Link,
                         First => Matches (1).First,
                         Last  => Matches (1).Last);
         end if;
      end Update_Pattern_Loc;

      Link        : Hyper_Links := Console.Links;
      Min         : Natural;
      Min_Pattern : Natural;
      Internal    : Boolean;
      Start_Iter, Last_Iter : Gtk_Text_Iter;

   begin
      --  Initialize the locations array, so that we try and match the regexps
      --  as few times as possible for efficiency
      for L in Locs'Range loop
         Update_Pattern_Loc (L, Link);
         Link := Link.Next;
      end loop;

      Prepare_For_Output (Console, Internal, Last_Iter);

      while Index <= Text'Last loop
         Min         := Text'Last + 1;
         Min_Pattern := Locs'Last + 1;
         for L in Locs'Range loop
            if Locs (L).First < Min then
               Min         := Locs (L).First;
               Min_Pattern := L;
            end if;
         end loop;

         if Min <= Text'Last then
            --  Found a regexp. Insert the leading text first, no hyper link
            if Min - 1 >= Index then
               Get_End_Iter (Console.Buffer, Start_Iter);
               if Highlight then
                  Insert_With_Tags
                    (Buffer => Console.Buffer,
                     Iter   => Start_Iter,
                     Text   => Glib.Convert.Locale_To_UTF8
                       (Text (Index .. Min - 1)),
                     Tag    => Console.Highlight_Tag);
               else
                  Insert
                    (Buffer => Console.Buffer,
                     Iter   => Start_Iter,
                     Text   => Glib.Convert.Locale_To_UTF8
                       (Text (Index .. Min - 1)));
               end if;
            end if;

            --  Then insert the hyper link
            Get_End_Iter (Console.Buffer, Start_Iter);
            Insert_With_Tags
              (Buffer         => Console.Buffer,
               Iter           => Start_Iter,
               Text           => Glib.Convert.Locale_To_UTF8
                 (Text (Min .. Locs (Min_Pattern).Last)),
               Tag            => Locs (Min_Pattern).Link.Tag);

            Index := Locs (Min_Pattern).Last + 1;

            --  Update the locations of the matches to find the next one. This
            --  is done as efficiently as possible, since we do not need to
            --  refresh those for which we know the next location is later on.

            for L in Locs'Range loop
               if Locs (L).First < Index then
                  Update_Pattern_Loc (L, Locs (L).Link);
               end if;
            end loop;

         else
            --  Insert the remaining of the text
            Get_End_Iter (Console.Buffer, Start_Iter);
            if Highlight then
               Insert_With_Tags
                 (Buffer => Console.Buffer,
                  Iter   => Start_Iter,
                  Text   => Text (Index .. Text'Last),
                  Tag    => Console.Highlight_Tag);
            else
               Insert
                 (Buffer => Console.Buffer,
                  Iter   => Start_Iter,
                  Text   => Text (Index .. Text'Last));
            end if;
            Index := Text'Last + 1;
         end if;
      end loop;

      if Add_LF then
         Get_End_Iter (Console.Buffer, Start_Iter);
         Insert
           (Buffer => Console.Buffer,
            Iter   => Start_Iter,
            Text   => "" & ASCII.LF);
      end if;

      Terminate_Output (Console, Internal, Show_Prompt => False);
   end Insert_With_Links;

end Interactive_Consoles;
