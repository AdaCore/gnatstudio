------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;             use Ada.Calendar;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System;                   use System;

with GNAT.Expect;              use GNAT.Expect;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GNAT.Regpat;              use GNAT.Regpat;
with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.Iconv;           use GNATCOLL.Iconv;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Scripts.Gtkada;  use GNATCOLL.Scripts, GNATCOLL.Scripts.Gtkada;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;

with Glib;                     use Glib;
with Glib.Main;                use Glib.Main;
with Glib.Convert;
with Glib.Values;              use Glib.Values;
with Glib.Object;              use Glib.Object;
with Glib.Properties;          use Glib.Properties;
with Glib.Unicode;             use Glib.Unicode;
with Gdk.Keyval;               use Gdk.Keyval;
with Gdk.Types;                use Gdk, Gdk.Types;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Event;                use Gdk.Event;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Style_Context;        use Gtk.Style_Context;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Mark;            use Gtk.Text_Mark;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;       use Gtk.Text_Tag_Table;
with Gtk.Toolbar;              use Gtk.Toolbar;
with Gtk.Widget;               use Gtk.Widget;
with Gtk.Selection_Data;       use Gtk.Selection_Data;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.Terminal;          use Gtkada.Terminal;
with Gtkada.MDI;               use Gtkada.MDI;
with Pango.Enums;              use Pango.Enums;

with Config;                   use Config;
with Histories;                use Histories;
with GUI_Utils;                use GUI_Utils;

with GPS.Default_Styles;       use GPS.Default_Styles;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Scripts;       use GPS.Kernel.Scripts;
with GPS.Kernel.Style_Manager; use GPS.Kernel.Style_Manager;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;

package body Interactive_Consoles is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.INTERACTIVE_CONSOLE");

   Max_Lines_Displayed : constant := 2000;
   --  Number of lines to display in the console - older lines are removed
   --  when the text exceeds this.

   Max_Line_Length : constant := 500;
   --  The maximum character length to accept in outside output: lines
   --  exceeding this lenth are split at that mark.

   Process_Lines_Timeout : constant Duration := 0.1;
   --  The number of seconds that is allowed for each loop of the function
   --  that processes pending lines.

   Line_Processing_Interval : constant := 200;
   --  The timeout interval to process the pending lines

   package Console_Idle is new Glib.Main.Generic_Sources (Interactive_Console);

   function Process_Lines (Self : Interactive_Console) return Boolean;
   --  Process the lines that are left to process, if any

   ---------------------------------
   -- Interactive_Virtual_Console --
   ---------------------------------

   type Interactive_Virtual_Console_Record is
     new Virtual_Console_Record
   with record
      Console   : Interactive_Console;
      Script    : Scripting_Language;
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
   overriding procedure Set_As_Default_Console
     (Console : access Interactive_Virtual_Console_Record;
      Script  : GNATCOLL.Scripts.Scripting_Language);
   overriding procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access Interactive_Virtual_Console_Record);
   overriding function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Interactive_Virtual_Console_Record)
      return Class_Instance;
   overriding function Read
     (Console    : access Interactive_Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String;
   overriding procedure Clear
     (Console : access Interactive_Virtual_Console_Record);
   --  See inherited subprograms

   type Hyper_Link_Tag_Record is new Gtk_Text_Tag_Record with null record;
   --  Special tags to highlight hyper links

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Mark_Set_Handler
     (Console : access Gtk_Widget_Record'Class;
      Params  : Glib.Values.GValues);
   --  Prevent cursor movements before the prompt

   function Button_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "button_press_event" signal

   function Button_Release_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handler for the "button_release_event" signal

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "key_press_event" signal

   procedure Selection_Received_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler for the "selection_received" signal

   procedure Size_Allocate_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Handler for the "size_allocate" signal

   function Delete_Event_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "delete_event" signal

   procedure Replace_Cursor (Console : Interactive_Console);
   --  If the cursor is in a forbidden zone, place it at the prompt

   function Place_Cursor_At_Prompt
     (Console : Interactive_Console) return Boolean;
   --  Place the cursor at the prompt mark

   procedure Destroy_Idle (Console : in out Interactive_Console);
   --  Destroy handler for idle callbacks

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class);
   --  Called when the console is destroyed

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

   procedure Display_Text_As_Prompt
     (Console : access Interactive_Console_Record'Class; Txt : String);
   --  Display Txt as if it was a prompt. Txt might be the empty string, which
   --  only results in moving the prompt mark

   procedure Insert_UTF8_With_Tag
     (Console        : access Interactive_Console_Record;
      UTF8           : Glib.UTF8_String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Highlight_Tag  : Gtk_Text_Tag;
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True;
      Text_Is_Input  : Boolean := False);
   --  Same as Insert_UTF8, but the tag to use for highlighting is specified

   procedure Prepare_For_Output
     (Console        : access Interactive_Console_Record'Class;
      Text_Is_Input  : Boolean := False;
      Internal       : out Boolean;
      Last_Iter      : out Gtk_Text_Iter);
   procedure Terminate_Output
     (Console       : access Interactive_Console_Record'Class;
      Internal      : Boolean;
      Show_Prompt   : Boolean);
   --  Prepare or terminate text insertion in the console. This properly takes
   --  care of the prompt, making the text read-only,... Any highlighting of
   --  the text must be done separately.

   function Replace_Zeros_And_Count_Lines (S : in out String) return Natural;
   pragma Inline (Replace_Zeros_And_Count_Lines);
   --  Replace ASCII.NULs in S. Return the number of lines in S.

   procedure Limit_Line_Count (B : Gtk_Text_Buffer; New_Lines : Natural);
   --  Assume that we are about to add New_Lines new lines to B, and remove
   --  the first lines in B to make sure we don't exceed the maximum line
   --  count.

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Console : access Interactive_Console_Record'Class;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

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
      if Console.Console /= null then
         Insert (Console.Console,
                 Text        => Txt,
                 Add_LF      => False,
                 Show_Prompt => False);
      end if;

      if Console.Child = null then
         Console.Child := Find_MDI_Child_From_Widget (Console.Console.View);
      end if;

      --  Always highlight the console when new output is displayed
      Highlight_Child (Console.Child);
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
      if not Get_Editable (Console.Console.View)
        or else Console.Console.Prompt = null
      then
         --  If the console does not support input, no need to display a
         --  prompt.
         return;
      end if;

      if Console.Console.Prompt.all /= "" then
         --  If the console has its own prompt, so ignore the one passed in
         --  parameter.
         Display_Text_As_Prompt (Console.Console, Console.Console.Prompt.all);
      else
         Display_Text_As_Prompt (Console.Console, Txt);
      end if;
   end Insert_Prompt;

   ------------------
   -- Insert_Error --
   ------------------

   overriding procedure Insert_Error
     (Console : access Interactive_Virtual_Console_Record; Txt : String) is
   begin
      Insert (Console.Console, Txt,
              Add_LF      => False,
              Highlight   => True,
              Show_Prompt => False);

      if Console.Child = null then
         Console.Child := Find_MDI_Child_From_Widget (Console.Console.View);
      end if;

      --  Give the focus to the console when an error message is displayed
      Raise_Child (Console.Child);
   end Insert_Error;

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

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Script  : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Console : access Interactive_Console_Record'Class)
         return GNATCOLL.Scripts.Class_Instance
   is
      Inst : Class_Instance := Get_Instance (Script, Console);
   begin
      if Inst = No_Class_Instance then
         Inst := New_Instance
           (Script, New_Class (Get_Kernel (Script), Console_Class_Name));
         Set_Data (Inst, Console.Get_Or_Create_Virtual_Console);
      end if;

      return Inst;
   end Get_Or_Create_Instance;

   -----------------------------------
   -- Get_Or_Create_Virtual_Console --
   -----------------------------------

   function Get_Or_Create_Virtual_Console
     (Console : access Interactive_Console_Record'Class)
      return GNATCOLL.Scripts.Virtual_Console
   is
   begin
      if Console = null then
         return null;

      elsif Console.Virtual = null then
         Console.Virtual := new Interactive_Virtual_Console_Record;
         Interactive_Virtual_Console (Console.Virtual).Console :=
           Interactive_Console (Console);
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
   begin
      Console.Idle_Id := 0;
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
      Show_Prompt    : Boolean := True;
      Text_Is_Input  : Boolean := False)
   is

      UTF8 : String := Iconv
        (Input           => Text,
         To_Code         => GNATCOLL.Iconv.UTF8,
         From_Code       => Locale,
         Ignore_Errors   => True,
         Transliteration => True,
         Ignore          => True);

      New_Lines : Natural;
   begin
      New_Lines := Replace_Zeros_And_Count_Lines (UTF8);
      Limit_Line_Count (Console.Buffer, New_Lines);

      Insert_UTF8_With_Tag
        (Console, UTF8,
         Add_LF         => Add_LF,
         Highlight      => Highlight,
         Highlight_Tag  => Console.Tags (Highlight_Tag),
         Add_To_History => Add_To_History,
         Show_Prompt    => Show_Prompt,
         Text_Is_Input  => Text_Is_Input);
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
      Show_Prompt    : Boolean := True;
      Text_Is_Input  : Boolean := False)
   is
   begin
      Insert_UTF8_With_Tag
        (Console        => Console,
         UTF8           => UTF8,
         Add_LF         => Add_LF,
         Highlight      => Highlight,
         Highlight_Tag  => Console.Tags (Highlight_Tag),
         Add_To_History => Add_To_History,
         Show_Prompt    => Show_Prompt,
         Text_Is_Input  => Text_Is_Input);
      Limit_Line_Count (Console.Buffer, 0);
   end Insert_UTF8;

   -----------------
   -- Clear_Input --
   -----------------

   procedure Clear_Input (Console : access Interactive_Console_Record) is
      Prompt_Iter, Last_Iter : Gtk_Text_Iter;
   begin
      --  If the console is managing the prompt on its own
      if Console.Prompt /= null then
         Get_End_Iter (Console.Buffer, Last_Iter);
         Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
         Delete (Console.Buffer, Prompt_Iter, Last_Iter);
      end if;
   end Clear_Input;

   ------------------------
   -- Prepare_For_Output --
   ------------------------

   procedure Prepare_For_Output
     (Console   : access Interactive_Console_Record'Class;
      Text_Is_Input  : Boolean := False;
      Internal  : out Boolean;
      Last_Iter : out Gtk_Text_Iter)
   is
      Prompt_Iter : Gtk_Text_Iter;
   begin
      Internal := Console.Internal_Insert;

      --  Read current user input (there might be none!). Then remove it from
      --  the console temporarily, so that output is not intermixed with user
      --  input. It will be put back after the output in Terminate_Output.
      --  However we do nothing when the console is not managing the prompt on
      --  its own but letting the user do it.

      if Console.Prompt /= null then
         Get_End_Iter (Console.Buffer, Last_Iter);

         if not Text_Is_Input and then Console.User_Input = null then
            Get_Iter_At_Mark
              (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
            Console.User_Input := new String'
              (Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter));
            Delete (Console.Buffer, Prompt_Iter, Last_Iter);
         end if;

      else
         Get_Iter_At_Mark
           (Console.Buffer, Last_Iter, Get_Insert (Console.Buffer));
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
      Prompt_Iter : Gtk_Text_Iter;
   begin
      if Console.Prompt /= null then
         --  Protect the text we just output so that it is not editable by
         --  users

         Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
         Get_End_Iter (Console.Buffer, Last_Iter);
         Apply_Tag
           (Console.Buffer,
            Console.Tags (Uneditable_Tag), Prompt_Iter, Last_Iter);
         Apply_Tag
           (Console.Buffer,
            Console.Tags (External_Messages_Tag), Prompt_Iter, Last_Iter);

         --  Move the prompt mark at the end of the output, so that user input
         --  is only read from that point on.

         if Show_Prompt then
            Display_Prompt (Console);
         else
            Display_Text_As_Prompt (Console, "");
         end if;

         --  Put back the partial input the user had started typing (was saved
         --  in Prepare_For_Ouptut).

         Get_End_Iter (Console.Buffer, Last_Iter);
         Insert (Console.Buffer, Last_Iter, Console.User_Input.all);
         Free (Console.User_Input);
      else
         Scroll_Mark_Onscreen (Console.View, Get_Insert (Console.Buffer));
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
      Show_Prompt    : Boolean := True;
      Text_Is_Input  : Boolean := False)
   is
      Last_Iter : Gtk_Text_Iter;
      Internal  : Boolean;

   begin
      if Console /= null
        and then Console.Kernel /= null
        and then Console.Kernel.Is_In_Destruction
      then
         --  Trying to write in a buffer while the kernel is being destroyed
         --  will trigger a deadlock in the g_mutex
         Trace (Me, UTF8);
         return;
      end if;

      Prepare_For_Output (Console, Text_Is_Input, Internal, Last_Iter);

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
         --  Do not display GtkWarnings coming from python under Windows,
         --  since they are usually harmless, and cause confusion in the
         --  test suite and on users.

         if Host = Windows
           and then UTF8'Length > 17
           and then UTF8
             (UTF8'First .. UTF8'First + 16) = "sys:1: GtkWarning"
         then
            Trace (Me, UTF8);
         else
            Insert (Console.Buffer, Last_Iter, UTF8);
         end if;
      end if;

      if not Text_Is_Input then
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
      end if;
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
         Trace (Me, E);
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
         Trace (Me, E);
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
         Set_Policy (Console.Scrolled, Policy_Always, Policy_Always);
      else
         Set_Policy (Console.Scrolled, Policy_Automatic, Policy_Automatic);
      end if;
   exception
      when E : others => Trace (Me, E);
   end Size_Allocate_Handler;

   -----------------------------------
   -- Replace_Zeros_And_Count_Lines --
   -----------------------------------

   function Replace_Zeros_And_Count_Lines (S : in out String) return Natural is
      Lines : Natural := 0;
   begin
      for C in S'Range loop
         if S (C) = ASCII.LF then
            Lines := Lines + 1;
         end if;
         if S (C) = ASCII.NUL then
            S (C) := '0';
         end if;
      end loop;
      return Lines;
   end Replace_Zeros_And_Count_Lines;

   ----------------------
   -- Limit_Line_Count --
   ----------------------

   procedure Limit_Line_Count (B : Gtk_Text_Buffer; New_Lines : Natural) is
      Count     : constant Natural := Natural (B.Get_Line_Count);
      Start, Iter : Gtk_Text_Iter;
   begin
      if Count + New_Lines > Max_Lines_Displayed then
         B.Get_Start_Iter (Start);
         B.Get_Iter_At_Line
           (Iter, Gint (Max_Lines_Displayed - Count - New_Lines + 1));
         B.Delete (Start, Iter);
      end if;
   end Limit_Line_Count;

   --------------------------------
   -- Selection_Received_Handler --
   --------------------------------

   procedure Selection_Received_Handler
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Console : constant Interactive_Console := Interactive_Console (Widget);
      Args : constant Gtk_Args := Gtk_Args (Params);
      Data : constant Gtk_Selection_Data := From_Object
        (Get_Address (Nth (Args, 1)));
      Tmp  : Boolean := False;
   begin
      if Get_Length (Data) > 0 then
         declare
            Str   : constant String := Strip_CR (Get_Data_As_String (Data));
            Index : Natural;
            Next  : Natural;
         begin
            if Console.On_Key /= null then
               --  We execute the callback for each input character. If at
               --  least one of them returns True, we will not process Str
               --  through the standard console mechanism.
               --  The string is UTF8, since it comes from gtk+

               Index := Str'First;
               while Index <= Str'Last loop
                  Next  := UTF8_Next_Char (Str, Index);

                  Tmp := Tmp
                    or Console.On_Key
                      (Console,
                       Modifier  => 0,
                       Uni       => UTF8_Get_Char (Str (Index .. Next - 1)),
                       User_Data => Console.Key_User_Data);

                  Index := Next;
               end loop;
            end if;

            if Tmp then
               Gtk.Handlers.Emit_Stop_By_Name
                 (Console.View, Signal_Selection_Received);
            else
               Insert_And_Execute (Console, Str);
            end if;
         end;
      end if;

   exception
      when E : others => Trace (Me, E);
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
      if Console.Idle_Registered
        and then Console.Idle_Id /= 0
      then
         Remove (Console.Idle_Id);
         Console.Idle_Id := 0;
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
         Trace (Me, E);
         return False;
   end Delete_Event_Handler;

   --------------------------------
   -- Default_Completion_Handler --
   --------------------------------

   function Default_Completion_Handler
     (Input     : String;
      View      : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      User_Data : System.Address) return String_List_Utils.String_List.Vector
   is
      pragma Unreferenced (User_Data);
      use String_Lists;
      Console     : constant Interactive_Console := From_View (View);
      Completions : String_Lists.List;
   begin
      return Result : String_List_Utils.String_List.Vector do
         if Console.Virtual /= null
           and then Interactive_Virtual_Console
             (Console.Virtual).Script /= null
         then
            Complete (Interactive_Virtual_Console (Console.Virtual).Script,
                      Input, Completions);

            for Item of Completions loop
               Result.Append (Item);
            end loop;
         end if;
      end return;
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
         begin
            Execute_Command
              (Script  => Interactive_Virtual_Console (Console.Virtual).Script,
               CL           => Parse_String
                 (Input,
                  Command_Line_Treatment
                    (Interactive_Virtual_Console (Console.Virtual).Script)),
               Show_Command => False,
               Hide_Output  => False,
               Errors       => Errors);

            --  Preserve the focus on the console after an interactive
            --  execution, although the command might have destroyed the MDI

            if Get_MDI (Console.Kernel) /= null then
               Grab_Toplevel_Focus (Get_MDI (Console.Kernel), Console.View);
            end if;

            --  Output is done via the scripting language already
            return "";
         end;
      end if;
      return "";

   exception
      when E : others =>
         Trace (Me, E);
         return "";
   end Default_Command_Handler;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Console : access Interactive_Console_Record'Class) return Boolean is
   begin
      if Console.Interrupt /= null then
         return Console.Interrupt (Console, Console.Interrupt_User_Data);
      end if;

      return False;
   end Interrupt;

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
      Ignore      : Boolean;

   begin
      if Console.On_Key /= null then
         if Console.On_Key (Console   => Console,
                            Modifier  => Get_State (Event),
                            Key       => Key,
                            Uni       => To_Unicode (Key),
                            User_Data => Console.Key_User_Data)
         then
            return True;
         end if;
      end if;

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
                  Ignore := Scroll_To_Iter
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
                     Uneditable_Tag  => Console.Tags (Uneditable_Tag),
                     User_Data       => Console.Completion_User_Data);

               else
                  Do_Completion
                    (View            => Console.View,
                     Completion      => Console.Completion,
                     Prompt_End_Mark => Console.Prompt_Mark,
                     Uneditable_Tag  => Console.Tags (Uneditable_Tag),
                     User_Data       => Console.Completion_User_Data);
               end if;
               Console.Internal_Insert := False;
               return True;
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
            Backward_Char (Last_Iter, Ignore);

            declare
               Command : constant String :=
                           Get_Slice (Console.Buffer, Prompt_Iter, Last_Iter);
               H       : String_List_Access;
            begin
               if Command = ""
                 and then Console.Empty_Equals_Repeat
                 and then Console.History /= null
               then
                  --  Move the prompt mark, since the text has been submitted
                  --  and should not be submitted again later.
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

               else
                  --  Move the prompt mark, so that what the user has typed
                  --  is not moved back to after the output of the command, as
                  --  if the user was still typing it
                  Display_Text_As_Prompt (Console, "");
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
         Trace (Me, E);
         return False;
   end Key_Press_Handler;

   ----------------------------
   -- Display_Text_As_Prompt --
   ----------------------------

   procedure Display_Text_As_Prompt
     (Console : access Interactive_Console_Record'Class; Txt : String)
   is
      First_Iter  : Gtk_Text_Iter;
      Prompt_Iter : Gtk_Text_Iter;
      Offset      : Gint;
   begin
      if not Console.Input_Blocked then
         Get_End_Iter (Console.Buffer, First_Iter);
         Offset := Get_Offset (First_Iter);

         if Txt /= "" then
            Insert (Console.Buffer, First_Iter, Txt);
         end if;

         Get_End_Iter (Console.Buffer, Prompt_Iter);
         Get_Iter_At_Offset (Console.Buffer, First_Iter, Offset);

         Apply_Tag
           (Console.Buffer,
            Console.Tags (Uneditable_Tag), First_Iter, Prompt_Iter);
         Apply_Tag
           (Console.Buffer,
            Console.Tags (Prompt_Tag), First_Iter, Prompt_Iter);
      else
         Get_End_Iter (Console.Buffer, Prompt_Iter);
      end if;

      Move_Mark (Console.Buffer, Console.Prompt_Mark, Prompt_Iter);
      Scroll_Mark_Onscreen (Console.View, Console.Prompt_Mark);
   end Display_Text_As_Prompt;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Console : access Interactive_Console_Record'Class) is
   begin
      if Console.Prompt /= null then
         Display_Text_As_Prompt (Console, Console.Prompt.all);
      end if;
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
      if Console.Prompt /= null then
         if Selection_Exists (Console.Buffer) then
            return False;
         end if;

         Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
         Get_Iter_At_Mark (Console.Buffer, Cursor_Iter, Console.Insert_Mark);

         if Compare (Cursor_Iter, Prompt_Iter) < 0 then
            Place_Cursor (Console.Buffer, Prompt_Iter);
            Console.Insert_Mark := Get_Insert (Console.Buffer);
         end if;
      end if;

      return False;
   end Place_Cursor_At_Prompt;

   --------------------
   -- Replace_Cursor --
   --------------------

   procedure Replace_Cursor (Console : Interactive_Console) is
   begin
      if not Console.Idle_Registered then
         Console.Idle_Registered := True;
         Console.Idle_Id :=
           Console_Idle.Idle_Add
             (Place_Cursor_At_Prompt'Access,
              Console,
              Notify => Destroy_Idle'Access);
      end if;
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

      if Console.In_Destruction then
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
      when E : others => Trace (Me, E);
   end Mark_Set_Handler;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Console : access Gtk_Widget_Record'Class) is
      C : constant Interactive_Console := Interactive_Console (Console);
   begin
      if C.Idle_Registered
        and then C.Idle_Id /= 0
      then
         Remove (C.Idle_Id);
         C.Idle_Id := 0;
      end if;

      Delete_Hyper_Links (C);

      for J in C.Tags'Range loop
         Unref (C.Tags (J));
      end loop;

      Free (C.Key);
      Free (C.Prompt);
      Free (C.User_Input);

      --  Disconnect virtual console (used in scripts) and real console

      if C.Virtual /= null then
         if Interactive_Virtual_Console (C.Virtual).Script /= null then
            Set_Default_Console
              (Interactive_Virtual_Console (C.Virtual).Script, null);
         end if;
         Interactive_Virtual_Console (C.Virtual).Console := null;

         --  Do not free memory, since we could still have instances of
         --  GPS.Console around. This memory will be freed when the last
         --  such instance is destroyed
         --  Unchecked_Free (C.Virtual);

         C.Virtual := null;
      end if;
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Console             : out Interactive_Console;
      Kernel              : access Kernel_Handle_Record'Class;
      Prompt              : String;
      Handler             : Command_Handler := Default_Command_Handler'Access;
      User_Data           : System.Address;
      History_List        : Histories.History;
      Key                 : Histories.History_Key;
      Highlight           : Preference := null;
      Wrap_Mode           : Gtk.Enums.Gtk_Wrap_Mode := Gtk.Enums.Wrap_None;
      Empty_Equals_Repeat : Boolean := False;
      ANSI_Support        : Boolean := False;
      Manage_Prompt       : Boolean := True;
      Toolbar_Name        : String := "") is
   begin
      Console := new Interactive_Console_Record;
      Initialize (Console, Kernel, Prompt, Handler, User_Data,
                  History_List, Key, Highlight, Wrap_Mode,
                  Empty_Equals_Repeat, ANSI_Support, Manage_Prompt,
                  Toolbar_Name => Toolbar_Name);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console             : access Interactive_Console_Record'Class;
      Kernel              : access Kernel_Handle_Record'Class;
      Prompt              : String;
      Handler             : Command_Handler := Default_Command_Handler'Access;
      User_Data           : System.Address;
      History_List        : Histories.History;
      Key                 : Histories.History_Key;
      Highlight           : Preference := null;
      Wrap_Mode           : Gtk.Enums.Gtk_Wrap_Mode;
      Empty_Equals_Repeat : Boolean := False;
      ANSI_Support        : Boolean := False;
      Manage_Prompt       : Boolean := True;
      Toolbar_Name        : String := "")
   is
      Iter : Gtk_Text_Iter;
      Term : Gtkada_Terminal;
   begin
      --  Initialize the text buffer and the text view

      if Manage_Prompt then
         Console.Prompt := new String'(Prompt);
      end if;

      Set_Command_Handler (Console, Handler, User_Data);
      Console.Key := new String'(String (Key));
      Console.History := History_List;
      Console.Highlight := Highlight;

      Initialize_Vbox (Console, Homogeneous => False);

      if Toolbar_Name /= "" then
         declare
            Toolbar : Gtk_Toolbar;
         begin
            Create_Toolbar (Kernel, Toolbar, Id => Toolbar_Name);
            Toolbar.Set_Style (Toolbar_Icons);
            Get_Style_Context (Toolbar).Add_Class ("gps-local-toolbar");
            Console.Pack_Start (Toolbar, Expand => False, Fill => False);
            Console.Set_Toolbar (Toolbar);
            Toolbar.Show_All;
         end;
      end if;

      Gtk_New (Console.Scrolled);
      Console.Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Console.Pack_Start (Console.Scrolled, Expand => True, Fill => True);

      if ANSI_Support then
         Gtk_New (Term, Prevent_Cursor_Motion_With_Mouse => True);
         Console.Buffer := Gtk_Text_Buffer (Term);
      else
         Gtk_New (Console.Buffer);
      end if;

      Gtk_New (Console.View, Console.Buffer);
      Set_Wrap_Mode (Console.View, Wrap_Mode);

      Set_Left_Margin (Console.View, 4);

      --  The buffer should be destroyed when the view is destroyed
      --  ??? Perhaps we should store it in the module_id, and always reuse it
      --  when the console is created. This allows the user to destroy the
      --  console without losing its contents.
      Unref (Console.Buffer);

      --  Create a new Gtk_Tag for each Console specific tag (i.e: not linker
      --  with a general style defined in GPS.Default_Styles).
      for J in Uneditable_Tag .. Highlight_Tag loop
         Gtk_New (Console.Tags (J));
         Add (Get_Tag_Table (Console.Buffer), Console.Tags (J));
      end loop;

      Set_Property
        (Console.Tags (Uneditable_Tag), Gtk.Text_Tag.Editable_Property, False);

      Set_Property
        (Console.Tags (External_Messages_Tag),
         Gtk.Text_Tag.Style_Property,
         Pango_Style_Normal);

      --  Retrieve the tag used for hyperlinks from GPS.Default_Styles, add it
      --  to the buffer's tag table and tell to underline all the hyperlinks.
      Console.Tags (Hyper_Links_Tag) := Get_Tag (Hyper_Links_Default_Style);
      Add (Get_Tag_Table (Console.Buffer), Console.Tags (Hyper_Links_Tag));
      Set_Property
        (Console.Tags (Hyper_Links_Tag),
         Gtk.Text_Tag.Underline_Property,
         Pango_Underline_Single);

      Set_Highlight_Color (Console, Highlight);

      Console.Scrolled.Add (Console.View);

      Console.Internal_Insert := True;

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

      Set_Completion_Handler
        (Console, Default_Completion_Handler'Access, User_Data);
      Set_Interrupt_Handler
        (Console, Default_Interrupt_Handler'Access, User_Data);

      Console.Internal_Insert := False;

      Console.Empty_Equals_Repeat := Empty_Equals_Repeat;

      Preferences_Changed_Hook.Add
         (new On_Pref_Changed'
             (Preferences_Hooks_Function with Console => Console),
          Watch => Console.View);
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

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Term  : not null access Interactive_Console_Record'Class;
      Color : Gtkada.Terminal.Color_Kind;
      Value : Gdk.RGBA.Gdk_RGBA) is
   begin
      if Term.Buffer.all in Gtkada.Terminal.Gtkada_Terminal_Record'Class then
         Gtkada_Terminal (Term.Buffer).Set_Foreground (Color, Value);
      end if;
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Term  : not null access Interactive_Console_Record'Class;
      Color : Gtkada.Terminal.Color_Kind;
      Value : Gdk.RGBA.Gdk_RGBA) is
   begin
      if Term.Buffer.all in Gtkada.Terminal.Gtkada_Terminal_Record'Class then
         Gtkada_Terminal (Term.Buffer).Set_Background (Color, Value);
      end if;
   end Set_Background;

   ----------------------------
   -- Set_Completion_Handler --
   ----------------------------

   procedure Set_Completion_Handler
     (Console   : access Interactive_Console_Record'Class;
      Handler   : GUI_Utils.Completion_Handler;
      User_Data : System.Address) is
   begin
      Console.Completion           := Handler;
      Console.Completion_User_Data := User_Data;
   end Set_Completion_Handler;

   ---------------------
   -- Set_Key_Handler --
   ---------------------

   procedure Set_Key_Handler
     (Console   : access Interactive_Console_Record'Class;
      Handler   : Key_Handler;
      User_Data : System.Address := System.Null_Address) is
   begin
      Console.On_Key        := Handler;
      Console.Key_User_Data := User_Data;
   end Set_Key_Handler;

   ---------------------------
   -- Set_Interrupt_Handler --
   ---------------------------

   procedure Set_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      Handler   : Interrupt_Handler;
      User_Data : System.Address := System.Null_Address) is
   begin
      Console.Interrupt := Handler;
      Console.Interrupt_User_Data := User_Data;
   end Set_Interrupt_Handler;

   -------------------------
   -- Set_Highlight_Color --
   -------------------------

   procedure Set_Highlight_Color
     (Console : access Interactive_Console_Record'Class;
      Pref    : Preference := null)
   is
      Foreground : Gdk_RGBA;
      Background : Gdk_RGBA;
   begin
      Console.Highlight := Pref;

      if Pref = null then
         return;
      end if;

      if Pref.all in Variant_Preference_Record'Class then
         Foreground := Variant_Preference (Pref).Get_Pref_Fg_Color;
         Background := Variant_Preference (Pref).Get_Pref_Bg_Color;
      elsif Pref.all in Style_Preference_Record'Class then
         Foreground := Style_Preference (Pref).Get_Pref_Fg;
         Background := Style_Preference (Pref).Get_Pref_Bg;
      elsif Pref.all in Color_Preference_Record'Class then
         Foreground := Color_Preference (Pref).Get_Pref;
         Background := Null_RGBA;
      end if;

      if Foreground /= Null_RGBA then
         Set_Property
           (Console.Tags (Highlight_Tag),
            Foreground_Rgba_Property, Foreground);
      end if;

      if Background /= Null_RGBA then
         Set_Property
           (Console.Tags (Highlight_Tag),
            Background_Rgba_Property, Background);
      end if;
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
      when E : others => Trace (Me, E);
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
      Destroyed   : Boolean := False;

      procedure On_Console_Destroy (Console, Data : System.Address);
      pragma Convention (C, On_Console_Destroy);

      procedure On_Console_Destroy (Console, Data : System.Address) is
         pragma Unreferenced (Console, Data);
      begin
         Destroyed := True;
      end On_Console_Destroy;

   begin
      --  Processing the command could close the console (for instance
      --  executing "q" in the debugger). So we need to monitor its
      --  status.

      Weak_Ref (Console, On_Console_Destroy'Unrestricted_Access);

      --  This call might close the console
      Output := new String'
        (Console.Handler (Console, Command, Console.User_Data));

      if Destroyed then
         return;
      end if;

      Weak_Unref (Console, On_Console_Destroy'Unrestricted_Access);

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
         Console.Tags (Uneditable_Tag), Prompt_Iter, Last_Iter);

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
      --  If the console is not managing the prompt itself, this subprogram
      --  should have no effect
      if Console.Prompt /= null then
         Free (Console.Prompt);
         Console.Prompt := new String'(Prompt);
      end if;
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

   ---------------
   -- From_View --
   ---------------

   function From_View
     (View    : access Gtk.Text_View.Gtk_Text_View_Record'Class)
      return Interactive_Console
   is
      P : Gtk_Widget;
   begin
      --  Depending on whether we have a local toolbar or not, the parent
      --  tree might not be the same. So we explicitly look for the parent
      --  of interest

      P := Get_Parent (View);
      while P /= null
        and then P.all not in Interactive_Console_Record'Class
      loop
         P := Get_Parent (P);
      end loop;

      return Interactive_Console (P);
   end From_View;

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
         Grab_Toplevel_Focus (Get_MDI (Console.Kernel), Get_View (Console));
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
     (Console    : access Interactive_Console_Record;
      Regexp     : GNAT.Regpat.Pattern_Matcher;
      Callback   : not null access Hyper_Link_Callback_Record'Class;
      Foreground : String;
      Background : String;
      Underline  : Boolean;
      Font       : String)
   is
      Tag : Gtk_Text_Tag;
   begin
      --  Create a new hyperlink tag if non-empty values have been provided for
      --  at least one of the parameters. Otherwise, use the defaut hyperlink
      --  tag.
      if Foreground /= "" or else Background /= "" or else Font /= "" then
         Tag := new Hyper_Link_Tag_Record;
         Gtk.Text_Tag.Initialize (Tag);
         Add (Get_Tag_Table (Console.Buffer), Tag);

         if Font /= "" then
            Set_Property (Tag, Gtk.Text_Tag.Font_Property, Font);
         end if;

         if Foreground /= "" then
            Set_Property (Tag, Gtk.Text_Tag.Foreground_Property, Foreground);
         end if;

         if Background /= "" then
            Set_Property (Tag, Gtk.Text_Tag.Background_Property, Background);
         end if;

         if Underline then
            Set_Property
              (Tag, Gtk.Text_Tag.Underline_Property, Pango_Underline_Single);
         end if;
      else
         Tag := Console.Tags (Hyper_Links_Tag);
      end if;

      Console.Links := new Hyper_Link_Record'
        (Pattern  => new Pattern_Matcher'(Regexp),
         Callback => Hyper_Link_Callback (Callback),
         Tag      => Tag,
         Next     => Console.Links);
      Console.Links_Count := Console.Links_Count + 1;
   end Create_Hyper_Link;

   ------------------------
   -- Delete_Hyper_Links --
   ------------------------

   procedure Delete_Hyper_Links
     (Console : access Interactive_Console_Record)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Pattern_Matcher, Pattern_Matcher_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hyper_Link_Callback_Record'Class, Hyper_Link_Callback);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Hyper_Link_Record, Hyper_Links);

      procedure Get_Tag
        (Tag   : not null access Gtk_Text_Tag_Record'Class;
         Dummy : Boolean);
      --  Save given Tag into local tag array

      L, L2 : Hyper_Links;

      Table : constant Gtk_Text_Tag_Table := Get_Tag_Table (Console.Buffer);

      Tags  : array (1 .. Table.Get_Size) of Gtk_Text_Tag;
      Index : Gint := 0;

      -------------
      -- Get_Tag --
      -------------

      procedure Get_Tag
        (Tag   : not null access Gtk_Text_Tag_Record'Class;
         Dummy : Boolean)
      is
         pragma Unreferenced (Dummy);
      begin
         if Tag.all in Hyper_Link_Tag_Record'Class then
            Index := Index + 1;
            Tags (Index) := Gtk_Text_Tag (Tag);
         end if;
      end Get_Tag;

      package Foreach_Tag is new Foreach_User_Data (Boolean);
   begin
      --  Extract all tags from the table
      Foreach_Tag.Foreach (Table, Get_Tag'Access, False);

      --  Destroy all tags in the table
      for J in 1 .. Index loop
         Table.Remove (Tags (J));
      end loop;

      L := Console.Links;
      while L /= null loop
         L2 := L.Next;
         On_Destroy (L.Callback.all);
         Unchecked_Free (L.Callback);
         Unchecked_Free (L.Pattern);
         Unchecked_Free (L);
         L := L2;
      end loop;

      Console.Links := null;
      Console.Links_Count := 0;
   end Delete_Hyper_Links;

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
         Link  : Hyper_Links := null;
         First : Natural := Integer'Last;
         Last  : Natural := Integer'Last;
      end record;
      Locs : array (1 .. Console.Links_Count) of Link_And_Location;
      --  Index in Text of the first occurrence of each regexp

      Fixed : String := Text;
      --  This is copy of Text argument with ASCII.NUL chars replaced by '0'
      --  since Gtk+ does not handle them well.

      Index : Natural := Fixed'First;

      procedure Update_Pattern_Loc (L : Natural; Link : Hyper_Links);
      --  Update the next location of the pattern after Index

      ------------------------
      -- Update_Pattern_Loc --
      ------------------------

      procedure Update_Pattern_Loc (L : Natural; Link : Hyper_Links) is
         Matches : Match_Array (0 .. 1);
      begin
         --  Do nothing if the hyperlink does not apply to any regexp
         --  (i.e : when created with Insert_Hyper_Link).
         if Link.Pattern = null then
            return;
         end if;

         Match (Link.Pattern.all, Fixed, Matches, Data_First => Index);

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
      New_Lines   : Natural;

   begin
      New_Lines := Replace_Zeros_And_Count_Lines (Fixed);
      Limit_Line_Count (Console.Buffer, New_Lines);

      --  Initialize the locations array, so that we try and match the regexps
      --  as few times as possible for efficiency.
      for L in Locs'Range loop
         Update_Pattern_Loc (L, Link);
         Link := Link.Next;
      end loop;

      Prepare_For_Output
        (Console, Text_Is_Input => False,
         Internal => Internal, Last_Iter => Last_Iter);

      while Index <= Fixed'Last loop
         Min         := Fixed'Last + 1;
         Min_Pattern := Locs'Last + 1;
         for L in Locs'Range loop
            if Locs (L).First < Min then
               Min         := Locs (L).First;
               Min_Pattern := L;
            end if;
         end loop;

         if Min <= Fixed'Last then
            --  Found a regexp. Insert the leading text first, no hyper link
            if Min - 1 >= Index then
               Get_End_Iter (Console.Buffer, Start_Iter);
               if Highlight then
                  Insert_With_Tags
                    (Buffer => Console.Buffer,
                     Iter   => Start_Iter,
                     Text   => Glib.Convert.Locale_To_UTF8
                       (Fixed (Index .. Min - 1)),
                     Tag    => Console.Tags (Highlight_Tag));
               else
                  Insert
                    (Buffer => Console.Buffer,
                     Iter   => Start_Iter,
                     Text   => Glib.Convert.Locale_To_UTF8
                       (Fixed (Index .. Min - 1)));
               end if;
            end if;

            --  Then insert the hyper link
            Get_End_Iter (Console.Buffer, Start_Iter);
            Insert_With_Tags
              (Buffer         => Console.Buffer,
               Iter           => Start_Iter,
               Text           => Glib.Convert.Locale_To_UTF8
                 (Fixed (Min .. Locs (Min_Pattern).Last)),
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
                  Text   => Fixed (Index .. Fixed'Last),
                  Tag    => Console.Tags (Highlight_Tag));
            else
               Insert
                 (Buffer => Console.Buffer,
                  Iter   => Start_Iter,
                  Text   => Fixed (Index .. Fixed'Last));
            end if;
            Index := Fixed'Last + 1;
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

   -------------------
   -- Process_Lines --
   -------------------

   function Process_Lines (Self : Interactive_Console) return Boolean is
      Start : constant Time := Clock;
      U     : Unbounded_String;
   begin
      while not Self.Lines_To_Process.Is_Empty loop
         U := Self.Lines_To_Process.First_Element;
         Self.Lines_To_Process.Delete_First;

         Self.Insert_With_Links (To_String (U), Add_LF => False);

         exit when Clock - Start > Process_Lines_Timeout;
      end loop;

      if Self.Lines_To_Process.Is_Empty then
         Self.Process_Timeout := No_Source_Id;
         return False;
      else
         --  Let's process the rest later!
         return True;
      end if;
   exception
      when E : others =>
         Trace (Me, E);
         Self.Lines_To_Process.Clear;
         Self.Process_Timeout := No_Source_Id;
         return False;
   end Process_Lines;

   ---------------------------------
   -- Insert_With_Links_Protected --
   ---------------------------------

   procedure Insert_With_Links_Protected
     (Console : access Interactive_Console_Record;
      Text    : String;
      Add_LF  : Boolean := True)
   is
      Last_Start : Natural := Text'First;
   begin
      --  If we are calling this, we do not want to blindly accept any amount
      --  of incoming output and insert this in the text: this could lead to
      --  infinite loop as GPS tries to consume any amount of output. What we
      --  do instead is the following:
      --     - limit the number of lines displayed
      --     - store text to display in Lines_To_Process
      --     - process the lines in a timeout

      for J in Text'Range loop
         Console.Last_Line_Break := Console.Last_Line_Break + 1;
         if Text (J) = ASCII.LF then
            Console.Last_Line_Break := 0;
         end if;

         if Console.Last_Line_Break > Max_Line_Length then
            Console.Lines_To_Process.Append
              (To_Unbounded_String (Text (Last_Start .. J) & ASCII.LF));
            Console.Last_Line_Break := 0;
            Last_Start := J + 1;
         end if;
      end loop;

      if Add_LF then
         Console.Lines_To_Process.Append
           (To_Unbounded_String (Text (Last_Start .. Text'Last) & ASCII.LF));
      else
         Console.Lines_To_Process.Append
           (To_Unbounded_String (Text (Last_Start .. Text'Last)));
      end if;

      if Console.Process_Timeout = No_Source_Id then
         --  We are currently not processing lines: do one call to
         --  Process_Lines immediately (useful to have output available
         --  right away for trivial tests / for the testsuite) - if this
         --  call returns True, it means we have more processing to do:
         --  do this in a timeout.
         if Process_Lines (Interactive_Console (Console)) then
            Console.Process_Timeout := Console_Idle.Timeout_Add
              (Interval => Line_Processing_Interval,
               Func     => Process_Lines'Access,
               Data     => Console);
         end if;
      end if;
   end Insert_With_Links_Protected;

   -----------------------
   -- Insert_Hyper_Link --
   -----------------------

   procedure Insert_Hyper_Link
     (Console  : not null access Interactive_Console_Record;
      Text     : String;
      Callback : not null access Hyper_Link_Callback_Record'Class)
   is
      Internal  : Boolean;
      Last_Iter : Gtk_Text_Iter;
      Tag       : constant Gtk_Text_Tag := Console.Tags (Hyper_Links_Tag);
   begin
      --  Create the hyper link
      Console.Links := new Hyper_Link_Record'
        (Pattern  => null,
         Callback => Hyper_Link_Callback (Callback),
         Tag      => Tag,
         Next     => Console.Links);
      Console.Links_Count := Console.Links_Count + 1;

      --  Insert it in the console
      Prepare_For_Output
        (Console,
         Text_Is_Input => False,
         Internal      => Internal,
         Last_Iter     => Last_Iter);

      Get_End_Iter (Console.Buffer, Last_Iter);
      Insert_With_Tags
        (Buffer => Console.Buffer,
         Iter   => Last_Iter,
         Text   => Text,
         Tag    => Tag);

      Terminate_Output (Console, Internal, Show_Prompt => False);
   end Insert_Hyper_Link;

   ---------------
   -- Interrupt --
   ---------------

   overriding function Interrupt
     (Child : access GPS_Console_MDI_Child_Record) return Boolean
   is
      Console : constant Interactive_Console :=
                  Interactive_Console (Get_Widget (Child));
   begin
      return Interrupt (Console);
   end Interrupt;

   -----------------------------
   -- Get_Interactive_Console --
   -----------------------------

   function Get_Interactive_Console
     (Self : access Console_Messages_Window)
      return Interactive_Console is
   begin
      return Interactive_Console (Self.Console);
   end Get_Interactive_Console;

   ---------------------------------
   -- Get_Console_Messages_Window --
   ---------------------------------

   function Get_Console_Messages_Window
     (Self : access Interactive_Console_Record'Class)
      return access Console_Messages_Window is
   begin
      if Self = null then
         return null;
      elsif Self.Console = null then
         Self.Console := new Console_Messages_Window'(Console => Self);
      end if;

      return Self.Console;
   end Get_Console_Messages_Window;

   ------------
   -- Insert --
   ------------

   overriding procedure Insert
     (Self   : not null access Console_Messages_Window;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : GPS.Kernel.Message_Type)
   is
      use type GPS.Kernel.Message_Type;
   begin
      Self.Console.Insert (Text, Add_LF, Mode = GPS.Kernel.Error);
   end Insert;

   -----------------
   -- Insert_UTF8 --
   -----------------

   overriding procedure Insert_UTF8
     (Self   : not null access Console_Messages_Window;
      UTF8   : String;
      Add_LF : Boolean := True;
      Mode   : GPS.Kernel.Message_Type)
   is
      use type GPS.Kernel.Message_Type;
   begin
      Self.Console.Insert_UTF8 (UTF8, Add_LF, Mode = GPS.Kernel.Error);
   end Insert_UTF8;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self   : not null access Console_Messages_Window) is
   begin
      Self.Console.Clear;
   end Clear;

   -------------------
   -- Raise_Console --
   -------------------

   overriding procedure Raise_Console
     (Self       : not null access Console_Messages_Window;
      Give_Focus : Boolean) is
   begin
      Raise_Child (Find_MDI_Child_From_Widget (Self.Console), Give_Focus);
   end Raise_Console;

   -------------------------
   -- Get_Virtual_Console --
   -------------------------

   overriding function Get_Virtual_Console
     (Self : not null access Console_Messages_Window)
      return GNATCOLL.Scripts.Virtual_Console is
   begin
      return Self.Console.Get_Or_Create_Virtual_Console;
   end Get_Virtual_Console;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel, Pref);
   begin
      if Self.Console.Highlight /= null then
         Set_Highlight_Color (Self.Console, Self.Console.Highlight);
      end if;
   end Execute;

   ------------
   -- Export --
   ------------

   function Export
     (View : access Interactive_Console_Record;
      File : GNATCOLL.VFS.Virtual_File)
      return Boolean
   is
      Writable    : Writable_File := File.Write_File;
      Start, Last : Gtk_Text_Iter;
      Result : Boolean := True;
   begin
      View.Buffer.Get_Bounds (Start, Last);
      begin
         Write (Writable, String'(View.Buffer.Get_Text (Start, Last)));
      exception
         when E : others =>
            Trace (Me, E);
            Result := False;
      end;
      Close (Writable);
      return Result;
   end Export;

end Interactive_Consoles;
