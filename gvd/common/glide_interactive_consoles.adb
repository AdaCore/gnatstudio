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
with Glib.Values;         use Glib.Values;
with Glib.Object;         use Glib.Object;
with Glib.Properties;     use Glib.Properties;

with Gtk.Enums;           use Gtk.Enums;
with Gtk.Main;            use Gtk.Main;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Text_Tag;        use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;  use Gtk.Text_Tag_Table;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Traces;                   use Traces;

with System;               use System;

with Ada.Exceptions;       use Ada.Exceptions;

package body Glide_Interactive_Consoles is

   Me : constant Debug_Handle := Create ("Interactive_Console");

   package Buffer_Console_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk_Text_Buffer_Record,
      User_Type   => Glide_Interactive_Console);

   package Console_Idle is new Gtk.Main.Idle (Glide_Interactive_Console);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Display_Prompt
     (Console : access Glide_Interactive_Console_Record'Class);
   --  Displays the prompt at the end of the current text

   procedure First_Insert_Text
     (Buffer  : access Gtk_Text_Buffer_Record'Class;
      Params  : Glib.Values.GValues;
      Console : Glide_Interactive_Console);
   --  Called before inserting text;

   procedure Changed_Handler
     (Buffer  : access Gtk_Text_Buffer_Record'Class;
      Console : Glide_Interactive_Console);
   --  Prevent cursor movements before the prompt.

   procedure Mark_Set_Handler
     (Buffer  : access Gtk_Text_Buffer_Record'Class;
      Params  : Glib.Values.GValues;
      Console : Glide_Interactive_Console);
   --  Prevent cursor movements before the prompt.

   function Button_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handler for the "button_press_event" signal.

   function Button_Release_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handler for the "button_release_event" signal.

   procedure Replace_Cursor (Console : Glide_Interactive_Console);
   --  If the cursor is in a forbidden zone, place it at the prompt.

   function Place_Cursor_At_Prompt
     (Console : in Glide_Interactive_Console) return Boolean;
   --  Place the cursor at the prompt mark.

   --------------------------
   -- Button_Press_Handler --
   --------------------------

   function Button_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is

      pragma Unreferenced (Params);

      Console : Glide_Interactive_Console :=
        Glide_Interactive_Console (Object);
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

      Console : Glide_Interactive_Console :=
        Glide_Interactive_Console (Object);
   begin
      Console.Button_Press := False;
      Replace_Cursor (Console);

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Release_Handler;

   --------------------
   -- Display_Prompt --
   --------------------

   procedure Display_Prompt
     (Console : access Glide_Interactive_Console_Record'Class)
   is
      First_Iter  : Gtk_Text_Iter;
      Prompt_Iter : Gtk_Text_Iter;

      Prompt : constant String := "[GPS In The Shell]$ ";

      Offset : Gint;
   begin
      Get_End_Iter (Console.Buffer, First_Iter);
      Offset := Get_Offset (First_Iter);

      --  Console.Uneditable_Length := Prompt'Length;
      Insert (Console.Buffer, First_Iter, Prompt);
      --  ??? Must add flexibility to the prompt.

      Get_End_Iter (Console.Buffer, Prompt_Iter);
      Get_Iter_At_Offset (Console.Buffer, First_Iter, Offset);

      Apply_Tag
        (Console.Buffer, Console.Uneditable_Tag, First_Iter, Prompt_Iter);

      Move_Mark (Console.Buffer, Console.Prompt_Mark, Prompt_Iter);

   end Display_Prompt;

   -----------------------
   -- First_Insert_Text --
   -----------------------

   procedure First_Insert_Text
     (Buffer  : access Gtk_Text_Buffer_Record'Class;
      Params  : Glib.Values.GValues;
      Console : Glide_Interactive_Console)
   is
      pragma Unreferenced (Buffer);

      Prompt      : Gtk_Text_Iter;
      Pos         : Gtk_Text_Iter;
      Length      : constant Gint := Get_Int (Nth (Params, 3));
      Text        : constant String :=
        Get_String (Nth (Params, 2), Length => Length);
   begin
      if Console.Internal_Insert then
         return;
      end if;

      Console.Internal_Insert := True;

      Get_Text_Iter (Nth (Params, 1), Pos);

      if Text'Length >= 1
        and then Text (Text'First) = ASCII.LF
      then
         Get_Iter_At_Mark (Console.Buffer, Prompt, Console.Prompt_Mark);

         declare
            Text    : constant String :=
              Get_Slice (Console.Buffer, Prompt, Pos);
            Command : constant String := Text (Text'First .. Text'Last - 1);
         begin
            Insert
              (Console.Buffer,
               Pos,
               Interpret_Command (Console.Kernel, Command));

            --  ??? Must add Command to the history.

            Display_Prompt (Console);
         end;
      end if;

      Console.Internal_Insert := False;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end First_Insert_Text;

   ----------------------------
   -- Place_Cursor_At_Prompt --
   ----------------------------

   function Place_Cursor_At_Prompt
     (Console : in Glide_Interactive_Console) return Boolean
   is
      Prompt_Iter   : Gtk_Text_Iter;
      Cursor_Iter   : Gtk_Text_Iter;

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

   procedure Replace_Cursor (Console : Glide_Interactive_Console) is
      Id            : Idle_Handler_Id;
   begin
      Id := Console_Idle.Add (Place_Cursor_At_Prompt'Access, Console);
   end Replace_Cursor;

   ---------------------
   -- Changed_Handler --
   ---------------------

   procedure Changed_Handler
     (Buffer  : access Gtk_Text_Buffer_Record'Class;
      Console : Glide_Interactive_Console)
   is
      pragma Unreferenced (Buffer);

   begin
      if Console.Internal_Insert then
         return;
      end if;

      Console.Internal_Insert := True;
      Replace_Cursor (Console);
      Console.Internal_Insert := False;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Changed_Handler;

   ----------------------
   -- Mark_Set_Handler --
   ----------------------

   procedure Mark_Set_Handler
     (Buffer  : access Gtk_Text_Buffer_Record'Class;
      Params  : Glib.Values.GValues;
      Console : Glide_Interactive_Console)
   is
      pragma Unreferenced (Buffer);

      Mark : constant Gtk_Text_Mark :=
        Get_Text_Mark (Glib.Values.Nth (Params, 2));
      Mark_Name : constant String := Get_Name (Mark);
   begin
      --  Prevent recursion

      if Console.Internal_Insert then
         return;
      end if;

      Console.Internal_Insert := True;

      if Console.Insert_Mark = null
        or else Get_Object (Mark) /= Get_Object (Console.Insert_Mark)
      then
         --  If the mark corresponds to a cursor position, set the stored
         --  Insert_Mark accordingly.

         if Mark_Name = "insert"
           or else Mark_Name = "gtk_drag_target"
         then
            Console.Insert_Mark := Mark;
         end if;
      end if;

      Console.Internal_Insert := False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Mark_Set_Handler;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Console : out Glide_Interactive_Console;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Console := new Glide_Interactive_Console_Record;
      Initialize (Console, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console : access Glide_Interactive_Console_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Iter : Gtk_Text_Iter;
   begin
      --  Initialize the text buffer and the text view.

      Gtk.Scrolled_Window.Initialize (Console);
      Set_Policy
        (Console,
         H_Scrollbar_Policy => Policy_Automatic,
         V_Scrollbar_Policy => Policy_Automatic);

      Gtk_New (Console.Buffer);
      Gtk_New (Console.View, Console.Buffer);

      Gtk_New (Console.Uneditable_Tag);
      Set_Property (Console.Uneditable_Tag, Editable_Property, False);
      Add (Get_Tag_Table (Console.Buffer), Console.Uneditable_Tag);

      Add (Console, Console.View);

      Console.Internal_Insert := True;

      Console.Kernel := Kernel_Handle (Kernel);

      Set_Size_Request (Console, -1, 100);

      Modify_Font (Console.View, Get_Pref (Kernel, Source_Editor_Font));

      Buffer_Console_Callback.Connect
        (Console.Buffer, "insert_text",
         Cb        => First_Insert_Text'Access,
         After     => True,
         User_Data => Glide_Interactive_Console (Console));

      if 1 = 2 then
         Buffer_Console_Callback.Connect
           (Console.Buffer, "changed",
            Buffer_Console_Callback.To_Marshaller (Changed_Handler'Access),
            User_Data => Glide_Interactive_Console (Console),
            After => True);

      end if;

      Buffer_Console_Callback.Connect
        (Console.Buffer, "mark_set",
         Cb => Mark_Set_Handler'Access,
         User_Data => Glide_Interactive_Console (Console),
         After => False);

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

      Get_End_Iter (Console.Buffer, Iter);

      Console.Prompt_Mark := Create_Mark (Console.Buffer, "", Iter);
      Console.Insert_Mark := Get_Insert (Console.Buffer);

      Console.Internal_Insert := False;

      Display_Prompt (Console);
   end Initialize;

   -----------
   -- Clear --
   -----------

   procedure Clear (Console : access Glide_Interactive_Console_Record) is
      pragma Unreferenced (Console);
   begin
      null;
   end Clear;
end Glide_Interactive_Consoles;
