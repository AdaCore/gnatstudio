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
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Pango.Font;          use Pango.Font;
with Pango.Enums;         use Pango.Enums;

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

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Handler for the "key_press_event" signal.

   procedure Replace_Cursor (Console : Glide_Interactive_Console);
   --  If the cursor is in a forbidden zone, place it at the prompt.

   function Place_Cursor_At_Prompt
     (Console : in Glide_Interactive_Console) return Boolean;
   --  Place the cursor at the prompt mark.

   ---------------------------
   -- Enable_Prompt_Display --
   ---------------------------

   procedure Enable_Prompt_Display
     (Console : access Glide_Interactive_Console_Record;
      Enable  : Boolean)
   is
      Last_Iter : Gtk_Text_Iter;
   begin
      Set_Editable (Console.View, Enable);
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
     (Console : access Glide_Interactive_Console_Record;
      Text    : String;
      Add_LF  : Boolean := True)
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

      if not Console.Message_Was_Displayed then
         Insert (Console.Buffer, Last_Iter, "" & ASCII.LF);
         Get_End_Iter (Console.Buffer, Last_Iter);
      end if;

      if Add_LF then
         Insert (Console.Buffer, Last_Iter, Text & ASCII.LF);
      else
         Insert (Console.Buffer, Last_Iter, Text);
      end if;

      Get_Iter_At_Mark (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Console.Buffer, Last_Iter);

      Success :=
        Scroll_To_Iter (Console.View, Last_Iter, 0.0, False, 0.0, 0.0);

      Apply_Tag
        (Console.Buffer, Console.Uneditable_Tag, Prompt_Iter, Last_Iter);

      Forward_Chars (Prompt_Iter, Console.User_Input'Length, Success);
      Apply_Tag
        (Console.Buffer,
         Console.External_Messages_Tag,
         Prompt_Iter, Last_Iter);

      Display_Prompt (Console);

      Console.Message_Was_Displayed := True;
      Console.Internal_Insert := Internal;
   end Insert;

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

      Console : constant Glide_Interactive_Console :=
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

   -----------------------
   -- Key_Press_Handler --
   -----------------------

   function Key_Press_Handler
     (Object : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      Console : constant Glide_Interactive_Console :=
        Glide_Interactive_Console (Object);

      Key         : constant Gdk_Key_Type  := Get_Key_Val (Event);
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;
   begin
      case Key is
         when GDK_Up =>
            if Console.Current_Position = Null_Node then
               Console.Current_Position := First (Console.History);

               Get_Iter_At_Mark
                 (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
               Get_End_Iter (Console.Buffer, Last_Iter);
               Delete (Console.Buffer, Prompt_Iter, Last_Iter);

               if Console.Current_Position /= Null_Node then
                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Insert
                    (Console.Buffer,
                     Prompt_Iter,
                     Data (Console.Current_Position));
                  Get_End_Iter (Console.Buffer, Last_Iter);
                  Place_Cursor (Console.Buffer, Last_Iter);
               end if;
            else
               if Next (Console.Current_Position) = Null_Node then
                  null;
               else
                  Console.Current_Position := Next (Console.Current_Position);

                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Get_End_Iter (Console.Buffer, Last_Iter);
                  Delete (Console.Buffer, Prompt_Iter, Last_Iter);

                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Insert
                    (Console.Buffer,
                     Prompt_Iter,
                     Data (Console.Current_Position));
                  Get_End_Iter (Console.Buffer, Last_Iter);
                  Place_Cursor (Console.Buffer, Last_Iter);
               end if;
            end if;

            return True;

         when GDK_Down =>
            if Console.Current_Position = Null_Node then
               null;
            else
               Console.Current_Position :=
                 Prev (Console.History, Console.Current_Position);

               Get_Iter_At_Mark
                 (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
               Get_End_Iter (Console.Buffer, Last_Iter);
               Delete (Console.Buffer, Prompt_Iter, Last_Iter);

               if Console.Current_Position /= Null_Node then
                  Get_Iter_At_Mark
                    (Console.Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Insert
                    (Console.Buffer,
                     Prompt_Iter,
                     Data (Console.Current_Position));
                  Get_End_Iter (Console.Buffer, Last_Iter);
                  Place_Cursor (Console.Buffer, Last_Iter);
               end if;
            end if;

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
     (Console : access Glide_Interactive_Console_Record'Class)
   is
      First_Iter  : Gtk_Text_Iter;
      Prompt_Iter : Gtk_Text_Iter;

      Prompt : constant String := "[GPS In The Shell]$ ";

      Offset : Gint;
   begin
      if Console.Input_Blocked then
         return;
      end if;

      Get_End_Iter (Console.Buffer, First_Iter);
      Offset := Get_Offset (First_Iter);

      Insert (Console.Buffer, First_Iter, Prompt);
      --  ??? Must add flexibility to the prompt.

      Get_End_Iter (Console.Buffer, Prompt_Iter);
      Get_Iter_At_Offset (Console.Buffer, First_Iter, Offset);

      Apply_Tag
        (Console.Buffer, Console.Uneditable_Tag, First_Iter, Prompt_Iter);
      Apply_Tag
        (Console.Buffer, Console.Prompt_Tag, First_Iter, Prompt_Iter);

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
            Output  : constant String :=
              Interpret_Command (Console.Kernel, Command);
         begin
            Get_End_Iter (Console.Buffer, Pos);
            Insert (Console.Buffer, Pos, Output);

            if Command /= "" then
               Prepend (Console.History, Command);
               Console.Current_Position := Null_Node;
            end if;

            Get_Iter_At_Mark (Console.Buffer, Prompt, Console.Prompt_Mark);
            Get_End_Iter (Console.Buffer, Pos);

            Apply_Tag
              (Console.Buffer, Console.Uneditable_Tag, Prompt, Pos);

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

      if not Console.Button_Press then
         Replace_Cursor (Console);
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

      Gtk_New (Console.External_Messages_Tag);
      Set_Property
        (Console.External_Messages_Tag,
         Gtk.Text_Tag.Style_Property,
         Pango_Style_Oblique);
      Add (Get_Tag_Table (Console.Buffer), Console.External_Messages_Tag);

      Gtk_New (Console.Prompt_Tag);
      Set_Property
        (Console.Prompt_Tag,
         Gtk.Text_Tag.Font_Desc_Property,
         Get_Pref (Kernel, Keyword_Font));

      Add (Get_Tag_Table (Console.Buffer), Console.Prompt_Tag);

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

      Buffer_Console_Callback.Connect
        (Console.Buffer, "mark_set",
         Cb => Mark_Set_Handler'Access,
         User_Data => Glide_Interactive_Console (Console),
         After => True);

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
