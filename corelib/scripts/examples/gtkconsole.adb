
with Gdk;                 use Gdk;
with Gdk.Event;           use Gdk.Event;
with Gdk.Types;           use Gdk.Types;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Glib;                use Glib;
with Glib.Convert;        use Glib.Convert;
with Glib.Main;           use Glib.Main;
with Glib.Object;         use Glib.Object;
with GNAT.IO;             use GNAT.IO;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Main;            use Gtk.Main;
with Gtk.Object;          use Gtk.Object;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;     use Gtk.Text_Buffer;
with Gtk.Text_Iter;       use Gtk.Text_Iter;
with Gtk.Text_Mark;       use Gtk.Text_Mark;
with Gtk.Text_View;       use Gtk.Text_View;
with Gtk.Widget;          use Gtk.Widget;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Scripts;             use Scripts;
with Scripts.Gtkada;      use Scripts.Gtkada;

package body GtkConsole is

   package Console_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Gtk_Console);
   package Console_Return_Callback is new Gtk.Handlers.User_Return_Callback
     (Gtk_Widget_Record, Boolean, Gtk_Console);

   package Console_Sources is new Glib.Main.Generic_Sources (Gtk_Console);

   procedure On_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Console : Gtk_Console);
   --  Called when the console is destroyed.

   function Key_Press_Handler
     (Object  : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event;
      Console : Gtk_Console) return Boolean;
   --  Called when a key press occurs in the console

   procedure Execute_Command
     (Console : access Gtk_Console_Record'Class;
      Command : String);
   --  Execute the command

   function On_Idle (Console : Gtk_Console) return Boolean;
   --  When gtk+ is idle and we are blocked in a call to read()

   ---------------------
   -- Execute_Command --
   ---------------------

   procedure Execute_Command
     (Console : access Gtk_Console_Record'Class;
      Command : String)
   is
      Errors : Boolean;
   begin
      if Console.Script /= null then
         Execute_Command
           (Script       => Console.Script,
            Command      => Command,
            Show_Command => False,
            Hide_Output  => False,
            Errors       => Errors);
      end if;
   end Execute_Command;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Widget : access Gtk_Widget_Record'Class;
      Console : Gtk_Console) is
   begin
      if Console.Script /= null then
         Set_Default_Console (Console.Script, null);
         Console.Script := null;
      end if;
   end On_Destroy;

   ------------
   -- Create --
   ------------

   function Create
     (Wraps : access Gtk.Text_View.Gtk_Text_View_Record'Class)
      return Gtk_Console
   is
      C      : Gtk_Console := new Gtk_Console_Record;
      Buffer : Gtk_Text_Buffer;
      Iter   : Gtk_Text_Iter;
   begin
      C.View := Gtk_Text_View (Wraps);
      Console_Callback.Connect (C.View, Signal_Destroy, On_Destroy'Access, C);

      Buffer := Get_Buffer (C.View);
      Get_End_Iter (Buffer, Iter);

      C.Prompt_Mark := Create_Mark (Buffer, "", Iter);
      C.Insert_Mark := Get_Insert (Buffer);

      Console_Return_Callback.Connect
        (C.View, Signal_Key_Press_Event,
         Console_Return_Callback.To_Marshaller (Key_Press_Handler'Access),
         User_Data => C,
         After => False);

      return C;
   end Create;

   ---------
   -- Ref --
   ---------

   procedure Ref (Console : access Gtk_Console_Record) is
   begin
      Ref (Console.View);
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Console : access Gtk_Console_Record) is
   begin
      Unref (Console.View);
   end Unref;

   -----------------
   -- Insert_Text --
   -----------------

   procedure Insert_Text
     (Console : access Gtk_Console_Record; Txt : String)
   is
      UTF8        : constant String := Glib.Convert.Locale_To_UTF8 (Txt);
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (Console.View);
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Buffer, Last_Iter);

      Insert (Buffer, Last_Iter, UTF8);

      Get_Iter_At_Mark (Buffer, Prompt_Iter, Console.Prompt_Mark);
      Get_End_Iter (Buffer, Last_Iter);
   end Insert_Text;

   ----------------
   -- Insert_Log --
   ----------------

   procedure Insert_Log
     (Console : access Gtk_Console_Record; Txt : String)
   is
      pragma Unreferenced (Console);
   begin
      Put_Line ("log: " & Txt);
   end Insert_Log;

   ------------------
   -- Insert_Error --
   ------------------

   procedure Insert_Error
     (Console : access Gtk_Console_Record; Txt : String) is
   begin
      Insert_Text (Console, "error: " & Txt & ASCII.LF);
   end Insert_Error;

   -------------------
   -- Insert_Prompt --
   -------------------

   procedure Insert_Prompt
     (Console : access Gtk_Console_Record; Txt : String)
   is
      First_Iter  : Gtk_Text_Iter;
      Prompt_Iter : Gtk_Text_Iter;
      Offset      : Gint;
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (Console.View);
   begin
      Get_End_Iter (Buffer, First_Iter);
      Offset := Get_Offset (First_Iter);

      Insert (Buffer, First_Iter, Txt);

      Get_End_Iter (Buffer, Prompt_Iter);
      Get_Iter_At_Offset (Buffer, First_Iter, Offset);

--        Apply_Tag
--          (Buffer, Console.Uneditable_Tag, First_Iter, Prompt_Iter);
--        Apply_Tag
--          (Buffer, Console.Prompt_Tag, First_Iter, Prompt_Iter);

      Move_Mark (Buffer, Console.Prompt_Mark, Prompt_Iter);

      Scroll_Mark_Onscreen (Console.View, Console.Prompt_Mark);
   end Insert_Prompt;

   -----------------
   -- Grab_Events --
   -----------------

   procedure Grab_Events
     (Console : access Gtk_Console_Record; Grab : Boolean) is
   begin
      if Grab then
         Console.Took_Grab := False;

         --  Grab the mouse, keyboard,... so as to avoid recursive loops in
         --  GPS (user selecting a menu while python is running)
         Ref (Console.View);

         if Get_Window (Console.View) /= null then
            --  If we already have a grab (for instance when the user is
            --  displaying a menu and we are running the python command as a
            --  filter for that menu), no need to take another. In fact,
            --  taking another would break the above scenario, since in
            --  gtkmenu.c the handler for grab_notify cancels the menu when
            --  another grab is taken (G305-005)

            if Gtk.Main.Grab_Get_Current = null then
               Gtk.Main.Grab_Add (Console.View);
               Console.Took_Grab := True;
            end if;
         end if;

      else
         --  Note: the widget might have been destroyed by the python command,
         --  we need to check that it still exists.

         if Console.Took_Grab then
            Gtk.Main.Grab_Remove (Console.View);
            Unref (Console.View);
         end if;
      end if;
   end Grab_Events;

   ----------------------------
   -- Set_As_Default_Console --
   ----------------------------

   procedure Set_As_Default_Console
     (Console     : access Gtk_Console_Record;
      Old_Console : Virtual_Console;
      Script      : access Scripts.Scripting_Language_Record'Class)
   is
   begin
      if Virtual_Console (Console) = Old_Console then
         return;
      end if;

      Console.Script := Scripting_Language (Script);
   end Set_As_Default_Console;

   ---------
   -- Get --
   ---------

   function Get
     (Console : access Gtk_Console_Record) return Gtk_Text_View is
   begin
      return Console.View;
   end Get;

   -----------------------
   -- Key_Press_Handler --
   -----------------------

   function Key_Press_Handler
     (Object  : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event;
      Console : Gtk_Console) return Boolean
   is
      Key         : constant Gdk_Key_Type  := Get_Key_Val (Event);
      Prompt_Iter : Gtk_Text_Iter;
      Last_Iter   : Gtk_Text_Iter;
      Success     : Boolean;
      Buffer      : Gtk_Text_Buffer;

   begin
      if Console.Script /= null
        and then Get_State (Event) = Control_Mask
        and then (Key = GDK_C or Key = GDK_LC_c)
      then
         if Console.Waiting_For_Input > 0 then
            Gtk.Main.Main_Quit;
            return True;
         else
            return Interrupt (Console.Script);
         end if;
      end if;

      --  If we are not blocked in a call to read()

      if Console.Waiting_For_Input = 0 then
         case Key is
            when GDK_Return | GDK_KP_Enter =>
               Buffer := Get_Buffer (Console.View);
               Get_End_Iter (Buffer, Last_Iter);
               Insert (Buffer, Last_Iter, (1 => ASCII.LF));

               Get_Iter_At_Mark (Buffer, Prompt_Iter, Console.Prompt_Mark);
               Get_End_Iter (Buffer, Last_Iter);
               Backward_Char (Last_Iter, Success);

               declare
                  Command : constant String :=
                    Get_Slice (Buffer, Prompt_Iter, Last_Iter);
               begin
                  Put_Line ("Command: " & Command);
                  Execute_Command (Console, Command);
               end;

               return True;

            when GDK_Tab | GDK_KP_Tab =>
               if Console.Script /= null then
                  Buffer := Get_Buffer (Console.View);
                  Get_Iter_At_Mark (Buffer, Prompt_Iter, Console.Prompt_Mark);
                  Get_End_Iter (Buffer, Last_Iter);

                  declare
                     Text : constant String :=
                       Get_Slice (Buffer, Prompt_Iter, Last_Iter);
                     Completions : String_Lists.List;
                     C           : String_Lists.Cursor;
                  begin
                     Complete (Console.Script, Text, Completions);

                     --  In this simple example, the completions are output on
                     --  the terminal. In practice, once should look for the
                     --  longest matching string and/or display the list of
                     --  completions graphically

                     Put_Line ("Possible completions for """ & Text & """");
                     C := String_Lists.First (Completions);
                     while String_Lists.Has_Element (C) loop
                        Put_Line (String_Lists.Element (C));
                        String_Lists.Next (C);
                     end loop;
                  end;
                  return True;
               else
                  return False;
               end if;

            when others =>
               return False;
         end case;
      end if;

      --  Are we currently blocked in a call to read() ? If yes, stop as soon
      --  as needed
      if Console.Waiting_For_Input > 0 then
         Put_Line ("Key press: " & Key'Img & Console.Waiting_For_Input'Img);
         Console.Waiting_For_Input := Console.Waiting_For_Input - 1;
         if Console.Waiting_For_Newline
           and then (Key = GDK_Return or Key = GDK_KP_Enter)
         then
            Console.Waiting_For_Input := 0;
            Gtk.Main.Main_Quit;
         end if;
      end if;

      return False;

   exception
      when E : others =>
         return False;
   end Key_Press_Handler;

   -------------
   -- On_Idle --
   -------------

   function On_Idle (Console : Gtk_Console) return Boolean is
   begin
      if Console.Waiting_For_Input = 0 then
         Gtk.Main.Main_Quit;
      end if;
      return True;
   end On_Idle;

   ------------------------
   -- Set_Data_Primitive --
   ------------------------

   procedure Set_Data_Primitive
     (Instance : Class_Instance;
      Console  : access Gtk_Console_Record)
   is
   begin
      Scripts.Gtkada.Set_Data (Instance, GObject (Console.View));
   end Set_Data_Primitive;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Script  : access Scripting_Language_Record'Class;
      Console : access Gtk_Console_Record)
      return Class_Instance
   is
   begin
      return Scripts.Gtkada.Get_Instance (Script, GObject (Console.View));
   end Get_Instance;

   --------------------------------------
   -- Process_Pending_Events_Primitive --
   --------------------------------------

   procedure Process_Pending_Events_Primitive
     (Console : access Gtk_Console_Record)
   is
      Dead : Boolean;
      pragma Unreferenced (Dead);
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

   function Read
     (Console    : access Gtk_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String
   is
      Last_Iter, Prompt_Iter : Gtk_Text_Iter;
      End_Mark               : Gtk_Text_Mark;
      Buffer                 : Gtk_Text_Buffer := Get_Buffer (Console.View);
      Id                     : G_Source_Id;
   begin
      Get_End_Iter (Buffer, Last_Iter);
      End_Mark := Create_Mark (Buffer, "", Last_Iter);

      Console.Waiting_For_Input   := Size;
      Console.Waiting_For_Newline := Whole_Line;
      Grab_Focus (Console.View);

      Id := Console_Sources.Idle_Add
        (On_Idle'Access,
         Priority => Glib.Main.Priority_Default_Idle + 1,
         Data     => Gtk_Console (Console));
      Gtk.Main.Main;
      Remove (Id);
      Console.Waiting_For_Input := 0;

      Get_Iter_At_Mark (Buffer, Prompt_Iter, End_Mark);
      Delete_Mark (Buffer, End_Mark);
      Get_End_Iter (Buffer, Last_Iter);
      return Get_Slice (Buffer, Prompt_Iter, Last_Iter);
   end Read;

end GtkConsole;
