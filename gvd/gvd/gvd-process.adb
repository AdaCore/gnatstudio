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

with Glib;         use Glib;

with Gdk.Input;
with Gdk.Types;
with Gdk.Cursor;   use Gdk.Cursor;
with Gdk.Color;    use Gdk.Color;
with Gdk.Cursor;   use Gdk.Cursor;
with Gdk.Types;    use Gdk.Types;
with Gdk.Window;   use Gdk.Window;
with Gdk.Event;    use Gdk.Event;

with Gtk.Text;     use Gtk.Text;
with Gtk.Main;     use Gtk.Main;
with Gtk.Menu;     use Gtk.Menu;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label;    use Gtk.Label;
with Gtk.Object;   use Gtk.Object;
with Gtk.Dialog;   use Gtk.Dialog;
with Gtk.Window;   use Gtk.Window;

with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Text_IO;     use Ada.Text_IO;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with Gtkada.Canvas;   use Gtkada.Canvas;
with Gtkada.Types;    use Gtkada.Types;
with Gtkada.Handlers; use Gtkada.Handlers;
with Odd.Pixmaps;     use Odd.Pixmaps;
with Display_Items;   use Display_Items;
with Debugger.Gdb;    use Debugger.Gdb;
with Debugger.Jdb;    use Debugger.Jdb;
with Odd.Strings;     use Odd.Strings;
with Odd.Types;       use Odd.Types;
with Process_Proxies; use Process_Proxies;
with Odd.Code_Editors; use Odd.Code_Editors;
with GNAT.Regpat;     use GNAT.Regpat;
with Gtk.Handlers;    use Gtk.Handlers;
with Odd.Menus;       use Odd.Menus;
with Items.Simples;   use Items.Simples;

with Main_Debug_Window_Pkg;      use Main_Debug_Window_Pkg;
with Breakpoints_Pkg;            use Breakpoints_Pkg;
with Breakpoints_Pkg.Callbacks;  use Breakpoints_Pkg.Callbacks;
with System;
with Unchecked_Conversion;

pragma Warnings (Off, Debugger.Jdb);

package body Odd.Process is

   Enable_Block_Search : constant Boolean := False;
   --  Whether we should try to find the block of a variable when printing
   --  it, and memorize it with the item.

   Process_User_Data_Name : constant String := "odd_editor_to_process";
   --  User data string.
   --  ??? Should use some quarks, which would be just a little bit faster.

   package Canvas_Event_Handler is new Gtk.Handlers.Return_Callback
     (Debugger_Process_Tab_Record, Boolean);

   package My_Input is new Gdk.Input.Input_Add (Debugger_Process_Tab_Record);

   function To_Main_Debug_Window is new
     Unchecked_Conversion (System.Address, Main_Debug_Window_Access);

   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused
   Class_Record : System.Address := System.Null_Address;

   --  Array of the signals created for this widget
   Signals : Chars_Ptr_Array := "process_stopped" + "context_changed";

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Gint is new Unchecked_Conversion (File_Descriptor, Gint);

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  Called whenever some output becomes available from the debugger.
   --  All it does is read all the available data and call the filters
   --  that were set for the debugger.

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address);
   --  Standard handler to add gdb's output to the debugger window.

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access Main_Debug_Window_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor) return Debugger_Process_Tab
   is
      Page      : Gtk_Widget;
      Num_Pages : Gint :=
        Gint (Page_List.Length
          (Get_Children (Main_Debug_Window.Process_Notebook)));
      Process   : Debugger_Process_Tab;
   begin
      --  For all the process tabs in the application, check whether
      --  this is the one associated with Pid.

      for Page_Num in 0 .. Num_Pages - 1 loop
         Page := Get_Nth_Page (Main_Debug_Window.Process_Notebook, Page_Num);
         if Page /= null then
            Process := Process_User_Data.Get (Page);

            --  Note: The process might have been already killed when this
            --  function is called.

            if Get_Descriptor
              (Get_Process (Process.Debugger)).all = Descriptor
            then
               return Process;
            end if;
         end if;
      end loop;

      raise Debugger_Not_Found;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Text : access Odd.Code_Editors.Code_Editor_Record'Class)
     return Debugger_Process_Tab
   is
   begin
      return Process_User_Data.Get (Text, Process_User_Data_Name);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access Gtk.Window.Gtk_Window_Record'Class;
      Debugger : access Debugger_Root'Class)
     return Debugger_Process_Tab
   is
   begin
      return Convert (Main_Debug_Window_Access (Main_Debug_Window),
                      Get_Descriptor (Get_Process (Debugger)).all);
   end Convert;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   procedure Text_Output_Handler
     (Process : Debugger_Process_Tab;
      Str     : String;
      Is_Command : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Start   : Positive := Str'First;

   begin
      Freeze (Process.Debugger_Text);
      Set_Point (Process.Debugger_Text, Get_Length (Process.Debugger_Text));

      --  ??? Should strip ^M from the text.

      --  Should all the string be highlighted ?

      if Is_Command then
         Insert (Process.Debugger_Text,
                 Process.Debugger_Text_Font,
                 Process.Debugger_Text_Highlight_Color,
                 Null_Color,
                 Str);

      --  If not, highlight only parts of it

      else
         while Start <= Str'Last loop
            Match (Highlighting_Pattern (Process.Debugger),
                   Str (Start .. Str'Last),
                   Matched);
            if Matched (0) /= No_Match then
               if Matched (0).First - 1 >= Start then
                  Insert (Process.Debugger_Text,
                          Process.Debugger_Text_Font,
                          Black (Get_System),
                          Null_Color,
                          Str (Start .. Matched (0).First - 1));
               end if;

               Insert (Process.Debugger_Text,
                       Process.Debugger_Text_Font,
                       Process.Debugger_Text_Highlight_Color,
                       Null_Color,
                       Str (Matched (0).First .. Matched (0).Last));
               Start := Matched (0).Last + 1;
            else
               Insert (Process.Debugger_Text,
                       Process.Debugger_Text_Font,
                       Black (Get_System),
                       Null_Color,
                       Str (Start .. Str'Last));
               Start := Str'Last + 1;
            end if;
         end loop;
      end if;

      Thaw (Process.Debugger_Text);

      --  Note: we can not modify Process.Edit_Pos in this function, since
      --  otherwise the history (up and down keys in the command window) will
      --  not work properly.
   end Text_Output_Handler;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor;
      Str        : String;
      Window     : System.Address)
   is
      Process     : constant Debugger_Process_Tab :=
        Convert (To_Main_Debug_Window (Window), Descriptor);

      File_First  : Natural := 0;
      File_Last   : Positive;
      Line        : Natural := 0;
      First, Last : Natural;

   begin
      if Get_Parse_File_Name (Get_Process (Process.Debugger)) then
         Found_File_Name
           (Process.Debugger,
            Str, File_First, File_Last, First, Last, Line);
      end if;

      --  Do not show the output if we have an internal command

      if not Is_Internal_Command (Get_Process (Process.Debugger))  then
         if First = 0 then
            Text_Output_Handler (Process, Str);
         else
            Text_Output_Handler (Process, Str (Str'First .. First - 1));
            Text_Output_Handler (Process, Str (Last + 1 .. Str'Last));
         end if;
         Process.Edit_Pos := Get_Length (Process.Debugger_Text);
         Set_Point (Process.Debugger_Text, Process.Edit_Pos);
         Set_Position (Process.Debugger_Text, Gint (Process.Edit_Pos));
      end if;

      --  Do we have a file name ?

      if File_First /= 0 then

         --  Get everything in the buffer (since the following command
         --  needs to interact with the debugger, and we want to hide its
         --  output).

         Wait_Prompt (Process.Debugger);

         --  Override the language currently defined in the editor.
         --  Since the text file has been given by the debugger, the language
         --  to use is the one currently defined by the debugger.
         Set_Current_Language
           (Process.Editor_Text, Get_Language (Process.Debugger));

         --  Display the file

         Push_Internal_Command_Status (Get_Process (Process.Debugger), True);
         Load_File
           (Process.Editor_Text,
            Str (File_First .. File_Last));
         Update_Breakpoints (Process);

         Pop_Internal_Command_Status (Get_Process (Process.Debugger));
      end if;

      if Line /= 0 then
         Set_Line (Process.Editor_Text, Line);
      end if;
   end Text_Output_Handler;

   ----------------------
   -- Output_Available --
   ----------------------

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
   begin
      --  Get everything that is available (and transparently call the
      --  output filters set for Pid).
      --  Nothing should be done if we are already processing a command
      --  (ie somewhere we are blocked on a Wait call for this Debugger),
      --  since otherwise that Wait won't see the output and will lose some
      --  output. We don't have to do that anyway, since the other Wait will
      --  indirectly call the output filter.

      if not Command_In_Process (Get_Process (Debugger.Debugger)) then
         Empty_Buffer
           (Get_Process (Debugger.Debugger),
            At_Least_One => True);
      end if;
   end Output_Available;

   ---------------------------
   -- Debugger_Button_Press --
   ---------------------------

   function Debugger_Button_Press
     (Process : access Debugger_Process_Tab_Record'Class;
      Event    : Gdk.Event.Gdk_Event)
     return Boolean
   is
   begin
      if Get_Button (Event) = 3 then
         Popup (Debugger_Contextual_Menu (Process),
                Button            => Get_Button (Event),
                Activate_Time     => Get_Time (Event));
         Emit_Stop_By_Name (Process.Debugger_Text, "button_press_event");
         return True;
      end if;
      return False;
   end Debugger_Button_Press;

   ---------------------
   -- Create_Debugger --
   ---------------------

   function Create_Debugger
     (Window          : access
        Main_Debug_Window_Pkg.Main_Debug_Window_Record'Class;
      Kind            : Debugger_Type;
      Executable      : String;
      Params          : Argument_List;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "";
      Title           : String := "") return Debugger_Process_Tab
   is
      Process : Debugger_Process_Tab;
      Id      : Gint;
      Label   : Gtk_Label;

   begin
      Process := new Debugger_Process_Tab_Record;
      Initialize (Process);
      Initialize_Class_Record (Process, Signals, Class_Record);
      Process.Window := Window.all'Access;

      Widget_Callback.Connect
        (Gtk_Widget (Process), "process_stopped",
         Widget_Callback.To_Marshaller (On_Canvas_Process_Stopped'Access));
      Canvas_Handler.Connect
        (Process.Data_Canvas, "background_click",
         Canvas_Handler.To_Marshaller (On_Background_Click'Access));
      Widget_Callback.Connect
        (Gtk_Widget (Process), "process_stopped",
         Widget_Callback.To_Marshaller (Update_Breakpoints'Access));

      --  Set up the command window for the contextual menus

      Add_Events (Process.Debugger_Text, Button_Press_Mask);
      Canvas_Event_Handler.Object_Connect
        (Process.Debugger_Text, "button_press_event",
         Canvas_Event_Handler.To_Marshaller (Debugger_Button_Press'Access),
         Process);

      --  Allocate the colors for highlighting. This needs to be done before
      --  Initializing the debugger, since some file name might be output at
      --  that time.

      Process.Debugger_Text_Highlight_Color :=
        Parse (Debugger_Highlight_Color);

      Alloc (Get_System, Process.Debugger_Text_Highlight_Color);

      Process.Debugger_Text_Font :=
        Get_Gdkfont (Debugger_Font, Debugger_Font_Size);

      Align_On_Grid (Process.Data_Canvas, Align_Items_On_Grid);

      --  Spawn the debugger

      case Kind is
         when Gdb_Type =>
            Process.Debugger := new Gdb_Debugger;
         when Jdb_Type =>
            Process.Debugger := new Jdb_Debugger;
         when others =>
            raise Debugger_Not_Supported;
      end case;

      Spawn
        (Process.Debugger,
         Executable,
         Params,
         new Gui_Process_Proxy,
         Window.all'Access,
         Remote_Host,
         Remote_Target,
         Remote_Protocol,
         Debugger_Name);

      --  Add a new page to the notebook

      if Title = "" then
         if Params'Length > 0 then
            Gtk_New (Label, "Gdb - " & Params (Params'First).all);
         else
            Gtk_New (Label, "Gdb -" &
              Guint'Image (Page_List.Length (Get_Children
                (Window.Process_Notebook)) + 1));
         end if;
      else
         Gtk_New (Label, Title);
      end if;

      Append_Page (Window.Process_Notebook, Process.Process_Paned, Label);
      Show_All (Window.Process_Notebook);
      Set_Page (Window.Process_Notebook, -1);

      --  Initialize the code editor.
      --  This should be done before initializing the debugger, in case the
      --  debugger outputs a file name that should be displayed in the editor.
      --  The language of the editor will automatically be set by the output
      --  filter.

      Configure (Process.Editor_Text, Editor_Font, Editor_Font_Size,
                 dot_xpm, arrow_xpm, stop_xpm,
                 Comments_Color    => Comments_Color,
                 Strings_Color     => Strings_Color,
                 Keywords_Color    => Keywords_Color);
      Set_Show_Line_Nums (Process.Editor_Text, Editor_Show_Line_Nums);
      Set_Show_Lines_With_Code
        (Process.Editor_Text, Editor_Show_Line_With_Code);

      --  Set the user data, so that we can easily convert afterwards.

      Process_User_Data.Set
        (Process.Editor_Text, Process, Process_User_Data_Name);
      Process_User_Data.Set (Process.Process_Paned, Process.all'Access);

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window. This filter must be inserted after all the other filters,
      --  so that for instance the language detection takes place before we
      --  try to detect any reference to a file/line.

      Add_Output_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         Text_Output_Handler'Access, Window.all'Address,
         After => True);
      Id := My_Input.Add
        (To_Gint
         (Get_Output_Fd
          (Get_Descriptor (Get_Process (Process.Debugger)).all)),
         Gdk.Types.Input_Read,
         Output_Available'Access,
         My_Input.Data_Access (Process));

      --  Initialize the debugger, and possibly get the name of the initial
      --  file.
      Initialize (Process.Debugger);

      return Process;
   end Create_Debugger;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed
     (Debugger : access Debugger_Process_Tab_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (Gtk_Widget (Debugger), "context_changed");
      Widget_Callback.Emit_By_Name (Gtk_Widget (Debugger), "process_stopped");
   end Context_Changed;

   ---------------------
   -- Process_Stopped --
   ---------------------

   procedure Process_Stopped
     (Debugger : access Debugger_Process_Tab_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (Gtk_Widget (Debugger), "process_stopped");
   end Process_Stopped;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command (Debugger : Debugger_Process_Tab;
                                   Command  : String)
   is
      Item     : Display_Item;
      Command2 : String := To_Lower (Command);
      First    : Natural := Command2'First;
      Start    : Natural;
      Tmp      : Natural;
      Cursor   : Gdk_Cursor;
   begin
      Append (Debugger.Command_History, Command);

      Gdk_New (Cursor, Gdk.Types.Watch);
      Set_Cursor (Get_Window (Debugger.Window), Cursor);
      Destroy (Cursor);

      --  ??? Should forbid commands that modify the configuration of the
      --  debugger, like "set annotate" for gdb, otherwise we can't be sure
      --  what to expect from the debugger.

      --  Command has been converted to lower-cases, but the new version
      --  should be used only to compare with our standard list of commands.
      --  We should pass the original string to the debugger, in case we are
      --  in a case-sensitive language.

      --  Ignore the blanks at the beginning of lines

      Skip_Blanks (Command2, First);

      if Looking_At (Command2, First, "graph display")
        or else Looking_At (Command2, First, "graph print")
      then
         Start := First + 11;
         Skip_To_Blank (Command2, Start);
         Skip_Blanks (Command2, Start);

         if Command (Start) = '`' then
            Tmp := Start + 1;
            Skip_To_Char (Command, Tmp, '`');

            declare
               Entity : Items.Generic_Type_Access :=
                 New_Debugger_Type (Command (Start + 1 .. Tmp - 1));
            begin
               Set_Value
                 (Debugger_Output_Type (Entity.all),
                  Send (Debugger.Debugger,
                        Refresh_Command (Debugger_Output_Type (Entity.all))));
               Gtk_New
                 (Item, Get_Window (Debugger.Data_Canvas),
                  Variable_Name  => Command (Start + 1 .. Tmp - 1),
                  Debugger       => Debugger,
                  Auto_Refresh   => Command2 (First + 6) = 'd',
                  Default_Entity => Entity);
            end;

         elsif Enable_Block_Search then
            Gtk_New
              (Item, Get_Window (Debugger.Data_Canvas),
               Variable_Name => Variable_Name_With_Frame
               (Debugger.Debugger, Command (Start .. Command2'Last)),
               Debugger      => Debugger,
               Auto_Refresh  => Command2 (First + 6) = 'd');
         end if;

         --  If we could not get the variable with the block, try without,
         --  since some debuggers (gdb most notably) can have more efficient
         --  algorithms to find the variable.

         if Item = null then
            Gtk_New
              (Item, Get_Window (Debugger.Data_Canvas),
               Variable_Name => Command (Start .. Command2'Last),
               Debugger      => Debugger,
               Auto_Refresh  => Command2 (First + 6) = 'd');
         end if;

         if Item /= null then
            Put (Debugger.Data_Canvas, Item);
         end if;
         Display_Prompt (Debugger.Debugger);

      elsif Command2 = "quit" then
         Main_Quit;

      else
         --  Regular debugger command, send it.
         Send (Debugger.Debugger, Command,
               Wait_For_Prompt =>
                 not Command_In_Process (Get_Process (Debugger.Debugger)));
      end if;

      --  Put back the standard cursor

      Gdk_New (Cursor, Gdk.Types.Left_Ptr);
      Set_Cursor (Get_Window (Debugger.Window), Cursor);
      Destroy (Cursor);

      Unregister_Dialog (Debugger);

   end Process_User_Command;

   ---------------------
   -- Input_Available --
   ---------------------

   procedure Input_Available
     (Debugger  : Standard_Input_Package.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition)
   is
      Tab       : Debugger_Process_Tab;
      Buffer    : String (1 .. 8192);
      Len       : Natural;

   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Debugger.Process_Notebook)));
      Get_Line (Buffer, Len);
      Process_User_Command (Tab, Buffer (1 .. Len));
   end Input_Available;

   ---------------------
   -- Register_Dialog --
   ---------------------

   procedure Register_Dialog
     (Process : access Debugger_Process_Tab_Record;
      Dialog  : access Gtk.Dialog.Gtk_Dialog_Record'Class)
   is
   begin
      if Process.Registered_Dialog /= null then
         raise Program_Error;
      end if;
      Process.Registered_Dialog := Gtk_Dialog (Dialog);
   end Register_Dialog;

   -----------------------
   -- Unregister_Dialog --
   -----------------------

   procedure Unregister_Dialog
     (Process : access Debugger_Process_Tab_Record)
   is
   begin
      if Process.Registered_Dialog /= null then
         Destroy (Process.Registered_Dialog);
         Process.Registered_Dialog := null;
      end if;
   end Unregister_Dialog;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Process : Debugger_Process_Tab := Debugger_Process_Tab (Object);
   begin
      Free (Process.Breakpoints);
      Process.Breakpoints := new Breakpoint_Array'
        (List_Breakpoints (Process.Debugger));

      --  Update the breakpoints in the editor
      Update_Breakpoints (Process.Editor_Text, Process.Breakpoints.all);

      --  Update the breakpoints dialog if necessary
      if Process.Window.Breakpoints_Editor /= null
        and then Mapped_Is_Set (Process.Window.Breakpoints_Editor)
      then
         Update_Breakpoint_List
           (Breakpoints_Access (Process.Window.Breakpoints_Editor));
      end if;
   end Update_Breakpoints;

   -----------------------------
   -- Toggle_Breakpoint_State --
   -----------------------------

   function Toggle_Breakpoint_State
     (Process        : access Debugger_Process_Tab_Record;
      Breakpoint_Num : Integer)
     return Boolean
   is
   begin
      --  ??? Maybe we should also update the icons in the code_editor to have
      --  an icon of a different color ?
      if Process.Breakpoints /= null then
         for J in Process.Breakpoints'Range loop
            if Process.Breakpoints (J).Num = Breakpoint_Num then
               Process.Breakpoints (J).Enabled :=
                 not Process.Breakpoints (J).Enabled;
               Enable_Breakpoint
                 (Process.Debugger, Breakpoint_Num,
                  Process.Breakpoints (J).Enabled,
                  Display => True);
               return Process.Breakpoints (J).Enabled;
            end if;
         end loop;
      end if;
      return False;
   end Toggle_Breakpoint_State;

end Odd.Process;
