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

with Glib; use Glib;

with Gdk.Input;
with Gdk.Types;
with Gdk.Color;    use Gdk.Color;
with Gdk.Types;    use Gdk.Types;
with Gdk.Event;    use Gdk.Event;

with Gtk;          use Gtk;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Text;     use Gtk.Text;
with Gtk.Menu;     use Gtk.Menu;
with Gtk.Widget;   use Gtk.Widget;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Label;    use Gtk.Label;
with Gtk.Object;   use Gtk.Object;
with Gtk.Dialog;   use Gtk.Dialog;
with Gtk.Window;   use Gtk.Window;

with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;

with Gtkada.Types;    use Gtkada.Types;
with Gtkada.Handlers; use Gtkada.Handlers;

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Text_IO;              use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

with Process_Tab_Pkg;           use Process_Tab_Pkg;
with Display_Items;             use Display_Items;
with Debugger.Gdb;              use Debugger.Gdb;
with Debugger.Jdb;              use Debugger.Jdb;
with Process_Proxies;           use Process_Proxies;
with Items.Simples;             use Items.Simples;
with Main_Debug_Window_Pkg;     use Main_Debug_Window_Pkg;
with Breakpoints_Pkg;           use Breakpoints_Pkg;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;
with Odd.Canvas;                use Odd.Canvas;
with Odd.Dialogs;               use Odd.Dialogs;
with Odd.Pixmaps;               use Odd.Pixmaps;
with Odd.Strings;               use Odd.Strings;
with Odd.Types;                 use Odd.Types;
with Odd.Code_Editors;          use Odd.Code_Editors;
with Odd.Menus;                 use Odd.Menus;
with Odd.Preferences;           use Odd.Preferences;
with Odd.Utils;                 use Odd.Utils;

with System;
with Unchecked_Conversion;

pragma Warnings (Off, Debugger.Jdb);

package body Odd.Process is

   Enable_Block_Search    : constant Boolean := False;
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
   Signals : Chars_Ptr_Array :=
     "process_stopped" + "context_changed";

   Graph_Cmd_Format : constant Pattern_Matcher := Compile
     ("graph\s+(print|display)\s+(`([^`]+)`|""([^""]+)"")?(.*)",
      Case_Insensitive);
   --  Format of the graph print commands, and how to parse them

   Graph_Cmd_Type_Paren          : constant := 1;
   Graph_Cmd_Expression_Paren    : constant := 3;
   Graph_Cmd_Quoted_Paren        : constant := 4;
   Graph_Cmd_Rest_Paren          : constant := 5;
   --  Indexes of the parentheses pairs in Graph_Cmd_Format for each of the
   --  relevant fields.

   Graph_Cmd_Dependent_Format : constant Pattern_Matcher := Compile
     ("dependent\s+on\s+(\d+)\s*", Case_Insensitive);
   --  Partial analyses of the last part of a graph command

   Graph_Cmd_Link_Format : constant Pattern_Matcher := Compile
     ("link_name\s+(.+)", Case_Insensitive);
   --  Partial analyses of the last part of a graph command

   Graph_Cmd_Format2 : constant Pattern_Matcher := Compile
     ("graph\s+(enable|disable)\s+display\s+(.*)", Case_Insensitive);
   --  Second possible set of commands.

   Graph_Cmd_Format3 : constant Pattern_Matcher := Compile
     ("graph\s+undisplay\s+(.*)", Case_Insensitive);
   --  Third possible set of commands

   --------------------
   -- Post processes --
   --------------------
   --  For reliability reasons, it is not recommended to directly load
   --  a file in Text_Output_Handler as soon as a file indication is found
   --  in the output of the debugger.
   --  Instead, we register a "post command", to be executed when the current
   --  call to Wait or Wait_Prompt is finished.
   --  There are a number of such functions, defined below, and that all use
   --  the same type of User_Data.

   type Load_File_Data is record
      Process   : Debugger_Process_Tab;
      File_Name : Odd.Types.String_Access;
      Line      : Natural;
      Addr      : Odd.Types.String_Access;
   end record;
   type Load_File_Data_Access is access Load_File_Data;
   function Convert is new Unchecked_Conversion
     (System.Address, Load_File_Data_Access);
   function Convert is new Unchecked_Conversion
     (Load_File_Data_Access, System.Address);

   procedure Load_File_Post_Process (User_Data : System.Address);
   --  Load a file, whose name was found while we were previously waiting for
   --  a prompt.

   procedure Set_Line_Post_Process (User_Data : System.Address);
   --  Set a line, whose name was found while we were previously waiting for
   --  a prompt.

   procedure Set_Addr_Post_Process (User_Data : System.Address);
   --  Set an address, whose name was found while we were previously waiting
   --  for a prompt.

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
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Standard handler to add gdb's output to the debugger window.

   function Debugger_Button_Press
     (Process : access Debugger_Process_Tab_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback for all the button press events in the debugger command window.
   --  This is used to display the contexual menu.

   procedure Process_Graph_Cmd
     (Process : access Debugger_Process_Tab_Record'Class;
      Cmd     : String);
   --  Parse and process a "graph print" or "graph display" command

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access Main_Debug_Window_Record'Class;
      Descriptor : GNAT.Expect.Process_Descriptor'Class)
     return Debugger_Process_Tab
   is
      Page      : Gtk_Widget;
      Num_Pages : constant Gint :=
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

   exception
      when Constraint_Error =>
         raise Debugger_Not_Found;
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Text : access Odd.Code_Editors.Code_Editor_Record'Class)
      return Debugger_Process_Tab is
   begin
      return Process_User_Data.Get (Text, Process_User_Data_Name);
   end Convert;

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access Gtk.Window.Gtk_Window_Record'Class;
      Debugger : access Debugger_Root'Class) return Debugger_Process_Tab is
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
      Is_Command : Boolean := False;
      Set_Position : Boolean := False)
   is
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Str2    : String := Strip_Control_M (Str);
      Start   : Positive := Str2'First;

   begin
      Freeze (Process.Debugger_Text);
      Set_Point (Process.Debugger_Text, Get_Length (Process.Debugger_Text));

      --  Should all the string be highlighted ?

      if Is_Command then
         Insert
           (Process.Debugger_Text,
            Process.Debugger_Text_Font,
            Process.Debugger_Text_Highlight_Color,
            Null_Color,
            Str2);

      --  If not, highlight only parts of it

      else
         while Start <= Str2'Last loop
            Match (Highlighting_Pattern (Process.Debugger),
                   Str2 (Start .. Str2'Last),
                   Matched);

            if Matched (0) /= No_Match then
               if Matched (0).First - 1 >= Start then
                  Insert (Process.Debugger_Text,
                          Process.Debugger_Text_Font,
                          Black (Get_System),
                          Null_Color,
                          Str2 (Start .. Matched (0).First - 1));
               end if;

               Insert (Process.Debugger_Text,
                       Process.Debugger_Text_Font,
                       Process.Debugger_Text_Highlight_Color,
                       Null_Color,
                       Str2 (Matched (0).First .. Matched (0).Last));
               Start := Matched (0).Last + 1;

            else
               Insert (Process.Debugger_Text,
                       Process.Debugger_Text_Font,
                       Black (Get_System),
                       Null_Color,
                       Str2 (Start .. Str2'Last));
               Start := Str2'Last + 1;
            end if;
         end loop;
      end if;

      Thaw (Process.Debugger_Text);

      --  Note: we can not systematically modify Process.Edit_Pos in this
      --  function, since otherwise the history (up and down keys in the
      --  command window) will not work properly.

      if Set_Position then
         Process.Edit_Pos := Get_Length (Process.Debugger_Text);
         Set_Point (Process.Debugger_Text, Process.Edit_Pos);
         Gtk.Text.Set_Position
           (Process.Debugger_Text, Gint (Process.Edit_Pos));
      end if;
   end Text_Output_Handler;

   ----------------------------
   -- Load_File_Post_Process --
   ----------------------------

   procedure Load_File_Post_Process (User_Data : System.Address) is
      Data : Load_File_Data_Access := Convert (User_Data);
   begin
      --  Override the language currently defined in the editor.
      --  Since the text file has been given by the debugger, the language
      --  to use is the one currently defined by the debugger.

      Set_Current_Language
        (Data.Process.Editor_Text, Get_Language (Data.Process.Debugger));

      --  Display the file

      --  Update the list of breakpoints in the editor.
      --  If there is no breakpoint defined, we force an update (in fact,
      --  "start" will always define such a breakpoint, and this is used to
      --  initialize the list).
      if Data.Process.Breakpoints /= null
        and then Data.Process.Breakpoints'Length > 0
      then
         Update_Breakpoints
           (Data.Process.Editor_Text, Data.Process.Breakpoints.all);
      else
         Update_Breakpoints (Data.Process, Force => True);
      end if;

      Load_File (Data.Process.Editor_Text, Data.File_Name.all);

      --  Free unused memory
      Free (Data.File_Name);
   end Load_File_Post_Process;

   ---------------------------
   -- Set_Line_Post_Process --
   ---------------------------

   procedure Set_Line_Post_Process (User_Data : System.Address) is
      Data : Load_File_Data_Access := Convert (User_Data);
   begin
      Set_Line (Data.Process.Editor_Text, Data.Line);
   end Set_Line_Post_Process;

   ---------------------------
   -- Set_Addr_Post_Process --
   ---------------------------

   procedure Set_Addr_Post_Process (User_Data : System.Address) is
      Data : Load_File_Data_Access := Convert (User_Data);
   begin
      Set_Address (Data.Process.Editor_Text, Data.Addr.all);
      Free (Data.Addr);
   end Set_Addr_Post_Process;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   procedure Text_Output_Handler
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address)
   is
      Process     : constant Debugger_Process_Tab :=
        Convert (To_Main_Debug_Window (Window), Descriptor);

      File_First  : Natural := 0;
      File_Last   : Positive;
      Line        : Natural := 0;
      First, Last : Natural;
      Addr_First  : Natural := 0;
      Addr_Last   : Natural;

   begin
      if Get_Parse_File_Name (Get_Process (Process.Debugger)) then
         Found_File_Name
           (Process.Debugger,
            Str, File_First, File_Last, First, Last, Line,
            Addr_First, Addr_Last);
      end if;

      --  Do not show the output if we have an internal or hidden command

      case Get_Command_Mode (Get_Process (Process.Debugger)) is
         when User | Odd.Types.Visible =>
            if First = 0 then
               Text_Output_Handler (Process, Str, Set_Position => True);
            else
               Text_Output_Handler (Process, Str (Str'First .. First - 1));
               Text_Output_Handler
                 (Process, Str (Last + 1 .. Str'Last), Set_Position => True);
            end if;

         when Hidden | Internal =>
            null;
      end case;

      --  Do we have a file name or line number indication: if yes, do not
      --  process them immediatly, but wait for the current command to be
      --  full processed (since Text_Output_Handler is called while a
      --  call to Wait or Wait_Prompt is being processed).
      --  The memory allocated for Load_File_Data is freed in
      --  process_proxies.adb:Process_Post_Processes. ??? Check this comment
      --  The memory allocated for the string is freed in
      --  Load_File_Post_Process.

      if File_First /= 0 then
         Register_Post_Cmd
           (Get_Process (Process.Debugger),
            Load_File_Post_Process'Access,
            Convert (new Load_File_Data'
                     (Process => Process,
                      File_Name => new String'(Str (File_First .. File_Last)),
                      Line      => 1,
                      Addr      => null)));
      end if;

      if Line /= 0 then
         Register_Post_Cmd
           (Get_Process (Process.Debugger),
            Set_Line_Post_Process'Access,
            Convert (new Load_File_Data'
                     (Process   => Process,
                      File_Name => null,
                      Line      => Line,
                      Addr      => null)));
      end if;

      if Addr_First /= 0 then
         Register_Post_Cmd
           (Get_Process (Process.Debugger),
            Set_Addr_Post_Process'Access,
            Convert (new Load_File_Data'
                     (Process   => Process,
                      File_Name => null,
                      Line      => 1,
                      Addr    => new String'(Str (Addr_First .. Addr_Last)))));
      end if;
   end Text_Output_Handler;

   ----------------------
   -- Output_Available --
   ----------------------

   procedure Output_Available
     (Debugger  : My_Input.Data_Access;
      Source    : Gint;
      Condition : Gdk.Types.Gdk_Input_Condition) is
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
      Event    : Gdk.Event.Gdk_Event) return Boolean is
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
      Debugger_Name   : String := "") return Debugger_Process_Tab
   is
      Process       : Debugger_Process_Tab;
      --  Id            : Gint;
      Label         : Gtk_Label;
      Debugger_List : Debugger_List_Link;
      Debugger_Num  : Natural := 1;

   begin

      Process := new Debugger_Process_Tab_Record;
      Initialize (Process);
      Initialize_Class_Record (Process, Signals, Class_Record);

      Process.Descriptor.Debugger := Kind;
      Process.Descriptor.Remote_Host := new String' (Remote_Host);
      Process.Descriptor.Remote_Target := new String' (Remote_Target);
      Process.Descriptor.Protocol := new String' (Remote_Protocol);
      Process.Descriptor.Program := new String' (Executable);
      Process.Descriptor.Debugger_Name := new String' (Debugger_Name);

      Process.Window := Window.all'Access;
      Set_Process (Process.Data_Canvas, Process);

      Widget_Callback.Object_Connect
        (Process,
         "executable_changed",
         Widget_Callback.To_Marshaller
           (Odd.Code_Editors.On_Executable_Changed'Access),
         Process.Editor_Text);

      Widget_Callback.Connect
        (Gtk_Widget (Process), "process_stopped",
         Widget_Callback.To_Marshaller (On_Canvas_Process_Stopped'Access));
      Widget_Callback.Connect
        (Gtk_Widget (Process), "context_changed",
         Widget_Callback.To_Marshaller (On_Canvas_Process_Stopped'Access));
      Widget_Callback.Connect
        (Gtk_Widget (Process), "process_stopped",
         Widget_Callback.To_Marshaller (On_Backtrace_Process_Stopped'Access));
      Widget_Callback.Connect
        (Gtk_Widget (Process), "context_changed",
         Widget_Callback.To_Marshaller (On_Backtrace_Process_Stopped'Access));
      Widget_Callback.Connect
        (Gtk_Widget (Process), "process_stopped",
         Widget_Callback.To_Marshaller (On_Task_Process_Stopped'Access));
      Canvas_Handler.Connect
        (Process.Data_Canvas, "background_click",
         Canvas_Handler.To_Marshaller (On_Background_Click'Access));

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

      Gtk_New (Label, "");
      Append_Page (Window.Process_Notebook, Process.Process_Paned, Label);
      Show_All (Window.Process_Notebook);
      Set_Page (Window.Process_Notebook, -1);

      if Page_List.Length (Get_Children (Window.Process_Notebook)) > 0 then
         Set_Sensitive (Gtk_Widget (Window.Open_Program1), True);
      end if;

      --  Initialize the code editor.
      --  This should be done before initializing the debugger, in case the
      --  debugger outputs a file name that should be displayed in the editor.
      --  The language of the editor will automatically be set by the output
      --  filter.

      Configure
        (Process.Editor_Text,
         Editor_Font, Editor_Font_Size,
         dot_xpm, arrow_xpm, stop_xpm,
         Comments_Color    => Comments_Color,
         Strings_Color     => Strings_Color,
         Keywords_Color    => Keywords_Color);

      --  Initialize the canvas

      if not Display_Grid then
         Configure (Process.Data_Canvas, Grid_Size => 0);
      end if;

      --  Set the user data, so that we can easily convert afterwards.

      Process_User_Data.Set
        (Process.Editor_Text, Process, Process_User_Data_Name);
      Process_User_Data.Set (Process.Process_Paned, Process.all'Access);

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window. This filter must be inserted after all the other filters,
      --  so that for instance the language detection takes place before we
      --  try to detect any reference to a file/line.

      Add_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         Text_Output_Handler'Access, Output, Window.all'Address,
         After => True);

      --  This filter is only required in some rare cases, since most of the
      --  time we do some polling.
--        Id := My_Input.Add
--          (To_Gint
--           (Get_Output_Fd
--            (Get_Descriptor (Get_Process (Process.Debugger)).all)),
--           Gdk.Types.Input_Read,
--           Output_Available'Access,
--           My_Input.Data_Access (Process));

      if Window.First_Debugger = null then
         Process.Debugger_Num := Debugger_Num;
         Window.First_Debugger := new Debugger_List_Node'
           (Next     => null,
            Debugger => Gtk_Widget (Process));
      else
         Debugger_Num := Debugger_Num + 1;
         Debugger_List := Window.First_Debugger;

         while Debugger_List.Next /= null loop
            Debugger_Num := Debugger_Num + 1;
            Debugger_List := Debugger_List.Next;
         end loop;

         Process.Debugger_Num := Debugger_Num;
         Debugger_List.Next := new Debugger_List_Node'
           (Next     => null,
            Debugger => Gtk_Widget (Process));
      end if;

      --  Initialize the debugger, and possibly get the name of the initial
      --  file.

      Initialize (Process.Debugger);

      --  Display the initial prompt

      Display_Prompt (Process.Debugger);

      return Process;
   end Create_Debugger;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed
     (Debugger : access Debugger_Process_Tab_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (Gtk_Widget (Debugger), "context_changed");
   end Context_Changed;

   ------------------------
   -- Executable_Changed --
   ------------------------

   procedure Executable_Changed
     (Debugger : access Debugger_Process_Tab_Record'Class;
      Executable_Name : String)
   is
      Debug : constant String :=
        Debugger_Type'Image (Debugger.Descriptor.Debugger);
      Label : Gtk_Widget;
   begin

      --  Changes the title of the tab for that debugger

      Label :=  Get_Tab_Label
        (Debugger.Window.Process_Notebook, Debugger.Process_Paned);
      Set_Text (Gtk_Label (Label),
                Debug (1 .. Debug'Last - 5) & " - "
                & Base_File_Name (Executable_Name));

      --  Emit the signal

      Widget_Callback.Emit_By_Name
        (Gtk_Widget (Debugger), "executable_changed");
   end Executable_Changed;

   ---------------------
   -- Process_Stopped --
   ---------------------

   procedure Process_Stopped
     (Debugger : access Debugger_Process_Tab_Record'Class) is
   begin
      Widget_Callback.Emit_By_Name (Gtk_Widget (Debugger), "process_stopped");
   end Process_Stopped;

   -----------------------
   -- Process_Graph_Cmd --
   -----------------------

   procedure Process_Graph_Cmd
     (Process : access Debugger_Process_Tab_Record'Class;
      Cmd     : String)
   is
      Matched  : Match_Array (0 .. 10);
      Matched2 : Match_Array (0 .. 10);
      Item    : Display_Item;
      Index,
      Last    : Positive;
      Enable  : Boolean;
      First   : Natural;
      Dependent_On_First : Natural := Natural'Last;
      Link_Name_First    : Natural := Natural'Last;
      Link_Name : Odd.Types.String_Access;
      Link_From : Display_Item;

   begin
      --  graph (print|display) expression [dependent on display_num]
      --        [link_name name]
      --  graph (print|display) `command`
      --  graph enable display display_num [display_num ...]
      --  graph disable display display_num [display_num ...]
      --  graph undisplay display_num

      Match (Graph_Cmd_Format, Cmd, Matched);

      if Matched (0) /= No_Match then
         Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd'
           or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'D';

         --  Do we have any 'dependent on' expression ?

         if Matched (Graph_Cmd_Rest_Paren).First >= Cmd'First then
            Match (Graph_Cmd_Dependent_Format,
                   Cmd (Matched (Graph_Cmd_Rest_Paren).First
                        .. Matched (Graph_Cmd_Rest_Paren).Last),
                   Matched2);

            if Matched2 (1) /= No_Match then
               Dependent_On_First := Matched2 (0).First;
               Link_From := Find_Item
                 (Process.Data_Canvas,
                  Integer'Value
                  (Cmd (Matched2 (1).First .. Matched2 (1).Last)));
            end if;
         end if;

         --  Do we have any 'link name' expression ?

         if Matched (Graph_Cmd_Rest_Paren).First >= Cmd'First then
            Match (Graph_Cmd_Link_Format,
                   Cmd (Matched (Graph_Cmd_Rest_Paren).First
                        .. Matched (Graph_Cmd_Rest_Paren).Last),
                   Matched2);

            if Matched2 (0) /= No_Match then
               Link_Name_First := Matched2 (0).First;
               Link_Name := new String'
                 (Cmd (Matched2 (1).First .. Matched2 (1).Last));
            end if;
         end if;

         --  A general expression (graph print `cmd`)
         if Matched (Graph_Cmd_Expression_Paren) /= No_Match then
            declare
               Expr : constant String := Cmd
                 (Matched (Graph_Cmd_Expression_Paren).First ..
                  Matched (Graph_Cmd_Expression_Paren).Last);
               Entity : Items.Generic_Type_Access := New_Debugger_Type (Expr);

            begin
               Set_Value
                 (Debugger_Output_Type (Entity.all),
                  Send (Process.Debugger,
                        Refresh_Command (Debugger_Output_Type (Entity.all)),
                        Mode => Internal));

               --  No link ?

               if Dependent_On_First = Natural'Last then
                  Gtk_New
                    (Item, Get_Window (Process.Data_Canvas),
                     Variable_Name  => Expr,
                     Debugger       => Process,
                     Auto_Refresh   => Enable,
                     Default_Entity => Entity);
                  Put (Process.Data_Canvas, Item);

               else
                  Gtk_New_And_Put
                    (Item, Get_Window (Process.Data_Canvas),
                     Variable_Name  => Expr,
                     Debugger       => Process,
                     Auto_Refresh   => Enable,
                     Link_From      => Link_From,
                     Link_Name      => Link_Name.all,
                     Default_Entity => Entity);
               end if;
            end;

         --  A quoted name or standard name

         else
            --  Quoted

            if Matched (Graph_Cmd_Quoted_Paren) /= No_Match then
               First := Matched (Graph_Cmd_Quoted_Paren).First;
               Last  := Matched (Graph_Cmd_Quoted_Paren).Last;

            --  Standard

            else
               First := Matched (Graph_Cmd_Rest_Paren).First;
               Last  := Natural'Min (Link_Name_First, Dependent_On_First);
               if Last = Natural'Last then
                  Last  := Matched (Graph_Cmd_Rest_Paren).Last;
               else
                  Last := Last - 1;
               end if;
            end if;

            --  If we don't want any link:

            if Dependent_On_First = Natural'Last then
               if Enable_Block_Search then
                  Gtk_New
                    (Item, Get_Window (Process.Data_Canvas),
                     Variable_Name => Variable_Name_With_Frame
                     (Process.Debugger, Cmd (First .. Last)),
                     Debugger      => Process,
                     Auto_Refresh  =>
                       Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd');
               end if;

               --  If we could not get the variable with the block, try
               --  without, since some debuggers (gdb most notably) can have
               --  more efficient algorithms to find the variable.

               if Item = null then
                  Gtk_New
                    (Item, Get_Window (Process.Data_Canvas),
                     Variable_Name => Cmd (First .. Last),
                     Debugger      => Process,
                     Auto_Refresh  =>
                       Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd');
               end if;

               if Item /= null then
                  Put (Process.Data_Canvas, Item);
                  Recompute_All_Aliases
                    (Process.Data_Canvas, Recompute_Values => False);
               end if;

            --  Else if we have a link

            else
               if Link_Name = null then
                  Link_Name := new String' (Cmd (First .. Last));
               end if;

               if Enable_Block_Search then
                  Gtk_New_And_Put
                    (Item, Get_Window (Process.Data_Canvas),
                     Variable_Name => Variable_Name_With_Frame
                     (Process.Debugger, Cmd (First .. Last)),
                     Debugger      => Process,
                     Auto_Refresh  => Enable,
                     Link_From     => Link_From,
                     Link_Name     => Link_Name.all);
               end if;

               if Item = null then
                  Gtk_New_And_Put
                    (Item, Get_Window (Process.Data_Canvas),
                     Variable_Name => Cmd (First .. Last),
                     Debugger      => Process,
                     Auto_Refresh  => Enable,
                     Link_From     => Link_From,
                     Link_Name     => Link_Name.all);
               end if;
            end if;
         end if;

         Free (Link_Name);

      else
         --  Is this an enable/disable command ?

         Match (Graph_Cmd_Format2, Cmd, Matched);

         if Matched (2) /= No_Match then
            Index := Matched (2).First;
            Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'e'
              or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'E';

            while Index <= Cmd'Last loop
               Last := Index;
               Skip_To_Blank (Cmd, Last);
               Set_Auto_Refresh
                 (Find_Item (Process.Data_Canvas,
                             Integer'Value (Cmd (Index .. Last - 1))),
                  Get_Window (Process),
                  Enable,
                  Update_Value => True);
               Index := Last + 1;
               Skip_Blanks (Cmd, Index);
            end loop;

         --  Third possible set of commands

         else
            Match (Graph_Cmd_Format3, Cmd, Matched);
            if Matched (1) /= No_Match then
               Index := Matched (1).First;
               while Index <= Cmd'Last loop
                  Last := Index;
                  Skip_To_Blank (Cmd, Last);
                  Free
                    (Find_Item (Process.Data_Canvas,
                                Integer'Value (Cmd (Index .. Last - 1))));
                  Index := Last + 1;
                  Skip_Blanks (Cmd, Index);
               end loop;
            end if;
         end if;
      end if;
   end Process_Graph_Cmd;

   --------------------
   -- Close_Debugger --
   --------------------

   procedure Close_Debugger (Debugger : Debugger_Process_Tab) is
      Top      : constant Main_Debug_Window_Access := Debugger.Window;
      Notebook : constant Gtk_Notebook := Debugger.Window.Process_Notebook;
      use String_History;

   begin
      --  Remove the notebook page.

      Close (Debugger.Debugger);
      Remove_Page (Notebook, Page_Num (Notebook, Debugger.Process_Paned));

      --  If the last notebook page was destroyed, disable "Open Program"
      --  in the menu.

      if Page_List.Length (Get_Children (Top.Process_Notebook)) = 0 then
         Set_Sensitive (Gtk_Widget (Top.Open_Program1), False);
      end if;
   end Close_Debugger;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Debugger       : Debugger_Process_Tab;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Command_Type := Odd.Types.Visible)
   is
      Command2 : constant String := To_Lower (Command);
      First    : Natural := Command2'First;
      Data     : History_Data;
      use String_History;
   begin

      if Output_Command then
         Text_Output_Handler
           (Debugger, Command & ASCII.LF, Is_Command => True);
      end if;

      Set_Busy_Cursor (Debugger, True);

      --  ??? Should forbid commands that modify the configuration of the
      --  debugger, like "set annotate" for gdb, otherwise we can't be sure
      --  what to expect from the debugger.

      --  Command has been converted to lower-cases, but the new version
      --  should be used only to compare with our standard list of commands.
      --  We should pass the original string to the debugger, in case we are
      --  in a case-sensitive language.

      --  Ignore the blanks at the beginning of lines

      Skip_Blanks (Command2, First);

      if Looking_At (Command2, First, "graph") then
         Data.Mode := Mode;
         Data.Debugger_Num := Integer (Get_Num (Debugger));
         Skip_Blanks (Command, First);
         Data.Command := new String'(Command);
         Append (Debugger.Window.Command_History, Data);
         Process_Graph_Cmd (Debugger, Command);
         Display_Prompt (Debugger.Debugger);

      elsif Command2 = "quit" then
         Close_Debugger (Debugger);
      else

         --  Regular debugger command, send it.
         Send
           (Debugger.Debugger, Command,
            Wait_For_Prompt =>
              not Command_In_Process (Get_Process (Debugger.Debugger)),
            Mode => Mode);
      end if;

      --  Put back the standard cursor

      Set_Busy_Cursor (Debugger, False);
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
      use String_History;
   begin
      Tab := Process_User_Data.Get
        (Get_Child (Get_Cur_Page (Debugger.Process_Notebook)));

      --  If we are already processing a command, just wait until
      --  the debugger is available
      if not Command_In_Process (Get_Process (Tab.Debugger)) then
         Get_Line (Buffer, Len);
         if Len = 0 then
            Find_Match (Tab.Window.Command_History,
                        Natural (Get_Num (Tab)),
                        Backward);
            Process_User_Command (Tab, Get_Current
                                  (Tab.Window.Command_History).Command.all,
                                  Output_Command => True,
                                  Mode => User);
         else
            Process_User_Command (Tab, Buffer (1 .. Len));
         end if;
      end if;

   exception
      --  The history was empty, so there is no command to execute...
      when No_Such_Item =>
         null;
   end Input_Available;

   ---------------------
   -- Register_Dialog --
   ---------------------

   procedure Register_Dialog
     (Process : access Debugger_Process_Tab_Record;
      Dialog  : access Gtk.Dialog.Gtk_Dialog_Record'Class) is
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
     (Process : access Debugger_Process_Tab_Record) is
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
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Force  : Boolean)
   is
      Process : constant Debugger_Process_Tab := Debugger_Process_Tab (Object);
   begin
      --  We only need to update the list of breakpoints when we have a
      --  temporary breakpoint (since its status might be changed upon
      --  reaching the line).

      if Force or else Process.Has_Temporary_Breakpoint then

         Free (Process.Breakpoints);
         Process.Breakpoints := new Breakpoint_Array'
           (List_Breakpoints (Process.Debugger));

         --  Check whether there is any temporary breakpoint
         Process.Has_Temporary_Breakpoint := False;
         for J in Process.Breakpoints'Range loop
            if Process.Breakpoints (J).Disposition /= Keep
              and then Process.Breakpoints (J).Enabled
            then
               Process.Has_Temporary_Breakpoint := True;
               exit;
            end if;
         end loop;

         --  Update the breakpoints in the editor
         Update_Breakpoints (Process.Editor_Text, Process.Breakpoints.all);

         --  Update the breakpoints dialog if necessary
         if Process.Window.Breakpoints_Editor /= null
           and then Mapped_Is_Set (Process.Window.Breakpoints_Editor)
         then
            Update_Breakpoint_List
           (Breakpoints_Access (Process.Window.Breakpoints_Editor));
         end if;
      end if;
   end Update_Breakpoints;

   -----------------------------
   -- Toggle_Breakpoint_State --
   -----------------------------

   function Toggle_Breakpoint_State
     (Process        : access Debugger_Process_Tab_Record;
      Breakpoint_Num : Integer) return Boolean is
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
                  Mode => Odd.Types.Visible);
               return Process.Breakpoints (J).Enabled;
            end if;
         end loop;
      end if;

      return False;
   end Toggle_Breakpoint_State;

   -------------------------
   -- Get_Current_Process --
   -------------------------

   function Get_Current_Process
     (Main_Window : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Debugger_Process_Tab
   is
      Page : Gtk.Gtk_Notebook_Page := Get_Cur_Page
        (Main_Debug_Window_Access (Main_Window).Process_Notebook);
   begin
      if Page = null then
         return null;
      else
         return Process_User_Data.Get (Get_Child (Page));
      end if;
   end Get_Current_Process;

   ---------------------
   -- Set_Busy_Cursor --
   ---------------------

   procedure Set_Busy_Cursor
     (Debugger : access Debugger_Process_Tab_Record'Class;
      Busy     : Boolean := True) is
   begin
      Set_Busy_Cursor (Get_Window (Debugger.Window), Busy);
   end Set_Busy_Cursor;

   -------------
   -- Get_Num --
   -------------

   function Get_Num (Tab : Debugger_Process_Tab) return Gint is
   begin
      return Gint (Tab.Debugger_Num);
   end Get_Num;

end Odd.Process;
