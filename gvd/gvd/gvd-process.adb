-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
pragma Warnings (Off);
with GNAT.TTY;                   use GNAT.TTY;
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with System;                     use System;

with Gdk.Color;                  use Gdk.Color;
with Gdk.Event;                  use Gdk.Event;

with Glib.Object;                use Glib.Object;
with Glib; use Glib;

with Gtk.Arguments;              use Gtk.Arguments;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Object;                 use Gtk.Object;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtk;                        use Gtk;

with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Types;               use Gtkada.Types;

with Pango.Enums;                use Pango.Enums;
with Pango.Font;                 use Pango.Font;

with Basic_Types;                use Basic_Types;
with Breakpoints_Editor;         use Breakpoints_Editor;
with Config;                     use Config;
with Debugger.Gdb;               use Debugger.Gdb;
with Display_Items;              use Display_Items;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel;
with GPS.Main_Window.Debug;      use GPS.Main_Window.Debug;
with GPS.Main_Window;            use GPS.Main_Window;
with GUI_Utils;                  use GUI_Utils;
with GVD.Canvas;                 use GVD.Canvas;
with GVD.Code_Editors;           use GVD.Code_Editors;
with GVD.Dialogs;                use GVD.Dialogs;
with GVD.Preferences;            use GVD.Preferences;
with GVD.Source_Editor;          use GVD.Source_Editor;
with GVD.Trace;                  use GVD.Trace;
with GVD.Types;                  use GVD.Types;
with GVD_Module;                 use GVD_Module;
with Histories;                  use Histories;
with Items.Simples;              use Items.Simples;
with Pixmaps_IDE;                use Pixmaps_IDE;
with String_List_Utils;          use String_List_Utils;
with String_Utils;               use String_Utils;
with VFS;                        use VFS;

package body GVD.Process is

   package TTY_Timeout is new Gtk.Main.Timeout (Visual_Debugger);

   function TTY_Cb (Data : Visual_Debugger) return Boolean;
   --  Callback for communication with a tty.

   package Canvas_Event_Handler is new Gtk.Handlers.Return_Callback
     (Visual_Debugger_Record, Boolean);

   pragma Warnings (Off);
   --  This UC is safe aliasing-wise, so kill warning
   function To_Main_Debug_Window is new
     Ada.Unchecked_Conversion (System.Address, GPS_Window);
   pragma Warnings (On);

   --  This pointer will keep a pointer to the C 'class record' for
   --  gtk. To avoid allocating memory for each widget, this may be done
   --  only once, and reused
   Class_Record : Gtk.Object.GObject_Class := Gtk.Object.Uninitialized_Class;

   --  Array of the signals created for this widget
   Signals : constant Chars_Ptr_Array :=
     "executable_changed" + "process_stopped" + "context_changed" +
     "debugger_closed";

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
     ("\s+dependent\s+on\s+(\d+)\s*", Case_Insensitive);
   --  Partial analyses of the last part of a graph command

   Graph_Cmd_Link_Format : constant Pattern_Matcher := Compile
     ("\s+link_name\s+(.+)", Case_Insensitive);
   --  Partial analyses of the last part of a graph command

   Graph_Cmd_Format2 : constant Pattern_Matcher := Compile
     ("graph\s+(enable|disable)\s+display\s+(.*)", Case_Insensitive);
   --  Second possible set of commands.

   Graph_Cmd_Format3 : constant Pattern_Matcher := Compile
     ("graph\s+undisplay\s+(.*)", Case_Insensitive);
   --  Third possible set of commands

   Regexp_Any : constant Pattern_Matcher :=
     Compile (".+", Single_Line or Multiple_Lines);
   --  Any non empty string, as long as possible.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Debugger_Contextual_Menu
     (Process  : access Visual_Debugger_Record'Class)
      return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used in the
   --  debugger command window.

   procedure First_Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Standard handler to add gdb's output to the debugger window.
   --  Simply strip CR characters if needed and then call Text_Output_Filter

   procedure Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address);
   --  Real handler called by First_Text_Output_Filter

   function Debugger_Button_Press
     (Process : access Visual_Debugger_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback for all the button press events in the debugger command window.
   --  This is used to display the contexual menu.

   function On_Stack_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the Call Stack window.

   function On_Editor_Text_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the editor text

   function On_Command_Scrolledwindow_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the command window.

   procedure On_Debugger_Text_Grab_Focus
     (Object : access Glib.Object.GObject_Record'Class);
   --  Callback for the "grab_focus" signal on the command window.

   procedure On_Data_Canvas_Destroy
     (Canvas : access Gtk_Widget_Record'Class);
   --  Called when the data canvas is destroyed

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      Object  : System.Address) return String;
   --  Launch the command interpreter for Input and return the output.

   function Debuggee_Console_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      Object  : System.Address) return String;
   --  Handler of I/O for the debuggee console.

   function On_Debuggee_Console_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the debuggee console.

   function Complete_Command
     (Input  : in String;
      Object : System.Address)
      return String_List_Utils.String_List.List;
   --  Return the list of completions for Input.

   procedure Setup_Command_Window
     (Process : access Visual_Debugger_Record'Class);
   --  Set up/initialize the command window associated with Process.

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Visual_Debugger);

   ------------
   -- TTY_Cb --
   ------------

   function TTY_Cb (Data : Visual_Debugger) return Boolean is
      Match  : Expect_Match;
   begin
      Expect (Data.Debuggee_Descriptor, Match, Regexp_Any, Timeout => 1);

      if Match /= Expect_Timeout then
         Insert (Data.Debuggee_Console,
                 Expect_Out (Data.Debuggee_Descriptor),
                 Add_LF => False);
         Highlight_Child
           (Find_MDI_Child (Data.Window.MDI, Data.Debuggee_Console));
      end if;

      return True;

   exception
      when Process_Died =>
         Insert (Data.Debuggee_Console,
                 Expect_Out (Data.Debuggee_Descriptor),
                 Add_LF => False);
         Highlight_Child
           (Find_MDI_Child (Data.Window.MDI, Data.Debuggee_Console));

         if Command_In_Process (Get_Process (Data.Debugger)) then
            Data.Cleanup_TTY := True;
         else
            Close_TTY (Data.Debuggee_TTY);
            Allocate_TTY (Data.Debuggee_TTY);
            Close_Pseudo_Descriptor (Data.Debuggee_Descriptor);
            Pseudo_Descriptor (Data.Debuggee_Descriptor, Data.Debuggee_TTY, 0);
            Set_TTY (Data.Debugger, TTY_Name (Data.Debuggee_TTY));
            Flush (Data.Debuggee_Descriptor);
         end if;

         return True;
   end TTY_Cb;

   ----------------------
   -- Complete_Command --
   ----------------------

   function Complete_Command
     (Input  : in String;
      Object : System.Address)
     return String_List_Utils.String_List.List
   is
      use String_List_Utils.String_List;
      Top    : constant Visual_Debugger := Convert (Object);
      Result : List;
   begin
      if not Command_In_Process (Get_Process (Top.Debugger)) then
         --  Do not launch completion if the last character is ' ', as that
         --  might result in an output of several thousand entries, which
         --  will take a long time to parse.

         if Input /= ""
           and then Input (Input'Last) /= ' '
         then
            declare
               S : String_Array := Complete (Top.Debugger, Input);
            begin
               for J in S'Range loop
                  Append (Result, S (J).all);
               end loop;

               Free (S);
            end;
         end if;
      end if;

      if Is_Empty (Result) then
         Append (Result, Input);
      end if;

      return Result;
   end Complete_Command;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input  : String;
      Object : System.Address) return String
   is
      pragma Unreferenced (Console);
      Top : constant Visual_Debugger := Convert (Object);
   begin
      Top.Interactive_Command := True;
      Process_User_Command (Top, Input, Mode => User);
      return "";
   end Interpret_Command_Handler;

   ------------------------------
   -- Debuggee_Console_Handler --
   ------------------------------

   function Debuggee_Console_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      Object  : System.Address) return String
   is
      pragma Unreferenced (Console);
      Top : constant Visual_Debugger := Convert (Object);
      NL  : aliased Character := ASCII.LF;
      N   : Integer;
      pragma Unreferenced (N);

   begin
      N := Write
        (TTY_Descriptor (Top.Debuggee_TTY), Input'Address, Input'Length);
      N := Write (TTY_Descriptor (Top.Debuggee_TTY), NL'Address, 1);
      return "";
   end Debuggee_Console_Handler;

   ---------------------------
   -- On_Stack_Delete_Event --
   ---------------------------

   function On_Stack_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Object);

   begin
      Process.Stack := null;
      return False;
   end On_Stack_Delete_Event;

   ---------------------------------
   -- On_Editor_Text_Delete_Event --
   ---------------------------------

   function On_Editor_Text_Delete_Event
     (Object : access GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Object, Params);
   begin
      return False;
   end On_Editor_Text_Delete_Event;

   --------------------------------------------
   -- On_Command_Scrolledwindow_Delete_Event --
   --------------------------------------------

   function On_Command_Scrolledwindow_Delete_Event
     (Object : access GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Object);

   begin
      if Process.Exiting then
         Process.Debugger_Text := null;
         return False;
      else
         return True;
      end if;
   end On_Command_Scrolledwindow_Delete_Event;

   --------------------------------------
   -- On_Debuggee_Console_Delete_Event --
   --------------------------------------

   function On_Debuggee_Console_Delete_Event
     (Object : access GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);
      Process : constant Visual_Debugger :=
        Visual_Debugger (Object);

   begin
      if Process.Exiting then
         Process.Debuggee_Console := null;
         Close_Pseudo_Descriptor (Process.Debuggee_Descriptor);
         Close_TTY (Process.Debuggee_TTY);

         if Process.Debuggee_Id /= 0 then
            Gtk.Main.Timeout_Remove (Process.Debuggee_Id);
         end if;

         return False;
      else
         return True;
      end if;
   end On_Debuggee_Console_Delete_Event;

   ---------------------------------
   -- On_Debugger_Text_Grab_Focus --
   ---------------------------------

   procedure On_Debugger_Text_Grab_Focus
     (Object : access GObject_Record'Class)
   is
      Debugger : constant Visual_Debugger := Visual_Debugger (Object);
   begin
      Switch_Debugger (Debugger.Window, GObject (Object));
      Wind (Debugger.Command_History, Forward);
   end On_Debugger_Text_Grab_Focus;

   -----------------------
   -- Add_Regexp_Filter --
   -----------------------

   procedure Add_Regexp_Filter
     (Process : access Visual_Debugger_Record'Class;
      Filter  : Regexp_Filter_Function;
      Regexp  : Pattern_Matcher) is
   begin
      Process.Filters :=
        new Regexp_Filter_List_Elem'
          (Filter => Filter,
           Regexp => new Pattern_Matcher'(Regexp),
           Next   => Process.Filters);
   end Add_Regexp_Filter;

   -------------
   -- Convert --
   -------------

   function Convert
     (Main_Debug_Window : access GPS_Window_Record'Class;
      Descriptor        : GNAT.Expect.Process_Descriptor'Class)
      return Visual_Debugger
   is
      Process : Visual_Debugger;
      List    : Debugger_List_Link :=
        Get_Debugger_List (Main_Debug_Window.Kernel);

   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            --  Note: The process might have been already killed when this
            --  function is called.

            if Get_Descriptor
              (Get_Process (Process.Debugger)).all = Descriptor
            then
               return Process;
            end if;
         end if;

         List := List.Next;
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
     (Main_Debug_Window : access Gtk.Window.Gtk_Window_Record'Class;
      Debugger          : access Debugger_Root'Class)
      return Visual_Debugger is
   begin
      return Convert (GPS_Window (Main_Debug_Window),
                      Get_Descriptor (Get_Process (Debugger)).all);
   end Convert;

   ------------------------------
   -- Debugger_Contextual_Menu --
   ------------------------------

   function Debugger_Contextual_Menu
     (Process : access Visual_Debugger_Record'Class)
      return Gtk.Menu.Gtk_Menu
   is
      Mitem : Gtk_Menu_Item;
   begin
      if Process.Contextual_Menu /= null then
         return Process.Contextual_Menu;
      end if;

      Gtk_New (Process.Contextual_Menu);
      Gtk_New (Mitem, Label => -"Info");
      Set_State (Mitem, State_Insensitive);
      Append (Process.Contextual_Menu, Mitem);
      Show_All (Process.Contextual_Menu);
      return Process.Contextual_Menu;
   end Debugger_Contextual_Menu;

   -----------------
   -- Output_Text --
   -----------------

   procedure Output_Text
     (Process      : Visual_Debugger;
      Str          : String;
      Is_Command   : Boolean := False;
      Set_Position : Boolean := False)
   is
      pragma Unreferenced (Set_Position);
      Matched : GNAT.Regpat.Match_Array (0 .. 0);
      Start   : Positive := Str'First;
   begin
      if Is_Command then
         Insert (Process.Debugger_Text, Str, False, True, True);
      else
         while Start <= Str'Last loop
            Match (Highlighting_Pattern (Process.Debugger),
                   Str (Start .. Str'Last),
                   Matched);

            if Matched (0) /= No_Match then
               if Matched (0).First - 1 >= Start then
                  Insert (Process.Debugger_Text,
                          Str (Start .. Matched (0).First - 1),
                          False, False);
               end if;

               Insert (Process.Debugger_Text,
                       Str (Matched (0).First .. Matched (0).Last),
                       False, True);
               Start := Matched (0).Last + 1;

            else
               Insert (Process.Debugger_Text,
                       Str (Start .. Str'Last),
                       False, False);
               Start := Str'Last + 1;
            end if;
         end loop;
      end if;

      Highlight_Child
        (Find_MDI_Child (Process.Window.MDI, Process.Debugger_Text));
   end Output_Text;

   ------------------------
   -- Final_Post_Process --
   ------------------------

   procedure Final_Post_Process
     (Process : access Visual_Debugger_Record'Class;
      Mode    : GVD.Types.Command_Type)
   is
      File_First  : Natural := 0;
      File_Last   : Positive;
      Line        : Natural := 0;
      First, Last : Natural := 0;
      Addr_First  : Natural := 0;
      Addr_Last   : Natural;
      Pc          : Address_Type;
      Frame_Info  : Frame_Info_Type := Location_Not_Found;

   begin
      if Process.Post_Processing or else Process.Current_Output = null then
         return;
      end if;

      Process.Post_Processing := True;

      if Get_Parse_File_Name (Get_Process (Process.Debugger)) then
         Found_File_Name
           (Process.Debugger,
            Process.Current_Output
              (Process.Current_Output'First .. Process.Current_Output_Pos - 1),
            File_First, File_Last, First, Last, Line,
            Addr_First, Addr_Last);

         --  We have to make a temporary copy of the address, since
         --  the call to Load_File below might modify the current_output
         --  of the process, and thus make the address inaccessible afterwards.

         if Addr_First /= 0 then
            Pc := String_To_Address
              (Process.Current_Output (Addr_First .. Addr_Last));
         end if;
      end if;

      --  Do we have a file name or line number indication?

      if File_First /= 0 then
         --  Override the language currently defined in the editor.

         declare
            File_Name : constant Virtual_File := Create
              (Full_Filename =>
               --  ??? Normalize_Pathname only needed when to get absolute
               --  file name
                 Normalize_Pathname
                   (Process.Current_Output (File_First .. File_Last),
                    Resolve_Links => False));
         begin
            Load_File (Process.Editor_Text, File_Name);
         end;
      end if;

      if Line /= 0
        and then Mode /= Internal
      then
         Set_Line (Process.Editor_Text, Line, GObject (Process));
      end if;

      --  Change the current assembly source displayed, before updating
      --  the breakpoints. Otherwise, they won't be correctly updated for the
      --  newly displayed frame.

      if Addr_First /= 0 then
         Set_Address (Process.Editor_Text, Pc);
      end if;

      if (Line /= 0 and then Mode /= Internal) or else Addr_First /= 0 then
         Update_Assembly_View (Process.Editor_Text);
      end if;

      Found_Frame_Info
        (Process.Debugger,
         Process.Current_Output.all,
         First, Last, Frame_Info);

      if Process.Stack /= null
        and then Frame_Info = Location_Found
      then
         Highlight_Frame
           (Process.Stack,
            Integer'Value (Process.Current_Output (First .. Last)));
      end if;

      if Frame_Info = No_Debug_Info then
         Show_Message (Process.Editor_Text,
                       -"There is no debug information for this frame.");
      end if;

      --  Last step is to update the breakpoints once all the rest has been
      --  set up correctly.
      --  If there is no breakpoint defined, we force an update.

      if File_First /= 0 then
         if Process.Breakpoints = null then
            Update_Breakpoints (Process, Force => True);

         elsif Process.Breakpoints'Length > 0 then
            Update_Breakpoints
              (Process.Editor_Text, Process.Breakpoints.all);
         end if;
      end if;

      if Process.Cleanup_TTY then
         Process.Cleanup_TTY := False;
         Close_TTY (Process.Debuggee_TTY);
         Allocate_TTY (Process.Debuggee_TTY);
         Close_Pseudo_Descriptor (Process.Debuggee_Descriptor);
         Pseudo_Descriptor
           (Process.Debuggee_Descriptor, Process.Debuggee_TTY, 0);
         Set_TTY (Process.Debugger, TTY_Name (Process.Debuggee_TTY));
         Flush (Process.Debuggee_Descriptor);
      end if;

      Process.Post_Processing := False;
      Free (Process.Current_Output);

      --  Preserve the focus in the console for interactive execution
      if Process.Interactive_Command then
         Set_Focus_Child
           (Find_MDI_Child
              (Process.Window.MDI, Process.Debugger_Text));
         Process.Interactive_Command := False;
      end if;
   end Final_Post_Process;

   ------------------------------
   -- First_Text_Output_Filter --
   ------------------------------

   procedure First_Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address) is
   begin
      if Need_To_Strip_CR then
         Text_Output_Filter (Descriptor, Strip_CR (Str), Window);
      else
         Text_Output_Filter (Descriptor, Str, Window);
      end if;
   end First_Text_Output_Filter;

   ------------------------
   -- Text_Output_Filter --
   ------------------------

   procedure Text_Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      Window     : System.Address)
   is
      Process        : constant Visual_Debugger :=
        Convert (To_Main_Debug_Window (Window), Descriptor);
      Tmp_Str        : GNAT.OS_Lib.String_Access;
      Current_Filter : Regexp_Filter_List;
      Matched        : Match_Array (0 .. Max_Paren_Count);
      First, Last    : Natural := 0;
      Last_Match     : Natural := 0;
      Min_Size       : Natural;
      New_Size       : Natural;

   begin
      --  Concatenate current output

      if Process.Current_Output = null then
         Process.Current_Output := new String (1 .. 1024);
         Process.Current_Output_Pos := 1;
         Process.Last_Match := 0;
      end if;

      Min_Size := Process.Current_Output_Pos + Str'Length;

      if Process.Current_Output'Last < Min_Size then
         New_Size := Process.Current_Output'Length * 2;

         while New_Size < Min_Size loop
            New_Size := New_Size * 2;
         end loop;

         Tmp_Str := new String (1 .. New_Size);
         Tmp_Str (1 .. Process.Current_Output_Pos - 1) :=
           Process.Current_Output (1 .. Process.Current_Output_Pos - 1);
         Free (Process.Current_Output);
         Process.Current_Output := Tmp_Str;
      end if;

      Process.Current_Output
        (Process.Current_Output_Pos ..
         Process.Current_Output_Pos + Str'Length - 1) := Str;
      Process.Current_Output_Pos := Process.Current_Output_Pos + Str'Length;

      --  Process the filters

      Current_Filter := Process.Filters;

      while Current_Filter /= null loop
         Match
           (Current_Filter.Regexp.all,
            Process.Current_Output
              (Process.Last_Match + 1 .. Process.Current_Output_Pos - 1),
            Matched);

         if Matched (0) /= No_Match then
            if Matched (0).Last > Last_Match then
               Last_Match := Matched (0).Last;
            end if;

            Current_Filter.Filter
              (Process,
               Process.Current_Output (1 .. Process.Current_Output_Pos - 1),
               Matched);
         end if;

         Current_Filter := Current_Filter.Next;
      end loop;

      if Last_Match /= 0 then
         Process.Last_Match := Last_Match;
      end if;

      --  Do not show the output if we have an internal or hidden command

      case Get_Command_Mode (Get_Process (Process.Debugger)) is
         when User | GVD.Types.Visible =>
            --  Strip every line starting with ^Z^Z.
            --  Note that this is GDB specific ???

            Outer_Loop :
            for J in Str'First + 1 .. Str'Last loop
               if Str (J) = ASCII.SUB and then Str (J - 1) = ASCII.SUB then
                  First := J - 1;

                  for K in J + 1 .. Str'Last loop
                     if Str (K) = ASCII.LF then
                        Last := K;
                        exit Outer_Loop;
                     end if;
                  end loop;

                  Last := Str'Last;
                  exit Outer_Loop;
               end if;
            end loop Outer_Loop;

            if First = 0 then
               Output_Text (Process, Str, Set_Position => True);
            else
               Output_Text (Process, Str (Str'First .. First - 1));
               Output_Text
               (Process, Str (Last + 1 .. Str'Last), Set_Position => True);
            end if;

         when Hidden | Internal =>
            null;
      end case;
   end Text_Output_Filter;

   ---------------------------
   -- Debugger_Button_Press --
   ---------------------------

   function Debugger_Button_Press
     (Process : access Visual_Debugger_Record'Class;
      Event    : Gdk.Event.Gdk_Event) return Boolean is
   begin
      if Get_Button (Event) = 3 then
         Popup (Debugger_Contextual_Menu (Process),
                Button        => Get_Button (Event),
                Activate_Time => Get_Time (Event));
         Emit_Stop_By_Name (Process.Debugger_Text, "button_press_event");

         return True;
      end if;

      return False;
   end Debugger_Button_Press;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Process : out Visual_Debugger;
      Window  : access GPS.Main_Window.GPS_Window_Record'Class;
      Source  : GVD.Source_Editor.Source_Editor) is
   begin
      Process := new Visual_Debugger_Record;
      Initialize (Process, Window, Source);
   end Gtk_New;

   -----------------------
   -- Create_Call_Stack --
   -----------------------

   procedure Create_Call_Stack
     (Process : access Visual_Debugger_Record'Class)
   is
      Child : MDI_Child;
   begin
      Gtk_New (Process.Stack);

      Object_Return_Callback.Object_Connect
        (Process.Stack, "delete_event",
         On_Stack_Delete_Event'Access, Process);

      Child := Put
        (Get_Kernel (Debugger_Module_ID.all),
         Process.Stack,
         Position => Position_Debugger_Stack,
         Module   => Debugger_Module_ID);
      Set_Focus_Child (Child);
      Split (Process.Window.MDI,
             Orientation_Horizontal,
             Reuse_If_Possible => True,
             Width => 200,
             After => True);

      if Process.Debugger_Num = 1 then
         Set_Title (Child, -"Call Stack");
      else
         Set_Title (Child, (-"Call Stack") & " <" &
                    Image (Process.Debugger_Num) & ">");
      end if;
   end Create_Call_Stack;

   ----------------------------
   -- On_Data_Canvas_Destroy --
   ----------------------------

   procedure On_Data_Canvas_Destroy
     (Canvas : access Gtk_Widget_Record'Class)
   is
      Process : constant Visual_Debugger :=
        Visual_Debugger (Get_Process (GVD_Canvas (Canvas)));
   begin
      Process.Data_Canvas := null;
      Process.Data_Scrolledwindow := null;
   end On_Data_Canvas_Destroy;

   ------------------------
   -- Create_Data_Window --
   ------------------------

   procedure Create_Data_Window
     (Process : access Visual_Debugger_Record'Class)
   is
      Data_Window_Name : constant String := -"Debugger Data";
      Child           : MDI_Child;
      Annotation_Font : Pango_Font_Description;
   begin
      if Process.Data_Canvas = null then
         --  Create the data area

         Gtk_New (Process.Data_Scrolledwindow);
         Set_Policy
           (Process.Data_Scrolledwindow, Policy_Automatic, Policy_Automatic);

         --  Create the canvas for this visual debugger.

         Gtk_New (GVD_Canvas (Process.Data_Canvas),
                  GPS.Kernel.Get_History (GPS_Window (Process.Window).Kernel));
         Add (Process.Data_Scrolledwindow, Process.Data_Canvas);
         Set_Process (GVD_Canvas (Process.Data_Canvas), Process);
         Widget_Callback.Connect
           (Process.Data_Canvas, "background_click",
            Widget_Callback.To_Marshaller (On_Background_Click'Access));
         Widget_Callback.Connect
           (Process.Data_Canvas, "destroy",
            On_Data_Canvas_Destroy'Access);
         Align_On_Grid (Process.Data_Canvas, True);

         --  Initialize the canvas

         Annotation_Font :=
           Copy (Get_Pref (GPS.Kernel.Preferences.Default_Font));
         Set_Size
           (Annotation_Font,
            Gint'Max (Pango_Scale,
                      Get_Size (Annotation_Font) - 2 * Pango_Scale));
         Configure (Process.Data_Canvas, Annotation_Font => Annotation_Font);
         Free (Annotation_Font);

         Child := Put
           (Get_Kernel (Debugger_Module_ID.all),
            Process.Data_Scrolledwindow,
            Position => Position_Debugger_Data,
            Module   => Debugger_Module_ID);
         Set_Focus_Child (Child);
         Split (Process.Window.MDI,
                Orientation_Vertical,
                Reuse_If_Possible => True,
                Height => 200,
                After => False);

         if Process.Debugger_Num = 1 then
            Set_Title (Child, Data_Window_Name);
         else
            Set_Title (Child, Data_Window_Name & " <" &
                       Image (Process.Debugger_Num) & ">");
         end if;

         --  Initialize the pixmaps and colors for the canvas

         Realize (Process.Data_Canvas);
         Init_Graphics (GVD_Canvas (Process.Data_Canvas));

      else
         Child := Find_MDI_Child
           (Process.Window.MDI, Process.Data_Scrolledwindow);
         Set_Focus_Child (Child);
      end if;
   end Create_Data_Window;

   --------------------------
   -- Setup_Command_Window --
   --------------------------

   procedure Setup_Command_Window
     (Process : access Visual_Debugger_Record'Class)
   is
      Child : MDI_Child;
      H     : constant History := GPS.Kernel.Get_History
        (GPS_Window (Process.Window).Kernel);

   begin
      Gtk_New
        (Process.Debugger_Text,
         "",
         Interpret_Command_Handler'Access,
         Process.all'Address,
         Process.Debugger_Text_Font,
         History_List => H,
         Key          => "gvd_console",
         Wrap_Mode    => Wrap_Char,
         Empty_Equals_Repeat => True);

      if H /= null then
         Set_Max_Length (H.all, 100, "gvd_console");
         Allow_Duplicates (H.all, "gvd_console", True, True);
      end if;

      Set_Highlight_Color
        (Process.Debugger_Text,
         Process.Debugger_Text_Highlight_Color);

      Set_Completion_Handler
        (Process.Debugger_Text,
         Complete_Command'Access);

      Object_Return_Callback.Object_Connect
        (Process.Debugger_Text, "delete_event",
         On_Command_Scrolledwindow_Delete_Event'Access, Process);

      Object_Callback.Object_Connect
        (Get_View (Process.Debugger_Text), "grab_focus",
         Object_Callback.To_Marshaller (On_Debugger_Text_Grab_Focus'Access),
         Process);

      --  Set up the command window for the contextual menus

      Add_Events (Process.Debugger_Text, Button_Press_Mask);
      Canvas_Event_Handler.Object_Connect
        (Process.Debugger_Text, "button_press_event",
         Canvas_Event_Handler.To_Marshaller (Debugger_Button_Press'Access),
         Process);

      --  Add debugger console in the MDI

      Child := Put
        (Process.Window.MDI, Process.Debugger_Text,
         Flags => All_Buttons,
         Position => Position_Bottom,
         Focus_Widget => Gtk_Widget (Get_View (Process.Debugger_Text)));
      Set_Focus_Child (Child);

      if Process.Debugger_Num = 1 then
         Set_Title (Child, -"Debugger Console");
      else
         Set_Title (Child, (-"Debugger Console") & " <" &
                    Image (Process.Debugger_Num) & ">");
      end if;
      Raise_Child (Child);
   end Setup_Command_Window;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Process : access Visual_Debugger_Record'Class;
      Window  : access GPS.Main_Window.GPS_Window_Record'Class;
      Source  : GVD.Source_Editor.Source_Editor)
   is
      Debugger_List : Debugger_List_Link;
      Debugger_Num  : Natural := 1;
   begin
      Initialize (Process);
      Ref (Process);
      Process.Window := Window.all'Access;

      Gtk_New_Hbox (Process.Editor_Text, Process);
      Object_Return_Callback.Object_Connect
        (Process.Editor_Text, "delete_event",
         On_Editor_Text_Delete_Event'Access, Process);

      Glib.Object.Initialize_Class_Record
        (Process, Signals, Class_Record,
         Type_Name => "GvdDebuggerProcessTab");

      Object_Callback.Connect
        (Process, "process_stopped",
         Object_Callback.To_Marshaller (On_Canvas_Process_Stopped'Access));
      Object_Callback.Connect
        (Process, "context_changed",
         Object_Callback.To_Marshaller (On_Canvas_Process_Stopped'Access));
      Object_Callback.Connect
        (Process, "process_stopped",
         Object_Callback.To_Marshaller (On_Stack_Process_Stopped'Access));
      Object_Callback.Connect
        (Process, "context_changed",
         Object_Callback.To_Marshaller (On_Stack_Process_Stopped'Access));
      Object_Callback.Connect
        (Process, "process_stopped",
         Object_Callback.To_Marshaller (On_Task_Process_Stopped'Access));
      Object_Callback.Connect
        (Process, "process_stopped",
         Object_Callback.To_Marshaller (On_Thread_Process_Stopped'Access));

      --  Allocate the colors for highlighting. This needs to be done before
      --  Initializing the debugger, since some file name might be output at
      --  that time.

      Process.Debugger_Text_Highlight_Color :=
        Get_Pref (Debugger_Highlight_Color);

      Process.Debugger_Text_Font := Get_Pref_Font (Default_Style);

      --  Initialize the code editor.
      --  This should be done before initializing the debugger, in case the
      --  debugger outputs a file name that should be displayed in the editor.
      --  The language of the editor will automatically be set by the output
      --  filter.

      Configure
        (Process.Editor_Text,
         Source,
         Get_Pref_Font (Default_Style),
         arrow_xpm, stop_xpm);

      Set_Current_Debugger (Window.Kernel, GObject (Process));

      if Get_Debugger_List (Window.Kernel) = null then
         Process.Debugger_Num := Debugger_Num;
         Set_First_Debugger
           (Window.Kernel,
            new Debugger_List_Node'
              (Next     => null,
               Debugger => Get_Current_Debugger (Window.Kernel)));

      else
         Debugger_Num := Debugger_Num + 1;
         Debugger_List := Get_Debugger_List (Window.Kernel);

         while Debugger_List.Next /= null loop
            Debugger_Num := Debugger_Num + 1;
            Debugger_List := Debugger_List.Next;
         end loop;

         Process.Debugger_Num := Debugger_Num;
         Debugger_List.Next := new Debugger_List_Node'
           (Next     => null,
            Debugger => Get_Current_Debugger (Window.Kernel));
      end if;
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Process         : access Visual_Debugger_Record'Class;
      Kind            : Debugger_Type;
      Proxy           : Process_Proxy_Access;
      Executable      : String;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "";
      Success         : out Boolean)
   is
      Timeout       : constant Guint32 := 50;

      Child         : MDI_Child;
      Window        : constant GPS_Window :=
        GPS_Window (Process.Window);
      Buttons       : Message_Dialog_Buttons;
      pragma Unreferenced (Buttons);
      WTX_Version   : Natural := 0;
      Widget        : Gtk_Menu_Item;

   begin
      Set_Busy (Process, True);
      Setup_Command_Window (Process);

      if Get_Pref (Show_Call_Stack) then
         Create_Call_Stack (Process);
      end if;

      Process.Descriptor.Debugger := Kind;
      Process.Descriptor.Remote_Host := new String'(Remote_Host);

      if Remote_Protocol = "" then
         Process.Descriptor.Remote_Target := new String'("");
         Process.Descriptor.Protocol := new String'("");
      else
         Process.Descriptor.Remote_Target := new String'(Remote_Target);
         Process.Descriptor.Protocol := new String'(Remote_Protocol);
      end if;

      Process.Descriptor.Program := new String'(Executable);
      Process.Descriptor.Debugger_Name := new String'(Debugger_Name);

      case Kind is
         when Gdb_Type =>
            Process.Debugger := new Gdb_Debugger;
         when others =>
            Set_Busy (Process, False);
            raise Debugger_Not_Supported;
      end case;

      --  Spawn the debugger.

      if Remote_Host /= "" or else Is_Regular_File (Executable) then
         Spawn
           (Process.Debugger,
            Executable,
            Debugger_Args,
            Executable_Args,
            Proxy,
            Process.Window.all'Access,
            Remote_Host,
            Remote_Target,
            Remote_Protocol,
            Debugger_Name);
      else
         Spawn
           (Process.Debugger, "", Debugger_Args, Executable_Args,
            Proxy,
            Process.Window.all'Access, Remote_Host, Remote_Target,
            Remote_Protocol, Debugger_Name);

         if Executable /= "" then
            Output_Error
              (GPS_Window (Process.Window).Kernel,
               (-" Could not find file: ") & Executable);
         end if;
      end if;

      --  Set the output filter, so that we output everything in the Gtk_Text
      --  window.

      Add_Filter
        (Get_Descriptor (Get_Process (Process.Debugger)).all,
         First_Text_Output_Filter'Access, Output, Process.Window.all'Address);

      --  Initialize the debugger, and possibly get the name of the initial
      --  file.

      Initialize (Process.Debugger);

      --  Hide or show AE653 specific capabilities according to the debugger
      --  we are using

      Widget := Find_Menu_Item
        (Window.Kernel, -"/Debug/Data/Protection Domains");

      if Widget /= null then
         Info_WTX (Process.Debugger, WTX_Version);
         Set_Sensitive (Widget, WTX_Version >= 3);
      end if;

      if Get_Pref (Execution_Window)
        and then Support_TTY (Process.Debugger)
        and then GNAT.TTY.TTY_Supported
      then
         Gtk_New
           (Process.Debuggee_Console,
            "",
            Debuggee_Console_Handler'Access,
            Process.all'Address,
            Process.Debugger_Text_Font,
            History_List => null,
            Key          => "gvd_tty_console",
            Wrap_Mode    => Wrap_Char);

         Object_Return_Callback.Object_Connect
           (Process.Debuggee_Console, "delete_event",
            On_Debuggee_Console_Delete_Event'Access, Process);

         Allocate_TTY (Process.Debuggee_TTY);
         Set_TTY (Process.Debugger, TTY_Name (Process.Debuggee_TTY));
         Pseudo_Descriptor
           (Process.Debuggee_Descriptor, Process.Debuggee_TTY, 0);
         Flush (Process.Debuggee_Descriptor);

         Process.Debuggee_Id :=
           TTY_Timeout.Add (Timeout, TTY_Cb'Access, Process.all'Access);

         Child := Put
           (Window.MDI, Process.Debuggee_Console,
            Flags => All_Buttons,
            Position       => Position_Bottom,
            Focus_Widget => Gtk_Widget (Get_View (Process.Debuggee_Console)));

         if Process.Debugger_Num = 1 then
            Set_Title (Child, -"Debugger Execution");
         else
            Set_Title (Child, (-"Debugger Execution") & " <" &
                       Image (Process.Debugger_Num) & ">");
         end if;
      end if;

      Set_Busy (Process, False);
      Success := True;

   exception
      when Process_Died =>
         Set_Busy (Process, False);
         Buttons :=
           Message_Dialog
             (Expect_Out (Get_Process (Process.Debugger)) & ASCII.LF &
              (-"Could not launch the debugger"),
              Error, Button_OK, Button_OK);
         Process.Exiting := True;

         Close (Window.MDI, Process.Debugger_Text);
         if Process.Data_Scrolledwindow /= null then
            Close (Window.MDI, Process.Data_Scrolledwindow);
         end if;

         if Process.Stack /= null then
            Close (Window.MDI, Process.Stack);
         end if;

         Process.Exiting := False;
         Success := False;

      when Spawn_Error =>
         --  Do not display a dialog here since the Spawn procedure displays
         --  a dialog before raising Spawn_Error.

         Set_Busy (Process, False);
         Success := False;
   end Configure;

   ---------------------
   -- Context_Changed --
   ---------------------

   procedure Context_Changed
     (Debugger : access Visual_Debugger_Record'Class) is
   begin
      --  Emit the signal
      Object_Callback.Emit_By_Name (GObject (Debugger), "context_changed");
   end Context_Changed;

   ------------------------
   -- Executable_Changed --
   ------------------------

   procedure Executable_Changed
     (Debugger        : access Visual_Debugger_Record'Class;
      Executable_Name : String)
   is
      pragma Unreferenced (Executable_Name);
   begin
      --  Emit the signal
      Object_Callback.Emit_By_Name (GObject (Debugger), "executable_changed");
   end Executable_Changed;

   ---------------------
   -- Process_Stopped --
   ---------------------

   procedure Process_Stopped
     (Debugger : access Visual_Debugger_Record'Class) is
   begin
      Object_Callback.Emit_By_Name (GObject (Debugger), "process_stopped");
   end Process_Stopped;

   -----------------------
   -- Process_Graph_Cmd --
   -----------------------

   procedure Process_Graph_Cmd
     (Process : access Visual_Debugger_Record'Class;
      Cmd     : String)
   is
      Matched   : Match_Array (0 .. 10);
      Matched2  : Match_Array (0 .. 10);
      Item      : Display_Item;
      Index,
      Last      : Positive;
      Enable    : Boolean;
      First     : Natural;
      Link_Name : Basic_Types.String_Access;
      Link_From : Display_Item;
      Dependent_On_First : Natural := Natural'Last;
      Link_Name_First    : Natural := Natural'Last;

   begin
      --  graph (print|display) expression [dependent on display_num]
      --        [link_name name]
      --  graph (print|display) `command`
      --  graph enable display display_num [display_num ...]
      --  graph disable display display_num [display_num ...]
      --  graph undisplay display_num

      Match (Graph_Cmd_Format, Cmd, Matched);

      if Matched (0) /= No_Match then
         Create_Data_Window (Process);
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
               Entity : constant Items.Generic_Type_Access :=
                 New_Debugger_Type (Expr);

            begin
               Set_Value
                 (Debugger_Output_Type (Entity.all),
                  Send
                    (Process.Debugger,
                     Refresh_Command (Debugger_Output_Type (Entity.all)),
                     Mode => Internal));

               --  No link ?

               if Dependent_On_First = Natural'Last then
                  Gtk_New
                    (Item,
                     Variable_Name  => Expr,
                     Debugger       => Process,
                     Auto_Refresh   => Enable,
                     Default_Entity => Entity);
                  Put (Process.Data_Canvas, Item);

               else
                  Gtk_New
                    (Item,
                     Variable_Name  => Expr,
                     Debugger       => Process,
                     Auto_Refresh   => Enable,
                     Default_Entity => Entity,
                     Link_From      => Link_From,
                     Link_Name      => Link_Name.all);
               end if;

               if Item /= null then
                  Show_Item (Process.Data_Canvas, Item);
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
                  Last := Matched (Graph_Cmd_Rest_Paren).Last;
               else
                  Last := Last - 1;
               end if;
            end if;

            --  If we don't want any link:

            if Dependent_On_First = Natural'Last then
               Gtk_New
                 (Item,
                  Variable_Name => Cmd (First .. Last),
                  Debugger      => Process,
                  Auto_Refresh  =>
                    Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'd');

               if Item /= null then
                  Put (Process.Data_Canvas, Item);
                  Show_Item (Process.Data_Canvas, Item);
                  Recompute_All_Aliases
                    (Process.Data_Canvas, Recompute_Values => False);
               end if;

            --  Else if we have a link

            else
               if Link_Name = null then
                  Link_Name := new String'(Cmd (First .. Last));
               end if;

               Gtk_New
                 (Item,
                  Variable_Name => Cmd (First .. Last),
                  Debugger      => Process,
                  Auto_Refresh  => Enable,
                  Link_From     => Link_From,
                  Link_Name     => Link_Name.all);

               if Item /= null then
                  Show_Item (Process.Data_Canvas, Item);
               end if;
            end if;
         end if;

         Free (Link_Name);

      else
         --  Is this an enable/disable command ?

         Match (Graph_Cmd_Format2, Cmd, Matched);

         if Matched (2) /= No_Match then
            Create_Data_Window (Process);

            Index := Matched (2).First;
            Enable := Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'e'
              or else Cmd (Matched (Graph_Cmd_Type_Paren).First) = 'E';

            while Index <= Cmd'Last loop
               Last := Index;
               Skip_To_Blank (Cmd, Last);
               Set_Auto_Refresh
                 (Find_Item
                    (Process.Data_Canvas,
                     Integer'Value (Cmd (Index .. Last - 1))),
                  Enable,
                  Update_Value => True);
               Index := Last + 1;
               Skip_Blanks (Cmd, Index);
            end loop;

         --  Third possible set of commands

         else
            Match (Graph_Cmd_Format3, Cmd, Matched);
            if Matched (1) /= No_Match then
               Create_Data_Window (Process);
               Index := Matched (1).First;
               while Index <= Cmd'Last loop
                  Last := Index;
                  Skip_To_Blank (Cmd, Last);
                  Free
                    (Find_Item
                      (Process.Data_Canvas,
                       Integer'Value (Cmd (Index .. Last - 1))));
                  Index := Last + 1;
                  Skip_Blanks (Cmd, Index);
               end loop;
            end if;
         end if;
      end if;

   exception
      when Constraint_Error =>
         --  Usually because Find_Item returned a null value.
         Output_Error
           (GPS_Window (Process.Window).Kernel,
            (-" Error while processing: ") & Cmd);
   end Process_Graph_Cmd;

   --------------------
   -- Close_Debugger --
   --------------------

   procedure Close_Debugger (Debugger : Visual_Debugger) is
      Top  : constant GPS_Window := Debugger.Window;
      List : Debugger_List_Link;
   begin
      if Debugger.Exiting then
         return;
      end if;

      Object_Callback.Emit_By_Name (GObject (Debugger), "debugger_closed");
      Debugger.Exiting := True;
      Free (Debugger.Breakpoints);
      Unregister_Dialog (Debugger);

      if Debugger.Timeout_Id /= 0 then
         Set_Busy (Debugger, False);
         Timeout_Remove (Debugger.Timeout_Id);
         Debugger.Timeout_Id := 0;
      end if;

      if Debugger.Debugger /= null then
         if Get_Command_Mode (Get_Process (Debugger.Debugger))
           in Visible_Command
         then
            Set_Busy (Debugger, False);
         end if;

         Close (Debugger.Debugger);
         Debugger.Debugger := null;
      end if;

      --  Recompute Top.Current_Debugger and Top.First_Debugger:

      List := Get_Debugger_List (Top.Kernel);

      if List /= null then
         if Visual_Debugger (List.Debugger) = Debugger then
            Set_First_Debugger (Top.Kernel, List.Next);
            Set_Current_Debugger (Top.Kernel, List.Next.Debugger);
         else
            while List.Next /= null loop
               if Visual_Debugger (List.Next.Debugger) = Debugger then
                  List.Next := List.Next.Next;
                  Set_Current_Debugger (Top.Kernel, List.Debugger);

                  exit;
               end if;

               List := List.Next;
            end loop;
         end if;
      end if;

      Free (Debugger.Command_History);
      Unref (Debugger);
   end Close_Debugger;

   --------------------------
   -- Process_User_Command --
   --------------------------

   procedure Process_User_Command
     (Debugger       : Visual_Debugger;
      Command        : String;
      Output_Command : Boolean := False;
      Mode           : Visible_Command := GVD.Types.Visible)
   is
      Quit_String     : constant String := "quit     ";
      Lowered_Command : constant String := To_Lower (Command);
      First           : Natural := Lowered_Command'First;
      Data            : History_Data;

      procedure Pre_User_Command;
      --  Handle all the set up for a user command (logs, history, ...)

      procedure Pre_User_Command is
      begin
         Output_Message (Debugger, Command, Mode);
         Data.Mode := Mode;
         Skip_Blanks (Command, First);
         Data.Command := new String'(Command);
         Append (Debugger.Command_History, Data);
         Set_Busy (Debugger);
      end Pre_User_Command;

   begin
      if Debugger.Debugger = null then
         return;
      end if;

      if Output_Command then
         Output_Text (Debugger, Command & ASCII.LF, Is_Command => True);
      end if;

      --  ??? Should forbid commands that modify the configuration of the
      --  debugger, like "set annotate" for gdb, otherwise we can't be sure
      --  what to expect from the debugger.

      --  Command has been converted to lower-cases, but the new version
      --  should be used only to compare with our standard list of commands.
      --  We should pass the original string to the debugger, in case we are
      --  in a case-sensitive language.

      --  Ignore the blanks at the beginning of lines

      Skip_Blanks (Lowered_Command, First);

      if not Command_In_Process (Get_Process (Debugger.Debugger))
        and then Looking_At (Lowered_Command, First, "graph")
      then
         Pre_User_Command;
         Process_Graph_Cmd (Debugger, Command);

         if not Command_In_Process (Get_Process (Debugger.Debugger)) then
            Display_Prompt (Debugger.Debugger);
         end if;

         Set_Busy (Debugger, False);

      elsif Lowered_Command'Length <= Quit_String'Length
        and then Lowered_Command = Quit_String (1 .. Lowered_Command'Length)
      then
         if Command_In_Process (Get_Process (Debugger.Debugger))
           and then not Separate_Execution_Window (Debugger.Debugger)
         then
            --  If the debugger does not have a separate execution window,
            --  send the command right away.

            Send
              (Debugger.Debugger,
               Command, Wait_For_Prompt => False, Mode => Mode);

         else
            Close_Debugger (Debugger);
         end if;

      else
         --  Regular debugger command, send it.
         --  If a dialog is currently displayed, do not wait for the debugger
         --  prompt, since the prompt won't be displayed before the user
         --  answers the question...

         if Continuation_Line (Debugger.Debugger)
           or else Debugger.Registered_Dialog /= null
         then
            Send
              (Debugger.Debugger,
               Command, Wait_For_Prompt => False, Mode => Mode);
         else
            Send (Debugger.Debugger, Command, Mode => Mode);
         end if;
      end if;
   end Process_User_Command;

   ---------------------
   -- Register_Dialog --
   ---------------------

   procedure Register_Dialog
     (Process : access Visual_Debugger_Record;
      Dialog  : access Gtk.Dialog.Gtk_Dialog_Record'Class) is
   begin
      if Process.Registered_Dialog /= null then
         --  Typically happens when the filter used to create a dialog
         --  is called several times for the same dialog.

         Destroy (Process.Registered_Dialog);
      end if;

      Process.Registered_Dialog := Gtk_Dialog (Dialog);
   end Register_Dialog;

   -----------------------
   -- Unregister_Dialog --
   -----------------------

   procedure Unregister_Dialog
     (Process : access Visual_Debugger_Record) is
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
     (Process : access GObject_Record'Class;
      Force   : Boolean)
   is
      Debugger  : constant Visual_Debugger := Visual_Debugger (Process);
      Bp_Editor : Breakpoint_Editor_Access;
   begin
      --  We only need to update the list of breakpoints when we have a
      --  temporary breakpoint (since its status might be changed upon
      --  reaching the line).

      if Force or else Debugger.Has_Temporary_Breakpoint then
         Free (Debugger.Breakpoints);
         Debugger.Breakpoints := new Breakpoint_Array'
           (List_Breakpoints (Debugger.Debugger));

         --  Check whether there is any temporary breakpoint

         Debugger.Has_Temporary_Breakpoint := False;

         for J in Debugger.Breakpoints'Range loop
            if Debugger.Breakpoints (J).Disposition /= Keep
              and then Debugger.Breakpoints (J).Enabled
            then
               Debugger.Has_Temporary_Breakpoint := True;
               exit;
            end if;
         end loop;

         --  Update the breakpoints in the editor
         Update_Breakpoints (Debugger.Editor_Text, Debugger.Breakpoints.all);

         --  Update the breakpoints dialog if necessary
         Bp_Editor := Breakpoint_Editor_Access
           (Get_Breakpoints_Editor (Debugger.Window.Kernel));

         if Bp_Editor /= null and then Mapped_Is_Set (Bp_Editor) then
            Update_Breakpoint_List (Bp_Editor);
         end if;
      end if;
   end Update_Breakpoints;

   -----------------------------
   -- Toggle_Breakpoint_State --
   -----------------------------

   function Toggle_Breakpoint_State
     (Process        : access Visual_Debugger_Record;
      Breakpoint_Num : Breakpoint_Identifier) return Boolean is
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
                  Mode => GVD.Types.Visible);
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
      return Visual_Debugger is
   begin
      return Visual_Debugger
        (Get_Current_Debugger (GPS_Window (Main_Window).Kernel));
   end Get_Current_Process;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy
     (Debugger      : access Visual_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False) is
   begin
      Set_Busy_Cursor (Get_Window (Debugger.Window), Busy, Force_Refresh);
   end Set_Busy;

   -------------
   -- Get_Num --
   -------------

   function Get_Num (Tab : Visual_Debugger) return Gint is
   begin
      return Gint (Tab.Debugger_Num);
   end Get_Num;

   ---------------------------------
   -- Set_Current_Source_Location --
   ---------------------------------

   procedure Set_Current_Source_Location
     (Process : access Visual_Debugger_Record;
      File    : VFS.Virtual_File;
      Line    : Integer) is
   begin
      Process.Current_File := File;
      Process.Current_Line := Line;
   end Set_Current_Source_Location;

   -----------------------------
   -- Get_Current_Source_File --
   -----------------------------

   function Get_Current_Source_File
     (Process : access Visual_Debugger_Record)
      return VFS.Virtual_File is
   begin
      return Process.Current_File;
   end Get_Current_Source_File;

   -----------------------------
   -- Get_Current_Source_Line --
   -----------------------------

   function Get_Current_Source_Line
     (Process : access Visual_Debugger_Record)
     return Integer is
   begin
      return Process.Current_Line;
   end Get_Current_Source_Line;

end GVD.Process;
