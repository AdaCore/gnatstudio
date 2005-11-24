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

with Glib.Object;                use Glib.Object;
with Glib; use Glib;

with Gtk.Arguments;              use Gtk.Arguments;
with Gtk.Dialog;                 use Gtk.Dialog;
with Gtk.Main;                   use Gtk.Main;
with Gtk.Menu_Item;              use Gtk.Menu_Item;
with Gtk.Object;                 use Gtk.Object;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;
with Gtk;                        use Gtk;

with Gtkada.Dialogs;             use Gtkada.Dialogs;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Types;               use Gtkada.Types;

with Breakpoints_Editor;         use Breakpoints_Editor;
with Config;                     use Config;
with Debugger.Gdb;               use Debugger.Gdb;
with Display_Items;              use Display_Items;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel;
with GPS.Main_Window;            use GPS.Main_Window;
with GUI_Utils;                  use GUI_Utils;
with GVD.Call_Stack;             use GVD.Call_Stack;
with GVD.Canvas;                 use GVD.Canvas;
with GVD.Code_Editors;           use GVD.Code_Editors;
with GVD.Consoles;               use GVD.Consoles;
with GVD.Dialogs;                use GVD.Dialogs;
with GVD.Preferences;            use GVD.Preferences;
with GVD.Source_Editor;          use GVD.Source_Editor;
with GVD.Trace;                  use GVD.Trace;
with GVD.Types;                  use GVD.Types;
with GVD_Module;                 use GVD_Module;
with Pixmaps_IDE;                use Pixmaps_IDE;
with String_Utils;               use String_Utils;
with VFS;                        use VFS;

package body GVD.Process is

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

   -----------------------
   -- Local Subprograms --
   -----------------------

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

   function On_Editor_Text_Delete_Event
     (Object : access Glib.Object.GObject_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   --  Callback for the "delete_event" signal on the editor text

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

      Highlight_Call_Stack_Frame (Process);

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

      if Process.Debuggee_Console /= null then
         Cleanup_TTY_If_Needed (Process.Debuggee_Console);
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
      Window        : constant GPS_Window :=
        GPS_Window (Process.Window);
      Buttons       : Message_Dialog_Buttons;
      pragma Unreferenced (Buttons);
      WTX_Version   : Natural := 0;
      Widget        : Gtk_Menu_Item;

   begin
      Set_Busy (Process, True);
      Attach_To_Debugger_Console (Process, Create_If_Necessary => True);
      Attach_To_Call_Stack
        (Process,
         Create_If_Necessary => Get_Pref (Show_Call_Stack));
      Attach_To_Data_Window (Process, Create_If_Necessary => False);
      Attach_To_Thread_Dialog (Process, Create_If_Necessary => False);
      Attach_To_Tasks_Dialog (Process, Create_If_Necessary => False);

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

      --  If we have a debuggee console in the desktop, always use it.
      --  Otherwise, we only create one when the user has asked for it

      Attach_To_Debuggee_Console
        (Process,
         Create_If_Necessary =>
           Get_Pref (Execution_Window)
           and then Support_TTY (Process.Debugger)
           and then GNAT.TTY.TTY_Supported);

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

   -----------------
   -- Get_Console --
   -----------------

   function Get_Console
     (Process : access Visual_Debugger_Record'Class)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Process.Debugger_Text);
   end Get_Console;

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
