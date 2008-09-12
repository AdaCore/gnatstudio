-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2008, AdaCore              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System;                 use System;

with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Gdk.Event;              use Gdk.Event;
with Gtk.Enums;              use Gtk.Enums;
with Gtk.Main;               use Gtk.Main;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Menu_Item;          use Gtk.Menu_Item;
with Gtk.Object;             use Gtk.Object;
with Gtk.Text_View;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Handlers;        use Gtkada.Handlers;
with Gtkada.MDI;             use Gtkada.MDI;

pragma Warnings (Off);
with GNAT.Expect.TTY;        use GNAT.Expect.TTY;
with GNAT.TTY;               use GNAT.TTY;
pragma Warnings (On);
with GNAT.Expect;            use GNAT.Expect;
with GNAT.Regpat;            use GNAT.Regpat;
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Utils;         use GNATCOLL.Utils;

with Debugger;               use Debugger;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with GPS.Intl;               use GPS.Intl;
with GPS.Main_Window.Debug;  use GPS.Main_Window.Debug;
with GVD.Preferences;        use GVD.Preferences;
with GVD.Process;            use GVD.Process;
with GVD.Types;              use GVD.Types;
with GVD.Views;              use GVD.Views;
with GVD_Module;             use GVD_Module;
with Histories;              use Histories;
with Interactive_Consoles;   use Interactive_Consoles;
with Pango.Font;             use Pango.Font;
with Process_Proxies;        use Process_Proxies;
with String_List_Utils;      use String_List_Utils;
with Traces;                 use Traces;

package body GVD.Consoles is

   Regexp_Any : constant Pattern_Matcher :=
     Compile (".+", Single_Line or Multiple_Lines);
   --  Any non empty string, as long as possible.

   Timeout       : constant Guint32 := 50;
   --  Timeout between updates of the debuggee console

   type Debugger_Console_Record is new Console_Views.Process_View_Record with
      record
         Contextual_Menu : Gtk_Menu;
      end record;
   type Debugger_Console is access all Debugger_Console_Record'Class;

   type Debuggee_Console_Record is new Console_Views.Process_View_Record with
      record
         Debuggee_TTY        : GNAT.TTY.TTY_Handle;
         Debuggee_Descriptor : GNAT.Expect.TTY.TTY_Process_Descriptor;
         Debuggee_Id         : Gtk.Main.Timeout_Handler_Id := 0;
         TTY_Initialized     : Boolean := False;
         Cleanup_TTY         : Boolean := False;
      end record;
   type Debuggee_Console is access all Debuggee_Console_Record'Class;

   package TTY_Timeout is new Gtk.Main.Timeout (Debuggee_Console);

   procedure Initialize
     (Console : access Debugger_Console_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class);
   procedure Initialize
     (Console : access Debuggee_Console_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Create each of the console types

   procedure Allocate_TTY (Console : access Debuggee_Console_Record'Class);
   procedure Close_TTY (Console : access Debuggee_Console_Record'Class);
   --  Allocate or close, if not done yet, a new tty on the console

   overriding procedure On_Attach
     (Console : access Debuggee_Console_Record;
      Process : access Visual_Debugger_Record'Class);
   --  Requires initialized when attaching the console to a process

   function Get_Debugger_Console
     (Process : access Visual_Debugger_Record'Class)
      return Interactive_Console;
   function Get_Debuggee_Console
     (Process : access Visual_Debugger_Record'Class)
      return Interactive_Console;
   procedure Set_Debugger_Console
     (Process : access Visual_Debugger_Record'Class;
      Console : Interactive_Console);
   procedure Set_Debuggee_Console
     (Process : access Visual_Debugger_Record'Class;
      Console : Interactive_Console);
   --  Get or set the consoles from the process

   package Debugger_Views is new Console_Views.Simple_Views
     (Module_Name        => "Debugger_Console",
      View_Name          => -"Debugger Console",
      Formal_View_Record => Debugger_Console_Record,
      Get_View           => Get_Debugger_Console,
      Set_View           => Set_Debugger_Console,
      Group              => Group_Consoles,
      Position           => Position_Bottom,
      Initialize         => Initialize);
   package Debuggee_Views is new Console_Views.Simple_Views
     (Module_Name        => "Debugger_Execution",
      View_Name          => -"Debugger Execution",
      Formal_View_Record => Debuggee_Console_Record,
      Get_View           => Get_Debuggee_Console,
      Set_View           => Set_Debuggee_Console,
      Group              => Group_Consoles,
      Position           => Position_Bottom,
      Initialize         => Initialize);

   function Complete_Command
     (Input     : String;
      View      : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      User_Data : System.Address)
      return String_List_Utils.String_List.List;
   --  Return the list of completions for Input.

   procedure On_Debuggee_Destroy (Console : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal on the debugee console

   function TTY_Cb (Console : Debuggee_Console) return Boolean;
   --  Callback for communication with a tty.

   function Interpret_Command_Handler
     (Console            : access Interactive_Console_Record'Class;
      Input              : String;
      Console_User_Data  : System.Address) return String;
   --  Launch the command interpreter for Input and return the output.

   function Debuggee_Console_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      Object  : System.Address) return String;
   --  Handler of I/O for the debuggee console.

   procedure On_Grab_Focus (Console : access Gtk_Widget_Record'Class);
   --  Callback for the "grab_focus" signal on the console.

   procedure Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu);
   --  Create the context for the contextual menus

   --------------------------
   -- Get_Debugger_Console --
   --------------------------

   function Get_Debugger_Console
     (Process : access Visual_Debugger_Record'Class)
      return Interactive_Console is
   begin
      return Process.Debugger_Text;
   end Get_Debugger_Console;

   --------------------------
   -- Get_Debuggee_Console --
   --------------------------

   function Get_Debuggee_Console
     (Process : access Visual_Debugger_Record'Class)
      return Interactive_Console is
   begin
      return Process.Debuggee_Console;
   end Get_Debuggee_Console;

   --------------------------
   -- Set_Debugger_Console --
   --------------------------

   procedure Set_Debugger_Console
     (Process : access Visual_Debugger_Record'Class;
      Console : Interactive_Console) is
   begin
      --  If we are detaching, clear the old view
      if Console = null
        and then Process.Debugger_Text /= null
      then
         Clear (Process.Debugger_Text);
      end if;

      Process.Debugger_Text := Console;
   end Set_Debugger_Console;

   --------------------------
   -- Set_Debuggee_Console --
   --------------------------

   procedure Set_Debuggee_Console
     (Process : access Visual_Debugger_Record'Class;
      Console : Interactive_Console) is
   begin
      --  If we are detaching, clear the old view
      if Console = null
        and then Process.Debuggee_Console /= null
      then
         Close_TTY (Debuggee_Console (Process.Debuggee_Console));
         Clear (Process.Debuggee_Console);
      end if;

      Process.Debuggee_Console := Console;
   end Set_Debuggee_Console;

   ------------
   -- TTY_Cb --
   ------------

   function TTY_Cb (Console : Debuggee_Console) return Boolean is
      Match  : Expect_Match;
   begin
      if Get_Process (Console) /= null then
         Expect (Console.Debuggee_Descriptor, Match, Regexp_Any, Timeout => 1);

         if Match /= Expect_Timeout then
            Insert (Console,
                    Expect_Out (Console.Debuggee_Descriptor),
                    Add_LF => False);
            Highlight_Child
              (Find_MDI_Child (Get_Process (Console).Window.MDI, Console));
         end if;
      end if;

      return True;

   exception
      when Process_Died =>
         Insert (Console,
                 Expect_Out (Console.Debuggee_Descriptor),
                 Add_LF => False);
         Highlight_Child
           (Find_MDI_Child (Get_Process (Console).Window.MDI, Console));

         --  Reset the TTY linking with the debugger and the console
         Close_TTY (Console);
         Allocate_TTY (Console);

         return True;
   end TTY_Cb;

   ---------------------
   -- Context_Factory --
   ---------------------

   procedure Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Kernel, Event_Widget, Event, Object, Context);
      Mitem : Gtk_Menu_Item;
   begin
      if Menu /= null then
         Gtk_New (Mitem, Label => -"Info");
         Set_State (Mitem, State_Insensitive);
         Append (Menu, Mitem);
      end if;
   end Context_Factory;

   -------------------------
   -- On_Debuggee_Destroy --
   -------------------------

   procedure On_Debuggee_Destroy (Console : access Gtk_Widget_Record'Class) is
   begin
      Close_TTY (Debuggee_Console (Console));
   end On_Debuggee_Destroy;

   -------------------
   -- On_Grab_Focus --
   -------------------

   procedure On_Grab_Focus (Console : access Gtk_Widget_Record'Class)
   is
      C : constant Debugger_Console := Debugger_Console (Console);
   begin
      if Get_Process (C) /= null then
         Switch_Debugger (Get_Process (C).Window, GObject (Get_Process (C)));
         String_History.Wind
           (Get_Process (C).Command_History, String_History.Forward);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Grab_Focus;

   ----------------------
   -- Complete_Command --
   ----------------------

   function Complete_Command
     (Input   : String;
      View    : access Gtk.Text_View.Gtk_Text_View_Record'Class;
      User_Data : System.Address)
      return String_List_Utils.String_List.List
   is
      pragma Unreferenced (User_Data);
      use String_List_Utils.String_List;
      C     : constant Debugger_Console := Debugger_Console (From_View (View));
      Result : List;
   begin
      if Get_Process (C) = null then
         return String_List_Utils.String_List.Null_List;

      elsif not
        Command_In_Process (Get_Process (Get_Process (C).Debugger))
      then
         --  Do not launch completion if the last character is ' ', as that
         --  might result in an output of several thousand entries, which
         --  will take a long time to parse.
         --  Do not complete either when the last character is '\' as it will
         --  hang GPS.
         if Input /= ""
           and then Input (Input'Last) /= ' '
           and then Input (Input'Last) /= '\'
         then
            declare
               S : GNAT.Strings.String_List :=
                     Complete (Get_Process (C).Debugger, Input);
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
     (Console           : access Interactive_Console_Record'Class;
      Input             : String;
      Console_User_Data : System.Address) return String
   is
      pragma Unreferenced (Console_User_Data);
      C   : constant Debugger_Console := Debugger_Console (Console);
   begin
      if Get_Process (C) /= null then
         Get_Process (C).Interactive_Command := True;
         Process_User_Command (Get_Process (C), Input, Mode => User);
      end if;

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
      pragma Unreferenced (Object);
      C   : constant Debuggee_Console := Debuggee_Console (Console);
      NL  : aliased Character := ASCII.LF;
      N   : Integer;
      pragma Unreferenced (N);

   begin
      if Get_Process (C) /= null then
         N := Write
           (TTY_Descriptor (C.Debuggee_TTY), Input'Address, Input'Length);
         N := Write (TTY_Descriptor (C.Debuggee_TTY), NL'Address, 1);
      end if;

      return "";
   end Debuggee_Console_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console : access Debugger_Console_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class) is
   begin
      Initialize
        (Console,
         Handler             => Interpret_Command_Handler'Access,
         User_Data           => Console.all'Address,
         Prompt              => "",
         Font                => Default_Style.Get_Pref_Font,
         History_List        => Get_History (Kernel),
         Key                 => "gvd_console",
         Wrap_Mode           => Wrap_Char,
         Empty_Equals_Repeat => True);

      Set_Max_Length (Get_History (Kernel).all, 100, "gvd_console");
      Allow_Duplicates
        (Get_History (Kernel).all, "gvd_console", True, True);

      Set_Highlight_Color    (Console, Debugger_Highlight_Color.Get_Pref);
      Set_Completion_Handler
        (Console, Complete_Command'Access, System.Null_Address);
      Widget_Callback.Object_Connect
        (Get_View (Console), Signal_Grab_Focus, On_Grab_Focus'Access, Console);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Get_View (Console),
         Object          => Console,
         ID              => Debugger_Module_ID,
         Context_Func    => Context_Factory'Access);
   end Initialize;

   ------------------
   -- Allocate_TTY --
   ------------------

   procedure Allocate_TTY (Console : access Debuggee_Console_Record'Class) is
   begin
      if not Console.TTY_Initialized then
         Allocate_TTY (Console.Debuggee_TTY);
         Pseudo_Descriptor
           (Console.Debuggee_Descriptor, Console.Debuggee_TTY, 0);
         Flush (Console.Debuggee_Descriptor);

         Console.TTY_Initialized := True;
         Console.Debuggee_Id :=
           TTY_Timeout.Add (Timeout, TTY_Cb'Access, Console.all'Access);

         if Get_Process (Console) /= null
           and then Get_Process (Console).Debugger /= null
         then
            Set_TTY
              (Get_Process (Console).Debugger,
               TTY_Name (Console.Debuggee_TTY));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
   end Allocate_TTY;

   ---------------
   -- Close_TTY --
   ---------------

   procedure Close_TTY (Console : access Debuggee_Console_Record'Class) is
   begin
      if Console.TTY_Initialized then
         if Console.Debuggee_Id /= 0 then
            Gtk.Main.Timeout_Remove (Console.Debuggee_Id);
            Console.Debuggee_Id := 0;
         end if;

         if Get_Process (Console) /= null
           and then Get_Process (Console).Debugger /= null
         then
            Set_TTY (Get_Process (Console).Debugger,  "");
         end if;

         Close_TTY (Console.Debuggee_TTY);
         Close_Pseudo_Descriptor (Console.Debuggee_Descriptor);
         Console.TTY_Initialized := False;
      end if;
   end Close_TTY;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Console : access Debuggee_Console_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Initialize
        (Console,
         Prompt      => "",
         Handler     => Debuggee_Console_Handler'Access,
         User_Data   => Console.all'Address,
         Font         => Default_Style.Get_Pref_Font,
         History_List => null,
         Key          => "gvd_tty_console",
         Wrap_Mode    => Wrap_Char);
      Widget_Callback.Connect
        (Console, Signal_Destroy, On_Debuggee_Destroy'Access);
   end Initialize;

   --------------------------------
   -- Attach_To_Debugger_Console --
   --------------------------------

   procedure Attach_To_Debugger_Console
     (Debugger            : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
     renames Debugger_Views.Attach_To_View;

   procedure Attach_To_Debuggee_Console
     (Debugger            : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean)
     renames Debuggee_Views.Attach_To_View;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Console : access Debuggee_Console_Record;
      Process : access Visual_Debugger_Record'Class)
   is
      pragma Unreferenced (Process);
   begin
      Allocate_TTY (Console);
   end On_Attach;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Debugger_Views.Register_Desktop_Functions (Kernel);
      Debuggee_Views.Register_Desktop_Functions (Kernel);
   end Register_Module;
end GVD.Consoles;
