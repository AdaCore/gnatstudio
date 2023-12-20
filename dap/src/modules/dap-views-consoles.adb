------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Unchecked_Conversion;
with System;

with GNAT.Expect;                use GNAT.Expect;
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (Off, ".* is an internal GNAT unit");
with GNAT.Expect.TTY.Temporary;
pragma Warnings (On, ".* is an internal GNAT unit");
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regpat;                use GNAT.Regpat;
with GNAT.TTY;                   use GNAT.TTY;

with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;

with Gtk.Box;
with Gtk.Clipboard;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;

with GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;

with DAP.Module;
with DAP.Modules.Preferences;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;

with Default_Preferences;        use Default_Preferences;
with Generic_Views;              use Generic_Views;
with Histories;                  use Histories;
with Remote;                     use Remote;

package body DAP.Views.Consoles is

   Me : constant Trace_Handle := Create ("GPS.DAP.Consoles", On);

   type Console_Record is new DAP.Views.View_Record with
      record
         Console : Interactive_Console := null;
      end record;

   type Debugger_Console_Record is new Console_Record with null record;
   type Debugger_Console is access all Debugger_Console_Record'Class;

   function Initialize
     (Self : access Debugger_Console_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   overriding procedure On_Attach
     (Self   : not null access Debugger_Console_Record;
      Client : not null access DAP.Clients.DAP_Client'Class);
   --  Called when a new DAP client is spawned and attached to the debugger
   --  console.
   --  Used to reciprocally attach the debugger console to the DAP client.

   overriding procedure Create_Menu
     (View    : not null access Debugger_Console_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   package Console_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Debugger_Console",
      View_Name                       => "Debugger Console",
      Formal_View_Record              => Debugger_Console_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => False,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Areas                           => Gtkada.MDI.Sides_Only,
      Group                           => Group_Consoles,
      Position                        => Gtkada.MDI.Position_Bottom,
      Initialize                      => Initialize,
      Local_Toolbar                   => True,
      Local_Config                    => True);

   subtype Console_MDI is Console_MDI_Views.View_Access;
   use type Console_MDI;

   package Console_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Console_MDI_Views,
      Formal_View_Record => Debugger_Console_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record);

   function Key_Handler
     (Console  : access Interactive_Console_Record'Class;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
      Key      : Gdk.Types.Gdk_Key_Type := 0;
      Uni      : Glib.Gunichar := 0;
      User     : System.Address) return Boolean;
   --  Console key handler.

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Debugger_Console);

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      User    : System.Address) return String;
   --  Input command interpreter. Execute command and display output.

   procedure On_Grab_Focus (Console : access Gtk_Widget_Record'Class);
   --  Callback for the "grab_focus" signal on the console.

   type Clear_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Clear a console

   type No_Execution_Console_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_Execution_Console_Filter;
      Context : Selection_Context) return Boolean;
   --  True if Execution console doesn't exist

   -- Debuggee_Console_Record --

   type Debuggee_Console_Record is new Console_Record with
      record
         Debuggee_Descriptor : GNAT.Expect.TTY.TTY_Process_Descriptor;
         Debuggee_Id         : Glib.Main.G_Source_Id := 0;
         TTY_Initialized     : Boolean := False;
         Cleanup_TTY         : Boolean := False;
      end record;
   type Debuggee_Console is access all Debuggee_Console_Record'Class;

   function Initialize
     (Self    : access Debuggee_Console_Record'Class) return Gtk_Widget;
   --  Create each of the console types

   procedure Allocate_TTY (Console : access Debuggee_Console_Record'Class);
   procedure Close_TTY (Console : access Debuggee_Console_Record'Class);
   --  Allocate or close, if not done yet, a new tty on the console

   overriding procedure On_Attach
     (Console : access Debuggee_Console_Record;
      Client  : not null access DAP_Client'Class);
   --  Requires initialized when attaching the console to a process

   function Debuggee_Console_Handler
     (Console    : access Interactive_Console_Record'Class;
      Input      : String;
      Debuggee_C : System.Address) return String;
   --  Handler of I/O for the debuggee console.

   procedure On_Debuggee_Destroy (Console : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal on the debugee console

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Debuggee_Console);

   package Debuggee_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Debugger_Execution",
      View_Name                       => "Debugger Execution",
      Formal_View_Record              => Debuggee_Console_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => False,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Areas                           => Gtkada.MDI.Sides_Only,
      Group                           => Group_Consoles,
      Position                        => Gtkada.MDI.Position_Bottom,
      Initialize                      => Initialize);
   package Debuggee_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Debuggee_MDI_Views,
      Formal_View_Record => Debuggee_Console_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record);

   procedure Attach_To_Debuggee_Console
     (Client              : access DAP.Clients.DAP_Client'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean;
      Update_On_Attach    : Boolean;
      Name                : String);
   --  Attach to the console for the program that is debugged.

   Regexp_Any : constant Pattern_Matcher :=
     Compile (".+", Single_Line or Multiple_Lines);
   --  Any non empty string, as long as possible.

   package TTY_Timeout is new Glib.Main.Generic_Sources (Debuggee_Console);

   function TTY_Cb (Console : Debuggee_Console) return Boolean;
   --  Callback for communication with a tty.

   Timeout  : constant Guint := 50;
   --  Timeout between updates of the debuggee console

   Null_TTY : GNAT.TTY.TTY_Handle;

   type Open_Execution_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_Execution_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open execution console

   ------------------
   -- Allocate_TTY --
   ------------------

   procedure Allocate_TTY (Console : access Debuggee_Console_Record'Class) is
      Created : Boolean := False;
   begin
      if not Console.TTY_Initialized
        and then Console.Get_Client /= null
      then
         if Console.Get_Client.Get_Debuggee_TTY = Null_TTY then
            Console.Get_Client.Allocate_TTY;
            Created := True;
         end if;

         GNAT.Expect.TTY.Temporary.Initialize_Process
           (Console.Debuggee_Descriptor);
         Pseudo_Descriptor
           (Console.Debuggee_Descriptor,
            Console.Get_Client.Get_Debuggee_TTY,
            0);
         Console.Debuggee_Descriptor.Flush;

         Console.TTY_Initialized := True;
         Console.Debuggee_Id :=
           TTY_Timeout.Timeout_Add
             (Timeout, TTY_Cb'Access, Console.all'Access);

         if Created
           and then Console.Get_Client /= null
         then
            Console.Get_Client.Set_TTY
              (TTY_Name (Console.Get_Client.Get_Debuggee_TTY));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Allocate_TTY;

   procedure Attach_To_Debuggee_Console
     (Client              : access DAP.Clients.DAP_Client'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean;
      Update_On_Attach    : Boolean;
      Name                : String)
     renames Debuggee_Views.Attach_To_View;

   --------------------------------
   -- Attach_To_Debugger_Console --
   --------------------------------

   procedure Attach_To_Debugger_Console
     (Client              : access DAP_Client'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean;
      Name                : String) is
   begin
      Console_Views.Attach_To_View
        (Client              => Client,
         Kernel              => Kernel,
         Create_If_Necessary => Create_If_Necessary,
         Update_On_Attach    => False,
         Name                => Name);
   end Attach_To_Debugger_Console;

   ---------------
   -- Close_TTY --
   ---------------

   procedure Close_TTY (Console : access Debuggee_Console_Record'Class) is
   begin
      if Console.TTY_Initialized then
         if Console.Debuggee_Id /= 0 then
            Glib.Main.Remove (Console.Debuggee_Id);
            Console.Debuggee_Id := 0;
         end if;

         Close_Pseudo_Descriptor (Console.Debuggee_Descriptor);
         Console.TTY_Initialized := False;
      end if;
   end Close_TTY;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Self   : not null access Debugger_Console_Record;
      Client : not null access DAP.Clients.DAP_Client'Class) is
   begin
      Client.Set_Debugger_Console
        (Console => Generic_Views.Abstract_View_Access (Self));
   end On_Attach;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Debugger_Console_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu
        (Menu, View.Kernel, DAP.Modules.Preferences.Debugger_Console_Console);
   end Create_Menu;

   --------------------------------------
   -- Get_Debugger_Interactive_Console --
   --------------------------------------

   function Get_Debugger_Interactive_Console
     (Client : DAP_Client'Class) return Interactive_Console
   is
      View : constant Generic_Views.Abstract_View_Access :=
        Client.Get_Debugger_Console;

   begin
      if View /= null then
         return Debugger_Console (View).Console;
      else
         return null;
      end if;
   end Get_Debugger_Interactive_Console;

   --------------------------------------
   -- Get_Debuggee_Interactive_Console --
   --------------------------------------

   function Get_Debuggee_Interactive_Console
     (Client : DAP_Client'Class) return Interactive_Console
   is
      View : constant Generic_Views.Abstract_View_Access :=
        Client.Get_Debuggee_Console;
   begin
      if View /= null then
         return Debuggee_Console (View).Console;
      else
         return null;
      end if;
   end Get_Debuggee_Interactive_Console;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self    : access Debuggee_Console_Record'Class) return Gtk_Widget is
   begin
      Gtk.Box.Initialize_Vbox (Self);
      Gtk_New
        (Self.Console,
         Self.Kernel,
         Prompt      => "",
         Handler     => Debuggee_Console_Handler'Access,
         User_Data   => Self.all'Address,
         History_List => null,
         Key          => "dap_tty_console",
         ANSI_Support => True,
         Wrap_Mode    => Wrap_Char);
      Self.Pack_Start (Self.Console, Expand => True, Fill => True);

      Set_Font_And_Colors (Self.Console.Get_View, Fixed_Font => True);
      Widget_Callback.Object_Connect
        (Self.Console, Signal_Destroy, On_Debuggee_Destroy'Access, Self);

      return Gtk_Widget (Self);
   end Initialize;

   ------------
   -- TTY_Cb --
   ------------

   function TTY_Cb (Console : Debuggee_Console) return Boolean is
      Match : Expect_Match;
   begin
      if Console.Get_Client /= null then
         Expect (Console.Debuggee_Descriptor, Match, Regexp_Any, Timeout => 1);

         if Match /= Expect_Timeout then
            Console.Console.Insert
              (Expect_Out (Console.Debuggee_Descriptor), Add_LF => False);
            Find_MDI_Child (Get_MDI (Console.Kernel), Console).Highlight_Child;
         end if;
      end if;

      return True;

   exception
      when Process_Died =>
         Console.Console.Insert
           (Expect_Out (Console.Debuggee_Descriptor), Add_LF => False);
         Find_MDI_Child
           (Get_MDI (Console.Kernel), Console).Highlight_Child;

         --  Reset the TTY linking with the debugger and the console
         Console.Close_TTY;
         if Console.Get_Client.Get_Debuggee_TTY /= Null_TTY then
            Console.Get_Client.Close_TTY;
         end if;
         Console.Allocate_TTY;

         return False;
   end TTY_Cb;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      User    : System.Address) return String
   is
      pragma Unreferenced (Console);

      DC     : constant Debugger_Console := Convert (User);
      Client : constant DAP.Clients.DAP_Client_Access := DC.Get_Client;
   begin
      if Client = null then
         return "";
      end if;

      Client.Process_User_Command
        (Cmd => Input, Output_Command => False, Result_In_Console => True);

      return "";
   end Interpret_Command_Handler;

   -----------------
   -- Key_Handler --
   -----------------

   function Key_Handler
     (Console  : access Interactive_Console_Record'Class;
      Modifier : Gdk.Types.Gdk_Modifier_Type;
      Key      : Gdk.Types.Gdk_Key_Type := 0;
      Uni      : Glib.Gunichar := 0;
      User     : System.Address) return Boolean
   is
      pragma Unreferenced (Uni, User);

      Buffer      : constant Gtk_Text_Buffer := Console.Get_View.Get_Buffer;

      Last_Iter   : Gtk_Text_Iter;
      Cursor_Iter : Gtk_Text_Iter;
      Result      : Boolean;

   begin
      if (Modifier and Control_Mask) /= 0
        and then (Key = GDK_K or else Key = GDK_LC_k)
      then
         Buffer.Begin_User_Action;
         Buffer.Get_Iter_At_Mark (Cursor_Iter, Buffer.Get_Insert);
         Assign (Last_Iter, Cursor_Iter);
         Forward_To_Line_End (Last_Iter, Result);

         Buffer.Select_Range (Cursor_Iter, Last_Iter);
         Buffer.Cut_Clipboard (Gtk.Clipboard.Get, True);

         Buffer.End_User_Action;

         return True;
      else
         return False;
      end if;
   end Key_Handler;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Console : access Debuggee_Console_Record;
      Client  : not null access DAP_Client'Class)
   is
      pragma Unreferenced (Client);
   begin
      Console.Allocate_TTY;
   end On_Attach;

   -------------------------
   -- On_Debuggee_Destroy --
   -------------------------

   procedure On_Debuggee_Destroy (Console : access Gtk_Widget_Record'Class) is
   begin
      Debuggee_Console (Console).Close_TTY;
   end On_Debuggee_Destroy;

   -------------------
   -- On_Grab_Focus --
   -------------------

   procedure On_Grab_Focus (Console : access Gtk_Widget_Record'Class)
   is
      C : constant Debugger_Console := Debugger_Console (Console);
   begin
      if C.Get_Client /= null then
         DAP.Module.Set_Current_Debugger (C.Get_Client);
         String_History.Wind
           (C.Get_Client.Get_Command_History,
            String_History.Forward);
      end if;
   end On_Grab_Focus;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Console_MDI :=
        Console_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         View.Console.Clear;
         View.Console.Display_Prompt;
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Open_Execution_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Client : constant DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
   begin
      if Client /= null
        and then Client.Get_Debuggee_Console = null
      then
         Create_Execution_Console (Client);
      end if;

      return Success;
   end Execute;

   ------------------------------
   -- Create_Execution_Console --
   ------------------------------

   procedure Create_Execution_Console
     (Client : access DAP.Clients.DAP_Client'Class) is
   begin
      Attach_To_Debuggee_Console
        (Client,
         Client.Kernel,
         Name => " " & (+Base_Name (Client.Get_Executable)),
         Update_On_Attach    => True,
         Create_If_Necessary =>
           DAP.Modules.Preferences.Execution_Window.Get_Pref
         and then Is_Local (Remote.Debug_Server)
         and then GNAT.TTY.TTY_Supported);
   end Create_Execution_Console;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Debugger_Console_Record'Class) return Gtk_Widget is
   begin
      Gtk.Box.Initialize_Vbox (Self);
      Gtk_New
        (Self.Console,
         Self.Kernel,
         Handler             => Interpret_Command_Handler'Access,
         User_Data           => Self.all'Address,
         Prompt              => ">",
         History_List        => Get_History (Self.Kernel),
         Key                 => "dap_console",
         Wrap_Mode           => Gtk.Enums.Wrap_Char,
         ANSI_Support        => False,
         Empty_Equals_Repeat => True);
      Self.Console.Get_View.Set_Name ("Debugger_Console");
      Self.Console.Set_Key_Handler (Key_Handler'Access, System.Null_Address);
      Self.Pack_Start (Self.Console, Fill => True, Expand => True);
      Set_Font_And_Colors (Self.Console.Get_View, Fixed_Font => True);

      Set_Max_Length (Get_History (Self.Kernel).all, 100, "dap_console");
      Allow_Duplicates
        (Get_History (Self.Kernel).all, "dap_console", True, True);

      Widget_Callback.Object_Connect
        (Self.Console.Get_View, Signal_Grab_Focus, On_Grab_Focus'Access,
         Self);

      Setup_Contextual_Menu
        (Kernel          => Self.Kernel,
         Event_On_Widget => Self.Console.Get_View);

      return Gtk_Widget (Self.Console.Get_View);
   end Initialize;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access No_Execution_Console_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);

      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
   begin
      return Client /= null and then Client.Get_Debuggee_Console = null;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Debuggee_Console_Handler --
   ------------------------------

   function Debuggee_Console_Handler
     (Console    : access Interactive_Console_Record'Class;
      Input      : String;
      Debuggee_C : System.Address) return String
   is
      pragma Unreferenced (Console);
      C  : constant Debuggee_Console := Convert (Debuggee_C);
      NL : aliased Character := ASCII.LF;
      N  : Integer with Unreferenced;

   begin
      if C.Get_Client /= null
        and then C.Get_Client.Get_Debuggee_TTY /= Null_TTY
      then
         N := Write
           (TTY_Descriptor (C.Get_Client.Get_Debuggee_TTY),
            Input'Address, Input'Length);
         N := Write
           (TTY_Descriptor (C.Get_Client.Get_Debuggee_TTY), NL'Address, 1);
      end if;

      return "";
   end Debuggee_Console_Handler;

   ---------------------------------
   -- Display_In_Debugger_Console --
   ---------------------------------

   procedure Display_In_Debugger_Console
     (Client         : access DAP_Client'Class;
      Text           : String;
      Mode           : GPS.Kernel.Message_Type := Info;
      Add_To_History : Boolean := False)
   is
      Console : constant Interactive_Console :=
        Get_Debugger_Interactive_Console (Client.all);
   begin
      if Console /= null then
         Console.Insert
           (Text,
            Add_LF         => False,
            Mode           => Mode,
            Add_To_History => Add_To_History);
      end if;
   end Display_In_Debugger_Console;

   ---------------------------------
   -- Display_In_Debuggee_Console --
   ---------------------------------

   procedure Display_In_Debuggee_Console
     (Client : access DAP_Client'Class;
      Text   : String;
      Mode   : GPS.Kernel.Message_Type := Info)
   is
      View : constant Generic_Views.Abstract_View_Access :=
        Client.Get_Debuggee_Console;
   begin
      if View /= null then
         Debuggee_Console (View).Console.Insert
           (Text,
            Add_LF         => False,
            Mode           => Mode,
            Add_To_History => False);
      else
         Display_In_Debugger_Console
           (Client         => Client,
            Text           => Text,
            Mode           => Mode,
            Add_To_History => False);
      end if;
   end Display_In_Debuggee_Console;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter : Action_Filter;
   begin
      Console_Views.Register_Module (Kernel);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug clear console", new Clear_Command,
         "Clear the debugger console",
         Icon_Name => "gps-clear-symbolic",
         Category  => "Debug");

      Debuggee_Views.Register_Module (Kernel);

      Filter := new No_Execution_Console_Filter;
      Kernel.Register_Filter (Filter, "No Execution console");

      if GNAT.TTY.TTY_Supported or else
        GNATCOLL.Traces.Active (GPS.Kernel.Menu_Generation_Handle)
      then
         GPS.Kernel.Actions.Register_Action
           (Kernel, "open debugger execution",
            Command     => new Open_Execution_Command,
            Description => "Open the Debugger Execution console",
            Filter      => Filter,
            Category    => "Debug");
      end if;
   end Register_Module;

end DAP.Views.Consoles;
