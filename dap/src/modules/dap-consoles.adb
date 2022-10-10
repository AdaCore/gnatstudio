------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with Glib;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;

with Gtk.Box;
with Gtk.Clipboard;
with Gtk.Enums;
with Gtk.Menu;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;

with VSS.Strings.Conversions;

with GPS.Kernel.Actions;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;

with DAP.Module;
with DAP.Preferences;
with DAP.Tools;
with DAP.Requests.Evaluate;
with DAP.Views;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;

with Default_Preferences;        use Default_Preferences;
with Generic_Views;              use Generic_Views;
with Histories;                  use Histories;

package body DAP.Consoles is

   type Debugger_Console_Record is new DAP.Views.View_Record with
      record
         Console : Interactive_Console := null;
      end record;
   type Debugger_Console is access all Debugger_Console_Record'Class;

   function Initialize
     (Self : access Debugger_Console_Record'Class) return Gtk_Widget;
   --  Internal initialization function

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

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Debugger_Console_Record'Class;

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Debugger_Console_Record'Class := null);

   package Console_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Console_MDI_Views,
      Formal_View_Record => Debugger_Console_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

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

   -- Evaluate_Request --
   type Evaluate_Request is
     new DAP.Requests.Evaluate.Evaluate_DAP_Request
   with record
      Console : Debugger_Console;
   end record;
   type Evaluate_Request_Access is access all Evaluate_Request;
   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);
   overriding procedure On_Rejected (Self : in out Evaluate_Request);
   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String);

   --------------------------------
   -- Attach_To_Debugger_Console --
   --------------------------------

   procedure Attach_To_Debugger_Console
     (Client              : access DAP_Client'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean) is
   begin
      Console_Views.Attach_To_View
        (Client, Kernel, Create_If_Necessary, False);
   end Attach_To_Debugger_Console;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Debugger_Console_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu
        (Menu, View.Kernel, DAP.Preferences.Debugger_Console_Console);
      Append_Menu
        (Menu, View.Kernel, DAP.Preferences.Debugger_Console_Stdout);
   end Create_Menu;

   --------------------------------------
   -- Get_Debugger_Interactive_Console --
   --------------------------------------

   function Get_Debugger_Interactive_Console
     (Client : DAP_Client)
      return access Interactive_Console_Record'Class
   is
      C : constant Debugger_Console :=
        Debugger_Console (Client.Get_Debugger_Console);

   begin
      if C /= null then
         return C.Console;
      else
         return null;
      end if;
   end Get_Debugger_Interactive_Console;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access Debugger_Console_Record'Class is
   begin
      return Debugger_Console (Client.Get_Debugger_Console);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Client : not null access DAP.Clients.DAP_Client'Class;
      View   : access Debugger_Console_Record'Class := null)
   is
      Old : constant Debugger_Console :=
        Debugger_Console (Client.Get_Debugger_Console);
   begin
      if Old /= null then
         Old.On_Process_Terminated;
      end if;

      Client.Set_Debugger_Console (Generic_Views.Abstract_View_Access (View));
   end Set_View;

   -------------------------------
   -- Interpret_Command_Handler --
   -------------------------------

   function Interpret_Command_Handler
     (Console : access Interactive_Console_Record'Class;
      Input   : String;
      User    : System.Address) return String
   is
      DC    : constant Debugger_Console := Convert (User);
      Req   : Evaluate_Request_Access := new Evaluate_Request (Console.Kernel);
      Frame : constant Integer := DC.Get_Client.Get_Selected_Frame;
   begin
      Req.Console := DC;
      Req.Parameters.arguments.expression :=
        VSS.Strings.Conversions.To_Virtual_String (Input);
      if Frame /= 0 then
         Req.Parameters.arguments.frameId := (Is_Set => True, Value => Frame);
      end if;
      Req.Parameters.arguments.context :=
        (Is_Set => True, Value => DAP.Tools.Enum.repl);
      DC.Get_Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));

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

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Evaluate_Request;
      Result      : in out DAP.Tools.EvaluateResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);
   begin
      Self.Console.Console.Insert
        (VSS.Strings.Conversions.To_UTF_8_String (Result.a_body.result));
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Evaluate_Request) is
   begin
      Self.Console.Console.Insert ("The request is rejected");
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Evaluate_Request;
      Message : VSS.Strings.Virtual_String) is
   begin
      Self.Console.Console.Insert
        ("Error: " & VSS.Strings.Conversions.To_UTF_8_String (Message));
   end On_Error_Message;

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

      Self.Console.Set_Highlight_Color (Preference (Comments_Style));

      --  Self.Console.Set_Completion_Handler
      --    (Complete_Command'Access, Self.all'Address);

      Widget_Callback.Object_Connect
        (Self.Console.Get_View, Signal_Grab_Focus, On_Grab_Focus'Access,
         Self);

      Setup_Contextual_Menu
        (Kernel          => Self.Kernel,
         Event_On_Widget => Self.Console.Get_View);

      return Gtk_Widget (Self.Console.Get_View);
   end Initialize;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Console_Views.Register_Module (Kernel);

      GPS.Kernel.Actions.Register_Action
        (Kernel,
         "debug clear console", new Clear_Command,
         "Clear the debugger console",
         Icon_Name => "gps-clear-symbolic",
         Category  => "Debug");
   end Register_Module;

end DAP.Consoles;
