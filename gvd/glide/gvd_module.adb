-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gdk.Color;               use Gdk.Color;
with Gdk.Pixmap;              use Gdk.Pixmap;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gdk.Window;              use Gdk.Window;
with Gtk.Bin;                 use Gtk.Bin;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Container;           use Gtk.Container;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Label;               use Gtk.Label;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Pixmap;              use Gtk.Pixmap;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Table;               use Gtk.Table;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.File_Selector;    use Gtkada.File_Selector;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;
with Factory_Data;            use Factory_Data;

with Display_Items;             use Display_Items;
with Items;                     use Items;
with GVD.Code_Editors;          use GVD.Code_Editors;
with GVD.Call_Stack;            use GVD.Call_Stack;
with GVD.Dialogs;               use GVD.Dialogs;
with GVD.Main_Window;           use GVD.Main_Window;
with GVD.Memory_View;           use GVD.Memory_View;
with GVD.Menu;                  use GVD.Menu;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Text_Box.Asm_Editor;   use GVD.Text_Box.Asm_Editor;
with GVD.Types;                 use GVD.Types;
with GVD.Toolbar;               use GVD.Toolbar;
with GVD.Process;               use GVD.Process;
with Process_Proxies;           use Process_Proxies;
with Debugger;                  use Debugger;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Basic_Types;               use Basic_Types;
with GUI_Utils;                 use GUI_Utils;
with Projects;                  use Projects;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;
with GNAT.OS_Lib;

with Glide_Main_Window;         use Glide_Main_Window;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Glide_Intl;                use Glide_Intl;
with Pixmaps_IDE;               use Pixmaps_IDE;
with Traces;                    use Traces;
with GUI_Utils;                 use GUI_Utils;
with String_Utils;              use String_Utils;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with Generic_List;
with Debugger_Pixmaps;          use Debugger_Pixmaps;

with Commands;                  use Commands;
with Commands.Debugger;         use Commands.Debugger;

with Commands.Generic_Asynchronous;

with GVD.Text_Box.Source_Editor; use GVD.Text_Box.Source_Editor;
with GVD.Text_Box.Source_Editor.Glide;
use  GVD.Text_Box.Source_Editor.Glide;

with Interactive_Consoles;      use Interactive_Consoles;

package body GVD_Module is

   Me : constant Debug_Handle := Create ("Debugger");

   Max_Tooltip_Width : constant := 400;
   --  Maximum size to use for the tooltip windows

   type GPS_Debugger_Record is new
     GVD.Process.Visual_Debugger_Record with null record;
   type GPS_Debugger is access all GPS_Debugger_Record'Class;

   type Bp_Array is array (Integer range <>) of Breakpoint_Identifier;

   procedure Gtk_New
     (Debugger : out GPS_Debugger;
      Window   : access Glide_Window_Record'Class);

   procedure Set_Busy
     (Debugger      : access GPS_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False);

   type File_Line_Record is record
      File : String_Access;
      Line : Integer;
   end record;

   procedure Free (X : in out File_Line_Record);
   --  Free memory associated to X.

   package File_Line_List is new Generic_List (File_Line_Record);

   type GPS_Proxy is new Process_Proxy with record
      Kernel : Kernel_Handle;
   end record;
   --  GPS specific proxy, used to redefine Set_Command_In_Process

   procedure Set_Command_In_Process
     (Proxy      : access GPS_Proxy;
      In_Process : Boolean := True);
   --  Set the appropriate debugger menu items to the corresponding state.

   type GVD_Module_Record is new Module_ID_Record with record
      Kernel           : Kernel_Handle;

      Initialized      : Boolean := False;
      --  Whether the debugger is running;

      Unexplored_Lines : File_Line_List.List := File_Line_List.Null_List;
      --  The list of lines which are currently revealed in the editor
      --  but the status of which has not yet been queried from the debugger.

      Total_Unexplored_Lines : Integer := 0;
      Total_Explored_Lines   : Integer := 0;

      Slow_Query       : Boolean := False;
      --  Set to True when the interval between two debugger queries should
      --  be long (for example when the debugger was detected to be busy).

      List_Modified    : Boolean := False;
      --  Set to True when the list has been modified by a callback.

      Show_Lines_With_Code : Boolean;
      --  Whether the lines with code should be explicitly queried.

      Initialize_Menu  : Gtk_Menu;

      Delete_Id         : Handler_Id := (Null_Signal_Id, null);
      File_Edited_Id    : Handler_Id := (Null_Signal_Id, null);
      Lines_Revealed_Id : Handler_Id := (Null_Signal_Id, null);
   end record;
   type GVD_Module is access all GVD_Module_Record'Class;

   procedure Free_Line_Information (Id : in out GVD_Module);
   --  Free the line information in Id.

   procedure Line_Information_Compute
     (Id      : in out GVD_Module;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Query the line information from the debugger.

   package Reveal_Lines_Commands is new Commands.Generic_Asynchronous
     (Data_Type   => GVD_Module,
      Description => -"Querying line information",
      Free        => Free_Line_Information,
      Iterate     => Line_Information_Compute);
   --  Package that handles commands related to line breakpoints query.

   procedure Destroy (Id : in out GVD_Module_Record);
   --  Terminate the debugger the module, and kill the underlying debugger.

   procedure GVD_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Generate the contextual menu entries for contextual menus in other
   --  modules than the debugger.

   procedure Tooltip_Handler
     (Sel_Context : access Selection_Context'Class;
      Pixmap      : out Gdk.Gdk_Pixmap;
      Width       : out Gint;
      Height      : out Gint);
   --  Create a pixmap suitable for a tooltip, if debugger has been initialized

   type Debugger_State is (Debug_None, Debug_Busy, Debug_Available);
   --  Possible states of a debugger:
   --  - Debug_None: debugger is not running
   --  - Debug_Busy: debugger is busy processing a command
   --  - Debug_Available: debugger is available

   procedure Set_Sensitive
     (Kernel : Kernel_Handle;
      State  : Debugger_State);
   --  Change the sensitive state of the debugger menu items and toolbar
   --  buttons

   generic
      with procedure Debug_Command
        (Object : Data_Type_Access;
         Action : Glib.Guint;
         Widget : Limited_Widget);
   procedure Generic_Debug_Command
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Generic procedure used for most debugger callbacks.

   procedure On_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   procedure Create_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : String);
   --  Create the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, create them for all files.

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : String);
   --  Remove the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, remove them for all files.

   procedure Preferences_Changed
     (Kernel : access GObject_Record'Class; User : GObject);
   --  Called when the preferences are changed in the GPS kernel

   function Delete_Asm
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Callback for the "delete_event" signal of the assembly view.
   --  Widget is a Code_Editor.

   procedure Debug_Terminate (Kernel : Kernel_Handle);
   --  Terminate the debugging session

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Debugger : out GPS_Debugger;
      Window   : access Glide_Window_Record'Class)
   is
      Edit : Glide.GEdit;
   begin
      Debugger := new GPS_Debugger_Record;

      Glide.Gtk_New (Edit, Window);
      GVD.Process.Initialize (Debugger, Window, Source_Editor (Edit));
   end Gtk_New;

   --------------
   -- Set_Busy --
   --------------

   procedure Set_Busy
     (Debugger      : access GPS_Debugger_Record;
      Busy          : Boolean := True;
      Force_Refresh : Boolean := False)
   is
      pragma Unreferenced (Force_Refresh);
   begin
      if Busy then
         Push_State (Glide_Window (Debugger.Window).Kernel, Processing);
      else
         Pop_State (Glide_Window (Debugger.Window).Kernel);
      end if;
   end Set_Busy;

   ---------------------------
   -- Generic_Debug_Command --
   ---------------------------

   procedure Generic_Debug_Command
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);
      use Debugger;

   begin
      if Process /= null and then Process.Debugger /= null then
         Debug_Command (Top.all'Access, 0, Null_Widget);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Generic_Debug_Command;

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Debug->Initialize

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Connect to Board

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Load File

   procedure On_Add_Symbols
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Add Symbols

   procedure On_Load_Core
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Debug Core File

   procedure On_Attach is new
     Generic_Debug_Command (GVD.Menu.On_Attach_To_Process);
   --  Debug->Debug->Attach

   procedure On_Detach is new
     Generic_Debug_Command (GVD.Menu.On_Detach_Process);
   --  Debug->Debug->Detach

   procedure On_Kill is new
     Generic_Debug_Command (GVD.Menu.On_Kill);
   --  Debug->Debug->Kill

   --  ??? procedure On_Session_Open is new
   --    Generic_Debug_Command (GVD.Menu.On_Open_Session);
   --  Debug->Session->Open

   --  ??? procedure On_Session_Save is new
   --    Generic_Debug_Command (GVD.Menu.On_Save_Session_As);
   --  Debug->Session->Save As

   procedure On_Command_History is new
     Generic_Debug_Command (GVD.Menu.On_Command_History);
   --  Debug->Session->Command History

   procedure On_Call_Stack is new
     Generic_Debug_Command (GVD.Menu.On_Call_Stack);
   --  Debug->Data->Call Stack

   procedure On_Threads is new Generic_Debug_Command (GVD.Menu.On_Threads);
   --  Debug->Data->Threads

   procedure On_Tasks is new Generic_Debug_Command (GVD.Menu.On_Tasks);
   --  Debug->Data->Tasks

   procedure On_PD is new Generic_Debug_Command (GVD.Menu.On_PD);
   --  Debug->Data->Protection Domains

   procedure On_Edit_Breakpoints is new
     Generic_Debug_Command (GVD.Menu.On_Edit_Breakpoints);
   --  Debug->Data->Edit Breakpoints

   procedure On_Examine_Memory is new
     Generic_Debug_Command (GVD.Menu.On_Examine_Memory);
   --  Debug->Data->Examine Memory

   procedure On_Display_Locals is new
     Generic_Debug_Command (GVD.Menu.On_Display_Local_Variables);
   --  Debug->Data->Display Local Variables

   procedure On_Display_Args is new
     Generic_Debug_Command (GVD.Menu.On_Display_Arguments);
   --  Debug->Data->Display Arguments

   procedure On_Display_Regs is new
     Generic_Debug_Command (GVD.Menu.On_Display_Registers);
   --  Debug->Data->Display Registers

   procedure On_Display_Expression is new
     Generic_Debug_Command (GVD.Menu.On_Display_Expression);
   --  Debug->Data->Display Any Expression

   procedure On_Data_Refresh is new
     Generic_Debug_Command (GVD.Menu.On_Refresh);
   --  Debug->Data->Refresh

   procedure On_Start is new
     Generic_Debug_Command (GVD.Menu.On_Run);
   --  Debug->On_Start menu

   procedure On_Step is new
     Generic_Debug_Command (GVD.Menu.On_Step);
   --  Debug->Step menu

   procedure On_Step_Instruction is new
     Generic_Debug_Command (GVD.Menu.On_Step_Instruction);
   --  Debug->Step Instruction menu

   procedure On_Next is new
     Generic_Debug_Command (GVD.Menu.On_Next);
   --  Debug->Next menu

   procedure On_Next_Instruction is new
     Generic_Debug_Command (GVD.Menu.On_Next_Instruction);
   --  Debug->Next Instruction menu

   procedure On_Finish is new
     Generic_Debug_Command (GVD.Menu.On_Finish);
   --  Debug->Finish menu

   procedure On_Continue is new
     Generic_Debug_Command (GVD.Menu.On_Continue);
   --  Debug->Continue menu

   procedure On_Interrupt is new
     Generic_Debug_Command (GVD.Menu.On_Interrupt);
   --  Debug->Interrupt

   procedure On_Debug_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Terminate

   -------------------------------
   -- Contextual Menu Callbacks --
   -------------------------------

   procedure Set_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Set a breakpoint on a specific line.

   procedure Set_Subprogram_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Set a breakpoint at the beginning of a specified subprogram.

   procedure Till_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Set a temporary breakpoint on a line, and continue execution.

   procedure Print_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "print" contextual menu items.

   procedure Graph_Display_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "display" contextual menu items.

   procedure View_Into_Memory
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "view memory at address of" contextual menu item.

   procedure Set_Value
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "set value of" contextual menu item.

   procedure Show_Current_Line_Menu
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Display the current file and current line in the editor.

   -----------------------
   -- Toolbar Callbacks --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "start/continue" button

   --------------------
   -- Misc Callbacks --
   --------------------

   procedure On_Destroy_Window (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal to clean up the debugger.

   procedure On_Executable_Changed (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "executable_changed" signal on the debugger process.

   procedure File_Edited_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Args    : GValues);
   --  Callback for the "file_edited" signal.

   procedure Lines_Revealed_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Args    : GValues);
   --  Callback for the "lines_revealed" signal.

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   procedure Initialize_Debugger
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      On_Debug_Init (Kernel, (0, Get_Project (Kernel), ""));
   end Initialize_Debugger;

   -----------------------------
   -- Create_Debugger_Columns --
   -----------------------------

   procedure Create_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : String) is
   begin
      --  Create the information column for the current line.
      Create_Line_Information_Column
        (Kernel,
         File,
         "Current Line",
         --  ??? That should be centralized somewhere !!!
         Stick_To_Data => True,
         Every_Line    => False);

      --  Create the information column for the breakpoints
      Create_Line_Information_Column
        (Kernel,
         File,
         Breakpoints_Column_Id,
         Stick_To_Data => True,
         Every_Line    => True);
   end Create_Debugger_Columns;

   -----------------------------
   -- Remove_Debugger_Columns --
   -----------------------------

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : String) is
   begin
      Remove_Line_Information_Column (Kernel, File, "Current Line");
      Remove_Line_Information_Column
        (Kernel, File, Breakpoints_Column_Id);
   end Remove_Debugger_Columns;

   ----------------------------
   -- Set_Command_In_Process --
   ----------------------------

   procedure Set_Command_In_Process
     (Proxy      : access GPS_Proxy;
      In_Process : Boolean := True) is
   begin
      Set_Command_In_Process (Process_Proxy (Proxy.all)'Access, In_Process);

      if In_Process then
         Set_Sensitive (Proxy.Kernel, Debug_Busy);
      else
         Set_Sensitive (Proxy.Kernel, Debug_Available);
      end if;
   end Set_Command_In_Process;

   --------------------
   -- On_Add_Symbols --
   --------------------

   procedure On_Add_Symbols
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);
      use Debugger;

   begin
      declare
         S : constant String :=
           Select_File
             (Title             => -"Select Module",
              Parent            => Gtk_Window (Top),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if S = "" then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else GNAT.OS_Lib.Is_Regular_File (S)
         then
            Add_Symbols (Process.Debugger, S, Mode => GVD.Types.Visible);
         else
            Console.Insert (Kernel, (-"Could not find file: ") & S,
                            Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Add_Symbols;

   ----------------
   -- Delete_Asm --
   ----------------

   function Delete_Asm
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      Data_Sub : constant String := '/' & (-"Debug") & '/' & (-"Data") & '/';
      Id       : constant GVD_Module  := GVD_Module (GVD_Module_ID);
      Editor   : constant Code_Editor := Code_Editor (Widget);
      Asm      : constant Asm_Editor  := Get_Asm (Editor);

   begin
      Set_Sensitive
        (Find_Menu_Item (Id.Kernel, Data_Sub & (-"Assembly")), True);
      Ref (Asm);
      Remove (Gtk_Container (Get_Parent (Asm)), Asm);
      Set_Mode (Editor, Source);
      Disconnect (Asm, Id.Delete_Id);

      return False;
   end Delete_Asm;

   -----------------
   -- On_Assembly --
   -----------------

   procedure On_Assembly
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Data_Sub : constant String := '/' & (-"Debug") & '/' & (-"Data") & '/';
      Top      : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process  : constant Visual_Debugger := Get_Current_Process (Top);
      Editor   : constant Code_Editor  := Process.Editor_Text;
      Address  : constant String       := Get_Asm_Address (Editor);
      Assembly : constant Asm_Editor   := Get_Asm (Editor);
      Child    : MDI_Child;

   begin
      if Get_Mode (Editor) = Source_Asm then
         return;
      end if;

      Set_Sensitive (Find_Menu_Item (Kernel, Data_Sub & (-"Assembly")), False);
      GVD_Module (GVD_Module_ID).Delete_Id :=
        Gtkada.Handlers.Return_Callback.Object_Connect
          (Assembly, "delete_event",
           Gtkada.Handlers.Return_Callback.To_Marshaller (Delete_Asm'Access),
           Editor);

      Set_Mode (Editor, Source_Asm);
      Child := Put (Kernel, Assembly, Module => GVD_Module_ID);
      Set_Focus_Child (Child);
      Set_Dock_Side (Child, Right);
      Dock_Child (Child);
      Unref (Assembly);
      Set_Title (Child, -"Assembly View");

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         if Address /= "" then
            Set_Address (Assembly, Address);
         end if;

         Highlight_Address_Range (Assembly, Get_Line (Editor));

         if Process.Breakpoints /= null then
            Update_Breakpoints (Assembly, Process.Breakpoints.all);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Assembly;

   -------------------------
   -- On_Connect_To_Board --
   -------------------------

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process      : constant Visual_Debugger :=
        Get_Current_Process (Top);
      Dialog       : Gtk_Dialog;
      Table        : Gtk_Table;
      Ent_Protocol : Gtk_Entry;
      Ent_Target   : Gtk_Entry;
      Label        : Gtk_Label;
      Button       : Gtk_Widget;
      pragma Unreferenced (Widget, Button);

      use Debugger;

   begin
      Gtk_New
        (Dialog,
         Title  => -"Connect to board",
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Position (Dialog, Win_Pos_Mouse);
      Set_Default_Size (Dialog, 300, 100);

      Gtk_New (Table, 2, 2, False);
      Pack_Start (Get_Vbox (Dialog), Table, Expand => False);

      Gtk_New (Label, -"Target name:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 0, 1, Xpadding => 3, Ypadding => 3);

      Gtk_New (Ent_Target);
      Set_Width_Chars (Ent_Target, 20);
      Attach (Table, Ent_Target, 1, 2, 0, 1);
      Grab_Focus (Ent_Target);
      Set_Activates_Default (Ent_Target, True);

      Gtk_New (Label, -"Protocol:");
      Set_Alignment (Label, 0.0, 0.0);
      Attach (Table, Label, 0, 1, 1, 2, Xpadding => 3, Ypadding => 3);

      Gtk_New (Ent_Protocol);
      Set_Width_Chars (Ent_Protocol, 20);
      Attach (Table, Ent_Protocol, 1, 2, 1, 2);
      Set_Activates_Default (Ent_Protocol, True);

      Button := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
      Set_Default_Response (Dialog, Gtk_Response_OK);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         Process.Descriptor.Remote_Target :=
           new String'(Get_Text (Ent_Target));
         Process.Descriptor.Protocol :=
           new String'(Get_Text (Ent_Protocol));
         Connect_To_Target
           (Process.Debugger,
            Process.Descriptor.Remote_Target.all,
            Process.Descriptor.Protocol.all,
            GVD.Types.Visible);
      end if;

      Destroy (Dialog);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Connect_To_Board;

   -------------------------
   -- On_Debug_Executable --
   -------------------------

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top         : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger :=
        Get_Current_Process (Top);
      Exec        : GNAT.OS_Lib.String_Access;
      Ptr         : GNAT.OS_Lib.String_Access :=
        GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Suffix : constant String := Ptr.all;

      use Debugger, GNAT.OS_Lib;

   begin
      Free (Ptr);

      declare
         S : constant String :=
           Select_File
             (Title             => -"Select File to Debug",
              File_Pattern      => "*" & Exec_Suffix,
              Pattern_Name      => -"Executable files",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if S = "" then
            return;
         end if;

         Exec := Locate_Exec_On_Path (S);

         if Exec /= null then
            Set_Executable (Process.Debugger, Exec.all, Mode => Hidden);
            Change_Dir (Dir_Name (Exec.all));
            Free (Exec);

         elsif Process.Descriptor.Remote_Host'Length /= 0
           or else GNAT.OS_Lib.Is_Regular_File (S)
           or else GNAT.OS_Lib.Is_Regular_File (S & Exec_Suffix)
         then
            Set_Executable (Process.Debugger, S, Mode => Hidden);
            Change_Dir (Dir_Name (S));
         else
            Console.Insert (Kernel, (-"Could not find file: ") & S,
                            Mode => Error);
         end if;

      exception
         when Executable_Not_Found =>
            Console.Insert (Kernel, (-"Could not find file: ") & S,
                            Mode => Error);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Executable;

   ------------------
   -- On_Load_Core --
   ------------------

   procedure On_Load_Core
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);
      use Debugger;

   begin
      declare
         S : constant String :=
           Select_File
             (Title             => -"Select Core File",
              File_Pattern      => "core*",
              Pattern_Name      => -"Core files",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if S = "" then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else GNAT.OS_Lib.Is_Regular_File (S)
         then
            Load_Core_File (Process.Debugger, S, Mode => GVD.Types.Visible);
         else
            Console.Insert (Kernel, (-"Could not find core file: ") & S,
                            Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Load_Core;

   --------------------
   -- GVD_Contextual --
   --------------------

   procedure GVD_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      Process  : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
      Entity   : Entity_Selection_Context_Access;
      Mitem    : Gtk_Menu_Item;
      Submenu  : Gtk_Menu;
      Line     : Integer;
      Debugger : Debugger_Access;
      Lang     : Language_Access;

   begin
      if Process = null
        or else Process.Debugger = null
        or else Context.all not in Entity_Selection_Context'Class
      then
         return;
      end if;

      Entity   := Entity_Selection_Context_Access (Context);
      Debugger := Process.Debugger;

      Gtk_New (Mitem, Label => -"Debug");
      Gtk_New (Submenu);
      Set_Submenu (Mitem, Gtk_Widget (Submenu));
      Append (Menu, Mitem);

      if not Command_In_Process (Get_Process (Debugger)) then
         if Has_Entity_Name_Information (Entity) then
            declare
               Ent : constant String := Entity_Name_Information (Entity);
            begin
               Gtk_New (Mitem, -"Print " & Krunch (Ent));
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (Print_Variable'Access),
                  Selection_Context_Access (Context));
               Gtk_New (Mitem, -"Display " & Krunch (Ent));
               Append (Submenu, Mitem);
                  Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller
                    (Graph_Display_Variable'Access),
                  Selection_Context_Access (Context));

               Lang := Get_Language_From_File
                 (Get_Language_Handler (Get_Kernel (Context)),
                  File_Information (Entity));

               if Lang /= null then
                  declare
                     Ent_Deref : constant String :=
                       Krunch (Dereference_Name (Lang, Ent));
                  begin
                     Gtk_New (Mitem, -"Print " & Ent_Deref);
                     Append (Submenu, Mitem);
                     Context_Callback.Connect
                       (Mitem, "activate",
                        Context_Callback.To_Marshaller
                          (Print_Variable'Access),
                        Selection_Context_Access (Context));
                     Gtk_New (Mitem, -"Display " & Ent_Deref);
                     Append (Submenu, Mitem);
                     Context_Callback.Connect
                       (Mitem, "activate",
                        Context_Callback.To_Marshaller
                          (Graph_Display_Variable'Access),
                        Selection_Context_Access (Context));
                  end;
               end if;

               Gtk_New (Mitem, -"Set value of " & Krunch (Ent));
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller
                    (Set_Value'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Mitem, -"View memory at address of " & Krunch (Ent));
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller
                    (View_Into_Memory'Access),
                  Selection_Context_Access (Context));
               Gtk_New (Mitem);
               Append (Submenu, Mitem);
               Gtk_New (Mitem, -"Set breakpoint on " & Krunch (Ent));
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller
                    (Set_Subprogram_Breakpoint'Access),
                  Selection_Context_Access (Context));
            end;
         end if;

         if Has_Line_Information (Entity) then
            Line := Line_Information (Entity);
            Gtk_New (Mitem, -"Set breakpoint on line" & Line'Img);
            Append (Submenu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (Set_Breakpoint'Access),
               Selection_Context_Access (Context));
            Gtk_New (Mitem, -"Continue until line" & Line'Img);
            Append (Submenu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (Till_Breakpoint'Access),
               Selection_Context_Access (Context));
            Gtk_New (Mitem);
            Append (Submenu, Mitem);
         end if;
      end if;

      Gtk_New (Mitem, -"Show Current Location");
      Append (Submenu, Mitem);
      Context_Callback.Connect
        (Mitem, "activate",
         Context_Callback.To_Marshaller (Show_Current_Line_Menu'Access),
         Selection_Context_Access (Context));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end GVD_Contextual;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   procedure Tooltip_Handler
     (Sel_Context : access Selection_Context'Class;
      Pixmap      : out Gdk.Gdk_Pixmap;
      Width       : out Gint;
      Height      : out Gint)
   is
      Selection : Entity_Selection_Context_Access;
      Debugger  : Visual_Debugger;
      Kernel    : Kernel_Handle;
      Value     : Basic_Types.String_Access;
      Context   : Items.Drawing_Context;
      pragma Unreferenced (Context);

   begin
      Pixmap := null;
      Width  := 0;
      Height := 0;

      if Sel_Context.all not in Entity_Selection_Context'Class then
         return;
      end if;

      Kernel    := Get_Kernel (Sel_Context);
      Selection := Entity_Selection_Context_Access (Sel_Context);
      Debugger  := Get_Current_Process (Get_Main_Window (Kernel));

      if Debugger = null
        or else Debugger.Debugger = null
        or else not Has_Entity_Name_Information (Selection)
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return;
      end if;

      Push_State (Kernel, Busy);

      declare
         Variable_Name : constant String :=
           Entity_Name_Information (Selection);
      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Get_Language (Debugger.Debugger), Variable_Name)
         then
            Pop_State (Kernel);
            return;

         else
            Value := new String'(Value_Of (Debugger.Debugger, Variable_Name));
         end if;

         if Value.all /= "" then
            Context := Create_Tooltip_Drawing_Context
              (Debugger.Data_Canvas, Null_Pixmap);

            Create_Pixmap_From_Text
              (Text       => Value.all,
               Font       =>
                 Get_Pref (Kernel, Glide_Kernel.Preferences.Default_Font),
               Bg_Color   => White (Get_Default_Colormap),
               Widget     => Get_Main_Window (Kernel),
               Pixmap     => Pixmap,
               Width      => Width,
               Height     => Height,
               Wrap_Width => Max_Tooltip_Width);
         end if;
      end;

      Free (Value);
      Pop_State (Kernel);

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Pop_State (Kernel);
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Tooltip_Handler;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Kernel : Kernel_Handle;
      State  : Debugger_State)
   is
      Top       : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Debug     : constant String := '/' & (-"Debug") & '/';
      Available : constant Boolean := State = Debug_Available;
      Sensitive : constant Boolean := State /= Debug_None;

   begin
      Set_Sensitive
        (Find_Menu_Item (Kernel, Debug & (-"Initialize")), not Sensitive);

      Set_Sensitive
        (Find_Menu_Item (Kernel, Debug & (-"Debug")), Available);
      Set_Sensitive
        (Find_Menu_Item (Kernel, Debug & (-"Data")), Available);

      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Run...")), Available);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Step")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Step Instruction")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Next")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Next Instruction")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Finish")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Continue")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Interrupt")), State = Debug_Busy);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Terminate")), Sensitive);

      Set_Sensitive (Top.Cont_Button, Sensitive);
      Set_Sensitive (Top.Step_Button, Sensitive);
      Set_Sensitive (Top.Next_Button, Sensitive);
      Set_Sensitive (Top.Finish_Button, Sensitive);
      Set_Sensitive (Top.Up_Button, Sensitive);
      Set_Sensitive (Top.Down_Button, Sensitive);
   end Set_Sensitive;

   -------------------
   -- On_Debug_Init --
   -------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record)
   is
      K            : constant Kernel_Handle := Kernel_Handle (Kernel);
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (K));
      Page         : GPS_Debugger;
      Module       : String_Access;
      Program_Args : String_Access;
      Blank_Pos    : Natural;
      Proxy        : Process_Proxy_Access;
      Success      : Boolean;
      Id           : constant GVD_Module  := GVD_Module (GVD_Module_ID);

      use Debugger;
      use type GNAT.OS_Lib.String_Access;

   begin
      Push_State (K, Busy);

      if Get_Current_Process (Top) = null then
         Gtk_New (Page, Top);
         Kernel_Callback.Connect
           (Page,
            "debugger_closed",
            Kernel_Callback.To_Marshaller (On_Debug_Terminate'Access),
            K);

      else
         Page := GPS_Debugger (Get_Current_Process (Top));
      end if;

      --  Initialize the debugger if necessary

      if Page.Debugger = null then
         Program_Args := new String'("");

         if Data.File /= "" then
            Module := new String'
              (Executables_Directory (Data.Project) & Data.File);

         elsif Top.Program_Args /= null then
            Blank_Pos := Ada.Strings.Fixed.Index (Top.Program_Args.all, " ");

            if Blank_Pos = 0 then
               Module := new String'(Top.Program_Args.all);
            else
               Module := new String'
                 (Top.Program_Args (Top.Program_Args'First .. Blank_Pos - 1));
               Free (Program_Args);
               Program_Args := new String'
                 (Top.Program_Args (Blank_Pos + 1 .. Top.Program_Args'Last));
            end if;

         else
            Module := new String'("");
         end if;

         declare
            Ptr         : GNAT.OS_Lib.String_Access :=
              GNAT.OS_Lib.Get_Executable_Suffix;
            Module_Exec : constant String := Module.all & Ptr.all;

         begin
            GNAT.OS_Lib.Free (Ptr);

            if Module'Length > 0
              and then not GNAT.OS_Lib.Is_Regular_File (Module.all)
              and then GNAT.OS_Lib.Is_Regular_File (Module_Exec)
            then
               Free (Module);
               Module := new String'(Module_Exec);
            end if;
         end;

         declare
            Args : GNAT.OS_Lib.Argument_List_Access :=
              GNAT.OS_Lib.Argument_String_To_List (Get_Attribute_Value
                (Get_Project (K), Debugger_Command_Attribute,
                 Default => "gdb"));

         begin
            Proxy := new GPS_Proxy;
            GPS_Proxy (Proxy.all).Kernel := K;
            Configure
              (Process         => Page,
               Kind            => Gdb_Type,
               Proxy           => Proxy,
               Executable      => Module.all,
               Debugger_Args   => Args (2 .. Args'Last),
               Executable_Args => Program_Args.all,
               Remote_Host     =>
                 Get_Attribute_Value (Get_Project (K), Remote_Host_Attribute),
               Remote_Target   =>
                 Get_Attribute_Value (Get_Project (K), Program_Host_Attribute),
               Remote_Protocol =>
                 Get_Attribute_Value (Get_Project (K), Protocol_Attribute),
               Debugger_Name   => Args (1).all,
               History         => Get_History (K),
               Success => Success);
            GNAT.OS_Lib.Free (Args);
            Free (Module);

            if not Success then
               Pop_State (K);
               Debug_Terminate (K);
               return;
            end if;
         end;

         Set_Sensitive (K, Debug_Available);
         Page.Destroy_Id := Widget_Callback.Connect
           (Top, "destroy",
            Widget_Callback.To_Marshaller (On_Destroy_Window'Access));

         Widget_Callback.Object_Connect
           (Page, "executable_changed",
            Widget_Callback.To_Marshaller (On_Executable_Changed'Access),
            Top);
      end if;

      --  Add columns information for not currently opened files.

      Id.Lines_Revealed_Id := Widget_Callback.Object_Connect
        (K, Source_Lines_Revealed_Signal, Lines_Revealed_Cb'Access, Top);
      Id.File_Edited_Id := Widget_Callback.Object_Connect
        (K, File_Edited_Signal, File_Edited_Cb'Access, Top);

      --  Add columns for debugging information to all the files that
      --  are currently open.

      Create_Debugger_Columns (K, "");
      Pop_State (K);

      Id.Initialized := True;
   exception
      when E : others =>
         Pop_State (K);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Init;

   ---------------------
   -- Debug_Terminate --
   ---------------------

   procedure Debug_Terminate (Kernel : Kernel_Handle) is
      Top    : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Page   : constant GPS_Debugger :=
        GPS_Debugger (Get_Current_Process (Top));
      Id     : constant GVD_Module   := GVD_Module (GVD_Module_ID);
      Editor : Code_Editor;

      use Debugger;

   begin
      Id.Initialized := False;

      File_Line_List.Free (Id.Unexplored_Lines);
      Id.Total_Explored_Lines := 0;
      Id.Total_Unexplored_Lines := 0;

      if Page = null or else Page.Debugger = null then
         return;
      end if;

      Push_State (Kernel, Busy);
      Page.Exiting := True;
      Editor := Page.Editor_Text;

      Gtk.Handlers.Disconnect (Top, Page.Destroy_Id);
      Gtk.Handlers.Disconnect (Kernel, Id.Lines_Revealed_Id);
      Gtk.Handlers.Disconnect (Kernel, Id.File_Edited_Id);

      Set_Pref (Kernel, Show_Call_Stack, Page.Stack /= null);

      if Page.Debugger /= null
        and then Get_Process (Page.Debugger) /= null
      then
         Close (Page.Debugger);
      end if;

      Page.Debugger := null;

      --  This might have been closed by the user

      if Page.Debugger_Text /= null then
         Close (Top.Process_Mdi, Page.Debugger_Text);
      end if;

      if Page.Debuggee_Console /= null then
         Close (Top.Process_Mdi, Page.Debuggee_Console);
      end if;

      if Page.Data_Scrolledwindow /= null then
         Close (Top.Process_Mdi, Page.Data_Scrolledwindow);
      end if;

      if Page.Stack /= null then
         Close (Top.Process_Mdi, Page.Stack);
      end if;

      if Top.History_Dialog /= null then
         Hide (Top.History_Dialog);
      end if;

      if Top.Thread_Dialog /= null then
         Hide (Top.Thread_Dialog);
      end if;

      if Top.Task_Dialog /= null then
         Hide (Top.Task_Dialog);
      end if;

      if Top.PD_Dialog /= null then
         Hide (Top.PD_Dialog);
      end if;

      if Top.Breakpoints_Editor /= null then
         Hide (Top.Breakpoints_Editor);
      end if;

      if Page.Breakpoints /= null then
         Free (Page.Breakpoints);
      end if;

      if Get_Mode (Editor) /= Source then
         Gtkada.MDI.Close (Get_MDI (Kernel), Get_Asm (Editor));
      end if;

      Remove_Debugger_Columns (Kernel, "");
      Free_Debug_Info (GEdit (Get_Source
        (Visual_Debugger (Get_Current_Process (Top)).Editor_Text)));
      Page.Exiting := False;
      Set_Sensitive (Kernel, Debug_None);
      Pop_State (Kernel);
   end Debug_Terminate;

   ------------------------
   -- On_Debug_Terminate --
   ------------------------

   procedure On_Debug_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

   begin
      Debug_Terminate (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Terminate;

   --------------------
   -- Set_Breakpoint --
   --------------------

   procedure Set_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      pragma Unreferenced (Widget);

   begin
      Break_Source
        (Debugger,
         File_Information (Entity),
         Line_Information (Entity),
         Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Set_Breakpoint;

   -------------------------------
   -- Set_Subprogram_Breakpoint --
   -------------------------------

   procedure Set_Subprogram_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      pragma Unreferenced (Widget);

   begin
      Break_Subprogram
        (Debugger,
         Entity_Name_Information (Entity),
         Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Set_Subprogram_Breakpoint;

   ---------------------
   -- Till_Breakpoint --
   ---------------------

   procedure Till_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Entity   : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      pragma Unreferenced (Widget);

   begin
      Break_Source
        (Debugger,
         File_Information (Entity),
         Line_Information (Entity),
         Temporary => True);
      Continue (Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Till_Breakpoint;

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      Name     : constant String :=
        Get_Text (Gtk_Label (Get_Child (Gtk_Bin (Widget))));
      Print    : constant String := -"Print ";

   begin
      Print_Value (Debugger, Name (Name'First + Print'Length .. Name'Last));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Print_Variable;

   ----------------------------
   -- Graph_Display_Variable --
   ----------------------------

   procedure Graph_Display_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Process : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
      Name    : constant String :=
        Get_Text (Gtk_Label (Get_Child (Gtk_Bin (Widget))));
      Display : constant String := -"Display ";

   begin
      Process_User_Command
        (Process,
         "graph display " & Name (Name'First + Display'Length .. Name'Last),
         Output_Command => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Graph_Display_Variable;

   ----------------------
   -- View_Into_Memory --
   ----------------------

   procedure View_Into_Memory
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Top  : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Get_Kernel (Context)));
      Name : constant String :=
        Get_Text (Gtk_Label (Get_Child (Gtk_Bin (Widget))));
      View : constant String := -"View memory at address of ";

   begin
      Show_All (Top.Memory_View);
      Display_Memory
        (Top.Memory_View,
         Name (Name'First + View'Length .. Name'Last));
      Gdk_Raise (Get_Window (Top.Memory_View));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end View_Into_Memory;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Process  : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
      Name     : constant String :=
        Get_Text (Gtk_Label (Get_Child (Gtk_Bin (Widget))));
      Val      : constant String := -"Set value of ";
      Variable : constant String :=
        Name (Name'First + Val'Length .. Name'Last);

   begin
      declare
         S : constant String :=
           Simple_Entry_Dialog
             (Parent   => Process.Window,
              Title    => -"Setting value of " & Variable,
              Message  => -"Setting value of " & Variable & ':',
              Position => Win_Pos_Mouse,
              Key      => "gvd_set_value_dialog");

      begin
         if S /= "" and then S (S'First) /= ASCII.NUL then
            Set_Variable (Process.Debugger, Variable, S);
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Set_Value;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Top  : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Get_Kernel (Context)));
      Edit : constant GEdit := GEdit (Get_Source
        (Visual_Debugger (Get_Current_Process (Top)).Editor_Text));
      Name : constant String := Get_Current_File (Edit);

   begin
      if Name /= "" then
         Highlight_Current_Line (Edit);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Show_Current_Line_Menu;

   -----------------------
   -- On_Start_Continue --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         if Is_Started (Tab.Debugger) then
            Continue (Tab.Debugger, Mode => GVD.Types.Visible);
         else
            Start (Tab.Debugger, Mode => GVD.Types.Visible);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Start_Continue;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed (Object : access Gtk_Widget_Record'Class) is
      Top : constant Glide_Window := Glide_Window (Object);
   begin
      --  Re-create all debugger columns.
      Remove_Debugger_Columns (Top.Kernel, "");
      Create_Debugger_Columns (Top.Kernel, "");
   end On_Executable_Changed;

   -----------------------
   -- On_Destroy_Window --
   -----------------------

   procedure On_Destroy_Window (Object : access Gtk_Widget_Record'Class) is
   begin
      --  ??? Should destroy all debuggers, not only the current one.
      Close (Get_Current_Process (Object).Debugger);
   end On_Destroy_Window;

   ------------------------------
   -- Line_Information_Compute --
   ------------------------------

   procedure Line_Information_Compute
     (Id      : in out GVD_Module;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Kind       : Line_Kind;
      C          : Set_Breakpoint_Command_Access;
      File_Line  : File_Line_Record;

      Tab        : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Id.Kernel));
      Debugger   : constant Debugger_Access := Tab.Debugger;
      --  ??? Should attach the right debugger.

   begin
      if File_Line_List.Is_Empty (Id.Unexplored_Lines)
        or else Debugger = null
        or else Get_Process (Debugger) = null
      then
         Result := Success;
         return;

      elsif Command_In_Process (Get_Process (Debugger)) then
         Id.Slow_Query := True;
         Result := Lower_Priority;
         return;

      elsif Id.Slow_Query then
         Id.Slow_Query := False;
         Result := Raise_Priority;
         return;
      end if;

      File_Line := File_Line_List.Head (Id.Unexplored_Lines);

      Kind := Line_Contains_Code
        (Debugger, File_Line.File.all, File_Line.Line);

      Id.Total_Explored_Lines := Id.Total_Explored_Lines + 1;

      if Id.List_Modified then
         Id.List_Modified := False;
         Result := Execute_Again;
         return;
      end if;

      declare
         L          : constant Integer := File_Line.Line;
         A          : Line_Information_Array (L .. L);
         Mode       : Breakpoint_Command_Mode := Set;

         use File_Line_List;

         Node      : List_Node;
         Prev_Node : List_Node;

      begin
         if Kind = Have_Code then
            A (L).Image := Line_Has_Code_Pixbuf;

            --  Check whether a breakpoint is set at this location, if so,
            --  set the mode accordingly.
            --  ??? Maybe we could optimize a little bit here.

            if Tab.Breakpoints /= null then
               for J in Tab.Breakpoints'Range loop
                  if Tab.Breakpoints (J).Line = File_Line.Line
                    and then Tab.Breakpoints (J).File /= null
                    and then Tab.Breakpoints (J).File.all = File_Line.File.all
                  then
                     Mode := Unset;
                     A (L).Image := Line_Has_Breakpoint_Pixbuf;
                     exit;
                  end if;
               end loop;
            end if;

            Create
              (C, Id.Kernel, Tab, Mode, File_Line.File.all,
               File_Line.Line);
            A (L).Associated_Command := Command_Access (C);

         elsif Kind = No_More_Code then

            --  If Kind is No_More_Code, browse through the list of
            --  unexplored lines, and clean it of all lines after the
            --  current line in the current file.

            Node := First (Id.Unexplored_Lines);

            while Node /= Null_Node loop
               if Data (Node).File.all = File_Line.File.all
                 and then Data (Node).Line > File_Line.Line
               then
                  Prev_Node := Prev (Id.Unexplored_Lines, Node);
                  Remove_Nodes (Id.Unexplored_Lines, Prev_Node, Node);

                  if Prev_Node = Null_Node then
                     Node := First (Id.Unexplored_Lines);

                     if Node = Null_Node then
                        Result := Success;
                        return;
                     end if;
                  else
                     Node := Prev_Node;
                  end if;
               end if;

               Node := Next (Node);
            end loop;

            if File_Line_List.Is_Empty (Id.Unexplored_Lines) then
               Result := Success;
               return;
            end if;
         end if;

         Add_Line_Information
           (Id.Kernel,
            File_Line.File.all,
            Breakpoints_Column_Id,
            new Line_Information_Array'(A));
      end;

      File_Line_List.Next (Id.Unexplored_Lines);

      if File_Line_List.Is_Empty (Id.Unexplored_Lines) then
         Result := Success;
         return;
      end if;

      Set_Progress
        (Command,
         (Running, Id.Total_Explored_Lines, Id.Total_Unexplored_Lines));

      Result := Execute_Again;

   exception
      when E : GNAT.Expect.Process_Died =>
         Debug_Terminate (Id.Kernel);
         Trace (Me, "Debugger died unexpectedly: "
                & Exception_Information (E));

         Result := Success;

      when E : others =>
         Debug_Terminate (Id.Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));

         Result := Failure;
   end Line_Information_Compute;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Args    : GValues)
   is
      Top  : constant Glide_Window := Glide_Window (Widget);
      File : constant String := Get_String (Nth (Args, 1));
   begin
      Create_Debugger_Columns (Top.Kernel, File);

   exception
      when E : others =>
         Debug_Terminate (GVD_Module (GVD_Module_ID).Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

   -----------------------
   -- Lines_Revealed_Cb --
   -----------------------

   procedure Lines_Revealed_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Args    : GValues)
   is
      pragma Unreferenced (Widget);
      Context      : constant Selection_Context_Access :=
        To_Selection_Context_Access (Get_Address (Nth (Args, 1)));
      Area_Context : File_Area_Context_Access;

      Process      : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
      Id           : constant GVD_Module := GVD_Module (GVD_Module_ID);

   begin
      if Process = null
        or else Process.Debugger = null
        or else Context.all not in File_Area_Context'Class
      then
         return;
      end if;

      Area_Context := File_Area_Context_Access (Context);

      declare
         Line1, Line2 : Integer;
         File : constant String := Directory_Information (Area_Context) &
           File_Information (Area_Context);
         C    : Reveal_Lines_Commands.Generic_Asynchronous_Command_Access;

      begin
         Get_Area (Area_Context, Line1, Line2);

         if Id.Show_Lines_With_Code then
            if File_Line_List.Is_Empty (Id.Unexplored_Lines) then
               Reveal_Lines_Commands.Create (C, Id);

               Launch_Background_Command
                 (Id.Kernel, Command_Access (C), True, "Debugger");
            else
               Id.List_Modified := True;
               Id.Unexplored_Lines := File_Line_List.Null_List;
            end if;

            for J in Line1 .. Line2 loop
               File_Line_List.Prepend
                 (Id.Unexplored_Lines, (new String'(File), J));
               --  ??? We might want to use a LIFO structure here
               --  instead of FIFO, so that the lines currently shown
               --  are displayed first.
            end loop;

            Id.Total_Unexplored_Lines := Id.Total_Unexplored_Lines
              + Line2 - Line1 + 1;

            return;
         end if;

         --  If we are not showing lines with code, no need to do the
         --  following in an idle loop.

         declare
            A        : Line_Information_Array (Line1 .. Line2);
            C        : Set_Breakpoint_Command_Access;
            Mode     : Breakpoint_Command_Mode := Set;
            Tab      : constant Visual_Debugger :=
              Get_Current_Process (Get_Main_Window (Id.Kernel));
            Bps      : Bp_Array (Line1 .. Line2) := (others => 0);

         begin
            --  Build an array of breakpoints in the current range, more
            --  efficient than re-browsing the whole array of breakpoints
            --  for each line.

            if Tab.Breakpoints /= null then
               for J in Tab.Breakpoints'Range loop
                  if Tab.Breakpoints (J).Line in Bps'Range
                    and then Tab.Breakpoints (J).File /= null
                    and then Tab.Breakpoints (J).File.all = File
                  then
                     Bps (Tab.Breakpoints (J).Line) :=
                       Tab.Breakpoints (J).Num;
                  end if;
               end loop;
            end if;

            for J in A'Range loop
               A (J).Image := Line_Might_Have_Code_Pixbuf;

               if Bps (J) /= 0 then
                  Mode        := Unset;
                  A (J).Image := Line_Has_Breakpoint_Pixbuf;
               else
                  Mode := Set;
               end if;

               Create (C, Id.Kernel, Tab, Mode, File, J);
               A (J).Associated_Command := Command_Access (C);
            end loop;

            Add_Line_Information
              (Id.Kernel, File, Breakpoints_Column_Id,
               new Line_Information_Array'(A));
         end;
      end;

   exception
      when E : others =>
         Debug_Terminate (Id.Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Lines_Revealed_Cb;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      use GNAT.OS_Lib;

      pragma Unreferenced (K);
      Mitem : Gtk_Menu_Item;
      Menu  : Gtk_Menu renames GVD_Module (GVD_Module_ID).Initialize_Menu;
      Iter  : Imported_Project_Iterator := Start (Get_Project (Kernel));
      Debuggable_Suffix : GNAT.OS_Lib.String_Access := Get_Debuggable_Suffix;

   begin
      --  Remove all existing menus
      Remove_All_Children (Menu);

      if Debuggable_Suffix = null then
         Debuggable_Suffix := new String'("");
      end if;

      --  Add all the main units from all the imported projects.
      while Current (Iter) /= No_Project loop
         declare
            Mains : GNAT.OS_Lib.Argument_List := Get_Attribute_Value
              (Current (Iter), Attribute => Main_Attribute);
         begin
            for M in Mains'Range loop
               declare
                  Exec : constant String := Get_Executable_Name
                    (Current (Iter), Mains (M).all);
               begin
                  Gtk_New (Mitem, Exec);
                  Append (Menu, Mitem);
                  File_Project_Cb.Object_Connect
                    (Mitem, "activate",
                     File_Project_Cb.To_Marshaller (On_Debug_Init'Access),
                     Slot_Object => Kernel,
                     User_Data => File_Project_Record'
                       (Length  => Exec'Length,
                        Project => Current (Iter),
                        File    => Exec));
               end;
            end loop;

            Free (Mains);
         end;

         Next (Iter);
      end loop;

      Free (Debuggable_Suffix);

      --  Specific entry to start the debugger without any main program
      Gtk_New (Mitem, -"<no main file>");
      Append (Menu, Mitem);
      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Debug_Init'Access),
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Length  => 0,
            Project => Get_Project (Kernel),
            File    => ""));
      Show_All (Menu);

   exception
      when E : others =>
         Debug_Terminate (GVD_Module (GVD_Module_ID).Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access GObject_Record'Class; User : GObject)
   is
      pragma Unreferenced (User);
      Window : constant Gtk_Window := Get_Main_Window (Kernel_Handle (Kernel));
      Top    : constant Glide_Window := Glide_Window (Window);
      Id     : constant GVD_Module  := GVD_Module (GVD_Module_ID);
      Prev   : Boolean;

   begin
      GVD.Main_Window.Preferences_Changed (Top);

      if Id.Initialized then
         Prev   := Id.Show_Lines_With_Code;
         Id.Show_Lines_With_Code :=
           Get_Pref (GVD_Prefs, Editor_Show_Line_With_Code);

         if Prev /= Id.Show_Lines_With_Code then
            Remove_Debugger_Columns (Kernel_Handle (Kernel), "");
            Create_Debugger_Columns (Kernel_Handle (Kernel), "");
         end if;
      end if;
   end Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar      : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Window       : constant Gtk_Window  := Get_Main_Window (Kernel);
      Top          : constant Glide_Window := Glide_Window (Window);
      Debug        : constant String := '/' & (-"Debug") & '/';
      Debug_Sub    : constant String := Debug & (-"_Debug") & '/';
      Data_Sub     : constant String := Debug & (-"Data") & '/';
      Mitem        : Gtk_Menu_Item;
      Menu         : Gtk_Menu;
      --  ??? Should get the right process

   begin
      GVD_Module_ID := new GVD_Module_Record;
      GVD_Module (GVD_Module_ID).Kernel := Kernel_Handle (Kernel);
      GVD_Module (GVD_Module_ID).Show_Lines_With_Code
        := Get_Pref (GVD_Prefs, Editor_Show_Line_With_Code);

      Register_Module
        (Module                  => GVD_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => GVD_Module_Name,
         Priority                => Default_Priority + 20,
         Contextual_Menu_Handler => GVD_Contextual'Access,
         Tooltip_Handler         => Tooltip_Handler'Access);

      --  Dynamic Initialize menu
      Mitem := Register_Menu (Kernel, Debug, -"Initialize", "", null,
                              Ref_Item => -"Data");
      Gtk_New (Menu);
      Set_Submenu (Mitem, Menu);
      GVD_Module (GVD_Module_ID).Initialize_Menu := Menu;
      Kernel_Callback.Connect
        (Kernel, "project_view_changed",
         Kernel_Callback.To_Marshaller (On_View_Changed'Access),
         User_Data => Kernel_Handle (Kernel));

      --  Add debugger menus

      Register_Menu (Kernel, Debug_Sub, Ref_Item => -"Data");
      Register_Menu (Kernel, Debug_Sub, -"_Connect to Board...", "",
                     On_Connect_To_Board'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Load File...", "",
                     On_Debug_Executable'Access);
      Register_Menu (Kernel, Debug_Sub, -"Add _Symbols...", "",
                     On_Add_Symbols'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Attach...", "",
                     On_Attach'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Detach", "",
                     On_Detach'Access);
      Register_Menu (Kernel, Debug_Sub, -"Debug _Core File...", "",
                     On_Load_Core'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Kill", "",
                     On_Kill'Access);

      Register_Menu (Kernel, Data_Sub, -"_Call Stack", "",
                     On_Call_Stack'Access, Ref_Item => -"Protection Domains");
      Register_Menu (Kernel, Data_Sub, -"_Threads", "",
                     On_Threads'Access, Ref_Item => -"Protection Domains");
      Register_Menu (Kernel, Data_Sub, -"T_asks", "",
                     On_Tasks'Access, Ref_Item => -"Protection Domains");
      Mitem := Find_Menu_Item (Kernel, Data_Sub & (-"Protection Domains"));
      Set_Sensitive (Mitem, False);
      Kernel_Callback.Connect
        (Mitem, "activate",
         Kernel_Callback.To_Marshaller (On_PD'Access),
         User_Data => Kernel_Handle (Kernel));
      Register_Menu (Kernel, Data_Sub, -"A_ssembly", "", On_Assembly'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Edit _Breakpoints", "",
                     On_Edit_Breakpoints'Access);
      Register_Menu (Kernel, Data_Sub, -"Examine _Memory", "",
                     On_Examine_Memory'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu
        (Kernel, Data_Sub, -"_Command History", Stock_Index,
         On_Command_History'Access, Sensitive => False);      Gtk_New (Mitem);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Display _Local Variables", "",
                     On_Display_Locals'Access, null,
                     GDK_L, Mod1_Mask);
      Register_Menu (Kernel, Data_Sub, -"Display _Arguments", "",
                     On_Display_Args'Access, null,
                     GDK_U, Mod1_Mask);
      Register_Menu (Kernel, Data_Sub, -"Display _Registers", "",
                     On_Display_Regs'Access);
      Register_Menu (Kernel, Data_Sub, -"Display Any _Expression...", "",
                     On_Display_Expression'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"R_efresh", Stock_Refresh,
                     On_Data_Refresh'Access, null,
                     GDK_L, Control_Mask);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);

      Register_Menu (Kernel, Debug, -"_Run...", "",
                     On_Start'Access, null, GDK_F2, Sensitive => False);
      Register_Menu (Kernel, Debug, -"S_tep", "",
                     On_Step'Access, null,  GDK_F5, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Step _Instruction", "",
                     On_Step_Instruction'Access, null,
                     GDK_F5, Shift_Mask, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Next", "",
                     On_Next'Access, null, GDK_F6, Sensitive => False);
      Register_Menu (Kernel, Debug, -"N_ext Instruction", "",
                     On_Next_Instruction'Access, null,
                     GDK_F6, Shift_Mask, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Finish", "",
                     On_Finish'Access, null, GDK_F7, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Continue", "",
                     On_Continue'Access, null, GDK_F8, Sensitive => False);
      Register_Menu (Kernel, Debug, -"_Interrupt", Stock_Stop,
                     On_Interrupt'Access, null,
                     GDK_backslash, Control_Mask, Sensitive => False);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);
      Register_Menu (Kernel, Debug, -"Ter_minate", "",
                     On_Debug_Terminate'Access, Sensitive => False);

      --  Add debugger buttons in the toolbar

      Top.Toolbar_Space := Append_Element
        (Toolbar  => Toolbar,
         The_Type => Toolbar_Child_Space);
      Top.Cont_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Go",
         Tooltip_Text => -"Start/Continue the debugged program",
         Icon         => Gtk_Widget (Create_Pixmap (run_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Cont_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Start_Continue'Access),
         Window);

      Top.Step_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Step",
         Tooltip_Text => -"Step",
         Icon         => Gtk_Widget (Create_Pixmap (step_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Step_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Step'Access), Window);

      Top.Next_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Next",
         Tooltip_Text => -"Next",
         Icon         => Gtk_Widget (Create_Pixmap (next_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Next_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Next'Access), Window);

      Top.Finish_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Finish",
         Tooltip_Text => -"Execute until selected stack frame returns",
         Icon         => Gtk_Widget (Create_Pixmap (finish_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Finish_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Finish'Access), Window);

      Top.Up_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Up",
         Tooltip_Text => -"Select and print stack frame that called this one",
         Icon         => Gtk_Widget (Create_Pixmap (up_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Up_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Up'Access), Window);

      Top.Down_Button := Append_Element
        (Toolbar      => Toolbar,
         The_Type     => Toolbar_Child_Button,
         Text         => -"Down",
         Tooltip_Text => -"Select and print stack frame called by this one",
         Icon         => Gtk_Widget (Create_Pixmap (down_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Down_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Down'Access), Window);

      Set_Sensitive (Kernel_Handle (Kernel), Debug_None);

      Object_User_Callback.Connect
        (Kernel,
         Preferences_Changed_Signal,
         Object_User_Callback.To_Marshaller (Preferences_Changed'Access),
         GObject (Kernel));

      Init_Graphics;
   end Register_Module;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out File_Line_Record) is
   begin
      Free (X.File);
   end Free;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out GVD_Module_Record) is
   begin
      Debug_Terminate (Id.Kernel);
   end Destroy;

   ---------------------------
   -- Free_Line_Information --
   ---------------------------

   procedure Free_Line_Information (Id : in out GVD_Module) is
   begin
      Id.Total_Explored_Lines := 0;
      Id.Total_Unexplored_Lines := 0;
   end Free_Line_Information;

end GVD_Module;
