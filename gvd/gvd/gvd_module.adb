-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;

with Gdk.Color;                 use Gdk.Color;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Accel_Group;           use Gtk.Accel_Group;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Container;             use Gtk.Container;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Types;               use Basic_Types;
with Breakpoints_Editor;        use Breakpoints_Editor;
with Commands.Debugger;         use Commands.Debugger;
with Commands.Interactive;      use Commands.Interactive;
with Commands;                  use Commands;
with Debugger;                  use Debugger;
with Debugger_Pixmaps;          use Debugger_Pixmaps;
with Entities;                  use Entities;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Main_Window.Debug;     use GPS.Main_Window.Debug;
with GPS.Main_Window;           use GPS.Main_Window;
with GUI_Utils;                 use GUI_Utils;
with GVD.Assembly_View;         use GVD.Assembly_View;
with GVD.Call_Stack;            use GVD.Call_Stack;
with GVD.Canvas;                use GVD.Canvas;
with GVD.Consoles;              use GVD.Consoles;
with GVD.Code_Editors;          use GVD.Code_Editors;
with GVD.Dialogs;               use GVD.Dialogs;
with GVD.Memory_View;           use GVD.Memory_View;
with GVD.Menu;                  use GVD.Menu;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Proc_Utils;            use GVD.Proc_Utils;
with GVD.Process;               use GVD.Process;
with GVD.Scripts;               use GVD.Scripts;
with GVD.Source_Editor.GPS;     use GVD.Source_Editor.GPS;
with GVD.Source_Editor;         use GVD.Source_Editor;
with GVD.Types;                 use GVD.Types;
with Histories;                 use Histories;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with List_Select_Pkg;           use List_Select_Pkg;
with Process_Proxies;           use Process_Proxies;
with Projects;                  use Projects;
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body GVD_Module is

   Me : constant Debug_Handle := Create ("Debugger");

   Cst_Run_Arguments_History : constant History_Key := "gvd_run_arguments";
   --  The key in the history for the arguments to the run command.
   --  WARNING: this constant is shared with builder_module.adb, since we want
   --  to have the same history for the run command in GPS.

   Debug_Menu_Prefix : constant String := "<gps>/Debug/Initialize/";

   Max_Tooltip_Width : constant := 400;
   --  Maximum size to use for the tooltip windows

   type Bp_Array is array (Integer range <>) of Breakpoint_Identifier;

   type File_Edited_Hook_Record is new Function_With_Args with record
      Top : GPS_Window;
   end record;
   type File_Edited_Hook is access File_Edited_Hook_Record'Class;
   procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook.

   type Lines_Revealed_Hook_Record is new Function_With_Args with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "source_lines_revealed_hook" hook.

   type GVD_Module_Record is new Module_ID_Record with record
      Initialized                    : Boolean := False;
      --  Whether the debugger is running;

      Show_Lines_With_Code           : Boolean;
      --  Whether the lines with code should be explicitly queried.

      Initialize_Menu                : Gtk_Menu;

      Delete_Id                      : Handler_Id := (Null_Handler_Id, null);
      File_Hook                      : File_Edited_Hook;
      Lines_Hook                     : Lines_Revealed_Hook;

      Cont_Button,
      Step_Button,
      Next_Button,
      Finish_Button,
      Up_Button,
      Down_Button                    : Gtk.Tool_Button.Gtk_Tool_Button;

      First_Debugger                 : Debugger_List_Link;
      --  Points to the list of debuggers

      Current_Debugger               : Glib.Object.GObject;
      --  The current visual debugger

      Breakpoints_Editor             : Breakpoint_Editor_Access;
   end record;
   type GVD_Module is access all GVD_Module_Record'Class;

   procedure Destroy (Id : in out GVD_Module_Record);
   --  Terminate the debugger module, and kill the underlying debugger.

   function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : Selection_Context) return Gdk.Gdk_Pixmap;
   --  See inherited documentation

   GVD_Module_Name : constant String := "Debugger";
   GVD_Module_ID   : GVD_Module;

   procedure Add_Debug_Buttons (Kernel : access Kernel_Handle_Record'Class);
   --  Add debugger related buttons to the main toolbar.

   procedure Remove_Debug_Buttons
     (Kernel : access Kernel_Handle_Record'Class);
   --  Remove debugger related buttons from the main toolbar.

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File);
   --  Remove the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, remove them for all files.

   procedure Preferences_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences are changed in the GPS kernel

   procedure Create_Debugger_Columns
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File);
   --  Create the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, create them for all files.

   function Delete_Asm
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean;
   --  Callback for the "delete_event" signal of the assembly view.
   --  Widget is a Code_Editor.

   procedure Debug_Init
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : File_Project_Record;
      Args   : String);
   --  Initialize the debugger

   function Get_Variable_Name
     (Context     : Selection_Context;
      Dereference : Boolean) return String;
   --  If Context contains an entity, get the entity name.
   --  Dereference the entity if Dereference is True.
   --  Return "" if entity name could not be found in Context.

   procedure On_Assembly
     (Widget : access Glib.Object.GObject_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Display the assembly view.
   --  Used e.g. for implementing menu Debug->Data->Assembly
   --  Widget parameter is ignored.

   procedure Start_Program
     (Process : Visual_Debugger; Start_Cmd : Boolean := False);
   --  Start the execution of the main program.
   --  A dialog is pop up for setting the argument list to give to the program
   --  being debugged when it is started. If Start_Cmd is True then the
   --  debugger must stop at the beginning of the main procedure. Otherwise,
   --  the dialog will include a checkbox so that the user will be able to
   --  select whether he/she wants to stop at the beginning of the program.
   --  For VxWorks systems, the entry point to be executed must also be
   --  specified together with the arguments; in addition, the multi-task-mode
   --  can be set as desired.

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

   procedure On_Attach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Attach

   procedure On_Detach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Detach

   procedure On_Kill
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Kill

   procedure On_Edit_Breakpoints
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Edit Breakpoints

   procedure On_Examine_Memory
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Examine Memory

   procedure On_Display_Locals
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Local Variables

   procedure On_Display_Args
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Arguments

   procedure On_Display_Regs
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Registers

   procedure On_Display_Expression
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Display Any Expression

   procedure On_Start
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Start menu

   procedure On_Step
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Step menu

   procedure On_Step_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Step Instruction menu

   procedure On_Next
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Next menu

   procedure On_Next_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Next Instruction menu

   procedure On_Finish
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Finish menu

   procedure On_Continue
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Continue menu

   procedure On_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Interrupt

   procedure On_Debug_Terminate_Current
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Terminate Current

   procedure On_Debug_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Terminate

   -----------------------
   -- Toolbar Callbacks --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "start/continue" button

   procedure On_Step (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "step" button

   procedure On_Next (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "next" button

   procedure On_Finish (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "finish" button

   procedure On_Up (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "up" button

   procedure On_Down (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "down" button

   --------------------
   -- Misc Callbacks --
   --------------------

   procedure On_Executable_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Hook for "debugger_executable_changed"

   --------------------
   -- GVD_Contextual --
   --------------------

   Is_Printable_Entity : constant array (E_Kinds) of Boolean :=
     (Overloaded_Entity     => True,
      Unresolved_Entity     => True,
      Access_Kind           => True,
      Array_Kind            => True,
      Boolean_Kind          => True,
      Class_Wide            => True,
      Class                 => True,
      Decimal_Fixed_Point   => True,
      Enumeration_Literal   => True,
      Enumeration_Kind      => True,
      Exception_Entity      => True,
      Floating_Point        => True,
      Modular_Integer       => True,
      Named_Number          => True,
      Ordinary_Fixed_Point  => True,
      Record_Kind           => True,
      Signed_Integer        => True,
      String_Kind           => True,
      others                => False);
   --  Set of printable entities

   Is_Access_Entity : constant array (E_Kinds) of Boolean :=
     (Overloaded_Entity => True,
      Unresolved_Entity => True,
      Access_Kind       => True,
      Array_Kind        => True,
      String_Kind       => True,
      others            => False);
   --  Set of potentially dereferenceable entities

   ----------------
   -- Contextual --
   ----------------

   type Debugger_Active_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean;

   type Printable_Variable_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Access_Variable_Filter is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Subprogram_Variable_Filter
     is new Action_Filter_Record with null record;
   function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Print_Variable_Command is new Interactive_Command with record
      Display     : Boolean := False;
      Dereference : Boolean := False;
   end record;
   function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Value_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type View_Memory_Command is new Interactive_Command with null record;
   function Execute
     (Command : access View_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_Location_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Breakpoint_Command is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   function Custom_Label_Expansion
     (Context : Selection_Context) return String;
   --  Provide expansion for "$!" in the labels for contextual menus

   ----------------------------
   -- Get_Breakpoints_Editor --
   ----------------------------

   function Get_Breakpoints_Editor
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Gtk.Window.Gtk_Window
   is
      pragma Unreferenced (Kernel);
   begin
      return Gtk_Window (GVD_Module_ID.Breakpoints_Editor);
   end Get_Breakpoints_Editor;

   -----------------------
   -- Get_Debugger_List --
   -----------------------

   function Get_Debugger_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Debugger_List_Link
   is
      pragma Unreferenced (Kernel);
   begin
      return GVD_Module_ID.First_Debugger;
   end Get_Debugger_List;

   --------------------------
   -- Get_Current_Debugger --
   --------------------------

   function Get_Current_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Glib.Object.GObject
   is
      pragma Unreferenced (Kernel);
   begin
      return GVD_Module_ID.Current_Debugger;
   end Get_Current_Debugger;

   ------------------------
   -- Set_First_Debugger --
   ------------------------

   procedure Set_First_Debugger
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : Debugger_List_Link)
   is
      pragma Unreferenced (Kernel);
   begin
      GVD_Module_ID.First_Debugger := Debugger;
   end Set_First_Debugger;

   --------------------------
   -- Set_Current_Debugger --
   --------------------------

   procedure Set_Current_Debugger
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Current : Glib.Object.GObject)
   is
      pragma Unreferenced (Kernel);
   begin
      GVD_Module_ID.Current_Debugger := Current;
   end Set_Current_Debugger;

   -----------------------
   -- Add_Debug_Buttons --
   -----------------------

   procedure Add_Debug_Buttons (Kernel : access Kernel_Handle_Record'Class) is
      Toolbar  : constant Gtk_Toolbar  := Get_Toolbar (Kernel);
      Window   : constant Gtk_Window := Get_Main_Window (Kernel);

   begin
      if GVD_Module_ID.Cont_Button /= null then
         return;
      end if;

      Gtk_New_From_Stock (GVD_Module_ID.Cont_Button, "gps-debugger-run");
      Set_Name (GVD_Module_ID.Cont_Button, "gps-debugger-run-button");
      Set_Tooltip (GVD_Module_ID.Cont_Button, Get_Tooltips (Kernel),
                   -"Start/Continue the debugged program");
      Insert (Toolbar, GVD_Module_ID.Cont_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Cont_Button, "clicked",
         On_Start_Continue'Access, Window);
      Show_All (GVD_Module_ID.Cont_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Step_Button, "gps-debugger-step");
      Set_Name (GVD_Module_ID.Step_Button, "gps-debugger-step-button");
      Set_Tooltip (GVD_Module_ID.Step_Button, Get_Tooltips (Kernel),
                   -"Step");
      Insert (Toolbar, GVD_Module_ID.Step_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Step_Button, "clicked", On_Step'Access, Window);
      Show_All (GVD_Module_ID.Step_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Next_Button, "gps-debugger-next");
      Set_Name (GVD_Module_ID.Next_Button, "gps-debugger-next-button");
      Set_Tooltip (GVD_Module_ID.Next_Button, Get_Tooltips (Kernel),
                   -"Next");
      Insert (Toolbar, GVD_Module_ID.Next_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Next_Button, "clicked", On_Next'Access, Window);
      Show_All (GVD_Module_ID.Next_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Finish_Button, "gps-debugger-finish");
      Set_Name (GVD_Module_ID.Finish_Button, "gps-debugger-finish-button");
      Set_Tooltip (GVD_Module_ID.Finish_Button, Get_Tooltips (Kernel),
                   -"Execute until selected stack frame returns");
      Insert (Toolbar, GVD_Module_ID.Finish_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Finish_Button, "clicked", On_Finish'Access, Window);
      Show_All (GVD_Module_ID.Finish_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Up_Button, "gps-debugger-up");
      Set_Name (GVD_Module_ID.Up_Button, "gps-debugger-up-button");
      Set_Tooltip (GVD_Module_ID.Up_Button, Get_Tooltips (Kernel),
                   -"Select and print stack frame that called this one");
      Insert (Toolbar, GVD_Module_ID.Up_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Up_Button, "clicked", On_Up'Access, Window);
      Show_All (GVD_Module_ID.Up_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Down_Button, "gps-debugger-down");
      Set_Name (GVD_Module_ID.Down_Button, "gps-debugger-down-button");
      Set_Tooltip (GVD_Module_ID.Down_Button, Get_Tooltips (Kernel),
                   -"Select and print stack frame called by this one");
      Insert (Toolbar, GVD_Module_ID.Down_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Down_Button, "clicked", On_Down'Access, Window);
      Show_All (GVD_Module_ID.Down_Button);
   end Add_Debug_Buttons;

   --------------------------
   -- Remove_Debug_Buttons --
   --------------------------

   procedure Remove_Debug_Buttons
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Toolbar : constant Gtk_Toolbar := Get_Toolbar (Kernel);

   begin
      if Toolbar /= null and then GVD_Module_ID.Cont_Button /= null then
         Remove (Toolbar, GVD_Module_ID.Cont_Button);
         Remove (Toolbar, GVD_Module_ID.Step_Button);
         Remove (Toolbar, GVD_Module_ID.Next_Button);
         Remove (Toolbar, GVD_Module_ID.Finish_Button);
         Remove (Toolbar, GVD_Module_ID.Up_Button);
         Remove (Toolbar, GVD_Module_ID.Down_Button);
         GVD_Module_ID.Cont_Button := null;
      end if;
   end Remove_Debug_Buttons;

   -------------------------
   -- Initialize_Debugger --
   -------------------------

   procedure Initialize_Debugger
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Args   : String) is
   begin
      Debug_Init
        (Kernel,
         File_Project_Record'(Get_Project (Kernel), VFS.No_File), Args);
   end Initialize_Debugger;

   -----------------------------
   -- Create_Debugger_Columns --
   -----------------------------

   procedure Create_Debugger_Columns
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) is
   begin
      --  Create the information column for the current line.
      Create_Line_Information_Column
        (Kernel,
         File,
         "Current Line",
         --  ??? That should be centralized somewhere !!!
         Every_Line    => False);

      --  Create the information column for the breakpoints
      Create_Line_Information_Column
        (Kernel,
         File,
         Breakpoints_Column_Id,
         Every_Line    => True);
   end Create_Debugger_Columns;

   -----------------------------
   -- Remove_Debugger_Columns --
   -----------------------------

   procedure Remove_Debugger_Columns
     (Kernel : Kernel_Handle;
      File   : Virtual_File) is
   begin
      Remove_Line_Information_Column (Kernel, File, "Current Line");
      Remove_Line_Information_Column
        (Kernel, File, Breakpoints_Column_Id);
   end Remove_Debugger_Columns;

   --------------------
   -- On_Add_Symbols --
   --------------------

   procedure On_Add_Symbols
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);

      use GNAT.Strings;

   begin
      declare
         S : constant Virtual_File :=
           Select_File
             (Title             => -"Select Module",
              Parent            => Gtk_Window (Top),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              File_Pattern      => "*",
              Pattern_Name      => -"All files",
              History           => Get_History (Kernel));
      begin
         if S = VFS.No_File then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Add_Symbols
              (Process.Debugger, Full_Name (S).all,
               Mode => GVD.Types.Visible);
         else
            Console.Insert
              (Kernel, (-"Could not find file: ") & Full_Name (S).all,
               Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Add_Symbols;

   ---------------
   -- On_Attach --
   ---------------

   procedure On_Attach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top           : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process       : constant Visual_Debugger := Get_Current_Process (Top);
      Process_List  : List_Select_Access;
      Success       : Boolean;
      Info          : Process_Info;
      Button        : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot attach to a task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      Gtk_New (Process_List, Title => -"Process Selection");

      Open_Processes (Process.Debugger);

      loop
         Next_Process (Process.Debugger, Info, Success);

         exit when not Success;

         Add_Item (Process_List, Info.Id, Info.Info);
      end loop;

      Close_Processes (Process.Debugger);

      declare
         Argument : constant String := Show (Process_List);
      begin
         if Argument /= "" then
            Attach_Process
              (Process.Debugger, Argument, Mode => GVD.Types.Visible);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Attach;

   ---------------
   -- On_Detach --
   ---------------

   procedure On_Detach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot detach the task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);

      else
         Detach_Process (Process.Debugger, Mode => GVD.Types.Visible);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Detach;

   -------------
   -- On_Step --
   -------------

   procedure On_Step
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Into (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Step;

   -------------------------
   -- On_Step_Instruction --
   -------------------------

   procedure On_Step_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Into_Instruction (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Step_Instruction;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Over (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Next;

   -------------------------
   -- On_Next_Instruction --
   -------------------------

   procedure On_Next_Instruction
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Step_Over_Instruction (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Next_Instruction;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Finish (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Finish;

   -----------------
   -- On_Continue --
   -----------------

   procedure On_Continue
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Continue (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Continue;

   -------------
   -- On_Kill --
   -------------

   procedure On_Kill
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Kill_Process (Process.Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Kill;

   ------------------
   -- On_Interrupt --
   ------------------

   procedure On_Interrupt
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      --  Give some visual feedback to the user

      Output_Text (Process, "<^C>" & ASCII.LF, Is_Command => True);
      Unregister_Dialog (Process);

      --  Need to flush the queue of commands
      Clear_Queue (Process.Debugger);

      Interrupt (Process.Debugger);

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         Display_Prompt (Process.Debugger);
      end if;

      Set_Busy (Process, False);

      --  We used to flush the output here, so that if the program was
      --  outputting a lot of things, we just stop there.
      --  However, this is not doable, since it in fact also flushes the
      --  prompt that the debugger prints after interruption. Calling
      --  Display_Prompt is also not acceptable, since we might be busy
      --  processing another command.

      --  Note that doing anything at this point is very unsafe, since we got
      --  called while handling a command, and this command has not been fully
      --  handled yet, so we cannot reliably send new commands to the debugger
      --  without creating a synchronization problem. Also, we should be able
      --  to clean up properly the current command, which is particularly
      --  tricky when handling an internal command.

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Interrupt;

   -------------------------
   -- On_Edit_Breakpoints --
   -------------------------

   procedure On_Edit_Breakpoints
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);
      Button  : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot edit breakpoints while the debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      Breakpoint_Editor (GVD_Module_ID.Breakpoints_Editor, Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Edit_Breakpoints;

   -----------------------
   -- On_Examine_Memory --
   -----------------------

   procedure On_Examine_Memory
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top         : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger := Get_Current_Process (Top);
      Memory_View : GVD_Memory_View;

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Gtk_New (Memory_View, Gtk_Widget (Top));
      Show_All (Memory_View);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Examine_Memory;

   -----------------------
   -- On_Display_Locals --
   -----------------------

   procedure On_Display_Locals
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Locals (Process.Debugger) & '`',
         Output_Command => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Locals;

   ---------------------
   -- On_Display_Args --
   ---------------------

   procedure On_Display_Args
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Args (Process.Debugger) & '`',
         Output_Command => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Args;

   ---------------------
   -- On_Display_Regs --
   ---------------------

   procedure On_Display_Regs
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Process_User_Command
        (Process,
         "graph display `" & Info_Registers (Process.Debugger) & '`',
         Output_Command => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Regs;

   ---------------------------
   -- On_Display_Expression --
   ---------------------------

   procedure On_Display_Expression
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      Display_Expression (Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Display_Expression;

   -------------------
   -- Start_Program --
   -------------------

   procedure Start_Program
     (Process : Visual_Debugger; Start_Cmd : Boolean := False)
   is
      Is_Multitask : aliased Boolean := False;
      Is_Start     : aliased Boolean := False;
      Arg_Msg      : aliased String := -"Run arguments:";
      Entry_Msg    : aliased String := -"Entry point and arguments:";
      Start_Msg    : aliased String := -"Stop at beginning of main subprogram";
      Multi_Msg    : aliased String := -"Enable VxWorks multi-tasks mode";
      Start_Key    : aliased String := "stop_beginning_debugger";
      Multi_Key    : aliased String := "multitask_mode_debugger";
      No_Msg       : aliased String := "";
      Button1      : Boolean_Access := null;
      Button2      : Boolean_Access := null;
      Cmd_Msg      : GNAT.Strings.String_Access := Arg_Msg'Unchecked_Access;
      Msg1         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Msg2         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Key1         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Key2         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      WTX_Version  : Natural;
   begin
      --  If the user has already requested to stop at the beginning (Start
      --  command) do not ask the same question again. Otherwise, we enable
      --  a checkbox so that the user can select whether he/she wants to
      --  stop at the beginning of the main program.

      if Start_Cmd then
         Is_Start := True;
      else
         Button1 := Is_Start'Unchecked_Access;
         Msg1    := Start_Msg'Unchecked_Access;
         Key1    := Start_Key'Unchecked_Access;
      end if;

      --  If we are debugging against VxWorks we as for the entry point to be
      --  executed, and we enable the multi-tasks-mode checkbox.

      Info_WTX (Process.Debugger, WTX_Version);

      if WTX_Version = 2 then
         --  Change the message in the dialog window indicating that the entry
         --  point needs to be specified together with the arguments.

         Cmd_Msg := Entry_Msg'Unchecked_Access;

         Button2 := Is_Multitask'Unchecked_Access;
         Msg2    := Multi_Msg'Unchecked_Access;
         Key2    := Multi_Key'Unchecked_Access;
      end if;

      declare
         Arguments : constant String := Display_Entry_Dialog
           (Parent         => Process.Window,
            Title          => -"Run/Start",
            Message        => Cmd_Msg.all,
            Key            => Cst_Run_Arguments_History,
            History        => Get_History (GPS_Window (Process.Window).Kernel),
            Check_Msg      => Msg1.all,
            Check_Msg2     => Msg2.all,
            Button_Active  => Button1,
            Button2_Active => Button2,
            Key_Check      => History_Key (Key1.all),
            Key_Check2     => History_Key (Key2.all));
      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            --  For VxWorks we need to set the desired multi-tasks-mode mode

            if WTX_Version = 2 then
               if Is_Multitask then
                  Send
                    (Process.Debugger,
                     "set multi-tasks-mode on",
                     Mode => GVD.Types.Hidden);
               else
                  Send
                    (Process.Debugger,
                     "set multi-tasks-mode off",
                     Mode => GVD.Types.Hidden);
               end if;
            end if;

            if Is_Start then
               Start (Process.Debugger, Arguments, Mode => GVD.Types.Visible);
            else
               Run (Process.Debugger, Arguments, Mode => GVD.Types.Visible);
            end if;
         end if;
      end;

   end Start_Program;

   --------------
   -- On_Start --
   --------------

   procedure On_Start
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Process : constant Visual_Debugger :=
        Get_Current_Process (GPS_Window (Get_Main_Window (Kernel)));
      Button  : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Button := Message_Dialog
           ((-"Cannot rerun while the underlying debugger is busy.") &
            ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      --  Launch the dialog for starting the application

      Start_Program (Process);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Start;

   ----------------
   -- Delete_Asm --
   ----------------

   function Delete_Asm
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      Data_Sub : constant String := '/' & (-"Debug") & '/' & (-"Data") & '/';
      Editor   : constant Code_Editor := Code_Editor (Widget);
      Asm      : constant GVD_Assembly_View  := Get_Asm (Editor);

   begin
      Set_Sensitive
        (Find_Menu_Item (Get_Kernel (GVD_Module_ID.all),
         Data_Sub & (-"Assembly")), True);
      Ref (Asm);
      Remove (Gtk_Container (Get_Parent (Asm)), Asm);
      Set_Mode (Editor, Source);
      Disconnect (Asm, GVD_Module_ID.Delete_Id);

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
      Top      : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process  : constant Visual_Debugger := Get_Current_Process (Top);
      Editor   : constant Code_Editor  := Process.Editor_Text;
      Address  : constant Address_Type := Get_Asm_Address (Editor);
      Assembly : constant GVD_Assembly_View := Get_Asm (Editor);
      Child    : GPS_MDI_Child;

   begin
      Trace (Me, "*** [On_Assembly] *** Address = " &
             Address_To_String (Address));
      if Get_Mode (Editor) = Source_Asm then
         return;
      end if;

      Set_Sensitive (Find_Menu_Item (Kernel, Data_Sub & (-"Assembly")), False);

      GVD_Module_ID.Delete_Id :=
        Gtkada.Handlers.Return_Callback.Object_Connect
          (Assembly, "delete_event",
           Gtkada.Handlers.Return_Callback.To_Marshaller (Delete_Asm'Access),
           Editor);

      Set_Mode (Editor, Source_Asm);
      Gtk_New
        (Child  => Child,
         Widget => Assembly,
         Module => GVD_Module_ID,
         Group  => Group_Debugger_Stack);
      Set_Title (Child, -"Assembly View");
      Put (Get_MDI (Kernel), Child, Initial_Position => Position_Right);
      Unref (Assembly);
      Set_Focus_Child (Child);
      Raise_Child (Child);

      if not Command_In_Process (Get_Process (Process.Debugger)) then
         Set_Source_Line (Assembly, Get_Line (Editor));

         if Address /= Invalid_Address then
            Set_Address (Assembly, Address);
            Set_Address (Editor, Address);
         end if;

         Update_Display (Assembly);

         if Process.Breakpoints /= null then
            Update_Breakpoints (Assembly, Process.Breakpoints.all);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Assembly;

   -------------------------
   -- On_Connect_To_Board --
   -------------------------

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top          : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process      : constant Visual_Debugger :=
                       Get_Current_Process (Top);
      Dialog       : Gtk_Dialog;
      Table        : Gtk_Table;
      Ent_Protocol : Gtk_Entry;
      Ent_Target   : Gtk_Entry;
      Label        : Gtk_Label;
      Button       : Gtk_Widget;
      pragma Unreferenced (Widget, Button);

   begin
      Gtk_New
        (Dialog,
         Title  => -"Connect to board",
         Parent => Get_Current_Window (Kernel),
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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Connect_To_Board;

   -------------------------
   -- On_Debug_Executable --
   -------------------------

   procedure On_Debug_Executable
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top         : constant GPS_Window :=
                      GPS_Window (Get_Main_Window (Kernel));
      Process     : constant Visual_Debugger :=
                      Get_Current_Process (Top);
      Exec        : GNAT.Strings.String_Access;
      Ptr         : GNAT.Strings.String_Access :=
                      GNAT.OS_Lib.Get_Executable_Suffix;
      Exec_Suffix : constant String := Ptr.all;

      use GNAT.OS_Lib;

   begin
      Free (Ptr);

      declare
         S : Virtual_File :=
           Select_File
             (Title             => -"Select File to Debug",
              File_Pattern      => "*" & Exec_Suffix & ";*",
              Pattern_Name      => -"Executable files;All files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if S = VFS.No_File then
            return;
         end if;

         if not Is_Regular_File (S) then
            Exec := Locate_Exec_On_Path (Base_Name (S));

            if Exec /= null then
               S := Create (Full_Filename => Exec.all);

               if not Is_Regular_File (S) then
                  S := Create (Full_Filename => Exec.all & Exec_Suffix);
               end if;

               Free (Exec);
            else
               Console.Insert
                 (Kernel, (-"Could not find file: ") & Base_Name (S),
                  Mode => Error);
               S := VFS.No_File;
            end if;
         end if;

         if S /= No_File then
            Set_Executable (Process.Debugger, S, Mode => Hidden);
            Change_Dir (Dir_Name (S).all);
         end if;

      exception
         when Executable_Not_Found =>
            Console.Insert
              (Kernel, (-"Could not find file: ") & Full_Name (S).all,
               Mode => Error);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Executable;

   ------------------
   -- On_Load_Core --
   ------------------

   procedure On_Load_Core
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window :=
        GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger :=
        Get_Current_Process (Top);

      use GNAT.Strings;

   begin
      declare
         S : constant Virtual_File :=
           Select_File
             (Title             => -"Select Core File",
              File_Pattern      => "core*;*",
              Pattern_Name      => -"Core files;All files",
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));

      begin
         if S = VFS.No_File then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Load_Core_File (Process.Debugger, Full_Name (S).all,
                            Mode => GVD.Types.Visible);
         else
            Console.Insert
              (Kernel, (-"Could not find core file: ") & Full_Name (S).all,
               Mode => Error);
         end if;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Load_Core;

   ----------------------------
   -- Custom_Label_Expansion --
   ----------------------------

   function Custom_Label_Expansion
     (Context : Selection_Context) return String is
   begin
      return Get_Variable_Name (Context, True);
   end Custom_Label_Expansion;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Top  : constant GPS_Window :=
               GPS_Window (Get_Main_Window (Get_Kernel (Context.Context)));
      Edit : constant GEdit :=
               GEdit
                 (Get_Source
                    (Visual_Debugger (Get_Current_Process (Top)).Editor_Text));
      Name : constant Virtual_File := Get_Current_File (Edit);
   begin
      if Name /= VFS.No_File then
         Highlight_Current_Line (Edit);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Process : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context.Context)));
      Debugger : constant Debugger_Access := Process.Debugger;
      Name     : constant String :=
        Get_Variable_Name (Context.Context, Command.Dereference);
   begin
      if Name /= "" then
         if Command.Display then
            Process_User_Command
              (Process, "graph display " & Name, Output_Command => True);
         else
            Send (Debugger, Print_Value_Cmd (Debugger, Name),
                  Mode => GVD.Types.Visible);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context.Context)));
      Variable : constant String := Entity_Name_Information (Context.Context);
      S        : constant String :=
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
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access View_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Memory_View : GVD_Memory_View;

   begin
      Gtk_New
        (Memory_View,
         Gtk_Widget (Get_Main_Window (Get_Kernel (Context.Context))));
      Show_All (Memory_View);
      Display_Memory (Memory_View, Entity_Name_Information (Context.Context));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Debugger : constant Debugger_Access := Get_Current_Process
        (Get_Main_Window (Get_Kernel (Context.Context))).Debugger;
   begin
      if not Command.On_Line then
         Break_Subprogram
           (Debugger,
            Entity_Name_Information (Context.Context),
            Mode => GVD.Types.Visible);
      elsif Command.Continue_Till then
         Break_Source
           (Debugger,
            File_Information (Context.Context),
            Line_Information (Context.Context),
            Temporary => True);
         Continue (Debugger, Mode => GVD.Types.Visible);
      else
         Break_Source
           (Debugger,
            File_Information (Context.Context),
            Line_Information (Context.Context),
            Mode => GVD.Types.Visible);
      end if;

      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process  : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
   begin
      return Process /= null
        and then Process.Debugger /= null
        and then (Has_File_Information (Context)
                  or else Has_Area_Information (Context))
        and then not Command_In_Process (Get_Process (Process.Debugger));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Has_Entity_Name_Information (Context) then
         Entity := Get_Entity (Context);
         return Entity = null or else Is_Subprogram (Entity);
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Has_Entity_Name_Information (Context) then
         Entity := Get_Entity (Context);

         return Entity = null
           or else (not Get_Kind (Entity).Is_Type
                    and then Is_Printable_Entity (Get_Kind (Entity).Kind));

      elsif Has_Area_Information (Context) then
         --  We assume the user knows best
         return True;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Entity : Entity_Information;
   begin
      if Has_Entity_Name_Information (Context) then
         Entity := Get_Entity (Context);
         return Entity = null
           or else (not Get_Kind (Entity).Is_Type
                    and then Is_Access_Entity (Get_Kind (Entity).Kind));

      elsif Has_Area_Information (Context) then
         return True;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : Selection_Context) return Gdk.Gdk_Pixmap
   is
      pragma Unreferenced (Module);
      Pixmap    : Gdk.Gdk_Pixmap;
      Debugger  : Visual_Debugger;
      Kernel    : Kernel_Handle;
      Value     : GNAT.Strings.String_Access;

   begin
      if not Has_Entity_Name_Information (Context) then
         return null;
      end if;

      Kernel    := Get_Kernel (Context);
      Debugger  := Get_Current_Process (Get_Main_Window (Kernel));

      if Debugger = null
        or else Debugger.Debugger = null
        or else not Has_Entity_Name_Information (Context)
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return null;
      end if;

      Push_State (Kernel, Busy);

      declare
         Variable_Name : constant String :=
           Entity_Name_Information (Context);
      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Get_Language (Debugger.Debugger), Variable_Name)
         then
            Pop_State (Kernel);
            return null;

         else
            Value := new String'(Value_Of (Debugger.Debugger, Variable_Name));
         end if;

         if Value.all /= "" then
            Create_Pixmap_From_Text
              (Text       => Value.all,
               Font       =>
                 Get_Pref (GPS.Kernel.Preferences.Default_Font),
               Bg_Color   => White (Get_Default_Colormap),
               Widget     => Get_Main_Window (Kernel),
               Pixmap     => Pixmap,
               Wrap_Width => Max_Tooltip_Width);
         end if;
      end;

      GNAT.Strings.Free (Value);
      Pop_State (Kernel);
      return Pixmap;

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Pop_State (Kernel);
         return null;
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return null;
   end Tooltip_Handler;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      State  : Debugger_State)
   is
      Debug     : constant String := '/' & (-"Debug") & '/';
      Available : constant Boolean := State = Debug_Available;
      Sensitive : constant Boolean := State /= Debug_None;
      Item      : Gtk_Menu_Item;

   begin
      if Get_Main_Window (Kernel) /= null
        and then not Gtk.Object.In_Destruction_Is_Set
          (Get_Main_Window (Kernel))
      then
         if State = Debug_Available then
            Add_Debug_Buttons (Kernel);
         elsif State = Debug_None then
            Remove_Debug_Buttons (Kernel);
         end if;

         --  The menu might not exist if GPS is being destroyed at this point,
         --  or it could have been destroyed from a python script
         Item := Find_Menu_Item (Kernel, Debug & (-"Debug"));
         if Item /= null then
            if GVD_Module_ID.Cont_Button /= null then
               Set_Sensitive (GVD_Module_ID.Cont_Button, Available);
               Set_Sensitive (GVD_Module_ID.Step_Button, Available);
               Set_Sensitive (GVD_Module_ID.Next_Button, Available);
               Set_Sensitive (GVD_Module_ID.Finish_Button, Available);
               Set_Sensitive (GVD_Module_ID.Up_Button, Available);
               Set_Sensitive (GVD_Module_ID.Down_Button, Available);
            end if;

            Set_Sensitive (Item, Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Data")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Run...")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Step")), Available);
            Set_Sensitive (Find_Menu_Item
              (Kernel, Debug & (-"Step Instruction")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Next")), Available);
            Set_Sensitive
              (Find_Menu_Item
                 (Kernel, Debug & (-"Next Instruction")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Finish")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Continue")), Available);
            Set_Sensitive
              (Find_Menu_Item
                 (Kernel, Debug & (-"Interrupt")), State = Debug_Busy);
            Set_Sensitive
              (Find_Menu_Item
                 (Kernel, Debug & (-"Terminate Current")), Sensitive);
            Set_Sensitive (Find_Menu_Item
                           (Kernel, Debug & (-"Terminate")), Sensitive);
         end if;
      end if;
   end Set_Sensitive;

   ----------------
   -- Debug_Init --
   ----------------

   procedure Debug_Init
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : File_Project_Record;
      Args   : String)
   is
      Page : Visual_Debugger;
      pragma Unreferenced (Page);
   begin
      Page := Spawn (Kernel, Data.File, Get_Project (Kernel), Args);
   end Debug_Init;

   -------------------
   -- On_Debug_Init --
   -------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record) is
   begin
      Debug_Init (Kernel_Handle (Kernel), Data, "");
   end On_Debug_Init;

   ------------------------
   -- Setup_Side_Columns --
   ------------------------

   procedure Setup_Side_Columns
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Top : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
   begin
      if GVD_Module_ID.Lines_Hook = null then
         --  Add columns information for not currently opened files.

         GVD_Module_ID.Lines_Hook := new Lines_Revealed_Hook_Record;
         Add_Hook
           (Kernel, Source_Lines_Revealed_Hook, GVD_Module_ID.Lines_Hook,
            Name  => "gvd.lines_revealed",
            Watch => GObject (Top));

         GVD_Module_ID.File_Hook := new File_Edited_Hook_Record;
         GVD_Module_ID.File_Hook.Top := Top;
         Add_Hook
           (Kernel, GPS.Kernel.File_Edited_Hook, GVD_Module_ID.File_Hook,
            Name  => "gvd.file_edited",
            Watch => GObject (Top));

         --  Add columns for debugging information to all the files that
         --  are currently open.

         Create_Debugger_Columns (Kernel, VFS.No_File);
      end if;
      GVD_Module_ID.Initialized := True;
   end Setup_Side_Columns;

   ---------------------
   -- Debug_Terminate --
   ---------------------

   procedure Debug_Terminate (Kernel : Kernel_Handle) is
      Debugger_List    : Debugger_List_Link := Get_Debugger_List (Kernel);
      Current_Debugger : Visual_Debugger;
   begin
      Push_State (Kernel, Busy);

      while Debugger_List /= null loop
         Current_Debugger := Visual_Debugger (Debugger_List.Debugger);
         Debugger_List := Debugger_List.Next;
         Close_Debugger (Current_Debugger);
      end loop;

      GVD_Module_ID.Initialized := False;

      if GVD_Module_ID.Lines_Hook /= null then
         Remove_Hook
           (Kernel, Source_Lines_Revealed_Hook, GVD_Module_ID.Lines_Hook);
         GVD_Module_ID.Lines_Hook := null;
      end if;

      if GVD_Module_ID.File_Hook /= null then
         Remove_Hook
           (Kernel, GPS.Kernel.File_Edited_Hook, GVD_Module_ID.File_Hook);
         GVD_Module_ID.File_Hook := null;
      end if;

      Remove_Debugger_Columns (Kernel, VFS.No_File);

      if GVD_Module_ID.Breakpoints_Editor /= null then
         Hide (GVD_Module_ID.Breakpoints_Editor);
      end if;

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
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Terminate;

   --------------------------------
   -- On_Debug_Terminate_Current --
   --------------------------------

   procedure On_Debug_Terminate_Current
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Close_Debugger (Get_Current_Process (Get_Main_Window (Kernel)));

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Terminate_Current;

   -----------------------
   -- Get_Variable_Name --
   -----------------------

   function Get_Variable_Name
     (Context     : Selection_Context;
      Dereference : Boolean) return String
   is
      Lang   : Language_Access;
   begin
      if Context = No_Context then
         return "";
      end if;

      if Has_Area_Information (Context) then
         if Dereference then
            if Has_File_Information (Context) then
               Lang := Get_Language_From_File
                 (Get_Language_Handler (Get_Kernel (Context)),
                  File_Information (Context));

               if Lang /= null then
                  return Dereference_Name (Lang, Text_Information (Context));
               end if;
            end if;
         end if;

         return Text_Information (Context);
      end if;

      if Has_Entity_Name_Information (Context) then
         if Dereference then
            if Has_File_Information (Context) then
               Lang := Get_Language_From_File
                 (Get_Language_Handler (Get_Kernel (Context)),
                  File_Information (Context));

               if Lang /= null then
                  return Dereference_Name
                    (Lang, Entity_Name_Information (Context));
               end if;
            end if;
         else
            return Entity_Name_Information (Context);
         end if;
      end if;

      return "";
   end Get_Variable_Name;

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
            --  Launch the dialog for starting the application

            Start_Program (Tab, Start_Cmd => True);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Start_Continue;

   -------------
   -- On_Step --
   -------------

   procedure On_Step (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Step_Into (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Step;

   -------------
   -- On_Next --
   -------------

   procedure On_Next (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Step_Over (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Next;

   ---------------
   -- On_Finish --
   ---------------

   procedure On_Finish (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Finish (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Finish;

   -----------
   -- On_Up --
   -----------

   procedure On_Up (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Stack_Up (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Up;

   -------------
   -- On_Down --
   -------------

   procedure On_Down (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Visual_Debugger := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         Stack_Down (Tab.Debugger, Mode => GVD.Types.Visible);
      end if;
   end On_Down;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : access GPS.Kernel.Hooks.Hooks_Data'Class)
   is
      Process  : constant Visual_Debugger :=
        Get_Process (Debugger_Hooks_Data_Access (Data));
   begin
      --  Change the project to match the executable

      Load_Project_From_Executable (Kernel, Process);

      --  Re-create all debugger columns.

      Remove_Debugger_Columns (Kernel_Handle (Kernel), VFS.No_File);
      Create_Debugger_Columns (Kernel_Handle (Kernel), VFS.No_File);
   end On_Executable_Changed;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      Create_Debugger_Columns (Kernel_Handle (Kernel), D.File);

   exception
      when E : others =>
         Close_Debugger (Get_Current_Process (Hook.Top));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);
      D : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);

      Process      : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (D.Context)));

   begin
      if Process = null
        or else Process.Debugger = null
        or else not Has_Area_Information (D.Context)
      then
         return;
      end if;

      declare
         Line1, Line2 : Integer;
         File : constant Virtual_File := File_Information (D.Context);

      begin
         Get_Area (D.Context, Line1, Line2);

         if GVD_Module_ID.Show_Lines_With_Code
           and then Command_In_Process (Get_Process (Process.Debugger))
         then
            return;
         end if;

         declare
            Lines       : Line_Array (Line1 .. Line2);
            A           : Line_Information_Array (Line1 .. Line2);
            C           : Set_Breakpoint_Command_Access;
            Mode        : Breakpoint_Command_Mode := Set;
            Tab         : constant Visual_Debugger :=
              Get_Current_Process (Get_Main_Window (Kernel));
            Bps         : Bp_Array (Line1 .. Line2) := (others => 0);
            Lines_Valid : Boolean := False;

         begin
            if GVD_Module_ID.Show_Lines_With_Code then
               Lines_With_Code (Process.Debugger, File, Lines_Valid, Lines);
            end if;

            --  Build an array of breakpoints in the current range, more
            --  efficient than re-browsing the whole array of breakpoints
            --  for each line.

            if Tab.Breakpoints /= null then
               for J in Tab.Breakpoints'Range loop
                  if Tab.Breakpoints (J).Line in Bps'Range
                    and then Tab.Breakpoints (J).File /= VFS.No_File
                    and then Tab.Breakpoints (J).File = File
                  then
                     Bps (Tab.Breakpoints (J).Line) :=
                       Tab.Breakpoints (J).Num;
                  end if;
               end loop;
            end if;

            for J in A'Range loop
               if Lines_Valid then
                  if Lines (J) then
                     A (J).Image := Line_Has_Code_Pixbuf;
                  end if;
               else
                  A (J).Image := Line_Might_Have_Code_Pixbuf;
               end if;

               if Bps (J) /= 0 then
                  Mode        := Unset;
                  A (J).Image := Line_Has_Breakpoint_Pixbuf;
               else
                  Mode := Set;
               end if;

               if (not Lines_Valid)
                 or else Lines (J)
               then
                  Create (C, Kernel_Handle (Kernel), Tab, Mode, File, J);
                  A (J).Associated_Command := Command_Access (C);
               end if;
            end loop;

            Add_Line_Information
              (Kernel, File, Breakpoints_Column_Id,
               new Line_Information_Array'(A));
         end;
      end;

   exception
      when E : others =>
         Close_Debugger (Process);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      use GNAT.OS_Lib;
      Mitem             : Gtk_Menu_Item;
      Menu              : Gtk_Menu renames GVD_Module_ID.Initialize_Menu;
      Group             : constant Gtk_Accel_Group :=
                            Get_Default_Accelerators (Kernel);
      Loaded_Project    : constant Project_Type := Get_Project (Kernel);
      Loaded_Mains      : Argument_List :=
                            Get_Attribute_Value
                              (Loaded_Project,
                               Attribute => Main_Attribute);
      Loaded_Has_Mains  : constant Boolean := Loaded_Mains'Length > 0;
      Extended_Project  : constant Project_Type :=
                            Parent_Project (Loaded_Project);
      Iter              : Imported_Project_Iterator :=
                            Start (Loaded_Project);
      Current_Project   : Project_Type := Current (Iter);
      Debuggable_Suffix : GNAT.Strings.String_Access := Get_Debuggable_Suffix;

   begin
      --  Remove all existing menus

      Remove_All_Children (Menu);

      if Debuggable_Suffix = null then
         Debuggable_Suffix := new String'("");
      end if;

      --  The following loop should be factorized with the one in the builder
      --  module (see Builder_Module.On_View_Changed).

      while Current_Project /= No_Project loop
         if not Loaded_Has_Mains
           or else Current_Project /= Extended_Project
         then
            declare
               Mains : Argument_List :=
                         Get_Attribute_Value
                           (Current_Project, Attribute => Main_Attribute);
            begin
               for M in reverse Mains'Range loop
                  declare
                     Exec : constant String :=
                              Get_Executable_Name
                                (Current_Project, Mains (M).all);
                  begin
                     Gtk_New (Mitem, Exec);
                     Prepend (Menu, Mitem);
                     File_Project_Cb.Object_Connect
                       (Mitem, "activate",
                        On_Debug_Init'Access,
                        Slot_Object => Kernel,
                        User_Data   => File_Project_Record'
                          (Project => No_Project,
                           File    => Create
                             (Executables_Directory
                                (Current_Project) & Exec)));

                     if M = Mains'First
                       and then Current_Project = Loaded_Project
                     then
                        Set_Accel_Path
                          (Mitem, Debug_Menu_Prefix & "item" & Image (M),
                           Group);
                     end if;
                  end;
               end loop;

               Free (Mains);
            end;
         end if;

         Next (Iter);
         Current_Project := Current (Iter);
      end loop;

      Free (Loaded_Mains);
      Free (Debuggable_Suffix);

      --  Specific entry to start the debugger without any main program
      Gtk_New (Mitem, -"<no main file>");
      Append (Menu, Mitem);
      File_Project_Cb.Object_Connect
        (Mitem, "activate", On_Debug_Init'Access,
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Project => Get_Project (Kernel),
            File    => VFS.No_File));
      Set_Accel_Path (Mitem, Debug_Menu_Prefix & "<no main>", Group);
      Show_All (Menu);

   exception
      when E : others =>
         Debug_Terminate (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Window : constant Gtk_Window := Get_Main_Window (Kernel);
      Top    : constant GPS_Window := GPS_Window (Window);
      Prev   : Boolean;

   begin
      GPS.Main_Window.Debug.Preferences_Changed (Top);

      Prev   := GVD_Module_ID.Show_Lines_With_Code;
      GVD_Module_ID.Show_Lines_With_Code :=
        Get_Pref (Editor_Show_Line_With_Code);

      if GVD_Module_ID.Initialized
        and then Prev /= GVD_Module_ID.Show_Lines_With_Code
      then
         Remove_Debugger_Columns (Kernel_Handle (Kernel), VFS.No_File);
         Create_Debugger_Columns (Kernel_Handle (Kernel), VFS.No_File);
      end if;

      Init_Graphics (Gtk_Widget (Get_Main_Window (Kernel)));
   end Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug          : constant String := '/' & (-"_Debug") & '/';
      Debug_Sub      : constant String := Debug & (-"_Debug") & '/';
      Data_Sub       : constant String := Debug & (-"D_ata") & '/';
      Mitem          : Gtk_Menu_Item;
      Menu           : Gtk_Menu;
      Command        : Interactive_Command_Access;
      Filter         : Action_Filter;
      Debugger_Filter   : Action_Filter;
      Printable_Filter  : Action_Filter;
      Access_Filter     : Action_Filter;
      Subprogram_Filter : Action_Filter;

   begin
      GVD_Module_ID := new GVD_Module_Record;
      Debugger_Module_ID := Module_ID (GVD_Module_ID);
      GVD.Preferences.Register_Default_Preferences (Get_Preferences (Kernel));
      GVD.Scripts.Create_Hooks (Kernel);
      GVD_Module_ID.Show_Lines_With_Code :=
        Get_Pref (Editor_Show_Line_With_Code);

      Register_Module
        (Module          => Module_ID (GVD_Module_ID),
         Kernel          => Kernel,
         Module_Name     => GVD_Module_Name,
         Priority        => Default_Priority + 20);

      Debugger_Filter := new Debugger_Active_Filter;
      Register_Filter (Kernel, Debugger_Filter, "Debugger active");

      Printable_Filter  := new Printable_Variable_Filter;
      Access_Filter     := new Access_Variable_Filter;
      Subprogram_Filter := new Subprogram_Variable_Filter;

      Register_Contextual_Submenu (Kernel, "Debug", Ref_Item => "References");

      Filter := Action_Filter (Debugger_Filter and Printable_Filter);
      Command := new Print_Variable_Command;
      Register_Contextual_Menu
        (Kernel, "Debug print variable",
         Label  => "Debug/Print %s",
         Action => Command,
         Filter => Filter);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Register_Contextual_Menu
        (Kernel, "Debug display variable",
         Label  => "Debug/Display %s",
         Action => Command,
         Filter => Filter);

      Filter := Action_Filter
        (Debugger_Filter and Printable_Filter and Access_Filter);
      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := False;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Contextual_Menu
        (Kernel, "Debug print dereferenced variable",
         Label  => "Debug/Print %C",
         Action => Command,
         Custom => Custom_Label_Expansion'Access,
         Filter => Filter);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Print_Variable_Command (Command.all).Dereference := True;
      Register_Contextual_Menu
        (Kernel, "Debug display dereferenced variable",
         Label  => "Debug/Display %C",
         Action => Command,
         Custom => Custom_Label_Expansion'Access,
         Filter => Filter);

      Command := new Set_Value_Command;
      Register_Contextual_Menu
        (Kernel, "Debug set value",
         Label  => -"Debug/Set value of %e",
         Action => Command,
         Filter => Action_Filter (Debugger_Filter and Printable_Filter));

      Command := new View_Memory_Command;
      Register_Contextual_Menu
        (Kernel, "Debug view memory",
         Label  => -"Debug/View memory at address of %e",
         Action => Command,
         Filter => Action_Filter (Debugger_Filter and Printable_Filter));

      Command := new Set_Breakpoint_Command;
      Register_Contextual_Menu
        (Kernel, "Debug set subprogram breakpoint",
         Label  => -"Debug/Set breakpoint on %e",
         Action => Command,
         Filter => Action_Filter (Debugger_Filter and Subprogram_Filter));

      Command := new Set_Breakpoint_Command;
      Set_Breakpoint_Command (Command.all).On_Line := True;
      Register_Contextual_Menu
        (Kernel, "Debug set line breakpoint",
         Label  => -"Debug/Set breakpoint on line %l",
         Action => Command,
         Filter => Debugger_Filter);

      Command := new Set_Breakpoint_Command;
      Set_Breakpoint_Command (Command.all).On_Line := True;
      Set_Breakpoint_Command (Command.all).Continue_Till := True;
      Register_Contextual_Menu
        (Kernel, "Debug continue until",
         Label  => -"Debug/Continue until line %l",
         Action => Command,
         Filter => Debugger_Filter);

      Command := new Show_Location_Command;
      Register_Contextual_Menu
        (Kernel, "Debug show current location",
         Label  => -"Debug/Show current location",
         Action => Command,
         Filter => Debugger_Filter);

      Register_Menu (Kernel, Debug, Ref_Item => -"Tools");

      --  Dynamic Initialize menu
      Mitem := Register_Menu (Kernel, Debug, -"Initialize", "", null,
                              Ref_Item => -"Data");
      Gtk_New (Menu);
      Set_Submenu (Mitem, Menu);
      GVD_Module_ID.Initialize_Menu := Menu;

      Add_Hook (Kernel, Project_View_Changed_Hook,
                Wrapper (On_View_Changed'Access),
                Name => "gvd.project_view_changed");

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
      Register_Menu (Kernel, Debug_Sub, -"Debug C_ore File...", "",
                     On_Load_Core'Access);
      Register_Menu (Kernel, Debug_Sub, -"_Kill", "",
                     On_Kill'Access);

      GVD.Canvas.Register_Module (Kernel);
      GVD.Call_Stack.Register_Module (Kernel);
      GVD.Consoles.Register_Module (Kernel);
      GVD.Dialogs.Register_Module (Kernel);

      Register_Menu (Kernel, Data_Sub, -"A_ssembly", "", On_Assembly'Access);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Edit _Breakpoints", "",
                     On_Edit_Breakpoints'Access);
      Register_Menu (Kernel, Data_Sub, -"Examine _Memory", "",
                     On_Examine_Memory'Access);
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

      Register_Menu (Kernel, Debug, -"Te_rminate Current", "",
                     On_Debug_Terminate_Current'Access, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Ter_minate", "",
                     On_Debug_Terminate'Access, Sensitive => False);

      Set_Sensitive (Kernel_Handle (Kernel), Debug_None);

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "gvd.preferences_changed");
      Add_Hook (Kernel, Debugger_Executable_Changed_Hook,
                Wrapper (On_Executable_Changed'Access),
                Name => "gvd.debugger_ext_changed");
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out GVD_Module_Record) is
   begin
      Debug_Terminate (Get_Kernel (Id));
   end Destroy;

   ----------------
   -- Get_Module --
   ----------------

   function Get_Module return GPS.Kernel.Modules.Module_ID is
   begin
      return Debugger_Module_ID;
   end Get_Module;

end GVD_Module;
