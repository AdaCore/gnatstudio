------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;

with Gdk.Color;                 use Gdk.Color;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Cairo;                     use Cairo;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Dialog;                use Gtk.Dialog;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item;   use Gtk.Separator_Tool_Item;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tool_Button;           use Gtk.Tool_Button;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Handlers;           use Gtkada.Handlers;

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
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
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
with Std_Dialogs;               use Std_Dialogs;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Editors; use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body GVD_Module is
   Me : constant Debug_Handle := Create ("GVD_MODULE");

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
   overriding procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_edited" hook

   type Lines_Revealed_Hook_Record is new Function_With_Args with null record;
   type Lines_Revealed_Hook is access Lines_Revealed_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "source_lines_revealed_hook" hook

   type GVD_Module_Record is new Module_ID_Record with record
      Initialized                    : Boolean := False;
      --  Whether the debugger is running

      Show_Lines_With_Code           : Boolean;
      --  Whether the lines with code should be explicitly queried

      Initialize_Menu                : Gtk_Menu;

      Delete_Id                      : Handler_Id := (Null_Handler_Id, null);
      File_Hook                      : File_Edited_Hook;
      Lines_Hook                     : Lines_Revealed_Hook;

      Space_Separator                : Gtk_Separator_Tool_Item;
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
   end record;
   type GVD_Module is access all GVD_Module_Record'Class;

   overriding procedure Destroy (Id : in out GVD_Module_Record);
   --  Terminate the debugger module, and kill the underlying debugger

   overriding function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : Selection_Context) return Cairo_Surface;
   --  See inherited documentation

   GVD_Module_Name : constant String := "Debugger";
   GVD_Module_ID   : GVD_Module;

   procedure Add_Debug_Buttons (Kernel : access Kernel_Handle_Record'Class);
   --  Add debugger related buttons to the main toolbar

   procedure Remove_Debug_Buttons
     (Kernel : access Kernel_Handle_Record'Class);
   --  Remove debugger related buttons from the main toolbar

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
      File   : GNATCOLL.VFS.Virtual_File);
   --  Create the side information columns corresponding to the debugger
   --  in the editors for file.
   --  If File is empty, create them for all files.

   procedure Debug_Init
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data   : File_Project_Record;
      Args   : String);
   --  Initialize the debugger

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
   --  For non VxWorks systems, a check box to allow the user to run under the
   --  exec dir of the program is also provided, similar to the Build->Run
   --  dialog.

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
   --  Debug->Run... menu

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
                           (Overloaded_Entity    => True,
                            Unresolved_Entity    => True,
                            Access_Kind          => True,
                            Array_Kind           => True,
                            Boolean_Kind         => True,
                            Class_Wide           => True,
                            Class                => True,
                            Decimal_Fixed_Point  => True,
                            Enumeration_Literal  => True,
                            Enumeration_Kind     => True,
                            Exception_Entity     => True,
                            Floating_Point       => True,
                            Modular_Integer      => True,
                            Named_Number         => True,
                            Ordinary_Fixed_Point => True,
                            Record_Kind          => True,
                            Signed_Integer       => True,
                            String_Kind          => True,
                            others               => False);
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
   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean;

   type Printable_Variable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Printable_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Access_Variable_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Access_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Subprogram_Variable_Filter
     is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Variable_Filter;
      Context : Selection_Context) return Boolean;

   type Print_Variable_Command is new Interactive_Command with record
      Display     : Boolean := False;
      Dereference : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Value_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_Location_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Set_Breakpoint_Command is new Interactive_Command with record
      On_Line       : Boolean := False;  --  If False, on entity
      Continue_Till : Boolean := False;  --  Continue until given line ?
   end record;
   overriding function Execute
     (Command : access Set_Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   function Custom_Label_Expansion
     (Context : Selection_Context) return String;
   --  Provide expansion for "$!" in the labels for contextual menus

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
      Trace (Me, "Set_Current_Debugger");
      GVD_Module_ID.Current_Debugger := Current;
   end Set_Current_Debugger;

   -----------------------
   -- Add_Debug_Buttons --
   -----------------------

   procedure Add_Debug_Buttons (Kernel : access Kernel_Handle_Record'Class) is
      Toolbar : constant Gtk_Toolbar  := Get_Toolbar (Kernel);
      Window  : constant Gtk_Window := Get_Main_Window (Kernel);

   begin
      if GVD_Module_ID.Cont_Button /= null then
         return;
      end if;

      Gtk_New (GVD_Module_ID.Space_Separator);
      Set_Draw (GVD_Module_ID.Space_Separator, True);
      Insert (Toolbar, GVD_Module_ID.Space_Separator);
      Show_All (GVD_Module_ID.Space_Separator);

      Gtk_New_From_Stock (GVD_Module_ID.Cont_Button, "gps-debugger-run");
      Set_Name (GVD_Module_ID.Cont_Button, "gps-debugger-run-button");
      Set_Tooltip_Text
        (GVD_Module_ID.Cont_Button,
         -"Start/Continue the debugged program");
      Insert (Toolbar, GVD_Module_ID.Cont_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Cont_Button, Signal_Clicked,
         On_Start_Continue'Access, Window);
      Show_All (GVD_Module_ID.Cont_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Step_Button, "gps-debugger-step");
      Set_Name (GVD_Module_ID.Step_Button, "gps-debugger-step-button");
      Set_Tooltip_Text (GVD_Module_ID.Step_Button, -"Step");
      Insert (Toolbar, GVD_Module_ID.Step_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Step_Button, Signal_Clicked, On_Step'Access, Window);
      Show_All (GVD_Module_ID.Step_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Next_Button, "gps-debugger-next");
      Set_Name (GVD_Module_ID.Next_Button, "gps-debugger-next-button");
      Set_Tooltip_Text (GVD_Module_ID.Next_Button, -"Next");
      Insert (Toolbar, GVD_Module_ID.Next_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Next_Button, Signal_Clicked, On_Next'Access, Window);
      Show_All (GVD_Module_ID.Next_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Finish_Button, "gps-debugger-finish");
      Set_Name (GVD_Module_ID.Finish_Button, "gps-debugger-finish-button");
      Set_Tooltip_Text
        (GVD_Module_ID.Finish_Button,
         -"Execute until selected stack frame returns");
      Insert (Toolbar, GVD_Module_ID.Finish_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Finish_Button,
         Signal_Clicked, On_Finish'Access, Window);
      Show_All (GVD_Module_ID.Finish_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Up_Button, "gps-debugger-up");
      Set_Name (GVD_Module_ID.Up_Button, "gps-debugger-up-button");
      Set_Tooltip_Text
        (GVD_Module_ID.Up_Button,
         -"Select and print stack frame that called this one");
      Insert (Toolbar, GVD_Module_ID.Up_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Up_Button, Signal_Clicked, On_Up'Access, Window);
      Show_All (GVD_Module_ID.Up_Button);

      Gtk_New_From_Stock (GVD_Module_ID.Down_Button, "gps-debugger-down");
      Set_Name (GVD_Module_ID.Down_Button, "gps-debugger-down-button");
      Set_Tooltip_Text (GVD_Module_ID.Down_Button,
                        -"Select and print stack frame called by this one");
      Insert (Toolbar, GVD_Module_ID.Down_Button);
      Widget_Callback.Object_Connect
        (GVD_Module_ID.Down_Button, Signal_Clicked, On_Down'Access, Window);
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
         Remove (Toolbar, GVD_Module_ID.Space_Separator);
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
         File_Project_Record'(Get_Project (Kernel), No_File), Args);
   end Initialize_Debugger;

   -----------------------------
   -- Create_Debugger_Columns --
   -----------------------------

   procedure Create_Debugger_Columns
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) is
   begin
      --  Create the information column for the current line
      Create_Line_Information_Column
        (Kernel,
         File,
         "Current Line",
         Empty_Line_Information,
         --  ??? That should be centralized somewhere !!!
         Every_Line    => False);

      --  Create the information column for the breakpoints
      Create_Line_Information_Column
        (Kernel,
         File,
         Breakpoints_Column_Id,
         Empty_Line_Information,
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

      Top     : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

      use GNAT.Strings;

   begin
      declare
         S : constant Virtual_File :=
               Select_File
                 (Title             => -"Select Module",
                  Parent            => Gtk_Window (Top),
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  File_Pattern      => "*",
                  Pattern_Name      => -"All files",
                  History           => Get_History (Kernel));
      begin
         if S = GNATCOLL.VFS.No_File then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            declare
               Addr : constant String :=
                        Query_User (Gtk_Window (Top),
                                    -"Enter starting address of module's text"
                                    & ASCII.LF
                                    & (-"Optionally leave empty on VxWorks"),
                                    False, False, "0");

            begin
               Add_Symbols
                 (Process.Debugger,
                  Module  => S,
                  Address => Addr,
                  Mode    => GVD.Types.Visible);
            end;

         else
            Console.Insert
              (Kernel, (-"Could not find file: ") & S.Display_Full_Name,
               Mode => Error);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Add_Symbols;

   ---------------
   -- On_Attach --
   ---------------

   procedure On_Attach
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top          : constant GPS_Window :=
                       GPS_Window (Get_Main_Window (Kernel));
      Process      : constant Visual_Debugger := Get_Current_Process (Top);
      Process_List : List_Select_Access;
      Success      : Boolean;
      Info         : Process_Info;
      Ignore       : Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Ignore := Message_Dialog
           ((-"Cannot attach to a task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);
         return;
      end if;

      Gtk_New
        (Process_List,
         Title         => -"Process Selection",
         Item_Label    => -"Pid",
         Comment_Label => -"Command");

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
      when E : others => Trace (Exception_Handle, E);
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
      Ignore  : Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Ignore := Message_Dialog
           ((-"Cannot detach the task/process while the") & ASCII.LF &
            (-"underlying debugger is busy.") & ASCII.LF &
            (-"Interrupt the debugger or wait for its availability."),
           Dialog_Type => Warning,
           Buttons => Button_OK);

      else
         Detach_Process (Process.Debugger, Mode => GVD.Types.Visible);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Interrupt;

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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end On_Display_Expression;

   -------------------
   -- Start_Program --
   -------------------

   procedure Start_Program
     (Process : Visual_Debugger; Start_Cmd : Boolean := False)
   is
      Is_Multitask : aliased Boolean := False;
      Is_Start     : aliased Boolean := False;
      Use_Exec_Dir : aliased Boolean := False;
      Arg_Msg      : aliased String := -"Run arguments:";
      Entry_Msg    : aliased String := -"Entry point and arguments:";
      Start_Msg    : aliased String := -"Stop at beginning of main subprogram";
      Dir_Msg      : aliased String := -"Use exec dir instead of current dir";
      Multi_Msg    : aliased String := -"Enable VxWorks multi-tasks mode";
      Start_Key    : aliased String := "stop_beginning_debugger";
      Dir_Key      : aliased String := "run_in_executable_directory";
      Multi_Key    : aliased String := "multitask_mode_debugger";
      No_Msg       : aliased String := "";
      Button1      : Boolean_Access := null;
      Button2      : Boolean_Access := null;
      Cmd_Msg      : GNAT.Strings.String_Access := Arg_Msg'Unchecked_Access;
      Msg1         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Msg2         : GNAT.Strings.String_Access;
      Key1         : GNAT.Strings.String_Access := No_Msg'Unchecked_Access;
      Key2         : GNAT.Strings.String_Access;

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

      if VxWorks_Version (Process.Debugger) in Vx5 .. Vx6 then
         --  Change the message in the dialog window indicating that the entry
         --  point needs to be specified together with the arguments.

         Cmd_Msg := Entry_Msg'Unchecked_Access;

         Button2 := Is_Multitask'Unchecked_Access;
         Msg2    := Multi_Msg'Unchecked_Access;
         Key2    := Multi_Key'Unchecked_Access;

      else
         Button2 := Use_Exec_Dir'Unchecked_Access;
         Msg2    := Dir_Msg'Unchecked_Access;
         Key2    := Dir_Key'Unchecked_Access;
      end if;

      declare
         Arguments : constant String :=
           Strip_Ending_Linebreaks
             (Display_Entry_Dialog
                (Parent         => Process.Window,
                 Title          => -"Run/Start",
                 Message        => Cmd_Msg.all,
                 Key            => Cst_Run_Arguments_History,
                 History        => Get_History (Process.Window.Kernel),
                 Check_Msg      => Msg1.all,
                 Check_Msg2     => Msg2.all,
                 Button_Active  => Button1,
                 Button2_Active => Button2,
                 Key_Check      => History_Key (Key1.all),
                 Key_Check2     => History_Key (Key2.all)));

      begin
         if Arguments = ""
           or else Arguments (Arguments'First) /= ASCII.NUL
         then
            --  For VxWorks we need to set the desired multi-tasks-mode mode

            if VxWorks_Version (Process.Debugger) in Vx5 .. Vx6 then
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

            if Use_Exec_Dir then
               Change_Directory
                 (Process.Debugger,
                  Process.Descriptor.Program.Dir);
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
      Ignore  : Message_Dialog_Buttons;
      pragma Unreferenced (Ignore);
   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Command_In_Process (Get_Process (Process.Debugger)) then
         Ignore := Message_Dialog
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
      when E : others => Trace (Exception_Handle, E);
   end On_Start;

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
      Ignore       : Gtk_Widget;
      pragma Unreferenced (Widget, Ignore);

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

      Ignore := Add_Button (Dialog, Stock_Ok, Gtk_Response_OK);
      Ignore := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
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
      when E : others => Trace (Exception_Handle, E);
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
      Exec        : Virtual_File;
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
                  File_Pattern      => +("*" & Exec_Suffix & ";*"),
                  Pattern_Name      => -"Executable files;All files",
                  Parent            => Get_Current_Window (Kernel),
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  History           => Get_History (Kernel));
      begin
         if S = GNATCOLL.VFS.No_File then
            return;
         end if;

         if not Is_Regular_File (S) then
            Exec := GNATCOLL.VFS.Locate_On_Path (Base_Name (S));

            if not Is_Regular_File (Exec) then
               Console.Insert
                 (Kernel, (-"Could not find file: ") & Display_Base_Name (S),
                  Mode => Error);
               S := GNATCOLL.VFS.No_File;
            else
               S := Exec;
            end if;
         end if;

         if S /= No_File then
            Set_Executable (Process.Debugger, S, Mode => Hidden);
         end if;

      exception
         when Executable_Not_Found =>
            Console.Insert
              (Kernel, (-"Could not find file: ") & Display_Full_Name (S),
               Mode => Error);
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Debug_Executable;

   ------------------
   -- On_Load_Core --
   ------------------

   procedure On_Load_Core
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top     : constant GPS_Window := GPS_Window (Get_Main_Window (Kernel));
      Process : constant Visual_Debugger := Get_Current_Process (Top);

      use GNAT.Strings;

   begin
      declare
         S : constant Virtual_File :=
               Select_File
                 (Title             => -"Select Core File",
                  File_Pattern      => "core*;*",
                  Pattern_Name      => -"Core files;All files",
                  Parent            => Get_Current_Window (Kernel),
                  Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                  Kind              => Open_File,
                  History           => Get_History (Kernel));
      begin
         if S = GNATCOLL.VFS.No_File then
            return;
         end if;

         if Process.Descriptor.Remote_Host /= null
           or else Is_Regular_File (S)
         then
            Load_Core_File
              (Process.Debugger, S,
               Mode => GVD.Types.Visible);

         else
            Console.Insert
              (Kernel, (-"Could not find core file: ") &
               Display_Full_Name (S),
               Mode => Error);
         end if;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Load_Core;

   ----------------------------
   -- Custom_Label_Expansion --
   ----------------------------

   function Custom_Label_Expansion
     (Context : Selection_Context) return String is
   begin
      return Emphasize (Get_Variable_Name (Context, True));
   end Custom_Label_Expansion;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Location_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Top  : constant GPS_Window :=
               GPS_Window (Get_Main_Window (Get_Kernel (Context.Context)));
      Edit : constant GEdit :=
               GEdit (Get_Source (Get_Current_Process (Top).Editor_Text));
      Name : constant Virtual_File := Get_Current_File (Edit);
   begin
      if Name /= GNATCOLL.VFS.No_File then
         Highlight_Current_Line (Edit);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Print_Variable_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Process  : constant Visual_Debugger :=
                   Get_Current_Process
                     (Get_Main_Window (Get_Kernel (Context.Context)));
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

   overriding function Execute
     (Command : access Set_Value_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process  : constant Visual_Debugger :=
                   Get_Current_Process
                     (Get_Main_Window (Get_Kernel (Context.Context)));
      Variable : constant String :=
                   Get_Variable_Name (Context.Context, False);
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

   overriding function Execute
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
            Contexts.Line_Information (Context.Context),
            Temporary => True);
         Continue (Debugger, Mode => GVD.Types.Visible);
      else
         Break_Source
           (Debugger,
            File_Information (Context.Context),
            Contexts.Line_Information (Context.Context),
            Mode => GVD.Types.Visible);
      end if;

      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Debugger_Active_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Process : constant Visual_Debugger :=
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

   overriding function Filter_Matches_Primitive
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

   overriding function Filter_Matches_Primitive
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

   overriding function Filter_Matches_Primitive
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

   overriding function Tooltip_Handler
     (Module  : access GVD_Module_Record;
      Context : Selection_Context) return Cairo_Surface
   is
      pragma Unreferenced (Module);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Debugger : constant Visual_Debugger :=
        Get_Current_Process (Get_Main_Window (Kernel));
      Value    : GNAT.Strings.String_Access;

   begin
      if Debugger = null
        or else Debugger.Debugger = null
        or else not Has_Entity_Name_Information (Context)
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return Null_Surface;
      end if;

      Push_State (Kernel, Busy);

      declare
         Variable_Name : constant String := Get_Variable_Name
           (Context, Dereference => False);
         Pixmap        : Cairo_Surface;

      begin
         if Variable_Name = ""
           or else not Can_Tooltip_On_Entity
             (Get_Language (Debugger.Debugger), Variable_Name)
         then
            Pop_State (Kernel);
            return Null_Surface;

         else
            Value := new String'(Value_Of (Debugger.Debugger, Variable_Name));
         end if;

         if Value.all /= "" then
            Create_Pixmap_From_Text
              (Text       => Value.all,
               Font       => GPS.Kernel.Preferences.Default_Font.Get_Pref_Font,
               Bg_Color   => White (Get_Default_Colormap),
               Widget     => Get_Main_Window (Kernel),
               Pixmap     => Pixmap,
               Wrap_Width => Max_Tooltip_Width);
         else
            --  Note: if Value.all is "", we will return Pixmap below, hence
            --  the assignment.

            Pixmap := Null_Surface;
         end if;

         GNAT.Strings.Free (Value);
         Pop_State (Kernel);
         return Pixmap;
      end;

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Pop_State (Kernel);
         return Null_Surface;
      when E : others => Trace (Exception_Handle, E);
         Pop_State (Kernel);
         return Null_Surface;
   end Tooltip_Handler;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      State    : Debugger_State)
   is
      Debug     : constant String := '/' & (-"Debug") & '/';
      Available : constant Boolean := State = Debug_Available;
      Sensitive : constant Boolean := State /= Debug_None;
      Item      : Gtk_Menu_Item;

   begin
      if Get_Main_Window (Kernel) /= null
        and then not In_Destruction_Is_Set (Get_Main_Window (Kernel))
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
            Set_Sensitive (Item, Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Data")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Run...")), Available);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Step")), Sensitive);
            Set_Sensitive (Find_Menu_Item
              (Kernel, Debug & (-"Step Instruction")), Sensitive);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Next")), Sensitive);
            Set_Sensitive
              (Find_Menu_Item
                 (Kernel, Debug & (-"Next Instruction")), Sensitive);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Finish")), Sensitive);
            Set_Sensitive
              (Find_Menu_Item (Kernel, Debug & (-"Continue")), Sensitive);
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
      Ignore : Visual_Debugger;
      pragma Unreferenced (Ignore);
   begin
      Ignore := Spawn
        (Kernel, Debugger_Kind.Get_Pref, Data.File,
         Get_Project (Kernel), Args);
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
         --  Add columns information for not currently opened files

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

         Create_Debugger_Columns (Kernel, GNATCOLL.VFS.No_File);
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

      Remove_Debugger_Columns (Kernel, GNATCOLL.VFS.No_File);

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
      when E : others => Trace (Exception_Handle, E);
         Pop_State (Kernel);
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
      when E : others => Trace (Exception_Handle, E);
         Pop_State (Kernel);
   end On_Debug_Terminate_Current;

   -----------------------
   -- Get_Variable_Name --
   -----------------------

   function Get_Variable_Name
     (Context     : Selection_Context;
      Dereference : Boolean) return String
   is
      Lang  : Language_Access;
   begin
      if Context = No_Context then
         return "";
      end if;

      if Has_File_Information (Context) then
         Lang := Get_Language_From_File
           (Get_Language_Handler (Get_Kernel (Context)),
            File_Information (Context));
      end if;

      if Has_Area_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name (Lang, Text_Information (Context));
         end if;

         return Text_Information (Context);
      end if;

      if Has_Expression_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name (Lang, Expression_Information (Context));
         end if;

         return Expression_Information (Context);
      end if;

      if Has_Entity_Name_Information (Context) then
         if Dereference and then Lang /= null then
            return Dereference_Name
              (Lang, Entity_Name_Information (Context));
         end if;

         return Entity_Name_Information (Context);
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
      when E : others => Trace (Exception_Handle, E);
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
      Process : constant Visual_Debugger :=
                  Get_Process (Debugger_Hooks_Data_Access (Data));
   begin
      --  Change the project to match the executable

      Load_Project_From_Executable (Kernel, Process);

      --  Verify the language used in the executable

      Detect_Language (Process.Debugger);

      --  Re-create all debugger columns

      Remove_Debugger_Columns (Kernel_Handle (Kernel), GNATCOLL.VFS.No_File);
      Create_Debugger_Columns (Kernel_Handle (Kernel), GNATCOLL.VFS.No_File);
   end On_Executable_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Edited_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D : constant File_Hooks_Args := File_Hooks_Args (Data.all);
   begin
      Create_Debugger_Columns (Kernel_Handle (Kernel), D.File);

   exception
      when E : others =>  Trace (Exception_Handle, E);
         Close_Debugger (Get_Current_Process (Hook.Top));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Lines_Revealed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Hook);
      D       : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);

      Process : constant Visual_Debugger :=
                  Get_Current_Process
                    (Get_Main_Window (Get_Kernel (D.Context)));

   begin
      if Process = null
        or else Process.Debugger = null
        or else not Has_Area_Information (D.Context)
      then
         return;
      end if;

      declare
         File         : constant Virtual_File := File_Information (D.Context);
         Line1, Line2 : Integer;

      begin
         Get_Area (D.Context, Line1, Line2);

         if GVD_Module_ID.Show_Lines_With_Code
           and then Command_In_Process (Get_Process (Process.Debugger))
         then
            return;
         end if;

         declare
            Tab         : constant Visual_Debugger :=
                            Get_Current_Process (Get_Main_Window (Kernel));
            Lines       : Line_Array (Line1 .. Line2);
            A           : Line_Information_Array (Line1 .. Line2);
            C           : Set_Breakpoint_Command_Access;
            Mode        : Breakpoint_Command_Mode := Set;
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
                    and then Tab.Breakpoints (J).File /= GNATCOLL.VFS.No_File
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
      when E : others => Trace (Exception_Handle, E);
         Close_Debugger (Process);
   end Execute;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed (Kernel : access Kernel_Handle_Record'Class) is
      use GNAT.OS_Lib;
      Menu              : Gtk_Menu renames GVD_Module_ID.Initialize_Menu;
      Mitem             : Gtk_Menu_Item;
      Loaded_Project    : constant Project_Type := Get_Project (Kernel);
      Iter              : Project_Iterator := Loaded_Project.Start;
      Current_Project   : Project_Type := Current (Iter);
      Tmp               : Project_Type;
      Debuggable_Suffix : GNAT.Strings.String_Access := Get_Debuggable_Suffix;

      procedure Add_Entries (Mains : in out Argument_List; Prj : Project_Type);
      --  Add menu entries for all executables in Main. Main is freed on exit

      procedure Add_Entries
        (Mains : in out Argument_List; Prj : Project_Type)
      is
      begin
         for M in reverse Mains'Range loop
            declare
               Exec : constant Filesystem_String :=
                 Prj.Executable_Name (+Mains (M).all);
               Dir  : constant Virtual_File := Executables_Directory (Prj);
               File : Virtual_File;

            begin
               Gtk_New (Mitem, +Exec);
               --  ??? What if Exec is not utf-8 ?
               Prepend (Menu, Mitem);

               if Dir = No_File then
                  File := Create_From_Base (Exec);
               else
                  File := Create_From_Dir (Dir, Exec);
               end if;

               File_Project_Cb.Object_Connect
                 (Mitem, Gtk.Menu_Item.Signal_Activate,
                  On_Debug_Init'Access,
                  Slot_Object => Kernel,
                  User_Data   => File_Project_Record'
                    (Project => No_Project,
                     File    => File));

               --  Only set accelerators for main units of the root project
               if Prj = Loaded_Project then
                  Set_Accel_Path
                    (Mitem, Debug_Menu_Prefix & "item" & Image (M),
                     Get_Default_Accelerators (Kernel));
               end if;
            end;
         end loop;

         Free (Mains);
      end Add_Entries;

   begin
      --  Remove all existing menus

      Remove_All_Children (Menu);

      if Debuggable_Suffix = null then
         Debuggable_Suffix := new String'("");
      end if;

      --  The following loop should be factorized with the one in the builder
      --  module (see Builder_Module.On_View_Changed).

      while Current_Project /= No_Project loop
         --  Never show the main units from an extended project, since we only
         --  look at the actual executables in the extending project

         if Extending_Project (Current_Project) /= No_Project then
            null;

         --  If we have an extending project, the list of main units could
         --  come from the project itself, or be inherited from extended
         --  projects.

         else
            Tmp := Current_Project;
            while Tmp /= No_Project loop
               declare
                  Mains : String_List_Access :=
                    Tmp.Attribute_Value (Main_Attribute);
               begin
                  if Mains /= null and then Mains'Length /= 0 then
                     --  Basenames inherited, but exec_dir is current project
                     Add_Entries (Mains.all, Current_Project);

                     --  Stop looking in inherited project, since the attribute
                     --  has been overridden.
                     Tmp := No_Project;
                  else
                     Tmp := Extended_Project (Tmp);
                  end if;

                  Free (Mains);
               end;
            end loop;
         end if;

         Next (Iter);
         Current_Project := Current (Iter);
      end loop;

      Free (Debuggable_Suffix);

      --  Specific entry to start the debugger without any main program
      Gtk_New (Mitem, -"<no main file>");
      Append (Menu, Mitem);
      File_Project_Cb.Object_Connect
        (Mitem, Gtk.Menu_Item.Signal_Activate, On_Debug_Init'Access,
         Slot_Object => Kernel,
         User_Data   => File_Project_Record'
           (Project => Get_Project (Kernel),
            File    => GNATCOLL.VFS.No_File));
      Set_Accel_Path (Mitem, Debug_Menu_Prefix & "<no main>",
                      Get_Default_Accelerators (Kernel));
      Show_All (Menu);

   exception
      when E : others => Trace (Exception_Handle, E);
         Debug_Terminate (Kernel_Handle (Kernel));
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
        Editor_Show_Line_With_Code.Get_Pref;

      if GVD_Module_ID.Initialized
        and then Prev /= GVD_Module_ID.Show_Lines_With_Code
      then
         Remove_Debugger_Columns (Kernel_Handle (Kernel), No_File);
         Create_Debugger_Columns (Kernel_Handle (Kernel), No_File);
      end if;

      Init_Graphics (Gtk_Widget (Get_Main_Window (Kernel)));
   end Preferences_Changed;

   -----------------------
   -- Create_GVD_Module --
   -----------------------

   procedure Create_GVD_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      GVD_Module_ID := new GVD_Module_Record;
      Debugger_Module_ID := Module_ID (GVD_Module_ID);
      if Kernel /= null then
         Register_Module
           (Module          => Module_ID (GVD_Module_ID),
            Kernel          => Kernel,
            Module_Name     => GVD_Module_Name,
            Priority        => Default_Priority + 20);
      end if;
   end Create_GVD_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug             : constant String := '/' & (-"_Debug") & '/';
      Debug_Sub         : constant String := Debug & (-"_Debug") & '/';
      Data_Sub          : constant String := Debug & (-"D_ata") & '/';
      Mitem             : Gtk_Menu_Item;
      Sepitem           : Gtk_Separator_Menu_Item;
      Menu              : Gtk_Menu;
      Command           : Interactive_Command_Access;
      Filter            : Action_Filter;
      Debugger_Filter   : Action_Filter;
      Printable_Filter  : Action_Filter;
      Access_Filter     : Action_Filter;
      Subprogram_Filter : Action_Filter;

   begin
      Create_GVD_Module (Kernel);
      GVD.Preferences.Register_Default_Preferences (Get_Preferences (Kernel));
      GVD.Scripts.Create_Hooks (Kernel);
      GVD_Module_ID.Show_Lines_With_Code :=
        Editor_Show_Line_With_Code.Get_Pref;

      Debugger_Filter := new Debugger_Active_Filter;
      Register_Filter (Kernel, Debugger_Filter, "Debugger active");

      Printable_Filter  := new Printable_Variable_Filter;
      Register_Filter
        (Kernel, Printable_Filter, "Debugger printable variable");

      Access_Filter     := new Access_Variable_Filter;
      Subprogram_Filter := new Subprogram_Variable_Filter;

      Register_Contextual_Submenu (Kernel, "Debug", Ref_Item => "References");

      Filter := Debugger_Filter and Printable_Filter;
      Command := new Print_Variable_Command;
      Register_Contextual_Menu
        (Kernel, "Debug print variable",
         Label  => "Debug/Print %S",
         Action => Command,
         Filter => Filter);

      Command := new Print_Variable_Command;
      Print_Variable_Command (Command.all).Display := True;
      Register_Contextual_Menu
        (Kernel, "Debug display variable",
         Label  => "Debug/Display %S",
         Action => Command,
         Filter => Filter);

      Filter := Debugger_Filter and Printable_Filter and Access_Filter;
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
         Label  => -"Debug/Set value of %S",
         Action => Command,
         Filter => Debugger_Filter and Printable_Filter);

      Command := new Set_Breakpoint_Command;
      Register_Contextual_Menu
        (Kernel, "Debug set subprogram breakpoint",
         Label  => -"Debug/Set breakpoint on %e",
         Action => Command,
         Filter => Debugger_Filter and Subprogram_Filter);

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
      GVD.Assembly_View.Register_Module (Kernel);
      Breakpoints_Editor.Register_Module (Kernel);
      GVD.Memory_View.Register_Module (Kernel);

      Gtk_New (Sepitem);
      Register_Menu (Kernel, Data_Sub, Sepitem);
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

      Gtk_New (Sepitem);
      Register_Menu (Kernel, Debug, Sepitem);

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
      Gtk_New (Sepitem);
      Register_Menu (Kernel, Debug, Sepitem);

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

   overriding procedure Destroy (Id : in out GVD_Module_Record) is
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
