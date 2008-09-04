-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2008, AdaCore              --
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

--  This package contains the implementation for a specific scripting language,
--  the simple GPS shell.

with System;                    use System;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Scripts;              use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Impl;         use GNATCOLL.Scripts.Impl;
with GNATCOLL.Scripts.Shell;        use GNATCOLL.Scripts.Shell;

with Glib.Xml_Int;              use Glib.Xml_Int;
with Glib.Object;               use Glib.Object;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Object;                use Gtk.Object;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel;                use GPS.Kernel;
with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with Histories;                 use Histories;
with Interactive_Consoles;      use Interactive_Consoles;
with String_Utils;              use String_Utils;

package body Shell_Script is

   -------------------------
   -- Shell_GPS_Scripting --
   -------------------------

   type Shell_GPS_Scripting_Record
     is new Shell_Scripting_Record with null record;
   overriding function Create
     (Script          : access Shell_GPS_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class;
   --  Create our own callback_data

   ----------------------
   -- Shell_Subprogram --
   ----------------------

   type Shell_GPS_Subprogram_Record is new Shell_Subprogram_Record
      with null record;
   type Shell_GPS_Subprogram is access all Shell_GPS_Subprogram_Record'Class;
   --  A subprogram that executes its command as a GPS action

   overriding function Execute
     (Subprogram : access Shell_GPS_Subprogram_Record;
      Args       : Callback_Data'Class) return String;
   overriding function Get_Name
     (Subprogram : access Shell_GPS_Subprogram_Record) return String;
   --  See doc from inherited subprograms

   -------------------
   -- Callback_Data --
   -------------------

   type Shell_GPS_Callback_Data is new Shell_Callback_Data with null record;
   overriding function Nth_Arg
     (Data : Shell_GPS_Callback_Data; N : Positive) return Subprogram_Type;
   --  See doc from inherited subprogram

   procedure Module_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Internal handler for the shell functions defined in this module

   -------------
   -- Console --
   -------------

   type Shell_Console_Record
      is new Interactive_Console_Record with null record;
   --  The shell console. This is mostly use to have a unique tag when saving
   --  the console.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Support functions for saving the desktop

   function Get_Or_Create_Console (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child;
   --  Return a handle to the shell console, or create a new one if necessary

   procedure Open_Shell_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Open a new shell console (if none exists). This is a callback for the
   --  menu bar items.

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Shell_Console" then
         return Get_Or_Create_Console (User);
      end if;
      return null;
   end Load_Desktop;

   ---------------------------
   -- Get_Or_Create_Console --
   ---------------------------

   function Get_Or_Create_Console (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child
   is
      Script  : constant Shell_Scripting := Shell_Scripting
        (Lookup_Scripting_Language (Get_Scripts (Kernel), GPS_Shell_Name));
      Virtual : constant Virtual_Console := Get_Default_Console (Script);
      Child   : GPS_MDI_Child;
      Console : Interactive_Console;
   begin
      if Virtual = null then
         Console := new Shell_Console_Record;
         Initialize
           (Console      => Console,
            Prompt       => "GPS> ",
            User_Data    => Kernel.all'Address,
            Font         => Default_Style.Get_Pref_Font,
            History_List => Get_History (Kernel),
            Key          => "shell",
            Wrap_Mode    => Wrap_Char);
         Gtk_New (Child, Console,
                  Default_Width       => 400,
                  Default_Height      => 120,
                  Focus_Widget        => Gtk_Widget (Get_View (Console)),
                  Group               => Group_Consoles,
                  Module              => null,
                  Desktop_Independent => True);
         Set_Title (Child, -"Shell");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Bottom);

         Set_Default_Console (Script, Get_Or_Create_Virtual_Console (Console));
      else
         Child := GPS_MDI_Child
           (Find_MDI_Child (Get_MDI (Kernel), Get_Console (Virtual)));
      end if;

      Raise_Child (Child);
      return MDI_Child (Child);
   end Get_Or_Create_Console;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Shell_Console_Record'Class then
         N := new Node;
         N.Tag := new String'("Shell_Console");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ------------------------
   -- Open_Shell_Console --
   ------------------------

   procedure Open_Shell_Console
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child : MDI_Child;
      pragma Unreferenced (Widget, Child);
   begin
      Child := Get_Or_Create_Console (Kernel);
   end Open_Shell_Console;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Script : Shell_Scripting;
   begin
      Script := new Shell_GPS_Scripting_Record;
      Register_Shell_Scripting (Get_Scripts (Kernel), Script);
      Set_Prompt (Script, "GPS>");

      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu
        (Kernel,
         Parent_Path => "/" & (-"_Tools") & '/' & (-"_Consoles"),
         Text        => -"_GPS Shell",
         Callback    => Open_Shell_Console'Access);

      --  Only remember the last 100 commands.
      Set_Max_Length (Get_History (Kernel).all, 100, "shell");
      Allow_Duplicates (Get_History (Kernel).all, "shell", True, True);

      --  The following commands are specific to the GPS shell script.
      Register_Command
        (Script, "help",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Handler      => Module_Command_Handler'Access);
   end Register_Module;

   ----------------------------
   -- Module_Command_Handler --
   ----------------------------

   procedure Module_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);

   begin
      if Command = "help" then
         if Number_Of_Arguments (Data) = 0 then
            Insert_Text
              (Get_Script (Data), null,
               -"The following commands are defined:" & ASCII.LF);
            List_Commands (Shell_Scripting (Get_Script (Data)), null);
            Insert_Text
              (Get_Script (Data), null,
               -"Type ""help <cmd>"" to get help about a specific command."
               & ASCII.LF);

         else
            declare
               Usage  : constant String := Execute_GPS_Shell_Command
                 (Kernel  => Kernel,
                  Command => "Help; Help.getdoc %1 GPS." & Nth_Arg (Data, 1));

               --  Needs to be executed separately, or we wouldn't get output
               --  in Usage.
               Ignored : constant String := Execute_GPS_Shell_Command
                 (Kernel  => Kernel,
                  Command => "Help.reset %2");
               pragma Unreferenced (Ignored);
            begin
               Insert_Text
                 (Get_Script (Data), null,
                  -("Usage: ") & Nth_Arg (Data, 1) & ASCII.LF & Usage);
            end;
         end if;
      end if;
   end Module_Command_Handler;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Script          : access Shell_GPS_Scripting_Record;
      Arguments_Count : Natural) return Callback_Data'Class
   is
      Data : Shell_GPS_Callback_Data;
   begin
      Initialize (Data, Script, Arguments_Count);
      return Data;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Subprogram : access Shell_GPS_Subprogram_Record;
      Args       : Callback_Data'Class) return String
   is
      D    : constant Shell_Callback_Data := Shell_Callback_Data (Args);
      Custom : Command_Access;
      A    : constant Action_Record_Access := Lookup_Action
        (Get_Kernel (Args), Get_Command (Subprogram));
   begin
      Custom := Create_Proxy
        (A.Command,
         Context => (Event       => null,
                     Context     => No_Context,
                     Synchronous => True,
                     Dir         => null,
                     Args        => new Argument_List'(Clone (Get_Args (D))),
                     Label       => null,
                     Repeat_Count => 1,
                     Remaining_Repeat => 0));

      Launch_Background_Command
        (Kernel          => Get_Kernel (Args),
         Command         => Custom,
         Active          => True,
         Show_Bar        => True,
         Queue_Id        => "",
         Destroy_On_Exit => True);

      --  ??? Should evaluate output properly, but we are in asynchronous mode
      --  ??? In fact, this is no longer true if we use Active set to False
      --  above, since we now know how to execute external actions
      --  synchronously
      return "";
   end Execute;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Subprogram : access Shell_GPS_Subprogram_Record) return String is
   begin
      return "action: " & Get_Command (Subprogram);
   end Get_Name;

   -------------
   -- Nth_Arg --
   -------------

   overriding function Nth_Arg
     (Data : Shell_GPS_Callback_Data; N : Positive) return Subprogram_Type
   is
      A    : Action_Record_Access;
      Name : constant String := Nth_Arg (Data, N);
      Subp : Shell_GPS_Subprogram;
   begin
      A := Lookup_Action (Get_Kernel (Data), Name);
      if A = null then
         raise Invalid_Parameter;
      else
         Subp := new Shell_GPS_Subprogram_Record;
         Initialize (Subp.all, Get_Script (Data), Command => Name);
         return Subprogram_Type (Subp);
      end if;
   end Nth_Arg;

end Shell_Script;
