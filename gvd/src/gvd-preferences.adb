------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GNATCOLL.Traces;

with Config;           use Config;

with GPS.Intl;         use GPS.Intl;
with GPS.Kernel;

package body GVD.Preferences is

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class)
   is
      Debugger_Page : constant Preferences_Page := Prefs.Get_Registered_Page
        (Name             => "Debugger",
         Create_If_Needed => True);
      General_Group : constant Preferences_Group :=
                        new Preferences_Group_Record;
   begin
      --  Make sure that the "General" preferences group is displayed at the
      --  top of the page.
      Debugger_Page.Register_Group
        (Name             => "General",
         Group            => General_Group,
         Priority         => 1);

      Break_On_Exception := Create
        (Manager   => Prefs,
         Name      => "Debugger-Break-On-Exception",
         Path      => -"Debugger:General",
         Label     => -"Break on exceptions",
         Doc       =>
           -("Stop when an exception is raised. Changes to this setting are"
             & " ignored by debuggers already running."),
         Default   => False);

      Open_Main_Unit := Create
        (Manager    => Prefs,
         Name       => "Debugger-Open-Main-Unit",
         Label      => -"Always open main unit",
         Path      => -"Debugger:General",
         Doc        => -("Open the main unit when initializing a debugger."),
         Default    => True);

      Execution_Window := Create
        (Manager   => Prefs,
         Name      => "Debugger-Execution-Window",
         Label     => -"Execution window",
         Path      => (if Support_Execution_Window
                       then -"Debugger:General" else ":Debugger"),
         Doc       =>
           -("Open a separate window to show output of debuggee."),
         Default   => Support_Execution_Window);

      Preserve_State_On_Exit := Create
        (Manager    => Prefs,
         Name       => "Debugger-Preserve_State-On-Exit",
         Label      => -"Preserve state on exit",
         Path       => -"Debugger:General",
         Doc        =>
            -("Save breakpoints and data window on exit, and restore them"
              & " when debugging the same executable."),
         Default    => True);

      Load_Executable_On_Init := Create
        (Manager   => Prefs,
         Name      => "Debugger-Load-On-Init",
         Path      => -"Debugger:General",
         Label     => -"Load executable on init",
         Doc       =>
            -("Load the currently debugged executable to the target when " &
               "initializing a remote debugging session."),
         Default   => False);

      Cancel_Multiple_Symbols := Create
        (Manager   => Prefs,
         Name      => "Debugger-Cancel-Multiple-Symbols",
         Label     => -"No multi-choice dialogs",
         Doc       =>
           "Send 'set multiple-symbols cancel' to gdb on startup, " &
           "to deactivate multiple-choice dialogs",
         Path      => -"Debugger:General",
         Default   => not GNATCOLL.Traces.Active
           (GPS.Kernel.Testsuite_Handle));

      Debugger_Kind := Debugger_Kind_Preferences.Create
        (Manager => Prefs,
         Name    => "GPS6-Debugger-Debugger-Kind",
         Label   => -"Debugger kind",
         Path    => "Debugger:General",
         Doc     => -("Prefered kind of debugger spawned by GPS. Project file"
                        & " settings may override this."),
         Default => GVD.Types.Gdb);

      if Config.Host /= Config.Unknown then
         Debugger_Kind.Hide (GVD.Types.LLDB);
      end if;

      Continue_To_Line_Buttons := Create
        (Manager   => Prefs,
         Name      => "Debugger-Continue-To-Line-Buttons",
         Path      => -"Debugger:Editors",
         Label     => -"Display 'Continue to line' buttons",
         Doc       =>
           -("Display the 'Continue to line' buttons on the left-side of "
           & "editors."),
         Default   => True);

      Assembly_Range_Size := Create
        (Manager  => Prefs,
         Name     => "Debugger-Assembly-Range-Size",
         Path     => -"Debugger:Assembly",
         Label    => -"Assembly range size",
         Doc      =>
         -"Number of lines to display initially (0 to show whole subprogram).",
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 200);

      Asm_Show_Addresses := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-show-addresses",
         Label    => -"Show addresses",
         Default  => True);

      Asm_Show_Offset := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-show-offset",
         Label    => -"Show offsets",
         Default  => True);

      Asm_Show_Opcodes := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-show-opcodes",
         Label    => -"Show opcodes",
         Default  => False);

      Asm_Highlight_Instructions := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-highlight-instructions",
         Label    => -"Highlight instructions",
         Default  => True);

      Memory_View_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Memory-View-Color",
         Path     => -"Debugger:Memory",
         Label    => -"Memory color",
         Doc      => -"Default color in memory view.",
         Default  => "#333399");

      Memory_Highlighted_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Memory-Highlighted-Color",
         Path     => -"Debugger:Memory",
         Label    => -"Memory highlighting",
         Doc      => -"Color used for highlighted items in the memory view.",
         Default  => "#DDDDDD");

      Memory_Selected_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Memory-Selected-Color",
         Path     => -"Debugger:Memory",
         Label    => -"Memory selection",
         Doc      => -"Color used for selected items in the memory view.",
         Default  => "#FF0000");

      Memory_Auto_Refresh := Create
        (Manager   => Prefs,
         Name      => "Debugger-Memory-Auto-Refresh",
         Label     => -"Refresh memory view after each step",
         Doc       => -"Auto-refresh the contents of memory view.",
         Path      => -"Debugger:Memory",
         Default   => True);

      Registers_Hexadecimal := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-hexadecimal",
         Label    => -"Hexadecimal",
         Default  => True);

      Registers_Octal := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-octal",
         Label    => -"Octal",
         Default  => False);

      Registers_Binary := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-binary",
         Label    => -"Binary",
         Default  => False);

      Registers_Decimal := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-decimal",
         Label    => -"Decimal",
         Default  => False);

      Registers_Raw := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-raw",
         Label    => -"Raw",
         Default  => False);

      Registers_Natural := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-natural",
         Label    => -"Natural",
         Default  => False);

      Debugger_Console_All_Interactions := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "debugger-all-interactions",
         Default  =>  False,
         Label    => -"All interactions",
         Doc      => -"Display all interactions in the console");

      Frames_Limit := Create
        (Manager  => Prefs,
         Name     => "debugger-frames-limit",
         Path     => "Debugger:Call Stack",
         Label    => "Frames limit",
         Doc      => "How many frames will be fetched at one time" &
           " (unlimited - 0).",
         Minimum  => 0,
         Maximum  => Integer'Last,
         Default  => 0);

   end Register_Default_Preferences;

end GVD.Preferences;
