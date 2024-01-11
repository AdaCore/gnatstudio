------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2023, AdaCore                     --
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

with Config;           use Config;

package body DAP.Modules.Preferences is

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class) is
   begin
      DAP_Adapter := Create
        (Manager => Prefs,
         Name    => "DAP-Adapter",
         Label   => "DAP Adapter",
         Doc     => "The adapter for the DAP protocol",
         Default => "",
         Path    => "Debugger:General");

      Break_On_Exception := Create
        (Manager   => Prefs,
         Name      => "Debugger-Break-On-Exception",
         Path      => "Debugger:General",
         Label     => "Break on exceptions",
         Doc       =>
           "Stop when an exception is raised. Changes to this setting are"
             & " ignored by debuggers already running.",
         Default   => False);

      Preserve_State_On_Exit := Create
        (Manager    => Prefs,
         Name       => "Debugger-Preserve_State-On-Exit",
         Label      => "Preserve state on exit",
         Path       => "Debugger:General",
         Doc        =>
            "Save breakpoints and data window on exit, and restore them"
              & " when debugging the same executable.",
         Default    => True);

      Execution_Window := Create
        (Manager   => Prefs,
         Name      => "Debugger-Execution-Window",
         Label     => "Execution window",
         Path      => (if Support_Execution_Window
                       then "Debugger:General" else ":Debugger"),
         Doc       => "Open a separate window to show output of debuggee.",
         Default   => Support_Execution_Window);

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

      Assembly_Range_Size := Create
        (Manager  => Prefs,
         Name     => "Debugger-Assembly-Range-Size",
         Path     => "Debugger:Assembly",
         Label    => "Assembly range size",
         Doc      =>
         "Number of lines to display initially (0 to show whole subprogram).",
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 200);

      Asm_Show_Addresses := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-show-addresses",
         Label    => "Show addresses",
         Default  => True);

      Asm_Show_Offset := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-show-offset",
         Label    => "Show offsets",
         Default  => True);

      Asm_Show_Opcodes := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "assembly_view-show-opcodes",
         Label    => "Show opcodes",
         Default  => False);

      Continue_To_Line_Buttons := Create
        (Manager   => Prefs,
         Name      => "Debugger-Continue-To-Line-Buttons",
         Path      => "Debugger:Editors",
         Label     => "Display 'Continue to line' buttons",
         Doc       =>
           "Display the 'Continue to line' buttons on the left-side of "
           & "editors.",
         Default   => True);

      Debugger_Console_Console := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "debugger-console-console",
         Default  =>  True,
         Label    => "Display console output category",
         Doc      => "Display the DAP 'console' output category.");

      Debugger_Console_In_Out := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "debugger-console-in-out",
         Default  =>  False,
         Label    => "Display DAP in/out JSON communication",
         Doc      => "Display the IN/OUT JSON communication with the "
         & "DAP server.");

      Pending_Breakpoints := Create
        (Manager   => Prefs,
         Name      => "Debugger-Pending-Breakpoints",
         Path      => "Debugger:General",
         Label     => "Pending breakpoints",
         Doc       =>
           "Enable pending breakpoints. A pending breakpoint will not be "
           & "removed when debugger can't set it.",
         Default   => True);

      Breakpoints_For_All_Debuggers  := Create
        (Manager   => Prefs,
         Name      => "Debugger-For_All-Breakpoints",
         Path      => "Debugger:General",
         Label     => "Breakpoints for all debuggers",
         Doc       =>
           "Add/Edit/Delete breakpoints for all debuggers. "
           & "Use only current debugger if not.",
         Default   => False);

      Memory_View_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Memory-View-Color",
         Path     => "Debugger:Memory",
         Label    => "Memory color",
         Doc      => "Default color in memory view.",
         Default  => "#333399");

      Memory_Highlighted_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Memory-Highlighted-Color",
         Path     => "Debugger:Memory",
         Label    => "Memory highlighting",
         Doc      => "Color used for highlighted items in the memory view.",
         Default  => "#DDDDDD");

      Memory_Selected_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Memory-Selected-Color",
         Path     => "Debugger:Memory",
         Label    => "Memory selection",
         Doc      => "Color used for selected items in the memory view.",
         Default  => "#FF0000");

      Memory_Auto_Refresh := Create
        (Manager   => Prefs,
         Name      => "Debugger-Memory-Auto-Refresh",
         Label     => "Refresh memory view after each step",
         Doc       => "Auto-refresh the contents of memory view.",
         Path      => "Debugger:Memory",
         Default   => True);

      Registers_Type := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "registers_view-type",
         Label    => "Type",
         Default  => False);

   end Register_Default_Preferences;

end DAP.Modules.Preferences;
