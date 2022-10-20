------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2022, AdaCore                     --
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

package body DAP.Preferences is

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs    : access Preferences_Manager_Record'Class;
      Base_Dir : Virtual_File) is
   begin
      DAP_Adapter := Create
        (Manager => Prefs,
         Name    => "DAP-Adapter",
         Label   => "DAP Adapter",
         Doc     => "The adapter for the DAP protocol",
         Default => "/usr/bin/node " &
         (+Base_Dir.Create_From_Dir ("gdb -i=dap").Full_Name),
         Path    => "Debugger:General");

      Preserve_State_On_Exit := Create
        (Manager    => Prefs,
         Name       => "Debugger-Preserve_State-On-Exit",
         Label      => "Preserve state on exit",
         Path       => "Debugger:General",
         Doc        =>
            "Save breakpoints and data window on exit, and restore them"
              & " when debugging the same executable.",
         Default    => True);

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
         Label    => "Display console layer",
         Doc      => "Display console layer in the console.");

      Debugger_Console_Stdout := Create_Invisible_Pref
        (Manager  => Prefs,
         Name     => "debugger-console-stdout",
         Default  =>  False,
         Label    => "Display stdout layer",
         Doc      => "Display stdout layer in the console.");
   end Register_Default_Preferences;

end DAP.Preferences;
