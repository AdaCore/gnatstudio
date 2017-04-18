------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Config;   use Config;
with GPS.Intl; use GPS.Intl;

package body GVD.Preferences is

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class) is
   begin
      Break_On_Exception := Create
        (Manager   => Prefs,
         Name      => "Debugger-Break-On-Exception",
         Path      => -"Debugger:Startup & Exit",
         Label     => -"Break on exceptions",
         Doc       =>
           -("Stop when an exception is raised. Changes to this setting are"
             & " ignored by debuggers already running."),
         Default   => False);

      Open_Main_Unit := Create
        (Manager    => Prefs,
         Name       => "Debugger-Open-Main-Unit",
         Label      => -"Always open main unit",
         Path      => -"Debugger:Startup & Exit",
         Doc        => -("Open the main unit when initializing a debugger."),
         Default    => True);

      Execution_Window := Create
        (Manager   => Prefs,
         Name      => "Debugger-Execution-Window",
         Label     => -"Execution window",
         Path      => (if Support_Execution_Window
                       then -"Debugger:Startup & Exit" else ":Debugger"),
         Doc       =>
           -("Open a separate window to show output of debuggee."),
         Default   => Support_Execution_Window);

      Preserve_State_On_Exit := Create
        (Manager    => Prefs,
         Name       => "Debugger-Preserve_State-On-Exit",
         Label      => -"Preserve state on exit",
         Path       => -"Debugger:Startup & Exit",
         Doc        =>
            -("Save breakpoints and data window on exit, and restore them"
              & " when debugging the same executable."),
         Default    => True);

      Load_Executable_On_Init := Create
        (Manager   => Prefs,
         Name      => "Debugger-Load-On-Init",
         Path      => -"Debugger:Startup & Exit",
         Label     => -"Load executable on init",
         Doc       =>
            -("Load the currently debugged executable to the target when " &
               "initializing a remote debugging session."),
         Default   => False);

      Debugger_Kind := Debugger_Kind_Preferences.Create
        (Manager => Prefs,
         Name    => "GPS6-Debugger-Debugger-Kind",
         Label   => -"Debugger kind",
         Path    => "Debugger:Startup & Exit",
         Doc     => -"Kind of debugger spawned by GPS",
         Default => GVD.Types.Gdb);

      Editor_Current_Line_Color := Create
        (Manager   => Prefs,
         Name      => "Debugger-Editor-Current-Line",
         Label     => -"Current line",
         Doc       => -"Color to highlight the current line in editors.",
         Path      => -"Debugger:Editors",
         Default   => "rgba(125,236,57,0.6)");

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

      Asm_Highlight_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Asm-Highlight-Color",
         Path     => -"Debugger:Assembly",
         Label    => -"Current assembly line",
         Doc      => -"Color to highlight current line in assembly view.",
         Default  => "#0000FF");

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

      Change_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Change-Color",
         Path     => -"Debugger:Data View",
         Label    => -"Changed data",
         Doc      => -"Color to highlight modified fields in Data view.",
         Default  => "#FF0000");

      Thaw_Bg_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Thaw-Bg-Color",
         Path     => ":Debugger",
         Label    => -"Auto-Refreshed",
         Doc      =>
            -"Background color for auto-refreshed items in Data view",
         Default  => "#FFFFFF");

      Freeze_Bg_Color := Create
        (Manager  => Prefs,
         Name     => "Debugger-Freeze-Bg-Color",
         Label    => -"Frozen",
         Doc      =>
            -"Background color for Data view items that are not refreshed.",
         Path     => ":Debugger",
         Default  => "#AAAAAA");

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

      Title_Font := Create
        (Manager  => Prefs,
         Name     => "Debugger-Title-Font",
         Path     => -"Debugger:Data View",
         Label    => -"Item name",
         Doc      => -"Font used for variable names.",
         Default  => "Sans Bold 9");

      Type_Font := Create
        (Manager  => Prefs,
         Name     => "Debugger-Type-Font",
         Path     => -"Debugger:Data View",
         Label    => -"Item type",
         Doc      => -"Font used for variable types.",
         Default  => "Sans Oblique 9");

      Max_Item_Width := Create
        (Manager => Prefs,
         Name    => "Browsers-Item-Max-Width",
         Path    => -"Debugger:Data View",
         Minimum => 1,
         Maximum => Integer'Last,
         Default => 1200,
         Doc     => -"Maximum width of a box in Data view.",
         Label   => -"Max item width");

      Max_Item_Height := Create
        (Manager => Prefs,
         Name    => "Browsers-Item-Max-Height",
         Path    => -"Debugger:Data View",
         Minimum => 1,
         Maximum => Integer'Last,
         Default => 12000,
         Doc     => -"Maximum height of a box in Data view.",
         Label   => -"Max item height");
   end Register_Default_Preferences;

end GVD.Preferences;
