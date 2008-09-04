-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2008, AdaCore              --
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

with Config;   use Config;
with GPS.Intl; use GPS.Intl;

package body GVD.Preferences is

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class)
   is
      XML_Prefix   : constant String := "Debugger-";
      General      : constant String := -"Debugger";

   begin
      Break_On_Exception := Create
        (Manager   => Prefs,
         Name      => XML_Prefix & "Break-On-Exception",
         Page      => General,
         Label     => -"Break on exceptions",
         Doc       =>
           -("True if process should be stopped when an exception is raised."
             & " This setup is only taken into account when a new debugger"
             & " is initialized, it doesn't modify the behavior for existing"
             & " debuggers."),
         Default   => False);

      Open_Main_Unit := Create
        (Manager    => Prefs,
         Name       => XML_Prefix & "Open-Main-Unit",
         Label      => -"Always open main unit",
         Page       => General,
         Doc        =>
         -("Enabled when initializing the debugger should always open the"
           & " main unit. If disabled, no new editor is opened"),
         Default    => True);

      if Support_Execution_Window then
         Execution_Window := Create
           (Manager   => Prefs,
            Name      => XML_Prefix & "Execution-Window",
            Label     => -"Execution window",
            Page      => General,
            Doc       =>
             -("If False, the debugged program is assumed to require no " &
               "input. If True, a separate execution window will be created"),
            Default   => True);
      else
         Execution_Window := Create
           (Manager   => Prefs,
            Name      => XML_Prefix & "Execution-Window",
            Label     => -"Execution window",
            Page      => "",
            Doc       => "",
            Default   => False);
      end if;

      Preserve_State_On_Exit := Create
        (Manager    => Prefs,
         Name       => XML_Prefix & "Preserve_State-On-Exit",
         Label      => -"Preserve state on exit",
         Page       => General,
         Doc        =>
            -("True if the breakpoints should be saved when the debugger"
             & " session is terminated, and restored the next time the same"
             & " executable is debugged. This preference also controls"
             & " whether the contents of the data window should be preserved"
             & " when the window is closed."),
         Default    => True);

      Debugger_Windows := Debugger_Windows_Preferences.Create
        (Manager => Prefs,
         Name    => XML_Prefix & "Debugger-Windows",
         Label   => -"Debugger windows",
         Page    => General,
         Doc     =>
         -("What should happen to debugger-related windows when the"
           & " debugger session terminates. The windows can either be"
           & " closed automatically, or be kept in the desktop, in which"
           & " case they can either be hidden or kept visible. The next"
           & " debugger session will reuse these windows, which is"
           & " convenient if you want to put them in a specific place"),
         Default => Hide_Windows);

      Editor_Show_Line_With_Code := Create
        (Manager   => Prefs,
         Name      => XML_Prefix & "Editor-Show-Line-With-Code",
         Label     => -"Show lines with code",
         Doc       => -("True if dots should be shown in the editor for lines"
           & " that contain code"),
         Page      => General,
         Default   => False);

      Default_Detect_Aliases := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Default-Detect-Aliases",
         Label    => -"Detect aliases",
         Page     => General,
         Doc      => -("True if we shouldn't create new items in the data "
                       & "window when an item at the same address already "
                       & "exists"),
         Default  => True);

      Assembly_Range_Size := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Assembly-Range-Size",
         Page     => General,
         Label    => -"Assembly range size",
         Doc      => -("Number of assembly lines to display in the initial"
                       & " display of the assembly window." & ASCII.LF
                       & "If this size is 0, then the whole subprogram"
                       & " is displayed, but this can take a very long time"
                       & " on slow machines"),
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 200);

      Asm_Highlight_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Asm-Highlight-Color",
         Page     => General,
         Label    => -"Current assembly line",
         Doc      => -("Color used to highlight the assembly code for the"
                       & " current source line"),
         Default  => "#0000FF");

      Asm_Breakpoint_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Asm-Breakpoint-Color",
         Page     => General,
         Label    => -"Breakpoint line",
         Doc      => -("Assembly line on which a breakpoint is set"),
         Default  => "#FF0000");

      Debugger_Highlight_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Highlight-Color",
         Page     => General,
         Label    => -"Color highlighting",
         Doc      => -"Color used for highlighting in the debugger console",
         Default  => "#0000FF");

      Xref_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Xref-Color",
         Page     => General,
         Label    => -"Clickable item",
         Doc      => -"Color used for the data items that are clickable",
         Default  => "#0000FF");

      Change_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Change-Color",
         Page     => General,
         Label    => -"Changed data",
         Doc      => -("Color used to highlight data fields that have changed"
                       & " since the last update"),
         Default  => "#FF0000");

      Thaw_Bg_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Thaw-Bg-Color",
         Page     => "",
         Label    => -"Auto-Refreshed",
         Doc      => -("Background color for the items that are recomputed"
                       & " every time the debugger stops"),
         Default  => "#FFFFFF");

      Freeze_Bg_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Freeze-Bg-Color",
         Label    => -"Frozen",
         Doc      =>
           -("Background color for the items that are never recomputed"),
         Page     => "",
         Default  => "#AAAAAA");

      Hide_Big_Items := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Hide-Big-Items",
         Label    => -"Fold big items",
         Doc      => -("True if items higher than a Big Item Height should"
                       & " be folded initially"),
         Page     => "",
         Default  => True);

      Big_Item_Height := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Big-Item-Height",
         Label    => -"Big item height",
         Doc      => -"See Fold big items",
         Page     => "",
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 150);

      Memory_View_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Memory-View-Color",
         Page     => General,
         Label    => -"Memory color",
         Doc      => -"Color used by default in the memory view window",
         Default  => "#333399");

      Memory_Highlighted_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Memory-Highlighted-Color",
         Page     => General,
         Label    => -"Memory highlighting",
         Doc      => -"Color used for highlighted items in the memory view",
         Default  => "#DDDDDD");

      Memory_Selected_Color := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Memory-Selected-Color",
         Page     => General,
         Label    => -"Memory selection",
         Doc      => -"Color used for selected items in the memory view",
         Default  => "#FF0000");

      Title_Font := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Title-Font",
         Page     => General,
         Label    => -"Item name",
         Doc      => -"Font used for the name of the variables",
         Default  => "Sans Bold 9");

      Type_Font := Create
        (Manager  => Prefs,
         Name     => XML_Prefix & "Type-Font",
         Page     => General,
         Label    => -"Item type",
         Doc      => -"Font used for the type of the variables",
         Default  => "Sans Oblique 9");

      Max_Item_Width := Create
        (Manager => Prefs,
         Name    => "Browsers-Item-Max-Width",
         Page    => General,
         Minimum => 1,
         Maximum => Integer'Last,
         Default => 1200,
         Doc     => -"The maximum width of an item",
         Label   => -"Max item width");

      Max_Item_Height := Create
        (Manager => Prefs,
         Name    => "Browsers-Item-Max-Height",
         Page    => General,
         Minimum => 1,
         Maximum => Integer'Last,
         Default => 12000,
         Doc     => -"The maximum height of an item",
         Label   => -"Max item height");
   end Register_Default_Preferences;

end GVD.Preferences;
