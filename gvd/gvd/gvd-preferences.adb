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
with Glib.Generic_Properties; use Glib.Generic_Properties;
with GPS.Intl; use GPS.Intl;

package body GVD.Preferences is

   package Debugger_Windows_Policy_Properties is new
     Generic_Enumeration_Property
       ("Debugger_Windows_Policy", Debugger_Windows_Policy);

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class)
   is
      XML_Prefix   : constant String := "Debugger-";
      General      : constant String := -"Debugger";
      Source_Flags : constant Param_Flags := Param_Readable;

   begin
      Break_On_Exception := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Break-On-Exception",
         Nick      => -"Break on exceptions",
         Blurb     =>
           -("True if process should be stopped when an exception is raised."
             & " This setup is only taken into account when a new debugger"
             & " is initialized, it doesn't modify the behavior for existing"
             & " debuggers."),
         Default   => False));
      Register_Property (Prefs, Param_Spec (Break_On_Exception), General);

      Open_Main_Unit := Param_Spec_Boolean (Gnew_Boolean
        (Name       => XML_Prefix & "Open-Main-Unit",
         Nick       => -"Always open main unit",
         Blurb      =>
         -("Enabled when initializing the debugger should always open the"
           & " main unit. If disabled, no new editor is opened"),
         Default    => True));
      Register_Property (Prefs, Param_Spec (Open_Main_Unit), General);

      if Support_Execution_Window then
         Execution_Window := Param_Spec_Boolean (Gnew_Boolean
           (Name      => XML_Prefix & "Execution-Window",
            Nick      => -"Execution window",
            Blurb     =>
             -("If False, the debugged program is assumed to require no " &
               "input. If True, a separate execution window will be created"),
            Default   => True));

      else
         Execution_Window := Param_Spec_Boolean (Gnew_Boolean
           (Name      => XML_Prefix & "Execution-Window",
            Nick      => -"Execution window",
            Blurb     => "",
            Flags     => Source_Flags,
            Default   => False));
      end if;

      Register_Property  (Prefs, Param_Spec (Execution_Window), General);

      Preserve_State_On_Exit := Param_Spec_Boolean (Gnew_Boolean
        (Name       => XML_Prefix & "Preserve_State-On-Exit",
         Nick       => -"Preserve state on exit",
         Blurb      =>
            -("True if the breakpoints should be saved when the debugger"
             & " session is terminated, and restored the next time the same"
             & " executable is debugged. This preference also controls"
             & " whether the contents of the data window should be preserved"
             & " when the window is closed."),
         Default    => True));
      Register_Property
        (Prefs, Param_Spec (Preserve_State_On_Exit), General);

      Debugger_Windows := Param_Spec_Enum
        (Debugger_Windows_Policy_Properties.Gnew_Enum
           (Name    => XML_Prefix & "Debugger-Windows",
            Nick    => -"Debugger windows",
            Blurb   =>
            -("What should happen to debugger-related windows when the"
              & " debugger session terminates. The windows can either be"
              & " closed automatically, or be kept in the desktop, in which"
              & " case they can either be hidden or kept visible. The next"
              & " debugger session will reuse these windows, which is"
              & " convenient if you want to put them in a specific place"),
            Default => Close_Windows));
      Register_Property (Prefs, Param_Spec (Debugger_Windows), General);

      Editor_Show_Line_With_Code := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Editor-Show-Line-With-Code",
         Nick      => -"Show lines with code",
         Blurb     => -("True if dots should be shown in the editor for lines"
                        & " that contain code"),
         Default   => False));
      Register_Property
        (Prefs, Param_Spec (Editor_Show_Line_With_Code), General);

      Default_Detect_Aliases := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Default-Detect-Aliases",
         Nick     => -"Detect aliases",
         Blurb    => -("True if we shouldn't create new items in the data "
                       & "window when an item at the same address already "
                       & "exists"),
         Default  => True));
      Register_Property (Prefs, Param_Spec (Default_Detect_Aliases), General);

      Assembly_Range_Size := Param_Spec_Int (Gnew_Int
        (Name     => XML_Prefix & "Assembly-Range-Size",
         Nick     => -"Assembly range size",
         Blurb    => -("Number of assembly lines to display in the initial"
                       & " display of the assembly window." & ASCII.LF
                       & "If this size is 0, then the whole subprogram"
                       & " is displayed, but this can take a very long time"
                       & " on slow machines"),
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 200));
      Register_Property (Prefs, Param_Spec (Assembly_Range_Size), General);

      Asm_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Asm-Highlight-Color",
         Nick     => -"Current assembly line",
         Blurb    => -("Color used to highlight the assembly code for the"
                       & " current source line"),
         Default  => "#0000FF"));
      Register_Property (Prefs, Param_Spec (Asm_Highlight_Color), General);

      Asm_Breakpoint_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Asm-Breakpoint-Color",
         Nick     => -"Breakpoint line",
         Blurb    => -("Assembly line on which a breakpoint is set"),
         Default  => "#FF0000"));
      Register_Property (Prefs, Param_Spec (Asm_Breakpoint_Color), General);

      Debugger_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Highlight-Color",
         Nick     => -"Color highlighting",
         Blurb    => -"Color used for highlighting in the debugger console",
         Default  => "#0000FF"));
      Register_Property
        (Prefs, Param_Spec (Debugger_Highlight_Color), General);

      Xref_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Xref-Color",
         Nick     => -"Clickable item",
         Blurb    => -"Color used for the data items that are clickable",
         Default  => "#0000FF"));
      Register_Property (Prefs, Param_Spec (Xref_Color), General);

      Change_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Change-Color",
         Nick     => -"Changed data",
         Blurb    => -("Color used to highlight data fields that have changed"
                       & " since the last update"),
         Default  => "#FF0000"));
      Register_Property (Prefs, Param_Spec (Change_Color), General);

      Thaw_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Thaw-Bg-Color",
         Nick     => -"Auto-Refreshed",
         Blurb    => -("Background color for the items that are recomputed"
                       & " every time the debugger stops"),
         Flags    => Source_Flags,
         Default  => "#FFFFFF"));
      Register_Property (Prefs, Param_Spec (Thaw_Bg_Color), General);

      Freeze_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Freeze-Bg-Color",
         Nick     => -"Frozen",
         Blurb    =>
           -("Background color for the items that are never recomputed"),
         Flags    => Source_Flags,
         Default  => "#AAAAAA"));
      Register_Property (Prefs, Param_Spec (Freeze_Bg_Color), General);

      Hide_Big_Items := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Hide-Big-Items",
         Nick     => -"Fold big items",
         Blurb    => -("True if items higher than a Big Item Height should"
                       & " be folded initially"),
         Flags    => Source_Flags,
         Default  => True));
      Register_Property (Prefs, Param_Spec (Hide_Big_Items), General);

      Big_Item_Height := Param_Spec_Int (Gnew_Int
        (Name     => XML_Prefix & "Big-Item-Height",
         Nick     => -"Big item height",
         Blurb    => -"See Fold big items",
         Flags    => Source_Flags,
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 150));
      Register_Property (Prefs, Param_Spec (Big_Item_Height), General);

      Show_Call_Stack := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Show-Call-Stack",
         Nick     => -"Show call stack",
         Blurb    => -"True if call stack should be displayed by default",
         Flags    => Source_Flags,
         Default  => False));
      Register_Property (Prefs, Param_Spec (Show_Call_Stack), General);

      Memory_View_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Memory-View-Color",
         Nick     => -"Memory color",
         Blurb    => -"Color used by default in the memory view window",
         Default  => "#333399"));
      Register_Property (Prefs, Param_Spec (Memory_View_Color), General);

      Memory_Highlighted_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Memory-Highlighted-Color",
         Nick     => -"Memory highlighting",
         Blurb    => -"Color used for highlighted items in the memory view",
         Default  => "#DDDDDD"));
      Register_Property
        (Prefs, Param_Spec (Memory_Highlighted_Color), General);

      Memory_Selected_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Memory-Selected-Color",
         Nick     => -"Memory selection",
         Blurb    => -"Color used for selected items in the memory view",
         Default  => "#FF0000"));
      Register_Property
        (Prefs, Param_Spec (Memory_Selected_Color), General);

      Title_Font := Param_Spec_Font (Gnew_Font
        (Name     => XML_Prefix & "Title-Font",
         Nick     => -"Item name",
         Blurb    => -"Font used for the name of the variables",
         Default  => "Sans Bold 9"));
      Register_Property (Prefs, Param_Spec (Title_Font), General);

      Type_Font := Param_Spec_Font (Gnew_Font
        (Name     => XML_Prefix & "Type-Font",
         Nick     => -"Item type",
         Blurb    => -"Font used for the type of the variables",
         Default  => "Sans Oblique 9"));
      Register_Property (Prefs, Param_Spec (Type_Font), General);

      Max_Item_Width := Param_Spec_Int (Gnew_Int
        (Name    => "Browsers-Item-Max-Width",
         Minimum => 1,
         Maximum => Gint'Last,
         Default => 1200,
         Blurb   => -"The maximum width of an item",
         Nick    => -"Max item width"));
      Register_Property
        (Prefs, Param_Spec (Max_Item_Width), General);

      Max_Item_Height := Param_Spec_Int (Gnew_Int
        (Name    => "Browsers-Item-Max-Height",
         Minimum => 1,
         Maximum => Gint'Last,
         Default => 12000,
         Blurb   => -"The maximum height of an item",
         Nick    => -"Max item height"));
      Register_Property
        (Prefs, Param_Spec (Max_Item_Height), General);

   end Register_Default_Preferences;

end GVD.Preferences;
