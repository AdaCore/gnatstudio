-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                             AdaCore                               --
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

with Glib;                     use Glib;
with Default_Preferences;      use Default_Preferences;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with GPS.Intl;               use GPS.Intl;
with Config;                   use Config;

package body GVD.Preferences is

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access GVD_Preferences_Manager;
      Name    : String;
      Value   : Glib.Gint) is
   begin
      Set_Pref (Preferences_Manager_Record (Manager.all)'Access, Name, Value);
   end Set_Pref;

   ----------------------------------
   -- Register_Default_Preferences --
   ----------------------------------

   procedure Register_Default_Preferences
     (Prefs       : access Preferences_Manager_Record'Class;
      Page_Prefix : String := "";
      XML_Prefix  : String := "")
   is
      General        : constant String := Page_Prefix;
      Assembly       : constant String := Page_Prefix & (-"Assembly");
      Data           : constant String := Page_Prefix & (-"Data");
      Memory         : constant String := Page_Prefix & (-"Memory");
      Helpers        : constant String := -"External Commands";
      Source_Flags   : constant Param_Flags := Param_Readable;
      External_Flags : constant Param_Flags :=
        Param_Readable or Param_Writable;
      HTML_Flags     : Param_Flags;

   begin
      if Config.Host = Windows then
         --  This preference is not used under Windows

         HTML_Flags := Source_Flags;

      else
         HTML_Flags := External_Flags;
      end if;

      Debugger_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Debugger-Highlight-Color",
         Nick     => -"Color highlighting",
         Blurb    => -"Color used for highlighting in the debugger console",
         Default  => "#0000FF"));
      Register_Property
        (Prefs, Param_Spec (Debugger_Highlight_Color), General);

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

      Execution_Window := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Execution-Window",
         Nick      => -"Execution window",
         Blurb     =>
           -("If False, the debugged program is assumed to require no input. "
             & "If True, a separate execution window will be created"),
         Default   => True));
      Register_Property  (Prefs, Param_Spec (Execution_Window), General);

      Editor_Show_Line_With_Code := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Editor-Show-Line-With-Code",
         Nick      => -"Show lines with code",
         Blurb     => -("True if dots should be shown in the editor for lines"
                        & " that contain code"),
         Default   => False));
      Register_Property
        (Prefs, Param_Spec (Editor_Show_Line_With_Code), General);

      Asm_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Asm-Highlight-Color",
         Nick     => -"Current assembly line",
         Blurb    => -("Color used to highlight the assembly code for the"
                       & " current source line"),
         Default  => "#FF0000"));
      Register_Property
        (Prefs, Param_Spec (Asm_Highlight_Color), Assembly);

      Assembly_Range_Size := Param_Spec_Int (Gnew_Int
        (Name     => XML_Prefix & "Assembly-Range-Size",
         Nick     => -"Range size",
         Blurb    => -("Number of assembly lines to display in the initial"
                       & " display of the assembly window." & ASCII.LF
                       & "If this size is 0, then the whole subprogram"
                       & " is displayed, but this can take a very long time"
                       & " on slow machines"),
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 200));
      Register_Property (Prefs, Param_Spec (Assembly_Range_Size), Assembly);

      Xref_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Xref-Color",
         Nick     => -"Clickable item",
         Blurb    => -"Color used for the items that are clickable",
         Default  => "#0000FF"));
      Register_Property (Prefs, Param_Spec (Xref_Color), Data);

      Title_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Title-Color",
         Nick     => -"Title background",
         Blurb    => -"Color used for the background of the title",
         Flags    => Source_Flags,
         Default  => "#BEBEBE"));
      Register_Property (Prefs, Param_Spec (Title_Color), Data);

      Change_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Change-Color",
         Nick     => -"Changed data",
         Blurb    => -("Color used to highlight fields that have changed"
                       & " since the last update"),
         Default  => "#FF0000"));
      Register_Property (Prefs, Param_Spec (Change_Color), Data);

      Thaw_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Thaw-Bg-Color",
         Nick     => -"Auto-Refreshed",
         Blurb    => -("Background color for the items that are recomputed"
                       & " every time the debugger stops"),
         Flags    => Source_Flags,
         Default  => "#FFFFFF"));
      Register_Property (Prefs, Param_Spec (Thaw_Bg_Color), Data);

      Freeze_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Freeze-Bg-Color",
         Nick     => -"Frozen",
         Blurb    =>
           -("Background color for the items that are never recomputed"),
         Flags    => Source_Flags,
         Default  => "#AAAAAA"));
      Register_Property (Prefs, Param_Spec (Freeze_Bg_Color), Data);

      Title_Font := Param_Spec_Font (Gnew_Font
        (Name     => XML_Prefix & "Title-Font",
         Nick     => -"Item name",
         Blurb    => -"Font used for the name of the variables",
         Default  => "Sans Bold 9"));
      Register_Property (Prefs, Param_Spec (Title_Font), Data);

      Type_Font := Param_Spec_Font (Gnew_Font
        (Name     => XML_Prefix & "Type-Font",
         Nick     => -"Item type",
         Blurb    => -"Font used for the type of the variables",
         Default  => "Sans Oblique 9"));
      Register_Property (Prefs, Param_Spec (Type_Font), Data);

      Hide_Big_Items := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Hide-Big-Items",
         Nick     => -"Fold big items",
         Blurb    => -("True if items higher than a Big Item Height should"
                       & " be folded initially"),
         Flags    => Source_Flags,
         Default  => True));
      Register_Property (Prefs, Param_Spec (Hide_Big_Items), Data);

      Big_Item_Height := Param_Spec_Int (Gnew_Int
        (Name     => XML_Prefix & "Big-Item-Height",
         Nick     => -"Big item height",
         Blurb    => -"See Fold big items",
         Flags    => Source_Flags,
         Minimum  => 0,
         Maximum  => 100000,
         Default  => 150));
      Register_Property (Prefs, Param_Spec (Big_Item_Height), Data);

      Default_Detect_Aliases := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Default-Detect-Aliases",
         Nick     => -"Detect aliases",
         Blurb    => -("True if we shouldn't create new items when an item at"
                       & " the same address already exists"),
         Default  => True));
      Register_Property (Prefs, Param_Spec (Default_Detect_Aliases), Data);

      Show_Call_Stack := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Show-Call-Stack",
         Nick     => -"Show call stack",
         Blurb    => -"True if call stack should be displayed by default",
         Flags    => Source_Flags,
         Default  => False));
      Register_Property (Prefs, Param_Spec (Show_Call_Stack), Data);

      Memory_View_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Memory-View-Color",
         Nick     => -"Default color",
         Blurb    => -"Color used by default in the memory view window",
         Default  => "#333399"));
      Register_Property (Prefs, Param_Spec (Memory_View_Color), Memory);

      Memory_Highlighted_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Memory-Highlighted-Color",
         Nick     => -"Color highlighting",
         Blurb    => -"Color used for highlighted items",
         Default  => "#DDDDDD"));
      Register_Property
        (Prefs, Param_Spec (Memory_Highlighted_Color), Memory);

      Memory_Selected_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Memory-Selected-Color",
         Nick     => -"Selection",
         Blurb    => -"Color used for selected items",
         Default  => "#FF0000"));
      Register_Property
        (Prefs, Param_Spec (Memory_Selected_Color), Memory);

      List_Processes := Param_Spec_String (Gnew_String
        (Name     => XML_Prefix & "List-Processes",
         Nick     => -"List processes",
         Blurb    => -"Command used to list processes running on the machine",
         Default  => Default_Ps));
      Register_Property (Prefs, Param_Spec (List_Processes), Helpers);

      Remote_Protocol := Param_Spec_String (Gnew_String
         (Name    => XML_Prefix & "Remote-Protocol",
          Nick    => -"Remote shell",
          Blurb   => -"Program used to run a process on a remote machine",
          Default => "rsh"));
      Register_Property (Prefs, Param_Spec (Remote_Protocol), Helpers);

      Remote_Copy := Param_Spec_String (Gnew_String
         (Name    => XML_Prefix & "Remote-Copy",
          Nick    => -"Remote copy",
          Blurb   => -"Program used to copy a file from a remote machine",
          Default => "rcp"));
      Register_Property (Prefs, Param_Spec (Remote_Copy), Helpers);

      Execute_Command := Param_Spec_String (Gnew_String
         (Name    => XML_Prefix & "Execute-Command",
          Nick    => -"Execute command",
          Blurb   => -"Program used to execute commands externally",
          Flags   => External_Flags,
          Default => Exec_Command));
      Register_Property (Prefs, Param_Spec (Execute_Command), Helpers);

      Html_Browser := Param_Spec_String (Gnew_String
        (Name     => XML_Prefix & "HTML-Browser",
         Nick     => -"HTML browser",
         Blurb    => -("Program used to browse HTML pages. " &
                       "No value means automatically try to find a suitable " &
                       "browser"),
         Flags    => HTML_Flags,
         Default  => ""));
      Register_Property (Prefs, Param_Spec (Html_Browser), Helpers);

      Print_Command := Param_Spec_String (Gnew_String
         (Name    => XML_Prefix & "Print-Command",
          Nick    => -"Print command",
          Blurb   => -("Program used to print files. No value means use " &
                       "the built-in printing capability (available under " &
                       "Windows only)"),
          Flags   => External_Flags,
          Default => Default_Print_Cmd));
      Register_Property (Prefs, Param_Spec (Print_Command), Helpers);

      Selected_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Item-Color",
         Default => "#888888",
         Blurb   => -"Color to use to draw the selected item",
         Nick    => -"Selected item color"));

      if Page_Prefix = "" then
         Register_Property
           (Prefs, Param_Spec (Selected_Item_Color), Data);
      else
         Register_Property
           (Prefs, Param_Spec (Selected_Item_Color),  -"Browsers");
      end if;
   end Register_Default_Preferences;

end GVD.Preferences;
