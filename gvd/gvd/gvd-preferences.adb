-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                    use Glib;
with Default_Preferences;     use Default_Preferences;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Generic_Properties;  use Glib.Generic_Properties;
with Odd_Intl;                use Odd_Intl;

package body GVD.Preferences is

   package Tooltips_Properties is new Generic_Enumeration_Property
     ("Tooltips_In_Source", Tooltips_In_Source_Type);

   ------------------
   -- Get_Tab_Size --
   ------------------

   function Get_Tab_Size (Pref : access GVD_Preferences_Manager) return Gint is
   begin
      return Pref.Tab_Size;
   end Get_Tab_Size;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Manager : access GVD_Preferences_Manager;
      Name    : String;
      Value   : Glib.Gint) is
   begin
      if Name = Pspec_Name (Param_Spec (Tab_Size)) then
         Manager.Tab_Size := Value;
      end if;
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
      General        : constant String := Page_Prefix & (-"General");
      Source         : constant String := Page_Prefix & (-"Source");
      Assembly       : constant String := Page_Prefix & (-"Assembly");
      Data           : constant String := Page_Prefix & (-"Data");
      Memory         : constant String := Page_Prefix & (-"Memory");
      Helpers        : constant String := -"Helpers";
      Source_Flags   : Param_Flags := Param_Readable;
      External_Flags : Param_Flags := Param_Readable;

   begin
      --  In standalone mode, the source editor prefs are also editable

      if XML_Prefix = "" then
         Source_Flags := Source_Flags or Param_Writable;
         Default_Font := Param_Spec_Font
           (Gnew_Font
              (Name      => XML_Prefix & "Default-Font",
               Nick      => -"Default font",
               Blurb     => -"Default font",
               Default   => GVD.Default_Font));
         Register_Property (Prefs, Param_Spec (Default_Font), General);

         Fixed_Font := Param_Spec_Font
           (Gnew_Font
              (Name      => XML_Prefix & "Fixed-Font",
               Nick      => -"Fixed font",
               Blurb     =>
                 -"Fixed font used to display e.g. debugger commands",
               Default   => "Courier 10"));
         Register_Property (Prefs, Param_Spec (Fixed_Font), General);

      else
         External_Flags := External_Flags or Param_Writable;
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
           -"True if process should be stopped when an exception is raised",
         Default   => False));
      Register_Property (Prefs, Param_Spec (Break_On_Exception), General);

      Hide_Delay := Param_Spec_Int (Gnew_Int
        (Name      => XML_Prefix & "Hide-Delay",
         Nick      => -"Status bar time out (ms)",
         Blurb     => -("Delay before the messages in the status bar"
                        & " disappear"),
         Minimum   => 0,
         Maximum   => 3600000,
         Default   => 5000,
         Flags     => Source_Flags));
      Register_Property (Prefs, Param_Spec (Hide_Delay), General);

      Ada_Extensions := Param_Spec_String (Gnew_String
        (Name      => XML_Prefix & "Ada-Extensions",
         Nick      => -"Ada files extensions",
         Blurb     => -"Semicolon separated list of extensions for Ada files",
         Default   => ".ads;.adb;.ada;.a;.dg",
         Flags     => Source_Flags));
      Register_Property (Prefs, Param_Spec (Ada_Extensions), General);

      C_Extensions := Param_Spec_String (Gnew_String
        (Name      => XML_Prefix & "C-Extensions",
         Nick      => -"C files extensions",
         Blurb     => -"Semicolon separated list of extensions for C files",
         Default   => ".c;.h",
         Flags     => Source_Flags));
      Register_Property (Prefs, Param_Spec (C_Extensions), General);

      Cpp_Extensions := Param_Spec_String (Gnew_String
        (Name      => XML_Prefix & "Cpp-Extensions",
         Nick      => -"C++ files extensions",
         Blurb     => -"Semicolon separated list of extensions for C++ files",
         Default   => ".cc;.cpp;.C;.hh;.H",
         Flags     => Source_Flags));
      Register_Property (Prefs, Param_Spec (Cpp_Extensions), General);

      Execution_Window := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Execution-Window",
         Nick      => -"Execution window",
         Blurb     =>
           -("If False, the debugged program is assumed to require no input. "
             & "If True, a separate execution window will be created"),
         Default   => True));
      Register_Property  (Prefs, Param_Spec (Execution_Window), General);

      Display_Explorer := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Display-Explorer",
         Nick      => -"Display explorer tree",
         Blurb     => -("True if the explorer on the left side of GVD should"
                        & " be displayed"),
         Flags     => Source_Flags,
         Default   => True));
      Register_Property  (Prefs, Param_Spec (Display_Explorer), Source);

      --  Hide the preferences for the source editor, unless in standalone mode
      File_Name_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name      => XML_Prefix & "File-Name-Bg-Color",
         Nick      => -"File name background",
         Blurb     => -("Color used for the background of the file name in the"
                        & " editor. This is also used for the background of"
                        & " the current frame in the call stack window"),
         Flags     => Source_Flags,
         Default   => "#BEBEBE"));
      Register_Property (Prefs, Param_Spec (File_Name_Bg_Color), Source);

      Editor_Show_Line_Nums := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Editor-Show-Line-Nums",
         Nick      => -"Show line numbers",
         Blurb    => -"True if line numbers should be displayed in the editor",
         Flags     => Source_Flags,
         Default   => True));
      Register_Property (Prefs, Param_Spec (Editor_Show_Line_Nums), Source);

      Editor_Show_Line_With_Code := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Editor-Show-Line-With-Code",
         Nick      => -"Show lines with code",
         Blurb     => -("True if dots should be shown in the editor for lines"
                        & " that contain code"),
         Default   => False));

      if XML_Prefix = "" then
         Register_Property
           (Prefs, Param_Spec (Editor_Show_Line_With_Code), Source);
      else
         Register_Property
           (Prefs, Param_Spec (Editor_Show_Line_With_Code), General);
      end if;

      Do_Color_Highlighting := Param_Spec_Boolean (Gnew_Boolean
        (Name      => XML_Prefix & "Do-Color-Highlighting",
         Nick      => -"Syntax highlighting",
         Blurb     => -"True if the editor should provide syntax highlighting",
         Flags     => Source_Flags,
         Default   => True));
      Register_Property (Prefs, Param_Spec (Do_Color_Highlighting), Source);

      Comments_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Comments-Color",
         Nick     => -"Comments",
         Blurb    => -"Color used for comments in the source editor",
         Flags     => Source_Flags,
         Default  => "#FF0000"));
      Register_Property (Prefs, Param_Spec (Comments_Color), Source);

      Strings_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Strings-Color",
         Nick     => -"Strings",
         Blurb    => -"Color used for strings in the source editor",
         Flags     => Source_Flags,
         Default  => "#A52A2A"));
      Register_Property (Prefs, Param_Spec (Strings_Color), Source);

      Keywords_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Keywords-Color",
         Nick     => -"Keywords",
         Blurb    => -"Color used for keywords in the source editor",
         Flags     => Source_Flags,
         Default  => "#0000FF"));
      Register_Property (Prefs, Param_Spec (Keywords_Color), Source);

      Editor_Highlight_Current_Line := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Editor-Highlight-Current-Line",
         Nick     => -"Highlight current line",
         Blurb    => -("True if the editor should highlight the current line"
                       & " with a background color, in addition to the arrow"),
         Flags     => Source_Flags,
         Default  => True));
      Register_Property
        (Prefs, Param_Spec (Editor_Highlight_Current_Line), Source);

      Editor_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name     => XML_Prefix & "Editor-Highlight-Color",
         Nick     => -"Current line",
         Blurb    => -"Color used to highlight the current line in the editor",
         Flags     => Source_Flags,
         Default  => "#00CC00"));
      Register_Property
        (Prefs, Param_Spec (Editor_Highlight_Color), Source);

      Tab_Size := Param_Spec_Int (Gnew_Int
        (Name     => XML_Prefix & "Tab-Size",
         Nick     => -"Tabulation size",
         Blurb   => -"Number of spaces that a tabulation character represents",
         Flags     => Source_Flags,
         Minimum  => 1,
         Default  => Default_Tab_Size,
         Maximum  => 16));
      Register_Property (Prefs, Param_Spec (Tab_Size), Source);

      Tooltips_In_Source := Param_Spec_Enum (Tooltips_Properties.Gnew_Enum
        (Name     => XML_Prefix & "Tooltips-In-Source",
         Nick     => -"Automatic display of variables",
         Blurb    => -("How should the tooltips be displayed:" & ASCII.LF
                       & " None: no tooltips is displayed" & ASCII.LF
                       & " Simple: no processing done on the debugger output"
                       & ASCII.LF
                       & " Full: The output of the processor is parsed, and"
                       & ASCII.LF
                       & "        displayed in a manner similar to the data"
                       & ASCII.LF
                       & "        window"),
         Flags    => Source_Flags,
         Default  => Simple));
      Register_Property (Prefs, Param_Spec (Tooltips_In_Source), Source);

      Should_Strip_CR := Param_Spec_Boolean (Gnew_Boolean
        (Name     => XML_Prefix & "Should-Strip-CR",
         Nick     => -"Strip carriage return",
         Blurb    => -("True if the carriage return characters should be"
                       & " removed when reading a file. This is for"
                       & " compatibility between unix and windows systems"),
         Flags    => Source_Flags,
         Default  => Need_To_Strip_CR));
      Register_Property (Prefs, Param_Spec (Should_Strip_CR), Source);

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

      Default_External_Editor := Param_Spec_String (Gnew_String
         (Name    => XML_Prefix & "Default-External-Editor",
          Nick    => -"Edit sources",
          Blurb   => -("Command to start an external editor" & ASCII.LF
                       & "%f is automatically replaced by the full path name"
                       & " of the file." & ASCII.LF
                       & "%l is replaced by the line number." & ASCII.LF
                       & "This is overriden by the environment variable"
                       & " GVD_EDITOR if it exists." & ASCII.LF
                       & "For vi, try using " & ASCII.LF
                       & " xterm -e /bin/vi %f +%l"),
          Flags    => Source_Flags,
          Default => "glide %f -emacs +%l"));
      Register_Property (Prefs, Param_Spec (Default_External_Editor), Helpers);

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
          Default => "xterm -e"));
      Register_Property (Prefs, Param_Spec (Execute_Command), Helpers);

      Html_Browser := Param_Spec_String (Gnew_String
        (Name     => XML_Prefix & "HTML-Browser",
         Nick     => -"HTML browser",
         Blurb    => -"Program used to browse HTML pages",
         Flags    => Source_Flags,
         Default  => Default_HTML_Browser));
      Register_Property (Prefs, Param_Spec (Html_Browser), Helpers);

      Print_Command := Param_Spec_String (Gnew_String
         (Name    => XML_Prefix & "Print-Command",
          Nick    => -"Print command",
          Blurb   => -"Program used to print files",
          Flags   => External_Flags,
          Default => "a2ps"));
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
           (Prefs, Param_Spec (Selected_Item_Color),  -"Browsers:General");
      end if;
   end Register_Default_Preferences;

end GVD.Preferences;
