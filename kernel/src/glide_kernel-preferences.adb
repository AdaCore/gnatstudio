-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Gdk.Color;           use Gdk.Color;
with Glib;                use Glib;
with Glib.Properties;     use Glib.Properties;
with Gdk.Color;           use Gdk.Color;
with Gtk.Box;             use Gtk.Box;
with Gtk.Dialog;          use Gtk.Dialog;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Label;           use Gtk.Label;
with Gtk.Notebook;        use Gtk.Notebook;
with Gtk.Table;           use Gtk.Table;
with Gtk.Widget;          use Gtk.Widget;
with Pango.Font;          use Pango.Font;
with Glide_Intl;          use Glide_Intl;

package body Glide_Kernel.Preferences is

   ---------------------------------
   -- Register_Global_Preferences --
   ---------------------------------

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      -- General --

      Default_Widget_Width := Param_Spec_Int (Gnew_Int
        (Name    => "General-Default-Widget-Width",
         Nick    => -"Default width",
         Blurb   => -"Default width for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 400));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Widget_Width), -"General");

      Default_Widget_Height := Param_Spec_Int (Gnew_Int
        (Name    => "General-Default-Widget-Height",
         Nick    => -"Default height",
         Blurb   => -"Default height for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 400));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Widget_Height), -"General");

      Animated_Image := Param_Spec_String (Gnew_String
        (Name    => "General-Animated-Image",
         Nick    => -"Animated image",
         Blurb   => -("Animated image used to inform the user about"
                      & " a command in process"),
         Default => "gps-animation.gif",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Animated_Image), -"General");

      Splash_Screen := Param_Spec_String (Gnew_String
        (Name    => "General-Splash-Screen",
         Nick    => -"Splash screen",
         Blurb   => -"Splash screen displayed (if found) when starting GPS",
         Default => "gps-splash.jpg",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Splash_Screen), -"General");

      Tmp_Dir := Param_Spec_String (Gnew_String
        (Name    => "General-Tmp-Dir",
         Nick    => -"Temporary directory",
         Blurb   => -"Directory used to create temporary files",
         Default => "/tmp"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tmp_Dir), -"General");

      -- Console --

      Highlight_File := Param_Spec_Color (Gnew_Color
        (Name  => "Console-Highlight-File",
         Nick  => -"Highlight color",
         Blurb => -"Color used to highlight a file reference in the console",
         Default => "#FF0000"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Highlight_File), -"Console");

      Highlight_Error := Param_Spec_Color (Gnew_Color
        (Name    => "Console-Highlight-Error",
         Nick    => -"Errors color",
         Blurb   => -"Color used to highlight an error in the console",
         Default => "#FF0000"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Highlight_Error), -"Console");

      -- Diff_Utils --

      Diff_Cmd := Param_Spec_String (Gnew_String
        (Name  => "Diff-Utils-Diff",
         Nick  => -"Diff command",
         Blurb => -("Command used to compute differences between two files."
                    & " Arguments can also be specified"),
         Default => "diff"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Diff_Cmd), -"Visual diff");

      Patch_Cmd := Param_Spec_String (Gnew_String
        (Name    => "Diff-Utils-Patch",
         Nick    => -"Patch command",
         Blurb   => -("Command used to apply a patch. Arguments can also be"
                     & " specified"),
         Default => "patch"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Patch_Cmd), -"Visual diff");

      -- Explorer --

      Normalized_Directories := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Explorer-Normalized-Directories",
         Nick    => -"Normalize directories",
         Blurb   => -("True if directories should be fully normalized, eg"
                      & " links should be resolved. False if the explorer"
                      & " should display the name as given in the project"
                      & " file"),
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (Normalized_Directories), -"Explorer");

      Show_Directories := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Explorer-Show-Directories",
         Nick    => -"Show directories",
         Blurb   => -"True if directories should be displayed in the explorer",
         Default => True,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Show_Directories), -"Explorer");
      --  ??? Does it really make sense.

      File_View_Shows_Only_Project := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Prj-Editor-File-View-Shows-Only-Project",
         Nick    => -"Files from project only",
         Blurb   => -("True if the files view should only show files belonging"
                      & " to the project"),
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (File_View_Shows_Only_Project),
         -"Explorer");

      -- Source Editor --

      Default_Keyword_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Keyword-Color",
         Nick    => -"Keywords color",
         Blurb   => -"Color for highlighting keywords",
         Default => "black"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Keyword_Color), -"Editor");

      Default_Comment_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Comment-Color",
         Default => "blue",
         Blurb   => -"Color for highlighting comments",
         Nick    => -"Comments color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Comment_Color), -"Editor");

      Default_String_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-String-Color",
         Default => "brown",
         Blurb   => -"Color for highlighting strings",
         Nick    => -"Strings color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_String_Color), -"Editor");

      Default_Character_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Character-Color",
         Default => "brown",
         Blurb   => -"Color for highlighting character constants",
         Nick    => -"Character constants color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Character_Color), -"Editor");

      Default_HL_Line_Color   := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Line-Color",
         Default => "green",
         Blurb   => -"Color for highlighting lines",
         Nick    => -"Line highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_HL_Line_Color), -"Editor");

      Default_HL_Region_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Region-Color",
         Default => "cyan",
         Blurb   => -"Color for highlighting regions",
         Nick    => -"Region highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_HL_Region_Color), -"Editor");

      Automatic_Indentation := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Automatic-Indentation",
         Default => True,
         Blurb   => -("Whether the editor should indent automatically"
                      & " the source"),
         Nick    => -"Auto indent"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Automatic_Indentation), -"Editor");

      Strip_Blanks := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Strip-Blanks",
         Default => True,
         Blurb   => -("Whether the editor should remove trailing blanks"
                      & " when saving a file"),
         Nick    => -"Strip blanks"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Strip_Blanks), -"Editor");

      Default_Source_Editor_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Default-Font",
         Default => "Courier 10",
         Blurb   => -"The font used in the source editor",
         Nick    => -"Default font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Source_Editor_Font),
         -"Editor");

      Display_Tooltip := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Blurb   => -"Whether tooltips should be displayed automatically",
         Nick    => -"Tooltips"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Tooltip), -"Editor");

      Periodic_Save := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Periodic-Save",
         Minimum => 0,
         Maximum => 3600,
         Default => 60,
         Blurb   => -("The period (in seconds) after which a source editor"
                      & " is automatically saved. 0 if none."),
         Nick    => -"Autosave delay"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Periodic_Save), -"Editor");

      Tab_Width := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Tab-Width",
         Minimum => 1,
         Maximum => 16,
         Default => 8,
         Blurb   => -"The width of a tabulation character, in characters",
         Nick    => -"Tabulation width"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tab_Width), -"Editor");

      -- Project Editor --

      Default_Switches_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Default-Switches-Color",
         Default => "#777777",
         Blurb   => -("Color to use when displaying switches that are set"
                      & " as default for all the files in the project"),
         Nick    => -"Default switches color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Switches_Color), -"Project");

      Switches_Editor_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Prj-Editor-Title-Font",
         Default => "helvetica bold oblique 14",
         Blurb   => -"Font to use for the switches editor dialog",
         Nick    => -"Title font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Switches_Editor_Title_Font),
         -"Project");

      Variable_Ref_Background := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Var-Ref-Bg",
         Default => "#AAAAAA",
         Blurb   => -("Color to use for the background of variable"
                      & " references in the value editor"),
         Nick    => -"Variable reference color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Variable_Ref_Background), -"Project");

      Invalid_Variable_Ref_Background := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Invalid-Var-Ref-Bg",
         Default => "#AA0000",
         Blurb   => -("Color to use for the foreground of invalid variable"
                      & " references"),
         Nick    => -"Invalid references color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Invalid_Variable_Ref_Background),
         -"Project");

      -- Wizards --

      Wizard_Toc_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Wizard-Toc-Highlight-Color",
         Default => "yellow",
         Blurb   => -"Color to use to highlight strings in the TOC",
         Nick    => -"Table of contents color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Wizard_Toc_Highlight_Color),
         -"Project wizard");

      Wizard_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Wizard-Title-Font",
         Default => "helvetica bold oblique 10",
         Blurb   => -"Font to use for the title of the pages in the wizard",
         Nick    => -"Title font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Wizard_Title_Font),
         -"Project wizard");

      -- Browsers --

      Browsers_Link_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Browsers-Link-Font",
         Default => "Helvetica 10",
         Blurb   => -"Font used to draw the links between items",
         Nick    => -"Links font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Link_Font), -"Browsers");

      Browsers_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Link-Color",
         Default => "#0000FF",
         Blurb   => -"Color used to draw the links between unselected items",
         Nick    => -"Link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Link_Color), -"Browsers");

      Selected_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Link-Color",
         Default => "#FF0000",
         Blurb   => -"Color to use for links between selected items",
         Nick    => -"Selected link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Link_Color), -"Browsers");

      Selected_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Item-Color",
         Default => "#888888",
         Blurb   => -"Color to use to draw the selected item",
         Nick    => -"Selected item color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Item_Color), -"Browsers");

      Parent_Linked_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Linked-Item-Color",
         Default => "#AAAAAA",
         Blurb   => -("Color to use for the background of the items linked"
                      & " to the selected item"),
         Nick    => -"Ancestor items color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Parent_Linked_Item_Color),
         -"Browsers");

      Child_Linked_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Child-Linked-Item-Color",
         Default => "#DDDDDD",
         Blurb   => -("Color to use for the background of the items linked"
                      & " from the selected item"),
         Nick    => -"Offspring items color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Child_Linked_Item_Color),
         -"Browsers");

      Browsers_Vertical_Layout := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-Vertical-Layout",
         Default => True,
         Blurb   => -("Whether the layout of the graph should be vertical"
                      & " or horizontal"),
         Nick    => -"Vertical layout"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Vertical_Layout),
         -"Browsers");

      -- VCS --

      VCS_Commit_File_Check := Param_Spec_String (Gnew_String
        (Name    => "VCS-Commit-File-Check",
         Default => "",
         Blurb   => -("Script called with one source file as argument,"
                      & " before a VCS commit operation"),
         Nick    => -"Commit file check"));
      Register_Property
        (Kernel.Preferences, Param_Spec (VCS_Commit_File_Check),
         -"VCS:General");

      VCS_Commit_Log_Check := Param_Spec_String (Gnew_String
        (Name    => "VCS-Commit-Log-Check",
         Default => "",
         Blurb   => -("Script that will be called with one log file as"
                      & " argument before a VCS Commit operations"),
         Nick    => -"Commit log check"));
      Register_Property
        (Kernel.Preferences, Param_Spec (VCS_Commit_Log_Check),
         -"VCS:General");

      -- CVS --

      CVS_Command := Param_Spec_String (Gnew_String
        (Name    => "CVS-Command",
         Default => "cvs",
         Blurb   => -"General CVS command",
         Nick    => -"CVS location"));
      Register_Property
        (Kernel.Preferences, Param_Spec (CVS_Command), -"VCS:CVS");

      -- Debugger --
      --  General

      Break_On_Exception := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Debugger-Break-On-Exception",
         Default => False,
         Blurb   => -("True if the debugger should automatically stop"
                      & " when an exception is raised"),
         Nick    => -"Break on exceptions"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Break_On_Exception),
         -"Debugger:General");

      --  Assembly Window

      Asm_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Asm-Highlight-Color",
         Default => "#FF0000",
         Blurb   => -("Color to use to highlight the assembly code for"
                      & " the current line"),
         Nick    => -"Current line"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Asm_Highlight_Color),
         -"Debugger:Assembly");

      Assembly_Range_Size := Param_Spec_Int (Gnew_Int
        (Name    => "Debugger-Assembly-Range-Size",
         Minimum => 1,
         Maximum => 10000,
         Default => 200,
         Blurb   => -("Size of the range to display when initially displaying"
                      & " the assembly window." & ASCII.LF
                      & " If this size is 0, then the whole function is"
                      & " displayed, but this can potentially take a very long"
                      & " time on slow machines or big functions"),
         Nick    => -"Range size"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Assembly_Range_Size),
         -"Debugger:Assembly");

      --  Data Window

      Xref_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Xref-Color",
         Default => "#0000FF",
         Blurb   => -"Color to use for the items that are clickable (blue)",
         Nick    => -"Clickable item"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Xref_Color), -"Debugger:Data");

      Title_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Title-Color",
         Default => "#BEBEBE",
         Blurb   => -"Color to use for the background of the title (grey)",
         Nick    => -"Title background"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Title_Color), -"Debugger:Data");

      Change_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Change-Color",
         Default => "#FF0000",
         Blurb   => -("Color used to highlight fields that have changed"
                      & " since the last update (default is red)"),
         Nick    => -"Changed data"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Change_Color), -"Debugger:Data");

      Selection_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Selection-Color",
         Default => "#000000",
         Blurb   => -"Color used to handle item selections",
         Nick    => -"Selected item"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selection_Color),
         -"Debugger:Data");

      Thaw_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Thaw-Bg-Color",
         Default => "#FFFFFF",
         Blurb   => -"Color used for auto-refreshed items (white)",
         Nick    => -"Auto-refreshed"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Thaw_Bg_Color),
         -"Debugger:Data");

      Freeze_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Freeze-Bg-Color",
         Default => "#AAAAAA",
         Blurb   => -"Color used for frozen items (light grey)",
         Nick    => -"Frozen"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Freeze_Bg_Color),
         -"Debugger:Data");

      Debugger_Data_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Debugger-Data-Title-Font",
         Default => "helvetica bold 10",
         Blurb   => -"Font used for the name of the item",
         Nick    => -"Item Name"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Debugger_Data_Title_Font),
         -"Debugger:Data");

      Value_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Debugger-Data-Value-Font",
         Default => "helvetica 10",
         Blurb   => -"Font used to display the value of the item",
         Nick    => -"Item Value"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Value_Font),
         -"Debugger:Data");

      Command_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Debugger-Data-Command-Font",
         Default => "courier 10",
         Blurb   => -("Font used to display the output of gdb commands"
                      & " (graph print `...`)"),
         Nick    => -"Command items"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Command_Font),
         -"Debugger:Data");

      Type_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Debugger-Data-Type-Font",
         Default => "helvetica oblique 10",
         Blurb   => -"Font used to display the type of the item",
         Nick    => -"Item Type"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Type_Font),
         -"Debugger:Data");

      Hide_Big_Items := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Debugger-Hide-Big-Items",
         Default => True,
         Blurb   => -("If True, items higher than a given limit will start"
                      & " in a hidden state"),
         Nick    => -"Hide Big Items"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Hide_Big_Items),
         -"Debugger:Data");

      Big_Item_Height := Param_Spec_Int (Gnew_Int
        (Name    => "Debugger-Big-Items-Height",
         Minimum => 0,
         Maximum => 100000,
         Default => 150,
         Blurb   => -"Items taller than this value will start hidden",
         Nick    => -"Big Item Height"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Big_Item_Height),
         -"Debugger:Data");

      Default_Detect_Aliases := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Debugger-Default-Detect-Aliased",
         Default => True,
         Blurb   => -("If True, do not create new items when a matching"
                      & " item is already present in the canvas"),
         Nick    => -"Detect aliases (shared data structures)"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Detect_Aliases),
         -"Debugger:Data");

      --  Command Window

      Debugger_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Command-Highlight-Color",
         Default => "#0000FF",
         Blurb   => -"Color used for highlighting in the debugger window",
         Nick    => -"Highlighting"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Debugger_Highlight_Color),
         -"Debugger:Command");

      Debugger_Command_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Debugger-Command-Font",
         Default => "courier 12",
         Blurb   => -"Font used in the debugger text window",
         Nick    => -"Font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Debugger_Command_Font),
         -"Debugger:Command");

      --  Memory Window

      Memory_View_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Debugger-Memory-View-Font",
         Default => "courier 12",
         Blurb   => -"Font use in the memory view window",
         Nick    => -"Font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Memory_View_Font),
         -"Debugger:Memory");

      Memory_View_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Memory-View-Color",
         Default => "#333399",
         Blurb   => -"Color used by default in the memory view window",
         Nick    => -"Default color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Memory_View_Color),
         -"Debugger:Memory");

      Memory_Highlighted_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Memory-Highlighted-Color",
         Default => "#DDDDDD",
         Blurb   => -"Color used for highlighted items in the memory view",
         Nick    => -"Color highlighting"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Memory_Highlighted_Color),
         -"Debugger:Memory");

      Memory_Selected_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Memory-Selected-Color",
         Default => "#00009C",
         Blurb   => -"Color used for selected items in the memory view",
         Nick    => -"Selection"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Memory_Selected_Color),
         -"Debugger:Memory");

      Memory_Modified_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Debugger-Memory-Modified-Color",
         Default => "#FF0000",
         Blurb   => -"Color used for modified items in the memory view",
         Nick    => -"Modified"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Memory_Modified_Color),
         -"Debugger:Memory");

      -- Helpers --

      List_Processes := Param_Spec_String (Gnew_String
        (Name    => "Helpers-List-Processes",
         Default => "ps x",
         Blurb   => -"Command to use to list processes running on the machine",
         Nick    => -"List processes"));
      Register_Property
        (Kernel.Preferences, Param_Spec (List_Processes),
         -"Debugger:Helpers");

      Remote_Protocol := Param_Spec_String (Gnew_String
        (Name    => "Helpers-Remote-Protocol",
         Default => "rsh",
         Blurb   => -"How to run a process on a remote machine",
         Nick    => -"Remote shell"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Remote_Protocol),
         -"Debugger:Helpers");

      Remote_Copy := Param_Spec_String (Gnew_String
        (Name    => "Helpers-Remote-Copy",
         Default => "rcp",
         Blurb   => -"Program used to copy a file from a remote host",
         Nick    => -"Remote copy"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Remote_Copy),
         -"Debugger:Helpers");
   end Register_Global_Preferences;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class) is

      function Find_Page
        (Note : access Gtk_Notebook_Record'Class; Name : String)
         return Gtk_Widget;
      --  Return the widget in the page whose name is Name, or null if the page
      --  wasn't found.

      ---------------
      -- Find_Page --
      ---------------

      function Find_Page
        (Note : access Gtk_Notebook_Record'Class;
         Name : String) return Gtk_Widget
      is
         Page_Num  : Gint := 0;
         Widget    : Gtk_Widget;
      begin
         loop
            Widget := Get_Nth_Page (Note, Page_Num);
            if Widget = null
              or else Get_Tab_Label_Text (Note, Widget) = Name
            then
               return Widget;
            end if;
            Page_Num := Page_Num + 1;
         end loop;
      end Find_Page;

      Dialog    : Gtk_Dialog;
      Prefs     : Param_Spec_Array := Get_All_Preferences (Kernel.Preferences);
      Table     : Gtk_Table;
      Label     : Gtk_Label;
      Note      : Gtk_Notebook;
      Main_Note : Gtk_Notebook;
      Tmp       : Gtk_Widget;

   begin
      Gtk_New
        (Dialog => Dialog,
         Title  => -"Preferences",
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);

      Gtk_New (Main_Note);
      Set_Show_Tabs (Main_Note, True);
      Set_Tab_Pos (Main_Note, Pos_Left);
      Pack_Start (Get_Vbox (Dialog), Main_Note);

      for P in Prefs'Range loop
         declare
            Page_Name  : constant String := Get_Page
              (Kernel.Preferences, Prefs (P));
            Last : Natural := Page_Name'First;
         begin
            while Last <= Page_Name'Last
              and then Page_Name (Last) /= ':'
            loop
               Last := Last + 1;
            end loop;

            --  Find the appropriate page for that module
            Note := Gtk_Notebook
              (Find_Page (Main_Note, Page_Name (Page_Name'First .. Last - 1)));
            if Note = null then
               Gtk_New (Note);
               Set_Scrollable (Note, True);
               Gtk_New (Label, Page_Name (Page_Name'First .. Last - 1));
               Set_Show_Tabs (Note, False);
               Append_Page (Main_Note, Note, Label);
            end if;

            --  Find the appropriate page in the module specific notebook
            if Last < Page_Name'Last then
               Table := Gtk_Table
                 (Find_Page (Note, Page_Name (Last + 1 .. Page_Name'Last)));
            else
               Table := Gtk_Table (Find_Page (Note, -"General"));
            end if;

            if Table = null then
               Gtk_New (Table, Rows => 1, Columns => 2,
                        Homogeneous => False);

               if Last < Page_Name'Last then
                  Gtk_New (Label, Page_Name (Last + 1 .. Page_Name'Last));
               else
                  Gtk_New (Label, -"General");
               end if;
               Append_Page (Note, Table, Label);
               Set_Show_Tabs (Note, Get_Nth_Page (Note, 1) /= null);
            else
               Resize (Table,
                       Rows    => Get_Property (Table, N_Rows_Property) + 1,
                       Columns => 2);
            end if;
         end;

         Gtk_New (Label, Nick_Name (Prefs (P)));
         Set_Alignment (Label, 0.0, 0.5);
         Attach (Table, Label, 0, 1, Guint (P - Prefs'First),
                 Guint (P - Prefs'First + 1),
                 Xoptions => Fill, Yoptions => 0);

         Attach
           (Table, Editor_Widget (Kernel.Preferences, Prefs (P)),
            1, 2, Guint (P - Prefs'First),
            Guint (P - Prefs'First + 1), Yoptions => 0);
      end loop;

      Tmp := Add_Button (Dialog, -"OK", Gtk_Response_OK);
      Tmp := Add_Button (Dialog, -"Apply", Gtk_Response_Apply);
      Tmp := Add_Button (Dialog, -"Cancel", Gtk_Response_Cancel);

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         null;
      end if;

      Destroy (Dialog);
   end Edit_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences
     (Kernel : access Kernel_Handle_Record'Class; File_Name : String) is
   begin
      Save_Preferences (Kernel.Preferences, File_Name);
   end Save_Preferences;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref    : Glib.Properties.Creation.Param_Spec_Int) return Glib.Gint is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_Boolean) return Boolean is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_String) return String is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Font) return Pango.Font.Pango_Font_Description is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_Enum) return Gint is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   -----------------------
   -- Register_Property --
   -----------------------

   procedure Register_Property
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Param  : Glib.Param_Spec;
      Page   : String) is
   begin
      Register_Property (Kernel.Preferences, Param, Page);
   end Register_Property;

end Glide_Kernel.Preferences;
