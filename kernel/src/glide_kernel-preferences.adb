-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Gdk.Color;                use Gdk.Color;
with Gdk.Types;                use Gdk.Types;
with Glib;                     use Glib;
with Glib.Properties;          use Glib.Properties;
with Glib.Generic_Properties;  use Glib.Generic_Properties;
with Gdk.Color;                use Gdk.Color;
with Pango.Font;               use Pango.Font;
with Glide_Intl;               use Glide_Intl;
with Language;                 use Language;

package body Glide_Kernel.Preferences is

   package Line_Terminators_Properties is new Generic_Enumeration_Property
     ("Line_Terminators", Line_Terminators);

   package Key_Themes_Properties is new Generic_Enumeration_Property
     ("Key_Themes", Key_Themes);

   ---------------------------------
   -- Register_Global_Preferences --
   ---------------------------------

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      -- General --

      Default_Font := Param_Spec_Font (Gnew_Font
        (Name    => "General-Default-Font",
         Default => GVD.Default_Font,
         Blurb   => -"The default font used in GPS",
         Nick    => -"Default font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Font), -"General");
      GVD.Preferences.Default_Font := Default_Font;

      Key_Theme_Name := Param_Spec_Enum (Key_Themes_Properties.Gnew_Enum
        (Name    => "General-Key-Theme-Name",
         Nick    => -"Key theme",
         Blurb   => -"Name of key theme to use",
         Default => Default));
      Register_Property
        (Kernel.Preferences, Param_Spec (Key_Theme_Name), -"General");

      Default_Charset := Param_Spec_String (Gnew_String
        (Name    => "General-Charset",
         Nick    => -"Character set",
         Blurb   => -"Name of character set to use for displaying text",
         Default => "ISO-8859-1"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Charset), -"General");

      Default_Widget_Width := Param_Spec_Int (Gnew_Int
        (Name    => "General-Default-Widget-Width",
         Nick    => -"Default width",
         Blurb   => -"Default width for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 400,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Widget_Width), -"General");

      Default_Widget_Height := Param_Spec_Int (Gnew_Int
        (Name    => "General-Default-Widget-Height",
         Nick    => -"Default height",
         Blurb   => -"Default height for all the newly created windows",
         Minimum => 50,
         Maximum => 2000,
         Default => 400,
         Flags   => Param_Readable));
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

      Use_Native_Dialogs := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Use-Native-Dialogs",
         Nick    => -"Native dialogs",
         Blurb   =>
           -"If true use OS native dialogs, otherwise use portable dialogs",
         Default => True,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Use_Native_Dialogs), -"General");

      Can_Change_Accels := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Can-Change-Accels",
         Nick    => -"Dynamic key bindings",
         Blurb   =>
           -"True if the menu key bindings can be changed interactively",
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (Can_Change_Accels), -"General");

      Splash_Screen := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "General-Splash-Screen",
            Nick    => -"Display splash screen",
            Blurb   =>
              -"True if a splash screen should be displayed when starting GPS",
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Splash_Screen), -"General");

      Display_Welcome := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Display-Welcome",
         Nick    => -"Display welcome window",
         Blurb   => -("True if GPS should display the welcome window"
                      & " for the selection of the project"),
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Welcome), -"General");

      Toolbar_Show_Text := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Toolbar-Text",
         Nick    => -"Show text in tool bar",
         Blurb   => -("True if the tool bar should show both text and icons,"
                      & " False if it should only show icons"),
         Default => False));
      Register_Property
        (Kernel.Preferences, Param_Spec (Toolbar_Show_Text), -"General");

      Auto_Save := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Auto-Save",
         Nick    => -"Auto save",
         Blurb   => -("Whether unsaved files/projects should be saved"
                      & " automatically before calling external tools"),
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Auto_Save), -"General");

      Save_Desktop_On_Exit := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Save-Desktop-On-Exit",
         Nick    => -"Save desktop on exit",
         Blurb   => -("True if the desktop should be saved when exiting GPS"),
         Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Save_Desktop_On_Exit), -"General");

      -- MDI --

      MDI_Opaque := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-Opaque",
         Default => GVD.Default_Opaque_MDI,
         Blurb   => -("If True, items will be resized or moved opaquely when"
                      & " not maximized"),
         Nick    => -"Opaque"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Opaque), -"General:Windows");

      MDI_Destroy_Floats := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-Destroy-Floats",
         Default => False,
         Blurb   => -("If False, closing the window associated with a floating"
                      & " item will put the item back in the main GPS window,"
                      & " but will not destroy it. If True, the item is"
                      & " destroyed"),
         Nick    => -"Destroy floats"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Destroy_Floats),
         -"General:Windows");

      MDI_All_Floating := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "MDI-All-Floating",
         Default => False,
         Blurb   => -("If True, all windows will be set as floating, and put"
                      & " under control of your window manager. Otherwise, a"
                      & " multiple document interface is used."),
         Nick    => -"All floating"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_All_Floating),
         -"General:Windows");

      MDI_Background_Color := Param_Spec_Color (Gnew_Color
        (Name    => "MDI-Background-Color",
         Default => "#666666",
         Blurb   => -"Color to use for the background of the MDI",
         Nick    => -"Background color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Background_Color),
         -"General:Windows");

      MDI_Title_Bar_Color := Param_Spec_Color (Gnew_Color
        (Name    => "MDI-Title-Bar-Color",
         Default => "#AAAAAA",
         Blurb   => -"Color to use for the title bar of unselected items",
         Nick    => -"Title bar color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Title_Bar_Color),
         -"General:Windows");

      MDI_Focus_Title_Color := Param_Spec_Color (Gnew_Color
        (Name    => "MDI-Focus-Title-Color",
         Default => "#000088",
         Blurb   => -"Color to use for the title bar of selected items",
         Nick    => -"Selected title bar color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (MDI_Focus_Title_Color),
         -"General:Windows");

      -- Source Editor --

      Strip_Blanks := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Strip-Blanks",
         Default => True,
         Blurb   =>
           -"Should the editor remove trailing blanks when saving files",
         Nick    => -"Strip blanks"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Strip_Blanks), -"Editor");

      Line_Terminator := Param_Spec_Enum (Line_Terminators_Properties.Gnew_Enum
        (Name  => "Src-Editor-Line-Terminator",
         Nick  => -"Line terminator",
         Blurb => -"Line terminator style to use when saving files",
         Default => Unchanged));
      Register_Property
        (Kernel.Preferences, Param_Spec (Line_Terminator), -"Editor");

      Display_Line_Numbers := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Line_Numbers",
         Default => True,
         Blurb   =>
           -"Whether the line numbers should be displayed in file editors",
         Nick    => -"Display line numbers"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Line_Numbers),
         -"Editor");

      Display_Tooltip := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Blurb   => -"Whether tooltips should be displayed automatically",
         Nick    => -"Tooltips"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Tooltip), -"Editor");

      Highlight_Delimiters := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Highlight-Delimiters",
         Default => True,
         Blurb   => -"Whether delimiters should be highlighted: (){}[]",
         Nick    => -"Highlight delimiters"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Highlight_Delimiters),
         -"Editor");

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

      Highlight_Column := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Highlight-Column",
         Minimum => 0,
         Maximum => 255,
         Default => 80,
         Blurb   => -"The column number to highlight. 0 if none.",
         Nick    => -"Column highlight"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Highlight_Column), -"Editor");

      Tab_Width := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Tab-Width",
         Minimum => 1,
         Maximum => 16,
         Default => 8,
         Blurb   => -"The width of a tabulation character, in characters",
         Nick    => -"Tabulation width",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tab_Width), -"Editor");

      Source_Editor_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Default-Font",
         Default => "Courier 10",
         Blurb   => -"The font used in the source editor",
         Nick    => -"Default font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Source_Editor_Font),
         -"Editor:Fonts & Colors");
      GVD.Preferences.Fixed_Font := Source_Editor_Font;

      Keywords_Style := Gnew_Style
        (Name    => "Src-Editor-Keywords-Style",
         Nick    => -"Keywords",
         Blurb   => -"Style to use when displaying keywords",
         Default_Font => "Courier Bold 10",
         Default_Fg   => "black",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Keywords_Style),
         -"Editor:Fonts & Colors");

      Comments_Style := Gnew_Style
        (Name    => "Src-Editor-Comments-Style",
         Nick    => -"Comments",
         Blurb   => -"Style to use when displaying comments",
         Default_Font => "Courier Medium Oblique 10",
         Default_Fg   => "blue",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Comments_Style),
         -"Editor:Fonts & Colors");

      Strings_Style := Gnew_Style
        (Name    => "Src-Editor-Strings-Style",
         Nick    => -"Strings",
         Blurb   => -"Style to use when displaying strings",
         Default_Font => "Courier 10",
         Default_Fg   => "brown",
         Default_Bg   => "white");
      Register_Property
        (Kernel.Preferences, Param_Spec (Strings_Style),
         -"Editor:Fonts & Colors");

      Current_Line_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Current-Line-Color",
         Default => "white",
         Blurb   => -"Color for highlighting the current line",
         Nick    => -"Current line color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Current_Line_Color),
           -"Editor:Fonts & Colors");

      Current_Block_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Current-Block-Color",
         Default => "white",
         Blurb   => -"Color for highlighting the current block",
         Nick    => -"Current block color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Current_Block_Color),
           -"Editor:Fonts & Colors");

      Default_HL_Line_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Line-Color",
         Default => "cyan",
         Blurb   => -"Color for highlighting lines",
         Nick    => -"Line highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_HL_Line_Color),
         -"Editor:Fonts & Colors");

      Default_HL_Region_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Region-Color",
         Default => "cyan",
         Blurb   => -"Color for highlighting regions",
         Flags   => Param_Readable,
         Nick    => -"Region highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_HL_Region_Color),
         -"Editor:Fonts & Colors");

      -- Debugger --

      GVD.Preferences.Register_Default_Preferences
        (Kernel.Preferences, -"Debugger" & ':', "Debugger-");

      -- Browsers --

      Browsers_Bg_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Bg-Color",
         Default => "#BBBBBB",
         Blurb   => -"Color used to draw the background of the browsers",
         Nick    => -"Background color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Bg_Color),
         -"Browsers");

      Browsers_Bg_Image := Param_Spec_String (Gnew_String
        (Name    => "Browsers-Bg-Image",
         Nick    => -"Background image",
         Flags   => Param_Readable,
         Blurb   =>
           -("Image to draw in the background of browsers. If left empty,"
             & " no image is drawn. Using a large image will slow down"
             & " performances"),
         Default => ""));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Bg_Image),
         -"Browsers");

      Browsers_Draw_Grid := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-Draw-Grid",
         Default => True,
         Blurb   => -("Whether a grid should be displayed in the browsers"),
         Flags   => Param_Readable,
         Nick    => -"Draw grid"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Draw_Grid),
         -"Browsers");

      Browsers_Hyper_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Hyper-Link-Color",
         Default => "#0000FF",
         Blurb   => -"Color used to draw the hyper links in the items",
         Nick    => -"Hyper link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Hyper_Link_Color),
         -"Browsers");

      Selected_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Link-Color",
         Default => "#FF0000",
         Blurb   => -"Color to use for links between selected items",
         Nick    => -"Selected link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Link_Color),
         -"Browsers");

      Unselected_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Unselected-Link-Color",
         Default => "#000000",
         Blurb   => -"Color to use for links between unselected items",
         Nick    => -"Default link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Unselected_Link_Color),
         -"Browsers");

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

      -- Diff_Utils --

      Diff_Context_Length := Param_Spec_Int (Gnew_Int
        (Name    => "Diff_Utils-Context-Length",
         Minimum => -1,
         Maximum => Gint'Last,
         Default => 5,
         Blurb   => -("The number of lines displayed before and after each"
                      & " chunk of differences. -1 to display the whole file"),
         Nick    => -"Context length"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Diff_Context_Length), -"Visual diff");

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
         Blurb   =>
           -"Command used to apply a patch. Arguments can also be specified",
         Default => "patch"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Patch_Cmd), -"Visual diff");

      -- Messages --

      Message_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Messages-Highlight-Color",
         Nick    => -"Color highlighting",
         Blurb   => -"Color used to highlight text in the messages window",
         Default => "#FF0000"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Message_Highlight), -"Messages");

      Message_Src_Highlight := Param_Spec_Color (Gnew_Color
        (Name    => "Messages-Src-Highlight-Color",
         Nick    => -"Errors highlighting",
         Blurb   => -"Color used to highlight errors in the source editors",
         Default => "#FFA0A0"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Message_Src_Highlight), -"Messages");

      Show_Build_Progress := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "Messages-Show-Build-Progress",
            Nick    => -"Show build progress",
            Blurb   =>
              -"True if the progress bar should show progress during builds",
            Default => True));
      Register_Property
        (Kernel.Preferences, Param_Spec (Show_Build_Progress), -"Messages");

      File_Pattern := Param_Spec_String
        (Gnew_String
           (Name  => "Messages-File-Pattern",
            Nick  => -"File pattern",
            Blurb =>
              -"Pattern used to detect file locations (e.g error messages)",
            Default => "^([^:]+):(\d+):(\d+)?"));
      Register_Property
        (Kernel.Preferences, Param_Spec (File_Pattern), -"Messages");

      File_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-File-Pattern-Index",
         Minimum => 1,
         Maximum => 9,
         Default => 1,
         Blurb   => -"Index of filename in the pattern",
         Nick    => -"File index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (File_Pattern_Index), -"Messages");

      Line_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Line-Pattern-Index",
         Minimum => 1,
         Maximum => 9,
         Default => 2,
         Blurb   => -"Index of line number in the pattern",
         Nick    => -"Line index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Line_Pattern_Index), -"Messages");

      Column_Pattern_Index := Param_Spec_Int (Gnew_Int
        (Name    => "Messages-Column-Pattern-Index",
         Minimum => 0,
         Maximum => 9,
         Default => 3,
         Blurb   => -"Index of column number in the pattern, 0 if none",
         Nick    => -"Column index"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Column_Pattern_Index), -"Messages");

      -- Project Editor --

      Default_Switches_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Prj-Editor-Default-Switches-Color",
         Default => "#777777",
         Blurb   => -("Color to use when displaying switches that are set"
                      & " as default for all the files in the project"),
         Nick    => -"Default switches color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Switches_Color), -"Project");

      Switches_Editor_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Prj-Editor-Title-Font",
         Default => "sans bold oblique 14",
         Blurb   => -"Font to use for the switches editor dialog",
         Nick    => -"Title font",
         Flags   => Param_Readable));
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

      Generate_Relative_Paths := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Prj-Editor-Generate-Relative-Paths",
         Default => True,
         Blurb   => -("Whether paths should be absolute or relative when the"
                      & " projects are modified"),
         Nick    => -"Relative paths"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Generate_Relative_Paths), -"Project");

      -- Wizards --

      Wizard_Toc_Highlight_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Wizard-Toc-Highlight-Color",
         Default => "yellow",
         Blurb   => -"Color to use to highlight strings in the TOC",
         Nick    => -"Table of contents color",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Wizard_Toc_Highlight_Color),
         -"Project wizard");

      Wizard_Title_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Wizard-Title-Font",
         Default => "sans bold oblique 10",
         Blurb   => -"Font to use for the title of the pages in the wizard",
         Nick    => -"Title font",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Wizard_Title_Font),
         -"Project wizard");

      -- Help --

      Help_Font_Adjust := Param_Spec_Int (Gnew_Int
        (Name    => "Help-Font-Adjust",
         Minimum => -10,
         Maximum => 10,
         Default => 0, --  ie undefined, see help_module.adb
         Blurb   => -"Zoom level for help windows",
         Nick    => -"Font Adjustment",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Help_Font_Adjust), -"Help");

      -- VCS --

      Hide_Up_To_Date := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "VCS-Hide-Up-To-Date",
         Default => False,
         Flags   => Param_Readable,
         Blurb   => -"Whether up to date files should be hidden by default",
         Nick    => -"Hide up-to-date files"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Hide_Up_To_Date), -"VCS");

      Hide_Not_Registered := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "VCS-Hide-Not-Registered",
         Default => False,
         Flags   => Param_Readable,
         Blurb   => -"Whether unregistered files should be hidden by default",
         Nick    => -"Hide non registered files"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Hide_Not_Registered), -"VCS");

      -- CVS --

      CVS_Command := Param_Spec_String (Gnew_String
        (Name    => "CVS-Command",
         Default => "cvs",
         Blurb   => -"General CVS command",
         Flags   => Param_Readable,
         Nick    => -"CVS command"));
      Register_Property
        (Kernel.Preferences, Param_Spec (CVS_Command), -"VCS:CVS");

      -- ClearCase --

      ClearCase_Command := Param_Spec_String (Gnew_String
        (Name    => "ClearCase-Command",
         Default => "cleartool",
         Blurb   => -"General ClearCase command",
         Flags   => Param_Readable,
         Nick    => -"ClearCase command"));
      Register_Property
        (Kernel.Preferences, Param_Spec (ClearCase_Command), -"VCS:ClearCase");

   end Register_Global_Preferences;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class) is
      procedure On_Changed (Manager : access Preferences_Manager_Record'Class);
      --  Called when the preferences have been changed.

      ----------------
      -- On_Changed --
      ----------------

      procedure On_Changed
        (Manager : access Preferences_Manager_Record'Class)
      is
         pragma Unreferenced (Manager);
      begin
         Preferences_Changed (Kernel);
      end On_Changed;

   begin
      Edit_Preferences
        (Manager           => Kernel.Preferences,
         Parent            => Get_Main_Window (Kernel),
         On_Changed        => On_Changed'Unrestricted_Access);
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
      Pref   : Glib.Properties.Creation.Param_Spec_Int) return Glib.Gint is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_Boolean) return Boolean is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_String) return String is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Font) return Pango.Font.Pango_Font_Description is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_Enum) return Gint is
   begin
      return Get_Pref (Kernel.Preferences, Pref);
   end Get_Pref;

   procedure Get_Pref
     (Kernel   : access Kernel_Handle_Record'Class;
      Pref     : Param_Spec_Key;
      Modifier : out Gdk_Modifier_Type;
      Key      : out Gdk_Key_Type) is
   begin
      Get_Pref (Kernel.Preferences, Pref, Modifier, Key);
   end Get_Pref;

   function Get_Pref_Font
     (Kernel   : access Kernel_Handle_Record'Class;
      Pref     : Param_Spec_Style) return Pango.Font.Pango_Font_Description is
   begin
      return Get_Pref_Font (Kernel.Preferences, Pref);
   end Get_Pref_Font;

   function Get_Pref_Fg
     (Kernel   : access Kernel_Handle_Record'Class;
      Pref     : Param_Spec_Style) return Gdk.Color.Gdk_Color is
   begin
      return Get_Pref_Fg (Kernel.Preferences, Pref);
   end Get_Pref_Fg;

   function Get_Pref_Bg
     (Kernel   : access Kernel_Handle_Record'Class;
      Pref     : Param_Spec_Style) return Gdk.Color.Gdk_Color is
   begin
      return Get_Pref_Bg (Kernel.Preferences, Pref);
   end Get_Pref_Bg;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Boolean;
      Value  : Boolean) is
   begin
      Set_Pref (Kernel.Preferences, Pspec_Name (Param_Spec (Pref)), Value);
   end Set_Pref;

   --------------
   -- Set_Pref --
   --------------

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Int;
      Value  : Glib.Gint) is
   begin
      Set_Pref (Kernel.Preferences, Pspec_Name (Param_Spec (Pref)), Value);
   end Set_Pref;

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
