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

      Display_Line_Numbers := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Line_Numbers",
         Default => True,
         Blurb   => -("Whether the line numbers should be displayed in"
                       & " file editors"),
         Nick    => -"Display line numbers"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Line_Numbers), -"Editor");

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

      GVD.Preferences.Register_Default_Preferences
        (Kernel.Preferences, -"Debugger" & ':', "Debugger-");
   end Register_Global_Preferences;

   ----------------------
   -- Edit_Preferences --
   ----------------------

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Edit_Preferences
        (Manager           => Kernel.Preferences,
         Parent            => Get_Main_Window (Kernel),
         On_Changed        => null);
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
