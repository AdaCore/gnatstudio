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

      Splash_Screen := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "General-Splash-Screen",
         Nick    => -"Display splash screen",
         Blurb   => -("True if a splash screen should be displayed when"
                      & " starting GPS"),
         Default => True));
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
         Default => True,
         Flags   => Param_Readable));
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
         Default => False,
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (File_View_Shows_Only_Project),
         -"Explorer");

      -- Source Editor --

      Strip_Blanks := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Strip-Blanks",
         Default => True,
         Blurb   => -("Whether the editor should remove trailing blanks"
                      & " when saving a file"),
         Nick    => -"Strip blanks"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Strip_Blanks), -"Editor:General");

      Display_Line_Numbers := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Line_Numbers",
         Default => True,
         Blurb   =>
           -("Whether the line numbers should be displayed in file editors"),
         Nick    => -"Display line numbers"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Line_Numbers),
         -"Editor:General");

      Display_Tooltip := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Display-Tooltip",
         Default => True,
         Blurb   => -"Whether tooltips should be displayed automatically",
         Nick    => -"Tooltips"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Display_Tooltip), -"Editor:General");

      Periodic_Save := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Periodic-Save",
         Minimum => 0,
         Maximum => 3600,
         Default => 60,
         Blurb   => -("The period (in seconds) after which a source editor"
                      & " is automatically saved. 0 if none."),
         Nick    => -"Autosave delay"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Periodic_Save), -"Editor:General");

      Tab_Width := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Tab-Width",
         Minimum => 1,
         Maximum => 16,
         Default => 8,
         Blurb   => -"The width of a tabulation character, in characters",
         Nick    => -"Tabulation width",
         Flags   => Param_Readable));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tab_Width), -"Editor:General");

      Default_Keyword_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Keyword-Color",
         Nick    => -"Keywords color",
         Blurb   => -"Color for highlighting keywords",
         Default => "black"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Keyword_Color),
         -"Editor:Fonts & Colors");

      Default_Comment_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Comment-Color",
         Default => "blue",
         Blurb   => -"Color for highlighting comments",
         Nick    => -"Comments color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Comment_Color),
         -"Editor:Fonts & Colors");

      Default_String_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-String-Color",
         Default => "brown",
         Blurb   => -"Color for highlighting strings",
         Nick    => -"Strings color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_String_Color),
         -"Editor:Fonts & Colors");

      Default_Character_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Character-Color",
         Default => "brown",
         Blurb   => -"Color for highlighting character constants",
         Nick    => -"Character constants color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_Character_Color),
         -"Editor:Fonts & Colors");

      Default_HL_Line_Color   := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Line-Color",
         Default => "green",
         Blurb   => -"Color for highlighting lines",
         Nick    => -"Line highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_HL_Line_Color),
         -"Editor:Fonts & Colors");

      Default_HL_Region_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Src-Editor-Highlight-Region-Color",
         Default => "cyan",
         Blurb   => -"Color for highlighting regions",
         Nick    => -"Region highlighting color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Default_HL_Region_Color),
         -"Editor:Fonts & Colors");

      Source_Editor_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Default-Font",
         Default => "Courier 10",
         Blurb   => -"The font used in the source editor",
         Nick    => -"Default font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Source_Editor_Font),
         -"Editor:Fonts & Colors");

      Keyword_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Keyword-Font",
         Default => "Courier Bold 10",
         Blurb   => -"The font used to display keywords",
         Nick    => -"Keyword font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Keyword_Font),
         -"Editor:Fonts & Colors");

      Comment_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Comment-Font",
         Default => "Courier Medium Oblique 10",
         Blurb   => -"The font used to display comments",
         Nick    => -"Comment font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Comment_Font),
         -"Editor:Fonts & Colors");

      String_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-String-Font",
         Default => "Courier 10",
         Blurb   => -"The font used to display strings",
         Nick    => -"String font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (String_Font),
         -"Editor:Fonts & Colors");

      Character_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Character-Font",
         Default => "Courier 10",
         Blurb   => -"The font used to display characters",
         Nick    => -"Character font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Character_Font),
         -"Editor:Fonts & Colors");

      Tooltip_Font := Param_Spec_Font (Gnew_Font
        (Name    => "Src-Editor-Tooltip-Font",
         Default => "Helvetica 10",
         Blurb   => -"The font used to display tooltips in the source editor",
         Nick    => -"Tooltip font"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Tooltip_Font),
         -"Editor:Fonts & Colors");

      Automatic_Indentation := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Src-Editor-Automatic-Indentation",
         Default => True,
         Blurb   => -("Whether the editor should indent automatically"
                      & " the source"),
         Nick    => -"Auto indentation"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Automatic_Indentation),
         -"Editor:Indentation");

      Indentation_Level := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Indent-Level",
         Minimum => 1,
         Maximum => 9,
         Default => 3,
         Blurb   => -"The number of spaces for the default indentation",
         Nick    => -"Default indentation"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Indentation_Level),
         -"Editor:Indentation");

      Continuation_Level := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Continuation-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 2,
         Blurb   => -"The number of extra spaces for continuation lines",
         Nick    => -"Continuation lines"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Continuation_Level),
         -"Editor:Indentation");

      Declaration_Level := Param_Spec_Int (Gnew_Int
        (Name    => "Src-Editor-Declaration-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 0,
         Blurb   =>
           -"The number of extra spaces for multiple line declarations",
         Nick    => -"Declaration lines"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Declaration_Level),
         -"Editor:Indentation");

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
         Default => "helvetica bold oblique 14",
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

      Selector_Show_Project_Hierarchy := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Prj-Editor-Selector-Show-Project-Hierarchy",
         Default => False,
         Blurb   => -("Whether the list of projects to modify through the"
                      & " project properties dialog should be displayed"
                      & " as a hierarchy of projects, or a simple sorted"
                      & " list"),
         Nick    => -"Show project hierarchy"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selector_Show_Project_Hierarchy),
         -"Project");

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
         Default => "helvetica bold oblique 10",
         Blurb   => -"Font to use for the title of the pages in the wizard",
         Nick    => -"Title font",
         Flags   => Param_Readable));
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
        (Kernel.Preferences, Param_Spec (Browsers_Link_Font),
         -"Browsers:General");

      Browsers_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Link-Color",
         Default => "#0000FF",
         Blurb   => -"Color used to draw the links between unselected items",
         Nick    => -"Link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Link_Color),
         -"Browsers:General");

      Selected_Link_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Link-Color",
         Default => "#FF0000",
         Blurb   => -"Color to use for links between selected items",
         Nick    => -"Selected link color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Link_Color),
         -"Browsers:General");

      Selected_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Selected-Item-Color",
         Default => "#888888",
         Blurb   => -"Color to use to draw the selected item",
         Nick    => -"Selected item color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Selected_Item_Color),
         -"Browsers:General");

      Parent_Linked_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Linked-Item-Color",
         Default => "#AAAAAA",
         Blurb   => -("Color to use for the background of the items linked"
                      & " to the selected item"),
         Nick    => -"Ancestor items color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Parent_Linked_Item_Color),
         -"Browsers:General");

      Child_Linked_Item_Color := Param_Spec_Color (Gnew_Color
        (Name    => "Browsers-Child-Linked-Item-Color",
         Default => "#DDDDDD",
         Blurb   => -("Color to use for the background of the items linked"
                      & " from the selected item"),
         Nick    => -"Offspring items color"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Child_Linked_Item_Color),
         -"Browsers:General");

      Browsers_Vertical_Layout := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-Vertical-Layout",
         Default => True,
         Blurb   => -("Whether the layout of the graph should be vertical"
                      & " or horizontal"),
         Nick    => -"Vertical layout"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Browsers_Vertical_Layout),
         -"Browsers:General");

      Dep_Browser_Show_System_Files := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-System-Files",
         Default => False,
         Blurb   => -("Whether the system files (Ada runtime or standard C"
                      & " include files) should be visible in the browser"),
         Nick    => -"Show system files"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Dep_Browser_Show_System_Files),
         -"Browsers:File Dependencies");

      Dep_Browser_Show_Implicit_Dep := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "Browsers-Implicit-Dep",
         Default => False,
         Blurb   => -("If False, then only the explicit dependencies"
                      & " are shown in the browser. Otherwise, all"
                      & " dependencies, even implicit, are displayed"),
         Nick    => -"Show implicit dependencies"));
      Register_Property
        (Kernel.Preferences, Param_Spec (Dep_Browser_Show_Implicit_Dep),
         -"Browsers:File Dependencies");

      -- CVS --

      CVS_Command := Param_Spec_String (Gnew_String
        (Name    => "CVS-Command",
         Default => "cvs",
         Blurb   => -"General CVS command",
         Nick    => -"CVS command"));
      Register_Property
        (Kernel.Preferences, Param_Spec (CVS_Command), -"VCS:CVS");

      GVD.Preferences.Register_Default_Preferences
        (Kernel.Preferences, -"Debugger" & ':', "Debugger-");
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
