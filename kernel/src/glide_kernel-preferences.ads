-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
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

with Gdk.Color;
with Glib.Properties;
with Glib;
with Pango.Font;
with Default_Preferences; use Default_Preferences;
with Glide_Intl;          use Glide_Intl;

package Glide_Kernel.Preferences is

   procedure Free_Preferences
     (Kernel    : access Kernel_Handle_Record'Class);
   --  Free the memory used by the preferences tree. Note that you should no
   --  longer call Get_Pref after calling this subprogram.

   procedure Load_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String);
   --  Load the preferences from the specified file.
   --  No query is allowed before loading the preferences

   procedure Save_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String);
   --  Save the preferences in the specified file.
   --  Note that only the preferences that have been modified by the user are
   --  saved.

   procedure Set_Default_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Reset the preferences to their default value.

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Int) return Glib.Gint;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Uint) return Glib.Guint;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_Boolean) return Boolean;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Property_String) return String;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Property_Color) return Gdk.Color.Gdk_Color;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Property_Font) return Pango.Font.Pango_Font_Description;
   --  Return the value for a specific property.
   --  The colors and fonts have already been allocated when they are returned.

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   -------------
   -- General --
   -------------

   Default_Widget_Width : constant Glib.Properties.Property_Int :=
     Register_Property
     ("General:Default_Widget_Width",
      400,
      -"Default width for all the newly created windows",
      -"Default width",
      -"General");

   Default_Widget_Height : constant Glib.Properties.Property_Int :=
     Register_Property
     ("General:Default_Widget_Height",
      400,
      -"Default height for all the newly created windows",
      -"Default height",
      -"General");

   Animated_Image : constant Glib.Properties.Property_String :=
     Register_Property
     ("General:Animated_Image",
      "gps-animation.gif",
      -"Animated image used to inform the user about a command in process",
      -"Animated image",
      -"General",
      Editable => False);

   Splash_Screen : constant Glib.Properties.Property_String :=
     Register_Property
     ("General:Splash_Screen",
      "gps-splash.jpg",
      -"Splash screen displayed (if found) when starting GPS",
      -"Splash screen",
      -"General",
      Editable => False);

   Tmp_Dir : constant Glib.Properties.Property_String :=
     Register_Property
     ("General:Tmp_Dir",
      "/tmp",
      -"Directory used to create temporary files",
      -"Temporary directory",
      -"General");

   -------------
   -- Console --
   -------------

   Highlight_File : constant Property_Color :=
     Register_Property
     ("Console:Highlight_File",
      "#FF0000",
      -"Color used to highlight a file reference in the console",
      -"Highlight color",
      -"Console");

   Highlight_Error : constant Property_Color :=
     Register_Property
     ("Console:Highlight_Error",
      "#FF0000",
      -"Color used to highlight an error in the console",
      -"Errors color",
      -"Console");

   ----------------
   -- Diff_Utils --
   ----------------

   Diff_Cmd : constant Glib.Properties.Property_String :=
     Register_Property
     ("Diff_Utils:Diff",
      "diff",
      -("Command used to compute differences between two files. Arguments"
        & " can also be specified"),
      -"Diff command",
      -"Visual diff");
   --  ??? not used

   Patch_Cmd : constant Glib.Properties.Property_String :=
     Register_Property
     ("Diff_Utils:Patch",
      "patch",
      -"Command used to apply a patch. Arguments can also be specified",
      -"Patch command",
      -"Visual diff");
   --  ??? not used

   --------------
   -- Explorer --
   --------------

   Normalized_Directories : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Explorer:Normalized_Directories",
      False,
      -("True if directories should be fully normalized, eg links should be"
        & " resolved. False if the explorer should display the name as given"
        & " in the project file"),
      -"Normalize directories",
      -"Explorer");

   Show_Directories : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Explorer:Show_Directories",
      True,
      -"True if directories should be displayed in the explorer",
      -"Show directories",
      -"Explorer",
      Editable => False);
   --  ??? Does it really make sense.

   File_View_Shows_Only_Project : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Prj_Editor:File_View_Shows_Only_Project",
      False,
      -("True if the files view should only show files belonging to the"
        & " project"),
      -"Files from project only",
      -"Explorer");

   -------------------
   -- Source Editor --
   -------------------

   Default_Keyword_Color : constant Property_Color :=
     Register_Property
     ("Src_Editor:Keyword_Color",
      "",
      -"Color for highlighting keywords",
      -"Keywords color",
      -"Editor");

   Default_Comment_Color : constant Property_Color :=
     Register_Property
     ("Src_Editor:Comment_Color",
      "blue",
      -"Color for highlighting comments",
      -"Comments color",
      -"Editor");

   Default_String_Color : constant Property_Color :=
     Register_Property
     ("Src_Editor:String_Color",
      "brown",
      -"Color for highlighting strings",
      -"Strings color",
      -"Editor");

   Default_Character_Color : constant Property_Color :=
     Register_Property
     ("Src_Editor:Character_Color",
      "brown",
      -"Color for highlighting character constants",
      -"Character constants color",
      -"Editor");

   Default_HL_Line_Color   : constant Property_Color :=
     Register_Property
     ("Src_Editor:Highlight_Line_Color",
      "green",
      -"Color for highlighting lines",
      -"Line highlighting color",
      -"Editor");
   --  ???

   Default_HL_Region_Color : constant Property_Color :=
     Register_Property
     ("Src_Editor:Highlight_Region_Color",
      "cyan",
      -"Color for highlighting regions",
      -"Region highlighting color",
      -"Editor");

   Automatic_Indentation : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Src_Editor:Automatic_Indentation",
      True,
      -"Whether the editor should indent automatically the source",
      -"Auto indent",
      -"Editor");

   Strip_Blanks : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Src_Editor:Strip_Blanks",
      True,
      -"Whether the editor should remove trailing blanks when saving a file",
      -"Strip blanks",
      -"Editor");

   Default_Source_Editor_Font : constant Property_Font :=
     Register_Property
     ("Src_Editor:Default_Font",
      "Courier 10",
      -"The font used in the source editor",
      -"Default font",
      -"Editor");

   Display_Tooltip : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Src_Editor:Display_Tooltip",
      True,
      -"Whether tooltips should be displayed automatically",
      -"Tooltips",
      -"Editor");

   Periodic_Save : constant Glib.Properties.Property_Int :=
     Register_Property
     ("Src_Editor:Periodic_Save",
      60,
      -("The period (in seconds) after which a source editor is automatically"
        & " saved. 0 if none."),
      -"Autosave delay",
      "Editor");

   Tab_Width : constant Glib.Properties.Property_Int :=
     Register_Property
     ("Src_Editor:Tab_Width",
      8,
      -"The width of a tabulation character, in characters",
      -"Tabulation width",
      -"Editor");

   --------------------
   -- External editor --
   ---------------------

   Default_External_Editor : constant Glib.Properties.Property_String :=
     Register_Property
     ("External_Editor:Default_Editor",
      "emacs",
      -"The default external editor to use",
      -"External editor",
      -"Editor");
   --  It should be a value from External_Editor_Module.Supported_Clients, or
   --  the empty string, in which case GPS will automatically select the first
   --  available client.
   --  ??? Should use an enumeration type.

   Always_Use_External_Editor : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("External_Editor:Always_Use_External_Editor",
      False,
      -("True if all editions should be done with the external editor. This"
        & " will deactivate completely the internal editor. False if the"
        & " external editor needs to be explicitely called by the user."),
      -"Always use external editor",
      -"Editor");

   --------------------
   -- Project Editor --
   --------------------

   Default_Switches_Color : constant Property_Color :=
     Register_Property
     ("Prj_Editor:Default_Switches_Color",
      "#777777",
      -("Color to use when displaying switches that are set as default"
        & " for all the files in the project"),
      -"Default switches color",
      -"Project editor");

   Switches_Editor_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Prj_Editor:Title_Font",
      "helvetica bold oblique 14",
      -"Font to use for the switches editor dialog",
      -"Title font",
      -"Project editor");

   Variable_Ref_Background : constant Property_Color :=
     Register_Property
     ("Prj_Editor:Var_Ref_Bg",
      "#AAAAAA",
      -("Color to use for the background of variable references in the value"
        & " editor"),
      -"Variable reference color",
      -"Project editor",
      Editable => False);
   --  ??? Not used.

   Invalid_Variable_Ref_Background : constant Property_Color :=
     Register_Property
     ("Prj_Editor:Invalid_Var_Ref_Bg",
      "#AA0000",
      -"Color to use for the foreground of invalid variable references",
      -"Invalid references color",
      -"Project editor",
      Editable => False);
   --  ??? Not used.

   -------------
   -- Wizards --
   -------------

   Wizard_Toc_Highlight_Color : constant Property_Color :=
     Register_Property
     ("Wizard:Toc_Highlight_Color",
      "yellow",
      -"Color to use to highlight strings in the TOC",
      -"Table of contents color",
      -"Project Wizard");

   Wizard_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Wizard:Title_Font",
      "helvetica bold oblique 10",
      -"Font to use for the title of the pages in the wizard",
      -"Title font",
      -"Project Wizard");

   --------------
   -- Browsers --
   --------------

   Browsers_Link_Font : constant Property_Font := Register_Property
     ("Browsers:Link_Font",
      "Helvetica 10",
      -"Font used to draw the links between items",
      -"Links font",
      -"Browsers");

   Browsers_Link_Color : constant Property_Color := Register_Property
     ("Browsers:Link_Color",
      "#0000FF",
      -"Color used to draw the links between unselected items",
      -"Link color",
      -"Browsers");

   Selected_Link_Color : constant Property_Color := Register_Property
     ("Browsers:Selected_Link_Color",
      "#FF0000",
      -"Color to use for links between selected items",
      -"Selected link color",
      -"Browsers");

   Selected_Item_Color : constant Property_Color := Register_Property
     ("Browsers:Selected_Item_Color",
      "#888888",
      -"Color to use to draw the selected item",
      -"Selected item color",
      -"Browsers");

   Parent_Linked_Item_Color : constant Property_Color := Register_Property
     ("Browsers:Linked_Item_Color",
      "#AAAAAA",
      -("Color to use for the background of the items linked to the"
        & " selected item"),
      -"Ancestor items color",
      -"Browsers");

   Child_Linked_Item_Color : constant Property_Color := Register_Property
     ("Browsers:Child_Linked_Item_Color",
      "#DDDDDD",
      -("Color to use for the background of the items linked from the"
        & " selected item"),
      -"Offspring items color",
      -"Browsers");

   Browsers_Vertical_Layout : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Browsers:Vertical_Layout",
      True,
      -"Whether the layout of the graph should be vertical or horizontal",
      -"Vertical layout",
      -"Browsers");

   ---------
   -- VCS --
   ---------

   VCS_Commit_File_Check : constant Glib.Properties.Property_String :=
     Register_Property
     ("VCS:Commit_File_Check",
      "",
      -("Script called with one source file as argument, before a VCS commit"
        & " operation"),
      -"Commit file check",
      -"VCS::General");

   VCS_Commit_Log_Check : constant Glib.Properties.Property_String :=
     Register_Property
     ("VCS:Commit_Log_Check",
      "",
      -("Script that will be called with one log file as argument before"
        & " aVCS Commit operations"),
      -"Commit log check",
      -"VCS::General");

   ---------
   -- CVS --
   ---------

   CVS_Command : constant Glib.Properties.Property_String :=
     Register_Property
     ("CVS:Command",
      "cvs",
      -"General CVS command",
      -"CVS location",
      -"VCS::CVS");

   --------------
   -- Debugger --
   --------------

   --  General

   Break_On_Exception : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Debugger:Break_On_Exception",
      False,
      -("True if the debugger should automatically stop when an exception"
        & " is raised"),
      -"Break on exceptions",
      -"Debugger::General");

   --  Assembly Window

   Asm_Highlight_Color : constant Property_Color := Register_Property
     ("Debugger:Asm_Highlight_Color",
      "#FF0000",
      -"Color to use to highlight the assembly code for the current line",
      -"Current line",
      -"Debugger::Assembly");

   Assembly_Range_Size : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Assembly_Range_Size",
      "200",
      -("Size of the range to display when initially displaying the"
        & " assembly window." & ASCII.LF
        & " If this size is 0, then the whole function is displayed, but"
        & " this can potentially take a very long time on slow machines"
        & " or big functions"),
      -"Range size",
      -"Debugger::Assembly");

   --  Data Window

   Xref_Color : constant Property_Color := Register_Property
     ("Debugger:Xref_Color",
      "#0000FF",
      -"Color to use for the items that are clickable (blue)",
      -"Clickable item",
      -"Debugger::Data");

   Title_Color : constant Property_Color := Register_Property
     ("Debugger:Title_Color",
      "#BEBEBE",
      -"Color to use for the background of the title (grey)",
      -"Title background",
      -"Debugger::Data");

   Change_Color : constant Property_Color := Register_Property
     ("Debugger:Change_Color",
      "#FF0000",
      -("Color used to highlight fields that have changed since the last"
        & " update (default is red)"),
      -"Changed data",
      -"Debugger::Data");

   Selection_Color : constant Property_Color := Register_Property
     ("Debugger:Selection_Color",
      "#000000",
      -"Color used to handle item selections",
      -"Selected item",
      -"Debugger::Data");

   Thaw_Bg_Color : constant Property_Color := Register_Property
     ("Debugger:Thaw_Bg_Color",
      "#FFFFFF",
      -"Color used for auto-refreshed items (white)",
      -"Auto-Refreshed",
      -"Debugger::Data");

   Freeze_Bg_Color : constant Property_Color := Register_Property
     ("Debugger:Freeze_Bg_Color",
      "#AAAAAA",
      -"Color used for frozen items (light grey)",
      -"Frozen",
      -"Debugger::Data");

   Debugger_Data_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Data_Title_Font",
      "helvetica bold 10",
      -"Font used for the name of the item",
      -"Item Name",
      -"Debugger::Data");

   Value_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Data_Value_Font",
      "helvetica 10",
      -"Font used to display the value of the item",
      -"Item Value",
      -"Debugger::Data");

   Command_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Data_Command_Font",
      "courier 10",
      -"Font used to display the output of gdb commands (graph print `...`)",
      -"Command items",
      -"Debugger::Data");

   Type_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Data_Type_Font",
      "helvetica oblique 10",
      -"Font used to display the type of the item",
      -"Item Type",
      -"Debugger::Data");

   Hide_Big_Items : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Debugger:Hide_Big_Items",
      True,
      -("If True, items higher than a given limit will start in a hidden"
        & " state"),
      -"Hide Big Items",
      -"Debugger::Data");

   Big_Item_Height : constant Glib.Properties.Property_Int :=
     Register_Property
     ("Debugger:Big_Items_Height",
      150,
      -"Items taller than this value will start hidden",
      -"Big Item Height",
      -"Debugger::Data");

   Default_Detect_Aliases : constant Glib.Properties.Property_Boolean :=
     Register_Property
     ("Debugger:Default_Detect_Aliased",
      True,
      -("If True, do not create new items when a matching item is already"
        & " present in the canvas"),
      -"Detect aliases (shared data structures)",
      -"Debugger::Data");

   --  Command Window

   Debugger_Highlight_Color : constant Property_Color := Register_Property
     ("Debugger:Command_Highlight_Color",
      "#0000FF",
      -"Color used for highlighting in the debugger window (blue)",
      -"Highlighting",
      -"Debugger::Command");

   Debugger_Command_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Command_Font",
      "courier 12",
      -"Font used in the debugger text window",
      -"Font",
      -"Debugger::Command");

   --  Memory Window

   Memory_View_Font : constant Glib.Properties.Property_String :=
     Register_Property
     ("Debugger:Memory_View_Font",
      "courier 12",
      -"Font use in the memory view window",
      -"Font",
      -"Debugger::Memory");

   Memory_View_Color : constant Property_Color :=
     Register_Property
     ("Debugger:Memory_View_Color",
      "#333399",
      -"Color used by default in the memory view window",
      -"Default color",
      -"Debugger::Memory");

   Memory_Highlighted_Color : constant Property_Color :=
     Register_Property
     ("Debugger:Memory_Highlighted_Color",
      "#DDDDDD",
      -"Color used for highlighted items in the memory view",
      -"Color highlighting",
      -"Debugger::Memory");

   Memory_Selected_Color : constant Property_Color :=
     Register_Property
     ("Debugger:Memory_Selected_Color",
      "#00009C",
      -"Color used for selected items in the memory view",
      -"Selection",
      -"Debugger::Memory");

   Memory_Modified_Color : constant Property_Color :=
     Register_Property
     ("Debugger:Memory_Modified_Color",
      "#FF0000",
      -"Color used for modified items in the memory view",
      -"Modified",
      -"Debugger::Memory");

   -------------
   -- Helpers --
   -------------

   List_Processes : constant Glib.Properties.Property_String :=
     Register_Property
     ("Helpers:List_Processes",
      "ps x",
      -"Command to use to list processes running on the machine",
      -"List processes",
      -"Debugger::Helpers");

   Remote_Protocol : constant Glib.Properties.Property_String :=
     Register_Property
     ("Helpers:Remote_Protocol",
      "rsh",
      -"How to run a process on a remote machine",
      -"Remote shell",
      -"Debugger::Helpers");

   Remote_Copy : constant Glib.Properties.Property_String :=
     Register_Property
     ("Helpers:Remote_Copy",
      "rcp",
      -"Program used to copy a file from a remote host",
      -"Remote copy",
      -"Debugger::Helpers");

end Glide_Kernel.Preferences;
