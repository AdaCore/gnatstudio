-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gdk.Color;
with Glib.Properties;
with Glib;
with Pango.Font;
with Default_Preferences; use Default_Preferences;

package Glide_Kernel.Preferences is

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
     Register_Property ("General:Default_Widget_Width", 400);
   --  Default width for the widgets put in the MDI

   Default_Widget_Height : constant Glib.Properties.Property_Int :=
     Register_Property ("General:Default_Widget_Height", 400);
   --  Default height for the widgets put in the MDI

   Animated_Image : constant Glib.Properties.Property_String :=
     Register_Property ("General:Animated_Image", "gps-animation.gif");
   --  Animated image used to inform the user about a command in process.

   Tmp_Dir : constant Glib.Properties.Property_String :=
     Register_Property ("General:Tmp_Dir", "/tmp");
   --  Directory used to create temporary files

   -------------
   -- Console --
   -------------

   Highlight_File : constant Property_Color :=
     Register_Property ("Console:Highlight_File", "#FF0000");
   --  Color used to highlight a file in the console

   Highlight_Error : constant Property_Color :=
     Register_Property ("Console:Highlight_Error", "#FF0000");
   --  Color used to highlight an error in the console

   ----------------
   -- Diff_Utils --
   ----------------

   Diff_Cmd : constant Glib.Properties.Property_String :=
     Register_Property ("Diff_Utils:Diff", "diff");
   --  Command used to compute differences between two files.
   --  ??? not used

   Patch_Cmd : constant Glib.Properties.Property_String :=
     Register_Property ("Diff_Utils:Patch", "patch");
   --  Command used to apply a patch.
   --  ??? not used

   --------------
   -- Explorer --
   --------------

   Absolute_Directories : constant Glib.Properties.Property_Boolean :=
     Register_Property ("Explorer:Absolute_Directories", False);
   --  True if directories should be displayed as absolute names,
   --  False if they should be relative to the current directory set by the
   --  user.

   Show_Directories : constant Glib.Properties.Property_Boolean :=
     Register_Property ("Explorer:Show_Directories", True);
   --  Whether directories should be displayed in the tree.  If False, only the
   --  projects are shown.

   Ada_Extensions : constant Glib.Properties.Property_String :=
     Register_Property ("File_Extensions:Ada", ".ads;.adb;.ada;.a;.dg");
   C_Extensions : constant Glib.Properties.Property_String :=
     Register_Property ("File_Extensions:C", ".c;.h");
   Cpp_Extensions : constant Glib.Properties.Property_String :=
     Register_Property ("File_Extensions:Cpp", ".cc;.cpp;.C;.hh;.H");
   --  The file extensions that are recognized for each language.
   --  Each possible extension should be separated from the others by a
   --  semicolon.

   -------------------
   -- Source Editor --
   -------------------

   Default_Keyword_Color : constant Property_Color :=
     Register_Property ("Src_Editor:Default_Keyword_Color", "");
   --  Default color for highlighting keywords
   --  ??? not used

   Default_Comment_Color : constant Property_Color :=
     Register_Property ("Src_Editor:Default_Comment_Color", "blue");
   --  Default color for highlighting comments
   --  ??? not used

   Default_String_Color : constant Property_Color :=
     Register_Property ("Src_Editor:Default_String_Color", "brown");
   --  Default color for highlighting strings
   --  ??? not used

   Default_Character_Color : constant Property_Color :=
     Register_Property ("Src_Editor:Default_Character_Color", "brown");
   --  Default color for highlighting characters
   --  ??? not used

   Automatic_Indentation : constant Glib.Properties.Property_Boolean :=
     Register_Property ("Src_Editor:Automatic_Indentation", True);
   --  Whether the editor should indent automatically the source
   --  ??? not used

   Default_Source_Editor_Font : constant Property_Font :=
     Register_Property ("Src_Editor:Default_Font", "Courier 10");
   --  The font used in the source editor.

   ---------------------
   -- External editor --
   ---------------------

   Default_External_Editor : constant Glib.Properties.Property_String :=
     Register_Property ("External_Editor:Default_Editor", "");
   --  The default external editor to use. It should be a value from
   --  External_Editor_Module.Supported_Clients, or the empty string, in which
   --  case gps will automatically select the first available client

   Always_Use_External_Editor : constant Glib.Properties.Property_Boolean :=
     Register_Property ("External_Editor:Always_Use_External_Editor", False);
   --  True if all editions should be done with the external editor. This will
   --  deactivate completely the internal editor. On the other hand, if this is
   --  False, then the external editor will need to be explicitely selected by
   --  the user.

   --------------------
   -- Project Editor --
   --------------------

   Timestamp_Picture : constant Glib.Properties.Property_String :=
     Register_Property ("Prj_Editor:Timestamp_Picture", "%Y/%m/%d %H:%M:%S");
   --  Format used to display timestamps in the project editor

   Default_Switches_Color : constant Property_Color :=
     Register_Property ("Prj_Editor:Default_Switches_Color", "#777777");
   --  Color to use when displaying switches that are not file specific, but
   --  set at the project or package level.

   Switches_Editor_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property ("Prj_Editor:Title_Font", "helvetica bold oblique 14");
   --  Font to use for the switches editor dialog

   Variable_Ref_Background : constant Property_Color :=
     Register_Property ("Prj_Editor:Var_Ref_Bg", "#AAAAAA");
   --  Color to use for the background of variable references in the value
   --  editor

   Invalid_Variable_Ref_Background : constant Property_Color :=
     Register_Property ("Prj_Editor:Invalid_Var_Ref_Bg", "#AA0000");
   --  Color to use for the foreground of invalid variable references.

   -------------
   -- Wizards --
   -------------

   Wizard_Toc_Highlight_Color : constant Property_Color :=
     Register_Property ("Wizard:Toc_Highlight_Color", "yellow");
   --  Color to use to highlight strings in the TOC.

   Wizard_Title_Font : constant Glib.Properties.Property_String :=
     Register_Property ("Wizard:Title_Font", "helvetica bold oblique 10");
   --  Font to use for the title of the pages in the wizard

   --------------
   -- Browsers --
   --------------

   Browsers_Link_Font : constant Property_Font :=
     Register_Property ("Browsers:Link_Font", "Helvetica 10");
   --  Font used to draw the links in the items

   Browsers_Link_Color : constant Property_Color :=
     Register_Property ("Browsers:Link_Color", "#0000FF");
   --  Color used to draw the links in the items

   Selected_Link_Color : constant Property_Color :=
     Register_Property ("Browsers:Selected_Link_Color", "#FF0000");
   --  Color to use links whose ends are selected.

   Selected_Item_Color : constant Property_Color :=
     Register_Property ("Browsers:Selected_Item_Color", "#888888");
   --  Color to use to draw the selected item.

   Parent_Linked_Item_Color : constant Property_Color :=
     Register_Property ("Browsers:Linked_Item_Color", "#AAAAAA");
   Child_Linked_Item_Color : constant Property_Color :=
     Register_Property ("Browsers:Child_Linked_Item_Color", "#DDDDDD");
   --  Color to use to draw the items that are linked to the selected item.

   Browsers_Vertical_Layout : constant Glib.Properties.Property_Boolean :=
     Register_Property ("Browsers:Vertical_Layout", True);
   --  Whether the layout of the graph should be vertical or horizontal

   ---------
   -- VCS --
   ---------

   VCS_Commit_File_Check : constant Glib.Properties.Property_String :=
     Register_Property ("VCS:Commit_File_Check", "");
   --  A script that will be called with one source file as argument
   --  before VCS Commit operations.

   VCS_Commit_Log_Check : constant Glib.Properties.Property_String :=
     Register_Property ("VCS:Commit_Log_Check", "");
   --  A script that will be called with one log file as argument before
   --  VCS Commit operations.

   ---------
   -- CVS --
   ---------

   CVS_Command : constant Glib.Properties.Property_String :=
     Register_Property ("CVS:Command", "cvs");
   --  General CVS command

end Glide_Kernel.Preferences;
