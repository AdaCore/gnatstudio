-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Glib; use Glib;
with Gdk.Color;
with General_Preferences_Pkg;

package GVD.Preferences is

   --  This package provides the general interface to the preference settings
   --  in GVD. We have chosen a string-based interface (ie you request a
   --  value by its name) mainly because of its adaptiveness. It provides an
   --  easier interface to a text-based preferences file. It is also easier
   --  to create signals that pass the name of variables around.
   --  Since the values of the preferences are requested only a few times, the
   --  efficiency is not really a crucial matter here.

   procedure Fill_Dialog
     (Dialog : General_Preferences_Pkg.General_Preferences_Access);
   --  Fill up the preference dialog given the current settings

   procedure Apply_Preferences
     (Dialog : General_Preferences_Pkg.General_Preferences_Access);
   --  Apply temporarily the preferences set in Dialog. This change can still
   --  be cancelled by calling Cancel_Preferences.

   procedure Set_Preferences
     (Dialog : General_Preferences_Pkg.General_Preferences_Access);
   --  Set the preferences set in Dialog. This doesn't save the preferences
   --  to a file.

   procedure Cancel_Preferences
     (Dialog : General_Preferences_Pkg.General_Preferences_Access);
   --  Cancel any previous temporary change in the preferences.

   procedure Load_Preferences (File_Name : String);
   --  Load the preferences file, and initialize the preferences database.
   --  No query is allowed before loading the preferences.

   procedure Save_Preferences (File_Name : String);
   --  Save the preferences in the given file.

   procedure Set_Default_Preferences;
   --  Set the default preferences for all values. Existing values are not
   --  overriden.

   type Tooltips_In_Source_Type is (None, Simple, Full);
   --  The types of tooltips that can be displayed in the source window:
   --    None: no tooltips will be displayed.
   --    Simple: the output of the debugger is displayed, no post-processing
   --    Full: the variable is parsed and the tooltip will contain the
   --     equivalent of the canvas'items.

   type String_Guint is new String;
   type String_String is new String;
   type String_Boolean is new String;
   type String_Gint is new String;
   type String_Tooltips_In_Source is new String;
   type String_Color is new String;
   type String_Font is new String;
   --  String used for names of variables that have a specific type

   function Get_Pref (Name : String_Guint) return Guint;
   function Get_Pref (Name : String_String) return String;
   function Get_Pref (Name : String_Boolean) return Boolean;
   function Get_Pref (Name : String_Gint) return Gint;
   function Get_Pref (Name : String_Color) return Gdk.Color.Gdk_Color;
   function Get_Pref (Name : String_Font) return String;
   function Get_Pref
     (Name : String_Tooltips_In_Source) return Tooltips_In_Source_Type;
   --  Get the value of a specific preference parameter.
   --  An assertion failure is raised if you are not getting an existing
   --  preference value or if you are requesting the wrong type.
   --  Colors are already allocated and can be used as is.

   function Get_Tab_Size return Gint;
   --  Special function since Tab_Size is used very often and we need fast
   --  access to it.

   -----------------------
   -- List of constants --
   -----------------------
   --  Note: Below is the list of all the preference settings that can be
   --  set. It is recommended to always access the preferences through these
   --  constant strings, since these are subject to change.
   --  Also, the type of the constant gives the type of the value associated
   --  with the preference.

   -------------------------
   -- General Preferences --
   -------------------------

   Break_On_Exception : constant String_Boolean := "Break_On_Exception";
   --  Break on exceptions.

   Hide_Delay : constant String_Guint := "Hide_Delay";
   --  Delay (in ms) after which the current message is hidden

   Ada_Extensions : constant String_String := "Ada_Extensions";
   --  Semicolon separated list of extensions for Ada files.

   C_Extensions : constant String_String := "C_Extensions";
   --  Semicolon separated list of extensions for C files.

   Cpp_Extensions : constant String_String := "Cpp_Extensions";
   --  Semicolon separated list of extensions for C++ files.

   ---------------------
   -- Explorer Window --
   ---------------------

   Display_Explorer : constant String_Boolean := "Display_Explorer";
   --  True if we should associate an explorer tree to each editor.

   File_Name_Bg_Color : constant String_Color := "File_Name_Bg_Color";
   --  Color used for the background of the file name in the editor (grey).
   --  This is also used for the background of the current frame in the
   --  stack_list window.

   -------------------
   -- Source Window --
   -------------------

   Editor_Font : constant String_Font := "Editor_Font";
   --  Font used in the editor.

   Editor_Font_Size : constant String_Gint := "Editor_Font_Size";
   --  Size of the font used in the editor.

   Editor_Show_Line_Nums : constant String_Boolean := "Editor_Show_Line_Nums";
   --  Whether line numbers should be shown in the code editor

   Editor_Show_Line_With_Code : constant String_Boolean :=
     "Editor_Show_Line_With_Code";
   --  Whether dots should be shown in the code editor for lines that
   --  contain code.

   Do_Color_Highlighting : constant String_Boolean := "Do_Color_Highlighting";
   --  Indicate whether the editor should provide color highlighting.

   Comments_Color : constant String_Color := "Comments_Color";
   --  Color used for comments.

   Strings_Color : constant String_Color := "Strings_Color";
   --  Color used for strings (brown).

   Keywords_Color : constant String_Color := "Keywords_Color";
   --  Color used for keywords (blue).

   Editor_Highlight_Current_Line : constant String_Boolean :=
     "Editor_Highlight_Current_Line";
   --  If True, the current line is displayed with a background color, in
   --  addition to the arrow that indicates the current line

   Editor_Highlight_Color : constant String_Color :=
     "Editor_Highlight_Color";
   --  The color to use to highlight the current line in the editor

   Tab_Size : constant String_Gint := "Tab_Size";
   --  Horizontal Tab size.
   --  Please note : the implemented tab behaviour is to jump at the next
   --  column with a number equal to a multiple of Tab_Size.

   Tooltips_In_Source : constant String_Tooltips_In_Source :=
     "Tooltips_In_Source";
   --  What kind of tooltips we want in the source window

   Should_Strip_CR : constant String_Boolean := "Strip_CR";
   --  If True, always strip CR characters when reading a file.

   ---------------------
   -- Assembly Window --
   ---------------------

   Asm_Highlight_Color : constant String_Color := "Asm_Highlight_Color";
   --  Color to use to highlight the assembly code for the current line
   --  (default is red).

   Assembly_Range_Size : constant String_String := "Assembly_Range_Size";
   --  Size of the range to display when initially displaying the
   --  assembly window.
   --  If this size is "0", then the whole function is displayed, but this
   --  can potentially take a very long time on slow machines or big
   --  functions.

   -----------------
   -- Data Window --
   -----------------

   Separate_Data : constant String_Boolean := "Separate_Data";
   --  Whether the Data window should be a separate window (False).

   Show_Stack : constant String_Boolean := "Show_Stack";
   --  Whether the Call Stack window should be visible (False);

   Xref_Color : constant String_Color := "Xref_Color";
   --  Color to use for the items that are clickable (blue).

   Title_Color : constant String_Color := "Title_Color";
   --  Color to use for the background of the title (grey).

   Change_Color : constant String_Color := "Change_Color";
   --  Color used to highlight fields that have changed since the last
   --  update (default is red).

   Selection_Color : constant String_Color := "Selection_Color";
   --  Color used to handle item selections.

   Thaw_Bg_Color : constant String_Color := "Thaw_Bg_Color";
   --  Color used for auto-refreshed items (white)

   Freeze_Bg_Color : constant String_Color := "Freeze_Bg_Color";
   --  Color used for frozen items (light grey)

   Look_3d : constant String_Boolean := "Look_3d";
   --  Should the items have a 3d look ?

   Title_Font : constant String_Font := "Title_Font";
   --  Font used for the name of the item.

   Title_Font_Size : constant String_Gint := "Title_Font_Size";
   --  Size of the font used for the name of the item.

   Value_Font : constant String_Font := "Value_Font";
   --  Font used to display the value of the item.

   Value_Font_Size : constant String_Gint := "Value_Font_Size";
   --  Size of the font used to display the value of the item.

   Command_Font : constant String_Font := "Command_Font";
   --  Font used to display the value for the commands
   --    graph print `...`  or graph display `...`

   Type_Font : constant String_Font := "Type_Font";
   --  Font used to display the type of the item.

   Type_Font_Size : constant String_Gint := "Type_Font_Size";
   --  Size of the font used to display the type of the item.

   Annotation_Font_Size : constant String_Gint := "Annotation_Font_Size";
   --  Size of the font used for annotation in the data canvas.

   Hide_Big_Items : constant String_Boolean := "Hide_Big_Items";
   --  If True, items higher than a given limit will start in a hidden
   --  state.

   Big_Item_Height : constant String_Gint := "Big_Item_Height";
   --  Items taller than this value will start hidden.

   Default_Detect_Aliases : constant String_Boolean :=
     "Default_Detect_Aliases";
   --  If True, do not create new items when a matching item is already
   --  present in the canvas.

   Display_Grid : constant String_Boolean := "Display_Grid";
   --  Whether the grid should be displayed in the canvas.

   Align_Items_On_Grid : constant String_Boolean := "Align_Items_On_Grid";
   --  Should items be aligned on the grid.

   --------------------
   -- Command Window --
   --------------------

   Debugger_Highlight_Color : constant String_Color :=
     "Debugger_Highlight_Color";
   --  Color used for highlighting in the debugger window (blue).

   Debugger_Font : constant String_Font := "Debugger_Font";
   --  Font used in the debugger text window.

   Debugger_Font_Size : constant String_Gint := "Debugger_Font_Size";
   --  Size of the font used in the debugger text window.

   -------------------
   -- Memory Window --
   -------------------

   Memory_View_Font : constant String_Font := "Memory_View_Font";
   --  Font use in the memory view window.

   Memory_View_Font_Size : constant String_Gint := "Memory_View_Font_Size";
   --  Size of the font used in the memory view window.

   Memory_View_Color : constant String_Color := "Memory_View_Color";
   --  Color used by default in the memory view window.

   Memory_Highlighted_Color : constant String_Color :=
     "Memory_Highlighted_Color";
   --  Color used for highlighted items in the memory view.

   Memory_Selected_Color : constant String_Color := "Memory_Selected_Color";
   --  Color used for selected items in the memory view.

   Memory_Modified_Color : constant String_Color := "Memory_Modified_Color";
   --  Color used for modified items in the memory view.

   -------------
   -- Helpers --
   -------------

   List_Processes : constant String_String := "List_Processes";
   --  Command to use to list processes running on the machine

   Default_External_Editor : constant String_String :=
     "Default_External_Editor";
   --  External editor to use.
   --  %f is replaced by the full path name for the file to edit.
   --  %l is the line number to show in the editor.
   --  This variable is superceded by the environment variable
   --  GVD_EDITOR if it exists.
   --  Try using "xterm -e /bin/vi %f +%l" if you prefer vi.

   Remote_Protocol : constant String_String := "Remote_Protocol";
   --  How to run a process on a remote machine ?

   Remote_Copy : constant String_String := "Remote_Copy";
   --  Program used to copy a file from a remote host.

   HTML_Browser : constant String_String := "HTML_Browser";
   --  Program used to browse HTML pages (e.g the GVD manual).

private
   pragma Inline (Get_Pref);
end GVD.Preferences;
