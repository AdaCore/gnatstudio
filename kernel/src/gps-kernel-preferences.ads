------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Default_Preferences;      use Default_Preferences;
with Default_Preferences.Enums;
with Language;
with GPS_Preferences_Types;    use GPS_Preferences_Types;
with Gtk.Widget;
with Gtk.Menu;                 use Gtk.Menu;

package GPS.Kernel.Preferences is

   type GPS_Preferences_Manager_Record is new Preferences_Manager_Record
     with private;
   type GPS_Preferences_Manager is access
     all GPS_Preferences_Manager_Record'Class;

   overriding procedure Notify_Pref_Changed
     (Self : not null access GPS_Preferences_Manager_Record;
      Pref : not null access Preference_Record'Class);
   overriding procedure Thaw
     (Self : not null access GPS_Preferences_Manager_Record);

   procedure Set_Kernel
     (Self   : not null access GPS_Preferences_Manager_Record;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Set the kernel

   function Get_Kernel
     (Self   : not null access GPS_Preferences_Manager_Record)
      return access GPS.Kernel.Kernel_Handle_Record'Class;
   --  Get the kernel

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register all the preferences defined below.
   --  This must be calld only after Gtk+ has been initialized.
   --  Note that as much as possible, the preferences should be registered in
   --  the modules themselves.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the preference module

   procedure Save_Preferences (Kernel : access Kernel_Handle_Record'Class);
   --  See Default_Preferences.Save_Preferences

   procedure Set_Pref
     (Pref   : Boolean_Preference;
      Kernel : access Kernel_Handle_Record'Class;
      Value  : Boolean);
   procedure Set_Pref
     (Pref   : Integer_Preference;
      Kernel : access Kernel_Handle_Record'Class;
      Value  : Integer);
   procedure Set_Pref
     (Pref   : Preference;
      Kernel : access Kernel_Handle_Record'Class;
      Value  : String);
   --  See Default_Preferences.Set_Pref

   procedure Set_Font_And_Colors
     (Widget     : access Gtk.Widget.Gtk_Widget_Record'Class;
      Fixed_Font : Boolean;
      Pref       : Default_Preferences.Preference := null);
   --  Change the style of the widget based on the preferences.
   --  If Pref is specified, nothing will be done unless that
   --  preference has an impact on the fonts and colors. This is used to limit
   --  the amount of work done when the user changes preferences.

   ------------------
   -- Enumerations --
   ------------------

   type Line_Terminators is (Unchanged, Unix, Windows);
   package Line_Terminators_Prefs is new
     Default_Preferences.Enums.Generics (Line_Terminators);
   --  The list of supported line terminators

   type Line_Number_Policy is (Never, Some_Lines, All_Lines);
   package Line_Number_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Line_Number_Policy);
   --  The list of possible line number settings

   type Editor_Desktop_Policy is (Never, From_Project, Always);
   package Editor_Desktop_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Editor_Desktop_Policy);
   --  The list of possible behaviours when saving editors in the desktop

   package Multi_Language_Builder_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Multi_Language_Builder_Policy);
   --  The List of possible multi-language builders

   type Vdiff_Modes is (Unified, Side_By_Side);
   package Vdiff_Modes_Prefs is new
     Default_Preferences.Enums.Generics (Vdiff_Modes);

   type Strip_Trailing_Blanks_Policy is (Never, Autodetect, Always);
   package Strip_Trailing_Blanks_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Strip_Trailing_Blanks_Policy);
   --  The list of possible behaviours for stripping trailing blanks

   type Current_Line_Highlighting_Type is
     (Whole_Line, Underline, Gutter_Only);
   package Current_Line_Highlighting_Prefs is new
     Default_Preferences.Enums.Generics (Current_Line_Highlighting_Type);
   --  The list of possible choices regarding editors' current line
   --  highlighting.

   ------------------------------------------
   -- Associating preferences with widgets --
   ------------------------------------------
   --  There are lots of settings in GPS that need to be preserved across
   --  sessions. One way to do that is to use the API in histories.ads. A
   --  better way is to use the functions below, which connect a widget to a
   --  preference, and automatically keep the two of them in sync.
   --  This approach provides the flexibilty of connecting to the
   --  "preferences_changed" signal, so that even if another API is used to
   --  change the preference, the views are still refreshed.

   procedure Append_Menu
     (Menu    : not null access Gtk_Menu_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pref    : Boolean_Preference);
   --  Append a new entry in the menu showing the preference.
   --  This entry stays synchronizes with the preference at all time.

   procedure Append_Menu
     (Menu    : not null access Gtk_Menu_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Pref    : Color_Preference);
   --  Some as above but for color preference.

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   -- General --
   Gtk_Theme                : Theme_Preference;

   Default_Font             : Font_Preference;
   View_Fixed_Font          : Font_Preference;
   Small_Font               : Font_Preference;

   Command_Windows_Bg_Color : Color_Preference;

   Use_Native_Dialogs       : Boolean_Preference;
   Splash_Screen            : Boolean_Preference;
   Display_Welcome          : Boolean_Preference;
   Auto_Save                : Boolean_Preference;
   Save_Desktop_On_Exit     : Boolean_Preference;
   Desktop_Backup_Save      : Boolean_Preference;
   Save_Editor_Desktop      : Editor_Desktop_Policy_Prefs.Preference;
   Multi_Language_Builder   : Multi_Language_Builder_Policy_Prefs.Preference;
   Hyper_Mode               : Boolean_Preference;
   Tip_Of_The_Day           : Boolean_Preference;
   System_Menus             : Boolean_Preference;

   Show_Ellipsis            : Boolean_Preference;
   --  Whether to use ellipsis for long filenames

   -- Messages --
   Message_Highlight              : Color_Preference;
   Search_Src_Highlight           : Color_Preference;
   Annotation_Messages_Highlight  : Color_Preference;
   Info_Messages_Highlight        : Color_Preference;
   High_Messages_Highlight        : Color_Preference;
   Medium_Messages_Highlight      : Color_Preference;
   Low_Messages_Highlight         : Color_Preference;
   File_Pattern                   : String_Preference;
   File_Pattern_Index             : Integer_Preference;
   Line_Pattern_Index             : Integer_Preference;
   Column_Pattern_Index           : Integer_Preference;
   Secondary_File_Pattern         : String_Preference;
   Secondary_File_Pattern_Index   : Integer_Preference;
   Secondary_Line_Pattern_Index   : Integer_Preference;
   Secondary_Column_Pattern_Index : Integer_Preference;
   Alternate_Secondary_Pattern    : String_Preference;
   Alternate_Secondary_Line_Index : Integer_Preference;
   Message_Pattern_Index          : Integer_Preference;
   Style_Pattern_Index            : Integer_Preference;
   Warning_Pattern_Index          : Integer_Preference;
   Info_Pattern_Index             : Integer_Preference;
   Preserve_Messages              : Boolean_Preference;

   -- Diff_Utils --
   Diff_Mode           : Vdiff_Modes_Prefs.Preference;
   Diff_Cmd            : String_Preference;
   Patch_Cmd           : String_Preference;

   -- Source Editor --
   Default_Style             : Style_Preference;
   Keywords_Style            : Variant_Preference;
   Blocks_Style              : Variant_Preference;
   Types_Style               : Variant_Preference;
   Comments_Style            : Variant_Preference;
   Strings_Style             : Variant_Preference;
   Numbers_Style             : Variant_Preference;
   Code_Annotations_Style    : Variant_Preference;
   Annotated_Comments_Style  : Variant_Preference;
   Aspects_Style             : Variant_Preference;
   Aspects_Keywords_Style    : Variant_Preference;
   Aspects_Blocks_Style      : Variant_Preference;
   Aspects_Types_Style       : Variant_Preference;
   Aspects_Comments_Style    : Variant_Preference;
   Aspects_Strings_Style     : Variant_Preference;
   Aspects_Numbers_Style     : Variant_Preference;
   Hyper_Links_Style         : Variant_Preference;
   Bookmark_Color            : Color_Preference;

   Ephemeral_Highlighting_Simple : Variant_Preference;
   Ephemeral_Highlighting_Smart  : Variant_Preference;

   Block_Folding             : Boolean_Preference;
   Transient_Mark            : Boolean_Preference;
   Block_Highlighting        : Boolean_Preference;
   Automatic_Syntax_Check    : Boolean_Preference;
   Current_Line_Color        : Color_Preference;
   Current_Line_Highlighting : Current_Line_Highlighting_Prefs.Preference;
   Current_Block_Color       : Color_Preference;
   Alter_Bg_For_RO_Files     : Boolean_Preference;

   --  stripping blanks at end of line
   Strip_Blanks              : Strip_Trailing_Blanks_Policy_Prefs.Preference;
   --  stripping blank lines at end of file
   Strip_Lines               : Strip_Trailing_Blanks_Policy_Prefs.Preference;
   Line_Terminator           : Line_Terminators_Prefs.Preference;
   Display_Line_Numbers      : Line_Number_Policy_Prefs.Preference;
   Display_Subprogram_Names  : Boolean_Preference;
   Auto_Indent_On_Paste      : Boolean_Preference;
   Display_Tooltip           : Boolean_Preference;
   Highlight_Delimiters      : Boolean_Preference;
   Periodic_Save             : Integer_Preference;
   Highlight_Column          : Integer_Preference;
   Gutter_Right_Margin       : Integer_Preference;
   Use_ACL                   : Boolean_Preference;

   -- Debugger --
   Debugger_Current_Line_Color   : Color_Preference;
   Breakpoint_Color              : Color_Preference;
   Conditional_Breakpoint_Color  : Color_Preference;
   Disabled_Breakpoint_Color     : Color_Preference;

   -- External Commands --
   List_Processes            : String_Preference;
   Html_Browser              : String_Preference;
   Execute_Command           : String_Preference;
   Print_Command             : String_Preference;
   Max_Output_Length         : Integer_Preference;

   -- Project Editor --
   Default_Switches_Color          : Color_Preference;
   Switches_Editor_Title_Font      : Font_Preference;
   Variable_Ref_Background         : Color_Preference;
   Invalid_Variable_Ref_Background : Color_Preference;
   Generate_Relative_Paths         : Boolean_Preference;
   Trusted_Mode                    : Boolean_Preference;

   Automatic_Xrefs_Load            : Boolean_Preference;
   --  Only when using the old xref engine, null otherwise

   Hidden_Files_Pattern            : String_Preference;
   Show_Hidden_Files               : Boolean_Preference;
   --  Whether to hide some files in explorers and other contexts

   -- Refactoring --

   Add_Subprogram_Box     : Default_Preferences.Boolean_Preference;
   Add_In_Keyword         : Default_Preferences.Boolean_Preference;
   Create_Subprogram_Decl : Default_Preferences.Boolean_Preference;

   -- Wizards --
   Wizard_Title_Font : Font_Preference;

   -- Browsers --
   Browsers_Bg_Color         : Color_Preference;
   Browsers_Hyper_Link_Color : Color_Preference;
   Selected_Link_Color       : Color_Preference;
   Unselected_Link_Color     : Color_Preference;
   Parent_Linked_Item_Color  : Color_Preference;
   Child_Linked_Item_Color   : Color_Preference;
   Browsers_Vertical_Layout  : Boolean_Preference;
   Selected_Item_Color       : Color_Preference;
   Title_Color               : Color_Preference;

   Doc_Search_Before_First   : Boolean_Preference;
   --  Whether we should look for the documentation of entities first in the
   --  comments before (and fallback to the comments after), or the opposite

   --  Debugger preferences are registered in GVD.Preferences

   -- LibAdaLang --
   Use_LAL_In_Outline   : Boolean_Preference;
   Use_LAL_In_Shell     : Boolean_Preference;
   Use_LAL_In_Info      : Boolean_Preference;
   Use_LAL_In_GNATHUB   : Boolean_Preference;
   Use_LAL_In_COV       : Boolean_Preference;
   Use_LAL_In_Indent    : Boolean_Preference;
   Use_LAL_In_Highlight : Boolean_Preference;

   package Indentation_Kind_Preferences is new
     Default_Preferences.Enums.Generics (Language.Indentation_Kind);

private

   type GPS_Preferences_Manager_Record is new Preferences_Manager_Record
     with record
      Kernel : GPS.Kernel.Kernel_Handle;

      Nested_Pref_Changed : Natural := 0;
      --  Monitor the nested calls to On_Pref_Changed to avoid saving the
      --  preferences file too often.
   end record;

end GPS.Kernel.Preferences;
