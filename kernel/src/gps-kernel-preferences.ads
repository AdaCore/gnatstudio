------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
with Entities.Queries;
with Language;
with GPS_Preferences_Types; use GPS_Preferences_Types;

package GPS.Kernel.Preferences is

   type GPS_Preferences_Record is new Preferences_Manager_Record
     with private;
   type GPS_Preferences is access GPS_Preferences_Record'Class;

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class);
   --  Graphically edit the preferences

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register all the preferences defined below.
   --  This must be calld only after Gtk+ has been initialized.
   --  Note that as much as possible, the preferences should be registered in
   --  the modules themselves.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the preference module

   procedure Save_Preferences
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : Virtual_File);
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

   --------------------------------
   -- Specific preferences pages --
   --------------------------------

   procedure Register_Page
     (Kernel : access Kernel_Handle_Record'Class;
      Page   : access Default_Preferences.Preferences_Page_Record'Class);
   --  Register a new pasge to display in the preferences dialog.
   --  This page will be put first on the list of preference pages in the
   --  dialog.

   --------------------------
   -- Saving and Restoring --
   --------------------------

   procedure Save_Preferences
     (Kernel : access Kernel_Handle_Record'Class;
      Saved  : out Default_Preferences.Saved_Prefs_Data);
   --  Save the current value of the preferences.
   --  Saved must be freed by the user

   ------------------
   -- Enumerations --
   ------------------

   type Line_Terminators is (Unchanged, Unix, Windows);
   package Line_Terminators_Prefs is new
     Default_Preferences.Enums.Generics (Line_Terminators);
   --  The list of supported line terminators

   type Speed_Column_Policies is (Never, Automatic, Always);
   package Speed_Column_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Speed_Column_Policies);
   --  The list of possible behaviours for the speed column

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

   package Dispatching_Menu_Policy_Prefs is new
     Default_Preferences.Enums.Generics
       (Entities.Queries.Dispatching_Menu_Policy);

   type Strip_Trailing_Blanks_Policy is (Never, Autodetect, Always);
   package Strip_Trailing_Blanks_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Strip_Trailing_Blanks_Policy);
   --  The list of possible behaviours for stripping trailing blanks

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   -- General --
   Gtk_Theme              : Theme_Preference;
   Default_Font           : Style_Preference;
   View_Fixed_Font        : Font_Preference;
   Use_Native_Dialogs     : Boolean_Preference;
   Default_Widget_Width   : Integer_Preference;
   Default_Widget_Height  : Integer_Preference;
   Splash_Screen          : Boolean_Preference;
   Display_Welcome        : Boolean_Preference;
   Toolbar_Show_Text      : Boolean_Preference;
   Auto_Save              : Boolean_Preference;
   Save_Desktop_On_Exit   : Boolean_Preference;
   Save_Editor_Desktop    : Editor_Desktop_Policy_Prefs.Preference;
   Multi_Language_Builder : Multi_Language_Builder_Policy_Prefs.Preference;
   Auto_Jump_To_First     : Boolean_Preference;
   Hyper_Mode             : Boolean_Preference;
   Tooltip_Color          : Color_Preference;
   Tip_Of_The_Day         : Boolean_Preference;

   SQLITE_Db      : Boolean_Preference;
   --  Internal preference: whether to use the GNATCOLL.Xref database.
   --  This preference follows the trace "SQLITE"

   -- Messages --
   Message_Highlight              : Color_Preference;
   Error_Src_Highlight            : Color_Preference;
   Warning_Src_Highlight          : Color_Preference;
   Style_Src_Highlight            : Color_Preference;
   Info_Src_Highlight             : Color_Preference;
   Search_Src_Highlight           : Color_Preference;
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

   -- Diff_Utils --
   Diff_Mode           : Vdiff_Modes_Prefs.Preference;
   Diff_Cmd            : String_Preference;
   Patch_Cmd           : String_Preference;
   Old_Vdiff           : Boolean_Preference;

   -- Source Editor --
   Default_Style             : Style_Preference;
   Keywords_Style            : Variant_Preference;
   Block_Style               : Variant_Preference;
   Type_Style                : Variant_Preference;
   Comments_Style            : Variant_Preference;
   Annotated_Comments_Style  : Variant_Preference;
   Strings_Style             : Variant_Preference;
   Hyper_Links_Style         : Variant_Preference;

   Delimiter_Color           : Color_Preference;
   Block_Folding             : Boolean_Preference;
   Block_Highlighting        : Boolean_Preference;
   Automatic_Syntax_Check    : Boolean_Preference;
   Current_Line_Color        : Color_Preference;
   Current_Line_Thin         : Boolean_Preference;
   Current_Block_Color       : Color_Preference;
   Search_Results_Color      : Color_Preference;

   --  stripping blanks at end of line
   Strip_Blanks              : Strip_Trailing_Blanks_Policy_Prefs.Preference;
   --  stripping blank lines at end of file
   Strip_Lines               : Strip_Trailing_Blanks_Policy_Prefs.Preference;
   Line_Terminator           : Line_Terminators_Prefs.Preference;
   Display_Line_Numbers      : Boolean_Preference;
   Display_Subprogram_Names  : Boolean_Preference;
   Display_Tooltip           : Boolean_Preference;
   Tooltip_Timeout           : Integer_Preference;
   Highlight_Delimiters      : Boolean_Preference;
   Periodic_Save             : Integer_Preference;
   Highlight_Column          : Integer_Preference;
   Speed_Column_Policy       : Speed_Column_Policy_Prefs.Preference;
   Use_ACL                   : Boolean_Preference;

   Submenu_For_Dispatching_Calls : Dispatching_Menu_Policy_Prefs.Preference;
   --  Of type Dispatching_Menu_Policy

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
   Hidden_Directories_Pattern      : String_Preference;

   -- Refactoring --

   Add_Subprogram_Box     : Default_Preferences.Boolean_Preference;
   Add_In_Keyword         : Default_Preferences.Boolean_Preference;
   Create_Subprogram_Decl : Default_Preferences.Boolean_Preference;

   -- Wizards --
   Wizard_Title_Font : Font_Preference;

   -- Browsers --
   Browsers_Bg_Color         : Color_Preference;
   Browsers_Bg_Image         : String_Preference;
   Browsers_Draw_Grid        : Boolean_Preference;
   Browsers_Hyper_Link_Color : Color_Preference;
   Selected_Link_Color       : Color_Preference;
   Unselected_Link_Color     : Color_Preference;
   Parent_Linked_Item_Color  : Color_Preference;
   Child_Linked_Item_Color   : Color_Preference;
   Browsers_Vertical_Layout  : Boolean_Preference;
   Selected_Item_Color       : Color_Preference;
   Title_Color               : Color_Preference;

   -- VCS --
   Implicit_Status           : Boolean_Preference;
   Hide_Up_To_Date           : Boolean_Preference;
   Hide_Not_Registered       : Boolean_Preference;
   CVS_Command               : String_Preference;
   ClearCase_Command         : String_Preference;
   Default_VCS               : String_Preference;

   -- Location View --
   Locations_Wrap            : Boolean_Preference;
   Locations_Auto_Close      : Boolean_Preference;

   --  Debugger preferences are registered in GVD.Preferences

   package Indentation_Kind_Preferences is new
     Default_Preferences.Enums.Generics (Language.Indentation_Kind);

private
   type GPS_Preferences_Record is new Preferences_Manager_Record
     with null record;
end GPS.Kernel.Preferences;
