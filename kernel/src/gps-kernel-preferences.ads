-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2001-2008, AdaCore                  --
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

with Default_Preferences;      use Default_Preferences;
with Default_Preferences.Enums;
with Entities.Queries;
with Language;

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
     (Kernel : access Kernel_Handle_Record'Class; File_Name : String);
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

   type Multi_Language_Builder_Policy is (Gprbuild, Gprmake);
   package Multi_Language_Builder_Policy_Prefs is new
     Default_Preferences.Enums.Generics (Multi_Language_Builder_Policy);
   --  The List of possible multi-language builders

   package Dispatching_Menu_Policy_Prefs is new
     Default_Preferences.Enums.Generics
       (Entities.Queries.Dispatching_Menu_Policy);

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   -- General --
   Default_Font           : Font_Preference;
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
   Tooltip_Color          : Color_Preference;

   -- Messages --
   Message_Highlight              : Color_Preference;
   Error_Src_Highlight            : Color_Preference;
   Warning_Src_Highlight          : Color_Preference;
   Style_Src_Highlight            : Color_Preference;
   Search_Src_Highlight           : Color_Preference;
   File_Pattern                   : String_Preference;
   File_Pattern_Index             : Integer_Preference;
   Line_Pattern_Index             : Integer_Preference;
   Column_Pattern_Index           : Integer_Preference;
   Secondary_File_Pattern         : String_Preference;
   Secondary_File_Pattern_Index   : Integer_Preference;
   Secondary_Line_Pattern_Index   : Integer_Preference;
   Secondary_Column_Pattern_Index : Integer_Preference;
   Message_Pattern_Index          : Integer_Preference;
   Style_Pattern_Index            : Integer_Preference;
   Warning_Pattern_Index          : Integer_Preference;

   -- Diff_Utils --
   Diff_Cmd            : String_Preference;
   Patch_Cmd           : String_Preference;
   Old_Vdiff           : Boolean_Preference;

   -- Source Editor --
   Default_Style             : Style_Preference;
   Keywords_Style            : Style_Preference;
   Comments_Style            : Style_Preference;
   Annotated_Comments_Style  : Style_Preference;
   Strings_Style             : Style_Preference;

   Delimiter_Color           : Color_Preference;
   Block_Folding             : Boolean_Preference;
   Block_Highlighting        : Boolean_Preference;
   Automatic_Syntax_Check    : Boolean_Preference;
   Current_Line_Color        : Color_Preference;
   Current_Block_Color       : Color_Preference;
   Search_Results_Color      : Color_Preference;

   Strip_Blanks              : Boolean_Preference;
   Line_Terminator           : Line_Terminators_Prefs.Preference;
   Display_Line_Numbers      : Boolean_Preference;
   Display_Subprogram_Names  : Boolean_Preference;
   Display_Tooltip           : Boolean_Preference;
   Tooltip_Timeout           : Integer_Preference;
   Highlight_Delimiters      : Boolean_Preference;
   Periodic_Save             : Integer_Preference;
   Tab_Width                 : Integer_Preference;
   Highlight_Column          : Integer_Preference;
   Speed_Column_Policy       : Speed_Column_Policy_Prefs.Preference;

   Submenu_For_Dispatching_Calls : Dispatching_Menu_Policy_Prefs.Preference;
   --  Of type Dispatching_Menu_Policy

   -- External Commands --
   List_Processes            : String_Preference;
   Html_Browser              : String_Preference;
   Execute_Command           : String_Preference;
   Print_Command             : String_Preference;
   Max_Output_Length         : Integer_Preference;

   -- MDI --
   MDI_Opaque            : Boolean_Preference;
   MDI_Destroy_Floats    : Boolean_Preference;
   MDI_Background_Color  : Color_Preference;
   MDI_Title_Bar_Color   : Color_Preference;
   MDI_Focus_Title_Color : Color_Preference;
   MDI_All_Floating      : Boolean_Preference;
   MDI_Float_Short_Title : Boolean_Preference;
   Pref_Draw_Title_Bars  : Boolean_Preference;

   -- Project Editor --
   Default_Switches_Color          : Color_Preference;
   Switches_Editor_Title_Font      : Font_Preference;
   Variable_Ref_Background         : Color_Preference;
   Invalid_Variable_Ref_Background : Color_Preference;
   Generate_Relative_Paths         : Boolean_Preference;
   Trusted_Mode                    : Boolean_Preference;
   Automatic_Xrefs_Load            : Boolean_Preference;
   Hidden_Directories_Pattern      : String_Preference;

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

   --  Debugger preferences are registered in GVD.Preferences

   package Indentation_Kind_Preferences is new
     Default_Preferences.Enums.Generics (Language.Indentation_Kind);

private
   type GPS_Preferences_Record is new Preferences_Manager_Record
     with null record;
end GPS.Kernel.Preferences;
