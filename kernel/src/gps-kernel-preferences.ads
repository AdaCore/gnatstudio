-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2008, AdaCore             --
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

with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glib.Generic_Properties;
with Gdk.Color;
with Gdk.Types;
with Pango.Font;

with Default_Preferences;      use Default_Preferences;
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
   --  See Default_Preferences.Save_Preferences.

   procedure Register_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Param  : Glib.Param_Spec;
      Page   : String);
   pragma Inline (Register_Property);
   --  See Default_Preferences.Register_Property.

   function Get_Pref
     (Pref    : Glib.Properties.Creation.Param_Spec_Int) return Glib.Gint
     renames Default_Preferences.Get_Pref;
   function Get_Pref
     (Pref   : Glib.Properties.Creation.Param_Spec_Boolean) return Boolean
     renames Default_Preferences.Get_Pref;
   function Get_Pref
     (Pref   : Glib.Properties.Creation.Param_Spec_String) return String
     renames Default_Preferences.Get_Pref;
   function Get_Pref
     (Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color
     renames Default_Preferences.Get_Pref;
   function Get_Pref
     (Pref    : Param_Spec_Color) return String
     renames Default_Preferences.Get_Pref;
   function Get_Pref
     (Pref   : Param_Spec_Font) return Pango.Font.Pango_Font_Description
     renames Default_Preferences.Get_Pref;
   function Get_Pref
     (Pref   : Glib.Properties.Creation.Param_Spec_Enum) return Glib.Gint
     renames Default_Preferences.Get_Pref;
   procedure Get_Pref
     (Pref     : Param_Spec_Key;
      Modifier : out Gdk.Types.Gdk_Modifier_Type;
      Key      : out Gdk.Types.Gdk_Key_Type)
     renames Default_Preferences.Get_Pref;
   function Get_Pref_Font
     (Pref     : Param_Spec_Style) return Pango.Font.Pango_Font_Description
     renames Default_Preferences.Get_Pref_Font;
   function Get_Pref_Fg
     (Pref     : Param_Spec_Style) return Gdk.Color.Gdk_Color
     renames Default_Preferences.Get_Pref_Fg;
   function Get_Pref_Bg
     (Pref     : Param_Spec_Style) return Gdk.Color.Gdk_Color
     renames Default_Preferences.Get_Pref_Bg;
   --  See Default_Preferences.Get_Pref

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Boolean;
      Value  : Boolean);
   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Int;
      Value  : Glib.Gint);
   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_String;
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
   for Line_Terminators'Size use Glib.Gint'Size;
   pragma Convention (C, Line_Terminators);
   --  The list of supported line terminators

   type Speed_Column_Policies is (Never, Automatic, Always);
   for Speed_Column_Policies'Size use Glib.Gint'Size;
   pragma Convention (C, Speed_Column_Policies);
   --  The list of possible behaviours for the speed column

   type Editor_Desktop_Policy is (Never, From_Project, Always);
   for Editor_Desktop_Policy'Size use Glib.Gint'Size;
   pragma Convention (C, Editor_Desktop_Policy);
   --  The list of possible behaviours when saving editors in the desktop

   type Multi_Language_Builder_Policy is (Gprbuild, Gprmake);
   for Multi_Language_Builder_Policy'Size use Glib.Gint'Size;
   pragma Convention (C, Multi_Language_Builder_Policy);
   --  The List of possible multi-language builders

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   -- General --
   Default_Font           : Param_Spec_Font;
   View_Fixed_Font        : Param_Spec_Font;
   Use_Native_Dialogs     : Param_Spec_Boolean;
   Default_Widget_Width   : Param_Spec_Int;
   Default_Widget_Height  : Param_Spec_Int;
   Splash_Screen          : Param_Spec_Boolean;
   Display_Welcome        : Param_Spec_Boolean;
   Toolbar_Show_Text      : Param_Spec_Boolean;
   Auto_Save              : Param_Spec_Boolean;
   Save_Desktop_On_Exit   : Param_Spec_Boolean;
   Save_Editor_Desktop    : Param_Spec_Enum;
   Multi_Language_Build   : Param_Spec_Boolean;
   Multi_Language_Builder : Param_Spec_Enum;
   Auto_Jump_To_First     : Param_Spec_Boolean;
   Tooltip_Color          : Param_Spec_Color;

   -- Messages --
   Message_Highlight      : Param_Spec_Color;
   Error_Src_Highlight    : Param_Spec_Color;
   Warning_Src_Highlight  : Param_Spec_Color;
   Style_Src_Highlight    : Param_Spec_Color;
   Search_Src_Highlight   : Param_Spec_Color;
   File_Pattern           : Param_Spec_String;
   File_Pattern_Index     : Param_Spec_Int;
   Line_Pattern_Index     : Param_Spec_Int;
   Column_Pattern_Index   : Param_Spec_Int;
   Secondary_File_Pattern           : Param_Spec_String;
   Secondary_File_Pattern_Index     : Param_Spec_Int;
   Secondary_Line_Pattern_Index     : Param_Spec_Int;
   Secondary_Column_Pattern_Index   : Param_Spec_Int;
   Message_Pattern_Index  : Param_Spec_Int;
   Style_Pattern_Index    : Param_Spec_Int;
   Warning_Pattern_Index  : Param_Spec_Int;

   -- Diff_Utils --
   Diff_Cmd            : Param_Spec_String;
   Patch_Cmd           : Param_Spec_String;
   Old_Vdiff           : Param_Spec_Boolean;

   -- Source Editor --
   Default_Style             : Param_Spec_Style;
   Keywords_Style            : Param_Spec_Style;
   Comments_Style            : Param_Spec_Style;
   Annotated_Comments_Style  : Param_Spec_Style;
   Strings_Style             : Param_Spec_Style;

   Delimiter_Color           : Param_Spec_Color;
   Block_Folding             : Param_Spec_Boolean;
   Block_Highlighting        : Param_Spec_Boolean;
   Automatic_Syntax_Check    : Param_Spec_Boolean;
   Current_Line_Color        : Param_Spec_Color;
   Current_Block_Color       : Param_Spec_Color;
   Search_Results_Color      : Param_Spec_Color;

   Strip_Blanks              : Param_Spec_Boolean;
   Line_Terminator           : Param_Spec_Enum;
   Display_Line_Numbers      : Param_Spec_Boolean;
   Display_Subprogram_Names  : Param_Spec_Boolean;
   Display_Tooltip           : Param_Spec_Boolean;
   Tooltip_Timeout           : Param_Spec_Int;
   Highlight_Delimiters      : Param_Spec_Boolean;
   Periodic_Save             : Param_Spec_Int;
   Tab_Width                 : Param_Spec_Int;
   Highlight_Column          : Param_Spec_Int;
   Speed_Column_Policy       : Param_Spec_Enum;

   Submenu_For_Dispatching_Calls : Param_Spec_Enum;
   --  Of type Dispatching_Menu_Policy

   -- External Commands --
   List_Processes                : Param_Spec_String;
   Html_Browser                  : Param_Spec_String;
   Execute_Command               : Param_Spec_String;
   Print_Command                 : Param_Spec_String;
   Max_Output_Length             : Param_Spec_Int;

   -- MDI --
   MDI_Opaque            : Param_Spec_Boolean;
   MDI_Destroy_Floats    : Param_Spec_Boolean;
   MDI_Background_Color  : Param_Spec_Color;
   MDI_Title_Bar_Color   : Param_Spec_Color;
   MDI_Focus_Title_Color : Param_Spec_Color;
   MDI_All_Floating      : Param_Spec_Boolean;
   MDI_Float_Short_Title : Param_Spec_Boolean;
   Pref_Draw_Title_Bars  : Param_Spec_Boolean;

   -- Project Editor --
   Default_Switches_Color          : Param_Spec_Color;
   Switches_Editor_Title_Font      : Param_Spec_Font;
   Variable_Ref_Background         : Param_Spec_Color;
   Invalid_Variable_Ref_Background : Param_Spec_Color;
   Generate_Relative_Paths         : Param_Spec_Boolean;
   Trusted_Mode                    : Param_Spec_Boolean;
   Automatic_Xrefs_Load            : Param_Spec_Boolean;
   Hidden_Directories_Pattern      : Param_Spec_String;

   -- Wizards --
   Wizard_Title_Font : Param_Spec_Font;

   -- Browsers --
   Browsers_Bg_Color         : Param_Spec_Color;
   Browsers_Bg_Image         : Param_Spec_String;
   Browsers_Draw_Grid        : Param_Spec_Boolean;
   Browsers_Hyper_Link_Color : Param_Spec_Color;
   Selected_Link_Color       : Param_Spec_Color;
   Unselected_Link_Color     : Param_Spec_Color;
   Parent_Linked_Item_Color  : Param_Spec_Color;
   Child_Linked_Item_Color   : Param_Spec_Color;
   Browsers_Vertical_Layout  : Param_Spec_Boolean;
   Selected_Item_Color       : Param_Spec_Color;
   Title_Color               : Param_Spec_Color;

   -- VCS --
   Hide_Up_To_Date     : Param_Spec_Boolean;
   Hide_Not_Registered : Param_Spec_Boolean;
   CVS_Command         : Param_Spec_String;
   ClearCase_Command   : Param_Spec_String;

   --  Debugger preferences are registered in GVD.Preferences

   package Indentation_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
     ("Indentation_Kind", Language.Indentation_Kind);

private
   type GPS_Preferences_Record is new Preferences_Manager_Record
     with null record;
end GPS.Kernel.Preferences;
