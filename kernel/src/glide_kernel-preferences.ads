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

with Glib.Properties.Creation; use Glib.Properties.Creation;
with Default_Preferences;      use Default_Preferences;
with Gdk.Color;
with Pango.Font;
with GVD.Preferences;

package Glide_Kernel.Preferences is

   type GPS_Preferences_Record is new GVD.Preferences.GVD_Preferences_Manager
     with private;
   type GPS_Preferences is access GPS_Preferences_Record'Class;

   procedure Edit_Preferences (Kernel : access Kernel_Handle_Record'Class);
   --  Graphically edit the preferences

   procedure Register_Global_Preferences
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register all the preferences defined below. This must be caleld only
   --  after gtk+ has been initialized.
   --  Note that as much as possible, the preferences should be registered in
   --  the modules themselves.

   procedure Save_Preferences
     (Kernel : access Kernel_Handle_Record'Class; File_Name : String);
   --  See Default_Preferences.Save_Preferences.

   procedure Register_Property
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Param  : Glib.Param_Spec;
      Page   : String);
   pragma Inline (Register_Property);
   --  See Default_Preferences.Register_Property.

   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref    : Glib.Properties.Creation.Param_Spec_Int) return Glib.Gint;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_Boolean) return Boolean;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_String) return String;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Color) return Gdk.Color.Gdk_Color;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Font) return Pango.Font.Pango_Font_Description;
   function Get_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Glib.Properties.Creation.Param_Spec_Enum) return Glib.Gint;
   pragma Inline (Get_Pref);
   --  See Default_Preferences.Get_Pref

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.


   -- General --
   Default_Widget_Width  : Param_Spec_Int;
   Default_Widget_Height : Param_Spec_Int;
   Animated_Image        : Param_Spec_String;
   Splash_Screen         : Param_Spec_String;
   Tmp_Dir               : Param_Spec_String;

   -- Console --
   Highlight_File  : Param_Spec_Color;
   Highlight_Error : Param_Spec_Color;

   -- Diff_Utils --
   Diff_Cmd  : Param_Spec_String;
   Patch_Cmd : Param_Spec_String;

   -- Explorer --
   Normalized_Directories       : Param_Spec_Boolean;
   Show_Directories             : Param_Spec_Boolean;
   File_View_Shows_Only_Project : Param_Spec_Boolean;

   -- Source Editor --
   Default_Keyword_Color      : Param_Spec_Color;
   Default_Comment_Color      : Param_Spec_Color;
   Default_String_Color       : Param_Spec_Color;
   Default_Character_Color    : Param_Spec_Color;
   Default_HL_Line_Color      : Param_Spec_Color;
   Default_HL_Region_Color    : Param_Spec_Color;
   Automatic_Indentation      : Param_Spec_Boolean;
   Strip_Blanks               : Param_Spec_Boolean;
   Default_Source_Editor_Font : Param_Spec_Font;
   Display_Tooltip            : Param_Spec_Boolean;
   Periodic_Save              : Param_Spec_Int;
   Tab_Width                  : Param_Spec_Int;

   -- Project Editor --
   Default_Switches_Color          : Param_Spec_Color;
   Switches_Editor_Title_Font      : Param_Spec_Font;
   Variable_Ref_Background         : Param_Spec_Color;
   Invalid_Variable_Ref_Background : Param_Spec_Color;

   -- Wizards --
   Wizard_Toc_Highlight_Color : Param_Spec_Color;
   Wizard_Title_Font : Param_Spec_Font;

   -- Browsers --
   Browsers_Link_Font       : Param_Spec_Font;
   Browsers_Link_Color      : Param_Spec_Color;
   Selected_Link_Color      : Param_Spec_Color;
   Selected_Item_Color      : Param_Spec_Color;
   Parent_Linked_Item_Color : Param_Spec_Color;
   Child_Linked_Item_Color  : Param_Spec_Color;
   Browsers_Vertical_Layout : Param_Spec_Boolean;

   -- VCS --
   VCS_Commit_File_Check : Param_Spec_String;
   VCS_Commit_Log_Check : Param_Spec_String;
   CVS_Command : Param_Spec_String;

   --  Debugger preferences are registered in GVD.Preferences

private
   type GPS_Preferences_Record is new GVD.Preferences.GVD_Preferences_Manager
     with null record;
end Glide_Kernel.Preferences;
