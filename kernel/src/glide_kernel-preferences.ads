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
with Gdk.Types;
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
   --  Register all the preferences defined below.
   --  This must be calld only after Gtk+ has been initialized.
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
   procedure Get_Pref
     (Kernel   : access Kernel_Handle_Record'Class;
      Pref     : Param_Spec_Key;
      Modifier : out Gdk.Types.Gdk_Modifier_Type;
      Key      : out Gdk.Types.Gdk_Key_Type);
   pragma Inline (Get_Pref);
   --  See Default_Preferences.Get_Pref

   procedure Set_Pref
     (Kernel : access Kernel_Handle_Record'Class;
      Pref   : Param_Spec_Boolean;
      Value  : Boolean);
   --  See Default_Preferences.Set_Pref

   ------------------
   -- Enumerations --
   ------------------

   type Line_Terminators is (Unchanged, Unix, Windows);
   for Line_Terminators'Size use Glib.Gint'Size;
   pragma Convention (C, Line_Terminators);
   --  The list of supported line terminators.

   type Key_Themes is (Default, Emacs);
   for Key_Themes'Size use Glib.Gint'Size;
   pragma Convention (C, Key_Themes);
   --  The list of supported key themes.

   -----------------------
   -- List of constants --
   -----------------------
   --  Below is the list of all the preference settings that can be set.
   --  The type of the constant gives the type of the value associated with the
   --  preference.

   -- General --
   Default_Font          : Param_Spec_Font;
   Key_Theme_Name        : Param_Spec_Enum;
   Use_Native_Dialogs    : Param_Spec_Boolean;
   Can_Change_Accels     : Param_Spec_Boolean;
   Default_Charset       : Param_Spec_String;
   Default_Widget_Width  : Param_Spec_Int;
   Default_Widget_Height : Param_Spec_Int;
   Animated_Image        : Param_Spec_String;
   Splash_Screen         : Param_Spec_Boolean;
   Toolbar_Show_Text     : Param_Spec_Boolean;
   Save_Desktop_On_Exit  : Param_Spec_Boolean;
   Display_Welcome       : Param_Spec_Boolean;

   -- Messages --
   Message_Highlight    : Param_Spec_Color;
   Show_Build_Progress  : Param_Spec_Boolean;
   File_Pattern         : Param_Spec_String;
   File_Pattern_Index   : Param_Spec_Int;
   Line_Pattern_Index   : Param_Spec_Int;
   Column_Pattern_Index : Param_Spec_Int;

   -- Diff_Utils --
   Diff_Context_Length : Param_Spec_Int;
   Diff_Cmd            : Param_Spec_String;
   Patch_Cmd           : Param_Spec_String;

   -- Explorer --
   Show_Directories             : Param_Spec_Boolean;
   File_View_Shows_Only_Project : Param_Spec_Boolean;

   -- Source Editor --
   Default_Keyword_Color     : Param_Spec_Color;
   Default_Comment_Color     : Param_Spec_Color;
   Default_String_Color      : Param_Spec_Color;
   Default_HL_Line_Color     : Param_Spec_Color;
   Default_HL_Region_Color   : Param_Spec_Color;
   Current_Line_Color        : Param_Spec_Color;

   Strip_Blanks              : Param_Spec_Boolean;
   Line_Terminator           : Param_Spec_Enum;
   Display_Line_Numbers      : Param_Spec_Boolean;
   Source_Editor_Font        : Param_Spec_Font;
   Keyword_Font              : Param_Spec_Font;
   Comment_Font              : Param_Spec_Font;
   String_Font               : Param_Spec_Font;
   Display_Tooltip           : Param_Spec_Boolean;
   Highlight_Delimiters      : Param_Spec_Boolean;
   Periodic_Save             : Param_Spec_Int;
   Tab_Width                 : Param_Spec_Int;

   Indentation_Key           : Param_Spec_Key;
   Completion_Key            : Param_Spec_Key;
   Delimiters_Jump_Key       : Param_Spec_Key;

   -- MDI --
   MDI_Opaque            : Param_Spec_Boolean;
   MDI_Destroy_Floats    : Param_Spec_Boolean;
   MDI_Background_Color  : Param_Spec_Color;
   MDI_Title_Bar_Color   : Param_Spec_Color;
   MDI_Focus_Title_Color : Param_Spec_Color;
   MDI_Switch_Child      : Param_Spec_Key;
   MDI_All_Floating      : Param_Spec_Boolean;

   -- Languages --
   Ada_Automatic_Indentation : Param_Spec_Boolean;
   Ada_Use_Tabs              : Param_Spec_Boolean;
   Ada_Indentation_Level     : Param_Spec_Int;
   Ada_Continuation_Level    : Param_Spec_Int;
   Ada_Declaration_Level     : Param_Spec_Int;
   Ada_Indent_Case_Extra     : Param_Spec_Boolean;
   C_Automatic_Indentation   : Param_Spec_Boolean;
   C_Use_Tabs                : Param_Spec_Boolean;
   C_Indentation_Level       : Param_Spec_Int;

   -- Project Editor --
   Default_Switches_Color          : Param_Spec_Color;
   Switches_Editor_Title_Font      : Param_Spec_Font;
   Variable_Ref_Background         : Param_Spec_Color;
   Invalid_Variable_Ref_Background : Param_Spec_Color;
   Generate_Relative_Paths         : Param_Spec_Boolean;
   Selector_Show_Project_Hierarchy : Param_Spec_Boolean;

   -- Wizards --
   Wizard_Toc_Highlight_Color : Param_Spec_Color;
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
   Selected_Item_Color       : Param_Spec_Color
     renames GVD.Preferences.Selected_Item_Color;

   Dep_Browser_Show_System_Files : Param_Spec_Boolean;
   Dep_Browser_Show_Implicit_Dep : Param_Spec_Boolean;

   -- VCS --
   Hide_Up_To_Date     : Param_Spec_Boolean;
   Hide_Not_Registered : Param_Spec_Boolean;
   CVS_Command         : Param_Spec_String;

   --  Debugger preferences are registered in GVD.Preferences

private
   type GPS_Preferences_Record is new GVD.Preferences.GVD_Preferences_Manager
     with null record;
end Glide_Kernel.Preferences;
