-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
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
with Default_Preferences;      use Default_Preferences;
with Glib.Properties.Creation; use Glib.Properties.Creation;

package GVD.Preferences is

   type GVD_Preferences_Manager is new Preferences_Manager_Record with private;
   type GVD_Preferences is access all GVD_Preferences_Manager'Class;

   GVD_Prefs : GVD_Preferences;

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class;
      Page_Prefix : String := "";
      XML_Prefix  : String := "");
   --  Register all the preferences known in GVD, and their default
   --  values. This doesn't override existing values of the preferences.
   --  Page_Prefix is added to all the page descriptions

   type Tooltips_In_Source_Type is (None, Simple, Full);
   pragma Convention (C, Tooltips_In_Source_Type);
   for Tooltips_In_Source_Type'Size use Gint'Size;
   --  The types of tooltips that can be displayed in the source window:
   --    None: no tooltips will be displayed.
   --    Simple: the output of the debugger is displayed, no post-processing
   --    Full: the variable is parsed and the tooltip will contain the
   --     equivalent of the canvas'items.

   function Get_Tab_Size (Pref : access GVD_Preferences_Manager) return Gint;
   --  Special function since Tab_Size is used very often and we need fast
   --  access to it.

   procedure Set_Pref
     (Manager : access GVD_Preferences_Manager;
      Name    : String;
      Value   : Glib.Gint);
   --  See Default_Preferences.
   --  This subprogram is overriden, so that a cached value of Tab_Size can be
   --  maintained.

   -----------------------
   -- List of constants --
   -----------------------
   --  See Register_Default_Preferences for a description of what these
   --  preferences should be used for.

   -- General Preferences --
   Fixed_Font                    : Param_Spec_Font;
   Break_On_Exception            : Param_Spec_Boolean;
   Hide_Delay                    : Param_Spec_Int;
   Ada_Extensions                : Param_Spec_String;
   C_Extensions                  : Param_Spec_String;
   Cpp_Extensions                : Param_Spec_String;
   Execution_Window              : Param_Spec_Boolean;

   -- Explorer Window --
   Display_Explorer              : Param_Spec_Boolean;
   File_Name_Bg_Color            : Param_Spec_Color;

   -- Source Window --
   Editor_Show_Line_Nums         : Param_Spec_Boolean;
   Editor_Show_Line_With_Code    : Param_Spec_Boolean;
   Do_Color_Highlighting         : Param_Spec_Boolean;
   Comments_Color                : Param_Spec_Color;
   Strings_Color                 : Param_Spec_Color;
   Keywords_Color                : Param_Spec_Color;
   Editor_Highlight_Current_Line : Param_Spec_Boolean;
   Editor_Highlight_Color        : Param_Spec_Color;
   Tab_Size                      : Param_Spec_Int;
   Tooltips_In_Source            : Param_Spec_Enum;
   Should_Strip_CR               : Param_Spec_Boolean;

   -- Assembly Window --
   Asm_Highlight_Color           : Param_Spec_Color;
   Assembly_Range_Size           : Param_Spec_Int;

   -- Data Window --
   Xref_Color                    : Param_Spec_Color;
   Title_Color                   : Param_Spec_Color;
   Change_Color                  : Param_Spec_Color;
   Thaw_Bg_Color                 : Param_Spec_Color;
   Freeze_Bg_Color               : Param_Spec_Color;
   Title_Font                    : Param_Spec_Font;
   Value_Font                    : Param_Spec_Font;
   Command_Font                  : Param_Spec_Font;
   Type_Font                     : Param_Spec_Font;
   Annotation_Font               : Param_Spec_Font;
   Hide_Big_Items                : Param_Spec_Boolean;
   Big_Item_Height               : Param_Spec_Int;
   Default_Detect_Aliases        : Param_Spec_Boolean;
   Selected_Item_Color           : Param_Spec_Color;
   Show_Call_Stack               : Param_Spec_Boolean;

   -- Command Window --
   Debugger_Highlight_Color      : Param_Spec_Color;

   -- Memory Window --
   Memory_View_Color             : Param_Spec_Color;
   Memory_Highlighted_Color      : Param_Spec_Color;
   Memory_Selected_Color         : Param_Spec_Color;
   Memory_Modified_Color         : Param_Spec_Color;

   -- Helpers --
   List_Processes                : Param_Spec_String;
   Default_External_Editor       : Param_Spec_String;
   Remote_Protocol               : Param_Spec_String;
   Remote_Copy                   : Param_Spec_String;
   Html_Browser                  : Param_Spec_String;
   Execute_Command               : Param_Spec_String;
   Print_Command                 : Param_Spec_String;

private
   Default_Tab_Size : constant Gint := 8;

   type GVD_Preferences_Manager is new Preferences_Manager_Record with record
      Tab_Size : Gint := Default_Tab_Size;
      --  Cached value of the tab_size preference
   end record;
end GVD.Preferences;
