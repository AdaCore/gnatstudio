------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with GVD.Types;

package GVD.Preferences is

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class);
   --  Register all the preferences relative to GVD, and their default
   --  values. This doesn't override existing values of the preferences.

   package Debugger_Kind_Preferences is new
     Default_Preferences.Enums.Generics (GVD.Types.Debugger_Type);

   -----------------------
   -- List of constants --
   -----------------------
   --  See Register_Default_Preferences for a description of what these
   --  preferences should be used for.

   -- General --
   Break_On_Exception            : Boolean_Preference;
   Open_Main_Unit                : Boolean_Preference;
   Execution_Window              : Boolean_Preference;
   Preserve_State_On_Exit        : Boolean_Preference;
   Debugger_Kind                 : Debugger_Kind_Preferences.Preference;

   -- Source Window --
   Editor_Show_Line_With_Code    : Boolean_Preference;

   -- Assembly Window --
   Asm_Highlight_Color           : Color_Preference;
   Asm_Breakpoint_Color          : Color_Preference;
   Assembly_Range_Size           : Integer_Preference;

   -- Data Window --
   Xref_Color                    : Color_Preference;
   Change_Color                  : Color_Preference;
   Thaw_Bg_Color                 : Color_Preference;
   Freeze_Bg_Color               : Color_Preference;
   Title_Font                    : Font_Preference;
   Type_Font                     : Font_Preference;
   Hide_Big_Items                : Boolean_Preference;
   Big_Item_Height               : Integer_Preference;
   Default_Detect_Aliases        : Boolean_Preference;
   Max_Item_Width                : Integer_Preference;
   Max_Item_Height               : Integer_Preference;

   -- Command Window --
   Debugger_Highlight_Color      : Color_Preference;

   -- Memory Window --
   Memory_View_Color             : Color_Preference;
   Memory_Highlighted_Color      : Color_Preference;
   Memory_Selected_Color         : Color_Preference;

end GVD.Preferences;
