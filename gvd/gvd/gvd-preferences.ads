-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2000-2007                       --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib; use Glib;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Default_Preferences;      use Default_Preferences;

package GVD.Preferences is

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class);
   --  Register all the preferences relative to GVD, and their default
   --  values. This doesn't override existing values of the preferences.

   type Debugger_Windows_Policy is
     (Close_Windows, Hide_Windows, Keep_Windows);
   for Debugger_Windows_Policy'Size use Glib.Gint'Size;
   pragma Convention (C, Debugger_Windows_Policy);
   --  What should happen to debugger-related windows when a debugger session
   --  is terminated.

   -----------------------
   -- List of constants --
   -----------------------
   --  See Register_Default_Preferences for a description of what these
   --  preferences should be used for.

   -- General --
   Break_On_Exception            : Param_Spec_Boolean;
   Open_Main_Unit                : Param_Spec_Boolean;
   Execution_Window              : Param_Spec_Boolean;
   Preserve_State_On_Exit        : Param_Spec_Boolean;
   Debugger_Windows              : Param_Spec_Enum;

   -- Source Window --
   Editor_Show_Line_With_Code    : Param_Spec_Boolean;

   -- Assembly Window --
   Asm_Highlight_Color           : Param_Spec_Color;
   Asm_Breakpoint_Color          : Param_Spec_Color;
   Assembly_Range_Size           : Param_Spec_Int;

   -- Data Window --
   Xref_Color                    : Param_Spec_Color;
   Change_Color                  : Param_Spec_Color;
   Thaw_Bg_Color                 : Param_Spec_Color;
   Freeze_Bg_Color               : Param_Spec_Color;
   Title_Font                    : Param_Spec_Font;
   Type_Font                     : Param_Spec_Font;
   Hide_Big_Items                : Param_Spec_Boolean;
   Big_Item_Height               : Param_Spec_Int;
   Default_Detect_Aliases        : Param_Spec_Boolean;
   Max_Item_Width                : Param_Spec_Int;
   Max_Item_Height               : Param_Spec_Int;

   -- Command Window --
   Debugger_Highlight_Color      : Param_Spec_Color;

   -- Memory Window --
   Memory_View_Color             : Param_Spec_Color;
   Memory_Highlighted_Color      : Param_Spec_Color;
   Memory_Selected_Color         : Param_Spec_Color;

private
   type GVD_Preferences_Manager is new Preferences_Manager_Record with
   null record;
end GVD.Preferences;
