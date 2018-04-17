------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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
   Debugger_Kind                 : Debugger_Kind_Preferences.Preference;

   Break_On_Exception            : Boolean_Preference;
   Open_Main_Unit                : Boolean_Preference;
   Execution_Window              : Boolean_Preference;
   Preserve_State_On_Exit        : Boolean_Preference;

   -- Source Window --
   Editor_Current_Line_Color     : Color_Preference;

   -- Assembly Window --
   Assembly_Range_Size           : Integer_Preference;
   Asm_Show_Addresses            : Boolean_Preference;
   Asm_Show_Offset               : Boolean_Preference;
   Asm_Show_Opcodes              : Boolean_Preference;
   Asm_Highlight_Instructions    : Boolean_Preference;

   -- Registers view --
   Registers_Hexadecimal         : Boolean_Preference;
   Registers_Octal               : Boolean_Preference;
   Registers_Binary              : Boolean_Preference;
   Registers_Decimal             : Boolean_Preference;
   Registers_Raw                 : Boolean_Preference;
   Registers_Natural             : Boolean_Preference;

   -- Data Window --
   Change_Color                  : Color_Preference;
   Thaw_Bg_Color                 : Color_Preference;
   Freeze_Bg_Color               : Color_Preference;
   Title_Font                    : Font_Preference;
   Type_Font                     : Font_Preference;
   Max_Item_Width                : Integer_Preference;
   Max_Item_Height               : Integer_Preference;

   -- Memory Window --
   Memory_View_Color             : Color_Preference;
   Memory_Highlighted_Color      : Color_Preference;
   Memory_Selected_Color         : Color_Preference;
   Memory_Auto_Refresh           : Boolean_Preference;

   --  Remote Debugging
   Load_Executable_On_Init       : Boolean_Preference;
   Connection_Timeout            : Integer_Preference;

   -- Console --
   Debugger_Console_All_Interactions : Boolean_Preference;
   --  Whether display all interactions in the debugger console

   --  Call stack
   Frames_Limit                  : Integer_Preference;
   --  How many frames will be fetched at one time

end GVD.Preferences;
