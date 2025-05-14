------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2023, AdaCore                     --
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

with DAP.Types;                 use DAP.Types;
with Default_Preferences;       use Default_Preferences;
with Default_Preferences.Enums; use Default_Preferences.Enums;

package DAP.Modules.Preferences is

   package Debuggee_Start_Preferences is new
     Default_Preferences.Enums.Generics (DAP.Types.Debuggee_Start_Type);

   procedure Register_Default_Preferences
     (Prefs : access Preferences_Manager_Record'Class);
   --  Register all the preferences relative to GVD, and their default
   --  values. This doesn't override existing values of the preferences.

   DAP_Adapter                   : String_Preference;

   -- General --
   Break_On_Exception            : Boolean_Preference;
   Preserve_State_On_Exit        : Boolean_Preference;
   Execution_Window              : Boolean_Preference;
   Auto_Start_Debuggee           : Debuggee_Start_Preferences.Preference;

   -- Source Window --
   Continue_To_Line_Buttons      : Boolean_Preference;

   -- Breakpoints --
   Breakpoints_For_All_Debuggers : Boolean_Preference;
   Pending_Breakpoints           : Boolean_Preference;

   -- Console --
   Debugger_Console_Console      : Boolean_Preference;
   Debugger_Console_In_Out       : Boolean_Preference;

   --  Call stack
   Frames_Limit                  : Integer_Preference;
   --  How many frames will be fetched at one time

   -- Assembly Window --
   Assembly_Range_Size           : Integer_Preference;
   Asm_Show_Addresses            : Boolean_Preference;
   Asm_Show_Offset               : Boolean_Preference;
   Asm_Show_Opcodes              : Boolean_Preference;

   -- Memory Window --
   Memory_View_Color             : Color_Preference;
   Memory_Highlighted_Color      : Color_Preference;
   Memory_Selected_Color         : Color_Preference;
   Memory_Auto_Refresh           : Boolean_Preference;

   -- Registers view --
   Registers_Type                : Boolean_Preference;

end DAP.Modules.Preferences;
