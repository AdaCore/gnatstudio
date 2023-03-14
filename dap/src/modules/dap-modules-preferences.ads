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

with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Default_Preferences;      use Default_Preferences;

package DAP.Modules.Preferences is

   procedure Register_Default_Preferences
     (Prefs    : access Preferences_Manager_Record'Class;
      Base_Dir : Virtual_File);
   --  Register all the preferences relative to GVD, and their default
   --  values. This doesn't override existing values of the preferences.

   DAP_Adapter                   : String_Preference;

   -- General --
   Open_Main_Unit                : Boolean_Preference;
   Preserve_State_On_Exit        : Boolean_Preference;

   -- Source Window --
   Continue_To_Line_Buttons      : Boolean_Preference;

   -- Breakpoints --
   Breakpoints_For_All_Debuggers : Boolean_Preference;
   Pending_Breakpoints           : Boolean_Preference;

   -- Console --
   Debugger_Console_Console : Boolean_Preference;
   Debugger_Console_Stdout  : Boolean_Preference;
   --  Whether display layers in the debugger console

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

end DAP.Modules.Preferences;
