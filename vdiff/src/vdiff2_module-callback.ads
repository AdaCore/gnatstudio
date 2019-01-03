------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Glib.Object;          use Glib.Object;
with Commands.Interactive; use Commands, Commands.Interactive;
with Default_Preferences;  use Default_Preferences;
with GNATCOLL.VFS;         use GNATCOLL.VFS;
with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;

package Vdiff2_Module.Callback is

   type On_Diff is new Diff_Hooks_Function with null record;
   overriding function Execute
     (Self   : On_Diff;
      Kernel : not null access Kernel_Handle_Record'Class;
      Vcs_File, Orig_File, New_File, Diff_File : Virtual_File;
      Title  : String)
      return Boolean;
   --  Process, if possible, the data sent by the kernel

   type Compare_Three_Files is new Interactive_Command with null record;
   overriding function Execute
     (Self  : access Compare_Three_Files;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the action to compare three files

   type Compare_Two_Files is new Interactive_Command with null record;
   overriding function Execute
     (Self  : access Compare_Two_Files;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Callback for the action to compare two files

   procedure On_Merge_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the action to merge three files

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for the action to merge two files

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self    : On_File_Closed;
      Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File);
   --  Callback for the "file_closed" signal

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type Change_Ref_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Change_Ref_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Change the Ref File for the current diff and reload hightlighting

   type Hide_Difference_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Hide_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Hide the highlighting and free the difference list

   type Remove_Difference_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Hide the highlighting and free the difference list

   type Recompute_Diff_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Recompute_Diff_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Hide difference ,recalculate the difference
   --  and show the new difference list

end Vdiff2_Module.Callback;
