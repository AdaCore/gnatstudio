-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Glide_Kernel;
with GNAT.OS_Lib;
with Glib.Object;
with String_List_Utils;

package Shell is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Module_Command_Function is access function
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  A function called when an interactive command is issued.
   --  Command is the command that was issued with parameters Args.
   --  This function must NOT modify or free the contents of Args.

   procedure Register_Command
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command      : String;
      Usage        : String;
      Description  : String;
      Minimum_Args : Natural := 0;
      Maximum_Args : Natural := 0;
      Handler      : Module_Command_Function);
   --  Register Command, with Help as its help/usage text, and Handler
   --  as the default command handler.
   --  Usage should only describe the command name and its arguments. Optional
   --  arguments should be reported between [].
   --  Description should describe the effects of the command. The description
   --  should end with a '.'.
   --  The number of parameters the command expects should be between
   --  Minimum_Args and Maximum_Args.

   function Interpret_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String) return String;
   --  Command is a string followed by a list of arguments, for example
   --     "edit gps.adb"
   --  Commands are registered using Register_Command, see above.
   --  The following commands are recognized by the kernel:
   --    "help"   : brings up the list of currently recognized commands.
   --    "help X" : brings up help concerning command X.

   function Interpret_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String;
   --  Same as above, the arguments not included in Command.

   procedure Interpret_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String);
   --  Same as above, but do not return any result, and display the output
   --  on the console.

   procedure Interpret_Command
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List);
   --  Same as above, the arguments not included in Command.

   function Commands_As_List
     (Prefix : String;
      Kernel : access Glib.Object.GObject_Record'Class)
      return String_List_Utils.String_List.List;
   --  Return the list of commands. The list must be freed by the caller.

   -----------------------------------
   -- Specialized shell interaction --
   -----------------------------------
   --  ??? Would be better in the src_editor module

   function Create_Mark
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : String;
      Line     : Natural := 1;
      Column   : Natural := 1;
      Length   : Natural := 0) return String;
   --  Create a mark for Filename, at position given by Line, Column, with
   --  length Length.
   --  Return the identifier corresponding to the mark that has been created.

   procedure Highlight_Line
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename           : String;
      Line               : Natural := 1;
      Highlight_Category : String;
      Highlight          : Boolean := True);
   --  Highlight the line with the corresponding category.
   --  If Highlight is set to False, remove the highlighting.
   --  If Line = 0, highlight / unhighlight all lines in file.

end Shell;
