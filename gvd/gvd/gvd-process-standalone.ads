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

--  This package provides extensions to GVD.Process when GVD is used
--  stand alone.

with Glib;
with Gdk.Input;
with Gdk.Types;
with GVD.Main_Window;
with GVD.Types;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package GVD.Process.Standalone is

   package Standard_Input_Package is new Gdk.Input.Input_Add
     (GVD.Main_Window.GVD_Main_Window_Record'Class);
   --  This package is needed to handle the tty mode.

   procedure Input_Available
     (Debugger  : Standard_Input_Package.Data_Access;
      Source    : Glib.Gint;
      Condition : Gdk.Types.Gdk_Input_Condition);
   --  This procedure should be used in conjunction with Standard_Input above.
   --  This is the callback input function that will retrieve the current
   --  page in the process notebook contained by Debugger and send the
   --  command (line read from Source) using Process_User_Command.
   --  Note that this handler currently assumes that Source is the standard
   --  input file descriptor.

   function Create_Debugger
     (Window          : access GVD.Main_Window.GVD_Main_Window_Record'Class;
      Kind            : GVD.Types.Debugger_Type;
      Executable      : String;
      Debugger_Args   : Argument_List;
      Executable_Args : String;
      Remote_Host     : String := "";
      Remote_Target   : String := "";
      Remote_Protocol : String := "";
      Debugger_Name   : String := "") return Visual_Debugger;
   --  Create a debugger with a given list of arguments.
   --  A new page is added to the window's main notebook.
   --  See GVD.Process.Configure for details on the arguments.

end GVD.Process.Standalone;
