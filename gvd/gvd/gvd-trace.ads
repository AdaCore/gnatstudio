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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Expect;     use GNAT.Expect;
with GVD.Process;     use GVD.Process;
with GVD.Types;       use GVD.Types;
with GVD.Main_Window; use GVD.Main_Window;
with System;

package GVD.Trace is

   type IO_Kind is (Input_Kind, Output_Kind);
   --  Kind of IO in the log files.
   --  Input_Kind means input strings sent to the debugger.
   --  Output_Kind means output strings received from the debugger.

   procedure Output_Error (Window : GVD_Main_Window; Str : String);
   --  Output an error line on the statusbar associated with Window, and
   --  on its log file.

   procedure Output_Info (Window : GVD_Main_Window; Str : String);
   --  Output an information line on the statusbar associated with Window, and
   --  on its log file.

   procedure Output_Line (Window : GVD_Main_Window; Str : String);
   --  Write on the log file associated with Window.

   procedure Output_Message
     (Process : Visual_Debugger;
      Str     : String;
      Mode    : Command_Type;
      Kind    : IO_Kind := Input_Kind);
   --  Write on the log file associated with Process.
   --  Replace ASCII.LF by "\n" and ASCII.HT by "\t", and put lines in quotes.

   procedure Input_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Filter that should be called when GVD receives inputs.

   procedure Output_Filter
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Filter that should be called when GVD receives output from the
   --  underlying debugger.

end GVD.Trace;
