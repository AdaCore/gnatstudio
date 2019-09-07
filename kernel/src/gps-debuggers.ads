------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

--  An abstract definition of what graphical interfaces to debuggers are,
--  and what they can do.

with Glib.Object;      use Glib.Object;

package GPS.Debuggers is
   type Base_Visual_Debugger is abstract new Glib.Object.GObject_Record
      with null record;
   type Base_Visual_Debugger_Access is access all Base_Visual_Debugger'Class;

   function Get_Num
     (Self : not null access Base_Visual_Debugger) return Glib.Gint
      is abstract;
   --  Return a unique number identifying the debugger associated with Self.

   function Command_In_Process
     (Self : not null access Base_Visual_Debugger) return Boolean
      is abstract;
   --  Whether a command is currently being processed by the debugger.

   type Debugger_State is (Debug_None, Debug_Busy, Debug_Available);
   --  Possible states of a debugger:
   --  - Debug_None: debugger is not running
   --  - Debug_Busy: debugger is busy processing a command
   --  - Debug_Available: debugger is available

   function To_String (State : Debugger_State) return String
      is (if State = Debug_None then "none"
          elsif State = Debug_Busy then "busy"
          else "idle");
   function From_String (State : String) return Debugger_State
      is (if State = "none" then Debug_None
          elsif State = "busy" then Debug_Busy
          else Debug_Available);
   --  Use by the hooks to pass the information python.

end GPS.Debuggers;
