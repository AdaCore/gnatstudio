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

with Gdk.Event;   use Gdk.Event;

package body Commands.Interactive is

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Interactive_Command)
     return Command_Return_Type
   is
   begin
      return Execute (Interactive_Command_Access (Command), Event => null);
   end Execute;

   ------------------------------------
   -- Launch_Synchronous_Interactive --
   ------------------------------------

   procedure Launch_Synchronous_Interactive
     (Command : access Interactive_Command'Class;
      Event   : Gdk.Event.Gdk_Event;
      Wait    : Duration := 0.0)
   is
      function Execute_Command
        (Command : Command_Access) return Command_Return_Type;

      function Execute_Command
        (Command : Command_Access) return Command_Return_Type is
      begin
         return Execute
           (Interactive_Command_Access (Command), Event);
      end Execute_Command;

      procedure Internal is new Launch_Synchronous_Generic
        (Execute_Command);
   begin
      Internal (Command_Access (Command), Wait);
   end Launch_Synchronous_Interactive;

end Commands.Interactive;
