-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Glide_Kernel.Modules; use Glide_Kernel.Modules;

package body Commands.Socket is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Socket_Command_Access;
      Kernel  : Kernel_Handle;
      Command : String;
      Stream  : Stream_Access) is
   begin
      Item := new Socket_Command;
      Item.Kernel := Kernel;
      Item.Command := new String'(Command);
      Item.Stream := Stream;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Socket_Command) is
   begin
      Free (X.Command);
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Socket_Command) return Boolean is
   begin
      if Command.Command /= null then
         String'Write
           (Command.Stream,
            Interpret_Command (Command.Kernel, Command.Command.all) &
              ASCII.LF);
      end if;

      return True;
   end Execute;

end Commands.Socket;
