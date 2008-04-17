-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2007, AdaCore              --
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

with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package body Commands.Socket is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item    : out Socket_Command_Access;
      Kernel  : Kernel_Handle;
      Command : String;
      Shell   : String := GPS.Kernel.Scripts.GPS_Shell_Name;
      Stream  : Stream_Access) is
   begin
      Item         := new Socket_Command;
      Item.Kernel  := Kernel;
      Item.Command := new String'(Command);
      Item.Shell   := new String'(Shell);
      Item.Stream  := Stream;
   end Create;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Socket_Command) is
   begin
      Free (X.Command);
      Free (X.Shell);
   end Free;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Socket_Command) return Command_Return_Type
   is
      Errors : aliased Boolean;
   begin
      if Command.Command /= null then
         String'Write
           (Command.Stream,
            Execute_Command
              (Lookup_Scripting_Language
                 (Get_Scripts (Command.Kernel), Command.Shell.all),
               Command.Command.all, null, True, True,
               Errors'Unchecked_Access) &
            ASCII.LF & "GPS>> ");
      end if;

      return Success;
   end Execute;

end Commands.Socket;
