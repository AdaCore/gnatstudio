-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2003 - 2004                       --
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
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Commands.Interactive is

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Interactive_Command)
     return Command_Return_Type
   is
   begin
      return Execute (Interactive_Command_Access (Command), Null_Context);
   end Execute;

   ------------------------------------
   -- Launch_Synchronous_Interactive --
   ------------------------------------

   procedure Launch_Synchronous_Interactive
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context;
      Wait    : Duration := 0.0)
   is
      function Execute_Command
        (Command : Command_Access) return Command_Return_Type;

      function Execute_Command
        (Command : Command_Access) return Command_Return_Type is
      begin
         return Execute
           (Interactive_Command_Access (Command), Context);
      end Execute_Command;

      procedure Internal is new Launch_Synchronous_Generic
        (Execute_Command);
   begin
      Internal (Command_Access (Command), Wait);
   end Launch_Synchronous_Interactive;

   ------------------
   -- Create_Proxy --
   ------------------

   function Create_Proxy
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context) return Command_Access
   is
      C : Interactive_Command_Proxy_Access := new Interactive_Command_Proxy;
   begin
      C.Command := Interactive_Command_Access (Command);
      C.Context := Context;
      return Command_Access (C);
   end Create_Proxy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Interactive_Command_Proxy) return Command_Return_Type
   is
   begin
      if Command.Context.Dir = null then
         return Execute (Command.Command, Command.Context);
      else
         declare
            Old_Dir : constant Dir_Name_Str := Get_Current_Dir;
            Result  : Command_Return_Type;
         begin
            Change_Dir (Command.Context.Dir.all);
            Result := Execute (Command.Command, Command.Context);
            Change_Dir (Old_Dir);

            return Result;
         end;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Interactive_Command_Proxy) is
   begin
      Free (X.Context);
   end Free;

   procedure Free (X : in out Interactive_Command_Context) is
   begin
      Glide_Kernel.Unref (X.Context);

      if X.Args /= null then
         for J in X.Args'Range loop
            Free (X.Args (J));
         end loop;

         Free (X.Args);
      end if;

      Free (X.Dir);
   end Free;

end Commands.Interactive;
