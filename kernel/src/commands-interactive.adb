------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Gdk.Event;    use Gdk.Event;
with GPS.Intl;     use GPS.Intl;
with GPS.Kernel;   use GPS.Kernel;

package body Commands.Interactive is

   -------------------------
   -- Create_Null_Context --
   -------------------------

   function Create_Null_Context
     (From : GPS.Kernel.Selection_Context)
      return Interactive_Command_Context
   is
   begin
      return
        (Event            => null,
         Context          => From,
         Synchronous      => False,
         Dir              => No_File,
         Args             => null,
         Label            => null,
         Via_Menu         => False,
         Repeat_Count     => 1,
         Remaining_Repeat => 0);
   end Create_Null_Context;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Interactive_Command) return Command_Return_Type is
   begin
      return Execute
        (Interactive_Command_Access (Command),
         Create_Null_Context (No_Context));
   end Execute;

   ------------------
   -- Create_Proxy --
   ------------------

   function Create_Proxy
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context) return Command_Access
   is
      C : constant Interactive_Command_Proxy_Access :=
            new Interactive_Command_Proxy;
   begin
      C.Command := Interactive_Command_Access (Command);
      C.Context := Context;

      if Context.Event /= null then
         C.Context.Event := Copy (Context.Event);
      end if;

      return Command_Access (C);
   end Create_Proxy;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Interactive_Command_Proxy) return Command_Return_Type
   is
   begin
      return Execute (Command.Command, Command.Context);
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Interactive_Command_Proxy) return String is
   begin
      if Command.Context.Label = null then
         --  ??? Could ask the name for the proxied command, but in most
         --  cases it will be "generic command", so that's not very useful.
         return -"Interactive command";
      else
         return Command.Context.Label.all;
      end if;
   end Name;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
     (X : in out Interactive_Command_Proxy) is
   begin
      Free (X.Context);
   end Primitive_Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Interactive_Command_Context) is
   begin
      if X.Args /= null then
         for J in X.Args'Range loop
            Free (X.Args (J));
         end loop;

         Free (X.Args);
      end if;

      if X.Event /= null then
         Free (X.Event);
      end if;

      Free (X.Label);
   end Free;

   --------------
   -- Progress --
   --------------

   overriding function Progress
     (Command : access Interactive_Command_Proxy) return Progress_Record is
   begin
      return Progress (Command.Command);
   end Progress;

   ------------------
   -- Set_Progress --
   ------------------

   overriding procedure Set_Progress
     (Command  : access Interactive_Command_Proxy;
      Progress : Progress_Record) is
   begin
      Set_Progress (Command.Command, Progress);
   end Set_Progress;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Interactive_Command_Proxy) return Boolean is
   begin
      return Undo (Command.Command);
   end Undo;

   ---------------
   -- Interrupt --
   ---------------

   overriding procedure Interrupt
     (Command : in out Interactive_Command_Proxy) is
   begin
      Interrupt (Command.Command.all);
   end Interrupt;

end Commands.Interactive;
