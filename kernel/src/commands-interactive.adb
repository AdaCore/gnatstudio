------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Gdk.Event;    use Gdk.Event;
with GPS.Intl;     use GPS.Intl;

with XML_Utils;    use XML_Utils;

package body Commands.Interactive is

   type Internal_Component_Iterator is new Component_Iterator_Record with
      record
         At_End : Boolean := False;
      end record;

   overriding function Get
     (Iter : access Internal_Component_Iterator) return Command_Component;
   overriding procedure Next (Iter : access Internal_Component_Iterator);
   --  See docs from inherited subprograms

   type Internal_Component_Record is new Command_Component_Record
      with null record;

   overriding function Get_Name
     (Component : access Internal_Component_Record) return String;
   --  See docs for inherited subprograms

   Internal_Component : aliased Internal_Component_Record;
   --  Used for all internal components, ie that can't be edited graphically

   ----------------
   -- On_Failure --
   ----------------

   function On_Failure
     (Iter : access Component_Iterator_Record) return Component_Iterator
   is
      pragma Unreferenced (Iter);
   begin
      return null;
   end On_Failure;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Component_Iterator_Record) is
      pragma Unreferenced (Iter);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Component_Iterator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Component_Iterator_Record'Class, Component_Iterator);
   begin
      Free (Iter.all);
      Unchecked_Free (Iter);
   end Free;

   -----------
   -- Start --
   -----------

   function Start
     (Command : access Interactive_Command) return Component_Iterator
   is
      pragma Unreferenced (Command);
   begin
      return new Internal_Component_Iterator'
        (Component_Iterator_Record with At_End => False);
   end Start;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : access Internal_Component_Iterator) return Command_Component is
   begin
      if Iter.At_End then
         return null;
      else
         return Internal_Component'Access;
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : access Internal_Component_Iterator) is
   begin
      Iter.At_End := True;
   end Next;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Interactive_Command) return Command_Return_Type is
   begin
      return Execute (Interactive_Command_Access (Command), Null_Context);
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

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Interactive_Command_Proxy) is
   begin
      Free (X.Context);
   end Free;

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

   ---------------------------
   -- Create_Command_Editor --
   ---------------------------

   function Create_Command_Editor
     (Command : access Interactive_Command;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Command_Editor
   is
      pragma Unreferenced (Command, Kernel);
   begin
      return null;
   end Create_Command_Editor;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Component : access Internal_Component_Record) return String
   is
      pragma Unreferenced (Component);
   begin
      return -"Built-in command";
   end Get_Name;

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
