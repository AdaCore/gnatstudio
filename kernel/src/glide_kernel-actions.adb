-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

with Commands;                  use Commands;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Intl;                use Glide_Intl;
with Ada.Unchecked_Deallocation;

package body Glide_Kernel.Actions is

   use Actions_Htable.String_Hash_Table;

   ----------
   -- Free --
   ----------

   procedure Free (Action : in out Action_Record_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Action_Record, Action_Record_Access);
   begin
      --  Do not free Action.Command itself, since some menus might still
      --  be referring to it. It will be freed when the whole htable is
      --  reset

      Free (Action.Description);
      Unchecked_Free (Action);
   end Free;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Command     : access Commands.Interactive.Interactive_Command'Class;
      Description : String := "";
      Filter      : Action_Filter := null)
   is
      Old : constant Action_Record_Access := Lookup_Action (Kernel, Name);
      Overriden : Boolean := False;
   begin
      --  Initialize the kernel actions table.
      if Kernel.Actions = null then
         Kernel.Actions := new Actions_Htable_Record;
      end if;

      if Old /= null then
         Insert (Kernel,
                 -("Action """ & Name & """ is defined several times. Future"
                   & " references to this action will execute the last"
                   & " definition encountered"),
                 Mode => Error);
         Overriden := True;
      end if;

      Set (Actions_Htable_Access (Kernel.Actions).Table,
           Name,
           new Action_Record'
             (Commands.Interactive.Interactive_Command_Access (Command),
              Filter,
              new String'(Description),
              Modified  => False,
              Overriden => Overriden));
   end Register_Action;

   -----------
   -- Start --
   -----------

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Iterator
   is
      Iter : Action_Iterator;
   begin
      Get_First (Actions_Htable_Access (Kernel.Actions).Table, Iter.Iterator);
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Iterator) is
   begin
      Get_Next (Actions_Htable_Access (Kernel.Actions).Table, Iter.Iterator);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Iterator) return String is
   begin
      return Get_Key (Iter.Iterator);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Iterator) return Action_Record_Access is
   begin
      return Get_Element (Iter.Iterator);
   end Get;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Action_Record_Access is
   begin
      if Kernel.Actions = null then
         return null;
      else
         return Get (Actions_Htable_Access (Kernel.Actions).Table, Name);
      end if;
   end Lookup_Action;

   -----------
   -- Reset --
   -----------

   procedure Reset (X : access Actions_Htable_Record) is
      Iter   : Iterator;
      Action : Action_Record_Access;
   begin
      --  Free all the commands
      Get_First (X.Table, Iter);
      loop
         Action := Get_Element (Iter);
         exit when Action = null;

         Commands.Destroy (Command_Access (Action.Command));
         Get_Next (X.Table, Iter);
      end loop;

      Reset (X.Table);
   end Reset;

end Glide_Kernel.Actions;
