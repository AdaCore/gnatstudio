-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2011, AdaCore                 --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Commands;                  use Commands;
with GNAT.Strings;              use GNAT.Strings;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Intl;                  use GPS.Intl;
with Ada.Unchecked_Deallocation;
with Commands.Interactive;      use Commands.Interactive;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body GPS.Kernel.Actions is

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
      --  reset (since it was registered through Register_Perma_Command)

      --  Do not free the Action.Filter, which will be taken care of when the
      --  kernel itself is destroyed. This means that filters always have a
      --  lifespan equal to that of GPS

      Free (Action.Category);
      Free (Action.Description);
      Free (Action.Name);
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
      Filter      : Action_Filter := null;
      Category    : String := "General";
      Defined_In  : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
   is
      Action : Action_Record_Access;
      pragma Unreferenced (Action);
   begin
      Action := Register_Action
        (Kernel, Name, Command, Description, Filter, Category, Defined_In);
   end Register_Action;

   ---------------------
   -- Register_Action --
   ---------------------

   function Register_Action
     (Kernel      : access Kernel_Handle_Record'Class;
      Name        : String;
      Command     : access Commands.Interactive.Interactive_Command'Class;
      Description : String := "";
      Filter      : Action_Filter := null;
      Category    : String := "General";
      Defined_In  : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
      return Action_Record_Access
   is
      Old : constant Action_Record_Access := Lookup_Action (Kernel, Name);
      Overriden : Boolean := False;
      Cat : String_Access;
      Future : constant String := ASCII.LF &
        (-("Future references to this action will execute the last"
         & " definition encountered"));
      Overrides_Builtin : constant String :=
        -"Action overrides a builtin action" & ASCII.LF;
      Overrides_Old : constant String :=
        -"Action already defined in ";
      Overridden_In : constant String :=
        -" and overriden in ";
      Action : Action_Record_Access;
   begin
      --  Initialize the kernel actions table.
      if Kernel.Actions = null then
         Kernel.Actions := new Actions_Htable_Record;
      end if;

      if Old /= null then
         if Name (Name'First) = '/' then
            --  This is a menu: do not display a message about the overriding,
            --  since it is legitimate to want to recreate or redefine a menu,
            --  for instance in the Build module.

            null;

         elsif Old.Defined_In /= GNATCOLL.VFS.No_File then
            if Defined_In /= GNATCOLL.VFS.No_File then
               Insert
                 (Kernel,
                  '"' & Name & """: " & Overrides_Old
                  & Display_Full_Name (Old.Defined_In)
                  & Overridden_In & Display_Full_Name (Defined_In)
                  & Future,
                  Mode => Error);
            else
               Insert
                 (Kernel,
                  '"' & Name & """: " & Overrides_Old
                  & Display_Full_Name (Old.Defined_In) & Future,
                  Mode => Error);
            end if;
         else
            if Defined_In /= GNATCOLL.VFS.No_File then
               Insert
                 (Kernel,
                  '"' & Name & """: " & Overrides_Builtin
                  & (-" New definition in ")
                  & Display_Full_Name (Defined_In) & Future,
                  Mode => Error);
            else
               Insert (Kernel, '"' & Name & """: " & Overrides_Builtin
                       & Future, Mode => Error);
            end if;
         end if;

         Overriden := True;
      end if;

      if Category /= "" then
         Cat := new String'(Category);
      end if;

      --  Handle memory management for the filter

      if Filter /= null then
         Register_Filter (Kernel, Filter, Name => "");
      end if;

      --  Create the action

      Action := new Action_Record'
        (Commands.Interactive.Interactive_Command_Access (Command),
         Filter,
         new String'(Description),
         Name       => new String'(Name),
         Modified   => False,
         Category   => Cat,
         Defined_In => Defined_In,
         Overriden  => Overriden);

      Register_Perma_Command (Kernel, Command);

      Set (Actions_Htable_Access (Kernel.Actions).Table,
           To_Lower (Name), Action);
      return Action;
   end Register_Action;

   -----------------------
   -- Unregister_Action --
   -----------------------

   procedure Unregister_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) is
   begin
      loop
         exit when Get
           (Actions_Htable_Access (Kernel.Actions).Table, To_Lower (Name))
           = null;

         Remove (Actions_Htable_Access (Kernel.Actions).Table,
                 To_Lower (Name));
      end loop;
   end Unregister_Action;

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
      Name   : String) return Action_Record_Access
   is
      Action  : Action_Record_Access;
   begin
      if Kernel.Actions = null then
         return null;
      else
         Action := Get (Actions_Htable_Access (Kernel.Actions).Table, Name);

         return Action;
      end if;
   end Lookup_Action;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (X : access Actions_Htable_Record) is
   begin
      --  Free the actions and their filters
      Reset (X.Table);
   end Reset;

end GPS.Kernel.Actions;
