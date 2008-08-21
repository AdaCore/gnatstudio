-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2008, AdaCore                 --
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
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Traces;                    use Traces;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;

package body GPS.Kernel.Actions is

   Me : constant Debug_Handle := Create ("Actions");

   use Actions_Htable.String_Hash_Table;

   type Menu_Command_Record is new Interactive_Command with record
      Kernel    : Kernel_Handle;
      Menu_Name : GNAT.Strings.String_Access;
   end record;
   type Menu_Command is access all Menu_Command_Record'Class;
   overriding function Execute
     (Command : access Menu_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc for interactive commands

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Menu_Command_Record;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
      Menu : constant Gtk_Menu_Item := Find_Menu_Item
        (Command.Kernel, Command.Menu_Name.all);
   begin
      if Menu /= null then
         Trace (Me, "Executing " & Command.Menu_Name.all);
         Activate (Menu);
         return Success;
      else
         Trace (Me, "Can't execute " & Command.Menu_Name.all);
         return Failure;
      end if;
   end Execute;

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
         if Old.Defined_In /= GNATCOLL.VFS.No_File then
            if Defined_In /= GNATCOLL.VFS.No_File then
               Insert
                 (Kernel,
                  '"' & Name & """: " & Overrides_Old
                  & Full_Name (Old.Defined_In).all
                  & Overridden_In & Full_Name (Defined_In).all
                  & Future,
                  Mode => Error);
            else
               Insert
                 (Kernel,
                  '"' & Name & """: " & Overrides_Old
                  & Full_Name (Old.Defined_In).all & Future,
                  Mode => Error);
            end if;
         else
            if Defined_In /= GNATCOLL.VFS.No_File then
               Insert
                 (Kernel,
                  '"' & Name & """: " & Overrides_Builtin
                  & (-" New definition in ")
                  & Full_Name (Defined_In).all & Future,
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

      Action := new Action_Record'
        (Commands.Interactive.Interactive_Command_Access (Command),
         Filter,
         new String'(Description),
         Modified   => False,
         Category   => Cat,
         Defined_In => Defined_In,
         Overriden  => Overriden);

      Set (Actions_Htable_Access (Kernel.Actions).Table,
           To_Lower (Name), Action);
      return Action;
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
      Name   : String) return Action_Record_Access
   is
      Action  : Action_Record_Access;
      Command : Menu_Command;
   begin
      if Kernel.Actions = null then
         return null;
      else
         Action := Get (Actions_Htable_Access (Kernel.Actions).Table, Name);

         if Action = null
           and then Name (Name'First) = '/'
         then
            Command := new Menu_Command_Record;
            Command.Kernel    := Kernel_Handle (Kernel);
            Command.Menu_Name := new String'(Name);

            Action := new Action_Record'
              (Command     => Interactive_Command_Access (Command),
               Filter      => null,
               Description => null,
               Category    => null,
               Defined_In  => GNATCOLL.VFS.No_File,
               Modified    => False,
               Overriden   => False);
            Set (Actions_Htable_Access (Kernel.Actions).Table, Name, Action);
         end if;

         return Action;
      end if;
   end Lookup_Action;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (X : access Actions_Htable_Record) is
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

end GPS.Kernel.Actions;
