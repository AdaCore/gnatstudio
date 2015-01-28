------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Commands;                  use Commands;
with Gdk.Event;                 use Gdk.Event;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with Gtkada.MDI;                use Gtkada.MDI;

package body GPS.Kernel.Actions is
   Me : constant Trace_Handle := Create ("ACTIONS");

   use Actions_Htable.String_Hash_Table;

   Future            : constant String := ASCII.LF &
     (-("Future references to this action will execute the last"
        & " definition encountered"));
   Overrides_Builtin : constant String :=
     -"Action overrides a builtin action" & ASCII.LF;

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
      Free (Action.Menus);
      Free (Action.Stock_Id);
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
      Stock_Id    : String := "")
   is
      Old : constant Action_Record_Access := Lookup_Action (Kernel, Name);
      Overriden : Boolean := False;
      Cat : String_Access;
      Action : Action_Record_Access;
      Stock  : String_Access;
      Cmd    : access Interactive_Command'Class;
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
         else
            Insert (Kernel, '"' & Name & """: " & Overrides_Builtin
                    & Future, Mode => Error);
         end if;

         Overriden := True;
      end if;

      if Category /= "" then
         Cat := new String'(Category);
      end if;

      if Stock_Id /= "" then
         Stock := new String'(Stock_Id);
      end if;

      --  Handle memory management for the filter

      if Filter /= null then
         Register_Filter (Kernel, Filter, Name => "");
      end if;

      if Command /= null then
         --  ??? The use of Unrestricted_Access is ugly, but it allows nicer
         --  user code :
         --   * users can extend the Interactive_Command type in package bodies
         --   * and still call Register_Action ( new My_Command);
         --     without using a temporary variable to store the allocated
         --     command.
         Cmd := Command.all'Unrestricted_Access;
      end if;

      --  Create the action

      Action := new Action_Record'
        (Cmd,
         Filter,
         new String'(Description),
         Name       => new String'(Name),
         Modified   => False,
         Category   => Cat,
         Overriden  => Overriden,
         Menus      => null,
         Stock_Id   => Stock);

      Register_Perma_Command (Kernel, Cmd);

      Set (Actions_Htable_Access (Kernel.Actions).Table,
           To_Lower (Name), Action);
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

   ---------------------------
   -- Execute_In_Background --
   ---------------------------

   function Execute_In_Background
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Action  : String;
      Context : Selection_Context := No_Context;
      Event   : Gdk.Event.Gdk_Event := null;
      Repeat  : Positive := 1) return Boolean
   is
      Child : GPS_MDI_Child;
      --  The child that currently has the focus

      procedure Undo_Group (Start : Boolean);
      --  Start or end an undo group

      ----------------
      -- Undo_Group --
      ----------------

      procedure Undo_Group (Start : Boolean) is
         C : MDI_Child;
      begin
         if Start then
            if Repeat >= 2 then
               C := Get_Focus_Child (Get_MDI (Kernel));
               if C /= null
                  and then C.all in GPS_MDI_Child_Record'Class
               then
                  Child := GPS_MDI_Child (C);
               end if;

               if Child /= null then
                  Start_Group (Get_Command_Queue (Child));
               end if;
            end if;

         elsif Child /= null then
            End_Group (Get_Command_Queue (Child));
         end if;
      end Undo_Group;

      Act : constant Action_Record_Access := Lookup_Action (Kernel, Action);
      C : Selection_Context := Context;
   begin
      if Act = null then
         Insert (Kernel, -"Action not defined : " & Action);
         return False;
      end if;

      if Context = No_Context then
         C := Get_Current_Context (Kernel);  --  no need to free
      end if;

      if Filter_Matches (Act.Filter, C) then
         if Active (Me) then
            Trace (Me, "Executing action " & Action & Repeat'Img & " times");
         end if;

         Undo_Group (Start => True);
         for R in 1 .. Repeat loop
            Launch_Background_Command
               (Kernel,
                Create_Proxy
                   (Act.Command,
                    (Event       => Event,
                     Context     => C,
                     Synchronous => False,
                     Dir         => No_File,
                     Args        => null,
                     Via_Menu    =>
                       Event /= null and then
                         (Get_Event_Type (Event) = Button_Press or else
                          Get_Event_Type (Event) = Button_Release or else
                          Get_Event_Type (Event) = Key_Press or else
                          Get_Event_Type (Event) = Key_Release),
                     Label       => new String'(Action),
                     Repeat_Count => R,
                     Remaining_Repeat => Repeat - R)),
                Destroy_On_Exit => True,
                Active          => True,
                Show_Bar        => False,
                Queue_Id        => "");
         end loop;

         Undo_Group (Start => False);
         return True;
      end if;
      return False;
   end Execute_In_Background;

end GPS.Kernel.Actions;
