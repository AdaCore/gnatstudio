-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
--                              AdaCore                              --
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

with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Commands.Generic_Asynchronous;  use Commands;
with Entities.Queries;               use Entities.Queries;
with Glib.Object;                    use Glib.Object;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Task_Manager;        use GPS.Kernel.Task_Manager;
with GPS.Intl;                       use GPS.Intl;
with Gtk.Widget;                     use Gtk.Widget;
with System;                         use System;
with Traces;                         use Traces;

package body Entities.Commands is

   type Examine_Callback is record
      Iter           : Entity_Reference_Iterator_Access;
      Kernel         : Kernel_Handle;
      Entity         : Entity_Information;
      Data           : Commands_User_Data;
      Watch          : Gtk_Widget;
      Cancelled      : Boolean;
   end record;
   type Examine_Callback_Access is access Examine_Callback;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Commands_User_Data_Record'Class, Commands_User_Data);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Examine_Callback, Examine_Callback_Access);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Examine_Callback_Access);

   procedure Destroy_Idle (Data : in out Examine_Callback_Access);
   --  Called when the idle loop is destroyed.

   package Ancestor_Commands is new Generic_Asynchronous
     (Examine_Callback_Access, Destroy_Idle);

   procedure Examine_Ancestors_Idle
     (Data    : in out Examine_Callback_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Called for every occurrence of Data.Entity

   procedure Watch_Destroyed_While_Computing
     (Data : System.Address; Object : System.Address);
   pragma Convention (C, Watch_Destroyed_While_Computing);

   -------------------------------------
   -- Watch_Destroyed_While_Computing --
   -------------------------------------

   procedure Watch_Destroyed_While_Computing
     (Data : System.Address; Object : System.Address)
   is
      pragma Unreferenced (Object);
   begin
      Convert (Data).Cancelled := True;
   end Watch_Destroyed_While_Computing;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Callback_Access) is
   begin
      if not Data.Cancelled
        and then Data.Watch /= null
      then
         Weak_Unref (Data.Watch, Watch_Destroyed_While_Computing'Access,
                     Data.all'Address);
      end if;

      Destroy (Data.Data.all, Data.Cancelled);
      Unchecked_Free (Data.Data);
      Destroy (Data.Iter);
      Unref (Data.Entity);
      Pop_State (Data.Kernel);
      Unchecked_Free (Data);
   end Destroy_Idle;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Data : in out Commands_User_Data_Record; Cancelled : Boolean)
   is
      pragma Unreferenced (Data, Cancelled);
   begin
      null;
   end Destroy;

   ----------------------------
   -- Examine_Ancestors_Idle --
   ----------------------------

   procedure Examine_Ancestors_Idle
     (Data    : in out Examine_Callback_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Parent : Entity_Information;
      Ref    : Entity_Reference;
   begin
      if Data.Cancelled then
         Result := Failure;

      elsif At_End (Data.Iter.all) then
         Result := Success;

      else
         Ref := Get (Data.Iter.all);
         Result := Execute_Again;

         if Ref /= No_Entity_Reference then
            Parent := Get_Caller (Ref);
            if Parent /= null
              and then Show_In_Call_Graph (Get_Kind (Ref))
              and then Is_Container (Get_Kind (Parent).Kind)
            then
               if not On_Entity_Found
                 (Data.Data, Data.Entity, Parent, Ref, False)
               then
                  Result := Failure;
               end if;
            end if;
         end if;

         Next (Data.Iter.all);

         if Command /= null then
            Set_Progress
              (Command,
               (Running,
                Get_Current_Progress (Data.Iter.all),
                Get_Total_Progress (Data.Iter.all)));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         Result := Failure;
   end Examine_Ancestors_Idle;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel          : access Kernel_Handle_Record'Class;
      Entity          : Entity_Information;
      User_Data       : access Commands_User_Data_Record'Class;
      Background_Mode : Boolean := True;
      Watch           : Gtk.Widget.Gtk_Widget := null)
   is
      Cb     : Examine_Callback_Access;
      Rename : Entity_Information;
      C      : Ancestor_Commands.Generic_Asynchronous_Command_Access;
      Result : Command_Return_Type;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Cb := new Examine_Callback'
        (Kernel    => Kernel_Handle (Kernel),
         Data      => Commands_User_Data (User_Data),
         Entity    => Entity,
         Watch     => Watch,
         Cancelled => False,
         Iter      => new Entity_Reference_Iterator);
      Ref (Entity);

      --  If we have a renaming, report it

      Rename := Renaming_Of (Entity);
      if Rename /= null then
         if not On_Entity_Found
           (User_Data, Entity, Rename, No_Entity_Reference, True)
         then
            Destroy_Idle (Cb);
            return;
         end if;
      end if;

      Find_All_References (Iter => Cb.Iter.all, Entity => Entity);

      if Watch /= null then
         Weak_Ref
           (Watch,
            Watch_Destroyed_While_Computing'Access,
            Cb.all'Address);
      end if;

      if Background_Mode then
         Ancestor_Commands.Create
           (C, -"Called by", Cb, Examine_Ancestors_Idle'Access);
         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "call graph");
      else
         loop
            Examine_Ancestors_Idle (Cb, Command_Access (C), Result);
            exit when Result /= Execute_Again;
         end loop;
         Destroy_Idle (Cb);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Ancestors_Call_Graph;

end Entities.Commands;
