-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2008, AdaCore                  --
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
      Iter              : Entity_Reference_Iterator_Access;
      Kernel            : Kernel_Handle;
      Entity            : Entity_Information;
      Data              : Commands_User_Data;
      Watch             : Gtk_Widget;
      Dispatching_Calls : Boolean;
      Cancelled         : Boolean;
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
      pragma Warnings (Off, Data);
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
               --  If we are seeing a dispatching call to an overridden
               --  subprogram, this could also result in a call to the entity
               --  and we report it

               if Get_Entity (Data.Iter.all) /= Data.Entity then
                  if Get_Kind (Ref) = Dispatching_Call then
                     if not On_Entity_Found
                       (Data.Data, Get_Entity (Data.Iter.all), Parent, Ref,
                        Through_Dispatching => True,
                        Is_Renaming         => False)
                     then
                        Result := Failure;
                     end if;
                  end if;

               else
                  if not On_Entity_Found
                    (Data.Data, Data.Entity, Parent, Ref,
                     Through_Dispatching => False,
                     Is_Renaming         => False)
                  then
                     Result := Failure;
                  end if;
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
         Trace (Exception_Handle, E);
         Result := Failure;
   end Examine_Ancestors_Idle;

   ----------------------------------
   -- Examine_Ancestors_Call_Graph --
   ----------------------------------

   procedure Examine_Ancestors_Call_Graph
     (Kernel            : access Kernel_Handle_Record'Class;
      Entity            : Entity_Information;
      User_Data         : access Commands_User_Data_Record'Class;
      Background_Mode   : Boolean := True;
      Dispatching_Calls : Boolean := False;
      Watch             : Gtk.Widget.Gtk_Widget := null)
   is
      Cb     : Examine_Callback_Access;
      Rename : Entity_Information;
      C      : Ancestor_Commands.Generic_Asynchronous_Command_Access;
      Result : Command_Return_Type;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      Cb := new Examine_Callback'
        (Kernel            => Kernel_Handle (Kernel),
         Data              => Commands_User_Data (User_Data),
         Entity            => Entity,
         Watch             => Watch,
         Cancelled         => False,
         Dispatching_Calls => Dispatching_Calls,
         Iter              => new Entity_Reference_Iterator);
      Ref (Entity);

      --  If we have a renaming, report it

      Rename := Renaming_Of (Entity);
      if Rename /= null then
         if not On_Entity_Found
           (User_Data, Entity, Rename, No_Entity_Reference,
            Through_Dispatching => False,
            Is_Renaming         => True)
         then
            Destroy_Idle (Cb);
            return;
         end if;
      end if;

      Find_All_References
        (Iter               => Cb.Iter.all,
         Entity             => Entity,
         Include_Overridden => Dispatching_Calls);

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
         Trace (Exception_Handle, E);
         Pop_State (Kernel_Handle (Kernel));
   end Examine_Ancestors_Call_Graph;

   -------------------------------
   -- Examine_Entity_Call_Graph --
   -------------------------------

   procedure Examine_Entity_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : Entity_Information;
      User_Data         : access Commands_User_Data_Record'Class;
      Get_All_Refs      : Boolean;
      Dispatching_Calls : Boolean)
   is
      Calls       : Calls_Iterator;
      Called_E    : Entity_Information;
      Refs        : Entity_Reference_Iterator;
      Ref         : Entity_Reference;
      Data        : Commands_User_Data;
      Is_First    : Boolean;
   begin
      if Entity /= null then
         Push_State (Kernel_Handle (Kernel), Busy);
         Calls := Get_All_Called_Entities (Entity);

         For_Each_Entity :
         while not At_End (Calls) loop
            Called_E := Get (Calls);

            if Called_E /= null
              and then Is_Subprogram (Called_E)
            then
               if Get_All_Refs or Dispatching_Calls then
                  --  No search for all references. This was either requested
                  --  explicitly or is needed to resolve dispatching calls

                  Find_All_References
                    (Iter     => Refs,
                     Entity   => Called_E,
                     In_Scope => Entity);
                  Is_First := True;

                  while not At_End (Refs) loop
                     Ref := Get (Refs);
                     if Ref /= No_Entity_Reference
                       and then Show_In_Call_Graph (Get_Kind (Ref))
                       and then Get_Caller (Ref) = Entity
                       and then Is_Subprogram (Get_Entity (Refs))
                       and then Get_Declaration_Of (Called_E) /=
                       Get_Location (Ref)
                     then
                        --  If we want to see all references, report this one
                        --  now, unless it is a dispatching call which is
                        --  already reported later on

                        if Get_All_Refs then
                           if not Dispatching_Calls
                             or else Get_Kind (Ref) /= Dispatching_Call
                           then
                              if not On_Entity_Found
                                (User_Data,
                                 Entity              => Get_Entity (Refs),
                                 Parent              => Entity,
                                 Ref                 => Ref,
                                 Through_Dispatching =>
                                   Get_Kind (Ref) = Dispatching_Call,
                                 Is_Renaming         => False)
                              then
                                 exit For_Each_Entity;
                              end if;
                           end if;

                        --  Else we only want to report the callee once, ie on
                        --  its first reference. We still have to examine all
                        --  references through to solve dispatching calls.

                        elsif Is_First
                          and then Get_Kind (Ref) /= Dispatching_Call
                        then
                           Is_First := False;

                           if not On_Entity_Found
                             (User_Data,
                              Entity              => Get_Entity (Refs),
                              Parent              => Entity,
                              Ref                 => No_Entity_Reference,
                              Through_Dispatching => False,
                              Is_Renaming         => False)
                           then
                              exit For_Each_Entity;
                           end if;
                        end if;

                        --  Now if the reference is in fact a dispatching call,
                        --  report all called entities.

                        if Dispatching_Calls then
                           declare
                              Stop : Boolean := False;
                              function On_Callee
                                (Callee, Primitive_Of : Entity_Information)
                                 return Boolean;

                              function On_Callee
                                (Callee, Primitive_Of : Entity_Information)
                                 return Boolean
                              is
                                 pragma Unreferenced (Primitive_Of);
                              begin
                                 if not On_Entity_Found
                                   (User_Data,
                                    Entity              => Callee,
                                    Parent              => Entity,
                                    Ref                 => Ref,
                                    Through_Dispatching => True,
                                    Is_Renaming         => False)
                                 then
                                    Stop := True;
                                    return False;
                                 end if;
                                 return True;
                              end On_Callee;

                           begin
                              --  Always compute accurate information for the
                              --  call graph, since, as opposed to the
                              --  contextual menu, we have more time to do the
                              --  computation
                              For_Each_Dispatching_Call
                                (Entity    => Get_Entity (Refs),
                                 Ref       => Ref,
                                 On_Callee => On_Callee'Access,
                                 Policy    => Accurate);
                              exit For_Each_Entity when Stop;
                           end;
                        end if;
                     end if;

                     Next (Refs);
                  end loop;
                  Destroy (Refs);
               else
                  if not On_Entity_Found
                    (User_Data,
                     Entity              => Called_E,
                     Parent              => Entity,
                     Ref                 => No_Entity_Reference,
                     Through_Dispatching => False,
                     Is_Renaming         => False)
                  then
                     exit For_Each_Entity;
                  end if;
               end if;
            end if;

            Next (Calls);
         end loop For_Each_Entity;

         Destroy (Calls);

         Destroy (User_Data.all, Cancelled => False);
         Data := Commands_User_Data (User_Data);
         Unchecked_Free (Data);
         Pop_State (Kernel_Handle (Kernel));
      end if;
   end Examine_Entity_Call_Graph;

end Entities.Commands;
