-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Commands;              use Commands;
with Glide_Kernel;          use Glide_Kernel;
with Glide_Kernel.Project;  use Glide_Kernel.Project;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Glide_Intl;            use Glide_Intl;
with Src_Info;              use Src_Info;
with Src_Info.Queries;      use Src_Info.Queries;
with Traces;                use Traces;
with VFS;                   use VFS;
with Commands.Generic_Asynchronous;

package body Refactoring.Performers is

   Me : constant Debug_Handle := Create ("Refactor");

   type Renaming_Error_Record is new File_Error_Reporter_Record with
      record
         No_LI_List          : File_Array_Access;
         No_LI_List_Index    : Natural := 1;
      end record;
   type Renaming_Error is access all Renaming_Error_Record'Class;
   procedure Error
     (Report : in out Renaming_Error_Record; File : VFS.Virtual_File);

   type Get_Locations_Data is record
      Refs                : Location_Array_Access;
      Refs_Index          : Natural := 1;
      Stale_LI_List       : File_Array_Access;
      Stale_LI_List_Index : Natural := 1;
      On_Completion       : Refactor_Performer;
      Kernel              : Kernel_Handle;
      Entity              : Entity_Information;
      Iter                : Entity_Reference_Iterator_Access;
      Errors              : Renaming_Error;
   end record;
   procedure Free (Data : in out Get_Locations_Data);
   package Get_Locations_Commands is new Commands.Generic_Asynchronous
     (Get_Locations_Data, Free);
   use Get_Locations_Commands;
   --  Commands used to search for all occurrences in the background, and
   --  perform some refactoring afterwards

   procedure Find_Next_Location
     (Data    : in out Get_Locations_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Find the next location, and stores it in Data

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Get_Locations_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Refactor_Performer_Record'Class, Refactor_Performer);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Renaming_Error_Record'Class, Renaming_Error);
   begin
      Unchecked_Free (Data.Refs);
      Unchecked_Free (Data.Errors.No_LI_List);
      Unchecked_Free (Data.Stale_LI_List);
      Destroy (Data.Iter);
      Free (Data.On_Completion.all);
      Unchecked_Free (Data.Errors);
      Unchecked_Free (Data.On_Completion);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Factor : in out Refactor_Performer_Record) is
      pragma Unreferenced (Factor);
   begin
      null;
   end Free;

   -----------
   -- Error --
   -----------

   procedure Error
     (Report : in out Renaming_Error_Record; File : VFS.Virtual_File) is
   begin
      Add (Report.No_LI_List, Report.No_LI_List_Index, File);
   end Error;

   -----------------------
   -- Get_All_Locations --
   -----------------------

   procedure Get_All_Locations
     (Kernel                : access Kernel_Handle_Record'Class;
      Entity                : Entity_Information;
      On_Completion         : access Refactor_Performer_Record'Class)
   is
      Data : Get_Locations_Data;
      C    : Get_Locations_Commands.Generic_Asynchronous_Command_Access;
   begin
      Data.On_Completion     := Refactor_Performer (On_Completion);
      Data.Refs              := new Location_Array (1 .. 100);
      Data.Stale_LI_List     := new File_Array (1 .. 10);
      Data.Kernel            := Kernel_Handle (Kernel);
      Data.Entity            := Entity;
      Data.Iter              := new Entity_Reference_Iterator;
      Data.Errors            := new Renaming_Error_Record;
      Data.Errors.No_LI_List := new File_Array (1 .. 10);

      Push_State (Data.Kernel, Busy);

      Add (Data.Refs, Data.Refs_Index,
           (File   => Get_Declaration_File_Of (Entity),
            Line   => Get_Declaration_Line_Of (Entity),
            Column => Get_Declaration_Column_Of (Entity)));
      Find_All_References
        (Root_Project          => Get_Project (Kernel),
         Lang_Handler          => Get_Language_Handler (Kernel),
         Entity                => Entity,
         File_Has_No_LI_Report => File_Error_Reporter (Data.Errors),
         Iterator              => Data.Iter.all);

      Create (C, -"Refactoring", Data, Find_Next_Location'Access);
      Set_Progress (Command_Access (C),
                    (Running,
                     Get_Current_Progress (Data.Iter.all),
                     Get_Total_Progress   (Data.Iter.all)));
      Launch_Background_Command
        (Kernel, Command_Access (C), True, True, "Refactoring");

   exception
      when E : others =>
         Free (Data);
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Get_All_Locations;

   ------------------------
   -- Find_Next_Location --
   ------------------------

   procedure Find_Next_Location
     (Data    : in out Get_Locations_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Ref : constant E_Reference := Get (Data.Iter.all);
   begin
      if Ref = No_Reference then
         Pop_State (Data.Kernel);

         if Confirm_Files
           (Data.Kernel,
            Data.Errors.No_LI_List
              (Data.Errors.No_LI_List'First
                 .. Data.Errors.No_LI_List_Index - 1),
            Data.Stale_LI_List
              (Data.Stale_LI_List'First .. Data.Stale_LI_List_Index - 1))
         then
            Push_State (Data.Kernel, Busy);
            Execute
              (Data.On_Completion,
               Data.Kernel,
               Data.Entity,
               Data.Refs (Data.Refs'First .. Data.Refs_Index - 1),
               Data.Errors.No_LI_List
                 (Data.Errors.No_LI_List'First
                    .. Data.Errors.No_LI_List_Index - 1),
               Data.Stale_LI_List
                 (Data.Stale_LI_List'First .. Data.Stale_LI_List_Index - 1));
            Pop_State (Data.Kernel);
         end if;

         Result := Success;

      else
         if Is_Up_To_Date
           (Get_LI (Data.Iter.all), Compare_With_Sources => True)
         then
            Add (Data.Refs, Data.Refs_Index,
                 (File   => Get_File (Get_Location (Ref)),
                  Line   => Get_Line (Get_Location (Ref)),
                  Column => Get_Column (Get_Location (Ref))));

         --  If we have duplicates, they will always come one after the.
         --  other. So we just have to check the previous one.
         elsif Data.Stale_LI_List_Index = Data.Stale_LI_List'First
           or else Get_LI_Filename (Get_LI (Data.Iter.all)) /=
             Data.Stale_LI_List (Data.Stale_LI_List_Index - 1)
         then
            declare
               Tmp : Boolean;
               pragma Unreferenced (Tmp);
            begin
               Tmp := Is_Up_To_Date (Get_LI (Data.Iter.all),
                                     Compare_With_Sources => True);
            end;
            Add (Data.Stale_LI_List, Data.Stale_LI_List_Index,
                 Get_LI_Filename (Get_LI (Data.Iter.all)));
         end if;

         Next (Get_Language_Handler (Data.Kernel), Data.Iter.all);

         Set_Progress (Command,
                       (Running,
                        Get_Current_Progress (Data.Iter.all),
                        Get_Total_Progress (Data.Iter.all)));
         Result := Execute_Again;
      end if;
   end Find_Next_Location;

end Refactoring.Performers;
