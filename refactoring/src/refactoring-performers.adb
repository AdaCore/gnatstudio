-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
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
with GPS.Kernel;          use GPS.Kernel;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Intl;            use GPS.Intl;
with Entities;              use Entities;
with Entities.Queries;      use Entities.Queries;
with Traces;                use Traces;
with Commands.Generic_Asynchronous;

package body Refactoring.Performers is
   use Location_Arrays;
   use File_Arrays;

   type Renaming_Error_Record is new File_Error_Reporter_Record with
      record
         No_LI_List : File_Arrays.Instance := File_Arrays.Empty_Instance;
      end record;
   type Renaming_Error is access all Renaming_Error_Record'Class;
   procedure Error
     (Report : in out Renaming_Error_Record; File : Source_File);

   type Get_Locations_Data is record
      Refs                : Location_Arrays.Instance;
      Stale_LI_List       : File_Arrays.Instance;
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

   procedure On_End_Of_Search (Data : in out Get_Locations_Data);
   --  Called when all the related files have been searched and the refactoring
   --  should be performed

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Get_Locations_Data) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Refactor_Performer_Record'Class, Refactor_Performer);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Renaming_Error_Record'Class, Renaming_Error);
   begin
      Free (Data.Refs);
      Free (Data.Errors.No_LI_List);
      Free (Data.Stale_LI_List);
      Destroy (Data.Iter);
      if Data.On_Completion /= null then
         Free (Data.On_Completion.all);
         Unchecked_Free (Data.On_Completion);
      end if;
      Unchecked_Free (Data.Errors);
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
     (Report : in out Renaming_Error_Record; File : Entities.Source_File) is
   begin
      Append (Report.No_LI_List, File);
   end Error;

   -----------------------
   -- Get_All_Locations --
   -----------------------

   procedure Get_All_Locations
     (Kernel                : access Kernel_Handle_Record'Class;
      Entity                : Entity_Information;
      On_Completion         : access Refactor_Performer_Record'Class;
      Auto_Compile          : Boolean := False;
      Background_Mode       : Boolean := True)
   is
      pragma Unreferenced (Auto_Compile);
      Data : Get_Locations_Data;
      C    : Get_Locations_Commands.Generic_Asynchronous_Command_Access;
      Result : Command_Return_Type;
   begin
      Data.On_Completion     := Refactor_Performer (On_Completion);
      Data.Kernel            := Kernel_Handle (Kernel);
      Data.Entity            := Entity;
      Data.Iter              := new Entity_Reference_Iterator;
      Data.Errors            := new Renaming_Error_Record;

      Push_State (Data.Kernel, Busy);

      Find_All_References
        (Iter                  => Data.Iter.all,
         Entity                => Entity,
         File_Has_No_LI_Report => File_Error_Reporter (Data.Errors));

      Create (C, -"Refactoring", Data, Find_Next_Location'Access);
      Set_Progress
        (Command_Access (C),
         (Running,
          Get_Current_Progress (Data.Iter.all),
          Get_Total_Progress   (Data.Iter.all)));
      if Background_Mode then
         Launch_Background_Command
           (Kernel, Command_Access (C), True, True, "Refactoring");
      else
         loop
            Result := Execute (C);
            exit when Result /= Execute_Again;
         end loop;
         On_End_Of_Search (Data);
      end if;

   exception
      when E : others =>
         Free (Data);
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Get_All_Locations;

   ----------------------
   -- On_End_Of_Search --
   ----------------------

   procedure On_End_Of_Search (Data : in out Get_Locations_Data) is
   begin
      Pop_State (Data.Kernel);

      if Confirm_Files
        (Data.Kernel, Data.Errors.No_LI_List, Data.Stale_LI_List)
      then
         Push_State (Data.Kernel, Busy);
         Execute
           (Data.On_Completion,
            Data.Kernel,
            Data.Entity,
            Data.Refs,
            Data.Errors.No_LI_List,
            Data.Stale_LI_List);
         Pop_State (Data.Kernel);
      end if;
   end On_End_Of_Search;

   ------------------------
   -- Find_Next_Location --
   ------------------------

   procedure Find_Next_Location
     (Data    : in out Get_Locations_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      Ref    : constant Entity_Reference := Get (Data.Iter.all);
      Source : Source_File;
   begin
      if At_End (Data.Iter.all) then
         On_End_Of_Search (Data);
         Result := Success;

      elsif Ref /= No_Entity_Reference then
         Source := Get_File (Get_Location (Ref));
         if Is_Up_To_Date (Source) then
            Append (Data.Refs,
                    (File   => Source,
                     Line   => Get_Line (Get_Location (Ref)),
                     Column => Get_Column (Get_Location (Ref))));

         --  If we have duplicates, they will always come one after the
         --  other. So we just have to check the previous one.
         else
            Append (Data.Refs,
                    (File   => Source,
                     Line   => Get_Line (Get_Location (Ref)),
                     Column => Get_Column (Get_Location (Ref))));

            if Length (Data.Stale_LI_List) = 0
              or else Source /=
                Data.Stale_LI_List.Table (Last (Data.Stale_LI_List))
            then
               Append (Data.Stale_LI_List, Source);
            end if;
         end if;

         Next (Data.Iter.all);

         Set_Progress (Command,
                       (Running,
                        Get_Current_Progress (Data.Iter.all),
                        Get_Total_Progress (Data.Iter.all)));
         Result := Execute_Again;

      else
         Next (Data.Iter.all);
         Result := Execute_Again;
      end if;
   end Find_Next_Location;

end Refactoring.Performers;
