-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
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

with Ada.Unchecked_Deallocation;

with Commands;          use Commands;
with Projects;          use Projects;
with Projects.Registry; use Projects.Registry;
with Task_Manager;      use Task_Manager;
with Traces;            use Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Commands.Generic_Asynchronous;

with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel.Project;      use GPS.Kernel.Project;

package body GPS.Kernel.Commands is

   type File_Iterate_Data is record
      Kernel           : Kernel_Handle;
      Current_Progress : Natural;
      Total_Progress   : Natural;
      Std_Files        : File_Array_Access;
      Project_Files    : File_Array_Access;
      Index_In_Std     : Natural;
      Index_In_Project : Natural;
      Stop             : Boolean := False;

      Chunk_Size       : Integer := 1;
      Callback         : File_Callback;
   end record;
   type File_Iterate_Data_Access is access File_Iterate_Data;

   procedure Free (D : in out File_Iterate_Data_Access);
   --  Free memory associated to D;

   procedure File_Iterate
     (Data    : in out File_Iterate_Data_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Iterate on a file

   package File_Iterate_Commands is new Generic_Asynchronous
     (Data_Type => File_Iterate_Data_Access,
      Free      => Free);

   function Kill_File_Queue
     (Kernel : access Kernel_Handle_Record'Class; Queue_Name : String)
      return Boolean;
   --  Interrupts the commands scheduled in the queue.

   ------------------
   -- File_Iterate --
   ------------------

   procedure File_Iterate
     (Data    : in out File_Iterate_Data_Access;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      pragma Warnings (Off, Data);

      D : File_Iterate_Data_Access renames Data;

      procedure Iter_From_File_Array
        (Files : File_Array_Access; Index : in out Natural);

      --------------------------
      -- Iter_From_File_Array --
      --------------------------

      procedure Iter_From_File_Array
        (Files : File_Array_Access; Index : in out Natural)
      is
         Start, Stop : Integer;
      begin
         Start := Index;
         Stop := Index + D.Chunk_Size;

         if Stop > Files'Last then
            Stop := Files'Last;
         end if;

         for J in Start .. Stop loop
            D.Callback (D.Kernel, Files (J));
         end loop;

         Index := Stop;
         D.Current_Progress := D.Current_Progress + 1;
         Set_Progress
           (Command,
            (Running,
             D.Current_Progress,
             D.Total_Progress));
      end Iter_From_File_Array;

   begin
      if D.Stop then
         Result := Success;
         return;
      end if;

      if D.Std_Files /= null
        and then D.Index_In_Std < D.Std_Files'Last
      then
         Iter_From_File_Array (D.Std_Files, D.Index_In_Std);
         Result := Execute_Again;
      elsif D.Project_Files /= null
        and then D.Index_In_Project < D.Project_Files'Last
      then
         Iter_From_File_Array (D.Project_Files, D.Index_In_Project);
         Result := Execute_Again;
      else
         Result := Success;
      end if;
   end File_Iterate;

   ---------------------
   -- Kill_File_Queue --
   ---------------------

   function Kill_File_Queue
     (Kernel : access Kernel_Handle_Record'Class; Queue_Name : String)
      return Boolean
   is
      use File_Iterate_Commands;

      Old_Command : Scheduled_Command_Access;
      Old_Data    : File_Iterate_Data_Access;
   begin
      Old_Command := Scheduled_Command_Access
        (Head (Get_Task_Manager (Kernel), Queue_Name));

      if Old_Command /= null then
         --  If there is already something in the queue, then interrupt it.

         Old_Data := Get_Data
           (File_Iterate_Commands.Generic_Asynchronous_Command_Access
              (Get_Command (Old_Command)));
         Old_Data.Stop := True;
         Set_Data (File_Iterate_Commands.Generic_Asynchronous_Command_Access
                   (Get_Command (Old_Command)), Old_Data);

         Interrupt_Queue (Kernel, Command_Access (Old_Command));

         return True;
      end if;

      return False;
   end Kill_File_Queue;

   ---------------------
   -- Do_On_Each_File --
   ---------------------

   procedure Do_On_Each_File
     (Handle              : access Kernel_Handle_Record'Class;
      Callback            : File_Callback;
      Chunk_Size          : Positive := 1;
      Queue_Base_Name     : String := "";
      Kill_Existing_Queue : Boolean := False;
      Operation_Name      : String := "")
   is
      use File_Iterate_Commands;

      C              : Generic_Asynchronous_Command_Access;
      Projects_Count : Natural := 0;
      Iter           : Imported_Project_Iterator :=
        Start (Get_Project (Handle));

      Queue_Name : String := Queue_Base_Name & "_0";

      Std_Files      : File_Array_Access;
      Project_Files  : File_Array_Access;
      Total_Progress : Natural;
   begin
      if Kill_Existing_Queue then
         if Kill_File_Queue (Handle, Queue_Name) then
            --  If there is already something on queue 0, then kill it and load
            --  queue 1.
            Queue_Name := Queue_Base_Name & "_1";
         else
            declare
               Dummy : constant Boolean :=
                 Kill_File_Queue (Handle, Queue_Base_Name & "_1");
               pragma Unreferenced (Dummy);
               --  Just in case there is something on queue 1
            begin
               null;
            end;
         end if;
      end if;

      while Current (Iter) /= No_Project loop
         Projects_Count := Projects_Count + 1;
         Next (Iter);
      end loop;

      Std_Files := Get_Predefined_Source_Files (Get_Registry (Handle).all);
      Project_Files := Get_Source_Files
        (Get_Root_Project (Get_Registry (Handle).all), True);

      Total_Progress := Std_Files'Length / Chunk_Size +
        Project_Files'Length / Chunk_Size;

      if Std_Files'Length mod Chunk_Size /= 0 then
         Total_Progress := Total_Progress + 1;
      end if;

      if Project_Files'Length mod Chunk_Size /= 0 then
         Total_Progress := Total_Progress + 1;
      end if;

      File_Iterate_Commands.Create
        (C, Operation_Name,
         new File_Iterate_Data'
           (Kernel_Handle (Handle),
            Current_Progress => 0,
            Total_Progress   => Total_Progress,
            Std_Files        => Std_Files,
            Project_Files    => Project_Files,
            Index_In_Std     => 1,
            Index_In_Project => 1,
            Stop             => False,
            Chunk_Size       => Chunk_Size,
            Callback         => Callback),
         File_Iterate'Access);

      if Queue_Base_Name /= "" then
         Launch_Background_Command
           (Handle,
            Command_Access (C),
            True,
            True,
            Queue_Name,
            Block_Exit => False);
      else
         Launch_Background_Command
           (Handle,
            Command_Access (C),
            True,
            True,
            "",
            Block_Exit => False);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Do_On_Each_File;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out File_Iterate_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Iterate_Data, File_Iterate_Data_Access);
   begin
      Unchecked_Free (D.Std_Files);
      Unchecked_Free (D.Project_Files);
      Unchecked_Free (D);
   end Free;

   -------------------------
   -- Kill_File_Iteration --
   -------------------------

   procedure Kill_File_Iteration
     (Kernel : access Kernel_Handle_Record'Class; Queue_Base_Name : String)
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Kill_File_Queue (Kernel, Queue_Base_Name & "_0");
      Dummy := Kill_File_Queue (Kernel, Queue_Base_Name & "_1");
   end Kill_File_Iteration;

end GPS.Kernel.Commands;
