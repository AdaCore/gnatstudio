------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Commands;          use Commands;
with Projects;          use Projects;
with Task_Manager;      use Task_Manager;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Commands.Generic_Asynchronous;

with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Scripts.Commands;    use GPS.Scripts.Commands;

package body GPS.Kernel.Commands is

   type File_Iterate_Data is record
      Kernel           : Kernel_Handle;
      Current_Progress : Natural;
      Total_Progress   : Natural;
      Files            : File_Array_Access;
      Index            : Integer;
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
   --  Interrupts the commands scheduled in the queue

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
         Stop := Index + D.Chunk_Size - 1;

         if Stop > Files'Last then
            Stop := Files'Last;
         end if;

         for J in Start .. Stop loop
            D.Callback (D.Kernel, Files (J));
         end loop;

         Index := Stop + 1;
         D.Current_Progress := D.Current_Progress + 1;
         Set_Progress
           (Command,
            (Running,
             D.Current_Progress,
             D.Total_Progress));
      end Iter_From_File_Array;

   begin
      if D.Stop or else D.Files = null then
         Result := Success;
         return;
      end if;

      if D.Index <= D.Files'Last then
         Iter_From_File_Array (D.Files, D.Index);
      end if;

      if D.Index <= D.Files'Last then
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
      Old_Command := Head (Get_Task_Manager (Kernel), Queue_Name);

      if Old_Command /= null then
         --  If there is already something in the queue, then interrupt it

         Old_Data := Get_Data
           (File_Iterate_Commands.Generic_Asynchronous_Command_Access
              (Get_Command (Old_Command)));
         Old_Data.Stop := True;   --  modify in place via the pointer

         return True;
      end if;

      return False;
   end Kill_File_Queue;

   ---------------------
   -- Do_On_Each_File --
   ---------------------

   procedure Do_On_Each_File
     (Handle         : access Kernel_Handle_Record'Class;
      Callback       : File_Callback;
      Chunk_Size     : Positive := 1;
      Queue_Name     : String := "";
      Operation_Name : String := "";
      Files          : File_Array_Access := null)
   is
      use File_Iterate_Commands;

      C              : Generic_Asynchronous_Command_Access;
      Projects_Count : Natural := 0;
      Iter           : Project_Iterator :=
                         Start (Get_Project (Handle));

      Project_Files  : File_Array_Access;
      All_Files      : File_Array_Access;
      Total_Progress : Natural;

      Old_Command    : Scheduled_Command_Access;
      Command_Data   : File_Iterate_Data_Access;

   begin
      while Current (Iter) /= No_Project loop
         Projects_Count := Projects_Count + 1;
         Next (Iter);
      end loop;

      if Files = null then
         Project_Files := Get_Project (Handle).Source_Files (True);

         All_Files := new File_Array'
           (Get_Registry (Handle).Environment.Predefined_Source_Files
            & Project_Files.all);
         Unchecked_Free (Project_Files);
      else
         All_Files := Files;
      end if;

      Total_Progress := All_Files'Length / Chunk_Size;

      if All_Files'Length mod Chunk_Size /= 0 then
         Total_Progress := Total_Progress + 1;
      end if;

      Old_Command := Head (Get_Task_Manager (Handle), Queue_Name);

      Command_Data := new File_Iterate_Data'
        (Kernel_Handle (Handle),
         Current_Progress => 0,
         Total_Progress   => Total_Progress,
         Files            => All_Files,
         Index            => All_Files'First,
         Stop             => False,
         Chunk_Size       => Chunk_Size,
         Callback         => Callback);

      if Old_Command /= null then
         C := File_Iterate_Commands.Generic_Asynchronous_Command_Access
           (Get_Command (Old_Command));
         Set_Data (C, Command_Data);  --  Free the old data as well
      else
         File_Iterate_Commands.Create
           (C, Operation_Name,
            Command_Data,
            File_Iterate'Access);

         Launch_Background_Command
           (Handle,
            Command_Access (C),
            True,
            True,
            Queue_Name,
            Block_Exit => False);
      end if;
   end Do_On_Each_File;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out File_Iterate_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Iterate_Data, File_Iterate_Data_Access);
   begin
      if D /= null then
         Unchecked_Free (D.Files);
         Unchecked_Free (D);
      end if;
   end Free;

   -------------------------
   -- Kill_File_Iteration --
   -------------------------

   procedure Kill_File_Iteration
     (Kernel : access Kernel_Handle_Record'Class; Queue_Name : String)
   is
      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Kill_File_Queue (Kernel, Queue_Name);
   end Kill_File_Iteration;

end GPS.Kernel.Commands;
