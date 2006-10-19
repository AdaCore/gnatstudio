-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

--  This package holds some generic commands that could be used widely in GPS.

package GPS.Kernel.Commands is

   type File_Callback is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File);

   procedure Do_On_Each_File
     (Handle              : access Kernel_Handle_Record'Class;
      Callback            : File_Callback;
      Chunk_Size          : Positive := 1;
      Queue_Base_Name     : String := "";
      Kill_Existing_Queue : Boolean := False;
      Operation_Name      : String := "");
   --  This procedure will launch a GPS command wich will call the given
   --  callback sequentially on each file of the project, including the files
   --  found from the ada library. It's possible to change the number of files
   --  analyzed per iteration by modifiying the Chunk_Size parameter.
   --  Queue_Base_Name is used to give the basename of the queue. The actual
   --  queues will be suffixed with _0 or _1, in order to allow the destruction
   --  of a previous queue if Kill_Existing_Queue is true.

   procedure Kill_File_Iteration
     (Kernel : access Kernel_Handle_Record'Class; Queue_Base_Name : String);
   --  Kills the queue deduced from the base name given in parameter, assuming
   --  that these queues have been made for a file iteration from
   --  Do_On_Each_File.

end GPS.Kernel.Commands;
