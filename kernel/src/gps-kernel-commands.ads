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

--  This package holds some generic commands that could be used widely in GPS.

package GPS.Kernel.Commands is

   type File_Callback is access procedure
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);

   procedure Do_On_Each_File
     (Handle            : access Kernel_Handle_Record'Class;
      Callback          : File_Callback;
      Chunk_Size        : Positive := 1;
      Queue_Name        : String := "";
      Operation_Name    : String := "";
      Files             : File_Array_Access := null);
   --  This procedure will launch a GPS command wich will call the given
   --  callback sequentially on each file given in parameter It's possible to
   --  change the number of files analyzed per iteration by modifiying the
   --  Chunk_Size parameter. Queue_Base_Name is used to give the basename of
   --  the queue. If Files is null, then all files of the registry will be
   --  used, otherwise, only the argument given in parameter will be. The file
   --  array will be freed by the command at the end of the process.

   procedure Kill_File_Iteration
     (Kernel : access Kernel_Handle_Record'Class; Queue_Name : String);
   --  Kills the queue deduced from the base name given in parameter, assuming
   --  that these queues have been made for a file iteration from
   --  Do_On_Each_File.

end GPS.Kernel.Commands;
