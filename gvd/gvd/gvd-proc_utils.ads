-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides system specific utilities to get information about
--  processes.

package GVD.Proc_Utils is

   type Process_Handle is limited private;
   --  Handle of a process list.

   subtype Info_Len_Range is Natural range 0 .. 255;
   --  Range of a process information.

   type Process_Info
     (Id_Len   : Info_Len_Range := Info_Len_Range'First;
      Info_Len : Info_Len_Range := Info_Len_Range'First) is
   record
      Id   : String (1 .. Id_Len);
      Info : String (1 .. Info_Len);
   end record;
   --  Individual information concerning a process.

   procedure Open_Processes (Handle : out Process_Handle);
   --  Initialize a connection to the current machine in order to retrieve
   --  process information.

   procedure Open_Processes (Handle : out Process_Handle; Host : String);
   --  Initialize a connection to a remote host in order to retrieve process
   --  information.

   procedure Next_Process
     (Handle  : Process_Handle;
      Info    : out Process_Info;
      Success : out Boolean);
   --  Return information concerning the next process.
   --  Success is set to True if there is a remaining process, false otherwise.

   procedure Close_Processes (Handle : in out Process_Handle);
   --  Close the connection established in handle.

private

   type Process_Record;
   type Process_Handle is access all Process_Record;

end GVD.Proc_Utils;
