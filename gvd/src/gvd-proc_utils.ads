------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

--  This package provides system specific utilities to get information about
--  processes.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with GPS.Kernel; use GPS.Kernel;
with VSS.Strings; use VSS.Strings;

package GVD.Proc_Utils is

   type Process_Handle is private;
   --  Handle of a process list.

   subtype Info_Len_Range is Natural range 0 .. 4095;
   --  Range of a process information.

   procedure Open_Processes (Handle : out Process_Handle;
                             Kernel : Kernel_Handle);
   --  Initialize a connection to the debug machine in order to retrieve
   --  process information.

   type Process_Info is record
      Id : Virtual_String;
      Name : Virtual_String;
   end record;

   procedure Next_Process
     (Handle  : Process_Handle;
      Info    : out Process_Info;
      Success : out Boolean);
   --  Return information concerning the next process.
   --  Success is set to True if there is a remaining process, false otherwise.

   procedure Close_Processes (Handle : in out Process_Handle);
   --  Close the connection established in handle.

   package Process_Info_List is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Process_Info, "=");

   function Py_PSUtils
     (Kernel : Kernel_Handle)
      return Process_Info_List.List;
   --  Return the list of processes using the psutils python package.

private

   type Process_Record;
   type Process_Handle is access all Process_Record;

end GVD.Proc_Utils;
