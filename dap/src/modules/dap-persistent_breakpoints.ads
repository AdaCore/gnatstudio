------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.VFS;        use GNATCOLL.VFS;

with GPS.Kernel;          use GPS.Kernel;
with DAP.Types;           use DAP.Types;
with DAP.Breakpoint_Maps; use DAP.Breakpoint_Maps;

package DAP.Persistent_Breakpoints is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   function Get_Persistent_Breakpoints return All_Breakpoints;

   procedure Show_Breakpoint
     (Kernel  : not null access Kernel_Handle_Record'Class;
      B       : Breakpoint_Data);

   procedure Show_Breakpoints_In_All_Editors
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Update the side column for all editors, and show the persistent
   --  breakpoints info

   procedure Hide_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);

   procedure Delete_Multiple_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List);
   --  Go through the list and delete the breakpoints. The list is not freed
   --  by this procedure.

   procedure Clear_All_Breakpoints
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Delete all breakpoints

   procedure Set_Breakpoints_State
     (Kernel : not null access Kernel_Handle_Record'Class;
      List   : Breakpoint_Identifier_Lists.List;
      State  : Boolean);

   procedure Debugger_Initialization;

   procedure Synchronize
     (Kernel : not null access Kernel_Handle_Record'Class;
      Actual : All_Breakpoints);

end DAP.Persistent_Breakpoints;
