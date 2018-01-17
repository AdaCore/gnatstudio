------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

private with GPS.Kernel.MDI;
with GPS.Kernel.Modules;
with GPS.Kernel.Style_Manager;
private with GNATCOLL.VFS;
private with GNATStack.Call_Tree_Views;
private with GNATStack.Data_Model;

package GNATStack.Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module

private

   type GNATStack_Module_Id_Record
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
     new GPS.Kernel.Modules.Module_ID_Record with
   record
      Data                   :
        aliased GNATStack.Data_Model.Analysis_Information;
      Loaded                 : Boolean := False;
      Annotations_Style      : GPS.Kernel.Style_Manager.Style_Access;

      File                   : GNATCOLL.VFS.Virtual_File;
      Subprogram             :
        GNATStack.Data_Model.Subprogram_Information_Access;

      CI_Editor_MDI          : GPS.Kernel.MDI.GPS_MDI_Child;
      Call_Tree_View         : GNATStack.Call_Tree_Views.Call_Tree_View;
      Call_Tree_View_MDI     : GPS.Kernel.MDI.GPS_MDI_Child;
   end record;

   GNATStack_Editor_Annotations : constant String :=
                                    "GNATStack editor annotations";

   type GNATStack_Module_Id is access all GNATStack_Module_Id_Record'Class;

   Module : GNATStack_Module_Id;

end GNATStack.Module;
