-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2010, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

private with Default_Preferences;
private with GPS.Kernel.MDI;
with GPS.Kernel.Modules;
private with GPS.Styles.UI;
private with GNATCOLL.VFS;
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
      Annotations_Foreground : Default_Preferences.Color_Preference;
      Annotations_Background : Default_Preferences.Color_Preference;
      Annotations_Style      : GPS.Styles.UI.Style_Access;

      File                   : GNATCOLL.VFS.Virtual_File;
      Subprogram             :
        GNATStack.Data_Model.Subprogram_Information_Access;

      CI_Editor_MDI          : GPS.Kernel.MDI.GPS_MDI_Child;
      Call_Tree_View_MDI     : GPS.Kernel.MDI.GPS_MDI_Child;
   end record;

   GNATStack_Editor_Annotations : constant String :=
                                    "GNATStack editor annotations";

   type GNATStack_Module_Id is access all GNATStack_Module_Id_Record'Class;

   Module : GNATStack_Module_Id;

end GNATStack.Module;
