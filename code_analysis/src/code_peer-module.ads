-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
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

with Glib.Object;
with Gtk.Menu;

with Default_Preferences;
with GNATCOLL.Filesystem; use GNATCOLL.Filesystem;
with GPS.Kernel.Modules;
with GPS.Kernel.Styles;
with GPS.Kernel.MDI;
with Code_Analysis;

private with Code_Peer.Summary_Reports;

package Code_Peer.Module is

   type Module_Id_Record
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
     new GPS.Kernel.Modules.Module_ID_Record with private;

   type Code_Peer_Module_Id is access all Module_Id_Record'Class;

   procedure Show_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access);

   procedure Hide_Annotations
     (Self : access Module_Id_Record'Class;
      File : Code_Analysis.File_Access);

   procedure Load
     (Self : access Module_Id_Record'Class;
      File : Filesystem_String);

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : Code_Peer.Message_Access);

   procedure Review_Message
     (Self    : access Module_Id_Record'Class;
      Message : Code_Peer.Message_Access;
      File    : String);

   type Submenu_Factory_Record
     (Module : access Module_Id_Record'Class) is
     new GPS.Kernel.Modules.Submenu_Factory_Record with null record;

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Object  : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module

private

   type Message_Probability_Style_Array is
     array (Code_Peer.Message_Probability_Level)
       of GPS.Kernel.Styles.Style_Access;

   type Module_Id_Record
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
     new GPS.Kernel.Modules.Module_ID_Record with record
      Tree             : Code_Analysis.Code_Analysis_Tree;
      Report_Subwindow : GPS.Kernel.MDI.GPS_MDI_Child;
      Report           : Code_Peer.Summary_Reports.Summary_Report;
      Annotation_Style : GPS.Kernel.Styles.Style_Access;
      Message_Styles   : Message_Probability_Style_Array;

      --  Global messages filter

      Filter_Criteria  : Code_Peer.Message_Filter_Criteria;

      --  Preferences

      Expert_Mode      : Default_Preferences.Boolean_Preference;
   end record;

   procedure Update_Location_View (Self : access Module_Id_Record'Class);

   function Is_In_Expert_Mode
     (Self : access Module_Id_Record'Class) return Boolean;
   --  Returns True if expert mode is activated.

end Code_Peer.Module;
