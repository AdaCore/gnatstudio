------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  ??? missing description

with Glib.Object;        use Glib.Object;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Scripts;
with GPS.Kernel;         use GPS.Kernel;
with Gtk.Menu;           use Gtk.Menu;
with VCS;                use VCS;

package VCS_View_API is

   -------------------------------
   -- Contextual menu callbacks --
   -------------------------------

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Create_Tag
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Create a new tag or branch

   procedure On_Menu_Switch_Tag
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Switch to a tag or branch

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Add
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Add_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Add_Directory_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Remove
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Remove_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Remove_Directory_No_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Revert
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Resolved
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Remove_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Diff_Working
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Diff_Base_Head
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Diff_Specific
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Diff2
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Diff_Tag
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Do a diff between the current version and a specific tag

   procedure On_Menu_Diff_Other_Revision
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Do a diff between the other revision and revision

   procedure On_Menu_View_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_View_Log_Text
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_View_Log_Rev
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_View_File_Revision
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  View a specific revision of a file

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Merge
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Merge changes from a branch

   procedure On_Menu_Edit_ChangeLog
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Get_Status_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Update_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_List_Project_Files
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_List_Project_Files_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Get_Status_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Update_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  ???

   procedure On_Menu_Remove_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Remove the project node from the explorer

   procedure VCS_Explorer_Contextual_Menu
     (Context         : Selection_Context;
      Menu            : access Gtk.Menu.Gtk_Menu_Record'Class;
      Show_Everything : Boolean);
   --  Complete Menu with the commands related to the VCS explorer,
   --  according to the information in Context.
   --  If Show_Everything is True, add insensitive menus for items that do not
   --  correspond to the context.

   procedure Open_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context);
   --  If the VCS Explorer is not displayed, display it

   function Get_Current_Ref
     (Context : Selection_Context) return VCS_Access;
   --  Convenience function to get the current VCS system.
   --  If the creator of the current context is the VCS module, then
   --  the Ref will be obtained from the VCS Explorer, otherwise it
   --  will be obtained from the project.

   procedure Query_Status_For_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  List all open files in the project

   procedure Update_All
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Update all files in the project

   function Get_Current_Ref
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type) return VCS_Access;
   --  Return the VCS reference registered in Project

   procedure VCS_Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for VCS commands, that take a file name as their first parameter

end VCS_View_API;
