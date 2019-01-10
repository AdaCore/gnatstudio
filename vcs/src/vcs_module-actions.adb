------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with Glib.Object;               use Glib.Object;
with Glib;                      use Glib;

with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Widget;                use Gtk.Widget;

with Commands.VCS;              use Commands.VCS;

with Log_Utils;                 use Log_Utils;

with GPS.Core_Kernels;          use GPS.Core_Kernels;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;

with VCS.Generic_VCS;           use VCS.Generic_VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_View_API;              use VCS_View_API;

------------------------
-- VCS_Module.Actions --
------------------------

package body VCS_Module.Actions is

   type Has_VCS_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : Selection_Context) return Boolean;
   --  True when the current context is associated with a known VCS

   type No_File_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access No_File_Filter;
      Context : Selection_Context) return Boolean;
   --  Return True if the filter does not contain a valid file

   type Has_Revision_Log_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Revision_Log_Filter;
      Context : Selection_Context) return Boolean;
   --  Return True if Context contains a file which has a revision log

   type Is_Revision_Log_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Revision_Log_Filter;
      Context : Selection_Context) return Boolean;
   --  Return True if Context contains a file which is a revision log

   type Has_Project_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Project_Filter;
      Context : Selection_Context) return Boolean;
   --  Return True if the context has a project

   type Has_Revision_Information is new Action_Filter_Record with
     null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Revision_Information;
      Context : Selection_Context) return Boolean;
   --  Return True if the context contains revision information

   type Has_Other_Revision_Information is new Action_Filter_Record with
     null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_Revision_Information;
      Context : Selection_Context) return Boolean;
   --  Return True if the context contains the information for the other
   --  revision.

   type Has_Tag_Information is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Tag_Information;
      Context : Selection_Context) return Boolean;
   --  Return True if the context contains tag information

   type VCS_Contextual_Menu is new Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions (Kernel : access Kernel_Handle_Record'Class) is

      procedure Register_Action_Menu
        (Action_Label  : String;
         Description   : String;
         Filter        : Action_Filter;
         Callback      : Context_Callback.Marshallers.Void_Marshaller.Handler);
      --  Registers an action.

      Has_Revision_Log : constant Action_Filter :=
                           new Has_Revision_Log_Filter;
      Is_Revision_Log  : constant Action_Filter :=
                           new Is_Revision_Log_Filter;

      Is_File_Filter : constant Action_Filter :=
                         Lookup_Filter (Kernel, "File");

      File_Filter : constant Action_Filter :=
                      Lookup_Filter (Kernel, "File") and not Is_Revision_Log;

      Dir_Filter  : constant Action_Filter :=
                      Lookup_Filter (Kernel, "Directory")
                        and not Is_Revision_Log;

      Prj_Filter  : constant Action_Filter :=
                      new Has_Project_Filter;

      Directory_No_File_Filter : constant Action_Filter :=
                                   new No_File_Filter;

      Has_Revision_Filter : constant Action_Filter :=
                              new Has_Revision_Information;

      Has_Other_Revision_Filter : constant Action_Filter :=
                                    new Has_Other_Revision_Information;

      Has_Tag_Filter : constant Action_Filter :=
                         new Has_Tag_Information;

      --------------------------
      -- Register_Action_Menu --
      --------------------------

      procedure Register_Action_Menu
        (Action_Label  : String;
         Description   : String;
         Filter        : Action_Filter;
         Callback      : Context_Callback.Marshallers.Void_Marshaller.Handler)
      is
         Command       : Generic_Kernel_Command_Access;
      begin
         Create (Command, Kernel, Callback);
         Register_Action
           (Kernel, Action_Label, Command, Description,
            Filter,
            Category => "VCS");
      end Register_Action_Menu;

      Filter : Action_Filter;

   begin
      Register_Action_Menu
        ("Status",
         -"Query the status of the current selection",
         File_Filter,
         On_Menu_Get_Status'Access);

      Register_Action_Menu
        ("Update",
         -"Update to the current repository revision",
         File_Filter,
         On_Menu_Update'Access);

      Register_Action_Menu
        ("Commit",
         -"Commit current file, or file corresponding to the current log",
         Is_File_Filter and (Has_Revision_Log or Is_Revision_Log),
         On_Menu_Commit'Access);

      Register_Action_Menu
        ("Commit (from revision log)",
         -"Commit current file, or file corresponding to the current log",
         Is_Revision_Log,
         On_Menu_Commit'Access);

      Register_Action_Menu
        ("Commit (via revision log)",
         -"Commit current file, or file corresponding to the current log",
         File_Filter and not Has_Revision_Log,
         On_Menu_Commit'Access);

      Register_Action_Menu
        ("Open",
         -"Open the current file for editing",
         File_Filter,
         On_Menu_Open'Access);

      Register_Action_Menu
        ("History (as text)",
         -"View the revision history for the current file as text",
         File_Filter,
         On_Menu_View_Log_Text'Access);

      Register_Action_Menu
        ("History",
         -"View the revision history for the current file",
         File_Filter,
         On_Menu_View_Log'Access);

      Register_Action_Menu
        ("History for revision",
         -"View the revision history for one revision of the current file",
         File_Filter,
         On_Menu_View_Log_Rev'Access);

      Register_Action_Menu
        ("Diff against head",
         -"Compare current file with the most recent revision",
         File_Filter,
         On_Menu_Diff'Access);

      Register_Action_Menu
        ("Diff against revision",
         -"Compare current file against a specified revision",
         File_Filter,
         On_Menu_Diff_Specific'Access);

      Register_Action_Menu
        ("Diff between two revisions",
         -"Compare two specified revisions of current file",
         File_Filter,
         On_Menu_Diff2'Access);

      Register_Action_Menu
        ("Diff base against head",
         -"Compare base and head revisions of current file",
         File_Filter,
         On_Menu_Diff_Base_Head'Access);

      Register_Action_Menu
        ("Diff against base",
         -"Compare against base revision of current file",
         File_Filter,
         On_Menu_Diff_Working'Access);

      --  ??? To do

      Register_Action_Menu
        ("Diff against tag",
         -"Compare base and head revisions of current file",
         Has_Tag_Filter,
         On_Menu_Diff_Tag'Access);

      Register_Action_Menu
        ("Diff against selected revision",
         -"Compare against selected revision",
         Has_Revision_Filter and Has_Other_Revision_Filter,
         On_Menu_Diff_Other_Revision'Access);

      Register_Action_Menu
        ("Annotate",
         -"Annotate the current file",
         File_Filter,
         On_Menu_Annotate'Access);

      Register_Action_Menu
        ("Remove Annotate",
         -"Remove the annotations from current file",
         File_Filter,
         On_Menu_Remove_Annotate'Access);

      --  Add the log handling actions only if at least one VCS supports log

      Register_Action_Menu
        ("Edit revision log",
         -"Edit the revision log for the current file",
         File_Filter,
         On_Menu_Edit_Log'Access);

      Register_Action_Menu
        ("Edit global ChangeLog",
         -"Edit the global ChangeLog for the current selection",
         File_Filter,
         On_Menu_Edit_ChangeLog'Access);

      Register_Action_Menu
        ("Remove revision log",
         -"Remove the revision log corresponding to the current file",
         Is_File_Filter and Has_Revision_Log,
         On_Menu_Remove_Log'Access);

      Register_Action_Menu
        ("Add",
         -"Add the current file to repository",
         Is_File_Filter and Has_Revision_Log,
         On_Menu_Add'Access);

      Register_Action_Menu
        ("Add (via revision log)",
         -"Add the current file to repository",
         File_Filter and not Has_Revision_Log,
         On_Menu_Add'Access);

      Register_Action_Menu
        ("Add no commit",
         -"Add the current file to repository, do not commit",
         File_Filter,
         On_Menu_Add_No_Commit'Access);

      Register_Action_Menu
        ("Remove",
         -"Remove the current file from repository",
         Is_File_Filter and Has_Revision_Log,
         On_Menu_Remove'Access);

      Register_Action_Menu
        ("Remove (via revision log)",
         -"Remove the current file from repository",
         File_Filter and not Has_Revision_Log,
         On_Menu_Remove'Access);

      Register_Action_Menu
        ("Remove no commit",
         -"Remove the current file from repository, do not commit",
         File_Filter,
         On_Menu_Remove_No_Commit'Access);

      Register_Action_Menu
        ("Revert",
         -"Revert the current file to repository revision",
         File_Filter,
         On_Menu_Revert'Access);

      Register_Action_Menu
        ("Resolved",
         -"Mark file conflicts resolved",
         File_Filter,
         On_Menu_Resolved'Access);

      Register_Action_Menu
        ("Create tag",
         -"Create a tag or branch tag",
         File_Filter,
         On_Menu_Create_Tag'Access);

      Register_Action_Menu
        ("Switch tag",
         -"Switch to a specific tag or branch",
         File_Filter,
         On_Menu_Switch_Tag'Access);

      Register_Action_Menu
        ("Merge",
         -"Merge into tag",
         Has_Tag_Filter,
         On_Menu_Merge'Access);

      Register_Action_Menu
        ("View revision",
         -"View a specific revision of current file",
         Has_Revision_Filter,
         On_Menu_View_File_Revision'Access);

      Register_Action_Menu
        ("Add directory, no commit",
         -"Add the current directory",
         Directory_No_File_Filter,
         On_Menu_Add_Directory_No_Commit'Access);

      Register_Action_Menu
        ("Remove directory, no commit",
         -"Remove the current directory",
         Directory_No_File_Filter,
         On_Menu_Remove_Directory_No_Commit'Access);

      Register_Action_Menu
        ("Commit directory",
         -"Commit the current directory",
         Directory_No_File_Filter,
         On_Menu_Commit'Access);

      Register_Action_Menu
        ("Status dir",
         -"Query the status of the current directory",
         Dir_Filter,
         On_Menu_Get_Status_Dir'Access);

      Register_Action_Menu
        ("Update dir",
         -"Update the current directory",
         Dir_Filter,
         On_Menu_Update_Dir'Access);

      Register_Action_Menu
        ("Status dir (recursively)",
         -"Query the status of the current directory recursively",
         Dir_Filter,
         On_Menu_Get_Status_Dir_Recursive'Access);

      Register_Action_Menu
        ("Update dir (recursively)",
         -"Update the current directory (recursively)",
         Dir_Filter,
         On_Menu_Update_Dir_Recursive'Access);

      Register_Action_Menu
        ("List project",
         -"List all the files in project",
         Prj_Filter or File_Filter,
         On_Menu_List_Project_Files'Access);

      Register_Action_Menu
        ("Status project",
         -"Query the status of the current project",
         Prj_Filter or File_Filter,
         On_Menu_Get_Status_Project'Access);

      Register_Action_Menu
        ("Update project",
         -"Update the current project",
         Prj_Filter,
         On_Menu_Update_Project'Access);

      Register_Action_Menu
        ("List project (recursively)",
         -"List all the files in project and subprojects",
         Prj_Filter,
         On_Menu_List_Project_Files_Recursive'Access);

      Register_Action_Menu
        ("Status project (recursively)",
         -"Query the status of the current project recursively",
         Prj_Filter,
         On_Menu_Get_Status_Project_Recursive'Access);

      Register_Action_Menu
        ("Update project (recursively)",
         -"Update the current project (recursively)",
         Prj_Filter,
         On_Menu_Update_Project_Recursive'Access);

      Filter := new Has_VCS_Filter;
      Register_Filter (Kernel, Filter, "VCS");

      Register_Action_Menu
        ("VCS update all projects",
         -"Update all projects (recursively)",
         null,
         Update_All'Access);

      Register_Action_Menu
        ("VCS query status for all projects",
         -"Query status for all projects (recursively)",
         null,
         Query_Status_For_Project'Access);

      Register_Contextual_Submenu
        (Kernel     => Kernel,
         Name       => -"Version Control",
         Filter     => Filter,
         Submenu    => new VCS_Contextual_Menu);
   end Register_Actions;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_VCS_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return (Has_File_Information (Context)
              or else Has_Project_Information (Context)
              or else Has_Directory_Information (Context))
        and then Get_Current_Ref (Context) /= Unknown_VCS_Reference
        and then (not Has_Entity_Name_Information (Context)
                  or else Get_Name (Module_ID (Get_Creator (Context))) =
                    "Source_Editor");
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access No_File_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      File : Virtual_File;
   begin
      --  No directory: no match
      if not Has_Directory_Information (Context) then
         return False;
      end if;

      --  A diretory but no file information: match
      if not Has_File_Information (Context) then
         return True;
      end if;

      --  If we reach this point, it is possible that File_Information is
      --  set to the directory, in which case we also match.
      File := File_Information (Context);

      if File.Is_Directory then
         return True;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Revision_Log_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Ref : constant VCS_Access := Get_Current_Ref (Context);
   begin
      --  If the VCS does not require a log, this filter should never be
      --  effective

      if Ref /= null
        and then not Ref.Require_Log
      then
         return True;
      end if;

      if not Has_File_Information (Context) then
         return False;
      end if;

      if Get_Log_From_File
        (Get_Kernel (Context),
         File_Information (Context),
         False) /= No_File
      then
         return not Is_A_Log (File_Information (Context));
      end if;

      return False;
   end Filter_Matches_Primitive;

   --------------
   -- Is_A_Log --
   --------------

   function Is_A_Log (File : Virtual_File) return Boolean is
   begin
      return Has_Suffix (File, "$log");
   end Is_A_Log;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Revision_Log_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if not Has_File_Information (Context) then
         return False;
      end if;

      return Is_A_Log (File_Information (Context));
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Project_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return Has_Project_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Revision_Information;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return GPS.Kernel.Contexts.Has_Revision_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_Revision_Information;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return GPS.Kernel.Contexts.Has_Other_Revision_Information (Context);
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Tag_Information;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      return GPS.Kernel.Contexts.Has_Tag_Information (Context);
   end Filter_Matches_Primitive;

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access VCS_Contextual_Menu;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory);
      Creator : constant Abstract_Module := Get_Creator (Context);
   begin
      --  ??? Should be in the filter for the contextual menu
      if (Creator /= Abstract_Module (VCS_Module_ID)
          and then Creator /= Abstract_Module (VCS_Explorer_Module_Id))
        or else Has_Activity_Information (Context)
      then
         VCS_View_API.VCS_Explorer_Contextual_Menu (Context, Menu, False);
      end if;
   end Append_To_Menu;

end VCS_Module.Actions;
