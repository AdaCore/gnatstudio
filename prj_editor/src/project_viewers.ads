-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with GNAT.OS_Lib;
with Glide_Kernel;
with Gtk.Widget;
with Histories;
with Projects;
with Switches_Editors;
with Naming_Editors;

package Project_Viewers is

   Project_History_Key : constant Histories.History_Key := "project_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   --------------------------
   -- Project editor pages --
   --------------------------

   type Project_Editor_Page_Record is abstract tagged private;
   type Project_Editor_Page is access all Project_Editor_Page_Record'Class;
   --  A page that should be inserted in the project creation wizard and the
   --  project properties editor.

   procedure Destroy (Page : in out Project_Editor_Page_Record);
   --  Free the memory allocated for the page. Inherited subprograms should
   --  always call the parent's Destroy.

   function Widget_Factory
     (Page         : access Project_Editor_Page_Record;
      Project      : Projects.Project_Type;
      Full_Project : String;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return a new widget to display in the project properties editor or the
   --  project creation wizard. This can be used to store extra information
   --  closely associated with each projects (either in the project file itself
   --  or in some external files).
   --  This function should expect Project_View to be No_Project in some cases,
   --  when called from the project wizard.
   --  This subprogram should call Show_All on the returned widget. This allows
   --  it to hide some of the components when necessary. The caller should not
   --  force a Show_All on the widget.
   --  Full_Project is the directory/name  the user has chosen for the project
   --  file. It should be used for the pages that need initial values for the
   --  directories.
   --
   --  Refresh is always called just after Widget_Factory.

   function Project_Editor
     (Page          : access Project_Editor_Page_Record;
      Project       : Projects.Project_Type;
      Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      Widget        : access Gtk.Widget.Gtk_Widget_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Ref_Project   : Projects.Project_Type) return Boolean is abstract;
   --  Modifies Project given the data in Widget. Widget is the same that was
   --  created through a Project_Editor_Page_Factor.
   --
   --  Return True if at least one project was modified.
   --
   --  This subprogram should not recompute the project view itself,
   --  since this is already done once after all the modifications have been
   --  done.
   --  This function should expect Project to be No_Project in some cases.
   --
   --  Ref_Project is the project whose properties the user decided to edit
   --  initially (through the contextual menu). In some cases, an editor might
   --  decide that is cannot modify projects other than this one (for instance,
   --  the object directory editor only modifies ref_project). This function
   --  will not be called with Project /= Ref_Project if the flags do not
   --  include Multiple_Projects in Register_Project_Editor_Page.
   --
   --  This function might be called several times with the same project, but a
   --  different scenario if the user has decided to modify several
   --  scenarios.

   procedure Refresh
     (Page      : access Project_Editor_Page_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Project   : Projects.Project_Type := Projects.No_Project;
      Languages : GNAT.OS_Lib.Argument_List);
   --  Refresh the contents of Widget, that was created by Widget_Factory.
   --  Since Project_View is still the one when the project creation wizard or
   --  the project properties editor were initially displayed, the list of
   --  supported languages should be read from languages.
   --  By default, it does nothing.

   function Get_Label (Page : access Project_Editor_Page_Record'Class)
      return String;
   --  Return the label that should be used to identify the page in the project
   --  properties editor.

   function Get_Toc (Page : access Project_Editor_Page_Record'Class)
      return String;
   --  Return the table-of-contents label to be used in the project creation
   --  wizard.

   function Get_Title (Page : access Project_Editor_Page_Record'Class)
      return String;
   --  Return the title that should be used for this page in the project
   --  creation wizard.

   type Selector_Flags is mod 4;
   Multiple_Projects  : constant Selector_Flags := 2 ** 0;
   Multiple_Scenarios : constant Selector_Flags := 2 ** 1;
   --  The projects or scenarios the project editor applies to.
   --  Multiple_Project should be set if multiple projects can be modified by
   --  the editor.
   --  Multiple_Scenarios should be set if multiple scenarios can be modified
   --  at the same time by the editor.
   --  This flags is used to desactivate the selector widgets in the project
   --  properties dialog.

   function Get_Flags (Page : access Project_Editor_Page_Record'Class)
      return Selector_Flags;
   --  Return the list of selectors recognized by this editor

   -----------------------------------------------
   -- Registering new pages to edit the project --
   -----------------------------------------------

   procedure Register_Project_Editor_Page
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Page      : Project_Editor_Page;
      Label     : String;
      Toc       : String;
      Title     : String;
      Flags     : Selector_Flags := Multiple_Projects or Multiple_Scenarios;
      Ref_Page  : String := "";
      Add_After : Boolean := True);
   --  Register a page that should be displayed both in the project wizard and
   --  the project properties editor.
   --  The new page will be put after or before the page whose label is
   --  Ref_Page, or after all the pages if Ref_Page is the empty string.

   function Project_Editor_Pages_Count
      (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) return Natural;
   --  Return the number of registered project editor pages

   function Get_Nth_Project_Editor_Page
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class; Num : Natural)
      return Project_Editor_Page;
   --  Return the Num-th registered project editor page.
   --  First page is number 1.

   ------------------------------------
   -- Registering new switches pages --
   ------------------------------------
   --  See switches_editors.ads

   type Switches_Page_Creator_Record is abstract tagged null record;
   type Switches_Page_Creator is access all Switches_Page_Creator_Record'Class;

   function Create
     (Creator : access Switches_Page_Creator_Record;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Switches_Editors.Switches_Editor_Page is abstract;
   --  Create a new switches editor page.

   procedure Destroy (Creator : in out Switches_Page_Creator_Record);
   procedure Destroy (Creator : in out Switches_Page_Creator);
   --  Free the memory occupied by the creator object. The default is to do
   --  nothing. This is called when the page has been created the first time,
   --  since the page is then cached.

   procedure Register_Switches_Page
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Creator : access Switches_Page_Creator_Record'Class);
   --  Register a new switches page creator. This will be called to create
   --  the page initially when the project properties or project wizard are
   --  displayed, and the resulting switches page is destroyed when the project
   --  properties dialog is closed.
   --
   --  The reason to register a creator instead of the page itself is to save
   --  startup time, and create widgets lazily.

   function Get_Nth_Switches_Page
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Num    : Positive)
      return Switches_Editors.Switches_Editor_Page;
   --  Return the Num-th switches editor page, or null if there are no more
   --  such pages.
   --  The first page is number 1.
   --  The returned widget must be Unref-ed by the caller.

   function Switches_Page_Count
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) return Natural;
   --  Return the number of switches pages registered

   --------------------------------------------
   -- Registering new naming schemes editors --
   --------------------------------------------
   --  See naming_editors.ads

   type Naming_Scheme_Editor_Creator is access function
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Language : String)
     return Naming_Editors.Language_Naming_Editor;

   procedure Register_Naming_Scheme_Editor
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Language : String;
      Creator  : Naming_Scheme_Editor_Creator);
   --  Register a new page used to edit the naming scheme for a specific
   --  language.
   --  As is done for the switches pages, this is created lazily when the page
   --  is actually needed

   function Get_Naming_Scheme_Page
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Language : String) return Naming_Editors.Language_Naming_Editor;
   --  Return the naming scheme editor page for the specific language.
   --  null is returned if there are no such page.

private

   type Project_Editor_Page_Record is abstract tagged record
      Label, Toc, Title : GNAT.OS_Lib.String_Access;
      Flags             : Selector_Flags;
   end record;

end Project_Viewers;
