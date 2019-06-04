------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNAT.Strings;       use GNAT.Strings;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with Gtk.Notebook;       use Gtk.Notebook;
with Gtk.Widget;         use Gtk.Widget;
with GPS.Kernel;         use GPS.Kernel;
with Dialog_Utils;       use Dialog_Utils;

package Project_Viewers is

   Action_Add_Scenario_Variable : constant String := "Add scenario variable";

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   --------------------------
   -- Project editor pages --
   --------------------------

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

   type Project_Editor_Page_Record
     (Flags : Selector_Flags) is abstract new Dialog_View_Record with
     null record;
   type Project_Editor_Page is access all Project_Editor_Page_Record'Class;
   --  A widget used to edit some properties of a project.
   --  This is used in the project properties editor

   procedure Destroy (Page : in out Project_Editor_Page_Record) is null;
   --  Free the memory allocated for the page. Inherited subprograms should
   --  always call the parent's Destroy.

   procedure Initialize
     (Self         : not null access Project_Editor_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project) is abstract;
   --  Initialize the widget based on the project's settings.
   --  Project might be set to No_Project when this function is called from a
   --  project creation wizard.
   --  Full_Project is the directory/name  the user has chosen for the project
   --  file. It should be used for the pages that need initial values for the
   --  directories.

   function Is_Visible
     (Self            : not null access Project_Editor_Page_Record;
      Dummy_Languages : GNAT.Strings.String_List) return Boolean is (True);
   --  Called to check whether a page should be visible, given the current
   --  list of languages.
   --  This function should also take care of hidding nested pages, when they
   --  exist, depending on the result of their own Is_Visible primitive.

   function Is_Valid
     (Self         : not null access Project_Editor_Page_Record)
      return String is ("");
   --  Whether the current contents of the page is valid.
   --  Should return the empty string if all is valid, or an error message
   --  otherwise.
   --  Invalid fields should be highlighted in red.

   function Edit_Project
     (Self               : not null access Project_Editor_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean is abstract;
   --  Modifies Project given the data in Self.
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

   ------------------------
   -- Checking languages --
   ------------------------

   function In_List
     (Lang : String; List : GNAT.Strings.String_List) return Boolean;
   --  Whether the language is in the list

   ---------------------------
   -- Notebook editor pages --
   ---------------------------
   --  This type can be used to group multiple project editor pages into a
   --  single one, where each of the page is displayed in its own notebook tab

   type Project_Editor_Multi_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios) with private;

   overriding procedure Destroy
     (Self : in out Project_Editor_Multi_Page_Record);
   overriding procedure Initialize
     (Self         : not null access Project_Editor_Multi_Page_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Project_Editor_Multi_Page_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding function Is_Visible
     (Self         : not null access Project_Editor_Multi_Page_Record;
      Languages    : GNAT.Strings.String_List) return Boolean;

   procedure Add_Page
     (Self  : not null access Project_Editor_Multi_Page_Record;
      Page  : not null access Project_Editor_Page_Record'Class;
      Title : Unbounded_String);
   --  Add a new page

   type Page_Iterator_Callback is not null access procedure
     (Page : not null access Project_Editor_Page_Record'Class);
   procedure For_Each_Page
     (Self     : not null access Project_Editor_Multi_Page_Record;
      Callback : Page_Iterator_Callback);
   --  Executes Callback for each registered page

   ---------------------------
   -- Naming scheme editors --
   ---------------------------

   type Naming_Scheme_Editor_Creator is access function
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String) return not null Project_Editor_Page;

   procedure Register_Naming_Scheme_Editor
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String;
      Creator  : Naming_Scheme_Editor_Creator);
   --  Register a new page used to edit the naming scheme for a specific
   --  language.
   --  As is done for the switches pages, this is created lazily when the page
   --  is actually needed

   function Get_Naming_Scheme_Page
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Language : String) return Project_Editor_Page;
   --  Return the naming scheme editor page for the specific language. This is
   --  a newly created widget.
   --  null is returned if there are no such page.

   function Get_All_Naming_Scheme_Page
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Project_Editor_Page;
   --  A page to edit the naming schemes for all languages

private
   type Page_Descr is record
      Title : Unbounded_String;
      Page  : Project_Editor_Page;
   end record;

   package Project_Editor_Page_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Page_Descr);

   type Project_Editor_Multi_Page_Record is new Project_Editor_Page_Record
     (Flags => Multiple_Projects or Multiple_Scenarios) with
   record
      Notebook : Gtk_Notebook;
      Pages    : Project_Editor_Page_Lists.List;
   end record;

end Project_Viewers;
