
--  This package provides a tree widget that lists a project and all its
--  dependencies (and the dependencies thereof...)

with Glib;
with Gtk.Ctree;
with Gdk.Pixmap;
with Gdk.Bitmap;

--  GNAT sources
with Types;
with Prj.Tree;

package Project_Trees is

   type Project_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with private;
   type Project_Tree is access all Project_Tree_Record'Class;

   type Name_Id_Array is array (Natural range <>) of Types.Name_Id;

   procedure Gtk_New
     (Tree        : out Project_Tree;
      Columns     : Glib.Gint;
      Tree_Column : Glib.Gint := 0);
   --  Create a new tree

   procedure Load
     (Tree         : access Project_Tree_Record;
      Project      : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id);
   --  Load a new project and its dependencies in the tree.
   --  The project previously loaded is removed from the tree.
   --  Prj_Tree is the project parsed in a tree form, whereas Project should
   --  be the currently evaluated form, depending on the scenario
   --  variables.
   --  if Project is the same that was already loaded, and since the list of
   --  withed projects can not changed, the open/close status of all the
   --  project nodes is kept.
   --  !!WARNING: this assumes that the hierarchy of projects can not change
   --  when the project_view is changed.

   ---------------
   -- Selection --
   ---------------

   function Get_Selected_Project (Tree : access Project_Tree_Record)
      return Prj.Project_Id;
   --  Return the selected project.
   --  In case a directory is selected in the tree, this returns the project
   --  this directory belongs to.

   function Get_Selected_Directories
     (Tree    : access Project_Tree_Record;
      Project : Prj.Project_Id) return Name_Id_Array;
   --  Return the list of selected directories for Project. Note that this
   --  doesn't include any imported projects.
   --  This returns an empty array if no directory are selected.
   --  It is your responsability to provide a correct semantic in your
   --  application if Project wasn't selected.

   -------------
   -- Signals --
   -------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "tree_select_row" and "tree_unselect_row"
   --    These are the standard Gtk_Ctree signals. You should connect to these
   --    to get information when the selection has changed. Use the subprograms
   --    Get_Selected_Projects and Get_Selected_Dirs to get more information
   --    on the current state of the selection

private
   type Project_Tree_Record is new Gtk.Ctree.Gtk_Ctree_Record with record
      Project : Prj.Tree.Project_Node_Id := Prj.Tree.Empty_Node;
      Current_View : Prj.Project_Id;

      Folder_Open_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Open_Mask   : Gdk.Bitmap.Gdk_Bitmap;
      Folder_Pixmap      : Gdk.Pixmap.Gdk_Pixmap;
      Folder_Mask        : Gdk.Bitmap.Gdk_Bitmap;
   end record;
end Project_Trees;
