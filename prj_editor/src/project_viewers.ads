
--  Dependencies on GtkAda
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Clist;

--  Dependencies on GNAT sources
with Prj;
with Types;

package Project_Viewers is

   type Project_Viewer_Record is new Gtk_Notebook_Record with private;
   type Project_Viewer is access all Project_Viewer_Record'Class;

   type View_Type is (System, Version_Control, Switches);
   subtype Filter is Types.Name_Id;
   No_Filter : constant Filter := Types.No_Name;

   procedure Gtk_New (Viewer : out Project_Viewer);
   --  Create a new project viewer.

   procedure Initialize (Viewer : access Project_Viewer_Record'Class);
   --  Internal subprogram for creating widgets

   procedure Show_Project
     (Viewer           : access Project_Viewer_Record;
      Project_View     : Prj.Project_Id;
      Directory_Filter : Filter := No_Filter);
   --  Shows all the direct source files of Project_View (ie not including
   --  imported projects, but including all source directories).
   --  This doesn't clear the list first!
   --  Directory_Filter should be used to limit the search path for the files.
   --  Only the files in one of the directories in Directory_Filter will be
   --  displayed. If Directory_Filter is an empty array, no filter is applied.
   --  Note that if Parent/ is not in the filter, but Parent/Child is, then
   --  all the files from Child will be shown.
   --
   --  Project_View mustn't be No_Project.

   procedure Clear (Viewer : access Project_Viewer_Record);
   --  Removes all files currently displayed in Viewer.

private
   type Boolean_View_Array is array (View_Type) of Boolean;
   type Clist_View_Array is array (View_Type) of Gtk.Clist.Gtk_Clist;

   type Project_Viewer_Record is new Gtk_Notebook_Record with record
      Page_Is_Up_To_Date : Boolean_View_Array := (others => False);
      --  If an entry is False, then the contents of the page needs to be
      --  refreshed upon switching to a new page.

      Pages : Clist_View_Array;
      --  The contents of all the pages in the viewer

      Project_View : Prj.Project_Id := Prj.No_Project;
   end record;
end Project_Viewers;
