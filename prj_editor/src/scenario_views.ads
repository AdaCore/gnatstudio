
with Gtk.Box;
with Gtk.GEntry;

with Prj.Tree;

package Scenario_Views is

   type Scenario_View_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Scenario_View is access all Scenario_View_Record;

   procedure Gtk_New
     (View : out Scenario_View; Project : Prj.Tree.Project_Node_Id);
   --  Create a new scenario view associated with Project.

   procedure Initialize
     (View : access Scenario_View_Record'Class;
      Project : Prj.Tree.Project_Node_Id);
   --  Internal function for creating new widgets

   procedure Refresh
     (View                 : access Scenario_View_Record'Class;
      Current_Project_View : Prj.Project_Id);
   --  Refresh the list of scenario variables from the associated project,
   --  and recompute the value to display.
   --  Current_Project_View describes the value of all the variables.

private
   type Scenario_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Project : Prj.Tree.Project_Node_Id;
      Field   : Gtk.GEntry.Gtk_Entry;
   end record;

end Scenario_Views;
