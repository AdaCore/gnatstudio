
with Gtk.Clist;
with Gtk.GEntry;
with Gtk.Menu;
with Gtk.Style;

with Wizards;
with Directory_Tree;
with Switches_Editors;

package Creation_Wizard is

   type Prj_Wizard_Record is new Wizards.Wizard_Record with private;
   type Prj_Wizard is access all Prj_Wizard_Record'Class;

   procedure Gtk_New (Wiz : out Prj_Wizard);
   --  Create a new project wizard

   procedure Initialize (Wiz : access Prj_Wizard_Record'Class);
   --  Internal function for the creation of a new wizard

private
   type Prj_Wizard_Record is new Wizards.Wizard_Record with record
      Project_Name      : Gtk.GEntry.Gtk_Entry;
      Project_Location  : Gtk.GEntry.Gtk_Entry;
      Src_Dir_Selection : Directory_Tree.Dir_Tree;
      Src_Dir_List      : Gtk.Clist.Gtk_Clist;
      Obj_Dir_Selection : Directory_Tree.Dir_Tree;
      Switches          : Switches_Editors.Switches_Edit;
      Title_Style       : Gtk.Style.Gtk_Style;

      Dir_Contextual_Menu : Gtk.Menu.Gtk_Menu;
      Src_Dir_Contextual_Menu : Gtk.Menu.Gtk_Menu;
   end record;

end Creation_Wizard;
