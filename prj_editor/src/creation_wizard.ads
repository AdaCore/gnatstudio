
with Wizards;

package Creation_Wizard is

   type Prj_Wizard_Record is new Wizards.Wizard_Record with private;
   type Prj_Wizard is access all Prj_Wizard_Record'Class;

   procedure Gtk_New (Wiz : out Prj_Wizard);
   --  Create a new project wizard

   procedure Initialize (Wiz : access Prj_Wizard_Record'Class);
   --  Internal function for the creation of a new wizard

private
   type Prj_Wizard_Record is new Wizards.Wizard_Record with record
      null;
   end record;

end Creation_Wizard;
