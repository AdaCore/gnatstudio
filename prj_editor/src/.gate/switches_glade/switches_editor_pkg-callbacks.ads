with Gtk.Arguments;
with Gtk.Widget; use Gtk.Widget;

package Switches_Editor_Pkg.Callbacks is
   procedure Refresh_All_Switches
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure Refresh_Make_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Make_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Comp_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Compiler_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Binder_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Bind_Switches
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Linker_Switches_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure Refresh_Linker_Switches
     (Object : access Gtk_Widget_Record'Class);

end Switches_Editor_Pkg.Callbacks;
