with Gtk.Widget; use Gtk.Widget;
with Gtk.Clist; use Gtk.Clist;
with Diff_Utils; use Diff_Utils;

package Vdiff_Utils is

   procedure Fill_Diff_Lists
     (List1 : access Gtk_Clist_Record'Class;
      List2 : access Gtk_Clist_Record'Class;
      File1 : String;
      File2 : String;
      Diff  : Diff_Occurrence_Link);
   --  Fill List1 and List2 with lines of File given a result of diff Diff.

   procedure Value1_Changed (Object : access Gtk_Widget_Record'Class);
   --  Callback for the value_changed signal on the first
   --  scrolled window of a Vdiff_Access.

   procedure Value2_Changed (Object : access Gtk_Widget_Record'Class);
   --  Callback for the value_changed signal on the second
   --  scrolled window of a Vdiff_Access.

end Vdiff_Utils;
