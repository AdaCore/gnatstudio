with Gtk.Window; use Gtk.Window;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text; use Gtk.Text;
with Gtk.Button; use Gtk.Button;

with Gtk.Tree_View; use Gtk.Tree_View;
with Gtk.Tree_Store; use Gtk.Tree_Store;

with GNAT.OS_Lib;     use GNAT.OS_Lib;

with VCS; use VCS;

package VCS_View_Pkg is

   type VCS_View_Record;
   type VCS_View_Access is access all VCS_View_Record'Class;

   type VCS_View_Record is new Gtk_Window_Record with record
      Current_Directory : String_Access;
      --  The directory that is currently being viewed.
      --  It must be an absolute directory name ending
      --  with Directory_Separator.

      Tree  : Gtk_Tree_View;
      Model : Gtk_Tree_Store;

      Model_Sync : Boolean := False;
      --  This boolean indicates whether the model is currently
      --  being synchronized with the view.

      All_Selected : Boolean := False;
      --  Indicates whether all the files in the view are selected.

      VCS_Ref : VCS_Access := null;

      Edit_Log_Button            : Gtk_Widget;
      Edit_Multiple_Log_Button   : Gtk_Widget;
      View_Log_Button            : Gtk_Widget;
      View_Diff_Button           : Gtk_Widget;
      Annotate_Button            : Gtk_Widget;
      Get_Status_Button          : Gtk_Widget;
      Update_Button              : Gtk_Widget;
      Open_Button                : Gtk_Widget;
      Commit_Button              : Gtk_Widget;
      Revert_Button              : Gtk_Widget;
      Add_Button                 : Gtk_Widget;
      Remove_Button              : Gtk_Widget;

      Message_Text               : Gtk_Text;
   end record;

   procedure Gtk_New (VCS_View : out VCS_View_Access);
   procedure Initialize (VCS_View : access VCS_View_Record'Class);

   procedure Show_Files (Explorer  : VCS_View_Access;
                         Directory : String);
   --

end VCS_View_Pkg;
