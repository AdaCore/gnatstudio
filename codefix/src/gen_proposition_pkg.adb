with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Codefix_Interface; use Callbacks_Codefix_Interface;
with Codefix_Interface_Intl; use Codefix_Interface_Intl;

package body Gen_Proposition_Pkg is

procedure Gtk_New (Gen_Proposition : out Gen_Proposition_Access) is
begin
   Gen_Proposition := new Gen_Proposition_Record;
   Gen_Proposition_Pkg.Initialize (Gen_Proposition);
end Gtk_New;

procedure Initialize (Gen_Proposition : access Gen_Proposition_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Gen_Proposition, Window_Toplevel);
   Set_Title (Gen_Proposition, -"window1");
   Set_Policy (Gen_Proposition, False, True, False);
   Set_Position (Gen_Proposition, Win_Pos_None);
   Set_Modal (Gen_Proposition, False);

   Gtk_New_Vbox (Gen_Proposition.Choices_Box, False, 0);
   Add (Gen_Proposition, Gen_Proposition.Choices_Box);

   Gtk_New (Gen_Proposition.Scrolledwindow4);
   Set_Policy (Gen_Proposition.Scrolledwindow4, Policy_Never, Policy_Always);
   Pack_Start (Gen_Proposition.Choices_Box, Gen_Proposition.Scrolledwindow4, True, True, 0);

   Gtk_New (Gen_Proposition.Old_Text);
   Set_Editable (Gen_Proposition.Old_Text, False);
   Add (Gen_Proposition.Scrolledwindow4, Gen_Proposition.Old_Text);

   Gtk_New (Gen_Proposition.Scrolledwindow5);
   Set_Policy (Gen_Proposition.Scrolledwindow5, Policy_Never, Policy_Always);
   Pack_Start (Gen_Proposition.Choices_Box, Gen_Proposition.Scrolledwindow5, True, True, 0);

   Gtk_New (Gen_Proposition.New_Text);
   Set_Editable (Gen_Proposition.New_Text, False);
   Add (Gen_Proposition.Scrolledwindow5, Gen_Proposition.New_Text);

end Initialize;

end Gen_Proposition_Pkg;
