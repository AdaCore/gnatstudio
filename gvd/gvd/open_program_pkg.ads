with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Table; use Gtk.Table;
with Gtk.Radio_Button; use Gtk.Radio_Button;
with Gtk.Combo; use Gtk.Combo;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Button; use Gtk.Button;
with Gtk.Label; use Gtk.Label;
with Gtk.Option_Menu; use Gtk.Option_Menu;
with Gtk.Menu; use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Object; use Gtk.Object;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Debugger; use Debugger;

package Open_Program_Pkg is

   type Open_Program_Record is new Gtk_Window_Record with record
      Vbox13 : Gtk_Vbox;
      Frame8 : Gtk_Frame;
      Table7 : Gtk_Table;
      Gdb_Button : Gtk_Radio_Button;
      Dbx_Button : Gtk_Radio_Button;
      Xdb_Button : Gtk_Radio_Button;
      Jdb_Button : Gtk_Radio_Button;
      Pydb_Button : Gtk_Radio_Button;
      Perl_Button : Gtk_Radio_Button;
      Program_Combo : Gtk_Combo;
      Program_Entry : Gtk_Entry;
      Open_Button : Gtk_Button;
      Host_Combo : Gtk_Combo;
      Host_Entry : Gtk_Entry;
      Label57 : Gtk_Label;
      Label55 : Gtk_Label;
      Label56 : Gtk_Label;
      Label58 : Gtk_Label;
      Launch_Menu : Gtk_Option_Menu;
      Label59 : Gtk_Label;
      Combo8 : Gtk_Combo;
      Protocol_Entry : Gtk_Entry;
      Label60 : Gtk_Label;
      Combo7 : Gtk_Combo;
      Target_Entry : Gtk_Entry;
      Hbuttonbox7 : Gtk_Hbutton_Box;
      Ok_Button : Gtk_Button;
      Cancel_Button : Gtk_Button;
      Help_Button : Gtk_Button;
      Valid : Boolean;
   end record;
   type Open_Program_Access is access all Open_Program_Record'Class;

   type Launch_Method is (None, Current_Debugger, New_Debugger);

   type Program_Descriptor is record
      Program       : String_Access;
      Debugger      : Debugger_Type;
      Remote_Host   : String_Access;
      Remote_Target : String_Access;
      Protocol      : String_Access;
      Launch        : Launch_Method;
   end record;

   procedure Open_Program
     (Open       : in out Open_Program_Access;
      Descriptor : out Program_Descriptor);
   --  Open a program window and launch a main loop until the ok or cancel
   --  button has been pressed.
   --  Open if null is set to the created window, that is hidden on return.
   --  If non null, Open_Program will show it instead of creating a new one.
   --  Return the program descriptor. If Launch is None,
   --  this means a cancellation from the user.
   --  Note that this is your responsibility to free the memory associated with
   --  Descriptor, using Free below.

   procedure Free (Descriptor : in out Program_Descriptor);
   --  Free the dynamic memory associated with program.

private

   procedure Gtk_New (Open_Program : out Open_Program_Access);
   procedure Initialize (Open_Program : access Open_Program_Record'Class);

end Open_Program_Pkg;
