with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Viewport; use Gtk.Viewport;
with Gtk.List; use Gtk.List;
with Gtk.Label; use Gtk.Label;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Button; use Gtk.Button;

package List_Select_Pkg is

   type List_Select_Record is new Gtk_Window_Record with record
      Vbox           : Gtk_Vbox;
      Scrolledwindow : Gtk_Scrolled_Window;
      Viewport       : Gtk_Viewport;
      List           : Gtk_List;
      Hbox           : Gtk_Hbox;
      Label          : Gtk_Label;
      The_Entry      : Gtk_Entry;
      Hbuttonbox     : Gtk_Hbutton_Box;
      Ok             : Gtk_Button;
      Cancel         : Gtk_Button;
      Help           : Gtk_Button;
   end record;
   type List_Select_Access is access all List_Select_Record'Class;

   procedure Gtk_New (List_Select : out List_Select_Access);
   procedure Initialize (List_Select : access List_Select_Record'Class);

   procedure Change_Title
     (List_Select : List_Select_Access;
      Title       : String);
   --  Change the title of the window.

   procedure Add_Item
     (List_Select : List_Select_Access;
      Label       : String);
   --  Add an item to the list with the specified label.

   procedure Remove_All_Items (List_Select : List_Select_Access);
   --  Removes all the items in the list.

   function Get_Item_In_Entry
     (List_Select : List_Select_Access) return String;
   --  Return the label in the entry.

   procedure Set_Label
     (List_Select : List_Select_Access;
      Label       : String);
   --  Set the entry label.

end List_Select_Pkg;
