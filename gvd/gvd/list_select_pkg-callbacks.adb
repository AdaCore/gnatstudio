with Gtk.Label; use Gtk.Label;
with Gtk.List; use Gtk.List;
with Gtk.Container; use Gtk.Container;
with Gtk.Object; use Gtk.Object;
with Gtk.Widget; use Gtk.Widget;

package body List_Select_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------
   -- On_List_Select_Child --
   --------------------------

   procedure On_List_Select_Child
     (Object : access Gtk_List_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Open : List_Select_Access :=
        List_Select_Access (Get_Toplevel (Object));
      Arg1 : Gtk_Widget := Gtk_Widget (To_Object (Params, 1));

      use Widget_List;

      --  ??? This will not work without Gtk.Type_Conversion
      Text : String :=
        Get (Gtk_Label (Get_Data (Children (Gtk_Container (Arg1)))));

   begin
      Set_Text (Open.The_Entry, Text);
   end On_List_Select_Child;

   --------------------------------
   -- On_List_Button_Press_Event --
   --------------------------------

   function On_List_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      --  Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_List_Button_Press_Event;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked (Object : access Gtk_Button_Record'Class) is
   begin
      null;
   end On_Cancel_Clicked;

   ---------------------
   -- On_Help_Clicked --
   ---------------------

   procedure On_Help_Clicked (Object : access Gtk_Button_Record'Class) is
   begin
      null;
   end On_Help_Clicked;

end List_Select_Pkg.Callbacks;
