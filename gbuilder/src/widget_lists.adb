with Glib;              use Glib;
with Glib.Object;       use Glib.Object;
with Gdk.Bitmap;        use Gdk.Bitmap;
with Gdk.Color;         use Gdk.Color;
with Gdk.Event;         use Gdk.Event;
with Gdk.Pixmap;        use Gdk.Pixmap;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Label;         use Gtk.Label;
with Gtk.Notebook;      use Gtk.Notebook;
with Gtk.Pixmap;        use Gtk.Pixmap;
with Gtk.Table;         use Gtk.Table;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;
with Gtk.Tooltips;      use Gtk.Tooltips;
with Gtk.Widget;        use Gtk.Widget;
with Gtk.Window;        use Gtk.Window;
with Gtkada.Handlers;   use Gtkada.Handlers;

with System;            use System;

with Placeholders;     use Placeholders;
with Common;           use Common;

package body Widget_Lists is

   Widgets_Per_Row : constant Guint := 4;

   type List_Page_Record is new Gtk_Table_Record with record
      Last_Row    : Guint := 1;
      Last_Column : Guint := 0;
      --  Coordinates of the last item put in the page
   end record;
   type List_Page is access all List_Page_Record'Class;

   type Widget_Button_Record is new Gtk_Toggle_Button_Record with record
      Typ : GType;
   end record;
   type Widget_Button is access all Widget_Button_Record'Class;

   procedure Gtk_New (Page : out List_Page);
   procedure Initialize (Page : access List_Page_Record'Class);
   --  Create a new page to put in the list

   function Create_Nth_Page (List : access Widget_List_Record; Page : Gint)
      return List_Page;
   --  Create the page-nth page in the list, as well as previous pages if they
   --  do not exist. Nothing is done if the page already exists.
   --  The page is returned.

   function G_New (Typ : GType; Last_Args : System.Address := Null_Address)
      return System.Address;
   pragma Import (C, G_New, "g_object_new");
   --  Create a new object, from its type.

   function Type_Is_A (Typ : GType; Klass : GType) return Boolean;
   pragma Import (C, Type_Is_A, "g_type_is_a");
   --  Return True if Typ belongs to the hierarchy of Klass

   procedure Selected (Button : access Gtk_Widget_Record'Class);

   --------------
   -- Selected --
   --------------

   procedure Selected (Button : access Gtk_Widget_Record'Class) is
      B : Widget_Button := Widget_Button (Button);
      Holder : Placeholder;
   begin
      if Type_Is_A (B.Typ, Gtk.Window.Get_Type) then
         declare
            Stub : Gtk_Window_Record;
            O : Gtk_Window := Gtk_Window (Get_User_Data (G_New (B.Typ), Stub));
         begin
            Gtk_New (Holder);
            Add (O, Holder);

            --  ??? Problem: when we modify the event mask like this, it
            --  reflects in the property_editor...
            Add_Events (O, Button_Press_Mask or Button_Release_Mask);
            Set_Gui_Builder_Window (O);
            Show_All (O);
         end;
      elsif Type_Is_A (B.Typ, Gtk.Widget.Get_Type) then
         declare
            Stub : Gtk_Widget_Record;
            O : Gtk_Widget := Gtk_Widget (Get_User_Data (G_New (B.Typ), Stub));
         begin
            Add_Events (O, Button_Press_Mask or Button_Release_Mask);
            Set_Gui_Builder_Window (O);
            Show_All (O);
         end;
      end if;
   end Selected;

   ----------------
   -- Add_Widget --
   ----------------

   procedure Add_Widget
     (List        : access Widget_List_Record;
      Page        : Gint;
      Name        : String;
      Pixmap      : Interfaces.C.Strings.chars_ptr_array;
      Object_Type : Glib.GType)
   is
      P : List_Page := Create_Nth_Page (List, Page);
      Pix : Gdk_Pixmap;
      Mask : Gdk_Bitmap;
      Pixm : Gtk_Pixmap;
      Button : Widget_Button;
   begin
      if P.Last_Column = Widgets_Per_Row then
         Resize (P, Rows => P.Last_Row + 1, Columns => Widgets_Per_Row);
         P.Last_Row := P.Last_Row + 1;
         P.Last_Column := 1;
      else
         P.Last_Column := P.Last_Column + 1;
      end if;

      Gdk.Pixmap.Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Pixmap);
      Gtk_New (Pixm, Pix, Mask);

      Button := new Widget_Button_Record;
      Gtk.Toggle_Button.Initialize (Button, "");
      Add (Button, Pixm);
      Set_Relief (Button, Relief_None);

      Attach (P, Button,
              P.Last_Column - 1, P.Last_Column,
              P.Last_Row - 1, P.Last_Row,
              Xoptions => 0,
              Yoptions => 0,
              Xpadding => 0, Ypadding => 0);
      Button.Typ := Object_Type;

      Set_Tip (List.Tooltips, Button, Name);

      Widget_Callback.Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Selected'Access));
   end Add_Widget;

   ---------------------
   -- Create_Nth_Page --
   ---------------------

   function Create_Nth_Page
     (List : access Widget_List_Record; Page : Gint)
     return List_Page
   is
      Label  : Gtk_Label;
      List_P : List_Page;
   begin
      loop
         List_P := List_Page (Get_Nth_Page (List, Page));
         exit when List_P /= null;
         Gtk_New (Label, "");
         Gtk_New (List_P);
         Append_Page (List, List_P, Label);
      end loop;
      return List_P;
   end Create_Nth_Page;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Page : out List_Page) is
   begin
      Page := new List_Page_Record;
      Initialize (Page);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (List : out Widget_List) is
   begin
      List := new Widget_List_Record;
      Widget_Lists.Initialize (List);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Page : access List_Page_Record'Class) is
   begin
      Initialize (Page, Rows => 1, Columns => Widgets_Per_Row,
                  Homogeneous => True);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (List : access Widget_List_Record'Class) is
   begin
      Gtk.Notebook.Initialize (List);
      Gtk_New (List.Tooltips);
   end Initialize;

   --------------------
   -- Set_Page_Title --
   --------------------

   procedure Set_Page_Title
     (List  : access Widget_List_Record;
      Page  : Gint;
      Title : String) is
   begin
      Set_Tab_Label_Text (List, Create_Nth_Page (List, Page), Title);
   end Set_Page_Title;

end Widget_Lists;

