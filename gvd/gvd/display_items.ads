with Glib;
with Gtkada.Canvas;
with Gdk.Window;

package Display_Items is

   type Display_Item_Record is new
     Gtkada.Canvas.Canvas_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   procedure Gtk_New
     (Item     : out Display_Item;
      Win      : Gdk.Window.Gdk_Window;
      Title    : String := "<No Title>";
      Contents : String := "");

   procedure Initialize
     (Item     : access Display_Item_Record'Class;
      Win      : Gdk.Window.Gdk_Window;
      Title    : String := "<No Title>";
      Contents : String := "");

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Button : Glib.Guint;
                              X, Y   : Glib.Gint);

private
   type Display_Item_Record is new Gtkada.Canvas.Canvas_Item_Record with
     null record;

end Display_Items;
