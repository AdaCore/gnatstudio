with Glib;
with Qwz_Canvas;
with Gdk.Window;

package Display_Items is

   type Display_Item_Record is new Qwz_Canvas.Canvas_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   procedure Gtk_New (Item : out Display_Item;
                      Win  : in Gdk.Window.Gdk_Window);
   procedure Initialize (Item : access Display_Item_Record'Class;
                         Win  : in Gdk.Window.Gdk_Window);

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Button : Glib.Guint;
                              X, Y   : Glib.Gint);

private
   type Display_Item_Record is new Qwz_Canvas.Canvas_Item_Record with
     null record;

end Display_Items;
