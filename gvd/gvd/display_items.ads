with Glib;
with Gtkada.Canvas;
with Gdk.Window;
with Generic_Values;

package Display_Items is

   type Display_Item_Record is new
     Gtkada.Canvas.Canvas_Item_Record with private;
   type Display_Item is access all Display_Item_Record'Class;

   procedure Gtk_New
     (Item          : out Display_Item;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Entity        : Generic_Values.Generic_Type_Access;
      Auto_Refresh  : Boolean := True);
   --  No copy of entity is made, we simply keep the pointer as is.
   --  Auto_Refresh should be set to True if the value of Entity should
   --  be parsed again whenever the debugger stops. This is the default
   --  behavior, that can be changed by the user.

   procedure Initialize
     (Item          : access Display_Item_Record'Class;
      Win           : Gdk.Window.Gdk_Window;
      Variable_Name : String;
      Entity        : Generic_Values.Generic_Type_Access;
      Auto_Refresh  : Boolean := True);

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Button : Glib.Guint;
                              X, Y   : Glib.Gint);

private
   type Display_Item_Record is new Gtkada.Canvas.Canvas_Item_Record with
      record
         Name   : Generic_Values.String_Access := null;
         Entity : Generic_Values.Generic_Type_Access := null;
         Auto_Refresh : Boolean := True;
      end record;

end Display_Items;
