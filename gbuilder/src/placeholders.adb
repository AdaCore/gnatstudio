with Glib;                 use Glib;
with Gdk.Color;            use Gdk.Color;
with Gdk.Event;            use Gdk.Event;
with Gdk.Pixmap;           use Gdk.Pixmap;
with Gdk.Window;           use Gdk.Window;
with Gtk.Drawing_Area;     use Gtk.Drawing_Area;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Object;           use Gtk.Object;
with Gtk.Style;            use Gtk.Style;
with Gtk.Widget;           use Gtk.Widget;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.Types;

with Common;               use Common;

package body Placeholders is

   Placeholder_Bg_Color : constant String := "#888888";
   --  <preference> The background color for placeholders.

   Placeholder_Xpm     : aliased chars_ptr_array (0 .. 0);
   pragma Import (C, Placeholder_Xpm, "placeholder_xpm");

   Placeholder_Quark : GQuark := Unknown_Quark;

   procedure Realized (Holder : access Gtk_Widget_Record'Class);
   --  Callback for the "realize" signal

   function Expose (Holder : access Gtk_Widget_Record'Class)
      return Boolean;
   --  Callback for the "expose_event" signal

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Holder : out Placeholder) is
   begin
      Holder := new Placeholder_Record;
      Placeholders.Initialize (Holder);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Holder : access Placeholder_Record'Class) is
   begin
      Gtk.Drawing_Area.Initialize (Holder);

      Set_Placeholder (Holder);

      Add_Events (Holder, Button_Press_Mask or Button_Release_Mask);
      Set_Name (Holder, "placeholder");

      Widget_Callback.Connect
        (Holder, "realize",
         Widget_Callback.To_Marshaller (Realized'Access));
      Return_Callback.Connect
        (Holder, "expose_event",
         Return_Callback.To_Marshaller (Expose'Access));
   end Initialize;

   --------------------
   -- Is_Placeholder --
   --------------------

   function Is_Placeholder
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Boolean
   is
   begin
      if Placeholder_Quark = Unknown_Quark then
         Placeholder_Quark := Quark_From_String ("Gbuilder_Placeholder");
      end if;

      return Boolean_User_Data.Get (Object, Placeholder_Quark);
   exception
      when Gtkada.Types.Data_Error =>
         return False;
   end Is_Placeholder;

   ---------------------
   -- Set_Placeholder --
   ---------------------

   procedure Set_Placeholder
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
   is
   begin
      if Placeholder_Quark = Unknown_Quark then
         Placeholder_Quark := Quark_From_String ("GBuilder_Placeholder");
      end if;

      Boolean_User_Data.Set (Object, True, Placeholder_Quark);
   end Set_Placeholder;

   --------------
   -- Realized --
   --------------

   procedure Realized (Holder : access Gtk_Widget_Record'Class) is
      Pix   : Gdk_Pixmap;
      Mask  : Gdk.Gdk_Bitmap;
   begin
      Create_From_Xpm_D
        (Pix, Get_Window (Holder), Mask, Null_Color, Placeholder_Xpm);
      Set_Back_Pixmap (Get_Window (Holder), Pix, False);
   end Realized;

   ------------
   -- Expose --
   ------------

   function Expose (Holder : access Gtk_Widget_Record'Class)
      return Boolean is
   begin
      Draw_Shadow
        (Get_Style (Holder),
         Get_Window (Holder),
         State_Normal,
         Shadow_Out,
         0, 0, -1, -1);
      return False;
   end Expose;

end Placeholders;

