with Glib;             use Glib;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.GC;           use Gdk.GC;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Gtk.Widget;       use Gtk.Widget;
with Gdk.Color;        use Gdk.Color;
with Gdk.Font;         use Gdk.Font;
with Gtk.Extra.PsFont; use Gtk.Extra.PsFont;

with Ada.Text_IO;      use Ada.Text_IO;

package body Display_Items is

   White_GC : Gdk.GC.Gdk_GC;
   Grey_GC  : Gdk.GC.Gdk_GC;
   Black_GC : Gdk.GC.Gdk_GC;
   Font     : Gdk.Font.Gdk_Font;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item     : out Display_Item;
      Win      : Gdk.Window.Gdk_Window;
      Title    : String := "<No Title>";
      Contents : String := "")
   is
   begin
      Item := new Display_Item_Record;
      Display_Items.Initialize (Item, Win, Title, Contents);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item     : access Display_Item_Record'Class;
      Win      : Gdk.Window.Gdk_Window;
      Title    : String := "<No Title>";
      Contents : String := "")
   is
      use type Gdk.GC.Gdk_GC;
      Alloc_Width  : Gint;
      Alloc_Height : Gint;
      Height : Gint;
      Grey  : Gdk_Color;

   begin
      if White_GC = null then
         Gdk_New (White_GC, Win);
         Set_Foreground (White_GC, White (Get_Default_Colormap));

         Grey := Parse ("grey");
         Alloc (Gtk.Widget.Get_Default_Colormap, Grey);

         Gdk_New (Grey_GC, Win);
         Set_Foreground (Grey_GC, Grey);

         Gdk_New (Black_GC, Win);
         Set_Foreground (Black_GC, Black (Gtk.Widget.Get_Default_Colormap));

         Font := Get_Gdkfont ("Helvetica-Bold", 8);
         if Font = Null_Font then
            null;  --  ??  Should use a default font
         end if;

      end if;

      Alloc_Width :=
        Gint'Max (String_Width (Font, Title),
                  String_Width (Font, Contents)) + 8;
      Height := String_Height (Font, Title) + 4;
      Alloc_Height := Height * 2 + String_Height (Font, Contents);

      Gtkada.Canvas.Initialize (Item, Win, Alloc_Width, Alloc_Height);

      Draw_Rectangle (Pixmap (Item),
                      GC     => White_GC,
                      Filled => True,
                      X      => 0,
                      Y      => Height,
                      Width  => Alloc_Width - 1,
                      Height => Alloc_Height - Height - 1);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Grey_GC,
                      Filled => True,
                      X      => 0,
                      Y      => 0,
                      Width  => Alloc_Width - 1,
                      Height => Height);

      Draw_Rectangle (Pixmap (Item),
                      GC     => Black_GC,
                      Filled => False,
                      X      => 0,
                      Y      => 0,
                      Width  => Alloc_Width - 1,
                      Height => Alloc_Height - 1);

      Draw_Line (Pixmap (Item),
                 GC     => Black_GC,
                 X1     => 0,
                 Y1     => Height,
                 X2     => Alloc_Width - 1,
                 Y2     => Height);

      Draw_Text (Pixmap (Item),
                 Font   => Font,
                 GC     => Black_GC,
                 X      => 4,
                 Y      => Height - 4,
                 Text   => Title);

      if Contents /= "" then
         Draw_Text
           (Pixmap (Item),
            Font   => Font,
            GC     => Black_GC,
            X      => 4,
            Y      => Height + 8,
            Text   => Contents);
      end if;
   end Initialize;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click (Item   : access Display_Item_Record;
                              Button : Glib.Guint;
                              X, Y   : Glib.Gint)
   is
   begin
      Put_Line ("Was clicked on");
   end On_Button_Click;

end Display_Items;

