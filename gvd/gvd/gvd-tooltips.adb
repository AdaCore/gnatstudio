with Glib;            use Glib;
with Gtk.Widget;      use Gtk.Widget;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gdk.Window;      use Gdk.Window;
with Gtk.Main;        use Gtk.Main;
with Gtk.Handlers;    use Gtk.Handlers;
with Gdk.Types;       use Gdk.Types;
with Gdk.Event;       use Gdk.Event;
with Gdk.Visual;      use Gdk.Visual;
with Gdk.Window_Attr; use Gdk.Window_Attr;
with Unchecked_Deallocation;

package body Odd.Tooltips is

   --------------------
   -- Local packages --
   --------------------

   package Odd_Tooltips_Timeout is new Timeout (Tooltips);

   ---------------------
   -- Local functions --
   ---------------------

   function Display_Tooltip (Tooltip : in Tooltips) return Boolean;
   --  Call the drawing function, then create a window which contains
   --  the pixmap, and display it.

   procedure Set_Tooltip (Tooltip : Tooltips);
   --  Begin timeout countdown.

   procedure Remove_Tooltip (Tooltip : Tooltips);
   --  Cancel timeout countdown.

   procedure Free_User_Type is new Unchecked_Deallocation
     (User_Type, User_Type_Access);
   --  Free memory used by user data.

   procedure Free_Tooltips is new Unchecked_Deallocation
     (Tooltips_Record, Tooltips);
   --  Free memory used by tooltip.

   --------------------
   -- Mouse_Moved_Cb --
   --------------------

   procedure Mouse_Moved_Cb (Widget  : access Widget_Type'Class;
                             Event   : Gdk.Event.Gdk_Event;
                             Tooltip : Tooltips)
   is
   begin
      Set_Tooltip (Tooltip);
   end Mouse_Moved_Cb;

   --------------------
   -- Mouse_Enter_Cb --
   --------------------

   procedure Mouse_Enter_Cb (Widget  : access Widget_Type'Class;
                             Event   : Gdk.Event.Gdk_Event;
                             Tooltip : Tooltips)
   is
   begin
      Set_Tooltip (Tooltip);
   end Mouse_Enter_Cb;

   --------------------
   -- Mouse_Leave_Cb --
   --------------------

   procedure Mouse_Leave_Cb (Widget  : access Widget_Type'Class;
                             Event   : Gdk.Event.Gdk_Event;
                             Tooltip : Tooltips)
   is
   begin
      Remove_Tooltip (Tooltip);
   end Mouse_Leave_Cb;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout (Tooltip : in out Tooltips;
                          T       : in Glib.Guint32) is
   begin
      Tooltip.Timeout := T;
      --  Reset the tooltip
      Set_Tooltip (Tooltip);
   end Set_Timeout;

   -----------------
   -- New_Tooltip --
   -----------------

   procedure New_Tooltip
     (Widget        : access Widget_Type'Class;
      Data          : in User_Type;
      Tooltip       : out Tooltips)
   is
      use type Gdk.Window.Gdk_Window;
   begin
      Add_Events (Widget, Pointer_Motion_Mask or Enter_Notify_Mask);
      Tooltip := new Tooltips_Record'
        (Timeout        => Default_Timeout,
         Data           => new User_Type' (Data),
         Display_Window => Null_Window,
         Parent_Window  => Get_Window (Widget),
         Handler_Id     => 0,
         Active         => False,
         Widget         => Widget_Type_Access (Widget));
      Tooltip_Handler.Connect
        (Widget, "motion_notify_event",
         Tooltip_Handler.To_Marshaller (Mouse_Moved_Cb'Access),
         User_Data => Tooltip);
      Tooltip_Handler.Connect
        (Widget, "enter_notify_event",
         Tooltip_Handler.To_Marshaller (Mouse_Enter_Cb'Access),
         User_Data => Tooltip);
      Tooltip_Handler.Connect
        (Widget, "leave_notify_event",
         Tooltip_Handler.To_Marshaller (Mouse_Leave_Cb'Access),
         User_Data => Tooltip);
   end New_Tooltip;

   ---------------------
   -- Display_Tooltip --
   ---------------------

   function Display_Tooltip (Tooltip : in Tooltips) return Boolean
   is
      use type Gdk_Window;
      Pixmap      : Gdk_Pixmap;
      Visual      : Gdk_Visual;
      Window_Attr : Gdk_Window_Attr;
      Mask        : Gdk_Modifier_Type;
      Window      : Gdk_Window;
      Width, Height : Gint;
      X, Y        : Gint;
   begin
      if not Tooltip.Active then
         return False;
      end if;

      --  To avoid overlapping windows when the user moves the mouse while
      --  a tooltip is being prepared and is about to be displayed.
      if Tooltip.Display_Window /= Null_Window then
         Destroy (Tooltip.Display_Window);
         Tooltip.Display_Window := Null_Window;
      end if;

      Draw_Tooltip (Tooltip.Widget,
                    Tooltip.Data.all,
                    Pixmap,
                    Width,
                    Height);

      if Width /= 0 and then Height /= 0 then

         Get_Best (Visual);

         Gdk_New
           (Window_Attr,
            Height => Height,
            Width => Width,
            Title => "",
            Window_Type => Window_Temp,
            Visual => Visual);

         Gdk.Window.Gdk_New
           (Tooltip.Display_Window,
            Tooltip.all.Parent_Window,
            Window_Attr,
            Wa_Cursor and Wa_Wmclass
            and Wa_Colormap and Wa_Visual);

         Set_Back_Pixmap (Tooltip.Display_Window, Pixmap, 0);
         Get_Pointer (Tooltip.Parent_Window, X, Y, Mask, Window);
         Move (Tooltip.Display_Window, X + 10, Y + 10);
         Gdk.Pixmap.Unref (Pixmap);
         Show (Tooltip.Display_Window);
      end if;
      return False;
   end Display_Tooltip;

   -----------------
   -- Set_Tooltip --
   -----------------

   procedure Set_Tooltip (Tooltip : Tooltips) is
   begin
      if Tooltip.Active = True then
         Remove_Tooltip (Tooltip);
      end if;
      Tooltip.Active := True;
      Tooltip.Handler_Id := Odd_Tooltips_Timeout.Add
        (Tooltip.Timeout,
         Display_Tooltip'Access,
         Tooltip);
   end Set_Tooltip;

   --------------------
   -- Remove_Tooltip --
   --------------------

   procedure Remove_Tooltip (Tooltip : Tooltips) is
      use type Gdk_Window;
   begin
      if Tooltip.Active = True then
         Timeout_Remove (Tooltip.Handler_Id);
         if Tooltip.Display_Window /= Null_Window then
            Destroy (Tooltip.Display_Window);
            Tooltip.Display_Window := Null_Window;
         end if;
         Tooltip.Active := False;
      end if;
   end Remove_Tooltip;

   ---------------------
   -- Destroy_Tooltip --
   ---------------------

   procedure Destroy_Tooltip (Tooltip : in out Tooltips) is
   begin
      Remove_Tooltip (Tooltip);
      Free_User_Type (Tooltip.Data);
      Free_Tooltips (Tooltip);
   end Destroy_Tooltip;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Tooltip : in out Tooltips; Data : in User_Type) is
   begin
      Free_User_Type (Tooltip.Data);
      Tooltip.Data := new User_Type' (Data);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Tooltip : in Tooltips) return User_Type is
   begin
      return Tooltip.Data.all;
   end Get_Data;

end Odd.Tooltips;
