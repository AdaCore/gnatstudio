-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                use Glib;
with Gdk.Color;           use Gdk.Color;
with Gdk.GC;              use Gdk.GC;
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gdk.Event;           use Gdk.Event;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Browsers.Dependency_Items; use Browsers.Dependency_Items;
with Browsers.Module;           use Browsers.Module;
with Layouts;                   use Layouts;

package body Browsers.Canvas is

   Selected_Link_Color : constant String := "#FF0000";
   --  <preference> Color to use links whose ends are selected.

   Selected_Item_Color : constant String := "#888888";
   --  <preference> Color to use to draw the selected item.

   Linked_Item_Color : constant String := "#BBBBBB";
   --  <preference> Color to use to draw the items that are linked to the
   --  selected item.

   Zoom_Levels : constant array (Positive range <>) of Guint :=
     (25, 50, 75, 100, 150, 200, 300, 400);
   --  All the possible zoom levels. We have to use such an array, instead
   --  of doing the computation directly, so as to avoid rounding errors that
   --  would appear in the computation and make zoom_in not the reverse of
   --  zoom_out.

   Zoom_Steps : constant := 7;
   --  Number of steps while zooming in or out.

   type Cb_Data is record
      Browser : Glide_Browser;
      Zoom    : Guint;
   end record;

   package Contextual_Cb is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Cb_Data);

   procedure Zoom_In (Browser : access Gtk_Widget_Record'Class);
   --  Zoom in to the previous zoom level, if any

   procedure Zoom_Out (Browser : access Gtk_Widget_Record'Class);
   --  Zoom out to the next zoom level, if any

   procedure Zoom_Level
     (Browser : access Gtk_Widget_Record'Class; Data : Cb_Data);
   --  Zoom directly to a specific level (Data.Zoom)

   procedure Zoomed (Browser : access Gtk_Widget_Record'Class);
   --  Called when the Canvas has been zoomed. This redraws all the items

   procedure Realized (Browser : access Gtk_Widget_Record'Class);
   --  Callback for the "realized" signal.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Browser : out Glide_Browser;
      Mask    : Browser_Type_Mask;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Browser := new Glide_Browser_Record;
      Initialize (Browser, Mask, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Browser : out Glide_Browser;
      Mask    : Browser_Type_Mask;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Gtk.Scrolled_Window.Initialize (Browser);
      Set_Policy (Browser, Policy_Automatic, Policy_Automatic);
      Gtk_New (Browser.Canvas);
      Add (Browser, Browser.Canvas);
      Browser.Mask := Mask;
      Browser.Kernel := Kernel_Handle (Kernel);

      Set_Layout_Algorithm (Browser.Canvas, Layer_Layout'Access);
      Set_Auto_Layout (Browser.Canvas, False);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "zoomed",
         Widget_Callback.To_Marshaller (Zoomed'Access), Browser);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "realize",
         Widget_Callback.To_Marshaller (Realized'Access), Browser);
   end Initialize;

   --------------
   -- Realized --
   --------------

   procedure Realized (Browser : access Gtk_Widget_Record'Class) is
      use type Gdk.Gdk_GC;
      B : Glide_Browser := Glide_Browser (Browser);
      Color : Gdk_Color;
   begin
      if B.Selected_Link_GC = null then
         Gdk_New (B.Selected_Link_GC, Get_Window (B.Canvas));
         Color := Parse (Selected_Link_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Selected_Link_GC, Color);

         Gdk_New (B.Selected_Item_GC, Get_Window (B.Canvas));
         Color := Parse (Selected_Item_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Selected_Item_GC, Color);

         Gdk_New (B.Linked_Item_GC, Get_Window (B.Canvas));
         Color := Parse (Linked_Item_Color);
         Alloc (Get_Default_Colormap, Color);
         Set_Foreground (B.Linked_Item_GC, Color);
      end if;
   end Realized;

   --------------
   -- Get_Mask --
   --------------

   function Get_Mask (Browser : access Glide_Browser_Record)
      return Browser_Type_Mask is
   begin
      return Browser.Mask;
   end Get_Mask;

   ----------------
   -- Get_Canvas --
   ----------------

   function Get_Canvas (Browser : access Glide_Browser_Record)
      return Interactive_Canvas is
   begin
      return Browser.Canvas;
   end Get_Canvas;

   -----------------------------
   -- Browser_Context_Factory --
   -----------------------------

   function Browser_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access
   is
      B          : Glide_Browser := Glide_Browser (Object);
      Context    : Browser_Selection_Context_Access :=
        new Browser_Selection_Context;
      Mitem      : Gtk_Menu_Item;
      Check      : Gtk_Check_Menu_Item;
      Zooms_Menu : Gtk_Menu;

   begin
      Unlock (Gtk.Accel_Group.Get_Default);

      Gtk_New (Mitem, Label => "Open file...");
      Append (Menu, Mitem);
      Context_Callback.Object_Connect
        (Mitem, "activate",
         Context_Callback.To_Marshaller (Open_File'Access),
         Slot_Object => B,
         User_Data   => Selection_Context_Access (Context));

      Gtk_New (Check, Label => "Hide system files");
      Set_Active (Check, True);
      Set_Sensitive (Check, False);
      Append (Menu, Check);

      Gtk_New (Check, Label => "Hide implicit dependencies");
      Set_Active (Check, True);
      Set_Sensitive (Check, False);
      Append (Menu, Check);

      Gtk_New (Mitem, Label => "Zoom in");
      Append (Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_In'Access), B);
      Add_Accelerator
        (Mitem, "activate",
         Gtk.Accel_Group.Get_Default, GDK_equal, 0, Accel_Visible);

      Gtk_New (Mitem, Label => "Zoom out");
      Append (Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_Out'Access), B);
      Add_Accelerator
        (Mitem, "activate",
         Gtk.Accel_Group.Get_Default, GDK_minus, 0, Accel_Visible);

      Gtk_New (Zooms_Menu);

      for J in Zoom_Levels'Range loop
         Gtk_New (Mitem, Label => Guint'Image (Zoom_Levels (J)) & '%');
         Append (Zooms_Menu, Mitem);
         Contextual_Cb.Connect
           (Mitem, "activate",
            Contextual_Cb.To_Marshaller (Zoom_Level'Access),
            (Browser => B,
             Zoom    => Zoom_Levels (J)));
      end loop;

      Gtk_New (Mitem, Label => "Zoom");
      Append (Menu, Mitem);
      Set_Submenu (Mitem, Zooms_Menu);

      Lock (Gtk.Accel_Group.Get_Default);

      return Selection_Context_Access (Context);
   end Browser_Context_Factory;

   -------------
   -- Zoom_In --
   -------------

   procedure Zoom_In (Browser : access Gtk_Widget_Record'Class) is
      Canvas : Interactive_Canvas := Glide_Browser (Browser).Canvas;
      Z : constant Guint := Get_Zoom (Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'Last then
               Zoom (Canvas, Zoom_Levels (J + 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_In;

   --------------
   -- Zoom_Out --
   --------------

   procedure Zoom_Out (Browser : access Gtk_Widget_Record'Class) is
      Canvas : Interactive_Canvas := Glide_Browser (Browser).Canvas;
      Z : constant Guint := Get_Zoom (Canvas);
   begin
      for J in Zoom_Levels'Range loop
         if Zoom_Levels (J) = Z then
            if J /= Zoom_Levels'First then
               Zoom (Canvas, Zoom_Levels (J - 1), Zoom_Steps);
            end if;
         end if;
      end loop;
   end Zoom_Out;

   ----------------
   -- Zoom_Level --
   ----------------

   procedure Zoom_Level
     (Browser : access Gtk_Widget_Record'Class; Data : Cb_Data) is
   begin
      Zoom (Data.Browser.Canvas, Data.Zoom, 1);
   end Zoom_Level;

   ------------
   -- Zoomed --
   ------------

   procedure Zoomed (Browser : access Gtk_Widget_Record'Class) is
      B : Glide_Browser := Glide_Browser (Browser);
   begin
      For_Each_Item (B.Canvas, Refresh_File_Item'Access);
   end Zoomed;

   ---------------
   -- To_Brower --
   ---------------

   function To_Brower
     (Canvas : access Gtkada.Canvas.Interactive_Canvas_Record'Class)
      return Glide_Browser is
   begin
      return Glide_Browser (Get_Parent (Canvas));
   end To_Brower;

   -------------------
   -- Selected_Item --
   -------------------

   function Selected_Item (Browser : access Glide_Browser_Record)
      return Gtkada.Canvas.Canvas_Item is
   begin
      return Browser.Selected_Item;
   end Selected_Item;

   -----------------
   -- Select_Item --
   -----------------

   procedure Select_Item
     (Browser : access Glide_Browser_Record;
      Item    : access Gtkada.Canvas.Canvas_Item_Record'Class) is
   begin
      Browser.Selected_Item := Canvas_Item (Item);
   end Select_Item;

   --------------------------
   -- Get_Selected_Link_GC --
   --------------------------

   function Get_Selected_Link_GC (Browser : access Glide_Browser_Record)
      return Gdk.GC.Gdk_GC is
   begin
      return Browser.Selected_Link_GC;
   end Get_Selected_Link_GC;

   --------------------------
   -- Get_Selected_Item_GC --
   --------------------------

   function Get_Selected_Item_GC (Browser : access Glide_Browser_Record)
      return Gdk.GC.Gdk_GC is
   begin
      return Browser.Selected_Item_GC;
   end Get_Selected_Item_GC;

   ------------------------
   -- Get_Linked_Item_GC --
   ------------------------

   function Get_Linked_Item_GC
     (Browser : access Glide_Browser_Record) return Gdk.GC.Gdk_GC is
   begin
      return Browser.Linked_Item_GC;
   end Get_Linked_Item_GC;

end Browsers.Canvas;
