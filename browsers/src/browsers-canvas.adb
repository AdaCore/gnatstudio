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
with Gtkada.Canvas;       use Gtkada.Canvas;
with Gtkada.Handlers;     use Gtkada.Handlers;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gdk.Event;           use Gdk.Event;
with Gdk.Types.Keysyms;   use Gdk.Types.Keysyms;
with Gtk.Accel_Group;     use Gtk.Accel_Group;
with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Enums;           use Gtk.Enums;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Widget;          use Gtk.Widget;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Browsers;     use Glide_Kernel.Browsers;
with GUI_Utils;                 use GUI_Utils;
with Browsers.Dependency_Items; use Browsers.Dependency_Items;

package body Browsers.Canvas is

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

   procedure Open_File (Browser : access Gtk_Widget_Record'Class);
   --  Open a new file for analyzis in the browser

   function Contextual_Background_Menu
     (Browser : access Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Gtk_Menu;
   --  Return the contextual menu to use in the browser


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
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu : Gtk_Menu;
   begin
      Gtk.Scrolled_Window.Initialize (Browser);
      Set_Policy (Browser, Policy_Automatic, Policy_Automatic);
      Gtk_New (Browser.Canvas);
      Add (Browser, Browser.Canvas);
      Browser.Mask := Mask;
      Browser.Kernel := Kernel_Handle (Kernel);

      --  Create the  background contextual menu now, so that the key shortcuts
      --  are activated
      Menu := Contextual_Background_Menu (Browser, null);

      Widget_Callback.Object_Connect
        (Browser.Canvas, "zoomed",
         Widget_Callback.To_Marshaller (Zoomed'Access), Browser);

      Register_Contextual_Menu (Browser, Contextual_Background_Menu'Access);
   end Initialize;

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

   --------------------------------
   -- Contextual_Background_Menu --
   --------------------------------

   function Contextual_Background_Menu
     (Browser : access Gtk_Widget_Record'Class;
      Event   : Gdk.Event.Gdk_Event) return Gtk_Menu
   is
      B          : Glide_Browser := Glide_Browser (Browser);
      Mitem      : Gtk_Menu_Item;
      Zooms_Menu : Gtk_Menu;
      Check      : Gtk_Check_Menu_Item;
   begin
      if B.Contextual_Background_Menu /= null then
         return B.Contextual_Background_Menu;
      end if;

      Unlock (Gtk.Accel_Group.Get_Default);

      Gtk_New (B.Contextual_Background_Menu);

      Gtk_New (Mitem, Label => "Open file...");
      Append (B.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Open_File'Access), B);

      Gtk_New (Mitem);
      Append (B.Contextual_Background_Menu, Mitem);

      Gtk_New (Check, Label => "Hide system files");
      Set_Active (Check, True);
      Set_Sensitive (Check, False);
      Append (B.Contextual_Background_Menu, Check);

      Gtk_New (Check, Label => "Hide implicit dependencies");
      Set_Active (Check, True);
      Set_Sensitive (Check, False);
      Append (B.Contextual_Background_Menu, Check);

      Gtk_New (Mitem);
      Append (B.Contextual_Background_Menu, Mitem);

      Gtk_New (Mitem, Label => "Zoom in");
      Append (B.Contextual_Background_Menu, Mitem);
      Widget_Callback.Object_Connect
        (Mitem, "activate",
         Widget_Callback.To_Marshaller (Zoom_In'Access), B);
      Add_Accelerator
        (Mitem, "activate",
         Gtk.Accel_Group.Get_Default, GDK_equal, 0, Accel_Visible);

      Gtk_New (Mitem, Label => "Zoom out");
      Append (B.Contextual_Background_Menu, Mitem);
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
      Append (B.Contextual_Background_Menu, Mitem);
      Set_Submenu (Mitem, Zooms_Menu);

      Show_All (B.Contextual_Background_Menu);

      Lock (Gtk.Accel_Group.Get_Default);
      return B.Contextual_Background_Menu;
   end Contextual_Background_Menu;

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
   -- Open_File --
   ---------------

   procedure Open_File (Browser : access Gtk_Widget_Record'Class) is
      B : Glide_Browser := Glide_Browser (Browser);

      File : constant String := Select_File (Base_Directory => "");
      --  ??? Should set up filters to only open file from the current project.
   begin
      if File /= "" then
         Examine_Dependencies (B.Kernel, B, File);
      end if;
   end Open_File;

end Browsers.Canvas;
