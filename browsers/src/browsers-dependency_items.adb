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

with Glib;              use Glib;
with Gdk.Color;         use Gdk.Color;
with Gdk.Drawable;      use Gdk.Drawable;
with Gdk.Font;          use Gdk.Font;
with Gdk.GC;            use Gdk.GC;
with Gdk.Window;        use Gdk.Window;
with Gtkada.Canvas;     use Gtkada.Canvas;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Style;         use Gtk.Style;
with Gtk.Widget;        use Gtk.Widget;
with Gtkada.MDI;        use Gtkada.MDI;

with Src_Info;                 use Src_Info;
with Src_Info.Queries;         use Src_Info.Queries;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Browsers;    use Glide_Kernel.Browsers;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel;             use Glide_Kernel;

with GNAT.OS_Lib;              use GNAT.OS_Lib;

package body Browsers.Dependency_Items is

   procedure Update_Display
     (Item : access Dependency_Item_Record'Class; Win : Gdk_Window);
   --  Update the display of the item

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item : out Dependency_Item;
      Win  : Gdk_Window;
      Kernel : access Kernel_Handle_Record'Class;
      Dep  : Dependency)
   is
   begin
      Item := new Dependency_Item_Record;
      Initialize (Item, Win, Kernel, Dep);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item : access Dependency_Item_Record'Class;
      Win  : Gdk_Window;
      Kernel : access Kernel_Handle_Record'Class;
      Dep  : Dependency)
   is
      use type Gdk_Window;
   begin
      pragma Assert (Win /= null);
      Item.Dep := Dep;
      Item.Kernel := Kernel_Handle (Kernel);
      Update_Display (Item, Win);
   end Initialize;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (Item : access Dependency_Item_Record'Class; Win : Gdk_Window)
   is
      Margin : constant := 2;

      Str : constant String := Get_Filename (Item.Dep);
      Str2 : String_Access;
      Font : Gdk_Font := Get_Pref (Item.Kernel, Browsers_Link_Font);
      GC   : Gdk_GC;
      Width, Height : Gint;

      Browser : MDI_Child := Open_Browser (Item.Kernel, Dependency_Browser);
      List : LI_File_List;

   begin
      Gdk_New (GC, Win);
      Set_Foreground (GC, White (Get_Default_Colormap));

      List := Get_Source_Info_List (Item.Kernel);
      Get_Unit_Name
        (List,
         Get_Project_View (Item.Kernel),
         Get_Source_Path (Item.Kernel),
         Get_Object_Path (Item.Kernel),
         Item.Dep,
         Str2);

      Width  := String_Width (Font, Str);

      if Str2 /= null then
         Width := Gint'Max (Width, String_Width (Font, Str2.all));
      else
         Width := Gint'Max
           (Width, String_Width (Font, String' ("<unknown unit name>")));
      end if;

      Width := Width + 4 * Margin;
      Height := (Get_Ascent (Font) + Get_Descent (Font)) * 2 + 4 * Margin;
      Set_Screen_Size_And_Pixmap (Item, Win, Width, Height);

      Draw_Rectangle
        (Pixmap (Item),
         GC     => GC,
         Filled => True,
         X      => 0,
         Y      => 0,
         Width  => Width,
         Height => Height);

      Draw_Shadow
        (Style       => Get_Style (Get_Widget (Browser)),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Etched_Out,
         X           => 0,
         Y           => 0,
         Width       => Width,
         Height      => Height);

      Set_Foreground (GC, Get_Pref (Item.Kernel, Browsers_Link_Color));

      Draw_Text
        (Pixmap (Item),
         Font  => Font,
         GC    => GC,
         X     => 2,
         Y     => 2 + Get_Ascent (Font),
         Text  => Str);

      if Str2 /= null then
         Draw_Text
           (Pixmap (Item),
            Font  => Font,
            GC    => GC,
            X     => 2,
            Y     => 2 + Get_Ascent (Font) * 2 + Get_Descent (Font),
            Text  => Str2.all);
      else
         Draw_Text
           (Pixmap (Item),
            Font  => Font,
            GC    => GC,
            X     => 2,
            Y     => 2 + Get_Ascent (Font) * 2 + Get_Descent (Font),
            Text  => "<unknown unit name>");
      end if;

      Unref (GC);
   end Update_Display;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access Dependency_Item;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      null;
   end On_Button_Click;

end Browsers.Dependency_Items;
