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
with Gdk.Event;         use Gdk.Event;
with Gdk.Font;          use Gdk.Font;
with Gdk.GC;            use Gdk.GC;
with Gdk.Window;        use Gdk.Window;
with Gtkada.Canvas;     use Gtkada.Canvas;
with Gtk.Enums;         use Gtk.Enums;
with Gtk.Style;         use Gtk.Style;
with Gtk.Widget;        use Gtk.Widget;
with Gtkada.MDI;        use Gtkada.MDI;
with Pango.Font;        use Pango.Font;

with Src_Info;                 use Src_Info;
with Src_Info.ALI;             use Src_Info.ALI;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Browsers;    use Glide_Kernel.Browsers;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel;             use Glide_Kernel;
with Browsers.Canvas;          use Browsers.Canvas;

with GNAT.OS_Lib;              use GNAT.OS_Lib;

package body Browsers.Dependency_Items is

   procedure Update_Display
     (Item : access File_Item_Record'Class; Win : Gdk_Window);
   --  Update the display of the item

   procedure Ensure_Browser_Link (Item : access File_Item_Record'Class);
   --  Make sure that the link to the parent browser has been initialized.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item : out File_Item;
      Win  : Gdk_Window;
      Kernel : access Kernel_Handle_Record'Class;
      File  : Internal_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Win, Kernel, Copy (File));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item            : out File_Item;
      Win             : Gdk_Window;
      Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String)
   is
      ALI : constant String := ALI_Filename_From_Source
        (Source_Filename, Get_Project_View (Kernel), Get_Source_Path (Kernel));
   begin
      Item := new File_Item_Record;
      Initialize (Item, Win, Kernel, Make_Source_File (Source_Filename, ALI));
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item : access File_Item_Record'Class;
      Win  : Gdk_Window;
      Kernel : access Kernel_Handle_Record'Class;
      File  : Internal_File)
   is
      use type Gdk_Window;
   begin
      pragma Assert (Win /= null);
      Item.Source := File;
      Item.Kernel := Kernel_Handle (Kernel);
      Update_Display (Item, Win);
   end Initialize;

   -------------------------
   -- Ensure_Browser_Link --
   -------------------------

   procedure Ensure_Browser_Link
     (Item : access File_Item_Record'Class)
   is
      Browser : MDI_Child;
   begin
      if Item.Browser /= null then
         return;
      end if;

      --  Note: We do not need to check (for now) that browser we find is
      --  indeed the one that contains the item, since there can be only one
      --  browser that contains Dependency_Items.
      Browser := Open_Browser (Item.Kernel, Dependency_Browser);
      pragma Assert (Browser /= null);

      Item.Browser := Glide_Browser (Get_Widget (Browser));
   end Ensure_Browser_Link;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (Item : access File_Item_Record'Class; Win : Gdk_Window)
   is
      Margin : constant := 2;

      Str : constant String := Get_Source_Filename (Item.Source);
      Str2 : String_Access;
      Descr : Pango_Font_Description :=
        Get_Pref (Item.Kernel, Browsers_Link_Font);
      Font : Gdk_Font;
      GC   : Gdk_GC;
      Width, Height : Gint;

   begin
      Ensure_Browser_Link (Item);

      Set_Size
        (Descr,
         Get_Size (Descr) * Gint (Get_Zoom (Get_Canvas (Item.Browser))) / 100);
      Font := From_Description (Descr);

      Gdk_New (GC, Win);
      Set_Foreground (GC, White (Get_Default_Colormap));

      Get_Unit_Name (Item.Kernel, Item.Source, Str2);

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
        (Style       => Get_Style (Item.Browser),
         Window      => Pixmap (Item),
         State_Type  => State_Normal,
         Shadow_Type => Shadow_Out,
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
     (Item  : access File_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         Ensure_Browser_Link (Item);
         Examine_Dependencies
           (Item.Kernel, Item.Browser, Get_Source_Filename (Item.Source));
      end if;
   end On_Button_Click;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Item : access File_Item_Record)
      return Src_Info.Internal_File is
   begin
      return Item.Source;
   end Get_Source;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Link : out Dependency_Link;
      Dep  : Src_Info.Dependency_Info) is
   begin
      Link := new Dependency_Link_Record;
      Link.Dep := Dep;
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out File_Item_Record) is
   begin
      Destroy (Item.Source);
   end Destroy;

   -----------------------
   -- Refresh_File_Item --
   -----------------------

   function Refresh_File_Item
     (Canvas : access Interactive_Canvas_Record'Class;
      Item   : access Canvas_Item_Record'Class) return Boolean is
   begin
      Update_Display (File_Item (Item), Get_Window (Canvas));
      return True;
   end Refresh_File_Item;

end Browsers.Dependency_Items;
