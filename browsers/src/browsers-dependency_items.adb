-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;              use Glib;
with Gdk.Drawable;      use Gdk.Drawable;
with Gdk.Event;         use Gdk.Event;
with Gdk.Font;          use Gdk.Font;
with Gdk.Window;        use Gdk.Window;
with Gtkada.Canvas;     use Gtkada.Canvas;

with Src_Info;                 use Src_Info;
with Src_Info.ALI;             use Src_Info.ALI;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel;             use Glide_Kernel;
with Browsers.Canvas;          use Browsers.Canvas;
with Browsers.Module;          use Browsers.Module;

with GNAT.OS_Lib;              use GNAT.OS_Lib;

package body Browsers.Dependency_Items is

   Display_Unit_Name : constant Boolean := False;
   --  <preference> True if the unit name should be displayed

   Margin : constant := 2;

   procedure Update_Display
     (Browser : access Glide_Browser_Record'Class;
      Item : access Buffered_Item_Record'Class);
   --  Update the display of the item

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out File_Item;
      Win     : Gdk_Window;
      Browser : access Glide_Browser_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      File    : Internal_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Win, Browser, Kernel, Copy (File));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item            : out File_Item;
      Win             : Gdk_Window;
      Browser         : access Glide_Browser_Record'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String)
   is
      ALI : constant String := ALI_Filename_From_Source
        (Source_Filename, Get_Project_View (Kernel),
         Get_Predefined_Source_Path (Kernel));
   begin
      Item := new File_Item_Record;
      Initialize (Item, Win, Browser, Kernel,
                  Make_Source_File (Source_Filename, ALI));
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item : access File_Item_Record'Class;
      Win  : Gdk_Window;
      Browser : access Glide_Browser_Record'Class;
      Kernel : access Kernel_Handle_Record'Class;
      File  : Internal_File)
   is
      use type Gdk_Window;
      Str : constant String := Get_Source_Filename (File);
      Str2 : String_Access;
      Font : Gdk_Font;
      Width, Height : Gint;

   begin
      pragma Assert (Win /= null);
      Item.Source := File;
      Item.Kernel := Kernel_Handle (Kernel);
      Item.Browser := Glide_Browser (Browser);

      Font := Get_Text_Font (Item.Browser);

      if Display_Unit_Name then
         Get_Unit_Name (Kernel, Item.Source, Str2);
      end if;

      Width  := String_Width (Font, Str);
      Height := (Get_Ascent (Font) + Get_Descent (Font)) + 2 * Margin;

      if Display_Unit_Name then
         if Str2 /= null then
            Width := Gint'Max (Width, String_Width (Font, Str2.all));
         else
            Width := Gint'Max
              (Width, String_Width (Font, String' ("<unknown unit name>")));
         end if;

         Height := Height + (Get_Ascent (Font) + Get_Descent (Font))
           + 2 * Margin;
      end if;

      Width := Width + 4 * Margin;
      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Item.Browser), Width, Height);

      Update_Display (Item.Browser, Item);
   end Initialize;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (Browser : access Glide_Browser_Record'Class;
      Item    : access Buffered_Item_Record'Class)
   is
      use type Gdk.Gdk_GC;
      Font : Gdk_Font := Get_Text_Font (Browser);
      Str2 : String_Access;

   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Font  => Font,
         GC    => Get_Text_GC (Browser),
         X     => Margin,
         Y     => Margin + Get_Ascent (Font),
         Text  => Get_Source_Filename (File_Item (Item).Source));

      if Display_Unit_Name then
         Get_Unit_Name
           (File_Item (Item).Kernel, File_Item (Item).Source, Str2);
         if Str2 /= null then
            Draw_Text
              (Pixmap (Item),
               Font  => Font,
               GC    => Get_Text_GC (Browser),
               X     => Margin,
               Y     => Margin + Get_Ascent (Font) * 2 + Get_Descent (Font),
               Text  => Str2.all);
         else
            Draw_Text
              (Pixmap (Item),
               Font  => Font,
               GC    => Get_Text_GC (Browser),
               X     => Margin,
               Y     => Margin + Get_Ascent (Font) * 2 + Get_Descent (Font),
               Text  => "<unknown unit name>");
         end if;
      end if;
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
         Examine_Dependencies
           (Item.Kernel, Item.Browser, Get_Source_Filename (Item.Source));
      elsif Get_Event_Type (Event) = Button_Press then
         Select_Item (Item.Browser, Item, Update_Display'Access);
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

end Browsers.Dependency_Items;
