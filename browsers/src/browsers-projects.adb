-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Browsers.Canvas;      use Browsers.Canvas;
with Gdk.Drawable;         use Gdk.Drawable;
with Gdk.Event;            use Gdk.Event;
with Gdk.Font;             use Gdk.Font;
with Glib.Graphs;          use Glib.Graphs;
with Glib;                 use Glib;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel;         use Glide_Kernel;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Namet;                use Namet;
with Prj_API;              use Prj_API;
with Prj.Tree;             use Prj.Tree;
with Project_Browsers;     use Project_Browsers;
with Types;                use Types;

package body Browsers.Projects is

   Margin : constant := 2;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item   : access Browser_Project_Vertex;
      Event  : Gdk.Event.Gdk_Event_Button) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         Select_Item (Item.Browser, Item, True);
      end if;
   end On_Button_Click;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Browser : access Glide_Browser_Record'Class;
                      Item    : access Browser_Project_Vertex) is
   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Get_Text_Font (Browser),
         Get_Text_GC (Browser),
         Margin,
         Get_Ascent (Get_Text_Font (Browser)) + Margin,
         Get_Name_String (Item.Name) & Prj.Project_File_Extension);
   end Refresh;

   -------------------------------
   -- Examine_Project_Hierarchy --
   -------------------------------

   procedure Examine_Project_Hierarchy
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      In_Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Project    : Prj.Tree.Project_Node_Id)
   is
      Font : Gdk_Font := Get_Text_Font (In_Browser);

      function Vertex_Factory (Project_Name : Types.Name_Id)
         return Canvas_Item;
      --  Return a new project vertex for the project

      function Edge_Factory (V1, V2 : access Canvas_Item_Record'Class)
         return Canvas_Link;
      --  Return a new edge

      ------------------
      -- Edge_Factory --
      ------------------

      function Edge_Factory (V1, V2 : access Canvas_Item_Record'Class)
         return Canvas_Link
      is
         L : Glide_Browser_Link := new Glide_Browser_Link_Record;
      begin
         return Canvas_Link (L);
      end Edge_Factory;

      --------------------
      -- Vertex_Factory --
      --------------------

      function Vertex_Factory (Project_Name : Types.Name_Id)
         return Canvas_Item
      is
         V : Browser_Project_Vertex_Access :=
           new Browser_Project_Vertex;
         Width, Height : Gint;
      begin
         Height := Get_Ascent (Font) + Get_Descent (Font) + 2 * Margin;
         Width := String_Width (Font, Get_Name_String (Project_Name)
                                & Prj.Project_File_Extension) + 2 * Margin;
         V.Name := Project_Name;
         V.Browser := Glide_Browser (In_Browser);

         Set_Screen_Size_And_Pixmap
           (V, Get_Window (In_Browser), Width, Height);
         Refresh (In_Browser, V);
         return Canvas_Item (V);
      end Vertex_Factory;

      G : Graph;
   begin
      G := Dependency_Graph
        (Project,
         Vertex_Factory'Unrestricted_Access,
         Edge_Factory'Unrestricted_Access);

      Set_Items (Get_Canvas (In_Browser), G);
      Layout (Get_Canvas (In_Browser),
              Force => False,
              Vertical_Layout => True);
      Refresh_Canvas (Get_Canvas (In_Browser));
   end Examine_Project_Hierarchy;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access Browser_Project_Vertex;
      Browser : access Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      Context : Selection_Context_Access := new File_Selection_Context;
   begin
      Set_File_Information
        (File_Selection_Context_Access (Context),
         Project_View => Get_Project_View_From_Name (Item.Name));
      return Context;
   end Contextual_Factory;

end Browsers.Projects;
