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

with Glide_Kernel;     use Glide_Kernel;
with Prj.Tree;         use Prj.Tree;
with Gtkada.Canvas;    use Gtkada.Canvas;
with Glib.Graphs;      use Glib.Graphs;
with Project_Browsers; use Project_Browsers;
with Browsers.Canvas;  use Browsers.Canvas;
with Types;            use Types;
with Namet;            use Namet;
with Gdk.Drawable;     use Gdk.Drawable;
with Gdk.Event;        use Gdk.Event;
with Gdk.Font;         use Gdk.Font;
with Glib;             use Glib;

package body Browsers.Projects is

   Margin : constant := 2;

   procedure Update_Display
     (Browser : access Glide_Browser_Record'Class;
      Item    : access Gtkada.Canvas.Buffered_Item_Record'Class);
   --  Refresh the display of the item

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
         Select_Item (Item.Browser, Item, Update_Display'Access);
      end if;
   end On_Button_Click;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display
     (Browser : access Glide_Browser_Record'Class;
      Item    : access Gtkada.Canvas.Buffered_Item_Record'Class) is
   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Get_Text_Font (Browser),
         Get_Text_GC (Browser),
         Margin,
         Get_Ascent (Get_Text_Font (Browser)) + Margin,
         Get_Name_String (Project_Name (Browser_Project_Vertex_Access (Item)))
         & Prj.Project_File_Extension);
   end Update_Display;

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
         return Project_Vertex_Access;
      --  Return a new project vertex for the project

      function Edge_Factory (V1, V2 : access Project_Vertex'Class)
         return Canvas_Link;
      --  Return a new edge

      ------------------
      -- Edge_Factory --
      ------------------

      function Edge_Factory (V1, V2 : access Project_Vertex'Class)
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
         return Project_Vertex_Access
      is
         V : Browser_Project_Vertex_Access :=
           new Browser_Project_Vertex;
         Width, Height : Gint;
      begin
         Height := Get_Ascent (Font) + Get_Descent (Font) + 2 * Margin;
         Width := String_Width (Font, Get_Name_String (Project_Name)
                                & Prj.Project_File_Extension) + 2 * Margin;

         Set_Project_Name (V, Project_Name);
         V.Browser := Glide_Browser (In_Browser);

         Set_Screen_Size_And_Pixmap
           (V, Get_Window (In_Browser), Width, Height);
         Update_Display (In_Browser, V);
         return Project_Vertex_Access (V);
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

end Browsers.Projects;
