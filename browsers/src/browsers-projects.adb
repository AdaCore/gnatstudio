-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2002                      --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Browsers.Canvas;          use Browsers.Canvas;
with Gdk.Drawable;             use Gdk.Drawable;
with Gdk.Event;                use Gdk.Event;
with Gdk.Font;                 use Gdk.Font;
with Glib;                     use Glib;
with Glib.Graphs;              use Glib.Graphs;
with Glib.Object;              use Glib.Object;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Intl;               use Glide_Intl;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Canvas;            use Gtkada.Canvas;
with Gtkada.MDI;               use Gtkada.MDI;
with Namet;                    use Namet;
with Prj.Tree;                 use Prj.Tree;
with Prj_API;                  use Prj_API;
with Project_Browsers;         use Project_Browsers;
with Types;                    use Types;
with Ada.Exceptions;   use Ada.Exceptions;
with Traces;           use Traces;

package body Browsers.Projects is

   Margin : constant := 2;

   Me : Debug_Handle := Create ("Browsers.Projects");

   Project_Browser_Module_ID : Module_ID;

   procedure On_Examine_Prj_Hierarchy
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Open the project hierarchy browser for a specific project

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the appropriate contextual menus

   function Create_Project_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Project_Browser;
   --  Create a new project browser

   function Open_Project_Browser
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Find, or create, a project browser

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Node_Ptr;
   --  Support functions for the MDI

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create a current kernel context, based on the currently selected item

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

   procedure Refresh
     (Browser : access Glide_Browser_Record'Class;
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
      pragma Unreferenced (Kernel);
      Font : Gdk_Font := Get_Text_Font (In_Browser);

      function Vertex_Factory (Project_Name : Types.Name_Id)
         return Vertex_Access;
      --  Return a new project vertex for the project

      function Edge_Factory (V1, V2 : access Vertex'Class)
         return Edge_Access;
      --  Return a new edge

      ------------------
      -- Edge_Factory --
      ------------------

      function Edge_Factory (V1, V2 : access Vertex'Class)
         return Edge_Access
      is
         pragma Unreferenced (V1, V2);
         L : Glide_Browser_Link := new Glide_Browser_Link_Record;
      begin
         return Edge_Access (L);
      end Edge_Factory;

      --------------------
      -- Vertex_Factory --
      --------------------

      function Vertex_Factory (Project_Name : Types.Name_Id)
         return Vertex_Access
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
         return Vertex_Access (V);
      end Vertex_Factory;

      G : Graph;

   begin
      G := Dependency_Graph
        (Project,
         Vertex_Factory'Unrestricted_Access,
         Edge_Factory'Unrestricted_Access);

      Set_Items (Get_Canvas (In_Browser), G);
      Layout (Get_Canvas (In_Browser), False, Vertical_Layout => True);
      Refresh_Canvas (Get_Canvas (In_Browser));
   end Examine_Project_Hierarchy;

   ------------------------------
   -- On_Examine_Prj_Hierarchy --
   ------------------------------

   procedure On_Examine_Prj_Hierarchy
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Browser : MDI_Child;
   begin
      Push_State (Get_Kernel (Context), Busy);
      Browser := Open_Project_Browser (Get_Kernel (Context));
      Examine_Project_Hierarchy
        (Get_Kernel (Context),
         Glide_Browser (Get_Widget (Browser)),
         Get_Project_From_View
         (Project_Information (File_Selection_Context_Access (Context))));
      Pop_State (Get_Kernel (Context));

   exception
      when E : others =>
         Pop_State (Get_Kernel (Context));
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Examine_Prj_Hierarchy;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Browser : access Glide_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Browser, Event, Menu);
      Context : Selection_Context_Access := new File_Selection_Context;
   begin
      Set_File_Information
        (File_Selection_Context_Access (Context),
         Project_View => Get_Project_View_From_Name (Item.Name));
      return Context;
   end Contextual_Factory;

   ----------------------------
   -- Create_Project_Browser --
   ----------------------------

   function Create_Project_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Project_Browser
   is
      Browser : Project_Browser;
   begin
      Browser := new Project_Browser_Record;
      Initialize (Browser, Kernel);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Project_Browser_Module_ID,
         Context_Func    => Default_Browser_Context_Factory'Access);
      Set_Size_Request
        (Browser,
         Get_Pref (Kernel, Default_Widget_Width),
         Get_Pref (Kernel, Default_Widget_Height));
      return Browser;
   end Create_Project_Browser;

   --------------------------
   -- Open_Project_Browser --
   --------------------------

   function Open_Project_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Gtkada.MDI.MDI_Child
   is
      Child : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Child := Put (Get_MDI (Kernel), Create_Project_Browser (Kernel));
         Set_Title (Child, -"Project Browser");
      end if;

      return Child;
   end Open_Project_Browser;

   -----------------------------
   -- Browser_Contextual_Menu --
   -----------------------------

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Item         : Gtk_Menu_Item;
      File_Context : File_Selection_Context_Access;
   begin
      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context)
           and then not Has_Directory_Information (File_Context)
           and then not Has_File_Information (File_Context)
         then
            Gtk_New (Item, Label =>
                     -"Examine project hierarchy for "
                     & Project_Name (Project_Information (File_Context)));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Examine_Prj_Hierarchy'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end Browser_Contextual_Menu;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Node_Ptr; User : Kernel_Handle) return Gtk_Widget is
   begin
      if Node.Tag.all = "Project_Browser" then
         return Gtk_Widget (Create_Project_Browser (User));
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class) return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Project_Browser_Record'Class then
         N := new Node;
         N.Tag := new String' ("Project_Browser");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel);
      Browser : Project_Browser := Project_Browser (Child);
   begin
      if Selected_Item (Browser) = null then
         return null;
      end if;

      return Contextual_Factory
        (Item    => Glide_Browser_Item (Selected_Item (Browser)),
         Browser => Browser,
         Event   => null,
         Menu    => null);
   end Default_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      Register_Module
        (Module                  => Project_Browser_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Project_Browser_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access,
         MDI_Child_Tag           => Project_Browser_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

end Browsers.Projects;
