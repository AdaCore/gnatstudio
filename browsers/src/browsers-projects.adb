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
with Gdk.Event;                use Gdk.Event;
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
with Prj.Tree;                 use Prj.Tree;
with Prj_API;                  use Prj_API;
with Project_Browsers;         use Project_Browsers;
with Types;                    use Types;
with Ada.Exceptions;           use Ada.Exceptions;
with Traces;                   use Traces;
with Find_Utils;               use Find_Utils;

package body Browsers.Projects is

   Me : constant Debug_Handle := Create ("Browsers.Projects");

   Project_Browser_Module_ID : Module_ID;

   type Browser_Search_Context is new Search_Context with null record;
   type Browser_Search_Context_Access is access all Browser_Search_Context;

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

   function Browser_Search_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Create a new search context for the explorer

   function Search
     (Context         : access Browser_Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search the next occurrence in the explorer

   --------------------------
   -- Button_Click_On_Left --
   --------------------------

   procedure Button_Click_On_Left (Item : access Browser_Project_Vertex) is
      pragma Unreferenced (Item);
   begin
      null;
   end Button_Click_On_Left;

   ---------------------------
   -- Button_Click_On_Right --
   ---------------------------

   procedure Button_Click_On_Right (Item : access Browser_Project_Vertex) is
      pragma Unreferenced (Item);
   begin
      null;
   end Button_Click_On_Right;

   -------------------------------
   -- Examine_Project_Hierarchy --
   -------------------------------

   procedure Examine_Project_Hierarchy
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      In_Browser : access Browsers.Canvas.Glide_Browser_Record'Class;
      Project    : Prj.Tree.Project_Node_Id)
   is
      pragma Unreferenced (Kernel);

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
         L : constant Glide_Browser_Link := new Glide_Browser_Link_Record;
      begin
         return Edge_Access (L);
      end Edge_Factory;

      --------------------
      -- Vertex_Factory --
      --------------------

      function Vertex_Factory (Project_Name : Types.Name_Id)
         return Vertex_Access
      is
         V : Browser_Project_Vertex_Access := new Browser_Project_Vertex;
      begin
         Browsers.Canvas.Initialize
           (V, In_Browser,
            Get_String (Project_Name) & Prj.Project_File_Extension);
         V.Name := Project_Name;
         V.Browser := Glide_Browser (In_Browser);

         Set_Left_Arrow (V, False);
         Set_Right_Arrow (V, False);
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
      Context : constant Selection_Context_Access :=
        new File_Selection_Context;
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
         N.Tag := new String'("Project_Browser");
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
      Browser : constant Project_Browser := Project_Browser (Child);
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

   ----------------------------
   -- Browser_Search_Factory --
   ----------------------------

   function Browser_Search_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access
   is
      pragma Unreferenced (Kernel, All_Occurences, Extra_Information);
      Context : Browser_Search_Context_Access;
   begin
      Context := new Browser_Search_Context;
      return Search_Context_Access (Context);
   end Browser_Search_Factory;

   ------------
   -- Search --
   ------------

   function Search
     (Context         : access Browser_Search_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean
   is
      pragma Unreferenced (Search_Backward);
      First_Match  : Canvas_Item;
      Saw_Selected : Boolean;
      Child        : constant MDI_Child := Open_Project_Browser (Kernel);
      Browser      : Project_Browser;

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Check if Item matches the context

      ----------------
      -- Check_Item --
      ----------------

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         pragma Unreferenced (Canvas);
         It : constant Browser_Project_Vertex_Access :=
           Browser_Project_Vertex_Access (Item);
      begin
         --  No need to test if we have already found one, but haven't
         --  encountered the current selection.

         if First_Match = null or else Saw_Selected then
            if Match (Context, Get_String (It.Name)) /= -1 then
               First_Match := Canvas_Item (Item);

               if Saw_Selected then
                  return False;
               end if;
            end if;
         end if;

         if Selected_Item (Browser) = Canvas_Item (Item) then
            Saw_Selected := True;
         end if;

         return True;
      end Check_Item;

   begin
      if Child = null then
         return False;
      end if;

      Browser := Project_Browser (Get_Widget (Child));

      Saw_Selected := Selected_Item (Browser) = null;

      --  We have to start from the beginning, but to save some time in case we
      --  have no more item after the current one, we memorize the first
      --  matching item right away

      For_Each_Item (Get_Canvas (Browser), Check_Item'Unrestricted_Access);

      if First_Match /= null then
         Select_Item (Browser, First_Match, Refresh_Items => True);
         Show_Item (Get_Canvas (Browser), First_Match);
         return True;
      else
         return False;
      end if;
   end Search;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Name : constant String := "Project Browser";
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

      Find_Utils.Register_Search_Function
        (Kernel => Kernel,
         Data   => (Length            => Name'Length,
                    Label             => Name,
                    Factory           => Browser_Search_Factory'Access,
                    Extra_Information => null,
                    Id                => Project_Browser_Module_ID,
                    Mask              => All_Options and not Supports_Replace
                      and not Search_Backward and not All_Occurrences));
   end Register_Module;

end Browsers.Projects;
