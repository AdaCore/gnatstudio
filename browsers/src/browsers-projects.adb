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
with Glib.Object;              use Glib.Object;
with Glib.Xml_Int;             use Glib.Xml_Int;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Intl;               use Glide_Intl;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Image;                use Gtk.Image;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Image_Menu_Item;      use Gtk.Image_Menu_Item;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Canvas;            use Gtkada.Canvas;
with Gtkada.MDI;               use Gtkada.MDI;
with Pango.Layout;
with Projects;                 use Projects;
with Projects.Registry;        use Projects.Registry;
with Types;                    use Types;
with Ada.Exceptions;           use Ada.Exceptions;
with Traces;                   use Traces;
with Find_Utils;               use Find_Utils;

package body Browsers.Projects is

   Me : constant Debug_Handle := Create ("Browsers.Projects");

   Project_Browser_Module_ID : Module_ID;

   type Browser_Search_Context is new Search_Context with null record;
   type Browser_Search_Context_Access is access all Browser_Search_Context;

   ---------------------
   -- Project_Browser --
   ---------------------

   type Project_Browser_Record is new Browsers.Canvas.General_Browser_Record
     with null record;
   type Project_Browser is access all Project_Browser_Record'Class;

   ----------------------------
   -- Browser_Project_Vertex --
   ----------------------------

   type Browser_Project_Vertex is new Browsers.Canvas.Arrow_Item_Record with
      record
         Name    : Types.Name_Id;
      end record;
   type Browser_Project_Vertex_Access is access all Browser_Project_Vertex;

   function Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   procedure Resize_And_Draw
     (Item             : access Browser_Project_Vertex;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   procedure Gtk_New
     (V       : out Browser_Project_Vertex_Access;
      Browser : access Project_Browser_Record'Class;
      Project : Standard.Projects.Project_Type);
   --  Create a new project vertex

   ----------
   -- Misc --
   ----------

   procedure Examine_Project_Hierarchy
     (Browser : access Project_Browser_Record'Class;
      Project : Project_Type;
      Recursive : Boolean);
   --  Display the project hierarchy for Project in the canvas.
   --  If Recursive is True, then the projects imported indirectly are also
   --  displayed.

   procedure On_Examine_Prj_Hierarchy
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Open the project hierarchy browser for a specific project

   procedure On_Examine_Full_Prj_Hierarchy
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Same as above, but also display the projects imported indirectly

   procedure On_Examine_Ancestor_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Add to the browser all the projects that import the specific project

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

   function Project_Of (Item : access Browser_Project_Vertex'Class)
      return Project_Type;
   --  Return the project associated with Item

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

   function Find_Project
     (Browser      : access Project_Browser_Record'Class;
      Project_Name : Name_Id) return Browser_Project_Vertex_Access;
   --  Return the first item representing Project_Name

   function Add_Project_If_Not_Present
     (Browser : access Project_Browser_Record'Class;
      Project : Project_Type) return Browser_Project_Vertex_Access;
   --  Add a new item for Project if there is currently none in the browser

   procedure Examine_Ancestor_Project_Hierarchy
     (Browser          : access Project_Browser_Record'Class;
      Root_Project     : Project_Type;
      Project          : Project_Type);
   --  Add to the browser all the projects that with Project.

   procedure On_Examine_Prj_Hierarchy
     (Item : access Arrow_Item_Record'Class);
   procedure On_Examine_Ancestor_Hierarchy
     (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the button in the title bar

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of (Item : access Browser_Project_Vertex'Class)
      return Project_Type is
   begin
      return Get_Project_From_Name
        (Get_Registry (Get_Kernel (Get_Browser (Item))), Item.Name);
   end Project_Of;

   ------------------
   -- Find_Project --
   ------------------

   function Find_Project
     (Browser      : access Project_Browser_Record'Class;
      Project_Name : Name_Id) return Browser_Project_Vertex_Access
   is
      Iter : Item_Iterator := Start (Get_Canvas (Browser));
      Item : Canvas_Item;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null
           or else Browser_Project_Vertex_Access (Item).Name = Project_Name;
         Next (Iter);
      end loop;
      return Browser_Project_Vertex_Access (Item);
   end Find_Project;

   --------------------------------
   -- Add_Project_If_Not_Present --
   --------------------------------

   function Add_Project_If_Not_Present
     (Browser : access Project_Browser_Record'Class;
      Project : Project_Type) return Browser_Project_Vertex_Access
   is
      V : Browser_Project_Vertex_Access := Find_Project
        (Browser, Project_Name (Project));
   begin
      if V = null then
         Gtk_New (V, Browser, Project);
         Put (Get_Canvas (Browser), V);
         Refresh (V);
      end if;

      return V;
   end Add_Project_If_Not_Present;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (V       : out Browser_Project_Vertex_Access;
      Browser : access Project_Browser_Record'Class;
      Project : Project_Type) is
   begin
      V := new Browser_Project_Vertex;
      Initialize (V, Browser,
                  Project_Name (Project) & Project_File_Extension,
                  On_Examine_Ancestor_Hierarchy'Access,
                  On_Examine_Prj_Hierarchy'Access);
      V.Name := Project_Name (Project);

      Set_Children_Shown (V, not Has_Imported_Projects (Project));
   end Gtk_New;

   -------------------------------
   -- Examine_Project_Hierarchy --
   -------------------------------

   procedure Examine_Project_Hierarchy
     (Browser          : access Project_Browser_Record'Class;
      Project          : Project_Type;
      Recursive        : Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Browser);

      procedure Process_Project
        (Local : Project_Type; Src : Browser_Project_Vertex_Access);
      --  Display all the imported projects from Local.
      --  Src is the item associated with Local

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project
        (Local : Project_Type; Src : Browser_Project_Vertex_Access)
      is
         L           : Browser_Link;
         Dest        : Browser_Project_Vertex_Access;
         Iter        : Imported_Project_Iterator;
      begin
         Set_Children_Shown (Src, True);

         --  If we are displaying the recursive hierarchy for the root project,
         --  we know that there won't be remaining ancestor projects, so we can
         --  make it clear for the user.
         if Recursive and then Project = Get_Project (Kernel) then
            Set_Parents_Shown (Src, True);
         end if;

         Iter := Start (Local, Recursive => True, Direct_Only => True);

         while Current (Iter) /= No_Project loop
            Dest := Add_Project_If_Not_Present (Browser, Current (Iter));

            if not Has_Link (Get_Canvas (Browser), Src, Dest) then
               L := new Browser_Link_Record;
               Add_Link (Get_Canvas (Browser), L, Src, Dest);
            end if;

            --  ??? Could be more efficient if we could use Direct_Only =>
            --  False in the call to Start.

            if Recursive and then not Children_Shown (Dest) then
               Process_Project (Current (Iter), Dest);
            end if;

            Next (Iter);
         end loop;

         Redraw_Title_Bar (Browser_Item (Src));
      end Process_Project;

      Src : Browser_Project_Vertex_Access;
      Item_Was_Present : Boolean;
   begin
      Trace (Me, "Examine_Project_Hierarchy for "
             & Project_Name (Project));
      Push_State (Kernel, Busy);

      Src := Find_Project (Browser, Project_Name (Project));
      Item_Was_Present := Src /= null;

      if Src = null then
         Src := Add_Project_If_Not_Present (Browser, Project);
      end if;

      Process_Project (Project, Src);

      if Item_Was_Present then
         --  Need to select the item as well: this doesn't impact the
         --  contextual menus, since the item is already selected anyway, but
         --  is necessary when displaying the browser from the explorer's
         --  contextual menu.
         --
         --  As a side effect, this also refreshes the canvas

         Clear_Selection (Get_Canvas (Browser));
         Add_To_Selection (Get_Canvas (Browser), Src);
         Show_Item (Get_Canvas (Browser), Src);
      end if;

      Layout (Browser, Force => False);
      Refresh_Canvas (Get_Canvas (Browser));

      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Examine_Project_Hierarchy;

   ----------------------------------------
   -- Examine_Ancestor_Project_Hierarchy --
   ----------------------------------------

   procedure Examine_Ancestor_Project_Hierarchy
     (Browser          : access Project_Browser_Record'Class;
      Root_Project     : Project_Type;
      Project          : Project_Type)
   is
      Kernel      : constant Kernel_Handle := Get_Kernel (Browser);
      Src, Dest   : Browser_Project_Vertex_Access;
      L           : Browser_Link;
      Iter        : Imported_Project_Iterator;
   begin
      Trace (Me, "Examine_Ancestor_Project_Hierarchy for "
             & Project_Name (Project));
      Push_State (Kernel, Busy);

      Dest := Add_Project_If_Not_Present (Browser, Project);
      Set_Parents_Shown (Dest, True);

      Iter := Find_All_Projects_Importing
        (Root_Project => Root_Project,
         Project      => Project,
         Include_Self => False,
         Direct_Only  => True);

      while Current (Iter) /= No_Project loop
         Src := Add_Project_If_Not_Present (Browser, Current (Iter));

         if not Has_Link (Get_Canvas (Browser), Src, Dest) then
            L := new Browser_Link_Record;
            Add_Link (Get_Canvas (Browser), L, Src, Dest);
         end if;

         Next (Iter);
      end loop;

      Redraw_Title_Bar (Browser_Item (Dest));

      Layout (Browser, Force => False);
      Refresh_Canvas (Get_Canvas (Browser));
      Pop_State (Kernel);

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Examine_Ancestor_Project_Hierarchy;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Browser_Project_Vertex;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      --  Project : constant Project_Id :=
      --        Get_Project_View_From_Name (Item.Name);
      W, H    : Gint;
   begin
      --  ??? Why is this code commented out
      --  Set_text (Layout, Project_Path (Project));
      --  Get_Pixel_Size (Layout, W, H);

      --  Just reserve a little bit of space so that there is something else
      --  than the title bar
      W := 0;
      H := 10;

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access,
         Gint'Max (Width, W + 2 * Margin), H + Height,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      --  Draw_Layout
      --    (Drawable => Pixmap (Item),
      --     GC       => Get_Black_GC (Get_Style (Get_Browser (Item))),
      --     X        => Xoffset + Margin,
      --     Y        => Yoffset,
      --     Layout   => Layout);
   end Resize_And_Draw;

   ------------------------------
   -- On_Examine_Prj_Hierarchy --
   ------------------------------

   procedure On_Examine_Prj_Hierarchy
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Browser : constant MDI_Child :=
        Open_Project_Browser (Get_Kernel (Context));
   begin
      Examine_Project_Hierarchy
        (Project_Browser (Get_Widget (Browser)),
         Project_Information (File_Selection_Context_Access (Context)),
         Recursive        => False);
   end On_Examine_Prj_Hierarchy;

   ------------------------------
   -- On_Examine_Prj_Hierarchy --
   ------------------------------

   procedure On_Examine_Prj_Hierarchy
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Project_Hierarchy
        (Project_Browser (Get_Browser (Item)),
         Project_Of (Browser_Project_Vertex_Access (Item)),
         Recursive        => False);
   end On_Examine_Prj_Hierarchy;

   -----------------------------------
   -- On_Examine_Ancestor_Hierarchy --
   -----------------------------------

   procedure On_Examine_Ancestor_Hierarchy
     (Item : access Arrow_Item_Record'Class)
   is
      B : constant Project_Browser := Project_Browser (Get_Browser (Item));
   begin
      Examine_Ancestor_Project_Hierarchy
        (Browser      => B,
         Root_Project => Get_Project (Get_Kernel (B)),
         Project      => Project_Of (Browser_Project_Vertex_Access (Item)));
   end On_Examine_Ancestor_Hierarchy;

   -----------------------------------
   -- On_Examine_Full_Prj_Hierarchy --
   -----------------------------------

   procedure On_Examine_Full_Prj_Hierarchy
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Browser : constant MDI_Child :=
        Open_Project_Browser (Get_Kernel (Context));
   begin
      Examine_Project_Hierarchy
        (Project_Browser (Get_Widget (Browser)),
         Project_Information (File_Selection_Context_Access (Context)),
         Recursive        => True);
   end On_Examine_Full_Prj_Hierarchy;

   -----------------------------------------
   -- On_Examine_Ancestor_From_Contextual --
   -----------------------------------------

   procedure On_Examine_Ancestor_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Browser : constant MDI_Child :=
        Open_Project_Browser (Get_Kernel (Context));
      B : constant Project_Browser := Project_Browser (Get_Widget (Browser));
   begin
      Examine_Ancestor_Project_Hierarchy
        (B, Get_Project (Get_Kernel (B)),
         Project_Information (File_Selection_Context_Access (Context)));
   end On_Examine_Ancestor_From_Contextual;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Browser, Event);
      Context : constant Selection_Context_Access :=
        new File_Selection_Context;
      Name : constant String := Get_String (Item.Name);
      Mitem : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
   begin
      Set_File_Information
        (File_Selection_Context_Access (Context),
         Project => Project_Of (Item));

      if Menu /= null then
         Gtk_New (Mitem, Label => (-"Examine dependencies for ") & Name);
         Gtk_New (Pix, Stock_Go_Forward, Icon_Size_Menu);
         Set_Image (Mitem, Pix);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller (On_Examine_Prj_Hierarchy'Access),
            Context);
         Set_Sensitive (Mitem, not Children_Shown (Item));

         Gtk_New (Mitem, Label => (-"Examine recursive dependencies for ")
                  & Name);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (On_Examine_Full_Prj_Hierarchy'Access),
            Context);

         Gtk_New
           (Mitem, Label => (-"Examining projects depending on ") & Name);
         Gtk_New (Pix, Stock_Go_Back, Icon_Size_Menu);
         Set_Image (Mitem, Pix);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (On_Examine_Ancestor_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Parents_Shown (Item));
      end if;

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
      Initialize (Browser, Kernel, Create_Toolbar => False);
      --  Setup_Default_Toolbar (Browser);

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
      Browser : Project_Browser;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Create_Project_Browser (Kernel);
         Child := Put
           (Get_MDI (Kernel), Browser,
            Focus_Widget => Gtk_Widget (Get_Canvas (Browser)));
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
                     -"Examine projects imported by "
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
      Iter    : constant Selection_Iterator := Start (Get_Canvas (Browser));
   begin
      --  If there is no selection, or more than one item, nothing we can do
      if Get (Iter) = null
        or else Get (Next (Iter)) /= null
      then
         return null;
      end if;

      return Contextual_Factory
        (Item    => Browser_Item (Get (Iter)),
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
      Iter         : Item_Iterator;
      It           : Browser_Project_Vertex_Access;
      Selection    : Selection_Iterator;

   begin
      if Child = null then
         return False;
      end if;

      Browser := Project_Browser (Get_Widget (Child));
      Selection := Start (Get_Canvas (Browser));

      Saw_Selected := Get (Selection) = null;

      --  We have to start from the beginning, but to save some time in case we
      --  have no more item after the current one, we memorize the first
      --  matching item right away

      Iter := Start (Get_Canvas (Browser));
      loop
         It := Browser_Project_Vertex_Access (Get (Iter));
         exit when It = null;

         --  No need to test if we have already found one, but haven't
         --  encountered the current selection.

         if First_Match = null or else Saw_Selected then
            if Match (Context, Get_String (It.Name)) /= -1 then
               First_Match := Canvas_Item (It);

               exit when Saw_Selected;
            end if;
         end if;

         if Is_Selected (Get_Canvas (Browser), It) then
            Saw_Selected := True;
         end if;

         Next (Iter);
      end loop;

      if First_Match /= null then
         Clear_Selection (Get_Canvas (Browser));
         Add_To_Selection (Get_Canvas (Browser), First_Match);
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
