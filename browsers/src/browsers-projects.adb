------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Glib;                   use Glib;
with Glib.Object;            use Glib.Object;
with Cairo;                  use Cairo;
with Gdk.Event;              use Gdk.Event;
with Gtk.Menu;               use Gtk.Menu;
with Gtk.Stock;              use Gtk.Stock;
with Gtk.Widget;             use Gtk.Widget;
with Gtkada.Canvas;          use Gtkada.Canvas;
with Gtkada.MDI;             use Gtkada.MDI;
with Pango.Layout;

with Generic_Views;
with GNATCOLL.Projects;      use GNATCOLL.Projects;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Intl;               use GPS.Intl;
with Projects;               use Projects;
with Browsers.Canvas;        use Browsers.Canvas;
with Prj;
with Traces;                 use Traces;
with Find_Utils;             use Find_Utils;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Vsearch;                use Vsearch;

package body Browsers.Projects is

   Me : constant Debug_Handle := Create ("Browsers.Projects");

   type Project_Browser_Module is new Module_ID_Record with null record;

   type Browser_Search_Context is new Search_Context with null record;
   type Browser_Search_Context_Access is access all Browser_Search_Context;

   overriding function Context_Look_In
     (Self : Browser_Search_Context) return String;
   overriding procedure Default_Context_Factory
     (Module  : access Project_Browser_Module;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

   ---------------------
   -- Project_Browser --
   ---------------------

   type Project_Browser_Record is new Browsers.Canvas.General_Browser_Record
     with null record;

   function Initialize
     (View   : access Project_Browser_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   --  Initialize the browser, and return the focus widget

   package Project_Views is new Generic_Views.Simple_Views
     (Module_Name            => "Project_Browser",
      View_Name              => -"Project Browser",
      Formal_View_Record     => Project_Browser_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Graphs);
   subtype Project_Browser is Project_Views.View_Access;
   use type Project_Browser;

   --------------
   -- Commands --
   --------------

   type Imported_By_Command is new Interactive_Command with record
      Recursive      : Boolean := False;
      Show_Ancestors : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Imported_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ----------------------------
   -- Browser_Project_Vertex --
   ----------------------------

   type Browser_Project_Vertex is new Browsers.Canvas.Arrow_Item_Record with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;
   type Browser_Project_Vertex_Access is access all Browser_Project_Vertex;

   overriding procedure Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Context : in out Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu);
   overriding procedure Resize_And_Draw
     (Item             : access Browser_Project_Vertex;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   procedure Gtk_New
     (V       : out Browser_Project_Vertex_Access;
      Browser : access Project_Browser_Record'Class;
      Project : Project_Type);
   --  Create a new project vertex

   ----------
   -- Misc --
   ----------

   procedure Examine_Project_Hierarchy
     (Browser   : access Project_Browser_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean);
   --  Display the project hierarchy for Project in the canvas.
   --  If Recursive is True, then the projects imported indirectly are also
   --  displayed.

   function Project_Of (Item : access Browser_Project_Vertex'Class)
      return Project_Type;
   --  Return the project associated with Item

   function Browser_Search_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Create a new search context for the explorer

   overriding procedure Search
     (Context         : access Browser_Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean);
   --  Search the next occurrence in the explorer

   function Find_Project
     (Browser      : access Project_Browser_Record'Class;
      Project_Name : String) return Browser_Project_Vertex_Access;
   --  Return the first item representing Project_Name

   function Add_Project_If_Not_Present
     (Browser : access Project_Browser_Record'Class;
      Project : Project_Type) return Browser_Project_Vertex_Access;
   --  Add a new item for Project if there is currently none in the browser

   procedure Examine_Ancestor_Project_Hierarchy
     (Browser : access Project_Browser_Record'Class;
      Project : Project_Type);
   --  Add to the browser all the projects that with Project

   procedure On_Examine_Prj_Hierarchy
     (Item : access Arrow_Item_Record'Class);
   procedure On_Examine_Ancestor_Hierarchy
     (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the button in the title bar

   procedure Add_Link_If_Not_Present
     (Browser      : access Project_Browser_Record'Class;
      Src, Dest    : Browser_Project_Vertex_Access;
      Limited_With : Boolean);
   --  Add a link between the two items

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of (Item : access Browser_Project_Vertex'Class)
      return Project_Type is
   begin
      return Get_Registry (Get_Kernel (Get_Browser (Item))).Tree
        .Project_From_Name (To_String (Item.Name));
   end Project_Of;

   ------------------
   -- Find_Project --
   ------------------

   function Find_Project
     (Browser      : access Project_Browser_Record'Class;
      Project_Name : String) return Browser_Project_Vertex_Access
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
      V : Browser_Project_Vertex_Access :=
        Find_Project (Browser, Project.Name);
   begin
      if V = null then
         Gtk_New (V, Browser, Project);
         Put (Get_Canvas (Browser), V);
         Refresh (V);
      end if;

      return V;
   end Add_Project_If_Not_Present;

   -----------------------------
   -- Add_Link_If_Not_Present --
   -----------------------------

   procedure Add_Link_If_Not_Present
     (Browser      : access Project_Browser_Record'Class;
      Src, Dest    : Browser_Project_Vertex_Access;
      Limited_With : Boolean)
   is
      L      : Browser_Link;
      P1, P2 : Project_Type;
   begin
      if not Has_Link (Get_Canvas (Browser), Src, Dest) then
         L := new Browser_Link_Record;

         P1 := Project_Of (Src);
         P2 := Project_Of (Dest);

         if Extended_Project (P1) = P2 then
            Add_Link
              (Get_Canvas (Browser), L, Src, Dest, Descr => -"extending");
         else
            if Limited_With then
               Add_Link (Get_Canvas (Browser), L, Src, Dest,
                         Descr => "limited");
            else
               Add_Link (Get_Canvas (Browser), L, Src, Dest);
            end if;
         end if;
      end if;
   end Add_Link_If_Not_Present;

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
                  Project.Name & Prj.Project_File_Extension,
                  On_Examine_Ancestor_Hierarchy'Access,
                  On_Examine_Prj_Hierarchy'Access);
      V.Name := To_Unbounded_String (Project.Name);

      Set_Children_Shown (V, not Has_Imported_Projects (Project));
   end Gtk_New;

   -------------------------------
   -- Examine_Project_Hierarchy --
   -------------------------------

   procedure Examine_Project_Hierarchy
     (Browser   : access Project_Browser_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean)
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
         Dest : Browser_Project_Vertex_Access;
         Iter : Project_Iterator;
         Cr   : Cairo_Context;
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
            Add_Link_If_Not_Present
              (Browser, Src, Dest, Limited_With => Is_Limited_With (Iter));

            --  ??? Could be more efficient if we could use Direct_Only =>
            --  False in the call to Start.

            if Recursive and then not Children_Shown (Dest) then
               Process_Project (Current (Iter), Dest);
            end if;

            Next (Iter);
         end loop;

         Cr := Create (Src);
         Redraw_Title_Bar (Browser_Item (Src), Cr);
         Destroy (Cr);
      end Process_Project;

      Src : Browser_Project_Vertex_Access;
      Item_Was_Present : Boolean;
   begin
      Trace (Me, "Examine_Project_Hierarchy for " & Project.Name);
      Push_State (Kernel, Busy);

      Src := Find_Project (Browser, Project.Name);
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
         Trace (Exception_Handle, E);
         Pop_State (Kernel);
   end Examine_Project_Hierarchy;

   ----------------------------------------
   -- Examine_Ancestor_Project_Hierarchy --
   ----------------------------------------

   procedure Examine_Ancestor_Project_Hierarchy
     (Browser : access Project_Browser_Record'Class;
      Project : Project_Type)
   is
      Kernel    : constant Kernel_Handle := Get_Kernel (Browser);
      Src, Dest : Browser_Project_Vertex_Access;
      Iter      : Project_Iterator;
      Cr        : Cairo_Context;
   begin
      Trace (Me, "Examine_Ancestor_Project_Hierarchy for " & Project.Name);
      Push_State (Kernel, Busy);

      Dest := Add_Project_If_Not_Present (Browser, Project);
      Set_Parents_Shown (Dest, True);

      Iter := Find_All_Projects_Importing
        (Project      => Project,
         Include_Self => False,
         Direct_Only  => True);

      while Current (Iter) /= No_Project loop
         Src := Add_Project_If_Not_Present (Browser, Current (Iter));
         Add_Link_If_Not_Present
           (Browser, Src, Dest,
            Limited_With => Is_Limited_With (Iter));

         Next (Iter);
      end loop;

      Cr := Create (Dest);
      Redraw_Title_Bar (Browser_Item (Dest), Cr);
      Destroy (Cr);

      Layout (Browser, Force => False);
      Refresh_Canvas (Get_Canvas (Browser));
      Pop_State (Kernel);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Kernel);
   end Examine_Ancestor_Project_Hierarchy;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   overriding procedure Resize_And_Draw
     (Item             : access Browser_Project_Vertex;
      Cr               : in out Cairo_Context;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class)
   is
      --  Project : constant Project_Id :=
      --        Get_Project_View_From_Name (Item.Name);
      W, H : Gint;
   begin
      --  ??? Why is this code commented out
      --  Set_text (Layout, Project_Path (Project));
      --  Get_Pixel_Size (Layout, W, H);

      --  Just reserve a little bit of space so that there is something else
      --  than the title bar
      W := 0;
      H := 10;

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access, Cr,
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
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Project_Hierarchy
        (Project_Browser (Get_Browser (Item)),
         Project_Of (Browser_Project_Vertex_Access (Item)),
         Recursive => False);
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
        (Browser => B,
         Project => Project_Of (Browser_Project_Vertex_Access (Item)));
   end On_Examine_Ancestor_Hierarchy;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   overriding procedure Contextual_Factory
     (Item    : access Browser_Project_Vertex;
      Context : in out Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Browser, Event, Menu);
   begin
      Set_File_Information (Context, Project => Project_Of (Item));
   end Contextual_Factory;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Project_Browser_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
   begin
      Initialize (View, Kernel, Create_Toolbar => False);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Project_Views.Get_Module,
         Context_Func    => Default_Browser_Context_Factory'Access);
      return Gtk_Widget (View);
   end Initialize;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Project_Browser_Module;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject)
   is
      pragma Unreferenced (Module);
      Browser : constant Project_Browser :=
        Project_Views.View_From_Widget (Child);
      Iter    : constant Item_Iterator :=
        Start (Get_Canvas (Browser), Selected_Only => True);
   begin
      --  If there is no selection, or more than one item, nothing we can do
      if Get (Iter) /= null
        and then Get (Next (Iter)) = null
      then
         Contextual_Factory
           (Item    => Browser_Item (Get (Iter)),
            Context => Context,
            Browser => Browser,
            Event   => null,
            Menu    => null);
      end if;
   end Default_Context_Factory;

   ----------------------------
   -- Browser_Search_Factory --
   ----------------------------

   function Browser_Search_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
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

   overriding procedure Search
     (Context         : access Browser_Search_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean;
      Give_Focus      : Boolean;
      Found           : out Boolean;
      Continue        : out Boolean)
   is
      pragma Unreferenced (Search_Backward, Give_Focus);
      First_Match  : Canvas_Item;
      Saw_Selected : Boolean;
      Browser : constant Project_Browser :=
        Project_Views.Retrieve_View (Kernel);
      Iter         : Item_Iterator;
      It           : Browser_Project_Vertex_Access;
      Selection    : Item_Iterator;

   begin
      if Browser = null then
         Found := False;
         Continue := False;
         return;
      end if;

      Selection := Start (Get_Canvas (Browser), Selected_Only => True);

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
            if Match (Context, To_String (It.Name)) /= -1 then
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
         Found := True;
      else
         Found := False;
      end if;

      if It = null then
         Continue := False;
      else
         Continue := True;
      end if;

   end Search;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Imported_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Browser : constant Project_Browser :=
        Project_Views.Get_Or_Create_View (Get_Kernel (Context.Context));
   begin
      if Command.Show_Ancestors then
         Examine_Ancestor_Project_Hierarchy
           (Browser,
            Project_Information (Context.Context));
      else
         Examine_Project_Hierarchy
           (Browser,
            Project_Information (Context.Context),
            Recursive => Command.Recursive);
      end if;
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Name    : constant String := "Project Browser";
      Command : Interactive_Command_Access;
      Project_Browser_Module_ID : Module_ID;
   begin
      Project_Browser_Module_ID := new Project_Browser_Module;
      Project_Views.Register_Module
        (Kernel, Project_Browser_Module_ID,
         Menu_Name => -"Browsers/Project");

      --  ??? will be done in hook
      --     Set_Sensitive (Mitem, not Children_Shown (Item));
      Command := new Imported_By_Command;
      Register_Contextual_Menu
        (Kernel, "Show projects imported",
         Label       => -"Show projects imported by %p",
         Stock_Image => Stock_Go_Forward,
         Action      => Command,
         Filter      => Lookup_Filter (Kernel, "Project only"));

      Command := new Imported_By_Command;
      Imported_By_Command (Command.all).Recursive := True;
      Register_Contextual_Menu
        (Kernel, "Show recursive projects imported",
         Label  => -"Show projects imported by %p recursively",
         Action => Command,
         Filter => Create (Module => Project_Browser_Module_Name));

      --  ??? Set_Sensitive (Mitem, not Parents_Shown (Item));
      Command := new Imported_By_Command;
      Imported_By_Command (Command.all).Show_Ancestors := True;
      Register_Contextual_Menu
        (Kernel, "Show projects importing",
         Label  => -"Show projects depending on %p",
         Stock_Image => Stock_Go_Back,
         Action => Command,
         Filter => Lookup_Filter (Kernel, "Project only"));

      Register_Search_Function
        (Kernel  => Kernel,
         Label   => Name,
         Factory => Browser_Search_Factory'Access,
         Id      => Project_Views.Get_Module,
         Mask    => All_Options and not Supports_Replace
            and not Search_Backward and not All_Occurrences);
   end Register_Module;

   ---------------------
   -- Context_Look_In --
   ---------------------

   overriding function Context_Look_In
     (Self : Browser_Search_Context) return String
   is
      pragma Unreferenced (Self);
   begin
      return -"project browser";
   end Context_Look_In;

end Browsers.Projects;
