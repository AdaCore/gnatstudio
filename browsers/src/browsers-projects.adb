------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gtk.Stock;                 use Gtk.Stock;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Canvas_View;        use Gtkada.Canvas_View;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Style;              use Gtkada.Style;

with Generic_Views;
with GNATCOLL.Projects;      use GNATCOLL.Projects;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with GPS.Kernel;             use GPS.Kernel;
with GPS.Kernel.Contexts;    use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;         use GPS.Kernel.MDI;
with GPS.Kernel.Modules;     use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;  use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;     use GPS.Kernel.Project;
with GPS.Search;
with GPS.Intl;               use GPS.Intl;
with Projects;               use Projects;
with Browsers.Canvas;        use Browsers.Canvas;
with Find_Utils;             use Find_Utils;
with Commands.Interactive;   use Commands, Commands.Interactive;
with Vsearch;                use Vsearch;

package body Browsers.Projects is

   Me : constant Trace_Handle := Create ("Browsers.Projects");

   type Project_Browser_Module is new Module_ID_Record with null record;

   type Browser_Search_Context is new Root_Search_Context with null record;
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
     (View   : access Project_Browser_Record'Class)
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
      Group                  => Group_Default);
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

   ------------------
   -- Browser_Item --
   ------------------

   type Project_Item_Record is new GPS_Item_Record with record
      Name : Ada.Strings.Unbounded.Unbounded_String;
      --  Store the name, and not the project, in case the latter becomes
      --  invalid after we reload a project

      Circle : Ellipse_Item;
      --  Might be set or not (depends on the chosen draw mode)
   end record;
   type Project_Item is access all Project_Item_Record'Class;

   procedure Gtk_New
     (V       : out Project_Item;
      Browser : not null access Project_Browser_Record'Class;
      Project : Project_Type);
   --  Create a new project vertex

   function Project_Of
     (Self : not null access Project_Item_Record'Class) return Project_Type;
   --  Return the project associated with Item

   overriding procedure Set_Context
     (Item    : not null access Project_Item_Record;
      Context : in out Selection_Context);
   --  Set the GPS context from a selected item.

   -------------------
   -- Browser links --
   -------------------

   procedure Gtk_New_Link
     (L               : out Canvas_Link;
      Browser         : not null access Project_Browser_Record'Class;
      Src, Dest       : not null access Abstract_Item_Record'Class;
      Is_Limited_With : Boolean := False;
      Extending       : Boolean := False);
   --  Create a new link

   ----------
   -- Misc --
   ----------

   procedure Examine_Project_Hierarchy
     (Browser   : not null access Project_Browser_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean;
      Rescale : Boolean);
   --  Display the project hierarchy for Project in the canvas.
   --  If Recursive is True, then the projects imported indirectly are also
   --  displayed.

   function Browser_Search_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Root_Search_Context_Access;
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
     (Browser      : not null access Project_Browser_Record'Class;
      Project_Name : String) return Project_Item;
   --  Return the first item representing Project_Name

   function Add_Project_If_Not_Present
     (Browser : not null access Project_Browser_Record'Class;
      Project : Project_Type) return Project_Item;
   --  Add a new item for Project if there is currently none in the browser

   procedure Examine_Ancestor_Project_Hierarchy
     (Browser : not null access Project_Browser_Record'Class;
      Project : Project_Type;
      Rescale : Boolean);
   --  Add to the browser all the projects that with Project

   type Show_Importing_Projects_Button is new Left_Arrow_Record
      with null record;
   overriding procedure On_Click
     (Self : not null access Show_Importing_Projects_Button;
      View : not null access GPS_Canvas_View_Record'Class);

   type Show_Imported_Projects_Button is new Right_Arrow_Record
   with null record;
   overriding procedure On_Click
     (Self : not null access Show_Imported_Projects_Button;
      View : not null access GPS_Canvas_View_Record'Class);

   procedure Add_Link_If_Not_Present
     (Browser      : not null access Project_Browser_Record'Class;
      Src, Dest    : Project_Item;
      Limited_With : Boolean);
   --  Add a link between the two items

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of (Self : not null access Project_Item_Record'Class)
      return Project_Type is
   begin
      return Get_Registry (Self.Browser.Kernel).Tree
        .Project_From_Name (To_String (Self.Name));
   end Project_Of;

   ------------------
   -- Find_Project --
   ------------------

   function Find_Project
     (Browser      : not null access Project_Browser_Record'Class;
      Project_Name : String) return Project_Item
   is
      Result : Project_Item;

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if Item.all in Project_Item_Record'Class
           and then Project_Item (Item).Name = Project_Name
         then
            Result := Project_Item (Item);
         end if;
      end On_Item;

   begin
      Browser.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);
      return Result;
   end Find_Project;

   --------------------------------
   -- Add_Project_If_Not_Present --
   --------------------------------

   function Add_Project_If_Not_Present
     (Browser : not null access Project_Browser_Record'Class;
      Project : Project_Type) return Project_Item
   is
      V : Project_Item := Find_Project (Browser, Project.Name);
   begin
      if V = null then
         Gtk_New (V, Browser, Project);
         V.Set_Position ((0.0, 0.0));  --  ???
         Browser_Model (Browser.Get_View.Model).Add (V);
      end if;

      return V;
   end Add_Project_If_Not_Present;

   ------------------
   -- Gtk_New_Link --
   ------------------

   procedure Gtk_New_Link
     (L               : out Canvas_Link;
      Browser         : not null access Project_Browser_Record'Class;
      Src, Dest       : not null access Abstract_Item_Record'Class;
      Is_Limited_With : Boolean := False;
      Extending       : Boolean := False)
   is
      S  : constant access Browser_Styles := Browser.Get_View.Get_Styles;
      L2 : constant GPS_Link := new GPS_Link_Record;
   begin
      if Is_Limited_With then
         L2.Default_Style := S.Link2;
         Initialize
           (L2,
            From        => Src,
            To          => Dest,
            Routing     => Curve,
            Label       => Gtk_New_Text (S.Title, "limited"),
            Anchor_From => (X => 1.0, others => <>),
            Anchor_To   => (X => 0.0, others => <>),
            Style       => L2.Default_Style);
      elsif Extending then
         L2.Default_Style := S.Link2;
         Initialize
           (L2,
            From        => Src,
            To          => Dest,
            Routing     => Curve,
            Label       => Gtk_New_Text (S.Title, "extending"),
            Anchor_From => (X => 1.0, others => <>),
            Anchor_To   => (X => 0.0, others => <>),
            Style       => L2.Default_Style);
      else
         L2.Default_Style := S.Link;
         Initialize
           (L2,
            From        => Src,
            To          => Dest,
            Routing     => Curve,
            Anchor_From => (X => 1.0, others => <>),
            Anchor_To   => (X => 0.0, others => <>),
            Style       => L2.Default_Style);
      end if;
      L := Canvas_Link (L2);
   end Gtk_New_Link;

   -----------------------------
   -- Add_Link_If_Not_Present --
   -----------------------------

   procedure Add_Link_If_Not_Present
     (Browser      : not null access Project_Browser_Record'Class;
      Src, Dest    : Project_Item;
      Limited_With : Boolean)
   is
      L      : Canvas_Link;
      P1, P2 : Project_Type;
      S1, S2 : access Abstract_Item_Record'Class;

   begin
      if not Browser.Has_Link (Src, Dest) then
         P1 := Project_Of (Src);
         P2 := Project_Of (Dest);

         S1 := (if Src.Circle = null then Src else Src);
         S2 := (if Dest.Circle = null then Dest else Dest);

         Gtk_New_Link
           (L, Browser, S1, S2,
            Is_Limited_With => Limited_With,
            Extending       => Extended_Project (P1) = P2);
         Browser_Model (Browser.Get_View.Model).Add (L);
      end if;
   end Add_Link_If_Not_Present;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (V       : out Project_Item;
      Browser : not null access Project_Browser_Record'Class;
      Project : Project_Type)
   is
      Contents : access Container_Item_Record'Class;
      S     : constant access Browser_Styles := Browser.Get_View.Get_Styles;
   begin
      V := new Project_Item_Record;
      V.Browser := General_Browser (Browser);
      V.Name := To_Unbounded_String (Project.Name);

      if True then
         V.Initialize_Rect (Style => S.Invisible);
         V.Set_Child_Layout (Horizontal_Stack);

         V.Circle := Gtk_New_Ellipse
           (Style => S.Circle, Width => 8.0, Height => 8.0);
         V.Add_Child (V.Circle);

         Contents := Gtk_New_Text (Style => S.Label, Text  => Project.Name);
         V.Add_Child (Contents, Margin => (0.0, 0.0, 0.0, 3.0));
      else
         V.Initialize_Rect (Style => S.Item, Radius => 5.0);
         Setup_Titlebar
           (V, Browser, Name => Project.Name,
            Left  => new Show_Importing_Projects_Button,
            Right => new Show_Imported_Projects_Button);

         if Project.Is_Aggregate_Project then
            Contents := Gtk_New_Text (S.Text_Font, "<<aggregate>>");
         else
            Contents := Gtk_New_Rect (S.Text_Font, Height => 1.0);
         end if;

         V.Add_Child (Contents, Margin => (2.0, 2.0, 2.0, 2.0));
      end if;
   end Gtk_New;

   -------------------------------
   -- Examine_Project_Hierarchy --
   -------------------------------

   procedure Examine_Project_Hierarchy
     (Browser   : not null access Project_Browser_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean;
      Rescale   : Boolean)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Browser);
      pragma Unreferenced (Kernel);

      procedure Process_Project (Local : Project_Type; Src : Project_Item);
      --  Display all the imported projects from Local.
      --  Src is the item associated with Local

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Local : Project_Type; Src : Project_Item) is
         Dest : Project_Item;
         Iter : Project_Iterator;
      begin
         --  If we are displaying the recursive hierarchy for the root project,
         --  we know that there won't be remaining ancestor projects, so we can
         --  make it clear for the user.
         Iter := Start (Local, Recursive => True, Direct_Only => True);
         while Current (Iter) /= No_Project loop
            if Current (Iter) /= Local then
               Dest := Add_Project_If_Not_Present (Browser, Current (Iter));
               Add_Link_If_Not_Present
                 (Browser, Src, Dest, Limited_With => Is_Limited_With (Iter));
            end if;
            Next (Iter);
         end loop;
      end Process_Project;

      Src, Src2        : Project_Item;
      Item_Was_Present : Boolean;
      Iter             : Project_Iterator;

   begin
      Trace (Me, "Examine_Project_Hierarchy for " & Project.Name
             & " Recursive=" & Recursive'Img);

      Src := Find_Project (Browser, Project.Name);
      Item_Was_Present := Src /= null;

      if Src = null then
         Src := Add_Project_If_Not_Present (Browser, Project);
      end if;

      if Recursive then
         Iter := Start (Project, Recursive => True, Direct_Only => False);
         while Current (Iter) /= No_Project loop
            Src2 := Add_Project_If_Not_Present (Browser, Current (Iter));
            Process_Project (Current (Iter), Src2);
            Next (Iter);
         end loop;

      else
         Process_Project (Project, Src);
      end if;

      if Item_Was_Present then
         --  Need to select the item as well: this doesn't impact the
         --  contextual menus, since the item is already selected anyway, but
         --  is necessary when displaying the browser from the explorer's
         --  contextual menu.
         --
         --  As a side effect, this also refreshes the canvas

         Browser.Get_View.Model.Clear_Selection;
         Browser.Get_View.Model.Add_To_Selection (Src);
         Browser.Get_View.Scroll_Into_View (Src);
      end if;

      Browser.Refresh_Layout (Rescale => Rescale);
   end Examine_Project_Hierarchy;

   ----------------------------------------
   -- Examine_Ancestor_Project_Hierarchy --
   ----------------------------------------

   procedure Examine_Ancestor_Project_Hierarchy
     (Browser : not null access Project_Browser_Record'Class;
      Project : Project_Type;
      Rescale : Boolean)
   is
      Src, Dest : Project_Item;
      Iter      : Project_Iterator;
   begin
      Trace (Me, "Examine_Ancestor_Project_Hierarchy for " & Project.Name);

      Dest := Add_Project_If_Not_Present (Browser, Project);

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

      Browser.Refresh_Layout (Rescale => Rescale);
   end Examine_Ancestor_Project_Hierarchy;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self : not null access Show_Imported_Projects_Button;
      View : not null access GPS_Canvas_View_Record'Class)
   is
      pragma Unreferenced (View);
      Prj : constant Project_Item := Project_Item (Self.Get_Toplevel_Item);
   begin
      Examine_Project_Hierarchy
        (Project_Browser (Prj.Browser), Project_Of (Prj), Recursive => False,
         Rescale => False);
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self : not null access Show_Importing_Projects_Button;
      View : not null access GPS_Canvas_View_Record'Class)
   is
      pragma Unreferenced (View);
      Prj : constant Project_Item := Project_Item (Self.Get_Toplevel_Item);
   begin
      Examine_Ancestor_Project_Hierarchy
        (Browser => Project_Browser (Prj.Browser),
         Project => Project_Of (Prj),
         Rescale => False);
   end On_Click;

   -----------------
   -- Set_Context --
   -----------------

   overriding procedure Set_Context
     (Item    : not null access Project_Item_Record;
      Context : in out Selection_Context)
   is
   begin
      Set_File_Information (Context, Project => Item.Project_Of);
   end Set_Context;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Project_Browser_Record'Class)
      return Gtk_Widget
   is
   begin
      Initialize (View, Use_Canvas_View => True);
      Register_Contextual_Menu
        (Kernel          => View.Kernel,
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
   begin
      Browser.Set_Context (Context);
   end Default_Context_Factory;

   ----------------------------
   -- Browser_Search_Factory --
   ----------------------------

   function Browser_Search_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Root_Search_Context_Access
   is
      pragma Unreferenced (Kernel, All_Occurences, Extra_Information);
      Context : Browser_Search_Context_Access;
   begin
      Context := new Browser_Search_Context;
      return Root_Search_Context_Access (Context);
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

      Saw_Selected : Boolean := False;
      --  We need to search after the currently selected item, so that Next
      --  works properly

      Browser : constant Project_Browser :=
        Project_Views.Retrieve_View (Kernel);

      procedure On_Item (Item : not null access Abstract_Item_Record'Class);
      procedure On_Item (Item : not null access Abstract_Item_Record'Class) is
      begin
         if not Saw_Selected
           and then Browser.Get_View.Model.Is_Selected (Item)
         then
            Saw_Selected := True;
            return;
         end if;

         if not Found
           and then Item.all in Project_Item_Record'Class
           and then not GPS.Search.Failed
             (Match (Context, To_String (Project_Item (Item).Name)))
         then
            --  If we are searching after the current selection, select the
            --  new item and we are done. Otherwise we'll have to loop
            if Saw_Selected then
               Found := True;
               Browser.Get_View.Model.Clear_Selection;
               Browser.Get_View.Model.Add_To_Selection (Item);
            elsif First_Match = null then
               First_Match := Canvas_Item (Item);
            end if;
         end if;
      end On_Item;

   begin
      Found := False;

      if Browser = null then
         Continue := False;
         return;
      end if;

      Browser.Get_View.Model.For_Each_Item
        (On_Item'Access, Filter => Kind_Item);

      if not Found and then First_Match /= null then
         Browser.Get_View.Model.Clear_Selection;
         Browser.Get_View.Model.Add_To_Selection (First_Match);
         Found := True;
      end if;

      Continue := Found;
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
            Project_Information (Context.Context),
            Rescale => True);
      else
         Examine_Project_Hierarchy
           (Browser,
            Project_Information (Context.Context),
            Recursive => Command.Recursive,
            Rescale => True);
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
        (Kernel, Project_Browser_Module_ID);

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
         Filter => Lookup_Filter (Kernel, "Project only"));

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
