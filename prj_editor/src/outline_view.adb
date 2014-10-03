------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Rectangle;             use Gdk.Rectangle;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Abstract_Tree_Model;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Basic_Types;               use Basic_Types;
with Entities_Tooltips;
with Generic_Views;             use Generic_Views;
with GPS.Editors;               use GPS.Editors;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Search;                use GPS.Search;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Language;                  use Language;
with Tooltips;                  use Tooltips;

with Outline_View.Model; use Outline_View.Model;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package body Outline_View is

   type Outline_View_Module_Record is new Module_ID_Record with null record;

   Outline_View_Module : Module_ID;
   Outline_View_Module_Name : constant String := "Outline_View";

   Hist_Show_Profile      : constant History_Key := "outline-show-profile";
   Hist_Sort_Alphabetical : constant History_Key := "outline-alphabetical";
   Hist_Editor_Link       : constant History_Key := "outline-editor-link";
   Hist_Show_Decls        : constant History_Key := "outline-show-decls";
   Hist_Show_Types        : constant History_Key := "outline-show-types";
   Hist_Show_Tasks        : constant History_Key := "outline-show-tasks";
   Hist_Show_Objects      : constant History_Key := "outline-show-objects";
   Hist_Show_With         : constant History_Key := "outline-show-with";
   Hist_Group_Spec_And_Body : constant History_Key :=
     "outline-group-spec-and-body";
   Hist_Flat_View         : constant History_Key := "outline-flat-view";

   overriding procedure Default_Context_Factory
     (Module  : access Outline_View_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the context has changed

   type Outline_View_Record is new Generic_Views.View_Record with record
      Tree        : Gtk_Tree_View;
      File        : GNATCOLL.VFS.Virtual_File;
      Icon        : Gdk_Pixbuf;
      File_Icon   : Gdk_Pixbuf;
      Filter      : Gtk.GEntry.Gtk_Entry;

      Spec_Column : Gtk_Tree_View_Column;
      Body_Column : Gtk_Tree_View_Column;
   end record;
   overriding procedure Create_Toolbar
     (View    : not null access Outline_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Outline_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Outline_View_Record;
      Pattern : in out Search_Pattern_Access);

   function Initialize
     (Outline : access Outline_View_Record'Class)
     return Gtk.Widget.Gtk_Widget;
   --  Create a new outline view, and return the focus widget.

   package Outline_Views is new Generic_Views.Simple_Views
     (Module_Name        => Outline_View_Module_Name,
      View_Name          => "Outline",
      Formal_View_Record => Outline_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Position           => Position_Left,
      Initialize         => Initialize);
   use Outline_Views;
   subtype Outline_View_Access is Outline_Views.View_Access;

   procedure Refresh (View : access Gtk_Widget_Record'Class);
   --  Recompute the information for Outline.File, and redisplay it.
   --  If the constructs are up-to-date, do nothing.

   procedure Force_Refresh (View : access Gtk_Widget_Record'Class);
   --  Same as above, but force a full refresh

   function Button_Press
     (Outline : access GObject_Record'Class;
      Event   : Gdk_Event_Button) return Boolean;
   --  Called every time a row is clicked

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  React to changes in the preferences

   function Get_Filter_Record
     (Kernel : access Kernel_Handle_Record'Class) return Tree_Filter;
   --  Return the filters properties extracted from the kernel

   procedure Outline_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu);
   --  Context factory when creating contextual menus

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the current editor reaches a new location

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class);
   --  Called when the outline is destroyed

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class);

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been modified

   procedure File_Closed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been closed

   procedure File_Edited
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been edited

   procedure Set_File
     (Outline : access Outline_View_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File);
   --  Set the file viewed in Outline

   --------------
   -- Tooltips --
   --------------

   type Outline_View_Tooltips is new Tooltips.Tooltips with record
      Outline : Outline_View_Access;
   end record;
   type Outline_View_Tooltips_Access is access all Outline_View_Tooltips;
   overriding function Create_Contents
     (Tooltip  : not null access Outline_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   ------------------------
   -- Outline_Tree Model --
   ------------------------

   function Get_Outline_Model
     (View : Outline_View_Access) return Outline_Model;
   --  Return the outline model stored in this view

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Outline_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Iter     : Gtk_Tree_Iter;
      Area     : Gdk_Rectangle;
   begin
      Initialize_Tooltips (Tooltip.Outline.Tree, X, Y, Area, Iter);

      if Iter /= Null_Iter then
         Tooltip.Set_Tip_Area (Area);
         declare
            SN : constant Semantic_Node'Class :=
              Tooltip.Outline.Kernel.Get_Abstract_Tree_For_File
                (Tooltip.Outline.File).Node_At
              (Get_Info
                 (Get_Outline_Model (Tooltip.Outline), Iter).Sloc_Start);
         begin
            if SN /= No_Semantic_Node then
               return Entities_Tooltips.Draw_Tooltip
                 (Kernel      => Tooltip.Outline.Kernel,
                  Draw_Border => True,
                  Entity      => SN);
            end if;
         end;
      end if;
      return null;
   end Create_Contents;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
      Model   : Outline_Model;
      Path    : Gtk_Tree_Path;
      Loc     : File_Location_Hooks_Args_Access;
   begin
      if Get_History (Get_History (Kernel).all, Hist_Editor_Link)
        and then Outline /= null
      then
         Model   := Get_Outline_Model (Outline);
         Unselect_All (Get_Selection (Outline.Tree));

         if Model = null then
            return;
         end if;

         Loc := File_Location_Hooks_Args_Access (Data);

         Path := Get_Path_Enclosing_Location (Model, Loc.Line, Loc.Column);

         if Get_Depth (Path) >= 1 then
            declare
               Indices     : constant Glib.Gint_Array := Get_Indices (Path);
               Parent_Path : Gtk_Tree_Path;
            begin
               Gtk_New (Parent_Path);
               for J in Indices'First .. Indices'Last - 1 loop
                  Append_Index (Parent_Path, Indices (J));
               end loop;

               Expand_To_Path (Outline.Tree, Parent_Path);
               Path_Free (Parent_Path);
            end;

            Set_Cursor (Outline.Tree, Path, null, False);
         end if;

         Path_Free (Path);
      end if;
   end Location_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null then
         Set_Font_And_Colors
           (Outline.Tree, Fixed_Font => True, Pref => Get_Pref (Data));
      end if;
   end Preferences_Changed;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Outline_View_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject)
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.View_From_Widget (Child);
   begin
      Outline_Context_Factory
        (Context      => Context,
         Kernel       => Get_Kernel (Module.all),
         Event_Widget => Outline.Tree,
         Object       => Outline,
         Event        => null,
         Menu         => null);
   end Default_Context_Factory;

   -----------------------------
   -- Outline_Context_Factory --
   -----------------------------

   procedure Outline_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget, Menu, Kernel);
      Outline   : constant Outline_View_Access := Outline_View_Access (Object);
      Model     : constant Outline_Model :=
        Outline_Model (-Get_Model (Outline.Tree));
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      Line      : Integer := 1;
      Node_Info : Semantic_Node_Info;
   begin
      Iter := Find_Iter_For_Event (Outline.Tree, Event);

      Set_File_Information
        (Context => Context,
         Project => No_Project,
         Files   => (1 => Outline.File),
         Line    => Line);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         if not Path_Is_Selected (Get_Selection (Outline.Tree), Path) then
            Set_Cursor (Outline.Tree, Path, null, False);
         end if;
         Path_Free (Path);

         Node_Info := Get_Info (Model, Iter);

         Set_Entity_Information
           (Context       => Context,
            Entity_Name   => Get (Node_Info.Name).all,
            Entity_Column =>
              Node_Info.Sloc_Start.Column);

         Line := Node_Info.Sloc_Start.Line;
      end if;
   end Outline_Context_Factory;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Outline_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "outline",
         Tooltip     => -"Filter the contents of the outline view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Outline_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Check    : Gtk_Check_Menu_Item;
      Sep      : Gtk_Menu_Item;
   begin
      Gtk_New (Check, Label => -"Show profiles");
      Associate (Get_History (View.Kernel).all, Hist_Show_Profile, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Show types");
      Associate (Get_History (View.Kernel).all, Hist_Show_Types, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Show objects");
      Associate (Get_History (View.Kernel).all, Hist_Show_Objects, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Show tasks, entries, and protected types");
      Associate (Get_History (View.Kernel).all, Hist_Show_Tasks, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Show specifications");
      Associate (Get_History (View.Kernel).all, Hist_Show_Decls, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Show with clauses");
      Associate (Get_History (View.Kernel).all, Hist_Show_With, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Sep);
      Menu.Append (Sep);

      Gtk_New (Check, Label => -"Sort alphabetically");
      Associate (Get_History (View.Kernel).all, Hist_Sort_Alphabetical, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Flat view");
      Associate (Get_History (View.Kernel).all, Hist_Flat_View, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Check, Label => -"Group spec and body");
      Associate
        (Get_History (View.Kernel).all, Hist_Group_Spec_And_Body, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);

      Gtk_New (Sep);
      Menu.Append (Sep);

      Gtk_New (Check, Label => -"Dynamic link with editor");
      Associate (Get_History (View.Kernel).all, Hist_Editor_Link, Check);
      Menu.Append (Check);
      Widget_Callback.Object_Connect
        (Check, Signal_Toggled, Force_Refresh'Access, View);
   end Create_Menu;

   -----------------------
   -- Get_Filter_Record --
   -----------------------

   function Get_Filter_Record
     (Kernel : access Kernel_Handle_Record'Class) return Tree_Filter is
   begin
      return
        (Hide_Types        => not Get_History
           (Get_History (Kernel).all, Hist_Show_Types),
         Hide_Objects      => not Get_History
           (Get_History (Kernel).all, Hist_Show_Objects),
         Hide_Declarations => not Get_History
           (Get_History (Kernel).all, Hist_Show_Decls),
         Hide_Tasks => not Get_History
           (Get_History (Kernel).all, Hist_Show_Tasks),
         Hide_Withes => not Get_History
           (Get_History (Kernel).all, Hist_Show_With),
         Show_Profile => Get_History
           (Get_History (Kernel).all, Hist_Show_Profile),
         Sorted       => Get_History
           (Get_History (Kernel).all, Hist_Sort_Alphabetical),
         Group_Spec_And_Body => Get_History
           (Get_History (Kernel).all, Hist_Group_Spec_And_Body),
         Flat_View           => Get_History
           (Get_History (Kernel).all, Hist_Flat_View));
   end Get_Filter_Record;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Outline : access GObject_Record'Class;
      Event   : Gdk_Event_Button) return Boolean
   is
      View                : constant Outline_View_Access :=
                              Outline_View_Access (Outline);
      Model : constant Outline_Model := Outline_Model (-Get_Model (View.Tree));
      Iter                : Gtk_Tree_Iter;
      Path                : Gtk_Tree_Path;
      Col                 : Gtk_Tree_View_Column;

      procedure Goto_Node (Node : Semantic_Node_Info; Is_Spec : Boolean);
      --  goto and highlight the entity

      procedure Goto_Node (Node : Semantic_Node_Info; Is_Spec : Boolean) is
      begin
         if Node = No_Node_Info then
            return;
         end if;

         declare
            use type Visible_Column_Type;
            Buffer   : constant Editor_Buffer'Class :=
              Get (Get_Buffer_Factory (View.Kernel).all,
                   View.File, False, False, False);
            HSD : constant Boolean := Node.Sloc_Def.Index /= 0;
            Location : constant Editor_Location'Class :=
              (if HSD
               then New_Location (Buffer, Node.Sloc_Def.Line,
                                  Node.Sloc_Def.Column)
               else New_Location
                 (Buffer, Node.Sloc_Start.Line,
                  Node.Sloc_Start.Column));

            End_Location : constant Editor_Location'Class :=
              (if HSD
               then New_Location (Buffer, Node.Sloc_Def.Line,
                 Node.Sloc_Def.Column
                 + Get (Node.Name)'Length)
               else Location);
            Editor : constant Editor_View'Class := Current_View (Buffer);
         begin
            if Col /= View.Spec_Column
              and then Col /= View.Body_Column
              and then Is_Spec
              and then Location.Line = Editor.Cursor.Line
            then
               --  Clicking on the name will jump to the spec, unless this is
               --  already the current location
               Goto_Node (Get_Info (Model, Iter, Body_Pixbuf_Column),
                            Is_Spec => False);
            else
               Editor.Cursor_Goto (Location, Raise_View => True);
               Select_Text (Buffer, Location, End_Location);
            end if;
         end;
      end Goto_Node;

   begin
      if Event.Button = 1 then
         if View.File = No_File then
            return False;
         end if;

         Coordinates_For_Event
           (Tree   => View.Tree,
            Event  => Event,
            Iter   => Iter,
            Column => Col);

         if Col = View.Tree.Get_Expander_Column then
            --  Verify that we are not clicking on an expander. If this is the
            --  case, let the click through.

            declare
               Path : Gtk_Tree_Path;
               Rect : Gdk_Rectangle;
            begin
               Path := Model.Get_Path (Iter);

               View.Tree.Get_Cell_Area (Path   => Path,
                                        Column => Col,
                                        Rect   => Rect);

               Path_Free (Path);

               if Rect.X > Gint (Event.X) then
                  return False;
               end if;
            end;
         end if;

         if Iter /= Null_Iter then
            Path := Get_Path (Model, Iter);
            Set_Cursor (View.Tree, Path, null, False);
            Path_Free (Path);

            if Col = View.Body_Column then
               Goto_Node (Get_Info (Model, Iter, Body_Pixbuf_Column),
                            Is_Spec => False);
            else
               Goto_Node (Get_Info (Model, Iter), Is_Spec => True);
            end if;
            return True;
         end if;
      end if;
      return False;
   end Button_Press;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Outline_View_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      Get_Outline_Model (Self).Set_Filter (Pattern);
      Force_Refresh (Self);
   end Filter_Changed;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Outline : access Outline_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Text_Col      : Gtk_Tree_View_Column;
      Col_Number    : Gint;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Pixbuf_Render : Gtk_Cell_Renderer_Pixbuf;
      Tooltip       : Outline_View_Tooltips_Access;
      Scrolled      : Gtk_Scrolled_Window;
      Data          : aliased Context_Hooks_Args;

      pragma Unreferenced (Col_Number);
      Out_Model : Outline_Model;

   begin
      Initialize_Vbox (Outline);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Outline.Pack_Start (Scrolled, Expand => True, Fill => True);

      --  Create the tree view using the sorting model

      Out_Model := new Outline_Model_Record;
      Gtkada.Abstract_Tree_Model.Initialize (Out_Model);
      Gtk_New (Outline.Tree, Out_Model);
      Unref (Out_Model);
      Set_Name (Outline.Tree, "Outline View Tree");  --  For testsuite

      Set_Headers_Visible (Outline.Tree, False);

      --  Create an explicit columns for the expander

      Gtk_New (Outline.Spec_Column);
      Outline.Spec_Column.Set_Sizing (Tree_View_Column_Autosize);
      Col_Number := Append_Column (Outline.Tree, Outline.Spec_Column);
      Gtk_New (Pixbuf_Render);
      Pack_Start (Outline.Spec_Column, Pixbuf_Render, False);
      Add_Attribute
        (Outline.Spec_Column, Pixbuf_Render, "stock-id", Spec_Pixbuf_Column);

      Gtk_New (Outline.Body_Column);
      Outline.Body_Column.Set_Sizing (Tree_View_Column_Autosize);
      Col_Number := Append_Column (Outline.Tree, Outline.Body_Column);
      Gtk_New (Pixbuf_Render);
      Pack_Start (Outline.Body_Column, Pixbuf_Render, False);
      Add_Attribute
        (Outline.Body_Column, Pixbuf_Render, "stock-id", Body_Pixbuf_Column);

      Gtk_New (Text_Col);
      Col_Number := Append_Column (Outline.Tree, Text_Col);
      Gtk_New (Text_Render);
      Pack_Start (Text_Col, Text_Render, False);
      Add_Attribute
        (Text_Col, Text_Render,
         "markup", Outline_View.Model.Display_Name_Column);
      Clicked (Text_Col);

      Scrolled.Add (Outline.Tree);

      Outline.Icon := Render_Icon
        (Get_Main_Window (Outline.Kernel), "gps-box", Icon_Size_Menu);
      Outline.File_Icon := Render_Icon
        (Get_Main_Window (Outline.Kernel), "gps-file", Icon_Size_Menu);

      Set_Font_And_Colors (Outline.Tree, Fixed_Font => True);

      Outline.Tree.On_Button_Press_Event (Button_Press'Access, Outline);

      Widget_Callback.Connect
        (Outline, Signal_Destroy, On_Destroy'Access);

      Register_Contextual_Menu
        (Kernel          => Outline.Kernel,
         Event_On_Widget => Outline.Tree,
         Object          => Outline,
         ID              => Outline_View_Module,
         Context_Func    => Outline_Context_Factory'Access);

      Tooltip := new Outline_View_Tooltips;
      Tooltip.Outline := Outline;
      Set_Tooltip (Tooltip, Outline.Tree);

      Data := Context_Hooks_Args'
        (Hooks_Data with Context => Get_Current_Context (Outline.Kernel));

      On_Context_Changed (Outline.Kernel, Data'Unchecked_Access);

      Add_Hook (Outline.Kernel, Context_Changed_Hook,
                Wrapper (On_Context_Changed'Access),
                Name => "outline.context_changed",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, Preference_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "outline.preferences_changed",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, Location_Changed_Hook,
                Wrapper (Location_Changed'Access),
                Name  => "outline.location_changed",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, File_Saved_Hook,
                Wrapper (File_Saved'Access),
                Name  => "outline.file_saved",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, File_Closed_Hook,
                Wrapper (File_Closed'Access),
                Name  => "outline.file_closed",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, File_Edited_Hook,
                Wrapper (File_Edited'Access),
                Name  => "outline.file_edited",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, Buffer_Modified_Hook,
                Wrapper (File_Saved'Access),
                Name  => "outline.file_modified",
                Watch => GObject (Outline));
      Add_Hook (Outline.Kernel, Project_View_Changed_Hook,
                Wrapper (On_Project_Changed'Access),
                Name => "outline.projet_changed",
                Watch => GObject (Outline));

      return Gtk_Widget (Outline.Tree);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class)
   is
      O     : constant Outline_View_Access := Outline_View_Access (Outline);
      Model : constant Outline_Model := Get_Outline_Model (O);
   begin

      if Model /= null then
         Model.Free;
      end if;

      Unref (O.Icon);
      Unref (O.File_Icon);
   end On_Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Gtk_Widget_Record'Class) is
      Outline : constant Outline_View_Access := Outline_View_Access (View);
   begin
      if Outline.File /= No_File then
         Outline.Kernel.Get_Abstract_Tree_For_File (Outline.File).Update;
         File_Updated (Get_Outline_Model (View));
      end if;
   end Refresh;

   -------------------
   -- Force_Refresh --
   -------------------

   procedure Force_Refresh (View : access Gtk_Widget_Record'Class) is
      Outline : constant Outline_View_Access := Outline_View_Access (View);
   begin
      if Outline.File /= No_File then
         Set_File (Outline, Outline.File);
      end if;
   end Force_Refresh;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Outline.File = D.File then
         Refresh (Outline);
      end if;
   end File_Saved;

   -----------------
   -- File_Closed --
   -----------------

   procedure File_Closed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Outline.File = D.File then
         Outline.Set_File (GNATCOLL.VFS.No_File);
         Refresh (Outline);
      end if;
   end File_Closed;

   -----------------
   -- File_Edited --
   -----------------

   procedure File_Edited
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null then
         if Outline.File = GNATCOLL.VFS.No_File then
            Outline.Set_File (D.File);
         end if;

         Refresh (Outline);
      end if;
   end File_Edited;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      type Context_Args is access all Context_Hooks_Args'Class;
      D       : constant Context_Args := Context_Args (Data);
      Module  : constant Module_ID := Module_ID (Get_Creator (D.Context));
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
      File    : Virtual_File;
   begin
      if Outline /= null
        and then Module /= null
        and then
          (Get_Name (Module) = "Source_Editor"
           or else Get_Name (Module) = Outline_View_Module_Name)
      then
         if Has_File_Information (D.Context) then
            File := File_Information (D.Context);
         else
            File := GNATCOLL.VFS.No_File;
         end if;

         if File /= Outline.File then
            Outline.Set_File (File);
            Refresh (Outline);
         elsif Outline.File = GNATCOLL.VFS.No_File then
            Refresh (Outline);
         end if;
      end if;
   end On_Context_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Outline_View_Module := new Outline_View_Module_Record;
      Outline_Views.Register_Module (Kernel, Outline_View_Module);

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Show_Profile, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Sort_Alphabetical, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Editor_Link, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Show_Decls, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Show_Types, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Show_Objects, True);
      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, Hist_Flat_View, False);
   end Register_Module;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Outline : access Outline_View_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File)
   is
      Model       : Outline_Model;
      Filter      : constant Tree_Filter :=
        Get_Filter_Record (Outline.Kernel);
      Tree : constant Semantic_Tree'Class :=
        Outline.Kernel.Get_Abstract_Tree_For_File (File);
   begin
      Outline.File := File;
      Model := Get_Outline_Model (Outline_View_Access (Outline));

      if Model = null then
         Model := new Outline_Model_Record;
         Gtkada.Abstract_Tree_Model.Initialize (Model);
         Outline.Tree.Set_Model (To_Interface (Model));
      end if;

      --  This function is called directly after the settings have changed,
      --  and should take their new value into account.

      Outline.Tree.Set_Show_Expanders (Enabled => not Filter.Flat_View);
      Outline.Body_Column.Set_Visible (Filter.Group_Spec_And_Body);

      Model.Set_Tree (Tree, Filter);

      if Tree /= No_Semantic_Tree then
         declare
            Path : Gtk_Tree_Path;
         begin
            Gtk_New (Path);
            Append_Index (Path, 0);
            Expand_To_Path (Outline.Tree, Path);
            Path_Free (Path);
         end;
      end if;
   end Set_File;

   -----------------------
   -- Get_Outline_Model --
   -----------------------

   function Get_Outline_Model
     (View : Outline_View_Access) return Outline_Model
   is
      Model : constant Gtk_Root_Tree_Model :=
        -Get_Model (View.Tree);
   begin
      if Model = null then
         return null;
      elsif Model.all in Outline_Model_Record'Class then
         return Outline_Model (Model);
      else
         return null;
      end if;
   end Get_Outline_Model;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed (Kernel : access Kernel_Handle_Record'Class) is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Outline.File /= No_File then
         --  We need to force the set of the file here. In certain cases,
         --  the outline is computed to early and work on an unknown
         --  language, which is then set when the project is loaded.

         Set_File (Outline, Outline.File);
      end if;
   end On_Project_Changed;

end Outline_View;
