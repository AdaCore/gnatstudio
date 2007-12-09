-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2007, AdaCore                  --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Rectangle;             use Gdk.Rectangle;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Unicode;              use Glib.Unicode;
with Glib.Xml_Int;              use Glib.Xml_Int;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Model_Sort;       use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Sortable;         use Gtk.Tree_Sortable;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with Entities.Tooltips;         use Entities.Tooltips;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                use GPS.Kernel;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Project_Explorers_Common;  use Project_Explorers_Common;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with Tooltips;                  use Tooltips;
with VFS;                       use VFS;

package body Outline_View is

   type Outline_View_Module_Record is new Module_ID_Record with null record;
   Outline_View_Module : Module_ID;
   Outline_View_Module_Name : constant String := "Outline_View";

   Hist_Show_Profile      : constant History_Key := "outline-show-profile";
   Hist_Sort_Alphabetical : constant History_Key := "outline-alphabetical";
   Hist_Editor_Link       : constant History_Key := "outline-editor-link";
   Hist_Show_Decls        : constant History_Key := "outline-show-decls";
   Hist_Show_Types        : constant History_Key := "outline-show-types";

   Pixbuf_Column       : constant := 0;
   Display_Name_Column : constant := 1;
   Entity_Name_Column  : constant := 2;
   Mark_Column         : constant := 3;
   Line_Column         : constant := 4;
   Declaration_Column  : constant := 5;

   procedure Default_Context_Factory
     (Module  : access Outline_View_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget);
   --  See inherited documentation

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the context has changed

   function Open_Outline
     (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child;
   --  Open the outline view, or return a handle to it if it already exists.

   procedure On_Open_Outline
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Raise the existing explorer, or open a new one.

   type Outline_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Kernel    : Kernel_Handle;
      Tree      : Gtk_Tree_View;
      File      : VFS.Virtual_File;
      Icon      : Gdk_Pixbuf;
      File_Icon : Gdk_Pixbuf;
   end record;
   type Outline_View_Access is access all Outline_View_Record'Class;

   procedure Gtk_New
     (Outline : out Outline_View_Access;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Create a new outline view

   type Refresh_Outline_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Refresh_Outline_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See inherited documentation

   procedure Refresh (View : access Gtk_Widget_Record'Class);
   --  Recompute the information for Outline.File, and redisplay it.
   --  Change Outline.File first if needed

   function Button_Press
     (Outline : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Handling of desktops

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  React to changes in the preferences

   function Category_Filter
     (Category : Language.Language_Category) return Boolean;
   --  Return False if the category should be filtered out

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
   --  Called when the outline is destroye

   procedure Clear (Outline : access Outline_View_Record'Class);
   --  Clear the contents of the outline view, and reset all marks

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been saved

   procedure File_Closed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been closed

   procedure File_Edited
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when a file has been edited

   procedure Entity_At_Iter
     (Outline : access Outline_View_Record'Class;
      Iter    : Gtk_Tree_Iter;
      Line    : out Integer;
      Column  : out Visible_Column_Type);
   --  Return the current coordinates for the entity referenced at Iter

   function Outline_View_Sort
     (Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Outline view sorting routine

   function Get_Base_Model
     (Tree_View : access Gtk_Tree_View_Record'Class) return Gtk_Tree_Model;
   pragma Inline (Get_Base_Model);
   --  Return the base model for the tree view, the top-level model is a
   --  Gtk_Tree_Model_Sort, the base model is the Gtk_Tree_Store.

   --------------
   -- Tooltips --
   --------------

   type Outline_View_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Outline : Outline_View_Access;
   end record;
   type Outline_View_Tooltips_Access is access all Outline_View_Tooltips;
   procedure Draw
     (Tooltip : access Outline_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);

   --------------------
   -- Entity_At_Iter --
   --------------------

   procedure Entity_At_Iter
     (Outline : access Outline_View_Record'Class;
      Iter    : Gtk_Tree_Iter;
      Line    : out Integer;
      Column  : out Visible_Column_Type)
   is
      Model     : constant Gtk_Tree_Store :=
                    Gtk_Tree_Store (Get_Base_Model (Outline.Tree));
      Mark_Name : aliased String := Get_String (Model, Iter, Mark_Column);
      Args      : constant Argument_List := (1 => Mark_Name'Unchecked_Access);
   begin
      Line := Safe_Value
        (Execute_GPS_Shell_Command (Outline.Kernel, "Editor.get_line", Args));
      Column := Visible_Column_Type
        (Safe_Value
           (Execute_GPS_Shell_Command
              (Outline.Kernel, "Editor.get_column", Args)));
   end Entity_At_Iter;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access Outline_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Model   : constant Gtk_Tree_Store :=
                  Gtk_Tree_Store (Get_Base_Model (Tooltip.Outline.Tree));
      Status  : Find_Decl_Or_Body_Query_Status := Success;
      Entity  : Entity_Information;
      Iter    : Gtk_Tree_Iter;
      Line    : Integer;
      Column  : Visible_Column_Type;
      Closest : Entity_Reference;
   begin
      Pixmap := null;
      Initialize_Tooltips (Tooltip.Outline.Tree, Area, Iter);

      if Iter /= Null_Iter then
         Convert_Iter_To_Child_Iter
           (Gtk_Tree_Model_Sort
              (Get_Model (Tooltip.Outline.Tree)), Iter, Iter);
         Entity_At_Iter
           (Outline => Tooltip.Outline,
            Iter    => Iter,
            Line    => Line,
            Column  => Column);
         Find_Declaration_Or_Overloaded
           (Kernel            => Tooltip.Outline.Kernel,
            File              => Get_Or_Create
              (Db     => Get_Database (Tooltip.Outline.Kernel),
               File   => Tooltip.Outline.File),
            Entity_Name       => Get_String (Model, Iter, Entity_Name_Column),
            Line              => Line,
            Column            => Column,
            Ask_If_Overloaded => False,
            Entity            => Entity,
            Closest_Ref       => Closest,
            Status            => Status);

         if Entity /= null then
            Ref (Entity);
            Pixmap := Entities.Tooltips.Draw_Tooltip
              (Kernel => Tooltip.Outline.Kernel,
               Entity => Entity,
               Ref    => No_Entity_Reference,
               Status => Status);
            Unref (Entity);
         end if;
      end if;
   end Draw;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Child       : constant MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);
      Editor_Line : constant Natural :=
                      File_Location_Hooks_Args_Access (Data).Line;
      Outline     : Outline_View_Access;
      Iter        : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Store;
      Path        : Gtk_Tree_Path;
      Subprogram  : Entity_Information;
      Distance    : Gint := Gint'Last;
      Closest     : Gtk_Tree_Iter;
   begin
      if Get_History (Get_History (Kernel).all, Hist_Editor_Link)
        and then Child /= null
      then
         Outline := Outline_View_Access (Get_Widget (Child));
         Model   := Gtk_Tree_Store (Get_Base_Model (Outline.Tree));
         Unselect_All (Get_Selection (Outline.Tree));

         Subprogram := Compute_Parent_Entity
           (Kernel, File_Location_Hooks_Args_Access (Data));

         if Subprogram /= null then
            declare
               Subprogram_Name : constant String := Get_Name (Subprogram).all;
               Handler         : constant LI_Handler := Get_LI_Handler
                 (Get_Database (Kernel),
                  Source_Filename =>
                    File_Location_Hooks_Args_Access (Data).File);

               --  If no LI handler is found, assume case sensitivity

               Case_Insensitive : constant Boolean :=
                                    (Handler /= null
                                       and then
                                     Case_Insensitive_Identifiers (Handler));
               Line : Gint := Gint'First;
               Loc  : File_Location;
            begin
               --  Find the relevant line for the entity, so that the outline
               --  view can handle overloaded entities at least approximately.
               --  We need to know whether we should look at the spec or at one
               --  of the bodies for the entity.
               Loc := Get_Declaration_Of (Subprogram);
               if Get_Filename (Loc.File) = Outline.File then
                  Line := Gint (Loc.Line);
               end if;

               --  Check whether the body is closer
               Loc := Entities.No_File_Location;
               loop
                  Find_Next_Body
                    (Subprogram,
                     Current_Location     => Loc,
                     Location             => Loc,
                     No_Location_If_First => True);
                  exit when Loc = Entities.No_File_Location;

                  if Get_Filename (Loc.File) = Outline.File then
                     if abs (Loc.Line - Editor_Line) <
                        abs (Integer (Line) - Editor_Line)
                     then
                        Line := Gint (Loc.Line);
                     end if;
                  end if;
               end loop;

               --  Next find all occurrences for entities with the same name,
               --  and select the closest one to the current line

               Iter := Get_Iter_First (Model);

               while Iter /= Null_Iter loop
                  if Equal
                    (Get_String (Model, Iter, Entity_Name_Column),
                     Subprogram_Name,
                     Case_Sensitive => not Case_Insensitive)
                  then
                     if abs (Get_Int (Model, Iter, Line_Column) - Line) <
                       Distance
                     then
                        Iter_Copy (Iter, Closest);
                        Distance :=
                          abs (Get_Int (Model, Iter, Line_Column) - Line);
                     end if;
                  end if;

                  Next (Model, Iter);
               end loop;

               if Distance /= Gint'Last then
                  Path := Get_Path (Model, Closest);
                  Path := Convert_Child_Path_To_Path
                    (Gtk_Tree_Model_Sort (Get_Model (Outline.Tree)), Path);
                  Set_Cursor (Outline.Tree, Path, null, False);
                  Path_Free (Path);
               end if;
            end;
         end if;
      end if;
   end Location_Changed;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Child   : constant MDI_Child :=
                  Find_MDI_Child_By_Tag
                    (Get_MDI (Kernel), Outline_View_Record'Tag);
      Outline : Outline_View_Access;
   begin
      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));
         Modify_Font (Outline.Tree, Get_Pref (View_Fixed_Font));
      end if;
   end Preferences_Changed;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   procedure Default_Context_Factory
     (Module  : access Outline_View_Module_Record;
      Context : in out Selection_Context;
      Child   : Gtk.Widget.Gtk_Widget)
   is
      Outline : constant Outline_View_Access := Outline_View_Access (Child);
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
      pragma Unreferenced (Event_Widget);
      Outline : constant Outline_View_Access := Outline_View_Access (Object);
      Model   : constant Gtk_Tree_Model := Get_Model (Outline.Tree);
      B_Model : constant Gtk_Tree_Store :=
                  Gtk_Tree_Store (Get_Base_Model (Outline.Tree));
      Path    : Gtk_Tree_Path;
      Iter    : Gtk_Tree_Iter;
      Line    : Integer;
      Column  : Visible_Column_Type;
      Check   : Gtk_Check_Menu_Item;
      Item    : Gtk_Menu_Item;
      Sep     : Gtk_Menu_Item;
      Submenu : Gtk_Menu;
   begin
      Iter := Find_Iter_For_Event (Outline.Tree, Model, Event);

      if Iter /= Null_Iter then
         Convert_Iter_To_Child_Iter (Gtk_Tree_Model_Sort (Model), Iter, Iter);
         Path := Get_Path (B_Model, Iter);
         if not Path_Is_Selected (Get_Selection (Outline.Tree), Path) then
            Set_Cursor (Outline.Tree, Path, null, False);
         end if;
         Path_Free (Path);

         Entity_At_Iter
           (Outline => Outline,
            Iter    => Iter,
            Line    => Line,
            Column  => Column);
         Set_Entity_Information
           (Context       => Context,
            Entity_Name   => Get_String (B_Model, Iter, Entity_Name_Column),
            Entity_Column => Column);
      end if;

      Set_File_Information
        (Context => Context,
         Project => Projects.No_Project,
         File    => Outline.File,
         Line    => Line);

      if Menu /= null then
         Gtk_New (Item, Label => -"Outline");
         Append (Menu, Item);
         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Submenu);
         Set_Submenu (Item, Submenu);

         Gtk_New (Check, Label => -"Show profiles");
         Associate (Get_History (Kernel).all, Hist_Show_Profile, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Show types");
         Associate (Get_History (Kernel).all, Hist_Show_Types, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Show specifications");
         Associate (Get_History (Kernel).all, Hist_Show_Decls, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Sort alphabetically");
         Associate (Get_History (Kernel).all, Hist_Sort_Alphabetical, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Dynamic link with editor");
         Associate (Get_History (Kernel).all, Hist_Editor_Link, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh'Access, Outline);
      end if;
   end Outline_Context_Factory;

   ---------------------
   -- Category_Filter --
   ---------------------

   function Category_Filter
     (Category : Language_Category) return Boolean is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      case Category is
         when Subprogram_Explorer_Category |
              Cat_Package .. Cat_Task |
              Type_Category =>
            return True;

         when others =>
            return False;
      end case;
   end Category_Filter;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle) return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Outline_View_Record'Class then
         N := new Node;
         N.Tag := new String'("Outline_View");
         return N;
      end if;
      return null;
   end Save_Desktop;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "Outline_View" then
         return Open_Outline (User);
      end if;
      return null;
   end Load_Desktop;

   --------------------
   -- Get_Base_Model --
   --------------------

   function Get_Base_Model
     (Tree_View : access Gtk_Tree_View_Record'Class) return Gtk_Tree_Model
   is
      Model : constant Gtk_Tree_Model := Get_Model (Tree_View);
   begin
      if Model.all in Gtk_Tree_Model_Sort_Record then
         return Get_Model (Gtk_Tree_Model_Sort (Model));
      else
         return Model;
      end if;
   end Get_Base_Model;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Outline : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean
   is
      View  : constant Outline_View_Access := Outline_View_Access (Outline);
      Model : constant Gtk_Tree_Model := Get_Model (View.Tree);
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
   begin
      if Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (View.Tree, Model, Event);
         if Iter /= Null_Iter then
            Path := Get_Path (Model, Iter);
            Set_Cursor (View.Tree, Path, null, False);
            Path_Free (Path);

            declare
               Mark_Name : aliased String :=
                             Get_String (Model, Iter, Mark_Column);
               Args      : constant Argument_List :=
                             (1 => Mark_Name'Unchecked_Access);
            begin
               Execute_GPS_Shell_Command
                 (View.Kernel, "Editor.goto_mark", Args);
            end;

            return True;
         end if;
      end if;
      return False;
   end Button_Press;

   -----------------------
   -- Outline_View_Sort --
   -----------------------

   function Outline_View_Sort
     (Model : access Gtk_Tree_Model_Record'Class;
      A     : Gtk_Tree_Iter;
      B     : Gtk_Tree_Iter) return Gint
   is
      Order  : constant array (Boolean) of Gint := (1, 0);
      A_Name : constant String :=
                 Get_String (Model, A, Entity_Name_Column);
      B_Name : constant String :=
                 Get_String (Model, B, Entity_Name_Column);
      A_Decl : constant Boolean :=
                 Get_Boolean (Model, A, Declaration_Column);
      B_Decl : constant Boolean :=
                 Get_Boolean (Model, B, Declaration_Column);
   begin
      --  If names are identical we want to have the declarations first then
      --  the bodies.

      if A_Name = B_Name then
         return Order (A_Decl > B_Decl);
      else
         return Order (A_Name < B_Name);
      end if;
   end Outline_View_Sort;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Outline : out Outline_View_Access;
      Kernel  : access Kernel_Handle_Record'Class)
   is
      Scrolled      : Gtk_Scrolled_Window;
      Model         : Gtk_Tree_Store;
      Sort_Model    : Gtk_Tree_Model_Sort;

      Col           : Gtk_Tree_View_Column;
      Col_Number    : Gint;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Pixbuf_Render : Gtk_Cell_Renderer_Pixbuf;
      Tooltip       : Outline_View_Tooltips_Access;

      pragma Unreferenced (Col_Number);
   begin
      Outline := new Outline_View_Record;
      Outline.Kernel := Kernel_Handle (Kernel);

      Init_Graphics (Gtk_Widget (Get_Main_Window (Kernel)));

      Initialize_Vbox (Outline, Homogeneous => False);

      Gtk_New (Scrolled);
      Pack_Start (Outline, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      --  Create the tree view model

      Gtk.Tree_Store.Gtk_New
        (Model,
         Types =>
           (Pixbuf_Column       => Gdk.Pixbuf.Get_Type,
            Display_Name_Column => GType_String,
            Entity_Name_Column  => GType_String,
            Mark_Column         => GType_String, --  mark ID
            Line_Column         => GType_Int,
            Declaration_Column  => GType_Boolean));

      --  Stack a Sorting model on top of it

      Gtk_New_With_Model (Sort_Model, Gtk_Tree_Model (Model));

      --  Create the tree view using the sorting model

      Gtk_New (Outline.Tree, Sort_Model);
      Set_Name (Outline.Tree, "Outline View Tree");  --  For testsuite

      --  Setup the sorting support

      Set_Sort_Column_Id (+Sort_Model, Entity_Name_Column, Sort_Ascending);
      Set_Sort_Func
        (+Sort_Model, Entity_Name_Column, Outline_View_Sort'Access);

      Set_Headers_Visible (Outline.Tree, False);

      --  Create an explicit columns for the expander

      Gtk_New (Col);
      Col_Number := Append_Column (Outline.Tree, Col);
      Set_Expander_Column (Outline.Tree, Col);
      Set_Visible (Col, False);

      Gtk_New (Col);
      Col_Number := Append_Column (Outline.Tree, Col);
      Gtk_New (Pixbuf_Render);
      Pack_Start (Col, Pixbuf_Render, False);
      Add_Attribute (Col, Pixbuf_Render, "pixbuf", Pixbuf_Column);

      Gtk_New (Col);
      Col_Number := Append_Column (Outline.Tree, Col);
      Gtk_New (Text_Render);
      Pack_Start (Col, Text_Render, False);
      Add_Attribute (Col, Text_Render, "markup", Display_Name_Column);
      Clicked (Col);

      Add (Scrolled, Outline.Tree);

      Outline.Icon := Render_Icon
        (Get_Main_Window (Kernel), "gps-box", Icon_Size_Menu);
      Outline.File_Icon := Render_Icon
        (Get_Main_Window (Kernel), "gps-file", Icon_Size_Menu);

      Modify_Font (Outline.Tree, Get_Pref (View_Fixed_Font));

      Return_Callback.Object_Connect
        (Outline.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => Outline,
         After       => False);
      Widget_Callback.Connect
        (Outline, Signal_Destroy, On_Destroy'Access);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Outline.Tree,
         Object          => Outline,
         ID              => Outline_View_Module,
         Context_Func    => Outline_Context_Factory'Access);

      Tooltip := new Outline_View_Tooltips;
      Tooltip.Outline := Outline;
      Set_Tooltip (Tooltip, Outline.Tree);
   end Gtk_New;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class)
   is
      O : constant Outline_View_Access := Outline_View_Access (Outline);
   begin
      Clear (O);

      Unref (O.Icon);
      Unref (O.File_Icon);
   end On_Destroy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Refresh_Outline_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Child   : constant MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Get_Kernel (Context.Context)), Outline_View_Record'Tag);
      Outline : Outline_View_Access;
   begin
      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));
         Refresh (Outline);
         return Success;
      end if;
      return Failure;
   end Execute;

   -----------
   -- Clear --
   -----------

   procedure Clear (Outline : access Outline_View_Record'Class) is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Base_Model (Outline.Tree));
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Model);
      Args  : Argument_List (1 .. 1);

   begin
      while Iter /= Null_Iter loop
         declare
            Name : aliased String := Get_String (Model, Iter, Mark_Column);
         begin
            if Name /= "" then
               Args (1) := Name'Unchecked_Access;
               Execute_GPS_Shell_Command
                 (Outline.Kernel, "Editor.delete_mark", Args);
            end if;
         end;
         Next (Model, Iter);
      end loop;

      Clear (Model);
   end Clear;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Gtk_Widget_Record'Class) is
      Outline       : constant Outline_View_Access :=
                        Outline_View_Access (View);
      Show_Specs    : constant Boolean :=
                        Get_History
                          (Get_History (Outline.Kernel).all, Hist_Show_Decls);
      Show_Types    : constant Boolean :=
                        Get_History
                          (Get_History (Outline.Kernel).all, Hist_Show_Types);
      Show_Profiles : constant Boolean :=
                        Get_History
                          (Get_History (Outline.Kernel).all,
                           Hist_Show_Profile);
      Model         : constant Gtk_Tree_Store :=
                        Gtk_Tree_Store (Get_Base_Model (Outline.Tree));
      Root          : constant Gtk_Tree_Iter := Null_Iter;
      Languages     : constant Language_Handler :=
                        Language_Handler
                          (Get_Language_Handler (Outline.Kernel));
      Sorting       : constant Boolean :=
                        Get_History
                          (Get_History (Outline.Kernel).all,
                           Hist_Sort_Alphabetical);
      Sort_Column   : constant Gint := Freeze_Sort (Model);
      pragma Unreferenced (Sort_Column);
      Iter          : Gtk_Tree_Iter := Null_Iter;
      Args          : Argument_List (1 .. 4);
      Lang          : Language_Access;
      Handler       : LI_Handler;
      Constructs    : Construct_List;
      Is_Type       : Boolean;
   begin
      Push_State (Outline.Kernel, Busy);
      Clear (Outline);

      if Sorting then
         Set_Sort_Column_Id
           (+Gtk_Tree_Model_Sort (Get_Model (Outline.Tree)),
            Entity_Name_Column, Sort_Ascending);
      else
         Set_Sort_Column_Id
           (+Gtk_Tree_Model_Sort (Get_Model (Outline.Tree)),
            Default_Sort_Column_Id, Sort_Ascending);
      end if;

      if Outline.File /= VFS.No_File then
         Handler := Get_LI_Handler_From_File (Languages, Outline.File);
         Lang := Get_Language_From_File (Languages, Outline.File);
      end if;

      if Handler = null or else Lang = null then
         Append (Model, Iter, Root);
         Set (Model, Iter, Pixbuf_Column, C_Proxy (Outline.Icon));
         Set (Model, Iter, Display_Name_Column, "<i>No outline available</i>");

      else
         Parse_File_Constructs
           (Handler, Languages, Outline.File, Constructs);
         Constructs.Current := Constructs.First;

         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               Is_Type := Constructs.Current.Category in Data_Type_Category;

               if Category_Filter (Constructs.Current.Category)
                 and then (Show_Specs or not Constructs.Current.Is_Declaration)
                 and then (Show_Types or not Is_Type)
               then
                  Append (Model, Iter, Root);

                  Set (Model, Iter, Pixbuf_Column,
                       C_Proxy (Entity_Icon_Of (Constructs.Current.all)));

                  Display_Profile : declare
                     Profile : constant String :=
                                 Entity_Name_Of
                                   (Constructs.Current.all,
                                    Show_Profiles      => Show_Profiles,
                                    Max_Profile_Length => 500);
                  begin
                     Set (Model, Iter, Display_Name_Column, Profile);
                  end Display_Profile;

                  Set (Model, Iter, Entity_Name_Column,
                       Constructs.Current.Name.all);
                  Set (Model, Iter, Line_Column,
                       Gint (Constructs.Current.Sloc_Entity.Line));

                  Set (Model, Iter, Declaration_Column,
                       Constructs.Current.Is_Declaration);

                  Args (1) := new String'(Full_Name (Outline.File).all);
                  Args (2) := new String'
                    (Constructs.Current.Sloc_Entity.Line'Img);
                  Args (3) := new String'
                    (Constructs.Current.Sloc_Entity.Column'Img);
                  Args (4) := new String'
                    (Glong'Image (UTF8_Strlen (Constructs.Current.Name.all)));

                  Set (Model, Iter, Mark_Column,
                       Execute_GPS_Shell_Command
                         (Outline.Kernel, "Editor.create_mark", Args));
                  Free (Args);
               end if;
            end if;
            Constructs.Current := Constructs.Current.Next;
         end loop;
      end if;

      Free (Constructs);

      Expand_All (Outline.Tree);

      if Sorting then
         Thaw_Sort (Model, Entity_Name_Column);
      end if;

      Pop_State (Outline.Kernel);
   end Refresh;

   ---------------------
   -- On_Open_Outline --
   ---------------------

   procedure On_Open_Outline
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Outline : MDI_Child;
   begin
      Outline := Open_Outline (Kernel);

      Raise_Child (Outline);
      Set_Focus_Child (Get_MDI (Kernel), Outline);
   end On_Open_Outline;

   ------------------
   -- Open_Outline --
   ------------------

   function Open_Outline
     (Kernel : access Kernel_Handle_Record'Class) return MDI_Child
   is
      Child   : GPS_MDI_Child;
      Outline : Outline_View_Access;
      Data    : aliased Context_Hooks_Args;
   begin
      Child := GPS_MDI_Child (Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag));

      if Child = null then
         Gtk_New (Outline, Kernel);
         Gtk_New (Child, Outline,
                  Default_Width  => 215,
                  Default_Height => 600,
                  Group          => Group_View,
                  Module         => Outline_View_Module);
         Set_Title (Child, -"Outline View", -"Outline View");
         Put (Get_MDI (Kernel), Child, Initial_Position => Position_Left);

         Data := Context_Hooks_Args'
           (Hooks_Data with Context => Get_Current_Context (Kernel));

         On_Context_Changed (Kernel, Data'Unchecked_Access);

         Add_Hook (Kernel, Context_Changed_Hook,
                   Wrapper (On_Context_Changed'Access),
                   Name => "outline.context_changed",
                   Watch => GObject (Outline));
         Add_Hook (Kernel, Preferences_Changed_Hook,
                   Wrapper (Preferences_Changed'Access),
                   Name => "outline.preferences_changed",
                   Watch => GObject (Outline));
         Add_Hook (Kernel, Location_Changed_Hook,
                   Wrapper (Location_Changed'Access),
                   Name  => "outline.location_changed",
                   Watch => GObject (Outline));
         Add_Hook (Kernel, File_Saved_Hook,
                   Wrapper (File_Saved'Access),
                   Name  => "outline.file_saved",
                   Watch => GObject (Outline));
         Add_Hook (Kernel, File_Closed_Hook,
                   Wrapper (File_Closed'Access),
                   Name  => "outline.file_closed",
                   Watch => GObject (Outline));
         Add_Hook (Kernel, File_Edited_Hook,
                   Wrapper (File_Edited'Access),
                   Name  => "outline.file_edited",
                   Watch => GObject (Outline));
      end if;

      return MDI_Child (Child);
   end Open_Outline;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Outline : Outline_View_Access;
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);

      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));

         if Outline.File = D.File then
            Refresh (Outline);
         end if;
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
      Outline : Outline_View_Access;
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);

      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));

         if Outline.File = D.File then
            Outline.File := VFS.No_File;
            Refresh (Outline);
         end if;
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
      Outline : Outline_View_Access;
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);

      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));

         if Outline.File = D.File then
            Refresh (Outline);
         elsif Outline.File = VFS.No_File then
            Outline.File := D.File;
            Refresh (Outline);
         end if;
      end if;
   end File_Edited;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Module  : constant Module_ID := Module_ID (Get_Creator (D.Context));
      Outline : Outline_View_Access;
      File    : Virtual_File;
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);

      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));

         if Module /= null
           and then
             (Get_Name (Module) = "Source_Editor"
              or else Get_Name (Module) = Outline_View_Module_Name)
         then
            if Has_File_Information (D.Context) then
               File := File_Information (D.Context);
            else
               File := VFS.No_File;
            end if;

            if File /= Outline.File then
               Outline.File := File;
               Refresh (Outline);
            elsif Outline.File = VFS.No_File then
               Refresh (Outline);
            end if;
         end if;
      end if;
   end On_Context_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools   : constant String := '/' & (-"Tools") & '/' & (-"Views");
      Command : Interactive_Command_Access;
   begin
      Outline_View_Module := new Outline_View_Module_Record;
      Register_Module
        (Module      => Outline_View_Module,
         Module_Name => Outline_View_Module_Name,
         Kernel      => Kernel);

      Register_Menu
        (Kernel, Tools, -"_Outline", "", On_Open_Outline'Access,
         Ref_Item => -"Project Properties");

      Command := new Refresh_Outline_Command;
      Register_Contextual_Menu
        (Kernel, "Outline View Refresh",
         Action => Command,
         Filter => Action_Filter (Create (Module => Outline_View_Module_Name)),
         Label  => -"Refresh");

      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

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
   end Register_Module;

end Outline_View;
