-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2005-2009, AdaCore                  --
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

with Ada.Unchecked_Deallocation;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Rectangle;             use Gdk.Rectangle;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with XML_Utils;                 use XML_Utils;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
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

with Language;                  use Language;
with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Entities.Tooltips;         use Entities.Tooltips;
with GPS.Editors;               use GPS.Editors;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                use GPS.Kernel;
with Histories;                 use Histories;
with GUI_Utils;                 use GUI_Utils;
with Project_Explorers_Common;  use Project_Explorers_Common;
with Projects;                  use Projects;
with Tooltips;                  use Tooltips;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with Traces;                    use Traces;

with Language.Tree;          use Language.Tree;
with Language.Tree.Database; use Language.Tree.Database;
with Language_Handlers;      use Language_Handlers;

with Outline_View.Model; use Outline_View.Model;

package body Outline_View is

   type Outline_View_Module_Record is new Module_ID_Record with record
      Construct_Annotation_Key : Construct_Annotations_Pckg.Annotation_Key;
   end record;

   Outline_View_Module : Module_ID;
   Outline_View_Module_Name : constant String := "Outline_View";

   Hist_Show_Profile      : constant History_Key := "outline-show-profile";
   Hist_Sort_Alphabetical : constant History_Key := "outline-alphabetical";
   Hist_Editor_Link       : constant History_Key := "outline-editor-link";
   Hist_Show_Decls        : constant History_Key := "outline-show-decls";
   Hist_Show_Types        : constant History_Key := "outline-show-types";

   overriding procedure Default_Context_Factory
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
   --  Open the outline view, or return a handle to it if it already exists

   procedure On_Open_Outline
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Raise the existing explorer, or open a new one

   type Outline_Db_Listener;
   type Outline_Db_Listener_Access is access all Outline_Db_Listener'Class;

   type Outline_View_Record is new Gtk.Box.Gtk_Box_Record with record
      Kernel    : Kernel_Handle;
      Tree      : Gtk_Tree_View;
      File      : GNATCOLL.VFS.Virtual_File;
      Icon      : Gdk_Pixbuf;
      File_Icon : Gdk_Pixbuf;
      Db_Listener : Outline_Db_Listener_Access;
   end record;
   type Outline_View_Access is access all Outline_View_Record'Class;

   type Outline_Db_Listener is new Database_Listener with record
      Outline : Outline_View_Access;
   end record;

   overriding procedure File_Updated
     (Listener : access Outline_Db_Listener;
      File     : Structured_File_Access;
      Old_Tree : Construct_Tree;
      Kind     : Update_Kind);

   overriding procedure Before_Clear_Db
     (Listener : access Outline_Db_Listener;
      Db       : access Construct_Database'Class);

   procedure Free is new Ada.Unchecked_Deallocation
     (Outline_Db_Listener'Class, Outline_Db_Listener_Access);

   procedure Gtk_New
     (Outline : out Outline_View_Access;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Create a new outline view

   type Refresh_Outline_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Refresh_Outline_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See inherited documentation

   procedure Refresh (View : access Gtk_Widget_Record'Class);
   --  Recompute the information for Outline.File, and redisplay it.
   --  If the constructs are up-to-date, do nothing.

   procedure Force_Refresh (View : access Gtk_Widget_Record'Class);
   --  Same as above, but force a full refresh.

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

   function Get_Filter_Record
     (Kernel : access Kernel_Handle_Record'Class)
      return Tree_Filter;
   --  Return the filters properties extracted from the kernel.

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

   type Outline_View_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Outline : Outline_View_Access;
   end record;
   type Outline_View_Tooltips_Access is access all Outline_View_Tooltips;
   overriding procedure Draw
     (Tooltip : access Outline_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);

   ------------------------
   -- Outline_Tree Model --
   ------------------------

   function Get_Outline_Model
     (View : Outline_View_Access) return Outline_Model;
   --  Return the outline model stored in this view

   procedure Set_Outline_Model
     (View : access Outline_View_Record'Class; Model : Outline_Model);
   --  Set the outline model to the tree of this view

   ----------
   -- Draw --
   ----------

   overriding procedure Draw
     (Tooltip : access Outline_View_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Iter     : Gtk_Tree_Iter;
      P_Entity : Entity_Persistent_Access;
      Entity   : Entity_Access;
   begin
      Pixmap := null;
      Initialize_Tooltips (Tooltip.Outline.Tree, Area, Iter);

      if Iter /= Null_Iter then
         P_Entity := Get_Entity (Iter);

         if Exists (P_Entity) then
            Entity := To_Entity_Access (P_Entity);
            Entity := Get_Declaration
              (Get_Tree_Language (Get_File (Entity)), Entity);

            Pixmap := Entities.Tooltips.Draw_Tooltip
              (Kernel => Tooltip.Outline.Kernel,
               Entity => Entity);
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
      Outline     : Outline_View_Access;
      Model       : Outline_Model;
      Path        : Gtk_Tree_Path;
      Loc         : File_Location_Hooks_Args_Access;
   begin
      null;
      if Get_History (Get_History (Kernel).all, Hist_Editor_Link)
        and then Child /= null
      then
         Outline := Outline_View_Access (Get_Widget (Child));
         Model   := Get_Outline_Model (Outline);
         Unselect_All (Get_Selection (Outline.Tree));

         if Model = null then
            return;
         end if;

         Loc := File_Location_Hooks_Args_Access (Data);

         Path := Get_Path_Enclosing_Location (Model, Loc.Line, Loc.Column);

         if Get_Depth (Path) > 1 then
            declare
               Indices     : constant Glib.Gint_Array := Get_Indices (Path);
               Parent_Path : constant Gtk_Tree_Path := Gtk_New;
            begin
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
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Child   : constant MDI_Child :=
                  Find_MDI_Child_By_Tag
                    (Get_MDI (Kernel), Outline_View_Record'Tag);
      Outline : Outline_View_Access;
   begin
      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));
         Modify_Font (Outline.Tree, View_Fixed_Font.Get_Pref);
      end if;
   end Preferences_Changed;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
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
      Outline  : constant Outline_View_Access := Outline_View_Access (Object);
      Model    : constant Gtk_Tree_Model := Get_Model (Outline.Tree);
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
      Line     : Integer := 1;
      Check    : Gtk_Check_Menu_Item;
      Item     : Gtk_Menu_Item;
      Sep      : Gtk_Menu_Item;
      Submenu  : Gtk_Menu;
      P_Entity : Entity_Persistent_Access;
   begin
      Iter := Find_Iter_For_Event (Outline.Tree, Model, Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         if not Path_Is_Selected (Get_Selection (Outline.Tree), Path) then
            Set_Cursor (Outline.Tree, Path, null, False);
         end if;
         Path_Free (Path);

         P_Entity := Get_Entity (Iter);

         Set_Entity_Information
           (Context       => Context,
            Entity_Name   => Get_Construct (P_Entity).Name.all,
            Entity_Column =>
              Visible_Column_Type
                (Get_Construct (P_Entity).Sloc_Entity.Column));

         Line := Get_Construct (P_Entity).Sloc_Entity.Line;
      end if;

      Set_File_Information
        (Context => Context,
         Project => Projects.No_Project,
         Files   => (1 => Outline.File),
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
           (Check, Signal_Toggled, Force_Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Show types");
         Associate (Get_History (Kernel).all, Hist_Show_Types, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Force_Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Show specifications");
         Associate (Get_History (Kernel).all, Hist_Show_Decls, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Force_Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Sort alphabetically");
         Associate (Get_History (Kernel).all, Hist_Sort_Alphabetical, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Force_Refresh'Access, Outline);

         Gtk_New (Check, Label => -"Dynamic link with editor");
         Associate (Get_History (Kernel).all, Hist_Editor_Link, Check);
         Append (Submenu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Force_Refresh'Access, Outline);
      end if;
   end Outline_Context_Factory;

   -----------------------
   -- Get_Filter_Record --
   -----------------------

   function Get_Filter_Record
     (Kernel : access Kernel_Handle_Record'Class)
      return Tree_Filter
   is
   begin
      return
        (Hide_Types        => not Get_History
           (Get_History (Kernel).all,
            Hist_Show_Types),
         Hide_Declarations => not Get_History
           (Get_History (Kernel).all,
            Hist_Show_Decls),
         Show_Profile => Get_History
           (Get_History (Kernel).all,
            Hist_Show_Profile));
   end Get_Filter_Record;

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

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Outline : access Gtk_Widget_Record'Class;
      Event   : Gdk_Event) return Boolean
   is
      View  : constant Outline_View_Access := Outline_View_Access (Outline);
      Model : constant Gtk_Tree_Model := Get_Model (View.Tree);
      Entity : Entity_Persistent_Access;
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
      Line, Column, End_Column  : Integer;
      Construct : access Simple_Construct_Information;
   begin
      if Get_Button (Event) = 1 then
         if View.File = No_File then
            return False;
         end if;

         Iter := Find_Iter_For_Event (View.Tree, Model, Event);

         if Iter /= Null_Iter then
            Path := Get_Path (Model, Iter);
            Set_Cursor (View.Tree, Path, null, False);
            Path_Free (Path);

            Entity := Get_Entity (Iter);

            Construct  := Get_Construct
              (To_Construct_Tree_Iterator (To_Entity_Access (Entity)));

            if Construct.Sloc_Entity.Index /= 0 then
               Line := Construct.Sloc_Entity.Line;
               Column := Construct.Sloc_Entity.Column;
               End_Column := Column + Construct.Name'Length;
            else
               Line := Construct.Sloc_Start.Line;
               Column := Construct.Sloc_Start.Column;
               End_Column := Column;
            end if;

            declare
               Buffer   : constant Editor_Buffer'Class :=
                 Get (Get_Buffer_Factory (View.Kernel).all,
                      View.File, False, False, False);
               Location : constant Editor_Location'Class :=
                 New_Location (Buffer, Line, Column);
               End_Location : constant Editor_Location'Class :=
                 New_Location (Buffer, Line, End_Column);

               View : constant Editor_View'Class := Current_View (Buffer);
            begin
               Cursor_Goto (View, Location);
               Select_Text (Buffer, Location, End_Location);
            end;

         end if;
      end if;
      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Button_Press;

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Listener : access Outline_Db_Listener;
      File     : Structured_File_Access;
      Old_Tree : Construct_Tree;
      Kind     : Update_Kind)
   is
      Model : constant Outline_Model := Get_Outline_Model (Listener.Outline);
   begin
      if Model /= null then
         File_Updated (Model, File, Old_Tree, Kind);
      end if;
   end File_Updated;

   ---------------------
   -- Before_Clear_Db --
   ---------------------

   overriding procedure Before_Clear_Db
     (Listener : access Outline_Db_Listener;
      Db       : access Construct_Database'Class)
   is
      pragma Unreferenced (Db);

      Model : Outline_Model;
   begin
      Listener.Outline.File := No_File;

      Model := Get_Outline_Model (Listener.Outline);

      if Model /= null then
         Free (Model);
      end if;
   end Before_Clear_Db;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Outline : out Outline_View_Access;
      Kernel  : access Kernel_Handle_Record'Class)
   is
      Scrolled      : Gtk_Scrolled_Window;

      Col           : Gtk_Tree_View_Column;
      Col_Number    : Gint;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Pixbuf_Render : Gtk_Cell_Renderer_Pixbuf;
      Tooltip       : Outline_View_Tooltips_Access;
      Model         : Outline_Model;

      pragma Unreferenced (Col_Number);
      Out_Model : Outline_Model;
   begin
      Outline := new Outline_View_Record;
      Outline.Kernel := Kernel_Handle (Kernel);

      Init_Graphics (Gtk_Widget (Get_Main_Window (Kernel)));

      Initialize_Vbox (Outline, Homogeneous => False);

      Gtk_New (Scrolled);
      Pack_Start (Outline, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      --  Create the tree view using the sorting model

      Out_Model := new Outline_Model_Record;
      Gtkada.Abstract_Tree_Model.Initialize (Out_Model);
      Gtk_New (Outline.Tree, Out_Model);
      Unref (Out_Model);
      Set_Name (Outline.Tree, "Outline View Tree");  --  For testsuite

      Set_Headers_Visible (Outline.Tree, False);

      --  ??? when do we free / release this object?
      Outline.Db_Listener := new Outline_Db_Listener;
      Outline.Db_Listener.Outline := Outline;

      Add_Database_Listener
        (Kernel.Get_Construct_Database,
         Database_Listener_Access (Outline.Db_Listener));

      --  Create an explicit columns for the expander

      Gtk_New (Col);
      Col_Number := Append_Column (Outline.Tree, Col);
      Gtk_New (Pixbuf_Render);
      Pack_Start (Col, Pixbuf_Render, False);
      Add_Attribute (Col, Pixbuf_Render, "pixbuf", Pixbuf_Column);

      Set_Expander_Column (Outline.Tree, Col);

      Gtk_New (Text_Render);
      Pack_Start (Col, Text_Render, False);
      Add_Attribute
        (Col, Text_Render, "markup", Outline_View.Model.Display_Name_Column);
      Clicked (Col);

      Add (Scrolled, Outline.Tree);

      Outline.Icon := Render_Icon
        (Get_Main_Window (Kernel), "gps-box", Icon_Size_Menu);
      Outline.File_Icon := Render_Icon
        (Get_Main_Window (Kernel), "gps-file", Icon_Size_Menu);

      Modify_Font (Outline.Tree, View_Fixed_Font.Get_Pref);

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

      Model := null;

      Set_Outline_Model (Outline, Model);
   end Gtk_New;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class)
   is
      O : constant Outline_View_Access := Outline_View_Access (Outline);
      Model : constant Outline_Model := Get_Outline_Model (O);
   begin
      Remove_Database_Listener
        (Get_Construct_Database (O.Kernel),
         Database_Listener_Access (O.Db_Listener));

      Free (O.Db_Listener);

      if Model /= null then
         Model.Free;
      end if;

      Unref (O.Icon);
      Unref (O.File_Icon);
   end On_Destroy;

   -------------
   -- Execute --
   -------------

   overriding function Execute
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

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Gtk_Widget_Record'Class) is
      Outline : constant Outline_View_Access := Outline_View_Access (View);
   begin
      if Outline.File /= No_File then
         Update_Contents
           (Get_Construct_Database (Outline.Kernel), Outline.File);
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
         Add_Hook (Kernel, Buffer_Modified_Hook,
                   Wrapper (File_Saved'Access),
                   Name  => "outline.file_modified",
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
            Outline.Set_File (GNATCOLL.VFS.No_File);
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
               File := GNATCOLL.VFS.No_File;
            end if;

            if File /= Outline.File then
               Outline.Set_File (File);
               Refresh (Outline);
            elsif Outline.File = GNATCOLL.VFS.No_File then
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
         Ref_Item => -"File Switches", Add_Before => False);

      Command := new Refresh_Outline_Command;
      Register_Contextual_Menu
        (Kernel, "Outline View Refresh",
         Action => Command,
         Filter => Action_Filter (Create (Module => Outline_View_Module_Name)),
         Label  => -"Refresh");

      GPS.Kernel.Register_Desktop_Functions
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

      Construct_Annotations_Pckg.Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry
           (Get_Construct_Database (Kernel)).all,
         Outline_View_Module_Record (Outline_View_Module.all).
           Construct_Annotation_Key);
   end Register_Module;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Outline : access Outline_View_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File)
   is
      Model       : Outline_Model;
      Struct_File : Structured_File_Access;
   begin
      Outline.File := File;

      Struct_File := Get_Or_Create
        (Get_Construct_Database (Outline.Kernel),
         Outline.File,
         Get_Language_From_File
           (Get_Language_Handler (Outline.Kernel),
            Outline.File),
         Get_Tree_Language_From_File
           (Get_Language_Handler (Outline.Kernel), Outline.File, True));

      Model := Get_Outline_Model (Outline_View_Access (Outline));

      if Model /= null then
         Model.Free;
      end if;

      if Struct_File /= null then
         Model := New_Model
           (Outline_View_Module_Record
              (Outline_View_Module.all).Construct_Annotation_Key,
            Struct_File,
            Get_Filter_Record (Outline.Kernel),
            Get_History
              (Get_History (Outline.Kernel).all,
               Hist_Sort_Alphabetical));

         Set_Outline_Model (Outline, Model);

         declare
            Path : Gtk_Tree_Path;
         begin
            Path := Gtk_New;

            Append_Index (Path, 0);

            Expand_To_Path (Outline.Tree, Path);
            Path_Free (Path);
         end;
      else
         Set_Model (Outline.Tree, null);
      end if;
   end Set_File;

   -----------------------
   -- Get_Outline_Model --
   -----------------------

   function Get_Outline_Model
     (View : Outline_View_Access) return Outline_Model
   is
      Model : Gtk_Tree_Model;
   begin
      Model := Get_Model (View.Tree);

      if Model = null then
         return null;
      elsif Model.all in Outline_Model_Record'Class then
         return Outline_Model (Model);
      else
         return null;
      end if;
   end Get_Outline_Model;

   -----------------------
   -- Set_Outline_Model --
   -----------------------

   procedure Set_Outline_Model
     (View : access Outline_View_Record'Class; Model : Outline_Model)
   is
   begin
      Set_Model (View.Tree, Gtk_Tree_Model (Model));
   end Set_Outline_Model;

end Outline_View;
