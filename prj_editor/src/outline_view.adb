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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                    use System;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Rectangle;             use Gdk.Rectangle;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with XML_Utils;                 use XML_Utils;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Model.Utils;      use Gtk.Tree_Model.Utils;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Window;                use Gtk.Window;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Abstract_Tree_Model; use Gtkada.Abstract_Tree_Model;

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

   function Construct_Filter
     (Construct : access Simple_Construct_Information) return Boolean;
   --  Return False if the construct should be filtered out

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

   ----------------
   -- Tree Model --
   ----------------

   type Outline_Model_Record is new Gtk_Abstract_Tree_Model_Record with
      record
         File : Structured_File_Access;
      end record;
   --  This model represents a structured tree. It contains instances of
   --  Entity_Persisent_Access. Using the update listener callback, we ensure
   --  that these entities are never referenced if they don't exist - so we
   --  don't have to manage their references, they'll get deleted
   --  automatically when irrelevant. We only do that for object that are
   --  explicitelly referenced in the graphical outline, as a defensive code
   --  for the case where they'd stay there longer because of a bug.

   type Outline_Model is access all Outline_Model_Record'Class;

   --  GtkTreeModel subprograms

   overriding function Get_N_Columns
     (Self : access Outline_Model_Record)
      return Glib.Gint;

   overriding function Get_Column_Type
     (Self  : access Outline_Model_Record;
      Index : Glib.Gint) return Glib.GType;

   overriding function Get_Iter
     (Self : access Outline_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   function Get_Path
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Gtk_Tree_Path;

   overriding function Get_Path
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path;

   overriding procedure Get_Value
     (Self   : access Outline_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   overriding procedure Next
     (Self : access Outline_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter);

   overriding function Children
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Has_Child
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

   overriding function N_Children
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint;

   overriding function Nth_Child
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding function Parent
     (Self  : access Outline_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter;

   overriding procedure Ref_Node
     (Tree_Model : access Outline_Model_Record;
      Iter       : Gtk_Tree_Iter);

   overriding procedure Unref_Node
     (Tree_Model : access Outline_Model_Record;
      Iter       : Gtk_Tree_Iter);

   procedure Clear (Model : access Outline_Model_Record'Class);

   function To_Entity is new Ada.Unchecked_Conversion
     (System.Address, Entity_Persistent_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (Entity_Persistent_Access, System.Address);

   function Filtered_Next
     (Tree         : Construct_Tree;
      It           : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation) return Construct_Tree_Iterator;

   function Filtered_Prev
     (Tree         : Construct_Tree;
      It           : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation) return Construct_Tree_Iterator;

   function Filtered_N_Children
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Gint;

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
         P_Entity := To_Entity (Get_User_Data_1 (Iter));

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
      Entity      : Construct_Tree_Iterator;

      Loc         : File_Location_Hooks_Args_Access;
   begin
      if Get_History (Get_History (Kernel).all, Hist_Editor_Link)
        and then Child /= null
      then
         Outline := Outline_View_Access (Get_Widget (Child));
         Model   := Outline_Model (Get_Model (Outline.Tree));
         Unselect_All (Get_Selection (Outline.Tree));

         if Model = null or else Model.File = null then
            return;
         end if;

         Loc := File_Location_Hooks_Args_Access (Data);
         Entity := Get_Iterator_At
           (Get_Tree (Model.File),
            To_Location (Loc.Line, Loc.Column),
            Position => Enclosing);

         --  Place the iterator on an entity present in the outline.

         while Entity /= Null_Construct_Tree_Iterator
           and then not Construct_Filter (Get_Construct (Entity))
         loop
            Entity :=
              Get_Parent_Scope (Get_Tree (Model.File), Entity);
         end loop;

         if Entity /= Null_Construct_Tree_Iterator then
            Path := Get_Path (Get_Tree (Model.File), Entity);

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
            Path_Free (Path);
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

         P_Entity := To_Entity (Get_User_Data_1 (Iter));

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

   ---------------------
   -- Category_Filter --
   ---------------------

   function Construct_Filter
     (Construct : access Simple_Construct_Information) return Boolean is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      case Construct.Category is
         when Subprogram_Explorer_Category |
              Cat_Package .. Cat_Task |
              Cat_Field | Cat_Variable |
              Type_Category =>
            null;

         when others =>
            return False;
      end case;

      return Construct.Name /= null;
   end Construct_Filter;

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
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
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

            declare
               Buffer   : constant Editor_Buffer'Class :=
                 Get (Get_Buffer_Factory (View.Kernel).all,
                      View.File, False, False, False);
               Entity : constant Entity_Persistent_Access :=
                 To_Entity (Get_User_Data_1 (Iter));
               Construct : constant access Simple_Construct_Information :=
                 Get_Construct
                   (To_Construct_Tree_Iterator (To_Entity_Access (Entity)));

               Line     : constant Integer := Construct.Sloc_Entity.Line;
               Column   : constant Integer := Construct.Sloc_Entity.Column;

               Location : constant Editor_Location'Class :=
                 New_Location (Buffer, Line, Column);
               P_Entity : constant Entity_Persistent_Access :=
                 To_Entity (Get_User_Data_1 (Iter));
               End_Location : constant Editor_Location'Class :=
                 New_Location
                   (Buffer, Line,
                    Column + Get_Construct (P_Entity).Name'Length);

               View     : constant Editor_View'Class :=
                 Current_View (Buffer);
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

   package Path_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Gtk_Tree_Path);

   overriding procedure File_Updated
     (Listener : access Outline_Db_Listener;
      File     : Structured_File_Access;
      Old_Tree : Construct_Tree;
      Kind     : Update_Kind)
   is
      use Path_Lists;
      pragma Unreferenced (Kind);

      To_Delete : Path_Lists.List;
      To_Insert : Path_Lists.List;

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind)
      is
         Path : Gtk_Tree_Path;
      begin
         case Kind is
            when Removed =>
               if Construct_Filter (Get_Construct (Old_Obj)) then
                  Path := Get_Path (Old_Tree, Old_Obj);

                  if Get_Depth (Path) > 0 then
                     To_Delete.Append (Path);
                  end if;
               end if;

            when Added =>
               if Construct_Filter (Get_Construct (New_Obj)) then
                  Path := Get_Path (Get_Tree (File), New_Obj);

                  if Get_Depth (Path) > 0 then
                     To_Insert.Append (Path);
                  end if;
               end if;

            when Preserved =>
               null;

         end case;
      end Diff_Callback;

   begin
      if Get_File_Path (File) = Listener.Outline.File
        and then Old_Tree /= Null_Construct_Tree
      then
         Diff
           (Lang     => Get_Tree_Language (File),
            Old_Tree => Old_Tree,
            New_Tree => Get_Tree (File),
            Callback => Diff_Callback'Unrestricted_Access);
      end if;

      declare
         It : Path_Lists.Cursor := To_Delete.Last;

      begin
         while It /= Path_Lists.No_Element loop
            Row_Deleted (Get_Model (Listener.Outline.Tree), Element (It));
            Path_Free (Element (It));

            It := Previous (It);
         end loop;
      end;

      declare
         It : Path_Lists.Cursor := To_Insert.First;
         Tree_Iter : Gtk_Tree_Iter;
      begin
         while It /= Path_Lists.No_Element loop
            Tree_Iter :=
              Get_Iter (Get_Model (Listener.Outline.Tree), Element (It));
            Row_Inserted
              (Get_Model (Listener.Outline.Tree), Element (It), Tree_Iter);

            Path_Free (Element (It));
            It := Next (It);
         end loop;
      end;
   end File_Updated;

   ---------------------
   -- Before_Clear_Db --
   ---------------------

   overriding procedure Before_Clear_Db
     (Listener : access Outline_Db_Listener;
      Db       : access Construct_Database'Class)
   is
      pragma Unreferenced (Db);

      Model : Gtk_Tree_Model;
   begin
      Listener.Outline.File := No_File;

      Model := Get_Model (Listener.Outline.Tree);

      if Model /= null then
         Clear (Outline_Model (Model));
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
   begin
      Outline := new Outline_View_Record;
      Outline.Kernel := Kernel_Handle (Kernel);

      Init_Graphics (Gtk_Widget (Get_Main_Window (Kernel)));

      Initialize_Vbox (Outline, Homogeneous => False);

      Gtk_New (Scrolled);
      Pack_Start (Outline, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      --  Create the tree view using the sorting model

      Gtk_New (Outline.Tree, new Outline_Model_Record);
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
      Add_Attribute (Col, Text_Render, "markup", Display_Name_Column);
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

      Model := new Outline_Model_Record'
        (Gtk_Abstract_Tree_Model_Record with File => null);
      Gtkada.Abstract_Tree_Model.Initialize (Model);
      Set_Model
        (Gtk_Tree_View (Outline.Tree), Gtk_Tree_Model (Model));
   end Gtk_New;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class)
   is
      O : constant Outline_View_Access := Outline_View_Access (Outline);
   begin
      Remove_Database_Listener
        (Get_Construct_Database (O.Kernel),
         Database_Listener_Access (O.Db_Listener));

      Free (O.Db_Listener);
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

   -----------
   -- Clear --
   -----------

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
         Update_Contents
           (Get_Construct_Database (Outline.Kernel), Outline.File);
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
         Ref_Item => -"Project Properties");

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
   end Register_Module;

   --   ??? add management of iter ref / unref

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

      Model := Outline_Model (Get_Model (Outline.Tree));

      if Model /= null then
         Clear (Model);
      end if;

      if Struct_File /= null then
         Model := new Outline_Model_Record;
         Model.File := Struct_File;
         Gtkada.Abstract_Tree_Model.Initialize (Model);

         Set_Model (Outline.Tree, Gtk_Tree_Model (Model));

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

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Outline_Model_Record)
      return Glib.Gint
   is
      pragma Unreferenced (Self);
   begin
      return 2;
   end Get_N_Columns;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Outline_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);
   begin
      if Index = Pixbuf_Column then
         return Gdk.Pixbuf.Get_Type;
      elsif Index = Display_Name_Column then
         return GType_String;
      end if;

      return GType_String;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Outline_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Tree     : Construct_Tree;
      It       : Construct_Tree_Iterator;
      First_It : Construct_Tree_Iterator;
   begin
      if Self.File = null or else Get_Depth (Path) = 0 then
         return Null_Iter;
      end if;

      Tree := Get_Tree (Self.File);

      if Tree = Null_Construct_Tree then
         return Null_Iter;
      end if;

      declare
         Indices : constant Glib.Gint_Array := Get_Indices (Path);
      begin
         for J in Indices'Range loop
            if J = Indices'First then
               First_It := First (Tree);

               if not Construct_Filter (Get_Construct (First_It)) then
                  First_It := Filtered_Next (Tree, First_It, Jump_Over);
               end if;
            else
               First_It := Filtered_Next (Tree, It, Jump_Into);
            end if;

            if First_It = Null_Construct_Tree_Iterator
              or else Get_Parent_Scope (Tree, First_It) /= It
            then
               return Null_Iter;
            end if;

            It := First_It;

            for K in 1 .. Indices (J) loop
               --  We skip the element 0 - we're already on it - and then jump
               --  over as many elements as needed.

               if It = Null_Construct_Tree_Iterator
                 or else Get_Parent_Scope (Tree, First_It)
                 /= Get_Parent_Scope (Tree, It)
               then
                  return Null_Iter;
               end if;

               It := Filtered_Next (Tree, It, Jump_Over);
            end loop;
         end loop;
      end;

      if It /= Null_Construct_Tree_Iterator then
         return Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address
              (To_Entity_Persistent_Access
                 (To_Entity_Access (Self.File, It))),
            User_Data_2 => System.Null_Address,
            User_Data_3 => System.Null_Address);
      else
         return Null_Iter;
      end if;
   end Get_Iter;

   --------------
   -- Get_Path --
   --------------

   function Get_Path
     (Tree : Construct_Tree; Iter : Construct_Tree_Iterator)
      return Gtk_Tree_Path
   is
      Result : constant Gtk_Tree_Path := Gtk_New;
      Cur_It, Prev_It : Construct_Tree_Iterator;
      Current_Index : Gint := 0;
      Cur_It_Parent : Construct_Tree_Iterator;
   begin
      Cur_It := Iter;

      while Cur_It /= Null_Construct_Tree_Iterator loop
         Prev_It := Cur_It;
         Cur_It := Filtered_Prev (Tree, Cur_It, Jump_Over);

         Cur_It_Parent := Get_Parent_Scope (Tree, Cur_It);

         if Cur_It_Parent /= Null_Construct_Tree_Iterator
           and then
             not Construct_Filter (Get_Construct (Cur_It_Parent))
         then
            --  If there's a parent that should be filtered out, then there's
            --  no path for this entity.

            Path_Free (Result);
            return Gtk_New;
         end if;

         if Cur_It_Parent = Get_Parent_Scope (Tree, Prev_It) then
            if Cur_It /= Null_Construct_Tree_Iterator then
               Current_Index := Current_Index + 1;
            end if;
         else
            Prepend_Index (Result, Current_Index);
            Current_Index := 0;
         end if;
      end loop;

      Prepend_Index (Result, Current_Index);

      return Result;
   end Get_Path;

   overriding function Get_Path
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Tree   : constant Construct_Tree := Get_Tree (Self.File);
      Entity : constant Entity_Persistent_Access :=
        To_Entity (Get_User_Data_1 (Iter));
   begin
      if not Exists (Entity) then
         return Gtk_New;
      end if;

      return Get_Path
        (Tree, To_Construct_Tree_Iterator (To_Entity_Access (Entity)));
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Outline_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Entity   : constant Entity_Persistent_Access :=
        To_Entity (Get_User_Data_1 (Iter));
      It : Construct_Tree_Iterator;
   begin
      It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));

      if Column = Pixbuf_Column then
         Init (Value, Gdk.Pixbuf.Get_Type);
         Set_Object
           (Value, GObject (Entity_Icon_Of (Get_Construct (It).all)));

      elsif Column = Display_Name_Column then
         Init (Value, GType_String);

         if Get_Construct (It).Name /= null then
            if Get_Construct (It).Category in Subprogram_Category then
               Set_String
                 (Value, Get_Construct (It).Name.all
                  & " <span foreground=""#A0A0A0"">"
                  & Get_Profile
                    (Get_Tree_Language (Self.File),
                     To_Entity_Access (Entity),
                     500)
                  & "</span>");
            else
               Set_String (Value, Get_Construct (It).Name.all);
            end if;

         else
            Set_String (Value, "no name");
         end if;
      end if;
   end Get_Value;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Outline_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Tree   : constant Construct_Tree := Get_Tree (Self.File);
      Entity : constant Entity_Persistent_Access :=
        To_Entity (Get_User_Data_1 (Iter));
      It, Next_It : Construct_Tree_Iterator;
   begin
      It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));
      Next_It := Filtered_Next (Tree, It, Jump_Over);

      if Next_It = Null_Construct_Tree_Iterator
        or else Get_Parent_Scope (Tree, It)
        /= Get_Parent_Scope (Tree, Next_It)
      then
         Iter := Null_Iter;
      else
         --   ??? unref iter
         Iter := Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address
              (To_Entity_Persistent_Access
                 (To_Entity_Access (Self.File, Next_It))),
            User_Data_2 => System.Null_Address,
            User_Data_3 => System.Null_Address);
      end if;
   end Next;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Tree         : Construct_Tree;
      Entity       : Entity_Persistent_Access;
      It, Child_It : Construct_Tree_Iterator;
   begin
      if Self.File = null then
         return Null_Iter;
      end if;

      Tree := Get_Tree (Self.File);

      if Parent = Null_Iter then
         Child_It := First (Tree);

         if not Construct_Filter (Get_Construct (Child_It)) then
            Child_It := Filtered_Next (Tree, It, Jump_Into);
         end if;

      else
         Entity := To_Entity (Get_User_Data_1 (Parent));
         It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));
         Child_It := Filtered_Next (Tree, It, Jump_Into);

         if Child_It = Null_Construct_Tree_Iterator
           or else It /= Get_Parent_Scope (Tree, Child_It)
         then
            return Null_Iter;
         end if;
      end if;

      return Init_Tree_Iter
        (Stamp       => 1,
         User_Data_1 => To_Address
           (To_Entity_Persistent_Access
              (To_Entity_Access (Self.File, Child_It))),
         User_Data_2 => System.Null_Address,
         User_Data_3 => System.Null_Address);
   end Children;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Entity   : constant Entity_Persistent_Access :=
                   To_Entity (Get_User_Data_1 (Iter));
      It : Construct_Tree_Iterator;
   begin
      It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));

      return Filtered_N_Children (Get_Tree (Self.File), It) /= 0;
   end Has_Child;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Outline_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Entity : Entity_Persistent_Access;
      It     : Construct_Tree_Iterator;
   begin
      if Self.File = null then
         return 0;
      end if;

      if Iter = Null_Iter then
         It := Null_Construct_Tree_Iterator;
      else
         Entity := To_Entity (Get_User_Data_1 (Iter));
         It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));
      end if;

      return Filtered_N_Children (Get_Tree (Self.File), It);
   end N_Children;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Outline_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Tree          : Construct_Tree;
      Iter          : Gtk_Tree_Iter;
      Parent_Entity : Entity_Persistent_Access;
      Parent_Iter   : Construct_Tree_Iterator;
      Child_Iter    : Construct_Tree_Iterator;
   begin
      if Self.File = null then
         return Null_Iter;
      end if;

      Tree := Get_Tree (Self.File);

      if Parent = Null_Iter then
         Parent_Iter := Null_Construct_Tree_Iterator;
         Child_Iter := First (Tree);
      else
         if not Exists (Parent_Entity) then
            return Null_Iter;
         end if;

         Parent_Entity := To_Entity (Get_User_Data_1 (Parent));
         Parent_Iter := To_Construct_Tree_Iterator
           (To_Entity_Access (Parent_Entity));
         Child_Iter := Filtered_Next (Tree, Parent_Iter, Jump_Into);
      end if;

      for J in 2 .. N loop
         if Child_Iter = Null_Construct_Tree_Iterator
           or else Get_Parent_Scope (Tree, Child_Iter) /= Parent_Iter
         then
            return Null_Iter;
         end if;

         Child_Iter := Filtered_Next (Tree, Child_Iter, Jump_Over);
      end loop;

      if Child_Iter = Null_Construct_Tree_Iterator
        or else Get_Parent_Scope (Tree, Child_Iter) /= Parent_Iter
      then
         return Null_Iter;
      end if;

      Iter := Init_Tree_Iter
        (Stamp       => 1,
         User_Data_1 => To_Address
           (To_Entity_Persistent_Access
              (To_Entity_Access (Self.File, Child_Iter))),
         User_Data_2 => System.Null_Address,
         User_Data_3 => System.Null_Address);

      return Iter;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Outline_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Tree   : constant Construct_Tree := Get_Tree (Self.File);
      Entity : constant Entity_Persistent_Access :=
        To_Entity (Get_User_Data_1 (Child));
      It, Parent_It : Construct_Tree_Iterator;
   begin
      It := To_Construct_Tree_Iterator (To_Entity_Access (Entity));

      Parent_It := Get_Parent_Scope (Tree, It);

      if Parent_It = Null_Construct_Tree_Iterator then
         return Null_Iter;
      else
         return Init_Tree_Iter
           (Stamp       => 1,
            User_Data_1 => To_Address
              (To_Entity_Persistent_Access
                 (To_Entity_Access (Self.File, Parent_It))),
            User_Data_2 => System.Null_Address,
            User_Data_3 => System.Null_Address);
      end if;
   end Parent;

   --------------
   -- Ref_Node --
   --------------

   overriding procedure Ref_Node
     (Tree_Model : access Outline_Model_Record;
      Iter       : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Tree_Model);

      Entity : Entity_Persistent_Access :=
                 To_Entity (Get_User_Data_1 (Iter));
   begin
      Ref (Entity);
   end Ref_Node;

   ----------------
   -- Unref_Node --
   ----------------

   overriding procedure Unref_Node
     (Tree_Model : access Outline_Model_Record;
      Iter       : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Tree_Model);

      Entity : Entity_Persistent_Access :=
                 To_Entity (Get_User_Data_1 (Iter));
   begin
      Unref (Entity);
   end Unref_Node;

   -----------
   -- Clear --
   -----------

   procedure Clear (Model : access Outline_Model_Record'Class) is
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter;
   begin
      if Model.File = null then
         return;
      end if;

      for J in 1 .. N_Children (Model) loop
         Path := Gtk_New;
         Append_Index (Path, J - 1);

         if J = 1 then
            Iter := Get_Iter (Model, Path);
         else
            Next (Model, Iter);
         end if;

         if Iter /= Null_Iter then
            Row_Deleted (Model, Path);
         end if;

         Path_Free (Path);
      end loop;

      Model.File := null;
   end Clear;

   -------------------
   -- Filtered_Next --
   -------------------

   function Filtered_Next
     (Tree         : Construct_Tree;
      It           : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation) return Construct_Tree_Iterator
   is
      Cur_It : Construct_Tree_Iterator := It;
   begin
      loop
         Cur_It := Next (Tree, Cur_It, Scope_Policy);

         exit when Cur_It = Null_Construct_Tree_Iterator
           or else Construct_Filter (Get_Construct (Cur_It));
      end loop;

      return Cur_It;
   end Filtered_Next;

   -------------------
   -- Filtered_Prev --
   -------------------

   function Filtered_Prev
     (Tree         : Construct_Tree;
      It           : Construct_Tree_Iterator;
      Scope_Policy : Scope_Navigation) return Construct_Tree_Iterator
   is
      Cur_It : Construct_Tree_Iterator := It;
   begin
      loop
         Cur_It := Prev (Tree, Cur_It, Scope_Policy);

         exit when Cur_It = Null_Construct_Tree_Iterator
           or else
             Construct_Filter (Get_Construct (Cur_It));
      end loop;

      return Cur_It;
   end Filtered_Prev;

   -------------------------
   -- Filtered_N_Children --
   -------------------------

   function Filtered_N_Children
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Gint
   is
      Cur_It : Construct_Tree_Iterator;
      Total  : Gint := 0;
   begin
      if It = Null_Construct_Tree_Iterator then
         Cur_It := First (Tree);

         if not Construct_Filter (Get_Construct (Cur_It)) then
            Cur_It := Filtered_Next (Tree, It, Jump_Over);
         end if;

      else
         Cur_It := Filtered_Next (Tree, It, Jump_Into);
      end if;

      while Cur_It /= Null_Construct_Tree_Iterator
        and then Get_Parent_Scope (Tree, Cur_It) = It
      loop
         Total := Total + 1;
         Cur_It := Filtered_Next (Tree, Cur_It, Jump_Over);
      end loop;

      return Total;
   end Filtered_N_Children;

end Outline_View;
