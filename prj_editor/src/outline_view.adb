-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
--                            AdaCore                                --
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

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Properties.Creation;    use Glib.Properties.Creation;
with Glib.Xml_Int;                use Glib.Xml_Int;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GUI_Utils;                   use GUI_Utils;
with Basic_Types;                 use Basic_Types;
with VFS;                         use VFS;
with Pixmaps_IDE;                 use Pixmaps_IDE;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with Gdk.Event;                   use Gdk.Event;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_Selection;          use Gtk.Tree_Selection;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtkada.MDI;                  use Gtkada.MDI;
with GPS.Intl;                  use GPS.Intl;
with Entities;                    use Entities;
with String_Utils;                use String_Utils;
with Projects;                    use Projects;
with Language;                    use Language;
with Language_Handlers.GPS;     use Language_Handlers.GPS;
with Basic_Types;                 use Basic_Types;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Project_Explorers_Common;    use Project_Explorers_Common;
with Commands.Interactive;        use Commands, Commands.Interactive;
with Default_Preferences;         use Default_Preferences;
with Entities;                    use Entities;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;
--  with Traces;                      use Traces;

package body Outline_View is

--   Me : constant Debug_Handle := Create ("Outline_View");

   Outline_View_Module : Module_ID;

   Outline_View_Font                : Param_Spec_Font;
   Outline_View_Profiles            : Param_Spec_Boolean;
   Outline_View_Sort_Alphabetically : Param_Spec_Boolean;
   Outline_View_Link_Editor         : Param_Spec_Boolean;

   Entity_Name_Column : constant := 2;
   Mark_Column        : constant := 3;

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
      Tree      : Gtk_Tree_View;
      Kernel    : Kernel_Handle;
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
      User   : Kernel_Handle)
      return Node_Ptr;
   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Handling of desktops

   function Filter_Category
     (Category : Language.Language_Category) return Language.Language_Category;
   --  Return Cat_Unknown if the category should be filtered out, and the
   --  name of the category to use otherwise.

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create the current context

   function Outline_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access;
   --  Context factory when creating contextual menus

   procedure Preferences_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the current editor reaches a new location

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class);
   --  Called when the outline is destroyed

   procedure Clear (Outline : access Outline_View_Record'Class);
   --  Clear the contents of the outline view, and reset all marks

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      Child : constant MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);
      Outline : Outline_View_Access;
      Iter    : Gtk_Tree_Iter;
      Model   : Gtk_Tree_Store;
      Path    : Gtk_Tree_Path;
      Subprogram : Entity_Information;
   begin
      if Get_Pref (Kernel, Outline_View_Link_Editor)
        and then Child /= null
      then
         Outline := Outline_View_Access (Get_Widget (Child));
         Model   := Gtk_Tree_Store (Get_Model (Outline.Tree));
         Unselect_All (Get_Selection (Outline.Tree));

         Subprogram := Compute_Parent_Entity
           (File_Location_Hooks_Args_Access (Data));

         if Subprogram /= null then
            declare
               Subprogram_Name : constant String := Get_Name (Subprogram).all;
            begin
               Iter := Children (Model, Get_Iter_First (Model));
               while Iter /= Null_Iter loop
                  if Get_String (Model, Iter, 1) = Subprogram_Name then
                     Path := Get_Path (Model, Iter);
                     Set_Cursor (Outline.Tree, Path, null, False);
                     Path_Free (Path);
                     exit;
                  end if;

                  Next (Model, Iter);
               end loop;
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
      Child : constant MDI_Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);
      Outline : Outline_View_Access;
      Sort_Column : Gint;
      pragma Unreferenced (Sort_Column);
   begin
      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));

         Modify_Font (Outline.Tree, Get_Pref (Kernel, Outline_View_Font));

         if Get_Pref (Kernel, Outline_View_Sort_Alphabetically) then
            Thaw_Sort (Gtk_Tree_Store (Get_Model (Outline.Tree)), 1);
         else
            Sort_Column :=
              Freeze_Sort (Gtk_Tree_Store (Get_Model (Outline.Tree)));
         end if;

         Refresh (Outline);
      end if;
   end Preferences_Changed;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      Outline : constant Outline_View_Access := Outline_View_Access (Child);
   begin
      return Outline_Context_Factory
        (Kernel       => Kernel,
         Event_Widget => Outline.Tree,
         Object       => Outline,
         Event        => null,
         Menu         => null);
   end Default_Factory;

   -----------------------------
   -- Outline_Context_Factory --
   -----------------------------

   function Outline_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel, Event_Widget, Menu);
      Context : File_Selection_Context_Access;
      Iter    : Gtk_Tree_Iter;
      Outline : constant Outline_View_Access := Outline_View_Access (Object);
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Outline.Tree));
      Path      : Gtk_Tree_Path;
      Line, Column : Integer;
   begin
      Iter := Find_Iter_For_Event (Outline.Tree, Model, Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         if not Path_Is_Selected (Get_Selection (Outline.Tree), Path) then
            Set_Cursor (Outline.Tree, Path, null, False);
         end if;
         Path_Free (Path);

         declare
            Mark_Name : aliased String :=
              Get_String (Model, Iter, Mark_Column);
            Args : constant Argument_List := (1 => Mark_Name'Unchecked_Access);
         begin
            Line := Safe_Value
              (Execute_GPS_Shell_Command
                 (Outline.Kernel, "Editor.get_line", Args));
            Column := Safe_Value
              (Execute_GPS_Shell_Command
                 (Outline.Kernel, "Editor.get_column", Args));
         end;

         Context := new Entity_Selection_Context;
         Set_Entity_Information
           (Context       => Entity_Selection_Context_Access (Context),
            Entity_Name   => Get_String (Model, Iter, Entity_Name_Column),
            Entity_Column => Column);
      else
         Context := new File_Selection_Context;
      end if;

      Set_File_Information
        (Context => Context,
         Project => Projects.No_Project,
         File    => Outline.File,
         Line    => Line);
      return Selection_Context_Access (Context);
   end Outline_Context_Factory;

   ---------------------
   -- Filter_Category --
   ---------------------

   function Filter_Category
     (Category : Language_Category) return Language_Category is
   begin
      --  No "with", "use", "#include"
      --  No constructs ("loop", "if", ...)

      if Category in Dependency_Category
        or else Category in Construct_Category
        or else Category = Cat_Representation_Clause
        or else Category = Cat_Local_Variable
      then
         return Cat_Unknown;

         --  All subprograms are grouped together

      elsif Category in Subprogram_Explorer_Category then
         return Cat_Procedure;

      elsif Category in Type_Category then
         return Cat_Type;

      end if;

      return Category;
   end Filter_Category;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr
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
      View : constant Outline_View_Access := Outline_View_Access (Outline);
      Model : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (View.Tree));
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path;
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
               Args : constant Argument_List :=
                 (1 => Mark_Name'Unchecked_Access);
            begin
               Execute_GPS_Shell_Command
                 (View.Kernel, "Editor.goto_mark", Args);
            end;
         end if;
      end if;
      return False;
   end Button_Press;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Outline : out Outline_View_Access;
      Kernel  : access Kernel_Handle_Record'Class)
   is
      Scrolled : Gtk_Scrolled_Window;
      Initial_Sort : Integer := 1;
   begin
      Outline := new Outline_View_Record;
      Outline.Kernel := Kernel_Handle (Kernel);
      Initialize_Vbox (Outline, Homogeneous => False);

      if not Get_Pref (Kernel, Outline_View_Sort_Alphabetically) then
         Initial_Sort := -1;
      end if;

      Gtk_New (Scrolled);
      Pack_Start (Outline, Scrolled, Expand => True);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Outline.Tree := Create_Tree_View
        (Column_Types       =>
           (0 => Gdk.Pixbuf.Get_Type,
            1 => GType_String,
            Entity_Name_Column => GType_String,
            Mark_Column        => GType_String), --  mark ID
         Column_Names       => (1 => null, 2 => null),
         Show_Column_Titles => False,
         Initial_Sort_On    => Initial_Sort,
         Selection_Mode     => Gtk.Enums.Selection_Single);
      Add (Scrolled, Outline.Tree);

      Outline.Icon := Gdk_New_From_Xpm_Data (var_xpm);
      Outline.File_Icon := Gdk_New_From_Xpm_Data (mini_page_xpm);

      Modify_Font (Outline.Tree, Get_Pref (Kernel, Outline_View_Font));

      Return_Callback.Object_Connect
        (Outline.Tree,
         "button_release_event",
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => Outline,
         After       => False);
      Widget_Callback.Connect
        (Outline, "destroy", On_Destroy'Access);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Outline.Tree,
         Object          => Outline,
         ID              => Outline_View_Module,
         Context_Func    => Outline_Context_Factory'Access);
   end Gtk_New;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class) is
   begin
      Clear (Outline_View_Access (Outline));
   end On_Destroy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Refresh_Outline_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Child : constant MDI_Child := Find_MDI_Child_By_Tag
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
        Gtk_Tree_Store (Get_Model (Outline.Tree));
      Iter  : Gtk_Tree_Iter := Get_Iter_First (Model);
      Args  : Argument_List (1 .. 1);
   begin
      if Iter /= Null_Iter then
         Iter := Children (Model, Iter);
      end if;

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
      Outline : constant Outline_View_Access := Outline_View_Access (View);
      Model      : constant Gtk_Tree_Store :=
        Gtk_Tree_Store (Get_Model (Outline.Tree));
      Iter, Root : Gtk_Tree_Iter := Null_Iter;
      Lang       : Language_Access;
      Handler    : LI_Handler;
      Languages  : constant GPS_Language_Handler :=
        GPS_Language_Handler (Get_Language_Handler (Outline.Kernel));
      Constructs : Construct_List;
      Show_Profiles : constant Boolean :=
        Get_Pref (Outline.Kernel, Outline_View_Profiles);
      Sort_Column : constant Gint := Freeze_Sort (Model);
      Args : Argument_List (1 .. 4);
   begin
      Push_State (Outline.Kernel, Busy);
      Clear (Outline);

      if Outline.File /= VFS.No_File then
         Handler := Get_LI_Handler_From_File (Languages, Outline.File);
         Lang := Get_Language_From_File (Languages, Outline.File);
         Append (Model, Root, Null_Iter);
         Set (Model, Root, 0, C_Proxy (Outline.File_Icon));
         Set (Model, Root, 1, "File: " & Base_Name (Outline.File));
      end if;

      if Handler = null or else Lang = null then
         Append (Model, Iter, Root);
         Set (Model, Iter, 0, C_Proxy (Outline.Icon));
         Set (Model, Iter, 1, "No outline available");
      else
         Parse_File_Constructs
           (Handler, Languages, Outline.File, Constructs);
         Constructs.Current := Constructs.First;
         while Constructs.Current /= null loop
            if Constructs.Current.Name /= null then
               if Filter_Category (Constructs.Current.Category) /=
                 Cat_Unknown
               then
                  Append (Model, Iter, Root);
                  Set (Model, Iter, 0, C_Proxy (Outline.Icon));
                  Set (Model, Iter, 1,
                       Entity_Name_Of (Constructs.Current.all,
                                       Show_Profiles => Show_Profiles));
                  Set (Model, Iter, Entity_Name_Column,
                       Constructs.Current.Name.all);

                  Args (1) := new String'(Full_Name (Outline.File).all);
                  Args (2) := new String'
                    (Constructs.Current.Sloc_Entity.Line'Img);
                  Args (3) := new String'
                    (Constructs.Current.Sloc_Entity.Column'Img);
                  Args (4) := new String'(Constructs.Current.Name'Length'Img);
                  Set (Model, Iter, Mark_Column,
                       Execute_GPS_Shell_Command
                         (Outline.Kernel, "Editor.create_mark", Args));
                  Free (Args);
               end if;
            end if;
            Constructs.Current := Constructs.Current.Next;
         end loop;
      end if;

      Expand_All (Outline.Tree);

      Pop_State (Outline.Kernel);
      Thaw_Sort (Model, Sort_Column);
   end Refresh;

   ---------------------
   -- On_Open_Outline --
   ---------------------

   procedure On_Open_Outline
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Outline : MDI_Child;
      pragma Unreferenced (Widget);
   begin
      Outline := Open_Outline (Kernel);
      Raise_Child (Outline);
      Set_Focus_Child (Get_MDI (Kernel), Outline);
   end On_Open_Outline;

   ------------------
   -- Open_Outline --
   ------------------

   function Open_Outline
     (Kernel : access Kernel_Handle_Record'Class)
      return MDI_Child
   is
      Child   : MDI_Child;
      Outline : Outline_View_Access;
      Data    : aliased Context_Hooks_Args;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);

      if Child = null then
         Gtk_New (Outline, Kernel);
         Child := Put
           (Kernel, Outline,
            Default_Width  => 215,
            Default_Height => 600,
            Position       => Position_Left,
            Module         => Outline_View_Module);
         Set_Title (Child, -"Outline View", -"Outline View");

         Data := Context_Hooks_Args'
           (Kernel  => Kernel_Handle (Kernel),
            Context => Get_Current_Context (Kernel));
         On_Context_Changed (Kernel, Data'Unchecked_Access);
      end if;

      return Child;
   end Open_Outline;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D       : constant Context_Hooks_Args := Context_Hooks_Args (Data.all);
      Outline : Outline_View_Access;
      File    : Virtual_File;
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Outline_View_Record'Tag);

      if Child /= null then
         Outline := Outline_View_Access (Get_Widget (Child));

         if D.Context.all in File_Selection_Context'Class
           and then Has_File_Information
             (File_Selection_Context_Access (D.Context))
         then
            File := File_Information
              (File_Selection_Context_Access (D.Context));
            if File /= Outline.File then
               Outline.File := File;
               Refresh (Outline);
            end if;
         else
            Outline.File := VFS.No_File;
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
      Outline_View_Module_Name : constant String := "Outline_View";
      Tools : constant String := '/' & (-"Tools");
      Command : Interactive_Command_Access;
   begin
      Register_Module
        (Module      => Outline_View_Module,
         Module_Name => Outline_View_Module_Name,
         Default_Context_Factory => Default_Factory'Access,
         Kernel      => Kernel);

      Register_Menu
        (Kernel, Tools, -"Outline View", "", On_Open_Outline'Access);

      Command := new Refresh_Outline_Command;
      Register_Contextual_Menu
        (Kernel, "Outline View Refresh",
         Action => Command,
         Filter => Action_Filter (Create (Module => Outline_View_Module_Name)),
         Label  => -"Refresh");

      GPS.Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
      Add_Default_Desktop_Item (Kernel, "Outline_View", Position_Left);

      Outline_View_Font := Param_Spec_Font
        (Gnew_Font
           (Name => "Outline-View-Font",
            Default => Get_Pref (Kernel, Param_Spec_String (Default_Font)),
            Blurb   => -"Font used in the outline view",
            Nick    => -"Outline View font"));
      Register_Property
        (Kernel, Param_Spec (Outline_View_Font), -"Outline");

      Outline_View_Profiles := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "Outline-View-Profiles",
            Default => True,
            Blurb   => -("Whether the outline view should display the profiles"
                         & " of the entities"),
            Nick    => -"Show parameter profiles"));
      Register_Property
        (Kernel, Param_Spec (Outline_View_Profiles), -"Outline");

      Outline_View_Sort_Alphabetically := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "Outline-View-Sort-Alphabetical",
            Default => True,
            Blurb   => -("If set, the entities are sorted alphabetically,"
                         & " otherwise they appear in the order they are"
                         & " found in the source file"),
            Nick    => -"Sort alphabetically"));
      Register_Property
        (Kernel, Param_Spec (Outline_View_Sort_Alphabetically), -"Outline");

      Outline_View_Link_Editor := Param_Spec_Boolean
        (Gnew_Boolean
           (Name    => "Outline-View-Link-Editor",
            Default => True,
            Blurb   => -("If true, the current subprogram in the editor is"
                         & " automatically highlighted in the outline view"),
            Nick    => -"Link with editor"));
      Register_Property
        (Kernel, Param_Spec (Outline_View_Link_Editor), -"Outline");

      Add_Hook (Kernel, Context_Changed_Hook, On_Context_Changed'Access);
      Add_Hook (Kernel, Preferences_Changed_Hook, Preferences_Changed'Access);
      Add_Hook (Kernel, Location_Changed_Hook, Location_Changed'Access);
   end Register_Module;

end Outline_View;
