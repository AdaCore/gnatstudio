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

with Glib;                    use Glib;
with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib.Object;

with Cairo;                   use Cairo;
with Cairo.Region;            use Cairo.Region;

with Gdk.Event;               use Gdk.Event;

with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;

with Gtkada.Canvas;           use Gtkada.Canvas;
with Gtkada.File_Selector;    use Gtkada.File_Selector;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;

with Browsers.Canvas;         use Browsers.Canvas;
with Commands.Interactive;    use Commands, Commands.Interactive;
with Fname;                   use Fname;
with Generic_Views;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;   use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GPS.Kernel;              use GPS.Kernel;
with Histories;               use Histories;
with Namet;                   use Namet;
with Pango.Layout;            use Pango.Layout;
with Projects;                use Projects;
with Xref;                    use Xref;

package body Browsers.Dependency_Items is

   Me : constant Trace_Handle := Create ("Browsers.Dependency");

   type Dependency_Browser_Module is new Module_ID_Record with null record;
   Dependency_Browser_Module_ID : Module_ID;

   Show_System_Files_Key : constant History_Key := "browser_show_system_files";
   Show_Implicit_Key     : constant History_Key := "browser_show_implicit";

   Include_Implicit_Cst  : aliased constant String := "include_implicit";
   Include_System_Cst    : aliased constant String := "include_system";

   overriding procedure Default_Context_Factory
     (Module  : access Dependency_Browser_Module;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

   --------------
   -- Commands --
   --------------

   type Show_Dep_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Show_Dep_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Show_Depending_On_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Show_Depending_On_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Examine_Other_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Examine_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   ------------------------
   -- Dependency browser --
   ------------------------

   type Dependency_Browser_Record is new
     Browsers.Canvas.General_Browser_Record with
   record
      Idle_Id : Glib.Main.G_Source_Id := 0;
   end record;

   function Initialize
     (View   : access Dependency_Browser_Record'Class)
      return Gtk_Widget;
   --  Creates the dependency browser and returns the focus widget

   package Dependency_Views is new Generic_Views.Simple_Views
     (Module_Name            => Dependency_Browser_Module_Name,
      View_Name              => -"Dependency Browser",
      Formal_View_Record     => Dependency_Browser_Record,
      Formal_MDI_Child       => GPS_MDI_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Graphs);
   subtype Dependency_Browser is Dependency_Views.View_Access;

   type Project_Changed_Hook_Record is new Function_No_Args with record
      Browser : Dependency_Browser;
   end record;
   type Project_Changed_Hook is access all Project_Changed_Hook_Record'Class;
   overriding procedure Execute (Hook   : Project_Changed_Hook_Record;
                                 Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project as changed

   ----------------
   -- File items --
   ----------------
   --  These items represent source files from the application

   type File_Item_Record is new Browsers.Canvas.Arrow_Item_Record with
   record
         Source  : Virtual_File;
         Project : GNATCOLL.Projects.Project_Type;
   end record;
   type File_Item is access all File_Item_Record'Class;

   procedure Gtk_New
     (Item    : out File_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      File    : Virtual_File;
      Project : GNATCOLL.Projects.Project_Type);
   --  Create a new dependency item that represents Dep

   procedure Initialize
     (Item    : access File_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      File    : Virtual_File;
      Project : GNATCOLL.Projects.Project_Type);
   --  Internal initialization function

   overriding procedure Destroy (Item : in out File_Item_Record);
   --  Free the memory associated with the item

   overriding procedure Contextual_Factory
     (Item    : access File_Item_Record;
      Context : in out Selection_Context;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Return the context to use for this item

   overriding procedure Compute_Size
     (Item          : not null access File_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo_Rectangle_Int);
   --  See doc for inherited subprogram

   ----------------------
   -- Dependency links --
   ----------------------

   type Dependency_Link_Record is new Browsers.Canvas.Browser_Link_Record
   with record
      Explicit : Boolean;
   end record;
   type Dependency_Link is access all Dependency_Link_Record'Class;

   procedure Gtk_New
     (Link     : out Dependency_Link;
      Explicit : Boolean);
   --  Create a new link

   ----------
   -- Misc --
   ----------

   type Examine_Dependencies_Idle_Data is record
      Iter             : File_Iterator_Access;
      Browser          : Dependency_Browser;
      Item             : File_Item;
      Recompute_Layout : Boolean;
   end record;
   package Dependency_Idle is
     new Glib.Main.Generic_Sources (Examine_Dependencies_Idle_Data);

   procedure Examine_Dependencies
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : GNATCOLL.Projects.Project_Type;
      Recompute_Layout : Boolean := True);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Examine_From_Dependencies
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : GNATCOLL.Projects.Project_Type;
      Interactive      : Boolean := True;
      Recompute_Layout : Boolean := True);
   --  Display the list of files that depend directly on File.
   --  if Interactive is True, then the computation is done in an idle loop, so
   --  that the application remains responsive for the user.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Open_File
     (Browser : access Glib.Object.GObject_Record'Class;
      Context : GPS.Kernel.Selection_Context);
   --  Open the file described in Context for analysis in the browser

   function Find_File
     (In_Browser : access General_Browser_Record'Class;
      Filename   : Virtual_File) return Canvas_Item;
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter
     (Kernel   : access Kernel_Handle_Record'Class;
      Explicit : Boolean;
      File     : Virtual_File) return Boolean;
   --  A filter function that decides whether Dep should be displayed in the
   --  canvas. It should return false if Dep should not be displayed.
   --
   --  Part is the unit_part of the file whose dependencies we are examining.
   --
   --  ??? This obviously needs to be modifiable from the browser itself.

   function Is_System_File (Source : Virtual_File) return Boolean;
   --  Return True if Source is a system file (runtime file for Ada).
   --  ??? This should be moved to a more general location, and perhaps be
   --  implemented with support from the project files.
   --  It could also simply use the paths to detect whether the file is in
   --  one of the predefined paths.

   procedure Browser_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Return the context to use when a contextual menu is displayed in the
   --  browser.

   procedure Destroy_Idle (Data : in out Examine_Dependencies_Idle_Data);
   --  Called when the idle loop is destroyed

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean;
   --  Idle loop for Examine_From_Dependencies

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type;
   --  Return the name of the project that contains Item

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   procedure Depends_On_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the command "uses" and "used_by"

   procedure Examine_Dependencies (Item : access Arrow_Item_Record'Class);
   procedure Examine_From_Dependencies (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the title bar buttons

   procedure Refresh_Browser (Browser : access Gtk_Widget_Record'Class);
   --  Refresh the browser after the settings have changed

   procedure Check_Dependencies
     (Browser  : access Dependency_Browser_Record'Class;
      Item     : File_Item);
   --  Check that the dependencies from Item are still valid

   ------------------------
   -- Check_Dependencies --
   ------------------------

   procedure Check_Dependencies
     (Browser : access Dependency_Browser_Record'Class;
      Item    : File_Item)
   is
      function Check_Dep
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Check that Link is still valid

      Kernel : constant Kernel_Handle := Get_Kernel (Browser);

      ---------------
      -- Check_Dep --
      ---------------

      function Check_Dep
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean
      is
         T      : constant File_Item := File_Item (Get_Dest (Link));
         Target : constant Virtual_File := T.Source;
         Iter   : File_Iterator :=
           Kernel.Databases.Find_Dependencies (Target, T.Project);
      begin
         while Iter.Has_Element loop
            if Filter (Kernel, Explicit => True, File => Iter.Element)
              and then Iter.Element = Target
            then
               return True;
            end if;

            Iter.Next;
         end loop;

         Remove_Link (Canvas, Link);
         return True;
      end Check_Dep;

   begin
      For_Each_Link
        (Get_Canvas (Browser), Check_Dep'Unrestricted_Access,
         From => Canvas_Item (Item));
   end Check_Dependencies;

   ---------------------
   -- Refresh_Browser --
   ---------------------

   procedure Refresh_Browser (Browser : access Gtk_Widget_Record'Class) is
      B    : constant Dependency_Browser := Dependency_Browser (Browser);
      Iter : Item_Iterator := Start (Get_Canvas (B));
      File : File_Item;
   begin
      --  All we do for now is check the currently displayed links, and reset
      --  the title bar buttons. It would be too costly to recompute all the
      --  displayed dependencies

      loop
         File := File_Item (Get (Iter));
         exit when File = null;

         Check_Dependencies (B, File);

         Set_Children_Shown (File, False);
         Set_Parents_Shown (File, False);

         B.Get_Canvas.Refresh (File);

         Next (Iter);
      end loop;
   end Refresh_Browser;

   -----------------------------
   -- Browser_Context_Factory --
   -----------------------------

   procedure Browser_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      Mitem : Gtk_Menu_Item;
      Sep   : Gtk_Separator_Menu_Item;
      Check : Gtk_Check_Menu_Item;
   begin
      Default_Browser_Context_Factory
        (Context, Kernel, Event_Widget, Object, Event, Menu);

      if not Has_File_Information (Context) then
         Gtk_New (Sep);
         Append (Menu, Sep);

         Gtk_New (Mitem, Label => -"Open file...");
         Append (Menu, Mitem);
         Context_Callback.Object_Connect
           (Mitem, Signal_Activate, Open_File'Access,
            Slot_Object => General_Browser (Object),
            User_Data   => Context);

         Gtk_New (Mitem, Label => -"Recompute dependencies");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, Signal_Activate, Refresh_Browser'Access, Event_Widget);

         Gtk_New (Check, Label => -"Show system files");
         Associate (Get_History (Kernel).all, Show_System_Files_Key, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh_Browser'Access, Event_Widget);

         Gtk_New (Check, Label => -"Show implicit dependencies");
         Associate (Get_History (Kernel).all, Show_Implicit_Key, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh_Browser'Access, Event_Widget);
      end if;
   end Browser_Context_Factory;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class) is
      B : constant Dependency_Browser := Dependency_Browser (Browser);
   begin
      if B.Idle_Id /= 0 then
         Remove (B.Idle_Id);
      end if;
   end On_Destroy;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Project_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Iter : Item_Iterator := Start (Get_Canvas (Hook.Browser));
      Item : Canvas_Item;
   begin
      --  Remove all items from the browser, since they are pointing to invalid
      --  Virtual_File anyway, and are no longer relevant for the new project

      loop
         Item := Get (Iter);
         exit when Item = null;

         Remove (Get_Canvas (Hook.Browser), Item);

         Next (Iter);
      end loop;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Dependency_Browser_Record'Class)
      return Gtk_Widget
   is
      Hook    : Project_Changed_Hook;
   begin
      Initialize (View, Create_Toolbar => False);
      Register_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View,
         Object          => View,
         ID              => Dependency_Views.Get_Module,
         Context_Func    => Browser_Context_Factory'Access);

      Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);

      Hook := new Project_Changed_Hook_Record'
        (Function_No_Args with Browser => View);
      Add_Hook
        (View.Kernel, GPS.Kernel.Project_Changed_Hook,
         Hook,
         Name  => "browsers.dependency_items.project_changed",
         Watch => GObject (View));

      return Gtk_Widget (View);
   end Initialize;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel           : access Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : GNATCOLL.Projects.Project_Type;
      Recompute_Layout : Boolean := True)
   is
      Browser       : Dependency_Browser;
      Item, Initial : File_Item;
      Link          : Dependency_Link;
      Intern        : Virtual_File;
      New_Item      : Boolean;
      Must_Add_Link : Boolean;
      Iter          : File_Iterator;

   begin
      --  Create the browser if it doesn't exist
      Browser := Dependency_Views.Get_Or_Create_View (Kernel, Focus => True);

      Initial := File_Item (Find_File (Browser, File));

      if Initial = null then
         Gtk_New (Initial, Browser, File, Project);
         Put (Get_Canvas (Browser), Initial);
      end if;

      if not Children_Shown (Initial) then
         Set_Children_Shown (Initial, True);

         Browser.Get_Canvas.Refresh (Initial);

         Iter := Kernel.Databases.Find_Dependencies (File, Project);

         while Iter.Has_Element loop
            Intern := Iter.Element;
            if Filter (Kernel, Explicit => True, File => Intern) then
               Item := File_Item (Find_File (Browser, Intern));
               New_Item := Item = null;
               Must_Add_Link := True;

               if New_Item then
                  Gtk_New
                    (Item, Browser, Intern,
                     Iter.Project (Get_Registry (Kernel).Tree.all));

               else
                  --  If the item already existed, chances are that the link
                  --  also existed. Don't duplicate it in that case.
                  Must_Add_Link := not Has_Link
                    (Get_Canvas (Browser), Initial, Item);
               end if;

               if Must_Add_Link then
                  Gtk_New (Link, Explicit => True);
                  Add_Link (Get_Canvas (Browser),
                            Link => Link,
                            Src  => Initial,
                            Dest => Item);
               end if;

               if New_Item then
                  Put (Get_Canvas (Browser), Item);
               end if;
            end if;

            Iter.Next;
         end loop;

         --  Center the initial item
         Show_Item (Get_Canvas (Browser), Initial);
      end if;

      if Recompute_Layout then
         Layout (Browser, Force => False);
      end if;

      Refresh_Canvas (Get_Canvas (Browser));
   end Examine_Dependencies;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Dependencies_Idle_Data) is
      procedure Clean;
      --  Clean up routine for Destroy_Idle

      -----------
      -- Clean --
      -----------

      procedure Clean is
      begin
         Data.Browser.Idle_Id := 0;
         Destroy (Data.Iter);
      end Clean;

   begin
      if Data.Recompute_Layout then
         Layout (Data.Browser, Force => False);
      end if;

      Refresh_Canvas (Get_Canvas (Data.Browser));

      --  Center the initial item
      Show_Item (Get_Canvas (Data.Browser), Data.Item);
      Clean;

   exception
      when E : others =>
         Trace (Me, E);
         Clean;
   end Destroy_Idle;

   ----------------------------
   -- Examine_Ancestors_Idle --
   ----------------------------

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean
   is
      Child : File_Item;
      Link  : Browser_Link;
      Dep   : Virtual_File;
      Kernel : constant Kernel_Handle := Get_Kernel (Data.Browser);
      Tree   : constant Project_Tree_Access := Get_Registry (Kernel).Tree;
   begin
      if not Data.Iter.Has_Element then
         return False;
      else
         Dep := Data.Iter.Element;
         if Filter (Kernel, Explicit => True, File => Dep) then
            Child := File_Item (Find_File (Data.Browser, Dep));
            if Child = null then
               Gtk_New
                 (Child, Data.Browser, Dep, Data.Iter.Project (Tree.all));
               Put (Get_Canvas (Data.Browser), Child);
            end if;

            if not Has_Link
              (Get_Canvas (Data.Browser), Child, Data.Item)
            then
               Link := new Browser_Link_Record;
               Add_Link
                 (Get_Canvas (Data.Browser), Link => Link,
                  Src => Child, Dest => Data.Item);
            end if;

            Data.Browser.Get_Canvas.Refresh (Child);
         end if;

         Data.Iter.Next;
         return True;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Examine_Ancestors_Idle;

   -------------------------------
   -- Examine_From_Dependencies --
   -------------------------------

   procedure Examine_From_Dependencies
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : Project_Type;
      Interactive      : Boolean := True;
      Recompute_Layout : Boolean := True)
   is
      Data          : Examine_Dependencies_Idle_Data;
      Browser       : Dependency_Browser;
      Item          : File_Item;

   begin
      --  Create the browser if it doesn't exist
      Browser := Dependency_Views.Get_Or_Create_View (Kernel, Focus => True);

      --  Look for an existing item corresponding to entity
      Item := File_Item (Find_File (Browser, File));

      if Item = null then
         Gtk_New (Item, Browser, File, Project);
         Put (Get_Canvas (Browser), Item);
      end if;

      Set_Parents_Shown (Item, True);
      Browser.Get_Canvas.Refresh (Item);

      Data := (Iter      => new File_Iterator'
                 (Kernel.Databases.Find_Ancestor_Dependencies (File, Project)),
               Browser   => Browser,
               Item      => Item,
               Recompute_Layout => Recompute_Layout);

      if Interactive then
         Browser.Idle_Id := Dependency_Idle.Idle_Add
           (Func     => Examine_Ancestors_Idle'Access,
            Data     => Data,
            Priority => Priority_Low,
            Notify   => Destroy_Idle'Access);
      else
         while Examine_Ancestors_Idle (Data) loop
            null;
         end loop;

         Destroy_Idle (Data);
         Layout (Browser);
         Refresh_Canvas (Get_Canvas (Browser));
      end if;

      --  All memory is freed at the end of Examine_From_Dependencies_Idle

   exception
      when E : others =>
         Trace (Me, E);
         Destroy (Data.Iter);
   end Examine_From_Dependencies;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File (Source : Virtual_File) return Boolean is
      Name : constant Filesystem_String := Source.Base_Name;
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := +Name;

      --  Check Language.Is_System_File as well, which does it depending on
      --  the specific language.

      return Is_Internal_File_Name (Name_Find, Renamings_Included => True);
   end Is_System_File;

   ------------
   -- Filter --
   ------------

   function Filter
     (Kernel   : access Kernel_Handle_Record'Class;
      Explicit : Boolean;
      File     : Virtual_File) return Boolean
   is
      Explicit_Dependency : Boolean;
      System_File         : Boolean;

   begin
      --  Only show explicit dependencies, not implicit ones
      Explicit_Dependency :=
        Get_History (Get_History (Kernel).all, Show_Implicit_Key)
        or else Explicit;

      --  Do not display dependencies on runtime files
      System_File :=
        Get_History (Get_History (Kernel).all, Show_System_Files_Key)
        or else not Is_System_File (File);

      return Explicit_Dependency and then System_File;
   end Filter;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (In_Browser : access General_Browser_Record'Class;
      Filename   : Virtual_File) return Canvas_Item
   is
      Iter : Item_Iterator := Start (Get_Canvas (In_Browser));
      Item : Canvas_Item;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null
           or else File_Item (Item).Source = Filename;

         Next (Iter);
      end loop;
      return Item;
   end Find_File;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Dep_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Examine_Dependencies
        (Get_Kernel (Context.Context),
         File_Information (Context.Context),
         Project_Information (Context.Context));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Depending_On_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
   begin
      Examine_From_Dependencies
        (Get_Kernel (Context.Context),
         File_Information (Context.Context),
         Project_Information (Context.Context));
      return Commands.Success;
   end Execute;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Browser : access GObject_Record'Class;
      Context : Selection_Context)
   is
      File : constant Virtual_File := Select_File
        (Title             => -"Select File",
         Parent            => Gtk_Window (Get_Toplevel (Gtk_Widget (Browser))),
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
         Kind              => Open_File,
         File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
         Pattern_Name      => -"All files;Ada files;C/C++ files",
         History           => Get_History (Get_Kernel (Context)));
      --  ??? Should set up filters to only open file from the current project

      Project : Project_Type;
      B  : constant Dependency_Browser := Dependency_Browser (Browser);
   begin
      if File /= GNATCOLL.VFS.No_File then
         --  Take the first possible project
         Project := Get_Registry
           (B.Get_Kernel).Tree.Info_Set (File).First_Element.Project;
         Examine_Dependencies (Get_Kernel (Context), File, Project);
      end if;
   end Open_File;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Dependency_Browser_Module;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject)
   is
      pragma Unreferenced (Module);
      Browser : constant Dependency_Browser :=
        Dependency_Views.View_From_Widget (Child);
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

   --------------------------------
   -- Depends_On_Command_Handler --
   --------------------------------

   procedure Depends_On_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Instance : constant Class_Instance :=
                   Nth_Arg (Data, 1, Get_File_Class (Kernel));
      File     : constant Virtual_File := Get_Data (Instance);
      Project  : Project_Type;
   begin
      --  ??? We do not know exactly which project to use, so we chose the
      --  first matching one

      Project := Get_Registry
        (Kernel).Tree.Info_Set (File).First_Element.Project;

      if Command = "uses" then
         Examine_Dependencies (Kernel, File => File, Project => Project);

      elsif Command = "used_by" then
         Examine_From_Dependencies (Kernel, File => File, Project => Project);

      elsif Command = "imports" then
         Name_Parameters (Data, (1 => Include_Implicit_Cst'Access,
                                 2 => Include_System_Cst'Access));
         declare
            Dependency : Virtual_File;

            Include_Implicit : constant Boolean := Nth_Arg (Data, 2, False);
            pragma Unreferenced (Include_Implicit);

            Include_System   : constant Boolean := Nth_Arg (Data, 3, True);
            Iter : File_Iterator := Kernel.Databases.Find_Dependencies
              (File, Project);
         begin
            Set_Return_Value_As_List (Data);

            while Iter.Has_Element loop
               Dependency := Iter.Element;

               if Include_System or else not Is_System_File (Dependency) then
                  Set_Return_Value
                    (Data,
                     Create_File (Get_Script (Data), Dependency));
               end if;

               Iter.Next;
            end loop;
         end;

      elsif Command = "imported_by" then
         Name_Parameters (Data, (1 => Include_Implicit_Cst'Access,
                                 2 => Include_System_Cst'Access));
         declare
            Dependency : Virtual_File;

            Include_Implicit : constant Boolean := Nth_Arg (Data, 2, False);
            pragma Unreferenced (Include_Implicit);

            Include_System   : constant Boolean := Nth_Arg (Data, 3, True);
            Iter       : File_Iterator :=
              Kernel.Databases.Find_Ancestor_Dependencies
                (File => File, Project => Project);
         begin
            Set_Return_Value_As_List (Data);

            while Iter.Has_Element loop
               Dependency := Iter.Element;

               if Include_System or else not Is_System_File (Dependency) then
                  Set_Return_Value
                    (Data, Create_File (Get_Script (Data), Dependency));
               end if;

               Iter.Next;
            end loop;
         end;
      end if;
   end Depends_On_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
      Filter  : constant Action_Filter :=
                  (not Lookup_Filter (Kernel, "Entity"))
                  and Lookup_Filter (Kernel, "In project");

   begin
      Dependency_Browser_Module_ID := new Dependency_Browser_Module;
      Dependency_Views.Register_Module
        (Kernel,
         Dependency_Browser_Module_ID);

      --  ??? Sensitivity will be handled in the hook "contextual_menu"

      Command := new Show_Dep_Command;
      Register_Contextual_Menu
        (Kernel, "File dependencies",
         Action      => Command,
         Label       => -"Show dependencies for %f",
         Filter      => Filter,
         Stock_Image => Stock_Go_Forward);

      Command := new Show_Depending_On_Command;
      Register_Contextual_Menu
        (Kernel, "File depending on",
         Action      => Command,
         Label       => -"Show files depending on %f",
         Filter      => Filter,
         Stock_Image => Stock_Go_Back);

      Command := new Examine_Other_File_Command;
      Register_Contextual_Menu
        (Kernel, "Analyze deps for other file",
         Label  => -"Analyze other file (spec or body)",
         Action => Command,
         Filter =>
           Create (Module => Dependency_Browser_Module_Name) and Filter);

      Register_Command
        (Kernel, "uses",
         Class   => Get_File_Class (Kernel),
         Handler => Depends_On_Command_Handler'Access);
      Register_Command
        (Kernel, "used_by",
         Class   => Get_File_Class (Kernel),
         Handler => Depends_On_Command_Handler'Access);
      Register_Command
        (Kernel, "imports",
         Minimum_Args => 0,
         Maximum_Args => 2,
         Class        => Get_File_Class (Kernel),
         Handler      => Depends_On_Command_Handler'Access);
      Register_Command
        (Kernel, "imported_by",
         Minimum_Args => 0,
         Maximum_Args => 2,
         Class        => Get_File_Class (Kernel),
         Handler      => Depends_On_Command_Handler'Access);
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out File_Item;
      Browser : access General_Browser_Record'Class;
      File    : Virtual_File;
      Project : Project_Type) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Browser, File, Project);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item    : access File_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      File    : Virtual_File;
      Project : Project_Type) is
   begin
      Initialize (Item, Browser, File.Display_Base_Name,
                  Examine_From_Dependencies'Access,
                  Examine_Dependencies'Access);
      Item.Source := File;
      Item.Project := Project;
      Recompute_Size (Item);
   end Initialize;

   -------------------------------
   -- Examine_From_Dependencies --
   -------------------------------

   procedure Examine_From_Dependencies
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_From_Dependencies
        (Get_Kernel (Get_Browser (Item)),
         File_Item (Item).Source,
         File_Item (Item).Project);
   end Examine_From_Dependencies;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Dependencies
        (Get_Kernel (Get_Browser (Item)),
         File_Item (Item).Source,
         File_Item (Item).Project);
   end Examine_Dependencies;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Link     : out Dependency_Link;
      Explicit : Boolean) is
   begin
      Link := new Dependency_Link_Record;
      Link.Explicit := Explicit;
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Item : in out File_Item_Record) is
   begin
      Destroy (Arrow_Item_Record (Item));
   end Destroy;

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type is
   begin
      return Item.Project;
   end Project_Of;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Examine_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      B : constant Dependency_Browser :=
        Dependency_Views.Get_Or_Create_View (Kernel, Focus => True);
      Project : constant Project_Type := Project_Information (Context.Context);
      Other_File : constant Virtual_File :=
        Get_Registry (Kernel).Tree.Other_File
           (File_Information (Context.Context));
      Item       : File_Item;
   begin
      if Other_File /= GNATCOLL.VFS.No_File then
         Item := File_Item (Find_File (B, Other_File));
         if Item = null then
            Gtk_New (Item, B, Other_File, Project);
            Put (Get_Canvas (B), Item);

            Layout (B, Force => False);
            Refresh_Canvas (Get_Canvas (B));

         end if;
         Clear_Selection (Get_Canvas (B));
         Add_To_Selection (Get_Canvas (B), Item);
         return Commands.Success;
      end if;
      return Commands.Failure;
   end Execute;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   overriding procedure Contextual_Factory
     (Item    : access File_Item_Record;
      Context : in out Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Browser, Menu, Event);
      Src      : constant Virtual_File := Item.Source;
   begin
      Set_File_Information
        (Context,
         Files   => (1 => Src),
         Project => Project_Of (Item));
   end Contextual_Factory;

   ------------------
   -- Compute_Size --
   ------------------

   overriding procedure Compute_Size
     (Item          : not null access File_Item_Record;
      Layout        : not null access Pango_Layout_Record'Class;
      Width, Height : out Glib.Gint;
      Title_Box     : in out Cairo_Rectangle_Int) is
   begin
      --  Just reserve a little bit of space so that there is something else
      --  than the title bar.

      Compute_Size
        (Arrow_Item_Record (Item.all)'Access, Layout, Width, Height,
         Title_Box);
      Width := Title_Box.Width;
      Height := Height + 10;
   end Compute_Size;

end Browsers.Dependency_Items;
