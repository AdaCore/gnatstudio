-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2004                      --
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

with Ada.Exceptions;       use Ada.Exceptions;

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;
with Glib.Xml_Int;         use Glib.Xml_Int;
with Gdk.Event;            use Gdk.Event;
with Gtk.Check_Menu_Item;  use Gtk.Check_Menu_Item;
with Gtk.Image;            use Gtk.Image;
with Gtk.Image_Menu_Item;  use Gtk.Image_Menu_Item;
with Gtk.Main;             use Gtk.Main;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Widget;           use Gtk.Widget;
with Gtk.Window;           use Gtk.Window;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.MDI;           use Gtkada.MDI;
with Pango.Layout;         use Pango.Layout;

with Browsers.Canvas;           use Browsers.Canvas;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Contexts;     use Glide_Kernel.Contexts;
with Glide_Kernel.Hooks;        use Glide_Kernel.Hooks;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with Entities.Queries;          use Entities.Queries;
with Entities;                  use Entities;
with Traces;                    use Traces;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with Fname;                     use Fname;
with Namet;                     use Namet;
with Histories;                 use Histories;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with VFS;                       use VFS;

with Ada.Exceptions;            use Ada.Exceptions;

with String_Utils;              use String_Utils;

package body Browsers.Dependency_Items is

   Me : constant Debug_Handle := Create ("Browsers.Dependency");

   Dependency_Browser_Module_ID : Module_ID;

   Show_System_Files_Key : constant History_Key := "browser_show_system_files";
   Show_Implicit_Key     : constant History_Key := "browser_show_implicit";

   ------------------------
   -- Dependency browser --
   ------------------------

   type Dependency_Browser_Record is new
     Browsers.Canvas.General_Browser_Record with
   record
      Idle_Id : Gtk.Main.Idle_Handler_Id;
   end record;
   type Dependency_Browser is access all Dependency_Browser_Record'Class;

   type Project_Changed_Hook_Record is new Hook_No_Args_Record with record
      Browser         : Dependency_Browser;
   end record;
   type Project_Changed_Hook is access all Project_Changed_Hook_Record'Class;
   procedure Execute (Hook   : Project_Changed_Hook_Record;
                      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project as changed

   ----------------
   -- File items --
   ----------------
   --  These items represent source files from the application.

   type File_Item_Record is new Browsers.Canvas.Arrow_Item_Record with
   record
      Source : Source_File;
   end record;
   type File_Item is access all File_Item_Record'Class;

   procedure Gtk_New
     (Item    : out File_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      File    : Source_File);
   --  Create a new dependency item that represents Dep.

   procedure Gtk_New
     (Item            : out File_Item;
      Browser         : access Browsers.Canvas.General_Browser_Record'Class;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename : Virtual_File);
   --  Create a new dependency item directly from a source filename

   procedure Initialize
     (Item    : access File_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      File    : Source_File);
   --  Internal initialization function

   function Get_Source
     (Item : access File_Item_Record'Class) return Source_File;
   pragma Inline (Get_Source);
   --  Return the source file associated with Item

   procedure Destroy (Item : in out File_Item_Record);
   --  Free the memory associated with the item

   function Contextual_Factory
     (Item  : access File_Item_Record;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use for this item

   procedure Resize_And_Draw
     (Item             : access File_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class);
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
     (Link      : out Dependency_Link;
      Explicit  : Boolean);
   --  Create a new link.

   ----------
   -- Misc --
   ----------

   type Examine_Dependencies_Idle_Data is record
      Iter    : Dependency_Iterator_Access;
      Browser : Dependency_Browser;
      Item    : File_Item;
      Recompute_Layout : Boolean;
   end record;
   package Dependency_Idle is new Gtk.Main.Idle
     (Examine_Dependencies_Idle_Data);

   procedure Examine_Dependencies
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      File       : Virtual_File;
      Recompute_Layout : Boolean := True);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Examine_From_Dependencies
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Interactive : Boolean := True;
      Recompute_Layout : Boolean := True);
   --  Display the list of files that depend directly on File.
   --  if Interactive is True, then the computation is done in an idle loop, so
   --  that the application remains responsive for the user.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Open_File
     (Browser : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Open the file described in Context for analysis in the browser.

   function Find_File
     (In_Browser : access General_Browser_Record'Class;
      Filename : Virtual_File) return Canvas_Item;
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter
     (Kernel   : access Kernel_Handle_Record'Class;
      Explicit : Boolean;
      File     : Source_File)
      return Boolean;
   --  A filter function that decides whether Dep should be displayed in the
   --  canvas. It should return false if Dep should not be displayed.
   --
   --  Part is the unit_part of the file whose dependencies we are examining.
   --
   --  ??? This obviously needs to be modifiable from the browser itself.

   function Is_System_File (Source : Source_File) return Boolean;
   --  Return True if Source is a system file (runtime file for Ada).
   --  ??? This should be moved to a more general location, and perhaps be
   --  implemented with support from the project files.
   --  It could also simply use the paths to detect whether the file is in
   --  one of the predefined paths.

   procedure On_Dependency_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Dependency Browser

   procedure Edit_Dependencies_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Examine the dependencies of a specific file

   procedure Edit_Ancestor_Dependencies_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Edit the ancestor dependencies for a specific file.

   procedure On_Examine_Other_File
     (Browser : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Insert a new item for the file described in Context.

   procedure Browser_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the appropriate contextual menus

   function Browser_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Return the context to use when a contextual menu is displayed in the
   --  browser.

   function Open_Dependency_Browser
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child;
   --  Open a new browser that supports all the types described in
   --  Browser_Type.
   --  If there is already a browser in GPS that handles all the types
   --  Browser_Type, we re-use this one instead.

   procedure Destroy_Idle (Data : in out Examine_Dependencies_Idle_Data);
   --  Called when the idle loop is destroyed.

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean;
   --  Idle loop for Examine_From_Dependencies

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type;
   --  Return the name of the project that contains Item.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Support functions for the MDI

   function Create_Dependency_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Dependency_Browser;
   --  Create a new dependency browser

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class);
   --  Called when the browser is destroyed

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Create a current kernel context, based on the currently selected item

   procedure Depends_On_Command_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Handler for the command "uses" and "used_by"

   procedure Examine_Dependencies (Item : access Arrow_Item_Record'Class);
   procedure Examine_From_Dependencies (Item : access Arrow_Item_Record'Class);
   --  Callbacks for the title bar buttons

   procedure Refresh_Browser (Browser : access Gtk_Widget_Record'Class);
   --  Refresh the browser after the settings have changed

   procedure Check_Dependencies
     (Browser  : access Dependency_Browser_Record'Class;
      Item     : File_Item);
   --  Check that the dependencies from Item are still valid.

   ------------------------
   -- Check_Dependencies --
   ------------------------

   procedure Check_Dependencies
     (Browser  : access Dependency_Browser_Record'Class;
      Item     : File_Item)
   is
      function Check_Dep
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Check that Link is still valid

      Source    : constant Source_File := Get_Source (Item);

      ---------------
      -- Check_Dep --
      ---------------

      function Check_Dep
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean
      is
         Iter        : File_Dependency_Iterator;
         Target      : constant File_Item := File_Item (Get_Dest (Link));
         Target_File : constant Source_File := Get_Source (Target);
      begin
         Find_Dependencies (Iter, Source);

         while not At_End (Iter) loop
            if Filter (Get_Kernel (Browser), Is_Explicit (Iter), Get (Iter))
              and then Get (Iter) = Target_File
            then
               return True;
            end if;

            Next (Iter);
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
      B         : constant Dependency_Browser := Dependency_Browser (Browser);
      Iter      : Item_Iterator := Start (Get_Canvas (B));
      File      : File_Item;
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
         Redraw_Title_Bar (File);

         Next (Iter);
      end loop;
   end Refresh_Browser;

   -----------------------------
   -- Browser_Context_Factory --
   -----------------------------

   function Browser_Context_Factory
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access
   is
      Context : Selection_Context_Access;
      Mitem   : Gtk_Menu_Item;
      Check   : Gtk_Check_Menu_Item;
   begin
      Context := Default_Browser_Context_Factory
        (Kernel, Event_Widget, Object, Event, Menu);

      if not (Context.all in File_Selection_Context'Class) then
         Gtk_New (Mitem, "");
         Append (Menu, Mitem);

         Gtk_New (Mitem, Label => -"Open file...");
         Append (Menu, Mitem);
         Context_Callback.Object_Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller (Open_File'Access),
            Slot_Object => General_Browser (Object),
            User_Data   => Context);

         Gtk_New (Mitem, Label => -"Refresh");
         Append (Menu, Mitem);
         Widget_Callback.Object_Connect
           (Mitem, "activate",
            Widget_Callback.To_Marshaller (Refresh_Browser'Access),
            Event_Widget);

         Gtk_New (Check, Label => -"Show system files");
         Associate (Get_History (Kernel).all, Show_System_Files_Key, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled",
            Widget_Callback.To_Marshaller (Refresh_Browser'Access),
            Event_Widget);

         Gtk_New (Check, Label => -"Show implicit dependencies");
         Associate (Get_History (Kernel).all, Show_Implicit_Key, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, "toggled",
            Widget_Callback.To_Marshaller (Refresh_Browser'Access),
            Event_Widget);
      end if;

      return Context;
   end Browser_Context_Factory;

   -------------------------------
   -- Create_Dependency_Browser --
   -------------------------------

   function Create_Dependency_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Dependency_Browser
   is
      Browser : Dependency_Browser;
   begin
      Browser := new Dependency_Browser_Record;
      Initialize (Browser, Kernel, Create_Toolbar => False);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Dependency_Browser_Module_ID,
         Context_Func    => Browser_Context_Factory'Access);

      Widget_Callback.Connect
        (Browser, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));
      return Browser;
   end Create_Dependency_Browser;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class) is
      B : constant Dependency_Browser := Dependency_Browser (Browser);
   begin
      if B.Idle_Id /= 0 then
         Idle_Remove (B.Idle_Id);
      end if;
   end On_Destroy;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Project_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Iter : Item_Iterator := Start (Get_Canvas (Hook.Browser));
      Item : Canvas_Item;
   begin
      --  Remove all items from the browser, since they are pointing to invalid
      --  Source_File anyway, and are no longer relevant for the new project

      loop
         Item := Get (Iter);
         exit when Item = null;

         Remove (Get_Canvas (Hook.Browser), Item);

         Next (Iter);
      end loop;
   end Execute;

   -----------------------------
   -- Open_Dependency_Browser --
   -----------------------------

   function Open_Dependency_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : MDI_Child;
      Browser : Dependency_Browser;
      Hook    : Project_Changed_Hook;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Dependency_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Create_Dependency_Browser (Kernel);
         Child := Put
           (Kernel, Browser,
            Focus_Widget => Gtk_Widget (Get_Canvas (Browser)),
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height),
            Module => Dependency_Browser_Module_ID);
         Set_Focus_Child (Child);
         Set_Title (Child, -"Dependency Browser");

         Hook := new Project_Changed_Hook_Record'
           (Hook_No_Args_Record with
            Browser         => Browser);
         Add_Hook
           (Kernel, Glide_Kernel.Project_Changed_Hook,
            Hook, Watch => GObject (Browser));
      end if;

      return Child;
   end Open_Dependency_Browser;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Recompute_Layout : Boolean := True)
   is
      Browser       : Dependency_Browser;
      Child_Browser : MDI_Child;
      Item, Initial : File_Item;
      Link          : Dependency_Link;
      Source        : Source_File;
      Intern        : Source_File;
      New_Item      : Boolean;
      Must_Add_Link : Boolean;
      Iter          : File_Dependency_Iterator;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Dependency_Browser (Kernel);
      Browser := Dependency_Browser (Get_Widget (Child_Browser));

      Source := Get_Or_Create (Get_Database (Kernel), File);
      Initial := File_Item (Find_File (Browser, File));

      if Initial = null then
         Gtk_New (Initial, Browser, Kernel, File);
         Put (Get_Canvas (Browser), Initial);
         Refresh (Initial);
      end if;

      if not Children_Shown (Initial) then
         Set_Children_Shown (Initial, True);
         Redraw_Title_Bar (Initial);

         Find_Dependencies (Iter => Iter, File => Source);

         while not At_End (Iter) loop
            Intern := Get (Iter);
            if Filter (Kernel, Is_Explicit (Iter), Intern) then
               Item := File_Item (Find_File (Browser, Get_Filename (Intern)));
               New_Item := Item = null;
               Must_Add_Link := True;

               if New_Item then
                  Gtk_New (Item, Browser, Intern);

               else
                  --  If the item already existed, chances are that the link
                  --  also existed. Don't duplicate it in that case.
                  Must_Add_Link := not Has_Link
                    (Get_Canvas (Browser), Initial, Item);
               end if;

               if Must_Add_Link then
                  Gtk_New (Link, Is_Explicit (Iter));
                  Add_Link (Get_Canvas (Browser),
                            Link => Link,
                            Src  => Initial,
                            Dest => Item);
               end if;

               Refresh (Item);

               if New_Item then
                  Put (Get_Canvas (Browser), Item);
               end if;
            end if;

            Next (Iter);
         end loop;

         --  Center the initial item
         Show_Item (Get_Canvas (Browser), Initial);
      end if;

      if Recompute_Layout then
         Layout (Browser, Force => False);
      end if;

      Refresh_Canvas (Get_Canvas (Browser));
      Pop_State (Kernel_Handle (Kernel));

   exception
      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_Dependencies;

   ------------------
   -- Destroy_Idle --
   ------------------

   procedure Destroy_Idle (Data : in out Examine_Dependencies_Idle_Data) is
      procedure Clean;
      --  Clean up routine for Destroy_Idle.

      procedure Clean is
      begin
         Data.Browser.Idle_Id := 0;
         Destroy (Data.Iter);
         Pop_State (Get_Kernel (Data.Browser));
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
         Clean;
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Destroy_Idle;

   ----------------------------
   -- Examine_Ancestors_Idle --
   ----------------------------

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean
   is
      Child : File_Item;
      Link  : Browser_Link;
      Dep   : Source_File;
   begin
      if At_End (Data.Iter.all) then
         return False;
      else
         Dep := Get (Data.Iter.all);
         if Dep /= null
           and then Filter
             (Get_Kernel (Data.Browser), Is_Explicit (Data.Iter.all), Dep)
         then
            declare
               File : constant Virtual_File := Get_Filename (Dep);
            begin
               Child := File_Item (Find_File (Data.Browser, File));
               if Child = null then
                  Gtk_New (Child, Data.Browser,
                           Get_Kernel (Data.Browser), File);
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

               Refresh (Child);
            end;
         end if;

         Next (Data.Iter.all);
         return True;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Examine_Ancestors_Idle;

   -------------------------------
   -- Examine_From_Dependencies --
   -------------------------------

   procedure Examine_From_Dependencies
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      File        : Virtual_File;
      Interactive : Boolean := True;
      Recompute_Layout : Boolean := True)
   is
      Data          : Examine_Dependencies_Idle_Data;
      Browser       : Dependency_Browser;
      Child_Browser : MDI_Child;
      Item          : File_Item;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Dependency_Browser (Kernel);
      Browser := Dependency_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Item := File_Item (Find_File (Browser, File));

      if Item = null then
         Gtk_New (Item, Browser, Kernel, File);
         Put (Get_Canvas (Browser), Item);
         Refresh (Item);
      end if;

      Set_Parents_Shown (Item, True);
      Redraw_Title_Bar (Item);

      Data := (Iter             => new Dependency_Iterator,
               Browser          => Browser,
               Item             => Item,
               Recompute_Layout => Recompute_Layout);
      Find_Ancestor_Dependencies
        (Iter         => Data.Iter.all,
         File         => Get_Or_Create (Get_Database (Kernel), File),
         Include_Self => False);

      if Interactive then
         Browser.Idle_Id := Dependency_Idle.Add
           (Cb       => Examine_Ancestors_Idle'Access,
            D        => Data,
            Priority => Priority_Low_Idle,
            Destroy  => Destroy_Idle'Access);
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
         Pop_State (Kernel_Handle (Kernel));
         Destroy (Data.Iter);
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Examine_From_Dependencies;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File (Source : Source_File) return Boolean is
      Name : constant String := Base_Name (Get_Filename (Source));
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;

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
      File     : Source_File)
      return Boolean
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
      Filename : Virtual_File)
      return Canvas_Item
   is
      Iter : Item_Iterator := Start (Get_Canvas (In_Browser));
      Item : Canvas_Item;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null
           or else Get_Filename (Get_Source (File_Item (Item))) = Filename;

         Next (Iter);
      end loop;
      return Item;
   end Find_File;

   ---------------------------
   -- On_Dependency_Browser --
   ---------------------------

   procedure On_Dependency_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Child : MDI_Child;
      pragma Unreferenced (Widget, Child);

      Context : Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      Ref (Context);
      Child := Open_Dependency_Browser (Kernel);

      if Context /= null
        and then Context.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Context))
      then
         Examine_Dependencies
           (Kernel,
            File_Information (File_Selection_Context_Access (Context)));
      end if;

      Unref (Context);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception in On_Dependency_Browser "
                & Exception_Information (E));
         Unref (Context);
   end On_Dependency_Browser;

   ---------------------------------------
   -- Edit_Dependencies_From_Contextual --
   ---------------------------------------

   procedure Edit_Dependencies_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Examine_Dependencies
        (Get_Kernel (Context),
         File_Information (File_Selection_Context_Access (Context)));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception in Edit_Dependencies_From_Contextual "
                & Exception_Information (E));
   end Edit_Dependencies_From_Contextual;

   ------------------------------------------------
   -- Edit_Ancestor_Dependencies_From_Contextual --
   ------------------------------------------------

   procedure Edit_Ancestor_Dependencies_From_Contextual
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Examine_From_Dependencies
        (Get_Kernel (Context),
         File_Information (File_Selection_Context_Access (Context)));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception in Edit_Dependencies_From_Contextual "
                & Exception_Information (E));
   end Edit_Ancestor_Dependencies_From_Contextual;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Browser  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      File : constant Virtual_File :=
        Select_File
          (Title             => -"Select File",
           Parent            =>
             Gtk_Window (Get_Toplevel (Gtk_Widget (Browser))),
           Use_Native_Dialog =>
             Get_Pref (Get_Kernel (Context), Use_Native_Dialogs),
           Kind              => Open_File,
           History           => Get_History (Get_Kernel (Context)));
      --  ??? Should set up filters to only open file from the current project.

   begin
      if File /= VFS.No_File then
         Examine_Dependencies (Get_Kernel (Context), File);
      end if;
   end Open_File;

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
      --  File selection (for instance from the explorer)

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);
         if Has_File_Information (File_Context) then
            declare
               Name : constant String :=
                 Krunch (Base_Name (File_Information (File_Context)));
            begin
               Gtk_New
                 (Item, Label => (-"Show dependencies for ") & Name);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Edit_Dependencies_From_Contextual'Access),
                  Selection_Context_Access (Context));

               Gtk_New
                 (Item, Label => (-"Show files depending on ") & Name);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Edit_Ancestor_Dependencies_From_Contextual'Access),
                  Selection_Context_Access (Context));
            end;
         end if;
      end if;
   end Browser_Contextual_Menu;

   ---------------------
   -- Default_Factory --
   ---------------------

   function Default_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Kernel);
      Browser : constant Dependency_Browser := Dependency_Browser (Child);
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

   --------------------------------
   -- Depends_On_Command_Handler --
   --------------------------------

   procedure Depends_On_Command_Handler
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
      Instance   : constant Class_Instance :=
        Nth_Arg (Data, 1, Get_File_Class (Kernel));
      File       : constant File_Info := Get_Data (Instance);
   begin
      if Command = "uses" then
         Examine_Dependencies (Kernel, File => Get_File (File));
      elsif Command = "used_by" then
         Examine_From_Dependencies (Kernel, File => Get_File (File));
      end if;
   end Depends_On_Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools");
   begin
      Register_Module
        (Module                  => Dependency_Browser_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => Dependency_Browser_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu (Kernel, Tools, -"Dependency Browser", "",
                     On_Dependency_Browser'Access);

      Register_Command
        (Kernel, "uses",
         Class        => Get_File_Class (Kernel),
         Handler      => Depends_On_Command_Handler'Access);
      Register_Command
        (Kernel, "used_by",
         Class        => Get_File_Class (Kernel),
         Handler      => Depends_On_Command_Handler'Access);
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out File_Item;
      Browser : access General_Browser_Record'Class;
      File    : Source_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Browser, File);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item            : out File_Item;
      Browser         : access General_Browser_Record'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : Virtual_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Browser,
                  Get_Or_Create (Get_Database (Kernel), Source_Filename));
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item : access File_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      File  : Source_File) is
   begin
      Initialize (Item, Browser, Base_Name (Get_Filename (File)),
                  Examine_From_Dependencies'Access,
                  Examine_Dependencies'Access);
      Item.Source := File;
   end Initialize;

   -------------------------------
   -- Examine_From_Dependencies --
   -------------------------------

   procedure Examine_From_Dependencies
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_From_Dependencies
        (Get_Kernel (Get_Browser (Item)),
         Get_Filename (File_Item (Item).Source));
   end Examine_From_Dependencies;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Dependencies
        (Get_Kernel (Get_Browser (Item)),
         Get_Filename (File_Item (Item).Source));
   end Examine_Dependencies;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Item : access File_Item_Record'Class)
      return Source_File is
   begin
      return Item.Source;
   end Get_Source;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Link : out Dependency_Link;
      Explicit : Boolean) is
   begin
      Link := new Dependency_Link_Record;
      Link.Explicit := Explicit;
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out File_Item_Record) is
   begin
      Destroy (Arrow_Item_Record (Item));
   end Destroy;

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type
   is
      File_Name : constant Virtual_File := Get_Filename (Get_Source (Item));
      P : Project_Type;
   begin
      P := Get_Project_From_File
        (Get_Registry (Get_Kernel (Get_Browser (Item))).all, File_Name);

      if P = No_Project then
         Trace (Me, "Project_Of return No_Project for "
                & Full_Name (File_Name).all);
      end if;

      return P;
   end Project_Of;

   ---------------------------
   -- On_Examine_Other_File --
   ---------------------------

   procedure On_Examine_Other_File
     (Browser : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      B : constant Dependency_Browser := Dependency_Browser (Browser);
      C : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Item : File_Item;
      Other_File : constant Virtual_File := Create
        (Other_File_Base_Name
           (Project_Information (C), File_Information (C)),
         Project_Information (C));
   begin
      if Other_File /= VFS.No_File then
         Item := File_Item (Find_File (B, Other_File));
         if Item = null then
            Gtk_New (Item, B,  Get_Kernel (Context), Other_File);
            Put (Get_Canvas (B), Item);
            Refresh (Item);

            Layout (B, Force => False);
            Refresh_Canvas (Get_Canvas (B));

         end if;
         Clear_Selection (Get_Canvas (B));
         Add_To_Selection (Get_Canvas (B), Item);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end On_Examine_Other_File;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access File_Item_Record;
      Browser : access General_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event);
      Context : constant Selection_Context_Access :=
         new File_Selection_Context;
      Src     : constant Source_File := Get_Source (Item);
      Filename : constant Virtual_File := Get_Filename (Src);
      Base  : constant String := Krunch (Base_Name (Filename));
      Mitem : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
   begin
      Set_File_Information
        (File_Selection_Context_Access (Context),
         File         => Filename,
         Project      => Project_Of (Item));

      if Menu /= null then
         Gtk_New (Mitem, -"Analyze other file (spec or body)");
         Add (Menu, Mitem);
         Context_Callback.Object_Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller (On_Examine_Other_File'Access),
            User_Data   => Context,
            Slot_Object => Browser);

         Gtk_New (Mitem, Label => (-"Show dependencies for ") & Base);
         Gtk_New (Pix, Get_Children_Arrow (Get_Browser (Item)));
         Set_Image (Mitem, Pix);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Dependencies_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Children_Shown (Item));

         Gtk_New
           (Mitem, Label => (-"Show files depending on ") & Base);
         Gtk_New (Pix, Get_Parents_Arrow (Get_Browser (Item)));
         Set_Image (Mitem, Pix);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Ancestor_Dependencies_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Parents_Shown (Item));
      end if;

      return Context;
   end Contextual_Factory;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
      Child : MDI_Child;
   begin
      if Node.Tag.all = "Dependency_Browser" then
         Child := Put
           (User, Gtk_Widget (Create_Dependency_Browser (User)),
            Default_Width  => Get_Pref (User, Default_Widget_Width),
            Default_Height => Get_Pref (User, Default_Widget_Height),
            Module => Dependency_Browser_Module_ID);
         Set_Title (Child, -"Dependency Browser");

         return Child;
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in Dependency_Browser_Record'Class then
         N := new Node;
         N.Tag := new String'("Dependency_Browser");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access File_Item_Record;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango.Layout.Pango_Layout_Record'Class) is
   begin
      --  Just reserve a little bit of space so that there is something else
      --  than the title bar.

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access,
         Width, Height + 10,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);
   end Resize_And_Draw;

end Browsers.Dependency_Items;
