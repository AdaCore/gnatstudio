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

with Ada.Exceptions;       use Ada.Exceptions;

with Glib;                 use Glib;
with Glib.Convert;         use Glib.Convert;
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
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with Src_Info.Queries;          use Src_Info.Queries;
with Src_Info;                  use Src_Info;
with Traces;                    use Traces;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with Fname;                     use Fname;
with Namet;                     use Namet;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with Histories;                 use Histories;

with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

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

   ----------------
   -- File items --
   ----------------
   --  These items represent source files from the application.

   type File_Item_Record is new Browsers.Canvas.Arrow_Item_Record with
   record
      Source : Src_Info.Internal_File;
   end record;
   type File_Item is access all File_Item_Record'Class;

   procedure Gtk_New
     (Item    : out File_Item;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      File    : Src_Info.Internal_File);
   --  Create a new dependency item that represents Dep.

   procedure Gtk_New
     (Item            : out File_Item;
      Browser         : access Browsers.Canvas.General_Browser_Record'Class;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename : String);
   --  Create a new dependency item directly from a source filename

   procedure Initialize
     (Item    : access File_Item_Record'Class;
      Browser : access Browsers.Canvas.General_Browser_Record'Class;
      File    : Src_Info.Internal_File);
   --  Internal initialization function

   function Get_Source (Item : access File_Item_Record'Class)
      return Src_Info.Internal_File;
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
      Dep : Src_Info.Dependency_Info;
   end record;
   type Dependency_Link is access all Dependency_Link_Record'Class;

   procedure Gtk_New
     (Link : out Dependency_Link;
      Dep  : Src_Info.Dependency_Info);
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
      File       : String;
      Recompute_Layout : Boolean := True);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Examine_From_Dependencies
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      File        : String;
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
     (In_Browser : access General_Browser_Record'Class; Filename : String)
      return Canvas_Item;
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter
     (Kernel : access Kernel_Handle_Record'Class; Dep : Dependency)
      return Boolean;
   --  A filter function that decides whether Dep should be displayed in the
   --  canvas. It should return false if Dep should not be displayed.
   --
   --  Part is the unit_part of the file whose dependencies we are examining.
   --
   --  ??? This obviously needs to be modifiable from the browser itself.

   function Is_System_File (Source : Internal_File) return Boolean;
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

   function Depends_On_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : Argument_List) return String;
   --  Handler for the command "depends_on"

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

      List      : Dependency_List := null;

      function Check_Dep
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean
      is
         Dep : Dependency_List := List;
         Target : constant File_Item := File_Item (Get_Dest (Link));
         Target_File : constant String :=
           Get_Source_Filename (Get_Source (Target));
      begin
         while Dep /= null loop
            if Filter (Get_Kernel (Browser), Dep.Value)
              and then Get_Source_Filename (File_Information (Dep.Value)) =
                Target_File
            then
               return True;
            end if;

            Dep := Dep.Next;
         end loop;

         Remove_Link (Canvas, Link);
         return True;
      end Check_Dep;

      Status    : Dependencies_Query_Status;
      File      : constant String := Get_Source_Filename (Get_Source (Item));
      Lib_Info  : constant LI_File_Ptr := Locate_From_Source_And_Complete
        (Get_Kernel (Browser), File);

   begin
      if Lib_Info /= No_LI_File then
         Find_Dependencies (Lib_Info, File, List, Status);
      end if;

      For_Each_Link
        (Get_Canvas (Browser), Check_Dep'Unrestricted_Access,
         From => Canvas_Item (Item));
      Destroy (List);
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

   -----------------------------
   -- Open_Dependency_Browser --
   -----------------------------

   function Open_Dependency_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : MDI_Child;
      Browser : Dependency_Browser;
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Dependency_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Browser := Create_Dependency_Browser (Kernel);
         Child := Put
           (Get_MDI (Kernel), Browser,
            Focus_Widget => Gtk_Widget (Get_Canvas (Browser)),
            Default_Width  => Get_Pref (Kernel, Default_Widget_Width),
            Default_Height => Get_Pref (Kernel, Default_Widget_Height));
         Set_Focus_Child (Child);
         Set_Title (Child, -"Dependency Browser");
      end if;

      return Child;
   end Open_Dependency_Browser;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : String;
      Recompute_Layout : Boolean := True)
   is
      Browser       : Dependency_Browser;
      Child_Browser : MDI_Child;
      F             : constant String := Base_Name (File);
      Item, Initial : File_Item;
      Link          : Dependency_Link;
      Dep, List     : Dependency_List;
      Lib_Info      : LI_File_Ptr;
      Status        : Dependencies_Query_Status;
      Intern        : Internal_File;
      New_Item      : Boolean;
      Must_Add_Link : Boolean;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Dependency_Browser (Kernel);
      Browser := Dependency_Browser (Get_Widget (Child_Browser));

      Lib_Info := Locate_From_Source_And_Complete (Kernel, F);

      if Lib_Info = No_LI_File then
         Trace (Me,
                "Examine_Dependencies: Couldn't find LI file for " & File);
         Insert (Kernel, -"Couldn't find dependency information for " & File,
                 Mode => Glide_Kernel.Console.Error);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      Initial := File_Item (Find_File (Browser, F));
      if Initial = null then
         Gtk_New (Initial, Browser, Kernel, F);
         Put (Get_Canvas (Browser), Initial);
         Refresh (Initial);
      end if;

      if not Children_Shown (Initial) then
         Set_Children_Shown (Initial, True);
         Redraw_Title_Bar (Initial);

         Find_Dependencies (Lib_Info, F, List, Status);

         if Status = Success then
            Dep := List;

            while Dep /= null loop
               if Filter (Kernel, Dep.Value) then
                  Intern := File_Information (Dep.Value);
                  Item := File_Item
                    (Find_File (Browser, Get_Source_Filename (Intern)));
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
                     Gtk_New (Link, Dependency_Information (Dep.Value));
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

               Dep := Dep.Next;
            end loop;

            --  Center the initial item
            Show_Item (Get_Canvas (Browser), Initial);

            Destroy (List);
         end if;
      end if;

      if Recompute_Layout then
         Layout (Browser, Force => False);
      end if;
      Refresh_Canvas (Get_Canvas (Browser));
      Pop_State (Kernel_Handle (Kernel));

   exception
      when Unsupported_Language =>
         Pop_State (Kernel_Handle (Kernel));
         Insert (Kernel, "Query unsupported for this language",
                 Mode => Glide_Kernel.Console.Error);

      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Destroy_Idle;

   ----------------------------
   -- Examine_Ancestors_Idle --
   ----------------------------

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean
   is
      Child : File_Item;
      Link  : Browser_Link;
      Dep   : Dependency;
      Kernel : constant Kernel_Handle := Get_Kernel (Data.Browser);
   begin
      if Get (Data.Iter.all) = No_LI_File then
         return False;
      else
         Dep := Get (Data.Iter.all);
         if Filter (Get_Kernel (Data.Browser), Dep) then
            declare
               File : constant String :=
                 Get_Source_Filename (File_Information (Dep));
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

         Destroy (Dep);
         Next (Get_Language_Handler (Kernel), Data.Iter.all,
               Get_LI_File_List (Kernel));
         return True;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Examine_Ancestors_Idle;

   -------------------------------
   -- Examine_From_Dependencies --
   -------------------------------

   procedure Examine_From_Dependencies
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      File        : String;
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
         Gtk_New (Item, Browser,  Kernel, File);
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
        (Get_Project (Kernel),
         Get_Language_Handler (Kernel),
         File,
         Get_LI_File_List (Kernel),
         Data.Iter.all,
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
      when Unsupported_Language =>
         Pop_State (Kernel_Handle (Kernel));
         Insert (Kernel, "Query unsupported for this language",
                 Mode => Glide_Kernel.Console.Error);
         --  No need to free Data, which hasn't been created at this point

      when E : others =>
         Pop_State (Kernel_Handle (Kernel));
         Destroy (Data.Iter);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Examine_From_Dependencies;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File (Source : Internal_File) return Boolean is
      Name : constant String := Get_Source_Filename (Source);
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
     (Kernel : access Kernel_Handle_Record'Class; Dep : Dependency)
      return Boolean
   is
      Explicit_Dependency : Boolean;
      System_File         : Boolean;
      Info                : constant Dependency_Info :=
        Dependency_Information (Dep);

   begin
      --  Only show explicit dependencies, not implicit ones
      Explicit_Dependency :=
        Get_History (Get_History (Kernel).all, Show_Implicit_Key)
        or else Get_Depends_From_Spec (Info)
        or else Get_Depends_From_Body (Info);

      --  Do not display dependencies on runtime files
      System_File :=
        Get_History (Get_History (Kernel).all, Show_System_Files_Key)
        or else not Is_System_File (File_Information (Dep));

      return Explicit_Dependency and then System_File;
   end Filter;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (In_Browser : access General_Browser_Record'Class; Filename : String)
      return Canvas_Item
   is
      Iter : Item_Iterator := Start (Get_Canvas (In_Browser));
      Item : Canvas_Item;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null
           or else Get_Source_Filename (Get_Source (File_Item (Item))) =
           Filename;

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
      then
         Examine_Dependencies
           (Kernel,
            File_Information (File_Selection_Context_Access (Context)));
      end if;

      Unref (Context);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_Dependency_Browser "
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
         Trace (Me,
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
         Trace (Me,
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
      File : constant String :=
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
      if File /= "" then
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
               Name : constant String := Locale_To_UTF8
                 (File_Information (File_Context));
            begin
               Gtk_New (Item, Label => (-"Examine dependencies for ") & Name);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                  (Edit_Dependencies_From_Contextual'Access),
                  Selection_Context_Access (Context));

               Gtk_New
                 (Item, Label => (-"Examining files depending on ") & Name);
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

   function Depends_On_Command_Handler
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : Argument_List) return String
   is
      Index : Natural := Args'First;
   begin
      while Index <= Args'Last loop
         if Command = "file.uses" then
            Examine_Dependencies (Kernel, File => Args (Index).all);
         elsif Command = "file.used_by" then
            Examine_From_Dependencies (Kernel, File => Args (Index).all);
         end if;
         Index := Index + 1;
      end loop;

      return "";
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
         MDI_Child_Tag           => Dependency_Browser_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu (Kernel, Tools, -"Dependency Browser", "",
                     On_Dependency_Browser'Access);

      Register_Command
        (Kernel,
         Command      => "file.uses",
         Usage        => "uses file_name [file_name...]",
         Description  =>
           -("Display in the dependency browser the list of files that"
             & " file_name depends on. This is done for each of the"
             & " file on the command line."),
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Depends_On_Command_Handler'Access);
      Register_Command
        (Kernel,
         Command      => "file.used_by",
         Usage        => "used_by file_name [file_name...]",
         Description  =>
           -("Display in the dependency browser the list of files that"
             & " depends on file_name. This is done for each of the"
             & " file on the command line."),
         Minimum_Args => 1,
         Maximum_Args => Natural'Last,
         Handler      => Depends_On_Command_Handler'Access);
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out File_Item;
      Browser : access General_Browser_Record'Class;
      File    : Internal_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Browser, Copy (File));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item            : out File_Item;
      Browser         : access General_Browser_Record'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String)
   is
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      Item := new File_Item_Record;
      Initialize
        (Item, Browser,
         Make_Source_File (Source_Filename,
                           Handler,
                           Get_Project (Kernel)));
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item : access File_Item_Record'Class;
      Browser : access General_Browser_Record'Class;
      File  : Internal_File) is
   begin
      Initialize (Item, Browser, Get_Source_Filename (File),
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
         Get_Source_Filename (File_Item (Item).Source));
   end Examine_From_Dependencies;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Item : access Arrow_Item_Record'Class) is
   begin
      Examine_Dependencies
        (Get_Kernel (Get_Browser (Item)),
         Get_Source_Filename (File_Item (Item).Source));
   end Examine_Dependencies;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Item : access File_Item_Record'Class)
      return Src_Info.Internal_File is
   begin
      return Item.Source;
   end Get_Source;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Link : out Dependency_Link;
      Dep  : Src_Info.Dependency_Info) is
   begin
      Link := new Dependency_Link_Record;
      Link.Dep := Dep;
   end Gtk_New;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out File_Item_Record) is
   begin
      Destroy (Arrow_Item_Record (Item));
      Destroy (Item.Source);
   end Destroy;

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type
   is
      File_Name : constant String := Get_Source_Filename (Get_Source (Item));
      P : Project_Type;
   begin
      P := Get_Project_From_File
        (Get_Registry (Get_Kernel (Get_Browser (Item))), File_Name);

      if P = No_Project then
         Trace (Me, "Project_Of return No_Project for " & File_Name);
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
      Other_File : constant String := Other_File_Name
        (Get_Kernel (Context), File_Information (C), False);
   begin
      if Other_File /= "" then
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
         Trace (Me, "Unexpected exception " & Exception_Information (E));
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
      Src     : constant Src_Info.Internal_File := Get_Source (Item);
      Filename : constant String := Get_Source_Filename (Src);
      Full_Name : constant String := Get_Full_Path_From_File
        (Registry        => Get_Registry (Get_Kernel (Browser)),
         Filename        => Filename,
         Use_Source_Path => True,
         Use_Object_Path => False);
      Utf8_File : constant String := Locale_To_UTF8 (Filename);
      Mitem : Gtk_Image_Menu_Item;
      Pix   : Gtk_Image;
   begin
      Set_File_Information
        (File_Selection_Context_Access (Context),
         Directory    => Dir_Name (Full_Name),
         File_Name    => Base_Name (Full_Name),
         Project      => Project_Of (Item));

      if Menu /= null then
         Gtk_New (Mitem, -"Analyze other file (spec or body)");
         Add (Menu, Mitem);
         Context_Callback.Object_Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller (On_Examine_Other_File'Access),
            User_Data   => Context,
            Slot_Object => Browser);

         Gtk_New (Mitem, Label => (-"Examine dependencies for ") & Utf8_File);
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
           (Mitem, Label => (-"Examining files depending on ") & Utf8_File);
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
      User : Kernel_Handle) return MDI_Child is
   begin
      if Node.Tag.all = "Dependency_Browser" then
         return Put
           (MDI, Gtk_Widget (Create_Dependency_Browser (User)),
            Default_Width  => Get_Pref (User, Default_Widget_Width),
            Default_Height => Get_Pref (User, Default_Widget_Height));
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
