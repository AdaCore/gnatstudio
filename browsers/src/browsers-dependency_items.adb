-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;       use Ada.Exceptions;

with Gdk.Drawable;         use Gdk.Drawable;
with Gdk.Event;            use Gdk.Event;
with Gdk.Font;             use Gdk.Font;
with Gdk.Pixbuf;           use Gdk.Pixbuf;
with Gdk.Window;           use Gdk.Window;
with Glib.Object;          use Glib.Object;
with Glib;                 use Glib;
with Gtk.Check_Menu_Item;  use Gtk.Check_Menu_Item;
with Gtk.Enums;            use Gtk.Enums;
with Gtk.Main;             use Gtk.Main;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Stock;            use Gtk.Stock;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtkada.Handlers;      use Gtkada.Handlers;
with Gtkada.MDI;           use Gtkada.MDI;

with Gint_Xml;                  use Gint_Xml;
with Browsers.Canvas;           use Browsers.Canvas;
with Browsers.Dependency_Items; use Browsers.Dependency_Items;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with Src_Info.Queries;          use Src_Info.Queries;
with Src_Info;                  use Src_Info;
with Traces;                    use Traces;
with Prj_API;                   use Prj_API;
with Prj;                       use Prj;
with Types;                     use Types;
with Fname;                     use Fname;
with Namet;                     use Namet;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Browsers.Dependency_Items is

   Margin : constant := 2;

   Dependency_Browser_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Browsers.Dependency");

   type Examine_Dependencies_Idle_Data is record
      Iter    : Dependency_Iterator_Access;
      Browser : Dependency_Browser;
      Item    : File_Item;
   end record;
   package Dependency_Idle is new Gtk.Main.Idle
     (Examine_Dependencies_Idle_Data);

   procedure Examine_Dependencies
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      File       : String);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.

   procedure Examine_From_Dependencies
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      File       : String);
   --  Display the list of files that depend directly on File.

   procedure Open_File
     (Browser : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Open the file described in Context for analysis in the browser.

   function Find_File
     (In_Browser : access Glide_Browser_Record'Class; Filename : String)
      return Canvas_Item;
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter (Dep : Dependency) return Boolean;
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
   --  If there is already a browser in Glide2 that handles all the types
   --  Browser_Type, we re-use this one instead.

   procedure Destroy_Idle (Data : in out Examine_Dependencies_Idle_Data);
   --  Called when the idle loop is destroyed.

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean;
   --  Idle loop for Examine_From_Dependencies

   function Project_Of
     (Kernel : access Kernel_Handle_Record'Class;
      Item : access File_Item_Record'Class) return Project_Id;
   --  Return the name of the project that contains Item.
   --  This is cached for efficiency.
   --  ??? Needs to be reset when the project or its view changes

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget;
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
            Slot_Object => Glide_Browser (Object),
            User_Data   => Selection_Context_Access (Context));

         Gtk_New (Check, Label => -"Hide system files");
         Set_Active (Check, True);
         Set_Sensitive (Check, False);
         Append (Menu, Check);

         Gtk_New (Check, Label => -"Hide implicit dependencies");
         Set_Active (Check, True);
         Set_Sensitive (Check, False);
         Append (Menu, Check);
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
      Initialize (Browser, Kernel);
      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Dependency_Browser_Module_ID,
         Context_Func    => Browser_Context_Factory'Access);
      Browser.Left_Arrow := Render_Icon
        (Browser, Stock_Go_Back, Icon_Size_Menu);
      Browser.Right_Arrow := Render_Icon
        (Browser, Stock_Go_Forward, Icon_Size_Menu);

      Widget_Callback.Connect
        (Browser, "destroy",
         Widget_Callback.To_Marshaller (On_Destroy'Access));

      Set_Size_Request
        (Browser,
         Get_Pref (Kernel, Default_Widget_Width),
         Get_Pref (Kernel, Default_Widget_Height));
      return Browser;
   end Create_Dependency_Browser;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Browser : access Gtk_Widget_Record'Class) is
      B : Dependency_Browser := Dependency_Browser (Browser);
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
   begin
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Dependency_Browser_Record'Tag);

      if Child /= null then
         Raise_Child (Child);
      else
         Child := Put (Get_MDI (Kernel), Create_Dependency_Browser (Kernel));
         Set_Title (Child, -"Dependency Browser");
      end if;

      return Child;
   end Open_Dependency_Browser;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      File         : String)
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
         Insert (Kernel, -"Couldn't find LI file for " & File,
                 Mode => Glide_Kernel.Console.Error);
         Pop_State (Kernel_Handle (Kernel));
         return;
      end if;

      --  For efficiency, do not recompute the layout for each item
      Set_Auto_Layout (Get_Canvas (Browser), False);

      Initial := File_Item (Find_File (Browser, F));
      if Initial = null then
         Gtk_New (Initial, Get_Window (Browser), Browser, Kernel,  F);
         Put (Get_Canvas (Browser), Initial);
      end if;

      if not Initial.To_Parsed then
         Initial.To_Parsed := True;
         Refresh (Browser, Initial);

         Find_Dependencies (Lib_Info, F, List, Status);

         if Status = Success then
            Dep := List;

            while Dep /= null loop
               if Filter (Dep.Value) then
                  Intern := File_Information (Dep.Value);
                  Item := File_Item
                    (Find_File (Browser, Get_Source_Filename (Intern)));
                  New_Item := Item = null;
                  Must_Add_Link := True;

                  if New_Item then
                     Gtk_New
                       (Item, Get_Window (Browser), Browser, Intern);

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

                  if New_Item then
                     Put (Get_Canvas (Browser), Item);
                  end if;
               end if;

               Dep := Dep.Next;
            end loop;

            --  Center the initial item
            Show_Item (Get_Canvas (Browser), Item);

            Destroy (List);
            Set_Auto_Layout (Get_Canvas (Browser), True);
            Layout (Get_Canvas (Browser),
                    Force => False,
                    Vertical_Layout =>
                      Get_Pref (Kernel, Browsers_Vertical_Layout));
            Refresh_Canvas (Get_Canvas (Browser));
         end if;
      end if;

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
   begin
      Set_Auto_Layout (Get_Canvas (Data.Browser), True);
      Layout (Get_Canvas (Data.Browser),
              Force => False,
              Vertical_Layout =>
                Get_Pref (Get_Kernel (Data.Browser),
                          Browsers_Vertical_Layout));
      Refresh_Canvas (Get_Canvas (Data.Browser));

      --  Center the initial item
      Show_Item (Get_Canvas (Data.Browser), Data.Item);

      Data.Browser.Idle_Id := 0;
      Destroy (Data.Iter);
      Pop_State (Get_Kernel (Data.Browser));
   end Destroy_Idle;

   ----------------------------
   -- Examine_Ancestors_Idle --
   ----------------------------

   function Examine_Ancestors_Idle
     (Data : Examine_Dependencies_Idle_Data) return Boolean
   is
      Child : File_Item;
      Link  : Glide_Browser_Link;
      Dep   : Dependency;
   begin
      if Get (Data.Iter.all) = No_LI_File then
         return False;
      else
         Dep := Get (Data.Iter.all);
         if Filter (Dep) then
            declare
               File : constant String :=
                 Get_Source_Filename (File_Information (Dep));
            begin
               Child := File_Item (Find_File (Data.Browser, File));
               if Child = null then
                  Gtk_New (Child, Get_Window (Data.Browser), Data.Browser,
                           Get_Kernel (Data.Browser), File);
                  Put (Get_Canvas (Data.Browser), Child);
               end if;

               if not Has_Link
                 (Get_Canvas (Data.Browser), Child, Data.Item)
               then
                  Link := new Glide_Browser_Link_Record;
                  Add_Link
                    (Get_Canvas (Data.Browser), Link => Link,
                     Src => Child, Dest => Data.Item);
               end if;
            end;
         end if;

         Destroy (Dep);
         Next (Get_Kernel (Data.Browser), Data.Iter.all);
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
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      File       : String)
   is
      Data : Examine_Dependencies_Idle_Data;
      Browser       : Dependency_Browser;
      Child_Browser : MDI_Child;
      Item : File_Item;

   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      --  Create the browser if it doesn't exist
      Child_Browser := Open_Dependency_Browser (Kernel);
      Browser := Dependency_Browser (Get_Widget (Child_Browser));

      --  Look for an existing item corresponding to entity
      Item := File_Item (Find_File (Browser, File));
      if Item = null then
         Gtk_New (Item, Get_Window (Browser), Browser,  Kernel, File);
         Put (Get_Canvas (Browser), Item);
      end if;

      Item.From_Parsed := True;
      Refresh (Browser, Item);

      --  For efficiency, do not recompute the layout for each item.
      Set_Auto_Layout (Get_Canvas (Browser), False);

      Data := (Iter    => new Dependency_Iterator,
               Browser => Dependency_Browser (Browser),
               Item    => Item);
      Find_Ancestor_Dependencies (Kernel, File, Data.Iter.all);

      Browser.Idle_Id := Dependency_Idle.Add
        (Cb       => Examine_Ancestors_Idle'Access,
         D        => Data,
         Priority => Priority_Low_Idle,
         Destroy  => Destroy_Idle'Access);

      --  All memory is freed at the end of Examine_From_Dependencies_Idle

   exception
      when Unsupported_Language =>
         Pop_State (Kernel_Handle (Kernel));
         Insert (Kernel, "Query unsupported for this language",
                 Mode => Glide_Kernel.Console.Error);
         Destroy (Data.Iter);

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

   function Filter (Dep : Dependency) return Boolean  is
      Explicit_Dependency : Boolean;
      Info                : constant Dependency_Info :=
        Dependency_Information (Dep);

   begin
      --  ??? This must be configurable at the GUI level.

      --  Only show explicit dependencies, not implicit ones

      Explicit_Dependency := Get_Depends_From_Spec (Info)
        or else Get_Depends_From_Body (Info);

      --  Do not display dependencies on runtime files

      return Explicit_Dependency
        and then not Is_System_File (File_Information (Dep));
   end Filter;

   ---------------
   -- Find_File --
   ---------------

   function Find_File
     (In_Browser : access Glide_Browser_Record'Class; Filename : String)
      return Canvas_Item
   is
      Found : Canvas_Item := null;

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean;
      --  Check whether Item contains File

      ----------------
      -- Check_Item --
      ----------------

      function Check_Item
        (Canvas : access Interactive_Canvas_Record'Class;
         Item   : access Canvas_Item_Record'Class) return Boolean
      is
         pragma Unreferenced (Canvas);
      begin
         if Item.all in File_Item_Record'Class
           and then Get_Source_Filename (Get_Source (File_Item (Item))) =
           Filename
         then
            Found := Canvas_Item (Item);
            return False;
         end if;

         return True;
      end Check_Item;

   begin
      For_Each_Item (Get_Canvas (In_Browser), Check_Item'Unrestricted_Access);
      return Found;
   end Find_File;

   ---------------------------
   -- On_Dependency_Browser --
   ---------------------------

   procedure On_Dependency_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Child : MDI_Child;
      Context : Selection_Context_Access := Get_Current_Context (Kernel);
   begin
      Child := Open_Dependency_Browser (Kernel);

      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         Examine_Dependencies
           (Kernel,
            File_Information (File_Selection_Context_Access (Context)));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_Dependency_Browser "
                & Exception_Information (E));
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
      pragma Unreferenced (Browser);
      File : constant String := Select_File (Base_Directory => "");
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
            Gtk_New (Item, Label => (-"Examine dependencies for ") &
                     File_Information (File_Context));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Dependencies_From_Contextual'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => (-"Examining files depending on ") &
                     File_Information (File_Context));
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (Edit_Ancestor_Dependencies_From_Contextual'Access),
               Selection_Context_Access (Context));
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
      Browser : Dependency_Browser := Dependency_Browser (Child);
   begin
      if Selected_Item (Browser) = null then
         return null;
      end if;

      return Contextual_Factory
        (Item    => Glide_Browser_Item (Selected_Item (Browser)),
         Browser => Browser,
         Event   => null,
         Menu    => null);
   end Default_Factory;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools");
   begin
      Dependency_Browser_Module_ID := Register_Module
        (Kernel                  => Kernel,
         Module_Name             => Dependency_Browser_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access,
         MDI_Child_Tag           => Dependency_Browser_Record'Tag,
         Default_Context_Factory => Default_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu (Kernel, Tools, -"Dependency Browser", "",
                     On_Dependency_Browser'Access);
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out File_Item;
      Win     : Gdk_Window;
      Browser : access Glide_Browser_Record'Class;
      File    : Internal_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Win, Browser, Copy (File));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item            : out File_Item;
      Win             : Gdk_Window;
      Browser         : access Glide_Browser_Record'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String)
   is
      Handler : Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      Item := new File_Item_Record;
      Initialize
        (Item, Win, Browser,
         Make_Source_File (Source_Filename,
                           Handler,
                           Get_Project_View (Kernel),
                           Get_Predefined_Source_Path (Kernel)));
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Item : access File_Item_Record'Class;
      Win  : Gdk_Window;
      Browser : access Glide_Browser_Record'Class;
      File  : Internal_File)
   is
      use type Gdk_Window;
      B             : Dependency_Browser := Dependency_Browser (Browser);
      Str           : constant String := Get_Source_Filename (File);
      Font          : Gdk_Font;
      Width, Height : Gint;

   begin
      pragma Assert (Win /= null);
      Item.Source := File;
      Item.Browser := Glide_Browser (Browser);

      Font := Get_Text_Font (Item.Browser);

      Width  := String_Width (Font, Str) + 4 * Margin
        + Get_Width (B.Left_Arrow) + Get_Width (B.Right_Arrow);
      Height := (Get_Ascent (Font) + Get_Descent (Font));
      Height := Gint'Max (Height, Get_Height (B.Left_Arrow));
      Height := Height + 2 * Margin;

      Set_Screen_Size_And_Pixmap
        (Item, Get_Window (Item.Browser), Width, Height);

      Refresh (Item.Browser, Item);
   end Initialize;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Browser : access Glide_Browser_Record'Class;
                      Item    : access File_Item_Record)
   is
      use type Gdk.Gdk_GC;
      B : Dependency_Browser := Dependency_Browser (Browser);
      Font : Gdk_Font := Get_Text_Font (Browser);
   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Font  => Font,
         GC    => Get_Text_GC (Browser),
         X     => Margin + Get_Width (B.Left_Arrow),
         Y     => Margin + Get_Ascent (Font),
         Text  => Get_Source_Filename (Item.Source));

      if not Item.From_Parsed then
         Render_To_Drawable_Alpha
           (Pixbuf          => B.Left_Arrow,
            Drawable        => Pixmap (Item),
            Src_X           => 0,
            Src_Y           => 0,
            Dest_X          => Margin,
            Dest_Y          => Margin,
            Width           => -1,
            Height          => -1,
            Alpha           => Alpha_Full,
            Alpha_Threshold => 128);
      end if;

      if not Item.To_Parsed then
         Render_To_Drawable_Alpha
           (Pixbuf          => B.Right_Arrow,
            Drawable        => Pixmap (Item),
            Src_X           => 0,
            Src_Y           => 0,
            Dest_X          => Gint (Get_Coord (Item).Width)
              - Margin - Get_Width (B.Right_Arrow),
            Dest_Y          => Margin,
            Width           => -1,
            Height          => -1,
            Alpha           => Alpha_Full,
            Alpha_Threshold => 128);
      end if;
   end Refresh;

   ---------------------
   -- On_Button_Click --
   ---------------------

   procedure On_Button_Click
     (Item  : access File_Item_Record;
      Event : Gdk.Event.Gdk_Event_Button) is
   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         --  Should we display the ancestors ?
         if Gint (Get_X (Event)) < Get_Coord (Item).Width / 2 then
            Examine_From_Dependencies
              (Get_Kernel (Item.Browser), Get_Source_Filename (Item.Source));
         else
            Examine_Dependencies
              (Get_Kernel (Item.Browser), Get_Source_Filename (Item.Source));
         end if;

      elsif Get_Event_Type (Event) = Button_Press then
         Select_Item (Item.Browser, Item, True);
      end if;
   end On_Button_Click;

   ----------------
   -- Get_Source --
   ----------------

   function Get_Source (Item : access File_Item_Record)
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
      Destroy (Item.Source);
   end Destroy;

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of
     (Kernel : access Kernel_Handle_Record'Class;
      Item : access File_Item_Record'Class) return Project_Id
   is
      File_Name : constant String := Get_Source_Filename (Get_Source (Item));
      P : Project_Id;
   begin
      if Item.Project_Name = No_Name then
         P := Get_Project_From_File (Get_Project_View (Kernel), File_Name);

         if P /= No_Project then
            declare
               P_Name : constant String := Project_Name (P);
            begin
               Name_Len := P_Name'Length;
               Name_Buffer (1 .. Name_Len) := P_Name;
               Item.Project_Name := Name_Find;
            end;
         else
            --  ??? Eventually useless when we have a real project file for the
            --  runtime
            Item.Project_Name := No_Name;
         end if;
      end if;

      if Item.Project_Name = No_Name then
         return No_Project;
      else
         return Get_Project_View_From_Name (Item.Project_Name);
      end if;
   end Project_Of;

   ---------------------------
   -- On_Examine_Other_File --
   ---------------------------

   procedure On_Examine_Other_File
     (Browser : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      B : Dependency_Browser := Dependency_Browser (Browser);
      C : File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Item : File_Item;
      Other_File : constant String := Get_Other_File_Of
        (Get_Kernel (Context), File_Information (C), False);
   begin
      if Other_File /= "" then
         Item := File_Item (Find_File (B, Other_File));
         if Item = null then
            Set_Auto_Layout (Get_Canvas (B), False);

            Gtk_New (Item, Get_Window (B), B,  Get_Kernel (Context),
                     Other_File);
            Put (Get_Canvas (B), Item);

            Set_Auto_Layout (Get_Canvas (B), True);
            Layout (Get_Canvas (B),
                    Force => False,
                    Vertical_Layout =>
                      Get_Pref (Get_Kernel (B), Browsers_Vertical_Layout));
            Refresh_Canvas (Get_Canvas (B));

         end if;
         Select_Item (B, Item, True);
      end if;
   exception
      when E : others =>
         Trace (Me,
                "Unexpected exception in On_Examine_Other_File "
                & Exception_Information (E));
   end On_Examine_Other_File;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access File_Item_Record;
      Browser : access Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event);
      Context : Selection_Context_Access := new File_Selection_Context;
      Src     : Src_Info.Internal_File := Get_Source (Item);
      Filename : constant String := Get_Source_Filename (Src);
      Full_Name : constant String := Find_Source_File
        (Get_Kernel (Browser), Filename, True);
      Mitem : Gtk_Menu_Item;
   begin
      Set_File_Information
        (File_Selection_Context_Access (Context),
         Directory    => Dir_Name (Full_Name),
         File_Name    => Base_Name (Full_Name),
         Project_View => Project_Of (Get_Kernel (Browser), Item));

      if Menu /= null then
         Gtk_New (Mitem, -"Analyze other file (spec or body)");
         Add (Menu, Mitem);
         Context_Callback.Object_Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller (On_Examine_Other_File'Access),
            User_Data   => Context,
            Slot_Object => Browser);

         Gtk_New (Mitem, Label => (-"Examine dependencies for ") & Filename);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Dependencies_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Item.To_Parsed);

         Gtk_New
           (Mitem, Label => (-"Examining files depending on ") & Filename);
         Append (Menu, Mitem);
         Context_Callback.Connect
           (Mitem, "activate",
            Context_Callback.To_Marshaller
              (Edit_Ancestor_Dependencies_From_Contextual'Access),
            Context);
         Set_Sensitive (Mitem, not Item.From_Parsed);
      end if;

      return Context;
   end Contextual_Factory;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (Node : Gint_Xml.Node_Ptr; User : Kernel_Handle)
      return Gtk_Widget is
   begin
      if Node.Tag.all = "Dependency_Browser" then
         return Gtk_Widget (Create_Dependency_Browser (User));
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
         N.Tag := new String' ("Dependency_Browser");
         return N;
      end if;

      return null;
   end Save_Desktop;

   -----------
   -- Reset --
   -----------

   procedure Reset (Browser : access Glide_Browser_Record'Class;
                    Item : access File_Item_Record)
   is
      pragma Unreferenced (Browser);
   begin
      Item.To_Parsed := False;
      Item.From_Parsed := False;
   end Reset;

end Browsers.Dependency_Items;
