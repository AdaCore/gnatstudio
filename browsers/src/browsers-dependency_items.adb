-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gdk.Drawable;         use Gdk.Drawable;
with Gdk.Event;            use Gdk.Event;
with Gdk.Font;             use Gdk.Font;
with Gdk.Window;           use Gdk.Window;
with Glib.Object;          use Glib.Object;
with Glib;                 use Glib;
with Gtk.Check_Menu_Item;  use Gtk.Check_Menu_Item;
with Gtk.Menu;             use Gtk.Menu;
with Gtk.Menu_Item;        use Gtk.Menu_Item;
with Gtk.Widget;           use Gtk.Widget;
with Gtkada.Canvas;        use Gtkada.Canvas;
with Gtkada.File_Selector; use Gtkada.File_Selector;
with Gtkada.MDI;           use Gtkada.MDI;

with Gint_Xml;                  use Gint_Xml;
with Browsers.Canvas;           use Browsers.Canvas;
with Browsers.Dependency_Items; use Browsers.Dependency_Items;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel;              use Glide_Kernel;
with Src_Info.Queries;          use Src_Info.Queries;
with Src_Info;                  use Src_Info;
with Traces;                    use Traces;
with Prj_API;                   use Prj_API;
with Basic_Types;               use Basic_Types;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Types;                     use Types;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Exceptions;            use Ada.Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Browsers.Dependency_Items is

   Display_Unit_Name : constant Boolean := False;
   --  <preference> True if the unit name should be displayed

   Margin : constant := 2;

   Dependency_Browser_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Browsers.Dependency");

   Default_Browser_Width  : constant := 400;
   Default_Browser_Height : constant := 400;
   --  <preference> Default size for the browsers

   Vertical_Layout : Boolean := True;
   --  <preference> Should the layout of the graph be vertical or horizontal ?

   procedure Examine_Dependencies
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      In_Browser : access Dependency_Browser_Record'Class;
      File       : String);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.

   procedure Open_File
     (Browser : access Gtk.Widget.Gtk_Widget_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Open the file described in Context for analysis in the browser.

   function Find_File
     (In_Browser : access Glide_Browser_Record'Class; Filename : String)
      return Canvas_Item;
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Part   : Unit_Part;
      Dep : Dependency)
      return Boolean;
   --  A filter function that decides whether Dep should be displayed in the
   --  canvas. It should return false if Dep should not be displayed.
   --
   --  Part is the unit_part of the file whose dependencies we are examining.
   --
   --  ??? This obviously needs to be modifiable from the browser itself.

   function Is_System_File
     (Kernel : access Kernel_Handle_Record'Class;
      Source : Internal_File) return Boolean;
   --  Return True if Source is a system file (runtime file for Ada).
   --  ??? This should be moved to a more general location, and perhaps be
   --  ??? implemented with support from the project files.
   --  ??? It could also simply use the paths to detect whether the file is in
   --  ??? one of the predefined paths.

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   procedure On_Dependency_Browser
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Tools->Dependency Browser

   procedure Edit_Dependencies_From_Contextual
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access);
   --  Examine the dependencies of a specific file

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
      Set_Size_Request
        (Browser, Default_Browser_Width, Default_Browser_Height);
      return Browser;
   end Create_Dependency_Browser;

   -----------------------------
   -- Open_Dependency_Browser --
   -----------------------------

   function Open_Dependency_Browser
     (Kernel       : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Child
   is
      Child   : MDI_Child;
   begin
      Child := Find_MDI_Child_By_Tag (Get_MDI (Kernel),
                                      Glide_Browser_Record'Tag);
      if Child /= null then
         Raise_Child (Child);
      else
         Child := Put (Get_MDI (Kernel), Create_Dependency_Browser (Kernel));
         Set_Title (Child, "<dependency browser>");
      end if;

      return Child;
   end Open_Dependency_Browser;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel       : access Kernel_Handle_Record'Class;
      In_Browser   : access Dependency_Browser_Record'Class;
      File         : String)
   is
      F             : constant String := Base_Name (File);
      Item, Initial : File_Item;
      Link          : Dependency_Link;
      Dep, List     : Dependency_List;
      Lib_Info      : LI_File_Ptr;
      Status        : Dependencies_Query_Status;
      Intern        : Internal_File;
      New_Item      : Boolean;
      Must_Add_Link : Boolean;
      Part : Unit_Part;

      function Has_One
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean;
      --  Set Add_Link to False if there is at least one link returned.
      --  ??? Would be nicer if we had real iterators in the canvas

      -------------
      -- Has_One --
      -------------

      function Has_One
        (Canvas : access Interactive_Canvas_Record'Class;
         Link   : access Canvas_Link_Record'Class) return Boolean is
      begin
         Must_Add_Link := False;
         return False;
      end Has_One;

   begin
      Set_Busy (Kernel_Handle (Kernel), True);
      Lib_Info := Locate_From_Source_And_Complete (Kernel, F);
      if Lib_Info = No_LI_File then
         Trace (Me,
                "Examine_Dependencies: Couldn't find ALI file for " & File);
         Insert (Kernel, -"Couldn't find ALI file for " & File,
                 Mode => Glide_Kernel.Console.Error);
         Set_Busy (Kernel_Handle (Kernel), False);
         return;
      end if;

      Initial := File_Item (Find_File (In_Browser, F));
      if Initial = null then
         Gtk_New (Initial, Get_Window (In_Browser), In_Browser, Kernel,  F);
         Put (Get_Canvas (In_Browser), Initial);

         --  ??? Should check if the item was already expanded, so as to avoid
         --  useless work.
      end if;

      Find_Dependencies (Lib_Info, List, Status);

      Part := Get_Unit_Part (Get_Source_Info_List (Kernel), F);

      if Status = Success then
         Dep := List;

         while Dep /= null loop
            if Filter (Kernel, Part, Dep.Value) then
               Intern := File_Information (Dep.Value);
               Item := File_Item
                 (Find_File (In_Browser, Get_Source_Filename (Intern)));
               New_Item := Item = null;
               Must_Add_Link := True;

               if New_Item then
                  Gtk_New (Item, Get_Window (In_Browser), In_Browser,
                           Kernel, Intern);

               else
                  --  If the item already existed, chances are that the link
                  --  also existed. Don't duplicate it in that case.

                  For_Each_Link
                    (Get_Canvas (In_Browser), Has_One'Unrestricted_Access,
                     From => Canvas_Item (Initial), To => Canvas_Item (Item));
               end if;

               if Must_Add_Link then
                  Gtk_New (Link, Dependency_Information (Dep.Value));
                  Add_Link (Get_Canvas (In_Browser),
                            Link => Link,
                            Src  => Initial,
                            Dest => Item);
               end if;

               if New_Item then
                  Put (Get_Canvas (In_Browser), Item);
               end if;
            end if;

            Dep := Dep.Next;
         end loop;

         Destroy (List);
         Layout (Get_Canvas (In_Browser),
                 Force => False,
                 Vertical_Layout => Vertical_Layout);
         Refresh_Canvas (Get_Canvas (In_Browser));
      end if;
      Set_Busy (Kernel_Handle (Kernel), False);
   end Examine_Dependencies;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Kernel : access Kernel_Handle_Record'Class;
      Source : Internal_File) return Boolean
   is
      Name : constant String := Get_Source_Filename (Source);
      Ada_Runtime_File, Gtk_System_File : Boolean;
   begin
      --  ??? The implementation here is too GNAT specific. However, getting
      --  the Unit_Name would be expensive.

      Ada_Runtime_File := Name'Length > 2
        and then (Name (Name'First .. Name'First + 1) = "a-"
                  or else Name (Name'First .. Name'First + 1) = "s-"
                  or else Name (Name'First .. Name'First + 1) = "i-"
                  or else Name (Name'First .. Name'First + 1) = "g-");

      Ada_Runtime_File := Ada_Runtime_File
        or else Name = "ada.ads"
        or else Name = "interfac.ads"
        or else Name = "system.ads"
        or else Name = "unchconv.ads"
        or else Name = "unchdeal.ads"
        or else Name = "text_io.ads"
        or else Name = "gnat.ads";

      Gtk_System_File := Name'Length > 4
        and then (Name (Name'First .. Name'First + 2) = "gtk"
                  or else Name (Name'First .. Name'First + 3) = "glib"
                  or else Name (Name'First .. Name'First + 2) = "gdk");

      return Ada_Runtime_File or else Gtk_System_File;
   end Is_System_File;

   ------------
   -- Filter --
   ------------

   function Filter
     (Kernel : access Kernel_Handle_Record'Class;
      Part   : Unit_Part;
      Dep    : Dependency) return Boolean
   is
      Explicit_Dependency : Boolean;
      Info                : constant Dependency_Info :=
        Dependency_Information (Dep);

   begin
      --  ??? This must be configurable at the GUI level.

      --  Only show explicit dependencies, not implicit ones

      Explicit_Dependency :=
        (Part = Unit_Spec and then Get_Depends_From_Spec (Info))
         or else (Part = Unit_Body and then Get_Depends_From_Body (Info));

      --  Do not display dependencies on runtime files

      return Explicit_Dependency
        and then not Is_System_File (Kernel, File_Information (Dep));
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
         Item   : access Canvas_Item_Record'Class) return Boolean is
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
      Child : MDI_Child;
   begin
      Child := Open_Dependency_Browser (Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in On_Dependency_Browser "
                & Exception_Information (E));
   end On_Dependency_Browser;

   ---------------------------------------
   -- Edit_Dependencies_From_Contextual --
   ---------------------------------------

   procedure Edit_Dependencies_From_Contextual
     (Widget  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access)
   is
      Browser : MDI_Child;
   begin
      Browser := Open_Dependency_Browser (Get_Kernel (Context));
      Examine_Dependencies
        (Get_Kernel (Context),
         Dependency_Browser (Get_Widget (Browser)),
         File_Information (File_Selection_Context_Access (Context)));
   exception
      when E : Unsupported_Language =>
         Insert (Get_Kernel (Context), Exception_Message (E),
                 Mode => Glide_Kernel.Console.Error);

      when E : others =>
         Trace (Me,
                "Unexpected exception in Edit_Dependencies_From_Contextual "
                & Exception_Information (E));
   end Edit_Dependencies_From_Contextual;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools");
   begin
      Register_Menu (Kernel, Tools, -"Dependency Browser", "",
                     On_Dependency_Browser'Access);
   end Initialize_Module;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Browser  : access Gtk_Widget_Record'Class;
      Context : Selection_Context_Access)
   is
      File : constant String := Select_File (Base_Directory => "");
      --  ??? Should set up filters to only open file from the current project.
   begin
      if File /= "" then
         Examine_Dependencies
           (Get_Kernel (Context), Dependency_Browser (Browser), File);
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
         end if;
      end if;
   end Browser_Contextual_Menu;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Dependency_Browser_Module_ID := Register_Module
        (Module_Name             => Dependency_Browser_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => Browser_Contextual_Menu'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);
   end Register_Module;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item    : out File_Item;
      Win     : Gdk_Window;
      Browser : access Glide_Browser_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class;
      File    : Internal_File) is
   begin
      Item := new File_Item_Record;
      Initialize (Item, Win, Browser, Kernel, Copy (File));
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item            : out File_Item;
      Win             : Gdk_Window;
      Browser         : access Glide_Browser_Record'Class;
      Kernel          : access Kernel_Handle_Record'Class;
      Source_Filename : String) is
   begin
      Item := new File_Item_Record;
      Initialize
        (Item, Win, Browser, Kernel,
         Make_Source_File (Source_Filename,
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
      Kernel : access Kernel_Handle_Record'Class;
      File  : Internal_File)
   is
      use type Gdk_Window;
      Str : constant String := Get_Source_Filename (File);
      Str2 : GNAT.OS_Lib.String_Access;
      Font : Gdk_Font;
      Width, Height : Gint;

   begin
      pragma Assert (Win /= null);
      Item.Source := File;
      Item.Browser := Glide_Browser (Browser);

      Font := Get_Text_Font (Item.Browser);

      if Display_Unit_Name then
         Get_Unit_Name (Kernel, Item.Source, Str2);
      end if;

      Width  := String_Width (Font, Str);
      Height := (Get_Ascent (Font) + Get_Descent (Font)) + 2 * Margin;

      if Display_Unit_Name then
         if Str2 /= null then
            Width := Gint'Max (Width, String_Width (Font, Str2.all));
         else
            Width := Gint'Max
              (Width, String_Width (Font, String' ("<unknown unit name>")));
         end if;

         Height := Height + (Get_Ascent (Font) + Get_Descent (Font))
           + 2 * Margin;
      end if;

      Width := Width + 4 * Margin;
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
      Font : Gdk_Font := Get_Text_Font (Browser);
      Str2 : GNAT.OS_Lib.String_Access;
   begin
      Draw_Item_Background (Browser, Item);
      Draw_Text
        (Pixmap (Item),
         Font  => Font,
         GC    => Get_Text_GC (Browser),
         X     => Margin,
         Y     => Margin + Get_Ascent (Font),
         Text  => Get_Source_Filename (Item.Source));

      if Display_Unit_Name then
         Get_Unit_Name (Get_Kernel (Browser), Item.Source, Str2);
         if Str2 /= null then
            Draw_Text
              (Pixmap (Item),
               Font  => Font,
               GC    => Get_Text_GC (Browser),
               X     => Margin,
               Y     => Margin + Get_Ascent (Font) * 2 + Get_Descent (Font),
               Text  => Str2.all);
         else
            Draw_Text
              (Pixmap (Item),
               Font  => Font,
               GC    => Get_Text_GC (Browser),
               X     => Margin,
               Y     => Margin + Get_Ascent (Font) * 2 + Get_Descent (Font),
               Text  => "<unknown unit name>");
         end if;
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
         Examine_Dependencies
           (Get_Kernel (Item.Browser), Dependency_Browser (Item.Browser),
            Get_Source_Filename (Item.Source));
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
      Item : access File_Item_Record'Class) return Project_Id is
   begin
      if Item.Project_Name = No_Name then
         declare
            Iter : Imported_Project_Iterator := Start
              (Get_Project (Kernel), True);
         begin
            Project_Loop :
            while Current (Iter) /= Empty_Node loop
               declare
                  Sources : String_Array_Access := Get_Source_Files
                    (Current (Iter), False);
               begin
                  for S in Sources'Range loop
                     if Base_Name (Sources (S).all) =
                       Get_Source_Filename (Get_Source (Item))
                     then
                        Item.Project_Name := Name_Of (Current (Iter));
                        exit Project_Loop;
                     end if;
                  end loop;

                  Free (Sources);
               end;

               Next (Iter);
            end loop Project_Loop;
         end;
      end if;

      return Get_Project_View_From_Name (Item.Project_Name);
   end Project_Of;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   function Contextual_Factory
     (Item  : access File_Item_Record;
      Browser : access Glide_Browser_Record'Class;
      Event : Gdk.Event.Gdk_Event;
      Menu  : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      Context : Selection_Context_Access := new File_Selection_Context;
      Src     : Src_Info.Internal_File := Get_Source (Item);
      Filename : constant String := Get_Full_Source_Filename
        (Src,
         Get_Source_Info_List (Get_Kernel (Browser)),
         Get_Project_View (Get_Kernel (Browser)),
         Get_Predefined_Source_Path (Get_Kernel (Browser)));
   begin
      Src := Get_Source (Item);
      Set_File_Information
        (File_Selection_Context_Access (Context),
         Project_View => Project_Of (Get_Kernel (Browser), Item),
         Directory    => Dir_Name (Filename),
         File_Name    => Base_Name (Filename));
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

end Browsers.Dependency_Items;
