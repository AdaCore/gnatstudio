------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2023, AdaCore                     --
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
with Glib.Object;             use Glib.Object;
with Gtk.Info_Bar;            use Gtk.Info_Bar;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Message_Dialog;      use Gtk.Message_Dialog;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;

with Gtkada.Canvas_View;       use Gtkada.Canvas_View;
with Gtkada.Canvas_View.Views; use Gtkada.Canvas_View.Views;
with Gtkada.MDI;               use Gtkada.MDI;

with Browsers.Canvas;         use Browsers.Canvas;
with Commands.Interactive;    use Commands, Commands.Interactive;
with Default_Preferences;     use Default_Preferences;
with Generic_Views;
with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Projects; use GNATCOLL.Scripts.Projects;

with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Actions;      use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;     use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;   use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with GPS.Kernel.Scripts;      use GPS.Kernel.Scripts;
with GUI_Utils;               use GUI_Utils;
with Projects;                use Projects;
with Xref;                    use Xref;

package body Browsers.Dependency_Items is

   Show_System_Files : Boolean_Preference;
   Show_Implicit     : Boolean_Preference;

   Include_Implicit_Cst  : aliased constant String := "include_implicit";
   Include_System_Cst    : aliased constant String := "include_system";

   Space_Between_Items  : constant Glib.Gdouble := 10.0;
   Space_Between_Layers : constant Glib.Gdouble := 60.0;

   Dependency_Browser_Provider : Dependency_Browser_Provider_Access;

   --------------
   -- Command  --
   --------------

   type Show_Dependencies_Command_Type is
     (Show_File_Dependencies, Show_Depends_On_File);
   --  The different type of commands to examine file dependencies.
   --
   --  . Show_File_Dependencies: show the dedependencies of the selected
   --    file(s)
   --
   --  . Show_Depends_On_File: show all the files that depend on the selected
   --    file(s)

   type Show_Dependencies_Command
     (Command_Type : Show_Dependencies_Command_Type)
   is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Show_Dependencies_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   function Show_Dependencies_Menu_Factory
     (Context : Selection_Context) return String;

   type Show_Importing_Button is new Left_Arrow_Record with null record;
   overriding procedure On_Click
     (Self    : not null access Show_Importing_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   type Show_Imported_Button is new Right_Arrow_Record with null record;
   overriding procedure On_Click
     (Self    : not null access Show_Imported_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);

   ------------------------
   -- Dependency browser --
   ------------------------

   type Dependency_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with null record;

   overriding procedure Create_Toolbar
     (View    : not null access Dependency_Browser_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Dependency_Browser_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding function Load_From_XML
     (Self : not null access Dependency_Browser_Record;
      Node : XML_Utils.Node_Ptr) return access GPS_Item_Record'Class;
   overriding procedure Load_From_XML
     (Self     : not null access Dependency_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class);
   overriding procedure Preferences_Changed
     (Self : not null access Dependency_Browser_Record;
      Pref : Default_Preferences.Preference);

   function Initialize
     (View   : access Dependency_Browser_Record'Class)
      return Gtk_Widget;
   --  Creates the dependency browser and returns the focus widget

   package Dependency_Views is new Generic_Views.Simple_Views
     (Module_Name            => Dependency_Browser_Module_Name,
      View_Name              => -"Dependency Browser",
      Formal_View_Record     => Dependency_Browser_Record,
      Formal_MDI_Child       => Browser_Child_Record,
      Reuse_If_Exist         => True,
      Initialize             => Initialize,
      Local_Toolbar          => True,
      Local_Config           => True,
      Position               => Position_Automatic,
      Group                  => Group_Default);
   subtype Dependency_Browser is Dependency_Views.View_Access;
   use Dependency_Views;

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project as changed

   procedure Refresh_Browser
     (Browser : not null access Dependency_Browser_Record'Class);
   --  Refresh the browser after the preferences have changed

   ---------------------------------
   -- Dependency browser provider --
   ---------------------------------

   type Xref_Dependency_Browser_Provider_Type
   is new Dependency_Browser_Provider_Interface with null record;

   overriding procedure Compute_Dependencies
     (Provider      : Xref_Dependency_Browser_Provider_Type;
      Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Project       : GNATCOLL.Projects.Project_Type;
      Kind          : Dependency_Kind_Type;
      Show_Implicit : Boolean);

   ----------------
   -- File items --
   ----------------
   --  These items represent source files from the application

   type File_Item_Record is new GPS_Item_Record with record
      Source       : Virtual_File;
      Project_Path : Virtual_File;
   end record;
   type File_Item is access all File_Item_Record'Class;

   overriding function Save_To_XML
     (Self : not null access File_Item_Record)
      return XML_Utils.Node_Ptr;
   overriding procedure Set_Context
     (Item    : not null access File_Item_Record;
      Context : in out Selection_Context);
   --  Return the context to use for this item

   ----------------------
   -- Dependency links --
   ----------------------

   type Dependency_Link_Record is new GPS_Link_Record with record
      Explicit : Boolean;
   end record;
   type Dependency_Link is access all Dependency_Link_Record'Class;

   overriding procedure Save_To_XML
     (Self : not null access Dependency_Link_Record;
      Node : not null XML_Utils.Node_Ptr);

   procedure Add_Link
     (Self     : not null access Dependency_Browser_Record'Class;
      From, To : File_Item;
      Explicit : Boolean);
   function Add_Link
     (Self     : not null access Dependency_Browser_Record'Class;
      From, To : File_Item;
      Explicit : Boolean) return Dependency_Link;
   --  Create a new link

   ----------
   -- Misc --
   ----------

   type Command_Data is record
      Browser          : Dependency_Browser;
      Link_From_Item   : Boolean := True;
      Item             : File_Item;
      Items            : Items_Lists.List;
      --  The items to tbe added. This does not include the items that were
      --  already in the browser.
   end record;

   procedure Destroy (Self : in out Command_Data);
   --  Called when the computation has finished

   procedure Examine_Dependencies
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : GNATCOLL.Projects.Project_Type);
   --  Examine the dependencies for File in In_Browser.
   --  The browser is not cleared first.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Examine_From_Dependencies
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : GNATCOLL.Projects.Project_Type);
   --  Display the list of files that depend directly on File.
   --  if Interactive is True, then the computation is done in an idle loop, so
   --  that the application remains responsive for the user.
   --  Layout is recomputed on exit if Recompute_Layout is true

   procedure Find_Or_Create_File
     (Self        : General_Browser;
      Filename    : Virtual_File;
      Project     : Project_Type;
      Item        : out File_Item;
      Newly_Added : out Boolean);
   --  Return the child that shows Filename in the browser, or null if Filename
   --  is not already displayed in the canvas.

   function Filter (File : Virtual_File) return Boolean;
   --  A filter function that decides whether File should be displayed in the
   --  canvas. It should return false if File should not be displayed.
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

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type;
   --  Return the name of the project that contains Item

   procedure Depends_On_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the command "uses" and "used_by"

   -----------------
   -- Save_To_XML --
   -----------------

   overriding function Save_To_XML
     (Self : not null access File_Item_Record)
      return XML_Utils.Node_Ptr
   is
      N : constant XML_Utils.Node_Ptr := new XML_Utils.Node;

   begin
      N.Tag := new String'("file");
      XML_Utils.Set_Attribute (N, "file", Self.Source.Display_Full_Name);
      XML_Utils.Set_Attribute
        (N, "project", Project_Of (Self).Project_Path.Display_Full_Name);

      return N;
   end Save_To_XML;

   -----------------
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (Self : not null access Dependency_Link_Record;
      Node : not null XML_Utils.Node_Ptr)
   is
   begin
      if Self.Explicit then
         XML_Utils.Set_Attribute (Node, "explicit", "1");
      end if;
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding function Load_From_XML
     (Self : not null access Dependency_Browser_Record;
      Node : XML_Utils.Node_Ptr) return access GPS_Item_Record'Class
   is
      It          : File_Item;
      Newly_Added : Boolean;
   begin
      Find_Or_Create_File
        (General_Browser (Self),
         Filename => Create (+XML_Utils.Get_Attribute (Node, "file")),
         Project  =>
           Lookup_Project
             (Self.Kernel,
              Create (+XML_Utils.Get_Attribute (Node, "project"))),
         Item     => It,
         Newly_Added => Newly_Added);
      return It;
   end Load_From_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (Self     : not null access Dependency_Browser_Record;
      Node     : XML_Utils.Node_Ptr;
      From, To : not null access GPS_Item_Record'Class)
   is
   begin
      Self.Add_Link
        (File_Item (From), File_Item (To),
         Explicit => XML_Utils.Get_Attribute (Node, "explicit") = "1");
   end Load_From_XML;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Dependency_Browser_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      General_Browser_Record (View.all).Create_Toolbar (Toolbar); --  inherited

   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Dependency_Browser_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      General_Browser_Record (View.all).Create_Menu (Menu);  --  inherited
      Append_Menu (Menu, View.Kernel, Show_System_Files);
      Append_Menu (Menu, View.Kernel, Show_Implicit);
   end Create_Menu;

   -----------------
   -- Set_Context --
   -----------------

   overriding procedure Set_Context
     (Item    : not null access File_Item_Record;
      Context : in out Selection_Context) is
   begin
      Set_File_Information
        (Context,
         Files   => (1 => Item.Source),
         Project => Project_Of (Item));
   end Set_Context;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      B : constant Dependency_Browser :=
        Dependency_Views.Retrieve_View (Kernel);
   begin
      if B /= null then
         Browser_Model (B.Get_View.Model).Clear;
      end if;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Dependency_Browser_Record'Class)
      return Gtk_Widget is
   begin
      Browsers.Canvas.Initialize (View);
      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View,
         Context_Func    => null);
      return Gtk_Widget (View.Get_View);
   end Initialize;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Show_Importing_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Self, View);
      It : constant File_Item := File_Item (Details.Toplevel_Item);
   begin
      Examine_From_Dependencies
        (Get_Kernel (It.Browser), It.Source, Project_Of (It));
   end On_Click;

   --------------
   -- On_Click --
   --------------

   overriding procedure On_Click
     (Self    : not null access Show_Imported_Button;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access)
   is
      pragma Unreferenced (Self, View);
      It : constant File_Item := File_Item (Details.Toplevel_Item);
   begin
      Examine_Dependencies (Get_Kernel (It.Browser), It.Source,
                            Project_Of (It));
   end On_Click;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Command_Data) is
      Horizontal : constant Boolean := Self.Browser.Horizontal_Layout;
      Dir   : Specific_Direction;
   begin
      if Horizontal then
         Dir := (if Self.Link_From_Item then Right else Left);
      else
         Dir := (if Self.Link_From_Item then Down else Up);
      end if;

      Self.Browser.Get_View.Model.Clear_Selection;
      Self.Browser.Get_View.Model.Add_To_Selection (Self.Item);
      Insert_And_Layout_Items
        (Self.Browser.Get_View,
         Ref   => Self.Item,
         Items                => Self.Items,
         Direction            => Dir,
         Space_Between_Items  => Space_Between_Items,
         Space_Between_Layers => Space_Between_Layers,
         Duration             => 0.3);
   end Destroy;

   -----------------------
   -- Show_Dependencies --
   -----------------------

   procedure Show_Dependencies
     (Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Project      : Project_Type;
      Dependencies : Dependency_Description_Vectors.Vector)
   is
      Browser       : constant Dependency_Browser :=
        Dependency_Views.Get_Or_Create_View (Kernel, Focus => True);
      Item          : File_Item;
      Link          : Dependency_Link;
      Must_Add_Link : Boolean;
      Data          : Command_Data;
      Newly_Added   : Boolean;
   begin
      Browser.Set_Activity_Progress_Bar_Visibility (False);

      Find_Or_Create_File
        (Self        => General_Browser (Browser),
         Filename    => File,
         Project     => Project,
         Item        => Data.Item,
         Newly_Added => Newly_Added);
      Data.Link_From_Item := True;
      Data.Browser := Browser;

      for Dependency of Dependencies loop
         Find_Or_Create_File
           (General_Browser (Browser),
            Filename    => Dependency.File,
            Project     => Get_Registry (Kernel).Tree.Project_From_Path
            (Dependency.Project_Path),
            Item        => Item,
            Newly_Added => Newly_Added);

         if Newly_Added then
            Data.Items.Append (Abstract_Item (Item));
            Must_Add_Link := True;
         else
            Must_Add_Link := not Browser.Has_Link (Data.Item, Item);
         end if;

         if Must_Add_Link then
            Link := Add_Link (Browser, Data.Item, Item, Explicit => True);
         end if;

         if not Filter (Dependency.File) then
            Item.Hide;

            if Link /= null then
               Link.Hide;
            end if;
         end if;
      end loop;

      Destroy (Data);
   end Show_Dependencies;

   --------------------------
   -- Compute_Dependencies --
   --------------------------

   overriding procedure Compute_Dependencies
     (Provider      : Xref_Dependency_Browser_Provider_Type;
      Kernel        : not null access Kernel_Handle_Record'Class;
      File          : Virtual_File;
      Project       : GNATCOLL.Projects.Project_Type;
      Kind          : Dependency_Kind_Type;
      Show_Implicit : Boolean)
   is
      Iter         : File_Iterator;
      Dependencies : Dependency_Description_Vectors.Vector;
   begin
      case Kind is
         when Show_Imported =>
            Iter := Kernel.Databases.Find_Dependencies
              (File, Project);
         when Show_Importing =>
            Iter := Kernel.Databases.Find_Ancestor_Dependencies
              (File, Project);
      end case;

      while Iter.Has_Element loop
         Dependencies.Append
           (Dependency_Description_Type'
              (File         => Iter.Element,
               Project_Path => Iter.Project
                 (Get_Registry (Kernel).Tree.all).Project_Path));
         Iter.Next;
      end loop;

      Show_Dependencies
        (Kernel,
         File         => File,
         Project      => Project,
         Dependencies => Dependencies);
   end Compute_Dependencies;

   --------------------------
   -- Examine_Dependencies --
   --------------------------

   procedure Examine_Dependencies
     (Kernel           : access Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : GNATCOLL.Projects.Project_Type) is
   begin
      Dependency_Views.Get_Or_Create_View
        (Kernel => Kernel, Focus => True).Set_Activity_Progress_Bar_Visibility
        (True);

      Dependency_Browser_Provider.Compute_Dependencies
        (Kernel        => Kernel,
         File          => File,
         Project       => Project,
         Kind          => Show_Imported,
         Show_Implicit => Show_Implicit.Get_Pref);
   end Examine_Dependencies;

   -------------------------------
   -- Examine_From_Dependencies --
   -------------------------------

   procedure Examine_From_Dependencies
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      File             : Virtual_File;
      Project          : Project_Type) is
   begin
      Dependency_Views.Get_Or_Create_View
        (Kernel => Kernel, Focus => True).Set_Activity_Progress_Bar_Visibility
        (True);

      Dependency_Browser_Provider.Compute_Dependencies
        (Kernel,
         File,
         Project,
         Kind          => Show_Importing,
         Show_Implicit => Show_Implicit.Get_Pref);
   end Examine_From_Dependencies;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File (Source : Virtual_File) return Boolean is
      Name : constant String :=
        Source.Display_Base_Name (Source.File_Extension);

      subtype Str8 is String (1 .. 8);
      Name_To_8 : Str8 := (others => ' ');

      Predef_Names : constant array (1 .. 11) of Str8 :=
        ("ada     ",       -- Ada
         "interfac",       -- Interfaces
         "system  ",       -- System
         "calendar",       -- Calendar
         "machcode",       -- Machine_Code
         "unchconv",       -- Unchecked_Conversion
         "unchdeal",       -- Unchecked_Deallocation
         "directio",       -- Direct_IO
         "ioexcept",       -- IO_Exceptions
         "sequenio",       -- Sequential_IO
         "text_io ");      -- Text_IO
   begin
      if Name'Length < 3 then
         return False;
      end if;

      if Name (Name'First .. Name'First + 1) in "g-" | "a-" | "s-" | "i-" then
         if
           Name (Name'First + 2) in 'a' .. 'z'
           or else Name (Name'First + 2) in 'A' .. 'Z'
         then
            return True;
         end if;
      end if;

      --  Check for renamings.
      if Name'Length < 9 then
         Name_To_8 (1 .. Name'Length) := Name;
         for I in Predef_Names'Range loop
            if Name_To_8 = Predef_Names (I) then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Is_System_File;

   ------------
   -- Filter --
   ------------

   function Filter (File : Virtual_File) return Boolean is
   begin
      --  Do not display dependencies on runtime files if the associated
      --  preference is not set.

      return Show_System_Files.Get_Pref or else not Is_System_File (File);
   end Filter;

   -------------------------
   -- Find_Or_Create_File --
   -------------------------

   procedure Find_Or_Create_File
     (Self        : General_Browser;
      Filename    : Virtual_File;
      Project     : Project_Type;
      Item        : out File_Item;
      Newly_Added : out Boolean)
   is
      S : constant access Browser_Styles := Self.Get_View.Get_Styles;

      procedure On_Item (It : not null access Abstract_Item_Record'Class);
      procedure On_Item (It : not null access Abstract_Item_Record'Class) is
      begin
         if Item = null   --  not found yet
           and then File_Item (It).Source = Filename
           and then Project_Of (File_Item (It)) = Project
         then
            Item := File_Item (It);
         end if;
      end On_Item;

   begin
      Item := null;
      Newly_Added := False;
      Self.Get_View.Model.For_Each_Item (On_Item'Access, Filter => Kind_Item);

      if Item = null then
         Newly_Added  := True;
         Item         := new File_Item_Record;
         Item.Browser := Self;
         Item.Source  := Filename;
         Item.Project_Path := Project.Project_Path;

         Item.Initialize_Rect (Style => S.Item, Radius => 5.0);

         Setup_Titlebar
           (Item    => Item,
            Browser => Self,
            Name    => Item.Source.Display_Base_Name,
            Left    => new Show_Importing_Button,
            Right   => new Show_Imported_Button);

         Browser_Model (Self.Get_View.Model).Add (Item);
         Item.Set_Position (No_Position);
      end if;
   end Find_Or_Create_File;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Dependencies_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Browser : constant Dependency_Browser :=
        Dependency_Views.Retrieve_View (Kernel);
      Focus_Child : constant MDI_Child :=
        Get_MDI (Kernel).Get_Focus_Child;
   begin
      --  If we have a browser focused, try to examine the dependencies of
      --  the selected items, if any.

      if Browser /= null
        and then MDI_Child
          (Dependency_Views.Child_From_View (Browser)) = Focus_Child
      then
         declare
            Model          : constant Canvas_Model :=
              Browser.Get_View.Model;
            Selected_Items : constant Item_Sets.Set :=
              Model.Get_Selected_Items;
         begin
            if not Selected_Items.Is_Empty then
               for Item of Selected_Items loop
                  if Command.Command_Type = Show_File_Dependencies then
                     Examine_Dependencies
                       (Get_Kernel (Context.Context),
                        File_Item (Item).Source,
                        Project_Information (Context.Context));
                  else
                     Examine_From_Dependencies
                       (Get_Kernel (Context.Context),
                        File_Item (Item).Source,
                        Project_Information (Context.Context));
                  end if;
               end loop;

               return Commands.Success;
            end if;
         end;
      end if;

      --  If there is no focused browser or no selected items, use the
      --  context's file information.

      if Command.Command_Type = Show_File_Dependencies then
         Examine_Dependencies
           (Get_Kernel (Context.Context),
            File_Information (Context.Context),
            Project_Information (Context.Context));
      else
         Examine_From_Dependencies
           (Get_Kernel (Context.Context),
            File_Information (Context.Context),
            Project_Information (Context.Context));
      end if;

      return Commands.Success;
   end Execute;

   ------------------------------------
   -- Show_Dependencies_Menu_Factory --
   ------------------------------------

   function Show_Dependencies_Menu_Factory
     (Context : Selection_Context) return String
   is
      Kernel      : constant Kernel_Handle := Get_Kernel (Context);
      Browser     : constant Dependency_Browser :=
        Dependency_Views.Retrieve_View (Get_Kernel (Context));
      Focus_Child : constant MDI_Child :=
        Get_MDI (Kernel).Get_Focus_Child;
   begin
      --  Check if have a focused browser with a multiple selection: if it's
      --  the case, adapt the contextual menu.

      if Browser /= null
        and then MDI_Child
          (Dependency_Views.Child_From_View (Browser)) = Focus_Child
          and then Integer
            (Item_Sets.Length
               (Browser.Get_View.Model.Get_Selected_Items)) > 1
      then
         return "selected items";
      elsif Has_File_Information (Context) then
         return "<b>" & File_Information (Context).Display_Base_Name & "</b>";
      else
         return "";
      end if;
   end Show_Dependencies_Menu_Factory;

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

      declare
         F_Info : constant File_Info'Class :=
           File_Info'Class
             (Get_Registry
                (Kernel).Tree.Info_Set (File).First_Element);
      begin
         Project := F_Info.Project;
      end;

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
   -- Refresh_Browser --
   ---------------------

   procedure Refresh_Browser
     (Browser : not null access Dependency_Browser_Record'Class)
   is

      procedure On_Link (Link : not null access Abstract_Item_Record'Class);
      --  Checks that the given link is still valid

      procedure On_Link (Link : not null access Abstract_Item_Record'Class) is
         L         : constant GPS_Link := GPS_Link (Link);
         Src_Item  : constant File_Item := File_Item (Get_From (L));
         Dest_Item : constant File_Item := File_Item (Get_To (L));
         Show_Link : Boolean := True;
      begin
         if not Filter (Src_Item.Source) then
            Src_Item.Hide;
            Show_Link := False;
         else
            Src_Item.Show;
         end if;

         if not Filter (Dest_Item.Source) then
            Dest_Item.Hide;
            Show_Link := False;
         else
            Dest_Item.Show;
         end if;

         --  Show the link only if both source and dest items are visible
         if Show_Link then
            L.Show;
         else
            L.Hide;
         end if;
      end On_Link;

   begin
      Browser.Get_View.Model.For_Each_Item
        (On_Link'Access, Filter => Kind_Link);
   end Refresh_Browser;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   overriding procedure Preferences_Changed
     (Self : not null access Dependency_Browser_Record;
      Pref : Default_Preferences.Preference) is
   begin
      if Pref = null   --  multiple preferences updated
        or else Pref = Preference (Show_System_Files)
      then
         Force_Refresh (Self);
         Refresh_Browser (Self);

      elsif Pref = Preference (Show_Implicit) then

         --  The user needs to re-compute the dependencies manually to take the
         --  value of the "Show implicit dependencies" preference into account,
         --  so warn him with an info bar displayed at the top of the browser.

         declare
            Info_Bar : constant Gtk_Info_Bar := Create_Info_Bar
              ("You need to re-compute the dependencies manually to take the "
               & "new value for ""Show implicit dependencies"" "
               & "preference into account.",
              Message_Warning);
         begin
            Self.Pack_Start (Info_Bar, Expand => False);
            Self.Reorder_Child (Info_Bar, 0);
            Info_Bar.Show_All;
         end;
      end if;
   end Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter : constant Action_Filter := not Lookup_Filter (Kernel, "Entity")
        and Lookup_Filter (Kernel, "In project");
   begin
      Dependency_Views.Register_Module (Kernel);

      --  Use the provider based on cross-references by default
      if Dependency_Browser_Provider = null then
         Dependency_Browser_Provider :=
           new Xref_Dependency_Browser_Provider_Type;
      end if;

      Show_System_Files := Kernel.Get_Preferences.Create_Invisible_Pref
        ("browser_show_system_files", False,
         Label => -"Show system files");
      Show_Implicit := Kernel.Get_Preferences.Create_Invisible_Pref
        ("browser_show_implicit", False,
         Label => -"Show implicit dependencies");

      Register_Action
        (Kernel, "Browser: show dependencies for file",
         Command     => new Show_Dependencies_Command'
           (Root_Command with Command_Type => Show_File_Dependencies),
         Description =>
           "Open the Dependency Browser to show all source files"
         & " that the selected file depends on",
         Filter    => Filter,
         Category  => -"Views");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Browsers/Show dependencies for %C",
         Custom => Show_Dependencies_Menu_Factory'Access,
         Action => "Browser: show dependencies for file");

      Register_Action
        (Kernel, "Browser: show files depending on file",
         Command     => new Show_Dependencies_Command'
           (Root_Command with Command_Type => Show_Depends_On_File),
         Description =>
           "Open the Dependency Browser to show all source files"
         & " that depend on the selected file",
         Filter    => Filter,
         Category  => -"Views");
      Register_Contextual_Menu
        (Kernel => Kernel,
         Label  => -"Browsers/Show files depending on %C",
         Custom => Show_Dependencies_Menu_Factory'Access,
         Action => "Browser: show files depending on file");

      Kernel.Scripts.Register_Command
        ("uses",
         Class   => Get_File_Class (Kernel),
         Handler => Depends_On_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("used_by",
         Class   => Get_File_Class (Kernel),
         Handler => Depends_On_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("imports",
         Minimum_Args => 0,
         Maximum_Args => 2,
         Class        => Get_File_Class (Kernel),
         Handler      => Depends_On_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("imported_by",
         Minimum_Args => 0,
         Maximum_Args => 2,
         Class        => Get_File_Class (Kernel),
         Handler      => Depends_On_Command_Handler'Access);

      Project_Changed_Hook.Add (new On_Project_Changed);
   end Register_Module;

   -------------------------------------
   -- Set_Dependency_Browser_Provider --
   -------------------------------------

   procedure Set_Dependency_Browser_Provider
     (Provider : not null access Dependency_Browser_Provider_Interface'Class)
   is
   begin
      Dependency_Browser_Provider := Provider;
   end Set_Dependency_Browser_Provider;

   --------------
   -- Add_Link --
   --------------

   procedure Add_Link
     (Self     : not null access Dependency_Browser_Record'Class;
      From, To : File_Item;
      Explicit : Boolean)
   is
      Link : constant Dependency_Link := Self.Add_Link
        (From => From, To => To, Explicit => Explicit)
        with Unreferenced;
   begin
      null;
   end Add_Link;

   --------------
   -- Add_Link --
   --------------

   function Add_Link
     (Self     : not null access Dependency_Browser_Record'Class;
      From, To : File_Item;
      Explicit : Boolean) return Dependency_Link
   is
      Styles : constant access Browser_Styles := Self.Get_View.Get_Styles;
      Link   : Dependency_Link;
   begin
      Link := new Dependency_Link_Record;
      Link.Explicit := Explicit;

      if Explicit then
         Link.Default_Style := Styles.Link;
      else
         Link.Default_Style := Styles.Link2;
      end if;

      Initialize
        (Link,
         From    => From,
         To      => To,
         Routing => Curve,
         Style   => Link.Default_Style);
      Browser_Model (Self.Get_View.Model).Add (Link);

      return Link;
   end Add_Link;

   ----------------
   -- Project_Of --
   ----------------

   function Project_Of
     (Item : access File_Item_Record'Class) return Project_Type is
   begin
      return GNATCOLL.Scripts.Projects
        .Project_Tree.Project_From_Path (Item.Project_Path);
   end Project_Of;

end Browsers.Dependency_Items;
