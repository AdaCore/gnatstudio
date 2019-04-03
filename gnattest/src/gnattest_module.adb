------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2019, AdaCore                     --
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

with Commands.GNATTest;
with Commands.Interactive;
with Glib.Object;                       use Glib.Object;
with Glib.Values;                       use Glib.Values;

with GNAT.Calendar.Time_IO;

with GNATCOLL.Scripts;                  use GNATCOLL.Scripts;

with Basic_Types;                       use Basic_Types;
with Entities_Tooltips;
with GNATTest_Module.Tree_Models;       use GNATTest_Module.Tree_Models;
with Language.Abstract_Language_Tree;
with Tooltips;                          use Tooltips;

with GPS.Kernel;                        use GPS.Kernel;
with GPS.Kernel.Actions;                use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;             use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;
with GPS.Kernel.Scripts;                use GPS.Kernel.Scripts;
with Gdk.Event;                         use Gdk.Event;
with Gdk.Rectangle;                     use Gdk.Rectangle;
with Gdk.Types.Keysyms;                 use Gdk.Types.Keysyms;
with Gtk.Box;                           use Gtk.Box;
with Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf;          use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;            use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                         use Gtk.Enums;
with Gtk.Gesture_Multi_Press;           use Gtk.Gesture_Multi_Press;
with Gtk.Handlers;
with Gtk.Label;                         use Gtk.Label;
with Gtk.Menu;
with Gtk.Menu_Item;
with Gtk.Tree_Model;                    use Gtk.Tree_Model;
with Gtk.Tree_View;                     use Gtk.Tree_View;
with Gtk.Tree_View_Column;              use Gtk.Tree_View_Column;
with Gtk.Scrolled_Window;               use Gtk.Scrolled_Window;
with Gtk.Widget;                        use Gtk.Widget;
with Gtkada.MDI;
with Gtkada.Abstract_Tree_Model;

with Input_Sources.File;
with Generic_Views;
with Sax.Readers;
with Sax.Attributes;
with Src_Editor_Box;
with Unicode.CES;
with Xref;                              use Xref;

package body GNATTest_Module is

   type GNATTest_Module_Record is new Module_ID_Record with null record;
   GNATTest_Module_ID   : Module_ID;
   GNATTest_Module_Name : constant String := "GNATTest_Support";

   type Harness_Project_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Non_Harness_Project_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Non_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Go_To_Tested_Filter is
     new GPS.Kernel.Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Go_To_Tested_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Package_Declaration_Filter is
     new GPS.Kernel.Action_Filter_Record with null record;

   overriding function Filter_Matches_Primitive
     (Filter  : access Package_Declaration_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   type Submenu_Factory_Record is
     new GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   function Get_Mapping_File
     (Project : GNATCOLL.Projects.Project_Type)
     return String;

   function Is_Harness_Project
     (Project : GNATCOLL.Projects.Project_Type)
     return Boolean;
   --  Check if given project is harness project

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);

   package Test_Entity_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Virtual_File,
      Element_Type => Source_Entity);

   type Modes is (Monolith, Separates);

   type Stub_Unit_Type is record
      Original : Virtual_File;
      Stub     : Virtual_File;
      Setter   : Virtual_File;
   end record;

   type Mapping_File is new Sax.Readers.Reader with record
      Kernel       : GPS.Kernel.Kernel_Handle;
      Mode         : Modes;
      Last_Source  : Source_Entity;
      Source_Map   : Source_Entity_Maps.Map;
      Test_Map     : Test_Entity_Maps.Map;
      First_Test   : Boolean;
      First_Tested : Boolean;
      Setup        : Test_Entity;
      Teardown     : Test_Entity;
      Stub_Unit    : Stub_Unit_Type;
   end record;

   overriding procedure Start_Element
     (Self          : in out Mapping_File;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   type Show_Not_Implemented_Tests_Command_Type is
     new Commands.Interactive.Interactive_Command with null record;

   overriding function Execute
     (Command : access Show_Not_Implemented_Tests_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the commands

   Map : Mapping_File;

   function Find_In_Map
     (File_Name : GNATCOLL.VFS.Virtual_File)
     return Test_Entity_Maps.Cursor;

   function Tested_Subprogram_Name
     (Context : GPS.Kernel.Selection_Context)
     return String;

   type Menu_Data is record
      Entity : Test_Entity;
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

   package Test_Entity_CB is new Gtk.Handlers.User_Callback
     (Gtk.Menu_Item.Gtk_Menu_Item_Record, Menu_Data);

   procedure Test_Entity_Callback
     (Widget    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      User_Data : Menu_Data);

   type Tests_View_Record is new Generic_Views.View_Record with record
      Tree_View     : Gtk_Tree_View;
      Tree_Model    : Tree_Models.Tree_Model;
      Icon_Column   : Gtk_Tree_View_Column;
      Icon_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Multipress    : Gtk_Gesture_Multi_Press;
   end record;

   function Initialize
     (Self : access Tests_View_Record'Class) return Gtk_Widget;
   --  Initialize the tests view widget

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble);
   --  Called every time a row in Tests View is clicked

   function On_Key_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean;
   --  Handle key events in the Tests View

   package Tests_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Tests_View",
      View_Name                 => "Tests",
      Formal_View_Record        => Tests_View_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Initialize                => Initialize,
      Areas                     => Gtkada.MDI.Sides_Only,
      Position                  => Gtkada.MDI.Position_Left);

   --------------
   -- Tooltips --
   --------------

   type Tests_View_Tooltip is new Tooltips.Tooltip_Handler with record
      View : Tests_MDI_Views.View_Access;
   end record;
   overriding function Create_Contents
     (Tooltip  : not null access Tests_View_Tooltip;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Source_Entity) return Boolean is
      Left_Name  : constant String := Left.Source_File.Display_Base_Name;
      Right_Name : constant String := Right.Source_File.Display_Base_Name;
   begin
      if Left_Name = Right_Name then
         if Left.Line = Right.Line then
            return Left.Test_Case_Name < Right.Test_Case_Name;
         else
            return Left.Line < Right.Line;
         end if;
      else
         return Left_Name < Right_Name;
      end if;
   end "<";

   --------------------
   -- Append_To_Menu --
   --------------------

   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Factory);

      Item : Gtk.Menu_Item.Gtk_Menu_Item;

      Entity : constant Root_Entity'Class := Get_Entity (Context);

      Declaration : constant General_Entity_Declaration :=
        Get_Declaration (Entity);

      Lookup : Source_Entity;
      Cursor : Source_Entity_Maps.Cursor;
   begin
      Lookup.Source_File := Declaration.Loc.File;
      Lookup.Line := Declaration.Loc.Line;

      Cursor := Map.Source_Map.Floor (Lookup);

      if Source_Entity_Maps.Has_Element (Cursor) then
         Cursor := Source_Entity_Maps.Next (Cursor);
      else
         Cursor := Map.Source_Map.First;
      end if;

      while Source_Entity_Maps.Has_Element (Cursor) loop
         declare
            Found : constant Source_Entity := Source_Entity_Maps.Key (Cursor);
         begin

            exit when Found.Source_File /= Lookup.Source_File or
              Found.Line /= Lookup.Line;

            Gtk.Menu_Item.Gtk_New
              (Item, "Go to " & To_String (Found.Test_Case_Name));

            Menu.Append (Item);

            Test_Entity_CB.Connect
              (Item,
               Gtk.Menu_Item.Signal_Activate,
               Test_Entity_Callback'Access,
               (Entity => Source_Entity_Maps.Element (Cursor),
                Kernel => GPS.Kernel.Get_Kernel (Context)));

            Source_Entity_Maps.Next (Cursor);
         end;
      end loop;

   end Append_To_Menu;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      use GNATCOLL.Projects;

      function Origin_Project (Project : Project_Type) return Project_Type;
      --  Guess origin project - source for gnattest to generate given one

      --------------------
      -- Origin_Project --
      --------------------

      function Origin_Project (Project : Project_Type) return Project_Type is
         File : Virtual_File;

      begin
         if Is_Harness_Project (Project)
           and then Project.Has_Attribute (Origin_Project_Attribute)
         then
            File := Create
              (+Project.Attribute_Value (Origin_Project_Attribute));

            if not File.Is_Absolute_Path then
               File := Project.Project_Path.Dir_Name / File;
               File.Normalize_Path;
            end if;

            return
              GPS.Kernel.Project.Lookup_Project (Get_Kernel (Data), File);

         else
            return No_Project;
         end if;
      end Origin_Project;

   begin
      if Command = "is_harness_project" then
         declare
            Project : constant Project_Type := Get_Data (Data, 1);

         begin
            Set_Return_Value (Data, Is_Harness_Project (Project));
         end;
      elsif Command = "original_project" then
         declare
            Project : constant Project_Type := Get_Data (Data, 1);

         begin
            Set_Return_Value
              (Data,
               Create_Project (Data.Get_Script, Origin_Project (Project)));
         end;
      end if;
   end Command_Handler;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Tests_View_Tooltip;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Iter     : Gtk_Tree_Iter;
      Area     : Gdk_Rectangle;
      Value    : Glib.Values.GValue;
   begin
      Initialize_Tooltips (Tooltip.View.Tree_View, X, Y, Area, Iter);

      if Iter = Null_Iter then
         return null;
      end if;

      Tooltip.View.Tree_Model.Get_Value (Iter, File_Level_Column, Value);
      Tooltip.Set_Tip_Area (Area);

      declare
         Src : constant Source_Entity :=
           Tooltip.View.Tree_Model.Get_Source_Entity (Iter);

         Ref : Root_Entity_Reference_Ref;

         Entity : constant Root_Entity'Class :=
           Tooltip.View.Kernel.Databases.Get_Entity
             (Name        => To_String (Src.Subprogram_Name),
              Closest_Ref => Ref,
              Loc         =>
                (File   => Src.Source_File,
                 Line   => Src.Line,
                 Column => Visible_Column_Type (Src.Column + 9),
                 others => <>));

      begin
         if Get_Boolean (Value) then  --  If we are on file level of tree

            declare
               Label : Gtk_Label;
            begin
               Gtk_New
                 (Label, Get_Tooltip_For_File
                    (Tooltip.View.Kernel, Src.Source_File));

               Label.Set_Use_Markup (True);

               return Gtk_Widget (Label);
            end;
         else

               return Entities_Tooltips.Draw_Tooltip
                 (Kernel      => Tooltip.View.Kernel,
                  Entity      => Entity,
                  Ref         => Ref.Element,
                  Draw_Border => True);
         end if;
      end;
   end Create_Contents;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Show_Not_Implemented_Tests_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Command);
      use type Ada.Calendar.Time;

      Flags    : constant GPS.Kernel.Messages.Message_Flags :=
        (GPS.Kernel.Messages.Locations => True,
         others                        => False);

      Category : constant String := "GNATtest";
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Messages : constant not null GPS.Kernel.Messages_Container_Access :=
                   Kernel.Get_Messages_Container;
      Cursor   : Source_Entity_Maps.Cursor := Map.Source_Map.First;

   begin
      Kernel.Get_Messages_Container.Remove_Category (Category, Flags);

      while Source_Entity_Maps.Has_Element (Cursor) loop
         declare
            Key  : constant Source_Entity := Source_Entity_Maps.Key (Cursor);
            Item : constant Test_Entity := Source_Entity_Maps.Element (Cursor);
            File : constant GNATCOLL.VFS.Virtual_File := Key.Source_File;
            Test : constant GNATCOLL.VFS.Virtual_File := Item.File_Name;

         begin
            if Test.File_Time_Stamp = Item.Stamp then
               GPS.Kernel.Messages.Simple.Create_Simple_Message
                 (Container  => Messages,
                  Category   => Category,
                  File       => File,
                  Line       => Key.Line,
                  Column     => Basic_Types.Visible_Column_Type (Key.Column),
                  Text       => "Unimplemented " &
                    To_String (Key.Test_Case_Name) & " " &
                    To_String (Key.Subprogram_Name),
                  Importance => GPS.Kernel.Messages.Low,
                  Flags      => Flags);
            end if;

            Source_Entity_Maps.Next (Cursor);
         end;
      end loop;

      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      declare
         Project : constant GNATCOLL.Projects.Project_Type
           := GPS.Kernel.Project.Get_Project (GPS.Kernel.Get_Kernel (Context));

      begin
         return Is_Harness_Project (Project);
      end;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Non_Harness_Project_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      declare
         Project : constant GNATCOLL.Projects.Project_Type
           := GPS.Kernel.Project.Get_Project (GPS.Kernel.Get_Kernel (Context));

      begin
         return not Is_Harness_Project (Project);
      end;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Go_To_Tested_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_File_Information (Context) then
         return Test_Entity_Maps.Has_Element
             (Find_In_Map (File_Information (Context)));
      else
         return False;
      end if;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Package_Declaration_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      use type GNATCOLL.Projects.Unit_Parts;
      File : GNATCOLL.VFS.Virtual_File;
   begin
      if Has_File_Information (Context) then
         File := File_Information (Context);

         if File.Is_Regular_File then
            --  Safe to use the first matching info, since the language will
            --  most likely be the same in all projects anyway.

            declare
               Info : constant GNATCOLL.Projects.File_Info'Class :=
                 GNATCOLL.Projects.File_Info'Class
                   (GPS.Kernel.Project.Get_Registry
                      (GPS.Kernel.Get_Kernel (Context)).Tree.Info_Set (File)
                    .First_Element);
            begin

               return Info.Language = "ada" and then
                 Info.Unit_Part = GNATCOLL.Projects.Unit_Spec;
            end;
         end if;
      end if;

      return False;
   end Filter_Matches_Primitive;

   -----------------
   -- Find_Tested --
   -----------------

   procedure Find_Tested
     (File_Name       : Virtual_File;
      File            : out Virtual_File;
      Subprogram_Name : out Ada.Strings.Unbounded.Unbounded_String;
      Line            : out Natural;
      Column          : out Basic_Types.Visible_Column_Type)
   is
      Cursor : constant Test_Entity_Maps.Cursor := Find_In_Map (File_Name);
   begin
      if Test_Entity_Maps.Has_Element (Cursor) then
         File := Test_Entity_Maps.Element (Cursor).Source_File;
         Subprogram_Name := Test_Entity_Maps.Element (Cursor).Subprogram_Name;
         Line := Test_Entity_Maps.Element (Cursor).Line;
         Column := Basic_Types.Visible_Column_Type
           (Test_Entity_Maps.Element (Cursor).Column);
      else
         File := GNATCOLL.VFS.No_File;
         Subprogram_Name := Ada.Strings.Unbounded.Null_Unbounded_String;
         Line := 0;
         Column := 0;
      end if;
   end Find_Tested;

   -----------------
   -- Find_In_Map --
   -----------------

   function Find_In_Map
     (File_Name : GNATCOLL.VFS.Virtual_File)
     return Test_Entity_Maps.Cursor is
   begin
      return Map.Test_Map.Find (File_Name);
   end Find_In_Map;

   ----------------------
   -- Get_Mapping_File --
   ----------------------

   function Get_Mapping_File
     (Project : GNATCOLL.Projects.Project_Type)
     return String
   is
      Name  : constant GNATCOLL.Projects.Attribute_Pkg_String
        := GNATCOLL.Projects.Build ("GNATtest", "GNATtest_Mapping_File");
   begin
      return Project.Attribute_Value (Name);
   end Get_Mapping_File;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);

      File : Input_Sources.File.File_Input;
      View : Tests_MDI_Views.View_Access;
      pragma Unreferenced (View);

      Project       : constant GNATCOLL.Projects.Project_Type :=
         GPS.Kernel.Project.Get_Project (Kernel);
      Map_File_Name : constant String := Get_Mapping_File (Project);
   begin
      Tests_MDI_Views.Close (Kernel);
      Map.Source_Map.Clear;
      Map.Test_Map.Clear;
      Map.Kernel := GPS.Kernel.Kernel_Handle (Kernel);

      if Map_File_Name /= "" then
         Input_Sources.File.Open (Map_File_Name, File);
         Map.Parse (File);
         Input_Sources.File.Close (File);
         View := Tests_MDI_Views.Get_Or_Create_View (Kernel);
      end if;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Tests_View_Record'Class) return Gtk_Widget
   is

      Icon_Renderer     : Gtk_Cell_Renderer_Pixbuf;
      Tooltip           : Tooltip_Handler_Access;
      Scrolled          : Gtk_Scrolled_Window;
      Column            : Gtk_Tree_View_Column;
      Text_Renderer     : Gtk_Cell_Renderer_Text;
      Column_Id         : Glib.Gint;
      pragma Unreferenced (Column_Id);
   begin
      --  Initialize the view itself
      Initialize_Vbox (Self, Homogeneous => False);

      --  Create a scrolled window to contain all the view's widgets
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      --  Create a tree view to display the test case list
      Gtk_New (Self.Tree_Model, Map.Source_Map);
      Gtk_New (Self.Tree_View,
               Gtkada.Abstract_Tree_Model."+" (Self.Tree_Model));
      Self.Tree_View.Set_Headers_Visible (False);
      Self.Tree_View.Set_Search_Column (Name_Column);
      Scrolled.Add (Self.Tree_View);

      --  Create a tree view column to display icons
      Gtk_New (Column);
      --  Add setup/source icon
      Gtk_New (Icon_Renderer);
      Column.Pack_Start (Icon_Renderer, Expand => False);
      Column.Add_Attribute (Icon_Renderer, "icon-name", First_Icon_Column);
      Column.Set_Title ("Icons");
      --  Keep Icon_Renderer to allow icon size calculaton on clicks
      Self.Icon_Renderer := Icon_Renderer;

      --  Add teardown icon
      Gtk_New (Icon_Renderer);
      Column.Pack_Start (Icon_Renderer, Expand => False);
      Column.Add_Attribute (Icon_Renderer, "icon-name", Second_Icon_Column);
      Column.Add_Attribute (Icon_Renderer, "visible", File_Level_Column);
      --  This text renderer with empty text aligns icons to the left
      Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, Expand => True);

      Column_Id := Self.Tree_View.Append_Column (Column);
      Self.Icon_Column := Column;

      --  Create a tree view column to display the tested source
      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "text", Name_Column);
      Column.Set_Title ("Name");
      Column_Id := Self.Tree_View.Append_Column (Column);

      Gtk_New (Self.Multipress, Widget => Self.Tree_View);
      Self.Multipress.On_Pressed (On_Multipress'Access, Slot => Self);
      Self.Multipress.Watch (Self);

      Self.Tree_View.On_Key_Press_Event
        (On_Key_Press'Access, Slot => Self, After => False);

      Tooltip := new Tests_View_Tooltip'
        (Tooltips.Tooltip_Handler
         with View => Tests_MDI_Views.View_Access (Self));

      Tooltip.Associate_To_Widget (Self.Tree_View);
      --  No widget to focus
      return null;
   end Initialize;

   ------------------------
   -- Is_Harness_Project --
   ------------------------

   function Is_Harness_Project
     (Project : GNATCOLL.Projects.Project_Type)
      return Boolean is
   begin
      return Get_Mapping_File (Project) /= "";
   end Is_Harness_Project;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk_Event_Key) return Boolean
   is
      use type Gdk.Types.Gdk_Key_Type;

      Iter        : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;
      Source      : Source_Entity;
      Destination : Test_Entity;
      View        : constant Tests_MDI_Views.View_Access :=
        Tests_MDI_Views.View_Access (Self);
   begin
      View.Tree_View.Get_Selection.Get_Selected (Model, Iter);

      if Iter /= Null_Iter then
         if Event.Keyval = GDK_Return then
            Source := View.Tree_Model.Get_Source_Entity (Iter);

            if Map.Source_Map.Contains (Source) then

               Destination := Map.Source_Map.Element (Source);

               Open_File
                 (View.Kernel,
                  GNATCOLL.Projects.No_Project,
                  Destination.File_Name,
                  Destination.Line,
                  Basic_Types.Visible_Column_Type (Destination.Column),
                  To_String (Destination.Subprogram_Name));
            else

               Open_File
                 (View.Kernel,
                  GNATCOLL.Projects.No_Project,
                  Source.Source_File, 1, 1);
            end if;

            return True;
         end if;
      end if;

      return False;
   end On_Key_Press;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble)
   is
      use type Glib.Gint;

      View : constant Tests_MDI_Views.View_Access :=
        Tests_MDI_Views.View_Access (Self);
      Iter  : Gtk_Tree_Iter;

      Cell_X, Cell_Y  : Glib.Gint;
      Column          : Gtk_Tree_View_Column;
      Success         : Boolean;
      Area, Cell_Area : Gdk_Rectangle;
      Path            : Gtk_Tree_Path;
      Source          : Source_Entity;
      Destination     : Test_Entity;
      Value           : Glib.Values.GValue;
      Is_File_Level   : Boolean;
   begin
      View.Tree_View.Get_Path_At_Pos
        (Glib.Gint (X), Glib.Gint (Y), Path, Column, Cell_X, Cell_Y, Success);

      if N_Press = 2 and Success then
         --  Area is the rectangle, within the TreeView, where the
         --  column is displayed. For instance x=20 to 264
         View.Tree_View.Get_Cell_Area (Path, Column, Cell_Area);

         --  Aligned area is the rectangle within the rectangle where
         --  the renderer is displayed. Only the size seems to be set
         --  to an interesting value, the X coordinate is unclear.
         --  We just assume all icons have the same size.

         View.Icon_Renderer.Get_Aligned_Area
           (Widget       => View.Tree_View,
            Flags        => Gtk.Cell_Renderer.Cell_Renderer_Focused,
            Cell_Area    => Cell_Area,
            Aligned_Area => Area);

         Iter := View.Tree_Model.Get_Iter (Path);
         Source := View.Tree_Model.Get_Source_Entity (Iter);
         View.Tree_Model.Get_Value (Iter, File_Level_Column, Value);
         Is_File_Level := Get_Boolean (Value);

         if Column = View.Icon_Column
           and Glib.Gint (X) >= Cell_Area.X
           and Source.Test_Case_Name /= ""
         then
            --  Click in some of icon in the icon column

            if Glib.Gint (X) - Cell_Area.X <= Area.Width then

               if Is_File_Level then
                  --  Click on the first icon on file level (test setup)
                  Source.Test_Case_Name := Test_Setup;
                  Destination := Map.Source_Map.Element (Source);

                  Open_File
                    (View.Kernel,
                     GNATCOLL.Projects.No_Project,
                     Destination.File_Name,
                     Destination.Line,
                     Basic_Types.Visible_Column_Type (Destination.Column),
                     To_String (Destination.Subprogram_Name));
               else
                  --  Click on the first icon on test level (tested unit)

                  Open_File
                    (View.Kernel,
                     GNATCOLL.Projects.No_Project,
                     Source.Source_File,
                     Source.Line,
                     Basic_Types.Visible_Column_Type (Source.Column),
                     To_String (Source.Subprogram_Name));
               end if;

            elsif Glib.Gint (X) - Cell_Area.X <= 2 * Area.Width then

               if Is_File_Level then
                  --  Click on the second icon (test teardown)
                  Source.Test_Case_Name := Test_Teardown;
                  Destination := Map.Source_Map.Element (Source);

                  Open_File
                    (View.Kernel,
                     GNATCOLL.Projects.No_Project,
                     Destination.File_Name,
                     Destination.Line,
                     Basic_Types.Visible_Column_Type (Destination.Column),
                     To_String (Destination.Subprogram_Name));
               end if;
            end if;
         else

            if Is_File_Level or else not Map.Source_Map.Contains (Source) then

               Open_File
                 (View.Kernel,
                  GNATCOLL.Projects.No_Project,
                  Source.Source_File, 1, 1);
            else

               Destination := Map.Source_Map.Element (Source);

               Open_File
                 (View.Kernel,
                  GNATCOLL.Projects.No_Project,
                  Destination.File_Name,
                  Destination.Line,
                  Basic_Types.Visible_Column_Type (Destination.Column),
                  To_String (Destination.Subprogram_Name));
            end if;
         end if;

         View.Multipress.Set_State (Event_Sequence_Claimed);
      end if;
   end On_Multipress;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Kernel          : GPS.Kernel.Kernel_Handle;
      Project         : GNATCOLL.Projects.Project_Type;
      File            : Virtual_File;
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Subprogram_Name : String := "") is

   begin
      Src_Editor_Box.Go_To_Closest_Match
        (Kernel      => Kernel,
         Filename    => File,
         Project     => Project,
         Line        => Editable_Line_Type (Line),
         Column      => Column,
         Entity_Name => Subprogram_Name);
   end Open_File;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      use Commands.GNATTest;
      Filter : Action_Filter;
   begin
      GNATTest_Module_ID := new GNATTest_Module_Record;

      Register_Module
        (Module      => GNATTest_Module_ID,
         Kernel      => Kernel,
         Module_Name => GNATTest_Module_Name,
         Priority    => Default_Priority);

      Filter := new Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Harness project");

      GPS.Kernel.Actions.Register_Action
        (Kernel      => Kernel,
         Name        => "Show not implemented tests",
         Command     => new Show_Not_Implemented_Tests_Command_Type,
         Filter      => Filter);

      Filter := new Non_Harness_Project_Filter;
      Register_Filter (Kernel, Filter, "Non harness project");

      Filter := new Package_Declaration_Filter;
      Register_Filter (Kernel, Filter, "Library package declaration");

      Filter := new Go_To_Tested_Filter;
      Register_Filter (Kernel, Filter, "Tested exists");

      Register_Contextual_Submenu
        (Kernel   => Kernel,
         Name     => "GNATtest",
         Label    => "GNATtest",
         Submenu  => new Submenu_Factory_Record,
         Ref_Item => "Coverage");
      --  Filter cannot be applied since we have additional items
      --  for this submenu in gnattest.py plugin which have its own filters
      --  with different conditions

      Register_Action
        (Kernel      => Kernel,
         Name        => "go to tested procedure",
         Command     => new Go_To_Tested_Command_Type,
         Filter      => Filter);
      Register_Contextual_Menu
        (Kernel      => Kernel,
         Name        => "Goto tested subprogram",
         Action      => "go to tested procedure",
         Label       => "GNATtest/Go to %C",
         Custom      => Tested_Subprogram_Name'Access,
         Ref_Item    => "GNATtest",
         Add_Before  => False);

      Kernel.Scripts.Register_Command
        ("is_harness_project",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Project_Class (Kernel.all'Access),
         Handler      => Command_Handler'Access);

      Kernel.Scripts.Register_Command
        ("original_project",
         Minimum_Args => 0,
         Maximum_Args => 0,
         Class        => Get_Project_Class (Kernel.all'Access),
         Handler      => Command_Handler'Access);

      Project_View_Changed_Hook.Add (new On_Project_Changed);
      Tests_MDI_Views.Register_Module (Kernel);
   end Register_Module;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Mapping_File;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);

      function To_Integer (Name : String) return Integer;
      function To_Time (Name : String) return Ada.Calendar.Time;
      function To_File (Name : String) return Virtual_File;

      function Get_Attribute (Name : String) return Unbounded_String;
      --  Return value of attribute with given Name or empty string if no such

      procedure Add_Setup_Teardown;

      procedure Add_Setup_Teardown is
      begin
         Self.Last_Source.Test_Case_Name := Test_Setup;
         Self.Source_Map.Include (Self.Last_Source, Self.Setup);

         Self.Last_Source.Test_Case_Name := Test_Teardown;
         Self.Source_Map.Include (Self.Last_Source, Self.Teardown);

         if Self.First_Tested then
            Self.Test_Map.Include (Self.Teardown.File_Name, Self.Last_Source);
            Self.First_Tested := False;
         end if;
      end Add_Setup_Teardown;

      function Get_Attribute (Name : String) return Unbounded_String is
         Index : constant Integer := Atts.Get_Index (Name);
      begin
         if Index < 0 then
            return Null_Unbounded_String;
         else
            return To_Unbounded_String (Atts.Get_Value (Index));
         end if;
      end Get_Attribute;

      function To_File (Name : String) return Virtual_File is
         Text : constant Filesystem_String :=
           Filesystem_String (Atts.Get_Value (Name));
         Dir  : constant Virtual_File :=
           Self.Kernel.Registry.Tree.Root_Project.Project_Path.Dir;
         File : constant Virtual_File :=
           Create_From_Dir (Dir, Filesystem_String (Text), True);
      begin
         if File.Base_Name = Text then
            --  if just base name is provided in XML - find it in project
            return GPS.Kernel.Create (Text, Self.Kernel);
         end if;

         --  othervise resolve it relative to directory of root project file
         return File;
      end To_File;

      function To_Integer (Name : String) return Integer is
      begin
         return Integer'Value (Atts.Get_Value (Name));
      end To_Integer;

      Null_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of
        (Year    => Ada.Calendar.Year_Number'First,
         Month   => Ada.Calendar.Month_Number'First,
         Day     => Ada.Calendar.Day_Number'First);

      function To_Time (Name : String) return Ada.Calendar.Time is
         Image : constant Unbounded_String := Get_Attribute (Name);
      begin
         if Image /= "" and Image /= "modified" then
            return GNAT.Calendar.Time_IO.Value (To_String (Image));
         end if;

         return Null_Time;
      end To_Time;

   begin
      if Local_Name = "tests_mapping" then
         if Get_Attribute ("mode") = "monolith" then
            Self.Mode := Monolith;
         else
            Self.Mode := Separates;
         end if;
      elsif Local_Name = "unit" then
         Self.Last_Source.Source_File := To_File ("source_file");

      elsif Local_Name = "test_unit" then
         Self.First_Tested := True;

      elsif Local_Name = "tested" then
         Self.Last_Source.Subprogram_Name := Get_Attribute ("name");
         Self.Last_Source.Line := To_Integer ("line");
         Self.Last_Source.Column := To_Integer ("column");
         Self.Last_Source.Test_Case_Name := To_Unbounded_String ("test case");
         Self.First_Test := True;

      elsif Local_Name = "test_case" then
         Self.Last_Source.Test_Case_Name := Get_Attribute ("name");

      elsif Local_Name = "setup" then
         Self.Setup.File_Name := To_File ("file");
         Self.Setup.Subprogram_Name := Get_Attribute ("name");
         Self.Setup.Line := To_Integer ("line");
         Self.Setup.Column := To_Integer ("column");
         Self.Setup.Stamp := Null_Time;

      elsif Local_Name = "teardown" then
         Self.Teardown.File_Name := To_File ("file");
         Self.Teardown.Subprogram_Name := Get_Attribute ("name");
         Self.Teardown.Line := To_Integer ("line");
         Self.Teardown.Column := To_Integer ("column");
         Self.Setup.Stamp := Null_Time;

      elsif Local_Name = "test" then
         declare
            Target : Test_Entity;
         begin
            Target.File_Name := To_File ("file");
            Target.Subprogram_Name := Get_Attribute ("name");
            Target.Line := To_Integer ("line");
            Target.Column := To_Integer ("column");
            Target.Stamp := To_Time ("timestamp");

            Self.Source_Map.Include (Self.Last_Source, Target);
            Self.Test_Map.Include (Target.File_Name, Self.Last_Source);

            if Self.First_Test then
               Add_Setup_Teardown;
               Self.First_Test := False;
            end if;
         end;
      elsif Local_Name = "stub_unit" then
         Self.Stub_Unit.Original := To_File ("Original_body_file");
         Self.Stub_Unit.Stub := To_File ("stub_body_file");
         Self.Stub_Unit.Setter := To_File ("setter_file");

      elsif Local_Name = "stubbed" then
         Self.Last_Source.Subprogram_Name := Get_Attribute ("name");
         Self.Last_Source.Line := To_Integer ("line");
         Self.Last_Source.Column := To_Integer ("column");
         Self.Last_Source.Test_Case_Name := To_Unbounded_String ("test case");

      elsif Local_Name in "stub_body" | "setter" then
         declare
            Target : Test_Entity;
         begin
            Target.Stamp := Null_Time;

            if Local_Name = "stub_body" then
               Target.Line := 1;
               Target.Column := 1;
               Target.File_Name := Self.Stub_Unit.Original;
               Self.Last_Source.Test_Case_Name :=
                 To_Unbounded_String ("original body");
               Self.Source_Map.Include (Self.Last_Source, Target);
               Self.Test_Map.Include (Target.File_Name, Self.Last_Source);

               Target.File_Name := Self.Stub_Unit.Stub;
               Self.Last_Source.Test_Case_Name :=
                 To_Unbounded_String ("stub body");
            else
               Target.File_Name := Self.Stub_Unit.Setter;
               Self.Last_Source.Test_Case_Name :=
                 To_Unbounded_String ("setter");
            end if;

            Target.Line := To_Integer ("line");
            Target.Column := To_Integer ("column");
            Self.Source_Map.Include (Self.Last_Source, Target);
            Self.Test_Map.Include (Target.File_Name, Self.Last_Source);

         end;
      end if;
   end Start_Element;

   --------------------------
   -- Test_Entity_Callback --
   --------------------------

   procedure Test_Entity_Callback
     (Widget    : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      User_Data : Menu_Data)
   is
      pragma Unreferenced (Widget);
   begin
      Open_File
        (User_Data.Kernel,
         GNATCOLL.Projects.No_Project,  --  will use any project
         User_Data.Entity.File_Name,
         User_Data.Entity.Line,
         Basic_Types.Visible_Column_Type (User_Data.Entity.Column),
         To_String (User_Data.Entity.Subprogram_Name));
   end Test_Entity_Callback;

   ----------------------------
   -- Tested_Subprogram_Name --
   ----------------------------

   function Tested_Subprogram_Name
     (Context : GPS.Kernel.Selection_Context)
     return String is

      Cursor : constant Test_Entity_Maps.Cursor :=
        Find_In_Map (File_Information (Context));
   begin
      if Test_Entity_Maps.Has_Element (Cursor) then
         return GPS.Kernel.Modules.UI.Emphasize
           (To_String (Test_Entity_Maps.Element (Cursor).Subprogram_Name));
      else
         return "";
      end if;
   end Tested_Subprogram_Name;

end GNATTest_Module;
