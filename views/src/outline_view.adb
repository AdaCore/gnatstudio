------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2024, AdaCore                     --
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

with Ada.Strings;
with Ada.Strings.Fixed;

with Glib;                       use Glib;
with Glib.Object;                use Glib.Object;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gdk.Event;                  use Gdk.Event;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.Types.Keysyms;          use Gdk.Types, Gdk.Types.Keysyms;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu;
with Gtk.Separator_Menu_Item;    use Gtk.Separator_Menu_Item;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Toolbar;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;

with Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Tree_View;           use Gtkada.Tree_View;

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with VSS.Strings.Conversions;

with Commands;
with Commands.Interactive;       use Commands.Interactive;
with Default_Preferences;        use Default_Preferences;
with Filter_Panels;              use Filter_Panels;
with Generic_Views;              use Generic_Views;
with GUI_Utils;                  use GUI_Utils;
with Histories;                  use Histories;
with Language_Handlers;          use Language_Handlers;
with Language.Icons;             use Language.Icons;
with Tooltips;                   use Tooltips;
with Unchecked_Deallocation;
with XML_Utils;

with GPS.Editors;                use GPS.Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Search;                 use GPS.Search;

------------------
-- Outline_View --
------------------

package body Outline_View is

   Me : constant Trace_Handle := Create ("OUTLINE_VIEW_DEBUG", Off);

   Outline_View_Class_Name : constant String := "OutlineView";
   pragma Unreferenced (Outline_View_Class_Name);

   type Outline_View_Module_Record is new Module_ID_Record with record
      Tooltip_Factory      : Outline_Tooltip_Factory_Type;
      LSP_Provider         : Outline_Provider_Access;
      --  LSP provider: can be null
      Default_Provider     : Outline_Provider_Access;
      --  Default provider: can be null
      Synchronous_Tooltips : Boolean := True;
   end record;
   type Outline_View_Module_Access is
     access all Outline_View_Module_Record'Class;

   Outline_View_Module : Outline_View_Module_Access := null;
   Outline_View_Module_Name : constant String := "Outline_View";

   Show_Profile      : Boolean_Preference;
   Sort_Alphabetical : Boolean_Preference;
   Sort_Category     : Boolean_Preference;
   Editor_Link       : Boolean_Preference;
   Show_Decls        : Boolean_Preference;
   Show_Types        : Boolean_Preference;
   Show_Fields       : Boolean_Preference;
   Show_Tasks        : Boolean_Preference;
   Show_Objects      : Boolean_Preference;
   Show_With         : Boolean_Preference;
   Show_Pragmas      : Boolean_Preference;
   Flat_View         : Boolean_Preference;
   Group_By_Category : Boolean_Preference;

   --  User defined preference values, used as a cache

   Icon_Column         : constant := 0;
   --  icon representing the entity type
   Name_Column         : constant := 1;
   --  defining name + profile

   --  All the columns below should be hidden
   Start_Line_Column   : constant := 2;
   --  line containing the defining name
   Start_Col_Column    : constant := 3;
   --  column of the defining name

   Def_End_Line_Column : constant := 4;
   --  defining name end symbol line
   Def_End_Col_Column  : constant := 5;
   --  defining name end symbol column

   End_Line_Column     : constant := 6;
   --  end of the block
   Category_Column     : constant := 7;
   --  integer representing the weight of the category

   Id_Column           : constant := 8;
   --  Id defined by QGEN plugin, can be refered by the python API

   type Outline_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Outline_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;
   --  See inherited documentation

   type On_Context_Changed is new Context_Hooks_Function with null record;
   overriding procedure Execute
     (Self    : On_Context_Changed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context);
   --  Called when the context has changed

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  React to changes in the preferences

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Project_Changed;
       Kernel : not null access Kernel_Handle_Record'Class);

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been closed

   type On_Buffer_Modified is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Buffer_Modified;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when the buffer has been modified and the user has stopped
   --  editing it.

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been opened

   type On_Location_Changed is new File_Location_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type);
   --  Called when the current editor reaches a new location

   type Outline_Tree_Record is new Tree_View_Record with record
      Filter : GPS.Search.Search_Pattern_Access := null;
   end record;
   type Outline_Tree_View is access all Outline_Tree_Record'Class;
   overriding function Is_Visible
     (Self : not null access Outline_Tree_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean;

   type Outline_View_Record is new Generic_Views.View_Record with record
      Tree       : Gtkada.Tree_View.Tree_View;
      File       : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Prev_File  : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Filter     : Tree_Filter;
   end record;
   overriding procedure Create_Toolbar
     (View    : not null access Outline_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Outline_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Outline_View_Record;
      Pattern : in out Search_Pattern_Access);

   function Initialize
     (Outline : access Outline_View_Record'Class)
     return Gtk.Widget.Gtk_Widget;
   --  Create a new outline view, and return the focus widget.

   package Outline_Views is new Generic_Views.Simple_Views
     (Module_Name        => Outline_View_Module_Name,
      View_Name          => "Outline",
      Formal_View_Record => Outline_View_Record,
      Formal_MDI_Child   => Outline_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Position           => Position_Left,
      Initialize         => Initialize);
   use Outline_Views;
   subtype Outline_View_Access is Outline_Views.View_Access;

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);

   procedure Refresh
     (View     : access Gtk_Widget_Record'Class;
      Only_LSP : Boolean := False);
   --  Ask the provider to refresh the model.
   --  If Only_LSP, only try to refresh the model using the LSP_Provider:
   --  do nothing if the LSP_Provider doesn't support the current context.

   Sort_Entities : constant array (Language_Category) of Natural :=
     (Dependency_Category => 1,
      Namespace_Category  => 2,
      Type_Category       => 3,
      Subprogram_Category => 4,
      Data_Category       => 5,
      others              => 99);
   --  This array provide a way of sorting / grouping entities when order
   --  is required.

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Insertion sort

   procedure On_Destroy
     (Self : access Gtk_Widget_Record'Class);
   --  Called when the outline is destroyed

   procedure On_Changed
     (Outline : not null Outline_View_Access;
      Context : Selection_Context);
   --  Update outline with given Context.

   function On_Key_Press
     (Outline : access GObject_Record'Class;
      Event   : Gdk_Event_Key) return Boolean;
   --  Handle key events in the outline

   function On_Button_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean;
   --  Handle mouse click in the outline

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Force  : Boolean := False);
   --  Recompute the Outline state using the cursor position in File.
   --  Use Force to bypass the preferences.

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Integer;
      Force  : Boolean := False);
   --  Select the Outline's node corresponding editor context

   function Get_Enclosing_Path
     (Model  : Gtk_Tree_Model;
      Line   : Natural;
      Column : Integer)
      return Gtk_Tree_Path;
   --  Find the nearest path enclosing Line and Column

   procedure Goto_Selected (Outline : access Outline_View_Record'Class);
   --  Place the cursor in the editor using the Outline

   procedure Refresh_Filter (Outline : access Outline_View_Record'Class);

   function Is_Visible
     (Filter         : Tree_Filter;
      Category       : Language_Category;
      Is_Declaration : Boolean)
      return Boolean;

   procedure Clear (Outline : access Outline_View_Record'Class);

   procedure Stop_Providers;
   procedure Start_Provider
     (Kernel   : Kernel_Handle;
      File     : Virtual_File;
      Only_LSP : Boolean := False);

   ----------------------
   -- Profile Encoding --
   ----------------------

   Span_Header : constant String := " <span foreground=""#A0A0A0"">";
   Span_End    : constant String := "</span>";
   function Encode_Name (Name : String; Profile : String) return String;
   function Decode_Name (S : String) return String;
   function Decode_Profile (S : String) return String;

   --------------
   -- Tooltips --
   --------------

   type Outline_View_Tooltip_Handler is new Tooltips.Tooltip_Handler with
   record
      Outline : Outline_View_Access;
   end record;
   type Outline_View_Tooltip_Handler_Access is
     access all Outline_View_Tooltip_Handler'Class;
   overriding function Create_Contents
     (Tooltip : not null access Outline_View_Tooltip_Handler;
      Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y    : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   overriding function Show_Tooltip_On_Create_Contents
     (Tooltip : not null access Outline_View_Tooltip_Handler) return Boolean
   is
     (Outline_View_Module.Synchronous_Tooltips);

   -------------
   -- Actions --
   -------------

   type Outline_Collapse_Or_Expand_Command
     (Is_Expand : Boolean) is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Outline_Collapse_Or_Expand_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Collapse or Expand all the rows

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip : not null access Outline_View_Tooltip_Handler;
      Widget  : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y    : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Area  : Gdk_Rectangle;

      Line   : Integer;
      Column : Visible_Column;
   begin
      Initialize_Tooltips (Tooltip.Outline.Tree, X, Y, Area, Iter);

      if Iter /= Null_Iter then
         Model := Tooltip.Outline.Tree.Get_Model;

         if Get_Int (Model, Iter, Start_Line_Column) = 0 then
            --  This happen when hovering on Category name
            return null;
         end if;

         Tooltip.Set_Tip_Area (Area);
         declare
            Name : constant String := Decode_Name
              (Get_String (Model, Iter, Name_Column));
         begin
            Line   := Integer (Get_Int (Model, Iter, Start_Line_Column));
            Column := Visible_Column
              (Get_Int (Model, Iter, Start_Col_Column));

            if Ada.Strings.Fixed.Index (Name, ".") in Name'Range then
               --  Dotted notation, getting the corresponding buffer
               declare
                  Buf          : constant Editor_Buffer'Class :=
                    Tooltip.Outline.Kernel.Get_Buffer_Factory.Get
                      (Tooltip.Outline.File,
                       Open_View => False, Focus => False);
                  Def_End_Line : constant Integer := Integer
                    (Get_Int (Model, Iter, Def_End_Line_Column));
                  Def_End_Col  : constant Visible_Column := Visible_Column
                    (Get_Int (Model, Iter, Def_End_Col_Column) - 1);
               begin
                  --  use the position of the last symbol in the name if
                  --  we did not get the buffer

                  if Buf /= Nil_Editor_Buffer
                    and then Def_End_Line > -1
                    and then Def_End_Col > -1
                  then
                     --  Got the buffer, find the starting position of the
                     --  last word in dotted notation
                     declare
                        Loc : constant Editor_Location'Class :=
                          Buf.New_Location (Def_End_Line, Def_End_Col).
                          Backward_To_Word_Start;
                     begin
                        Line   := Loc.Line;
                        Column := Loc.Column;
                     end;
                  end if;
               end;
            end if;

            return Outline_View_Module.Tooltip_Factory
              (Tooltip.Outline.Kernel,
               Tooltip.Outline.File, Name, Line, Column);
         end;
      end if;
      return null;
   end Create_Contents;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Outline_Collapse_Or_Expand_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Outline : constant Outline_View_Access
        := Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null then
         if Self.Is_Expand then
            Outline.Tree.Expand_All;
         else
            Outline.Tree.Collapse_All;
         end if;
      end if;

      return Commands.Success;
   end Execute;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Outline_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Outline : constant Outline_View_Access :=
        Outline_View_Access (GPS_MDI_Child (Self).Get_Actual_Widget);
      Context : Selection_Context;
      Iter    : Gtk_Tree_Iter;
      Model   : constant Gtk_Tree_Model := Outline.Tree.Get_Model;
      Line    : Integer := 1;
      Column  : Visible_Column_Type := 1;
      Path    : Gtk_Tree_Path;
   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);

      Iter := Find_Iter_For_Event (Outline.Tree, Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         if not Path_Is_Selected (Get_Selection (Outline.Tree), Path) then
            Set_Cursor (Outline.Tree, Path, null, False);
         end if;
         Path_Free (Path);

         Line := Integer (Get_Int (Model, Iter, Start_Line_Column));
         Column :=
           Visible_Column_Type (Get_Int (Model, Iter, Start_Col_Column));

         Set_Entity_Information
           (Context       => Context,
            Entity_Name   =>
              Decode_Name (Get_String (Model, Iter, Name_Column)),
            Entity_Line   => Editable_Line_Type (Line),
            Entity_Column => Column);
      end if;

      Set_File_Information
        (Context => Context,
         Project => No_Project,
         Files   => (1 => Outline.File),
         Line    => Line,
         Column  => Column);

      return Context;
   end Build_Context;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_Context_Changed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context)
   is
      pragma Unreferenced (Self);
      Module  : constant Module_ID := Module_ID (Get_Creator (Context));
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null

        --  We disable the Outline view for some specific modules (instead
        --  of allowing for a specific set of modules, because the user could
        --  create his own like we do for QGen).
        and then Module /= null
        and then Get_Name (Module) /= "Location_View_Record"
      then
         On_Changed (Outline, Context);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Pref = null then
         return;
      end if;

      --  Group_By_Category and Flat_View are incompatible
      if Pref = Preference (Group_By_Category)
        and then Group_By_Category.Get_Pref
        and then Flat_View.Get_Pref
      then
         Set_Pref (Flat_View, Kernel.Get_Preferences, "false");

      elsif Pref = Preference (Flat_View)
        and then Flat_View.Get_Pref
        and then Group_By_Category.Get_Pref
      then
         Set_Pref (Group_By_Category, Kernel.Get_Preferences, "false");
      end if;

      if Outline /= null then
         Set_Font_And_Colors (Outline.Tree, Fixed_Font => True, Pref => Pref);

         if Pref = Preference (Show_Profile)
           or else Pref = Preference (Sort_Alphabetical)
           or else Pref = Preference (Sort_Category)
           or else Pref = Preference (Editor_Link)
           or else Pref = Preference (Show_Decls)
           or else Pref = Preference (Show_Types)
           or else Pref = Preference (Show_Tasks)
           or else Pref = Preference (Show_Objects)
           or else Pref = Preference (Show_Fields)
           or else Pref = Preference (Show_With)
           or else Pref = Preference (Show_Pragmas)
           or else Pref = Preference (Flat_View)
           or else Pref = Preference (Group_By_Category)
         then
            Refresh_Filter (Outline);
            Refresh (Outline);
         end if;
      end if;
   end Execute;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Self : access Gtk_Widget_Record'Class)
   is
      Outline : constant Outline_View_Access := Outline_View_Access (Self);
   begin
      if Outline /= null then
         Stop_Providers;
      end if;
   end On_Destroy;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
     (Outline : not null Outline_View_Access;
      Context : Selection_Context)
   is
      File : Virtual_File;
   begin
      if Has_File_Information (Context) then
         File := File_Information (Context);
      elsif Outline.File = No_File then
         --  Fallback to last used editor
         File := Get_Kernel (Context).Get_Buffer_Factory
           .Get (Open_View => False).File;
      end if;

      if File /= No_File and then File /= Outline.File then
         Outline.Prev_File := Outline.File;
         Outline.File := File;
         Refresh (Outline);
      end if;
   end On_Changed;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Outline : access GObject_Record'Class;
      Event   : Gdk_Event_Key) return Boolean
   is
      View : constant Outline_View_Access := Outline_View_Access (Outline);
   begin
      if Event.Keyval = GDK_Return then
         Goto_Selected (View);
         return True;
      end if;

      return False;
   end On_Key_Press;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Self  : access GObject_Record'Class;
      Event : Gdk.Event.Gdk_Event_Button) return Boolean
   is
      View    : constant Outline_View_Access := Outline_View_Access (Self);
      Cell_X  : Gint;
      Cell_Y  : Gint;
      Column  : Gtk_Tree_View_Column;
      Success : Boolean;
      Path    : Gtk_Tree_Path;
      Area    : Gdk_Rectangle;

      function Jump_If_New_Node return Boolean;
      --  Jump if the clicked node is different to the selected node
      --  or if the user has double-clicked.

      ----------------------
      -- Jump_If_New_Node --
      ----------------------

      function Jump_If_New_Node return Boolean is
         Iter       : Gtk_Tree_Iter;
         Model      : Gtk_Tree_Model;
         Store_Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      begin
         View.Tree.Get_Selection.Get_Selected (Model, Iter);
         if Iter /= Null_Iter then
            Store_Path := Get_Path (Model, Iter);
         end if;

         if Event.The_Type = Gdk_2button_Press
           or else Store_Path = Null_Gtk_Tree_Path
           or else To_String (Store_Path) /= To_String (Path)
         then
            View.Tree.Get_Selection.Select_Path (Path);
            Goto_Selected (View);
            Path_Free (Store_Path);
            Path_Free (Path);
            return True;
         else
            return False;
         end if;
      end Jump_If_New_Node;

   begin
      if Event.Button = 1
        and then
          (Event.The_Type = Button_Press
           or else Event.The_Type = Gdk_2button_Press)
      then
         View.Tree.Get_Path_At_Pos
           (Gint (Event.X), Gint (Event.Y), Path,
            Column, Cell_X, Cell_Y, Success);

         if Success then
            --  Get the area of the column
            View.Tree.Get_Cell_Area (Path, Column, Area);

            if Cell_X < Area.X then
               --  Do nothing if we clicked in the area before the column,
               --  it's needed to be able to expand.
               null;
            elsif Path /= Null_Gtk_Tree_Path then
               return Jump_If_New_Node;
            end if;
         end if;
      end if;
      return False;
   end On_Button_Press;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      Res : Gint;

      function Compare_Name (Col : Gint) return Gint;
      function Compare_Value (Col : Gint) return Gint;

      ------------------
      -- Compare_Name --
      ------------------

      function Compare_Name (Col : Gint) return Gint
      is
         S_A    : constant String := Get_String (Model, A, Col);
         S_B    : constant String := Get_String (Model, B, Col);
         Name_A : constant String := Decode_Name (S_A);
         Name_B : constant String := Decode_Name (S_B);
      begin
         if Name_A < Name_B then
            return -1;

         elsif Name_A > Name_B then
            return 1;
         else
            declare
               Profile_A : constant String := Decode_Profile (S_A);
               Profile_B : constant String := Decode_Profile (S_B);
            begin
               if Profile_A < Profile_B then
                  return -1;
               elsif Profile_A > Profile_B then
                  return 1;
               else
                  return 0;
               end if;
            end;
         end if;
      end Compare_Name;

      -------------------
      -- Compare_Value --
      -------------------

      function Compare_Value (Col : Gint) return Gint
      is
         Val_A : constant Gint := Get_Int (Model, A, Col);
         Val_B : constant Gint := Get_Int (Model, B, Col);
      begin
         if Val_A < Val_B then
            return -1;
         elsif Val_A > Val_B then
            return 1;
         else
            return 0;
         end if;
      end Compare_Value;

   begin
      if Sort_Category.Get_Pref then
         Res := Compare_Value (Category_Column);
         if Res /= 0 then
            return Res;
         end if;
      end if;

      if Sort_Alphabetical.Get_Pref then
         Res := Compare_Name (Name_Column);
         if Res /= 0 then
            return Res;
         end if;
      end if;

      Res := Compare_Value (Start_Line_Column);
      if Res /= 0 then
         return Res;
      end if;

      return Compare_Value (Start_Col_Column);
   end Sort_Func;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Force  : Boolean := False)
   is
      Ed : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (File, Open_View => False, Focus => False);
   begin
      if Ed /= Nil_Editor_Buffer then
         declare
            Mark : constant Editor_Mark'Class :=
              Ed.Get_Main_Cursor.Get_Insert_Mark;
         begin
            Location_Changed
              (Kernel => Kernel,
               File   => File,
               Line   => Natural (Mark.Line),
               Column => Integer (Mark.Column),
               Force  => Force);
         end;
      else
         --  The context is not coming from an editor => expand the first node
         Location_Changed
           (Kernel => Kernel,
            File   => File,
            Line   => 1,
            Column => 1,
            Force  => Force);
      end if;
   end Location_Changed;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Column : Integer;
      Force  : Boolean := False)
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
      Model   : Gtk_Tree_Model;
      Path    : Gtk_Tree_Path;
      Dummy   : Boolean;
   begin
      if (Force or else Editor_Link.Get_Pref)
        and then Outline /= null
        and then (File = No_File or else Outline.File = File)
      then
         Model := Outline.Tree.Get_Model;
         --  Do nothing if the model is empty
         if Model /= Null_Gtk_Tree_Model then
            Unselect_All (Get_Selection (Outline.Tree));
            Path := Get_Enclosing_Path (Model, Line, Column);

            if Path /= Null_Gtk_Tree_Path then
               if Get_Depth (Path) = 1 then
                  --  This is a root node: make its children visible
                  Dummy :=
                    Expand_Row (Outline.Tree, Path, Open_All => False);
               else
                  declare
                     Parent_Iter : constant Gtk_Tree_Iter :=
                       Parent (Model, Get_Iter (Model, Path));
                     Parent_Path : constant Gtk_Tree_Path :=
                       Get_Path (Model, Parent_Iter);
                  begin
                     --  Make the path visible
                     Expand_To_Path (Outline.Tree, Parent_Path);
                     Path_Free (Parent_Path);
                  end;
               end if;

               if Editor_Link.Get_Pref then
                  Outline.Tree.Get_Selection.Select_Path (Path);
                  Outline.Tree.Scroll_To_Cell (Path, null, False, 0.0, 0.0);
               end if;
               Path_Free (Path);
            end if;
         end if;
      end if;
   end Location_Changed;

   ------------------------
   -- Get_Enclosing_Path --
   ------------------------

   function Get_Enclosing_Path
     (Model  : Gtk_Tree_Model;
      Line   : Natural;
      Column : Integer)
      return Gtk_Tree_Path
   is
      function Safe_Get_Path (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path;

      function "<=" (A, B : Gtk_Tree_Iter) return Boolean;

      function Tree_Search (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path;
      --  Optimized search for Outline in tree mode

      function Flat_Search (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path;
      --  Search for Outline in flat mode. Is smarter if the Outline is sorted
      --  by line position.

      function Group_Search (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path;
      --  Search for Outline in grouped mode: for each category get the
      --  nearest enclosing node.

      -------------------
      -- Safe_Get_Path --
      -------------------

      function Safe_Get_Path (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path is
      begin
         if Iter /= Null_Iter then
            return Get_Path (Model, Iter);
         else
            return Null_Gtk_Tree_Path;
         end if;
      end Safe_Get_Path;

      ----------
      -- "<=" --
      ----------

      function "<=" (A, B : Gtk_Tree_Iter) return Boolean
      is
         L_A : constant Gint := Get_Int (Model, A, Start_Line_Column);
         L_B : constant Gint := Get_Int (Model, B, Start_Line_Column);
      begin
         if L_A = L_B then
            declare
               C_A : constant Gint := Get_Int (Model, A, Start_Col_Column);
               C_B : constant Gint := Get_Int (Model, B, Start_Col_Column);
            begin
               --  Multiple entities in the same line: choose the entity
               --  depending on the current Column
               if C_A < C_B then
                  return Gint (Column) >= C_B;
               else
                  return Gint (Column) >= C_A;
               end if;
            end;
         else
            return L_A < L_B;
         end if;
      end "<=";

      -----------------
      -- Tree_Search --
      -----------------

      function Tree_Search (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path
      is
         Cur : Gtk_Tree_Iter := Iter;
      begin
         while Cur /= Null_Iter loop
            declare
               Start_Line : constant Natural :=
                 Natural (Get_Int (Model, Cur, Start_Line_Column));
               End_Line   : constant Natural :=
                 Natural (Get_Int (Model, Cur, End_Line_Column));
               Child_Path : Gtk_Tree_Path;
            begin
               if Start_Line <= Line and then Line <= End_Line then
                  --  Check the entity category with a special handling for
                  --  data entities
                  if Natural (Get_Int (Model, Cur, Category_Column)) /=
                    Sort_Entities (Data_Category'First)
                  then
                     --  Normal entity => look at the children
                     Child_Path := Tree_Search (Children (Model, Cur));
                  else
                     --  Data entity needs to check for multiple
                     --  declarations in the same line => look at the brothers
                     Child_Path := Flat_Search (Cur);
                  end if;

                  if Child_Path /= Null_Gtk_Tree_Path then
                     --  Return the nearest child
                     return Child_Path;
                  else
                     --  None of the children are nearest to Line
                     return Get_Path (Model, Cur);
                  end if;
               end if;
            end;
            Next (Model, Cur);
         end loop;

         return Null_Gtk_Tree_Path;
      end Tree_Search;

      -----------------
      -- Flat_Search --
      -----------------

      function Flat_Search (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path
      is
         Cur          : Gtk_Tree_Iter := Iter;
         Nearest_Iter : Gtk_Tree_Iter := Null_Iter;
         Smart        : constant Boolean :=
           not Sort_Alphabetical.Get_Pref and not Sort_Category.Get_Pref;
      begin
         while Cur /= Null_Iter loop
            if Natural (Get_Int (Model, Cur, Start_Line_Column)) <= Line
              and then Line <= Natural (Get_Int (Model, Cur, End_Line_Column))
            then
               if Nearest_Iter = Null_Iter or else Nearest_Iter <= Cur then
                  Nearest_Iter := Cur;
               end if;
            end if;

            exit when Smart
              and then
                Natural (Get_Int (Model, Cur, Start_Line_Column)) > Line;

            Next (Model, Cur);
         end loop;

         return Safe_Get_Path (Nearest_Iter);
      end Flat_Search;

      ------------------
      -- Group_Search --
      ------------------

      function Group_Search (Iter : Gtk_Tree_Iter) return Gtk_Tree_Path
      is
         Cur          : Gtk_Tree_Iter := Iter;
         Nearest_Iter : Gtk_Tree_Iter := Null_Iter;
      begin
         while Cur /= Null_Iter loop
            declare
               Child_Path : constant Gtk_Tree_Path :=
                 Flat_Search (Children (Model, Cur));
               Child_Iter : Gtk_Tree_Iter;
            begin
               if Child_Path /= Null_Gtk_Tree_Path then
                  Child_Iter := Get_Iter (Model, Child_Path);
                  Path_Free (Child_Path);
                  if Nearest_Iter = Null_Iter
                    or else Nearest_Iter <= Child_Iter
                  then
                     Nearest_Iter := Child_Iter;
                  end if;
               end if;
            end;
            Next (Model, Cur);
         end loop;

         return Safe_Get_Path (Nearest_Iter);
      end Group_Search;

      Iter : constant Gtk_Tree_Iter := Get_Iter_First (Model);
   begin
      if Flat_View.Get_Pref then
         return Flat_Search (Iter);
      elsif Group_By_Category.Get_Pref then
         return Group_Search (Iter);
      else
         declare
            Path : constant Gtk_Tree_Path := Tree_Search (Iter);
         begin
            if Path /= Null_Gtk_Tree_Path then
               return Path;
            else
               --  By default return the first node,
               --  thus we will always expand the package node.
               return Safe_Get_Path (Iter);
            end if;
         end;
      end if;
   end Get_Enclosing_Path;

   -------------------
   -- Goto_Selected --
   -------------------

   procedure Goto_Selected (Outline : access Outline_View_Record'Class)
   is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      Outline.Tree.Get_Selection.Get_Selected (Model, Iter);
      if Iter /= Null_Iter
        and then Get_Int (Model, Iter, Start_Line_Column) /= -1
      then
         declare
            Start_Line   : constant Integer :=
              Integer (Get_Int (Model, Iter, Start_Line_Column));
            Start_Column : constant Visible_Column :=
              Visible_Column (Get_Int (Model, Iter, Start_Col_Column));
            End_Line     : constant Integer :=
              Integer (Get_Int (Model, Iter, Def_End_Line_Column));
            End_Column   : constant Visible_Column :=
              Visible_Column (Get_Int (Model, Iter, Def_End_Col_Column));
            Name         : constant String :=
              Decode_Name (Get_String (Model, Iter, Name_Column));
            Unique_ID    : constant String :=
              Get_String (Model, Iter, Id_Column);
            Lang         : constant Language_Access := Get_Language_From_File
              (Outline.Kernel.Get_Language_Handler, Outline.File);
         begin

            --  Does the language have a special handling for constructs ?
            if Lang.Clicked_On_Construct
              (File      => Outline.File,
               Unique_ID => Unique_ID,
               Name      => Name,
               Start_Loc => Language.Sloc_T'
                 (Line   => Start_Line,
                  Column => Start_Column,
                  Index  => 0))
            then
               return;
            end if;

            --  If not, just go to the editor location corresponding to the
            --  selected node.
            declare
               Buffer       : constant Editor_Buffer'Class :=
                 Get (Get_Buffer_Factory (Outline.Kernel).all,
                      Outline.File, Open_View => True);
               Editor       : constant Editor_View'Class :=
                 Current_View (Buffer);
               Start_Loc    : constant Editor_Location'Class :=
                 New_Location (Buffer, Start_Line, Start_Column);
               End_Loc      : constant Editor_Location'Class :=
                 (if End_Line > -1 and then End_Column > -1
                  then New_Location (Buffer, End_Line, End_Column)
                  else New_Location
                    (Buffer, Start_Line, Start_Column + Name'Length));
            begin
               Editor.Cursor_Goto (Start_Loc, Raise_View => True);
               Select_Text (Buffer, Start_Loc, End_Loc);
            end;
         end;
      end if;
   end Goto_Selected;

   --------------------
   -- Refresh_Filter --
   --------------------

   procedure Refresh_Filter (Outline : access Outline_View_Record'Class) is
   begin
      Outline.Filter.Show_Profile      := Show_Profile.Get_Pref;
      Outline.Filter.Sort_Alphabetical := Sort_Alphabetical.Get_Pref;
      Outline.Filter.Sort_Category     := Sort_Category.Get_Pref;
      Outline.Filter.Editor_Link       := Editor_Link.Get_Pref;
      Outline.Filter.Show_Decls        := Show_Decls.Get_Pref;
      Outline.Filter.Show_Types        := Show_Types.Get_Pref;
      Outline.Filter.Show_Fields       := Show_Fields.Get_Pref;
      Outline.Filter.Show_Tasks        := Show_Tasks.Get_Pref;
      Outline.Filter.Show_Objects      := Show_Objects.Get_Pref;
      Outline.Filter.Show_With         := Show_With.Get_Pref;
      Outline.Filter.Show_Pragmas      := Show_Pragmas.Get_Pref;
      Outline.Filter.Flat_View         := Flat_View.Get_Pref;
      Outline.Filter.Group_By_Category := Group_By_Category.Get_Pref;
   end Refresh_Filter;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Self : not null access Gtkada.Tree_View.Tree_View_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return String is
   begin
      if Iter /= Null_Iter then
         declare
            Name : constant String :=
              Self.Model.Get_String (Iter, Name_Column);
            Line : constant Integer :=
              Integer (Self.Model.Get_Int (Iter, Start_Line_Column));
         begin
            return Name & ":" & Integer'Image (Line);
         end;
      else
         return "";
      end if;
   end Get_Id;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Project_Changed;
       Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Outline.File /= No_File then
         Refresh (Outline);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Outline.File = File then
         --  The file being shown was closed => clear the Outline
         Outline.Prev_File := Outline.File;
         Outline.File := No_File;
         Refresh (Outline);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Buffer_Modified;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Outline.File = File then
         --  The LSP_Provider reacts on buffer modified while
         --  the Default_Provider reacts on Semantic_Tree_Updated_Hook
         Refresh (Outline, Only_LSP => True);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null then
         if Outline.File /= File then
            Outline.Prev_File := Outline.File;
            Outline.File := File;
         end if;
         Refresh (Outline);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type)
   is
      pragma Unreferenced (Self, Project);
   begin
      Location_Changed (Kernel, File, Line, Column);
   end Execute;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Outline_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "outline",
         Tooltip     => -"Filter the contents of the outline view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy,
         Name        => "Outline_Filter");
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Outline_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K        : constant Kernel_Handle := View.Kernel;
      Sep      : Gtk_Separator_Menu_Item;
   begin
      Append_Menu (Menu, K, Show_Profile);

      Append_Menu (Menu, K, Show_Types);
      Append_Menu (Menu, K, Show_Objects);
      Append_Menu (Menu, K, Show_Fields);
      Append_Menu (Menu, K, Show_Tasks);
      Append_Menu (Menu, K, Show_Decls);
      Append_Menu (Menu, K, Show_With);
      Append_Menu (Menu, K, Show_Pragmas);

      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, K, Sort_Alphabetical);
      Append_Menu (Menu, K, Sort_Category);
      Append_Menu (Menu, K, Flat_View);
      Append_Menu (Menu, K, Group_By_Category);

      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, K, Editor_Link);
   end Create_Menu;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Outline : access Outline_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Scrolled    : Gtk_Scrolled_Window;
      Tree_Column : Gtk_Tree_View_Column;
      Text_Render : Gtk_Cell_Renderer_Text;
      Icon_Pixbuf : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;
      Tooltip     : Outline_View_Tooltip_Handler_Access;
   begin
      Initialize_Vbox (Outline);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Outline.Pack_Start (Scrolled, Expand => True, Fill => True);

      --  Create the tree view using the sorting model

      Outline.Tree := new Outline_Tree_Record;
      Initialize
        (Outline.Tree,
         Column_Types    => (Icon_Column         => GType_String,
                             Name_Column         => GType_String,
                             Start_Line_Column   => GType_Int,
                             Start_Col_Column    => GType_Int,
                             Def_End_Line_Column => GType_Int,
                             Def_End_Col_Column  => GType_Int,
                             End_Line_Column     => GType_Int,
                             Category_Column     => GType_Int,
                             Id_Column           => GType_String),
         Capability_Type  => Filtered_And_Sortable,
         Set_Visible_Func => True);

      --  Add the Icon
      Gtk_New (Tree_Column);
      Gtk_New (Icon_Pixbuf);
      Tree_Column.Pack_Start (Icon_Pixbuf, False);
      Tree_Column.Add_Attribute (Icon_Pixbuf, "icon-name", Icon_Column);
      --  Add the description
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, True);
      Tree_Column.Set_Sort_Column_Id (Icon_Column);
      Tree_Column.Add_Attribute (Text_Render, "markup", Name_Column);
      Dummy := Outline.Tree.Append_Column (Tree_Column);

      --  The following columns should always be invisible
      Gtk_New (Tree_Column);
      --  Add Start_Line
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", Start_Line_Column);
      --  Add Start_Column
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", Start_Col_Column);
      --  Add Def_End_Line_Column
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", Def_End_Line_Column);
      --  Add Def_End_Col_Column
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", Def_End_Col_Column);
      --  Add End_Line
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", End_Line_Column);
      --  Add Category weight
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", Category_Column);
      --  Add QGEN ID
      Gtk_New (Text_Render);
      Tree_Column.Pack_Start (Text_Render, False);
      Tree_Column.Add_Attribute (Text_Render, "text", Id_Column);
      Tree_Column.Set_Visible (False);
      Dummy := Outline.Tree.Append_Column (Tree_Column);

      Set_Name (Outline.Tree, "Outline View Tree");  --  For testsuite
      Outline.Tree.Set_Headers_Visible (False);
      Outline.Tree.Set_Search_Column (Name_Column);
      Outline.Tree.Model.Set_Sort_Func (Name_Column, Sort_Func'Access);
      Outline.Tree.Model.Set_Sort_Column_Id (Name_Column, Sort_Ascending);
      Scrolled.Add (Outline.Tree);
      Set_Font_And_Colors (Outline.Tree, Fixed_Font => True);

      Setup_Contextual_Menu
        (Kernel          => Outline.Kernel,
         Event_On_Widget => Outline.Tree);

      Tooltip := new Outline_View_Tooltip_Handler;
      Tooltip.Outline := Outline_View_Access (Outline);
      Associate_To_Widget (Tooltip, Outline.Tree);

      Outline.Tree.On_Button_Press_Event
        (On_Button_Press'Access, Slot => Outline);
      Outline.Tree.On_Key_Press_Event
        (On_Key_Press'Access, Slot => Outline);

      Gtkada.Handlers.Widget_Callback.Connect
        (Outline, Signal_Destroy, On_Destroy'Access);
      Context_Changed_Hook.Add_Debounce
        (Obj => new On_Context_Changed, Watch => Outline);

      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => Outline);
      Location_Changed_Hook.Add_Debounce
        (new On_Location_Changed, Watch => Outline);
      File_Closed_Hook.Add (new On_File_Closed, Watch => Outline);
      Buffer_Edited_Hook.Add (new On_Buffer_Modified, Watch => Outline);
      File_Edited_Hook.Add (new On_File_Edited, Watch => Outline);
      Project_View_Changed_Hook.Add (new On_Project_Changed, Watch => Outline);

      Refresh_Filter (Outline);
      Outline.File := No_File;
      return Gtk_Widget (Outline.Tree);
   end Initialize;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self : not null access Outline_Tree_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Boolean is
   begin
      return
        Iter = Null_Iter
        or else Self.Filter = null
        or else
          Self.Filter.Start
            (Decode_Name
               (Self.Model.Get_String (Iter, Name_Column))) /= No_Match;
   end Is_Visible;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Outline_View_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      GPS.Search.Free (Outline_Tree_View (Self.Tree).Filter);
      Outline_Tree_View (Self.Tree).Filter := Pattern;
      Self.Tree.Refilter;
   end Filter_Changed;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (View     : access Gtk_Widget_Record'Class;
      Only_LSP : Boolean := False)
   is
      Outline : constant Outline_View_Access := Outline_View_Access (View);
   begin
      if Outline /= null then
         Stop_Providers;
         if Outline.File /= Outline.Prev_File then
            Clear (Outline);
            Outline.Prev_File := Outline.File;
         end if;
         if Outline.File /= No_File then
            Generic_Views.Abstract_View_Access
              (Outline).Set_Activity_Progress_Bar_Visibility (True);
         end if;
         Start_Provider (Outline.Kernel, Outline.File, Only_LSP => Only_LSP);
      end if;
   end Refresh;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Data.Get_Script);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null and then Command = "select_construct" then
         declare
            ID        : constant String := Data.Nth_Arg (1, "");
            Iter      : Gtk_Tree_Iter;
            Selection : Gtk.Tree_Selection.Gtk_Tree_Selection;
         begin
            if ID /= "" then
               Iter := Find_Node
                 (Model     => Outline.Tree.Model,
                  Name      => ID,
                  Column    => Id_Column,
                  Recursive => True);

               if Iter = Null_Iter then
                  Data.Set_Error_Msg
                    ("No construct with ID " & ID
                     & " has been found in Outline view");
               else
                  Selection := Get_Selection (Outline.Tree);
                  Selection.Select_Iter
                    (Outline.Tree.Convert_To_Sortable_Model_Iter (Iter));
               end if;
            end if;
         end;
      end if;
   end Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Outline_View_Class : constant Class_Type :=
        Kernel.Scripts.New_Class ("OutlineView");
   begin
      Outline_View_Module := new Outline_View_Module_Record;
      Outline_Views.Register_Module (Kernel, Module_ID (Outline_View_Module));

      --  Register the OulineView python class

      Kernel.Scripts.Register_Command
        ("select_construct",
         Params         => (1 => Param ("id")),
         Class          => Outline_View_Class,
         Static_Method  => True,
         Handler        => Command_Handler'Access);

      --  Register the Outline view's preferences

      Show_Profile := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-profile",
         Default => True,
         Label   => -"Show profiles",
         Doc     => -"Add procedure/function profile");
      Sort_Alphabetical := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-alphabetical-sort",
         Default => True,
         Label   => -"Sort alphabetically",
         Doc     =>
           -("Sort alphabetically, can be combined with Sort by category"));
      Sort_Category := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-category-sort",
         Default => True,
         Label   => -"Sort by category",
         Doc     =>
           -("Sort by category, can be combined with Sort alphabetically"));
      Editor_Link := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-editor-link",
         Default => True,
         Label   => -"Dynamic link with editor",
         Doc     => -"Modify the view selection accordingly to the context.");
      Show_Decls := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-decls",
         Default => True,
         Label   => -"Show specifications");
      Show_Types := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-types",
         Default => True,
         Label   => -"Show types");
      Show_Tasks := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-tasks",
         Default => True,
         Label   => -"Show tasks, entries and protected types");
      Show_Objects := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-objects",
         Default => True,
         Label   => -"Show objects",
         Doc     =>
           -("Show the top-level objects: variables,"
           & " parameters, discriminants."));
      Show_Fields := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-field",
         Default => True,
         Label   => -"Show fields");
      Show_With := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-with",
         Default => False,
         Label   => -"Show with clauses");
      Show_Pragmas := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-show-pragma",
         Default => False,
         Label   => -"Show pragmas");
      Flat_View := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-flat-view",
         Default => False,
         Label   => -"Flat view",
         Doc     =>
           -("Flaten the view. Enabling this preference will "
           & "disable Group names by category"));
      Group_By_Category := Kernel.Get_Preferences.Create_Invisible_Pref
        (Name    => "outline-group-by-category",
         Default => False,
         Label   => -"Group names by category",
         Doc     => -("Group by category. Enabling this preference will "
           & "disable Flat view"));

      Register_Action
        (Kernel, "outline expand all",
         new Outline_Collapse_Or_Expand_Command (True),
         -"Expand all the rows in the Outline view",
         Icon_Name => "gps-expand-all-symbolic",
         Category => -"Outline");

      Register_Action
        (Kernel, "outline collapse all",
         new Outline_Collapse_Or_Expand_Command (False),
         -"Collapse all the rows in the Outline view",
         Icon_Name => "gps-collapse-all-symbolic",
         Category => -"Outline");
   end Register_Module;

   ---------------------------------
   -- Set_Outline_Tooltip_Factory --
   ---------------------------------

   procedure Set_Outline_Tooltip_Factory
     (Tooltip_Factory : not null Outline_Tooltip_Factory_Type) is
   begin
      if Outline_View_Module /= null then
         Outline_View_Module.Tooltip_Factory := Tooltip_Factory;
      end if;
   end Set_Outline_Tooltip_Factory;

   --------------------------------------
   -- Set_Outline_Tooltips_Synchronous --
   --------------------------------------

   procedure Set_Outline_Tooltips_Synchronous (Synchronous : Boolean) is
   begin
      if Outline_View_Module /= null then
         Outline_View_Module.Synchronous_Tooltips := Synchronous;
      end if;
   end Set_Outline_Tooltips_Synchronous;

   -----------------
   -- Encode_Name --
   -----------------

   function Encode_Name (Name : String; Profile : String) return String is
   begin
      if Show_Profile.Get_Pref and then Profile /= "" then
         declare
            --  See GC07-025 + it's useless to put too many characters in
            --  the profile (the full profile can be shown in the tooltips)
            Truncated_Profile : constant String :=
              Profile
                (Profile'First ..
                   Integer'Min (Profile'Last, Profile'First + 500));
         begin
            return
              XML_Utils.Protect (Name)
              & Span_Header
              & XML_Utils.Protect (Truncated_Profile)
              & Span_End;
         end;
      else
         return Name;
      end if;
   end Encode_Name;

   -----------------
   -- Decode_Name --
   -----------------

   function Decode_Name (S : String) return String
   is
      I : constant Integer := Ada.Strings.Fixed.Index (S, Span_Header);
   begin
      declare
         Decoded : constant String := S (S'First .. S'First + I - 2);
      begin
         if Decoded = "" then
            return S;
         else
            return Decoded;
         end if;
      end;
   exception
      when E : others =>
         --  Defensive code to not pass the exception to Gtk
         Trace (Me, E);
         return "";
   end Decode_Name;

   --------------------
   -- Decode_Profile --
   --------------------

   function Decode_Profile (S : String) return String is
      I : constant Integer := Ada.Strings.Fixed.Index (S, Span_Header);
   begin
      return
        S (S'First + I + Span_Header'Length - 1 .. S'Last - Span_End'Length);
   exception
         when others => return "";
   end Decode_Profile;

   ----------------------
   -- Get_LSP_Provider --
   ----------------------

   function Get_LSP_Provider return Outline_Provider_Access is
   begin
      return Outline_View_Module.LSP_Provider;
   end Get_LSP_Provider;

   -----------
   -- Clear --
   -----------

   procedure Clear (Outline : access Outline_View_Record'Class) is
   begin
      declare
         Is_Refresh : constant Boolean :=
           Outline.File = Outline.Prev_File;
         Model      : constant Expansion.Detached_Model :=
           Expansion.Detach_Model_From_View
             (Self           => Outline.Tree,
              Freeze         => True,
              Save_Expansion => Is_Refresh,
              Save_Scrolling => Is_Refresh);
      begin
         Expansion.Clear_Model (Model);
      end;
   end Clear;

   -----------------------
   -- Get_Outline_Model --
   -----------------------

   function Get_Outline_Model
     (Kernel  : Kernel_Handle;
      File    : Virtual_File;
      Default : Boolean := False)
      return Outline_Model_Access
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Default
        and then Outline_View_Module /= null
        and then Outline_View_Module.LSP_Provider /= null
        and then Outline_View_Module.LSP_Provider.Support_Language
          (Get_Language_From_File (Get_Language_Handler (Kernel), File))
      then
         --  This will be handled by the LSP provider
         return null;
      end if;

      if Outline /= null and then Outline.File = File then
         declare
            Is_Refresh : constant Boolean :=
              Outline.File = Outline.Prev_File;
            Model : constant Outline_Model_Access :=
              new Outline_Model'(Model        =>
                                   Expansion.Detach_Model_From_View
                                     (Self           => Outline.Tree,
                                      Freeze         => True,
                                      Save_Expansion => Is_Refresh,
                                      Save_Scrolling => Is_Refresh),
                                 Current_Path => Null_Gtk_Tree_Path,
                                 Category_Map =>
                                   Category_To_Path_Map.Empty_Map,
                                 Filter       => Outline.Filter);
         begin
            Outline.Prev_File := Outline.File;
            return Model;
         end;
      elsif Outline = null then
         raise Outline_Error;
      else
         return null;
      end if;
   end Get_Outline_Model;

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Filter         : Tree_Filter;
      Category       : Language_Category;
      Is_Declaration : Boolean)
      return Boolean is
   begin
      if Category in Type_Category then
         return Filter.Show_Types;
      elsif Category = Cat_Field then
         return Filter.Show_Fields;
      elsif Category = Cat_Variable or else Category = Cat_Constant then
         return Filter.Show_Objects;
      elsif Category = Cat_Local_Variable then
         return Filter.Show_Objects;
      elsif Category in Cat_Task | Cat_Protected then
         return Filter.Show_Tasks;
      elsif Category in Subprogram_Category
        and then Is_Declaration
      then
         return Filter.Show_Decls;
      elsif Category = Cat_With then
         return Filter.Show_With;
      elsif Category in Cat_Package .. Cat_Structure then
         return True;
      elsif Category = Cat_Pragma then
         return Filter.Show_Pragmas;
      else
         return False;
      end if;
   end Is_Visible;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Self           : Outline_Model_Access;
      Name           : VSS.Strings.Virtual_String;
      Profile        : String;
      Category       : Language_Category;
      Is_Declaration : Boolean;
      Visibility     : Construct_Visibility;
      Def_Line       : Integer;
      Def_Col        : Visible_Column_Type;
      Def_End_Line   : Integer;
      Def_End_Col    : Visible_Column_Type;
      End_Line       : Integer;
      Id             : String;
      Visible        : out Boolean)
   is
      Model  : constant Gtk_Tree_Store := Self.Model.Tree.Model;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Iter   : Gtk_Tree_Iter;

      function Get_Parent
        (Parent   : Gtk_Tree_Iter;
         Category : Language_Category)
         return Gtk_Tree_Iter;

      ----------------
      -- Get_Parent --
      ----------------

      function Get_Parent
        (Parent   : Gtk_Tree_Iter;
         Category : Language_Category)
         return Gtk_Tree_Iter is
      begin
         if Self.Filter.Group_By_Category then
            if Self.Category_Map.Contains (Category) then
               return Model.Get_Iter (Self.Category_Map (Category));
            else
               declare
                  Cat_Iter : Gtk_Tree_Iter;
               begin
                  Model.Append (Cat_Iter, Null_Iter);
                  Set_And_Clear
                    (Model,
                     Cat_Iter,
                     (Icon_Column         => As_String
                        (Stock_From_Category
                           (False, Visibility_Public, Category)),
                      Name_Column         =>
                        As_String (Category_Name (Category)),
                      Start_Line_Column   => As_Int (-1),
                      Start_Col_Column    => As_Int (-1),
                      Def_End_Line_Column => As_Int (-1),
                      Def_End_Col_Column  => As_Int (-1),
                      End_Line_Column     => As_Int (0),
                      Category_Column     =>
                        As_Int (Gint (Sort_Entities (Category))),
                      Id_Column           => As_String (String'(""))));
                  Self.Category_Map.Include
                    (Category, Get_Path (Model, Cat_Iter));
                  return Cat_Iter;
               end;
            end if;
         elsif Self.Filter.Flat_View then
            return Null_Iter;
         else
            return Parent;
         end if;
      end Get_Parent;

   begin
      if Self.Current_Path /= Null_Gtk_Tree_Path then
         Parent := Model.Get_Iter (Self.Current_Path);
      end if;

      Visible := Is_Visible (Self.Filter, Category, Is_Declaration);

      if Visible then
         Model.Append (Iter, Get_Parent (Parent, Category));
         Set_And_Clear
           (Model,
            Iter,
            (Icon_Column         => As_String
                 (Stock_From_Category (Is_Declaration, Visibility, Category)),
             Name_Column         =>
               As_String
                 (Encode_Name
                    (VSS.Strings.Conversions.To_UTF_8_String (Name), Profile)),
             Start_Line_Column   => As_Int (Gint (Def_Line)),
             Start_Col_Column    => As_Int (Gint (Def_Col)),
             Def_End_Line_Column => As_Int (Gint (Def_End_Line)),
             Def_End_Col_Column  => As_Int (Gint (Def_End_Col)),
             End_Line_Column     => As_Int (Gint (End_Line)),
             Category_Column     => As_Int (Gint (Sort_Entities (Category))),
             Id_Column           => As_String (Id)));
         Self.Current_Path := Get_Path (Model, Iter);
      end if;
   end Add_Row;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor
     (Self     : Outline_Model_Access;
      Movement : Insertion_Movement) is
   begin
      if Self.Current_Path /= Null_Gtk_Tree_Path then
         declare
            Model : constant Gtk_Tree_Store := Self.Model.Tree.Model;
            Iter  : Gtk_Tree_Iter := Model.Get_Iter (Self.Current_Path);
         begin
            case Movement is
               when Up =>
                  Iter := Model.Parent (Iter);
               when Down =>
                  Iter := Model.Children (Iter);
               when Stay =>
                  null;
            end case;
            if Iter /= Null_Iter then
               Path_Free (Self.Current_Path);
               Self.Current_Path := Model.Get_Path (Iter);
            else
               Self.Current_Path := Null_Gtk_Tree_Path;
            end if;
         end;
      end if;
   end Move_Cursor;

   ------------------------
   -- Finished_Computing --
   ------------------------

   procedure Finished_Computing
     (Kernel : Kernel_Handle;
      Status : Computing_Status := Succeeded)
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
   begin
      if Outline /= null then
         Generic_Views.Abstract_View_Access
           (Outline).Set_Activity_Progress_Bar_Visibility (False);
         case Status is
            when Failed    =>
               Outline.Prev_File := No_File;
               Outline.File      := No_File;

            when Succeeded =>

               --  If we have no symbols after finishing computing, display a
               --  message in the Outline.
               if Outline.Tree.Model.N_Children = 0 then
                  declare
                     Iter : Gtk_Tree_Iter;
                  begin
                     Outline.Tree.Model.Append (Iter, Null_Iter);
                     Set_And_Clear
                       (Outline.Tree.Model,
                        Iter,
                        (Icon_Column         => As_String (String'("")),
                         Name_Column         => As_String
                           (Span_Header & "No symbols available" & Span_End),
                         Start_Line_Column   => As_Int (0),
                         Start_Col_Column    => As_Int (0),
                         Def_End_Line_Column => As_Int (0),
                         Def_End_Col_Column  => As_Int (0),
                         End_Line_Column     => As_Int (0),
                         Category_Column     => As_Int (4),
                         Id_Column           => As_String (String'(""))));
                  end;
               else
                  Outline.Tree.Refilter;
                  Location_Changed (Kernel, Outline.File, Force => True);
               end if;

               --  For testing purpose.
               --  Careful this must be the last statement affecting
               --  Outline because it can be called by an Idle and will
               --  trigger the next python command in the testsuite
               Outline_Loaded_Hook.Run (Kernel, Outline.File);

            when Stopped =>
               null;
         end case;
      end if;
   end Finished_Computing;

   -------------------------
   -- Clear_Outline_Model --
   -------------------------

   procedure Clear_Outline_Model (Self : Outline_Model_Access) is
   begin
      if Self /= null then
         Trace (Me, "Clear Model");
         Self.Model.Clear_Model;
      end if;
   end Clear_Outline_Model;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Outline_Model_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Outline_Model, Outline_Model_Access);
   begin
      if Self /= null then
         Trace (Me, "Free Outline_Access");
         Path_Free (Self.Current_Path);
         for Item of Self.Category_Map loop
            Path_Free (Item);
         end loop;
         Self.Category_Map.Clear;
         Unchecked_Free (Self);
      end if;
   end Free;

   --------------------------
   -- Set_Default_Provider --
   --------------------------

   procedure Set_Default_Provider (Provider : Outline_Provider_Access) is
   begin
      if Outline_View_Module /= null then
         Outline_View_Module.Default_Provider := Provider;
      end if;
   end Set_Default_Provider;

   ----------------------
   -- Set_LSP_Provider --
   ----------------------

   procedure Set_LSP_Provider (Provider : Outline_Provider_Access) is
   begin
      if Outline_View_Module /= null then
         Outline_View_Module.LSP_Provider := Provider;
      end if;
   end Set_LSP_Provider;

   --------------------
   -- Stop_Providers --
   --------------------

   procedure Stop_Providers is
   begin
      Trace (Me, "Stop_Providers");
      if Outline_View_Module.LSP_Provider /= null then
         Outline_View_Module.LSP_Provider.Stop_Fill;
      end if;
      if Outline_View_Module.Default_Provider /= null then
         Outline_View_Module.Default_Provider.Stop_Fill;
      end if;
   end Stop_Providers;

   --------------------
   -- Start_Provider --
   --------------------

   procedure Start_Provider
     (Kernel   : Kernel_Handle;
      File     : Virtual_File;
      Only_LSP : Boolean := False)
   is
      Lang     : constant Language.Language_Access :=
        Get_Language_From_File (Get_Language_Handler (Kernel), File);
      Provider : Outline_Provider_Access := null;
   begin
      if Outline_View_Module.LSP_Provider /= null
        and then Outline_View_Module.LSP_Provider.Support_Language (Lang)
      then
         Trace (Me, "Start_Provider LSP");
         Provider := Outline_View_Module.LSP_Provider;
      elsif not Only_LSP then
         Trace (Me, "Start_Provider Semantic");
         Provider := Outline_View_Module.Default_Provider;
      end if;

      if Provider /= null then
         Provider.Start_Fill (File);
      else
         Finished_Computing (Kernel);
      end if;
   end Start_Provider;

end Outline_View;
