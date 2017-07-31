------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types.Keysyms;         use Gdk.Types, Gdk.Types.Keysyms;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Gesture_Multi_Press;   use Gtk.Gesture_Multi_Press;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Toolbar;               use Gtk.Toolbar;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Abstract_Tree_Model;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Basic_Types;               use Basic_Types;
with Default_Preferences;       use Default_Preferences;
with Entities_Tooltips;
with Generic_Views;             use Generic_Views;
with GPS.Editors;               use GPS.Editors;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Properties;
with GPS.Search;                use GPS.Search;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with Language;                  use Language;
with Tooltips;                  use Tooltips;

with Outline_View.Model; use Outline_View.Model;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package body Outline_View is

   Outline_View_Class_Name : constant String := "OutlineView";

   Me : constant Trace_Handle := Create ("OUTLINE.VIEW");

   type Outline_View_Module_Record is new Module_ID_Record with null record;

   Outline_View_Module : Module_ID;
   Outline_View_Module_Name : constant String := "Outline_View";

   Show_Profile           : Boolean_Preference;
   Sort_Alphabetical      : Boolean_Preference;
   Editor_Link            : Boolean_Preference;
   Show_Decls             : Boolean_Preference;
   Show_Types             : Boolean_Preference;
   Show_Tasks             : Boolean_Preference;
   Show_Objects           : Boolean_Preference;
   Show_With              : Boolean_Preference;
   Group_Spec_And_Body    : Boolean_Preference;
   Flat_View              : Boolean_Preference;
   No_Param_Names         : Boolean_Preference;

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

   type Outline_View_Record is new Generic_Views.View_Record with record
      Tree        : Gtk_Tree_View;
      File        : GNATCOLL.VFS.Virtual_File;
      Filter      : Gtk.GEntry.Gtk_Entry;

      Multipress  : Gtk_Gesture_Multi_Press;

      Spec_Pixbuf      : Gtk_Cell_Renderer_Pixbuf;
      Body_Pixbuf      : Gtk_Cell_Renderer_Pixbuf;
      --  One of the cell renderers used for the spec and body icons. This is
      --  used to compute the size of the icons, and thus whether the user
      --  clicked on one or the other.
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

   procedure Refresh (View : access Gtk_Widget_Record'Class);
   --  Recompute the information for Outline.File, and redisplay it.
   --  If the constructs are up-to-date, do nothing.

   procedure Force_Refresh (View : access Gtk_Widget_Record'Class);
   --  Same as above, but force a full refresh

   procedure Goto_Node
     (View     : not null access Outline_View_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Column   : Gint := Display_Name_Column;
      Fallback : Boolean);
   --  goto and highlight the entity.
   --  If Fallback is true, and the buffer is already showing node's
   --  location, then we jump to the body.

   function Select_Node_From_ID
     (View : not null access Outline_View_Record'Class;
      ID   : GNATCOLL.Symbols.Symbol) return Boolean;
   --  Select the node identified with ID in the outline.
   --  This does nothing if the given ID does not match any node in the tree.
   --
   --  Return True if a node has been found for ID, False otherwise.

   function On_Key_Press
     (Outline : access GObject_Record'Class;
      Event   : Gdk_Event_Key) return Boolean;
   --  Handle key events in the outline

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble);
   --  Called every time a row is clicked

   procedure On_Changed
     (Outline : not null Outline_View_Access;
      Context : Selection_Context);
   --  Update outline with given Context

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  React to changes in the preferences

   function Get_Filter_Record
     (Kernel : access Kernel_Handle_Record'Class) return Tree_Filter;
   --  Return the filters properties extracted from the kernel

   type On_Location_Changed is new File_Location_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type);
   --  Called when the current editor reaches a new location

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class; Line, Column : Natural);

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      F : GNATCOLL.VFS.Virtual_File);

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class);
   --  Called when the outline is destroyed

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

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been edited

   type On_Semantic_Tree_Updated is
     new File_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Semantic_Tree_Updated;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File);

   procedure Set_File
     (Outline : access Outline_View_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File);
   --  Set the file viewed in Outline

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);

   --------------
   -- Tooltips --
   --------------

   type Outline_View_Tooltips is new Tooltips.Tooltips with record
      Outline : Outline_View_Access;
   end record;
   type Outline_View_Tooltips_Access is access all Outline_View_Tooltips;
   overriding function Create_Contents
     (Tooltip  : not null access Outline_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   ------------------------
   -- Outline_Tree Model --
   ------------------------

   function Get_Outline_Model
     (View : Outline_View_Access) return Outline_Model;
   --  Return the outline model stored in this view

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Outline_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Iter     : Gtk_Tree_Iter;
      Area     : Gdk_Rectangle;
   begin
      Initialize_Tooltips (Tooltip.Outline.Tree, X, Y, Area, Iter);

      if Iter /= Null_Iter then
         Tooltip.Set_Tip_Area (Area);
         declare
            SN : constant Semantic_Node'Class :=
              Tooltip.Outline.Kernel.Get_Abstract_Tree_For_File
                ("OUTLINE", Tooltip.Outline.File).Node_At
              (Get_Info (Get_Outline_Model (Tooltip.Outline), Iter)
               .Sloc_Start_No_Tab);
         begin
            if SN /= No_Semantic_Node then
               return Entities_Tooltips.Draw_Tooltip
                 (Kernel      => Tooltip.Outline.Kernel,
                  Draw_Border => True,
                  Entity      => SN);
            end if;
         end;
      end if;
      return null;
   end Create_Contents;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      F      : GNATCOLL.VFS.Virtual_File)
   is
      Ed : constant Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Get
          (F, Open_View => False, Focus => False);
   begin
      if Ed /= Nil_Editor_Buffer then
         declare
            Mark : constant Editor_Mark'Class :=
              Ed.Get_Main_Cursor.Get_Insert_Mark;
         begin
            Location_Changed
              (Kernel,
               Natural (Mark.Line),
               Natural (Mark.Column));
         end;
      else
         --  We are not in an editor: check if the current file has a construct
         --  that we should explicitly select.
         declare
            Outline   : constant Outline_View_Access :=
              Outline_Views.Retrieve_View (Kernel);
            Lang      : Language_Access;
            ID        : GNATCOLL.Symbols.Symbol;
            Success   : Boolean;
         begin
            Lang := Outline.Kernel.Lang_Handler.Get_Language_From_File
              (Outline.File);
            ID := Lang.Get_Last_Selected_Construct_ID (Outline.File);

            Success := Select_Node_From_ID (Outline, ID);

            if not Success then
               Trace (Me, "no construct with ID " & Debug_Print (ID)
                      & " has been found when trying to select it in tree");
            end if;
         end;
      end if;
   end Location_Changed;

   ----------------------
   -- Location_Changed --
   ----------------------

   procedure Location_Changed
     (Kernel : access Kernel_Handle_Record'Class; Line, Column : Natural)
   is
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);
      Model   : Outline_Model;
      Path    : Gtk_Tree_Path;
   begin
      if Editor_Link.Get_Pref and then Outline /= null then
         Model   := Get_Outline_Model (Outline);
         Unselect_All (Get_Selection (Outline.Tree));

         if Model = null then
            return;
         end if;

         Path := Get_Path_Enclosing_Location (Model, Line, Column);

         if Get_Depth (Path) >= 1 then
            declare
               Indices     : constant Glib.Gint_Array := Get_Indices (Path);
               Parent_Path : Gtk_Tree_Path;
            begin
               Gtk_New (Parent_Path);
               for J in Indices'First .. Indices'Last - 1 loop
                  Append_Index (Parent_Path, Indices (J));
               end loop;

               Expand_To_Path (Outline.Tree, Parent_Path);
               Path_Free (Parent_Path);
            end;

            Set_Cursor (Outline.Tree, Path, null, False);
         end if;

         Path_Free (Path);
      end if;
   end Location_Changed;

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
      pragma Unreferenced (Self, Project, File);
   begin
      Location_Changed (Kernel, Line, Column);
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
      if Outline /= null then
         Set_Font_And_Colors (Outline.Tree, Fixed_Font => True, Pref => Pref);

         if Pref = null
           or else Pref = Preference (Show_Profile)
           or else Pref = Preference (Sort_Alphabetical)
           or else Pref = Preference (Editor_Link)
           or else Pref = Preference (Show_Decls)
           or else Pref = Preference (Show_Types)
           or else Pref = Preference (Show_Tasks)
           or else Pref = Preference (Show_Objects)
           or else Pref = Preference (Show_With)
           or else Pref = Preference (Group_Spec_And_Body)
           or else Pref = Preference (Flat_View)
           or else Pref = Preference (No_Param_Names)
         then
            Force_Refresh (Outline);
         end if;
      end if;
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
      Model    : constant Outline_Model :=
        Outline_Model (-Get_Model (Outline.Tree));
      Line     : Integer := 1;
      Path     : Gtk_Tree_Path;
      Node_Info : Semantic_Node_Info;
   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);

      Iter := Find_Iter_For_Event (Outline.Tree, Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Model, Iter);
         if not Path_Is_Selected (Get_Selection (Outline.Tree), Path) then
            Set_Cursor (Outline.Tree, Path, null, False);
         end if;
         Path_Free (Path);

         Node_Info := Get_Info (Model, Iter);

         Set_Entity_Information
           (Context       => Context,
            Entity_Name   => Get (Node_Info.Name).all,
            Entity_Column => Node_Info.Sloc_Start_No_Tab.Column);

         Line := Node_Info.Sloc_Start_No_Tab.Line;
      end if;

      Set_File_Information
        (Context => Context,
         Project => No_Project,
         Files   => (1 => Outline.File),
         Line    => Line);

      return Context;
   end Build_Context;

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
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Outline_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K        : constant Kernel_Handle := View.Kernel;
      Sep      : Gtk_Menu_Item;
   begin
      Append_Menu (Menu, K, Show_Profile);
      Append_Menu (Menu, K, No_Param_Names);

      Append_Menu (Menu, K, Show_Types);
      Append_Menu (Menu, K, Show_Objects);
      Append_Menu (Menu, K, Show_Tasks);
      Append_Menu (Menu, K, Show_Decls);
      Append_Menu (Menu, K, Show_With);

      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, K, Sort_Alphabetical);
      Append_Menu (Menu, K, Flat_View);
      Append_Menu (Menu, K, Group_Spec_And_Body);

      Gtk_New (Sep);
      Menu.Append (Sep);

      Append_Menu (Menu, K, Editor_Link);
   end Create_Menu;

   -----------------------
   -- Get_Filter_Record --
   -----------------------

   function Get_Filter_Record
     (Kernel : access Kernel_Handle_Record'Class) return Tree_Filter
   is
      pragma Unreferenced (Kernel);
   begin
      return
        (Hide_Types          => not Show_Types.Get_Pref,
         Hide_Objects        => not Show_Objects.Get_Pref,
         Hide_Declarations   => not Show_Decls.Get_Pref,
         Hide_Tasks          => not Show_Tasks.Get_Pref,
         Hide_Withes         => not Show_With.Get_Pref,
         Show_Profile        => Show_Profile.Get_Pref,
         Sorted              => Sort_Alphabetical.Get_Pref,
         Group_Spec_And_Body => Group_Spec_And_Body.Get_Pref,
         Show_Param_Names    => not No_Param_Names.Get_Pref,
         Flat_View           => Flat_View.Get_Pref);
   end Get_Filter_Record;

   ---------------
   -- Goto_Node --
   ---------------

   procedure Goto_Node
     (View     : not null access Outline_View_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Column   : Gint := Display_Name_Column;
      Fallback : Boolean)
   is
      Model : constant Outline_Model := Outline_Model (-Get_Model (View.Tree));
      Node : constant Semantic_Node_Info := Get_Info (Model, Iter, Column);
      Lang : Language_Access;
   begin
      if Node = No_Node_Info then
         return;
      end if;

      --  Does the language have a special handling for constructs ?

      Lang := View.Kernel.Lang_Handler.Get_Language_From_File (View.File);
      if Lang.Clicked_On_Construct (View.File, Node) then
         return;
      end if;

      --  If not, the default is to open a source editor

      declare
         use type Visible_Column_Type;
         Buffer   : constant Editor_Buffer'Class :=
           Get (Get_Buffer_Factory (View.Kernel).all,
                View.File, False, False, False);
         HSD : constant Boolean := Node.Sloc_Def_No_Tab.Index /= 0;
         Location : constant Editor_Location'Class :=
           (if HSD
            then New_Location (Buffer, Node.Sloc_Def_No_Tab.Line,
              Node.Sloc_Def_No_Tab.Column)
            else New_Location
              (Buffer, Node.Sloc_Start_No_Tab.Line,
               Node.Sloc_Start_No_Tab.Column));

         End_Location : constant Editor_Location'Class :=
           (if HSD
            then New_Location (Buffer, Node.Sloc_Def_No_Tab.Line,
              Node.Sloc_Def_No_Tab.Column
              + Get (Node.Name)'Length)
            else Location);
         Editor       : constant Editor_View'Class := Current_View (Buffer);

      begin
         if Fallback
           and then Location.Line = Editor.Cursor.Line
         then
            --  Clicking on the name will jump to the spec, unless this is
            --  already the current location
            Goto_Node (View, Iter, Body_Pixbuf_Column, Fallback => False);
         else
            Editor.Cursor_Goto (Location, Raise_View => True);
            Select_Text (Buffer, Location, End_Location);
         end if;
      end;
   end Goto_Node;

   -------------------------
   -- Select_Node_From_ID --
   -------------------------

   function Select_Node_From_ID
     (View : not null access Outline_View_Record'Class;
      ID   : GNATCOLL.Symbols.Symbol) return Boolean
   is
      Path    : Gtk_Tree_Path;
      Success : Boolean;
   begin
      if ID = GNATCOLL.Symbols.No_Symbol then
         return False;
      end if;

      Path := Get_Outline_Model (View).Get_Path_From_Unique_ID (ID);

      if Path /= Null_Gtk_Tree_Path then
         if Get_Depth (Path) >= 1 then
            declare
               Indices     : constant Glib.Gint_Array :=
                 Get_Indices (Path);
               Parent_Path : Gtk_Tree_Path;
            begin
               Gtk_New (Parent_Path);
               for J in Indices'First .. Indices'Last - 1 loop
                  Append_Index (Parent_Path, Indices (J));
               end loop;

               Expand_To_Path (View.Tree, Parent_Path);
               Path_Free (Parent_Path);
            end;

            Set_Cursor (View.Tree, Path, null, False);

            Success := True;
         end if;
      else
         Success := False;
      end if;

      Path_Free (Path);

      return Success;
   end Select_Node_From_ID;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble)
   is
      View : constant Outline_View_Access := Outline_View_Access (Self);
      Model : constant Outline_Model := Outline_Model (-Get_Model (View.Tree));
      Iter  : Gtk_Tree_Iter;

      Cell_X, Cell_Y  : Gint;
      Column          : Gtk_Tree_View_Column;
      Success         : Boolean;
      Area, Cell_Area : Gdk_Rectangle;
      Filter_Path     : Gtk_Tree_Path;
      Filter     : constant Tree_Filter := Get_Filter_Record (View.Kernel);
   begin
      if N_Press = 1
        and then View.File /= No_File
      then
         View.Tree.Get_Path_At_Pos
           (Gint (X), Gint (Y), Filter_Path,
            Column, Cell_X, Cell_Y, Success);

         if Success then
            --  Area is the rectangle, within the TreeView, where the
            --  column is displayed. For instance x=20 to 264
            View.Tree.Get_Cell_Area (Filter_Path, Column, Area);

            --  Aligned area is the rectangle within the rectangle where
            --  the renderer is displayed. Only the size seems to be set
            --  to an interesting value, the X coordinate is unclear.
            --  Since the bookmarks view displays one icon for
            --  'bookmark/folder', then one optional one for 'tag',
            --  we just assume both have the same size.

            View.Spec_Pixbuf.Get_Aligned_Area
              (Widget       => View.Tree,
               Flags        => Gtk.Cell_Renderer.Cell_Renderer_Focused,
               Cell_Area    => Area,
               Aligned_Area => Cell_Area);

            Iter := Model.Get_Iter (Filter_Path);
            Path_Free (Filter_Path);

            --  In the expander column ?

            if Gint (X) < Area.X then
               null;

            --  If we clicked on the spec column always jump to exact location

            elsif Gint (X) - Area.X <= Cell_Area.Width then
               Goto_Node (View, Iter, Fallback => False);

            --  If we clicked in the body column, assuming it was displayed
            elsif Filter.Group_Spec_And_Body
              and then Cell_Area.Width <= Gint (X) - Area.X
              and then Gint (X) - Area.X <= 2 * Cell_Area.Width
              and then Model.Get_String (Iter, Body_Pixbuf_Column) /= ""
            then
               Goto_Node (View, Iter, Column => Body_Pixbuf_Column,
                          Fallback => False);

            else
               Goto_Node (View, Iter, Fallback => True);
            end if;

            View.Multipress.Set_State (Event_Sequence_Claimed);
         end if;
      end if;
   end On_Multipress;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Outline_View_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      Get_Outline_Model (Self).Set_Filter (Pattern);
      Force_Refresh (Self);
   end Filter_Changed;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (Outline : access GObject_Record'Class;
      Event   : Gdk_Event_Key) return Boolean
   is
      View  : constant Outline_View_Access := Outline_View_Access (Outline);
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
   begin
      View.Tree.Get_Selection.Get_Selected (Model, Iter);
      if Iter /= Null_Iter then
         if Event.Keyval = GDK_Return then
            Goto_Node (View, Iter, Fallback => True);
            return True;
         end if;
      end if;

      return False;
   end On_Key_Press;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Outline : access Outline_View_Record'Class)
      return Gtk.Widget.Gtk_Widget
   is
      Text_Col      : Gtk_Tree_View_Column;
      Col_Number    : Gint;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Tooltip       : Outline_View_Tooltips_Access;
      Scrolled      : Gtk_Scrolled_Window;

      pragma Unreferenced (Col_Number);
      Out_Model : Outline_Model;

   begin
      Initialize_Vbox (Outline);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Outline.Pack_Start (Scrolled, Expand => True, Fill => True);

      --  Create the tree view using the sorting model

      Out_Model := new Outline_Model_Record;
      Gtkada.Abstract_Tree_Model.Initialize (Out_Model);
      Gtk_New (Outline.Tree, Out_Model);
      Unref (Out_Model);
      Set_Name (Outline.Tree, "Outline View Tree");  --  For testsuite

      Set_Headers_Visible (Outline.Tree, False);

      --  Create an explicit columns for the expander

      Gtk_New (Text_Col);
      Col_Number := Append_Column (Outline.Tree, Text_Col);

      Gtk_New (Outline.Spec_Pixbuf);
      Text_Col.Pack_Start (Outline.Spec_Pixbuf, Expand => False);
      Text_Col.Add_Attribute
        (Outline.Spec_Pixbuf, "icon-name", Spec_Pixbuf_Column);

      Gtk_New (Outline.Body_Pixbuf);
      Text_Col.Pack_Start (Outline.Body_Pixbuf, Expand => False);
      Text_Col.Add_Attribute
        (Outline.Body_Pixbuf, "visible", Has_Body_Column);
      Text_Col.Add_Attribute
        (Outline.Body_Pixbuf, "icon-name", Body_Pixbuf_Column);

      Gtk_New (Text_Render);
      Text_Col.Pack_Start (Text_Render, Expand => False);
      Add_Attribute
        (Text_Col, Text_Render,
         "markup", Outline_View.Model.Display_Name_Column);
      Clicked (Text_Col);

      Scrolled.Add (Outline.Tree);

      Set_Font_And_Colors (Outline.Tree, Fixed_Font => True);

      Gtk_New (Outline.Multipress, Widget => Outline.Tree);
      Outline.Multipress.On_Pressed (On_Multipress'Access, Slot => Outline);
      Outline.Multipress.Watch (Outline);

      Gtkada.Handlers.Widget_Callback.Connect
        (Outline, Signal_Destroy, On_Destroy'Access);

      Outline.Tree.On_Key_Press_Event
        (On_Key_Press'Access, Slot => Outline, After => False);

      Setup_Contextual_Menu
        (Kernel          => Outline.Kernel,
         Event_On_Widget => Outline.Tree);

      Tooltip := new Outline_View_Tooltips;
      Tooltip.Outline := Outline;
      Set_Tooltip (Tooltip, Outline.Tree);

      declare
         P : constant access On_Context_Changed := new On_Context_Changed;
      begin
         On_Changed (Outline, Get_Current_Context (Outline.Kernel));
         Context_Changed_Hook.Add_Debounce (P, Watch => Outline);
      end;

      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => Outline);
      Location_Changed_Hook.Add_Debounce
        (new On_Location_Changed, Watch => Outline);
      File_Closed_Hook.Add (new On_File_Closed, Watch => Outline);
      File_Edited_Hook.Add (new On_File_Edited, Watch => Outline);
      Project_View_Changed_Hook.Add (new On_Project_Changed, Watch => Outline);
      Semantic_Tree_Updated_Hook.Add
        (new On_Semantic_Tree_Updated, Watch => Outline);

      return Gtk_Widget (Outline.Tree);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Outline : access Gtk_Widget_Record'Class)
   is
      O     : constant Outline_View_Access := Outline_View_Access (Outline);
      Model : constant Outline_Model := Get_Outline_Model (O);
   begin

      if Model /= null then
         Model.Free;
      end if;
   end On_Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Gtk_Widget_Record'Class) is
      Outline : constant Outline_View_Access := Outline_View_Access (View);
   begin
      if Outline.File /= No_File then
         Outline.Kernel.Get_Abstract_Tree_For_File
           ("OUTLINE", Outline.File).Update_Async;
         --  will be completed after trigger Semantic_Tree_Updated_Hook
      end if;
   end Refresh;

   -------------------
   -- Force_Refresh --
   -------------------

   procedure Force_Refresh (View : access Gtk_Widget_Record'Class) is
      Outline : constant Outline_View_Access := Outline_View_Access (View);
   begin
      if Outline.File /= No_File then
         Set_File (Outline, Outline.File);
      end if;
   end Force_Refresh;

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
         Outline.Set_File (GNATCOLL.VFS.No_File);
         Refresh (Outline);
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
            Outline.Set_File (File);
         end if;

         Refresh (Outline);
         Location_Changed (Kernel, File);
      end if;
   end Execute;

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
      else
         --  Fallback to last used editor
         File := Get_Kernel (Context).Get_Buffer_Factory
           .Get (Open_View => False).File;
      end if;

      if File /= Outline.File then
         Outline.Set_File (File);
         Refresh (Outline);
      elsif Outline.File = GNATCOLL.VFS.No_File then
         Refresh (Outline);
      end if;
   end On_Changed;

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

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Outline_View_Class : constant Class_Type :=
        Kernel.Scripts.New_Class (Outline_View_Class_Name);
   begin
      Outline_View_Module := new Outline_View_Module_Record;
      Outline_Views.Register_Module (Kernel, Outline_View_Module);

      --  Register the OulineView python class

      Kernel.Scripts.Register_Command
        ("select_construct",
         Params         => (1 => Param ("id")),
         Class          => Outline_View_Class,
         Static_Method  => True,
         Handler        => Command_Handler'Access);

      --  Register the Outline view's preferences

      Show_Profile := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-show-profile", True, Label => -"Show profiles");
      Sort_Alphabetical := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-alphabetical", True, Label => -"Sort alphabetically");
      Editor_Link := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-editor-link", True, Label => -"Dynamic link with editor");
      Show_Decls := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-show-decls", True, Label => -"Show specifications");
      Show_Types := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-show-types", True, Label => -"Show types");
      Show_Tasks := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-show-tasks", True,
         Label => -"Show tasks, entries and protected types");
      Show_Objects := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-show-objects", True, Label => -"Show objects");
      Show_With := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-show-with", False, Label => -"Show with clauses");
      Group_Spec_And_Body := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-group-spec-and-body", False,
         Label => -"Group spec and body");
      Flat_View := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-flat-view", False, Label => -"Flat view");
      No_Param_Names := Kernel.Get_Preferences.Create_Invisible_Pref
        ("outline-no-param-names",
         Default => False,
         Label   => -"Hide parameter names (Ada)",
         Doc     =>
           -("Hide the name of parameters in subprogram profiles. This"
           & " only applies to some programming languages, notably Ada"));
   end Register_Module;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Outline : access Outline_View_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File)
   is
      Model  : Outline_Model;
      Filter : constant Tree_Filter :=
        Get_Filter_Record (Outline.Kernel);
      Tree   : constant Semantic_Tree'Class :=
        Outline.Kernel.Get_Abstract_Tree_For_File ("OUTLINE", File);

      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Node   : Gint := 0;
   begin
      Model := Get_Outline_Model (Outline_View_Access (Outline));

      if Model = null then
         Model := new Outline_Model_Record;
         Gtkada.Abstract_Tree_Model.Initialize (Model);
         Outline.Tree.Set_Model (To_Interface (Model));

      elsif not Flat_View.Get_Pref
        and then Show_With.Get_Pref
        and then Outline.File /= No_File
      then
         --  Store state of Root_With node
         Iter := Model.Root_With_Iter;
         if Iter /= Null_Iter then
            declare
               Value : access GPS.Properties.Boolean_Property;
            begin
               Path  := Model.Get_Path (Model.Root_With_Iter);
               Value := new GPS.Properties.Boolean_Property'
                 (Value => Outline.Tree.Row_Expanded (Path));

               GPS.Kernel.Properties.Set_Property
                 (Outline.Kernel,
                  Outline.File,
                  "Outline_Root_With",
                  Value,
                  False);
               Path_Free (Path);
            end;
         end if;
      end if;

      Outline.File := File;

      --  This function is called directly after the settings have changed,
      --  and should take their new value into account.

      Outline.Tree.Set_Show_Expanders (Enabled => not Filter.Flat_View);

      Model.Set_Tree (Tree, Filter);

      if Tree /= No_Semantic_Tree
        and then Tree.Is_Ready
        and then not Flat_View.Get_Pref
      then
         if Show_With.Get_Pref then
            Node := 1;  -- package node is second

            --  Restore state of Root_With node
            declare
               Value  : GPS.Properties.Boolean_Property;
               Found  : Boolean;
            begin
               GPS.Properties.Get_Property
                 (Value, Outline.File, "Outline_Root_With", Found);

               if Found
                 and then Value.Value
               then
                  Iter := Model.Root_With_Iter;
                  if Iter /= Null_Iter then
                     Path := Model.Get_Path (Model.Root_With_Iter);
                     Outline.Tree.Expand_To_Path (Path);
                     Path_Free (Path);
                  end if;
               end if;
            end;
         end if;

         --  Expand package node
         Gtk_New (Path);
         Append_Index (Path, Node);
         Expand_To_Path (Outline.Tree, Path);
         Path_Free (Path);
      end if;
   end Set_File;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "select_construct" then
         declare
            Outline : constant Outline_View_Access :=
              Outline_Views.Retrieve_View (Get_Kernel (Data.Get_Script));
            S_ID    : constant String := Data.Nth_Arg (1, "");
            ID      : GNATCOLL.Symbols.Symbol;
            Lang    : Language_Access;
            Success : Boolean;
         begin
            if S_ID /= "" then
               Lang := Outline.Kernel.Lang_Handler.Get_Language_From_File
                 (Outline.File);
               ID := Lang.Symbols.Find (S_ID);

               Success := Select_Node_From_ID (Outline, ID);

               if not Success then
                  Data.Set_Error_Msg
                    ("No construct with ID " & Debug_Print (ID)
                     & " has been found in Outline view");
               end if;
            end if;
         end;
      end if;
   end Command_Handler;

   -----------------------
   -- Get_Outline_Model --
   -----------------------

   function Get_Outline_Model
     (View : Outline_View_Access) return Outline_Model
   is
      Model : constant Gtk_Root_Tree_Model :=
        -Get_Model (View.Tree);
   begin
      if Model = null then
         return null;
      elsif Model.all in Outline_Model_Record'Class then
         return Outline_Model (Model);
      else
         return null;
      end if;
   end Get_Outline_Model;

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
         --  We need to force the set of the file here. In certain cases,
         --  the outline is computed to early and work on an unknown
         --  language, which is then set when the project is loaded.

         Set_File (Outline, Outline.File);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Semantic_Tree_Updated;
       Kernel : not null access Kernel_Handle_Record'Class;
       File   : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Self);
      Outline : constant Outline_View_Access :=
        Outline_Views.Retrieve_View (Kernel);

      Tree : constant Semantic_Tree'Class :=
        Outline.Kernel.Get_Abstract_Tree_For_File ("OUTLINE", File);
   begin
      if Outline /= null and then Outline.File /= No_File
        and then Outline.File = File
      then
         File_Updated (Get_Outline_Model (Outline), Tree);
         Location_Changed (Outline.Kernel, File);
      end if;
   end Execute;

end Outline_View;
