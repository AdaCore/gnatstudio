------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Glib;                     use Glib;
with Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib_Values_Utils;        use Glib_Values_Utils;

with Gdk.Event;                use Gdk.Event;
with Gdk.Rectangle;            use Gdk.Rectangle;
with Gdk.Types;                use Gdk.Types;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;             use Gtk.Handlers;
with Gtk.Label;                use Gtk.Label;
with Gtk.Menu;
with Gtk.Notebook;             use Gtk.Notebook;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Toolbar;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtkada.Tree_View;         use Gtkada.Tree_View;

with Basic_Types;
with Default_Preferences;      use Default_Preferences;
with Generic_Views;            use Generic_Views;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Search;        use GPS.Kernel.Search;
with GPS.Intl;                 use GPS.Intl;
with GPS.Search;               use GPS.Search;
with GPS.Search.GUI;           use GPS.Search.GUI;
with GPS.VCS;                  use GPS.VCS;
with GUI_Utils;                use GUI_Utils;
with Src_Editor_Module;        use Src_Editor_Module;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Tooltips;                 use Tooltips;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Filter_Panels;            use Filter_Panels;

package body Buffer_Views is
   Me : constant Trace_Handle := Create ("GPS.VIEWS.WINDOWS");

   File_Icon_Column : constant := 0;
   Name_Column      : constant := 1;
   Data_Column      : constant := 2;
   VCS_Icon_Column  : constant := 3;

   Column_Types : constant GType_Array :=
     (File_Icon_Column => GType_String,
      Name_Column      => GType_String,
      Data_Column      => GType_String,
      VCS_Icon_Column  => GType_String);

   Untitled    : constant String := "Untitled";
   --  Label used for new window that is not yet saved

   Editors_Only         : Boolean_Preference;
   Show_Notebooks       : Boolean_Preference;
   Sort_Alphabetical    : Boolean_Preference;
   Hide_Empty_Notebooks : Boolean_Preference;
   Show_Vcs_Status      : Boolean_Preference;

   type BV_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access BV_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   type Buffer_Tree_View_Record is new Gtkada.Tree_View.Tree_View_Record with
      record
         Kernel   : access Kernel_Handle_Record'Class;
         Pattern  : Search_Pattern_Access;
      end record;
   type Buffer_Tree_View is access all Buffer_Tree_View_Record'Class;
   overriding function Is_Visible
     (Self       : not null access Buffer_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean;

   type Buffer_View_Record is new Generic_Views.View_Record with record
      Tree              : Buffer_Tree_View;
      Child_Selected_Id : Gtk.Handlers.Handler_Id;
   end record;
   overriding procedure Create_Menu
     (View    : not null access Buffer_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Create_Toolbar
     (Self    : not null access Buffer_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Buffer_View_Record;
      Pattern : in out Search_Pattern_Access);

   function Initialize
     (View   : access Buffer_View_Record'Class) return Gtk_Widget;
   --  Create a new Buffer view

   Module_Name : constant String := "Windows_View";

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => Module_Name,
      View_Name          => "Windows",
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Formal_MDI_Child   => BV_Child_Record,
      Formal_View_Record => Buffer_View_Record,
      Areas              => Gtkada.MDI.Sides_Only);
   use Generic_View;
   subtype Buffer_View_Access is Generic_View.View_Access;

   procedure Child_Selected (View : access Gtk_Widget_Record'Class);
   --  Called when a new child is selected

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences change

   type On_VCS_Status_Changed is new Vcs_File_Status_Hooks_Function with record
      View : access Buffer_View_Record'Class;
   end record;
   overriding procedure Execute
     (Self   : On_VCS_Status_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Vcs    : not null access Abstract_VCS_Engine'Class;
      Files  : Basic_Types.File_Sets.Set;
      Props  : VCS_File_Properties);
   --  Called when the vcs status of a file changed

   procedure Refresh (View : access Gtk_Widget_Record'Class);
   --  Refresh the contents of the Buffer view

   function Button_Press
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean;
   --  Callback for the "button_press" event

   function Get_Path_At_Event
     (Self  : Buffer_View_Access;
      Event : Gdk_Event_Button) return Gtk_Tree_Path;
   --  Return the path at which Event has occured.
   --  The path referenced the filter model, not the underlying model.
   --  User must free memory associated to the returned path.

   type Close_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close the selected editors

   ---------------
   -- Searching --
   ---------------

   type Opened_Windows_Search is new Kernel_Search_Provider with record
      Pattern : Search_Pattern_Access;
      Iter    : Child_Iterator;
   end record;
   type Opened_Windows_Search_Access is
      access all Opened_Windows_Search'Class;
   overriding procedure Set_Pattern
      (Self     : not null access Opened_Windows_Search;
       Pattern  : not null access GPS.Search.Search_Pattern'Class;
       Limit    : Natural := Natural'Last);
   overriding procedure Next
      (Self     : not null access Opened_Windows_Search;
       Result   : out GPS.Search.Search_Result_Access;
       Has_Next : out Boolean);
   overriding function Display_Name
      (Self     : not null access Opened_Windows_Search) return String
      is (Provider_Opened_Win);
   overriding function Documentation
      (Self     : not null access Opened_Windows_Search) return String;
   overriding function Complete_Suffix
     (Self      : not null access Opened_Windows_Search;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

   type Opened_Windows_Result is new Kernel_Search_Result with null record;
   overriding procedure Execute
      (Self       : not null access Opened_Windows_Result;
       Give_Focus : Boolean);
   overriding function Full
      (Self       : not null access Opened_Windows_Result) return Gtk_Widget;

   --------------
   -- Tooltips --
   --------------

   type Buffer_View_Tooltip_Handler is
     new Tooltips.Tooltip_Handler with null record;
   overriding function Create_Contents
     (Tooltip  : not null access Buffer_View_Tooltip_Handler;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Buffer_View_Tooltip_Handler;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      Tree  : constant Buffer_Tree_View := Buffer_Tree_View (Widget);
      Filter_Iter  : Gtk_Tree_Iter;
      Area         : Gdk_Rectangle;
      Label        : Gtk_Label;
      Child        : MDI_Child;

   begin
      Initialize_Tooltips (Tree, X, Y, Area, Filter_Iter);
      if Filter_Iter /= Null_Iter then
         Tooltip.Set_Tip_Area (Area);

         Child := Find_MDI_Child_By_Name
           (Get_MDI (Tree.Kernel),
            Tree.Filter.Get_String (Filter_Iter, Data_Column));
         if Child /= null then
            declare
               Tip : constant String := Child.Get_Tooltip;
            begin
               if Tip /= "" then
                  Gtk_New (Label, Tip);
                  Label.Set_Use_Markup (Child.Get_Tooltip_Is_Markup);
               end if;
            end;
         end if;

         if Label = null then
            Gtk_New (Label, Tree.Filter.Get_String (Filter_Iter, Name_Column));
         end if;
      end if;

      return Gtk_Widget (Label);
   end Create_Contents;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel      : constant Kernel_Handle := Get_Kernel (Context.Context);
      View        : constant Buffer_View_Access :=
        Generic_View.Retrieve_View (Kernel);
      Child       : MDI_Child;
      Iter, Iter2 : Gtk_Tree_Iter;
      Count       : Natural := 0;
      CIter       : Child_Iterator := First_Child (Get_MDI (Kernel));
   begin
      if View = null then
         return Commands.Failure;
      end if;

      while Get (CIter) /= null loop
         Count := Count + 1;
         Next (CIter);
      end loop;

      declare
         Children : MDI_Child_Array (1 .. Count);
      begin
         Count := Children'First;

         Iter := View.Tree.Filter.Get_Iter_First;
         while Iter /= Null_Iter loop
            Iter2 := View.Tree.Filter.Children (Iter);
            while Iter2 /= Null_Iter loop
               if View.Tree.Get_Selection.Iter_Is_Selected (Iter2) then
                  Child := Find_MDI_Child_By_Name
                    (Get_MDI (Kernel),
                     View.Tree.Filter.Get_String (Iter2, Data_Column));
                  if Child /= null then
                     Children (Count) := Child;
                     Count := Count + 1;
                  end if;
               end if;

               View.Tree.Filter.Next (Iter2);
            end loop;

            if View.Tree.Get_Selection.Iter_Is_Selected (Iter) then
               Child := Find_MDI_Child_By_Name
                 (Get_MDI (Kernel),
                  View.Tree.Filter.Get_String (Iter, Data_Column));
               if Child /= null then
                  Children (Count) := Child;
                  Count := Count + 1;
               end if;
            end if;

            View.Tree.Filter.Next (Iter);
         end loop;

         for C in Children'Range loop
            if Children (C) /= null then
               Close_Child (Children (C));
            end if;
         end loop;
      end;

      return Success;
   end Execute;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : not null access Buffer_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean is
   begin
      return Self.Pattern = null
        or else Self.Pattern.Start
          (Self.Model.Get_String (Store_Iter, Name_Column))
            /= GPS.Search.No_Match;
   end Is_Visible;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Buffer_View_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      GPS.Search.Free (Self.Tree.Pattern);
      Self.Tree.Pattern := Pattern;
      Self.Tree.Refilter;  --  Recompute visibility of rows
   end Filter_Changed;

   -----------------------
   -- Get_Path_At_Event --
   -----------------------

   function Get_Path_At_Event
     (Self  : Buffer_View_Access;
      Event : Gdk_Event_Button) return Gtk_Tree_Path
   is
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;
   begin
      Get_Path_At_Pos
        (Self.Tree, Gint (Event.X), Gint (Event.Y),
         Path, Column, Buffer_X, Buffer_Y, Row_Found);
      return Path;
   end Get_Path_At_Event;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Button) return Boolean
   is
      Explorer : constant Buffer_View_Access := Buffer_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Filter_Path : Gtk_Tree_Path;
      Filter_Iter : Gtk_Tree_Iter;
      Child    : MDI_Child;
   begin
      if (Event.State and (Shift_Mask or Control_Mask)) /= 0 then
         --  If there is a ctrl or shift key modifier present, grab the focus
         --  on the tree so that ctrl-clicking and shift-clicking extend the
         --  multiple selection as expected.
         Grab_Focus (Explorer.Tree);
         return False;
      end if;

      Filter_Path := Get_Path_At_Event (Explorer, Event);
      if Filter_Path /= Null_Gtk_Tree_Path then
         Filter_Iter := Explorer.Tree.Filter.Get_Iter (Filter_Path);
         Path_Free (Filter_Path);

         --  Only for actual windows
         if Explorer.Tree.Filter.Children (Filter_Iter) = Null_Iter then

            Child := Find_MDI_Child_By_Name
              (Get_MDI (Kernel),
               Explorer.Tree.Filter.Get_String (Filter_Iter, Data_Column));

            if Event.Button = 3 then
               --  Right click ?
               return False;

            elsif Event.Button = 1 then
               if Event.The_Type = Gdk_2button_Press then
                  Raise_Child (Child, Give_Focus => True);
               elsif Event.The_Type = Button_Press then
                  Child_Drag_Begin
                    (Child => Child, Event => Event,
                     Areas => Child.Get_Allowed_Areas);
                  Raise_Child (Child, Give_Focus => True);
               end if;

               return True;
            end if;
         end if;
      end if;

      return False;
   end Button_Press;

   --------------------
   -- Child_Selected --
   --------------------

   procedure Child_Selected (View : access Gtk_Widget_Record'Class) is
      V     : constant Buffer_View_Access := Buffer_View_Access (View);
      Child : constant MDI_Child := Get_Focus_Child (Get_MDI (V.Kernel));
      Iter  : Gtk_Tree_Iter;
      Iter2 : Gtk_Tree_Iter;
   begin
      if Child = null then
         return;
      end if;

      Trace (Me, "Child_Selected " & Get_Title (Child));

      --  If we are in the buffers view, do not show it, since otherwise that
      --  breaks the selection of multiple lines

      if MDI_Child (Generic_View.Child_From_View (V)) /= Child then
         declare
            Selected : constant String := Get_Title (Child);
         begin
            V.Tree.Get_Selection.Unselect_All;

            Iter := V.Tree.Model.Get_Iter_First;
            while Iter /= Null_Iter loop
               Iter2 := V.Tree.Model.Children (Iter);

               if Iter2 = Null_Iter then
                  if V.Tree.Model.Get_String
                    (Iter, Data_Column) = Selected
                  then
                     V.Tree.Get_Selection.Select_Iter
                       (V.Tree.Convert_To_Filter_Iter (Iter));
                     exit;
                  end if;

               else
                  while Iter2 /= Null_Iter loop
                     if V.Tree.Model.Get_String (Iter2, Data_Column) =
                       Selected
                     then
                        V.Tree.Get_Selection.Select_Iter
                          (V.Tree.Convert_To_Filter_Iter (Iter2));
                        return;
                     end if;
                     V.Tree.Model.Next (Iter2);
                  end loop;
               end if;

               V.Tree.Model.Next (Iter);
            end loop;
         end;
      end if;
   end Child_Selected;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Gtk_Widget_Record'Class) is
      V       : constant Buffer_View_Access := Buffer_View_Access (View);
      P_Editors_Only   : constant Boolean := Editors_Only.Get_Pref;
      P_Show_Notebooks : constant Boolean := Show_Notebooks.Get_Pref;
      P_Hide_Empty     : constant Boolean := Hide_Empty_Notebooks.Get_Pref;
      P_Show_VCS       : constant Boolean := Show_Vcs_Status.Get_Pref;

      Notebook_Index      : Integer := -1;
      Notebook_Store_Iter : Gtk_Tree_Iter := Null_Iter;

      procedure Show_Child (Parent : Gtk_Tree_Iter; Child : MDI_Child);
      --  Insert the line for Child in the view

      procedure Purify;
      --  Clean up the tree so that we do not show empty notebook or
      --  notebooks with a single child

      ------------
      -- Purify --
      ------------

      procedure Purify is
         Iter2 : Gtk_Tree_Iter;
      begin
         if Notebook_Store_Iter /= Null_Iter then
            Iter2 := V.Tree.Model.Children (Notebook_Store_Iter);

            if Iter2 = Null_Iter then
               if P_Hide_Empty then
                  --  If we had an empty notebook, remove it
                  V.Tree.Model.Remove (Notebook_Store_Iter);
               else
                  Notebook_Index := Notebook_Index + 1;
               end if;

            elsif V.Tree.Model.N_Children (Notebook_Store_Iter) = 1 then
               if P_Hide_Empty then
                  --  Single child ?
                  Set_And_Clear
                    (V.Tree.Model, Notebook_Store_Iter,
                     (File_Icon_Column, Name_Column,
                      Data_Column, VCS_Icon_Column),
                     (1 => As_String
                        (V.Tree.Model.Get_String (Iter2, File_Icon_Column)),
                      2 => As_String
                        (V.Tree.Model.Get_String (Iter2, Name_Column)),
                      3 => As_String
                        (V.Tree.Model.Get_String (Iter2, Data_Column)),
                      4 => As_String
                        (V.Tree.Model.Get_String (Iter2, VCS_Icon_Column))));

                  V.Tree.Model.Remove (Iter2);
               else
                  Notebook_Index := Notebook_Index + 1;
               end if;

            else
               Notebook_Index := Notebook_Index + 1;
            end if;
         else
            Notebook_Index := Notebook_Index + 1;
         end if;
      end Purify;

      ----------------
      -- Show_Child --
      ----------------

      procedure Show_Child (Parent : Gtk_Tree_Iter; Child : MDI_Child) is
         Name       : constant String := Get_Short_Title (Child);
         VCS        : constant Abstract_VCS_System_Access := V.Kernel.VCS;
         Iter       : Gtk_Tree_Iter;
         File       : Virtual_File;
         VCS_Engine : Abstract_VCS_Engine_Access;
         VCS_Icon   : Unbounded_String;
      begin
         if not P_Editors_Only
           or else Is_Source_Box (Child)
         then
            V.Tree.Model.Append (Iter, Parent);
            if Name = "" then
               Set_And_Clear
                 (V.Tree.Model, Iter, (Name_Column, Data_Column),
                  (As_String (Untitled), As_String (Untitled)));

            else
               if P_Show_VCS and then VCS /= null then
                  File :=
                    Create_From_Base (Filesystem_String (Get_Title (Child)));
                  VCS_Engine := Guess_VCS_For_Directory (VCS, Dir (File));
                  VCS_Icon :=
                    VCS_Engine.Get_Display
                      (VCS_Engine.Get_VCS_File_Status (File)).Icon_Name;
               end if;

               Set_And_Clear
                 (V.Tree.Model, Iter,
                  (File_Icon_Column, Name_Column,
                   Data_Column, VCS_Icon_Column),
                  (1 => As_String (Get_Icon_Name (Child)),
                   2 => As_String (Name),
                   3 => As_String (Get_Title (Child)),
                   4 => As_String (To_String (VCS_Icon))));
            end if;

            if Child = Get_Focus_Child (Get_MDI (V.Kernel)) then
               declare
                  Path : constant Gtk_Tree_Path :=
                    V.Tree.Model.Get_Path (Iter);

               begin
                  V.Tree.Get_Selection.Select_Iter
                    (V.Tree.Convert_To_Filter_Iter (Iter));
                  V.Tree.Scroll_To_Cell
                    (Path      => Path,
                     Column    => null,
                     Use_Align => False,
                     Row_Align => 0.0,
                     Col_Align => 0.0);
                  Path_Free (Path);
               end;
            end if;
         end if;
      end Show_Child;

      I_Child          : Gtkada.MDI.Child_Iterator;
      Child            : MDI_Child;
      Column           : Gint;
      Current_Notebook : Gtk_Notebook;
      pragma Unreferenced (Column);

   begin
      if Get_MDI (V.Kernel) = null then
         return;
      end if;

      V.Tree.Model.Clear;
      Column := V.Tree.Model.Freeze_Sort;

      I_Child := First_Child
        (Get_MDI (V.Kernel), Group_By_Notebook => P_Show_Notebooks);

      loop
         Child := Get (I_Child);
         exit when Child = null;

         if P_Show_Notebooks then
            if Notebook_Index = -1
              or else Get_Notebook (I_Child) /= Current_Notebook
            then
               Purify;
               Current_Notebook := Get_Notebook (I_Child);
               V.Tree.Model.Append (Notebook_Store_Iter, Null_Iter);
               V.Tree.Model.Set
                 (Notebook_Store_Iter, Name_Column,
                  -"Notebook" & Integer'Image (Notebook_Index + 1));
            end if;

            Show_Child (Notebook_Store_Iter, Child);

         else
            Show_Child (Null_Iter, Child);
         end if;

         Next (I_Child);
      end loop;

      if P_Show_Notebooks then
         Purify;
      end if;

      Expand_All (V.Tree);

      if V.Tree.Pattern /= null then
         V.Tree.Refilter;
      end if;

      if Sort_Alphabetical.Get_Pref then
         V.Tree.Model.Thaw_Sort (1);
      end if;
   end Refresh;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Buffer_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, View.Kernel, Editors_Only);
      Append_Menu (Menu, View.Kernel, Sort_Alphabetical);
      Append_Menu (Menu, View.Kernel, Show_Notebooks);
      Append_Menu (Menu, View.Kernel, Hide_Empty_Notebooks);
      Append_Menu (Menu, View.Kernel, Show_Vcs_Status);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (Self    : not null access Buffer_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Self.Build_Filter
        (Toolbar,
         Hist_Prefix => "windows",
         Tooltip     => -"Filter the contents of the Windows view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
   end Create_Toolbar;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access BV_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Context : constant Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      V       : constant Buffer_View_Access :=
        Buffer_View_Access (GPS_MDI_Child (Self).Get_Actual_Widget);
      Iter    : Gtk_Tree_Iter;
   begin
      --  Focus on the window, so that the selection is correctly taken into
      --  account. But do not process the usual callback, since we do not want
      --  to unselect everything and select the Windows View itself
      Handler_Block (Get_MDI (V.Kernel), V.Child_Selected_Id);
      Raise_Child (Generic_View.Child_From_View (V));
      Handler_Unblock (Get_MDI (V.Kernel), V.Child_Selected_Id);

      if Event /= null then
         Iter := Find_Iter_For_Event (V.Tree, Event);

         if Iter /= Null_Iter then
            --  Nothing special in the context, just the module itself so that
            --  people can still add information if needed
            if not Iter_Is_Selected (Get_Selection (V.Tree), Iter) then
               Unselect_All (Get_Selection (V.Tree));
               Select_Iter (Get_Selection (V.Tree), Iter);
            end if;
         end if;
      end if;

      return Context;
   end Build_Context;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Buffer_View_Record'Class) return Gtk_Widget
   is
      Tooltip  : Tooltips.Tooltip_Handler_Access;
      Scrolled : Gtk_Scrolled_Window;
      Col      : Gtk_Tree_View_Column;
      Text     : Gtk_Cell_Renderer_Text;
      Icon     : Gtk_Cell_Renderer_Pixbuf;
      Dummy    : Gint;
   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      View.Pack_Start (Scrolled, Expand => True, Fill => True);

      View.Tree := new Buffer_Tree_View_Record;
      View.Tree.Kernel := View.Kernel;
      Initialize
        (View.Tree,
         Column_Types       => Column_Types,
         Capability_Type    => Filtered,
         Set_Visible_Func   => True);
      Scrolled.Add (View.Tree);
      View.Tree.Set_Headers_Visible (False);
      View.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      View.Tree.Set_Search_Column (Name_Column);

      Gtk_New (Col);
      Dummy := View.Tree.Append_Column (Col);
      Col.Set_Sort_Column_Id (Name_Column);

      --  VCS status icon
      Gtk_New (Icon);
      Col.Pack_Start (Icon, False);
      Col.Add_Attribute (Icon, "icon-name", VCS_Icon_Column);

      --  File status icon
      Gtk_New (Icon);
      Col.Pack_Start (Icon, False);
      Col.Add_Attribute (Icon, "icon-name", File_Icon_Column);

      Gtk_New (Text);
      Col.Pack_Start (Text, True);
      Col.Add_Attribute (Text, "text", Name_Column);

      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Child_Added,
         Refresh'Access, Slot_Object => View);
      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Child_Removed, Refresh'Access,
         Slot_Object => View);
      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel),
         Signal_Child_Title_Changed, Refresh'Access, View);
      View.Child_Selected_Id := Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Child_Selected,
         Widget_Callback.To_Marshaller (Child_Selected'Access), View);
      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Child_Icon_Changed,
         Refresh'Access, View);
      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Float_Child, Refresh'Access, View);
      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Unfloat_Child, Refresh'Access, View);
      Widget_Callback.Object_Connect
        (Get_MDI (View.Kernel), Signal_Children_Reorganized, Refresh'Access,
         View);

      View.Tree.On_Button_Press_Event (Button_Press'Access, View);

      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View.Tree);

      Set_Font_And_Colors (View.Tree, Fixed_Font => True);
      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => View);
      Vcs_File_Status_Changed_Hook.Add
        (new On_VCS_Status_Changed'
           (Vcs_File_Status_Hooks_Function with View => View),
         Watch => View);

      --  Initialize tooltips

      Tooltip := new Buffer_View_Tooltip_Handler;
      Tooltip.Associate_To_Widget (View.Tree);

      Refresh (View);

      return Gtk_Widget (View.Tree);
   end Initialize;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
      (Self     : not null access Opened_Windows_Search;
       Pattern  : not null access GPS.Search.Search_Pattern'Class;
       Limit    : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.Iter    := First_Child (Get_MDI (Self.Kernel));
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
      (Self     : not null access Opened_Windows_Search;
       Result   : out GPS.Search.Search_Result_Access;
       Has_Next : out Boolean)
   is
      C     : Search_Context;
      Child : constant MDI_Child := Get (Self.Iter);
   begin
      Result := null;

      if Child = null then
         Has_Next := False;
      else
         declare
            Short_Name : constant String := Child.Get_Short_Title;
         begin
            C := Self.Pattern.Start (Child.Get_Short_Title);
            if C /= GPS.Search.No_Match then
               Result := new Opened_Windows_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => C.Score,
                  Short    => new String'
                    (Self.Pattern.Highlight_Match (Short_Name, Context => C)),
                  Long     => new String'
                    (Glib.Convert.Escape_Text (Short_Name)),
                  Id       => new String'(Child.Get_Title));

               Self.Adjust_Score (Result);
            end if;
         end;

         Next (Self.Iter);
         Has_Next := Get (Self.Iter) /= null;
      end if;
   end Next;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Opened_Windows_Search;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      Suffix      : Unbounded_String;
      Suffix_Last : Natural := 0;
      C           : Search_Context;
      Child       : MDI_Child;
   begin
      Self.Set_Pattern (Pattern);

      loop
         Child := Get (Self.Iter);
         exit when Child = null;

         C := Self.Pattern.Start (Child.Get_Short_Title);
         if C /= GPS.Search.No_Match then
            Self.Pattern.Compute_Suffix
              (C, Child.Get_Short_Title, Suffix, Suffix_Last);
            exit when Suffix_Last = 0;

         elsif Child.Get_Short_Title /= Child.Get_Title then
            C := Self.Pattern.Start (Child.Get_Title);
            if C /= GPS.Search.No_Match then
               Self.Pattern.Compute_Suffix
                 (C, Child.Get_Title, Suffix, Suffix_Last);
               exit when Suffix_Last = 0;
            end if;
         end if;

         Next (Self.Iter);
      end loop;

      return Slice (Suffix, 1, Suffix_Last);
   end Complete_Suffix;

   -------------------
   -- Documentation --
   -------------------

   overriding function Documentation
      (Self     : not null access Opened_Windows_Search) return String
   is
      pragma Unreferenced (Self);
   begin
      return "Search amongst opened windows";
   end Documentation;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self       : not null access Opened_Windows_Result;
       Give_Focus : Boolean)
   is
      C : constant MDI_Child :=
        Find_MDI_Child_By_Name (Get_MDI (Self.Kernel), Self.Id.all);
   begin
      if C /= null then
         Raise_Child (C, Give_Focus => Give_Focus);
      end if;
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
      (Self       : not null access Opened_Windows_Result) return Gtk_Widget
   is
      pragma Unreferenced (Self);
   begin
      return null;
   end Full;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      V  : constant Buffer_View_Access := Generic_View.Retrieve_View (Kernel);
   begin
      if V /= null then
         Set_Font_And_Colors (V.Tree, Fixed_Font => True, Pref => Pref);

         if Pref = null
           or else Pref = Preference (Editors_Only)
           or else Pref = Preference (Show_Notebooks)
           or else Pref = Preference (Sort_Alphabetical)
           or else Pref = Preference (Hide_Empty_Notebooks)
           or else Pref = Preference (Show_Vcs_Status)
         then
            Refresh (V);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_VCS_Status_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Vcs    : not null access Abstract_VCS_Engine'Class;
      Files  : Basic_Types.File_Sets.Set;
      Props  : VCS_File_Properties)
   is
      pragma Unreferenced (Kernel);
      Tree   : constant Buffer_Tree_View := Self.View.Tree;
      Model  : constant Gtk_Tree_Store   := Tree.Model;
      Iter   : Gtk_Tree_Iter             := Get_Iter_First (Model);
      Status : constant Status_Display   := Vcs.Get_Display (Props.Status);
      File   : Virtual_File;
   begin
      if not Show_Vcs_Status.Get_Pref then
         return;
      end if;

      while Iter /= Null_Iter loop
         --  The files visible in the Windows view are already created
         --  thus the following statement only retrieved a Virtual_File

         File :=
           Create_From_Base
             (Filesystem_String (Get_String (Model, Iter, Data_Column)));

         if Files.Contains (File) then
            Self.View.Tree.Model.Set
              (Iter, VCS_Icon_Column,
               UTF8_String'(To_String (Status.Icon_Name)));
         end if;

         Next (Model, Iter);
      end loop;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      P : Opened_Windows_Search_Access;
   begin
      Generic_View.Register_Module (Kernel);

      Editors_Only := Kernel.Get_Preferences.Create_Invisible_Pref
        ("windows-view-editors-only", True,
         Label => "Show editors only");
      Show_Notebooks := Kernel.Get_Preferences.Create_Invisible_Pref
        ("windows-view-show-notebooks", False,
         Label => "Show notebooks");
      Sort_Alphabetical := Kernel.Get_Preferences.Create_Invisible_Pref
        ("windows-view-sort-alphabetical", True,
         Label => "Sort alphabetically",
         Doc   =>
           -("Sort names alphabetically, if true. Otherwise preserve the"
             & " order of notebook tabs (or in last-focus order when"
             & " notebooks are not displayed"));
      Hide_Empty_Notebooks := Kernel.Get_Preferences.Create_Invisible_Pref
        ("windows-view-hide-empty-notebooks", True,
         Label => "Hide empty notebooks",
         Doc   => -"Hide notebook nodes with one window or less");
      Show_Vcs_Status := Kernel.Get_Preferences.Create_Invisible_Pref
        ("windows-view-show-vcs-status", True,
         Label => "Show VCS status",
         Doc   => -"Show VCS status in the Windows View.");

      Register_Action
        (Kernel, "Windows view close selected",
         new Close_Command,
         -"Close all windows currently selected in the Windows view",
         Icon_Name => "gps-remove-symbolic",
         Category  => -"Windows view");

      P := new Opened_Windows_Search;
      Register_Provider_And_Action (Kernel, P);
   end Register_Module;

end Buffer_Views;
