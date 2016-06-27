------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                    use System;

with GNATCOLL.Scripts;          use GNATCOLL.Scripts;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNAT.Strings;              use GNAT.Strings;

with Glib;                      use Glib;
with Glib.Main;                 use Glib.Main;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Gdk.Event;                 use Gdk.Event;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences;       use Default_Preferences;
with Generic_Views;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Search;         use GPS.Kernel.Search;
with GPS.Markers;               use GPS.Markers;
with GPS.Search.GUI;            use GPS.Search.GUI;
with GPS.Intl;                  use GPS.Intl;
with GPS.Scripts;               use GPS.Scripts;
with GPS.Search;                use GPS.Search;
with GUI_Utils;                 use GUI_Utils;
with Generic_List;
with Tooltips;
with XML_Parsers;               use XML_Parsers;
with XML_Utils;                 use XML_Utils;

package body Bookmark_Views is

   Me : constant Trace_Handle := Create ("Bookmarks");

   Bookmark_Class_Name : constant String := "Bookmark";

   Icon_Name_Column : constant := 0;
   Name_Column      : constant := 1;
   Data_Column      : constant := 2;
   Editable_Column  : constant := 3;

   Column_Types : constant GType_Array :=
     (Icon_Name_Column => GType_Icon_Name_String,
      Name_Column      => GType_String,
      Data_Column      => GType_Pointer,
      Editable_Column  => GType_Boolean);

   type Bookmark_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : Bookmark_Proxy) return String
      is (Bookmark_Class_Name) with Inline;

   type Bookmark_Data is record
      Marker    : Location_Marker;
      Name      : GNAT.Strings.String_Access;
      Instances : Bookmark_Proxy;
   end record;
   type Bookmark_Data_Access is access Bookmark_Data;

   package Bookmark_Proxies is new Script_Proxies
      (Bookmark_Data_Access, Bookmark_Proxy);

   procedure Free (Data : in out Bookmark_Data_Access);
   package Bookmark_List is new Generic_List (Bookmark_Data_Access, Free);
   use Bookmark_List;

   type Bookmark_Views_Module_Record is new Module_ID_Record with record
      List : Bookmark_List.List;
   end record;
   type Bookmark_Views_Module_Access
     is access all Bookmark_Views_Module_Record'Class;
   overriding procedure Destroy (Module : in out Bookmark_Views_Module_Record);

   Bookmark_Views_Module : Bookmark_Views_Module_Access;

   type Bookmark_View_Record is new Generic_Views.View_Record with record
      Tree      : Gtk_Tree_View;
      Deleting  : Boolean := False;
      --  Whether we are deleting multiple bookmarks
   end record;

   package Bookmarks_Selection_Foreach is new
     Gtk.Tree_Selection.Selected_Foreach_User_Data (Bookmark_View_Record);
   use Bookmarks_Selection_Foreach;

   function Initialize
     (View   : access Bookmark_View_Record'Class) return Gtk_Widget;
   --  Create a new Bookmark view

   type Bookmark_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Bookmark_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Bookmark_View",
      View_Name          => "Bookmarks",
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Formal_MDI_Child   => Bookmark_Child_Record,
      Formal_View_Record => Bookmark_View_Record);
   use Generic_View;
   subtype Bookmark_View_Access is Generic_View.View_Access;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Bookmark_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Bookmark_Data_Access, System.Address);

   procedure Refresh (View : access Bookmark_View_Record'Class);
   --  Refresh the contents of the Bookmark view

   procedure Delete_Bookmark
     (Kernel   : access Kernel_Handle_Record'Class;
      Bookmark : Bookmark_Data);
   --  Delete an existing bookmark

   function Bookmark_From_Name
     (Name : String) return Bookmark_List.List_Node;
   --  Return the location marker for the first bookmark named Name.
   --  null is returned if not found

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called every time a row is clicked

   procedure Load_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   --  Load or save the bookmarks from the XML file

   procedure Edited_Callback
     (V      : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Called when a line is edited in the view

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for this module

   package Bookmark_Idle is new Glib.Main.Generic_Sources
     (Bookmark_View_Access);
   use Bookmark_Idle;
   function Start_Editing_Idle (View : Bookmark_View_Access) return Boolean;
   --  Function called to start editing the selected line. This is necessary
   --  since any editing is stopped as soon as the tree gains the focus back.

   type Refresh_Hook is new String_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : Refresh_Hook;
      Kernel : not null access Kernel_Handle_Record'Class;
      Name   : String);
   --  Function called when a hook has been added or removed, so that we can
   --  properly refresh the view.

   type Delete_Bookmark_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Delete_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Delete the selected bookmark

   type Create_Bookmark_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Create_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Create a new bookmark

   type Rename_Bookmark_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Rename_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Rename the selected bookmark

   type Next_Bookmark_Command (Backward : Boolean) is
     new Interactive_Command with null record;

   overriding function Execute
     (Command : access Next_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to next bookmark in current file

   --------------
   -- Tooltips --
   --------------

   type Bookmark_View_Tooltips is new Tooltips.Tooltips
     with null record;
   overriding function Create_Contents
     (Tooltip  : not null access Bookmark_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   ------------
   -- Search --
   ------------

   type Bookmarks_Search_Provider is new Kernel_Search_Provider
   with record
      Pattern : GPS.Search.Search_Pattern_Access;
      List    : Bookmark_List.List_Node;
   end record;
   overriding function Documentation
     (Self    : not null access Bookmarks_Search_Provider) return String
     is ("Search amongst all bookmarks");
   overriding procedure Set_Pattern
     (Self    : not null access Bookmarks_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Bookmarks_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Bookmarks_Search_Provider) return String
     is (Provider_Bookmarks);
   overriding function Complete_Suffix
     (Self      : not null access Bookmarks_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

   type Bookmarks_Search_Result is new Kernel_Search_Result with record
      Bookmark : Bookmark_Data_Access;
   end record;
   overriding procedure Execute
     (Self       : not null access Bookmarks_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access Bookmarks_Search_Result)
     return Gtk.Widget.Gtk_Widget;

   -----------------
   -- Set_Pattern --
   -----------------

   overriding procedure Set_Pattern
     (Self    : not null access Bookmarks_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last)
   is
      pragma Unreferenced (Limit);
   begin
      Self.Pattern := Search_Pattern_Access (Pattern);
      Self.List := First (Bookmark_Views_Module.List);
   end Set_Pattern;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self     : not null access Bookmarks_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean)
   is
      C        : Search_Context;
      Bookmark : Bookmark_Data_Access;
   begin
      if Self.List = Null_Node then
         Has_Next := False;
         Result := null;
      else
         Has_Next := True;
         Bookmark := Data (Self.List);

         declare
            Loc : constant String := To_String (Bookmark.Marker);
         begin
            C := Self.Pattern.Start (Bookmark.Name.all);
            if C /= GPS.Search.No_Match then
               Result := new Bookmarks_Search_Result'
                 (Kernel   => Self.Kernel,
                  Provider => Self,
                  Score    => C.Score,
                  Short    => new String'
                    (Self.Pattern.Highlight_Match (Bookmark.Name.all, C)),
                  Long     => new String'(Loc),
                  Id       => new String'(Bookmark.Name.all),
                  Bookmark => Bookmark);
               Self.Adjust_Score (Result);

            else
               C := Self.Pattern.Start (Loc);
               if C /= GPS.Search.No_Match then
                  Result := new Bookmarks_Search_Result'
                    (Kernel   => Self.Kernel,
                     Provider => Self,
                     Score    => C.Score,
                     Short    => new String'(Bookmark.Name.all),
                     Long     => new String'
                       (Self.Pattern.Highlight_Match (Loc, C)),
                     Id       => new String'(Bookmark.Name.all),
                     Bookmark => Bookmark);
                  Self.Adjust_Score (Result);
               end if;
            end if;
         end;

         Self.List := Next (Self.List);
      end if;
   end Next;

   ---------------------
   -- Complete_Suffix --
   ---------------------

   overriding function Complete_Suffix
     (Self      : not null access Bookmarks_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Suffix      : Unbounded_String;
      Suffix_Last : Natural := 0;
      C           : Search_Context;
      Bookmark    : Bookmark_Data_Access;
   begin
      Self.Set_Pattern (Pattern);

      while Self.List /= Null_Node loop
         Bookmark := Data (Self.List);

         C := Self.Pattern.Start (Bookmark.Name.all);
         if C /= GPS.Search.No_Match then
            Self.Pattern.Compute_Suffix
              (C, Bookmark.Name.all, Suffix, Suffix_Last);
            exit when Suffix_Last = 0;
         end if;

         Self.List := Next (Self.List);
      end loop;

      return Slice (Suffix, 1, Suffix_Last);
   end Complete_Suffix;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Bookmarks_Search_Result;
      Give_Focus : Boolean)
   is
      Ignore : Boolean;
      pragma Unreferenced (Ignore, Give_Focus);
   begin
      if Self.Bookmark /= null then
         Ignore := Go_To (Self.Bookmark.Marker);
         Push_Marker_In_History (Self.Kernel, Self.Bookmark.Marker);
      end if;
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self       : not null access Bookmarks_Search_Result)
      return Gtk.Widget.Gtk_Widget
   is
      Label : Gtk_Label;
   begin
      Gtk_New
        (Label,
         "<b>Name:</b> " & Self.Bookmark.Name.all & ASCII.LF &
         "<b>Location:</b> " & To_String (Self.Bookmark.Marker));
      Label.Set_Use_Markup (True);
      return Gtk_Widget (Label);
   end Full;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out Bookmark_Views_Module_Record) is
   begin
      --  Even though we are saving after every explicit modification, we
      --  should still save on exit to memorize the new location where a
      --  bookmark ends up after the buffer has been edited.
      --  ??? Would be better to do it when a buffer is closed (or even when
      --  it is modified). If GPS crashes, we would lose bookmarks for open
      --  files, but not for files that have been closed in between...

      Save_Bookmarks (Get_Kernel (Module));
   end Destroy;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Bookmark_View_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      Tree : constant Gtk_Tree_View := Gtk_Tree_View (Widget);
      Model : constant Gtk_Tree_Model := Get_Model (Tree);
      Iter  : Gtk_Tree_Iter;
      Data  : Bookmark_Data_Access;
      Label : Gtk_Label;
      Area  : Gdk_Rectangle;

   begin
      Tooltips.Initialize_Tooltips (Tree, X, Y, Area, Iter);

      if Iter /= Null_Iter then
         Data := Convert (Get_Address (Model, Iter, Data_Column));
         Tooltip.Set_Tip_Area (Area);

         declare
            Location : constant String := To_String (Data.Marker);
         begin
            if Location = Data.Name.all then
               Gtk_New (Label, "<b>Location:</b> " & Location);
            else
               Gtk_New
                 (Label,
                  "<b>Name:</b> " & Data.Name.all & ASCII.LF &
                  "<b>Location:</b> " & Location);
            end if;
            Label.Set_Use_Markup (True);
            return Gtk_Widget (Label);
         end;
      end if;
      return null;
   end Create_Contents;

   ---------------------
   -- Delete_Bookmark --
   ---------------------

   procedure Delete_Bookmark
     (Kernel   : access Kernel_Handle_Record'Class;
      Bookmark : Bookmark_Data)
   is
      Node, Prev  : List_Node;
   begin
      Node := First (Bookmark_Views_Module.List);
      while Node /= Null_Node loop
         --  Compare pointers to string directly. If they are the same, the
         --  pointers are the same anyway
         if Bookmark_List.Data (Node).Name = Bookmark.Name then
            declare
               Name : constant String := Bookmark_List.Data (Node).Name.all;
            begin
               Remove_Nodes (Bookmark_Views_Module.List, Prev, Node);
               Bookmark_Removed_Hook.Run (Kernel, Name);
               exit;
            end;
         end if;

         Prev := Node;
         Node := Next (Node);
      end loop;

      Save_Bookmarks (Kernel);
   end Delete_Bookmark;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Delete_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Bookmark_View_Access :=
                Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Model : Gtk_Tree_Model;
      Data  : Bookmark_Data_Access;
      Iter  : Gtk_Tree_Iter;
   begin
      if View /= null then
         View.Deleting := True;
         View.Tree.Get_Selection.Get_Selected (Model, Iter);

         if Iter /= Null_Iter then
            Data := Convert (Get_Address (Model, Iter, Data_Column));

            if Data /= null then
               Delete_Bookmark (Get_Kernel (Context.Context), Data.all);
               Remove (-Model, Iter);
            end if;
         end if;
         View.Deleting := False;
         Refresh (View);
      end if;

      return Success;
   end Execute;

   ------------------------
   -- Start_Editing_Idle --
   ------------------------

   function Start_Editing_Idle (View : Bookmark_View_Access) return Boolean is
      procedure Edit_Selected
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
      --  Foreach callback on a selection

      -------------------
      -- Edit_Selected --
      -------------------

      procedure Edit_Selected
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) is
         pragma Unreferenced (Model);
      begin
         if Iter /= Null_Iter then
            Set_Cursor
              (View.Tree,
               Path          => Path,
               Focus_Column  => Get_Column (View.Tree, 2),
               Start_Editing => True);
         end if;
      exception
         when E : others => Trace (Me, E);
      end Edit_Selected;

   begin
      View.Tree.Get_Selection.Selected_Foreach
        (Edit_Selected'Unrestricted_Access);
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Start_Editing_Idle;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Rename_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      View  : constant Bookmark_View_Access :=
                Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

      Ignore : G_Source_Id;
      pragma Unreferenced (Command, Ignore);
   begin
      if View /= null then
         View.Tree.Get_Selection.Get_Selected (Model, Iter);

         if Iter /= Null_Iter then
            --  Start the edition in idle mode, since otherwise the tree gains
            --  the focus when the menu is hidden, and stops the edition
            --  immediately.

            Ignore := Idle_Add (Start_Editing_Idle'Access, View,
                                Priority => Priority_High_Idle);
            return Success;
         end if;
      end if;
      return Failure;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Next_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Direction : constant array (Boolean) of Integer :=
        (False => 1, True => -1);

      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Marker  : constant Location_Marker := Create_Marker (Kernel);
      List    : Bookmark_List.List_Node := First (Bookmark_Views_Module.List);
      Nearest : Location_Marker;
      Min     : Integer := Integer'Last;
      Sign    : constant Integer := Direction (Command.Backward);
   begin
      while List /= Null_Node loop
         declare
            Next : constant Location_Marker := Data (List).Marker;
            Dist : constant Integer := Sign * Distance (Marker, Next);
         begin
            if Dist > 0 and abs Dist /= Integer'Last then
               if Min > Dist then
                  Min := Dist;
                  Nearest := Next;
               end if;
            end if;
         end;

         List := Next (List);
      end loop;

      if Min /= Integer'Last and then Go_To (Nearest) then
         return Success;
      else
         return Failure;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Bookmark_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Bookmark_Data, Bookmark_Data_Access);
   begin
      Data.Instances.Free;
      Free (Data.Name);
      Unchecked_Free (Data);
   end Free;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Bookmark_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Context : constant Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      V       : constant Bookmark_View_Access :=
        Bookmark_View_Access (GPS_MDI_Child (Self).Get_Actual_Widget);
      Iter    : Gtk_Tree_Iter;
   begin
      if Event /= null then
         Iter := Find_Iter_For_Event (V.Tree, Event);
         if Iter /= Null_Iter then
            Select_Iter (Get_Selection (V.Tree), Iter);
         end if;
      end if;
      return Context;
   end Build_Context;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Clip  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      View   : constant Bookmark_View_Access := Bookmark_View_Access (Clip);
      Path   : Gtk_Tree_Path;
      Model  : constant Gtk_Tree_Store := -Get_Model (View.Tree);
      Iter   : Gtk_Tree_Iter;
      Marker : Bookmark_Data_Access;
      Ignore : Boolean;
      pragma Unreferenced (Ignore);
   begin
      if (Get_State (Event) and
            (Primary_Mod_Mask or Control_Mask or Shift_Mask)) /= 0
      then
         --  If there is a ctrl or shift key modifier present, grab the focus
         --  on the tree so that ctrl-clicking and shift-clicking extend the
         --  multiple selection as expected.
         Grab_Focus (View.Tree);

      elsif Get_Button (Event) = 1 then
         Iter := Find_Iter_For_Event (View.Tree, Event);

         if Iter /= Null_Iter then
            --  Select the row that was clicked
            Path := Get_Path (Model, Iter);
            Set_Cursor (View.Tree, Path, null, False);
            Path_Free (Path);

            Marker := Convert (Get_Address (Model, Iter, Data_Column));

            if Marker /= null then
               Ignore := Go_To (Marker.Marker);
               Push_Marker_In_History (View.Kernel, Marker.Marker);

               --  Return True here to prevent focus from flickering between
               --  editor and bookmark view.

               return True;
            end if;
         end if;
      end if;

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Button_Press;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Create_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Mark   : constant Location_Marker := Create_Marker (Kernel);
      Child  : constant MDI_Child :=
                 Find_MDI_Child_By_Tag
                   (Get_MDI (Kernel), Bookmark_View_Record'Tag);
      View   : Bookmark_View_Access;
      Model  : Gtk_Tree_Store;
      Iter   : Gtk_Tree_Iter;
      Ignore : G_Source_Id;
      pragma Unreferenced (Ignore);

   begin
      if not Mark.Is_Null then
         Append (Bookmark_Views_Module.List,
                 new Bookmark_Data'
                   (Marker => Mark,
                    Name   => new String'(To_String (Mark)),
                    Instances => <>));

         if Child = null then
            View := Generic_View.Get_Or_Create_View (Kernel);
         else
            View := Bookmark_View_Access (Get_Widget (Child));
         end if;

         Model := -Get_Model (View.Tree);
         Refresh (View);
         Bookmark_Added_Hook.Run (Kernel, To_String (Mark));

         --  Start editing the name of the bookmark immediately

         Iter := Get_Iter_First (Model);
         while Iter /= Null_Iter loop
            if Convert (Get_Address (Model, Iter, Data_Column)).Marker =
              Mark
            then
               Unselect_All (Get_Selection (View.Tree));
               Select_Iter (Get_Selection (View.Tree), Iter);

               exit;
            end if;
            Next (Get_Model (View.Tree), Iter);
         end loop;

         --  Register a callback for editing the selected node

         Ignore := Idle_Add
           (Start_Editing_Idle'Access, View, Priority => Priority_Low);
         return Success;
      end if;
      return Failure;
   end Execute;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (View : access Bookmark_View_Record'Class) is
      Model : constant Gtk_Tree_Store := -Get_Model (View.Tree);
      List  : Bookmark_List.List_Node := First (Bookmark_Views_Module.List);
      Iter  : Gtk_Tree_Iter;

   begin
      if View.Deleting then
         return;
      end if;

      Clear (Model);

      while List /= Null_Node loop
         Append (Model, Iter, Null_Iter);

         Set_And_Clear
           (Model, Iter,
            (Icon_Name_Column, Name_Column, Data_Column, Editable_Column),
            (1 => As_String  ("gps-goto-symbolic"),
             2 => As_String  (Data (List).Name.all),
             3 => As_Pointer (Convert (Data (List))),
             4 => As_Boolean (True)));

         List := Next (List);
      end loop;
   end Refresh;

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (V      : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      View        : constant Gtk_Tree_View := Gtk_Tree_View (V);
      M           : constant Gtk_Tree_Store := -Get_Model (View);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Text_Value  : constant GValue := Nth (Params, 2);
      Iter        : Gtk_Tree_Iter;
      Mark        : Bookmark_Data_Access;
   begin
      Iter := Get_Iter_From_String (M, Path_String);
      Mark := Convert (Get_Address (M, Iter, Data_Column));
      Free (Mark.Name);
      Mark.Name := new String'(Get_String (Text_Value));
      Save_Bookmarks (Get_Kernel (Bookmark_Views_Module.all));
   end Edited_Callback;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Self);
      View : constant Bookmark_View_Access :=
               Generic_View.Get_Or_Create_View (Kernel, Focus => False);
   begin
      Set_Font_And_Colors (View.Tree, Fixed_Font => True, Pref => Pref);
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Bookmark_View_Record'Class) return Gtk_Widget
   is
      Tooltip   : Tooltips.Tooltips_Access;
      Scrolled  : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      View.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => Column_Types,
         Column_Names       => (1 => null, 2 => null),
         Editable_Columns   => (Name_Column => Editable_Column),
         Editable_Callback  => (Name_Column => Edited_Callback'Access),
         Show_Column_Titles => False,
         Sortable_Columns   => True,
         Initial_Sort_On    => 2,
         Merge_Icon_Columns => False,
         Hide_Expander      => True);
      Set_Name (View.Tree, "Bookmark TreeView"); --  For the testsuite
      Scrolled.Add (View.Tree);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View.Tree);

      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => View);
      Refresh (View);

      Bookmark_Added_Hook.Add (new Refresh_Hook, Watch => View);
      Bookmark_Removed_Hook.Add (new Refresh_Hook, Watch => View);

      Tooltip := new Bookmark_View_Tooltips;
      Tooltip.Set_Tooltip (View.Tree);

      return Gtk_Widget (View.Tree);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : Refresh_Hook;
      Kernel : not null access Kernel_Handle_Record'Class;
      Name   : String)
   is
      pragma Unreferenced (Self, Name);
      View : constant Bookmark_View_Access :=
          Generic_View.Retrieve_View (Kernel);
   begin
      if View /= null then
         Refresh (View);
      end if;
   end Execute;

   --------------------
   -- Load_Bookmarks --
   --------------------

   procedure Load_Bookmarks (Kernel : access Kernel_Handle_Record'Class) is
      Filename    : constant Virtual_File :=
                      Create_From_Dir (Get_Home_Dir (Kernel), "bookmarks.xml");
      File, Child : Node_Ptr;
      Err         : String_Access;
      Marker      : Location_Marker;
   begin
      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);

         else
            Child := File.Child;

            while Child /= null loop
               Marker := Create_Marker (Kernel, Child);

               if not Marker.Is_Null then
                  declare
                     Name : constant String :=
                              Get_Attribute (Child, "bookmark_name", "");
                  begin
                     if Name = "" then
                        Append
                          (Bookmark_Views_Module.List,
                           new Bookmark_Data'
                             (Marker    => Marker,
                              Instances => <>,
                              Name      => new String'(To_String (Marker))));
                     else
                        Append (Bookmark_Views_Module.List,
                                new Bookmark_Data'
                                  (Marker    => Marker,
                                   Instances => <>,
                                   Name      => new String'(Name)));
                     end if;
                  end;
               end if;

               Child := Child.Next;
            end loop;

            Free (File);
         end if;

         Bookmark_Added_Hook.Run (Kernel, "");
      end if;
   end Load_Bookmarks;

   --------------------
   -- Save_Bookmarks --
   --------------------

   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class) is
      Filename    : constant Virtual_File :=
                      Create_From_Dir (Get_Home_Dir (Kernel), "bookmarks.xml");
      File, Child : Node_Ptr;
      List        : Bookmark_List.List_Node :=
                      First (Bookmark_Views_Module.List);
      Success     : Boolean;
   begin
      Trace (Me, "Saving " & Filename.Display_Full_Name);
      File := new Node;
      File.Tag := new String'("Bookmarks");

      while List /= Null_Node loop
         Child := Save (Data (List).Marker);

         if Child /= null then
            Set_Attribute (Child, "bookmark_name",
                           Data (List).Name.all);
            Add_Child (File, Child, Append => True);
         end if;

         List := Next (List);
      end loop;

      Print (File, Filename, Success);
      Free (File);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Bookmarks;

   ------------------------
   -- Bookmark_From_Name --
   ------------------------

   function Bookmark_From_Name
     (Name : String) return Bookmark_List.List_Node
   is
      List : Bookmark_List.List_Node := First (Bookmark_Views_Module.List);
   begin
      while List /= Null_Node loop
         exit when Data (List).Name.all = Name;
         List := Next (List);
      end loop;
      return List;
   end Bookmark_From_Name;

   ---------------------
   -- Command_Handler --
   ---------------------

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel         : constant Kernel_Handle := Get_Kernel (Data);
      Bookmark_Class : constant Class_Type :=
         Kernel.Scripts.New_Class (Bookmark_Class_Name);
      Name_Cst       : aliased constant String := "name";
      Inst           : Class_Instance;
      Bookmark       : Bookmark_List.List_Node;
      Marker         : Location_Marker;
      Tmp            : Boolean;
      List           : Bookmark_List.List_Node;
      B              : Bookmark_Data_Access;
      pragma Unreferenced (Tmp);
   begin
      if Command = Constructor_Method then
         Set_Error_Msg
           (Data, "Cannot create instances of GPS.Bookmark."
            & " Use GPS.Bookmark.get() instead");

      elsif Command = "get" then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Bookmark := Bookmark_From_Name (Nth_Arg (Data, 1));
         if Bookmark = Null_Node then
            Set_Error_Msg (Data, "No such bookmark");
         else
            Data.Set_Return_Value
               (Bookmark_Proxies.Get_Or_Create_Instance
                  (Self   => Bookmark_List.Data (Bookmark).Instances,
                   Obj    => Bookmark_List.Data (Bookmark),
                   Script => Data.Get_Script));
         end if;

      elsif Command = "create" then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Marker := Create_Marker (Kernel);
         if Marker.Is_Null then
            Set_Error_Msg (Data, "Can't create bookmark for this context");
         else
            Prepend
              (Bookmark_Views_Module.List,
               new Bookmark_Data'
                 (Marker    => Marker,
                  Instances => <>,
                  Name      => new String'(Nth_Arg (Data, 1))));
            Save_Bookmarks (Kernel);
            Bookmark_Added_Hook.Run (Kernel, Data.Nth_Arg (1));
            Bookmark := First (Bookmark_Views_Module.List);
            Data.Set_Return_Value
               (Bookmark_Proxies.Get_Or_Create_Instance
                  (Self   => Bookmark_List.Data (Bookmark).Instances,
                   Obj    => Bookmark_List.Data (Bookmark),
                   Script => Data.Get_Script));
         end if;

      elsif Command = "name" then
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         Data.Set_Return_Value
            (Bookmark_Proxies.From_Instance (Inst).Name.all);

      elsif Command = "delete" then
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         B := Bookmark_Proxies.From_Instance (Inst);
         if B = null then
            Data.Set_Error_Msg ("Invalid bookmark");
         else
            Delete_Bookmark (Kernel, B.all);
         end if;

      elsif Command = "rename" then
         Name_Parameters (Data, (2 => Name_Cst'Unchecked_Access));
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         B := Bookmark_Proxies.From_Instance (Inst);
         if B = null then
            Data.Set_Error_Msg ("Invalid bookmark");
         else
            Bookmark_Added_Hook.Run (Kernel, B.Name.all);
            Free (B.Name);
            B.Name := new String'(Nth_Arg (Data, 2));
            Bookmark_Added_Hook.Run (Kernel, Data.Nth_Arg (2));
            Save_Bookmarks (Kernel);
         end if;

      elsif Command = "goto" then
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         B := Bookmark_Proxies.From_Instance (Inst);
         if B /= null
           and then Go_To (B.Marker)
         then
            Push_Marker_In_History (Kernel, B.Marker);
         else
            Data.Set_Error_Msg ("Invalid bookmark");
         end if;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         List := First (Bookmark_Views_Module.List);
         while List /= Null_Node loop
            Data.Set_Return_Value
               (Bookmark_Proxies.Get_Or_Create_Instance
                   (Self   => Bookmark_List.Data (List).Instances,
                    Obj    => Bookmark_List.Data (List),
                    Script => Data.Get_Script));
            List := Next (List);
         end loop;
      end if;
   end Command_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Bookmark_Class     : constant Class_Type :=
         Kernel.Scripts.New_Class (Bookmark_Class_Name);
      Src_Action_Context : constant Action_Filter :=
                             Lookup_Filter (Kernel, "Source editor");
      P         : Kernel_Search_Provider_Access;

   begin
      Bookmark_Views_Module := new Bookmark_Views_Module_Record;
      Generic_View.Register_Module (Kernel, Module_ID (Bookmark_Views_Module));

      Load_Bookmarks (Kernel);

      Register_Action
        (Kernel, "bookmark rename", new Rename_Bookmark_Command,
         -("Interactively rename the bookmark currently selected in the"
           & " bookmarks view"), Category => -"Bookmarks",
         Icon_Name => "gps-rename-symbolic");

      Register_Action
        (Kernel, "bookmark remove", new Delete_Bookmark_Command,
         -"Delete the bookmark currently selected in the bookmarks view",
         Icon_Name => "gps-remove-symbolic",
         Category => -"Bookmarks");

      Register_Action
        (Kernel, "bookmark create", new Create_Bookmark_Command,
         -("Create a bookmark at the current location"),
         Icon_Name => "gps-add-symbolic",
         Category => -"Bookmarks", Filter => Src_Action_Context);

      Register_Action
        (Kernel      => Kernel,
         Name        => "Goto Next Bookmark",
         Command     => new Next_Bookmark_Command (Backward => False),
         Description => -("Go to next bookmark in current file"),
         Filter      => Src_Action_Context);

      Register_Action
        (Kernel      => Kernel,
         Name        => "Goto Previous Bookmark",
         Command     => new Next_Bookmark_Command (Backward => True),
         Description => -("Go to previous bookmark in current file"),
         Filter      => Src_Action_Context);

      Register_Command
        (Kernel, Constructor_Method, 0, 0, Command_Handler'Access,
         Bookmark_Class);
      Register_Command
        (Kernel, "get", 1, 1, Command_Handler'Access, Bookmark_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "create", 1, 1, Command_Handler'Access, Bookmark_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "list", 0, 0, Command_Handler'Access, Bookmark_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "name", 0, 0, Command_Handler'Access, Bookmark_Class);
      Register_Command
        (Kernel, "rename", 1, 1, Command_Handler'Access, Bookmark_Class);
      Register_Command
        (Kernel, "delete", 0, 0, Command_Handler'Access, Bookmark_Class);
      Register_Command
        (Kernel, "goto", 0, 0, Command_Handler'Access, Bookmark_Class);

      P := new Bookmarks_Search_Provider;
      Register_Provider_And_Action (Kernel, P);
   end Register_Module;

end Bookmark_Views;
