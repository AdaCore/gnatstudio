------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2020, AdaCore                     --
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

with Ada.Containers;                 use Ada.Containers;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;                   use GNAT.Strings;
with System;                         use System;
with System.Address_Image;

with GNATCOLL.JSON;                  use GNATCOLL.JSON;
with GNATCOLL.Projects;              use GNATCOLL.Projects;
with GNATCOLL.Scripts;               use GNATCOLL.Scripts;
with GNATCOLL.Traces;                use GNATCOLL.Traces;
with GNATCOLL.VFS;                   use GNATCOLL.VFS;

with Glib;                           use Glib;
with Glib.Main;                      use Glib.Main;
with Glib.Object;                    use Glib.Object;
with Glib.Properties;                use Glib.Properties;
with Glib_Values_Utils;              use Glib_Values_Utils;

with Gdk.Drag_Contexts;              use Gdk.Drag_Contexts;
with Gdk.Event;                      use Gdk.Event;
with Gdk.Rectangle;                  use Gdk.Rectangle;
with Gdk.Property;                   use Gdk.Property;
with Gdk.Types;                      use Gdk.Types;

with Gtk.Box;                        use Gtk.Box;
with Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Text;         use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;       use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Dialog;                     use Gtk.Dialog;
with Gtk.Dnd;                        use Gtk.Dnd;
with Gtk.Enums;                      use Gtk.Enums;
with Gtk.Gesture_Long_Press;         use Gtk.Gesture_Long_Press;
with Gtk.Gesture_Multi_Press;        use Gtk.Gesture_Multi_Press;
with Gtk.Label;                      use Gtk.Label;
with Gtk.Menu;
with Gtk.Scrolled_Window;            use Gtk.Scrolled_Window;
with Gtk.Selection_Data;             use Gtk.Selection_Data;
with Gtk.Target_List;                use Gtk.Target_List;
with Gtk.Text_Buffer;                use Gtk.Text_Buffer;
with Gtk.Text_Iter;                  use Gtk.Text_Iter;
with Gtk.Text_View;                  use Gtk.Text_View;
with Gtk.Toolbar;
with Gtk.Tree_Drag_Source;
with Gtk.Tree_Model;                 use Gtk.Tree_Model;
with Gtk.Tree_Selection;             use Gtk.Tree_Selection;
with Gtk.Tree_Store;                 use Gtk.Tree_Store;
with Gtk.Tree_View;                  use Gtk.Tree_View;
with Gtk.Tree_View_Column;           use Gtk.Tree_View_Column;
with Gtk.Widget;                     use Gtk.Widget;
with Gtkada.MDI;                     use Gtkada.MDI;

with Basic_Types;                    use Basic_Types;
with Commands.Interactive;           use Commands, Commands.Interactive;
with Default_Preferences;            use Default_Preferences;
with Generic_Views;                  use Generic_Views;
with GPS.Default_Styles;             use GPS.Default_Styles;
with GPS.Editors;                    use GPS.Editors;
with GPS.Editors.Line_Information;   use GPS.Editors.Line_Information;
with GPS.Kernel;                     use GPS.Kernel;
with GPS.Kernel.Actions;             use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;               use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                 use GPS.Kernel.MDI;
with GPS.Kernel.Messages;            use GPS.Kernel.Messages;
with GPS.Kernel.Messages.References; use GPS.Kernel.Messages.References;
with GPS.Kernel.Messages.Simple;     use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Modules;             use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;          use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;         use GPS.Kernel.Preferences;
with GPS.Kernel.Project;             use GPS.Kernel.Project;
with GPS.Kernel.Properties;          use GPS.Kernel.Properties;
with GPS.Kernel.Scripts;             use GPS.Kernel.Scripts;
with GPS.Kernel.Search;              use GPS.Kernel.Search;
with GPS.Main_Window;                use GPS.Main_Window;
with GPS.Markers;                    use GPS.Markers;
with GPS.Properties;                 use GPS.Properties;
with GPS.Search.GUI;                 use GPS.Search.GUI;
with GPS.Intl;                       use GPS.Intl;
with GPS.Scripts;                    use GPS.Scripts;
with GPS.Search;                     use GPS.Search;
with Gtkada.Tree_View;               use Gtkada.Tree_View;
with GUI_Utils;                      use GUI_Utils;
with GPS.Dialogs;                    use GPS.Dialogs;
with Tooltips;                       use Tooltips;
with XML_Parsers;
with XML_Utils;                      use XML_Utils;
with JSON_Utils;
with Filter_Panels;                  use Filter_Panels;

package body Bookmark_Views is

   Me : constant Trace_Handle := Create ("GPS.OTHERS.BOOKMARKS");

   Bookmark_Class_Name : constant String := "Bookmark";

   Icon_For_Bookmarks  : constant String := "gps-goto-symbolic";
   Icon_For_Groups     : constant String := "gps-emblem-directory-open";
   Icon_For_Tag        : constant String := "gps-tag-symbolic";

   Line_Text_Attribute : constant String := "line_text";

   Messages_Category_For_Bookmarks : constant String := "bookmarks";
   Message_Flags_For_Bookmarks     : constant Message_Flags :=
     (Editor_Side => True,
      Locations   => False,
      Editor_Line => False);

   Icon_Name_Column : constant := 0;
   Name_Column      : constant := 1;
   Data_Column      : constant := 2;
   Has_Note_Column  : constant := 3;

   Column_Types : constant GType_Array :=
     (Icon_Name_Column => GType_String,
      Name_Column      => GType_String,
      Data_Column      => GType_Pointer,
      Has_Note_Column  => GType_Boolean);

   Editor_Link  : Boolean_Preference;
   --  Whether we should automatically select the bookmark corresponding to
   --  the current location in the editor.

   Append_At_Bottom : Boolean_Preference;
   --  Put Latest bookmark at the bottom of the list

   Dynamically_Moving : Boolean_Preference;
   --  Move bookmarks dynamically due to external file modifications.

   type Bookmark_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : Bookmark_Proxy) return String
      is (Bookmark_Class_Name) with Inline;
   --  Interface with python

   type Line_Text_Type (Is_Empty : Boolean := True) is record
      case Is_Empty is
         when True =>
            null;
         when False =>
            Text : Unbounded_String;
      end case;
   end record;
   --  Is used for holding the line text to which the bookmark is attached

   type Bookmark_Type is (Standard, Group, Unattached);
   type Group_Content_Type is (Unknown, Global, Project, Both);

   type Bookmark_Data;
   type Bookmark_Data_Access is access all Bookmark_Data;
   type Bookmark_Data (Typ : Bookmark_Type) is record
      Name      : Unbounded_String;
      --  Name of bookmark or group.
      --  Set to null for the toplevel group.

      Note      : Unbounded_String;
      --  Extra information associated with the bookmark

      Previous_Same_Level : Bookmark_Data_Access;
      Next_Same_Level     : Bookmark_Data_Access;
      Parent              : Bookmark_Data_Access;
      --  Next node at same level

      case Typ is
         when Group =>
            Contains    : Group_Content_Type := Unknown;
            First_Child : Bookmark_Data_Access;
         when Unattached =>
            null;
         when Standard =>
            Marker     : Location_Marker;
            Instances  : Bookmark_Proxy;
            Message    : Message_Reference;
            --  To highlight lines in the editor. This is only set when the
            --  bookmark is associated with a source file, but not for other
            --  kinds of bookmarks.
            On_Project : Boolean := False;
            --  The bookmark is belong to the project

            Line_Text   : Line_Text_Type;
            --  Content of the line on which the bookmark is set
            Marker_Data : JSON_Value;
            --  Data from the project bookmark marker, prepared for save in
            --  properties database, Needed because we save the database
            --  on exit and maker can be invalid in that point.
      end case;
   end record;

   procedure Free (Data : in out Bookmark_Data_Access);
   --  Free memory allocated for Data and its children

   procedure Remove_But_Not_Free (Data : Bookmark_Data_Access);
   --  Remove from the list, but don't free Data.

   procedure Insert
     (Data     : Bookmark_Data_Access;
      After    : Bookmark_Data_Access;
      In_Group : Bookmark_Data_Access)
     with Pre => After = null or else After.Parent = In_Group;
   --  Insert Data (which must have been removed first) in the list

   function New_Bookmark
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Mark          : Location_Marker;
      Name          : String := "";
      On_Project    : Boolean;
      Preserve_Name : Boolean := False)
      return Bookmark_Data_Access;
   --  Allocate a new bookmark data and return it.
   --  Preserve Name indicates that new bookmark should have Name without
   --   any changes (when we restore project bookmark which
   --   already have prepared name).

   function New_Group (Name : String) return Bookmark_Data_Access;
   --  Allocate a new empty group

   function Clone (Data : Bookmark_Data_Access) return Bookmark_Data_Access;
   --  Makes a copy of the Data for storing in project properties,
   --   result should be freed by caller

   package Bookmark_Proxies is new Script_Proxies
      (Bookmark_Data_Access, Bookmark_Proxy);

   type Bookmark_Views_Module_Record is new Module_ID_Record with record
      Loaded : Boolean := False;  --  whether bookmarks were loaded
      Root   : Bookmark_Data_Access;
   end record;
   type Bookmark_Views_Module_Access
     is access all Bookmark_Views_Module_Record'Class;
   overriding procedure Destroy (Module : in out Bookmark_Views_Module_Record);

   Bookmark_Views_Module : Bookmark_Views_Module_Access;

   subtype Bookmark_Iter is Bookmark_Data_Access;
   function Bookmark_Iter_First return Bookmark_Iter
      is (Bookmark_Iter (Bookmark_Views_Module.Root));
   function Next_Recursive (Iter : Bookmark_Iter) return Bookmark_Iter;
   --  Iter the whole tree recursively.

   type Bookmark_View_Record is tagged;

   type Bookmark_Tree_Record is new Gtkada.Tree_View.Tree_View_Record with
      record
         View        : access Bookmark_View_Record'Class;
         Text        : Gtk_Cell_Renderer_Text;
         Note_Pixbuf : Gtk_Cell_Renderer_Pixbuf;
         Pattern     : Search_Pattern_Access;
      end record;
   type Bookmark_Tree is access all Bookmark_Tree_Record'Class;
   overriding function Is_Visible
     (Self       : not null access Bookmark_Tree_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean;
   overriding procedure On_Edited
     (Self        : not null access Bookmark_Tree_Record;
      Store_Iter  : Gtk_Tree_Iter;
      View_Column : Edited_Column_Id;
      Text        : String);

   type Bookmark_View_Record is new Generic_Views.View_Record with record
      Tree      : Bookmark_Tree;
      Deleting  : Boolean := False;
      --  Whether we are deleting multiple bookmarks

      Multipress : Gtk_Gesture_Multi_Press;
      Longpress  : Gtk_Gesture_Long_Press;
      --  Handles gestures

      Data_Dropped : Boolean := False;
      --  Whether we just completed a drag-and-drop operation. This is used to
      --  perform the actual work in On_Drag_Data_Received
   end record;
   overriding procedure Create_Toolbar
     (Self    : not null access Bookmark_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Bookmark_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Bookmark_View_Record;
      Pattern : in out Search_Pattern_Access);

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
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Formal_MDI_Child   => Bookmark_Child_Record,
      Formal_View_Record => Bookmark_View_Record);
   use Generic_View;
   subtype Bookmark_View_Access is Generic_View.View_Access;

   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Bookmark_Data_Access);
   function Convert is new Ada.Unchecked_Conversion
     (Bookmark_Data_Access, System.Address);

   function Get_Data
     (Self       : not null access Bookmark_Tree_Record'Class;
      Store_Iter : Gtk_Tree_Iter) return Bookmark_Data_Access
     is (Convert (Self.Model.Get_Address (Store_Iter, Data_Column)));
   --  Retrieve the bookmark data stored in each row of the tree

   function Hash (B : Bookmark_Data_Access) return Ada.Containers.Hash_Type
     is (Ada.Strings.Hash (System.Address_Image (B.all'Address)));
   --  Return a hash for the bookmark.
   --  Since names are not unique, we use the pointer itself

   package Tree_Expansion is new Gtkada.Tree_View.Expansion_Support
     (Tree_Record => Bookmark_Tree_Record,
      Id          => Bookmark_Data_Access,
      Get_Id      => Get_Data,
      Hash        => Hash);

   procedure Refresh
     (View     : access Bookmark_View_Record'Class;
      Expand   : Bookmark_Data_Access := null;
      Selected : Bookmark_Data_Access := null);
   --  Refresh the contents of the Bookmark view.
   --  If Expand is specified, the corresponding row is expanded

   procedure Add_Note
     (Self     : not null access Bookmark_View_Record'Class;
      Bookmark : Bookmark_Data_Access;
      Note     : String;
      Append   : Boolean);
   --  Add a note to the bookmark

   procedure Edit_Note
     (View : not null access Bookmark_View_Record'Class;
      Data : not null Bookmark_Data_Access);
   --  Edit the note for a bookmark

   procedure Delete_Bookmark
     (Kernel   : access Kernel_Handle_Record'Class;
      Bookmark : in out Bookmark_Data_Access);
   --  Delete an existing bookmark

   function Bookmark_From_Name (Name : String) return Bookmark_Data_Access;
   --  Return the location marker for the first bookmark named Name.
   --  null is returned if not found

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Called when the project is changing. This is a good time to save the
   --  project bookmarks

   type On_Project_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when the project changes. This is a good time to load the
   --  persistent bookmarks

   type On_Loc_Changed is new File_Location_Hooks_Function with null record;
   overriding procedure Execute
     (Self         : On_Loc_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type);
   --  Called when the current editor reaches a new location

   type On_Lines_Deleted is new File_Lines_Hooks_Function with null record;
   overriding procedure Execute
     (Self         : On_Lines_Deleted;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Count  : Integer);
   --  Called when lines are deleted from a buffer

   type Bookmark_View_Tooltip_Handler is
     new Tooltips.Tooltip_Handler with null record;
   overriding function Create_Contents
     (Tooltip         : not null access Bookmark_View_Tooltip_Handler;
      Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y            : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  Support for tooltips: set the contents of the tooltips

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble);
   --  Called every time a row is clicked

   procedure On_Longpress
     (Self    : access Glib.Object.GObject_Record'Class;
      X, Y    : Gdouble);
   --  Called when the user presses for a while on a row.

   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class);
   --  Save the bookmarks to the XML file

   procedure Save_Project_Bookmarks
     (Kernel : access Kernel_Handle_Record'Class);
   --  Stores project bookmarks as project properties

   function On_Drag_Drop
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class;
      X, Y    : Gint;
      Time    : Guint) return Boolean;
   procedure On_Drag_Data_Received
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class;
      X, Y    : Gint;
      Data    : Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);
   --  Support for reordering rows and creating groups of bookmarks.
   --  Although we let gtk+ trees handle most of the behavior, we do not let it
   --  do the actual reordering, since we want to create new intermediate nodes
   --  for the groups, not just move a bookmark below another.
   --  The actual work is done in On_Drag_Data_Received, but we need to
   --  override On_Drag_Drop to prevent the gtk+ default behavior since we
   --  can't prevent it from On_Drag_Data_Received.
   --  Also, with the implementation in gtktreeview.c, On_Drag_Data_Received is
   --  called every time the mouse moves to verify whether the drop is
   --  possible.

   procedure Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands for this module
   procedure Write_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles setters shell commands for this module

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

   type Create_Bookmark_Command is new Interactive_Command with record
      Mode             : Bookmark_Type := Standard;
      Ignore_Selection : Boolean       := False;
      On_Project       : Boolean       := False;
   end record;
   overriding function Execute
     (Command : access Create_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Create a new bookmark

   type Rename_Bookmark_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Rename_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Rename the selected bookmark

   type Edit_Note_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_Note_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit note for selected bookmark

   type Next_Bookmark_Command (Backward : Boolean) is
     new Interactive_Command with null record;

   overriding function Execute
     (Command : access Next_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to next bookmark in current file

   procedure Set_Groups_Type;
   --  Called before bookmarks saving to set group content types so we will
   --   know which groups should be saved in project properties and which not.

   procedure Set
     (To   : in out Group_Content_Type;
      From : Group_Content_Type);
   --  Encapsulates rules for setting value based on current value in To

   function Get_Name (Bookmark : Bookmark_Data_Access) return String;
   --  Return the display name of the bookmark

   function Create_Message
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mark   : Location_Marker) return Simple_Message_Access;
   --  Create message for the location to mark the bookmark position
   --  in a source buffer

   ------------
   -- Search --
   ------------

   type Bookmarks_Search_Provider is new Kernel_Search_Provider
   with record
      Pattern : GPS.Search.Search_Pattern_Access;
      Pos     : Bookmark_Iter;
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

   procedure Dump (Me : Trace_Handle; First : Bookmark_Data_Access)
     with Warnings => Off;
   --  Debug procedure

   ----------------
   -- Properties --
   ----------------

   type Bookmarks_Property_Record is new Property_Record with record
      Kernel : Kernel_Handle;
      Root   : Bookmark_Data_Access;
   end record;
   type Bookmarks_Property is access all Bookmarks_Property_Record;

   overriding procedure Save
     (Property : access Bookmarks_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Property : in out Bookmarks_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value);
   overriding procedure Destroy (Property : in out Bookmarks_Property_Record);

   ----------
   -- Dump --
   ----------

   procedure Dump (Me : Trace_Handle; First : Bookmark_Data_Access) is
      Tmp : Bookmark_Data_Access := First;
   begin
      Increase_Indent (Me, "");
      while Tmp /= null loop
         Trace (Me, To_String (Tmp.Name)
                & " prev="
                & (if Tmp.Previous_Same_Level = null then ""
                  else To_String (Tmp.Previous_Same_Level.Name))
                & " next="
                & (if Tmp.Next_Same_Level = null then ""
                  else To_String (Tmp.Next_Same_Level.Name))
                & " parent="
                & (if Tmp.Parent = null then ""
                  else To_String (Tmp.Parent.Name)));
         case Tmp.Typ is
            when Group =>
               Dump (Me, Tmp.First_Child);
            when Unattached | Standard =>
               null;
         end case;
         Tmp := Tmp.Next_Same_Level;
      end loop;
      Decrease_Indent (Me);
   end Dump;

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
      Self.Pos     := Bookmark_Iter_First;
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
   begin
      if Self.Pos = null then
         Has_Next := False;
         Result := null;
      else
         case Self.Pos.Typ is
            when Group =>
               Has_Next := True;
               Result := null;

            when Unattached | Standard =>
               Has_Next := True;

               declare
                  Loc : constant String :=
                    (if Self.Pos.Typ = Standard
                     then To_String (Self.Pos.Marker)
                     else "");
               begin
                  C := Self.Pattern.Start (Get_Name (Self.Pos));
                  if C /= GPS.Search.No_Match then
                     Result := new Bookmarks_Search_Result'
                       (Kernel   => Self.Kernel,
                        Provider => Self,
                        Score    => C.Score,
                        Short    => new String'
                          (Self.Pattern.Highlight_Match
                               (Get_Name (Self.Pos), C)),
                        Long     => new String'(Loc),
                        Id       => new String'(Get_Name (Self.Pos)),
                        Bookmark => Bookmark_Data_Access (Self.Pos));
                     Self.Adjust_Score (Result);

                  else
                     C := Self.Pattern.Start (Loc);
                     if C /= GPS.Search.No_Match then
                        Result := new Bookmarks_Search_Result'
                          (Kernel   => Self.Kernel,
                           Provider => Self,
                           Score    => C.Score,
                           Short    => new String'(Get_Name (Self.Pos)),
                           Long     => new String'
                             (Self.Pattern.Highlight_Match (Loc, C)),
                           Id       => new String'(Get_Name (Self.Pos)),
                           Bookmark => Bookmark_Data_Access (Self.Pos));
                        Self.Adjust_Score (Result);
                     end if;
                  end if;
               end;
         end case;

         Self.Pos := Next_Recursive (Self.Pos);
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
   begin
      Self.Set_Pattern (Pattern);

      while Self.Pos /= null loop
         C := Self.Pattern.Start (Get_Name (Self.Pos));
         if C /= GPS.Search.No_Match then
            Self.Pattern.Compute_Suffix
              (C, Get_Name (Self.Pos), Suffix, Suffix_Last);
            exit when Suffix_Last = 0;
         end if;

         Self.Pos := Next_Recursive (Self.Pos);
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
         "<b>Name:</b> " & Get_Name (Self.Bookmark) & ASCII.LF &
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
      --  it is modified). If GNAT Studio crashes, we would lose bookmarks
      --  for open files, but not for files that have been closed in between...

      if Module.Loaded then
         Save_Bookmarks (Get_Kernel (Module));
      end if;

      Bookmark_Views_Module := null;
   end Destroy;

   ---------------------
   -- Delete_Bookmark --
   ---------------------

   procedure Delete_Bookmark
     (Kernel   : access Kernel_Handle_Record'Class;
      Bookmark : in out Bookmark_Data_Access)
   is
      Name : constant String := Get_Name (Bookmark);
   begin
      Free (Bookmark);
      Bookmark_Removed_Hook.Run (Kernel, Name);
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
      List   : Gtk_Tree_Path_List.Glist;
      G_Iter : Gtk_Tree_Path_List.Glist;
      Path   : Gtk_Tree_Path;
      Model  : Gtk_Tree_Model;
      Dummy  : Boolean;
      Data   : Bookmark_Data_Access;

      use Gtk_Tree_Path_List;
   begin
      if View /= null then
         View.Deleting := True;
         Get_Selected_Rows (View.Tree.Get_Selection, Model, List);

         if Model /= Null_Gtk_Tree_Model
           and then List /= Null_List
         then
            --  The children must be modified before there fathers
            G_Iter := Gtk_Tree_Path_List.Last (List);

            while G_Iter /= Gtk_Tree_Path_List.Null_List loop
               Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
               Data := View.Tree.Get_Data
                 (Store_Iter =>
                    View.Tree.Convert_To_Store_Iter (Get_Iter (Model, Path)));

               if Data /= null then
                  Delete_Bookmark (Get_Kernel (Context.Context), Data);
               end if;

               G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
            end loop;
         end if;

         Free_Path_List (List);
         View.Deleting := False;
         Refresh (View);
      end if;

      return Success;
   end Execute;

   --------------------
   -- Create_Message --
   --------------------

   function Create_Message
     (Kernel : not null access Kernel_Handle_Record'Class;
      Mark   : Location_Marker) return Simple_Message_Access
   is
      Msg : Simple_Message_Access;
   begin
      Msg := Create_Simple_Message
        (Get_Messages_Container (Kernel),
         Category                 => Messages_Category_For_Bookmarks,
         File                     => Get_File (Mark),
         Line                     => Integer (Get_Line (Mark)),
         Column                   => Get_Column (Mark),
         Text                     => To_String (Mark),
         Importance               => Unspecified,
         Flags                    => Message_Flags_For_Bookmarks,
         Allow_Auto_Jump_To_First => False);

      Msg.Set_Highlighting
        (Bookmark_Default_Style, Length => Highlight_Whole_Line);

      Msg.Set_Action
        (new Line_Information_Record'
           (Text         => Null_Unbounded_String,
            Tooltip_Text => To_Unbounded_String ("Bookmark"),
            Image        => To_Unbounded_String (Icon_For_Bookmarks),
            others       => <>));

      return Msg;
   end Create_Message;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (Self    : not null access Bookmark_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Self.Build_Filter
        (Toolbar,
         Hist_Prefix => "bookmarks",
         Tooltip     => -"Filter the contents of the Bookmarks view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Bookmark_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, View.Kernel, Editor_Link);
      Append_Menu (Menu, View.Kernel, Append_At_Bottom);
      Append_Menu (Menu, View.Kernel, Dynamically_Moving);
   end Create_Menu;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : not null access Bookmark_Tree_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean
   is
      B : Bookmark_Data_Access;
   begin
      if Self.Pattern = null then
         return True;
      end if;

      if Self.Pattern.Start (Self.Model.Get_String (Store_Iter, Name_Column))
        /= GPS.Search.No_Match
      then
         return True;
      end if;

      B := Get_Data (Self, Store_Iter => Store_Iter);
      return B.Typ = Standard
        and then Self.Pattern.Start
          (To_String (B.Marker)) /= GPS.Search.No_Match;
   end Is_Visible;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Bookmark_View_Record;
      Pattern : in out Search_Pattern_Access)
   is
   begin
      GPS.Search.Free (Self.Tree.Pattern);
      Self.Tree.Pattern := Pattern;
      Self.Tree.Refilter;  --  Recompute visibility of rows
   end Filter_Changed;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Rename_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View  : constant Bookmark_View_Access :=
                Generic_View.Get_Or_Create_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         View.Tree.Start_Editing (Render => View.Tree.Text);
      end if;
      return Success;
   end Execute;

   ---------------
   -- Edit_Note --
   ---------------

   procedure Edit_Note
     (View : not null access Bookmark_View_Record'Class;
      Data : not null Bookmark_Data_Access)
   is
      Dialog      : GPS_Dialog;
      Editor      : Gtk_Text_View;
      Buffer      : Gtk_Text_Buffer;
      From, To    : Gtk_Text_Iter;
      Scrolled    : Gtk_Scrolled_Window;
      W           : Gtk_Widget;
   begin
      Gtk_New
        (Self   => Dialog,
         Title  => "Edit Note for Bookmark",
         Kernel => View.Kernel);
      Set_Default_Size_From_History
        (Win    => Dialog,
         Name   => "Edit Note for Bookmark",
         Kernel => View.Kernel,
         Width  => 600,
         Height => 300);

      Gtk_New (Scrolled);
      Dialog.Get_Content_Area.Pack_Start (Scrolled, Expand => True);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      Gtk_New (Buffer);
      Gtk_New (Editor, Buffer);
      Unref (Buffer);
      Scrolled.Add (Editor);
      Buffer.Set_Text (To_String (Data.Note));

      W := Dialog.Add_Button (-"Apply", Gtk_Response_OK);
      W.Grab_Default;
      Dialog.Add_Button (-"Cancel", Gtk_Response_Cancel).Show;
      Dialog.Show_All;

      if Dialog.Run = Gtk_Response_OK then
         Buffer.Get_Start_Iter (From);
         Buffer.Get_End_Iter (To);
         View.Add_Note (Data, Buffer.Get_Text (From, To), Append => False);
      end if;

      Dialog.Destroy;
   end Edit_Note;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_Note_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant Bookmark_View_Access :=
        Generic_View.Get_Or_Create_View (Kernel);
      Filter_Iter : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;
      Data        : Bookmark_Data_Access;
   begin
      if View /= null then
         View.Tree.Get_First_Selected (Model, Filter_Iter);

         if Filter_Iter /= Null_Iter then
            Data := View.Tree.Get_Data
              (Store_Iter => View.Tree.Convert_To_Store_Iter (Filter_Iter));
            Edit_Note (View, Data);
         end if;
      end if;
      return Success;
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
      C       : Bookmark_Iter := Bookmark_Iter_First;
      Nearest : Location_Marker;
      Min     : Integer := Integer'Last;
      Sign    : constant Integer := Direction (Command.Backward);
   begin
      while C /= null loop
         if C.Typ = Standard then
            declare
               Next : constant Location_Marker := C.Marker;
               Dist : constant Integer := Sign * Distance (Marker, Next);
            begin
               if Dist > 0 and abs Dist /= Integer'Last then
                  if Min > Dist then
                     Min := Dist;
                     Nearest := Next;
                  end if;
               end if;
            end;
         end if;

         C := Next_Recursive (C);
      end loop;

      if Min /= Integer'Last and then Go_To (Nearest) then
         return Success;
      else
         return Failure;
      end if;
   end Execute;

   -------------------------
   -- Remove_But_Not_Free --
   -------------------------

   procedure Remove_But_Not_Free (Data : Bookmark_Data_Access) is
   begin
      if Data.Parent /= null then
         if Data.Parent.First_Child = Data then
            Data.Parent.First_Child := Data.Next_Same_Level;
         end if;
         Data.Parent := null;

      elsif Bookmark_Views_Module /= null
        and then Bookmark_Views_Module.Root = Data
      then
         Bookmark_Views_Module.Root := Data.Next_Same_Level;
      end if;

      if Data.Previous_Same_Level /= null then
         Data.Previous_Same_Level.Next_Same_Level := Data.Next_Same_Level;
      end if;

      if Data.Next_Same_Level /= null then
         Data.Next_Same_Level.Previous_Same_Level := Data.Previous_Same_Level;
      end if;

      Data.Previous_Same_Level := null;
      Data.Next_Same_Level := null;
   end Remove_But_Not_Free;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Bookmark_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Bookmark_Data, Bookmark_Data_Access);
      Tmp : Bookmark_Data_Access;
   begin
      if Data /= null then
         Remove_But_Not_Free (Data);

         --  Free node specific data

         case Data.Typ is
            when Group =>
               while Data.First_Child /= null loop
                  --  Can't pass Data.First_Child directly, or it will be set
                  --  to null directly (otherwise it is only set to null when
                  --  freeing the last child)
                  Tmp := Data.First_Child;
                  Free (Tmp);
               end loop;

            when Unattached =>
               null;

            when Standard =>
               Data.Instances.Free;
               if not Data.Message.Is_Empty then
                  Data.Message.Message.Remove;
               end if;
         end case;

         Unchecked_Free (Data);
      end if;
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (Data : Bookmark_Data_Access) return Bookmark_Data_Access
   is
      Result : constant Bookmark_Data_Access := new Bookmark_Data (Data.Typ);
   begin
      Result.Name := Data.Name;
      Result.Note := Data.Note;

      return Result;
   end Clone;

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
            V.Tree.Get_Selection.Unselect_All;
            Select_Iter (Get_Selection (V.Tree), Iter);
         end if;
      end if;
      return Context;
   end Build_Context;

   ---------------
   -- New_Group --
   ---------------

   function New_Group (Name : String) return Bookmark_Data_Access is
   begin
      return new Bookmark_Data'
        (Typ                 => Group,
         Contains            => Unknown,
         First_Child         => null,
         Next_Same_Level     => null,
         Previous_Same_Level => null,
         Parent              => null,
         Note                => Null_Unbounded_String,
         Name                => To_Unbounded_String (Name));
   end New_Group;

   ------------------
   -- New_Bookmark --
   ------------------

   function New_Bookmark
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Mark          : Location_Marker;
      Name          : String := "";
      On_Project    : Boolean;
      Preserve_Name : Boolean := False)
      return Bookmark_Data_Access
   is
      Msg : Simple_Message_Access;
      N   : Unbounded_String := To_Unbounded_String (Name);
   begin
      --  If the mark is associated with a file editor and a location
      if Get_Line (Mark) /= 0 then
         Msg := Create_Message (Kernel, Mark);
      end if;

      if Mark = No_Marker then
         return new Bookmark_Data'
           (Typ                 => Unattached,
            Previous_Same_Level => null,
            Next_Same_Level     => null,
            Parent              => null,
            Note                => Null_Unbounded_String,
            Name                => To_Unbounded_String
              ((if Name = "" then To_String (Mark) else Name)));
      else
         if not Preserve_Name then
            if On_Project then
               N := To_Unbounded_String
                 (Get_Project (Kernel).Name & ":" &
                  (if Name /= "" then Name else "%M"));

            else
               if Name = "" then
                  N := To_Unbounded_String ("%M");
               end if;
            end if;
         end if;

         return new Bookmark_Data'
           (Typ                 => Standard,
            Marker              => Mark,
            Message             => Create (Message_Access (Msg)),
            Previous_Same_Level => null,
            Next_Same_Level     => null,
            Parent              => null,
            Note                => Null_Unbounded_String,
            Name                => N,
            Instances           => <>,
            On_Project          => On_Project,
            Line_Text           => (Is_Empty => True),
            Marker_Data         => JSON_Null);
      end if;
   end New_Bookmark;

   --------------------
   -- Next_Recursive --
   --------------------

   function Next_Recursive (Iter : Bookmark_Iter) return Bookmark_Iter is
      Next : Bookmark_Data_Access;
   begin
      if Iter.Typ = Group and then Iter.First_Child /= null then
         Next := Iter.First_Child;
      elsif Iter.Next_Same_Level /= null then
         Next := Iter.Next_Same_Level;
      else
         Next := Iter.Parent;

         while Next /= null and then Next.Next_Same_Level = null loop
            Next := Next.Parent;
         end loop;

         if Next /= null then
            Next := Next.Next_Same_Level;
         end if;
      end if;

      return Bookmark_Iter (Next);
   end Next_Recursive;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Data     : Bookmark_Data_Access;
      After    : Bookmark_Data_Access;
      In_Group : Bookmark_Data_Access) is
   begin
      if After = null then
         --  Insert in front

         if In_Group /= null then
            Data.Next_Same_Level := In_Group.First_Child;
            In_Group.First_Child := Data;
         else
            Data.Next_Same_Level := Bookmark_Views_Module.Root;
            Bookmark_Views_Module.Root := Data;
         end if;

         if Data.Next_Same_Level /= null then
            Data.Next_Same_Level.Previous_Same_Level := Data;
         end if;

         Data.Parent := In_Group;

      else
         Data.Next_Same_Level := After.Next_Same_Level;
         if After.Next_Same_Level /= null then
            After.Next_Same_Level.Previous_Same_Level := Data;
         end if;

         After.Next_Same_Level    := Data;
         Data.Previous_Same_Level := After;

         Data.Parent := In_Group;
         --   In_Group.First_Child cannot be Data, since After is before
      end if;
   end Insert;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Create_Bookmark_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : Bookmark_View_Access;
      Gr, Bookmark : Bookmark_Data_Access;
      Filter_Iter : Gtk_Tree_Iter;
      Model       : Gtk_Tree_Model;

      function On_Row
        (Model : Gtk_Tree_Model; Path : Gtk_Tree_Path; Iter : Gtk_Tree_Iter)
         return Boolean;
      --  Start editing the newly inserted row

      function Get_Current_Mark return Location_Marker;
      --  Create a new marker for the current location

      function Get_Last
        (Group : Bookmark_Data_Access) return Bookmark_Data_Access
        with Pre => Group = null or else Group.Typ = Bookmark_Views.Group;
      --  Return last item in the group if any.

      ----------------------
      -- Get_Current_Mark --
      ----------------------

      function Get_Current_Mark return Location_Marker is
         Mark   : Location_Marker;
      begin
         --  If the current module doesn't support creating bookmarks, we fall
         --  back to the last editor that had focus, since that's what users
         --  will expect most of the time.

         Mark := Create_Marker (Kernel);
         if Mark.Is_Null then
            Trace
              (Me, "Current module cannot create bookmark, try last editor");
            declare
               --  Side effect is to give focus to the source edtor module to
               --  create the bookmark
               Buffer : constant Editor_Buffer'Class :=
                 Get_Buffer_Factory (Kernel).Get
                 (Open_Buffer => False, Open_View => False, Focus => False);
               Cursor : constant Editor_Location'Class :=
                 Buffer.Current_View.Cursor;

            begin
               Mark := Get_Buffer_Factory (Kernel).Create_Marker
                 (File    => Buffer.File,
                  Project => No_Project,
                  Line    => Editable_Line_Type (Cursor.Line),
                  Column  => Cursor.Column);
            end;
         end if;
         return Mark;
      end Get_Current_Mark;

      --------------
      -- Get_Last --
      --------------

      function Get_Last
        (Group : Bookmark_Data_Access) return Bookmark_Data_Access
      is
         Result : Bookmark_Data_Access;
      begin
         if Group = null then
            Result := Bookmark_Views_Module.Root;
         else
            Result := Group.First_Child;
         end if;

         if Result = null then
            return null;
         end if;

         while Result.Next_Same_Level /= null loop
            Result := Result.Next_Same_Level;
         end loop;

         return Result;
      end Get_Last;

      ------------
      -- On_Row --
      ------------

      function On_Row
        (Model : Gtk_Tree_Model; Path : Gtk_Tree_Path; Iter : Gtk_Tree_Iter)
         return Boolean
      is
         pragma Unreferenced (Model, Path);
         Dummy  : G_Source_Id;
         B      : constant Bookmark_Data_Access :=
           View.Tree.Get_Data (Store_Iter => Iter);
      begin
         if B = Bookmark then
            View.Tree.Start_Editing
              (Render => View.Tree.Text, Store_Iter => Iter);
            return True;  --  stop iteration
         else
            return False; --  continue iteration
         end if;
      end On_Row;

   begin
      Trace (Me, "bookmark create");
      View := Generic_View.Get_Or_Create_View (Kernel);

      --  If we have a selection, insert in the same group

      if Command.Ignore_Selection then
         Filter_Iter := Null_Iter;
      else
         View.Tree.Get_First_Selected (Model, Filter_Iter);
         if Filter_Iter /= Null_Iter then
            Gr := View.Tree.Get_Data
              (Store_Iter => View.Tree.Convert_To_Store_Iter (Filter_Iter));
            if Gr.Typ /= Group then
               Gr := Gr.Parent;
            end if;
         end if;
      end if;

      --  Create the bookmark

      case Command.Mode is
         when Group =>
            Bookmark := New_Group ("group");

         when Standard =>
            declare
               Mark : constant Location_Marker := Get_Current_Mark;
            begin
               if Mark.Is_Null then
                  return Failure;
               else
                  Bookmark := New_Bookmark
                    (Kernel,
                     Mark,
                     On_Project => Command.On_Project);
               end if;
            end;

         when Unattached =>
            Bookmark := New_Bookmark
              (Kernel,
               No_Marker,
               Name       => "unnamed",
               On_Project => Command.On_Project);
      end case;

      if Append_At_Bottom.Get_Pref then
         Insert (Bookmark, After => Get_Last (Gr), In_Group => Gr);
      else
         Insert (Bookmark, After => null, In_Group => Gr);
      end if;

      Refresh (View, Expand => Gr, Selected => Bookmark);
      Bookmark_Added_Hook.Run (Kernel, Get_Name (Bookmark));

      --  Start editing the name of the bookmark
      View.Tree.Model.Foreach (On_Row'Unrestricted_Access);
      return Success;
   end Execute;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (View     : access Bookmark_View_Record'Class;
      Expand   : Bookmark_Data_Access := null;
      Selected : Bookmark_Data_Access := null)
   is
      Expand_Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Select_Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;

      procedure Add_Level
        (First : Bookmark_Data_Access; Store_Parent : Gtk_Tree_Iter);
      --  Add all nodes for a given level, and their children

      ---------------
      -- Add_Level --
      ---------------

      procedure Add_Level
        (First : Bookmark_Data_Access; Store_Parent : Gtk_Tree_Iter)
      is
         Store_Iter : Gtk_Tree_Iter;
         Tmp        : Bookmark_Data_Access := First;

      begin
         while Tmp /= null loop
            View.Tree.Model.Append (Store_Iter, Store_Parent);
            Set_And_Clear
              (View.Tree.Model, Store_Iter,
               (Icon_Name_Column, Name_Column, Data_Column,
                Has_Note_Column),
               (1 => As_String
                    (case Tmp.Typ is
                        when Group      => Icon_For_Groups,
                        when Unattached => "",
                        when Standard   => Icon_For_Bookmarks),
                2 => As_String  (Get_Name (Tmp)),
                3 => As_Pointer (Convert (Tmp)),
                4 => As_Boolean (Tmp.Note /= Null_Unbounded_String)));

            if Tmp.Typ = Group then
               Add_Level (Tmp.First_Child, Store_Iter);
            end if;

            if Expand = Tmp then
               Expand_Path := View.Tree.Get_Filter_Path_For_Store_Iter
                 (Store_Iter);
            end if;

            if Selected = Tmp then
               Select_Path := View.Tree.Get_Filter_Path_For_Store_Iter
                 (Store_Iter);
            end if;

            Tmp := Tmp.Next_Same_Level;
         end loop;
      end Add_Level;

      Expansion   : Tree_Expansion.Expansion_Status;
      Dummy       : Boolean;
   begin
      if not View.Deleting then
         Tree_Expansion.Get_Expansion_Status (View.Tree, Expansion);
         View.Tree.Model.Clear;
         Add_Level (Bookmark_Views_Module.Root, Null_Iter);
         Tree_Expansion.Set_Expansion_Status (View.Tree, Expansion);

         if Expand_Path /= Null_Gtk_Tree_Path then
            Dummy := View.Tree.Expand_Row (Expand_Path, Open_All => False);
            Path_Free (Expand_Path);
         end if;

         if Select_Path /= Null_Gtk_Tree_Path then
            View.Tree.Get_Selection.Unselect_All;
            View.Tree.Get_Selection.Select_Iter
              (View.Tree.Filter.Get_Iter (Select_Path));
            Path_Free (Select_Path);
         end if;
      end if;
   end Refresh;

   ---------------
   -- On_Edited --
   ---------------

   overriding procedure On_Edited
     (Self        : not null access Bookmark_Tree_Record;
      Store_Iter  : Gtk_Tree_Iter;
      View_Column : Edited_Column_Id;
      Text        : String)
   is
      pragma Unreferenced (View_Column);
      Mark  : Bookmark_Data_Access;
   begin
      Mark := Self.Get_Data (Store_Iter => Store_Iter);
      if Get_Name (Mark) /= Text then
         Mark.Name := To_Unbounded_String (Text);
      end if;
      Save_Bookmarks (Get_Kernel (Bookmark_Views_Module.all));
      Self.View.Refresh (Selected => Mark);
   end On_Edited;

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
        Generic_View.Retrieve_View (Kernel);
   begin
      if View /= null then
         Set_Font_And_Colors (View.Tree, Fixed_Font => True, Pref => Pref);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Loc_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : Project_Type)
   is
      pragma Unreferenced (Self, Project, Column);
      View : constant Bookmark_View_Access :=
        Generic_View.Retrieve_View (Kernel);

      function On_Row
        (Model : Gtk_Tree_Model; Path : Gtk_Tree_Path; Iter : Gtk_Tree_Iter)
         return Boolean;
      --  For each row

      function On_Row
        (Model : Gtk_Tree_Model; Path : Gtk_Tree_Path; Iter : Gtk_Tree_Iter)
         return Boolean
      is
         pragma Unreferenced (Model, Path);
         B : constant Bookmark_Data_Access :=
           View.Tree.Get_Data (Store_Iter => Iter);
      begin
         if B.Typ = Standard
           and then Integer (Get_Line (B.Marker)) = Line
           and then Get_File (B.Marker) = File
         then
            View.Tree.Get_Selection.Unselect_All;
            View.Tree.Get_Selection.Select_Iter
              (View.Tree.Convert_To_Filter_Iter (Iter));
            return True;  --  Stop iteration
         end if;
         return False;  --  Continue iteration
      end On_Row;

   begin
      if View /= null and then Editor_Link.Get_Pref then
         View.Tree.Model.Foreach (On_Row'Unrestricted_Access);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Lines_Deleted;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Count  : Integer)
   is
      pragma Unreferenced (Self);

      -------------
      -- Process --
      -------------

      procedure Process (Data : Bookmark_Data_Access);
      procedure Process (Data : Bookmark_Data_Access) is
         D    : Bookmark_Data_Access := Data;
         Next : Bookmark_Data_Access;
      begin
         while D /= null loop
            Next := D.Next_Same_Level;

            case D.Typ is
               when Group =>
                  Process (D.First_Child);

               when Standard =>
                  if D.Marker.Get.Element.all in
                      Abstract_File_Marker_Data'Class
                  then
                     declare
                        M : constant Abstract_File_Marker :=
                          Abstract_File_Marker (D.Marker.Get.Element);
                     begin
                        if M.Get_File = File
                          and then Integer (M.Get_Line) >= Line
                          and then Integer (M.Get_Line) < Line + Count
                        then
                           Delete_Bookmark (Kernel, D);
                        end if;
                     end;
                  end if;

               when Unattached =>
                  null;
            end case;

            D := Next;
         end loop;
      end Process;

   begin
      Process (Bookmark_Views_Module.Root);
   end Execute;

   --------------
   -- Add_Note --
   --------------

   procedure Add_Note
     (Self     : not null access Bookmark_View_Record'Class;
      Bookmark : Bookmark_Data_Access;
      Note     : String;
      Append   : Boolean) is
   begin
      if Note = "" then
         Bookmark.Note := Null_Unbounded_String;
      elsif Append then
         if Bookmark.Note /= Null_Unbounded_String then
            Ada.Strings.Unbounded.Append (Bookmark.Note, "" & ASCII.LF);
         end if;
         Ada.Strings.Unbounded.Append (Bookmark.Note, Note);

      else
         Bookmark.Note := To_Unbounded_String (Note);
      end if;

      --  Display tag icon next to bookmarks with a note
      Self.Refresh;
   end Add_Note;

   ---------------------------
   -- On_Drag_Data_Received --
   ---------------------------

   procedure On_Drag_Data_Received
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class;
      X, Y    : Gint;
      Data    : Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint)
   is
      pragma Unreferenced (Context, Info, Time);
      View   : constant Bookmark_View_Access := Bookmark_View_Access (Self);
      Model  : Gtk_Tree_Model;
      Iter   : Gtk_Tree_Iter;
      Source, Target, Gr  : Bookmark_Data_Access;
      Path    : aliased Gtk_Tree_Path;
      Pos     : aliased Gtk_Tree_View_Drop_Position;
      Success : Boolean;
   begin
      if not View.Data_Dropped then
         return;
      end if;

      View.Data_Dropped := False;

      --  Get target row

      if not View.Tree.Get_Dest_Row_At_Pos
        (Drag_X => X, Drag_Y => Y, Path => Path'Access, Pos => Pos'Access)
      then
         return;
      end if;

      Iter := View.Tree.Filter.Get_Iter (Path);
      Target := View.Tree.Get_Data
        (Store_Iter => View.Tree.Convert_To_Store_Iter (Iter));
      Path_Free (Path);

      --  Dropping some text (possibly from the editor, or an external
      --  terminal) ?
      if Atom_Name (Data.Get_Target) = "UTF8_STRING" then
         Add_Note (View, Target, Data.Get_Text, Append => True);
         return;
      end if;

      --  Get source row

      Gtk.Tree_Drag_Source.Get_Row_Drag_Data
        (Selection_Data => Data,
         Tree_Model     => Model,
         Path           => Path,
         Success        => Success);
      if not Success then
         return;
      end if;

      Iter := Get_Iter (Model, Path);
      Source := View.Tree.Get_Data (Iter);
      Path_Free (Path);

      if Target = Source then
         return;
      end if;

      --  Move the items

      Trace (Me, "Drag-and-drop bookmark, Pos=" & Pos'Img
             & " source=" & Get_Name (Source)
             & " target=" & Get_Name (Target));

      Remove_But_Not_Free (Source);

      case Pos is
         when Tree_View_Drop_Before =>
            Insert (Source,
                    After    => Target.Previous_Same_Level,
                    In_Group => Target.Parent);
            View.Refresh (Selected => Source);

         when Tree_View_Drop_After =>
            Insert (Source, After => Target, In_Group => Target.Parent);
            View.Refresh (Selected => Source);

         when Tree_View_Drop_Into_Or_Before
            | Tree_View_Drop_Into_Or_After =>

            case Target.Typ is
               when Group =>
                  Insert (Source, After => null, In_Group => Target);
                  View.Refresh;
               when Unattached | Standard =>
                  Gr := New_Group (Name => "group for " &
                                     Get_Name (Source));
                  Insert (Gr,  After => Target, In_Group => Target.Parent);
                  Remove_But_Not_Free (Target);
                  Insert (Source, After => null,   In_Group => Gr);
                  Insert (Target, After => Source, In_Group => Gr);
                  View.Refresh (Expand => Gr, Selected => Gr);
            end case;
      end case;

      Save_Bookmarks (View.Kernel);
   end On_Drag_Data_Received;

   ------------------
   -- On_Drag_Drop --
   ------------------

   function On_Drag_Drop
     (Self    : access GObject_Record'Class;
      Context : not null access Drag_Context_Record'Class;
      X, Y    : Gint;
      Time    : Guint) return Boolean
   is
      View   : constant Bookmark_View_Access := Bookmark_View_Access (Self);
      Path   : aliased Gtk_Tree_Path;
      Pos    : aliased Gtk_Tree_View_Drop_Position;
      Target : Gdk_Atom;
      Success : Boolean := False;
   begin
      if View.Tree.Get_Dest_Row_At_Pos
        (Drag_X => X, Drag_Y => Y, Path => Path'Access, Pos => Pos'Access)
      then
         Success := True;

         --  Final request for the data. It is handled in the
         --  drag-data-received signal, and will perform the actual change of
         --  the model.
         View.Data_Dropped := True;
         Target := Gtk.Dnd.Dest_Find_Target
           (View.Tree, Drag_Context (Context), Null_Gtk_Target_List);
         Gtk.Dnd.Get_Data
           (View.Tree, Drag_Context (Context), Target, Guint32 (Time));
      end if;

      --  Report that the drop operation has completed
      Gtk.Dnd.Finish
        (Context => Drag_Context (Context),
         Success => Success,
         Del     => False,
         Time    => Guint32 (Time));
      return True;  --  Disable default behavior
   end On_Drag_Drop;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble)
   is
      View   : constant Bookmark_View_Access := Bookmark_View_Access (Self);
      Column : Gtk_Tree_View_Column;
      Filter_Path : Gtk_Tree_Path;
      B           : Bookmark_Data_Access;
      Dummy       : Boolean;
      Cell_X, Cell_Y  : Gint;
      Success         : Boolean;
      Area, Cell_Area : Gdk_Rectangle;
   begin
      Trace (Me, "Bookmarks multipress: " & N_Press'Img);
      if N_Press = 2 then

         View.Tree.Get_Path_At_Pos
           (Gint (X), Gint (Y), Filter_Path,
            Column, Cell_X, Cell_Y, Success);
         if Success then
            --  Select the row that was clicked
            View.Tree.Set_Cursor (Filter_Path, null, Start_Editing => False);
            B := View.Tree.Get_Data
              (Store_Iter =>
                 View.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path));

            if B /= null then
               if B.Note /= Null_Unbounded_String then
                  --  Check whether we clicked on the note icon

                  --  Area is the rectangle, within the TreeView, where the
                  --  column is displayed. For instance x=20 to 264
                  View.Tree.Get_Cell_Area (Filter_Path, Column, Area);

                  --  Aligned area is the rectangle within the rectangle where
                  --  the renderer is displayed. Only the size seems to be set
                  --  to an interesting value, the X coordinate is unclear.
                  --  Since the bookmarks view displays one icon for
                  --  'bookmark/folder', then one optional one for 'tag',
                  --  we just assume both have the same size.

                  View.Tree.Note_Pixbuf.Get_Aligned_Area
                    (Widget       => View.Tree,
                     Flags        => Gtk.Cell_Renderer.Cell_Renderer_Focused,
                     Cell_Area    => Area,
                     Aligned_Area => Cell_Area);

                  if Cell_Area.Width <= Gint (X) - Area.X
                    and then Gint (X) - Area.X <= 2 * Cell_Area.Width
                  then
                     Edit_Note (View, B);
                     Path_Free (Filter_Path);
                     return;
                  end if;
               end if;

               Path_Free (Filter_Path);

               if B.Typ = Standard then
                  Dummy := Go_To (B.Marker);
                  Push_Marker_In_History (View.Kernel, B.Marker);
                  View.Multipress.Set_State (Event_Sequence_Claimed);
               end if;
            end if;
         end if;
      end if;
   end On_Multipress;

   ------------------
   -- On_Longpress --
   ------------------

   procedure On_Longpress
     (Self    : access Glib.Object.GObject_Record'Class;
      X, Y    : Gdouble)
   is
      View  : constant Bookmark_View_Access := Bookmark_View_Access (Self);
      Filter_Iter : Gtk_Tree_Iter;
      Col         : Gtk_Tree_View_Column;
   begin
      Coordinates_For_Event (View.Tree, X, Y, Filter_Iter, Col);
      if Filter_Iter /= Null_Iter then
         View.Tree.Start_Editing
           (Render     => View.Tree.Text,
            Store_Iter => View.Tree.Convert_To_Store_Iter (Filter_Iter));
         View.Longpress.Set_State (Event_Sequence_Claimed);
      end if;
   end On_Longpress;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip         : not null access Bookmark_View_Tooltip_Handler;
      Widget          : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y            : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      Tree        : constant Bookmark_Tree := Bookmark_Tree (Widget);
      Filter_Path : Gtk_Tree_Path;
      Filter_Iter : Gtk_Tree_Iter;
      Data        : Bookmark_Data_Access;
      Text        : Unbounded_String;
      Area        : Gdk_Rectangle;
      Label       : Gtk_Label;
   begin
      Tooltips.Initialize_Tooltips (Tree, X, Y, Area, Filter_Iter);
      Tooltip.Set_Tip_Area (Area);

      if Filter_Iter /= Null_Iter then
         Data := Tree.Get_Data
           (Store_Iter => Tree.Convert_To_Store_Iter (Filter_Iter));
         case Data.Typ is
            when Group | Unattached =>
               Text := To_Unbounded_String
                 ("<b>Name:</b> " & Protect (Get_Name (Data)));
            when Standard =>
               declare
                  Location : constant String := To_String (Data.Marker);
               begin
                  if Location = Get_Name (Data) then
                     Text := To_Unbounded_String
                       ("<b>Location:</b> " & Protect (Location));
                  else
                     Text := To_Unbounded_String
                       ("<b>Name:</b> " & Protect (Get_Name (Data)) &
                          ASCII.LF & "<b>Location:</b> " & Protect (Location));
                  end if;
               end;
         end case;

         if Data.Note /= Null_Unbounded_String then
            Append (Text, ASCII.LF & "<b>Note:</b>" & ASCII.LF
                    & "<tt>" & Protect (To_String (Data.Note)) & "</tt>");
         end if;

         Gtk_New (Label, To_String (Text));
         Label.Set_Use_Markup (True);
         Path_Free (Filter_Path);
      end if;

      return Gtk_Widget (Label);
   end Create_Contents;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Bookmark : Bookmark_Data_Access) return String is
      S   : Unbounded_String := Bookmark.Name;
      Idx : Natural;
   begin
      --  Replace "%M" by actual marker location, which can be moved
      --  due to file modifications externally.
      if Bookmark.Typ = Standard
        and then Bookmark.Marker /= No_Marker
      then
         loop
            Idx := Index (S, "%M");
            exit when Idx = 0;

            Replace_Slice (S, Idx, Idx + 1, To_String (Bookmark.Marker));
         end loop;
      end if;

      return To_String (S);
   end Get_Name;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Bookmark_View_Record'Class) return Gtk_Widget
   is
      Col       : Gtk_Tree_View_Column;
      Num       : Gint with Unreferenced;
      Scrolled  : Gtk_Scrolled_Window;
      Pixbuf    : Gtk_Cell_Renderer_Pixbuf;
      Tooltip   : Tooltips.Tooltip_Handler_Access;
   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New (Scrolled);
      View.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      View.Tree := new Bookmark_Tree_Record;
      View.Tree.View := View;
      View.Tree.Initialize
           (Column_Types     => Column_Types,
            Capability_Type  => Filtered,
            Set_Visible_Func => True);
      View.Tree.Set_Name ("Bookmark TreeView"); --  For the testsuite
      View.Tree.Set_Search_Column (Name_Column);
      View.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Scrolled.Add (View.Tree);
      View.Tree.Set_Headers_Visible (False);

      Gtk_New (Col);
      Num := View.Tree.Append_Column (Col);

      Gtk_New (Pixbuf);
      Col.Pack_Start (Pixbuf, Expand => False);
      Col.Add_Attribute (Pixbuf, "icon_name",  Icon_Name_Column);

      Gtk_New (View.Tree.Note_Pixbuf);
      Col.Pack_Start (View.Tree.Note_Pixbuf, Expand => False);
      Set_Property (View.Tree.Note_Pixbuf, Icon_Name_Property, Icon_For_Tag);
      Col.Add_Attribute (View.Tree.Note_Pixbuf, "visible", Has_Note_Column);

      Gtk_New (View.Tree.Text);

      Set_Property
        (View.Tree.Text, Gtk.Cell_Renderer_Text.Editable_Property, False);
      Set_Property
        (View.Tree.Text, Gtk.Cell_Renderer_Text.Cancel_On_Focus_Out_Property,
         False);

      Col.Pack_Start (View.Tree.Text, Expand => False);
      Col.Add_Attribute (View.Tree.Text, "text", Name_Column);

      Gtk_New (View.Multipress, Widget => View.Tree);
      View.Multipress.On_Pressed (On_Multipress'Access, Slot => View);
      View.Multipress.Watch (View);

      Gtk_New (View.Longpress, Widget => View.Tree);
      View.Longpress.On_Pressed (On_Longpress'Access, Slot => View);
      View.Longpress.Watch (View);

      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View.Tree);

      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => View);
      Refresh (View);

      Bookmark_Added_Hook.Add (new Refresh_Hook, Watch => View);
      Bookmark_Removed_Hook.Add (new Refresh_Hook, Watch => View);
      Location_Changed_Hook.Add_Debounce (new On_Loc_Changed, Watch => View);

      Tooltip := new Bookmark_View_Tooltip_Handler;
      Tooltip.Associate_To_Widget (View.Tree);

      View.Tree.Set_Reorderable (True);
      Dest_Add_Text_Targets (View.Tree);
      View.Tree.On_Drag_Drop (On_Drag_Drop'Access, Slot => View);
      View.Tree.On_Drag_Data_Received
        (On_Drag_Data_Received'Access, Slot => View);

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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Self, File);

      procedure Remove_Project_Bookmarks (Data : Bookmark_Data_Access);
      --  Remove the project bookmarks

      ------------------------------
      -- Remove_Project_Bookmarks --
      ------------------------------

      procedure Remove_Project_Bookmarks (Data : Bookmark_Data_Access) is
         Tmp : Bookmark_Data_Access := Data;
         R   : Bookmark_Data_Access := null;

      begin
         while Tmp /= null loop
            case Tmp.Typ is
            when Group =>
               if Tmp.Contains = Project
                 or else Tmp.Contains = Both
               then
                  Remove_Project_Bookmarks (Tmp.First_Child);

                  if Tmp.Contains = Project then
                     R := Tmp;
                  end if;
               end if;

            when Standard =>
               if Tmp.On_Project = True then
                  R := Tmp;
               end if;

            when others =>
               null;
            end case;

            Tmp := Tmp.Next_Same_Level;

            if R /= null then
               Free (R);
               R := null;
            end if;
         end loop;
      end Remove_Project_Bookmarks;

   begin
      Set_Groups_Type;
      Save_Project_Bookmarks (Kernel);
      Remove_Project_Bookmarks (Bookmark_Iter_First);
      Bookmark_Removed_Hook.Run (Kernel, "");
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      Filename    : constant Virtual_File :=
        Create_From_Dir (Get_Home_Dir (Kernel), "bookmarks.xml");
      File        : Node_Ptr;
      Err         : GNAT.Strings.String_Access;
      Marker      : Location_Marker;

      procedure Load_Same_Level
        (Parent : Bookmark_Data_Access; First : Node_Ptr);
      --  Load all sibling bookmarks, recursively

      procedure Create_Messages (Bookmark : Bookmark_Data_Access);
      --  Create messages for new project to have markers in source buffers

      procedure Load_Project_Bookmarks;

      procedure Adjust_Marker
        (Marker : in out Location_Marker;
         Text   : String);
      --  Check whether the Marker line was moved and find it if so.
      --  Return Null marker if the line is not found.

      -------------------
      -- Adjust_Marker --
      -------------------

      procedure Adjust_Marker
        (Marker : in out Location_Marker;
         Text   : String)
      is
         Buf     : constant Editor_Buffer'Class :=
           Kernel.Get_Buffer_Factory.Get
             (Get_File (Marker),
              Open_Buffer => True,
              Open_View => False);
         Current : Integer := Integer (Get_Line (Marker));
         Min     : Integer := Current;
         Max     : Integer := Current;
         Forward : Boolean := True;
      begin
         if Text = "no-line-text"
           or else Buf = Nil_Editor_Buffer
         then
            return;
         end if;

         declare
            Line : constant Editor_Location'Class :=
              Buf.New_Location_At_Line (Current);
         begin
            if Buf.Get_Chars (Line, Line.End_Of_Line) = Text then
               --  The line has not been moved
               return;
            end if;
         end;

         --  From the current line, check if the line Current + X
         --  or else Current - X matches the Bookmark text
         loop
            if Min > 1 and then Max < Buf.Lines_Count then
               --  Moving either to the begin or to the end of the buffer
               if Forward then
                  Max     := Max + 1;
                  Current := Max;
                  Forward := False;

               else
                  Min     := Min - 1;
                  Current := Min;
                  Forward := True;
               end if;

            elsif Min > 1 then
               --  Moving to the begin of the buffer
               Min := Min - 1;
               Current := Min;

            elsif Max < Buf.Lines_Count then
               --  Moving to the end of the buffer
               Max := Max + 1;
               Current := Max;
            else
               --  All lines are tested, the line is not found
               exit;
            end if;

            declare
               Line : constant Editor_Location'Class :=
                 Buf.New_Location_At_Line (Current);
            begin
               if Buf.Get_Chars (Line, Line.End_Of_Line) = Text then
                  --  The line is found
                  Marker := Kernel.Get_Buffer_Factory.Create_Marker
                    (File   => Get_File (Marker),
                     Line   => Editable_Line_Type (Current),
                     Column => Get_Column (Marker));
                  return;
               end if;
            end;
         end loop;

         Marker := No_Marker;
      end Adjust_Marker;

      ---------------------
      -- Load_Same_Level --
      ---------------------

      procedure Load_Same_Level
        (Parent : Bookmark_Data_Access; First : Node_Ptr)
      is
         Child, Tmp : Node_Ptr := First;
         B     : Bookmark_Data_Access;
         Last  : Bookmark_Data_Access := null;
      begin
         while Child /= null loop
            B := null;

            if Child.Tag.all = "group" then
               B := New_Group (Get_Attribute (Child, "bookmark_name", ""));
               Load_Same_Level (Parent => B, First => Child.Child);

            elsif Child.Tag.all = "unattached" then
               B := New_Bookmark
                 (Kernel, No_Marker,
                  Name       => Encoded_ASCII_To_String
                    (Get_Attribute (Child, "bookmark_name", "")),
                  On_Project => False);

            else
               Marker := Create_Marker (Kernel, Child);

               if not Marker.Is_Null then
                  if Dynamically_Moving.Get_Pref then
                     Adjust_Marker
                       (Marker,
                        Encoded_ASCII_To_String
                          (Get_Attribute
                               (Child, Line_Text_Attribute, "no-line-text")));
                  end if;

                  if not Marker.Is_Null then
                     B := New_Bookmark
                       (Kernel,
                        Marker,
                        Name =>
                          Get_Attribute (Child, "bookmark_name", ""),
                        On_Project => False);
                  end if;
               end if;
            end if;

            if B /= null then
               --  Load notes

               Tmp := Child.Child;
               while Tmp /= null loop
                  if Tmp.Tag.all = "note" then
                     Append (B.Note, Encoded_ASCII_To_String (Tmp.Value.all));
                  end if;
                  Tmp := Tmp.Next;
               end loop;

               --  Insert in the same order we read them, so append
               Insert (B, After => Last, In_Group => Parent);
               Last := B;
            end if;

            Child := Child.Next;
         end loop;
      end Load_Same_Level;

      ----------------------------
      -- Load_Project_Bookmarks --
      ----------------------------

      procedure Load_Project_Bookmarks
      is
         Prop  : Bookmarks_Property_Record;
         Found : Boolean;

         -------------
         -- Restore --
         -------------

         procedure Restore (Parent, First : Bookmark_Data_Access);
         procedure Restore (Parent, First : Bookmark_Data_Access) is
            Tmp      : Bookmark_Data_Access := First;
            B        : Bookmark_Data_Access;
            Last     : Bookmark_Data_Access := null;
            Updating : Boolean;

         begin
            while Tmp /= null loop
               B        := null;
               Updating := False;

               case Tmp.Typ is
                  when Group =>
                     --  Check wether we already have this group!
                     if Parent = null then
                        B := Bookmark_Iter_First;
                     else
                        B := Parent.First_Child;
                     end if;

                     while B /= null loop
                        exit when B.Name = Tmp.Name;
                        B := B.Next_Same_Level;
                     end loop;

                     if B = null then
                        B := New_Group (To_String (Tmp.Name));
                     else
                        Updating := True;
                     end if;

                     Restore (B, Tmp.First_Child);

                  when Standard =>
                     if Tmp.Marker_Data /= JSON_Null then
                        declare
                           Marker : Location_Marker :=
                             Kernel.Get_Buffer_Factory.Create_Marker
                               (File   => JSON_Utils.Load
                                  (Get (Tmp.Marker_Data, "file")),
                                Line   => Editable_Line_Type'Value
                                  (Get (Tmp.Marker_Data, "line")),
                                Column => Visible_Column_Type'Value
                                  (Get (Tmp.Marker_Data, "column")));
                        begin
                           if Marker /= No_Marker then
                              if Dynamically_Moving.Get_Pref
                                and then not Tmp.Line_Text.Is_Empty
                              then
                                 Adjust_Marker
                                   (Marker,
                                    To_String (Tmp.Line_Text.Text));
                              end if;

                              if not Marker.Is_Null then
                                 B := New_Bookmark
                                   (Kernel,
                                    Marker,
                                    Name          => To_String (Tmp.Name),
                                    On_Project    => True,
                                    Preserve_Name => True);
                              end if;
                           end if;
                        end;
                     end if;

                  when Unattached =>
                     null;
               end case;

               if B /= null
                 and then not Updating
               then
                  --  If B is just created
                  --  Insert in the same order we read them, so append
                  Insert (B, After => Last, In_Group => Parent);
                  Last := B;
               end if;

               Tmp := Tmp.Next_Same_Level;
            end loop;
         end Restore;

      begin
         Trace (Me, "Loading project bookmarks");

         Get_Property
           (Prop, Get_Project (Kernel), Name => "bookmarks", Found => Found);

         if Found
           and then Prop.Root /= null
         then
            Restore
              (Parent => null,
               First  => Prop.Root.First_Child);
         end if;

         Trace (Me, "Project bookmarks are loaded");
      end Load_Project_Bookmarks;

      ---------------------
      -- Create_Messages --
      ---------------------

      procedure Create_Messages (Bookmark : Bookmark_Data_Access) is
         Current : Bookmark_Data_Access := Bookmark;
      begin
         while Current /= null loop
            case Current.Typ is
               when Group =>
                  Create_Messages (Current.First_Child);

               when Standard =>
                  if not Current.On_Project
                    and then Current.Marker /= No_Marker
                    and then Current.Message.Is_Empty
                  then
                     Current.Message := Create
                       (Message_Access
                          (Create_Message (Kernel, Current.Marker)));
                  end if;

               when Unattached =>
                  null;
            end case;

            Current := Current.Next_Same_Level;
         end loop;
      end Create_Messages;

   begin
      --  For now, the list of bookmarks is global to all projects, so we only
      --  need to load once (but we can't do it from Register_Module directly,
      --  since messages are not initialized yet)

      if Bookmark_Views_Module.Loaded then
         Load_Project_Bookmarks;
         Create_Messages (Bookmark_Views_Module.Root);
         Bookmark_Added_Hook.Run (Kernel, "");
         return;
      end if;

      Bookmark_Views_Module.Loaded := True;

      if Filename.Is_Regular_File
        and then Filename.Is_Readable
      then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);

         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
            Free (Err);

         else
            Load_Same_Level (null, First => File.Child);
            Free (File);
         end if;

         Load_Project_Bookmarks;
         Bookmark_Added_Hook.Run (Kernel, "");
      else
         Trace
           (Me,
            "Could not load " & Filename.Display_Full_Name
            & ": the file is not readable or does no exist on disk");
      end if;
   end Execute;

   ---------
   -- Set --
   ---------

   procedure Set
     (To   : in out Group_Content_Type;
      From : Group_Content_Type) is
   begin
      if From = Unknown then
         return;
      end if;

      if To = Unknown then
         To := From;
      else
         To := Both;
      end if;
   end Set;

   ---------------------
   -- Set_Groups_Type --
   ---------------------

   procedure Set_Groups_Type is

      procedure Clear_Groups_Contains (Data : Bookmark_Data_Access);
      --  Set Groups.Contains to Unknown which is used as an initial value

      function Set_Contains
        (Data : Bookmark_Data_Access)
         return Group_Content_Type;
      --  Set Groups.Contains to real value based on content of group itself
      --   and all its nested elements.

      ---------------------------
      -- Clear_Groups_Contains --
      ---------------------------

      procedure Clear_Groups_Contains (Data : Bookmark_Data_Access) is
         Tmp : Bookmark_Data_Access := Data;
      begin
         while Tmp /= null loop
            case Tmp.Typ is
               when Group =>
                  Tmp.Contains := Unknown;
                  Clear_Groups_Contains (Tmp.First_Child);

               when others =>
                  null;
            end case;

            Tmp := Tmp.Next_Same_Level;
         end loop;
      end Clear_Groups_Contains;

      ------------------
      -- Set_Contains --
      ------------------

      function Set_Contains
        (Data : Bookmark_Data_Access)
         return Group_Content_Type
      is
         Tmp    : Bookmark_Data_Access := Data;
         Result : Group_Content_Type := Unknown;
      begin
         while Tmp /= null loop
            case Tmp.Typ is
               when Group =>
                  Set (Result, Set_Contains (Tmp.First_Child));
                  Set (Tmp.Contains, Result);

               when Standard =>
                  if Tmp.On_Project = True then
                     Set (Result, Project);
                  else
                     Set (Result, Global);
                  end if;

               when Unattached =>
                  Set (Result, Global);
            end case;

            Tmp := Tmp.Next_Same_Level;
         end loop;

         return Result;
      end Set_Contains;

      Dummy : Group_Content_Type;
   begin
      Clear_Groups_Contains (Bookmark_Iter_First);
      Dummy := Set_Contains (Bookmark_Iter_First);
   end Set_Groups_Type;

   --------------------
   -- Save_Bookmarks --
   --------------------

   procedure Save_Bookmarks (Kernel : access Kernel_Handle_Record'Class) is
      Filename    : constant Virtual_File :=
        Create_From_Dir (Get_Home_Dir (Kernel), "bookmarks.xml");
      File        : Node_Ptr;

      procedure Save_Same_Level
        (Parent : Node_Ptr; First : Bookmark_Data_Access);
      --  Save all sibling bookmarks, recursively

      ---------------------
      -- Save_Same_Level --
      ---------------------

      procedure Save_Same_Level
        (Parent : Node_Ptr; First : Bookmark_Data_Access)
      is
         Tmp   : Bookmark_Data_Access := First;
         Child, Note : Node_Ptr;
      begin
         while Tmp /= null loop
            Child := null;

            case Tmp.Typ is
               when Group =>
                  Child := new Node;
                  Child.Tag := new String'("group");
                  Save_Same_Level (Child, Tmp.First_Child);

               when Unattached =>
                  Child := new Node;
                  Child.Tag := new String'("unattached");

               when Standard =>
                  if Tmp.On_Project /= True then
                     Child := Save (Tmp.Marker);

                     if Dynamically_Moving.Get_Pref then
                        declare
                           Buf : constant Editor_Buffer'Class :=
                             Kernel.Get_Buffer_Factory.Get
                               (Get_File (Tmp.Marker),
                                Open_Buffer => True,
                                Open_View => False);
                        begin
                           if Buf /= Nil_Editor_Buffer then
                              declare
                                 Line : constant Editor_Location'Class :=
                                   Buf.New_Location_At_Line
                                     (Integer (Get_Line (Tmp.Marker)));
                              begin
                                 Set_Attribute
                                   (Child, Line_Text_Attribute,
                                    String_To_Encoded_ASCII
                                      (Buf.Get_Chars
                                           (Line, Line.End_Of_Line)));
                              end;
                           end if;
                        end;
                     end if;
                  end if;
            end case;

            if Child /= null then
               Set_Attribute
                 (Child, "bookmark_name",
                  String_To_Encoded_ASCII (To_String (Tmp.Name)));

               Add_Child (Parent, Child, Append => True);

               if Tmp.Note /= Null_Unbounded_String then
                  Note       := new Node;
                  Note.Tag   := new String'("note");
                  Note.Value := new String'
                    (String_To_Encoded_ASCII (To_String (Tmp.Note)));
                  Add_Child (Child, Note, Append => True);
               end if;
            end if;

            Tmp := Tmp.Next_Same_Level;
         end loop;
      end Save_Same_Level;

      Success : Boolean := True;
   begin
      Trace (Me, "Saving " & Filename.Display_Full_Name);

      Set_Groups_Type;

      File := new Node;
      File.Tag := new String'("Bookmarks");
      Save_Same_Level (File, Bookmark_Iter_First);

      begin
         Print (File, Filename, Success);
      exception
         when E : others =>
            if not Active (Testsuite_Handle) then
               --  Skip error for testsuite
               Trace (Me, E);
               Success := False;
            end if;
      end;

      Free (File);

      Save_Project_Bookmarks (Kernel);

      if not Success then
         Report_Preference_File_Error (Kernel, Filename);
      end if;
   end Save_Bookmarks;

   ----------------------------
   -- Save_Project_Bookmarks --
   ----------------------------

   procedure Save_Project_Bookmarks
     (Kernel : access Kernel_Handle_Record'Class)
   is
      P : constant Bookmarks_Property := new Bookmarks_Property_Record;

      procedure Clone_Same_Level (Parent, First : Bookmark_Data_Access);
      --  Store all sibling bookmarks in the property, recursively

      ----------------------
      -- Clone_Same_Level --
      ----------------------

      procedure Clone_Same_Level (Parent, First : Bookmark_Data_Access)
      is
         Tmp  : Bookmark_Data_Access := First;
         Last : Bookmark_Data_Access := null;
      begin
         while Tmp /= null loop
            declare
               B : Bookmark_Data_Access := null;
            begin
               case Tmp.Typ is
                  when Group =>
                     if Tmp.Contains = Project
                       or else Tmp.Contains = Both
                     then
                        B := Clone (Tmp);
                        Clone_Same_Level
                          (Parent => B, First => Tmp.First_Child);
                     end if;

                  when Standard =>
                     if Tmp.On_Project = True
                       and then Tmp.Marker /= No_Marker
                     then
                        B := Clone (Tmp);

                        B.Marker_Data := Create_Object;
                        B.Marker_Data.Set_Field
                          ("file", JSON_Utils.Save (Get_File (Tmp.Marker)));
                        B.Marker_Data.Set_Field
                          ("line", Editable_Line_Type'Image
                             (Get_Line (Tmp.Marker)));
                        B.Marker_Data.Set_Field
                          ("column", Visible_Column_Type'Image
                             (Get_Column (Tmp.Marker)));

                        if Dynamically_Moving.Get_Pref then
                           declare
                              Buf : constant Editor_Buffer'Class :=
                                Kernel.Get_Buffer_Factory.Get
                                  (Get_File (Tmp.Marker),
                                   Open_Buffer => True,
                                   Open_View => False);
                           begin
                              if Buf /= Nil_Editor_Buffer then
                                 declare
                                    Line : constant Editor_Location'Class :=
                                      Buf.New_Location_At_Line
                                        (Integer (Get_Line (Tmp.Marker)));
                                 begin
                                    B.Line_Text :=
                                      (False,
                                       To_Unbounded_String
                                         (Buf.Get_Chars
                                              (Line, Line.End_Of_Line)));
                                 end;
                              end if;
                           end;
                        end if;
                     end if;

                  when Unattached =>
                     null;
               end case;

               if B /= null then
                  Insert (B, After => Last, In_Group => Parent);
                  Last := B;
               end if;
            end;

            Tmp := Tmp.Next_Same_Level;
         end loop;
      end Clone_Same_Level;

   begin
      Trace (Me, "Saving project bookmarks");
      P.Root := new Bookmark_Data (Group);
      Clone_Same_Level (Parent => P.Root, First => Bookmark_Iter_First);

      Set_Property
        (Kernel     => Kernel,
         Project    => Get_Project (Kernel),
         Name       => "bookmarks",
         Property   => Property_Access (P),
         Persistent => True);

      Trace (Me, "Project bookmarks are saved");
   end Save_Project_Bookmarks;

   ------------------------
   -- Bookmark_From_Name --
   ------------------------

   function Bookmark_From_Name (Name : String) return Bookmark_Data_Access is
      B : Bookmark_Data_Access := Bookmark_Iter_First;
   begin
      while B /= null loop
         if Get_Name (B) = Name then
            return B;
         end if;
         B := Next_Recursive (B);
      end loop;
      return null;
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
      After_Cst      : aliased constant String := "after";
      Inst           : Class_Instance;
      Bookmark       : Bookmark_Data_Access;
      Marker         : Location_Marker;
      Tmp            : Boolean;
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
         if Bookmark = null then
            Set_Error_Msg (Data, "No such bookmark");
         else
            Data.Set_Return_Value
               (Bookmark_Proxies.Get_Or_Create_Instance
                  (Self   => Bookmark.Instances,
                   Obj    => Bookmark,
                   Script => Data.Get_Script));
         end if;

      elsif Command = "create" then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Marker := Create_Marker (Kernel);
         if Marker.Is_Null then
            Set_Error_Msg (Data, "Can't create bookmark for this context");
         else
            Bookmark := New_Bookmark
              (Kernel, Marker, Name => Data.Nth_Arg (1), On_Project => False);
            Insert (Bookmark, After => null, In_Group => null);
            Save_Bookmarks (Kernel);
            Bookmark_Added_Hook.Run (Kernel, Get_Name (Bookmark));
            Data.Set_Return_Value
               (Bookmark_Proxies.Get_Or_Create_Instance
                  (Self   => Bookmark.Instances,
                   Obj    => Bookmark,
                   Script => Data.Get_Script));
         end if;

      elsif Command = "create_group" then
         Name_Parameters (Data, (1 => Name_Cst'Unchecked_Access));
         Bookmark := New_Group (Name => Data.Nth_Arg (1));
         Insert (Bookmark, After => null, In_Group => null);
         Save_Bookmarks (Kernel);
         Bookmark_Added_Hook.Run (Kernel, Get_Name (Bookmark));

      elsif Command = "name" then
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         Data.Set_Return_Value
            (Get_Name (Bookmark_Proxies.From_Instance (Inst)));

      elsif Command = "note" then
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         Data.Set_Return_Value
            (To_String (Bookmark_Proxies.From_Instance (Inst).Note));

      elsif Command = "delete" then
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         B := Bookmark_Proxies.From_Instance (Inst);
         if B = null then
            Data.Set_Error_Msg ("Invalid bookmark");
         else
            Delete_Bookmark (Kernel, B);
         end if;

      elsif Command = "rename" then
         Name_Parameters (Data, (2 => Name_Cst'Unchecked_Access));
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         B := Bookmark_Proxies.From_Instance (Inst);
         if B = null then
            Data.Set_Error_Msg ("Invalid bookmark");
         else
            Bookmark_Removed_Hook.Run (Kernel, Get_Name (B));
            B.Name := To_Unbounded_String (String'(Nth_Arg (Data, 2)));
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

      elsif Command = "reorder" then
         Name_Parameters (Data, (2 => After_Cst'Unchecked_Access));
         Inst := Data.Nth_Arg (1, Bookmark_Class);
         B := Bookmark_Proxies.From_Instance (Inst);

         if Data.Number_Of_Arguments > 1 then
            Inst := Data.Nth_Arg (2, Bookmark_Class);
            Bookmark := Bookmark_Proxies.From_Instance (Inst);
         end if;

         if B = null then
            Data.Set_Error_Msg ("Invalid bookmark");
         elsif Bookmark /= null and then B.Parent /= Bookmark.Parent then
            Data.Set_Error_Msg ("Not in the same group");
         else
            declare
               Parent : constant Bookmark_Data_Access := B.Parent;
               View   : constant Bookmark_View_Access :=
                 Generic_View.Retrieve_View (Kernel);
            begin
               Remove_But_Not_Free (B);

               Insert
                 (B,
                  After    => Bookmark,
                  In_Group => Parent);

               View.Refresh (Selected => B);
               Save_Bookmarks (Kernel);
            end;
         end if;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);
         Bookmark := Bookmark_Iter_First;
         while Bookmark /= null loop
            if Bookmark.Typ = Standard then
               Data.Set_Return_Value
                 (Bookmark_Proxies.Get_Or_Create_Instance
                    (Self   => Bookmark.Instances,
                     Obj    => Bookmark,
                     Script => Data.Get_Script));
            end if;
            Bookmark := Next_Recursive (Bookmark);
         end loop;
      end if;
   end Command_Handler;

   ---------------------------
   -- Write_Command_Handler --
   ---------------------------

   procedure Write_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Data);
      Class    : constant Class_Type :=
         Kernel.Scripts.New_Class (Bookmark_Class_Name);
      Inst     : constant Class_Instance := Data.Nth_Arg (1, Class);
      Note     : constant String := Data.Nth_Arg (2);
      Bookmark : constant Bookmark_Data_Access :=
           Bookmark_Proxies.From_Instance (Inst);
   begin
      if Command = "note" then
         Bookmark.Note :=
           To_Unbounded_String (Note);
         Bookmark_Added_Hook.Run (Kernel, Get_Name (Bookmark));
         Save_Bookmarks (Kernel);
      end if;
   end Write_Command_Handler;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Bookmarks_Property_Record;
      Value    : in out GNATCOLL.JSON.JSON_Value)
   is
      procedure Save_Same_Level
        (Values : in out JSON_Array;
         First  : Bookmark_Data_Access);
      --  Save all sibling bookmarks, recursively

      ---------------------
      -- Save_Same_Level --
      ---------------------

      procedure Save_Same_Level
        (Values : in out JSON_Array;
         First  : Bookmark_Data_Access)
      is
         Tmp : Bookmark_Data_Access := First;

      begin
         while Tmp /= null loop
            declare
               Value : JSON_Value := JSON_Null;

            begin
               case Tmp.Typ is
                  when Group =>
                     declare
                        Children : JSON_Array;
                     begin
                        Value := Create_Object;
                        Save_Same_Level (Children, Tmp.First_Child);
                        if Is_Empty (Children) then
                           --  Empty group, do not store
                           Value := JSON_Null;
                        else
                           Value.Set_Field ("children", Children);
                        end if;
                     end;

                  when Standard =>
                     if Tmp.Marker_Data /= JSON_Null then
                        Value := Create_Object;
                        Value.Set_Field ("marker", Tmp.Marker_Data);

                        if Dynamically_Moving.Get_Pref
                          and then not Tmp.Line_Text.Is_Empty
                        then
                           Value.Set_Field
                             (Line_Text_Attribute,
                              To_String (Tmp.Line_Text.Text));
                        end if;
                     end if;

                  when Unattached =>
                     null;
               end case;

               if Value /= JSON_Null then
                  Value.Set_Field ("type", Bookmark_Type'Image (Tmp.Typ));

                  if Tmp.Name /= "" then
                     Value.Set_Field ("name", To_String (Tmp.Name));
                  end if;

                  if Tmp.Note /= "" then
                     Value.Set_Field
                       ("note",
                        String_To_Encoded_ASCII (To_String (Tmp.Note)));
                  end if;

                  Append (Values, Value);
               end if;
            end;

            Tmp := Tmp.Next_Same_Level;
         end loop;
      end Save_Same_Level;

      Values : JSON_Array;

   begin
      Save_Same_Level (Values, Property.Root.First_Child);
      Value.Set_Field ("bookmarks", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Bookmarks_Property_Record;
      Value    : GNATCOLL.JSON.JSON_Value)
   is
      procedure Load_Level
        (Item   : JSON_Value;
         Parent : Bookmark_Data_Access;
         Last   : in out Bookmark_Data_Access);
      --  Load all sibling bookmarks, recursively

      ----------------
      -- Load_Level --
      ----------------

      procedure Load_Level
        (Item   : JSON_Value;
         Parent : Bookmark_Data_Access;
         Last   : in out Bookmark_Data_Access)
      is
         T      : constant Bookmark_Type :=
           Bookmark_Type'Value (Item.Get ("type"));
         N      : constant String :=
           (if Item.Has_Field ("name")
            then Item.Get ("name")
            else "");

         B      : Bookmark_Data_Access;
      begin
         case T is
            when Group =>
               declare
                  Values : JSON_Array;
                  L      : Bookmark_Data_Access := null;
               begin
                  B := New_Group (N);

                  if Value.Has_Field ("children") then
                     Values := Value.Get ("children");
                     for Index in 1 .. Length (Values) loop
                        Load_Level (Get (Values, Index), B, L);
                     end loop;
                  end if;
               end;

            when Standard =>
               if Item.Has_Field ("marker") then
                  B := new Bookmark_Data (Standard);

                  B.Name        := To_Unbounded_String (N);
                  B.On_Project  := True;
                  B.Marker_Data := Item.Get ("marker");

                  if Dynamically_Moving.Get_Pref
                    and then Item.Has_Field (Line_Text_Attribute)
                  then
                     B.Line_Text :=
                       (False,
                        To_Unbounded_String
                          (String'(Item.Get (Line_Text_Attribute))));
                  end if;
               end if;

            when Unattached =>
               null;
         end case;

         if B /= null then
            if Item.Has_Field ("note") then
               Append (B.Note, Encoded_ASCII_To_String (Item.Get ("note")));
            end if;

            Insert (B, After => Last, In_Group => Parent);
            Last := B;
         end if;
      end Load_Level;

      Values : JSON_Array;
      Last   : Bookmark_Data_Access := null;

   begin
      if not Value.Has_Field ("bookmarks") then
         return;
      end if;

      Values := Value.Get ("bookmarks");
      if Property.Root /= null then
         Destroy (Property);
      end if;

      Property.Root := new Bookmark_Data (Group);

      for Index in 1 .. Length (Values) loop
         Load_Level (Get (Values, Index), Property.Root, Last);
      end loop;
   end Load;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Property : in out Bookmarks_Property_Record)
   is
      procedure Remove (Data : Bookmark_Data_Access);
      procedure Remove (Data : Bookmark_Data_Access) is
         Tmp : Bookmark_Data_Access := Data;
         R   : Bookmark_Data_Access;

      begin
         while Tmp /= null loop
            if Tmp.Typ = Group then
               Remove (Tmp.First_Child);
            end if;

            R   := Tmp;
            Tmp := Tmp.Next_Same_Level;
            Free (R);
         end loop;
      end Remove;

   begin
      if Property.Root /= null then
         Remove (Property.Root.First_Child);
         Free (Property.Root);
      end if;
   end Destroy;

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

      P                  : Kernel_Search_Provider_Access;

   begin
      Bookmark_Views_Module := new Bookmark_Views_Module_Record;
      Generic_View.Register_Module (Kernel, Module_ID (Bookmark_Views_Module));

      Register_Action
        (Kernel, "bookmark rename", new Rename_Bookmark_Command,
         -("Interactively rename the bookmark currently selected in the"
           & " bookmarks view"), Category => -"Bookmarks",
         Icon_Name => "gps-rename-symbolic");

      Register_Action
        (Kernel, "bookmark remove selected", new Delete_Bookmark_Command,
         -"Delete the selected bookmarks in the bookmarks view",
         Icon_Name => "gps-remove-symbolic",
         Category => -"Bookmarks");

      Register_Action
        (Kernel, "bookmark create", new Create_Bookmark_Command,
         -("Create a bookmark at the current location in the editor"),
         Icon_Name    => "gps-add-symbolic",
         Category     => -"Bookmarks",
         For_Learning => True);

      Register_Action
        (Kernel, "project bookmark create",
         new Create_Bookmark_Command'
           (Interactive_Command with
                Mode             => Standard,
                Ignore_Selection => False,
                On_Project       => True),
         -("Create a project bookmark at the current location in the editor"),
         Icon_Name    => "gps-add-symbolic",
         Category     => -"Bookmarks",
         For_Learning => True);

      Register_Action
        (Kernel, "bookmark create unattached",
         new Create_Bookmark_Command'
           (Interactive_Command with
                Mode             => Unattached,
                Ignore_Selection => False,
                On_Project       => False),
         -("Create a bookmark at no specific location. This is mostly useful"
           & " as a way to have TODO items into the Bookmarks view"),
         Icon_Name => "gps-add-symbolic",
         Category  => -"Bookmarks");

      Register_Action
        (Kernel, "bookmark create group",
         new Create_Bookmark_Command'
           (Interactive_Command with
                Mode             => Group,
                Ignore_Selection => False,
                On_Project       => False),
         -("Create an empty bookmark group using the selection"),
         Icon_Name => "gps-emblem-directory-symbolic",
         Category  => -"Bookmarks");

      Register_Action
        (Kernel, "bookmark create root group",
         new Create_Bookmark_Command'
           (Interactive_Command with
                Mode             => Group,
                Ignore_Selection => True,
                On_Project       => False),
         -("Create an empty root bookmark group"),
         Icon_Name => "gps-emblem-directory-root-symbolic",
         Category  => -"Bookmarks");

      Register_Action
        (Kernel, "bookmark edit note", new Edit_Note_Command,
         -("Edit the note associated with the selected bookmark"),
         Icon_Name   => Icon_For_Tag,
         Category    => -"Bookmarks");

      Register_Action
        (Kernel       => Kernel,
         Name         => "Goto Next Bookmark",
         Command      => new Next_Bookmark_Command (Backward => False),
         Description  => -("Go to next bookmark in current file"),
         Filter       => Src_Action_Context,
         Category     => -"Bookmarks",
         For_Learning => True);

      Register_Action
        (Kernel       => Kernel,
         Name         => "Goto Previous Bookmark",
         Command      => new Next_Bookmark_Command (Backward => True),
         Description  => -("Go to previous bookmark in current file"),
         Filter       => Src_Action_Context,
         Category     => -"Bookmarks",
         For_Learning => True);

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
        (Kernel, "create_group", 1, 1, Command_Handler'Access, Bookmark_Class,
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
      Register_Command
        (Kernel, "reorder", 0, 1, Command_Handler'Access, Bookmark_Class);

      Kernel.Scripts.Register_Property
        (Name   => "note",
         Class  => Bookmark_Class,
         Setter => Write_Command_Handler'Access,
         Getter => Command_Handler'Access);

      P := new Bookmarks_Search_Provider;
      Register_Provider_And_Action (Kernel, P);

      Editor_Link := Kernel.Get_Preferences.Create_Invisible_Pref
        ("bookmark-editor-link", True, Label => -"Dynamic link with editor");

      Append_At_Bottom := Kernel.Get_Preferences.Create_Invisible_Pref
        ("bookmark-editor-add-to-end",
         False,
         Label => -"Place new bookmark at the bottom");

      Dynamically_Moving := Kernel.Get_Preferences.Create_Invisible_Pref
        ("bookmark-editor-dynamic",
         False,
         Label => -"Move dynamically",
         Doc => -"When enabled, GNAT Studio will try to move the bookmarks" &
           " dynamically if the location of the pointed code changes (i.e:" &
           " due to a Version Control system checkout).");

      Project_Changed_Hook.Add (new On_Project_Changed);
      Project_Changing_Hook.Add (new On_Project_Changing);
      Buffer_Before_Delete_Lines_Hook.Add (new On_Lines_Deleted);
   end Register_Module;

end Bookmark_Views;
