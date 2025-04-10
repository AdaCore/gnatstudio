------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2025, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Text_IO;                      use Ada.Text_IO;
with Ada.Strings.Unbounded;            use Ada.Strings.Unbounded;

with VSS.Strings.Conversions;

with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;              use GNATCOLL.VFS.GtkAda;

with Gdk.Event;                        use Gdk.Event;

with Glib;                             use Glib;
with Glib.Convert;
with Glib.Main;                        use Glib.Main;
with Glib.Object;                      use Glib.Object;
with Glib.Values;                      use Glib.Values;

with Gtk.Box;                          use Gtk.Box;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu;                         use Gtk.Menu;
with Gtk.Scrolled_Window;              use Gtk.Scrolled_Window;
with Gtk.Toolbar;                      use Gtk.Toolbar;
with Gtk.Tree_Selection;               use Gtk.Tree_Selection;

with Gtkada.File_Selector;
with Gtkada.Handlers;                  use Gtkada.Handlers;
with Gtkada.MDI;                       use Gtkada.MDI;

with Basic_Types;                      use Basic_Types;
with Commands.Interactive;             use Commands.Interactive;
with Default_Preferences;              use Default_Preferences;
with Generic_Views;
with GPS.Editors;                      use GPS.Editors;
with GPS.Editors.GtkAda;               use GPS.Editors.GtkAda;
with GPS.Editors.Line_Information;     use GPS.Editors.Line_Information;
with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel.Actions;               use GPS.Kernel.Actions;
with GPS.Kernel.Clipboard;             use GPS.Kernel.Clipboard;
with GPS.Kernel.Contexts;              use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Multilines;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Modules;               use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;            use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;               use GPS.Kernel.Scripts;
with GPS.Kernel.Style_Manager;         use GPS.Kernel.Style_Manager;
with GPS.Location_View_Filter;         use GPS.Location_View_Filter;
with GPS.Location_View.Listener;       use GPS.Location_View.Listener;
with GPS.Search;                       use GPS.Search;
with GPS.Tree_View;                    use GPS.Tree_View;
with GPS.Tree_View.Locations;          use GPS.Tree_View.Locations;
with GUI_Utils;                        use GUI_Utils;
with Histories;                        use Histories;
with Filter_Panels;                    use Filter_Panels;
with String_Utils;                     use String_Utils;

package body GPS.Location_View is

   Sort_By_Subcategory     : Boolean_Preference;
   Auto_Jump_To_First      : Boolean_Preference;
   Locations_Wrap          : Boolean_Preference;
   Auto_Close              : Boolean_Preference;
   Sort_Files_Alphabetical : Boolean_Preference;

   Locations_View_Name : constant String := "Locations";

   Locations_Message_Flags : constant GPS.Kernel.Messages.Message_Flags :=
     (GPS.Kernel.Messages.Editor_Side => False,
      GPS.Kernel.Messages.Editor_Line => False,
      GPS.Kernel.Messages.Locations   => True);

   type Locations_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Locations_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   type Expansion_Request is record
      Category   : VSS.Strings.Virtual_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Goto_First : Boolean;
   end record;

   package Expansion_Request_Vectors is
     new Ada.Containers.Vectors (Positive, Expansion_Request);

   type Location_View_Record is new Generic_Views.View_Record with record
      View                : GPS_Locations_Tree_View;

      --  Idle handlers

      Idle_Expand_Handler : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      Requests            : Expansion_Request_Vectors.Vector;
      --  Expansion requests.

      --  Message listener
      Listener            : GPS.Kernel.Messages.Listener_Access;
   end record;

   overriding procedure Create_Toolbar
     (View    : not null access Location_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Location_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Location_View_Record;
      Pattern : in out Search_Pattern_Access);
   overriding procedure On_Create
     (Self  : not null access Location_View_Record;
      Child : not null access GPS_MDI_Child_Record'Class);

   function Initialize
     (Self   : access Location_View_Record'Class)
      return Gtk_Widget;
   --  Creates the locations view, and returns the focus widget

   package Location_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Location_View_Record",
      View_Name          => Locations_View_Name,
      Formal_View_Record => Location_View_Record,
      Formal_MDI_Child   => Locations_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => Group_Consoles);
   use Location_Views;
   subtype Location_View is Location_Views.View_Access;

   package View_Idle is new Glib.Main.Generic_Sources (Location_View);

   type On_Location_Changed is new File_Location_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project : Project_Type);
   --  Called whenever the location in the current editor has changed, so that
   --  we can highlight the corresponding line in the locations window

   function Idle_Expand (Self : Location_View) return Boolean;
   --  Idle callback used to expand nodes of category and its first or defined
   --  file; select first message and the open first location if requested.

   function Is_Parent_Selected
     (Selection : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Path      : Gtk_Tree_Path;
      Depth     : Gint := 0)
      return Boolean;
   --  Check whether the one of parents is selected. Parent node well be
   --  checked if Depth is 0 or depth of node less or equal Depth.

   package Select_Function_With_View is
     new Set_Select_Function_User_Data (Location_View);

   function Selection_Function
     (Selection               : not null access
        Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
      Path                    : Gtk.Tree_Model.Gtk_Tree_Path;
      Path_Currently_Selected : Boolean;
      View                    : Location_View) return Boolean;
   --  Prevents unselect messages when action clicked

   -------------
   -- Actions --
   -------------

   type Clear_Locations_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Clear_Locations_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Removes all messages

   type Remove_Selection_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_Selection_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Removes selected message

   type Selection_To_String_Action is (Export, Clipboard);
   type Selection_To_String_Command (Action : Selection_To_String_Action)
   is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Selection_To_String_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Export selection to a text file

   type Toggle_Sort_By_Subcategory_Command is new Interactive_Command
     with null record;
   overriding function Execute
     (Self    : access Toggle_Sort_By_Subcategory_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Changes sort order in locations view.

   type Locations_Collapse_Or_Expand_Command
     (Command : Expansion_Command_Type) is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Locations_Collapse_Or_Expand_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Collapse or Expand the selected files

   --------------
   -- Messages --
   --------------

   type View_Manager
     (Kernel : not null access Kernel_Handle_Record'Class) is
     new Abstract_Listener with null record;
   type View_Manager_Access is access all View_Manager'Class;
   overriding procedure Message_Added
     (Self    : not null access View_Manager;
      Message : not null access Abstract_Message'Class);
   overriding procedure Category_Added
     (Self     : not null access View_Manager;
      Category : VSS.Strings.Virtual_String;
      Allow_Auto_Jump_To_First : Boolean);
   --  Monitoring messages.
   --  ??? Can this be done simply by monitoring the model instead ? We are at
   --  at a low-level anyway here

   ---------------------
   -- Local constants --
   ---------------------

   Output_Cst        : aliased constant String := "output";
   Category_Cst      : aliased constant String := "category";
   Regexp_Cst        : aliased constant String := "regexp";
   File_Index_Cst    : aliased constant String := "file_index";
   Line_Index_Cst    : aliased constant String := "line_index";
   Col_Index_Cst     : aliased constant String := "column_index";
   Msg_Index_Cst     : aliased constant String := "msg_index";
   Style_Index_Cst   : aliased constant String := "style_index";
   Warning_Index_Cst : aliased constant String := "warning_index";
   File_Cst          : aliased constant String := "file";
   Line_Cst          : aliased constant String := "line";
   Column_Cst        : aliased constant String := "column";
   Message_Cst       : aliased constant String := "message";
   Highlight_Cst     : aliased constant String := "highlight";
   Length_Cst        : aliased constant String := "length";
   Highlight_Cat_Cst : aliased constant String := "highlight_category";
   Style_Cat_Cst     : aliased constant String := "style_category";
   Warning_Cat_Cst   : aliased constant String := "warning_category";
   Look_Sec_Cst      : aliased constant String := "look_for_secondary";
   Hint_Cst          : aliased constant String := "hint";
   Importance_Cst    : aliased constant String := "importance";
   End_Line_Cst      : aliased constant String := "end_line";
   End_Column_Cst    : aliased constant String := "end_column";
   In_Location_Cst   : aliased constant String := "show_in_location";

   Parse_Location_Parameters   : constant Cst_Argument_List :=
                                   (1  => Output_Cst'Access,
                                    2  => Category_Cst'Access,
                                    3  => Regexp_Cst'Access,
                                    4  => File_Index_Cst'Access,
                                    5  => Line_Index_Cst'Access,
                                    6  => Col_Index_Cst'Access,
                                    7  => Msg_Index_Cst'Access,
                                    8  => Style_Index_Cst'Access,
                                    9  => Warning_Index_Cst'Access,
                                    10 => Highlight_Cat_Cst'Access,
                                    11 => Style_Cat_Cst'Access,
                                    12 => Warning_Cat_Cst'Access);
   Remove_Category_Parameters  : constant Cst_Argument_List :=
                                   (1 => Category_Cst'Access);
   Locations_Add_Parameters    : constant Cst_Argument_List :=
                                   (1 => Category_Cst'Access,
                                    2 => File_Cst'Access,
                                    3 => Line_Cst'Access,
                                    4 => Column_Cst'Access,
                                    5 => Message_Cst'Access,
                                    6 => Highlight_Cst'Access,
                                    7 => Length_Cst'Access,
                                    8 => Look_Sec_Cst'Access,
                                    9 => Importance_Cst'Access);
   Add_Multi_Loc_Parameters    : constant Cst_Argument_List :=
                                   (1  => Category_Cst'Access,
                                    2  => File_Cst'Access,
                                    3  => Line_Cst'Access,
                                    4  => Column_Cst'Access,
                                    5  => End_Line_Cst'Access,
                                    6  => End_Column_Cst'Access,
                                    7  => Message_Cst'Access,
                                    8  => Highlight_Cst'Access,
                                    9  => Importance_Cst'Access,
                                    10 => In_Location_Cst'Access);
   Set_Sorting_Hint_Parameters : constant Cst_Argument_List :=
                                   (1 => Category_Cst'Access,
                                    2 => Hint_Cst'Access);

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure On_Action_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);
   --  Activate corresponding command if any

   procedure On_File_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);
   --  Open the file editor for the file of Iter

   procedure On_Location_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter);
   --  Opens editor, moves text cursor to the location of the message and
   --  raises editor's window when specified node is a message node.

   procedure On_Location_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class);
   --  Handle change of selections for tree view.

   procedure On_Row_Deleted
     (Self : access Location_View_Record'Class);
   --  Called when a row has been delete in the model

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Interactive shell command handler

   procedure On_Change_Sort (Self : access Location_View_Record'Class);
   --  Callback for the activation of the sort contextual menu item

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   procedure Goto_Location (Self : access Location_View_Record'Class);
   --  Goto the selected location in the Location_View

   package Location_View_Callbacks is
     new Gtk.Handlers.Callback (Location_View_Record);

   function Format_Messages
     (Container : not null GPS.Kernel.Messages_Container_Access;
      Category  : VSS.Strings.Virtual_String;
      File      : GNATCOLL.VFS.Virtual_File)
      return String;
   function Format_Message
     (Message       : Message_Access;
      Add_File_Name : Boolean := True)
      return String;
   --  Format the message for export/copy to clipboard.
   --  Add the message's file name when Add_File_Name is True.

   --------------------
   -- Category_Added --
   --------------------

   overriding procedure Category_Added
     (Self     : not null access View_Manager;
      Category : VSS.Strings.Virtual_String;
      Allow_Auto_Jump_To_First : Boolean)
   is
      Auto : constant Boolean := Allow_Auto_Jump_To_First
        and then Auto_Jump_To_First.Get_Pref;
   begin
      Expand_Category
        (Location_View_Access
           (Location_Views.Get_Or_Create_View (Self.Kernel, Focus => Auto)),
         Category,
         Auto);
   end Category_Added;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access View_Manager;
      Message : not null access Abstract_Message'Class)
   is
      pragma Unreferenced (Message);
   begin
      Location_Views.Child_From_View
        (Location_Views.Get_Or_Create_View
           (Self.Kernel, Focus => False))
        .Highlight_Child;
   end Message_Added;

   ---------------------
   -- Expand_Category --
   ---------------------

   procedure Expand_Category
     (Self       : Location_View_Access;
      Category   : VSS.Strings.Virtual_String;
      Goto_First : Boolean)
   is
      Loc : constant Location_View := Location_View (Self);
   begin
      Loc.View.Get_Selection.Unselect_All;

      Loc.Requests.Prepend
        (Expansion_Request'(Category, GNATCOLL.VFS.No_File, Goto_First));

      if Loc.Idle_Expand_Handler = No_Source_Id then
         Loc.Idle_Expand_Handler :=
           View_Idle.Idle_Add (Idle_Expand'Access, Loc);
      end if;
   end Expand_Category;

   -----------------
   -- Expand_File --
   -----------------

   procedure Expand_File
     (Self       : Location_View_Access;
      Category   : VSS.Strings.Virtual_String;
      File       : GNATCOLL.VFS.Virtual_File;
      Goto_First : Boolean)
   is
      Loc : constant Location_View := Location_View (Self);
   begin
      Loc.View.Get_Selection.Unselect_All;

      Loc.Requests.Prepend (Expansion_Request'(Category, File, Goto_First));

      if Loc.Idle_Expand_Handler = No_Source_Id then
         Loc.Idle_Expand_Handler :=
           View_Idle.Idle_Add (Idle_Expand'Access, Loc);
      end if;
   end Expand_File;

   -----------------
   -- Expand_File --
   -----------------

   procedure Expand_File
     (Self     : Location_View_Access;
      Category : VSS.Strings.Virtual_String;
      File     : GNATCOLL.VFS.Virtual_File) is
   begin
      Expand_File (Self, Category, File, Auto_Jump_To_First.Get_Pref);
   end Expand_File;

   ---------------------
   -- Format_Messages --
   ---------------------

   function Format_Messages
     (Container : not null GPS.Kernel.Messages_Container_Access;
      Category  : VSS.Strings.Virtual_String;
      File      : GNATCOLL.VFS.Virtual_File)
      return String
   is
      Messages : constant GPS.Kernel.Messages.Message_Array :=
        Container.Get_Messages (Category, File);
      Result : Unbounded_String;
   begin
      for Message of Messages loop
         if Message.Get_Flags (GPS.Kernel.Messages.Locations) then
            Append (Result, Format_Message (Message) & ASCII.LF);
         end if;
      end loop;
      return To_String (Result);
   end Format_Messages;

   --------------------
   -- Format_Message --
   --------------------

   function Format_Message
     (Message       : Message_Access;
      Add_File_Name : Boolean := True)
      return String
   is
      Result : Unbounded_String;
   begin
      Append
        (Result,
         ((if Add_File_Name
           then String (Message.Get_File.Base_Name) & ':'
           else "")
          & Trim (Integer'Image (Message.Get_Line), Both)
          & ':'
          & Trim
            (Basic_Types.Visible_Column_Type'Image
               (Message.Get_Column),
             Both)
          & (if Add_File_Name then ": " else " ")
          & To_String (Message.Get_Text)));
      for Secondary of Message.Get_Children loop
         Append (Result, ASCII.LF & Format_Message (Secondary));
      end loop;
      return To_String (Result);
   end Format_Message;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self  : not null access Location_View_Record;
      Child : not null access GPS_MDI_Child_Record'Class)
   is
      pragma Unreferenced (Child);
   begin
      Self.Kernel.Get_Messages_Container.Refilter;
   end On_Create;

   -----------------
   -- Idle_Expand --
   -----------------

   function Idle_Expand (Self : Location_View) return Boolean is
      Model : constant Gtk_Tree_Model := Self.View.Get_Model;
      Iter  : Gtk_Tree_Iter;
      Path  : Gtk_Tree_Path;
      Dummy : Boolean;
      pragma Warnings (Off, Dummy);

   begin
      Requests : while not Self.Requests.Is_Empty loop
         Iter := Get_Iter_First (Model);

         while Iter /= Null_Iter loop
            exit Requests when Get_String (Model, Iter, -Category_Column)
              = VSS.Strings.Conversions.To_UTF_8_String
                  (Self.Requests.First_Element.Category);

            Next (Model, Iter);
         end loop;

         Self.Requests.Delete_First;
      end loop Requests;

      if Iter /= Null_Iter then

         --  Raise Locations window

         declare
            Child : constant MDI_Child :=
                      Find_MDI_Child_By_Tag
                        (Get_MDI (Self.Kernel), Location_View_Record'Tag);

         begin
            if Child /= null then
               Raise_Child (Child, Give_Focus => False);
            end if;
         end;

         --  Expand category node

         Path := Get_Path (Model, Iter);
         Dummy := Self.View.Expand_Row (Path, False);

         --  Expand file node

         Iter := Children (Model, Iter);

         while Iter /= Null_Iter loop
            exit when
              GNATCOLL.VFS.GtkAda.Get_File (Model, Iter, -File_Column)
                = Self.Requests.First_Element.File;

            Next (Model, Iter);
         end loop;

         if Iter /= Null_Iter then
            Gtk.Tree_Model.Path_Free (Path);
            Path := Get_Path (Model, Iter);

         else
            Down (Path);
         end if;

         Dummy := Self.View.Expand_Row (Path, False);
         Self.View.Scroll_To_Cell (Path, null, False, 0.0, 0.0);

         --  Select first message and make it visible

         Down (Path);
         Self.View.Get_Selection.Select_Path (Path);
         Self.View.Scroll_To_Cell (Path, null, False, 0.0, 0.0);

         Path_Free (Path);

         if Self.Requests.First_Element.Goto_First then
            --  If go to location operation is requested, when go to the
            --  location of the selected message (it is first message)

            Self.Goto_Location;
         end if;
      end if;

      Self.Idle_Expand_Handler := No_Source_Id;
      Self.Requests.Clear;

      return False;
   end Idle_Expand;

   -------------------
   -- Goto_Location --
   -------------------

   procedure Goto_Location (Self : access Location_View_Record'Class) is
      Iter    : Gtk_Tree_Iter;
      Model   : Gtk_Tree_Model;
      Path    : Gtk_Tree_Path;
      Success : Boolean := True;
      List    : Gtk_Tree_Path_List.Glist;

      use type Gtk_Tree_Path_List.Glist;
   begin
      if Self.View.Get_Selection.Count_Selected_Rows /= 1 then
         return;
      end if;

      Self.View.Get_Selection.Get_Selected_Rows (Model, List);

      if Model = Null_Gtk_Tree_Model
        or else List = Gtk_Tree_Path_List.Null_List
      then
         Free_Path_List (List);
         return;
      end if;

      Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (List));

      while Success and then Get_Depth (Path) < 3 loop
         Success := Expand_Row (Self.View, Path, False);
         Down (Path);
         Self.View.Get_Selection.Select_Path (Path);
      end loop;

      Iter := Get_Iter (Model, Path);

      if Iter /= Null_Iter then
         On_Location_Clicked (Self, Path, Iter);
      end if;

      Free_Path_List (List);
   end Goto_Location;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Locations_Collapse_Or_Expand_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      K : constant Kernel_Handle := Get_Kernel (Context.Context);
      V : constant Location_View := Location_Views.Retrieve_View (K);

   begin
      if V /= null then
         Expand_Or_Collapse_Selected_Rows
           (Tree    => V.View,
            Command => Self.Command);
      end if;

      return Commands.Success;
   end Execute;

   ---------------
   -- Next_Item --
   ---------------

   procedure Next_Item
     (Self        : Location_View_Access;
      Backwards   : Boolean := False;
      Same_Weight : Boolean := False)
   is
      Loc : constant Location_View := Location_View (Self);
      Path          : Gtk_Tree_Path;
      File_Path     : Gtk_Tree_Path;
      Category_Path : Gtk_Tree_Path;
      Model         : Gtk_Tree_Model;
      Success       : Boolean;
      List          : Gtk_Tree_Path_List.Glist;
      Ignore        : Boolean;
      Weight        : Gint;
      pragma Unreferenced (Ignore);

      function Next (Path : Gtk_Tree_Path) return Boolean;
      --  Get the "next" element at the same level. Return False if it fails.

      function Next_File (Path : Gtk_Tree_Path) return Boolean;
      --  Get the "next" file node containing an element of weight

      function Down_Path (Path : Gtk_Tree_Path) return Boolean;
      --  Move the Path down and select the first or last element depending
      --  of Backwards

      ----------
      -- Next --
      ----------

      function Next (Path : Gtk_Tree_Path) return Boolean
      is
         Success           : Boolean := True;
         Reach_Limit       : Boolean := False;
         Found_Same_Weight : Boolean := not Same_Weight;
         --  Set to True when we don't want a node of the same weight
         Iter              : Gtk_Tree_Iter := Get_Iter (Model, Path);
      begin
         if Iter = Null_Iter then
            return False;
         end if;

         loop
            if Backwards then
               Success := Path.Prev;
            else
               Path.Next;
            end if;
            Iter := Get_Iter (Model, Path);

            --  Stop when we reach a limit or when we found a node of the same
            --  weight
            Reach_Limit := not Success or else Iter = Null_Iter;
            Found_Same_Weight :=
              Found_Same_Weight
              or else
                (Iter /= Null_Iter
                 and then Weight = Get_Int (Model, Iter, -Weight_Column));
            exit when Reach_Limit or else Found_Same_Weight;
         end loop;
         return not Reach_Limit and then Found_Same_Weight;
      end Next;

      ---------------
      -- Next_File --
      ---------------

      function Next_File (Path : Gtk_Tree_Path) return Boolean is
      begin
         if Same_Weight then
            declare
               Success     : Boolean := True;
               Reach_Limit : Boolean := False;
               Copy_Path   : Gtk_Tree_Path;
               Found       : Boolean := False;
            begin
               loop
                  if Backwards then
                     Success := Path.Prev;
                  else
                     Path.Next;
                  end if;
                  Reach_Limit := not Success
                    or else Get_Iter (Model, Path) = Null_Iter;
                  exit when Reach_Limit;

                  Copy_Path := Path.Copy;
                  Found := Down_Path (Copy_Path);
                  exit when Found;
               end loop;
               Path_Free (Copy_Path);
               return Found;
            end;
         else
            return Next (Path);
         end if;
      end Next_File;

      ---------------
      -- Down_Path --
      ---------------

      function Down_Path (Path : Gtk_Tree_Path) return Boolean
      is
         Success : Boolean := True;
      begin
         Path.Down;
         --  At this point we have the first element
         if Backwards then
            --  Backward => get the last element
            while Get_Iter (Model, Path) /= Null_Iter loop
               Path.Next;
            end loop;

            Ignore := Path.Prev;
         end if;

         if Same_Weight then
            declare
               Iter      : constant Gtk_Tree_Iter := Get_Iter (Model, Path);
               Ignore    : Boolean;
               Path_Copy : Gtk_Tree_Path;
            begin
               if Iter /= Null_Iter then
                  if Path.Get_Depth = 2 then
                     Path_Copy := Path.Copy;
                     --  Does the current file node has a matching element?
                     if not Down_Path (Path_Copy) then
                        --  Get the nearest file containing an element with
                        --  the same weight
                        Success := Next_File (Path);
                     else
                        Success := True;
                     end if;
                     Path_Free (Path_Copy);
                  elsif Weight /= Get_Int (Model, Iter, -Weight_Column) then
                     --  Get the nearest element with the same weight
                     Success := Next (Path);
                  else
                     --  The current path has the right weight
                     Success := True;
                  end if;
               end if;
            end;
         end if;
         return Success;
      end Down_Path;

      use type Gtk_Tree_Path_List.Glist;
   begin
      Loc.View.Get_Selection.Get_Selected_Rows (Model, List);

      if Model = Null_Gtk_Tree_Model
        or else List = Gtk_Tree_Path_List.Null_List
      then
         Free_Path_List (List);
         return;
      end if;

      Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (List));

      Weight := Get_Int (Model, Get_Iter (Model, Path), -Weight_Column);

      --  First handle the case where the selected item is not a node

      if Path.Get_Depth < 3 then
         Success := True;
         while Success and then Path.Get_Depth < 3 loop
            Success := Loc.View.Expand_Row (Path, False);
            Path.Down;
            Loc.View.Get_Selection.Unselect_All;
            Loc.View.Get_Selection.Select_Path (Path);
         end loop;

         if not Backwards then
            --  We have found the first iter, our job is done.
            Free_Path_List (List);
            return;
         end if;
      end if;

      if Path.Get_Depth < 3 then
         Free_Path_List (List);
         return;
      end if;

      File_Path := Path.Copy;
      Ignore := File_Path.Up;

      Category_Path := File_Path.Copy;
      Ignore := Category_Path.Up;

      if not Next (Path) then
         if not Next_File (File_Path) then
            if Locations_Wrap.Get_Pref then
               File_Path := Category_Path.Copy;
               Ignore := Down_Path (File_Path);
            else
               Path_Free (File_Path);
               Free_Path_List (List);
               Path_Free (Category_Path);
               return;
            end if;
         end if;

         Ignore := Loc.View.Expand_Row (File_Path, False);
         Path := File_Path.Copy;
         Ignore := Down_Path (Path);
      end if;

      Loc.View.Get_Selection.Unselect_All;
      Loc.View.Get_Selection.Select_Path (Path);
      Loc.View.Scroll_To_Cell (Path, null, False, 0.1, 0.1);
      Goto_Location (Loc);

      Path_Free (File_Path);
      Free_Path_List (List);
      Path_Free (Category_Path);
   end Next_Item;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V : constant Location_View := Location_View (View);

   begin
      --  Disconnect the listener

      Unregister (V.Kernel, Locations_Listener_Access (V.Listener));

      if V.Idle_Expand_Handler /= No_Source_Id then
         Glib.Main.Remove (V.Idle_Expand_Handler);
         V.Idle_Expand_Handler := No_Source_Id;
      end if;
   end On_Destroy;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Locations_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Context    : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      Child      : constant GPS_MDI_Child :=
        GPS_MDI_Child_Record (Self.all)'Unchecked_Access;
      Explorer   : constant Location_View :=
        Location_View (Child.Get_Actual_Widget);
      Path       : Gtk_Tree_Path;
      Iter       : Gtk_Tree_Iter;
      Model      : Gtk_Tree_Model;
      List       : Gtk_Tree_Path_List.Glist;
      N_Selected : Natural;

      use type Gtk_Tree_Path_List.Glist;
   begin
      Iter       := Find_Iter_For_Event (Explorer.View, Event);
      N_Selected := Natural (Explorer.View.Get_Selection.Count_Selected_Rows);

      if Iter = Null_Iter or else N_Selected = 0 then
         return Context;
      end if;

      Explorer.View.Get_Selection.Get_Selected_Rows (Model, List);

      if Model = Null_Gtk_Tree_Model
        or else List = Gtk_Tree_Path_List.Null_List
      then
         Free_Path_List (List);
         return Context;
      end if;

      if N_Selected = 1 then
         --  Single selection
         declare
            Message  : Message_Access;
         begin
            Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (List));
            if Path.Get_Depth >= 3 then
               Message := Get_Message
                 (Model, Get_Iter (Model, Path), -Message_Column);

               Set_Messages_Information (Context, (1 => Message));

               Set_File_Information
                 (Context,
                  Files  => (1 => Message.Get_File),
                  Line   => Message.Get_Line,
                  Column => Message.Get_Column);
            end if;
         end;

      else
         declare
            G_Iter         : Gtk_Tree_Path_List.Glist;
            Messages       : GPS.Kernel.Messages.Message_Array
              (1 .. N_Selected);
            Messages_Index : Natural := 0;
            File           : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
            Only_Messages  : Boolean := True;
         begin
            G_Iter := Gtk_Tree_Path_List.First (List);
            while G_Iter /= Gtk_Tree_Path_List.Null_List loop
               Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
               if Path.Get_Depth >= 3 then
                  Messages_Index := Messages_Index + 1;
                  Messages (Messages_Index) := Get_Message
                    (Model, Get_Iter (Model, Path), -Message_Column);

                  if Messages_Index = 1 then
                     --  Store first message's file
                     File := Messages (1).Get_File;

                  elsif Only_Messages then
                     --  Is this message belong to first message's file
                     if File /= GNATCOLL.VFS.No_File
                       and then File /= Messages (Messages_Index).Get_File
                     then
                        File := GNATCOLL.VFS.No_File;
                     end if;
                  end if;

               else
                  --  Not message
                  Only_Messages := False;
               end if;
               G_Iter := Gtk_Tree_Path_List.Next (G_Iter);
            end loop;

            if Messages_Index /= 0 then
               --  Message(s) selected
               Set_Messages_Information
                 (Context, Messages (1 .. Messages_Index));

               if Only_Messages
                 and then File /= GNATCOLL.VFS.No_File
               then
                  --  Messages belong to one file
                  Set_File_Information
                    (Context,
                     Files  => (1 => File),
                     Line   => Messages (1).Get_Line,
                     Column => Messages (1).Get_Column);
               end if;
            end if;
         end;
      end if;

      Free_Path_List (List);
      return Context;
   end Build_Context;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Toggle_Sort_By_Subcategory_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      K : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Set_Pref
        (Sort_By_Subcategory,
         K.Get_Preferences, not Sort_By_Subcategory.Get_Pref);
      return Commands.Success;
   end Execute;

   --------------------
   -- On_Change_Sort --
   --------------------

   procedure On_Change_Sort (Self : access Location_View_Record'Class) is
      Msg_Order  : Messages_Sort_Order;
      File_Order : File_Sort_Order;
   begin
      if Sort_By_Subcategory.Get_Pref then
         Msg_Order := By_Weight;
      else
         Msg_Order := By_Location;
      end if;

      if Sort_Files_Alphabetical.Get_Pref then
         File_Order := Alphabetical;
      else
         File_Order := Category_Default_Sort;
      end if;

      Self.View.Set_Order (File_Order, Msg_Order);
   end On_Change_Sort;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project : Project_Type)
   is
      pragma Unreferenced (Self, Column, Project);
      Locations     : constant Location_View :=
        Location_Views.Get_Or_Create_View (Kernel, Focus => False);
      Category_Iter : Gtk_Tree_Iter;
      File_Iter     : Gtk_Tree_Iter;
      Message_Iter  : Gtk_Tree_Iter;
      Iter          : Gtk_Tree_Iter;
      Model         : Gtk_Tree_Model;
      Path          : Gtk_Tree_Path;
      List, Cursor  : Gtk_Tree_Path_List.Glist;

      use type Gtk_Tree_Path_List.Glist;
   begin
      --  Check current selection: if it is on the same line as the new
      --  location, do not change the selection. Otherwise, there is no easy
      --  way for a user to click on a secondary location found in the same
      --  error message.

      Locations.View.Get_Selection.Get_Selected_Rows (Model, List);
      if Model = Null_Gtk_Tree_Model then
         return;
      end if;

      if List /= Gtk_Tree_Path_List.Null_List then
         Cursor := Gtk_Tree_Path_List.First (List);
         while Cursor /= Gtk_Tree_Path_List.Null_List loop
            Iter := Get_Iter
              (Model, Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (Cursor)));

            if Iter /= Null_Iter
              and then Get_File (Model, Iter, -File_Column) = File
              and then Integer (Get_Int (Model, Iter, -Line_Column)) = Line
            then
               Free_Path_List (List);
               return;
            end if;

            Cursor := Gtk_Tree_Path_List.Next (Cursor);
         end loop;

         Free_Path_List (List);
      end if;

      --  Highlight the location. Use the same category as the current
      --  selection, since otherwise the user that has both "Builder results"
      --  and "search" would automatically be moved to the builder when
      --  traversing all search results.

      if Iter = Null_Iter then
         --  There is no selected node, look for "Builder results" category.

         Category_Iter := Get_Iter_First (Model);

         while Category_Iter /= Null_Iter loop
            exit when Get_String (Model, Category_Iter, -Category_Column)
              = "Builder results";

            Next (Model, Category_Iter);
         end loop;

         --  Otherwise try to use first visible category.

         if Category_Iter = Null_Iter then
            Category_Iter := Get_Iter_First (Model);
         end if;

      else
         --  Unwind to category node.

         while Iter /= Null_Iter loop
            Category_Iter := Iter;
            Iter := Parent (Model, Iter);
         end loop;
      end if;

      if Category_Iter /= Null_Iter then
         --  Look for file node

         File_Iter := Children (Model, Category_Iter);

         while File_Iter /= Null_Iter loop
            exit when Get_File (Model, File_Iter, -File_Column) = File;

            Next (Model, File_Iter);
         end loop;

         if File_Iter /= Null_Iter then
            --  Look for message node

            Message_Iter := Children (Model, File_Iter);

            while Message_Iter /= Null_Iter loop
               exit when
                 Integer (Get_Int (Model, Message_Iter, -Line_Column)) = Line;

               Next (Model, Message_Iter);
            end loop;

            if Message_Iter /= Null_Iter then
               Path := Get_Path (Model, Message_Iter);
               Expand_To_Path (Locations.View, Path);
               Locations.View.Get_Selection.Unselect_All;
               Locations.View.Get_Selection.Select_Iter (Message_Iter);
               Locations.View.Scroll_To_Cell (Path, null, False, 0.1, 0.1);
               Path_Free (Path);

               --  Notify about change of selected message

               Message_Selected_Hook.Run
                 (Locations.Kernel,
                  Get_Message (Model, Message_Iter, -Message_Column));
            end if;
         end if;
      end if;
   end Execute;

   ------------------------------------------
   -- Set_Activity_Progress_Bar_Visibility --
   ------------------------------------------

   procedure Set_Activity_Progress_Bar_Visibility
     (Self    : not null Location_View_Access;
      Visible : Boolean)
   is
      View : constant Location_View := Location_View (Self);
   begin
      View.Set_Activity_Progress_Bar_Visibility (Visible);
   end Set_Activity_Progress_Bar_Visibility;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self   : access Location_View_Record'Class)
      return Gtk_Widget
   is
      M        : Gtk_Tree_Model;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      --  Initialize the listener

      Self.Listener := Listener_Access (Register (Self.Kernel));

      --  Initialize the tree view

      M := Get_Model (Locations_Listener_Access (Self.Listener));
      Gtk_New (Self.View, M);
      Scrolled.Add (Self.View);
      Location_View_Callbacks.Object_Connect
        (Gtk.Tree_Model."-" (M),
         Signal_Row_Deleted,
         Location_View_Callbacks.To_Marshaller (On_Row_Deleted'Access),
         Location_View (Self),
         True);
      Self.View.Get_Selection.Set_Mode (Selection_Multiple);
      Select_Function_With_View.Set_Select_Function
        (Self.View.Get_Selection,
         Selection_Function'Access,
         Location_View (Self));

      Self.View.Set_Name ("Locations Tree");
      Set_Font_And_Colors (Self.View, Fixed_Font => True);

      Widget_Callback.Connect (Self, Signal_Destroy, On_Destroy'Access);

      Location_View_Callbacks.Object_Connect
        (Self.View,
         Signal_Action_Clicked,
         Location_View_Callbacks.To_Marshaller (On_Action_Clicked'Access),
         Self);
      Location_View_Callbacks.Object_Connect
        (Self.View,
         Signal_File_Clicked,
         Location_View_Callbacks.To_Marshaller (On_File_Clicked'Access),
         Self);
      Location_View_Callbacks.Object_Connect
        (Self.View,
         Signal_Location_Clicked,
         Location_View_Callbacks.To_Marshaller (On_Location_Clicked'Access),
         Self);
      Self.View.Get_Selection.On_Changed
        (On_Location_Selection_Changed'Access, Self, After => True);

      Setup_Contextual_Menu
        (Self.Kernel,
         Event_On_Widget => Self.View);

      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => Self);
      Set_Font_And_Colors (Self.View, Fixed_Font => True);

      Location_Changed_Hook.Add_Debounce
        (new On_Location_Changed, Watch => Self);

      --  Apply the current "sort by subcategory" setting
      On_Change_Sort (Self);

      return Gtk_Widget (Self.View);
   end Initialize;

   ------------------------
   -- Is_Parent_Selected --
   ------------------------

   function Is_Parent_Selected
     (Selection : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Path      : Gtk_Tree_Path;
      Depth     : Gint := 0)
      return Boolean
   is
   begin
      while Path.Up loop
         if (Depth = 0 or else Path.Get_Depth <= Depth)
           and then Selection.Path_Is_Selected (Path)
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Parent_Selected;

   -----------------------
   -- On_Action_Clicked --
   -----------------------

   procedure On_Action_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter)
   is
      use type Commands.Command_Access;

      Value   : GValue;
      Action  : GPS.Editors.Line_Information.Line_Information_Access;
      Ignore  : Commands.Command_Return_Type;
      pragma Unreferenced (Ignore);
      Context : Selection_Context;

   begin
      if Self.View.Get_Selection.Count_Selected_Rows = 1 then
         On_Location_Clicked (Self, Path, Iter);
      end if;

      Get_Value (Self.View.Get_Model, Iter, -Action_Command_Column, Value);
      Action := To_Line_Information_Access (Get_Address (Value));

      if Action /= null
        and then Action.Associated_Command /= null
      then
         Context := Location_Views.Child_From_View (Self).Build_Context;
         Self.Kernel.Context_Changed (Context);

         Ignore := Action.Associated_Command.Execute;

         Self.Kernel.Refresh_Context;
      end if;

      Unset (Value);
   end On_Action_Clicked;

   -----------------------------------
   -- On_Location_Selection_Changed --
   -----------------------------------

   procedure On_Location_Selection_Changed
     (Object : access Glib.Object.GObject_Record'Class)
   is
      Self : Location_View_Record'Class
        renames Location_View_Record'Class (Object.all);

   begin
      Self.Kernel.Refresh_Context;
   end On_Location_Selection_Changed;

   ---------------------
   -- On_File_Clicked --
   ---------------------

   procedure On_File_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Path);
      Model : constant Gtk_Tree_Model := Self.View.Get_Model;
      File  : constant Virtual_File :=
        Get_File (Gtk.Tree_Model."-" (Model), Iter, -File_Column);
   begin
      if File /= No_File then
         GPS.Editors.GtkAda.Get_MDI_Child
           (Self.Kernel.Get_Buffer_Factory.Get
              (File).Current_View).Raise_Child;
      end if;
   end On_File_Clicked;

   -------------------------
   -- On_Location_Clicked --
   -------------------------

   procedure On_Location_Clicked
     (Self : access Location_View_Record'Class;
      Path : Gtk_Tree_Path;
      Iter : Gtk_Tree_Iter)
   is
      pragma Unreferenced (Path);

   begin
      declare
         Mark     : constant Editor_Mark'Class :=
           Get_Mark (Gtk.Tree_Model."-" (Self.View.Get_Model),
                     Iter, -Node_Mark_Column);
         Message  : constant Message_Access :=
           Get_Message (Self.View.Get_Model, Iter, -Message_Column);
         File     : constant Virtual_File :=
           Get_File (Gtk.Tree_Model."-" (Self.View.Get_Model),
                     Iter, -File_Column);

      begin
         --  Notify about change of selected message

         if Message /= null then
            Message_Selected_Hook.Run (Self.Kernel, Message);
         end if;

         if Mark /= Nil_Editor_Mark
           and then File /= No_File
           and then File.Is_Regular_File
         then
            --  The user clicked on a message in the Locations view: open an
            --  unlocked editor for the message's file and go to the message's
            --  location.

            declare
               Buf : constant GPS.Editors.Editor_Buffer'Class :=
                 Self.Kernel.Get_Buffer_Factory.Get
                   (File, Unlocked_Only => True) with Unreferenced;
               Location : constant Editor_Location'Class :=
                 Mark.Location (True);
            begin
               Location.Buffer.Current_View.Cursor_Goto
                 (Location, Self.View.Get_Selection.Count_Selected_Rows < 2);
            end;
         end if;
      end;
   end On_Location_Clicked;

   ---------------------------------
   -- Get_Or_Create_Location_View --
   ---------------------------------

   function Get_Or_Create_Location_View
     (Kernel         : access Kernel_Handle_Record'Class;
      Allow_Creation : Boolean := True)
      return Location_View_Access is
   begin
      if Allow_Creation then
         return Location_View_Access
           (Location_Views.Get_Or_Create_View (Kernel, Focus => False));
      else
         return Location_View_Access
           (Location_Views.Retrieve_View (Kernel));
      end if;
   end Get_Or_Create_Location_View;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      View  : constant Location_View := Location_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Set_Font_And_Colors (View.View, Fixed_Font => True, Pref => Pref);

         if Pref = Preference (Sort_By_Subcategory)
           or else Pref = Preference (Sort_Files_Alphabetical)
         then
            On_Change_Sort (View);

         elsif Pref = Preference (Location_Only_High_Messages) then
            View.View.Refilter;
         end if;

         --  Nothing to do for Auto_Jump_To_First
         --  Nothing to do for Locations_Wrap
         --  Nothing to do for Auto_Close
      end if;
   end Execute;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Location_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "locations",
         Tooltip     => -"The text pattern or regular expression",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy,
         Name        => "Locations View Filter");
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Location_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K     : constant Kernel_Handle := View.Kernel;
   begin
      Append_Menu (Menu, K, Sort_By_Subcategory);     --  On_Change_Sort'Access
      Append_Menu (Menu, K, Sort_Files_Alphabetical); --  On_Change_Sort'Access
      Append_Menu (Menu, K, Auto_Jump_To_First);      --  On_Change_Sort'Access
      Append_Menu (Menu, K, Locations_Wrap);
      Append_Menu (Menu, K, Auto_Close);
      Append_Menu (Menu, K, Locations_Save_In_Desktop);
      Append_Menu (Menu, K, Preserve_Messages);
      Append_Menu (Menu, K, Location_Only_High_Messages);
   end Create_Menu;

   ----------------------------
   -- Raise_Locations_Window --
   ----------------------------

   procedure Raise_Locations_Window
     (Self             : not null access Kernel_Handle_Record'Class;
      Give_Focus       : Boolean := True;
      Create_If_Needed : Boolean := False)
   is
      L : constant GPS.Location_View.Location_View_Access :=
            (if Create_If_Needed
             then GPS.Location_View.Get_Or_Create_Location_View (Self)
             else null);
      pragma Unreferenced (L);
      --  Create Locations view when necessary
      C : constant MDI_Child :=
            Get_MDI (Self).Find_MDI_Child_By_Name (Locations_View_Name);

   begin
      if C /= null then
         C.Raise_Child (Give_Focus => Give_Focus);
      end if;
   end Raise_Locations_Window;

   --------------------------
   -- Set_Locations_Filter --
   --------------------------

   procedure Set_Locations_Filter
     (Self   : not null access Kernel_Handle_Record'Class;
      Value  : String;
      Expand : Boolean := False)
   is
      L   : constant GPS.Location_View.Location_View_Access :=
        GPS.Location_View.Get_Or_Create_Location_View (Self);
      Loc : constant Location_View := Location_View (L);
   begin
      Loc.Set_Filter (Value);
      if Expand then
         Loc.View.Expand_All;
      end if;
   end Set_Locations_Filter;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Manager : constant View_Manager_Access := new View_Manager (Kernel);
   begin
      Location_Views.Register_Module (Kernel);

      Sort_By_Subcategory := Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-sort-by-subcategory", False,
         Label => -"Sort by subcategory",
         Doc => -(
           "Sort messages by their subcategory (error vs warning messages for"
          & " instance). This also impacts the default sort order for files"));
      Auto_Jump_To_First := Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-auto-jump-to-first", True,
         Label => -"Jump to first location",
         Doc =>
            -("Jump to the first location"
            & " when entries are added to the Location window (error"
            & " messages, find results, ...)"));
      Locations_Wrap := Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-wrap", True,
         Label => -"Wrap around on next/previous",
         Doc =>
            -("Wrap around to the beginning when reaching the end of "
            & " the category when using the Next Tag and Previous"
            & "Tag actions."));
      Auto_Close := Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-auto-close", False,
         Label => -"Auto close Locations",
         Doc   => -"Close automatically Locations when it becomes empty.");
      Sort_Files_Alphabetical := Kernel.Get_Preferences.Create_Invisible_Pref
        ("locations-sort-Files-alphabetical", False,
         Label => -"Sort files alphabetically",
         Doc =>
            -("Force sorting of files alphabetically, and ignore the default"
            & " sort order (which depends on the category)"));

      Register_Action
        (Kernel, "locations remove selection",
         new Remove_Selection_Command,
         -"Remove the selected category, file or message",
         Icon_Name => "gps-remove-symbolic",
         Category => -"Locations");

      Register_Action
        (Kernel, "locations clear",
         new Clear_Locations_Command,
         -"Remove all the messages",
         Icon_Name => "gps-clear-symbolic",
         Category => -"Locations");

      Register_Action
        (Kernel, "locations export to text file",
         new Selection_To_String_Command (Export),
         -"Export the selected rows to a text file",
         Icon_Name => "gps-save-symbolic",
         Category => -"Locations");

      Register_Action
        (Kernel, "locations copy to clipboard",
         new Selection_To_String_Command (Clipboard),
         -"Copy the selected rows to the clipboard",
         Icon_Name => "gps-copy-symbolic",
         Category => -"Locations");

      Register_Action
        (Kernel, "locations toggle sort by subcategory",
         new Toggle_Sort_By_Subcategory_Command,
         -("Changes the sort order in the locations window. When active,"
           & " this will group all error messages together, and then"
           & " warning messages"),
         Category => -"Locations");

      Register_Action
        (Kernel, "locations expand selected",
         new Locations_Collapse_Or_Expand_Command (Expand_All_Rows),
         -"Expand the selected rows in the locations view",
         Icon_Name => "gps-expand-all-symbolic",
         Category => -"Locations");

      Register_Action
        (Kernel, "locations collapse selected",
         new Locations_Collapse_Or_Expand_Command (Collapse_Rows),
         -"Collapse the selected rows in the locations view",
         Icon_Name => "gps-collapse-all-symbolic",
         Category => -"Locations");

      Get_Messages_Container (Kernel).Register_Listener
        (Listener_Access (Manager),
         (Editor_Side => False,
          Editor_Line => False,
          GPS.Kernel.Messages.Locations => True));
   end Register_Module;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Locations_Class : constant Class_Type := New_Class (Kernel, "Locations");
   begin
      Register_Command
        (Kernel, "parse",
         Minimum_Args  => 2,
         Maximum_Args  => Parse_Location_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "add",
         Minimum_Args  => Locations_Add_Parameters'Length - 4,
         Maximum_Args  => Locations_Add_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "add_multilines",
         Minimum_Args  => Add_Multi_Loc_Parameters'Length - 3,
         Maximum_Args  => Add_Multi_Loc_Parameters'Length,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "remove_category",
         Minimum_Args  => 1,
         Maximum_Args  => 2,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_categories",
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "list_locations",
         Class         => Locations_Class,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel        => Kernel,
         Command       => "set_sort_order_hint",
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Handler       => Default_Command_Handler'Access,
         Class         => Locations_Class,
         Static_Method => True);
      Register_Command
        (Kernel, "dump",
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
      Register_Command
        (Kernel, "locations_dump",
         Class         => Locations_Class,
         Static_Method => True,
         Handler       => Default_Command_Handler'Access);
   end Register_Commands;

   -----------------------------
   -- Default_Command_Handler --
   -----------------------------

   procedure Default_Command_Handler
     (Data : in out Callback_Data'Class; Command : String) is
   begin
      if Command = "parse" then
         Name_Parameters (Data, Parse_Location_Parameters);
         declare
            Highlight_Category : constant String :=
                                   Nth_Arg (Data, 10, "Builder results");
            Style_Category     : constant String :=
                                   Nth_Arg (Data, 11, "Style errors");
            Warning_Category   : constant String :=
                                   Nth_Arg (Data, 12, "Builder warnings");
         begin
            Parse_File_Locations_Unknown_Encoding
              (Get_Kernel (Data),
               Highlight               => Highlight_Category /= ""
               or else Style_Category /= ""
               or else Warning_Category /= "",
               Text                    => Nth_Arg (Data, 1),
               Category                =>
                 VSS.Strings.Conversions.To_Virtual_String
                   (String'(Nth_Arg (Data, 2))),
               Highlight_Category      => Highlight_Category,
               Style_Category          => Style_Category,
               Warning_Category        => Warning_Category,
               File_Location_Regexp    => Nth_Arg (Data, 3, ""),
               File_Index_In_Regexp    => Nth_Arg (Data, 4, -1),
               Line_Index_In_Regexp    => Nth_Arg (Data, 5, -1),
               Col_Index_In_Regexp     => Nth_Arg (Data, 6, -1),
               Msg_Index_In_Regexp     => Nth_Arg (Data, 7, -1),
               Style_Index_In_Regexp   => Nth_Arg (Data, 8, -1),
               Warning_Index_In_Regexp => Nth_Arg (Data, 9, -1));
         end;

      elsif Command = "remove_category" then
         declare
            Category  : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (String'(Data.Nth_Arg (1)));
            Int_Flags : constant Integer := Data.Nth_Arg (2, Default => 2);
            Flags     : Message_Flags;
         begin
            case Int_Flags is
               when 0 =>
                  Flags := GPS.Kernel.Messages.Empty_Message_Flags;
               when 1 =>
                  Flags := GPS.Kernel.Messages.Sides_Only;
               when 2 =>
                  Flags := GPS.Kernel.Messages.Locations_Only;
               when others =>
                  Flags := GPS.Kernel.Messages.Side_And_Locations;
            end case;

            Name_Parameters (Data, Remove_Category_Parameters);
            Get_Messages_Container (Get_Kernel (Data)).Remove_Category
              (Category, Flags);
         end;

      elsif Command = "list_categories" then
         Set_Return_Value_As_List (Data);

         for Category of Get_Messages_Container
                           (Get_Kernel (Data)).Get_Categories
         loop
            Set_Return_Value
              (Data, VSS.Strings.Conversions.To_UTF_8_String (Category));
         end loop;

      elsif Command = "list_locations" then
         declare
            Script   : constant Scripting_Language := Get_Script (Data);
            Category : constant VSS.Strings.Virtual_String :=
              VSS.Strings.Conversions.To_Virtual_String
                (String'(Nth_Arg (Data, 1)));
            File     : constant GNATCOLL.VFS.Virtual_File := Create
              (Nth_Arg (Data, 2), Get_Kernel (Data), Use_Source_Path => True);
            Messages : constant Message_Array :=
              Get_Messages_Container (Get_Kernel (Data)).Get_Messages
                (Category, File);

         begin
            Set_Return_Value_As_List (Data);

            for J in Messages'Range loop
               Set_Return_Value
                 (Data,
                  Create_File_Location
                    (Script => Script,
                     File   => File,
                     Line   => Messages (J).Get_Line,
                     Column => Messages (J).Get_Column));
               Set_Return_Value (Data, To_String (Messages (J).Get_Text));
            end loop;
         end;

      elsif Command = "add" then
         Name_Parameters (Data, Locations_Add_Parameters);

         declare
            File : constant Virtual_File :=
              Get_Data
                (Nth_Arg (Data, 2, Get_File_Class (Get_Kernel (Data))));
            Ignore : Message_Access;
            pragma Unreferenced (Ignore);

         begin
            if File.Is_Absolute_Path then
               Ignore :=
                 GPS.Kernel.Messages.Tools_Output.Add_Tool_Message
                   (Get_Messages_Container (Get_Kernel (Data)),
                    VSS.Strings.Conversions.To_Virtual_String
                      (Glib.Convert.Escape_Text (Nth_Arg (Data, 1))),
                    File,
                    Nth_Arg (Data, 3),
                    Visible_Column_Type (Nth_Arg (Data, 4, Default => 1)),
                    Nth_Arg (Data, 5),
                    Message_Importance_Type'Val (Nth_Arg (Data, 9, 1)),
                    Get_Style_Manager
                      (Get_Kernel (Data)).Get
                    (Nth_Arg (Data, 6, ""), Allow_Null => True),
                    Highlight_Length
                      (Nth_Arg (Data, 7, Integer (Highlight_Whole_Line))),
                    Nth_Arg (Data, 8, False),
                    Show_In_Locations => True);
            end if;
         end;
      elsif Command = "add_multilines" then
         Name_Parameters (Data, Add_Multi_Loc_Parameters);

         declare
            File   : constant Virtual_File :=
              Get_Data
                (Nth_Arg (Data, 2, Get_File_Class (Get_Kernel (Data))));
            Ignore : GPS.Kernel.Messages.Multilines.Multiline_Message_Access;
            pragma Unreferenced (Ignore);

         begin
            if File.Is_Absolute_Path then
               Ignore :=
                 GPS.Kernel.Messages.Multilines.Create_Message
                   (Container          =>
                      Get_Messages_Container (Get_Kernel (Data)),
                    Category           =>
                      VSS.Strings.Conversions.To_Virtual_String
                        (Glib.Convert.Escape_Text (Nth_Arg (Data, 1))),
                    File               => File,
                    Line               => Nth_Arg (Data, 3),
                    Column             =>
                      Visible_Column_Type (Nth_Arg (Data, 4, Default => 1)),
                    End_Line           => Nth_Arg (Data, 5),
                    End_Column         =>
                      Visible_Column_Type (Nth_Arg (Data, 6, Default => 1)),
                    Text               =>
                      VSS.Strings.Conversions.To_Virtual_String
                        (Ada.Strings.Unbounded.Unbounded_String'
                           (Nth_Arg (Data, 7))),
                    Highlight_Category => Get_Style_Manager
                      (Get_Kernel (Data)).Get
                    (Nth_Arg (Data, 8, ""), Allow_Null => True),
                    Importance         =>
                      Message_Importance_Type'Val (Nth_Arg (Data, 9, 1)),
                    Show_In_Locations  => Nth_Arg (Data, 10, False));
            end if;
         end;

      elsif Command = "set_sort_order_hint" then
         Name_Parameters (Data, Set_Sorting_Hint_Parameters);

         Get_Messages_Container (Get_Kernel (Data)).Set_Sort_Order_Hint
           (VSS.Strings.Conversions.To_Virtual_String
              (String'(Nth_Arg (Data, 1))),
            Sort_Order_Hint'Value (Nth_Arg (Data, 2)));

      elsif Command = "dump" then
         Name_Parameters (Data, Locations_Add_Parameters);
         Get_Messages_Container (Get_Kernel (Data)).Save
           (Create (Nth_Arg (Data, 1)),
            (Editor_Side        => False,
             Editor_Line        => False,
             Messages.Locations => True),
            True);

      elsif Command = "locations_dump" then
         declare
            View   : constant Location_View :=
              Location_Views.Get_Or_Create_View (Get_Kernel (Data));
            Model  : constant Gtk.Tree_Model.Gtk_Tree_Model :=
              View.View.Get_Model;
            Path   : Gtk_Tree_Path;
            Iter   : Gtk_Tree_Iter;
            Result : Ada.Strings.Unbounded.Unbounded_String;

            procedure Get_String (Iter : Gtk_Tree_Iter);

            procedure Get_String (Iter : Gtk_Tree_Iter) is
            begin
               Path := Get_Path (Model, Iter);

               if Path.Get_Depth = 1 then
                  Append
                    (Result,
                     Get_String (Model, Iter, -Category_Column) & ASCII.LF);

               elsif Path.Get_Depth = 2 then
                  Append
                    (Result, "  " &
                     (+GNATCOLL.VFS.GtkAda.Get_File
                            (Model, Iter, -File_Column).Full_Name) & ASCII.LF);

               elsif Path.Get_Depth >= 3 then
                  Append
                    (Result, "    " &
                       Format_Message
                       (Get_Message (Model, Iter, -Message_Column), False) &
                       ASCII.LF);
               end if;
               Path_Free (Path);

               for Index in 1 .. N_Children (Model, Iter) loop
                  Get_String (Nth_Child (Model, Iter, Index - 1));
               end loop;
            end Get_String;

         begin
            Iter := Get_Iter_First (Model);
            while Iter /= Null_Iter loop
               Get_String (Iter);
               Next (Model, Iter);
            end loop;

            Set_Return_Value (Data, To_String (Result));
         end;
      end if;
   end Default_Command_Handler;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Location_View_Record;
      Pattern : in out Search_Pattern_Access)
   is
   begin
      Self.View.Get_Filter_Model.Set_Pattern (Pattern);
   end Filter_Changed;

   --------------------
   -- On_Row_Deleted --
   --------------------

   procedure On_Row_Deleted (Self : access Location_View_Record'Class) is
   begin
      if Auto_Close.Get_Pref
        and then Get_Iter_First (Self.View.Get_Model) = Null_Iter
      then
         Location_Views.Close (Self.Kernel);
      end if;
   end On_Row_Deleted;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Clear_Locations_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      View : constant Location_View :=
        Location_Views.Retrieve_View (Get_Kernel (Context.Context));
      Container : Messages_Container_Access;
   begin
      if View /= null then
         Container := Get_Messages_Container (View.Kernel);
         Remove_All_Messages
           (Container,
            (Editor_Side => False,
             Editor_Line => False,
             GPS.Kernel.Messages.Locations => True));
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Remove_Selection_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      pragma Unreferenced (Self);
      View      : constant Location_View :=
        Location_Views.Retrieve_View (Get_Kernel (Context.Context));
      Selection : Gtk.Tree_Selection.Gtk_Tree_Selection;
      Path      : Gtk_Tree_Path;
      Iter      : Gtk_Tree_Iter;
      Model     : Gtk_Tree_Model;
      Message   : Message_Access;
      List      : Gtk_Tree_Path_List.Glist;
      G_Iter    : Gtk_Tree_Path_List.Glist;

      use type Gtk_Tree_Path_List.Glist;
   begin
      if View = null then
         return Commands.Failure;
      end if;

      Selection := View.View.Get_Selection;
      Selection.Get_Selected_Rows (Model, List);

      if Model = Null_Gtk_Tree_Model
        or else List = Gtk_Tree_Path_List.Null_List
      then
         Free_Path_List (List);
         Selection.Unselect_All;

         return Commands.Failure;
      end if;

      G_Iter := Gtk_Tree_Path_List.Last (List);
      while G_Iter /= Gtk_Tree_Path_List.Null_List loop
         Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
         if Path /= Null_Gtk_Tree_Path then
            Iter := Get_Iter (Model, Path);
         end if;

         if Iter /= Null_Iter then
            if Path.Get_Depth = 1 then
               Get_Messages_Container (View.Kernel).Remove_Category
                 (VSS.Strings.Conversions.To_Virtual_String
                    (Get_String (Model, Iter, -Category_Column)),
                  Locations_Message_Flags);

            elsif Path.Get_Depth = 2 then
               if not Is_Parent_Selected (Selection, Path) then
                  Get_Messages_Container (View.Kernel).Remove_File
                    (VSS.Strings.Conversions.To_Virtual_String
                       (Get_String (Model, Iter, -Category_Column)),
                     Get_File (Model, Iter, -File_Column),
                     Locations_Message_Flags);
               end if;

            elsif Path.Get_Depth >= 3 then
               if not Is_Parent_Selected (Selection, Path, 2) then
                  Message := Get_Message (Model, Iter, -Message_Column);
                  Message.Remove;
               end if;
            end if;
         end if;

         G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
      end loop;

      Free_Path_List (List);
      Selection.Unselect_All;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : access Selection_To_String_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type
   is
      use type Gtk_Tree_Path_List.Glist;

      View        : constant Location_View :=
        Location_Views.Retrieve_View (Get_Kernel (Context.Context));
      Path        : Gtk_Tree_Path;
      Model       : Gtk_Tree_Model;
      Export_File : GNATCOLL.VFS.Virtual_File;
      Container   : GPS.Kernel.Messages_Container_Access;
      List        : Gtk_Tree_Path_List.Glist;
      G_Iter      : Gtk_Tree_Path_List.Glist;
      File        : Ada.Text_IO.File_Type;
      Result      : Unbounded_String;

      procedure Add_Messages (Path : Gtk_Tree_Path);
      --  Append the string representation of the row designed by Path to
      --  Result.

      ------------------
      -- Add_Messages --
      ------------------

      procedure Add_Messages (Path : Gtk_Tree_Path) is
         Iter : constant Gtk_Tree_Iter := Get_Iter (Model, Path);
      begin
         --  Category node, retrieve all the files and add their messages
         if Path.Get_Depth = 1 then
            declare
               Category : constant VSS.Strings.Virtual_String :=
                 VSS.Strings.Conversions.To_Virtual_String
                   (Get_String (Model, Iter, -Category_Column));
               Files    : constant Virtual_File_Array :=
                 Get_Files (Container, Category);
            begin
               for J in Files'Range loop
                  Append
                    (Result, Format_Messages (Container, Category, Files (J)));
               end loop;
            end;

         --  File node, add all the related messages
         elsif Path.Get_Depth = 2 then
            --  Prevent duplicates: don't add this node if the category is
            --  also selected
            if not Is_Parent_Selected (View.View.Get_Selection, Path) then
               declare
                  Category : constant VSS.Strings.Virtual_String :=
                    VSS.Strings.Conversions.To_Virtual_String
                      (Get_String (Model, Iter, -Category_Column));
                  F        : constant Virtual_File :=
                    GNATCOLL.VFS.GtkAda.Get_File (Model, Iter, -File_Column);
               begin
                  Append (Result, Format_Messages (Container, Category, F));
               end;
            end if;

         elsif Path.Get_Depth >= 3 then
            --  Prevent duplicates: don't add this node if either the category
            --  or the file is also selected
            if not Is_Parent_Selected (View.View.Get_Selection, Path) then
               Append
                 (Result,
                  Format_Message (Get_Message (Model, Iter, -Message_Column))
                  & ASCII.LF);
            end if;
         end if;
      end Add_Messages;

      Iter : Gtk_Tree_Iter := Null_Iter;
   begin
      if View = null then
         return Commands.Failure;
      end if;

      Container := View.Kernel.Get_Messages_Container;

      --  Build the string using the current selection
      View.View.Get_Selection.Get_Selected_Rows (Model, List);
      if Model /= Null_Gtk_Tree_Model
        and then List /= Gtk_Tree_Path_List.Null_List
      then
         G_Iter := Gtk_Tree_Path_List.First (List);
         while G_Iter /= Gtk_Tree_Path_List.Null_List loop
            Path := Gtk_Tree_Path
              (Gtk_Tree_Path_List.Get_Data (G_Iter));
            Add_Messages (Path);
            G_Iter := Gtk_Tree_Path_List.Next (G_Iter);
         end loop;
      else
         if View.View.Get_Model /= Null_Gtk_Tree_Model then
            Iter := Get_Iter_First (View.View.Get_Model);
         end if;

         if Iter /= Null_Iter then
            while Iter /= Null_Iter loop
               Add_Messages (Get_Path (Model, Iter));
               Next (Model, Iter);
            end loop;
         else
            --  Locations is empty
            View.Kernel.Messages_Window.Insert
              (-"The Locations view is empty: nothing to do",
               Mode => Error);
            Free_Path_List (List);
            return Commands.Failure;
         end if;
      end if;
      Free_Path_List (List);

      if Self.Action = Export then
         --  Use a dialog to ask for the export destination
         Export_File := Gtkada.File_Selector.Select_File
           (Parent => Get_Current_Window (View.Kernel));

         if Export_File /= No_File then
            --  Open a file and create it if needed
            Create (File, Out_File, String (Export_File.Full_Name.all));
            Ada.Text_IO.Put
              (File, Strip_Ending_Linebreaks (To_String (Result)));
            Ada.Text_IO.Close (File);
         end if;
      else
         Copy_Text_In_Clipboard
           (Get_Clipboard (View.Kernel),
            Strip_Ending_Linebreaks (To_String (Result)));
      end if;

      return Commands.Success;
   end Execute;

   ------------------------
   -- Selection_Function --
   ------------------------

   function Selection_Function
     (Selection               : not null access
        Gtk_Tree_Selection_Record'Class;
      Model                   : Gtk.Tree_Model.Gtk_Tree_Model;
      Path                    : Gtk.Tree_Model.Gtk_Tree_Path;
      Path_Currently_Selected : Boolean;
      View                    : Location_View) return Boolean
   is
      pragma Unreferenced (Selection);
      pragma Unreferenced (Model);
      pragma Unreferenced (Path);

   begin
      if Path_Currently_Selected then
         return not View.View.Get_Multiple_Action;
      else
         return True;
      end if;
   end Selection_Function;

end GPS.Location_View;
