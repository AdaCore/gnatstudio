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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Strings;                use GNAT.Strings;
with GNATCOLL.Projects;           use GNATCOLL.Projects;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;         use GNATCOLL.VFS.GtkAda;

with GPS.LSP_Module;
with GPS.LSP_Client.Utilities;    use GPS.LSP_Client.Utilities;
with LSP.Types; use LSP.Types;
with LSP.Messages;

with Glib;                        use Glib;
with Glib.Convert;                use Glib.Convert;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Glib_Values_Utils;           use Glib_Values_Utils;

with Gdk.Event;                   use Gdk.Event;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Types.Keysyms;           use Gdk.Types.Keysyms;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Paned;                   use Gtk.Paned;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_Selection;          use Gtk.Tree_Selection;
with Gtk.Tree_Store;              use Gtk.Tree_Store;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.List_Store;              use Gtk.List_Store;
with Gtk.Tree_Row_Reference;      use Gtk.Tree_Row_Reference;
with Gtk.Tree_View;               use Gtk.Tree_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtkada.MDI;

with Basic_Types;                 use Basic_Types;
with Language;                    use Language;
with Commands.Interactive;        use Commands, Commands.Interactive;
with Default_Preferences;         use Default_Preferences;
with Generic_Views;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Actions;          use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;          use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;       use GPS.Kernel.Modules.UI;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Project;          use GPS.Kernel.Project;
with GPS.Kernel.Xref;             use GPS.Kernel.Xref;
with GPS.Intl;                    use GPS.Intl;

with GPS.LSP_Client.Requests;           use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Called_By; use GPS.LSP_Client.Requests.Called_By;

with GUI_Utils;                   use GUI_Utils;
with Histories;                   use Histories;
with String_Utils;                use String_Utils;
with XML_Utils;                   use XML_Utils;
with Xref;                        use Xref;
with GNATCOLL.JSON;
with Ada.Containers.Doubly_Linked_Lists;

package body Call_Graph_Views is

   use type Basic_Types.Visible_Column_Type;

   Me : constant Trace_Handle := Create ("GPS.VIEWS.CALL_GRAPH_VIEW");

   ---------------
   -- Constants --
   ---------------

   Name_Column        : constant := 0; --  Title of the node, e.g. "xxx calls"
   Decl_Column        : constant := 1;
   Entity_Name_Column : constant := 2; --  Name of entity
   File_Column        : constant := 3; --  Entity declaration file
   Line_Column        : constant := 4; --  Entity declaration line
   Column_Column      : constant := 5; --  Entity declaration column
   List_Column        : constant := 6;
   Kind_Column        : constant := 7;
   Sort_Column        : constant := 8;
   Project_Column     : constant := 9; --  Entity declaration project

   Column_Types : GType_Array (0 .. 9);

   Location_Line_Column      : constant := 0;
   Location_Column_Column    : constant := 1;
   Location_Character_Column : constant := 2;
   Location_String_Column    : constant := 3;
   Location_File_Column      : constant := 4;
   Location_Project_Column   : constant := 5;

   History_Show_Locations : constant History_Key :=
                              "Call_Graph_Show_Locations";

   Computing_Label : constant String := "computing...";
   --  Label used while computing the ancestors call graph

   type CG_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access CG_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   ---------------------------
   -- LSP Requests Handling --
   ---------------------------

   type Called_By_Request is new Abstract_Called_By_Request with record
      Kernel : Kernel_Handle;

      Where : Gtk_Tree_Row_Reference;
      --  The node in which the results should be added

      Invalidated : Boolean := False;
      --  Flag for destruction handling: this should be set to True
      --  when the view gets invalidated
   end record;
   type Called_By_Request_Access is access all Called_By_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out Called_By_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector);

   overriding procedure On_Error_Message
     (Self    : in out Called_By_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   -----------------
   -- Local types --
   -----------------

   type View_Type is (View_Calls, View_Called_By);

   package Request_Lists is new Ada.Containers.Doubly_Linked_Lists
     (GPS.LSP_Client.Requests.Reference);

   type Callgraph_View_Record is new Generic_Views.View_Record with record
      Tree              : Gtk_Tree_View;

      Show_Locations    : Boolean := True;
      --  Whether we should show the locations in the call graph

      Block_On_Expanded : Boolean := False;
      --  If true, we do not recompute the contents of children nodes when a
      --  node is expanded

      Locations_Tree    : Gtk_Tree_View;
      Locations_Model   : Gtk_List_Store;

      Stored_Pos        : Float := 0.0;
      --  The position to set when Realizing this view; 0 means 'do not modify'

      Pane              : Gtk_Hpaned;

      Requests : Request_Lists.List;
      --  The currently ongoing LSP requests for this view
   end record;

   type Decl_Record is record
      --  Represents an entity by its basic variables
      Name   : Unbounded_String;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      File   : Virtual_File;

      Project : Virtual_File; --  Needed to disambiguate in aggregate projects
   end record;

   No_Decl : constant Decl_Record :=
     (Null_Unbounded_String,
      Editable_Line_Type'Last, Visible_Column_Type'Last,
      No_File, No_File);

   type Reference_Record is record
      Line                : Integer;
      Column              : Visible_Column_Type;
      File                : GNATCOLL.VFS.Virtual_File;
      Through_Dispatching : Boolean;
   end record;

   No_Reference_Record : constant Reference_Record
     := (Integer'Last, Visible_Column_Type'Last, No_File, False);
   --  Constant to denote no reference

   package Reference_List is
     new Ada.Containers.Vectors (Positive, Reference_Record);

   type List_Access is access Reference_List.Vector;

   function To_Reference_List is new Ada.Unchecked_Conversion
     (System.Address, List_Access);
   function To_Address is new Ada.Unchecked_Conversion
     (List_Access, System.Address);

   -----------------------
   -- Local subprograms --
   -----------------------

   function Get_Locations_List
     (View   : access Callgraph_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Create : Boolean := False) return List_Access;
   --  Get the locations list associated with Iter. Create it if necessary

   function To_Record
     (Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean) return Reference_Record;
   --  Extract the relevant information from Ref

   function To_XML (R : Reference_Record) return Node_Ptr;
   function From_XML (N : Node_Ptr) return Reference_Record;
   --  Conversion functions

   overriding procedure Save_To_XML
     (View : access Callgraph_View_Record; XML : in out XML_Utils.Node_Ptr);
   overriding procedure Load_From_XML
     (View : access Callgraph_View_Record; XML : XML_Utils.Node_Ptr);
   function Initialize
     (View   : access Callgraph_View_Record'Class) return Gtk_Widget;

   --  Limit to the sides of the MDI, because it can open editors and they
   --  would be displayed on top of the callgraph.
   package Generic_View is new Generic_Views.Simple_Views
     (Module_Name        => "Callgraph_View",
      View_Name          => "Call Trees",
      Reuse_If_Exist     => True,
      Group              => GPS.Kernel.MDI.Group_Consoles,
      Formal_MDI_Child   => CG_Child_Record,
      Formal_View_Record => Callgraph_View_Record,
      Local_Toolbar      => True,
      Areas              => Gtkada.MDI.Sides_Only);
   use Generic_View;
   subtype Callgraph_View_Access is Generic_View.View_Access;

   type Entity_Calls_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Search for the entities called by the current entity in Context

   type Entity_Called_By_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Search for th entities calling the current entity in Context

   type Calltree_Clear_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Calltree_Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Calltree_Remove_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Calltree_Remove_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Calltree_Next_Or_Previous_Command
   is new Interactive_Command with record
      Next : Boolean := True;
   end record;
   overriding function Execute
     (Command : access Calltree_Next_Or_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Calltree_Collapse_Or_Expand_Command (Is_Collapse : Boolean) is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Calltree_Collapse_Or_Expand_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Ancestors_User_Data is new Commands_User_Data_Record with record
      View           : Callgraph_View_Access;
      Entity_Ref     : Gtk_Tree_Row_Reference;
      Computing_Ref  : Gtk_Tree_Row_Reference;  --  "computing..." node
   end record;
   type Ancestors_User_Data_Access is access all Ancestors_User_Data'Class;
   overriding procedure Destroy
     (Data : in out Ancestors_User_Data; Cancelled : Boolean);
   overriding function On_Entity_Found
     (Data                : access Ancestors_User_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean;
   --  See inherited documentation

   function Get_Entity
     (View : access Callgraph_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Root_Entity'Class;
   --  Retrieve the entity at the given location

   function Get_Entity
     (View : access Callgraph_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Decl_Record;
   --  Same as above, returning the basic coordinates

   function Insert_Entity
     (View                : access Callgraph_View_Record'Class;
      Decl                : Decl_Record;
      Ref                 : Reference_Record;
      Suffix              : String := "";
      Kind                : View_Type;
      Parent_Iter         : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter;
   --  Insert an entry for Entity in the tree.
   --  Parent_Iter is the parent node for the node representing the entity.
   --  Parent is the caller of the entity.

   procedure On_Row_Expanded
     (View : access Gtk_Widget_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path);
   --  Called when a row is expanded by the user

   procedure On_Realize
     (View : access Gtk_Widget_Record'Class);
   --  Called when the view is realized

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
      (Self   : On_Pref_Changed;
       Kernel : not null access Kernel_Handle_Record'Class;
       Pref   : Default_Preferences.Preference);
   --  Called when the preferences change

   procedure On_Selection_Changed (View : access Gtk_Widget_Record'Class);
   --  Called when the selection changes in the view

   procedure On_View_Destroyed (View : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the main tree

   function Button_Press_On_List
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event on the locations list

   function On_Key_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "key_press" event

   procedure Select_Current_Location
     (View : access Callgraph_View_Record'Class);
   --  Open an editor to the current location

   procedure Open_Selected_Value
     (View : access Callgraph_View_Record'Class);
   --  Open the value currently selected in the main tree

   function Get_View_Type
     (Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter) return View_Type;
   pragma Inline (Get_View_Type);
   --  Returns the View_Type for the entity pointer to by Iter

   procedure Free_And_Remove
     (Model : Gtk_Tree_Store;
      Iter  : in out Gtk_Tree_Iter);
   --  Removes Iter from Tree_Store and free allocated memory.
   --  After being removed, Iter is set to the
   --  next valid row at that level, or invalidated if it previously pointed to
   --  the last one.

   procedure Get_Selected
     (View   : access Callgraph_View_Record'Class;
      Model  : out Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Returns the first selected element with Selection_Multiple.

   function Should_Use_ALS
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Boolean;
   --  Whether we should use the ALS and the ALS specific requests to
   --  compute the call tree.

   function Entity_To_Decl (X : Root_Entity'Class) return Decl_Record;
   --  Return a Decl_Record for the declaration of X

   function Context_To_Decl (C : Selection_Context) return Decl_Record;
   --  Extract the Declaration record from the context

   ---------------------
   -- Context_To_Decl --
   ---------------------

   function Context_To_Decl (C : Selection_Context) return Decl_Record is
      File : Virtual_File;
   begin
      if Has_Entity_Name_Information (C)
        and then Has_Entity_Line_Information (C)
        and then Has_Entity_Column_Information (C)
      then
         File := File_Information (C);
         return (Name    => To_Unbounded_String
                 (Entity_Name_Information (C)),
                 Line    => Entity_Line_Information (C),
                 Column  => Entity_Column_Information (C),
                 File    => File,
                 Project => Lookup_Project
                   (Get_Kernel (C), File).Project_Path);
      end if;
      return No_Decl;
   end Context_To_Decl;

   --------------------
   -- Entity_To_Decl --
   --------------------

   function Entity_To_Decl (X : Root_Entity'Class) return Decl_Record is
      Decl : constant General_Entity_Declaration :=
        Get_Declaration (X);
   begin
      return (Name    => Decl.Name,
              File    => Decl.Loc.File,
              Project => Decl.Loc.Project_Path,
              Line    => Editable_Line_Type (Decl.Loc.Line),
              Column  => Decl.Loc.Column);
   end Entity_To_Decl;

   --------------------
   -- Should_Use_ALS --
   --------------------

   function Should_Use_ALS
     (Kernel : Kernel_Handle;
      File   : Virtual_File) return Boolean
   is
      Ada_Lang : constant Language_Access := Get_Language_Handler
        (Kernel).Get_Language_By_Name ("ada");

   begin
      return GPS.LSP_Module.LSP_Is_Enabled (Ada_Lang)
        and then Kernel.Get_Language_Handler.Get_Language_From_File
          (File) = Ada_Lang;
   end Should_Use_ALS;

   ------------------
   -- Get_Selected --
   ------------------

   procedure Get_Selected
     (View   : access Callgraph_View_Record'Class;
      Model  : out Gtk.Tree_Model.Gtk_Tree_Model;
      Iter   : out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      List : Gtk_Tree_Path_List.Glist;
      Path : Gtk_Tree_Path;
      use Gtk_Tree_Path_List;
   begin
      View.Tree.Get_Selection.Get_Selected_Rows (Model, List);
      if List /= Null_List then
         Path := Gtk_Tree_Path
           (Gtk_Tree_Path_List.Get_Data (Gtk_Tree_Path_List.First (List)));
         Iter := Gtk.Tree_Model.Get_Iter (Model, Path);
      else
         Iter := Null_Iter;
      end if;
      Free_Path_List (List);
   end Get_Selected;

   ---------------------
   -- Free_And_Remove --
   ---------------------

   procedure Free_And_Remove
     (Model : Gtk_Tree_Store;
      Iter  : in out Gtk_Tree_Iter)
   is
      L_Value : GValue;
      L       : List_Access;
      Addr    : System.Address := System.Null_Address;

      use type System.Address;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Reference_List.Vector, List_Access);

   begin
      if Iter /= Null_Iter then
         Get_Value (Model, Iter, List_Column, L_Value);
         Addr := Get_Address (L_Value);

         if Addr /= System.Null_Address then
            L := To_Reference_List (Addr);
            Unchecked_Free (L);
         end if;

         Remove (Model, Iter);
      end if;
   end Free_And_Remove;

   -------------------
   -- Get_View_Type --
   -------------------

   function Get_View_Type
     (Model : Gtk_Tree_Model; Iter : Gtk_Tree_Iter) return View_Type is
   begin
      if Iter = Null_Iter then
         return View_Calls;
      else
         return View_Type'Val (Get_Int (Model, Iter, Kind_Column));
      end if;
   end Get_View_Type;

   ---------------
   -- To_Record --
   ---------------

   function To_Record
     (Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean) return Reference_Record is
   begin
      if Ref = No_Root_Entity_Reference then
         return No_Reference_Record;
      else
         return (Get_Location (Ref).Line,
                 Get_Location (Ref).Column,
                 Get_Location (Ref).File,
                 Through_Dispatching);
      end if;
   end To_Record;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (View : access Callgraph_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Root_Entity'Class
   is
      Model    : constant Gtk_Tree_Store :=
                   Gtk_Tree_Store (Gtk.Tree_Store."-"(Get_Model (View.Tree)));
   begin
      return View.Kernel.Databases.Get_Entity
        (Name => Get_String (Model, Iter, Entity_Name_Column),
         Loc  =>
           (File => Get_File (Model, Iter, File_Column),
            Project_Path => Get_File (Model, Iter, Project_Column),
            Line => Integer (Get_Int (Model, Iter, Line_Column)),
            Column  => Visible_Column_Type
              (Get_Int (Model, Iter, Column_Column))));
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (View : access Callgraph_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Decl_Record
   is
      Model    : constant Gtk_Tree_Store :=
                   Gtk_Tree_Store (Gtk.Tree_Store."-"(Get_Model (View.Tree)));
   begin
      return
        (Name    => To_Unbounded_String
           (Get_String (Model, Iter, Entity_Name_Column)),
         File    => Get_File (Model, Iter, File_Column),
         Project => Get_File (Model, Iter, Project_Column),
         Line    => Editable_Line_Type (Get_Int (Model, Iter, Line_Column)),
         Column  => Visible_Column_Type
           (Get_Int (Model, Iter, Column_Column)));
   end Get_Entity;

   -----------------------------
   -- Select_Current_Location --
   -----------------------------

   procedure Select_Current_Location
     (View : access Callgraph_View_Record'Class)
   is
      Iter, It   : Gtk_Tree_Iter;
      Model, Locations_Model  : Gtk_Tree_Model;
      File   : GNATCOLL.VFS.Virtual_File;
      Project  : Project_Type;
   begin
      Get_Selected (View, Model, Iter);

      if Iter /= Null_Iter then
         if Parent (Model, Iter) = Null_Iter then
            Open_Selected_Value (View);

         else
            --  The entity to highlight depends on the type of the view
            case Get_View_Type (Model, Iter) is
               when View_Calls =>  It := Iter;
               when View_Called_By => It := Parent (Model, Iter);
            end case;

            Get_Selected
              (Get_Selection (View.Locations_Tree), Locations_Model, Iter);

            if Iter /= Null_Iter then
               File := Get_File (Locations_Model, Iter, Location_File_Column);
               Project := Get_Registry (View.Kernel).Tree.Project_From_Path
                 (Get_File (Locations_Model, Iter, Location_Project_Column));

               --  Give the focus to the editor, to match the behavior of the
               --  Locations view.
               Open_File_Action_Hook.Run
                 (Kernel     => View.Kernel,
                  File       => File,
                  Project    => Project,
                  Line       => Natural
                    (Get_Int (Locations_Model, Iter, Location_Line_Column)),
                  Column     => Visible_Column_Type
                    (Get_Int (Locations_Model, Iter, Location_Column_Column)),
                  Column_End => Visible_Column_Type
                    (Get_Int (Locations_Model, Iter, Location_Column_Column))
                    + Get_String (Model, It, Entity_Name_Column)'Length,
                  New_File   => False,
                  Focus      => True);
            end if;
         end if;
      end if;
   end Select_Current_Location;

   ------------------
   -- On_Key_Press --
   ------------------

   function On_Key_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      V     : Callgraph_View_Access;
      Key   : Gdk_Key_Type;
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;

      procedure Strong_Select (T : Gtk_Tree_View; I : Gtk_Tree_Iter);
      --  Select and place the keyboard focus on I

      procedure Select_In_Base_Tree (Going_Down : Boolean := True);
      --  Select the next iter in the base tree, with the given direction

      -------------------
      -- Strong_Select --
      -------------------

      procedure Strong_Select (T : Gtk_Tree_View; I : Gtk_Tree_Iter) is
         Path : Gtk_Tree_Path;
      begin
         Select_Iter (Get_Selection (T), I);
         Path := Get_Path (Get_Model (T), I);
         Set_Cursor (T, Path, null, False);
         Path_Free (Path);
      end Strong_Select;

      -------------------------
      -- Select_In_Base_Tree --
      -------------------------

      procedure Select_In_Base_Tree (Going_Down : Boolean := True) is
         Selected : Gtk_Tree_Iter;
         Model    : Gtk_Tree_Model;
         New_Iter : Gtk_Tree_Iter;
         Path     : Gtk_Tree_Path;
         Result   : Boolean;
      begin
         Get_Selected (V, Model, Selected);

         if Selected /= Null_Iter then
            New_Iter := Selected;

            Path := Get_Path (Model, New_Iter);

            if Going_Down then
               if Row_Expanded (V.Tree, Path) then
                  New_Iter := Children (Model, New_Iter);
               else
                  Next (Model, New_Iter);

                  while New_Iter = Null_Iter and then Up (Path)
                    and then Get_Depth (Path) > 0
                  loop
                     New_Iter := Get_Iter (Model, Path);
                     Next (Model, New_Iter);
                  end loop;
               end if;

            else
               Result := Prev (Path);

               if Result then
                  if Row_Expanded (V.Tree, Path) then
                     while Row_Expanded (V.Tree, Path) loop
                        Down (Path);
                     end loop;

                     while Get_Iter (Model, Path) /= Null_Iter loop
                        New_Iter := Get_Iter (Model, Path);
                        Next (Path);
                     end loop;
                  else
                     New_Iter := Get_Iter (Model, Path);
                  end if;
               else
                  New_Iter := Parent (Model, Selected);
               end if;

            end if;

            Path_Free (Path);

            if New_Iter /= Null_Iter then
               Strong_Select (V.Tree, New_Iter);

               if Going_Down then
                  New_Iter := Get_Iter_First (V.Locations_Model);

               else
                  if Children (V.Locations_Model, Null_Iter) /= Null_Iter then
                     New_Iter := Nth_Child
                       (V.Locations_Model, Null_Iter,
                        N_Children (V.Locations_Model) - 1);
                  else
                     New_Iter := Null_Iter;
                  end if;
               end if;

               if New_Iter /= Null_Iter then
                  Strong_Select (V.Locations_Tree, New_Iter);
                  Grab_Focus (V.Locations_Tree);
               end if;
            end if;
         end if;
      end Select_In_Base_Tree;

      Path   : Gtk_Tree_Path;
      Result : Boolean;
   begin
      V := Callgraph_View_Access (View);

      Key := Get_Key_Val (Event);

      case Key is
         when GDK_Down | GDK_KP_Down =>
            Get_Selected (Get_Selection (V.Locations_Tree), Model, Iter);

            if Iter = Null_Iter then
               Select_In_Base_Tree (Going_Down => True);
            else
               Next (Model, Iter);

               if Iter = Null_Iter then
                  Select_In_Base_Tree (Going_Down => True);
               else
                  Strong_Select (V.Locations_Tree, Iter);
                  Grab_Focus (V.Locations_Tree);
               end if;
            end if;

            return True;

         when GDK_Up | GDK_KP_Up =>
            Get_Selected (Get_Selection (V.Locations_Tree), Model, Iter);

            if Iter = Null_Iter then
               Select_In_Base_Tree (Going_Down => False);
            else
               Path := Get_Path (Model, Iter);

               Result := Prev (Path);

               if Result then
                  Strong_Select (V.Locations_Tree, Get_Iter (Model, Path));
                  Grab_Focus (V.Locations_Tree);
               else
                  Select_In_Base_Tree (Going_Down => False);
               end if;

               Path_Free (Path);
            end if;

            return True;

         when GDK_Right | GDK_KP_Right =>
            Get_Selected (V, Model, Iter);

            if Iter /= Null_Iter then
               Path := Get_Path (Model, Iter);
               if Row_Expanded (V.Tree, Path) then
                  Iter := Children (Model, Iter);

                  if Iter /= Null_Iter then
                     Strong_Select (V.Tree, Iter);
                  end if;
               else
                  Result := Expand_Row (V.Tree, Path, False);
               end if;
               Path_Free (Path);
            end if;

            return True;

         when GDK_Left | GDK_KP_Left =>
            Get_Selected (V, Model, Iter);

            if Iter /= Null_Iter then
               Path := Get_Path (Model, Iter);

               if Row_Expanded (V.Tree, Path) then
                  Result := Collapse_Row (V.Tree, Path);
               else
                  if Up (Path) then
                     Result := Collapse_Row (V.Tree, Path);
                     if Result then
                        Strong_Select (V.Tree, Get_Iter (Model, Path));
                     end if;
                  end if;
               end if;

               Path_Free (Path);
            end if;

            return True;

         when GDK_Return =>
            Select_Current_Location (V);

            return True;
         when others =>
            return False;
      end case;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Key_Press;

   -------------------------
   -- Open_Selected_Value --
   -------------------------

   procedure Open_Selected_Value
     (View : access Callgraph_View_Record'Class)
   is
      Iter  : Gtk_Tree_Iter;
      Model : Gtk_Tree_Model;
      Decl  : General_Entity_Declaration;
   begin
      Get_Selected (View, Model, Iter);

      if Iter /= Null_Iter then
         Decl := Get_Declaration
           (Get_Entity (View, Iter));

         Open_File_Action_Hook.Run
           (Kernel     => View.Kernel,
            File       => Decl.Loc.File,
            Project    => Get_Project (Decl.Loc),
            Line       => Decl.Loc.Line,
            Column     => Decl.Loc.Column,
            Column_End => Decl.Loc.Column
               + Visible_Column_Type (Length (Decl.Name)),
            Focus      => True);
      end if;
   end Open_Selected_Value;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View : Callgraph_View_Access;
   begin
      View := Callgraph_View_Access (Widget);

      if Get_Event_Type (Event) = Gdk_2button_Press then
         Open_Selected_Value (View);
      end if;

      return False;
   end Button_Press;

   --------------------------
   -- Button_Press_On_List --
   --------------------------

   function Button_Press_On_List
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View : Callgraph_View_Access;
      Iter : Gtk_Tree_Iter;
   begin
      View := Callgraph_View_Access (Widget);

      if Get_Event_Type (Event) = Button_Press then
         Iter := Find_Iter_For_Event (View.Locations_Tree, Event);

         if Iter /= Null_Iter then
            Select_Iter (Get_Selection (View.Locations_Tree), Iter);
            Select_Current_Location (View);
            return True;
         end if;
      end if;

      return False;
   end Button_Press_On_List;

   -----------------------
   -- On_View_Destroyed --
   -----------------------

   procedure On_View_Destroyed (View : access Gtk_Widget_Record'Class) is
      V : constant Callgraph_View_Access := Callgraph_View_Access (View);
   begin
      for Ref of V.Requests loop
         if Has_Request (Ref) then
            Called_By_Request'Class (Request (Ref).all).Invalidated := True;
         end if;
      end loop;
      V.Requests.Clear;
   end On_View_Destroyed;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (View : access Gtk_Widget_Record'Class) is
      V             : constant Callgraph_View_Access :=
                        Callgraph_View_Access (View);
      L             : List_Access;
      Iter          : Gtk_Tree_Iter;
      Model         : Gtk_Tree_Model;
      Value         : GValue;
      T             : Gtk_Tree_Iter;
      Address       : System.Address;
      Appended      : Boolean := False;
      use type System.Address;
   begin
      Get_Selected (V, Model, Iter);

      --  Remove old locations. If there is nothing selected anymore, we should
      --  not show any location

      Clear (V.Locations_Model);

      if Iter /= Null_Iter then

         --  Then an entry for each call

         Get_Value (Model, Iter, List_Column, Value);
         Address := Get_Address (Value);

         if Address /= System.Null_Address then
            L := To_Reference_List (Address);

            for R of L.all loop
               Append (V.Locations_Model, T);
               Appended := True;

               Set (V.Locations_Model, T, Location_Line_Column,
                    Gint (R.Line));
               Set (V.Locations_Model, T, Location_Column_Column,
                    Gint (R.Column));
               Set (V.Locations_Model, T, Location_Character_Column, ":");
               Set_File (V.Locations_Model, T, Location_File_Column, R.File);
               Set_File
                 (V.Locations_Model, T, Location_Project_Column,
                  Get_Project_For_File
                    (V.Kernel.Get_Project_Tree, R.File).Project_Path);

               if R.Through_Dispatching then
                  Set (V.Locations_Model, T, Location_String_Column,
                       "    " & Display_Base_Name (R.File)
                       & " (through dispatching)");
               else
                  Set (V.Locations_Model, T, Location_String_Column,
                       "    " & Display_Base_Name (R.File));
               end if;
            end loop;
         end if;

         if Appended then
            Select_Iter
              (Get_Selection (V.Locations_Tree),
               Get_Iter_First (V.Locations_Model));
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Selection_Changed;

   ----------------
   -- On_Realize --
   ----------------

   procedure On_Realize
     (View : access Gtk_Widget_Record'Class)
   is
      V : constant Callgraph_View_Access := Callgraph_View_Access (View);

   begin
      if V.Stored_Pos /= 0.0 then
         Set_Position_Percent (V.Pane, V.Stored_Pos);
         V.Stored_Pos := 0.0;
      end if;
   end On_Realize;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (View : access Gtk_Widget_Record'Class;
      Iter : Gtk_Tree_Iter;
      Path : Gtk_Tree_Path)
   is
      V              : constant Callgraph_View_Access :=
                        Callgraph_View_Access (View);
      M              : constant Gtk_Tree_Store := -Get_Model (V.Tree);
      Child          : Gtk_Tree_Iter := Null_Iter;
      Dummy          : Gtk_Tree_Iter;
      Column         : Gint;
      Data           : Ancestors_User_Data_Access;
      Computing_Iter : Gtk_Tree_Iter := Null_Iter;

      Local_Path     : Gtk_Tree_Path;
      Model          : constant Gtk_Tree_Model := V.Tree.Get_Model;
      Row            : Gtk_Tree_Row_Reference;
      File           : Virtual_File;
   begin
      if V.Block_On_Expanded then
         return;
      end if;

      Column := Freeze_Sort (M);
      begin
         --  We always recompute the call graph. gtk+ would lose the
         --  expanded status of children anyway, so we might as well
         --  recompute everything. It is also more logical from the user's
         --  point of view that this would act as a refresh. Keep one child
         --  (the computing node), or the expanded status is lost by gtk+.

         Local_Path := Get_Path (Model, Iter);
         Gtk_New (Row, Model, Local_Path);
         Path_Free (Local_Path);

         Prepend (M, Computing_Iter, Iter);
         Set (M, Computing_Iter, Name_Column, Computing_Label);

         Child := Computing_Iter;
         Next (M, Child);

         while Child /= Null_Iter loop
            Free_And_Remove (M, Child);
         end loop;

         File := Get_File (Model, Iter, File_Column);

         --  We can use the ALS, if it's activated, for the "View_Called_By"
         --  request.

         if Should_Use_ALS (V.Kernel, File)
           and then Get_View_Type (Get_Model (V.Tree), Iter) = View_Called_By
         then

            --  Start the view's progress bar while waiting for results
            Generic_Views.Abstract_View_Access
              (V).Set_Activity_Progress_Bar_Visibility (True);

            declare
               Ada_Lang : constant Language_Access := Get_Language_Handler
                 (V.Kernel).Get_Language_By_Name ("ada");
               R : Called_By_Request_Access;
            begin
               R := new Called_By_Request'
                 (LSP_Request with
                  Kernel        => V.Kernel,
                  Text_Document => File,
                  Line          => Positive
                    (Get_Int (Model, Iter, Line_Column)),
                  Column        => Visible_Column_Type
                    (Get_Int (Model, Iter, Column_Column)),
                  Where         => Gtk_Tree_Row_Reference_New
                    (Model => Model,
                     Path  => Get_Path (Model, Iter)),
                  Invalidated   => False);

               V.Requests.Append
                 (GPS.LSP_Client.Requests.Execute
                    (Ada_Lang, Request_Access (R)));
            end;
         else
            Data := new Ancestors_User_Data'
              (Commands_User_Data_Record with
               View        => V,
               Computing_Ref => Null_Gtk_Tree_Row_Reference,
               Entity_Ref    => Row);

            Local_Path := Get_Path (Model, Computing_Iter);
            Gtk_New (Data.Computing_Ref, Model, Local_Path);
            Path_Free (Local_Path);

            declare
               Entity : constant Root_Entity'Class := Get_Entity (V, Iter);
            begin

               --  If we have the locations node, do nothing
               if Entity = No_Root_Entity then
                  return;
               end if;

               case Get_View_Type (Get_Model (V.Tree), Iter) is
               when View_Calls     =>
                  Examine_Entity_Call_Graph
                    (Entity            => Entity,
                     User_Data         => Data,
                     Dispatching_Calls => True,
                     Get_All_Refs      => True);

               when View_Called_By =>
                  Examine_Ancestors_Call_Graph
                    (Kernel            => V.Kernel,
                     Entity            => Entity,
                     User_Data         => Data,
                     Watch             => Gtk_Widget (V),
                     Dispatching_Calls => True,
                     Background_Mode   => True);
               end case;
            end;
         end if;

         Thaw_Sort (M, Column);

      exception
         when E : others =>
            Trace (Me, E);
            Thaw_Sort (M, Column);
      end;

      Set_Cursor (V.Tree, Path, null, False);
   exception
      when E : others =>
         Trace (Me, E);
   end On_Row_Expanded;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Calltree_Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Callgraph_View_Access :=
        Generic_View.Retrieve_View (Get_Kernel (Context.Context));
      Model : Gtk_Tree_Store;
      Iter  : Gtk_Tree_Iter;
   begin
      if View /= null then
         Model := -Get_Model (View.Tree);
         Iter := Model.Get_Iter_First;
         while Iter /= Null_Iter loop
            Free_And_Remove (Model, Iter);
         end loop;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Calltree_Collapse_Or_Expand_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      View   : constant Callgraph_View_Access :=
        Generic_View.Retrieve_View (Get_Kernel (Context.Context));
      List   : Gtk_Tree_Path_List.Glist;
      G_Iter : Gtk_Tree_Path_List.Glist;
      Path   : Gtk_Tree_Path;
      Model  : Gtk_Tree_Model;
      Dummy  : Boolean;

      use Gtk_Tree_Path_List;
   begin
      if View /= null then
         Get_Selected_Rows (View.Tree.Get_Selection, Model, List);

         if Model /= Null_Gtk_Tree_Model and then List /= Null_List then
            --  The children must be modified before there fathers
            G_Iter := Gtk_Tree_Path_List.Last (List);

            while G_Iter /= Gtk_Tree_Path_List.Null_List loop
               Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));

               if Path /= Null_Gtk_Tree_Path then
                  if Command.Is_Collapse then
                     Dummy := Collapse_Row (View.Tree, Path);
                  else
                     Dummy := Expand_Row (View.Tree, Path, False);
                  end if;
               end if;

               G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
            end loop;
         end if;
         Free_Path_List (List);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Calltree_Remove_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View   : constant Callgraph_View_Access :=
        Generic_View.Retrieve_View (Get_Kernel (Context.Context));
      List   : Gtk_Tree_Path_List.Glist;
      Iter   : Gtk_Tree_Iter;
      G_Iter : Gtk_Tree_Path_List.Glist;
      Path   : Gtk_Tree_Path;
      Model  : Gtk_Tree_Model;

      use Gtk_Tree_Path_List;
   begin
      if View /= null then
         Get_Selected_Rows (View.Tree.Get_Selection, Model, List);

         if Model /= Null_Gtk_Tree_Model and then List /= Null_List then
            G_Iter := Gtk_Tree_Path_List.Last (List);

            while G_Iter /= Gtk_Tree_Path_List.Null_List loop
               Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
               if Path /= Null_Gtk_Tree_Path then
                  Iter := Get_Iter (Model, Path);
               end if;

               if Iter /= Null_Iter then
                  Free_And_Remove (Gtk_Tree_Store'(-Model), Iter);
               end if;

               G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
            end loop;
         end if;
         Free_Path_List (List);
      end if;
      return Commands.Success;
   end Execute;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access CG_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Context : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      V      : constant Callgraph_View_Access :=
        Callgraph_View_Access (GPS_MDI_Child (Self).Get_Actual_Widget);
      Iter   : Gtk_Tree_Iter;
   begin
      if Event /= null then
         Iter := Find_Iter_For_Event (V.Tree, Event);

         if Iter /= Null_Iter then
            Select_Iter (Get_Selection (V.Tree), Iter);

            declare
               Entity : constant Root_Entity'Class := Get_Entity (V, Iter);
            begin
               if Entity /= No_Root_Entity then
                  Set_File_Information   (Context, Files  => Empty_File_Array);
                  Set_Entity_Information (Context, Entity => Entity);
               end if;
            end;
         else
            Unselect_All (Get_Selection (V.Tree));
         end if;
      end if;
      return Context;
   end Build_Context;

   ------------
   -- To_XML --
   ------------

   function To_XML (R : Reference_Record) return Node_Ptr is
      Result : Node_Ptr;
   begin
      Result := new Node;
      Result.Tag := new String'("loc");
      Set_Attribute (Result, "line", R.Line'Img);
      Set_Attribute (Result, "column", R.Column'Img);
      Set_Attribute (Result, "file", Display_Full_Name (R.File));
      --   ??? Wrong: we have to call Display_Full_Name above only because we
      --  are setting an attribute in an XML node. We should not put
      --  text which is potentially not UTF8 in an attribute.
      if R.Through_Dispatching then
         Set_Attribute (Result, "dispatch", "true");
      end if;
      return Result;
   end To_XML;

   --------------
   -- From_XML --
   --------------

   function From_XML (N : Node_Ptr) return Reference_Record is
      Result : Reference_Record;
   begin
      Result.Line := Integer'Value (Get_Attribute (N, "line"));
      Result.Column := Visible_Column_Type'Value (Get_Attribute (N, "column"));
      Result.File := Create (+Get_Attribute (N, "file"));
      Result.Through_Dispatching := Get_Attribute (N, "dispatch") = "true";
      return Result;
   end From_XML;

   ------------------------
   -- Get_Locations_List --
   ------------------------

   function Get_Locations_List
     (View   : access Callgraph_View_Record'Class;
      Iter   : Gtk_Tree_Iter;
      Create : Boolean := False) return List_Access
   is
      L_Value : GValue;
      L       : List_Access;
      Addr    : System.Address;

      use type System.Address;
   begin
      Get_Value (Get_Model (View.Tree), Iter, List_Column, L_Value);
      Addr := Get_Address (L_Value);

      if Addr /= System.Null_Address then
         return To_Reference_List (Addr);

      elsif Create then
         L := new Reference_List.Vector;
         Addr := To_Address (L);
         Init (L_Value, GType_Pointer);
         Set_Address (L_Value, Addr);
         Set_Value
           (Gtk_Tree_Store'(-Get_Model (View.Tree)),
            Iter, List_Column, L_Value);

         return L;
      end if;

      return null;
   end Get_Locations_List;

   -----------------
   -- Save_To_XML --
   -----------------

   overriding procedure Save_To_XML
     (View : access Callgraph_View_Record; XML : in out XML_Utils.Node_Ptr)
   is
      Model : constant Gtk_Tree_Store := -Get_Model (View.Tree);
      Root  : Node_Ptr;

      procedure Recursive_Save
        (Parent_Iter : Gtk_Tree_Iter; Parent : Node_Ptr);
      --  Save Iter recursively

      --------------------
      -- Recursive_Save --
      --------------------

      procedure Recursive_Save
        (Parent_Iter : Gtk_Tree_Iter; Parent : Node_Ptr)
      is
         Iter   : Gtk_Tree_Iter;
         N      : Node_Ptr;
         L      : List_Access;
         Path   : Gtk_Tree_Path;

      begin
         if Parent_Iter = Null_Iter then
            Iter := Get_Iter_First (Model);
         else
            Iter := Children (Model, Parent_Iter);
         end if;

         while Iter /= Null_Iter loop
            if Get_String (Model, Iter, Name_Column) /= Computing_Label then
               N := new XML_Utils.Node;
               N.Tag := new String'("entity");
               Add_Child (Parent, N, Append => True);

               Path := Get_Path (Model, Iter);
               if Row_Expanded (View.Tree, Path) then
                  Set_Attribute (N, "expanded", "true");
               end if;
               Path_Free (Path);

               Set_Attribute
                 (N, "name", Get_String (Model, Iter, Name_Column));
               Set_Attribute
                 (N, "decl", Get_String (Model, Iter, Decl_Column));

               Set_Attribute
                 (N, "type",
                  View_Type'Image
                    (View_Type'Val (Get_Int (Model, Iter, Kind_Column))));
               N.Tag := new String'("entity");
               Set_Attribute
                 (N, "entity_name",
                  Get_String (Model, Iter, Entity_Name_Column));
               --  ??? This is potentially not UTF8, should not be in an
               --  attribute
               Set_Attribute
                 (N, "entity_decl",
                  Get_File (Model, Iter, File_Column).Display_Full_Name);
               --  ??? This is potentially not UTF8, should not be in an
               --  attribute
               Set_Attribute
                 (N, "entity_line",
                  Image (Integer (Get_Int (Model, Iter, Line_Column))));
               Set_Attribute
                 (N, "entity_column",
                  Image (Integer (Get_Int (Model, Iter, Column_Column))));
               Set_Attribute
                 (N, "entity_project",
                  Get_File (Model, Iter, Project_Column).Display_Full_Name);

               L := Get_Locations_List (View, Iter, False);

               if L /= null then
                  for Item of L.all loop
                     XML_Utils.Add_Child (N, To_XML (Item), True);
                  end loop;
               end if;

               Recursive_Save (Iter, N);
            end if;

            Next (Model, Iter);
         end loop;
      end Recursive_Save;

   begin
      Root := new Node;
      XML.Child := Root;
      Root.Tag := new String'("callgraph");
      Set_Attribute
        (Root, "position",
         Float'Image (Get_Position_Percent (View.Pane)) & "%");

      Recursive_Save (Null_Iter, Root);
   end Save_To_XML;

   -------------------
   -- Load_From_XML --
   -------------------

   overriding procedure Load_From_XML
     (View : access Callgraph_View_Record; XML : XML_Utils.Node_Ptr)
   is
      Model    : constant Gtk_Tree_Store := -Get_Model (View.Tree);

      Is_Calls : Boolean := True;
      --  For upward compatibility

      procedure Recursive_Load
        (Parent_Iter   : Gtk_Tree_Iter;
         Node          : Node_Ptr;
         Expand_Parent : Boolean);
      --  Load Node into Iter, and the children of Node.
      --  Expand_Parent should be set to True if the parent node should be
      --  expanded after a first child has been added to it. We unfortunately
      --  need this parameter, since otherwise a child node cannot be expanded
      --  before its parent has been expanded, and a parent cannot be expanded
      --  before it has at least one child.

      --------------------
      -- Recursive_Load --
      --------------------

      procedure Recursive_Load
        (Parent_Iter   : Gtk_Tree_Iter;
         Node          : Node_Ptr;
         Expand_Parent : Boolean)
      is
         Iter   : Gtk_Tree_Iter := Null_Iter;
         Dummy  : Gtk_Tree_Iter;
         N      : Node_Ptr := Node;
         L      : List_Access;
         Tmp    : Boolean;
         pragma Unreferenced (Tmp);
      begin
         while N /= null loop
            if N.Tag.all = "loc" then
               L := Get_Locations_List (View, Parent_Iter, True);
               L.Append (From_XML (N));

            else
               Append (Model, Iter, Parent_Iter);

               if Expand_Parent and then N = Node then
                  View.Block_On_Expanded := True;
                  Expand_Row (View.Tree, Parent_Iter);
                  View.Block_On_Expanded := False;
               end if;

               --  We want to be compatible with previous version not having
               --  the type node. We then get information from top type node
               --  in this case.

               Set_And_Clear
                 (Model, Iter,
                  (Name_Column, Decl_Column, Entity_Name_Column, File_Column,
                   Line_Column, Column_Column, Project_Column, Kind_Column,
                   Sort_Column),
                  (1 => As_String (Get_Attribute (N, "name")),
                   2 => As_String (Get_Attribute (N, "decl")),
                   3 => As_String (Get_Attribute (N, "entity_name")),
                   4 => As_File   (Create (+Get_Attribute (N, "entity_decl"))),
                   5 => As_Int (Gint'Value (Get_Attribute (N, "entity_line"))),
                   6 => As_Int
                     (Gint'Value (Get_Attribute (N, "entity_column"))),
                   7 => As_File
                     (Create (+Get_Attribute (N, "entity_project"))),
                   8 => As_Int (View_Type'Pos
                     (View_Type'Value
                        (if Is_Calls
                           then Get_Attribute (N, "type", "view_calls")
                           else Get_Attribute (N, "type", "view_called_by")))),
                   9 => As_String (Get_Attribute
                     (N, "name") & " " & Get_Attribute (N, "decl"))));

               if N.Child /= null then
                  Recursive_Load
                    (Iter, N.Child,
                     Expand_Parent => Get_Attribute (N, "expanded") = "true");
               else
                  Append (Model, Dummy, Iter);
                  Model.Set (Dummy, Name_Column, Computing_Label);
               end if;
            end if;

            N := N.Next;
         end loop;
      end Recursive_Load;

      Callgraph : constant Node_Ptr := XML.Child;  --  The <callgraph> node
   begin
      if Callgraph = null then
         return;
      end if;

      declare
         Pos_Str : constant String := Get_Attribute (Callgraph, "position");
         V_Type  : constant String := Get_Attribute (XML, "type");
      begin
         if Pos_Str /= "" then
            if Pos_Str (Pos_Str'Last) = '%' then
               View.Stored_Pos := Float'Value
                 (Pos_Str (Pos_Str'First .. Pos_Str'Last - 1));
            else
               --  If the position is set in the old format (absolute position)
               --  don't take it into account: we don't know the size of the
               --  view at this stage.

               View.Stored_Pos := 0.0;
            end if;
         end if;

         Is_Calls := V_Type = "calls";
      end;

      Recursive_Load (Null_Iter, Callgraph.Child, False);
   end Load_From_XML;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (View   : access Callgraph_View_Record'Class) return Gtk_Widget
   is
      Names  : GNAT.Strings.String_List := (1 => new String'(-"Name"));
      Scroll : Gtk_Scrolled_Window;

   begin
      Initialize_Vbox (View, Homogeneous => False);

      Gtk_New_Hpaned (View.Pane);
      View.Pack_Start (View.Pane, Expand => True, Fill => True);

      Gtk_New (Scroll);
      View.Pane.Pack1 (Scroll, Resize => True, Shrink => False);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => Column_Types,
         Column_Names       => Names,
         Show_Column_Titles => False,
         Sortable_Columns   => True);
      Set_Name (View.Tree, "Call Graph Tree"); --  For test suite
      View.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      View.Tree.Set_Search_Column (Name_Column);
      Scroll.Add (View.Tree);

      --  Set custom order by column: Name & Decl
      View.Tree.Get_Column (Name_Column).Set_Sort_Column_Id (Sort_Column);
      View.Tree.Get_Column (Name_Column).Clicked;

      Gtk_New (Scroll);
      Scroll.Set_Policy (Policy_Automatic, Policy_Automatic);
      View.Pane.Pack2 (Scroll, Resize => True, Shrink => True);

      --  Create the lines list

      Gtk_New (View.Locations_Model,
               (Location_Line_Column      => GType_Int,
                Location_Column_Column    => GType_Int,
                Location_Character_Column => GType_String,
                Location_String_Column    => GType_String,
                Location_Project_Column   => Get_Virtual_File_Type,
                Location_File_Column      => Get_Virtual_File_Type));
      Gtk_New (View.Locations_Tree, View.Locations_Model);
      Set_Headers_Visible (View.Locations_Tree, False);
      View.Locations_Tree.Set_Enable_Search (False);
      Scroll.Add (View.Locations_Tree);

      Set_Name (View.Locations_Tree, "Call Graph Location Tree");
      --  For test suite

      --  Create the locations model

      declare
         C     : Gtk_Cell_Renderer_Text;
         Dummy : Gint;
         pragma Unreferenced (Dummy);
         Col   : Gtk_Tree_View_Column;
      begin
         Gtk_New (Col);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_Line_Column);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_Character_Column);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_Column_Column);
         Gtk_New (C);
         Pack_Start (Col, C, False);
         Add_Attribute (Col, C, "text", Location_String_Column);

         Dummy := Append_Column (View.Locations_Tree, Col);
      end;

      View.Show_Locations :=
        Get_History (Get_History (View.Kernel).all, History_Show_Locations);

      Modify_Font (View.Tree, View_Fixed_Font.Get_Pref);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press'Access),
         Slot_Object => View,
         After       => False);

      Return_Callback.Object_Connect
        (View.Locations_Tree,
         Signal_Button_Press_Event,
         Return_Callback.To_Marshaller (Button_Press_On_List'Access),
         Slot_Object => View,
         After       => False);

      Return_Callback.Object_Connect
        (View.Tree,
         Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press'Access),
         Slot_Object => View,
         After       => False);

      Return_Callback.Object_Connect
        (View.Locations_Tree,
         Signal_Key_Press_Event,
         Return_Callback.To_Marshaller (On_Key_Press'Access),
         Slot_Object => View,
         After       => False);

      Setup_Contextual_Menu
        (Kernel          => View.Kernel,
         Event_On_Widget => View.Tree);

      Widget_Callback.Object_Connect
        (View.Tree,
         Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (On_Row_Expanded'Access),
         Slot_Object => View);

      Widget_Callback.Object_Connect
        (View,
         Signal_Realize,
         Widget_Callback.To_Marshaller (On_Realize'Access),
         Slot_Object => View);

      Widget_Callback.Object_Connect
        (Get_Selection (View.Tree), Signal_Changed,
         Widget_Callback.To_Marshaller (On_Selection_Changed'Access),
         Slot_Object => View);

      Widget_Callback.Object_Connect
        (View.Tree, Signal_Destroy,
         Widget_Callback.To_Marshaller (On_View_Destroyed'Access),
         Slot_Object => View);

      Preferences_Changed_Hook.Add (new On_Pref_Changed, Watch => View);

      Free (Names);

      return Gtk_Widget (View.Tree);
   end Initialize;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Called_By_Request;
      Result : LSP.Messages.ALS_Subprogram_And_References_Vector)
   is
      function To_Decl
        (X : LSP.Messages.ALS_Subprogram_And_References) return Decl_Record;
      --  Convert from protocol data to our data

      function To_Reference_Record
        (X : LSP.Messages.Location) return Reference_Record;
      --  Convert from protocol data to our data

      -------------
      -- To_Decl --
      -------------

      function To_Decl
        (X : LSP.Messages.ALS_Subprogram_And_References) return Decl_Record
      is
         File : constant Virtual_File := To_Virtual_File (X.loc.uri);
      begin
         return (Name => To_UTF_8_Unbounded_String (X.name),
                 File => File,
                 Line => Editable_Line_Type (X.loc.span.first.line + 1),
                 Column => UTF_16_Offset_To_Visible_Column
                   (X.loc.span.first.character),
                 Project => Lookup_Project (Self.Kernel, File).Project_Path);
      end To_Decl;

      -------------------------
      -- To_Reference_Record --
      -------------------------

      function To_Reference_Record
        (X : LSP.Messages.Location) return Reference_Record
      is
         Is_Dispatching : Boolean := False;
      begin
         for K of X.alsKind.As_Strings loop
            if K = "dispatching call" then
               Is_Dispatching := True;
               exit;
            end if;
         end loop;

         return (Line   => Integer (X.span.first.line + 1),
                 Column => Visible_Column_Type (X.span.first.character + 1),
                 File   => To_Virtual_File (X.uri),
                 Through_Dispatching => Is_Dispatching);
      end To_Reference_Record;

      View        : constant Callgraph_View_Access :=
        Generic_View.Retrieve_View (Self.Kernel);
      Model       : Gtk_Tree_Store;
      Parent_Iter : Gtk_Tree_Iter;
      Decl        : Decl_Record;
      Dummy       : Gtk_Tree_Iter;
   begin
      --  If the request has been invalidated, simply do nothing here
      if Self.Invalidated then
         return;
      end if;

      --  If there is no view to be retrieved, this means that the view was
      --  hidden or closed by the user before the language server responded:
      --  in this case, simply exit. This should not happen in practice, but
      --  defensive programming can't hurt.
      if View = null then
         return;
      end if;

      --  Stop the view's progress bar
      Generic_Views.Abstract_View_Access
        (View).Set_Activity_Progress_Bar_Visibility (False);

      --  It's possible that the target row no longer exists by the time
      --  the request completes, for instance if the user has manually removed
      --  the parent row.
      if not Self.Where.Valid then
         return;
      end if;

      --  Find the path that corresponds to the declaration being explored

      if Self.Where = Null_Gtk_Tree_Row_Reference then
         Parent_Iter := Null_Iter;
      else
         Model := -Get_Model (View.Tree);
         Parent_Iter := Model.Get_Iter (Self.Where.Get_Path);
      end if;

      --  Add a child for each of the references found.

      for Reference of Result loop
         Decl := To_Decl (Reference);
         for R of Reference.refs loop
            Dummy := Insert_Entity
              (View        => View,
               Decl        => Decl,
               Ref         => To_Reference_Record (R),
               Parent_Iter => Parent_Iter,
               Kind        => View_Called_By);
         end loop;
      end loop;

      --  Remove the first iter, which was the "Computing..." iter.
      --  Do this at the end, to make sure the parent row remains
      --  expanded.

      Dummy := Children (Model, Parent_Iter);
      Free_And_Remove (Model, Dummy);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Called_By_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      View        : constant Callgraph_View_Access :=
                      Generic_View.Retrieve_View (Self.Kernel);
      Model       : Gtk_Tree_Store;
      Parent_Iter : Gtk_Tree_Iter;
      Dummy       : Gtk_Tree_Iter;
   begin
      if View /= null then
         --  Stop displaying the activity progress bar when an error is
         --  received.
         View.Set_Activity_Progress_Bar_Visibility (False);

         --  It's possible that the target row no longer exists by the time the
         --  request completes, for instance if the user has manually removed
         --  the parent row.
         if not Self.Where.Valid then
            return;
         end if;

         --  Find the path that corresponds to the declaration being explored

         if Self.Where = Null_Gtk_Tree_Row_Reference then
            Parent_Iter := Null_Iter;
         else
            Model := -Get_Model (View.Tree);
            Parent_Iter := Model.Get_Iter (Self.Where.Get_Path);
         end if;

         --  Remove the first child iter, which was the "computing..." iter.
         Dummy := Children (Model, Parent_Iter);
         Free_And_Remove (Model, Dummy);
      end if;
   end On_Error_Message;

   -------------------
   -- Insert_Entity --
   -------------------

   function Insert_Entity
     (View                : access Callgraph_View_Record'Class;
      Decl                : Decl_Record;
      Ref                 : Reference_Record;
      Suffix              : String := "";
      Kind                : View_Type;
      Parent_Iter         : Gtk_Tree_Iter := Null_Iter) return Gtk_Tree_Iter
   is
      Model     : constant Gtk_Tree_Store := -Get_Model (View.Tree);
      Iter      : Gtk_Tree_Iter;
      Locations : Gtk_Tree_Iter := Null_Iter;
   begin
      if Parent_Iter = Null_Iter then
         Iter := Get_Iter_First (Model);
      else
         Iter := Children (Model, Parent_Iter);
      end if;

      --  Check whether the entity already exists in the call graph

      while Iter /= Null_Iter loop
         declare
            Current_Entity : constant Decl_Record := Get_Entity (View, Iter);
         begin
            exit when Current_Entity = Decl
              and then
                Get_Int (Model, Iter, Kind_Column) = View_Type'Pos (Kind);

            Next (Model, Iter);

         end;
      end loop;

      --  If the Iter has no children, attempt to recompute it here.

      if Model.Children (Iter) = Null_Iter then
         Free_And_Remove (Model, Iter);
         Iter := Null_Iter;
      end if;

      if Iter = Null_Iter then
         --  The new node is inserted at the top of the tree

         Prepend (Model, Iter, Parent_Iter);

         declare
            Name : constant String :=
              Escape_Text (To_String (Decl.Name) & Suffix);
            Dcl  : constant String := Decl.File.Display_Base_Name & ':' &
              Image (Integer (Decl.Line))
              & ':' & Image (Integer (Decl.Column));
         begin
            Set_And_Clear
              (Model, Iter,
               (Name_Column, Decl_Column, Entity_Name_Column, File_Column,
                Line_Column, Column_Column, Project_Column, Kind_Column,
                Sort_Column),
               (1 => As_String (Name),
                2 => As_String (Dcl),
                3 => As_String (To_String (Decl.Name)),
                4 => As_File   (Decl.File),
                5 => As_Int    (Gint (Decl.Line)),
                6 => As_Int    (Gint (Decl.Column)),
                7 => As_File   (Decl.Project),
                8 => As_Int    (View_Type'Pos (Kind)),
                9 => As_String (Name & " " & Dcl)));
         end;

         --  Append a dummy child, so that the parent can be expanded to
         --  show its called entities.
         Append (Model, Locations, Iter);
         Model.Set (Locations, Name_Column, Computing_Label);
      end if;

      if Ref /= No_Reference_Record then
         declare
            L       : List_Access;
            Value   : GValue;
            Address : System.Address;
            use type System.Address;

         begin
            --  Get current reference list

            Get_Value (Model, Iter, List_Column, Value);
            Address := Get_Address (Value);

            --  Create a new list if neeeded

            if Address = System.Null_Address then
               L := new Reference_List.Vector;
            else
               L := To_Reference_List (Address);
            end if;

            --  Append new values to the list

            L.Append (Ref);

            Set_Address (Value, To_Address (L));

            --  Record back the new list

            Set_Value (Model, Iter, List_Column, Value);
         end;
      end if;

      --  Select the new node

      declare
         Selection : constant Gtk_Tree_Selection := Get_Selection (View.Tree);
         Path      : Gtk_Tree_Path;
      begin
         if Parent_Iter = Null_Iter then
            Path := Get_Path (Model, Iter);
         else
            Path := Get_Path (Model, Parent_Iter);
         end if;
         Select_Path (Selection, Path);

         --  Make the new node visible

         Scroll_To_Cell (View.Tree, Path, null, True, 0.5, 0.0);
         Path_Free (Path);
      end;

      return Iter;
   end Insert_Entity;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Data : in out Ancestors_User_Data; Cancelled : Boolean)
   is
      Model : Gtk_Tree_Store;
      Path  : Gtk_Tree_Path;
      Iter  : Gtk_Tree_Iter;
   begin
      if not Cancelled
        and then Valid (Data.Computing_Ref)
      then
         Path := Get_Path (Data.Computing_Ref);
         Model := -Get_Model (Data.View.Tree);
         Iter := Get_Iter (Model, Path);
         Free_And_Remove (Model, Iter);
         Path_Free (Path);
      end if;

      if Data.Computing_Ref /= Null_Gtk_Tree_Row_Reference then
         Free (Data.Computing_Ref);
      end if;

      if Data.Entity_Ref /= Null_Gtk_Tree_Row_Reference then
         Free (Data.Entity_Ref);
      end if;
   end Destroy;

   ---------------------
   -- On_Entity_Found --
   ---------------------

   overriding function On_Entity_Found
     (Data                : access Ancestors_User_Data;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean
   is
      Iter : Gtk_Tree_Iter;
      pragma Unreferenced (Is_Renaming, Iter);

      Entity_Iter : Gtk_Tree_Iter;
      Path        : Gtk_Tree_Path;

   begin
      if not Valid (Data.Entity_Ref) then
         return False;
      end if;

      Path := Get_Path (Data.Entity_Ref);
      Entity_Iter := Get_Iter (Get_Model (Data.Entity_Ref), Path);

      case Get_View_Type (Get_Model (Data.View.Tree), Entity_Iter) is
         when View_Called_By =>
            Iter := Insert_Entity
              (View                => Data.View,
               Decl                => Entity_To_Decl (Parent),
               Ref                 => To_Record (Ref, Through_Dispatching),
               Kind                => View_Called_By,
               Suffix              => "",
               Parent_Iter         => Entity_Iter);

         when View_Calls =>
            Iter := Insert_Entity
              (View                => Data.View,
               Decl                => Entity_To_Decl (Entity),
               Ref                 => To_Record (Ref, Through_Dispatching),
               Kind                => View_Calls,
               Suffix              => "",
               Parent_Iter         => Entity_Iter);
      end case;

      Path_Free (Path);

      return True;
   end On_Entity_Found;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Calltree_Next_Or_Previous_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      View   : constant Callgraph_View_Access :=
        Generic_View.Get_Or_Create_View (Kernel);
      Iter   : Gtk_Tree_Iter;
      Model  : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (View.Locations_Tree), Model, Iter);
      if Iter /= Null_Iter then
         if Command.Next then
            Next (Model, Iter);
         else
            Previous (Model, Iter);
         end if;

         if Iter /= Null_Iter then
            View.Locations_Tree.Get_Selection.Select_Iter (Iter);
            Select_Current_Location (View);
            return Commands.Success;
         else
            View.Locations_Tree.Get_Selection.Unselect_All;
         end if;
      end if;

      Get_Selected (View, Model, Iter);
      if Iter /= Null_Iter then
         Move_Row (View.Tree, Iter, Forward => Command.Next);
      end if;

      if Iter /= Null_Iter then
         View.Tree.Get_Selection.Select_Iter (Iter);

         if not Command.Next then
            View.Locations_Tree.Get_Selection.Select_Iter
              (Nth_Child
                 (View.Locations_Model, Null_Iter,
                  N_Children (View.Locations_Model) - 1));
         end if;

         Select_Current_Location (View);

      else
         View.Tree.Get_Selection.Unselect_All;
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Entity_Calls_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Entity : constant Root_Entity'Class :=
        Get_Entity (Context.Context);
      pragma Unreferenced (Command);

      View   : Callgraph_View_Access;
   begin
      if Entity /= No_Root_Entity
        and then Is_Subprogram (Entity)
      then
         View := Generic_View.Get_Or_Create_View (Kernel);
         Expand_Row
           (View.Tree,
            Insert_Entity
              (View, Entity_To_Decl (Entity),
               No_Reference_Record, -" calls ",
              Kind                => View_Calls));
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Entity_Called_By_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle     := Get_Kernel (Context.Context);
      pragma Unreferenced (Command);

      View   : Callgraph_View_Access;
      Decl   : Decl_Record;

   begin
      if not Has_File_Information (Context.Context) then
         return Commands.Failure;
      end if;

      View := Generic_View.Get_Or_Create_View (Kernel);
      Decl := Context_To_Decl (Context.Context);

      if Decl /= No_Decl then
         Expand_Row
           (View.Tree,
            Insert_Entity
              (View, Decl,
               No_Reference_Record, -" is called by ",
               Kind => View_Called_By));
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self   : On_Pref_Changed;
       Kernel : not null access Kernel_Handle_Record'Class;
       Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Self);
      View  : constant Callgraph_View_Access :=
        Generic_View.Retrieve_View (Kernel);
   begin
      if View /= null then
         Set_Font_And_Colors (View.Tree, Fixed_Font => True, Pref => Pref);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Column_Types :=
        (Name_Column        => GType_String,
         Decl_Column        => GType_String,
         Entity_Name_Column => GType_String,
         File_Column        => Get_Virtual_File_Type,
         Line_Column        => GType_Int,
         Column_Column      => GType_Int,
         List_Column        => GType_Pointer,
         Project_Column     => Get_Virtual_File_Type,
         Kind_Column        => GType_Int,
         Sort_Column        => GType_String);

      Generic_View.Register_Module (Kernel);

      Create_New_Boolean_Key_If_Necessary
        (Get_History (Kernel).all, History_Show_Locations, True);

      Register_Contextual_Submenu
        (Kernel,
         Name       => "Call Trees",
         Group      => Navigation_Contextual_Group,
         Ref_Item   => "find all references",
         Add_Before => False);
      Register_Action
        (Kernel, "Entity called by",
         Command     => new Entity_Called_By_Command,
         Description =>
           "Display the call graph view to show what entities are calling"
         & " the selected entity",
         Category  => -"Call trees");
      Register_Contextual_Menu
        (Kernel     => Kernel,
         Label      => -"Call Trees/%s is called by",
         Action     => "Entity called by",
         Group      => Navigation_Contextual_Group);

      if not GPS.LSP_Module.LSP_Ada_Support_Trace_Is_Active then
         Register_Action
           (Kernel, "Entity calls",
            Command     => new Entity_Calls_Command,
            Description =>
              "Display the call graph view to show what entities are called by"
            & " the selected entity",
            Category    => -"Call trees");
         Register_Contextual_Menu
           (Kernel     => Kernel,
            Label      => -"Call Trees/%s calls",
            Action     => "Entity calls",
            Group      => Navigation_Contextual_Group);
      end if;

      Register_Action
        (Kernel, "calltree clear",
         new Calltree_Clear_Command,
         -"Clear the contents of the call tree",
         Category => -"Call trees",
         Icon_Name => "gps-clear-symbolic");

      Register_Action
        (Kernel, "calltree remove selection",
         new Calltree_Remove_Command,
         -"Remove the selected lines from the calltree",
         Icon_Name => "gps-remove-symbolic",
         Category => -"Call trees");

      Register_Action
        (Kernel, "calltree collapse selected",
         new Calltree_Collapse_Or_Expand_Command (True),
         -"Close the selected nodes in the call tree",
         Icon_Name => "gps-collapse-all-symbolic",
         Category => -"Call trees");

      Register_Action
        (Kernel, "calltree expand selected",
         new Calltree_Collapse_Or_Expand_Command (False),
         -"Expand the selected nodes in the call tree",
         Icon_Name => "gps-expand-all-symbolic",
         Category => -"Call trees");

      Register_Action
        (Kernel, "calltree previous",
         new Calltree_Next_Or_Previous_Command'
           (Interactive_Command with Next => False),
         -"Move to the previous line in the call tree",
         Icon_Name => "gps-backward-symbolic",
         Category => -"Call trees");

      Register_Action
        (Kernel, "calltree next",
         new Calltree_Next_Or_Previous_Command'
           (Interactive_Command with Next => True),
         -"Move to the next line in the call tree",
         Icon_Name => "gps-forward-symbolic",
         Category => -"Call trees");
   end Register_Module;

end Call_Graph_Views;
