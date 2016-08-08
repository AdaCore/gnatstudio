------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation; use Ada;
with Ada.Containers.Doubly_Linked_Lists;

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;        use GNATCOLL.VFS.GtkAda;

with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gdk.Dnd;                    use Gdk.Dnd;
with Gdk.Drag_Contexts;          use Gdk.Drag_Contexts;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Check_Menu_Item;        use Gtk.Check_Menu_Item;
with Gtk.Dnd;                    use Gtk.Dnd;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Label;                  use Gtk.Label;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Selection_Data;         use Gtk.Selection_Data;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gdk.Types;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Handlers;            use Gtkada.Handlers;

with Commands.Interactive;       use Commands, Commands.Interactive;
with Default_Preferences;        use Default_Preferences;
with Generic_Views;              use Generic_Views;
with GPS.Kernel.Actions;         use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Intl;                   use GPS.Intl;
with Projects;                   use Projects;
with File_Utils;
with GUI_Utils;                  use GUI_Utils;
with Tooltips;                   use Tooltips;
with Project_Explorers_Common;   use Project_Explorers_Common;

package body Project_Explorers_Files is
   Me : constant Trace_Handle := Create ("FILES");

   File_View_Shows_Only_Project : Boolean_Preference;
   Dirs_From_Project            : Boolean_Preference;

   type Append_Directory_Idle_Data;
   type Append_Directory_Idle_Data_Access is access Append_Directory_Idle_Data;
   --  Custom data for the asynchronous fill function

   package File_Append_Directory_Timeout is
      new Glib.Main.Generic_Sources (Append_Directory_Idle_Data_Access);

   package Timeout_Id_List is new Ada.Containers.Doubly_Linked_Lists
     (Glib.Main.G_Source_Id);

   package Virtual_Files_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Virtual_File);

   type Project_Explorer_Files_Record is new Generic_Views.View_Record with
      record
         File_Tree           : Gtk.Tree_View.Gtk_Tree_View;
         File_Model          : Gtk.Tree_Store.Gtk_Tree_Store;
         Expanding           : Boolean := False;

         Scroll_To_Directory : Boolean := False;
         Path                : Gtk.Tree_Model.Gtk_Tree_Path;
         Realize_Cb_Id       : Gtk.Handlers.Handler_Id;

         Fill_Timeout_Ids    : Timeout_Id_List.List;
         --  ??? This is implemented as a list of handlers instead of just one
         --  handler, in case the fill function should call itself recursively
         --  : to be investigated.
      end record;
   overriding procedure Create_Menu
     (View    : not null access Project_Explorer_Files_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   function Initialize
     (Explorer : access Project_Explorer_Files_Record'Class)
      return Gtk_Widget;
   --  Create a new explorer and returns the focus widget

   type Explorer_Child_Record is
      new MDI_Explorer_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Explorer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   package Explorer_Files_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Files_View",
      View_Name          => -"Files",
      Formal_View_Record => Project_Explorer_Files_Record,
      Formal_MDI_Child   => Explorer_Child_Record,
      Reuse_If_Exist     => True,
      Initialize         => Initialize,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Position           => Position_Left);
   use Explorer_Files_Views;
   subtype Project_Explorer_Files is Explorer_Files_Views.View_Access;

   type Append_Directory_Idle_Data is record
      Explorer      : Project_Explorer_Files;
      Dir           : Virtual_File;
      Norm_Dest     : Virtual_File;
      Depth         : Integer := 0;
      Base          : Gtk_Tree_Iter;
      Files         : File_Array_Access := null;
      File_Index    : Natural := 0;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True;

      This_Timeout_ID : G_Source_Id := No_Source_Id;
   end record;

   procedure Free is new Unchecked_Deallocation
     (Append_Directory_Idle_Data, Append_Directory_Idle_Data_Access);

   procedure Set_Column_Types (Tree : Gtk_Tree_View);
   --  Sets the types of columns to be displayed in the tree_view

   procedure File_Append_Directory
     (Explorer      : access Project_Explorer_Files_Record'Class;
      Dir           : Virtual_File;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : Virtual_File  := No_File;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True);
   --  Add to the file view the directory Dir, at node given by Iter.
   --  If Append_To_Dir is not No_File, and is a sub-directory of Dir, then
   --  the path is expanded recursively all the way to Append_To_Dir.

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path);
   --  Called every time a node is expanded in the file view.
   --  It is responsible for automatically adding the children of the current
   --  node if they are not there already.

   function Expose_Event_Cb
     (Explorer : access Glib.Object.GObject_Record'Class;
      Values   : GValues) return Boolean;
   --  Scroll the explorer to the current directory

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path);
   --  Called every time a node is collapsed in the file view

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "destroy" event on the file view

   procedure File_Remove_Idle_Calls
     (Explorer : access Project_Explorer_Files_Record'Class);
   --  Remove the idle calls for filling the file view

   function File_Button_Press
     (Explorer : access GObject_Record'Class;
      Event    : Gdk_Event_Button) return Boolean;
   --  Callback for the "button_press" event on the file view

   function File_Key_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
   --  Callback for the "key_press" event on the file view

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class);
   --  Callback for the "button_press" event on the file view

   procedure Free_Children
     (T    : Project_Explorer_Files;
      Iter : Gtk_Tree_Iter);
   --  Free all the children of iter Iter in the file view

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean;
   --  ???
   --  Called by File_Append_Directory.

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when preferences change

   type Refresh_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Refresh_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   procedure Refresh (Files : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Refresh the contents of the explorer

   type File_View_Filter_Record is new Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access File_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   -----------
   -- Hooks --
   -----------

   type On_Deleted is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Deleted;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for the "file_deleted" hook

   type On_File_Saved is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);

   type On_Project_View_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  Callback for the "project_view_changed" hook

   type On_File_Renamed is new File2_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Renamed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File, Renamed   : Virtual_File);
   --  Callback for the "file_renamed" hook

   procedure Remove_File
     (View : Project_Explorer_Files;
      File : GNATCOLL.VFS.Virtual_File);
   --  Remove a file or directory node from the tree

   procedure Add_File
     (View : Project_Explorer_Files;
      File : GNATCOLL.VFS.Virtual_File);
   --  Add a file or directory node in the tree

   procedure Drag_Data_Get
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Get drag&drop data from File View's Tree

   procedure Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle);
   --  Accept drag&drop data in File View's Tree

   --------------
   -- Tooltips --
   --------------

   type Explorer_Tooltips is new Tooltips.Tooltips with record
      Explorer : Project_Explorer_Files;
   end record;
   type Explorer_Tooltips_Access is access all Explorer_Tooltips'Class;
   overriding function Create_Contents
     (Tooltip  : not null access Explorer_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;
   --  See inherited documentatoin

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access File_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Files_Views.Get_Module;
   end Filter_Matches_Primitive;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Explorer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      Context : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);
      Explorer     : constant Project_Explorer_Files :=
        Project_Explorer_Files (GPS_MDI_Child (Self).Get_Actual_Widget);
      Iter      : constant Gtk_Tree_Iter :=
        Find_Iter_For_Event (Explorer.File_Tree, Event);
      Path      : Gtk_Tree_Path;
      File      : Virtual_File;
      Node_Type : Node_Types;
   begin
      if Iter /= Null_Iter then
         if Event /= null then
            Path := Get_Path (Explorer.File_Model, Iter);
            Set_Cursor (Explorer.File_Tree, Path, null, False);
            Path_Free (Path);
         end if;

         Node_Type := Node_Types'Val
           (Integer (Get_Int (Explorer.File_Model, Iter, Node_Type_Column)));

         case Node_Type is
            when Directory_Node | File_Node =>
               File := Get_File (Explorer.File_Model, Iter, File_Column);
               Set_File_Information (Context, (1 => File));

            when Entity_Node =>
               --  ??? No entity information was set before, but isn't this
               --  strange ?
               null;

            when others =>
               null;

         end case;
      end if;
      return Context;
   end Build_Context;

   -------------------
   -- Drag_Data_Get --
   -------------------

   procedure Drag_Data_Get
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      pragma Unreferenced (Kernel);

      Tree  : constant Gtk_Tree_View := Gtk_Tree_View (Object);
      Model : Gtk_Tree_Model;
      M     : constant Gtk_Tree_Store := -Tree.Get_Model;
      Iter  : Gtk_Tree_Iter;
      Kind  : Node_Types;
      File  : Virtual_File;
      Data  : constant Gtk.Selection_Data.Gtk_Selection_Data :=
        From_Object (Get_Address (Nth (Args, 2)));
   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Kind := Node_Types'Val (Get_Int (M, Iter, Node_Type_Column));

      case Kind is

         when File_Node | Directory_Node =>
            File := Get_File (M, Iter, File_Column);

         when others =>
            return;
      end case;

      Gtk.Selection_Data.Selection_Data_Set
        (Data, Gtk.Selection_Data.Get_Target (Data), 8,
         "file:///" & File.Display_Full_Name);
   end Drag_Data_Get;

   ------------------------
   -- Drag_Data_Received --
   ------------------------

   procedure Drag_Data_Received
     (Object : access Glib.Object.GObject_Record'Class;
      Args   : Glib.Values.GValues;
      Kernel : GPS.Kernel.Kernel_Handle)
   is
      Tree    : constant Gtk_Tree_View := Gtk_Tree_View (Object);
      Model   : constant Gtk_Tree_Store := -Tree.Get_Model;
      Context : constant Drag_Context :=
                  Drag_Context (Get_Object (Nth (Args, 1)));
      X       : constant Gint := Get_Int (Nth (Args, 2));
      Y       : constant Gint := Get_Int (Nth (Args, 3));
      Data    : constant Gtk_Selection_Data :=
                  From_Object (Get_Address (Nth (Args, 4)));
      Time    : constant Guint32 := Guint32 (Get_Uint (Nth (Args, 6)));
      Action  : constant Drag_Action := Get_Actions (Context);
      Iter    : Gtk_Tree_Iter;
      Success : Boolean;
   begin
      declare
         Path      : Gtk_Tree_Path;
         Buffer_X  : Gint;
         Buffer_Y  : Gint;
         Column    : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      begin
         Get_Path_At_Pos
           (Tree, X, Y,
            Path,
            Column,
            Buffer_X,
            Buffer_Y,
            Success);

         if not Success or Path = Null_Gtk_Tree_Path then
            Iter := Null_Iter;
         else
            Iter := Get_Iter (Model, Path);
            Path_Free (Path);
         end if;
      end;

      if Get_Source_Widget (Context) /= Object then
         --  Forward requests from other applications/widgets to common handler
         GPS.Kernel.Modules.UI.Drag_Data_Received (Object, Args, Kernel);
      elsif Iter /= Null_Iter
        and then Get_Length (Data) >= 0
        and then Get_Format (Data) = 8
        and then (Action = Action_Copy or Action = Action_Move)
      then
         declare
            Source  : Virtual_File;
            Target  : Virtual_File;
            Node    : constant Virtual_File
              := Get_File (Model, Iter, File_Column);
            Dir     : constant Virtual_File := Node.Dir;
            Sources : constant File_Array_Access
              := File_Utils.URL_List_To_Files (Get_Data_As_String (Data));
         begin
            if Sources = null then
               Success := False;
            else
               --  Muti-selection not supported by Files View, so
               --  process only first file
               Source := Sources (Sources'First);
               Target := Dir.Create_From_Dir (Source.Base_Name);

               if Source = Target then
                  Success := False;
               elsif Action = Action_Move then
                  Source.Rename (Target, Success);

                  if Success then
                     File_Renamed_Hook.Run (Kernel, Source, Target);
                  end if;
               else
                  Source.Copy (Target.Full_Name, Success);

                  if Success then
                     File_Saved_Hook.Run (Kernel, Target);
                  end if;
               end if;
            end if;

            Gtk.Dnd.Finish
              (Context,
               Success => Success,
               Del     => Success and (Action = Action_Move),
               Time    => Time);

            if Success then
               Reload_Project_If_Needed (Kernel);
               Recompute_View (Kernel);
            end if;
         end;
      else
         Gtk.Dnd.Finish
           (Context, Success => False, Del => False, Time => Time);
      end if;
   end Drag_Data_Received;

   --------------------
   -- Read_Directory --
   --------------------

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean
   is
      Path_Found : Boolean := False;
      Iter       : Gtk_Tree_Iter;
      Empty      : Boolean := True;
      New_D      : Append_Directory_Idle_Data_Access;

      Values  : Glib.Values.GValue_Array (1 .. 4);
      Columns : constant Columns_Array (Values'Range) :=
        (File_Column, Display_Name_Column, Node_Type_Column, Icon_Column);

      procedure Clear_Timeout_Id;
      --  Clear the timeout_id associated with this idle callback, if any.
      --  This should be called whenever we are returning False from this
      --  function.

      procedure Clear_Timeout_Id is
         use Timeout_Id_List;
         C : Cursor;
      begin
         if D.This_Timeout_ID = No_Source_Id then
            --  This can happen when we are calling this synchronously rather
            --  than from a timeout/idle callback
            return;
         end if;
         C := D.Explorer.Fill_Timeout_Ids.Find (D.This_Timeout_ID);
         if C /= No_Element then
            D.Explorer.Fill_Timeout_Ids.Delete (C);
         end if;
      end Clear_Timeout_Id;

   begin
      if D = null then
         --  Cannot happen right now, but do this for safety
         return False;
      end if;

      --  If we are appending at the base, create a node indicating the
      --  absolute path to the directory.

      if D.Base = Null_Iter then
         Append (D.Explorer.File_Model, Iter, D.Base);

         Values (1 .. 3) :=
           (1 => As_File   (D.Dir),
            2 => As_String (D.Dir.Display_Base_Dir_Name),
            3 => As_Int    (Gint (Node_Types'Pos (Directory_Node))));

         if D.Physical_Read then
            Glib.Values.Init_Set_String
              (Values (4), Stock_For_Node (Directory_Node, Expanded => True));
            Set_And_Clear (D.Explorer.File_Model, Iter, Columns, Values);

            D.Base := Iter;

         else
            Append_Dummy_Iter (D.Explorer.File_Model, Iter);

            Glib.Values.Init_Set_String
              (Values (4), Stock_For_Node (Directory_Node, Expanded => False));
            Set_And_Clear (D.Explorer.File_Model, Iter, Columns, Values);

            Clear_Timeout_Id;

            New_D := D;
            Free (New_D);

            return False;
         end if;
      end if;

      if D.Files = null then
         D.Files := D.Dir.Read_Dir;
         D.File_Index := D.Files'First;
         Sort (D.Files.all);
      end if;

      if D.Depth >= 0
        and then D.File_Index <= D.Files'Last
      then
         if D.Files (D.File_Index) = No_File then
            null;

         elsif File_View_Shows_Only_Project.Get_Pref then
            if Is_Directory (D.Files (D.File_Index)) then
               if not Get_Registry (D.Explorer.Kernel).Tree.
                 Directory_Belongs_To_Project
                 (D.Files (D.File_Index).Full_Name,
                  Direct_Only => False)
               then
                  --  Remove from the list
                  D.Files (D.File_Index) := No_File;
               end if;

            else
               declare
                  File : constant Virtual_File :=
                    Get_Registry (D.Explorer.Kernel).Tree.Create
                    (Name => D.Files (D.File_Index).Base_Dir_Name);

                  --  First matching project, since we have nothing else to
                  --  base our guess on
                  F_Info : constant File_Info'Class :=
                    File_Info'Class
                      (Get_Registry (D.Explorer.Kernel).Tree
                       .Info_Set (File).First_Element);
                  P      : constant Project_Type    := F_Info.Project;
               begin
                  --  If not part of a project, then we remove the file
                  if P = No_Project
                    or else File /= D.Files (D.File_Index)
                  then
                     D.Files (D.File_Index) := No_File;
                  end if;
               end;
            end if;

         elsif Dirs_From_Project.Get_Pref then
            if Is_Directory (D.Files (D.File_Index)) then
               if not Get_Registry (D.Explorer.Kernel).Tree.
                 Directory_Belongs_To_Project
                 (D.Files (D.File_Index).Full_Name,
                  Direct_Only => False)
               then
                  --  Remove from the list
                  D.Files (D.File_Index) := No_File;
               end if;

            else
               declare
                  Dir : constant Virtual_File := D.Files (D.File_Index).Dir;
               begin
                  if not Get_Registry (D.Explorer.Kernel).Tree.
                    Directory_Belongs_To_Project
                       (Dir.Full_Name, Direct_Only => True)
                  then
                     --  Remove from the list
                     D.Files (D.File_Index) := No_File;
                  end if;
               end;
            end if;
         end if;

         if D.Depth = 0 then
            D.Depth := -1;
         end if;

         D.File_Index := D.File_Index + 1;

         --  give the hand to gtk main until next file is analysed
         return True;
      end if;

      for J in D.Files'Range loop
         if D.Files (J) /= No_File then
            Empty := False;
            exit;
         end if;
      end loop;

      if Empty then
         Set (D.Explorer.File_Model, D.Base, Icon_Column,
              Stock_For_Node (Directory_Node, Expanded => False));
      end if;

      for J in D.Files'Range loop
         if D.Files (J) /= No_File
           and then D.Files (J).Is_Directory
         then
            Append (D.Explorer.File_Model, Iter, D.Base);

            Values (1 .. 3) :=
              (1 => As_File   (D.Files (J)),
               2 => As_String (D.Files (J).Display_Base_Dir_Name),
               3 => As_Int    (Gint (Node_Types'Pos (Directory_Node))));

            if D.Depth = 0 then
               Set_And_Clear
                 (D.Explorer.File_Model, Iter,
                  Columns (1 .. 3), Values (1 .. 3));
               exit;
            end if;

            --  Are we on the path to the target directory ?

            if not Path_Found
              and then Is_Parent (D.Files (J), D.Norm_Dest)
            then
               Path_Found := True;

               declare
                  Ignore    : Boolean;
                  pragma Unreferenced (Ignore);

                  Path      : Gtk_Tree_Path;
                  Expanding : constant Boolean := D.Explorer.Expanding;
               begin
                  Path := Get_Path (D.Explorer.File_Model, D.Base);

                  D.Explorer.Expanding := True;
                  Ignore := Expand_Row (D.Explorer.File_Tree, Path, False);
                  D.Explorer.Expanding := Expanding;

                  D.Explorer.File_Model.Set
                    (D.Base, Icon_Column,
                     Stock_For_Node (Directory_Node, Expanded => True));

                  Path_Free (Path);
               end;

               --  Are we on the target directory ?

               if D.Norm_Dest = D.Files (J) then
                  declare
                     Ignore    : Boolean;
                     pragma Unreferenced (Ignore);

                     Expanding : constant Boolean := D.Explorer.Expanding;
                  begin
                     D.Explorer.Path := Get_Path (D.Explorer.File_Model, Iter);

                     File_Append_Directory
                       (D.Explorer, D.Files (J),
                        Iter, D.Depth, D.Norm_Dest, False);

                     D.Explorer.Expanding := True;
                     Ignore := Expand_Row
                       (D.Explorer.File_Tree,
                        D.Explorer.Path, False);
                     D.Explorer.Expanding := Expanding;

                     Select_Path
                       (Get_Selection (D.Explorer.File_Tree),
                        D.Explorer.Path);

                     Glib.Values.Init_Set_String
                       (Values (4), Stock_For_Node (Directory_Node, True));
                     Set_And_Clear
                       (D.Explorer.File_Model, Iter, Columns, Values);

                     D.Explorer.Scroll_To_Directory := True;
                     D.Explorer.Realize_Cb_Id :=
                       Gtkada.Handlers.Object_Return_Callback.Object_Connect
                         (D.Explorer.File_Tree, Signal_Draw,
                          Expose_Event_Cb'Access, D.Explorer, True);
                  end;

               else
                  Set_And_Clear
                    (D.Explorer.File_Model, Iter,
                     Columns (1 .. 3), Values (1 .. 3));

                  File_Append_Directory
                    (D.Explorer, D.Files (J),
                     Iter, D.Depth, D.Norm_Dest, D.Idle);
               end if;

            else
               Append_Dummy_Iter (D.Explorer.File_Model, Iter);

               Glib.Values.Init_Set_String
                 (Values (4), Stock_For_Node (Directory_Node, False));
               Set_And_Clear
                 (D.Explorer.File_Model, Iter, Columns, Values);
            end if;
         end if;
      end loop;

      for J in D.Files'Range loop
         if D.Files (J) /= No_File
           and then D.Files (J).Is_Regular_File
         then
            Append_File
              (D.Explorer.Kernel,
               D.Explorer.File_Model,
               D.Base,
               D.Files (J));
         end if;
      end loop;

      D.Norm_Dest := No_File;
      Unchecked_Free (D.Files);

      Clear_Timeout_Id;

      New_D := D;
      Free (New_D);

      return False;

   exception
      when VFS_Directory_Error =>
         --  The directory couldn't be open, probably because of permissions

         Clear_Timeout_Id;

         New_D := D;
         Free (New_D);

         return False;

      when E : others =>
         Trace (Me, E);

         Clear_Timeout_Id;
         return False;
   end Read_Directory;

   ---------------------------
   -- File_Append_Directory --
   ---------------------------

   procedure File_Append_Directory
     (Explorer      : access Project_Explorer_Files_Record'Class;
      Dir           : Virtual_File;
      Base          : Gtk_Tree_Iter;
      Depth         : Integer := 0;
      Append_To_Dir : Virtual_File := No_File;
      Idle          : Boolean := False;
      Physical_Read : Boolean := True)
   is
      D          : constant Append_Directory_Idle_Data_Access :=
                     new Append_Directory_Idle_Data;
      --  D is freed when Read_Directory ends (i.e. returns False)

   begin
      D.Dir           := Dir;
      Ensure_Directory (D.Dir);
      D.Norm_Dest     := Append_To_Dir;
      D.Depth         := Depth;
      D.Base          := Base;
      D.Explorer      := Project_Explorer_Files (Explorer);
      D.Idle          := Idle;
      D.Physical_Read := Physical_Read;

      if Idle then
         --  Do not append the first item in an idle loop.
         --  Necessary for preserving order in drive names.

         if Read_Directory (D) then
            D.This_Timeout_ID :=
              File_Append_Directory_Timeout.Timeout_Add
                (1, Read_Directory'Access, D);
            Timeout_Id_List.Append
              (Explorer.Fill_Timeout_Ids,
               D.This_Timeout_ID);
         end if;

      else
         loop
            exit when not Read_Directory (D);
         end loop;
      end if;
   end File_Append_Directory;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types (Tree : Gtk_Tree_View) is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);

      Set_Rules_Hint (Tree, False);

      Gtk_New (Col);
      Pack_Start (Col, Pixbuf_Rend, False);
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Pixbuf_Rend, "icon-name", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Display_Name_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Explorer : access Project_Explorer_Files_Record'Class)
      return Gtk_Widget
   is
      Tooltip      : Explorer_Tooltips_Access;
      Scrolled     : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (Explorer, Homogeneous => False);

      Gtk_New (Scrolled);
      Explorer.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      Gtk_New (Explorer.File_Model, Columns_Types);
      Gtk_New (Explorer.File_Tree, Explorer.File_Model);
      Set_Name (Explorer.File_Tree, "File Explorer Tree");

      --  The model should be destroyed as soon as the tree view is destroyed
      Unref (Explorer.File_Model);

      Scrolled.Add (Explorer.File_Tree);

      Set_Headers_Visible (Explorer.File_Tree, False);

      Explorer.File_Tree.On_Button_Press_Event
        (File_Button_Press'Access, Explorer);
      Explorer.File_Tree.On_Button_Release_Event
        (File_Button_Press'Access, Explorer);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.File_Tree,
         Signal_Key_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (File_Key_Press'Access),
         Slot_Object => Explorer,
         After       => False);

      Widget_Callback.Object_Connect
        (Get_Selection (Explorer.File_Tree),
         Signal_Changed,
         File_Selection_Changed'Access,
         Slot_Object => Explorer,
         After       => True);

      Set_Column_Types (Explorer.File_Tree);

      Setup_Contextual_Menu
        (Kernel          => Explorer.Kernel,
         Event_On_Widget => Explorer.File_Tree);

      Refresh (Explorer);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree,
         Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (File_Tree_Expand_Row_Cb'Access),
         Explorer,
         False);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree,
         Signal_Row_Collapsed,
         Widget_Callback.To_Marshaller (File_Tree_Collapse_Row_Cb'Access),
         Explorer,
         False);

      Widget_Callback.Object_Connect
        (Explorer.File_Tree, Signal_Destroy,
         On_File_Destroy'Access, Explorer, False);

      Gtk.Dnd.Dest_Set
        (Explorer.File_Tree, Dest_Default_All, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.File_Tree, Signal_Drag_Data_Received,
         Drag_Data_Received'Access, Explorer.Kernel);
      Explorer.File_Tree.Enable_Model_Drag_Source
        (Gdk.Types.Button1_Mask, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.File_Tree, Signal_Drag_Data_Get,
         Drag_Data_Get'Access, Explorer.Kernel);

      File_Deleted_Hook.Add (new On_Deleted, Watch => Explorer);
      File_Saved_Hook.Add (new On_File_Saved, Watch => Explorer);
      File_Renamed_Hook.Add (new On_File_Renamed, Watch => Explorer);
      Project_View_Changed_Hook.Add
         (new On_Project_View_Changed, Watch => Explorer);

      --  Initialize tooltips

      Tooltip := new Explorer_Tooltips;
      Tooltip.Explorer := Project_Explorer_Files (Explorer);
      Tooltip.Set_Tooltip (Explorer.File_Tree);

      return Gtk_Widget (Explorer.File_Tree);
   end Initialize;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Project_Explorer_Files_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
   begin
      Append_Menu (Menu, View.Kernel, File_View_Shows_Only_Project);
      Append_Menu (Menu, View.Kernel, Dirs_From_Project);
   end Create_Menu;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls
     (Explorer : access Project_Explorer_Files_Record'Class)
   is
      use Timeout_Id_List;
   begin
      for Id of Explorer.Fill_Timeout_Ids loop
         Glib.Main.Remove (Id);
      end loop;

      Explorer.Fill_Timeout_Ids.Clear;
   end File_Remove_Idle_Calls;

   ---------------------
   -- On_File_Destroy --
   ---------------------

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params   : Glib.Values.GValues)
   is
      pragma Unreferenced (Params);
      E : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
   begin
      File_Remove_Idle_Calls (E);
   end On_File_Destroy;

   -------------------------------
   -- File_Tree_Collapse_Row_Cb --
   -------------------------------

   procedure File_Tree_Collapse_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);

      T    : constant Project_Explorer_Files :=
               Project_Explorer_Files (Explorer);
      File : constant Virtual_File :=
               Get_File (T.File_Model, Iter, File_Column);

   begin
      if File.Is_Directory then
         T.File_Model.Set
           (Iter, Icon_Column, Stock_For_Node (Directory_Node, False));
      end if;

   exception
      when E : others => Trace (Me, E);
   end File_Tree_Collapse_Row_Cb;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (Explorer : access Glib.Object.GObject_Record'Class;
      Values   : GValues) return Boolean
   is
      pragma Unreferenced (Values);
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);

   begin
      if T.Scroll_To_Directory then
         Scroll_To_Cell
           (T.File_Tree,
            T.Path, null, True,
            0.1, 0.1);
         Disconnect (T.File_Tree, T.Realize_Cb_Id);
         T.Scroll_To_Directory := False;
      end if;

      return True;
   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Expose_Event_Cb;

   -----------------------------
   -- File_Tree_Expand_Row_Cb --
   -----------------------------

   procedure File_Tree_Expand_Row_Cb
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Iter     : Gtk_Tree_Iter;
      Path     : Gtk_Tree_Path)
   is
      T       : constant Project_Explorer_Files :=
                  Project_Explorer_Files (Explorer);
      Ignore  : Boolean;
      pragma Unreferenced (Ignore);

   begin
      if T.Expanding then
         return;
      end if;

      T.Expanding := True;

      declare
         File      : constant Virtual_File :=
           Get_File (T.File_Model, Iter, File_Column);
         N_Type    : constant Node_Types := Node_Types'Val
           (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

      begin
         case N_Type is
            when Directory_Node =>
               Free_Children (T, Iter);
               T.File_Model.Set
                 (Iter, Icon_Column, Stock_For_Node (Directory_Node, True));
               File_Append_Directory (T, File, Iter, 1);

            when File_Node =>
               Free_Children (T, Iter);
               Append_File_Info (T.Kernel, T.File_Model, Iter, File,
                                 Sorted => True);

            when Category_Node | Entity_Node | Dummy_Node
               | Obj_Directory_Node | Exec_Directory_Node
               | Lib_Directory_Node | Project_Node_Types | Runtime_Node =>
               null;
         end case;
      end;

      Ignore := Expand_Row (T.File_Tree, Path, False);
      Scroll_To_Cell (T.File_Tree, Path, null, True, 0.1, 0.1);

      T.Expanding := False;

   exception
      when E : others => Trace (Me, E);
   end File_Tree_Expand_Row_Cb;

   ----------------------------
   -- File_Selection_Changed --
   ----------------------------

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class)
   is
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
      Child : constant GPS_MDI_Child :=
        Explorer_Files_Views.Child_From_View (T);
   begin
      T.Kernel.Context_Changed (Child.Build_Context);
   end File_Selection_Changed;

   -----------------------
   -- File_Button_Press --
   -----------------------

   function File_Button_Press
     (Explorer : access GObject_Record'Class;
      Event    : Gdk_Event_Button) return Boolean
   is
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
   begin
      return On_Button_Press
        (T.Kernel,
         MDI_Explorer_Child (Explorer_Files_Views.Child_From_View (T)),
         T.File_Tree, T.File_Model, Event, Add_Dummy => True);
   end File_Button_Press;

   --------------------
   -- File_Key_Press --
   --------------------

   function File_Key_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
   begin
      return On_Key_Press (T.Kernel, T.File_Tree, Event);
   end File_Key_Press;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Refresh_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      V : constant Project_Explorer_Files :=
        Explorer_Files_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if V /= null then
         Refresh (V);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Files : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Explorer     : constant Project_Explorer_Files :=
                       Project_Explorer_Files (Files);
      Drives       : File_Array_Access;
      Cur_Dir      : constant Virtual_File := Get_Current_Dir;
      Dir_Inserted : Boolean := False;

      procedure Add_Drives_From_Files (Files : File_Array);
      --  Add all the logical drives used by Files in the files explorer.

      procedure Add_Drives_From_Files (Files : File_Array)
      is
         Added_Drives : Virtual_Files_Lists.List;
      begin
         for File of Files loop
            if not Added_Drives.Contains (File.Get_Root) then
               File_Append_Directory
                 (Explorer      => Explorer,
                  Dir           => File.Get_Root,
                  Base          => Null_Iter,
                  Depth         => 1,
                  Append_To_Dir => Get_Current_Dir,
                  Idle          => True);

               Added_Drives.Append (File.Get_Root);
            end if;
         end loop;
      end Add_Drives_From_Files;

   begin
      Clear (Explorer.File_Model);
      File_Remove_Idle_Calls (Explorer);

      if File_View_Shows_Only_Project.Get_Pref
         or else Dirs_From_Project.Get_Pref
      then
         declare
            Inc         : constant File_Array :=
                    Source_Dirs (Get_Project (Explorer.Kernel), True);
            Obj         : constant File_Array :=
                    Object_Path (Get_Project (Explorer.Kernel), True, False);
            Common_Path : constant Virtual_File :=
                            Greatest_Common_Path (Inc & Obj);
         begin
            if Common_Path /= No_File then
               File_Append_Directory
                 (Explorer      => Explorer,
                  Dir           => Common_Path,
                  Base          => Null_Iter,
                  Depth         => 1,
                  Append_To_Dir => Get_Current_Dir,
                  Idle          => True);
            else
               Add_Drives_From_Files (Files => Inc & Obj);
            end if;
         end;

      else
         Drives := GNATCOLL.VFS.Get_Logical_Drives;

         if Drives /= null then
            for J in Drives'Range loop
               if Drives (J).Is_Parent (Cur_Dir) then
                  File_Append_Directory
                    (Explorer, Drives (J),
                     Null_Iter, 1, Cur_Dir, True);
                  Dir_Inserted := True;

               else
                  File_Append_Directory
                    (Explorer, Drives (J),
                     Null_Iter, 0, No_File, False, False);
               end if;
            end loop;
         end if;

         if not Dir_Inserted then
            File_Append_Directory
              (Explorer, Cur_Dir.Get_Root,
               Null_Iter, 1, Cur_Dir, True);
         end if;
      end if;

   exception
      when E : others => Trace (Me, E);
   end Refresh;

   -------------------
   -- Free_Children --
   -------------------

   procedure Free_Children
     (T    : Project_Explorer_Files;
      Iter : Gtk_Tree_Iter) is
   begin
      Remove_Child_Nodes (T.File_Model, Iter);
   end Free_Children;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (View : Project_Explorer_Files;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Next_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path      : Gtk_Tree_Path;
   begin
      Iter := Get_Iter_First (View.File_Model);

      while Iter /= Null_Iter loop
         if Get_File (View.File_Model, Iter, File_Column) = File then
            --  First select the parent and set the 'scroll to dir' state
            Path := Get_Path (View.File_Model, Parent (View.File_Model, Iter));
            Set_Cursor (View.File_Tree, Path, null, False);
            View.Scroll_To_Directory := True;

            --  Now remove the node, this will invoke the expose event, that
            --  will scroll to the parent directory.
            Remove (View.File_Model, Iter);
            exit;
         end if;

         --  We look through the tree: first dir node, then children,
         --  then parent's next item.
         if Has_Child (View.File_Model, Iter) then
            Iter := Children (View.File_Model, Iter);

         else
            loop
               Next_Iter := Iter;
               Next (View.File_Model, Next_Iter);

               if Next_Iter = Null_Iter then
                  Iter := Parent (View.File_Model, Iter);
                  exit when Iter = Null_Iter;
               else
                  Iter := Next_Iter;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
   end Remove_File;

   --------------
   -- Add_File --
   --------------

   procedure Add_File
     (View : Project_Explorer_Files;
      File : GNATCOLL.VFS.Virtual_File)
   is
      Iter      : Gtk.Tree_Model.Gtk_Tree_Iter;
      Next_Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Iter2     : Gtk.Tree_Model.Gtk_Tree_Iter := Null_Iter;
      Dir       : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.Dir (File);
      Path      : Gtk_Tree_Path;
      Done      : Boolean;
      Ignore    : Boolean;
      pragma Unreferenced (Ignore);

   begin
      Iter := Get_Iter_First (View.File_Model);

      if Is_Directory (File) then
         Dir := GNATCOLL.VFS.Get_Parent (File);
      end if;

      while Iter /= Null_Iter loop
         if Get_File (View.File_Model, Iter, File_Column) = Dir then
            --  We found the file's directory

            Path := Get_Path (View.File_Model, Iter);

            if not Row_Expanded (View.File_Tree, Path)
              and then Children (View.File_Model, Iter) /= Null_Iter
            then
               --  File's directory is not expanded. Return now

               --  Note that we need to test if dir node has children: in the
               --  normal case, a non expanded dir always has a dummy child.
               --  When we rename a directory, we might have deleted the only
               --  dir's child, then this dir won't have children at all. We
               --  don't want to fall back in this case here.
               return;
            end if;

            --  file's directory is expanded. Let's look at the children
            Next_Iter := Children (View.File_Model, Iter);

            while Next_Iter /= Null_Iter loop
               if Get_File (View.File_Model, Next_Iter, File_Column) =
                 File
               then
                  --  File already present. Do nothing
                  return;
               end if;

               Next (View.File_Model, Next_Iter);
            end loop;

            --  If we are here, then this means that the saved file is not
            --  present in the view. Let's insert it.

            if Is_Directory (File) then
               Next_Iter := Children (View.File_Model, Iter);
               Done := False;

               while Next_Iter /= Null_Iter loop

                  if Get_Node_Type (View.File_Model, Next_Iter) =
                    Directory_Node
                  then
                     declare
                        Name : constant Filesystem_String :=
                                 Get_Base_Name (View.File_Model, Next_Iter);
                     begin
                        if Name > File.Base_Dir_Name then
                           Insert_Before
                             (View.File_Model, Iter2, Iter, Next_Iter);
                           Done := True;

                           exit;
                        end if;
                     end;

                  elsif Get_Node_Type (View.File_Model, Next_Iter) =
                    File_Node
                  then
                     Insert_Before
                       (View.File_Model, Iter2, Iter, Next_Iter);
                     Done := True;

                     exit;
                  end if;

                  Next (View.File_Model, Next_Iter);
               end loop;

               if not Done then
                  Append (View.File_Model, Iter2, Iter);
               end if;

               Set_And_Clear
                 (View.File_Model, Iter2,
                  (File_Column, Display_Name_Column),
                  (1 => As_File   (File),
                   2 => As_String (File.Display_Base_Dir_Name)));

               Set_Node_Type (View.File_Model, Iter2, Directory_Node, False);
               File_Append_Directory (View, File, Iter2);

            else
               Append_File
                 (View.Kernel,
                  View.File_Model,
                  Iter,
                  File,
                  Sorted => True);
            end if;

            Ignore := Expand_Row (View.File_Tree, Path, False);

            return;
         end if;

         --  We look through the tree: first dir node, then children,
         --  then parent's next item.
         if Has_Child (View.File_Model, Iter) then
            Iter := Children (View.File_Model, Iter);

         else
            loop
               Next_Iter := Iter;
               Next (View.File_Model, Next_Iter);

               if Next_Iter = Null_Iter then
                  Iter := Parent (View.File_Model, Iter);
                  exit when Iter = Null_Iter;
               else
                  Iter := Next_Iter;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
   end Add_File;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Tooltip  : not null access Explorer_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Tree : constant Gtk.Tree_View.Gtk_Tree_View :=
        Tooltip.Explorer.File_Tree;
      Model : constant Gtk_Tree_Store := -Get_Model (Tree);
      Iter : Gtk_Tree_Iter;
      Label : Gtk_Label;
      Area : Gdk_Rectangle;

   begin
      Initialize_Tooltips (Tree, X, Y, Area, Iter);

      if Iter /= Null_Iter then
         Tooltip.Set_Tip_Area (Area);
         declare
            File : constant Virtual_File :=
              Get_File (Model, Iter, File_Column);
            Dir  : constant Virtual_File := Get_Parent (File);
         begin
            Gtk_New (Label,
                     File.Display_Base_Dir_Name & ASCII.LF & "in "
                     & Dir.Display_Full_Name);
         end;
      end if;
      return Gtk_Widget (Label);
   end Create_Contents;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Deleted;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      V : constant Project_Explorer_Files :=
        Explorer_Files_Views.Retrieve_View (Kernel);
   begin
      Remove_File (V, File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      V : constant Project_Explorer_Files :=
        Explorer_Files_Views.Retrieve_View (Kernel);
   begin
      Add_File (V, File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_View_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      V : constant Project_Explorer_Files :=
        Explorer_Files_Views.Retrieve_View (Kernel);
   begin
      Refresh (V);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_File_Renamed;
      Kernel  : not null access Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Renamed : Virtual_File)
   is
      pragma Unreferenced (Self);
      V : constant Project_Explorer_Files :=
        Explorer_Files_Views.Retrieve_View (Kernel);
   begin
      Remove_File (V, File);
      Add_File (V, Renamed);
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
      Explorer : constant Project_Explorer_Files :=
        Explorer_Files_Views.Retrieve_View (Kernel);
   begin
      if Explorer /= null then
         Set_Font_And_Colors
           (Explorer.File_Tree, Fixed_Font => True, Pref => Pref);

         if Pref = null
           or else Pref = Preference (File_View_Shows_Only_Project)
           or else Pref = Preference (Dirs_From_Project)
         then
            Refresh (Explorer);
         end if;
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File_View_Filter : constant Action_Filter :=
                           new File_View_Filter_Record;
   begin
      Explorer_Files_Views.Register_Module (Kernel);
      Register_Filter
        (Kernel,
         Filter => File_View_Filter,
         Name   => "File_View");

      File_View_Shows_Only_Project :=
        Kernel.Get_Preferences.Create_Invisible_Pref
          ("explorers-file-show-project-only", False,
           Label => -"Show files from project only",
           Doc => -("Binary files are not shown"
                & " in object directories, and source directories only"
                & " contain files that match the project naming scheme."));

      Dirs_From_Project :=
        Kernel.Get_Preferences.Create_Invisible_Pref
          ("explorers-file-dirs-from-project", False,
           Label => -"Show all files in any project directory",
           Doc =>
                -("In particular, shows binary files in object directories"
                & " and all files found in any of the source directories."
                & " No effect if 'Show files from project only' is selected"));

      Register_Action
        (Kernel, "refresh files view", new Refresh_Command,
         -"Refrehs the contents of the Files view",
         Icon_Name => "gps-refresh-symbolic");

      Preferences_Changed_Hook.Add (new On_Pref_Changed);
   end Register_Module;

end Project_Explorers_Files;
