------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;        use GNATCOLL.VFS.GtkAda;

with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;
with Gdk.Dnd;                    use Gdk.Dnd;
with Gdk.Event;                  use Gdk.Event;
with Gtk.Check_Menu_Item;        use Gtk.Check_Menu_Item;
with Gtk.Dnd;                    use Gtk.Dnd;
with Gtk.Handlers;               use Gtk.Handlers;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu;                   use Gtk.Menu;
with Gtk.Selection;              use Gtk.Selection;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gdk.Types;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Handlers;            use Gtkada.Handlers;

with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;      use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Intl;                   use GPS.Intl;
with Projects;                   use Projects;
with File_Utils;
with GUI_Utils;                  use GUI_Utils;
with Traces;                     use Traces;
with Histories;                  use Histories;
with Project_Explorers_Common;   use Project_Explorers_Common;
with XML_Utils;                  use XML_Utils;

package body Project_Explorers_Files is
   Explorer_Files_Module_Id     : Module_ID;

   File_View_Shows_Only_Project : constant History_Key :=
                                    "explorers-file-show-project-only";

   type Explorer_Module_Record is new Module_ID_Record with null record;
   overriding procedure Default_Context_Factory
     (Module  : access Explorer_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject);
   --  See inherited documentation

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
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean;
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

   procedure Explorer_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu);
   --  ??? Unused for now while the files explorer is not a separate module.
   --  Return the context to use for the contextual menu.
   --  It is also used to return the context for
   --  GPS.Kernel.Get_Current_Context, and thus can be called with a null
   --  event or a null menu.

   procedure Refresh (Files : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Refresh the contents of the explorer

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Raise the existing explorer, or open a new one

   type File_View_Filter_Record is new Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access File_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   -----------
   -- Hooks --
   -----------

   type Internal_Hook_Record is abstract new Function_With_Args with record
      View : Project_Explorer_Files;
   end record;

   type File_Deleted_Hook_Record is new Internal_Hook_Record with null record;
   type File_Deleted_Hook is access File_Deleted_Hook_Record'Class;

   type File_Saved_Hook_Record is new Internal_Hook_Record with null record;
   type File_Saved_Hook is access File_Saved_Hook_Record'Class;

   type File_Renamed_Hook_Record is new Internal_Hook_Record with null record;
   type File_Renamed_Hook is access File_Renamed_Hook_Record'Class;

   type Project_View_Changed_Hook_Record is new Function_No_Args with record
      View : Project_Explorer_Files;
   end record;
   type Project_View_Changed_Hook
     is access Project_View_Changed_Hook_Record'Class;

   overriding procedure Execute
     (Hook   : File_Deleted_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_deleted" hook

   overriding procedure Execute
     (Hook   : Project_View_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Callback for the "project_view_changed" hook

   overriding procedure Execute
     (Hook   : File_Saved_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Callback for the "file_saved" hook

   overriding procedure Execute
     (Hook   : File_Renamed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access File_View_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      return Module_ID (Get_Creator (Ctxt)) = Explorer_Files_Module_Id;
   end Filter_Matches_Primitive;

   -----------------------------
   -- Default_Context_Factory --
   -----------------------------

   overriding procedure Default_Context_Factory
     (Module  : access Explorer_Module_Record;
      Context : in out Selection_Context;
      Child   : Glib.Object.GObject) is
   begin
      Explorer_Context_Factory
        (Context, Get_Kernel (Module.all),
         Gtk_Widget (Child), Child, null, null);
   end Default_Context_Factory;

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
      Model : Gtk_Tree_Model := Tree.Get_Model;
      Iter  : Gtk_Tree_Iter;
      Kind  : Node_Types;
      File  : Virtual_File;
      Data  : constant Gtk.Selection.Selection_Data :=
        Gtk.Selection.Selection_Data (Get_Proxy (Nth (Args, 2)));
   begin
      Get_Selected (Get_Selection (Tree), Model, Iter);

      if Iter = Null_Iter then
         return;
      end if;

      Kind := Node_Types'Val (Get_Int (Model, Iter, Node_Type_Column));

      case Kind is

         when File_Node | Directory_Node =>
            File := Get_File (Model, Iter, File_Column);

         when others =>
            return;
      end case;

      Gtk.Selection.Selection_Data_Set
        (Data, Gtk.Selection.Get_Target (Data), 8,
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
      Model   : constant Gtk_Tree_Model := Tree.Get_Model;
      Context : constant Drag_Context :=
                  Drag_Context (Get_Proxy (Nth (Args, 1)));
      X       : constant Gint := Get_Int (Nth (Args, 2));
      Y       : constant Gint := Get_Int (Nth (Args, 3));
      Data    : constant Selection_Data :=
                  Selection_Data (Get_Proxy (Nth (Args, 4)));
      Time    : constant Guint32 := Guint32 (Get_Uint (Nth (Args, 6)));
      Action  : constant Drag_Action := Get_Action (Context);
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

         if not Success or Path = null then
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
                     Kernel.File_Renamed (Source, Target);
                  end if;
               else
                  Source.Copy (Target.Full_Name, Success);

                  if Success then
                     Kernel.File_Saved (Target);
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

   begin
      --  If we are appending at the base, create a node indicating the
      --  absolute path to the directory.

      if D.Base = Null_Iter then
         Append (D.Explorer.File_Model, Iter, D.Base);

         Set_File (D.Explorer.File_Model, Iter, File_Column, D.Dir);
         Set (D.Explorer.File_Model, Iter, Display_Name_Column,
              D.Dir.Display_Base_Dir_Name);
         Set (D.Explorer.File_Model, Iter, Node_Type_Column,
              Gint (Node_Types'Pos (Directory_Node)));

         if D.Physical_Read then
            Set (D.Explorer.File_Model, Iter, Icon_Column,
                 GObject (Open_Pixbufs (Directory_Node)));
            D.Base := Iter;

         else
            Append_Dummy_Iter (D.Explorer.File_Model, Iter);
            Set (D.Explorer.File_Model, Iter, Icon_Column,
                 GObject (Close_Pixbufs (Directory_Node)));
            Pop_State (D.Explorer.Kernel);
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
        and then D.Files /= null
        and then D.File_Index <= D.Files'Last
      then
         if D.Files (D.File_Index) = No_File then
            null;

         elsif Get_History
           (Get_History (D.Explorer.Kernel).all,
            File_View_Shows_Only_Project)
         then
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
                  P    : constant Project_Type :=
                    Get_Registry (D.Explorer.Kernel).Tree.Info (File).Project;
               begin
                  --  If not part of a project, then we remove the file
                  if P = No_Project
                    or else File /= D.Files (D.File_Index)
                  then
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

      if D.Idle then
         Pop_State (D.Explorer.Kernel);
         Push_State (D.Explorer.Kernel, Busy);
      end if;

      for J in D.Files'Range loop
         if D.Files (J) /= No_File then
            Empty := False;
            exit;
         end if;
      end loop;

      if Empty then
         Set (D.Explorer.File_Model, D.Base, Icon_Column,
              GObject (Close_Pixbufs (Directory_Node)));
      end if;

      for J in D.Files'Range loop
         if D.Files (J) /= No_File
           and then D.Files (J).Is_Directory
         then
            Append (D.Explorer.File_Model, Iter, D.Base);
            Set_File (D.Explorer.File_Model, Iter, File_Column, D.Files (J));
            Set (D.Explorer.File_Model, Iter, Display_Name_Column,
                 D.Files (J).Display_Base_Dir_Name);
            Set (D.Explorer.File_Model, Iter, Node_Type_Column,
                 Gint (Node_Types'Pos (Directory_Node)));

            if D.Depth = 0 then
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

                  Set (D.Explorer.File_Model, D.Base, Icon_Column,
                       GObject (Open_Pixbufs (Directory_Node)));

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

                     Set (D.Explorer.File_Model, Iter, Icon_Column,
                          GObject (Open_Pixbufs (Directory_Node)));
                     D.Explorer.Scroll_To_Directory := True;
                     D.Explorer.Realize_Cb_Id :=
                       Gtkada.Handlers.Object_Return_Callback.Object_Connect
                         (D.Explorer.File_Tree, Signal_Expose_Event,
                          Expose_Event_Cb'Access, D.Explorer, True);
                  end;

               else
                  File_Append_Directory
                    (D.Explorer, D.Files (J),
                     Iter, D.Depth, D.Norm_Dest, D.Idle);
               end if;

            else
               Append_Dummy_Iter (D.Explorer.File_Model, Iter);

               Set (D.Explorer.File_Model, Iter, Icon_Column,
                    GObject (Close_Pixbufs (Directory_Node)));
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

      Pop_State (D.Explorer.Kernel);

      New_D := D;
      Free (New_D);

      return False;

   exception
      when VFS_Directory_Error =>
         --  The directory couldn't be open, probably because of permissions

         Pop_State (D.Explorer.Kernel);

         New_D := D;
         Free (New_D);
         return False;

      when E : others =>
         Pop_State (D.Explorer.Kernel);
         Trace (Exception_Handle, E);
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

      Timeout_Id : G_Source_Id;

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
         Push_State (Explorer.Kernel, Processing);
      else
         Push_State (Explorer.Kernel, Busy);
      end if;

      if Idle then
         --  Do not append the first item in an idle loop.
         --  Necessary for preserving order in drive names.

         if Read_Directory (D) then
            Timeout_Id :=
              File_Append_Directory_Timeout.Timeout_Add
                (1, Read_Directory'Access, D);
            Timeout_Id_List.Append (Explorer.Fill_Timeout_Ids, Timeout_Id);
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
      Add_Attribute (Col, Pixbuf_Rend, "pixbuf", Icon_Column);
      Add_Attribute (Col, Text_Rend, "text", Display_Name_Column);
      Dummy := Append_Column (Tree, Col);
   end Set_Column_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out Project_Explorer_Files;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Explorer := new Project_Explorer_Files_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Explorer : access Project_Explorer_Files_Record'Class;
      Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Deleted_Hook : File_Deleted_Hook;
      Saved_Hook   : File_Saved_Hook;
      Renamed_Hook : File_Renamed_Hook;
      Project_Hook : Project_View_Changed_Hook;
   begin
      Gtk.Scrolled_Window.Initialize (Explorer);
      Set_Policy (Explorer, Policy_Automatic, Policy_Automatic);

      Gtk_New (Explorer.File_Model, Columns_Types);
      Gtk_New (Explorer.File_Tree, Explorer.File_Model);
      Set_Name (Explorer.File_Tree, "File Explorer Tree");

      --  The model should be destroyed as soon as the tree view is destroyed
      Unref (Explorer.File_Model);

      Explorer.Kernel := Kernel_Handle (Kernel);

      Add (Explorer, Explorer.File_Tree);

      Set_Headers_Visible (Explorer.File_Tree, False);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.File_Tree,
         Signal_Button_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (File_Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.File_Tree,
         Signal_Button_Release_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller
           (File_Button_Press'Access),
         Slot_Object => Explorer,
         After       => False);

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

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Explorer.File_Tree,
         Object          => Explorer,
         ID              => Explorer_Files_Module_Id,
         Context_Func    => Explorer_Context_Factory'Access);

      Init_Graphics (Gtk_Widget (Explorer));

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
         Drag_Data_Received'Access, Kernel_Handle (Kernel));
      Explorer.File_Tree.Enable_Model_Drag_Source
        (Gdk.Types.Button1_Mask, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.File_Tree, Signal_Drag_Data_Get,
         Drag_Data_Get'Access, Kernel_Handle (Kernel));

      Deleted_Hook := new File_Deleted_Hook_Record;
      Deleted_Hook.View := Project_Explorer_Files (Explorer);
      Add_Hook (Kernel, GPS.Kernel.File_Deleted_Hook,
                Deleted_Hook,
                Name  => "project_explorers_files.file_deleted",
                Watch => GObject (Explorer));
      Saved_Hook := new File_Saved_Hook_Record;
      Saved_Hook.View := Project_Explorer_Files (Explorer);
      Add_Hook (Kernel, GPS.Kernel.File_Saved_Hook,
                Saved_Hook,
                Name  => "project_explorers_files.file_saved",
                Watch => GObject (Explorer));
      Renamed_Hook := new File_Renamed_Hook_Record;
      Renamed_Hook.View := Project_Explorer_Files (Explorer);
      Add_Hook (Kernel, GPS.Kernel.File_Renamed_Hook,
                Renamed_Hook,
                Name  => "project_explorers_files.file_renamed",
                Watch => GObject (Explorer));

      Project_Hook := new Project_View_Changed_Hook_Record;
      Project_Hook.View := Project_Explorer_Files (Explorer);
      Add_Hook (Kernel, GPS.Kernel.Project_View_Changed_Hook,
                Project_Hook,
                Name => "project_explorers_files.project_view_changed",
                Watch => GObject (Explorer));
   end Initialize;

   ------------------------------
   -- Explorer_Context_Factory --
   ------------------------------

   procedure Explorer_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget);

      T         : constant Project_Explorer_Files :=
                    Project_Explorer_Files (Object);
      Iter      : constant Gtk_Tree_Iter :=
                    Find_Iter_For_Event (T.File_Tree, T.File_Model, Event);
      Path      : Gtk_Tree_Path;
      File      : Virtual_File;
      Node_Type : Node_Types;
      Check     : Gtk_Check_Menu_Item;
   begin
      if Iter /= Null_Iter then
         --  If Menu is null, this means that this function is being called
         --  through Default_Context_Factory and is not filling a contextual
         --  menu. In this case we do not want to do the call to Set_Cursor
         --  which would cause scrolling of the currently selected cell if it
         --  is not visible.
         if Menu /= null then
            Path := Get_Path (T.File_Model, Iter);
            Set_Cursor (T.File_Tree, Path, null, False);
            Path_Free (Path);
         end if;

         Node_Type := Node_Types'Val
           (Integer (Get_Int (T.File_Model, Iter, Node_Type_Column)));

         case Node_Type is
            when Directory_Node | File_Node =>
               File := Get_File (T.File_Model, Iter, File_Column);
               Set_File_Information (Context, (1 => File));

            when Entity_Node =>
               --  ??? No entity information was set before, but isn't this
               --  strange ?
               null;

            when others =>
               null;

         end case;
      end if;

      if Menu /= null then
         Gtk_New (Check, Label => -"Show files from project only");
         Associate
           (Get_History (Kernel).all, File_View_Shows_Only_Project, Check);
         Append (Menu, Check);
         Widget_Callback.Object_Connect
           (Check, Signal_Toggled, Refresh'Access, T);
      end if;
   end Explorer_Context_Factory;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls
     (Explorer : access Project_Explorer_Files_Record'Class) is
   begin
      while not Timeout_Id_List.Is_Empty (Explorer.Fill_Timeout_Ids) loop
         Pop_State (Explorer.Kernel);
         Glib.Main.Remove (Timeout_Id_List.Head (Explorer.Fill_Timeout_Ids));
         Timeout_Id_List.Next (Explorer.Fill_Timeout_Ids);
      end loop;
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
         Set
           (T.File_Model,
            Iter,
            Icon_Column,
            GObject (Close_Pixbufs (Directory_Node)));
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
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
               Set (T.File_Model, Iter, Icon_Column,
                    GObject (Open_Pixbufs (Directory_Node)));
               File_Append_Directory (T, File, Iter, 1);

            when File_Node =>
               Free_Children (T, Iter);
               Append_File_Info (T.Kernel, T.File_Model, Iter, File);

            when Project_Node | Extends_Project_Node =>
               null;

            when Category_Node | Entity_Node =>
               null;

            when Obj_Directory_Node | Exec_Directory_Node =>
               null;

            when Modified_Project_Node =>
               null;
         end case;
      end;

      Ignore := Expand_Row (T.File_Tree, Path, False);
      Scroll_To_Cell (T.File_Tree, Path, null, True, 0.1, 0.1);

      T.Expanding := False;

   exception
      when E : others => Trace (Exception_Handle, E);
   end File_Tree_Expand_Row_Cb;

   ----------------------------
   -- File_Selection_Changed --
   ----------------------------

   procedure File_Selection_Changed
     (Explorer : access Gtk_Widget_Record'Class)
   is
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
   begin
      Context_Changed (T.Kernel);
   exception
      when E : others => Trace (Exception_Handle, E);
   end File_Selection_Changed;

   -----------------------
   -- File_Button_Press --
   -----------------------

   function File_Button_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean
   is
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
   begin
      return On_Button_Press
        (T.Kernel,
         MDI_Explorer_Child (Find_MDI_Child (Get_MDI (T.Kernel), T)),
         T.File_Tree, T.File_Model, Event, True);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
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

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end File_Key_Press;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Files : access Gtk.Widget.Gtk_Widget_Record'Class) is
      Explorer     : constant Project_Explorer_Files :=
                       Project_Explorer_Files (Files);
      Drives       : File_Array_Access;
      Cur_Dir      : constant Virtual_File := Get_Current_Dir;
      Dir_Inserted : Boolean := False;

   begin
      Clear (Explorer.File_Model);
      File_Remove_Idle_Calls (Explorer);

      if Get_History
        (Get_History (Explorer.Kernel).all, File_View_Shows_Only_Project)
      then
         declare
            Inc : constant File_Array :=
                    Source_Dirs (Get_Project (Explorer.Kernel), True);
            Obj : constant File_Array :=
                    Object_Path (Get_Project (Explorer.Kernel), True, False);
         begin
            File_Append_Directory
              (Explorer      => Explorer,
               Dir           => Greatest_Common_Path (Inc & Obj),
               Base          => Null_Iter,
               Depth         => 1,
               Append_To_Dir => Get_Current_Dir,
               Idle          => True);
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
      when E : others => Trace (Exception_Handle, E);
   end Refresh;

   -------------------
   -- Free_Children --
   -------------------

   procedure Free_Children
     (T    : Project_Explorer_Files;
      Iter : Gtk_Tree_Iter)
   is
      Current : Gtk_Tree_Iter := Children (T.File_Model, Iter);
   begin
      if Has_Child (T.File_Model, Iter) then
         while Current /= Null_Iter loop
            Remove (T.File_Model, Current);
            Current := Children (T.File_Model, Iter);
         end loop;
      end if;
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

               Set_File (View.File_Model, Iter2, File_Column, File);
               Set (View.File_Model, Iter2, Display_Name_Column,
                    File.Display_Base_Dir_Name);
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

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Deleted_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Remove_File (Hook.View, File_Hooks_Args (Data.all).File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Saved_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Add_File (Hook.View, File_Hooks_Args (Data.all).File);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Project_View_Changed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Refresh (Hook.View);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Renamed_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Remove_File (Hook.View, Files_2_Hooks_Args (Data.all).File);
      Add_File (Hook.View, Files_2_Hooks_Args (Data.all).Renamed);
   end Execute;

   ----------------------
   -- On_Open_Explorer --
   ----------------------

   procedure On_Open_Explorer
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : Project_Explorer_Files;
      Child : MDI_Child;
      C2    : MDI_Explorer_Child;
   begin
      --  Start with the files view, so that if both are needed, the project
      --  view ends up on top of the files view
      Child := Find_MDI_Child_By_Tag
        (Get_MDI (Kernel), Project_Explorer_Files_Record'Tag);

      if Child = null then
         Gtk_New (Files, Kernel);
         C2 := new MDI_Explorer_Child_Record;
         Initialize (C2, Files,
                     Default_Width  => 215,
                     Default_Height => 600,
                     Group          => Group_View,
                     Module         => Explorer_Files_Module_Id);
         Set_Title (C2, -"Files",  -"Files");
         Put (Get_MDI (Kernel), C2, Initial_Position => Position_Left);
         Child := MDI_Child (C2);
      end if;

      Raise_Child (Child);
      Set_Focus_Child (Get_MDI (Kernel), Child);

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Open_Explorer;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child is
   begin
      if Node.Tag.all = "Project_Explorer_Files" then
         On_Open_Explorer (MDI, User);
         return Find_MDI_Child_By_Tag
           (Get_MDI (User), Project_Explorer_Files_Record'Tag);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      User   : Kernel_Handle)
     return Node_Ptr
   is
      pragma Unreferenced (User);
      N : Node_Ptr;
   begin
      if Widget.all in Project_Explorer_Files_Record'Class then
         N := new Node;
         N.Tag := new String'("Project_Explorer_Files");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools") & '/' & (-"Views");
      File_View_Filter : constant Action_Filter :=
                           new File_View_Filter_Record;
   begin
      Explorer_Files_Module_Id := new Explorer_Module_Record;
      Register_Module
        (Module      => Explorer_Files_Module_Id,
         Kernel      => Kernel,
         Module_Name => "Files_View",
         Priority    => GPS.Kernel.Modules.Default_Priority);
      Register_Desktop_Functions (Save_Desktop'Access, Load_Desktop'Access);
      Register_Menu
        (Kernel, Tools, -"_Files", "", On_Open_Explorer'Access,
         Ref_Item => -"Entity", Add_Before => False);
      Register_Filter
        (Kernel,
         Filter => File_View_Filter,
         Name   => "File_View");
   end Register_Module;

end Project_Explorers_Files;
