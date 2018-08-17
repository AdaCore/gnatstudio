------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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
with Ada.Calendar;               use Ada.Calendar;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;

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
with Gdk.Event;                  use Gdk.Event;
with Gtk.Box;                    use Gtk.Box;
with Gtk.Dnd;                    use Gtk.Dnd;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_Selection;         use Gtk.Tree_Selection;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Menu;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gdk.Types;
with Gtk.Widget;                 use Gtk.Widget;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.Tree_View;           use Gtkada.Tree_View;

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
with GPS.VCS;                    use GPS.VCS;
with Projects;                   use Projects;
with GUI_Utils;                  use GUI_Utils;
with Tooltips;                   use Tooltips;
with Project_Explorers_Common;   use Project_Explorers_Common;

package body Project_Explorers_Files is
   Me : constant Trace_Handle := Create ("GPS.PRJ_EDITOR.FILES");

   use Explorer_Expansion;

   File_View_Shows_Only_Project : Boolean_Preference;
   Dirs_From_Project            : Boolean_Preference;

   package Virtual_Files_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Virtual_File);
   package Source_Id_Lists is new Ada.Containers.Doubly_Linked_Lists
     (G_Source_Id);

   type Files_View_Config is record
      Shows_Only_Project   : Boolean := False;
      Dirs_From_Project    : Boolean := False;
      Show_Hidden_Files    : Boolean := False;
      Hidden_Files_Pattern : Unbounded_String;
   end record;

   type Files_Tree_View_Record is new Base_Explorer_Tree_Record with record
      Config      : Files_View_Config;
      Timeout_Ids : Source_Id_Lists.List;
      Is_Locate   : Boolean := False;
      --  Need to be synchrone when trying to locate a file in the view
   end record;
   type Files_Tree_View is access all Files_Tree_View_Record'Class;
   overriding procedure Add_Children
     (Self       : not null access Files_Tree_View_Record;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter);

   type Project_Explorer_Files_Record is new Generic_Views.View_Record with
      record
         Tree                : Files_Tree_View;
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

   type Readdir_Step is
     (Step_Read_Files,   --  opendir, readdir, closedir
      Step_Filter_Files,
      Step_Insert_Dirs,
      Step_Insert_Files,
      Step_Setup_View,
      Step_Wait_Alone);  --  To find a file this timeout must be alone
   --  What is the current step for the background loop
   --  There are 2 distincts loop: Step_Read_File and Step_Wait_Alone

   type Append_Directory_Idle_Data is record
      Tree          : Files_Tree_View;
      Dir           : Virtual_File;   --  Dir currently processed
      Norm_Dest     : Virtual_File;   --  target directory
      Step          : Readdir_Step := Step_Read_Files;
      Base          : Gtk_Tree_Iter;  --  Node for Dir
      Files         : File_Array_Access := null;
      File_Index    : Natural := 0;
      Id            : G_Source_Id;

      Next_Iter     : Gtk_Tree_Iter;   --  Node to expand for the next level
      Next_File     : Virtual_File;    --  Directory to expand for next level

      VCS           : Abstract_VCS_Engine_Access;
      --  The VCS for the current directory

      Detached      : Explorer_Expansion.Detached_Model_Access;
      --  when set and then destroyed, this will detach and reattach the view,
      --  to optimize insertion time.
   end record;
   type Append_Directory_Idle_Data_Access is access Append_Directory_Idle_Data;
   --  Custom data for the asynchronous fill function

   procedure Free (Self : in out Append_Directory_Idle_Data_Access);

   package File_Append_Directory_Timeout is
     new Glib.Main.Generic_Sources (Append_Directory_Idle_Data_Access);

   procedure Set_Column_Types
     (Tree : not null access Gtk_Tree_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   procedure File_Append_Directory
     (Self          : not null access Files_Tree_View_Record'Class;
      Dir           : Virtual_File;
      Base          : Gtk_Tree_Iter;
      Append_To_Dir : Virtual_File  := No_File;
      Idle          : Boolean := False);
   --  Add to the file view the directory Dir, at node given by Iter.
   --  If Append_To_Dir is not No_File, and is a sub-directory of Dir, then
   --  the path is expanded recursively all the way to Append_To_Dir.

   function Create_Directory_Node
     (Self   : not null access Files_Tree_View_Record'Class;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Dir    : Virtual_File) return Gtk_Tree_Iter;
   --  Create a new node for a directory

   procedure Tree_Expand_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Called every time a node is expanded in the file view.
   --  It is responsible for automatically adding the children of the current
   --  node if they are not there already.

   procedure Tree_Collapse_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path);
   --  Called every time a node is collapsed in the file view

   procedure On_File_Destroy
     (Explorer : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Callback for the "destroy" event on the file view

   procedure File_Remove_Idle_Calls
     (Explorer : access Files_Tree_View_Record'Class);
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

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean;
   --  ???
   --  Called by File_Append_Directory.

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Explorer : Project_Explorer_Files;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when preferences change

   type Locate_File_In_Files_View is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Locate_File_In_Files_View;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Launch a timeout waiting for the other timeout to finish, then
   --  will go to synchronous mode and will finish the search

   procedure Locate_File (V : Files_Tree_View; F : Virtual_File);
   --  This function must be called in synchronous mode

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

   ---------------------------
   -- Create_Directory_Node --
   ---------------------------

   function Create_Directory_Node
     (Self   : not null access Files_Tree_View_Record'Class;
      Parent : Gtk_Tree_Iter := Null_Iter;
      Dir    : Virtual_File) return Gtk_Tree_Iter
   is
      Iter    : Gtk_Tree_Iter;
      Values  : Glib.Values.GValue_Array (1 .. 4);
      Columns : constant Columns_Array (Values'Range) :=
        (File_Column, Display_Name_Column, Node_Type_Column, Icon_Column);
   begin
      Self.Model.Append (Iter, Parent => Parent);
      Values :=
        (1 => As_File   (Dir),
         2 => As_String (Dir.Display_Base_Dir_Name),
         3 => As_Int    (Gint (Node_Types'Pos (Directory_Node))),
         4 => As_String (Stock_For_Node (Directory_Node, Expanded => False)));
      Set_And_Clear (Self.Model, Iter, Columns, Values);
      Self.Set_Might_Have_Children (Iter);
      return Iter;
   end Create_Directory_Node;

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
        Find_Iter_For_Event (Explorer.Tree, Event);
      Path      : Gtk_Tree_Path;
      File      : Virtual_File;
      Node_Type : Node_Types;
   begin
      if Iter /= Null_Iter then
         if Event /= null then
            Path := Get_Path (Explorer.Tree.Model, Iter);
            Set_Cursor (Explorer.Tree, Path, null, False);
            Path_Free (Path);
         end if;

         Node_Type := Explorer.Tree.Get_Node_Type (Iter);
         case Node_Type is
            when Directory_Node | File_Node =>
               File := Explorer.Tree.Get_File_From_Node (Iter);
               Set_File_Information (Context, (1 => File));

            when others =>
               null;
         end case;
      end if;
      return Context;
   end Build_Context;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Append_Directory_Idle_Data_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Append_Directory_Idle_Data, Append_Directory_Idle_Data_Access);
      C : Source_Id_Lists.Cursor;
   begin
      if Self /= null then
         Trace (Me, "Free append_directory_idle_data "
            & Self.Dir.Display_Full_Name);

         C := Self.Tree.Timeout_Ids.Find (Self.Id);
         if Source_Id_Lists.Has_Element (C) then
            Self.Tree.Timeout_Ids.Delete (C);
         end if;

         Unchecked_Free (Self.Detached);
         Unchecked_Free (Self);
      end if;
   end Free;

   --------------------
   -- Read_Directory --
   --------------------

   function Read_Directory
     (D : Append_Directory_Idle_Data_Access) return Boolean
   is
      Max_Idle_Duration : constant Duration := 0.04;
      --  Maximum time one iteration in this background loop should take.
      --  Since we are doing system calls, we might easily spend too much time
      --  filling the view otherwise.

      Start             : constant Time := Clock;

      Iter : Gtk_Tree_Iter;

      function File_Is_In_Project (F : Virtual_File) return Boolean;
      --  Whether the file belongs to any loaded project

      function File_Is_In_Project (F : Virtual_File) return Boolean is
         T : constant Project_Tree_Access := Get_Registry (D.Tree.Kernel).Tree;
      begin
         if F.Is_Directory then
            return T.Directory_Belongs_To_Project
              (F.Full_Name, Direct_Only => False);

         else
            declare
               File : constant Virtual_File :=
                 T.Create (Name => F.Base_Dir_Name);

               --  First matching project, since we have nothing else
               --  to base our guess on
               F_Info : constant File_Info'Class :=
                 File_Info'Class (T.Info_Set (File).First_Element);
               P      : constant Project_Type    := F_Info.Project;
            begin
               --  If not part of a project, then we remove the file
               return P /= No_Project and then File = F;
            end;
         end if;
      end File_Is_In_Project;

   begin
      if Active (Me) then
         Trace (Me, "Read dir " & D.Dir.Display_Full_Name
                & " step=" & D.Step'Img);
      end if;

      case D.Step is

         ----------------------
         -- Step_Read_Files --
         ---------------------

         when Step_Read_Files =>
            --  If we are appending at the base, create a node indicating the
            --  absolute path to the directory.

            if D.Base = Null_Iter then
               Trace (Me, "Create base node for " & D.Dir.Display_Full_Name);
               D.Base := Create_Directory_Node
                 (Self   => D.Tree,
                  Dir    => D.Dir);
            end if;

            --  Prepare the list of files.

            D.Files := D.Dir.Read_Dir;
            D.File_Index := D.Files'First;
            Sort (D.Files.all);

            --  Eliminate hidden files
            for F in D.Files'Range loop
               if D.Tree.Kernel.Is_Hidden (D.Files (F)) then
                  D.Files (F) := No_File;
               end if;
            end loop;

            D.Step := Step_Filter_Files;
            return True;  --  will continue at next iteration

         -----------------------
         -- Step_Filter_Files --
         -----------------------

         when Step_Filter_Files =>
            while D.File_Index <= D.Files'Last loop

               if Clock - Start > Max_Idle_Duration then
                  return True;  --  will continue at next iteration
               end if;

               if D.Files (D.File_Index) = No_File then
                  null;

               elsif D.Tree.Config.Shows_Only_Project then
                  if not File_Is_In_Project (D.Files (D.File_Index)) then
                     D.Files (D.File_Index) := No_File;
                  end if;

               elsif D.Tree.Config.Dirs_From_Project then
                  if Is_Directory (D.Files (D.File_Index)) then
                     if not Get_Registry (D.Tree.Kernel).Tree.
                       Directory_Belongs_To_Project
                         (D.Files (D.File_Index).Full_Name,
                          Direct_Only => False)
                     then
                        --  Remove from the list
                        D.Files (D.File_Index) := No_File;
                     end if;

                  else
                     declare
                        Dir : constant Virtual_File :=
                          D.Files (D.File_Index).Dir;
                     begin
                        if not Get_Registry (D.Tree.Kernel).Tree.
                          Directory_Belongs_To_Project
                            (Dir.Full_Name, Direct_Only => True)
                        then
                           --  Remove from the list
                           D.Files (D.File_Index) := No_File;
                        end if;
                     end;
                  end if;
               end if;

               D.File_Index := D.File_Index + 1;
            end loop;

            --  If there are no files or subdirectories, the node cannot be
            --  expanded

            declare
               Empty : Boolean := True;
            begin
               for J in D.Files'Range loop
                  if D.Files (J) /= No_File then
                     Empty := False;
                     exit;
                  end if;
               end loop;

               if Empty then
                  Set (D.Tree.Model, D.Base, Icon_Column,
                       Stock_For_Node (Directory_Node, Expanded => False));
                  return False;  --  done processing this directory
               end if;
            end;

            --  Ensure that all files will eventually get some VCS info

            D.VCS := D.Tree.Kernel.VCS.Guess_VCS_For_Directory (D.Dir);
            if Active (Me) then
               Trace (Me, "VCS for " & D.Dir.Display_Full_Name & " is "
                      & D.VCS.Name);
            end if;
            D.VCS.Ensure_Status_For_Files (D.Files.all);

            D.Step := Step_Insert_Dirs;
            D.File_Index := D.Files'First;

            if D.Detached = null then
               D.Detached := new Explorer_Expansion.Detached_Model'
                 (Explorer_Expansion.Detach_Model_From_View (D.Tree));
            end if;

            D.Tree.Remove_Dummy_Child (D.Base);

            return True;   --  will continue at next iteration

         ----------------------
         -- Step_Insert_Dirs --
         ----------------------

         when Step_Insert_Dirs =>
            while D.File_Index <= D.Files'Last loop

               if Clock - Start > Max_Idle_Duration then
                  return True;  --  will continue at next iteration
               end if;

               if D.Files (D.File_Index) /= No_File
                 and then D.Files (D.File_Index).Is_Directory
               then
                  Iter := Create_Directory_Node
                    (Self   => D.Tree,
                     Dir    => D.Files (D.File_Index),
                     Parent => D.Base);

                  if Is_Parent (D.Files (D.File_Index), D.Norm_Dest) then
                     D.Next_Iter := Iter;
                     D.Next_File := D.Files (D.File_Index);
                  end if;

                  D.Files (D.File_Index) := No_File;  --  already inserted
               end if;

               D.File_Index := D.File_Index + 1;
            end loop;

            D.Step := Step_Insert_Files;
            D.File_Index := D.Files'First;
            return True;  --  will continue at next iteration

         -----------------------
         -- Step_Insert_Files --
         -----------------------

         when Step_Insert_Files =>

            while D.File_Index <= D.Files'Last loop

               if Clock - Start > Max_Idle_Duration then
                  return True;  --  will continue at next iteration
               end if;

               if D.Files (D.File_Index) /= No_File then
                  Iter := Create_File
                    (D.Tree, D.Base, D.Files (D.File_Index),
                     Icon_Name => To_String
                       (D.VCS.Get_Display
                          (D.VCS.File_Properties_From_Cache
                             (D.Files (D.File_Index)).Status).Icon_Name));
               end if;

               D.File_Index := D.File_Index + 1;
            end loop;

            Unchecked_Free (D.Files);

            if D.Next_Iter /= Null_Iter then
               D.Step := Step_Read_Files;
               D.Dir := D.Next_File;
               D.Base := D.Next_Iter;
               D.Next_Iter := Null_Iter;
               D.Next_File := No_File;
               return True;   --  will now process that directory
            end if;

            D.Step := Step_Setup_View;
            Unchecked_Free (D.Detached);
            return True;  --  will continue at next itertion

         ---------------------
         -- Step_Setup_View --
         ---------------------

         when Step_Setup_View =>

            declare
               Path   : Gtk_Tree_Path;
            begin
               Path := D.Tree.Get_Filter_Path_For_Store_Iter (D.Base);
               D.Tree.Expand_To_Path (Path);
               D.Tree.Get_Selection.Select_Path (Path);
               D.Tree.Scroll_To_Cell
                 (Path,
                  Column    => null,
                  Use_Align => False,
                  Row_Align => 0.0,
                  Col_Align => 0.0);
               Path_Free (Path);
            end;

            return False;

         ---------------------
         -- Step_Wait_Alone --
         ---------------------

         when Step_Wait_Alone =>
            if Integer (D.Tree.Timeout_Ids.Length) = 1 then
               Locate_File (D.Tree, D.Norm_Dest);
               --  The search is ended
               D.Tree.Is_Locate := False;
               return False;
            else
               return True;  -- Will continue to wait
            end if;

      end case;

   exception
      when VFS_Directory_Error =>
         Trace (Me, "Directory error: " & D.Dir.Display_Full_Name);
         --  The directory couldn't be open, probably because of permissions
         return False;

      when E : others =>
         Trace (Me, E);
         return False;
   end Read_Directory;

   ---------------------------
   -- File_Append_Directory --
   ---------------------------

   procedure File_Append_Directory
     (Self          : not null access Files_Tree_View_Record'Class;
      Dir           : Virtual_File;
      Base          : Gtk_Tree_Iter;
      Append_To_Dir : Virtual_File := No_File;
      Idle          : Boolean := False)
   is
      D          : constant Append_Directory_Idle_Data_Access :=
                     new Append_Directory_Idle_Data;
      --  D is freed when Read_Directory ends (i.e. returns False)

   begin
      D.Dir           := Dir;
      Ensure_Directory (D.Dir);
      D.Norm_Dest     := Append_To_Dir;
      D.Base          := Base;
      D.Tree          := Files_Tree_View (Self);

      if Idle and not D.Tree.Is_Locate then
         --  Do not append the first item in an idle loop.
         --  Necessary for preserving order in drive names.
         if Read_Directory (D) then
            D.Id := File_Append_Directory_Timeout.Timeout_Add
               (1, Read_Directory'Access, D,
                  Notify => Free'Access);
            Self.Timeout_Ids.Append (D.Id);
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

   procedure Set_Column_Types
     (Tree : not null access Gtk_Tree_View_Record'Class)
   is
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
      P            : access On_Pref_Changed;
   begin
      Initialize_Vbox (Explorer, Homogeneous => False);

      Gtk_New (Scrolled);
      Explorer.Pack_Start (Scrolled, Expand => True, Fill => True);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);

      Explorer.Tree := new Files_Tree_View_Record;
      Explorer.Tree.Kernel := Explorer.Kernel;
      Explorer.Tree.Initialize
        (Column_Types => Columns_Types);
      Explorer.Tree.Set_Enable_Search (False);
      Set_Name (Explorer.Tree, "File Explorer Tree");

      Scrolled.Add (Explorer.Tree);

      Set_Headers_Visible (Explorer.Tree, False);

      Explorer.Tree.On_Button_Press_Event
        (File_Button_Press'Access, Explorer);
      Explorer.Tree.On_Button_Release_Event
        (File_Button_Press'Access, Explorer);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Key_Press_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (File_Key_Press'Access),
         Slot_Object => Explorer,
         After       => False);

      Widget_Callback.Object_Connect
        (Get_Selection (Explorer.Tree),
         Signal_Changed,
         File_Selection_Changed'Access,
         Slot_Object => Explorer,
         After       => True);

      Set_Column_Types (Explorer.Tree);

      Setup_Contextual_Menu
        (Kernel          => Explorer.Kernel,
         Event_On_Widget => Explorer.Tree);

      Widget_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Row_Expanded,
         Widget_Callback.To_Marshaller (Tree_Expand_Row_Cb'Access),
         Explorer,
         False);

      Widget_Callback.Object_Connect
        (Explorer.Tree,
         Signal_Row_Collapsed,
         Widget_Callback.To_Marshaller (Tree_Collapse_Row_Cb'Access),
         Explorer,
         False);

      Widget_Callback.Object_Connect
        (Explorer.Tree, Signal_Destroy,
         On_File_Destroy'Access, Explorer, False);

      Gtk.Dnd.Dest_Set
        (Explorer.Tree, Dest_No_Default, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.Tree,
         Signal_Drag_Data_Received,
         Project_Explorers_Common.Drag_Data_Received'Access,
         Explorer.Kernel);
      Explorer.Tree.Enable_Model_Drag_Source
        (Gdk.Types.Button1_Mask, Target_Table_Url, Action_Any);
      Kernel_Callback.Connect
        (Explorer.Tree, Signal_Drag_Data_Get,
         Drag_Data_Get'Access, Explorer.Kernel);

      File_Deleted_Hook.Add (new On_Deleted, Watch => Explorer);
      File_Saved_Hook.Add (new On_File_Saved, Watch => Explorer);
      File_Renamed_Hook.Add (new On_File_Renamed, Watch => Explorer);

      P := new On_Pref_Changed;
      P.Explorer := Project_Explorer_Files (Explorer);
      Preferences_Changed_Hook.Add (P, Watch => Explorer);
      P.Execute (Explorer.Kernel, null);  --  calls Refresh

      Tooltip := new Explorer_Tooltips;
      Tooltip.Tree := Explorer.Tree;
      Tooltip.Set_Tooltip (Explorer.Tree);

      Vcs_File_Status_Changed_Hook.Add
        (new On_VCS_Status_Changed'
           (Vcs_File_Status_Hooks_Function with Tree => Explorer.Tree),
         Watch => Explorer);

      return Gtk_Widget (Explorer.Tree);
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
      Append_Menu (Menu, View.Kernel, Show_Hidden_Files);
   end Create_Menu;

   ----------------------------
   -- File_Remove_Idle_Calls --
   ----------------------------

   procedure File_Remove_Idle_Calls
     (Explorer : access Files_Tree_View_Record'Class)
   is
      Tmp : constant Source_Id_Lists.List := Explorer.Timeout_Ids;
   begin
      Explorer.Timeout_Ids.Clear;
      for Id of Tmp loop
         Glib.Main.Remove (Id);
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
      File_Remove_Idle_Calls (E.Tree);
   end On_File_Destroy;

   -------------------------------
   -- Tree_Collapse_Row_Cb --
   -------------------------------

   procedure Tree_Collapse_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Iter);
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
      Iter : Gtk_Tree_Iter;
   begin
      Iter := T.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);
      if T.Tree.Get_Node_Type (Iter) = Directory_Node then
         T.Tree.Model.Set
           (Iter, Icon_Column, Stock_For_Node (Directory_Node, False));
      end if;
   end Tree_Collapse_Row_Cb;

   ------------------
   -- Add_Children --
   ------------------

   overriding procedure Add_Children
     (Self       : not null access Files_Tree_View_Record;
      Store_Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      File    : Virtual_File;
      N_Type  : constant Node_Types := Self.Get_Node_Type (Store_Iter);
   begin

      case N_Type is
         when Directory_Node =>
            File := Self.Get_File_From_Node (Store_Iter);
            File_Append_Directory
              (Self, File, Store_Iter, Idle => True);

         when others =>
            null;
      end case;
   end Add_Children;

   -----------------------------
   -- Tree_Expand_Row_Cb --
   -----------------------------

   procedure Tree_Expand_Row_Cb
     (Explorer    : access Gtk.Widget.Gtk_Widget_Record'Class;
      Filter_Iter : Gtk_Tree_Iter;
      Filter_Path : Gtk_Tree_Path)
   is
      pragma Unreferenced (Filter_Iter);
      T : constant Project_Explorer_Files := Project_Explorer_Files (Explorer);
      Iter    : Gtk_Tree_Iter;
   begin
      Iter   := T.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);
      if T.Tree.Get_Node_Type (Iter) = Directory_Node then
         T.Tree.Model.Set
           (Iter, Icon_Column, Stock_For_Node (Directory_Node, True));
      end if;
   end Tree_Expand_Row_Cb;

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
      if Child /= null
         and then MDI_Child (Child) = Get_MDI (T.Kernel).Get_Focus_Child
      then
         T.Kernel.Context_Changed (Child.Build_Context);
      end if;
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
        (MDI_Explorer_Child (Explorer_Files_Views.Child_From_View (T)),
         T.Tree, Event);
   end File_Button_Press;

   --------------------
   -- File_Key_Press --
   --------------------

   function File_Key_Press
     (Explorer : access Gtk_Widget_Record'Class;
      Event    : Gdk_Event) return Boolean is
   begin
      return On_Key_Press (Project_Explorer_Files (Explorer).Tree, Event);
   end File_Key_Press;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Locate_File_In_Files_View;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      V      : constant Project_Explorer_Files            :=
        Explorer_Files_Views.Get_Or_Create_View (Get_Kernel (Context.Context));
      D      : constant Append_Directory_Idle_Data_Access :=
        new Append_Directory_Idle_Data;
      --  D is freed when Read_Directory ends (i.e. returns False)
      Dummy  : Boolean;
   begin
      if not V.Tree.Is_Locate then
         --  Need to be synchrone
         V.Tree.Is_Locate := True;
         --  Setup D to do the search
         D.Tree := V.Tree;
         D.Step := Step_Wait_Alone;
         D.Norm_Dest := File_Information (Context.Context);
         D.Id := File_Append_Directory_Timeout.Timeout_Add
           (1, Read_Directory'Access, D, Notify => Free'Access);
         V.Tree.Timeout_Ids.Append (D.Id);
         Dummy := Read_Directory (D);

      else
         Trace (Me, "A search is already active in the view");
      end if;
      return Commands.Success;
   end Execute;

   -----------------
   -- Locate_File --
   -----------------

   procedure Locate_File (V : Files_Tree_View; F : Virtual_File)
   is
      Parent : Gtk_Tree_Iter;
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;

      function Find_Node
        (File   : Virtual_File; Expand : Boolean) return Gtk_Tree_Iter;
      --  Expand the node for File (and its parent directories)

      ---------------
      -- Find_Node --
      ---------------

      function Find_Node
        (File : Virtual_File; Expand : Boolean) return Gtk_Tree_Iter
      is
         Path : Gtk_Tree_Path;
         Iter : Gtk_Tree_Iter;
         F    : Virtual_File;
         Dummy : Boolean;
      begin
         if File = No_File
           --  ??? The test below seems unix-centric
           or else File.Full_Name.all = "/"
         then
            Iter := V.Model.Get_Iter_First;
         else
            --  Non tail terminal recursion: goes to the root node and
            --  recursevely expands the path to the wanted file
            Iter := Find_Node (File.Get_Parent, Expand => True);
            Iter := V.Model.Children (Iter);
         end if;

         --  Search in the cache for children
         while Iter /= Null_Iter loop
            F := Get_File (V.Model, Iter, File_Column);
            if File = F then
               if Expand then
                  Path := V.Model.Get_Path (Iter);
                  Dummy := V.Expand_Row (Path, Open_All => False);
                  Iter := V.Model.Get_Iter (Path);
                  Path_Free (Path);
               end if;
               Parent := Iter;
               exit;
            end if;
            V.Model.Next (Iter);
         end loop;

         if Iter = Null_Iter then
            Path := V.Model.Get_Path (Parent);
            --  Can't get the children, so try to expand the parent node
            --  to load the children in cache.
            Dummy := V.Expand_Row (Path, Open_All => False);
            Iter := V.Model.Children (V.Model.Get_Iter (Path));
            Path_Free (Path);

            --  Search in the added children
            while Iter /= Null_Iter loop
               F := Get_File (V.Model, Iter, File_Column);
               if File = F then
                  if Expand then
                     Path := V.Model.Get_Path (Iter);
                     Dummy := V.Expand_Row (Path, Open_All => False);
                     Iter := V.Model.Get_Iter (Path);
                     Path_Free (Path);
                  end if;
                  Parent := Iter;
                  exit;
               end if;
               V.Model.Next (Iter);
            end loop;
         end if;
         return Iter;
      end Find_Node;

   begin
      Iter := Find_Node (F, Expand => False);
      if Iter /= Null_Iter then
         V.Get_Selection.Select_Iter (Iter);
         Path := V.Model.Get_Path (Iter);
         V.Scroll_To_Cell
           (Path, V.Get_Column (0),
            Use_Align => False, Row_Align => 0.0, Col_Align => 0.0);
         V.Set_Cursor (Path, V.Get_Column (0), False);
         Path_Free (Path);
      end if;
   end Locate_File;

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
      Refresh (V);
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
      Iter         : Gtk_Tree_Iter with Unreferenced;

      procedure Add_Drives_From_Files (Files : File_Array);
      --  Add all the logical drives used by Files in the files explorer.

      procedure Add_Drives_From_Files (Files : File_Array)
      is
         Added_Drives : Virtual_Files_Lists.List;
      begin
         for File of Files loop
            if not Added_Drives.Contains (File.Get_Root) then
               File_Append_Directory
                 (Self          => Explorer.Tree,
                  Dir           => File.Get_Root,
                  Base          => Null_Iter,
                  Append_To_Dir => Get_Current_Dir,
                  Idle          => True);

               Added_Drives.Append (File.Get_Root);
            end if;
         end loop;
      end Add_Drives_From_Files;

   begin
      Trace (Me, "Clear Files view");
      Clear (Explorer.Tree.Model);
      File_Remove_Idle_Calls (Explorer.Tree);

      if Explorer.Tree.Config.Shows_Only_Project
         or else Explorer.Tree.Config.Dirs_From_Project
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
                 (Self          => Explorer.Tree,
                  Dir           => Common_Path,
                  Base          => Null_Iter,
                  Append_To_Dir => Cur_Dir,
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
                    (Explorer.Tree, Drives (J),
                     Null_Iter,
                     Append_To_Dir => Cur_Dir, Idle    => True);
                  Dir_Inserted := True;

               else
                  Iter := Create_Directory_Node
                    (Explorer.Tree, Null_Iter, Drives (J));
               end if;
            end loop;
         end if;

         if not Dir_Inserted then
            File_Append_Directory
              (Explorer.Tree, Cur_Dir.Get_Root,
               Null_Iter,
               Append_To_Dir => Cur_Dir,
               Idle          => True);
         end if;
      end if;
   end Refresh;

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
      Iter := Get_Iter_First (View.Tree.Model);

      while Iter /= Null_Iter loop
         if Get_File (View.Tree.Model, Iter, File_Column) = File then
            --  First select the parent and set the 'scroll to dir' state
            Path := Get_Path (View.Tree.Model, Parent (View.Tree.Model, Iter));
            Set_Cursor (View.Tree, Path, null, False);
            Path_Free (Path);

            --  Now remove the node, this will invoke the expose event, that
            --  will scroll to the parent directory.
            Remove (View.Tree.Model, Iter);
            exit;
         end if;

         --  We look through the tree: first dir node, then children,
         --  then parent's next item.
         if Has_Child (View.Tree.Model, Iter) then
            Iter := Children (View.Tree.Model, Iter);

         else
            loop
               Next_Iter := Iter;
               Next (View.Tree.Model, Next_Iter);

               if Next_Iter = Null_Iter then
                  Iter := Parent (View.Tree.Model, Iter);
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
      if View.Kernel.Is_Hidden (File) then
         return;
      end if;

      Iter := Get_Iter_First (View.Tree.Model);

      if Is_Directory (File) then
         Dir := GNATCOLL.VFS.Get_Parent (File);
      end if;

      while Iter /= Null_Iter loop
         if Get_File (View.Tree.Model, Iter, File_Column) = Dir then
            --  We found the file's directory

            Path := Get_Path (View.Tree.Model, Iter);

            if not Row_Expanded (View.Tree, Path)
              and then Children (View.Tree.Model, Iter) /= Null_Iter
            then
               --  File's directory is not expanded. Return now

               --  Note that we need to test if dir node has children: in the
               --  normal case, a non expanded dir always has a dummy child.
               --  When we rename a directory, we might have deleted the only
               --  dir's child, then this dir won't have children at all. We
               --  don't want to fall back in this case here.

               Path_Free (Path);

               return;
            end if;

            --  file's directory is expanded. Let's look at the children
            Next_Iter := Children (View.Tree.Model, Iter);

            while Next_Iter /= Null_Iter loop
               if Get_File (View.Tree.Model, Next_Iter, File_Column) =
                 File
               then
                  --  File already present. Do nothing
                  return;
               end if;

               Next (View.Tree.Model, Next_Iter);
            end loop;

            --  If we are here, then this means that the saved file is not
            --  present in the view. Let's insert it.

            if Is_Directory (File) then
               Next_Iter := Children (View.Tree.Model, Iter);
               Done := False;

               while Next_Iter /= Null_Iter loop

                  if View.Tree.Get_Node_Type (Next_Iter) = Directory_Node then
                     declare
                        Name : constant Filesystem_String :=
                          View.Tree.Get_File_From_Node (Next_Iter).Base_Name;
                     begin
                        if Name > File.Base_Dir_Name then
                           Insert_Before
                             (View.Tree.Model, Iter2, Iter, Next_Iter);
                           Done := True;

                           exit;
                        end if;
                     end;

                  elsif View.Tree.Get_Node_Type (Next_Iter) = File_Node then
                     Insert_Before
                       (View.Tree.Model, Iter2, Iter, Next_Iter);
                     Done := True;

                     exit;
                  end if;

                  Next (View.Tree.Model, Next_Iter);
               end loop;

               if not Done then
                  Append (View.Tree.Model, Iter2, Iter);
               end if;

               Set_And_Clear
                 (View.Tree.Model, Iter2,
                  (File_Column, Display_Name_Column),
                  (1 => As_File   (File),
                   2 => As_String (File.Display_Base_Dir_Name)));

               View.Tree.Set_Node_Type (Iter2, Directory_Node, False);
               File_Append_Directory (View.Tree, File, Iter2, Idle => False);

            else
               Iter := View.Tree.Create_File (Iter, File);
            end if;

            Ignore := Expand_Row (View.Tree, Path, False);
            Path_Free (Path);

            return;
         end if;

         --  We look through the tree: first dir node, then children,
         --  then parent's next item.
         if Has_Child (View.Tree.Model, Iter) then
            Iter := Children (View.Tree.Model, Iter);

         else
            loop
               Next_Iter := Iter;
               Next (View.Tree.Model, Next_Iter);

               if Next_Iter = Null_Iter then
                  Iter := Parent (View.Tree.Model, Iter);
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
      pragma Unreferenced (Kernel);
      Config : Files_View_Config;
   begin
      if Self.Explorer /= null then
         Set_Font_And_Colors
           (Self.Explorer.Tree, Fixed_Font => True, Pref => Pref);

         Config :=
           (Shows_Only_Project   => File_View_Shows_Only_Project.Get_Pref,
            Dirs_From_Project    => Dirs_From_Project.Get_Pref,
            Show_Hidden_Files    => Show_Hidden_Files.Get_Pref,
            Hidden_Files_Pattern =>
              To_Unbounded_String (Hidden_Files_Pattern.Get_Pref));

         if Config /= Self.Explorer.Tree.Config then
            Self.Explorer.Tree.Config := Config;
            Refresh (Self.Explorer);
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
         Category  => -"Files view",
         Icon_Name => "gps-refresh-symbolic");

      Register_Action
        (Kernel, "Locate in Files view",
         Command => new Locate_File_In_Files_View,
         Description =>
           -("Display the files view, and expand nodes to show the"
             & " selected file"),
         Filter      => Kernel.Lookup_Filter ("File")
            and not File_View_Filter,
         Category    => -"Files view");
   end Register_Module;

end Project_Explorers_Files;
