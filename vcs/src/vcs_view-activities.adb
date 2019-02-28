------------------------------------------------------------------------------
--                                  G P S                                   --
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

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.VFS.GtkAda;       use GNATCOLL.VFS.GtkAda;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GUI_Utils;                 use GUI_Utils;
with Gdk;
with Glib.Main;                 use Glib.Main;
with Glib.Values;               use Glib.Values;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Handlers;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Separator_Menu_Item;   use Gtk.Separator_Menu_Item;
with Gtk;                       use Gtk;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;
with Log_Utils;                 use Log_Utils;
with String_List_Utils;         use String_List_Utils;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Module;                use VCS_Module;
with VCS_Utils;                 use VCS_Utils;

package body VCS_View.Activities is

   ---------------------
   -- Local constants --
   ---------------------

   package Activities_Idle is new Glib.Main.Generic_Sources
     (VCS_Activities_View_Access);
   use Activities_Idle;
   function Start_Editing_Idle
     (Explorer : VCS_Activities_View_Access) return Boolean;
   --  Function called to start editing the selected line. This is necessary
   --  since any editing is stopped as soon as the tree gains the focus back.

   type Kernel_And_Int is record
      Kernel : Kernel_Handle;
      Value : Glib.Gint;
   end record;

   package Kernel_And_Int_Callback is new Gtk.Handlers.User_Callback
     (Glib.Object.GObject_Record, Kernel_And_Int);

   -------------------
   -- Columns_Types --
   -------------------

   overriding function Columns_Types
     (Explorer : access VCS_Activities_View_Record) return GType_Array
   is
      pragma Unreferenced (Explorer);
   begin
      return GType_Array'
        (Base_Name_Column          => GType_String,
         Name_Column               => GType_String,
         File_Column               => Get_Virtual_File_Type,
         Key_Column                => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Icon_Name_Column   => GType_String,
         Has_Log_Column            => GType_Boolean,
         Activity_Column           => GType_String,
         Control_Column            => GType_Boolean);
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Column_Types
     (Explorer : access VCS_Activities_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   function Get_Iter_From_Activity
     (Explorer : access VCS_Activities_View_Record'Class;
      Activity : Activity_Id) return Gtk_Tree_Iter;
   --  Get the Iter associated with the given activity.
   --  Return Null_Iter if no such iter was found.

   ---------------
   -- Callbacks --
   ---------------

   procedure Edited_Callback
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Kernel_And_Int);
   --  Called for the edited activity cells

   type On_File_Edited is new File_Hooks_Function with record
      Explorer : VCS_Activities_View_Access;
   end record;
   overriding procedure Execute
     (Self      : On_File_Edited;
      Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File);
   --  Callback for the "file_edited" signal

   procedure Contextual_Menu_Factory
     (Context : Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu);
   --  Default context factory

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Object : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Kernel_And_Int)
   is
      pragma Unreferenced (Object);
      K           : constant Kernel_Handle := Data.Kernel;
      Explorer    : constant VCS_Activities_View_Access :=
                      Get_Activities_Explorer (K, False, False);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Text_Value  : constant GValue := Nth (Params, 2);
      Iter        : Gtk_Tree_Iter;
      Activity    : Activity_Id;
   begin
      Iter := Get_Iter_From_String (Explorer.Model, Path_String);

      Activity := Value
        (Get_String (Explorer.Model, Iter, Activity_Column));

      --  Set value in the model

      Set_Value (Explorer.Model, Iter, Data.Value, Text_Value);

      --  Set value in the activities registry and save it

      Set_Name (K, Activity, Get_String (Text_Value));
      Save_Activities (K);

      Refresh (Get_Explorer (K, False, False));
   end Edited_Callback;

   ---------------
   -- Do_Delete --
   ---------------

   overriding procedure Do_Delete (Explorer : VCS_Activities_View_Record) is
      pragma Unreferenced (Explorer);
   begin
      Hide_VCS_Activities_Explorer;
   end Do_Delete;

   ----------------
   -- Do_Refresh --
   ----------------

   overriding procedure Do_Refresh
     (Explorer : access VCS_Activities_View_Record) is
   begin
      Query_Activities_Files
        (VCS_Activities_View_Access (Explorer), Explorer.Kernel, False);
   end Do_Refresh;

   ------------------------
   -- Start_Editing_Idle --
   ------------------------

   function Start_Editing_Idle
     (Explorer : VCS_Activities_View_Access) return Boolean
   is
      Path : Gtk_Tree_Path;
   begin
      if Explorer.Iter /= Null_Iter then
         Path := Get_Path (Explorer.Model, Explorer.Iter);
         Set_Cursor
           (Explorer.Tree,
            Path          => Path,
            Focus_Column  => Get_Column (Explorer.Tree, 0),
            Start_Editing => True);
         Path_Free (Path);
      end if;
      return False;
   end Start_Editing_Idle;

   ------------------------
   -- On_Create_Activity --
   ------------------------

   procedure On_Create_Activity (Kernel : Kernel_Handle) is
      Explorer : constant VCS_Activities_View_Access :=
                   Get_Activities_Explorer (Kernel, False, False);
      Activity : Activity_Id;
      Iter     : Gtk_Tree_Iter;
      Id       : G_Source_Id;
      pragma Unreferenced (Id);

   begin
      Activity := New_Activity (Kernel);

      Append (Explorer.Model, Iter, Null_Iter);

      Set_And_Clear
        (Explorer.Model, Iter,
         (Base_Name_Column, Activity_Column, Control_Column, Has_Log_Column),
         (1 => As_String  (Get_Name (Activity)),
          2 => As_String  (Image (Activity)),
          3 => As_Boolean (True),
          4 => As_Boolean (False)));

      Explorer.Iter := Iter;

      Id := Idle_Add (Start_Editing_Idle'Access, Explorer,
                      Priority => Priority_High_Idle);
   end On_Create_Activity;

   ------------------------
   -- On_Delete_Activity --
   ------------------------

   procedure On_Delete_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Explorer   : constant VCS_Activities_View_Access :=
                     Get_Activities_Explorer (Kernel, False, False);
      Log_File   : constant Virtual_File :=
                     Get_Log_File (Kernel, Activity);
      File_Count : constant Natural := Get_Files_In_Activity (Activity)'Length;
      Iter       : Gtk_Tree_Iter;
      Button     : Message_Dialog_Buttons := Button_OK;
   begin
      if File_Count > 0 then
         Button := GPS_Message_Dialog
           (Msg     =>
              (-"Activity") & " '" & Get_Name (Activity) & ''' & ASCII.LF &
               (-"will be deleted") & ASCII.LF,
            Dialog_Type => Warning,
            Title       => -"Delete Activity",
            Buttons     => Button_OK + Button_Cancel,
            Parent      => Kernel.Get_Main_Window);
      end if;

      if Button = Button_OK then
         Open_File_Action_Hook.Run
            (Kernel, Log_File, Project => No_Project,
             Line => -1);  --  close all editors
         Delete_Activity (Kernel, Activity);
         Iter := Get_Iter_From_Activity (Explorer, Activity);
         Remove (Explorer.Model, Iter);
         Refresh (Get_Explorer (Kernel, False, False));
      end if;
   end On_Delete_Activity;

   ----------------------------
   -- On_Close_Open_Activity --
   ----------------------------

   procedure On_Close_Open_Activity
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Activity : Activity_Id)
   is
      Closed : constant Boolean := Is_Closed (Activity);
      Ok     : Boolean := True;

   begin
      --  Before reopening an activity check that there is no file part of this
      --  activity that are already into an open activity.

      if Closed then
         declare
            Files : File_Array renames Get_Files_In_Activity (Activity);
         begin

            for J in Files'Range loop
               declare
                  File       : Virtual_File renames Files (J);
                  F_Activity : Activity_Id;
                  Button     : Message_Dialog_Buttons := Button_OK;
                  pragma Unreferenced (Button);
               begin
                  F_Activity := Get_File_Activity (File);
                  if F_Activity /= No_Activity then
                     Button := GPS_Message_Dialog
                       (Msg         =>
                          (-"Activity") & ''' & Get_Name (Activity) & ''' &
                        (-"can't be re-opened") & ASCII.LF &
                        (-"file ") & Display_Base_Name (File) &
                        (-" is part of activity '") & Get_Name (F_Activity) &
                        ''' & ASCII.LF,
                        Dialog_Type => Warning,
                        Title       => -"Open Activity",
                        Buttons     => Button_OK,
                        Parent      => Kernel.Get_Main_Window);
                     --  Revert back the status to its current setting
                     Ok := False;
                     exit;
                  end if;
               end;
            end loop;
         end;
      end if;

      if Ok then
         Toggle_Closed_Status (Kernel, Activity);
      end if;

      Refresh (Get_Activities_Explorer (Kernel, False, False));
   end On_Close_Open_Activity;

   -----------------------------
   -- On_Remove_From_Activity --
   -----------------------------

   procedure On_Remove_From_Activity
     (Kernel : Kernel_Handle; Activity : Activity_Id; File : Virtual_File)
   is
      Explorer : constant VCS_Activities_View_Access :=
                   Get_Activities_Explorer (Kernel, False, False);
      Iter     : Gtk_Tree_Iter;
   begin
      Remove_File (Kernel, Activity, File);

      Iter := Get_Iter_From_File
        (Explorer, File, Get_Iter_From_Activity (Explorer, Activity));
      Remove (Explorer.Model, Iter);

      Refresh (Get_Explorer (Kernel, False, False));
   end On_Remove_From_Activity;

   -------------------------
   -- Display_File_Status --
   -------------------------

   procedure Display_File_Status
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Activity       : Activity_Id;
      Status         : File_Status_List.Vector;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True)
   is
      Explorer     : constant VCS_Activities_View_Access :=
        Get_Activities_Explorer (Kernel, False, False);
      Root_Project : constant Virtual_File :=
        Get_Registry (Kernel).Tree.Root_Project.Project_Path;

      function Get_Activity_Iter (File : Virtual_File) return Gtk_Tree_Iter;
      --  Return the activity tree iter for the given file. Use Activity if set
      --  otherwise we use File to get the corresponding activity.

      function Create_Or_Remove_Activity
        (Activity : Activity_Id) return Gtk_Tree_Iter;
      --  This routine creates or removes an activity from the explorer. The
      --  activity is created if part of the root project and removed
      --  otherwise. It returns the tree iterator for the newly created
      --  activity or Null_Iter otherwise.

      procedure Push (Node : Gtk_Tree_Iter);
      --  Add Node into the list of node to expand. Does nothing if there is no
      --  more space in the table.

      procedure Expand_Nodes;
      --  Expand nodes found in To_Expand tree

      To_Expand : array (1 .. 64) of Gtk_Tree_Iter;
      --  A table of node to expand. The hard-coded limit is large enough to
      --  handle most cases and past this limit the nodes are not handled. This
      --  means that the node won't be expanded by default. Note that this will
      --  happen only when redrawing the Activites Explorer which is rare.

      Index     : Natural := 0;

      ----------
      -- Push --
      ----------

      procedure Push (Node : Gtk_Tree_Iter) is
      begin
         if Index < To_Expand'Last then
            Index := Index + 1;
            To_Expand (Index) := Node;
         end if;
      end Push;

      ------------------
      -- Expand_Nodes --
      ------------------

      procedure Expand_Nodes is
         Path   : Gtk_Tree_Path;
         Result : Boolean;
         pragma Unreferenced (Result);
      begin
         for K in 1 .. Index loop
            Path := Get_Path (Explorer.Model, To_Expand (Index));
            Result := Expand_Row (Explorer.Tree, Path, False);
            Path_Free (Path);
         end loop;
      end Expand_Nodes;

      -------------------------------
      -- Create_Or_Remove_Activity --
      -------------------------------

      function Create_Or_Remove_Activity
        (Activity : Activity_Id) return Gtk_Tree_Iter
      is
         A_Iter : Gtk_Tree_Iter;

         Values  : Glib.Values.GValue_Array (1 .. 4);
         Columns : Columns_Array (Values'Range);
         Last    : Gint := 0;

      begin
         A_Iter := Get_Iter_From_Activity (Explorer, Activity);

         if Root_Project = Get_Project_Path (Activity) then
            --  This activity belong to this project, create it if needed

            if A_Iter = Null_Iter then
               Append (Explorer.Model, A_Iter, Null_Iter);

               Columns (1 .. 2) := (Activity_Column, Has_Log_Column);
               Values  (1 .. 2) :=
                 (1 => As_String  (Image (Activity)),
                  2 => As_Boolean (Has_Log (Kernel, Activity)));
               Last := 2;

               if not Is_Closed (Activity) then
                  Push (A_Iter);
               end if;
            end if;

            Last := Last + 2;
            Columns (Last - 1 .. Last) := (Base_Name_Column, Control_Column);

            if Is_Closed (Activity) then
               Values (Last - 1 .. Last) :=
                 (As_String (Get_Name (Activity) & " (closed)"),
                  As_Boolean (False));

            else
               Values (Last - 1 .. Last) :=
                 (As_String (Get_Name (Activity)), As_Boolean (True));
            end if;

            Set_And_Clear
              (Explorer.Model, A_Iter,
               Columns (1 .. Last), Values (1 .. Last));

            return A_Iter;

         else
            --  This activity does not belong to this project, remove it if
            --  present in the current VCS Activities explorer.

            if A_Iter /= Null_Iter then
               Remove (Explorer.Model, A_Iter);
            end if;

            return Null_Iter;
         end if;
      end Create_Or_Remove_Activity;

      -----------------------
      -- Get_Activity_Iter --
      -----------------------

      function Get_Activity_Iter (File : Virtual_File) return Gtk_Tree_Iter is
         A : Activity_Id := Activity;
      begin
         if A = No_Activity then
            --  Activity is not specified, compute it using File
            A := Get_File_Activity (File);

            if A = No_Activity then
               --  This file is not part of the activity either, return now
               return Null_Iter;
            end if;
         end if;

         return Create_Or_Remove_Activity (A);
      end Get_Activity_Iter;

      Log               : Boolean;
      Line              : Line_Record;
      Sort_Id           : Gint;
      Up_To_Date_Status : VCS_File_Status;

   begin
      if VCS_Identifier /= null then
         declare
            Registered_Status : constant VCS.Status_Array :=
                                  Get_Registered_Status (VCS_Identifier);
         begin
            if Registered_Status'Length >= 2 then
               Up_To_Date_Status :=
                 Registered_Status (Registered_Status'First + 1);
            end if;
         end;
      end if;

      Update_Files_Status
        (Kernel, Status, VCS_Identifier, Clear_Logs, Up_To_Date_Status);

      if Explorer = null then
         return;
      end if;

      Sort_Id := Freeze_Sort (Explorer.Model);

      --  If activity is given, create the tree node now if needed

      if Activity /= No_Activity then
         declare
            A_Iter : Gtk_Tree_Iter;
            pragma Unreferenced (A_Iter);
         begin
            A_Iter := Create_Or_Remove_Activity (Activity);
         end;
      end if;

      --  Handle each files

      for Item of Status loop
         declare
            File   : constant Virtual_File  := Item.File;
            A_Iter : constant Gtk_Tree_Iter := Get_Activity_Iter (File);
         begin
            if A_Iter /= Null_Iter then

               Line := Get_Cache (Get_Status_Cache, File);

               if Line = No_Data or else Override_Cache then
                  Log := Get_Log_From_File
                    (Kernel, File, False) /= GNATCOLL.VFS.No_File;

                  Line := (Copy_File_Status (Item), Log);
                  Set_Cache (Get_Status_Cache, File, Line);
               end if;

               --  The info that we want to display is now in Line

               if Display then
                  declare
                     Iter    : Gtk_Tree_Iter := Null_Iter;
                     Success : Boolean;
                  begin
                     Iter := Get_Iter_From_File
                       (Explorer, Line.Status.File, A_Iter);

                     if Iter = Null_Iter and then Force_Display then
                        Append (Explorer.Model, Iter, A_Iter);

                        if not Is_Closed (Activity) then
                           Push (A_Iter);
                        end if;
                     end if;

                     if Iter /= Null_Iter then
                        Fill_Info (Explorer, Iter, Line, Success);
                     end if;
                  end;
               end if;
            end if;
         end;
      end loop;

      Expand_Nodes;

      Thaw_Sort (Explorer.Model, Sort_Id);
   end Display_File_Status;

   ------------------
   -- Do_Fill_Info --
   ------------------

   overriding procedure Do_Fill_Info
     (Explorer  : VCS_Activities_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean)
   is
      pragma Unreferenced (Line_Info);
   begin
      Explorer.Model.Set (Iter, Control_Column, False);
      Success := True;
   end Do_Fill_Info;

   ----------------------------
   -- Get_Iter_From_Activity --
   ----------------------------

   function Get_Iter_From_Activity
     (Explorer : access VCS_Activities_View_Record'Class;
      Activity : Activity_Id) return Gtk_Tree_Iter
   is
      Activity_Id : constant String := Image (Activity);
   begin
      return Get_Iter_For_Root_Node (Explorer, Activity_Column, Activity_Id);
   end Get_Iter_From_Activity;

   ----------------------
   -- Set_Column_Types --
   ----------------------

   procedure Set_Column_Types
     (Explorer : access VCS_Activities_View_Record'Class)
   is
      Col         : Gtk_Tree_View_Column;
      Text_Rend   : Gtk_Cell_Renderer_Text;
      Edit_Rend   : Gtk_Cell_Renderer_Text;
      Pixbuf_Rend : Gtk_Cell_Renderer_Pixbuf;
      Toggle_Rend : Gtk_Cell_Renderer_Toggle;
      Dummy       : Gint;
      pragma Unreferenced (Dummy);

   begin
      Gtk_New (Text_Rend);
      Gtk_New (Pixbuf_Rend);
      Gtk_New (Toggle_Rend);
      Gtk_New (Edit_Rend);

      Set_Rules_Hint (Explorer.Tree, True);

      Kernel_And_Int_Callback.Connect
        (Edit_Rend, Signal_Edited, Edited_Callback'Access,
         User_Data => (Explorer.Kernel, Base_Name_Column));

      Gtk_New (Explorer.File_Column);
      Set_Title (Explorer.File_Column, -"Activity / File");
      Pack_Start (Explorer.File_Column, Edit_Rend, True);
      Add_Attribute
        (Explorer.File_Column, Edit_Rend, "editable", Control_Column);
      Add_Attribute
        (Explorer.File_Column, Edit_Rend, "text", Base_Name_Column);
      Set_Clickable (Explorer.File_Column, True);
      Set_Sort_Column_Id (Explorer.File_Column, Base_Name_Column);
      Set_Resizable (Explorer.File_Column, True);
      Dummy := Append_Column (Explorer.Tree, Explorer.File_Column);

      Gtk_New (Explorer.Status_Column);
      Set_Title (Explorer.Status_Column, -"Status");
      Pack_Start (Explorer.Status_Column, Pixbuf_Rend, False);
      Add_Attribute
        (Explorer.Status_Column, Pixbuf_Rend,
         "icon-name", Status_Icon_Name_Column);
      Set_Clickable (Explorer.Status_Column, True);
      Set_Sort_Column_Id (Explorer.Status_Column, Status_Description_Column);
      Dummy := Append_Column (Explorer.Tree, Explorer.Status_Column);

      Gtk_New (Explorer.Log_Column);
      Set_Title (Explorer.Log_Column, -"Log");
      Pack_Start (Explorer.Log_Column, Toggle_Rend, False);
      Add_Attribute
        (Explorer.Log_Column, Toggle_Rend, "active", Has_Log_Column);
      Set_Clickable (Explorer.Log_Column, False);
      Dummy := Append_Column (Explorer.Tree, Explorer.Log_Column);

      Gtk_New (Col);
      Set_Title (Col, -"Working rev.");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Local_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);

      Gtk_New (Col);
      Set_Title (Col, -"Head rev.");
      Pack_Start (Col, Text_Rend, True);
      Add_Attribute (Col, Text_Rend, "text", Rep_Rev_Column);
      Dummy := Append_Column (Explorer.Tree, Col);
   end Set_Column_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Explorer : out VCS_Activities_View_Access;
      Kernel   : access Kernel_Handle_Record'Class := null) is
   begin
      Explorer := new VCS_Activities_View_Record;
      Initialize (Explorer, Kernel);
      Set_Name (Explorer.Tree, "Activities Explorer Tree");
   end Gtk_New;

   ------------------------
   -- Build_View_Context --
   ------------------------

   overriding function Build_View_Context
     (Explorer : not null access VCS_Activities_View_Record;
      Event : Gdk.Event.Gdk_Event)
      return Selection_Context
   is
      function Get_Selected_Activities
        (Explorer : VCS_View_Access) return String_List.Vector;
      --  Return the list of activities that are selected

      -----------------------------
      -- Get_Selected_Activities --
      -----------------------------

      function Get_Selected_Activities
        (Explorer : VCS_View_Access) return String_List.Vector
      is
         Result : String_List.Vector;

         procedure Add_Selected_Item
           (Model : Gtk.Tree_Model.Gtk_Tree_Model;
            Path  : Gtk.Tree_Model.Gtk_Tree_Path;
            Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);
         --  Add an item to Result

         -----------------------
         -- Add_Selected_Item --
         -----------------------

         procedure Add_Selected_Item
           (Model : Gtk.Tree_Model.Gtk_Tree_Model;
            Path  : Gtk.Tree_Model.Gtk_Tree_Path;
            Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
         is
            pragma Unreferenced (Model, Path);
         begin
            if Parent (Explorer.Model, Iter) = Null_Iter then
               --  Take root nodes, those are the activity name

               String_List.Append
                 (Result, Get_String (Explorer.Model, Iter, Activity_Column));
            end if;
         end Add_Selected_Item;

      begin
         if Explorer = null then
            return Result;
         end if;

         Explorer.Tree.Get_Selection.Selected_Foreach
           (Add_Selected_Item'Unrestricted_Access);
         return Result;
      end Get_Selected_Activities;

      Child : constant MDI_Child := Find_MDI_Child_From_Widget (Explorer);
      Context  : Selection_Context :=
        GPS_MDI_Child_Record (Child.all).Build_Context (Event);
      Files : File_Array_Access;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;
   begin
      --  If there is no selection, select the item under the cursor

      Iter := Find_Iter_For_Event (Explorer.Tree, Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Get_Model (Explorer.Tree), Iter);

         if Event /= null
           and then not Path_Is_Selected (Get_Selection (Explorer.Tree), Path)
         then
            --  Right click over a line which is not the current selection,
            --  this line becomes the new selection.
            Unselect_All (Get_Selection (Explorer.Tree));
            Select_Path (Get_Selection (Explorer.Tree), Path);
         end if;

         --  If Get_Depth (Path) is 1 then we are on an activity node

         if Get_Depth (Path) = 1 then
            Set_Activity_Information
              (Context,
               Get_Selected_Activities (VCS_View_Access (Explorer)));

         elsif Get_Depth (Path) > 1 then
            --  Here we are on a file node

            Files := Get_Selected_Files (VCS_View_Access (Explorer));

            Iter := Parent (Explorer.Model, Get_Iter (Explorer.Model, Path));

            Set_Activity_Information
              (Context,
               Get_String (Explorer.Model, Iter, Activity_Column));
            Set_File_Information (Context, Files => Files.all);

            Unchecked_Free (Files);
         end if;

         Path_Free (Path);
      end if;

      return Context;
   end Build_View_Context;

   -----------------------------
   -- Contextual_Menu_Factory --
   -----------------------------

   procedure Contextual_Menu_Factory
     (Context : Selection_Context;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      Mitem    : Gtk_Separator_Menu_Item;
   begin
      VCS_Activities_Contextual_Menu (Context, Menu);

      if Has_File_Information (Context) then
         --  This is a menu for a file line in the tree view, it is fine to add
         --  a separator.
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;
   end Contextual_Menu_Factory;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self      : On_File_Edited;
      Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File)
   is
      Line     : Line_Record;
   begin
      if Has_Suffix (File, "$log") then
         declare
            File_Name : constant Filesystem_String :=
                          Base_Name (File, "$log");
            Activity  : constant Activity_Id := Value (+File_Name);
         begin
            if Activity = No_Activity then
               --  This is a file

               declare
                  F : constant Virtual_File :=
                           Get_File_From_Log (Kernel, File);
               begin
                  Line := Get_Cache (Get_Status_Cache, F);
                  if Line /= No_Data then
                     Line.Log := True;
                     Set_Cache (Get_Status_Cache, F, Line);
                     Refresh (Self.Explorer);
                  end if;
               end;

            else
               --  This is an activity

               declare
                  Iter : constant Gtk_Tree_Iter :=
                    Get_Iter_From_Activity (Self.Explorer, Activity);
               begin
                  Self.Explorer.Model.Set (Iter, Has_Log_Column, True);
               end;
               Refresh (Self.Explorer);
            end if;
         end;
      end if;
   end Execute;

   -------------------
   -- Do_Initialize --
   -------------------

   overriding procedure Do_Initialize
     (Explorer : access VCS_Activities_View_Record;
      Kernel   : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
   begin
      Setup_Contextual_Menu
        (Explorer.Kernel,
         Explorer.Tree,
         Context_Func => Contextual_Menu_Factory'Access);

      Set_Column_Types (Explorer);

      File_Edited_Hook.Add
         (new On_File_Edited'
             (File_Hooks_Function with
              Explorer => VCS_Activities_View_Access (Explorer)),
          Watch => Explorer);
   end Do_Initialize;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return File_Array_Access is
   begin
      return Get_Selected_Files
        (VCS_View_Access (Get_Activities_Explorer (Kernel, False, False)));
   end Get_Selected_Files;

end VCS_View.Activities;
