-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;            use Ada.Exceptions;

with Gdk;
with Gdk.Pixbuf;                use Gdk.Pixbuf;

with Glib.Values;               use Glib.Values;

with Gtk;                       use Gtk;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;

with Gtkada.Dialogs;            use Gtkada.Dialogs;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GUI_Utils;                 use GUI_Utils;
with Log_Utils;                 use Log_Utils;
with Projects.Registry;         use Projects.Registry;
with Projects;                  use Projects;
with Traces;                    use Traces;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Module;                use VCS_Module;
with VCS_Utils;                 use VCS_Utils;

package body VCS_View.Activities is

   ---------------------
   -- Local constants --
   ---------------------

   package Activities_Idle is new Gtk.Main.Idle (VCS_Activities_View_Access);
   use Activities_Idle;
   function Start_Editing_Idle
     (Explorer : VCS_Activities_View_Access) return Boolean;
   --  Function called to start editing the selected line. This is necessary
   --  since any editing is stopped as soon as the tree gains the focus back.

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types
     (Explorer : access VCS_Activities_View_Record) return GType_Array
   is
      pragma Unreferenced (Explorer);
   begin
      return GType_Array'
        (Base_Name_Column          => GType_String,
         Name_Column               => GType_String,
         Key_Column                => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type,
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
     (Kernel : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Called for the edited activity cells

   type File_Hook_Record is new Function_With_Args with record
      Explorer : VCS_Activities_View_Access;
   end record;
   type File_Hook is access all File_Hook_Record'Class;
   procedure Execute
     (Hook      : File_Hook_Record;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Data : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);
   --  Default context factory

   ---------------------
   -- Edited_Callback --
   ---------------------

   procedure Edited_Callback
     (Kernel : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint)
   is
      K           : constant Kernel_Handle := Kernel_Handle (Kernel);
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

      Set_Value (Explorer.Model, Iter, Data, Text_Value);

      --  Set value in the activities registry and save it

      Set_Name (Activity, Get_String (Text_Value));
      Save_Activities (K);

      Refresh (Get_Explorer (K, False, False));

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Edited_Callback;

   ---------------
   -- Do_Delete --
   ---------------

   procedure Do_Delete (Explorer : VCS_Activities_View_Record) is
      pragma Unreferenced (Explorer);
   begin
      Hide_VCS_Activities_Explorer;
   end Do_Delete;

   ----------------
   -- Do_Refresh --
   ----------------

   procedure Do_Refresh (Explorer : access VCS_Activities_View_Record) is
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
      Id       : Idle_Handler_Id;
      pragma Unreferenced (Id);
   begin
      Activity := New_Activity (Kernel);

      Append (Explorer.Model, Iter, Null_Iter);

      Set (Explorer.Model, Iter, Base_Name_Column, Get_Name (Activity));
      Set (Explorer.Model, Iter, Activity_Column, Image (Activity));
      Set (Explorer.Model, Iter, Control_Column, True);
      Set (Explorer.Model, Iter, Has_Log_Column, False);

      Explorer.Iter := Iter;

      Id := Add (Start_Editing_Idle'Access, Explorer,
                 Priority => Priority_High_Idle);
   end On_Create_Activity;

   ------------------------
   -- On_Delete_Activity --
   ------------------------

   procedure On_Delete_Activity
     (Kernel : Kernel_Handle; Activity : Activity_Id)
   is
      Explorer   : constant VCS_Activities_View_Access :=
                     Get_Activities_Explorer (Kernel, False, False);
      Log_File   : constant Virtual_File :=
                     Get_Log_File (Kernel, Activity);
      File_Count : constant Natural :=
                     String_List.Length (Get_Files_In_Activity (Activity));
      Iter       : Gtk_Tree_Iter;
      Button     : Message_Dialog_Buttons := Button_OK;
   begin
      if File_Count > 0 then
         Button := Message_Dialog
           (Msg     =>
              (-"Activity") & " '" & Get_Name (Activity) & ''' & ASCII.LF &
               (-"will be deleted") & ASCII.LF,
            Dialog_Type => Warning,
            Title       => -"Delete Activity",
            Buttons     => Button_OK + Button_Cancel);
      end if;

      if Button = Button_OK then
         Close_File_Editors (Kernel, Log_File);
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
     (Kernel : Kernel_Handle; Activity : Activity_Id)
   is
      use type String_List.List_Node;
      Closed : constant Boolean := Is_Closed (Activity);
      Ok     : Boolean := True;
      Files  : String_List.List;
      Iter   : String_List.List_Node;
   begin
      --  Before reopening an activity check that there is no file part of this
      --  activity that are already into an open activity.

      if Closed then
         Files := Get_Files_In_Activity (Activity);

         Iter := String_List.First (Files);

         while Iter /= String_List.Null_Node loop
            declare
               File       : constant Virtual_File :=
                              Create (String_List.Data (Iter));
               F_Activity : Activity_Id;
               Button     : Message_Dialog_Buttons := Button_OK;
               pragma Unreferenced (Button);
            begin
               F_Activity := Get_File_Activity (File);
               if F_Activity /= No_Activity then
                  Button := Message_Dialog
                    (Msg     =>
                       (-"Activity") & ''' & Get_Name (Activity) & ''' &
                     (-"can't be re-opened") & ASCII.LF &
                     (-"file ") & Base_Name (File) &
                     (-" is part of activity '") & Get_Name (F_Activity) &
                     ''' & ASCII.LF,
                     Dialog_Type => Warning,
                     Title       => -"Open Activity",
                     Buttons     => Button_OK);
                  --  Revert back the status to its current setting
                  Ok := False;
                  exit;
               end if;
            end;
            Iter := String_List.Next (Iter);
         end loop;
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
     (Kernel         : Kernel_Handle;
      Activity       : Activity_Id;
      Status         : File_Status_List.List;
      VCS_Identifier : VCS_Access;
      Override_Cache : Boolean;
      Force_Display  : Boolean := False;
      Clear_Logs     : Boolean := False;
      Display        : Boolean := True)
   is
      Explorer     : constant VCS_Activities_View_Access :=
        Get_Activities_Explorer (Kernel, False, False);
      Root_Project : constant Virtual_File :=
        Project_Path (Get_Root_Project (Get_Registry (Kernel).all));

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
      begin
         A_Iter := Get_Iter_From_Activity (Explorer, Activity);

         if Root_Project = Get_Project_Path (Activity) then
            --  This activity belong to this project, create it if needed

            if A_Iter = Null_Iter then
               Append (Explorer.Model, A_Iter, Null_Iter);
               Set (Explorer.Model, A_Iter, Activity_Column, Image (Activity));
               Set (Explorer.Model, A_Iter,
                    Has_Log_Column, Has_Log (Kernel, Activity));

               if not Is_Closed (Activity) then
                  Push (A_Iter);
               end if;
            end if;

            if Is_Closed (Activity) then
               Set (Explorer.Model, A_Iter,
                    Base_Name_Column, Get_Name (Activity) & " (closed)");
               Set (Explorer.Model, A_Iter, Control_Column, False);
            else
               Set (Explorer.Model, A_Iter,
                    Base_Name_Column, Get_Name (Activity));
               Set (Explorer.Model, A_Iter, Control_Column, True);
            end if;

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

      Status_Temp       : File_Status_List.List_Node;
      Log               : Boolean;
      Line              : Line_Record;
      Sort_Id           : Gint;
      Up_To_Date_Status : VCS.File_Status;

      use type File_Status_List.List_Node;
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

      Status_Temp := File_Status_List.First (Status);

      Push_State (Kernel, Busy);
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

      while Status_Temp /= File_Status_List.Null_Node loop
         declare
            File   : constant Virtual_File :=
                       File_Status_List.Data (Status_Temp).File;
            A_Iter : constant Gtk_Tree_Iter := Get_Activity_Iter (File);
         begin
            if A_Iter /= Null_Iter then

               Line := Get_Cache (Get_Status_Cache, File);

               if Line = No_Data or else Override_Cache then
                  Log := Get_Log_From_File
                    (Kernel, File, False) /= VFS.No_File;

                  Line :=
                    (Copy_File_Status
                       (File_Status_List.Data (Status_Temp)), Log);
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

            Status_Temp := File_Status_List.Next (Status_Temp);
         end;
      end loop;

      Expand_Nodes;

      Thaw_Sort (Explorer.Model, Sort_Id);
      Pop_State (Kernel);
   end Display_File_Status;

   ------------------------
   -- Fill_Info_Specific --
   ------------------------

   procedure Do_Fill_Info
     (Explorer  : VCS_Activities_View_Record;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean)
   is
      pragma Unreferenced (Line_Info);
   begin
      Set (Explorer.Model, Iter, Control_Column, False);
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

      Tree_Model_Callback.Object_Connect
        (Edit_Rend, "edited", Edited_Callback'Access,
         Slot_Object => Explorer.Kernel, User_Data => Base_Name_Column);

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
        (Explorer.Status_Column, Pixbuf_Rend, "pixbuf", Status_Pixbuf_Column);
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
      Kernel   : Kernel_Handle := null) is
   begin
      Explorer := new VCS_Activities_View_Record;
      Initialize (Explorer, Kernel);
   end Gtk_New;

   ------------------
   -- Context_Func --
   ------------------

   procedure Context_Func
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget);

      Explorer : constant VCS_Activities_View_Access :=
                   VCS_Activities_View_Access (Object);
      Mitem    : Gtk_Menu_Item;
      Files    : String_List.List;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

   begin
      --  If there is no selection, select the item under the cursor

      Iter := Find_Iter_For_Event
        (Explorer.Tree, Get_Model (Explorer.Tree), Event);

      if Iter /= Null_Iter then
         Path := Get_Path (Get_Model (Explorer.Tree), Iter);

         if not Path_Is_Selected (Get_Selection (Explorer.Tree), Path) then
            --  Right click over a line which is not the current selection,
            --  this line becomes the new selection.
            Unselect_All (Get_Selection (Explorer.Tree));
            Select_Path (Get_Selection (Explorer.Tree), Path);
         end if;

         --  If Get_Depth (Path) is 1 then we are on an activity node

         if Get_Depth (Path) = 1 then
            Iter := Get_Iter (Explorer.Model, Path);

            Set_Activity_Information
              (Context, Get_String (Explorer.Model, Iter, Activity_Column));

         elsif Get_Depth (Path) > 1 then
            Files := Get_Selected_Files (VCS_View_Access (Explorer));

            declare
               First_File : constant Virtual_File := Create
                 (Full_Filename => String_List.Head (Files));
            begin
               Iter :=
                 Parent (Explorer.Model, Get_Iter (Explorer.Model, Path));

               Set_Activity_Information
                 (Context,
                  Get_String (Explorer.Model, Iter, Activity_Column));
               Set_File_Information (Context, File => First_File);
            end;

            String_List.Free (Files);
         end if;

         Path_Free (Path);
      end if;

      Set_Context_Information
        (Context, Kernel, Abstract_Module_ID (VCS_Module_ID));
      Set_Current_Context (Explorer, Context);

      VCS_Activities_Contextual_Menu (Kernel_Handle (Kernel), Context, Menu);

      if Has_File_Information (Context) then
         --  This is a menu for a file line in the tree view, it is fine to add
         --  a separator.
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Context_Func;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook      : File_Hook_Record;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Data : access Hooks_Data'Class)
   is
      D        : constant File_Hooks_Args := File_Hooks_Args (File_Data.all);
      Log_Name : constant String := Base_Name (D.File);
      Line     : Line_Record;
   begin
      if Log_Name'Length > 4
        and then Log_Name (Log_Name'Last - 3 .. Log_Name'Last) = "$log"
      then
         declare
            File_Name : constant String :=
                          Log_Name (Log_Name'First .. Log_Name'Last - 4);
            Activity  : constant Activity_Id :=
                          Get_Activity_From_Name (File_Name);
         begin
            if Activity = No_Activity then
               --  This is a file

               declare
                  File : constant Virtual_File :=
                           Get_File_From_Log (Kernel, D.File);
               begin
                  Line := Get_Cache (Get_Status_Cache, File);

                  if Line /= No_Data then
                     Line.Log := True;
                     Set_Cache (Get_Status_Cache, File, Line);
                     Refresh (Hook.Explorer);
                  end if;
               end;

            else
               --  This is an activity

               declare
                  Iter : constant Gtk_Tree_Iter :=
                           Get_Iter_From_Activity (Hook.Explorer, Activity);
               begin
                  Set (Hook.Explorer.Model, Iter, Has_Log_Column, True);
               end;
               Refresh (Hook.Explorer);
            end if;
         end;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Do_Initialize
     (Explorer : access VCS_Activities_View_Record;
      Kernel   : Kernel_Handle)
   is
      Hook : File_Hook;
   begin
      Register_Contextual_Menu
        (Explorer.Kernel,
         Explorer.Tree,
         Explorer,
         Module_ID (VCS_Module_ID),
         Context_Func'Access);

      Set_Column_Types (Explorer);

      Hook := new File_Hook_Record'
        (Function_With_Args with
         Explorer => VCS_Activities_View_Access (Explorer));
      Add_Hook (Kernel, File_Edited_Hook, Hook,
                Name => "vcs_activities.file_edited",
                Watch => GObject (Explorer));
   end Do_Initialize;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List is
   begin
      return Get_Selected_Files
        (VCS_View_Access (Get_Activities_Explorer (Kernel, False, False)));
   end Get_Selected_Files;

end VCS_View.Activities;
