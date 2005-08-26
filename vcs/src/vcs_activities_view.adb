-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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
with Ada.Characters.Handling;   use Ada.Characters.Handling;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Properties;           use Glib.Properties;
with Glib.Values;               use Glib.Values;

with Gdk;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gtk.Enums;
with Gdk.Pixmap;                use Gdk.Pixmap;
with Gdk.Rectangle;             use Gdk.Rectangle;
with Gdk.Pixbuf;                use Gdk.Pixbuf;
with Gdk.Types;                 use Gdk.Types;
with Gdk.Window;                use Gdk.Window;

with Gtk;                       use Gtk;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Toggle;  use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
with Gtk.Main;                  use Gtk.Main;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Widget;                use Gtk.Widget;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

with VCS;
with VCS_View_Pixmaps;          use VCS_View_Pixmaps;
with VCS_View_Pkg;              use VCS_View_Pkg;
with VCS_Activities_View_API;   use VCS_Activities_View_API;
with VCS_Module;                use VCS_Module;
with VCS_Utils;                 use VCS_Utils;

with Log_Utils;                 use Log_Utils;

with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                  use GPS.Intl;
with VFS;                       use VFS;
with Tooltips;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;

with Basic_Types;               use Basic_Types;
with Traces;                    use Traces;

with GUI_Utils;                 use GUI_Utils;
with File_Utils;                use File_Utils;

package body VCS_Activities_View is

   Me : constant Debug_Handle := Create ("VCS_ACTIVITIES_INTERFACE");

   type VCS_Tooltips is new Tooltips.Pixmap_Tooltips with record
      Explorer : VCS_Activities_View_Access;
   end record;
   type VCS_Tooltips_Access is access all VCS_Tooltips'Class;
   procedure Draw
     (Tooltip : access VCS_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle);
   --  See inherited documentation

   --------------------
   -- Local packages --
   --------------------

   package Explorer_Selection_Foreach is
     new Selection_Foreach (VCS_Activities_View_Record);

   ---------------------
   -- Local constants --
   ---------------------

   function Columns_Types return GType_Array;
   --  Returns the types for the columns in the Model.
   --  This is not implemented as
   --       Columns_Types : constant GType_Array ...
   --  because Gdk.Pixbuf.Get_Type cannot be called before
   --  Gtk.Main.Init.

   --  The following list must be synchronized with the array of types
   --  in Columns_Types.
   --  Base_Name_Column is the first column, because we want the built-in
   --  interactive search to be performed on the base name of the files.

   package Activities_Idle is new Gtk.Main.Idle (VCS_Activities_View_Access);
   use Activities_Idle;
   function Start_Editing_Idle
     (Explorer : VCS_Activities_View_Access) return Boolean;
   --  Function called to start editing the selected line. This is necessary
   --  since any editing is stopped as soon as the tree gains the focus back.

   Base_Name_Column          : constant := 0;
   Name_Column               : constant := 1;
   Local_Rev_Column          : constant := 2;
   Rep_Rev_Column            : constant := 3;
   Status_Description_Column : constant := 4;
   Status_Pixbuf_Column      : constant := 5;
   Has_Log_Column            : constant := 6;
   Activity_Id_Column        : constant := 7;

   -------------------
   -- Columns_Types --
   -------------------

   function Columns_Types return GType_Array is
   begin
      return GType_Array'
        (Base_Name_Column          => GType_String,
         Name_Column               => GType_String,
         Local_Rev_Column          => GType_String,
         Rep_Rev_Column            => GType_String,
         Status_Description_Column => GType_String,
         Status_Pixbuf_Column      => Gdk.Pixbuf.Get_Type,
         Has_Log_Column            => GType_Boolean,
         Activity_Id_Column        => GType_String);
   end Columns_Types;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Refresh (Explorer : VCS_Activities_View_Access);
   --  Redraw the files in the VCS Explorer

   procedure Create_Model (Explorer : access VCS_Activities_View_Record'Class);
   --  Creates the underlying tree model for VCS_View

   procedure Set_Column_Types
     (Explorer : access VCS_Activities_View_Record'Class);
   --  Sets the types of columns to be displayed in the tree_view

   procedure Fill_Info
     (Explorer  : access VCS_Activities_View_Record'Class;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean);
   --  Fills the tree info at the given Iter with values from
   --  Status_Record.
   --  Success tells whether the information has been filled or not.

   function Get_Iter_From_Name
     (Explorer : access VCS_Activities_View_Record'Class;
      Name     : VFS.Virtual_File) return Gtk_Tree_Iter;
   --  Return the Iter associated with the given name.
   --  Name is an absolute file name.
   --  Return Null_Iter if no such iter was found.

   function Get_Iter_From_Activity
     (Explorer : access VCS_Activities_View_Record'Class;
      Activity : Activity_Id) return Gtk_Tree_Iter;
   --  Get the Iter associated with the given activity.
   --  Return Null_Iter if no such iter was found.

   function Copy_Context
     (Context : Selection_Context_Access)
      return Selection_Context_Access;
   --  Copy the information in Context that are relevant to the explorer,
   --  and create a new context containing them.

   function Copy (X : Line_Record) return Line_Record;
   --  Return a deep copy of X

   procedure On_Selected (Explorer : access Gtk_Widget_Record'Class);
   --  Give the focus to the current page tree

   ---------------
   -- Callbacks --
   ---------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press" event

   procedure Edited_Callback
     (Kernel : access GObject_Record'Class;
      Params : Glib.Values.GValues;
      Data   : Glib.Gint);
   --  Called for the edited activity cells

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "delete_event" signal

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal, connected before

   type File_Hook_Record is new Hook_Args_Record with record
      Explorer : VCS_Activities_View_Access;
   end record;
   type File_Hook is access all File_Hook_Record'Class;
   procedure Execute
     (Hook      : File_Hook_Record;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Data : access Hooks_Data'Class);
   --  Callback for the "file_edited" signal

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access;
   --  Default context factory

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path;
   --  Return the path at which Event has occured.
   --  User must free memory associated to the returned path.

   function Get_Cached_Data
     (Explorer : access VCS_Activities_View_Record'Class;
      Index    : VFS.Virtual_File) return Line_Record;
   --  Get the cached data

   procedure Set_Cached_Data
     (Explorer : access VCS_Activities_View_Record'Class;
      Index    : VFS.Virtual_File;
      Data     : Line_Record);
   --  Set the cached data

   ---------------------
   -- Get_Cached_Data --
   ---------------------

   function Get_Cached_Data
     (Explorer : access VCS_Activities_View_Record'Class;
      Index    : VFS.Virtual_File) return Line_Record is
   begin
      return Copy (Status_Hash.Get (Explorer.Cached_Status, Index).Line);
   end Get_Cached_Data;

   ---------------------
   -- Set_Cached_Data --
   ---------------------

   procedure Set_Cached_Data
     (Explorer : access VCS_Activities_View_Record'Class;
      Index    : VFS.Virtual_File;
      Data     : Line_Record) is
   begin
      Status_Hash.Set (Explorer.Cached_Status, Index, (Line => Copy (Data)));
   end Set_Cached_Data;

   -----------------
   -- On_Selected --
   -----------------

   procedure On_Selected (Explorer : access Gtk_Widget_Record'Class) is
      View : constant VCS_Activities_View_Access :=
               VCS_Activities_View_Access (Explorer);
   begin
      Grab_Focus (View.Tree);
   end On_Selected;

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
        (Get_String (Explorer.Model, Iter, Activity_Id_Column));

      --  Set value in the model

      Set_Value (Explorer.Model, Iter, Data, Text_Value);

      --  Set value in the activities registry and save it

      Set_Name (Activity, Get_String (Text_Value));
      Save_Activities (K);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Edited_Callback;

   ----------
   -- Draw --
   ----------

   procedure Draw
     (Tooltip : access VCS_Tooltips;
      Pixmap  : out Gdk.Pixmap.Gdk_Pixmap;
      Area    : out Gdk.Rectangle.Gdk_Rectangle)
   is
      Window     : Gdk.Window.Gdk_Window;
      New_Window : Gdk_Window;
      Mask       : Gdk_Modifier_Type;

      X, Y       : Gint;
      Path       : Gtk_Tree_Path;
      Column     : Gtk_Tree_View_Column;
      Cell_X,
      Cell_Y     : Gint;
      Row_Found  : Boolean;
      Iter       : Gtk_Tree_Iter;

      Text       : String_Access;
   begin
      Window := Get_Bin_Window (Tooltip.Explorer.Tree);
      Get_Pointer (Window, X, Y, Mask, New_Window);

      Get_Path_At_Pos
        (Tooltip.Explorer.Tree, X, Y, Path, Column, Cell_X, Cell_Y, Row_Found);

      if not Row_Found then
         return;
      end if;

      Get_Cell_Area (Tooltip.Explorer.Tree, Path, Column, Area);
      Iter := Get_Iter (Tooltip.Explorer.Model, Path);
      Path_Free (Path);

      if Column = Tooltip.Explorer.Status_Column then
         Text := new String'
           (-"Status: "
            & Get_String
              (Tooltip.Explorer.Model, Iter, Status_Description_Column));

      elsif Column = Tooltip.Explorer.Log_Column then
         --  ??? check if we are on an activity line or a file line
         if Get_Boolean (Tooltip.Explorer.Model, Iter, Has_Log_Column) then
            Text := new String'(-"A revision log exists for this file");
         else
            Text := new String'(-"No revision log exists for this file");
         end if;

      elsif Column = Tooltip.Explorer.File_Column then
         Text := new String'
           (Get_String (Tooltip.Explorer.Model, Iter, Name_Column));
      end if;

      if Text /= null then
         Create_Pixmap_From_Text
           (Text.all,
            Get_Pref (Default_Font),
            White (Get_Default_Colormap),
            Tooltip.Explorer.Tree,
            Pixmap);
         Free (Text);
      end if;
   end Draw;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Record) is
   begin
      Free (X.Status);
   end Free;

   ----------
   -- Copy --
   ----------

   function Copy (X : Line_Record) return Line_Record is
   begin
      return (Copy_File_Status (X.Status), X.Log);
   end Copy;

   -----------
   -- Clear --
   -----------

   procedure Clear (Explorer : VCS_Activities_View_Access) is
   begin
      if Explorer /= null then
         Scroll_To_Point (Explorer.Tree, 0, 0);

         Clear (Explorer.Model);
         Status_Hash.Reset (Explorer.Stored_Status);
      end if;
   end Clear;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event, View);
   begin
      Hide_VCS_Activities_Explorer;
      return True;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end On_Delete;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      Explorer : constant VCS_Activities_View_Access :=
                   VCS_Activities_View_Access (View);
   begin
      Status_Hash.Reset (Explorer.Stored_Status);
      Status_Hash.Reset (Explorer.Cached_Status);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Destroy;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Explorer : VCS_Activities_View_Access) is
      use Status_Hash;
      Sort_Col : Gint;
   begin
      Push_State (Explorer.Kernel, Busy);

      Clear (Explorer.Model);
      Sort_Col := Freeze_Sort (Explorer.Model);

      Query_Activities_Files (Explorer.Kernel, False);

      Thaw_Sort (Explorer.Model, Sort_Col);
      Columns_Autosize (Explorer.Tree);
      Pop_State (Explorer.Kernel);
   end Refresh;

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
      Set (Explorer.Model, Iter, Activity_Id_Column, Image (Activity));
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
              (-"Activity") & ''' & Get_Name (Activity) & ''' & ASCII.LF &
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

      Iter := Get_Iter_From_Name (Explorer, File);
      Remove (Explorer.Model, Iter);

      Refresh (Get_Explorer (Kernel, False, False));
   end On_Remove_From_Activity;

   -----------------
   -- Refresh_Log --
   -----------------

   procedure Refresh_Log
     (Explorer : access VCS_Activities_View_Record;
      File     : VFS.Virtual_File)
   is
      Log   : Boolean;
      Dummy : Boolean;
      Iter  : Gtk_Tree_Iter;
      Line  : Line_Record;
   begin
      if Get_Log_From_File (Explorer.Kernel, File, False) = VFS.No_File then
         Log := False;
      else
         Log := True;
      end if;

      --  Refresh the information in the cache

      Line := Get_Cached_Data (Explorer, File);

      if Line /= No_Data then
         Set_Cached_Data (Explorer, File, (Line.Status, Log));
         Iter := Get_Iter_From_Name (Explorer, File);

         if Iter /= Null_Iter then
            Fill_Info (Explorer, Iter, (Line.Status, Log), Dummy);
         end if;
      end if;

      Free (Line);
   end Refresh_Log;

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
      Root_Project : constant String :=
                       Project_Path
                         (Get_Root_Project (Get_Registry (Kernel).all));

      function Get_Activity_Iter (File : Virtual_File) return Gtk_Tree_Iter;
      --  Return the activity tree iter for the given file. Use Activity if set
      --  otherwise we use File to get the corresponding activity.

      function Create_Or_Remove_Activity
        (Activity : Activity_Id) return Gtk_Tree_Iter;
      --  This routine creates or removes an activity from the explorer. The
      --  activity is created if part of the root project and removed
      --  otherwise. It returns the tree iterator for the newly created
      --  activity or Null_Iter otherwise.

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
            end if;

            Set (Explorer.Model, A_Iter,
                 Base_Name_Column, Get_Name (Activity));
            Set (Explorer.Model, A_Iter, Activity_Id_Column, Image (Activity));
            Set (Explorer.Model, A_Iter,
                 Has_Log_Column, Has_Log (Kernel, Activity));
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

      Status_Temp  : File_Status_List.List_Node :=
                       File_Status_List.First (Status);
      Found        : Boolean := False;
      Log          : Boolean;
      Line         : Line_Record;
      Sort_Id      : Gint;
      Cache_Empty  : Boolean;
      Iter         : Status_Hash.Iterator;

      use Status_Hash;

      Up_To_Date_Status : File_Status;

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

      --  Free the logs associated to the files that are up-to-date, and
      --  update the vcs label in the editors.

      while Status_Temp /= File_Status_List.Null_Node loop
         declare
            S    : constant File_Status_Record :=
                     File_Status_List.Data (Status_Temp);
            File : constant Virtual_File := S.File;
            Success : Boolean;
         begin
            --  Clear the logs

            if Clear_Logs and then S.Status = Up_To_Date_Status then
               declare
                  Log : constant Virtual_File :=
                          Get_Log_From_File (Kernel, File, False);
               begin
                  if Log /= VFS.No_File and then Is_Regular_File (Log) then
                     Delete (Log, Success);
                     Close_File_Editors (Kernel, Log);
                  end if;

                  Remove_File_From_Mapping (Kernel, File);
               end;
            end if;

            --  Display the editor status

            if Is_Open (Kernel, File) and then VCS_Identifier /= null then
               Display_Editor_Status (Kernel, VCS_Identifier, S);
            end if;

         exception
            when E : others =>
               Trace (Exception_Handle, "Unexpected exception: "
                      & Exception_Information (E));
         end;

         Status_Temp := File_Status_List.Next (Status_Temp);
      end loop;

      if Explorer = null then
         return;
      end if;

      Status_Temp := File_Status_List.First (Status);

      Status_Hash.Get_First (Explorer.Cached_Status, Iter);
      Cache_Empty := Get_Element (Iter) = No_Element;

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

               Line := Get_Cached_Data (Explorer, File);

               if Line = No_Data or else Override_Cache then
                  Log := Get_Log_From_File
                    (Kernel, File, False) /= VFS.No_File;

                  Line :=
                    (Copy_File_Status
                       (File_Status_List.Data (Status_Temp)), Log);
                  Set_Cached_Data (Explorer, File, Line);
               end if;

               --  The info that we want to display is now in Line,
               --  if it already exists in Page.Stored_Status, we simply modify
               --  the element, otherwise we add it to the list.

               if Display or else Cache_Empty then
                  declare
                     New_Status         : constant Line_Record := Copy (Line);
                     New_File           : constant Virtual_File :=
                                            New_Status.Status.File;
                     Temp_Stored_Status : Iterator;
                     E                  : Element;
                     Iter               : Gtk_Tree_Iter := Null_Iter;
                     Success            : Boolean;
                  begin
                     Get_First (Explorer.Stored_Status, Temp_Stored_Status);
                     Found := False;

                     if Get_Element (Temp_Stored_Status) = No_Element then
                        Set (Explorer.Stored_Status,
                             New_File, (Line => New_Status));

                     else
                        E := Get (Explorer.Stored_Status, New_File);

                        if E /= No_Element then
                           Found := True;
                           Set (Explorer.Stored_Status,
                                New_File, (Line => New_Status));
                           Iter := Get_Iter_From_Name
                             (Explorer, New_Status.Status.File);
                        end if;

                        if not Found and then Force_Display then
                           Set (Explorer.Stored_Status,
                                New_File, (Line => New_Status));
                        end if;
                     end if;

                     if Iter = Null_Iter and then Force_Display then
                        Append (Explorer.Model, Iter, A_Iter);
                     end if;

                     if Iter /= Null_Iter then
                        Fill_Info (Explorer, Iter, New_Status, Success);
                     end if;
                  end;
               end if;

               Free (Line);
            end if;

            Status_Temp := File_Status_List.Next (Status_Temp);
         end;
      end loop;

      Expand_All (Explorer.Tree);

      Thaw_Sort (Explorer.Model, Sort_Id);
      Pop_State (Kernel);
   end Display_File_Status;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (Explorer  : access VCS_Activities_View_Record'Class;
      Iter      : Gtk_Tree_Iter;
      Line_Info : Line_Record;
      Success   : out Boolean)
   is
      Pixbuf : Gdk_Pixbuf;
   begin
      Success := True;

      if Line_Info.Status.File = No_File
        or else VFS.Is_Directory (Line_Info.Status.File)
      then
         Success := False;
         return;
      end if;

      Set (Explorer.Model, Iter, Has_Log_Column, Line_Info.Log);
      Set (Explorer.Model, Iter, Name_Column,
           Full_Name (Line_Info.Status.File, True).all);

      Set (Explorer.Model, Iter, Base_Name_Column,
           Base_Name (Line_Info.Status.File));

      if not String_List.Is_Empty (Line_Info.Status.Working_Revision) then
         Set (Explorer.Model, Iter, Local_Rev_Column,
              String_List.Head (Line_Info.Status.Working_Revision));
      else
         Set (Explorer.Model, Iter, Local_Rev_Column, -"n/a");
      end if;

      if not String_List.Is_Empty (Line_Info.Status.Repository_Revision) then
         Set (Explorer.Model, Iter, Rep_Rev_Column,
              String_List.Head (Line_Info.Status.Repository_Revision));
      else
         Set (Explorer.Model, Iter, Rep_Rev_Column, -"n/a");
      end if;

      if Line_Info.Status.Status.Stock_Id /= null then
         Pixbuf := Render_Icon
           (Explorer,
            Line_Info.Status.Status.Stock_Id.all,
            Gtk.Enums.Icon_Size_Small_Toolbar);
      end if;

      if Pixbuf /= null then
         Set (Explorer.Model, Iter, Status_Pixbuf_Column, C_Proxy (Pixbuf));
      else
         Pixbuf := Render_Icon
           (Explorer,
            VCS.Unknown.Stock_Id.all,
            Gtk.Enums.Icon_Size_Menu);

         Set (Explorer.Model, Iter, Status_Pixbuf_Column, C_Proxy (Pixbuf));
      end if;

      if Line_Info.Status.Status.Label /= null then
         Set (Explorer.Model, Iter, Status_Description_Column,
              Line_Info.Status.Status.Label.all);
      else
         Set (Explorer.Model, Iter, Status_Description_Column,
              VCS.Unknown.Label.all);
      end if;
   end Fill_Info;

   ----------------------------
   -- Get_Iter_From_Activity --
   ----------------------------

   function Get_Iter_From_Activity
     (Explorer : access VCS_Activities_View_Record'Class;
      Activity : Activity_Id) return Gtk_Tree_Iter
   is
      Activity_Id : constant String := Image (Activity);
      Iter        : Gtk_Tree_Iter := Get_Iter_First (Explorer.Model);
   begin
      while Iter /= Null_Iter loop
         if Get_String (Explorer.Model, Iter, Activity_Id_Column) =
           Activity_Id
         then
            return Iter;
         end if;

         Next (Explorer.Model, Iter);
      end loop;

      return Null_Iter;
   end Get_Iter_From_Activity;

   ------------------------
   -- Get_Iter_From_Name --
   ------------------------

   function Get_Iter_From_Name
     (Explorer : access VCS_Activities_View_Record'Class;
      Name     : VFS.Virtual_File) return Gtk_Tree_Iter
   is
      function Look_In (Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter;
      --  Parse recursively the tree model (2 levels, activity and files)

      -------------
      -- Look_In --
      -------------

      function Look_In (Iter : Gtk_Tree_Iter) return Gtk_Tree_Iter is
         I, Res : Gtk_Tree_Iter;
      begin
         Iter_Copy (Iter, I);

         while I /= Null_Iter loop
            if Has_Child (Explorer.Model, I) then
               Res := Look_In (Children (Explorer.Model, I));

               if Res /= Null_Iter then
                  return Res;
               end if;

            else
               if Get_String (Explorer.Model, I, Name_Column) =
                 Full_Name (Name, True).all
               then
                  return I;
               end if;
            end if;

            Next (Explorer.Model, I);
         end loop;

         return Null_Iter;
      end Look_In;

   begin
      return Look_In (Get_Iter_First (Explorer.Model));
   end Get_Iter_From_Name;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Explorer : VCS_Activities_View_Access) return String_List.List
   is
      Result : String_List.List;

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access);
      --  Add an item to Result.

      -----------------------
      -- Add_Selected_Item --
      -----------------------

      procedure Add_Selected_Item
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
         Data  : Explorer_Selection_Foreach.Data_Type_Access)
      is
         pragma Unreferenced (Model, Path, Data);
      begin
         String_List.Append
           (Result, Get_String (Explorer.Model, Iter, Name_Column));
      end Add_Selected_Item;

   begin
      if Explorer = null then
         return Result;
      end if;

      Explorer_Selection_Foreach.Selected_Foreach
        (Get_Selection (Explorer.Tree),
         Add_Selected_Item'Unrestricted_Access,
         Explorer_Selection_Foreach.Data_Type_Access (Explorer));
      return Result;
   end Get_Selected_Files;

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

      Set_Property (Edit_Rend, Editable_Property, True);

      Tree_Model_Callback.Object_Connect
        (Edit_Rend, "edited", Edited_Callback'Access,
         Slot_Object => Explorer.Kernel, User_Data => Base_Name_Column);

      Gtk_New (Explorer.File_Column);
      Set_Title (Explorer.File_Column, -"Activity / File");
      Pack_Start (Explorer.File_Column, Edit_Rend, True);
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

   -------------------
   --  Create_Model --
   -------------------

   procedure Create_Model
     (Explorer : access VCS_Activities_View_Record'Class) is
   begin
      Gtk_New (Explorer.Model, Columns_Types);
   end Create_Model;

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

   -----------------------
   -- Get_Path_At_Event --
   -----------------------

   function Get_Path_At_Event
     (Tree  : Gtk_Tree_View;
      Event : Gdk_Event) return Gtk_Tree_Path
   is
      X         : constant Gdouble := Get_X (Event);
      Y         : constant Gdouble := Get_Y (Event);
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column := null;

   begin
      Path := Gtk_New;
      Get_Path_At_Pos
        (Tree, Gint (X), Gint (Y),
         Path, Column, Buffer_X, Buffer_Y, Row_Found);

      return Path;
   end Get_Path_At_Event;

   ------------------
   -- Context_Func --
   ------------------

   function Context_Func
     (Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu) return Selection_Context_Access
   is
      pragma Unreferenced (Event_Widget);

      Explorer : constant VCS_Activities_View_Access :=
                   VCS_Activities_View_Access (Object);
      Mitem    : Gtk_Menu_Item;
      Context  : Selection_Context_Access;
      Files    : String_List.List;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

   begin
      --  If there is no selection, select the item under the cursor

      Path := Get_Path_At_Event (Explorer.Tree, Event);

      if Path /= null
        and then not Path_Is_Selected (Get_Selection (Explorer.Tree), Path)
      then
         --  Right click over a line which is not the current selection, this
         --  line becomes the new selection.
         Unselect_All (Get_Selection (Explorer.Tree));
         Select_Path (Get_Selection (Explorer.Tree), Path);
      end if;

      --  If Get_Depth (Path) is 1 then we are on an activity node

      if Get_Depth (Path) = 1 then
         Context := new Activity_Context;

         Iter := Get_Iter (Explorer.Model, Path);

         Set_Context_Information
           (Context, Kernel, Abstract_Module_ID (VCS_Module_ID));
         Set_Activity_Information
           (Activity_Context_Access (Context),
            Get_String (Explorer.Model, Iter, Activity_Id_Column));

      elsif Get_Depth (Path) > 1 then
         Files := Get_Selected_Files (Explorer);

         declare
            First_File : constant Virtual_File := Create
              (Full_Filename => String_List.Head (Files));
         begin
            Context := new Activity_Context;

            Iter := Parent (Explorer.Model, Get_Iter (Explorer.Model, Path));

            Set_Activity_Information
              (Activity_Context_Access (Context),
               Get_String (Explorer.Model, Iter, Activity_Id_Column));
            Set_Context_Information
              (Context, Kernel, Abstract_Module_ID (VCS_Module_ID));
            Set_File_Information
              (File_Selection_Context_Access (Context), File => First_File);
         end;

         String_List.Free (Files);

      else
         --  Path is 0, outside the tree view data

         Context := new Selection_Context;

         Set_Context_Information
           (Context, Kernel, Abstract_Module_ID (VCS_Module_ID));
      end if;

      Path_Free (Path);

      Set_Current_Context (Explorer, Context);
      VCS_Activities_Contextual_Menu (Kernel_Handle (Kernel), Context, Menu);

      if Context.all in File_Selection_Context'Class
        and then Has_File_Information (File_Selection_Context_Access (Context))
      then
         --  This is a menu for a file line in the tree view, it is fine to add
         --  a separator.
         Gtk_New (Mitem);
         Append (Menu, Mitem);
      end if;

      return Context;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return null;
   end Context_Func;

   ------------------
   -- Button_Press --
   ------------------

   function Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Explorer : constant VCS_Activities_View_Access :=
                   VCS_Activities_View_Access (View);
      Kernel   : constant Kernel_Handle := Explorer.Kernel;
      Path     : Gtk_Tree_Path;
      Iter     : Gtk_Tree_Iter;

   begin
      if Get_Event_Type (Event) = Gdk_2button_Press then
         Path := Get_Path_At_Event (Explorer.Tree, Event);

         if Path /= null and then Get_Depth (Path) = 2 then
            Iter := Get_Iter (Explorer.Model, Path);
            Open_File_Editor
              (Kernel,
               Create
                 (Full_Filename =>
                    Get_String (Explorer.Model, Iter, Name_Column)));
            Emit_Stop_By_Name (Explorer.Tree, "button_press_event");
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press;

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
                  use Status_Hash;
                  File : constant Virtual_File :=
                           Get_File_From_Log (Kernel, D.File);
                  E    : Element;
               begin
                  Line := Get_Cached_Data (Hook.Explorer, File);

                  if Line /= No_Data then
                     Set_Cached_Data
                       (Hook.Explorer, File, (Line.Status, True));
                  end if;

                  E := Get (Hook.Explorer.Stored_Status, File);

                  if E /= No_Element then
                     Set (Hook.Explorer.Stored_Status, File,
                          (Line => (Copy_File_Status (Line.Status), True)));
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

   procedure Initialize
     (Explorer : access VCS_Activities_View_Record'Class;
      Kernel   : Kernel_Handle)
   is
      Hook            : File_Hook;
      Scrolledwindow1 : Gtk_Scrolled_Window;
      Tooltip         : VCS_Tooltips_Access;
      Selection       : Gtk_Tree_Selection;

   begin
      Init_Graphics;
      Initialize_Hbox (Explorer);

      Explorer.Kernel := Kernel;

      Create_Model (Explorer);

      Gtk_New (Explorer.Tree, Explorer.Model);

      Gtk_New (Scrolledwindow1);
      Set_Policy (Scrolledwindow1,
                  Gtk.Enums.Policy_Automatic,
                  Gtk.Enums.Policy_Automatic);
      Pack_Start (Explorer, Scrolledwindow1, True, True, 0);

      Selection := Get_Selection (Explorer.Tree);
      Set_Mode (Selection, Gtk.Enums.Selection_Multiple);
      Add (Scrolledwindow1, Explorer.Tree);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer.Tree,
         "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (Button_Press'Access),
         Explorer,
         After => False);

      Register_Contextual_Menu
        (Explorer.Kernel,
         Explorer.Tree,
         Explorer,
         Module_ID (VCS_Module_ID),
         Context_Func'Access);

      Set_Column_Types (Explorer);

      Tooltip := new VCS_Tooltips;
      Tooltip.Explorer := VCS_Activities_View_Access (Explorer);
      Set_Tooltip (Tooltip, Explorer.Tree);

      --  Emit a "clicked" signal on the file column to sort it

      Clicked (Explorer.File_Column);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (Explorer, "delete_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Delete'Access),
         Explorer, After => False);

      Gtkada.Handlers.Widget_Callback.Object_Connect
        (Explorer, "destroy", On_Destroy'Access, Explorer);

      Hook := new File_Hook_Record'
        (Hook_Args_Record with
         Explorer => VCS_Activities_View_Access (Explorer));
      Add_Hook (Kernel, File_Edited_Hook, Hook, Watch => GObject (Explorer));

      --  Can't do this through the Focus_Widget parameter to Gtkada.MDI.Put,
      --  since the focus child is dynamic.
      Widget_Callback.Connect
        (Explorer, "grab_focus", On_Selected'Access, After => True);
   end Initialize;

   ------------------
   -- Copy_Context --
   ------------------

   function Copy_Context
     (Context : Selection_Context_Access)
      return Selection_Context_Access
   is
      Result : Selection_Context_Access;
   begin
      if Context /= null then
         Result := new Activity_Context;

         if Context.all in Activity_Context'Class then
            declare
               Activity : constant Activity_Context_Access :=
                            Activity_Context_Access (Context);
            begin
               Set_Activity_Information
                 (Activity_Context_Access (Result),
                  Activity_Information (Activity));
            end;
         end if;

         if Context.all in File_Selection_Context'Class
           and then Has_File_Information
             (File_Selection_Context_Access (Context))
         then
            declare
               File : constant File_Selection_Context_Access :=
                        File_Selection_Context_Access (Context);
            begin
               Set_File_Information
                 (File_Selection_Context_Access (Result),
                  File_Information (File),
                  Project_Information (File));
            end;
         end if;

         Set_Context_Information
           (Result, Get_Kernel (Context), Get_Creator (Context));
      end if;

      return Result;
   end Copy_Context;

   -------------------------
   -- Get_Current_Context --
   -------------------------

   function Get_Current_Context
     (Explorer : access VCS_Activities_View_Record)
      return Selection_Context_Access is
   begin
      if Explorer.Context = null then
         declare
            Context : File_Selection_Context_Access;
            Files   : String_List.List;
            First   : VFS.Virtual_File;
         begin
            Files := Get_Selected_Files
              (VCS_Activities_View_Access (Explorer));

            if not String_List.Is_Empty (Files) then
               Context := new File_Selection_Context;
               First := Create (String_List.Head (Files));

               Set_Context_Information
                 (Context, Explorer.Kernel,
                  Abstract_Module_ID (VCS_Module_ID));
               Set_File_Information (Context, File => First);

               Set_Current_Context
                 (Explorer, Selection_Context_Access (Context));

               String_List.Free (Files);
               return Selection_Context_Access (Context);
            else
               return null;
            end if;
         end;

      else
         return Copy_Context (Explorer.Context);
      end if;
   end Get_Current_Context;

   -------------------------
   -- Set_Current_Context --
   -------------------------

   procedure Set_Current_Context
     (Explorer : access VCS_Activities_View_Record;
      Context  : Selection_Context_Access) is
   begin
      if Explorer.Context /= null then
         Unref (Explorer.Context);
         Explorer.Context := null;
      end if;

      Explorer.Context := Context;
      Ref (Explorer.Context);
   end Set_Current_Context;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Kernel : Kernel_Handle) return String_List.List
   is
      Result         : String_List.List;
      Focused_Child  : constant MDI_Child :=
                         Get_Focus_Child (Get_MDI (Kernel));
      Explorer_Child : constant MDI_Child :=
                         Find_MDI_Child_By_Tag
                           (Get_MDI (Kernel), VCS_Activities_View_Record'Tag);
   begin
      if Explorer_Child = Focused_Child
        and then Explorer_Child /= null
      then
         Result := Get_Selected_Files
           (VCS_Activities_View_Access (Get_Widget (Explorer_Child)));
      else
         declare
            File : constant VFS.Virtual_File :=
              Get_Current_File (Get_Current_Context (Kernel));
         begin
            if File /= VFS.No_File then
               String_List.Append (Result, Full_Name (File).all);
            end if;
         end;
      end if;

      return Result;
   end Get_Selected_Files;

   ---------------------
   -- Get_Current_Dir --
   ---------------------

   function Get_Current_Dir
     (Context : Selection_Context_Access) return String
   is
      File : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File) then
            Trace (Me, "Directory= " & Directory_Information (File));
            return Directory_Information (File);
         else
            Trace (Me, "No directory");
         end if;

      elsif Context = null then
         Trace (Me, "null context");
      else
         Trace (Me, "Invalid context");
      end if;

      return Get_Current_Dir;
   end Get_Current_Dir;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Context : Selection_Context_Access) return Virtual_File
   is
      File : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File := File_Selection_Context_Access (Context);

         return File_Information (File);
      end if;

      return VFS.No_File;
   end Get_Current_File;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Explorer : access VCS_Activities_View_Record) return Kernel_Handle is
   begin
      return Explorer.Kernel;
   end Get_Kernel;

   -----------------------
   -- Get_Cached_Status --
   -----------------------

   function Get_Cached_Status
     (Explorer : access VCS_Activities_View_Record;
      File     : VFS.Virtual_File) return File_Status_Record is
   begin
      return Get_Cached_Data (Explorer, File).Status;
   end Get_Cached_Status;

   ----------
   -- Hash --
   ----------

   function Hash (F : Virtual_File) return Header_Num is
      function Hash is new HTables.Hash (Header_Num);
   begin
      if Filenames_Are_Case_Sensitive then
         return Hash (Full_Name (F).all);
      else
         return Hash (To_Lower (Full_Name (F).all));
      end if;
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Virtual_File) return Boolean is
   begin
      return F1 = F2;
   end Equal;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Element) is
   begin
      Free (X.Line);
   end Free;

end VCS_Activities_View;
