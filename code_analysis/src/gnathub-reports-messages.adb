------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with GNATCOLL.Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with Gdk.RGBA;
with Glib.Object;
with Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Tree_Sortable;        use Gtk.Tree_Sortable;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model_Sort;      use Gtk.Tree_Model_Sort;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Pango.Layout;             use Pango.Layout;

with GNAThub.Messages;         use GNAThub.Messages;
with GNAThub.Module;           use GNAThub.Module;
with Default_Preferences;      use Default_Preferences;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GUI_Utils;                use GUI_Utils;
with String_Utils;             use String_Utils;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;

package body GNAThub.Reports.Messages is

   Me : constant Trace_Handle := Create ("GNATHUB.REPORTS.MESSAGES");

   File_Line_Sep : constant String := "<line>";
   --  Separator betwwen file full names and line numbers used to contruct
   --  row IDs.

   type Row_Kind_Type is
     (Total_Kind, Project_Kind, Dir_Kind, File_Kind, Subprogram_Kind);
   --  The different kind of rows displayed in the tree view

   type Update_Action_Type is
     (Message_Added, Message_Removed, Metric_Added);
   --  The different kind of actions that can update the tree view

   Total_Row_Name : constant String := "Total:";
   --  The total row's name

   Entity_Name_Column_Min_Width : constant := 250;
   --  The entity name column minimum width

   Prj_Pixbuf_Cst   : constant String := "gps-emblem-project-closed";
   --  Name of the icon used for project nodes in the analysis report

   Dir_Pixbuf_Cst   : constant String := "gps-emblem-directory-closed";
   --  Name of the icon used for directory nodes in the analysis report

   File_Pixbuf_Cst  : constant String := "gps-emblem-file-unmodified";
   --  Name of the icon used for file nodes in the analysis report

   Subp_Pixbuf_Cst  : constant String := "gps-emblem-entity-subprogram";
   --  Name of the icon used for subprogram nodes in the analysis report

   Entity_Icon_Column    : constant := 0;
   --  Column containing the name of the entity icon to display.

   Entity_ID_Column      : constant := 1;
   --  Column containing the entity's unique ID.

   Entity_Name_Column    : constant := 2;
   --  Column containing the name of the entity.

   Total_Column          : constant := 3;
   --  Column displaying the number of messages per entity.

   package Sorting_Functions is new Gtk.Tree_Sortable.Set_Sort_Func_User_Data
     (GNAThub_Report_Messages);

   function Get_Column_Types
     (Self : not null access GNAThub_Report_Messages_Record'Class)
      return GType_Array;
   --  Return the tree view's column types.

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble);
   --  Called when the user double-clicks on a row.
   --  Open the corresponding file and jump to the clicked entity, if any.

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed.

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : GNAThub_Report_Messages;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed.
   --  Update the severity columns background color if needed.

   procedure Create_Row
     (Self            : not null access GNAThub_Report_Messages_Record'Class;
      Iter            : out Gtk_Tree_Iter;
      Parent          : Gtk_Tree_Iter;
      Kind            : Row_Kind_Type;
      Name            : String;
      ID              : String;
      Update_Action   : Update_Action_Type;
      Severity_Column : Gint);
   --  Create and initialize a row in the tree.

   procedure Set_Row_Colors
     (Self : not null access GNAThub_Report_Messages_Record'Class;
      Iter : Gtk_Tree_Iter);
   --  Set the severity background colors for the given row.

   procedure Create_Or_Update_Row
     (Self       : not null access GNAThub_Report_Messages_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Project    : GNATCOLL.Projects.Project_Type;
      Entity     : Entity_Data;
      Update_Action : Update_Action_Type;
      Severity   : Severity_Access);
   --  Create or update the tree according to the given file/project/entity.
   --  Update_Action is used to increment/decrement the row's counters
   --  depending on the nature of the action (i.e: we should increment
   --  counters when a message is being added etc.);

   function Get_New_Value_For_Severity
     (Current_Value : String;
      Value_To_Add  : Gint) return String;
   --  Return a suitable string value for the severity total columns.
   --  Return an empty string when the new value is 0.

   function Entity_Name_Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree  : GNAThub_Report_Messages) return Gint;
   --  Used to sort the column displaying entity names.

   --------------------------------
   -- Get_New_Value_For_Severity --
   --------------------------------

   function Get_New_Value_For_Severity
     (Current_Value : String;
      Value_To_Add  : Gint) return String
   is
      Current_Int_Value : constant Gint := (if Current_Value = "" then
                                               0
                                            else
                                               Gint'Value (Current_Value));
      New_Int_Value     : constant Gint := Current_Int_Value + Value_To_Add;
   begin
      if New_Int_Value <= 0 then
         return "";
      else
         return String_Utils.Image (Integer (New_Int_Value));
      end if;
   end Get_New_Value_For_Severity;

   ---------------------------
   -- Entity_Name_Sort_Func --
   ---------------------------

   function Entity_Name_Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree  : GNAThub_Report_Messages) return Gint
   is
      A_Value : constant String := Get_String (Model, A, Entity_Name_Column);
      B_Value : constant String := Get_String (Model, B, Entity_Name_Column);
      Column  : Gint;
      Order   : Gtk_Sort_Type;
   begin
      Gtk.Tree_Model_Sort.Get_Sort_Column_Id
        (Tree.Sortable_Model, Column, Order);

      if A_Value = Total_Row_Name then
         return (if Order = Sort_Descending then 1 else -1);
      elsif A_Value > B_Value then
         return -1;
      else
         return 1;
      end if;
   end Entity_Name_Sort_Func;

   --------------------
   -- Set_Row_Colors --
   --------------------

   procedure Set_Row_Colors
     (Self : not null access GNAThub_Report_Messages_Record'Class;
      Iter : Gtk_Tree_Iter)
   is
      Model : constant Gtk_Tree_Store := Self.Model;
   begin
      for Severity of Self.Severities loop
         declare
            Columns_Infos : constant Severity_Columns_Info_Type :=
              Self.Severities_Columns_Info (Severity.Get_Name);
            Value         : Glib.Values.GValue;
         begin
            Glib.Values.Init (Value, Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value
              (Value, Background (Severity.Style));

            Set_Value
              (Tree_Store  => Model,
               Iter        => Iter,
               Column      => Columns_Infos.Color_Col,
               Value       => Value);
         end;
      end loop;
   end Set_Row_Colors;

   ----------------
   -- Create_Row --
   ----------------

   procedure Create_Row
     (Self            : not null access GNAThub_Report_Messages_Record'Class;
      Iter            : out Gtk_Tree_Iter;
      Parent          : Gtk_Tree_Iter;
      Kind            : Row_Kind_Type;
      Name            : String;
      ID              : String;
      Update_Action      : Update_Action_Type;
      Severity_Column : Gint)
   is
      Model : constant Gtk_Tree_Store := Self.Model;
      Value : constant Gint := (case Update_Action is
                                   when Message_Added => 1,
                                   when others        => 0);
      -------------------
      -- Get_Icon_Name --
      -------------------

      function Get_Icon_Name (Row_Kind : Row_Kind_Type) return String
      is
        (case Row_Kind is
            when Total_Kind      => "",
            when Project_Kind    => Prj_Pixbuf_Cst,
            when Dir_Kind        => Dir_Pixbuf_Cst,
            when File_Kind       => File_Pixbuf_Cst,
            when Subprogram_Kind => Subp_Pixbuf_Cst);

   begin
      Model.Append (Iter, Parent);

      Set_And_Clear
        (Model,
         Iter   => Iter,
         Values =>
           (Entity_Icon_Column => As_String (Get_Icon_Name (Kind)),
            Entity_ID_Column   => As_String (ID),
            Entity_Name_Column => As_String (Name),
            Total_Column       => As_Int (Value)));

      if Severity_Column /= -1 then
         Model.Set
           (Iter   => Iter,
            Column => Severity_Column,
            Value  => Get_New_Value_For_Severity
              (Current_Value => "",
               Value_To_Add  => Value));
      end if;

      Self.Set_Row_Colors (Iter);
   end Create_Row;

   -----------------------------
   -- Update_Tree_For_Message --
   -----------------------------

   procedure Create_Or_Update_Row
     (Self        : not null access GNAThub_Report_Messages_Record'Class;
      File        : GNATCOLL.VFS.Virtual_File;
      Project     : GNATCOLL.Projects.Project_Type;
      Entity      : Entity_Data;
      Update_Action  : Update_Action_Type;
      Severity    : Severity_Access)
   is
      Model        : constant Gtk_Tree_Store := Self.Model;
      Columns_Info : constant Severity_Columns_Info_Type :=
        (if Severity /= null then
            Self.Severities_Columns_Info (Severity.Get_Name)
         else
            Severity_Columns_Info_Type'
           (Total_Col => -1,
            Color_Col => -1));
      Value        : constant Gint := (case Update_Action is
                                          when Message_Added   => 1,
                                          when Message_Removed => -1,
                                          when Metric_Added    => 0);
      Dummy        : Gtk_Tree_Iter;
      Path         : Gtk_Tree_Path;

      function Insert_Or_Update_Row
        (Parent : Gtk_Tree_Iter;
         Kind   : Row_Kind_Type;
         Name   : String;
         ID     : String) return Gtk_Tree_Iter;

      --------------------------
      -- Insert_Or_Update_Row --
      --------------------------

      function Insert_Or_Update_Row
        (Parent : Gtk_Tree_Iter;
         Kind   : Row_Kind_Type;
         Name   : String;
         ID     : String) return Gtk_Tree_Iter
      is
         Iter : Gtk_Tree_Iter;

         procedure Update_Row
           (Iter : Gtk_Tree_Iter);

         ----------------
         -- Update_Row --
         ----------------

         procedure Update_Row
           (Iter : Gtk_Tree_Iter)
         is
            New_Total : constant Gint := Model.Get_Int (Iter, Total_Column)
              + Value;
         begin
            --  Update the row's counters. Negative total can happend when
            --  messages are being removed after the tree view's model has
            --  been cleared: avoid displaying them.

            if New_Total >= 0 then
               Model.Set
                 (Iter,
                  Column => Total_Column,
                  Value  => New_Total);
            end if;

            if Columns_Info.Total_Col /= -1 then
               Model.Set
                 (Iter,
                  Column => Columns_Info.Total_Col,
                  Value  => Get_New_Value_For_Severity
                    (Current_Value => Model.Get_String
                         (Iter, Columns_Info.Total_Col),
                     Value_To_Add  => Value));
            end if;
         end Update_Row;

      begin
         --  Find the corresponding row or create a new one

         Iter := Find_Node
           (Model  => Model,
            Name   => ID,
            Column => Entity_ID_Column,
            Parent => Parent);

         if Iter = Null_Iter then
            Self.Create_Row
              (Iter,
               Parent          => Parent,
               Kind            => Kind,
               Name            => Name,
               ID              => ID,
               Update_Action   => Update_Action,
               Severity_Column => Columns_Info.Total_Col);
         else
            Update_Row (Iter);
            Self.Refilter (Iter);
         end if;

         return Iter;
      end Insert_Or_Update_Row;

   begin

      Dummy := Insert_Or_Update_Row
        (Parent => Null_Iter,
         Kind   => Total_Kind,
         Name   => Total_Row_Name,
         ID     => Total_Row_Name);

      --  Insert/update the project's row

      Dummy := Insert_Or_Update_Row
        (Parent => Null_Iter,
         Kind   => Project_Kind,
         Name   => Project.Name,
         ID     => Project.Project_Path.Display_Full_Name);

      --  If the message is not associated to the project itself, insert/update
      --  rows for message's directory and file, and possibly its subprogram.

      if Project.Project_Path /= File then

         --  Insert/update or get the directory

         Dummy := Insert_Or_Update_Row
           (Parent => Dummy,
            Kind   => Dir_Kind,
            Name   => File.Get_Parent.Display_Base_Dir_Name,
            ID     => File.Display_Dir_Name);

         --  Expand the nodes until the message's file

         Path := Model.Get_Path (Dummy);
         Self.Expand_To_Path (Path);
         Path_Free (Path);

         --  Insert/update or get the file

         Dummy := Insert_Or_Update_Row
           (Parent => Dummy,
            Kind   => File_Kind,
            Name   => File.Display_Base_Name,
            ID     => File.Display_Full_Name);

         --  Insert/update or get the subprogram

         if Entity /= No_Entity_Data then
            declare
               Subp_Name   : constant String := To_String (Entity.Name);
               Line        : constant String := Integer'Image (Entity.Line);
               ID          : constant String :=
                 File.Display_Full_Name & File_Line_Sep & Line;
            begin
               Dummy := Insert_Or_Update_Row
                 (Parent => Dummy,
                  Kind   => Subprogram_Kind,
                  Name   => Subp_Name,
                  ID     => ID);
            end;
         end if;
      end if;
   end Create_Or_Update_Row;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Messages_And_Metrics_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class) is
   begin
      if Message.all in GNAThub_Message'Class then
         if Message.Get_Flags (Locations) then
            declare
               Msg  : constant GNAThub_Message_Access := GNAThub_Message_Access
                 (Message);
               File : constant GNATCOLL.VFS.Virtual_File := Msg.Get_File;
            begin
               Self.View.Create_Or_Update_Row
                 (File          => File,
                  Project       =>
                    Self.View.Kernel.Get_Project_Tree.Info (File).Project,
                  Entity        => Msg.Get_Entity,
                  Update_Action => Message_Added,
                  Severity      => Msg.Get_Severity);
            end;
         end if;
      end if;
   end Message_Added;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Messages_And_Metrics_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class) is
   begin
      if Message.all in GNAThub_Message'Class then
         declare
            Msg  : constant GNAThub_Message_Access := GNAThub_Message_Access
              (Message);
            File : constant GNATCOLL.VFS.Virtual_File := Msg.Get_File;
         begin
            Self.View.Create_Or_Update_Row
              (File          => File,
               Project       =>
                 Self.View.Kernel.Get_Project_Tree.Info (File).Project,
               Entity        => Msg.Get_Entity,
               Update_Action => Message_Removed,
               Severity      => Msg.Get_Severity);
         end;
      end if;
   end Message_Removed;

   ------------------
   -- Metric_Added --
   ------------------

   overriding procedure Metric_Added
     (Self   : not null access Messages_And_Metrics_Listener;
      Metric : not null access Metric_Record'Class) is
   begin
      Self.View.Create_Or_Update_Row
        (File          => Metric.Get_File,
         Project       => Metric.Get_Project,
         Entity        => Metric.Get_Entity,
         Update_Action => Metric_Added,
         Severity      => Metric.Get_Severity);
   end Metric_Added;

   ----------------------
   -- Get_Column_Types --
   ----------------------

   function Get_Column_Types
     (Self : not null access GNAThub_Report_Messages_Record'Class)
      return GType_Array
   is
      Nb_Severities    : constant Guint := Guint (Self.Severities.Length);
      Severity_Col_End : constant Guint :=
        Total_Column + Nb_Severities * 2;

      Column_Types       : GType_Array
        (Entity_Icon_Column .. Severity_Col_End) :=
        (Entity_Icon_Column     => GType_String,
         Entity_Name_Column     => GType_String,
         Entity_ID_Column       => GType_String,
         Total_Column           => GType_Int,
         others                 => <>);
   begin
      for Columns_Info of Self.Severities_Columns_Info loop
         Column_Types (Guint (Columns_Info.Total_Col)) := GType_String;
         Column_Types (Guint (Columns_Info.Color_Col)) := Gdk.RGBA.Get_Type;
      end loop;

      return Column_Types;
   end Get_Column_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self       : out GNAThub_Report_Messages;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Severities : GNAThub.Severities_Ordered_Sets.Set)
   is
      Column            : Gtk_Tree_View_Column;
      Icon_Renderer     : Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer     : Gtk_Cell_Renderer_Text;
      Dummy             : Gint;

      procedure Assign_Columns_To_Severities;

      ----------------------------------
      -- Assign_Columns_To_Severities --
      ----------------------------------

      procedure Assign_Columns_To_Severities is
         Nb_Severities      : constant Gint := Gint (Self.Severities.Length);
         Severity_Total_Col : Gint := Total_Column + 1;
      begin
         for Severity of Self.Severities loop
            Self.Severities_Columns_Info.Insert
              (Key      => Severity.Get_Name,
               New_Item => Severity_Columns_Info_Type'
                 (Total_Col => Severity_Total_Col,
                  Color_Col => Severity_Total_Col + Nb_Severities));
            Severity_Total_Col := Severity_Total_Col + 1;
         end loop;
      end Assign_Columns_To_Severities;

   begin
      Trace (Me, "Initializing the GNAThub Messages Report");

      --  Create the tree view

      Self := new GNAThub_Report_Messages_Record;
      Self.Kernel := Kernel;
      Self.Severities := Severities;

      Assign_Columns_To_Severities;

      Gtkada.Tree_View.Initialize
        (Widget           => Self,
         Column_Types     => Self.Get_Column_Types,
         Capability_Type  => Filtered_And_Sortable,
         Set_Visible_Func => True);
      Self.Get_Selection.Set_Mode (Selection_Multiple);
      Self.Set_Propagate_Filtered_Status (False);

      --  Create a tree view column to display an icon representing the type
      --  of the entity.

      Gtk_New (Column);
      Gtk_New (Icon_Renderer);
      Column.Set_Resizable (True);
      Column.Pack_Start (Icon_Renderer, Expand => False);
      Column.Add_Attribute (Icon_Renderer, "icon-name", Entity_Icon_Column);
      Dummy := Self.Append_Column (Column);

      --  Create a tree view column to display the name of entity.

      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Set_Min_Width (Entity_Name_Column_Min_Width);
      Column.Set_Title ("Entity");
      Column.Set_Resizable (True);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "markup", Entity_Name_Column);
      Set_Property
        (Text_Renderer,
         Gtk.Cell_Renderer_Text.Ellipsize_Property,
         Ellipsize_Middle);
      Column.Set_Sort_Column_Id (Entity_Name_Column);
      Dummy := Self.Append_Column (Column);

      --  Create a tree view column to display the entity's total number of
      --  messages.

      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Set_Title ("Total");
      Column.Set_Resizable (True);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "markup", Total_Column);
      Column.Set_Sort_Column_Id (Total_Column);
      Dummy := Self.Append_Column (Column);

      --  Create a tree view column for each severity

      for Severity of Self.Severities loop
         declare
            Columns_Info : constant Severity_Columns_Info_Type :=
              Self.Severities_Columns_Info (Severity.Get_Name);
         begin
            Gtk_New (Column);
            Gtk_New (Text_Renderer);
            Column.Set_Title (To_String (Severity.Get_Name));
            Column.Set_Resizable (True);
            Column.Pack_Start (Text_Renderer, Expand => False);
            Column.Add_Attribute
              (Text_Renderer, "markup", Columns_Info.Total_Col);
            Column.Add_Attribute
              (Text_Renderer, "cell-background-rgba", Columns_Info.Color_Col);
            Column.Set_Sort_Column_Id (Columns_Info.Total_Col);
            Dummy := Self.Append_Column (Column);
         end;
      end loop;

      --  Use a special sort function for the column displaying entity names
      --  in order to have the 'Total:' row always on top.

      Sorting_Functions.Set_Sort_Func
        (Sortable       => +Self.Sortable_Model,
         Sort_Column_Id => Entity_Name_Column,
         Sort_Func      => Entity_Name_Sort_Func'Access,
         User_Data      => Self);

      --  Regsiter all the needed listeners/callbacks/hook functions

      Self.Listener := new Messages_And_Metrics_Listener'
        (Abstract_Listener with View => Self);

      GPS.Kernel.Messages.Register_Listener
        (Self.Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Listener),
         GPS.Kernel.Messages.Locations_Only);

      GNAThub.Metrics.Register_Listener (Self.Listener);

      Preferences_Changed_Hook.Add
        (new On_Pref_Changed'(Hook_Function with
             View => Self),
         Watch => Self);

      Self.On_Destroy (On_Destroy'Access);

      Gtk_New (Self.Multipress, Widget => Self);
      Self.Multipress.On_Pressed (On_Multipress'Access, Slot => Self);
      Self.Multipress.Watch (Self);
   end Gtk_New;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Self : not null access GNAThub_Report_Messages_Record'Class) is
   begin
      Trace (Me, "Clearing the GNAThub Messages report");
      Self.Model.Clear;
   end Clear;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : not null access GNAThub_Report_Messages_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean
   is
     (Self.Model.Get_Int (Store_Iter, Total_Column) > 0
      or else Self.Get_ID (Store_Iter) = Total_Row_Name
      or else not Hide_Node_Without_Messages.Get_Pref);

   ------------
   -- Get_ID --
   ------------

   function Get_ID
     (Self : not null access GNAThub_Report_Messages_Record'Class;
      Row  : Gtk_Tree_Iter) return String
   is
     (if Row = Null_Iter then
         ""
      else
         Self.Model.Get_String (Row, Entity_ID_Column));

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Kernel);

      function Change_Row_Color
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;
      --  Used to change the row's color depending on the new color
      --  preferences.

      ----------------------------
      -- Is_Severity_Color_Pref --
      ----------------------------

      function Is_Severity_Color_Pref return Boolean
      is
        (for some Severity of Self.View.Severities =>
            Preference
           (Get_Color_Preference (Severity.Style)) = Pref);

      ----------------------
      -- Change_Row_Color --
      ----------------------

      function Change_Row_Color
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
      is
         pragma Unreferenced (Path, Model);
      begin
         Self.View.Set_Row_Colors (Iter);

         return False;
      end Change_Row_Color;

   begin
      if Is_Severity_Color_Pref then
         Self.View.Model.Foreach (Change_Row_Color'Unrestricted_Access);
      elsif Pref = Preference (Hide_Node_Without_Messages) then
         Self.View.Refilter;
      end if;
   end Execute;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Glib.Gint;
      X, Y    : Glib.Gdouble)
   is
      pragma Unreferenced (X, Y);
      use Glib;

      View       : constant GNAThub_Report_Messages :=
        GNAThub_Report_Messages (Self);
      Model      : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Sort_Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      if N_Press /= 2 then
         return;
      end if;

      View.Multipress.Set_State (Gtk.Enums.Event_Sequence_Claimed);

      View.Get_First_Selected (Model, Sort_Iter);

      if Sort_Iter = Null_Iter then
         return;
      end if;

      Iter := View.Convert_To_Store_Iter (Sort_Iter);

      --  Get the location where we should jump to from the row's location ID.

      declare
         Is_File : Boolean := True;
         ID      : constant String := View.Get_ID (Iter);
         File    : GNATCOLL.VFS.Virtual_File;
         Line    : Integer := 1;

         function For_Each (Item : String) return Boolean;

         --------------
         -- For_Each --
         --------------

         function For_Each (Item : String) return Boolean is
         begin
            --  Get the file first, and if there is a line appended to the ID,
            --  get it too.

            if Is_File then
               File := GNATCOLL.VFS.Create (+Item);
               Is_File := False;
            else
               Line := Integer'Value (Item);
            end if;

            return True;
         exception
            when Constraint_Error =>
               Trace
                 (Me,
                  "Error on multipress, could not find a location "
                  & "for ID: " & ID);

               return False;
         end For_Each;

      begin
         GNATCOLL.Utils.Split
           (Str      => View.Get_ID (Iter),
            On       => File_Line_Sep,
            For_Each => For_Each'Unrestricted_Access);

         if File.Is_Regular_File then
            Open_File_Action_Hook.Run
              (View.Kernel,
               File    => File,
               Project => GNATCOLL.Projects.No_Project,
               Line    => Line);
         end if;
      end;
   end On_Multipress;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Gtk_Widget_Record'Class)
   is
      View : constant GNAThub_Report_Messages :=
        GNAThub_Report_Messages (Self);
   begin
      Trace (Me, "Destroying the GNAThub messages report");

      GPS.Kernel.Messages.Unregister_Listener
        (View.Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (View.Listener));

      GNAThub.Metrics.Unregister_Listener (View.Listener);

      Free (View.Listener);
   end On_Destroy;

end GNAThub.Reports.Messages;
