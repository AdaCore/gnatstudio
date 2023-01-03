------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2023, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Float_Text_IO;

with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with Gdk.RGBA;
with Glib.Convert;             use Glib.Convert;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Cell_Renderer;        use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Sortable;        use Gtk.Tree_Sortable;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model_Sort;      use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;

with GNAThub.Messages;         use GNAThub.Messages;
with GNAThub.Module;           use GNAThub.Module;

with Default_Preferences;      use Default_Preferences;
with GPS.Default_Styles;       use GPS.Default_Styles;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Location_View;        use GPS.Location_View;
with String_Utils;             use String_Utils;
with Language.Icons;           use Language.Icons;

package body GNAThub.Reports.Messages is

   Me : constant Trace_Handle := Create ("GNATHUB.REPORTS.MESSAGES");

   type Row_Kind_Type is
     (Total_Kind, Project_Kind, Dir_Kind, File_Kind, Subprogram_Kind);
   --  The different kind of rows displayed in the tree view

   type Update_Action_Type is
     (Message_Added, Message_Removed, Metric_Added);
   --  The different kind of actions that can update the tree view

   Total_Row_Name : constant String := "Total nb messages:";
   --  The total row's name

   Entity_Name_Column_Min_Width : constant := 250;
   --  The entity name column minimum width

   Prj_Pixbuf_Cst   : constant String := "gps-emblem-project-closed";
   --  Name of the icon used for project nodes in the analysis report

   Dir_Pixbuf_Cst   : constant String := "gps-emblem-directory-closed";
   --  Name of the icon used for directory nodes in the analysis report

   File_Pixbuf_Cst  : constant String := "gps-emblem-file-unmodified";
   --  Name of the icon used for file nodes in the analysis report

   Unknown_Name     : constant String := "<others>";
   --  Name of the node containing the files outside of the project.

   Entity_Icon_Column    : constant := 0;
   --  Column containing the name of the entity icon to display.

   Entity_ID_Column      : constant := 1;
   --  Column containing the entity's unique ID.

   Entity_Name_Column    : constant := 2;
   --  Column containing the name of the entity.

   Total_Column          : constant := 3;
   --  Column displaying the number of messages per entity.

   package Sorting_Functions is new Gtk.Tree_Sortable.Set_Sort_Func_User_Data
     (GNAThub_Report_Tree_View);

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
      Info            : Semantic_Node_Info;
      Update_Action   : Update_Action_Type;
      Column          : Gint);
   --  Create and initialize a row in the tree.

   procedure Set_Row_Colors
     (Self : not null access GNAThub_Report_Messages_Record'Class;
      Iter : Gtk_Tree_Iter);
   --  Set the severity background colors for the given row.

   procedure Recompute_Metric_Columns_Colors
     (Self : not null access GNAThub_Report_Messages_Record'Class);
   --  Recompute the color of each metric column. alternating them to make them
   --  more visible.

   procedure Create_Or_Update_Row
     (Self          : not null access GNAThub_Report_Messages_Record'Class;
      File          : GNATCOLL.VFS.Virtual_File;
      Entity        : Entity_Data;
      Update_Action : Update_Action_Type;
      Column        : Gint;
      Metric_Value  : Float := 0.0);
   --  Create or update the tree according to the given file/project/entity.
   --  Update_Action is used to increment/decrement the row's counters
   --  depending on the nature of the action (i.e: we should increment
   --  counters when a message is being added etc.);

   function Get_New_Value_For_Severity
     (Current_Value : String;
      Value_To_Add  : Gint) return String;
   --  Return a suitable string value for the severity total columns.
   --  Return an empty string when the new value is 0.

   function Pretty_Print_Metric_Value (Value : Float) return String;
   --  Return a suitable string for the given metric value

   function Entity_Name_Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree  : GNAThub_Report_Tree_View) return Gint;
   --  Used to sort the column displaying entity names.

   function Metric_Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree  : GNAThub_Report_Tree_View) return Gint;
   --  USed to sort metric columns.

   function Get_ID
     (Self : not null access GNAThub_Report_Tree_View_Record'Class;
      Row  : Gtk_Tree_Iter) return String;
   --  Return the ID for the given iter

   procedure Show_Messages
     (Kernel : not null Kernel_Handle;
      ID     : String);
   --  Filter the Locations view to only show the message related to ID

   package Selection_Changed_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk_Tree_Selection_Record, GNAThub_Report_Tree_View);
   package On_Scroll_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk_Adjustment_Record, Gtk_Scrolled_Window);

   procedure On_Selection_Changed
     (Self      : access Gtk_Tree_Selection_Record'Class;
      Sync_Tree : GNAThub_Report_Tree_View);
   procedure On_Row_Expanded
     (Self : access Glib.Object.GObject_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);
   procedure On_Row_Collapsed
     (Self : access Glib.Object.GObject_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);
   procedure On_Scroll
     (Self         : access Gtk_Adjustment_Record'Class;
      Sync_Scolled : Gtk_Scrolled_Window);
   --  Callbacks used to synchronize both trees (entities and report).

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

   -------------------------------
   -- Pretty_Print_Metric_Value --
   -------------------------------

   function Pretty_Print_Metric_Value (Value : Float) return String is
      Tmp   : Unbounded_String;
      Index : Integer := 1;
   begin
      if Float'Truncation (Value) = Value then
         Tmp :=
           To_Unbounded_String
             (Integer'Image (Integer (Float'Truncation (Value))));
      else
         declare
            S : String (1 .. Float'Digits);
         begin
            --  Display only one digit after the '.'
            Ada.Float_Text_IO.Put (S, Value, Aft => 1, Exp => 0);
            Tmp := To_Unbounded_String (S);
         end;
      end if;

      --  Strip the whitespaces at the beginning
      GNATCOLL.Utils.Skip_Blanks (To_String (Tmp), Index);
      Delete (Tmp, 1, Index - 1);

      return To_String (Tmp);
   end Pretty_Print_Metric_Value;

   ---------------------------
   -- Entity_Name_Sort_Func --
   ---------------------------

   function Entity_Name_Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree  : GNAThub_Report_Tree_View) return Gint
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
      elsif B_Value = Total_Row_Name then
         return (if Order = Sort_Descending then -1 else 1);
      elsif A_Value > B_Value then
         return 1;
      else
         return -1;
      end if;
   end Entity_Name_Sort_Func;

   ----------------------
   -- Metric_Sort_Func --
   ----------------------

   function Metric_Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter;
      Tree  : GNAThub_Report_Tree_View) return Gint
   is
      Column : Gint;
      Order  : Gtk_Sort_Type;
   begin
      Gtk.Tree_Model_Sort.Get_Sort_Column_Id
        (Tree.Sortable_Model, Column, Order);

      declare
         S_A : constant String := Get_String (Model, A, Column);
         S_B : constant String := Get_String (Model, B, Column);
      begin
         if S_A = "" then
            return 1;
         elsif S_B = "" then
            return -1;
         end if;

         declare
            Val_A : constant Float := Float'Value (S_A);
            Val_B : constant Float := Float'Value (S_B);
         begin
            if Val_A < Val_B then
               return 1;
            else
               return -1;
            end if;
         end;
      end;
   end Metric_Sort_Func;

   --------------------
   -- Set_Row_Colors --
   --------------------

   procedure Set_Row_Colors
     (Self : not null access GNAThub_Report_Messages_Record'Class;
      Iter : Gtk_Tree_Iter)
   is
      Model : constant Gtk_Tree_Store := Self.Entities_Tree.Model;
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

   -------------------------------------
   -- Recompute_Metric_Columns_Colors --
   -------------------------------------

   procedure Recompute_Metric_Columns_Colors
     (Self : not null access GNAThub_Report_Messages_Record'Class)
   is
      Tree_Column     : Gtk_Tree_View_Column;
      Alternate_Color : Boolean := False;
   begin
      --  Iterate over al the columns to alternate their background color
      --  (make things more visible when there are a lot of columns).
      for Idx in
        0 .. Gint (Self.Metric_Rules_Column_IDs.Length) - 1
      loop
         Tree_Column := Self.Report_Tree.Get_Column (Idx);

         if Tree_Column.Get_Visible then
            declare
               Cells : Cell_Renderer_List.Glist := Tree_Column.Get_Cells;
            begin
               if Alternate_Color then
                  Gdk.RGBA.Set_Property
                    (Cell_Renderer_List.Get_Data (Cells),
                     Gtk.Cell_Renderer_Text.Background_Rgba_Property,
                     Get_Background (Editor_Ephemeral_Highlighting_Simple));
               else
                  Gdk.RGBA.Set_Property
                    (Cell_Renderer_List.Get_Data (Cells),
                     Gtk.Cell_Renderer_Text.Background_Rgba_Property,
                     Get_Background (Editor_Default_Style));
               end if;

               Cell_Renderer_List.Free (Cells);
               Alternate_Color := not Alternate_Color;
            end;
         end if;
      end loop;
   end Recompute_Metric_Columns_Colors;

   ----------------
   -- Create_Row --
   ----------------

   procedure Create_Row
     (Self          : not null access GNAThub_Report_Messages_Record'Class;
      Iter          : out Gtk_Tree_Iter;
      Parent        : Gtk_Tree_Iter;
      Kind          : Row_Kind_Type;
      Name          : String;
      ID            : String;
      Info          : Semantic_Node_Info;
      Update_Action : Update_Action_Type;
      Column        : Gint)
   is
      Model : constant Gtk_Tree_Store := Self.Entities_Tree.Model;
      Value : constant Gint :=
        (case Update_Action is
            when Message_Added   => 1,
            when Message_Removed => -1,
            when others          => 0);

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
            when Subprogram_Kind =>
               Stock_From_Category (Is_Declaration => Info.Is_Decl,
                                    Visibility     => Info.Visibility,
                                    Category       => Info.Category));

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

      if Column /= -1 then
         case Update_Action is
            when Message_Added | Message_Removed =>
               Model.Set
                 (Iter   => Iter,
                  Column => Column,
                  Value  => Get_New_Value_For_Severity
                    (Current_Value => "",
                     Value_To_Add  => Value));

            when Metric_Added =>
               null;
         end case;
      end if;

      Self.Set_Row_Colors (Iter);
   end Create_Row;

   -----------------------------
   -- Update_Tree_For_Message --
   -----------------------------

   procedure Create_Or_Update_Row
     (Self          : not null access GNAThub_Report_Messages_Record'Class;
      File          : GNATCOLL.VFS.Virtual_File;
      Entity        : Entity_Data;
      Update_Action : Update_Action_Type;
      Column        : Gint;
      Metric_Value  : Float := 0.0)
   is
      Model        : constant Gtk_Tree_Store := Self.Entities_Tree.Model;
      Value        : constant Gint := (case Update_Action is
                                          when Message_Added   => 1,
                                          when Message_Removed => -1,
                                          when Metric_Added    => 0);
      Dummy        : Gtk_Tree_Iter;
      Path         : Gtk_Tree_Path;
      Project      : Project_Type := No_Project;

      function Insert_Or_Update_Row
        (Parent : Gtk_Tree_Iter;
         Kind   : Row_Kind_Type;
         Name   : String;
         ID     : String;
         Info   : Semantic_Node_Info) return Gtk_Tree_Iter;

      --------------------------
      -- Insert_Or_Update_Row --
      --------------------------

      function Insert_Or_Update_Row
        (Parent : Gtk_Tree_Iter;
         Kind   : Row_Kind_Type;
         Name   : String;
         ID     : String;
         Info   : Semantic_Node_Info) return Gtk_Tree_Iter
      is
         Escaped_Name : constant String := Escape_Text (Name);
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
            --  Update the row's counters. Negative total can happen when
            --  messages are being removed after the tree view's model has
            --  been cleared: avoid displaying them.

            if New_Total >= 0 then
               Model.Set
                 (Iter,
                  Column => Total_Column,
                  Value  => New_Total);
            end if;

            if Column /= -1 then
               case Update_Action is
                  when Message_Added | Message_Removed =>
                     Model.Set
                       (Iter,
                        Column => Column,
                        Value  => Get_New_Value_For_Severity
                          (Current_Value => Model.Get_String (Iter, Column),
                           Value_To_Add  => Value));

                  when Metric_Added =>
                     --  Metrics are added once and don't need to get updated
                     null;
               end case;
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
               Parent        => Parent,
               Kind          => Kind,
               Name          => Escaped_Name,
               ID            => ID,
               Info          => Info,
               Update_Action => Update_Action,
               Column        => Column);
         else
            Update_Row (Iter);
            Self.Entities_Tree.Refilter (Iter);
            Self.Report_Tree.Refilter (Iter);
         end if;

         return Iter;
      end Insert_Or_Update_Row;

   begin

      Dummy := Insert_Or_Update_Row
        (Parent => Null_Iter,
         Kind   => Total_Kind,
         Name   => Total_Row_Name,
         ID     => Total_Row_Name,
         Info   => No_Node_Info);
      Dummy := Null_Iter;

      declare
         F_Info  : constant File_Info'Class :=
           File_Info'Class
             (Self.Kernel.Registry.Tree.Info_Set (File).First_Element);
      begin
         Project := F_Info.Project;

         --  Associate .gpr files with their coresponding project
         if Project = No_Project and then Has_Suffix (File, ".gpr") then
            Project := Lookup_Project (Self.Kernel, File);

            Dummy := Insert_Or_Update_Row
              (Parent => Dummy,
               Kind   => Project_Kind,
               Name   => Project.Name,
               ID     => Project.Project_Path.Display_Full_Name,
               Info   => No_Node_Info);

            if Update_Action = Metric_Added then
               Model.Set
                 (Dummy,
                  Column => Column,
                  Value  => Pretty_Print_Metric_Value (Metric_Value));
            end if;

            return;
         end if;

         Dummy := Insert_Or_Update_Row
           (Parent => Dummy,
            Kind   => Project_Kind,
            Name   => (if Project /= No_Project
                       then Project.Name
                       else Unknown_Name),
            ID     => (if Project /= No_Project
                       then Project.Project_Path.Display_Full_Name
                       else Unknown_Name),
            Info   => No_Node_Info);

         if Project = No_Project then
            --  This file is related to no project thus don't show it as
            --  relative to nothing.
            Normalize_Path (File, False);
         end if;
      exception
         when E : others =>
            Trace (Me,
                   "Issue when trying to link "
                   & File.Display_Full_Name
                   & " to the loaded project with the following error: "
                   & Ada.Exceptions.Exception_Message (E));
      end;

      --  Insert/update or get the directory

      Dummy := Insert_Or_Update_Row
        (Parent => Dummy,
         Kind   => Dir_Kind,
         Name   => File.Get_Parent.Display_Base_Dir_Name,
         ID     => File.Display_Dir_Name,
         Info   => No_Node_Info);

      --  Expand the nodes until the file
      Path := Model.Get_Path (Dummy);
      Self.Entities_Tree.Expand_To_Path (Path);
      Self.Report_Tree.Expand_To_Path (Path);
      Path_Free (Path);

      --  Insert/update or get the file

      Dummy := Insert_Or_Update_Row
        (Parent => Dummy,
         Kind   => File_Kind,
         Name   => File.Display_Base_Name,
         ID     => File.Display_Full_Name,
         Info   => No_Node_Info);

      --  Insert/update or get the subprogram

      if Entity /= No_Entity_Data then
         declare
            Subp_Name : constant String := To_String (Entity.Name);
            Line      : constant String := Integer'Image (Entity.Line);
            ID        : constant String :=
              File.Display_Full_Name & File_Line_Sep & Line;
            Info      : constant Semantic_Node_Info := Entity.Info;
         begin
            Dummy := Insert_Or_Update_Row
              (Parent => Dummy,
               Kind   => Subprogram_Kind,
               Name   => (if Subp_Name /= "" then Subp_Name else "<dummy>"),
               ID     => ID,
               Info   => Info);

            if Update_Action = Metric_Added then
               Model.Set
                 (Dummy,
                  Column => Column,
                  Value  => Pretty_Print_Metric_Value (Metric_Value));
            end if;
         end;
      elsif Update_Action = Metric_Added then
         Model.Set
           (Dummy,
            Column => Column,
            Value  => Pretty_Print_Metric_Value (Metric_Value));
      end if;
   end Create_Or_Update_Row;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Messages_And_Metrics_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
   is
      procedure Show_Message_Columns_If_Needed;
      --  Show the columns that display messages if they are not already shown.

      ------------------------------------
      -- Show_Message_Columns_If_Needed --
      ------------------------------------

      procedure Show_Message_Columns_If_Needed is
      begin
         Self.View.Report_Tree.Get_Column
           (Self.View.Total_Messages_Col).Set_Visible (True);

         if not Self.View.Message_Columns_Shown then
            for Columns_Info of Self.View.Severities_Columns_Info loop
               Self.View.Report_Tree.Get_Column
                 (Columns_Info.Tree_Col).Set_Visible (True);
            end loop;

            Self.View.Message_Columns_Shown := True;
         end if;
      end Show_Message_Columns_If_Needed;

   begin
      if Message.all in GNAThub_Message'Class then
         Show_Message_Columns_If_Needed;

         if Message.Get_Flags (Locations) then
            declare
               Msg      : constant GNAThub_Message_Access :=
                 GNAThub_Message_Access (Message);
               File     : constant GNATCOLL.VFS.Virtual_File := Msg.Get_File;
               Severity : constant Severity_Access := Msg.Get_Severity;
               Column   : constant Gint := Self.View.Severities_Columns_Info
                 (Severity.Get_Name).Total_Col;
            begin
               Self.View.Create_Or_Update_Row
                 (File          => File,
                  Entity        => Msg.Get_Entity,
                  Update_Action => Message_Added,
                  Column        => Column);
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
            Msg      : constant GNAThub_Message_Access :=
              GNAThub_Message_Access (Message);
            File     : constant GNATCOLL.VFS.Virtual_File := Msg.Get_File;
            Severity : constant Severity_Access := Msg.Get_Severity;
            Column   : constant Gint := Self.View.Severities_Columns_Info
              (Severity.Get_Name).Total_Col;
         begin
            Self.View.Create_Or_Update_Row
              (File          => File,
               Entity        => Msg.Get_Entity,
               Update_Action => Message_Removed,
               Column        => Column);
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
         Entity        => Metric.Get_Entity,
         Update_Action => Metric_Added,
         Column        => Self.View.Metric_Rules_Column_IDs
           (Metric.Get_Rule.Name).Model_Col,
         Metric_Value  => Metric.Get_Value);
   end Metric_Added;

   --------------------------------
   -- Metrics_Visibility_Changed --
   --------------------------------

   overriding procedure Metrics_Visibility_Changed
     (Self    : not null access Messages_And_Metrics_Listener;
      Metrics : Rule_Sets.Set)
   is
      Tree_Column : Gtk_Tree_View_Column;
   begin
      --  Hide all the metrics columns first...
      for Metric_Columns_Info of Self.View.Metric_Rules_Column_IDs loop
         Tree_Column := Self.View.Report_Tree.Get_Column
           (Metric_Columns_Info.Tree_Col);
         Tree_Column.Set_Visible (False);
      end loop;

      --  And show only the ones that should be visible now
      for Metric of Metrics loop
         Tree_Column := Self.View.Report_Tree.Get_Column
           (Self.View.Metric_Rules_Column_IDs (Metric.Name).Tree_Col);
         Tree_Column.Set_Visible (True);
      end loop;

      Recompute_Metric_Columns_Colors (Self.View);
   end Metrics_Visibility_Changed;

   ----------------------
   -- Get_Column_Types --
   ----------------------

   function Get_Column_Types
     (Self : not null access GNAThub_Report_Messages_Record'Class)
      return GType_Array
   is
      Nb_Severities : constant Guint := Guint (Self.Severities.Length);
      Last_Col_Idx  : constant Guint :=
        Total_Column + Nb_Severities * 2
          + Guint (Self.Metric_Rules_Column_IDs.Length);

      Column_Types  : GType_Array
        (Entity_Icon_Column .. Last_Col_Idx) :=
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

      for Metric_Columns_Info of Self.Metric_Rules_Column_IDs loop
         Column_Types (Guint (Metric_Columns_Info.Model_Col)) := GType_String;
      end loop;

      return Column_Types;
   end Get_Column_Types;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Self       : out GNAThub_Report_Messages;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Severities : GNAThub.Severities_Ordered_Sets.Set;
      Metrics    : Rule_Sets.Set)
   is
      Nb_Severities          : constant Gint := Gint (Severities.Length);
      Column                 : Gtk_Tree_View_Column;
      Icon_Renderer          : Gtk_Cell_Renderer_Pixbuf;
      Text_Renderer          : Gtk_Cell_Renderer_Text;
      Dummy                  : Gint;
      Scrolled_1, Scrolled_2 : Gtk_Scrolled_Window;

      procedure Assign_Columns_To_Severities;

      procedure Assign_Columns_To_Metrics;

      ----------------------------------
      -- Assign_Columns_To_Severities --
      ----------------------------------

      procedure Assign_Columns_To_Severities is
         Severity_Total_Col : Gint := Total_Column + 1;
      begin
         for Severity of Self.Severities loop
            Self.Severities_Columns_Info.Insert
              (Key      => Severity.Get_Name,
               New_Item => Severity_Columns_Info_Type'
                 (Total_Col => Severity_Total_Col,
                  Color_Col => Severity_Total_Col + Nb_Severities,
                  Tree_Col  => -1));
            Severity_Total_Col := Severity_Total_Col + 1;
         end loop;
      end Assign_Columns_To_Severities;

      -------------------------------
      -- Assign_Columns_To_Metrics --
      -------------------------------

      procedure Assign_Columns_To_Metrics is
         Metric_Col : Gint := Total_Column + Nb_Severities * 2 + 1;
      begin
         for Metric of Self.Metrics loop
            Self.Metric_Rules_Column_IDs.Insert
              (Key      => Metric.Name,
               New_Item => Metric_Columns_Info_Type'
                 (Has_Metrics => False,
                  Model_Col   => Metric_Col,
                  Tree_Col    => -1));
            Metric_Col := Metric_Col + 1;
         end loop;
      end Assign_Columns_To_Metrics;

   begin
      Trace (Me, "Initializing the GNAThub Messages Report");

      --  Create the tree view

      Self := new GNAThub_Report_Messages_Record;
      Gtk.Paned.Initialize (Self, Orientation_Horizontal);
      Self.Kernel := Kernel;
      Self.Severities := Severities;
      Self.Metrics := Metrics;

      Assign_Columns_To_Severities;
      Assign_Columns_To_Metrics;

      Self.Entities_Tree := new GNAThub_Report_Tree_View_Record;
      Self.Entities_Tree.Kernel := Kernel_Handle (Kernel);
      Gtkada.Tree_View.Initialize
        (Widget           => Self.Entities_Tree,
         Column_Types     => Self.Get_Column_Types,
         Capability_Type  => Filtered_And_Sortable,
         Set_Visible_Func => True);
      Self.Entities_Tree.Set_Name ("gnathub-entities-tree");
      Self.Entities_Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Self.Entities_Tree.Set_Propagate_Filtered_Status (False);
      Gtk_New (Scrolled_1);
      Scrolled_1.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled_1.Add (Self.Entities_Tree);
      Scrolled_1.Set_Size_Request (Entity_Name_Column_Min_Width, -1);
      Self.Pack1 (Scrolled_1, Resize => False, Shrink => False);

      --  Create a tree view column to display an icon representing the type
      --  of the entity.

      Gtk_New (Column);
      Gtk_New (Icon_Renderer);
      Column.Set_Resizable (False);
      Column.Pack_Start (Icon_Renderer, Expand => False);
      Column.Add_Attribute (Icon_Renderer, "icon-name", Entity_Icon_Column);
      Dummy := Self.Entities_Tree.Append_Column (Column);

      --  Create a tree view column to display the name of entity.

      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Set_Min_Width (Entity_Name_Column_Min_Width);
      Column.Set_Resizable (True);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "markup", Entity_Name_Column);
      Column.Set_Sort_Column_Id (Entity_Name_Column);
      Dummy := Self.Entities_Tree.Append_Column (Column);

      Self.Report_Tree := new GNAThub_Report_Tree_View_Record;
      Self.Report_Tree.Kernel := Kernel_Handle (Kernel);
      Gtkada.Tree_View.Initialize
        (Widget => Self.Report_Tree,
         Source => Self.Entities_Tree);
      Self.Report_Tree.Set_Name ("gnathub-report-tree");
      Self.Report_Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Self.Report_Tree.Set_Propagate_Filtered_Status (False);
      Gtk_New (Scrolled_2);
      Scrolled_2.Set_Policy (Policy_Automatic, Policy_Automatic);
      Scrolled_2.Add (Self.Report_Tree);
      Self.Pack2 (Scrolled_2, Resize => True, Shrink => False);

      --  Create a tree view for each metric rule

      for Metric of Self.Metrics loop
         declare
            Metric_Columns_Info : constant Metric_Columns_Info_Type :=
              Self.Metric_Rules_Column_IDs (Metric.Name);
         begin
            --  Create the column for the given metric rule.
            --  Hide it by default.

            Gtk_New (Column);
            Gtk_New (Text_Renderer);
            Column.Set_Title (To_String (Metric.Name));
            Column.Set_Resizable (True);
            Column.Pack_Start (Text_Renderer, Expand => False);
            Column.Add_Attribute
              (Text_Renderer, "markup", Metric_Columns_Info.Model_Col);

            Column.Set_Visible (False);

            Column.Set_Sort_Column_Id (Metric_Columns_Info.Model_Col);
            Sorting_Functions.Set_Sort_Func
              (Sortable       => +Self.Report_Tree.Sortable_Model,
               Sort_Column_Id => Metric_Columns_Info.Model_Col,
               Sort_Func      => Metric_Sort_Func'Access,
               User_Data      => Self.Report_Tree);

            --  Append it to the tree view, assigning its column ID at the
            --  same time.
            Self.Metric_Rules_Column_IDs (Metric.Name).Tree_Col :=
              Self.Report_Tree.Append_Column (Column) - 1;
         end;
      end loop;

      --  Create a tree view column to display the entity's total number of
      --  messages.

      Gtk_New (Column);
      Gtk_New (Text_Renderer);
      Column.Set_Title ("Nb of messages");
      Column.Set_Resizable (True);
      Column.Pack_Start (Text_Renderer, Expand => False);
      Column.Add_Attribute (Text_Renderer, "markup", Total_Column);
      Column.Set_Sort_Column_Id (Total_Column);
      Self.Total_Messages_Col := Self.Report_Tree.Append_Column (Column) - 1;
      Column.Set_Visible (False);

      --  Create a tree view column for each severity

      for Severity of Self.Severities loop
         declare
            Columns_Info : constant Severity_Columns_Info_Type :=
              Self.Severities_Columns_Info (Severity.Get_Name);
         begin
            --  Create the column for the given severity.

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

            Column.Set_Visible (False);

            --  Append it to the tree view, assigning its column ID at the
            --  same time.

            Self.Severities_Columns_Info (Severity.Get_Name).Tree_Col :=
              Self.Report_Tree.Append_Column (Column) - 1;
         end;
      end loop;

      --  Trick to avoid showing expanding arrows on the report tree: expanding
      --  is done through the entities tree

      Gtk_New (Column);
      Dummy := Self.Report_Tree.Append_Column (Column);
      Column.Set_Visible (False);
      Self.Report_Tree.Set_Expander_Column (Column);

      --  Use a special sort function for the column displaying entity names
      --  in order to have the 'Total:' row always on top.

      Sorting_Functions.Set_Sort_Func
        (Sortable       => +Self.Entities_Tree.Sortable_Model,
         Sort_Column_Id => Entity_Name_Column,
         Sort_Func      => Entity_Name_Sort_Func'Access,
         User_Data      => Self.Entities_Tree);
      Sorting_Functions.Set_Sort_Func
        (Sortable       => +Self.Report_Tree.Sortable_Model,
         Sort_Column_Id => Entity_Name_Column,
         Sort_Func      => Entity_Name_Sort_Func'Access,
         User_Data      => Self.Report_Tree);

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

      Gtk_New (Self.Entities_Tree.Multipress, Widget => Self.Entities_Tree);
      Gtk_New (Self.Report_Tree.Multipress, Widget => Self.Report_Tree);
      Self.Entities_Tree.Multipress.On_Pressed
        (On_Multipress'Access, Slot => Self.Entities_Tree);
      Self.Report_Tree.Multipress.On_Pressed
        (On_Multipress'Access, Slot => Self.Report_Tree);

      --  Callbacks to synchronize both trees regarding selection, expansion
      --  and scrolling.

      Selection_Changed_Callbacks.Object_Connect
        (Widget      => Self.Entities_Tree.Get_Selection,
         Name        => Gtk.Tree_Selection.Signal_Changed,
         Slot_Object => Self.Entities_Tree.Get_Selection,
         Cb          => On_Selection_Changed'Access,
         User_Data   => Self.Report_Tree);
      Selection_Changed_Callbacks.Object_Connect
        (Widget      => Self.Report_Tree.Get_Selection,
         Name        => Gtk.Tree_Selection.Signal_Changed,
         Slot_Object => Self.Report_Tree.Get_Selection,
         Cb          => On_Selection_Changed'Access,
         User_Data   => Self.Entities_Tree);
      On_Scroll_Callbacks.Object_Connect
        (Widget      => Scrolled_1.Get_Vadjustment,
         Name        => Gtk.Adjustment.Signal_Value_Changed,
         Slot_Object => Scrolled_1.Get_Vadjustment,
         Cb          => On_Scroll'Access,
         User_Data   => Scrolled_2);
      On_Scroll_Callbacks.Object_Connect
        (Widget      => Scrolled_2.Get_Vadjustment,
         Name        => Gtk.Adjustment.Signal_Value_Changed,
         Slot_Object => Scrolled_2.Get_Vadjustment,
         Cb          => On_Scroll'Access,
         User_Data   => Scrolled_1);
      Self.Entities_Tree.On_Row_Expanded
        (On_Row_Expanded'Access, Slot => Self.Report_Tree);
      Self.Entities_Tree.On_Row_Collapsed
        (On_Row_Collapsed'Access, Slot => Self.Report_Tree);
   end Gtk_New;

   --------------------------------------
   -- Expand_Or_Collapse_Selected_Rows --
   --------------------------------------

   procedure Expand_Or_Collapse_Selected_Rows
     (Self    : not null GNAThub_Report_Messages;
      Command : Expansion_Command_Type) is
   begin
      Expand_Or_Collapse_Selected_Rows
        (Self.Entities_Tree, Command);
      Expand_Or_Collapse_Selected_Rows
        (Self.Report_Tree, Command);
   end Expand_Or_Collapse_Selected_Rows;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : not null access GNAThub_Report_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter) return Boolean
   is
      ID : constant String  := Self.Get_ID (Store_Iter);
   begin
      if ID = Unknown_Name then
         return not Hide_Others_Node.Get_Pref;
      else
         return True;
      end if;
   end Is_Visible;

   -------------------
   -- Show_Messages --
   -------------------

   procedure Show_Messages
     (Kernel : not null Kernel_Handle;
      ID     : String)
   is
      File : Virtual_File;

      function For_Each (Item : String) return Boolean;

      --------------
      -- For_Each --
      --------------

      function For_Each (Item : String) return Boolean is
      begin
         File := GNATCOLL.VFS.Create (+Item);
         return False; --  We are only interrested in the first token
      end For_Each;

   begin
      if Auto_Location_Filtering.Get_Pref then
         GNATCOLL.Utils.Split
           (Str      => ID,
            On       => File_Line_Sep,
            For_Each => For_Each'Unrestricted_Access);
         Set_Locations_Filter
           (Kernel, File.Display_Base_Name, Expand => True);
      end if;
   end Show_Messages;

   ------------
   -- Get_ID --
   ------------

   function Get_ID
     (Self : not null access GNAThub_Report_Tree_View_Record'Class;
      Row  : Gtk_Tree_Iter) return String
   is
     (if Row = Null_Iter then
         ""
      else
         Self.Model.Get_String (Row, Entity_ID_Column));

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (Self      : access Gtk_Tree_Selection_Record'Class;
      Sync_Tree : GNAThub_Report_Tree_View)
   is
      Tree      : constant GNAThub_Report_Tree_View :=
        GNAThub_Report_Tree_View (Self.Get_Tree_View);
      Model     : Gtk_Tree_Model;
      List      : Gtk_Tree_Path_List.Glist;
      Iter      : Gtk_Tree_Iter;
      Sort_Iter : Gtk_Tree_Iter;

      use type Gtk_Tree_Path_List.Glist;
   begin
      --  Lock the tree to avoid recursion
      if Tree.Locked then
         return;
      end if;

      Sync_Tree.Locked := True;

      Tree.Get_Selection.Get_Selected_Rows (Model, List);

      if Model = Null_Gtk_Tree_Model
        or else List = Gtk_Tree_Path_List.Null_List
      then
         Free_Path_List (List);
         return;
      end if;

      Sync_Tree.Get_Selection.Unselect_All;

      declare
         G_Iter : Gtk_Tree_Path_List.Glist;
         Path   : Gtk_Tree_Path;
      begin
         G_Iter := Gtk_Tree_Path_List.First (List);

         while G_Iter /= Gtk_Tree_Path_List.Null_List loop
            Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));
            Sort_Iter := Get_Iter (Model, Path);

            Iter := Tree.Convert_To_Store_Iter (Sort_Iter);

            Show_Messages (Tree.Kernel, Tree.Get_ID (Iter));

            Sync_Tree.Get_Selection.Select_Iter
              (Sort_Iter);
            G_Iter := Gtk_Tree_Path_List.Next (G_Iter);
         end loop;

         Free_Path_List (List);
      end;

      Sync_Tree.Locked := False;
   end On_Selection_Changed;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   procedure On_Row_Expanded
     (Self : access Glib.Object.GObject_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Iter);
      Tree : constant GNAThub_Report_Tree_View :=
        GNAThub_Report_Tree_View (Self);
      Dummy : Boolean;
   begin
      Dummy := Tree.Expand_Row
        (Path     => Path,
         Open_All => False);
   end On_Row_Expanded;

   ----------------------
   -- On_Row_Collapsed --
   ----------------------

   procedure On_Row_Collapsed
     (Self : access Glib.Object.GObject_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Iter);
      Tree : constant GNAThub_Report_Tree_View :=
        GNAThub_Report_Tree_View (Self);
      Dummy : Boolean;
   begin
      Dummy := Tree.Collapse_Row (Path);
   end On_Row_Collapsed;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll
     (Self         : access Gtk_Adjustment_Record'Class;
      Sync_Scolled : Gtk_Scrolled_Window) is
   begin
      Sync_Scolled.Get_Vadjustment.Set_Value (Self.Get_Value);
   end On_Scroll;

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

      function Is_Severity_Color_Pref return Boolean;
      --  Return True if the preference being changed is one of the
      --  Severity color preferences.

      ----------------------------
      -- Is_Severity_Color_Pref --
      ----------------------------

      function Is_Severity_Color_Pref return Boolean is
      begin
         for Severity of Self.View.Severities loop
            if Preference (Get_Color_Preference (Severity.Style)) = Pref then
               return True;
            end if;
         end loop;
         return False;
      end Is_Severity_Color_Pref;

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
         Self.View.Report_Tree.Model.Foreach
           (Change_Row_Color'Unrestricted_Access);
      elsif Pref = Preference (Hide_Others_Node) then
         Self.View.Entities_Tree.Refilter;
         Self.View.Report_Tree.Refilter;
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

      View       : constant GNAThub_Report_Tree_View :=
        GNAThub_Report_Tree_View (Self);
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
            if Line /= 1 then
               --  explicit Line number => Jump to the beginning of the entity
               Open_File_Action_Hook.Run
                 (View.Kernel,
                  File    => File,
                  Project => GNATCOLL.Projects.No_Project,
                  Line    => Line);
            else
               declare
                  Message : constant Message_Access :=
                    Get_First_Message
                      (Self     => View.Kernel.Get_Messages_Container,
                       Category => Null_Unbounded_String,
                       File     => File);
               begin
                  if Message /= null then
                     --  Jump to the first message in the file
                     Open_File_Action_Hook.Run
                       (View.Kernel,
                        File    => File,
                        Project => GNATCOLL.Projects.No_Project,
                        Line    => Message.Get_Line,
                        Column  => Message.Get_Column);
                  else
                     --  Jump to the beginning of the file
                     Open_File_Action_Hook.Run
                       (View.Kernel,
                        File    => File,
                        Project => GNATCOLL.Projects.No_Project,
                        Line    => Line);
                  end if;
               end;
            end if;
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
