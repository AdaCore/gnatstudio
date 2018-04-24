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

with Code_Analysis_GUI;   use Code_Analysis_GUI;
with Gdk.RGBA;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Default_Preferences; use Default_Preferences;
with GPS.Intl;          use GPS.Intl;
with GPS.Kernel;        use GPS.Kernel;
with GPS.Kernel.Hooks;  use GPS.Kernel.Hooks;
with GNAThub.Module;

package body GNAThub.Reports.Models is

   use Code_Analysis.Tree_Models;

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      Model : Messages_Model;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   function Has_Metric (P : GNAThub_Project_Access) return Boolean;
   function Has_Metric (F : GNAThub_File_Access) return Boolean;
   function Has_Metric (S : GNAThub_Subprogram_Access) return Boolean;
   --  Return True is the preference display_node_with_metric is True
   --  and then if the container or its children have metrics

   ---------------------
   -- Calculate_Total --
   ---------------------

   procedure Calculate_Total (Model : access Messages_Model_Record'Class) is
   begin
      for Index in Model.Total'Range loop
         Model.Total (Index).Value := 0;
      end loop;

      for Project of Model.Tree.all loop
         for Index in Model.Total'Range loop
            if Index in GNAThub_Project_Access (Project).Counts'Range then
               Model.Total (Index).Value := Model.Total (Index).Value +
                 GNAThub_Project_Access (Project).Counts (Index);
            end if;
         end loop;
      end loop;
   end Calculate_Total;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Kernel);

      function Is_Color_Pref return Boolean;

      -------------------
      -- Is_Color_Pref --
      -------------------

      function Is_Color_Pref return Boolean is
      begin
         for Index in Self.Model.Total'Range loop
            if Self.Model.Total (Index).Severity /= null
              and then
                Preference
                  (Get_Color_Preference
                     (Self.Model.Total (Index).Severity.Style)) = Pref
            then
               return True;
            end if;
         end loop;
         return False;
      end Is_Color_Pref;

   begin
      if Pref = null
        or else Is_Color_Pref
        or else Pref = Preference (GNAThub.Module.Hide_Node_Without_Messages)
      then
         Self.Model.Reconstruct;
      end if;
   end Execute;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Messages_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      use type Glib.Gint;

   begin
      if Index < Glib.Gint (Self.Total'Last) + 2 then
         return Glib.GType_String;

      elsif Index < Self.Get_N_Columns then
         return Gdk.RGBA.Get_Type;

      else
         return Glib.GType_Invalid;
      end if;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Messages_Model_Record) return Glib.Gint
   is
      use type Glib.Gint;

   begin
      return Glib.Gint (Self.Total'Last) * 2 + 2;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Messages_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      use type Glib.Gint;

      Project    : constant Project_Item_Access    := Self.Project (Iter);
      File       : constant File_Item_Access       := Self.File (Iter);
      Subprogram : constant Subprogram_Item_Access := Self.Subprogram (Iter);

      procedure Set (Item : Natural; Suppress_Zero : Boolean);

      ---------
      -- Set --
      ---------

      procedure Set (Item : Natural; Suppress_Zero : Boolean)
      is
         Image : constant String := Natural'Image (Item);
      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Item = 0 and then Suppress_Zero then
            Glib.Values.Set_String (Value, "");

         else
            Glib.Values.Set_String
              (Value, Image (Image'First + 1 .. Image'Last));
         end if;
      end Set;

      Index    : Natural;
      Suppress : Boolean;

   begin
      if Column = Entity_Icon_Name_Column then
         Glib.Values.Init (Value, Glib.GType_String);

         if Subprogram /= null then
            Glib.Values.Set_String (Value, Subp_Pixbuf_Cst);
         elsif File /= null then
            Glib.Values.Set_String (Value, File_Pixbuf_Cst);
         elsif Project /= null then
            Glib.Values.Set_String (Value, Prj_Pixbuf_Cst);
         else
            Glib.Values.Set_String (Value, "");
         end if;

      elsif Column = Entity_Name_Column then
         Glib.Values.Init (Value, Glib.GType_String);
         if Subprogram /= null then
            Glib.Values.Set_String (Value, Subprogram.Node.Name.all);

         elsif File /= null then
            Glib.Values.Set_String (Value, +File.Node.Name.Base_Name);

         elsif Project /= null then
            if Project.Node.Name = No_Project then
               Glib.Values.Set_String (Value, -"RTL and removed");

            else
               Glib.Values.Set_String (Value, Project.Node.Name.Name);
            end if;

         else
            --  "Total" line
            Glib.Values.Set_String (Value, -"Total:");
         end if;

      elsif Column < Glib.Gint (Self.Total'Last) + 2 then
         Index    := Natural (Column) - 1;
         Suppress := True;

         if Index = Natural (Self.Total'Last) then
            Suppress := False;
         end if;

         if Subprogram /= null then
            Set (GNAThub_Subprogram_Access
                 (Subprogram.Node).Counts (Index), Suppress);
         elsif File /= null then
            Set (GNAThub_File_Access
                 (File.Node).Counts (Index), Suppress);
         elsif Project /= null then
            Set (GNAThub_Project_Access
                 (Project.Node).Counts (Index), Suppress);
         else
            Set (Self.Total (Index).Value, Suppress);
         end if;

      elsif Column < Self.Get_N_Columns then
         Index := Natural (Column - Glib.Gint (Self.Total'Last) - 1);
         if Self.Total (Index).Severity /= null then
            Glib.Values.Init (Value, Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value
              (Value, Background (Self.Total (Index).Severity.Style));
         end if;
      end if;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model      : out Messages_Model;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set) is
   begin
      Model := new Messages_Model_Record (Natural (Severities.Length) + 1);
      Initialize (Model, Kernel, Tree, Severities);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model      : access Messages_Model_Record'Class;
      Kernel     : GPS.Kernel.Kernel_Handle;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Severities : GNAThub.Severities_Ordered_Sets.Set)
   is
      Index : Positive := 1;
      Hook  : access On_Pref_Changed;
   begin
      Model.Kernel := Kernel;
      Model.Tree   := Tree;

      for Severety of Severities loop
         Model.Total (Index).Severity := Severety;
         Index := Index + 1;
      end loop;

      Model.Calculate_Total;

      Code_Analysis.Tree_Models.Initialize (Model, Tree);
      Model.Reconstruct;

      Hook := new On_Pref_Changed;
      Hook.Model := Messages_Model (Model);
      Preferences_Changed_Hook.Add (Hook, Watch => Model);
   end Initialize;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self    : access Messages_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean
   is
      pragma Unreferenced (Self);

      Prj : constant GNAThub_Project_Access :=
        GNAThub_Project_Access (Project.Node);

   begin
      return Prj.Counts (Prj.Counts'Last) > 0 or else Has_Metric (Prj);
   end Is_Visible;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self    : access Messages_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Project);

      F : constant GNAThub_File_Access := GNAThub_File_Access (File.Node);
   begin
      if +File.Node.Name.Base_Name = "Standard" then
         return False;
      end if;

      return F.Counts (F.Counts'Last) > 0 or else Has_Metric (F);
   end Is_Visible;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : access Messages_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Project, File);

      S : constant GNAThub_Subprogram_Access :=
        GNAThub_Subprogram_Access (Subprogram.Node);
   begin
      return S.Counts (S.Counts'Last) > 0 or else Has_Metric (S);
   end Is_Visible;

   ----------------
   -- Has_Metric --
   ----------------

   function Has_Metric (P : GNAThub_Project_Access) return Boolean is
   begin
      if GNAThub.Module.Hide_Node_Without_Messages.Get_Pref then
         return False;
      end if;

      for F of P.Files loop
         if Has_Metric (GNAThub_File_Access (F)) then
            return True;
         end if;
      end loop;
      return not P.Metrics.Is_Empty;
   end Has_Metric;

   ----------------
   -- Has_Metric --
   ----------------

   function Has_Metric (F : GNAThub_File_Access) return Boolean is
   begin
      if GNAThub.Module.Hide_Node_Without_Messages.Get_Pref then
         return False;
      end if;

      for S of F.Subprograms loop
         if Has_Metric (GNAThub_Subprogram_Access (S)) then
            return True;
         end if;
      end loop;
      return not F.Metrics.Is_Empty;
   end Has_Metric;

   ----------------
   -- Has_Metric --
   ----------------

   function Has_Metric (S : GNAThub_Subprogram_Access) return Boolean is
   begin
      if GNAThub.Module.Hide_Node_Without_Messages.Get_Pref then
         return False;
      end if;

      return not S.Metrics.Is_Empty;
   end Has_Metric;

end GNAThub.Reports.Models;
