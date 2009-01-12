-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2009, AdaCore                 --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with GPS.Intl; use GPS.Intl;
with Projects;

package body Code_Peer.Summary_Models is

   use type Code_Analysis.File_Access;
   use type Code_Analysis.Project_Access;
   use type Code_Analysis.Subprogram_Access;
   use type Code_Peer.Utilities.Counts;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Summary_Model_Record) is
   begin
      Self.Message_Categories.Clear;
   end Clear;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self : access Summary_Model_Record;
      File : Code_Analysis.File_Access)
      return Code_Analysis.Tree_Models.File_Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return new File_Item (File);
   end Create;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Project_Access)
      return Code_Analysis.Tree_Models.Project_Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return new Project_Item (Project);
   end Create;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self       : access Summary_Model_Record;
      Subprogram : Code_Analysis.Subprogram_Access)
      return Code_Analysis.Tree_Models.Subprogram_Item_Access
   is
      pragma Unreferenced (Self);

   begin
      return new Subprogram_Item (Subprogram);
   end Create;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Summary_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Entity_Name_Column
            | Informational_Base_Count_Column
            | Informational_Deltas_Count_Column
            | Informational_Current_Count_Column
            | Low_Base_Count_Column
            | Low_Deltas_Count_Column
            | Low_Current_Count_Column
            | Medium_Base_Count_Column
            | Medium_Deltas_Count_Column
            | Medium_Current_Count_Column
            | High_Base_Count_Column
            | High_Deltas_Count_Column
            | High_Current_Count_Column
            | Suppressed_Base_Count_Column
            | Suppressed_Deltas_Count_Column
            | Suppressed_Current_Count_Column
              =>
            return Glib.GType_String;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Summary_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Number_Of_Columns;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Summary_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      use type Code_Analysis.Tree_Models.Subprogram_Item_Access;

      Project_Node    : constant Project_Item_Access :=
                          Project_Item_Access (Self.Project (Iter));
      File_Node       : constant File_Item_Access :=
                          File_Item_Access (Self.File (Iter));
      Subprogram_Node : constant Subprogram_Item_Access :=
                          Subprogram_Item_Access (Self.Subprogram (Iter));

      procedure Set_Integer_Image (Item : Natural; Suppress_Zero : Boolean);

      procedure Set_Count_Image
        (Level   : Code_Peer.Message_Probability_Level;
         Current : Boolean);

      procedure Set_Deltas_Image
        (Level : Code_Peer.Message_Probability_Level);

      procedure Set_Deltas_Image (Added : Natural; Removed : Natural);

      ---------------------
      -- Set_Count_Image --
      ---------------------

      procedure Set_Count_Image
        (Level   : Code_Peer.Message_Probability_Level;
         Current : Boolean)
      is
      begin
         if Subprogram_Node /= null then
            if Current then
               Set_Integer_Image
                 (Subprogram_Node.Messages_Counts (Level).Current, True);

            else
               Set_Integer_Image
                 (Subprogram_Node.Messages_Counts (Level).Base, True);
            end if;

         elsif File_Node /= null then
            if Current then
               Set_Integer_Image
                 (File_Node.Messages_Counts (Level).Current, True);

            else
               Set_Integer_Image
                 (File_Node.Messages_Counts (Level).Base, True);
            end if;

         elsif Project_Node /= null then
            if Current then
               Set_Integer_Image
                 (Project_Node.Messages_Counts (Level).Current, True);

            else
               Set_Integer_Image
                 (Project_Node.Messages_Counts (Level).Base, True);
            end if;

         else
            --  "Total" line

            declare
               Counts : Code_Peer.Utilities.Messages_Counts;

            begin
               Code_Peer.Utilities.Compute_Messages_Count
                 (Self.Tree, Self.Message_Categories, Counts);

               if Current then
                  Set_Integer_Image (Counts (Level).Current, False);

               else
                  Set_Integer_Image (Counts (Level).Base, True);
               end if;
            end;
         end if;
      end Set_Count_Image;

      ----------------------
      -- Set_Deltas_Image --
      ----------------------

      procedure Set_Deltas_Image
        (Level   : Code_Peer.Message_Probability_Level) is
      begin
         if Subprogram_Node /= null then
            Set_Deltas_Image
              (Subprogram_Node.Messages_Counts (Level).Added,
               Subprogram_Node.Messages_Counts (Level).Removed);

         elsif File_Node /= null then
            Set_Deltas_Image
              (File_Node.Messages_Counts (Level).Added,
               File_Node.Messages_Counts (Level).Removed);

         elsif Project_Node /= null then
            Set_Deltas_Image
              (Project_Node.Messages_Counts (Level).Added,
               Project_Node.Messages_Counts (Level).Removed);

         else
            --  "Total" line

            declare
               Counts : Code_Peer.Utilities.Messages_Counts;

            begin
               Code_Peer.Utilities.Compute_Messages_Count
                 (Self.Tree, Self.Message_Categories, Counts);

               Set_Deltas_Image (Counts (Level).Added, Counts (Level).Removed);
            end;
         end if;
      end Set_Deltas_Image;

      ----------------------
      -- Set_Deltas_Image --
      ----------------------

      procedure Set_Deltas_Image (Added : Natural; Removed : Natural) is
         Added_Image   : constant String := Natural'Image (Added);
         Removed_Image : constant String := Natural'Image (Removed);

      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Added = 0 and then Removed = 0 then
            Glib.Values.Set_String (Value, "");

         else
            Glib.Values.Set_String
              (Value,
               '-'
               & Removed_Image (Removed_Image'First + 1 .. Removed_Image'Last)
               & "/+"
               & Added_Image (Added_Image'First + 1 .. Added_Image'Last));
         end if;
      end Set_Deltas_Image;

      -----------------------
      -- Set_Integer_Image --
      -----------------------

      procedure Set_Integer_Image (Item : Natural; Suppress_Zero : Boolean) is
         Image : constant String := Natural'Image (Item);

      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Item = 0 and then Suppress_Zero then
            Glib.Values.Set_String (Value, "");

         else
            Glib.Values.Set_String
              (Value, Image (Image'First + 1 .. Image'Last));
         end if;
      end Set_Integer_Image;

   begin
      case Column is
         when Entity_Name_Column =>
            if Subprogram_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, Subprogram_Node.Node.Name.all);

            elsif File_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, File_Node.Node.Name.Base_Name);

            elsif Project_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String
                 (Value, Projects.Project_Name (Project_Node.Node.Name));

            else
               --  "Total" line

               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, -"Total:");
            end if;

         when Informational_Base_Count_Column =>
            Set_Count_Image (Code_Peer.Informational, False);

         when Informational_Deltas_Count_Column =>
            Set_Deltas_Image (Code_Peer.Informational);

         when Informational_Current_Count_Column =>
            Set_Count_Image (Code_Peer.Informational, True);

         when Low_Base_Count_Column =>
            Set_Count_Image (Code_Peer.Low, False);

         when Low_Deltas_Count_Column =>
            Set_Deltas_Image (Code_Peer.Low);

         when Low_Current_Count_Column =>
            Set_Count_Image (Code_Peer.Low, True);

         when Medium_Base_Count_Column =>
            Set_Count_Image (Code_Peer.Medium, False);

         when Medium_Deltas_Count_Column =>
            Set_Deltas_Image (Code_Peer.Medium);

         when Medium_Current_Count_Column =>
            Set_Count_Image (Code_Peer.Medium, True);

         when High_Base_Count_Column =>
            Set_Count_Image (Code_Peer.High, False);

         when High_Deltas_Count_Column =>
            Set_Deltas_Image (Code_Peer.High);

         when High_Current_Count_Column =>
            Set_Count_Image (Code_Peer.High, True);

         when Suppressed_Base_Count_Column =>
            Set_Count_Image (Code_Peer.Suppressed, False);

         when Suppressed_Deltas_Count_Column =>
            Set_Deltas_Image (Code_Peer.Suppressed);

         when Suppressed_Current_Count_Column =>
            Set_Count_Image (Code_Peer.Suppressed, True);

         when others =>
            null;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model      : out Summary_Model;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Categories : Code_Peer.Message_Category_Sets.Set) is
   begin
      Model := new Summary_Model_Record;
      Initialize (Model, Tree, Categories);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model      : access Summary_Model_Record'Class;
      Tree       : Code_Analysis.Code_Analysis_Tree;
      Categories : Code_Peer.Message_Category_Sets.Set) is
   begin
      Code_Analysis.Tree_Models.Initialize (Model, Tree);

      Model.Tree := Tree;
      Model.Message_Categories := Categories;

      Model.Reconstruct;
   end Initialize;

   ----------------
   -- Is_Changed --
   ----------------

   overriding function Is_Changed
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean
   is
      pragma Unreferenced (Self, Project);

   begin
      return True;
   end Is_Changed;

   ----------------
   -- Is_Changed --
   ----------------

   overriding function Is_Changed
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean
   is
      pragma Unreferenced (Self, Project, File);

   begin
      return True;
   end Is_Changed;

   ----------------
   -- Is_Changed --
   ----------------

   overriding function Is_Changed
     (Self       : access Summary_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean
   is
      pragma Unreferenced (Self, Project, File, Subprogram);

   begin
      return True;
   end Is_Changed;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean
   is
      Project_Node : constant Project_Item_Access :=
                       Project_Item_Access (Project);

   begin
      Code_Peer.Utilities.Compute_Messages_Count
        (Project_Node.Node,
         Self.Message_Categories,
         Project_Node.Messages_Counts);
      Project_Node.Computed := True;

      return Self.Show_All_Projects
        or else Project_Node.Messages_Counts (Low)    /= (others => 0)
        or else Project_Node.Messages_Counts (Medium) /= (others => 0)
        or else Project_Node.Messages_Counts (High)   /= (others => 0);
   end Is_Visible;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self    : access Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean
   is
      pragma Unreferenced (Project);

      File_Node : constant File_Item_Access := File_Item_Access (File);

   begin
      if File.Node.Name.Base_Name = "Standard" then
         return False;
      end if;

      Code_Peer.Utilities.Compute_Messages_Count
        (File_Node.Node,
         Self.Message_Categories,
         File_Node.Messages_Counts);
      File_Node.Computed := True;

      return Self.Show_All_Files
        or else File_Node.Messages_Counts (Low)    /= (others => 0)
        or else File_Node.Messages_Counts (Medium) /= (others => 0)
        or else File_Node.Messages_Counts (High)   /= (others => 0);
   end Is_Visible;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self       : access Summary_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean
   is
      pragma Unreferenced (Project, File);

      Subprogram_Node : constant Subprogram_Item_Access :=
                          Subprogram_Item_Access (Subprogram);

   begin
      Code_Peer.Utilities.Compute_Messages_Count
        (Subprogram_Node.Node,
         Self.Message_Categories,
         Subprogram_Node.Messages_Counts);
      Subprogram_Node.Computed := True;

      return Self.Show_All_Subprograms
        or else Subprogram_Node.Messages_Counts (Low)    /= (others => 0)
        or else Subprogram_Node.Messages_Counts (Medium) /= (others => 0)
        or else Subprogram_Node.Messages_Counts (High)   /= (others => 0);
   end Is_Visible;

   ------------------------------
   -- Set_Show_All_Subprograms --
   ------------------------------

   procedure Set_Show_All_Subprograms
     (Self : access Summary_Model_Record'Class;
      Show : Boolean) is
   begin
      Self.Show_All_Subprograms := Show;
      Self.Reconstruct;
   end Set_Show_All_Subprograms;

   ------------------------------------
   -- Set_Visible_Message_Categories --
   ------------------------------------

   procedure Set_Visible_Message_Categories
     (Self : access Summary_Model_Record'Class;
      To   : Code_Peer.Message_Category_Sets.Set)
   is
   begin
      Self.Message_Categories := To;
      Self.Reconstruct;
   end Set_Visible_Message_Categories;

end Code_Peer.Summary_Models;
