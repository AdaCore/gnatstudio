------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Glib.Object;
with Gdk.Color;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPS.Intl;          use GPS.Intl;

with CodePeer.Module;

package body CodePeer.Messages_Summary_Models is

   use type Code_Analysis.Project_Access;
   use type Code_Analysis.Subprogram_Access;
   use type CodePeer.Utilities.Counts;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Messages_Summary_Model_Record) is
   begin
      Self.Message_Categories.Clear;
   end Clear;

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self : access Messages_Summary_Model_Record;
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
     (Self    : access Messages_Summary_Model_Record;
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
     (Self       : access Messages_Summary_Model_Record;
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
     (Self  : access Messages_Summary_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Entity_Icon_Column =>
            return Gdk.Pixbuf.Get_Type;

         when Entity_Name_Column
            | Entity_Lifeage_Column
            | Informational_Base_Count_Column
            | Informational_Added_Count_Column
            | Informational_Deltas_Count_Column
            | Informational_Removed_Count_Column
            | Informational_Current_Count_Column
            | Low_Base_Count_Column
            | Low_Added_Count_Column
            | Low_Deltas_Count_Column
            | Low_Removed_Count_Column
            | Low_Current_Count_Column
            | Medium_Base_Count_Column
            | Medium_Added_Count_Column
            | Medium_Deltas_Count_Column
            | Medium_Removed_Count_Column
            | Medium_Current_Count_Column
            | High_Base_Count_Column
            | High_Added_Count_Column
            | High_Deltas_Count_Column
            | High_Removed_Count_Column
            | High_Current_Count_Column
            | Suppressed_Base_Count_Column
            | Suppressed_Added_Count_Column
            | Suppressed_Deltas_Count_Column
            | Suppressed_Removed_Count_Column
            | Suppressed_Current_Count_Column
            | Total_Checks_Count_Column
            | Passed_Checks_Count_Column
              =>
            return Glib.GType_String;

         when High_Current_Color_Column
            | Medium_Current_Color_Column
            | Low_Current_Color_Column
            =>
            return Gdk.Color.Gdk_Color_Type;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Messages_Summary_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Number_Of_Columns;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Messages_Summary_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      use type Code_Analysis.Tree_Models.Subprogram_Item_Access;

      type Count_Kinds is (Base, Added, Removed, Current);

      Project_Node    : constant Project_Item_Access :=
                          Project_Item_Access (Self.Project (Iter));
      File_Node       : constant File_Item_Access :=
                          File_Item_Access (Self.File (Iter));
      Subprogram_Node : constant Subprogram_Item_Access :=
                          Subprogram_Item_Access (Self.Subprogram (Iter));

      procedure Set_Lifeage_Sign (Lifeage : Lifeage_Kinds);

      procedure Set_Integer_Image (Item : Natural; Suppress_Zero : Boolean);

      procedure Set_Count_Image
        (Level : CodePeer.Message_Ranking_Level;
         Kind  : Count_Kinds);

      procedure Set_Deltas_Image (Level : CodePeer.Message_Ranking_Level);

      procedure Set_Deltas_Image (Added : Natural; Removed : Natural);

      ---------------------
      -- Set_Count_Image --
      ---------------------

      procedure Set_Count_Image
        (Level : CodePeer.Message_Ranking_Level;
         Kind  : Count_Kinds)
      is
      begin
         if Subprogram_Node /= null then
            case Kind is
               when Base =>
                  Set_Integer_Image
                    (Subprogram_Node.Messages_Counts (Level).Base, True);

               when Added =>
                  Set_Integer_Image
                    (Subprogram_Node.Messages_Counts (Level).Added, True);

               when Removed =>
                  Set_Integer_Image
                    (Subprogram_Node.Messages_Counts (Level).Removed, True);

               when Current =>
                  Set_Integer_Image
                    (Subprogram_Node.Messages_Counts (Level).Current, True);
            end case;

         elsif File_Node /= null then
            case Kind is
               when Base =>
                  Set_Integer_Image
                    (File_Node.Messages_Counts (Level).Base, True);

               when Added =>
                  Set_Integer_Image
                    (File_Node.Messages_Counts (Level).Added, True);

               when Removed =>
                  Set_Integer_Image
                    (File_Node.Messages_Counts (Level).Removed, True);

               when Current =>
                  Set_Integer_Image
                    (File_Node.Messages_Counts (Level).Current, True);
            end case;

         elsif Project_Node /= null then
            case Kind is
               when Base =>
                  Set_Integer_Image
                    (Project_Node.Messages_Counts (Level).Base, True);

               when Added =>
                  Set_Integer_Image
                    (Project_Node.Messages_Counts (Level).Added, True);

               when Removed =>
                  Set_Integer_Image
                    (Project_Node.Messages_Counts (Level).Removed, True);

               when Current =>
                  Set_Integer_Image
                    (Project_Node.Messages_Counts (Level).Current, True);
            end case;

         else
            --  "Total" line

            declare
               Counts       : CodePeer.Utilities.Messages_Counts;
               Dummy_Checks : Natural;
               Dummy_Totals : Natural;

            begin
               CodePeer.Utilities.Compute_Messages_Count
                 (Self.Tree,
                  Self.Message_Categories,
                  Counts,
                  Dummy_Checks,
                  Dummy_Totals);

               case Kind is
                  when Base =>
                     Set_Integer_Image (Counts (Level).Base, True);

                  when Added =>
                     Set_Integer_Image (Counts (Level).Added, True);

                  when Removed =>
                     Set_Integer_Image (Counts (Level).Removed, True);

                  when Current =>
                     Set_Integer_Image (Counts (Level).Current, True);
               end case;
            end;
         end if;
      end Set_Count_Image;

      ----------------------
      -- Set_Deltas_Image --
      ----------------------

      procedure Set_Deltas_Image (Level : CodePeer.Message_Ranking_Level) is
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
               Counts       : CodePeer.Utilities.Messages_Counts;
               Dummy_Checks : Natural;
               Dummy_Totals : Natural;

            begin
               CodePeer.Utilities.Compute_Messages_Count
                 (Self.Tree,
                  Self.Message_Categories,
                  Counts,
                  Dummy_Checks,
                  Dummy_Totals);

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

      ----------------------
      -- Set_Lifeage_Sign --
      ----------------------

      procedure Set_Lifeage_Sign (Lifeage : Lifeage_Kinds) is
      begin
         Glib.Values.Init (Value, Glib.GType_String);

         case Lifeage is
            when Added =>
               Glib.Values.Set_String (Value, "+");

            when Unchanged =>
               Glib.Values.Set_String (Value, "");

            when Removed =>
               Glib.Values.Set_String (Value, "-");
         end case;
      end Set_Lifeage_Sign;

   begin
      case Column is
         when Entity_Icon_Column =>
            if Subprogram_Node /= null then
               Glib.Values.Init (Value, Glib.GType_Object);
               Glib.Values.Set_Object
                 (Value, Glib.Object.GObject (Self.Subprogram_Icon));

            elsif File_Node /= null then
               Glib.Values.Init (Value, Glib.GType_Object);
               Glib.Values.Set_Object
                 (Value, Glib.Object.GObject (Self.File_Icon));

            elsif Project_Node /= null then
               Glib.Values.Init (Value, Glib.GType_Object);
               Glib.Values.Set_Object
                 (Value, Glib.Object.GObject (Self.Project_Icon));

            else
               Glib.Values.Init (Value, Glib.GType_Object);
               Glib.Values.Set_Object (Value, null);
            end if;

         when Entity_Name_Column =>
            if Subprogram_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, Subprogram_Node.Node.Name.all);

            elsif File_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, +File_Node.Node.Name.Base_Name);

            elsif Project_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);

               if Project_Node.Node.Name = No_Project then
                  Glib.Values.Set_String (Value, -"RTL and removed");

               else
                  Glib.Values.Set_String
                    (Value, Project_Node.Node.Name.Name);
               end if;

            else
               --  "Total" line

               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, -"Total:");
            end if;

         when Entity_Lifeage_Column =>
            if Subprogram_Node /= null then
               Set_Lifeage_Sign
                 (CodePeer.Subprogram_Data
                    (Subprogram_Node.Node.Analysis_Data.CodePeer_Data.all).
                    Lifeage);

            elsif File_Node /= null then
               Set_Lifeage_Sign
                 (CodePeer.File_Data
                    (File_Node.Node.Analysis_Data.CodePeer_Data.all).Lifeage);

            else
               --  Projects and totals don't have lifeage

               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, "");
            end if;

         when Informational_Base_Count_Column =>
            Set_Count_Image (CodePeer.Informational, Base);

         when Informational_Added_Count_Column =>
            Set_Count_Image (CodePeer.Informational, Added);

         when Informational_Deltas_Count_Column =>
            Set_Deltas_Image (CodePeer.Informational);

         when Informational_Removed_Count_Column =>
            Set_Count_Image (CodePeer.Informational, Removed);

         when Informational_Current_Count_Column =>
            Set_Count_Image (CodePeer.Informational, Current);

         when Low_Base_Count_Column =>
            Set_Count_Image (CodePeer.Low, Base);

         when Low_Added_Count_Column =>
            Set_Count_Image (CodePeer.Low, Added);

         when Low_Deltas_Count_Column =>
            Set_Deltas_Image (CodePeer.Low);

         when Low_Removed_Count_Column =>
            Set_Count_Image (CodePeer.Low, Removed);

         when Low_Current_Count_Column =>
            Set_Count_Image (CodePeer.Low, Current);

         when Low_Current_Color_Column =>
            Glib.Values.Init (Value, Gdk.Color.Gdk_Color_Type);
            Gdk.Color.Set_Value (Value, CodePeer.Module.Get_Color (Low));

         when Medium_Base_Count_Column =>
            Set_Count_Image (CodePeer.Medium, Base);

         when Medium_Added_Count_Column =>
            Set_Count_Image (CodePeer.Medium, Added);

         when Medium_Deltas_Count_Column =>
            Set_Deltas_Image (CodePeer.Medium);

         when Medium_Removed_Count_Column =>
            Set_Count_Image (CodePeer.Medium, Removed);

         when Medium_Current_Count_Column =>
            Set_Count_Image (CodePeer.Medium, Current);

         when Medium_Current_Color_Column =>
            Glib.Values.Init (Value, Gdk.Color.Gdk_Color_Type);
            Gdk.Color.Set_Value (Value, CodePeer.Module.Get_Color (Medium));

         when High_Base_Count_Column =>
            Set_Count_Image (CodePeer.High, Base);

         when High_Added_Count_Column =>
            Set_Count_Image (CodePeer.High, Added);

         when High_Deltas_Count_Column =>
            Set_Deltas_Image (CodePeer.High);

         when High_Removed_Count_Column =>
            Set_Count_Image (CodePeer.High, Removed);

         when High_Current_Count_Column =>
            Set_Count_Image (CodePeer.High, Current);

         when High_Current_Color_Column =>
            Glib.Values.Init (Value, Gdk.Color.Gdk_Color_Type);
            Gdk.Color.Set_Value (Value, CodePeer.Module.Get_Color (High));

         when Suppressed_Base_Count_Column =>
            Set_Count_Image (CodePeer.Suppressed, Base);

         when Suppressed_Added_Count_Column =>
            Set_Count_Image (CodePeer.Suppressed, Added);

         when Suppressed_Deltas_Count_Column =>
            Set_Deltas_Image (CodePeer.Suppressed);

         when Suppressed_Removed_Count_Column =>
            Set_Count_Image (CodePeer.Suppressed, Removed);

         when Suppressed_Current_Count_Column =>
            Set_Count_Image (CodePeer.Suppressed, Current);

         when Total_Checks_Count_Column =>
            if Subprogram_Node /= null then
               --  Nothing to output, checks are counted per file

               Set_Integer_Image (0, True);

            elsif File_Node /= null then
               Set_Integer_Image
                 (CodePeer.File_Data
                    (File_Node.Node.Analysis_Data.CodePeer_Data.all).
                       Total_Checks,
                  True);

            elsif Project_Node /= null then
               Set_Integer_Image (Project_Node.Total_Checks, True);

            else
               declare
                  Counts : CodePeer.Utilities.Messages_Counts;
                  Checks : Natural;
                  Totals : Natural;

               begin
                  CodePeer.Utilities.Compute_Messages_Count
                    (Self.Tree,
                     Self.Message_Categories,
                     Counts,
                     Checks,
                     Totals);

                  Set_Integer_Image (Totals, True);
               end;
            end if;

         when Passed_Checks_Count_Column =>
            if Subprogram_Node /= null then
               --  Nothing to output, checks are counted per file

               Set_Integer_Image (0, True);

            elsif File_Node /= null then
               if CodePeer.File_Data
                    (File_Node.Node.Analysis_Data.CodePeer_Data.all).
                        Total_Checks < File_Node.Checks_Count
               then
                  --  KC30-001: total number of checks may be less than number
                  --  of check messages because of inconsistency of CodePeer
                  --  database. This code is used in such cases to prevent GPS
                  --  from crash.

                  Glib.Values.Init (Value, Glib.GType_String);
                  Glib.Values.Set_String (Value, "ERROR");

               else
                  Set_Integer_Image
                    (CodePeer.File_Data
                       (File_Node.Node.Analysis_Data.CodePeer_Data.all).
                          Total_Checks - File_Node.Checks_Count,
                     True);
               end if;

            elsif Project_Node /= null then
               if Project_Node.Total_Checks < Project_Node.Checks_Count then
                  --  KC30-001: total number of checks may be less than number
                  --  of check messages because of inconsistency of CodePeer
                  --  database. This code is used in such cases to prevent GPS
                  --  from crash.

                  Glib.Values.Init (Value, Glib.GType_String);
                  Glib.Values.Set_String (Value, "ERROR");

               else
                  Set_Integer_Image
                    (Project_Node.Total_Checks - Project_Node.Checks_Count,
                     True);
               end if;

            else
               declare
                  Counts : CodePeer.Utilities.Messages_Counts;
                  Checks : Natural;
                  Totals : Natural;

               begin
                  CodePeer.Utilities.Compute_Messages_Count
                    (Self.Tree,
                     Self.Message_Categories,
                     Counts,
                     Checks,
                     Totals);

                  Set_Integer_Image (Totals, True);
               end;
            end if;

         when others =>
            null;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model           : out Messages_Summary_Model;
      Tree            : Code_Analysis.Code_Analysis_Tree;
      Categories      : CodePeer.Message_Category_Sets.Set;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf) is
   begin
      Model := new Messages_Summary_Model_Record;
      Initialize
        (Model, Tree, Categories, Project_Icon, File_Icon, Subprogram_Icon);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model           : access Messages_Summary_Model_Record'Class;
      Tree            : Code_Analysis.Code_Analysis_Tree;
      Categories      : CodePeer.Message_Category_Sets.Set;
      Project_Icon    : Gdk.Pixbuf.Gdk_Pixbuf;
      File_Icon       : Gdk.Pixbuf.Gdk_Pixbuf;
      Subprogram_Icon : Gdk.Pixbuf.Gdk_Pixbuf) is
   begin
      Code_Analysis.Tree_Models.Initialize (Model, Tree);

      Model.Tree := Tree;
      Model.Message_Categories := Categories;

      Model.Project_Icon    := Project_Icon;
      Model.File_Icon       := File_Icon;
      Model.Subprogram_Icon := Subprogram_Icon;

      Model.Reconstruct;
   end Initialize;

   ----------------
   -- Is_Changed --
   ----------------

   overriding function Is_Changed
     (Self    : access Messages_Summary_Model_Record;
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
     (Self    : access Messages_Summary_Model_Record;
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
     (Self       : access Messages_Summary_Model_Record;
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
     (Self    : access Messages_Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access) return Boolean
   is
      Project_Node : constant Project_Item_Access :=
                       Project_Item_Access (Project);

   begin
      CodePeer.Utilities.Compute_Messages_Count
        (Project_Node.Node,
         Self.Message_Categories,
         Project_Node.Messages_Counts,
         Project_Node.Checks_Count,
         Project_Node.Total_Checks);
      Project_Node.Computed := True;

      return Self.Show_All_Projects
        or else Project_Node.Messages_Counts (Low)    /= (others => 0)
        or else Project_Node.Messages_Counts (Medium) /= (others => 0)
        or else Project_Node.Messages_Counts (High)   /= (others => 0)
        or else Project_Node.Total_Checks /= 0;
   end Is_Visible;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self    : access Messages_Summary_Model_Record;
      Project : Code_Analysis.Tree_Models.Project_Item_Access;
      File    : Code_Analysis.Tree_Models.File_Item_Access) return Boolean
   is
      pragma Unreferenced (Project);

      File_Node : constant File_Item_Access := File_Item_Access (File);

   begin
      if +File.Node.Name.Base_Name = "Standard" then
         return False;
      end if;

      CodePeer.Utilities.Compute_Messages_Count
        (File_Node.Node,
         Self.Message_Categories,
         File_Node.Messages_Counts,
         File_Node.Checks_Count);
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
     (Self       : access Messages_Summary_Model_Record;
      Project    : Code_Analysis.Tree_Models.Project_Item_Access;
      File       : Code_Analysis.Tree_Models.File_Item_Access;
      Subprogram : Code_Analysis.Tree_Models.Subprogram_Item_Access)
      return Boolean
   is
      pragma Unreferenced (Project, File);

      Subprogram_Node : constant Subprogram_Item_Access :=
                          Subprogram_Item_Access (Subprogram);
      Dummy           : Natural;

   begin
      CodePeer.Utilities.Compute_Messages_Count
        (Subprogram_Node.Node,
         Self.Message_Categories,
         Subprogram_Node.Messages_Counts,
         Dummy);
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
     (Self : access Messages_Summary_Model_Record'Class;
      Show : Boolean) is
   begin
      Self.Show_All_Subprograms := Show;
      Self.Reconstruct;
   end Set_Show_All_Subprograms;

   ------------------------------------
   -- Set_Visible_Message_Categories --
   ------------------------------------

   procedure Set_Visible_Message_Categories
     (Self : access Messages_Summary_Model_Record'Class;
      To   : CodePeer.Message_Category_Sets.Set)
   is
   begin
      Self.Message_Categories := To;
      Self.Reconstruct;
   end Set_Visible_Message_Categories;

end CodePeer.Messages_Summary_Models;
