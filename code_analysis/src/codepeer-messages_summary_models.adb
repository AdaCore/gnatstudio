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

with Code_Analysis_GUI; use Code_Analysis_GUI;
with Gdk.RGBA;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Utils;    use GNATCOLL.Utils;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GPS.Intl;          use GPS.Intl;
with CodePeer.Module;

package body CodePeer.Messages_Summary_Models is

   use type CodePeer.Utilities.Counts;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : access Messages_Summary_Model_Record) is
   begin
      Self.Message_Categories.Clear;
      Self.Message_Lifeages := (others => False);
      Self.Message_Statuses := (others => False);
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
         when Entity_Icon_Name_Column =>
            return Glib.GType_String;

         when Entity_Name_Column
            | Entity_Lifeage_Column
            | Low_Current_Count_Column
            | Medium_Current_Count_Column
            | High_Current_Count_Column
            | Total_Checks_Count_Column
            | Passed_Checks_Count_Column
              =>
            return Glib.GType_String;

         when High_Current_Color_Column
            | Medium_Current_Color_Column
            | Low_Current_Color_Column
            =>
            return Gdk.RGBA.Get_Type;

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
      Project_Node : constant Project_Item_Access :=
        Project_Item_Access (Self.Project (Iter));
      File_Node    : constant File_Item_Access :=
        File_Item_Access (Self.File (Iter));

      procedure Set_Lifeage_Sign (Lifeage : Lifeage_Kinds);
      --  Sets Value parameter to "+"/"-"/"" for Added/Removed/Unchanged
      --  subprograms/files.

      procedure Set_Integer_Image (Item : Natural; Suppress_Zero : Boolean);
      --  Sets Value parameter to textual representation of Item without
      --  leading space characters. If Suppress_Zero is True and Item is zero,
      --  empty string is set.

      procedure Set_Count_Image (Level : CodePeer.Message_Ranking_Level);
      --  Sets Value parameter to textual representation of count of messages
      --  of specified ranking level for current row. There are four kinds of
      --  row subprogram, file, project and total.

      function Percent_Image (Passed, Total : Natural) return String;
      --  Return a string representing the percentage Passed/Total in the
      --  form "(xxx%)"

      ---------------------
      -- Set_Count_Image --
      ---------------------

      procedure Set_Count_Image (Level : CodePeer.Message_Ranking_Level) is
      begin
         if File_Node /= null then
            Set_Integer_Image
              (File_Node.Messages_Counts (Level).Current, True);

         elsif Project_Node /= null then
            Set_Integer_Image
              (Project_Node.Messages_Counts (Level).Current, True);

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
                  Self.CWE_Categories,
                  Self.Message_Lifeages,
                  Self.Message_Statuses,
                  Counts,
                  Dummy_Checks,
                  Dummy_Totals);
               Set_Integer_Image (Counts (Level).Current, True);
            end;
         end if;
      end Set_Count_Image;

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

      -------------------
      -- Percent_Image --
      -------------------

      function Percent_Image (Passed, Total : Natural) return String is
      begin
         return "("
           & Image ((if Total = 0 then 100 else Passed * 100 / Total), 1)
           & "%)";
      end Percent_Image;

   begin
      case Column is
         when Entity_Icon_Name_Column =>
            Glib.Values.Init (Value, Glib.GType_String);

            if File_Node /= null then
               Glib.Values.Set_String (Value, File_Pixbuf_Cst);
            elsif Project_Node /= null then
               Glib.Values.Set_String (Value, Prj_Pixbuf_Cst);
            else
               Glib.Values.Set_String (Value, "");
            end if;

         when Entity_Name_Column =>
            if File_Node /= null then
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
            if File_Node /= null then
               Set_Lifeage_Sign
                 (CodePeer.File_Data
                    (File_Node.Node.Analysis_Data.CodePeer_Data.all).Lifeage);

            else
               --  Projects and totals don't have lifeage

               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, "");
            end if;

         when Low_Current_Count_Column =>
            Set_Count_Image (CodePeer.Low);

         when Low_Current_Color_Column =>
            Glib.Values.Init (Value, Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value (Value, CodePeer.Module.Get_Color (Low));

         when Medium_Current_Count_Column =>
            Set_Count_Image (CodePeer.Medium);

         when Medium_Current_Color_Column =>
            Glib.Values.Init (Value, Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value (Value, CodePeer.Module.Get_Color (Medium));

         when High_Current_Count_Column =>
            Set_Count_Image (CodePeer.High);

         when High_Current_Color_Column =>
            Glib.Values.Init (Value, Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value (Value, CodePeer.Module.Get_Color (High));

         when Passed_Checks_Count_Column =>
            if File_Node /= null then
               declare
                  Total  : constant Natural := CodePeer.File_Data
                    (File_Node.Node.Analysis_Data.CodePeer_Data.all).
                    Total_Checks;
                  Passed : constant Integer :=
                    Total - File_Node.Checks_Count;

               begin
                  Glib.Values.Init (Value, Glib.GType_String);

                  if Passed < 0 then
                     --  CodePeer reports incorrect total number of checks when
                     --  -messages max mode is used. See N212-045.

                     Glib.Values.Set_String (Value, "ERROR");

                  else
                     Glib.Values.Set_String
                       (Value,
                        Image (Passed, 1)
                        & " " & Percent_Image (Passed, Total));
                  end if;
               end;

            elsif Project_Node /= null then
               declare
                  Total  : constant Natural := Project_Node.Total_Checks;
                  Passed : constant Natural :=
                    Total - Project_Node.Checks_Count;

               begin
                  Glib.Values.Init (Value, Glib.GType_String);
                  Glib.Values.Set_String
                    (Value,
                     Image (Passed, 1)
                     & " " & Percent_Image (Passed, Total));
               end;

            else
               declare
                  Counts : CodePeer.Utilities.Messages_Counts;
                  Checks : Natural;
                  Totals : Natural;
                  Passed : Natural;

               begin
                  CodePeer.Utilities.Compute_Messages_Count
                    (Self.Tree,
                     Self.Message_Categories,
                     Self.CWE_Categories,
                     Self.Message_Lifeages,
                     Self.Message_Statuses,
                     Counts,
                     Checks,
                     Totals);

                  Passed := Totals - Checks;
                  Glib.Values.Init (Value, Glib.GType_String);
                  Glib.Values.Set_String
                    (Value,
                     Image (Passed, 1) & " " & Percent_Image (Passed, Totals));
               end;
            end if;

         when Total_Checks_Count_Column =>
            if File_Node /= null then
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
                     Self.CWE_Categories,
                     Self.Message_Lifeages,
                     Self.Message_Statuses,
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
      Categories      : CodePeer.Message_Category_Sets.Set) is
   begin
      Model := new Messages_Summary_Model_Record;
      Initialize (Model, Tree, Categories);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model           : access Messages_Summary_Model_Record'Class;
      Tree            : Code_Analysis.Code_Analysis_Tree;
      Categories      : CodePeer.Message_Category_Sets.Set) is
   begin
      Code_Analysis.Tree_Models.Initialize (Model, Tree);

      Model.Tree := Tree;
      Model.Message_Categories := Categories;
      Model.Message_Lifeages := (others => True);
      Model.Message_Statuses := (others => True);

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
         Self.CWE_Categories,
         Self.Message_Lifeages,
         Self.Message_Statuses,
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
         Self.CWE_Categories,
         Self.Message_Lifeages,
         Self.Message_Statuses,
         File_Node.Messages_Counts,
         File_Node.Checks_Count);
      File_Node.Computed := True;

      return Self.Show_All_Files
        or else File_Node.Messages_Counts (Low)    /= (others => 0)
        or else File_Node.Messages_Counts (Medium) /= (others => 0)
        or else File_Node.Messages_Counts (High)   /= (others => 0);
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

   --------------------------------
   -- Set_Visible_CWE_Categories --
   --------------------------------

   procedure Set_Visible_CWE_Categories
     (Self : access Messages_Summary_Model_Record'Class;
      To   : CodePeer.CWE_Category_Sets.Set) is
   begin
      Self.CWE_Categories := To;
      Self.Reconstruct;
   end Set_Visible_CWE_Categories;

   ------------------------------------
   -- Set_Visible_Message_Categories --
   ------------------------------------

   procedure Set_Visible_Message_Categories
     (Self : access Messages_Summary_Model_Record'Class;
      To   : CodePeer.Message_Category_Sets.Set) is
   begin
      Self.Message_Categories := To;
      Self.Reconstruct;
   end Set_Visible_Message_Categories;

   ----------------------------------
   -- Set_Visible_Message_Lifeages --
   ----------------------------------

   procedure Set_Visible_Message_Lifeages
     (Self : access Messages_Summary_Model_Record'Class;
      To   : CodePeer.Lifeage_Kinds_Flags) is
   begin
      Self.Message_Lifeages := To;
      Self.Reconstruct;
   end Set_Visible_Message_Lifeages;

   --------------------------------
   -- Set_Visible_Message_Status --
   --------------------------------

   procedure Set_Visible_Message_Status
     (Self : access Messages_Summary_Model_Record'Class;
      To   : CodePeer.Review_Status_Kinds_Flags) is
   begin
      Self.Message_Statuses := To;
      Self.Reconstruct;
   end Set_Visible_Message_Status;

end CodePeer.Messages_Summary_Models;
