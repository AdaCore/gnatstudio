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

package body CodePeer.Utilities is

   ---------
   -- "+" --
   ---------

   function "+"
     (Left : Messages_Counts; Right : Messages_Counts) return Messages_Counts
   is
      Result : Messages_Counts;

   begin
      for J in CodePeer.Message_Ranking_Level'Range loop
         Result (J).Current := Left (J).Current + Right (J).Current;
      end loop;

      return Result;
   end "+";

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Categories : CodePeer.Message_Category_Sets.Set;
      CWEs       : CodePeer.CWE_Category_Sets.Set;
      Lifeages   : CodePeer.Lifeage_Kinds_Flags;
      Statuses   : CodePeer.Review_Status_Kinds_Flags;
      Counts     : out Messages_Counts;
      Checks     : out Natural)
   is
      procedure Process (Position : Message_Vectors.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Vectors.Cursor) is
         Message : constant Message_Access :=
                     Message_Vectors.Element (Position);

      begin
         --  Count messages of specified categories, lifeage and review status.

         if (Categories.Contains (Message.Category)
               or else not Categories.Intersection (Message.Checks).Is_Empty
               or else not CWEs.Intersection (Message.Category.CWEs).Is_Empty)
           and Lifeages (Message.Lifeage)
           and Statuses (Message.Status.Id)
         then
            Counts (Message.Ranking).Current :=
              Counts (Message.Ranking).Current + 1;
         end if;

         --  Count all non-removed checks with non-suppressed ranking

         if Message.Is_Check
           and then Message.Ranking /= Suppressed
           and then Message.Ranking /= Not_An_Error
           and then Message.Lifeage /= Removed
         then
            Checks := Checks + 1;
         end if;
      end Process;

   begin
      Counts := (others => (others => 0));
      Checks := 0;

      Subprogram_Data'Class
        (Subprogram.Analysis_Data.CodePeer_Data.all).Messages.Iterate
        (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (File       : Code_Analysis.File_Access;
      Categories : CodePeer.Message_Category_Sets.Set;
      CWEs       : CodePeer.CWE_Category_Sets.Set;
      Lifeages   : CodePeer.Lifeage_Kinds_Flags;
      Statuses   : CodePeer.Review_Status_Kinds_Flags;
      Counts     : out Messages_Counts;
      Checks     : out Natural)
   is
      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor) is
         Subprogram : constant Code_Analysis.Subprogram_Access :=
                        Code_Analysis.Subprogram_Maps.Element (Position);
         Aux_Counts : Messages_Counts;
         Aux_Checks : Natural;

      begin
         Compute_Messages_Count
           (Subprogram,
            Categories,
            CWEs,
            Lifeages,
            Statuses,
            Aux_Counts,
            Aux_Checks);

         Counts := Counts + Aux_Counts;
         Checks := Checks + Aux_Checks;
      end Process;

   begin
      Counts := (others => (others => 0));
      Checks := 0;

      File.Subprograms.Iterate (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (Project      : Code_Analysis.Project_Access;
      Categories   : CodePeer.Message_Category_Sets.Set;
      CWEs         : CodePeer.CWE_Category_Sets.Set;
      Lifeages     : CodePeer.Lifeage_Kinds_Flags;
      Statuses     : CodePeer.Review_Status_Kinds_Flags;
      Counts       : out Messages_Counts;
      Checks       : out Natural;
      Total_Checks : out Natural)
   is
      procedure Process (Position : Code_Analysis.File_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.File_Maps.Cursor) is
         File       : constant Code_Analysis.File_Access :=
                        Code_Analysis.File_Maps.Element (Position);
         Aux_Counts : Messages_Counts;
         Aux_Checks : Natural;

      begin
         Compute_Messages_Count
           (File,
            Categories,
            CWEs,
            Lifeages,
            Statuses,
            Aux_Counts,
            Aux_Checks);

         Counts := Counts + Aux_Counts;
         Checks := Checks + Aux_Checks;
         Total_Checks :=
           Total_Checks
             + CodePeer.File_Data'Class
                 (File.Analysis_Data.CodePeer_Data.all).Total_Checks;
      end Process;

   begin
      Counts := (others => (others => 0));
      Checks := 0;
      Total_Checks := 0;

      Project.Files.Iterate (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (Tree         : Code_Analysis.Code_Analysis_Tree;
      Categories   : CodePeer.Message_Category_Sets.Set;
      CWEs         : CodePeer.CWE_Category_Sets.Set;
      Lifeages     : CodePeer.Lifeage_Kinds_Flags;
      Statuses     : CodePeer.Review_Status_Kinds_Flags;
      Counts       : out Messages_Counts;
      Checks       : out Natural;
      Total_Checks : out Natural)
   is
      procedure Process (Position : Code_Analysis.Project_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Project_Maps.Cursor) is
         Project    : constant Code_Analysis.Project_Access :=
                        Code_Analysis.Project_Maps.Element (Position);
         Aux_Counts : Messages_Counts;
         Aux_Checks : Natural;
         Aux_Total  : Natural;

      begin
         Compute_Messages_Count
           (Project,
            Categories,
            CWEs,
            Lifeages,
            Statuses,
            Aux_Counts,
            Aux_Checks,
            Aux_Total);

         Counts := Counts + Aux_Counts;
         Checks := Checks + Aux_Checks;
         Total_Checks := Total_Checks + Aux_Total;
      end Process;

   begin
      Counts := (others => (others => 0));
      Checks := 0;
      Total_Checks := 0;

      Tree.Iterate (Process'Access);
   end Compute_Messages_Count;

end CodePeer.Utilities;
