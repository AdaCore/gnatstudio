-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2010, AdaCore                 --
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

package body Code_Peer.Utilities is

   ---------
   -- "+" --
   ---------

   function "+"
     (Left : Messages_Counts; Right : Messages_Counts) return Messages_Counts
   is
      Result : Messages_Counts;

   begin
      for J in Code_Peer.Message_Ranking_Level'Range loop
         Result (J).Base := Left (J).Base + Right (J).Base;
         Result (J).Added := Left (J).Added + Right (J).Added;
         Result (J).Removed := Left (J).Removed + Right (J).Removed;
         Result (J).Current := Left (J).Current + Right (J).Current;
      end loop;

      return Result;
   end "+";

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Categories : Code_Peer.Message_Category_Sets.Set;
      Counts     : out Messages_Counts)
   is
      procedure Process (Position : Message_Vectors.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Vectors.Cursor) is
         Message : constant Message_Access :=
                     Message_Vectors.Element (Position);

      begin
         if Categories.Contains (Message.Category) then
            case Message.Lifeage is
               when Added =>
                  Counts (Message.Current_Ranking).Added :=
                    Counts (Message.Current_Ranking).Added + 1;
                  Counts (Message.Current_Ranking).Current :=
                    Counts (Message.Current_Ranking).Current + 1;

               when Unchanged =>
                  Counts (Message.Current_Ranking).Base :=
                    Counts (Message.Current_Ranking).Base + 1;
                  Counts (Message.Current_Ranking).Current :=
                    Counts (Message.Current_Ranking).Current + 1;

               when Removed =>
                  Counts (Message.Current_Ranking).Base :=
                    Counts (Message.Current_Ranking).Base + 1;
                  Counts (Message.Current_Ranking).Removed :=
                    Counts (Message.Current_Ranking).Removed + 1;
            end case;
         end if;
      end Process;

   begin
      Counts := (others => (others => 0));

      Subprogram_Data'Class
        (Subprogram.Analysis_Data.Code_Peer_Data.all).Messages.Iterate
        (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (File       : Code_Analysis.File_Access;
      Categories : Code_Peer.Message_Category_Sets.Set;
      Counts     : out Messages_Counts)
   is
      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor) is
         Subprogram : constant Code_Analysis.Subprogram_Access :=
                        Code_Analysis.Subprogram_Maps.Element (Position);
         Aux        : Messages_Counts;

      begin
         Compute_Messages_Count (Subprogram, Categories, Aux);

         Counts := Counts + Aux;
      end Process;

   begin
      Counts := (others => (others => 0));

      File.Subprograms.Iterate (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (Project    : Code_Analysis.Project_Access;
      Categories : Code_Peer.Message_Category_Sets.Set;
      Counts     : out Messages_Counts)
   is
      procedure Process (Position : Code_Analysis.File_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.File_Maps.Cursor) is
         File : constant Code_Analysis.File_Access :=
                           Code_Analysis.File_Maps.Element (Position);
         Aux  : Messages_Counts;

      begin
         Compute_Messages_Count (File, Categories, Aux);

         Counts := Counts + Aux;
      end Process;

   begin
      Counts := (others => (others => 0));

      Project.Files.Iterate (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   procedure Compute_Messages_Count
     (Tree       : Code_Analysis.Code_Analysis_Tree;
      Categories : Code_Peer.Message_Category_Sets.Set;
      Counts     : out Messages_Counts)
   is
      procedure Process (Position : Code_Analysis.Project_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Project_Maps.Cursor) is
         Project : constant Code_Analysis.Project_Access :=
                     Code_Analysis.Project_Maps.Element (Position);
         Aux     : Messages_Counts;

      begin
         Compute_Messages_Count (Project, Categories, Aux);

         Counts := Counts + Aux;
      end Process;

   begin
      Counts := (others => (others => 0));

      Tree.Iterate (Process'Access);
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Level      : Code_Peer.Message_Ranking_Level;
      Category   : Code_Peer.Message_Category_Access) return Natural
   is
      Result : Natural := 0;

      procedure Process (Position : Message_Vectors.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Message_Vectors.Cursor) is
         Message : constant Message_Access :=
                     Message_Vectors.Element (Position);

      begin
         if Message.Current_Ranking = Level
           and then Message.Category = Category
         then
            Result := Result + 1;
         end if;
      end Process;

   begin
      Subprogram_Data'Class
        (Subprogram.Analysis_Data.Code_Peer_Data.all).Messages.Iterate
        (Process'Access);

      return Result;
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (File     : Code_Analysis.File_Access;
      Level    : Code_Peer.Message_Ranking_Level;
      Category : Code_Peer.Message_Category_Access) return Natural
   is
      Result : Natural := 0;

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Subprogram_Maps.Cursor) is
         Subprogram : constant Code_Analysis.Subprogram_Access :=
                        Code_Analysis.Subprogram_Maps.Element (Position);

      begin
         Result :=
           Result + Compute_Messages_Count (Subprogram, Level, Category);
      end Process;

   begin
      File.Subprograms.Iterate (Process'Access);

      return Result;
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Project  : Code_Analysis.Project_Access;
      Level    : Code_Peer.Message_Ranking_Level;
      Category : Code_Peer.Message_Category_Access) return Natural
   is
      Result : Natural := 0;

      procedure Process (Position : Code_Analysis.File_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.File_Maps.Cursor) is
         File : constant Code_Analysis.File_Access :=
                  Code_Analysis.File_Maps.Element (Position);

      begin
         Result :=
           Result + Compute_Messages_Count (File, Level, Category);
      end Process;

   begin
      Project.Files.Iterate (Process'Access);

      return Result;
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Tree     : Code_Analysis.Code_Analysis_Tree;
      Level    : Code_Peer.Message_Ranking_Level;
      Category : Code_Peer.Message_Category_Access) return Natural
   is
      Result : Natural := 0;

      procedure Process (Position : Code_Analysis.Project_Maps.Cursor);

      -------------
      -- Process --
      -------------

      procedure Process (Position : Code_Analysis.Project_Maps.Cursor) is
         Project : constant Code_Analysis.Project_Access :=
                     Code_Analysis.Project_Maps.Element (Position);

      begin
         Result :=
           Result + Compute_Messages_Count (Project, Level, Category);
      end Process;

   begin
      Tree.Iterate (Process'Access);

      return Result;
   end Compute_Messages_Count;

end Code_Peer.Utilities;
