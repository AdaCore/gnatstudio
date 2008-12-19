-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
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

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Level      : Code_Peer.Message_Probability_Level) return Natural
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
         if Message.Probability = Level then
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
     (File  : Code_Analysis.File_Access;
      Level : Code_Peer.Message_Probability_Level) return Natural
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
         Result := Result + Compute_Messages_Count (Subprogram, Level);
      end Process;

   begin
      File.Subprograms.Iterate (Process'Access);

      return Result;
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Project : Code_Analysis.Project_Access;
      Level   : Code_Peer.Message_Probability_Level) return Natural
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
         Result := Result + Compute_Messages_Count (File, Level);
      end Process;

   begin
      Project.Files.Iterate (Process'Access);

      return Result;
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Tree  : Code_Analysis.Code_Analysis_Tree;
      Level : Code_Peer.Message_Probability_Level) return Natural
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
         Result := Result + Compute_Messages_Count (Project, Level);
      end Process;

   begin
      Tree.Iterate (Process'Access);

      return Result;
   end Compute_Messages_Count;

   ----------------------------
   -- Compute_Messages_Count --
   ----------------------------

   function Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Level      : Code_Peer.Message_Probability_Level;
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
         if Message.Probability = Level
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
      Level    : Code_Peer.Message_Probability_Level;
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
      Level    : Code_Peer.Message_Probability_Level;
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
      Level    : Code_Peer.Message_Probability_Level;
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
