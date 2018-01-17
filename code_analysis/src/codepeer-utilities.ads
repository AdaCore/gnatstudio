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

package CodePeer.Utilities is

   type Counts is record
      Current : Natural := 0;
   end record;

   type Messages_Counts is array (CodePeer.Message_Ranking_Level) of Counts;

   function "+"
     (Left : Messages_Counts; Right : Messages_Counts) return Messages_Counts;

   procedure Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Categories : CodePeer.Message_Category_Sets.Set;
      CWEs       : CodePeer.CWE_Category_Sets.Set;
      Lifeages   : CodePeer.Lifeage_Kinds_Flags;
      Statuses   : CodePeer.Review_Status_Kinds_Flags;
      Counts     : out Messages_Counts;
      Checks     : out Natural);
   --  Computes number of the subprogram's messages with specified categories

   procedure Compute_Messages_Count
     (File       : Code_Analysis.File_Access;
      Categories : CodePeer.Message_Category_Sets.Set;
      CWEs       : CodePeer.CWE_Category_Sets.Set;
      Lifeages   : CodePeer.Lifeage_Kinds_Flags;
      Statuses   : CodePeer.Review_Status_Kinds_Flags;
      Counts     : out Messages_Counts;
      Checks     : out Natural);
   --  Computes number of the file's messages with specified categories

   procedure Compute_Messages_Count
     (Project      : Code_Analysis.Project_Access;
      Categories   : CodePeer.Message_Category_Sets.Set;
      CWEs         : CodePeer.CWE_Category_Sets.Set;
      Lifeages     : CodePeer.Lifeage_Kinds_Flags;
      Statuses     : CodePeer.Review_Status_Kinds_Flags;
      Counts       : out Messages_Counts;
      Checks       : out Natural;
      Total_Checks : out Natural);
   --  Computes number of the project's messages with specified categories

   procedure Compute_Messages_Count
     (Tree         : Code_Analysis.Code_Analysis_Tree;
      Categories   : CodePeer.Message_Category_Sets.Set;
      CWEs         : CodePeer.CWE_Category_Sets.Set;
      Lifeages     : CodePeer.Lifeage_Kinds_Flags;
      Statuses     : CodePeer.Review_Status_Kinds_Flags;
      Counts       : out Messages_Counts;
      Checks       : out Natural;
      Total_Checks : out Natural);
   --  Computes number of the whole tree messages with specified categories

end CodePeer.Utilities;
