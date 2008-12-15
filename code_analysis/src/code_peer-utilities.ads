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

package Code_Peer.Utilities is

   function Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Level      : Code_Peer.Message_Probability_Level) return Natural;
   --  Computes number of the subprogram's messages with specified
   --  probability level.

   function Compute_Messages_Count
     (File  : Code_Analysis.File_Access;
      Level : Code_Peer.Message_Probability_Level) return Natural;
   --  Computes number of the file's messages with specified probability
   --  level.

   function Compute_Messages_Count
     (Project : Code_Analysis.Project_Access;
      Level   : Code_Peer.Message_Probability_Level) return Natural;
   --  Computes number of the project's messages with specified probability
   --  level.

   function Compute_Messages_Count
     (Tree  : Code_Analysis.Code_Analysis_Tree;
      Level : Code_Peer.Message_Probability_Level) return Natural;
   --  Computes number of the whole tree messages with specified probability
   --  level.

   function Compute_Messages_Count
     (Subprogram : Code_Analysis.Subprogram_Access;
      Level      : Code_Peer.Message_Probability_Level;
      Category   : Code_Peer.Message_Category_Access) return Natural;
   --  Computes number of the subprogram's messages with specified
   --  probability level and category.

   function Compute_Messages_Count
     (File     : Code_Analysis.File_Access;
      Level    : Code_Peer.Message_Probability_Level;
      Category : Code_Peer.Message_Category_Access) return Natural;
   --  Computes number of the file's messages with specified probability
   --  level and category.

   function Compute_Messages_Count
     (Project  : Code_Analysis.Project_Access;
      Level    : Code_Peer.Message_Probability_Level;
      Category : Code_Peer.Message_Category_Access) return Natural;
   --  Computes number of the project's messages with specified probability
   --  level and category.

   function Compute_Messages_Count
     (Tree     : Code_Analysis.Code_Analysis_Tree;
      Level    : Code_Peer.Message_Probability_Level;
      Category : Code_Peer.Message_Category_Access) return Natural;
   --  Computes number of the whole tree messages with specified probability
   --  level and category.

end Code_Peer.Utilities;
