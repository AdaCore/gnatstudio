-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

with Language.Tree; use Language.Tree;

package Completion.Constructs_Extractor is

   type Construct_Completion_Resolver is new Completion_Resolver with private;
   --  This resolver is based on the constructs found in a given file

   function New_Construct_Completion_Resolver (Tree : Construct_Tree_Access)
      return Construct_Completion_Resolver;
   --  Create a new resolver, based on a construct tree

   function Get_Possibilities
     (Resolver   : access Construct_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Natural;
      Filter     : Possibilities_Filter) return Completion_List;
   --  See inherited documentation

   procedure Free (This : in out Construct_Completion_Resolver);
   --  Free the data associated to a construct completion resolver

private

   type Construct_Completion_Resolver is new Completion_Resolver with record
      Tree : Construct_Tree_Access;
   end record;

   type Construct_Completion_Proposal is new Completion_Proposal with record
      Tree_Node : Construct_Tree_Iterator;
      Is_All    : Boolean := False;
   end record;

   function Get_Name (Proposal : Construct_Completion_Proposal) return String;
   --  See inherited documentation

   function Get_Category (Proposal : Construct_Completion_Proposal)
     return Language_Category;
   --  See inherited documentation

   function Get_Composition
     (Proposal : Construct_Completion_Proposal; Offset : Positive)
      return Completion_List;
   --  See inherited documentation

   function Get_Number_Of_Parameters (Proposal : Construct_Completion_Proposal)
     return Natural;
   --  See inherited documentation

end Completion.Constructs_Extractor;
