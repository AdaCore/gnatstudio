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

--  Provides a completer working on language constructs for Ada

with Language.Tree; use Language.Tree;

package Completion.Constructs_Extractor is

   type Construct_Completion_Resolver is new Completion_Resolver with private;
   --  This resolver is based on the constructs found in a given file

   function New_Construct_Completion_Resolver
     (Tree : Construct_Tree_Access; Current_File : Virtual_File)
      return Construct_Completion_Resolver;
   --  Create a new resolver, based on a construct tree, and the current
   --  analyzed file.

   procedure Get_Possibilities
     (Resolver   : access Construct_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List);
   --  See inherited documentation

   procedure Free (This : in out Construct_Completion_Resolver);
   --  Free the data associated to a construct completion resolver

private

   type Construct_Completion_Resolver is new Completion_Resolver with record
      Tree         : Construct_Tree_Access;
      Current_File : Virtual_File;
   end record;

   type Construct_Completion_Proposal is new Completion_Proposal with record
      Tree_Node            : Construct_Tree_Iterator;
      Is_All               : Boolean := False;
      Params_In_Expression : Integer := 0;
   end record;

   function Get_Completion
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category;
   --  See inherited documentation

   function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String;
   --  See inherited documentation

   function Get_Location
     (Proposal : Construct_Completion_Proposal) return File_Location;
   --  See inherited documentation

   procedure Get_Composition
     (Proposal   : Construct_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List);
   --  See inherited documentation

   function Get_Number_Of_Parameters
     (Proposal : Construct_Completion_Proposal) return Natural;
   --  See inherited documentation

   procedure Append_Expression
     (Proposal             : in out Construct_Completion_Proposal;
      Number_Of_Parameters : Natural);
   --  See inherited documentation

   procedure Free (Proposal : in out Construct_Completion_Proposal);
   --  See inherited documentation

end Completion.Constructs_Extractor;
