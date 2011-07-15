-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2011, AdaCore                   --
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

--  Simple completer for C/C++. Its list of proposals is composed of all the
--  C/C++ entities whose prefix matches the completion (that is, it includes
--  entities that are not appropriate according to the C/C++ context).

with GPS.Kernel;                 use GPS.Kernel;
with Language.Tree;              use Language.Tree;
with Language.Tree.Database;     use Language.Tree.Database;

private with Entities;

package Completion.C.Constructs_Extractor is

   type Construct_Completion_Resolver is new Completion_Resolver with private;

   function New_C_Construct_Completion_Resolver
     (Kernel       : Kernel_Handle;
      Current_File : Virtual_File) return Completion_Resolver_Access;
   --  Create a new resolver based on the analyzed file

   overriding
   procedure Get_Completion_Root
     (Resolver : access Construct_Completion_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Construct_Completion_Resolver) return String;
   --  See inherited documentation

   overriding procedure Free (This : in out Construct_Completion_Resolver);
   --  Free the data associated to a construct completion resolver

private
   use Entities;

   type Construct_Completion_Resolver is new Completion_Resolver with record
      GLI_Handler : LI_Handler;
   end record;

   type C_Completion_Proposal is new Simple_Completion_Proposal with record
      Entity_Info : Entity_Information;
   end record;

   overriding function Get_Category
      (Proposal : C_Completion_Proposal) return Language_Category;

   overriding function Get_Location
     (Proposal : C_Completion_Proposal) return File_Location;

   overriding function Get_Visibility
     (Proposal : C_Completion_Proposal) return Construct_Visibility;

end Completion.C.Constructs_Extractor;
