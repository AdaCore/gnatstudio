------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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
      Kernel      : Kernel_Handle;
      GLI_Handler : LI_Handler;
   end record;

   type C_Completion_Proposal is new Simple_Completion_Proposal with record
      Entity_Info : Entity_Information;

      With_Params : Boolean := False;
      --  Set to true if Entity_Info is a subprogram and we need to provide
      --  completions with all the parameters needed to call it

      Is_Param    : Boolean := False;
      --  Set to true if Entity_Info is a formal parameter of a subprogram call
      --  and we need to provide completion for this single parameter
   end record;

   overriding function Get_Category
      (Proposal : C_Completion_Proposal) return Language_Category;

   overriding function Get_Completion
     (Proposal : C_Completion_Proposal) return UTF8_String;
   --  Handle the completion of a single parameter of a subprogram call, the
   --  completion of all the parameters of a subprogram call, and also the
   --  completion of a single entity name.

   overriding function Get_Label
     (Proposal : C_Completion_Proposal) return UTF8_String;
   --  Generate the label "<entity> without params" when the proposal requests
   --  the completion of the parameters of a subprogram call and the entity of
   --  the proposal has no parameters; generate the label "params of <entity>"
   --  when the proposal requests the completion with parameters and the entity
   --  of the proposal has parameters; otherwise generate the label "<entity>".

   overriding function Get_Location
     (Proposal : C_Completion_Proposal) return File_Location;

   overriding function Get_Visibility
     (Proposal : C_Completion_Proposal) return Construct_Visibility;

   overriding function To_Completion_Id
     (Proposal : C_Completion_Proposal) return Completion_Id;

end Completion.C.Constructs_Extractor;
