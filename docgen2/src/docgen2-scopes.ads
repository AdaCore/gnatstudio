------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

--  Supplementary package used to perform the analysis of constructs.

with GNAT.Strings;      use GNAT.Strings;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Language;          use Language;
with Language.Tree;     use Language.Tree;
with Language_Handlers; use Language_Handlers;

with Docgen2.Entities;  use Docgen2.Entities;
with Docgen2.Comments.List;   use Docgen2.Comments.List;

private package Docgen2.Scopes is

   type Context_Element is record
      Parent_Entity : Entity_Info;
      Pkg_Entity    : Entity_Info;
      Parent_Iter   : Construct_Tree_Iterator;
   end record;

   type Analysis_Context is private;

   function New_Context
     (File_EInfo  : Entity_Info;
      Tree        : Construct_Tree;
      File_Buffer : GNAT.Strings.String_Access;
      Language    : Language_Handler;
      Comments    : Comments_List) return Analysis_Context;
   --  Constructor of Analysis_Context. Initializes the implicit cursor to
   --  start traversing the Tree of constructs.

   procedure Free (Context : in out Analysis_Context);

   procedure Enter_Scope
     (Context : in out Analysis_Context;
      Parent  : Entity_Info;
      Entity  : Entity_Info);
   --  Enter in the scope of Entity and displace the implicit cursor to start
   --  processing all the entities defined in the scope of Entity.

   procedure Exit_Scope
     (Context : in out Analysis_Context);
   --  Exit the current scope and move the implicit cursor to the next
   --  construct of the enclosing scope

   function Current_Scope
     (Context : Analysis_Context)
         return Context_Element;
   --  Return the current scope referenced by the implicit cursor.

   function Get_New_Pkg_Id
     (Context : in out Analysis_Context) return Natural;
   --  Generate and returns a new package number identifier

   --  Implicit cursor subprograms **************************************
   function Get_Construct
     (Context : Analysis_Context)
         return access Simple_Construct_Information;
   --  Returns the current construct referenced by the implicit cursor
   function Has_Next_Construct
     (Context : Analysis_Context) return Boolean;
   --  Return True if the current scope has more constructs to traverse
   procedure Next_Construct
     (Context : in out Analysis_Context);
   --  Move the implicit cursor to the next construct of the current scope

   --  Context getters/setters *****************************************
   function Get_Comments
     (Context : Analysis_Context) return Comments_List;
   function Get_File
     (Context : Analysis_Context) return Virtual_File;
   function Get_File_Buffer
     (Context : Analysis_Context) return GNAT.Strings.String_Access;
   function Get_Language
     (Context : Analysis_Context) return Language_Handler;
   function Get_Parent_Scope_Iterator
     (Context : Analysis_Context) return Construct_Tree_Iterator;
   function In_Body
     (Context : Analysis_Context) return Boolean;

private
   package Context_Stack is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Context_Element);

   type Analysis_Context is record
      Stack       : Context_Stack.Vector;
      Iter        : Construct_Tree_Iterator;
      Tree        : Construct_Tree;
      File_Buffer : GNAT.Strings.String_Access;
      File        : Virtual_File;
      Language    : Language_Handler;
      Pkg_Nb      : Natural;
      Comments    : Comments_List;
      In_Body     : Boolean;
   end record;
end Docgen2.Scopes;
