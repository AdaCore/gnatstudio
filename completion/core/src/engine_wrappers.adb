-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2009-2010, AdaCore             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada_Semantic_Tree.Parts; use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Lang;  use Ada_Semantic_Tree.Lang;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;

package body Engine_Wrappers is

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label (Proposal : Comp_Proposal) return String is
   begin
      return Get_Label (Proposal.P.all);
   end Get_Label;

   --------------------
   -- Get_Completion --
   --------------------

   overriding function Get_Completion
     (Proposal : Comp_Proposal)
      return String is
   begin
      return Get_Completion (Proposal.P.all);
   end Get_Completion;

   --------------------
   -- Get_Visibility --
   --------------------

   overriding function Get_Visibility
     (Proposal : Comp_Proposal)
      return Construct_Visibility is
   begin
      return Get_Visibility (Proposal.P.all);
   end Get_Visibility;

   ------------------
   -- Get_Category --
   ------------------

   overriding function Get_Category
     (Proposal : Comp_Proposal)
      return Language_Category is
   begin
      return Get_Category (Proposal.P.all);
   end Get_Category;

   ----------------------
   -- Get_Caret_Offset --
   ----------------------

   overriding function Get_Caret_Offset
     (Proposal : Comp_Proposal)
      return Character_Offset_Type is
   begin
      return Get_Caret_Offset (Proposal.P.all);
   end Get_Caret_Offset;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : Comp_Proposal)
      return String is
   begin
      return Get_Documentation (Proposal.P.all);
   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Proposal : Comp_Proposal)
      return File_Location is
   begin
      return Get_Location (Proposal.P.all);
   end Get_Location;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Comp_Proposal) is
   begin
      Free (X.P);
   end Free;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Comp_Iterator) return Boolean is
   begin
      return At_End (Iter.I);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Iter : Comp_Iterator) return Boolean is
   begin
      return Is_Valid (Iter.I);
   end Is_Valid;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Comp_Iterator) is
   begin
      Next (Iter.I);
   end Next;

   ------------------
   -- Get_Proposal --
   ------------------

   overriding function Get_Proposal
     (Iter    : Comp_Iterator) return Root_Proposal'Class is
   begin
      return Comp_Proposal'
        (P => new Completion_Proposal'Class'(Get_Proposal (Iter.I)));
   end Get_Proposal;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label (Proposal : Entity_Proposal) return String is
   begin
      if Proposal.Construct.Name = No_Symbol then
         return "<no name>";
      else
         return Get (Proposal.Construct.Name).all;
      end if;
   end Get_Label;

   --------------------
   -- Get_Completion --
   --------------------

   overriding function Get_Completion
     (Proposal : Entity_Proposal)
      return String is
   begin
      return Get_Label (Proposal);
   end Get_Completion;

   --------------------
   -- Get_Visibility --
   --------------------

   overriding function Get_Visibility
     (Proposal : Entity_Proposal)
      return Construct_Visibility is
   begin
      return Proposal.Construct.Visibility;
   end Get_Visibility;

   ------------------
   -- Get_Category --
   ------------------

   overriding function Get_Category
     (Proposal : Entity_Proposal)
      return Language_Category is
   begin
      return Proposal.Construct.Category;
   end Get_Category;

   ----------------------
   -- Get_Caret_Offset --
   ----------------------

   overriding function Get_Caret_Offset
     (Proposal : Entity_Proposal)
      return Character_Offset_Type
   is
      pragma Unreferenced (Proposal);
   begin
      return 0;
   end Get_Caret_Offset;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : Entity_Proposal) return String is
   begin
      return Proposal.Documentation.all;
   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Proposal : Entity_Proposal)
      return File_Location
   is
   begin
      return
        (File_Path => Proposal.File,
         Line      => Proposal.Construct.Sloc_Start.Line,
         Column    => Visible_Column_Type
           (Proposal.Construct.Sloc_Start.Column));
   end Get_Location;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Entity_Proposal) is
   begin
      Free (X.Documentation);
   end Free;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Entity_Iterator) return Boolean is
   begin
      return At_End (Iter.I);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Iter : Entity_Iterator) return Boolean is
   begin
      return Is_Valid (Iter.I);
   end Is_Valid;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Entity_Iterator) is
   begin
      Next (Iter.I);

      --  We do not want to list declaration in package specs
      while (not At_End (Iter.I))
        and then Get_First_Occurence
          (Get_Entity (Iter.I)) /= Get_Entity (Iter.I)
      loop
         Next (Iter.I);
      end loop;
   end Next;

   ------------------
   -- Get_Proposal --
   ------------------

   overriding function Get_Proposal
     (Iter    : Entity_Iterator)
      return Root_Proposal'Class
   is
      File : Virtual_File;
      Decl : Entity_View;
      Construct : Simple_Construct_Information;
   begin
      Decl := Get_View (Iter.I);

      declare
         Construct_Access : access Simple_Construct_Information;
      begin
         Construct_Access := Get_Construct (Decl);
         if Construct_Access = null then
            Construct := Null_Simple_Construct_Info;
         else
            Construct := Construct_Access.all;
         end if;
      end;

      File := Get_File_Path (Get_File (Decl));

      Free (Decl);

      return Entity_Proposal'
        (File      => File,
         Construct => Construct,
         Documentation => new String'
           (Get_Documentation (Ada_Tree_Lang, Get_Entity (Iter.I))));
   end Get_Proposal;

   -----------------------------
   -- Get_Underlying_Proposal --
   -----------------------------

   function Get_Underlying_Proposal
     (C : Comp_Proposal) return Completion_Proposal_Access is
   begin
      return C.P;
   end Get_Underlying_Proposal;

end Engine_Wrappers;
