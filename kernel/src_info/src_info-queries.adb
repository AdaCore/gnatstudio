-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Unchecked_Deallocation;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with System.Assertions;       use System.Assertions;
with Traces;                  use Traces;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Prj;                     use Prj;
with Prj.Tree;                use Prj.Tree;

with Prj_API;                 use Prj_API;
with Basic_Types;             use Basic_Types;

package body Src_Info.Queries is

   Me : Debug_Handle := Create ("SRC_INFO");

   use Name_Htable;

   procedure Free is new
     Unchecked_Deallocation (Dependency_Node, Dependency_List);

   function Search_Is_Completed
     (Status : Find_Decl_Or_Body_Query_Status)
      return Boolean;
   --  Return False unless Status is equal to Entity_Not_Found. The idea
   --  implemented behind this function is to have a single function to decide,
   --  given an xref query, whether the results from a sub-query should be
   --  presented to the end-user, or if some more search, when possible, should
   --  be performed.

   function Location_Matches
     (Location  : File_Location;
      File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Boolean;
   --  Return True if the given File_Location is pointing to the same
   --  Line, Column, and Filename. The filename comparison is done after
   --  comparing the position for better performance.
   --  ??? This could be adapted to do approximate matches, for when the LI
   --  ??? file wasn't up-to-date.

   function Find_Next_Body_Ref
     (Decl : E_Declaration_Info;
      Ref  : E_Reference_List := null) return E_Reference_List;
   --  Search for the body reference to the given declaration immediately
   --  following the given reference. If there is none then return the
   --  location of the declaration.
   --
   --  As a special case, if Ref is null, we search for the first body
   --  reference. If none, then return an xref failure (null File_Name_Found,
   --  etc).

   procedure Find_Spec_Or_Body
     (Decl            : E_Declaration_Info_List;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Entity_Decl     : out E_Declaration_Info;
      Ref             : out E_Reference_List;
      Status          : out Find_Decl_Or_Body_Query_Status);
   --  Same as Find_Declaration_Or_Body, but for a specific declaration list.
   --  Entity_Name must be all lower-cases if the language is case insensitive

   procedure Destroy (Dep : in out Dependency);
   --  Deallocates the memory associated with the given Dependency record.

   procedure Trace_Dump
     (Handler : Debug_Handle;
      Scope : Scope_List;
      Prefix : String;
      Subprograms_Pkg_Only : Boolean);
   --  Dump Scope to Handler, printing Prefix at the beginning of each line

   function Dump (L : Scope_List) return String;
   --  Return a string representation of L

   procedure Free (Scope : in out Scope_List);
   --  Free the memory occupied by Scope.

   function Find_Entity_Scope
     (Scope : Scope_List; Entity : Entity_Information) return Scope_Tree_Node;
   --  Find the declaration for Name in Scope.
   --  Name must be all lower-case if the language is case-insensitive

   procedure Find_Entity_References
     (Node     : Scope_List;
      Decl     : E_Declaration_Info;
      Callback : Node_Callback);
   --  Find the references to Decl in Node

   function Is_Same_Entity
     (Decl : E_Declaration; Entity : Entity_Information) return Boolean;
   function Is_Same_Entity (Decl1, Decl2 : E_Declaration)
      return Boolean;
   --  True if Entity1 and Entity2 represent the same entity. You can not use a
   --  direct equality test

   -------------------------
   -- Search_Is_Completed --
   -------------------------

   function Search_Is_Completed
     (Status : Find_Decl_Or_Body_Query_Status)
      return Boolean is
   begin
      case Status is
         when Entity_Not_Found =>
            return False;
            --  We should continue the search if we can.
         when Internal_Error =>
            return True;
            --  ??? We don't want to ignore internal errors at the moment,
            --  ??? so we stop the query, and report and error to the end-user.
            --  ??? we may want to change this at release time if we want
            --  ??? to provide a better fault tolerant product (by changing
            --  ??? the value returned to False, the net effect is to ignore
            --  ??? the internal error while taking our chance by continuing
            --  ??? the search).
         when No_Body_Entity_Found |
              Success =>
            return True;
            --  Obviously, we have completed our query.
      end case;
   end Search_Is_Completed;

   ----------------------
   -- Location_Matches --
   ----------------------

   function Location_Matches
     (Location  : File_Location;
      File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Boolean is
   begin
      return Location.Line = Line
        and then Location.Column = Column
        and then Get_Source_Filename (Location.File) = File_Name;
   end Location_Matches;

   ------------------------
   -- Find_Next_Body_Ref --
   ------------------------

   function Find_Next_Body_Ref
     (Decl : E_Declaration_Info;
      Ref  : E_Reference_List := null) return E_Reference_List
   is
      Current_Ref   : E_Reference_List;
   begin
      --  Search the body reference immediately placed after the given
      --  Ref. Note that the references are stored in _reverse_ order...
      Current_Ref := Decl.References;
      while Current_Ref /= Ref and then Current_Ref /= null loop
         --  The test against null is just a guard against programing errors,
         --  just in case we are given a ref which is not part of the reference
         --  list of Decl...
         if Current_Ref.Value.Kind = Body_Entity then
            return Current_Ref;
         end if;
         Current_Ref := Current_Ref.Next;
      end loop;

      return null;
   end Find_Next_Body_Ref;

   -----------------------
   -- Find_Spec_Or_Body --
   -----------------------

   procedure Find_Spec_Or_Body
     (Decl            : E_Declaration_Info_List;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Entity_Decl     : out E_Declaration_Info;
      Ref             : out E_Reference_List;
      Status          : out Find_Decl_Or_Body_Query_Status)
   is
      Current_Decl : E_Declaration_Info_List := Decl;
      Current_Ref  : E_Reference_List;
   begin
      --  Initialize the value of the returned parameters
      Entity_Decl := No_Declaration_Info;
      Ref         := null;
      Status      := Entity_Not_Found;

      --  Search the entity in the list of declarations
      Decl_Loop :
      while Current_Decl /= null loop

         --  Check the entity name to limit a bit the search in the
         --  Xref lists
         if Current_Decl.Value.Declaration.Name.all = Entity_Name then

            --  Check if the location corresponds to the declaration,
            --  in which case we need to jump to the first body.
            if Location_Matches
              (Current_Decl.Value.Declaration.Location,
               File_Name, Line, Column)
            then
               Entity_Decl := Current_Decl.Value;
               Ref         := null;
               Status      := Success;
               return;
            end if;

            --  Search in the list of references.
            Current_Ref := Current_Decl.Value.References;
            Ref_Loop :
            while Current_Ref /= null loop

               if Location_Matches
                 (Current_Ref.Value.Location, File_Name, Line, Column)
               then
                  Entity_Decl := Current_Decl.Value;
                  Ref         := Current_Ref;
                  Status      := Success;
                  return;
               end if;

               Current_Ref := Current_Ref.Next;
            end loop Ref_Loop;
         end if;
         Current_Decl := Current_Decl.Next;
      end loop Decl_Loop;
   end Find_Spec_Or_Body;

   ------------------------------
   -- Find_Declaration_Or_Body --
   ------------------------------

   procedure Find_Declaration_Or_Body
     (Lib_Info           : LI_File_Ptr;
      File_Name          : String;
      Entity_Name        : String;
      Line               : Positive;
      Column             : Positive;
      Entity_Declaration : out E_Declaration_Info;
      Location           : out File_Location;
      Status             : out Find_Decl_Or_Body_Query_Status)
   is
      Current_Sep : File_Info_Ptr_List;
      Current_Dep : Dependency_File_Info_List;
      Ref         : E_Reference_List;
      E_Name      : String := Entity_Name;

   begin
      if Case_Insensitive_Identifiers (Lib_Info.LI.Handler) then
         E_Name := To_Lower (E_Name);
      end if;

      Entity_Declaration := No_Declaration_Info;
      Ref                := null;
      Status             := Entity_Not_Found;

      --  Search a matching entity declaration in the Spec
      if Lib_Info.LI.Spec_Info /= null
        and then Lib_Info.LI.Spec_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.LI.Spec_Info.Declarations,
            File_Name, E_Name, Line, Column,
            Entity_Declaration, Ref, Status);
      end if;

      --  Search in the Body
      if not Search_Is_Completed (Status)
        and then Lib_Info.LI.Body_Info /= null
        and then Lib_Info.LI.Body_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.LI.Body_Info.Declarations,
            File_Name, E_Name, Line, Column,
            Entity_Declaration, Ref, Status);
      end if;

      --  Search in the separates
      if not Search_Is_Completed (Status) then
         Current_Sep := Lib_Info.LI.Separate_Info;
         while Current_Sep /= null loop
            if Current_Sep.Value.Declarations /= null then
               Find_Spec_Or_Body
                 (Current_Sep.Value.Declarations,
                  File_Name, E_Name, Line, Column,
                  Entity_Declaration, Ref, Status);

               if Status = Success then
                  exit;
               end if;
            end if;

            Current_Sep := Current_Sep.Next;
         end loop;
      end if;

      --  Search in the list of dependencies, if any
      if not Search_Is_Completed (Status) then
         Current_Dep := Lib_Info.LI.Dependencies_Info;
         while Current_Dep /= null loop
            if Current_Dep.Value.Declarations /= null then
               Find_Spec_Or_Body
                 (Current_Dep.Value.Declarations,
                  File_Name, E_Name, Line, Column,
                  Entity_Declaration, Ref, Status);
               if Status = Success then
                  exit;
               end if;
            end if;

            Current_Dep := Current_Dep.Next;
         end loop;
      end if;

      --  We have now found a reference. Now we must decide whether we want to
      --  get the reference to the declaration, one of the bodies,...
      --  Check if the location corresponds to the declaration,
      --  in which case we need to jump to the first body.
      --  Otherwise, if this is a body reference, then we try to navigate
      --  to the next body reference.

      if Status = Success then
         if Ref = null or else Ref.Value.Kind = Body_Entity then
            Ref := Find_Next_Body_Ref (Entity_Declaration, Ref);
         else
            Ref := null;
         end if;

         if Ref /= null then
            Location := Ref.Value.Location;
         else
            Location := Entity_Declaration.Declaration.Location;
         end if;
      else
         Trace (Me, "Couldn't find a valid xref for " & E_Name
                & " line=" & Line'Img & " Column=" & Column'Img
                & " file=" & File_Name);
      end if;

   exception
      when others =>
         --  Trap all exceptions for better robustness, and report an
         --  internal error
         Entity_Declaration := No_Declaration_Info;
         Ref                := null;
         Status             := Internal_Error;
   end Find_Declaration_Or_Body;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Dep : in out Dependency) is
   begin
      Destroy (Dep.File);
   end Destroy;

   procedure Destroy (List : in out Dependency_List) is
      Current_Dep : Dependency_List renames List;
      Next_Dep    : Dependency_List;
   begin
      while Current_Dep /= null loop
         Next_Dep := Current_Dep.Next;
         Destroy (Current_Dep.Value);
         Free (Current_Dep);
         Current_Dep := Next_Dep;
      end loop;
   end Destroy;

   -----------------------
   -- Find_Dependencies --
   -----------------------

   procedure Find_Dependencies
     (Lib_Info     : LI_File_Ptr;
      Dependencies : out Dependency_List;
      Status       : out Dependencies_Query_Status)
   is
      Current_Dep : Dependency_File_Info_List;
   begin
      if Lib_Info = null then
         Dependencies := null;
         Status := Internal_Error;
         Trace (Me, "No Lib_Info specified for Find_Dependencies");
         return;
      end if;

      Trace (Me, "Getting dependencies for "
             & Get_LI_Filename (Lib_Info));
      Current_Dep  := Lib_Info.LI.Dependencies_Info;
      Dependencies := null;

      while Current_Dep /= null loop
         declare
            FI : constant File_Info_Ptr :=
              Get_File_Info (Current_Dep.Value.File);
         begin
            if FI = null or else FI.Source_Filename = null then
               Destroy (Dependencies);
               Dependencies := null;
               Status := Internal_Error;
               Trace (Me, "Couldn't find the File_Info_Ptr for "
                      & Get_LI_Filename (Lib_Info));
               return;
            end if;

            Dependencies := new Dependency_Node'
              (Value =>
                 (File =>
                    (File_Name => new String' (FI.Source_Filename.all),
                     LI_Name   => new String'
                       (Current_Dep.Value.File.LI.LI.LI_Filename.all)),
                  Dep  => Current_Dep.Value.Dep_Info),
               Next  => Dependencies);

            Current_Dep := Current_Dep.Next;
         end;
      end loop;

      Status := Success;
   end Find_Dependencies;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information (Dep : Dependency) return Internal_File is
   begin
      return Dep.File;
   end File_Information;

   ----------------------------
   -- Dependency_Information --
   ----------------------------

   function Dependency_Information (Dep : Dependency) return Dependency_Info is
   begin
      return Dep.Dep;
   end Dependency_Information;

   ----------
   -- Dump --
   ----------

   function Dump (L : Scope_List) return String is
   begin
      case L.Typ is
         when Declaration =>
            if L.Decl.End_Of_Scope /= No_Reference then
               return "Decl:""" & L.Decl.Name.all
                 & L.Start_Of_Scope.Line'Img
                 & L.Start_Of_Scope.Column'Img
                 & " ->"
                 & L.Decl.End_Of_Scope.Location.Line'Img
                 & L.Decl.End_Of_Scope.Location.Column'Img
                 & ", decl at"
                 & L.Decl.Location.Line'Img
                 & L.Decl.Location.Column'Img
                 & """";
            else
               return "Decl:""" & L.Decl.Name.all
                 & L.Start_Of_Scope.Line'Img
                 & L.Start_Of_Scope.Column'Img
                 & ", decl at"
                 & L.Decl.Location.Line'Img
                 & L.Decl.Location.Column'Img
                 & """";
            end if;
         when Reference =>
            return "Ref:""" & L.Decl.Name.all
              & L.Ref.Location.Line'Img
              & L.Ref.Location.Column'Img & """";
      end case;
   end Dump;

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump
     (Handler : Debug_Handle;
      Scope : Scope_List;
      Prefix : String;
      Subprograms_Pkg_Only : Boolean)
   is
      L : Scope_List := Scope;
   begin
      while L /= null loop
         if (not Subprograms_Pkg_Only
             or else Is_Subprogram (Scope_Tree_Node (L)))
           or else L.Decl.Kind = Generic_Package
           or else L.Decl.Kind = Non_Generic_Package
         then
            Trace (Handler, Prefix & Dump (L));
            if L.Typ = Declaration then
               Trace_Dump
                 (Handler, L.Contents, Prefix & "  ", Subprograms_Pkg_Only);
            end if;
         end if;
         L := L.Sibling;
      end loop;
   end Trace_Dump;

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump
     (Handler              : Traces.Debug_Handle;
      Tree                 : Scope_Tree;
      Node                 : Scope_Tree_Node := Null_Scope_Tree_Node;
      Subprograms_Pkg_Only : Boolean := True) is
   begin
      if Tree /= Null_Scope_Tree then
         Trace (Handler, "Scope tree for " & Tree.LI_Filename.all);
         if Node = Null_Scope_Tree_Node then
            Trace (Handler, "Part= BODY");
            Trace_Dump (Handler, Tree.Body_Tree, "",
                        Subprograms_Pkg_Only);
            Trace (Handler, "Part= SPEC");
            Trace_Dump (Handler, Tree.Spec_Tree, "",
                        Subprograms_Pkg_Only);
            for P in Tree.Separate_Trees'Range loop
               Trace (Handler, "Part=" & P'Img);
               Trace_Dump (Handler, Tree.Separate_Trees (P), "",
                           Subprograms_Pkg_Only);
            end loop;
         else
            Trace_Dump (Handler, Scope_List (Node), "", Subprograms_Pkg_Only);
         end if;
      else
         Trace (Handler, "Null scope tree");
      end if;
   end Trace_Dump;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Node : Scope_Tree_Node) return Boolean is
      K : E_Kind := Node.Decl.Kind;
   begin
      return K = Generic_Function_Or_Operator
        or else K = Generic_Procedure
        or else K = Non_Generic_Function_Or_Operator
        or else K = Non_Generic_Procedure;
   end Is_Subprogram;

   ----------
   -- Free --
   ----------

   procedure Free (Scope : in out Scope_List) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Scope_Node, Scope_List);
      L : Scope_List;
   begin
      while Scope /= null loop
         L := Scope;
         Scope := Scope.Sibling;
         if L.Typ = Declaration then
            Free (L.Contents);
         end if;
         Unchecked_Free (L);
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Scope_Tree) is
   begin
      Free (Tree.LI_Filename);
      Free (Tree.Body_Tree);
      Free (Tree.Spec_Tree);
      for P in Tree.Separate_Trees'Range loop
         Free (Tree.Separate_Trees (P));
      end loop;
      Free (Tree.Separate_Trees);
   end Free;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree (Lib_Info : LI_File_Ptr) return Scope_Tree is

      procedure Add_Declarations
        (Decl : E_Declaration_Info_List; T : in out Scope_Tree);
      --  Add the declarations from Decl into L.

      procedure Add_Single_Entity
        (Decl : in out Scope_List;
         Node : in out Scope_List;
         Parent : Scope_List);
      procedure Add_Single_Entity
        (Decl : in out Scope_List; T : in out Scope_Tree);
      --  Add a single reference in the tree (either starting at the top-level
      --  of the tree if Node is null, or at Node otherwise).

      procedure Add_References
        (Decl : E_Declaration_Info;
         T : in out Scope_Tree;
         Decl_Start : File_Location);
      --  Add all the references to the entity declared in Decl.
      --  Decl_Start is the starting location of the scope for the entity.

      function "<" (L1, L2 : File_Location) return Boolean;
      --  True if L1 is before L2 (line and column)

      function In_Range (Decl : Scope_List; Loc : Scope_List) return Integer;
      --  True if Loc is in the range of Decl.
      --  -1 is returned if Decl is before Loc, 0 if within, 1 if after, or
      --  2 if Loc is contained in the scope of Decl.
      --  Both Decl and Loc must reference the same source file, or no
      --  comparison can be done.

      procedure Add_In_List
        (L        : in out Scope_List;
         Previous : in out Scope_List;
         Parent   : Scope_List;
         New_Item : Scope_List);
      --  Add New_Item in the list L, after Previous (or at the beginning if
      --  Previous is null.

      procedure Compute_Scope (L : in out Scope_List; Ref : E_Reference_List);
      --  Compute the beginning and end of scope for the declaration in L.

      -------------------
      -- Compute_Scope --
      -------------------

      procedure Compute_Scope
        (L : in out Scope_List; Ref : E_Reference_List)
      is
         R : E_Reference_List := Ref;
      begin
         L.Start_Of_Scope := L.Decl.Location;

         while R /= null loop
            if Is_Start_Reference (R.Value.Kind) then
               L.Start_Of_Scope := R.Value.Location;
               return;
            end if;
            R := R.Next;
         end loop;
      end Compute_Scope;

      -------
      -- < --
      -------

      function "<" (L1, L2 : File_Location) return Boolean is
      begin
         return L1.Line < L2.Line
           or else (L1.Line = L2.Line and then L1.Column < L2.Column);
      end "<";

      --------------
      -- In_Range --
      --------------

      function In_Range
        (Decl : Scope_List; Loc : Scope_List) return Integer
      is
         L : File_Location;
      begin
         case Loc.Typ is
            when Declaration => L := Loc.Start_Of_Scope; --  Decl.Location;
            when Reference   => L := Loc.Ref.Location;
         end case;

         case Decl.Typ is
            when Declaration =>
               case Loc.Typ is
                  when Declaration =>
                     if Decl.Decl.End_Of_Scope /= No_Reference then
                        if Decl.Decl.End_Of_Scope.Location < L then
                           return -1;
                        elsif Decl.Start_Of_Scope < L then
                           --  Entities are necessarily comprised within one
                           --  another
                           return 2;
                        elsif Loc.Decl.End_Of_Scope /= No_Reference
                          and then Decl.Start_Of_Scope <
                            Loc.Decl.End_Of_Scope.Location
                        then
                           return 0;
                        else
                           return 1;
                        end if;
                     elsif Loc.Decl.End_Of_Scope /= No_Reference then
                        if Decl.Start_Of_Scope < L then
                           return -1;
                        elsif Decl.Start_Of_Scope <
                          Loc.Decl.End_Of_Scope.Location
                        then
                           return 0;
                        else
                           return 1;
                        end if;
                     elsif Decl.Start_Of_Scope < L then
                        return -1;
                     else
                        return 1;
                     end if;

                  when Reference =>
                     if Decl.Decl.End_Of_Scope /= No_Reference then
                        if Decl.Decl.End_Of_Scope.Location < L then
                           return -1;
                        elsif Decl.Start_Of_Scope < L then
                           return 2;
                        else
                           return 1;
                        end if;
                     elsif Decl.Start_Of_Scope < L then
                        return -1;
                     else
                        return 1;
                     end if;
               end case;

            when Reference =>
               case Loc.Typ is
                  when Declaration =>
                     if Decl.Ref.Location < L then
                        return -1;
                     elsif Loc.Decl.End_Of_Scope /= No_Reference
                       and then Decl.Ref.Location <
                         Loc.Decl.End_Of_Scope.Location
                     then
                        return 0;
                     else
                        return 1;
                     end if;

                  when Reference =>
                     if Decl.Ref.Location < L then
                        return -1;
                     else
                        return 1;
                     end if;
               end case;
         end case;
      end In_Range;

      -----------------
      -- Add_In_List --
      -----------------

      procedure Add_In_List
        (L        : in out Scope_List;
         Previous : in out Scope_List;
         Parent   : Scope_List;
         New_Item : Scope_List) is
      begin
         Assert (Me, New_Item.Sibling = null,
                 "Inserting item with existing sibling");
         New_Item.Parent := Parent;
         if Previous = null then
            New_Item.Sibling := L;
            L := New_Item;
         else
            New_Item.Sibling := Previous.Sibling;
            Previous.Sibling := New_Item;
         end if;
      end Add_In_List;

      -----------------------
      -- Add_Single_Entity --
      -----------------------

      procedure Add_Single_Entity
        (Decl : in out Scope_List;
         Node : in out Scope_List;
         Parent : Scope_List)
      is
         Pos      : Integer;
         List     : Scope_List := Node;
         Previous : Scope_List := null;
         Save     : Scope_List;
      begin
         while List /= null loop
            Pos := In_Range (Decl, List);
            case In_Range (Decl, List) is
               when -1 =>
                  Add_In_List (Node, Previous, Parent, Decl);
                  return;

               when 0 =>
                  Add_Single_Entity (Decl, List.Contents, List);
                  return;

               when 1 =>
                  null;

               when 2 =>
                  Add_In_List (Node, Previous, Parent, Decl);
                  loop
                     Save := List.Sibling;
                     List.Sibling := null;
                     Add_Single_Entity (List, Decl.Contents, Decl);
                     Decl.Sibling := Save;
                     List := Save;

                     exit when List = null or else In_Range (Decl, List) /= 2;
                  end loop;
                  return;

               when others =>
                  null;
            end case;
            Previous := List;
            List := List.Sibling;
         end loop;

         Add_In_List (Node, Previous, Parent, Decl);
      end Add_Single_Entity;

      -----------------------
      -- Add_Single_Entity --
      -----------------------

      procedure Add_Single_Entity
        (Decl   : in out Scope_List;
         T      : in out Scope_Tree)
      is
         P : Unit_Part;
         File_List : File_Info_Ptr_List;
         Num : Positive := 1;
         Source_Filename : GNAT.OS_Lib.String_Access;
      begin
         if Decl.Typ = Declaration then
            if Decl.Start_Of_Scope.File.LI /= Lib_Info then
               Free (Decl);
               return;
            end if;

            P := Decl.Start_Of_Scope.File.Part;
            Source_Filename := Decl.Start_Of_Scope.File.Source_Filename;
         else
            if Decl.Ref.Location.File.LI /= Lib_Info then
               Free (Decl);
               return;
            end if;

            P := Decl.Ref.Location.File.Part;
            Source_Filename := Decl.Ref.Location.File.Source_Filename;
         end if;

         case P is
            when Unit_Body =>
               Add_Single_Entity (Decl, T.Body_Tree, null);

            when Unit_Spec =>
               Add_Single_Entity (Decl, T.Spec_Tree, null);

            when Unit_Separate =>
               File_List := Lib_Info.LI.Separate_Info;
               while File_List /= null loop
                  exit when File_List.Value.Source_Filename.all =
                    Source_Filename.all;
                  Num := Num + 1;
                  File_List := File_List.Next;
               end loop;

               if Num <= T.Separate_Trees'Last then
                  Add_Single_Entity (Decl, T.Separate_Trees (Num), null);
               end if;
         end case;
      end Add_Single_Entity;

      --------------------
      -- Add_References --
      --------------------

      procedure Add_References
        (Decl : E_Declaration_Info;
         T : in out Scope_Tree;
         Decl_Start : File_Location)
      is
         R : E_Reference_List := Decl.References;
         New_Item : Scope_List;
      begin
         while R /= null loop
            --  Do not add labels to the scope tree, since these only bring
            --  syntactic information, and do not impact the code.

            if R.Value.Kind /= Label
              and then R.Value.Location /= Decl_Start
              and then R.Value.Location /=
              Decl.Declaration.End_Of_Scope.Location
            then
               New_Item := new Scope_Node'
                 (Typ         => Reference,
                  Sibling     => null,
                  Parent      => null,
                  Decl        => Decl.Declaration'Unrestricted_Access,
                  Ref         => R.Value'Unrestricted_Access);
               Add_Single_Entity (New_Item, T);
            end if;
            R := R.Next;
         end loop;
      end Add_References;

      ----------------------
      -- Add_Declarations --
      ----------------------

      procedure Add_Declarations
        (Decl : E_Declaration_Info_List; T : in out Scope_Tree)
      is
         List : E_Declaration_Info_List := Decl;
         New_Item : Scope_List;
         Start_Of_Scope : File_Location;
      begin
         while List /= null loop
            New_Item := new Scope_Node'
              (Typ            => Declaration,
               Decl           => List.Value.Declaration'Unrestricted_Access,
               Contents       => null,
               Start_Of_Scope => Null_File_Location,
               Parent      => null,
               Sibling        => null);
            Compute_Scope (New_Item, List.Value.References);
            Start_Of_Scope := New_Item.Start_Of_Scope;

            --  Try to insert the declaration in the tree. Note that this might
            --  actually delete New_Item if the declaration doesn't fit in the
            --  tree.

            Add_Single_Entity (New_Item, T);
            Add_References (List.Value, T, Start_Of_Scope);
            List := List.Next;
         end loop;
      end Add_Declarations;

      T         : Scope_Tree;
      File_List : File_Info_Ptr_List;
      Dep       : Dependency_File_Info_List;
      Num_Separates : Natural := 0;

   begin
      Assert
        (Me, Lib_Info.LI.Parsed, "Create_Tree: LI file hasn't been parsed");

      File_List := Lib_Info.LI.Separate_Info;
      while File_List /= null loop
         Num_Separates := Num_Separates + 1;
         File_List := File_List.Next;
      end loop;

      T := (Lib_Info    => Lib_Info,
            LI_Filename => new String' (Lib_Info.LI.LI_Filename.all),
            Time_Stamp  => 0,
            Body_Tree   => null,
            Spec_Tree   => null,
            Separate_Trees => new Scope_List_Array (1 .. Num_Separates));

      if Lib_Info.LI.Spec_Info /= null then
         Add_Declarations (Lib_Info.LI.Spec_Info.Declarations, T);
      end if;

      if Lib_Info.LI.Body_Info /= null then
         Add_Declarations (Lib_Info.LI.Body_Info.Declarations, T);
      end if;

      File_List := Lib_Info.LI.Separate_Info;
      while File_List /= null loop
         Add_Declarations (File_List.Value.Declarations, T);
         File_List := File_List.Next;
      end loop;

      Dep := Lib_Info.LI.Dependencies_Info;
      while Dep /= null loop
         Add_Declarations (Dep.Value.Declarations, T);
         Dep := Dep.Next;
      end loop;

      return T;

   exception
      when E : Constraint_Error | Assert_Failure =>
         Assert (Me, False, "Unexpected exception in Create_Tree: "
                & Exception_Information (E));
         Free (T);
         return Null_Scope_Tree;
   end Create_Tree;

   -----------------------
   -- Find_Entity_Scope --
   -----------------------

   function Find_Entity_Scope
     (Scope : Scope_List; Entity : Entity_Information) return Scope_Tree_Node
   is
      L : Scope_List := Scope;
      Result : Scope_Tree_Node;
   begin
      while L /= null loop
         if L.Typ = Declaration then
            if Is_Same_Entity (L.Decl.all, Entity) then
               return Scope_Tree_Node (L);
            end if;

            Result := Find_Entity_Scope (L.Contents, Entity);
            if Result /= null then
               return Result;
            end if;
         end if;

         L := L.Sibling;
      end loop;
      return null;
   end Find_Entity_Scope;

   -----------------------
   -- Find_Entity_Scope --
   -----------------------

   function Find_Entity_Scope
     (Tree : Scope_Tree; Entity : Entity_Information)
      return Scope_Tree_Node
   is
      Result : Scope_Tree_Node;
   begin
      Result := Find_Entity_Scope (Tree.Body_Tree, Entity);
      if Result = null then
         Result := Find_Entity_Scope (Tree.Spec_Tree, Entity);

         if Result = null then
            for P in Tree.Separate_Trees'Range loop
               Result := Find_Entity_Scope
                 (Tree.Separate_Trees (P), Entity);
               exit when Result /= null;
            end loop;
         end if;
      end if;

      return Result;
   end Find_Entity_Scope;

   ----------------------------
   -- Find_Entity_References --
   ----------------------------

   procedure Find_Entity_References
     (Node     : Scope_List;
      Decl     : E_Declaration_Info;
      Callback : Node_Callback)
   is
      L : Scope_List := Node;
   begin
      while L /= null loop
         case L.Typ is
            when Declaration =>
               Find_Entity_References (L.Contents, Decl, Callback);

            when Reference =>
               if Is_Same_Entity (L.Decl.all, Decl.Declaration) then
                  Callback (Scope_Tree_Node (L));
               end if;
         end case;
         L := L.Sibling;
      end loop;
   end Find_Entity_References;

   ----------------------------
   -- Find_Entity_References --
   ----------------------------

   procedure Find_Entity_References
     (Tree : Scope_Tree;
      Decl : E_Declaration_Info;
      Callback : Node_Callback) is
   begin
      Find_Entity_References (Tree.Body_Tree, Decl, Callback);
      Find_Entity_References (Tree.Spec_Tree, Decl, Callback);
      for P in Tree.Separate_Trees'Range loop
         Find_Entity_References (Tree.Separate_Trees (P), Decl, Callback);
      end loop;
   end Find_Entity_References;

   -----------
   -- Start --
   -----------

   function Start (Node : Scope_Tree_Node) return Scope_Tree_Node_Iterator is
   begin
      return Scope_Tree_Node_Iterator (Node.Contents);
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Scope_Tree_Node_Iterator) is
   begin
      if Iter /= null then
         Iter := Scope_Tree_Node_Iterator (Iter.Sibling);
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Scope_Tree_Node_Iterator) return Scope_Tree_Node is
   begin
      return Scope_Tree_Node (Iter);
   end Get;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Node : Scope_Tree_Node) return Scope_Tree_Node is
   begin
      return Scope_Tree_Node (Node.Parent);
   end Get_Parent;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Entity : Entity_Information) return String is
   begin
      return Entity.Name.all;
   end Get_Name;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Node : Scope_Tree_Node) return Entity_Information is
   begin
      return Entity_Information'
        (Name        => new String' (Node.Decl.Name.all),
         Decl_Line   => Node.Decl.Location.Line,
         Decl_Column => Node.Decl.Location.Column,
         Decl_File   => new String'
           (Get_Source_Filename (Node.Decl.Location.File)));
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Decl : E_Declaration_Info) return Entity_Information is
   begin
      return Entity_Information'
        (Name        => new String' (Decl.Declaration.Name.all),
         Decl_Line   => Decl.Declaration.Location.Line,
         Decl_Column => Decl.Declaration.Location.Column,
         Decl_File   => new String'
           (Get_Source_Filename (Decl.Declaration.Location.File)));
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Entity : in out Entity_Information) is
   begin
      Free (Entity.Decl_File);
      Free (Entity.Name);
   end Destroy;

   -----------------------------
   -- Get_Declaration_Line_Of --
   -----------------------------

   function Get_Declaration_Line_Of (Entity : Entity_Information)
      return Positive is
   begin
      return Entity.Decl_Line;
   end Get_Declaration_Line_Of;

   -------------------------------
   -- Get_Declaration_Column_Of --
   -------------------------------

   function Get_Declaration_Column_Of (Entity : Entity_Information)
      return Natural is
   begin
      return Entity.Decl_Column;
   end Get_Declaration_Column_Of;

   -----------------------------
   -- Get_Declaration_File_Of --
   -----------------------------

   function Get_Declaration_File_Of (Entity : Entity_Information)
      return String is
   begin
      return Entity.Decl_File.all;
   end Get_Declaration_File_Of;

   --------------------
   -- Is_Same_Entity --
   --------------------

   function Is_Same_Entity
     (Decl : E_Declaration; Entity : Entity_Information)
      return Boolean is
   begin
      return Decl.Location.Line         = Entity.Decl_Line
        and then Decl.Location.Column   = Entity.Decl_Column
        and then Decl.Name.all          = Entity.Name.all
        and then Get_Source_Filename (Decl.Location.File) =
           Entity.Decl_File.all;
   end Is_Same_Entity;

   --------------------
   -- Is_Same_Entity --
   --------------------

   function Is_Same_Entity (Decl1, Decl2 : E_Declaration)
      return Boolean is
   begin
      return Decl1.Location.Line         = Decl2.Location.Line
        and then Decl1.Location.Column   = Decl2.Location.Column
        and then Decl1.Name.all          = Decl2.Name.all
        and then Get_Source_Filename (Decl1.Location.File) =
          Get_Source_Filename (Decl2.Location.File);
   end Is_Same_Entity;

   ----------
   -- Hash --
   ----------

   function Hash (F : GNAT.OS_Lib.String_Access) return Name_Htable_Num is
   begin
      return Hash (F.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : GNAT.OS_Lib.String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out Entity_Reference_Iterator) is
   begin
      Free (Iterator.Importing);
      Free (Iterator.Source_Files);
      Reset (Iterator.Examined);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out Entity_Reference_Iterator_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Entity_Reference_Iterator, Entity_Reference_Iterator_Access);
   begin
      Destroy (Iterator.all);
      Unchecked_Free (Iterator);
   end Destroy;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Entity_Reference_Iterator) return E_Reference is
   begin
      if Iterator.References = null then
         return No_Reference;
      else
         return Iterator.References.Value;
      end if;
   end Get;

   ------------
   -- Get_LI --
   ------------

   function Get_LI (Iterator : Entity_Reference_Iterator) return LI_File_Ptr is
   begin
      return Iterator.LI;
   end Get_LI;

   ----------
   -- Next --
   ----------

   procedure Next
     (Iterator : in out Entity_Reference_Iterator;
      List         : in out LI_File_List)
   is
      function Check_Declarations (Declarations : E_Declaration_Info_List)
         return E_Reference_List;
      --  Return the list of references to the entity in Declarations, or null
      --  if there is none.

      function Check_File return E_Reference_List;
      --  Check the current file in the iterator, and return the list of
      --  references that matches the declaration (or null if there is none).

      function Check_LI (LI : LI_File_Ptr) return E_Reference_List;
      --  Set the iterator to examine the declarations in LI

      function Check_Decl_File return E_Reference_List;
      --  Check the next declaration list in Iterator.LI

      ------------------------
      -- Check_Declarations --
      ------------------------

      function Check_Declarations (Declarations : E_Declaration_Info_List)
         return E_Reference_List
      is
         D : E_Declaration_Info_List := Declarations;
      begin
         while D /= null loop
            if Is_Same_Entity
              (D.Value.Declaration, Iterator.Decl.Declaration)
            then
               return D.Value.References;
            end if;

            D := D.Next;
         end loop;
         return null;
      end Check_Declarations;

      ----------------
      -- Check_File --
      ----------------

      function Check_File return E_Reference_List is
         LI      : LI_File_Ptr;
         Handler : LI_Handler;
         Sep_List : File_Info_Ptr_List;
      begin
         if not Get
           (Iterator.Examined,
            GNAT.OS_Lib.String_Access
            (Iterator.Source_Files (Iterator.Current_File)))
         then
            LI := Locate_From_Source
              (List, Iterator.Source_Files (Iterator.Current_File).all);
            Handler := Handler_From_Filename
              (Iterator.Importing (Iterator.Current_Project),
               Iterator.Source_Files (Iterator.Current_File).all);

            --  Do nothing if the file is not the same language
            if LI = null
              or else LI.LI.Handler = Handler
            then
               Create_Or_Complete_LI
                 (Handler                => Handler,
                  File                   => LI,
                  Source_Filename        =>
                    Iterator.Source_Files (Iterator.Current_File).all,
                  List                   => List,
                  Project                =>
                    Iterator.Importing (Iterator.Current_Project),
                  Predefined_Source_Path => "",
                  Predefined_Object_Path => "");
            end if;

            if LI /= null then
               --  Memorize the list of files that have been examined, to avoid
               --  parsing again the same LI file
               if LI.LI.Spec_Info /= null then
                  Set
                    (Iterator.Examined, LI.LI.Spec_Info.Source_Filename, True);
               end if;

               if LI.LI.Body_Info /= null then
                  Set
                    (Iterator.Examined, LI.LI.Body_Info.Source_Filename, True);
               end if;

               Sep_List := LI.LI.Separate_Info;
               while Sep_List /= null loop
                  if Sep_List.Value /= null then
                     Set (Iterator.Examined,
                          Sep_List.Value.Source_Filename, True);
                  end if;
                  Sep_List := Sep_List.Next;
               end loop;

               return Check_LI (LI);
            end if;
         end if;
         return null;
      end Check_File;

      --------------
      -- Check_LI --
      --------------

      function Check_LI (LI : LI_File_Ptr) return E_Reference_List is
         Ref       : E_Reference_List;
         Decl_List : Dependency_File_Info_List;
         Decl_LI   : LI_File_Ptr := Iterator.Decl.Declaration.Location.File.LI;
      begin
         --  If this is the LI file for Decl, we need to parse the
         --  body and spec infos. Otherwise, only the dependencies
         Iterator.LI := LI;
         if LI = Decl_LI then
            Iterator.Part := Unit_Spec;

            if LI.LI.Spec_Info /= null then
               Ref := Check_Declarations (LI.LI.Spec_Info.Declarations);
               if Ref /= null then
                  return Ref;
               end if;
            end if;

            return Check_Decl_File;


         --  Otherwise, check all the dependencies to see if we
         --  have the entity.
         else
            Decl_List := LI.LI.Dependencies_Info;
            while Decl_List /= null loop
               if Decl_List.Value.File.LI = Decl_LI then
                  return Check_Declarations (Decl_List.Value.Declarations);
               end if;

               Decl_List := Decl_List.Next;
            end loop;
         end if;
         return null;
      end Check_LI;

      ---------------------
      -- Check_Decl_File --
      ---------------------

      function Check_Decl_File return E_Reference_List is
         Ref : E_Reference_List;
      begin
         --  Were we checking the declarations from the spec ?
         if Iterator.Part = Unit_Spec then
            Iterator.Part := Unit_Body;
            if Iterator.LI.LI.Body_Info /= null then
               Ref := Check_Declarations
                 (Iterator.LI.LI.Body_Info.Declarations);
               if Ref /= null then
                  return Ref;
               end if;
            end if;
         end if;

         --  Were we checking the declarations from the body ?
         if Iterator.Part = Unit_Body then
            Iterator.Part := Unit_Separate;
            Iterator.Current_Separate := Iterator.LI.LI.Separate_Info;
         end if;

         --  Are we currently checking the separates
         if Iterator.Part = Unit_Separate then
            while Iterator.Current_Separate /= null loop
               Ref := Check_Declarations
                 (Iterator.Current_Separate.Value.Declarations);
               if Ref /= null then
                  return Ref;
               end if;

               Iterator.Current_Separate := Iterator.Current_Separate.Next;
            end loop;
         end if;
         return null;
      end Check_Decl_File;

   begin
      --  If necessary, force skipping to the next LI file
      if Iterator.LI_Once then
         Iterator.References := null;
         Iterator.Part := Unit_Separate;
         Iterator.Current_Separate := null;
      end if;

      --  If there are still some references
      if Iterator.References /= null then
         Iterator.References := Iterator.References.Next;
      end if;

      if Iterator.References = null then
         --  Were we processing the LI file for the source file that contains
         --  the initial declaration ?
         if Iterator.LI = Iterator.Decl.Declaration.Location.File.LI then
            Iterator.References := Check_Decl_File;
            if Iterator.References /= null then
               return;
            end if;
         end if;

         while Iterator.References = null loop

            --  Move to the next file in the current project
            loop
               Iterator.Current_File := Iterator.Current_File + 1;
               exit when Iterator.Current_File > Iterator.Source_Files'Last;
               Iterator.References := Check_File;
               if Iterator.References /= null then
                  return;
               end if;
            end loop;

            --  Move to the next project

            Free (Iterator.Source_Files);
            Reset (Iterator.Examined);
            Iterator.Current_Project := Iterator.Current_Project + 1;

            if Iterator.Current_Project > Iterator.Importing'Last then
               Free (Iterator.Importing);
               return;
            end if;

            Iterator.Source_Files := Get_Source_Files
              (Get_Project_From_View
                 (Iterator.Importing (Iterator.Current_Project)),
               Recursive => False,
               Full_Path => False);
            Iterator.Current_File := Iterator.Source_Files'First - 1;
         end loop;
      end if;
   end Next;

   -------------------------
   -- Find_All_References --
   -------------------------
   --  Algorithm:

   --  For efficiency, we only look in the direct sources of all the projects
   --  that import, even indirectly the project that contains the declaration
   --  source file. No files in other projects can reference that entity, since
   --  it is not visible.
   --  We also only look in the files that are in the same language.

   procedure Find_All_References
     (Root_Project : Prj.Tree.Project_Node_Id;
      Decl         : E_Declaration_Info;
      List         : in out LI_File_List;
      Iterator     : out Entity_Reference_Iterator;
      Project      : Prj.Project_Id := Prj.No_Project;
      LI_Once      : Boolean := False)
   is
      Decl_Project : Project_Id := Project;
   begin
      if Decl_Project = No_Project then
         Decl_Project := Get_Project_From_File
           (Get_Project_View_From_Project (Root_Project),
            Get_File (Get_Location (Decl)));
      end if;

      Iterator.Decl := Decl;
      Iterator.LI_Once := LI_Once;
      Iterator.Importing := new Project_Id_Array'
        (Find_All_Projects_Importing (Root_Project, Decl_Project));
      Iterator.Current_Project := Iterator.Importing'First;
      Iterator.Source_Files := Get_Source_Files
        (Get_Project_From_View (Iterator.Importing
                                (Iterator.Current_Project)),
         Recursive => False,
         Full_Path => False);
      Iterator.Current_File := Iterator.Source_Files'First - 1;

      Next (Iterator, List);
   end Find_All_References;

end Src_Info.Queries;
