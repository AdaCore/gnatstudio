-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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
with Traces; use Traces;
with GNAT.OS_Lib; use GNAT.OS_Lib;

package body Src_Info.Queries is

   Me : Debug_Handle := Create ("SRC_INFO");

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

   procedure Find_Next_Body_Ref
     (Decl            : E_Declaration_Info;
      Ref             : E_Reference_List := null;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Find_Decl_Or_Body_Query_Status);
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
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Find_Decl_Or_Body_Query_Status);
   --  ??? Document...

   procedure Destroy (Dep : in out Dependency);
   --  Deallocates the memory associated with the given Dependency record.

   procedure Trace_Dump
     (Handler : Debug_Handle; Scope : Scope_List; Prefix : String);
   --  Dump Scope to Handler, printing Prefix at the beginning of each line

   procedure Free (Scope : in out Scope_List);
   --  Free the memory occupied by Scope.

   function Find_Entity_Declaration
     (Scope : Scope_List; Name : String; Line, Column : Integer)
      return Scope_Tree_Node;
   --  Find the declaration for Name in Scope.

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

   procedure Find_Next_Body_Ref
     (Decl            : E_Declaration_Info;
      Ref             : E_Reference_List := null;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Find_Decl_Or_Body_Query_Status)
   is
      Last_Body_Ref : E_Reference_List;
      Current_Ref   : E_Reference_List;
      Entity_Name   : String renames Decl.Declaration.Name.all;
   begin
      --  Search the body reference immediately placed after the given
      --  Ref. Note that the references are stored in _reverse_ order...
      Current_Ref := Decl.References;
      while Current_Ref /= Ref and then Current_Ref /= null loop
         --  The test against null is just a guard against programing errors,
         --  just in case we are given a ref which is not part of the reference
         --  list of Decl...
         if Current_Ref.Value.Kind = Body_Entity then
            Last_Body_Ref := Current_Ref;
         end if;
         Current_Ref := Current_Ref.Next;
      end loop;

      if Last_Body_Ref = null and then Ref /= null then
         --  Case where we are on one of the body entities references and did
         --  not find a next body entity reference, so return the location
         --  of the declaration...
         File_Name_Found := new String'
           (Get_Source_Filename (Decl.Declaration.Location.File));
         Start_Line := Decl.Declaration.Location.Line;
         Start_Column := Decl.Declaration.Location.Column;
         End_Line := Start_Line;
         End_Column := Start_Column + Entity_Name'Length;
         Status := Success;

      elsif Last_Body_Ref /= null then
         --  Case where we found a body entity reference. Return its location.
         File_Name_Found := new String'
           (Get_Source_Filename (Last_Body_Ref.Value.Location.File));
         Start_Line   := Last_Body_Ref.Value.Location.Line;
         Start_Column := Last_Body_Ref.Value.Location.Column;
         End_Line     := Start_Line;
         End_Column   := Start_Column + Entity_Name'Length;
         Status := Success;

      else
         --  Case where we are located at the declaration itself, and could
         --  not find any body reference. Return no location.
         File_Name_Found := null;
         Status := No_Body_Entity_Found;
      end if;
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
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Find_Decl_Or_Body_Query_Status)
   is
      Current_Decl : E_Declaration_Info_List := Decl;
      Current_Ref  : E_Reference_List;
   begin
      --  Initialize the value of the returned parameters
      File_Name_Found := null;
      Start_Line := 1;
      Start_Column := 1;
      End_Line := 1;
      End_Column := 1;
      Status := Entity_Not_Found;

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
               Find_Next_Body_Ref
                 (Current_Decl.Value, null, File_Name_Found,
                  Start_Line, Start_Column, End_Line, End_Column, Status);
               exit Decl_Loop;
            end if;

            --  Search in the list of references.
            Current_Ref := Current_Decl.Value.References;
            Ref_Loop :
            while Current_Ref /= null loop

               if Location_Matches
                    (Current_Ref.Value.Location, File_Name, Line, Column)
               then
                  --  If this is a body reference, then we try to navigate
                  --  to the next body reference.
                  if Current_Ref.Value.Kind = Body_Entity then
                     Find_Next_Body_Ref
                       (Current_Decl.Value, Current_Ref, File_Name_Found,
                        Start_Line, Start_Column, End_Line, End_Column,
                        Status);
                  else
                     --  This is a non-body entity reference, so jump to the
                     --  declaration
                     File_Name_Found := new String'
                       (Get_Source_Filename
                         (Current_Decl.Value.Declaration.Location.File));
                     Start_Line :=
                       Current_Decl.Value.Declaration.Location.Line;
                     Start_Column :=
                       Current_Decl.Value.Declaration.Location.Column;
                     End_Line := Start_Line;
                     End_Column := Start_Column + Entity_Name'Length;
                     Status := Success;
                  end if;
                  exit Decl_Loop;
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
     (Lib_Info        : LI_File_Ptr;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Find_Decl_Or_Body_Query_Status)
   is
      Current_Sep : File_Info_Ptr_List;
      Current_Dep : Dependency_File_Info_List;
   begin
      --  Assumption: if the Lib_Info structure is up-to-date, then the casing
      --  of the entity we are searching (here Entity_Name) is identical
      --  to the casing inside the Lib_Info, in which case we do not need
      --  to do case-insensitive string matching. This is important to avoid
      --  breaking the support for case-sensitive languages such as C for
      --  instance.

      --  Search a matching entity declaration in the Spec
      if Lib_Info.LI.Spec_Info /= null
        and then Lib_Info.LI.Spec_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.LI.Spec_Info.Declarations,
            File_Name, Entity_Name, Line, Column,
            File_Name_Found, Start_Line, Start_Column, End_Line, End_Column,
            Status);

         if Search_Is_Completed (Status) then
            return;
         end if;
      end if;

      --  Search in the Body
      if Lib_Info.LI.Body_Info /= null
        and then Lib_Info.LI.Body_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.LI.Body_Info.Declarations,
            File_Name, Entity_Name, Line, Column,
            File_Name_Found, Start_Line, Start_Column, End_Line, End_Column,
            Status);
         if Search_Is_Completed (Status) then
            return;
         end if;
      end if;

      --  Search in the separates
      Current_Sep := Lib_Info.LI.Separate_Info;
      while Current_Sep /= null loop
         if Current_Sep.Value.Declarations /= null then
            Find_Spec_Or_Body
              (Current_Sep.Value.Declarations,
               File_Name, Entity_Name, Line, Column,
               File_Name_Found, Start_Line, Start_Column,
               End_Line, End_Column, Status);

            if Search_Is_Completed (Status) then
               return;
            end if;
         end if;

         Current_Sep := Current_Sep.Next;
      end loop;

      --  Search in the list of dependencies, if any
      Current_Dep := Lib_Info.LI.Dependencies_Info;
      while Current_Dep /= null loop
         if Current_Dep.Value.Declarations /= null then
            Find_Spec_Or_Body
              (Current_Dep.Value.Declarations,
               File_Name, Entity_Name, Line, Column,
               File_Name_Found, Start_Line, Start_Column,
               End_Line, End_Column, Status);

            if Search_Is_Completed (Status) then
               return;
            end if;
         end if;

         Current_Dep := Current_Dep.Next;
      end loop;

      --  If we reach this point, that means we did not find the entity in
      --  our list of declarations.
      File_Name_Found := null;
      Start_Line := 1;
      Start_Column := 1;
      End_Line := 1;
      End_Column := 1;
      Status := Entity_Not_Found;
   exception
      when others =>
         --  Trap all exceptions for better robustness, and report an
         --  internal error
         File_Name_Found := null;
         Start_Line := 1;
         Start_Column := 1;
         End_Line := 1;
         End_Column := 1;
         Status := Internal_Error;
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
                     Unit_Name => null,
                     LI_Name   => new String'
                       (Current_Dep.Value.File.LI.LI.LI_Filename.all)),
                  Dep  => Current_Dep.Value.Dep_Info),
               Next  => Dependencies);

            if FI.Unit_Name /= null then
               Dependencies.Value.File.Unit_Name :=
                 new String' (FI.Unit_Name.all);
            end if;

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

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump
     (Handler : Debug_Handle; Scope : Scope_List; Prefix : String)
   is
      L : Scope_List := Scope;
   begin
      while L /= null loop
         if L.Decl.Kind = Generic_Function_Or_Operator
            --  or else L.Decl.Kind = Generic_Package
           or else L.Decl.Kind = Generic_Procedure
           or else L.Decl.Kind = Non_Generic_Function_Or_Operator
            --  or else L.Decl.Kind = Non_Generic_Package
           or else L.Decl.Kind = Non_Generic_Procedure
         then
            case L.Typ is
               when Declaration =>
                  Trace (Handler, Prefix & "Decl: " & L.Decl.Name.all
                         & " range=" & L.Start_Of_Scope.Line'Img
                         & L.Decl.End_Of_Scope.Location.Line'Img
                         & "," & L.Decl.End_Of_Scope.Location.Column'Img);
                  Trace_Dump (Handler, L.Contents, Prefix & "  ");

               when Reference =>
                  Trace (Handler, Prefix & "Ref: " & L.Decl.Name.all
                         & " line=" & L.Ref.Location.Line'Img
                         & " col=" & L.Ref.Location.Column'Img);
            end case;
         end if;
         L := L.Sibling;
      end loop;
   end Trace_Dump;

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump (Handler : Traces.Debug_Handle; Tree : Scope_Tree) is
   begin
      if Tree /= Null_Scope_Tree then
         Trace (Handler, "Scope tree for " & Tree.LI_Filename.all);
         Trace_Dump (Handler, Tree.Body_Tree, "");
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
   end Free;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree (Lib_Info : LI_File_Ptr) return Scope_Tree is

      procedure Add_Declarations
        (Decl : E_Declaration_Info_List; L : in out Scope_List);
      --  Add the declarations from Decl into L.

      procedure Add_Single_Entity
        (Decl : in out Scope_List; L : in out Scope_List);
      --  Add a single reference in the tree

      procedure Add_References
        (Decl : E_Declaration_Info;
         L : in out Scope_List;
         Decl_Start : File_Location);
      --  Add all the references to the entity declared in Decl.
      --  Decl_Start is the starting location of the scope for the entity.

      function In_Range (Decl : Scope_List; Loc : Scope_List) return Integer;
      --  True if Loc is in the range of Decl.
      --  -1 is returned if Decl is before Loc, 0 if within, 1 if after.
      --  It returns -2 if the positions could not be compared (invalid file,
      --  Decl is not a range,...)

      procedure Add_In_List
        (L        : in out Scope_List;
         Previous : in out Scope_List;
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

      --------------
      -- In_Range --
      --------------

      function In_Range
        (Decl : Scope_List; Loc : Scope_List) return Integer
      is
         L : File_Location;
      begin
         case Loc.Typ is
            when Declaration => L := Loc.Decl.Location;
            when Reference   => L := Loc.Ref.Location;
         end case;

         if L.File.LI /= Lib_Info
           or else (Loc.Typ = Reference and then L.File.Part /= Unit_Body)
         then
            return -2;

         elsif Decl.Typ = Declaration then
            if Decl.Start_Of_Scope.Line > L.Line
              or else (L.Line = Decl.Start_Of_Scope.Line
                       and then Decl.Start_Of_Scope.Column > L.Column)
            then
               return 1;

            elsif Decl.Decl.End_Of_Scope = No_Reference then
               return -1;

            elsif Decl.Decl.End_Of_Scope.Location.Line < L.Line
              or else
              (L.Line = Decl.Decl.End_Of_Scope.Location.Line
               and then Decl.Decl.End_Of_Scope.Location.Column < L.Column)
            then
               return -1;

            else
               return 0;
            end if;

         elsif Decl.Ref.Location.Line < L.Line
           or else (Decl.Ref.Location.Line = L.Line
                    and then Decl.Ref.Location.Column < L.Column)
         then
            return -1;
         else
            return 1;
         end if;
      end In_Range;

      -----------------
      -- Add_In_List --
      -----------------

      procedure Add_In_List
        (L        : in out Scope_List;
         Previous : in out Scope_List;
         New_Item : Scope_List)
      is
         T : Scope_List;
      begin
         if Previous = null then
            T := L;
            L := New_Item;
            L.Sibling := T;
         else
            T := Previous.Sibling;
            Previous.Sibling := New_Item;
            Previous.Sibling.Sibling := T;
         end if;
      end Add_In_List;

      -----------------------
      -- Add_Single_Entity --
      -----------------------

      procedure Add_Single_Entity
        (Decl : in out Scope_List; L : in out Scope_List)
      is
         Pos : Integer;
         List : Scope_List := L;
         Previous : Scope_List := null;
         Save : Scope_List;
      begin
         while List /= null loop
            case List.Typ is
               when Declaration =>
                  Pos := In_Range (List, Decl);
                  if Pos = 0 then
                     Add_Single_Entity (Decl, List.Contents);
                     return;

                  elsif Pos = -2 then
                     Free (Decl);
                     return;

                  elsif Pos = -1 then
                     null;

                  else
                     Add_In_List (L, Previous, Decl);

                     --  If in fact the new declaration englobs Loc.
                     if Decl.Typ = Declaration then
                        while List /= null
                          and then In_Range (Decl, List) = 0
                        loop
                           Save := List.Sibling;
                           Add_Single_Entity (List, Decl.Contents);
                           Decl.Sibling := Save;
                           List := Save;
                        end loop;
                     end if;

                     return;
                  end if;

               when Reference =>
                  if In_Range (List, Decl) = 1 then
                     Add_In_List (L, Previous, Decl);
                     return;
                  end if;

            end case;
            Previous := List;
            List := List.Sibling;
         end loop;

         Add_In_List (L, Previous, Decl);
      end Add_Single_Entity;

      --------------------
      -- Add_References --
      --------------------

      procedure Add_References
        (Decl : E_Declaration_Info;
         L : in out Scope_List;
         Decl_Start : File_Location)
      is
         R : E_Reference_List := Decl.References;
         New_Item : Scope_List;
      begin
         while R /= null loop
            if R.Value.Location /= Decl_Start
              and then R.Value.Location /=
              Decl.Declaration.End_Of_Scope.Location
            then
               New_Item := new Scope_Node'
                 (Typ         => Reference,
                  Sibling     => null,
                  Decl        => Decl.Declaration'Unrestricted_Access,
                  Ref         => R.Value'Unrestricted_Access);
               Add_Single_Entity (New_Item, L);
            end if;
            R := R.Next;
         end loop;
      end Add_References;

      ----------------------
      -- Add_Declarations --
      ----------------------

      procedure Add_Declarations
        (Decl : E_Declaration_Info_List; L : in out Scope_List)
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
               Sibling        => null);
            Compute_Scope (New_Item, List.Value.References);
            Start_Of_Scope := New_Item.Start_Of_Scope;

            --  Try to insert the declaration in the tree. Note that this might
            --  actually delete New_Item if the declaration doesn't fit in the
            --  tree.
            Add_Single_Entity (New_Item, L);

            Add_References (List.Value, L, Start_Of_Scope);
            List := List.Next;
         end loop;
      end Add_Declarations;

      T : Scope_Tree;
      L : Scope_List;
      File_List : File_Info_Ptr_List;
      Dep : Dependency_File_Info_List;
   begin
      Assert
        (Me, Lib_Info.LI.Parsed, "Create_Tree: LI file hasn't been parsed");

      --  ??? Should make sure that Lib_Info is parsed.
      --  ??? Parse Lib_Info.Dependencies_Info (Dependency_File_Info_List)

      if Lib_Info.LI.Spec_Info /= null then
         Add_Declarations (Lib_Info.LI.Spec_Info.Declarations, L);
      end if;

      if Lib_Info.LI.Body_Info /= null then
         Add_Declarations (Lib_Info.LI.Body_Info.Declarations, L);
      end if;

      File_List := Lib_Info.LI.Separate_Info;
      while File_List /= null loop
         Add_Declarations (File_List.Value.Declarations, L);
         File_List := File_List.Next;
      end loop;

      Dep := Lib_Info.LI.Dependencies_Info;
      while Dep /= null loop
         Add_Declarations (Dep.Value.Declarations, L);
         Dep := Dep.Next;
      end loop;

      T := (Lib_Info    => Lib_Info,
            LI_Filename => new String' (Lib_Info.LI.LI_Filename.all),
            Time_Stamp  => 0,
            Body_Tree   => L);
      return T;
   end Create_Tree;

   -----------------------------
   -- Find_Entity_Declaration --
   -----------------------------

   function Find_Entity_Declaration
     (Scope : Scope_List; Name : String; Line, Column : Integer)
      return Scope_Tree_Node
   is
      L : Scope_List := Scope;
      Result : Scope_Tree_Node;
   begin
      while L /= null loop
         if L.Typ = Declaration then
            if L.Decl.Name.all = Name
              and then L.Decl.Location.Line = Line
              and then L.Decl.Location.Column = Column
            then
               return Scope_Tree_Node (L);
            end if;

            Result := Find_Entity_Declaration
              (L.Contents, Name, Line, Column);
            if Result /= null then
               return Result;
            end if;
         end if;

         L := L.Sibling;
      end loop;
      return null;
   end Find_Entity_Declaration;

   -----------------------------
   -- Find_Entity_Declaration --
   -----------------------------

   function Find_Entity_Declaration
     (Tree : Scope_Tree; Name : String; Line, Column : Integer)
      return Scope_Tree_Node is
   begin
      return Find_Entity_Declaration
        (Tree.Body_Tree, Name, Line, Column);
   end Find_Entity_Declaration;

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

   function Get_Entity (Tree : Scope_Tree; Node : Scope_Tree_Node)
      return Entity_Information is
   begin
      return Entity_Information'
        (Name        => new String' (Node.Decl.Name.all),
         Decl_Line   => Node.Decl.Location.Line,
         Decl_Column => Node.Decl.Location.Column,
         Decl_File   => new String'
           (Get_Source_Filename (Node.Decl.Location.File)));
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

end Src_Info.Queries;
