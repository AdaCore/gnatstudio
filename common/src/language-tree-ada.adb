-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2006                        --
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

with Ada.Unchecked_Deallocation; use Ada;

with Generic_List;
with Basic_Types; use Basic_Types;
with String_Utils;      use String_Utils;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Language.Ada; use Language.Ada;

package body Language.Tree.Ada is

   ----------
   -- Free --
   ----------

   procedure Free (Ada_Tree : in out Ada_Construct_Tree_Access) is
      procedure Internal is new Unchecked_Deallocation
        (Ada_Construct_Tree, Ada_Construct_Tree_Access);
   begin
      Internal (Ada_Tree);
   end Free;

   ---------------------------------
   -- Generate_Ada_Construct_Tree --
   ---------------------------------

   function Generate_Ada_Construct_Tree
     (Tree     : Construct_Tree;
      Language : Language_Access;
      Buffer   : String) return Ada_Construct_Tree
   is
      function Check_Profiles
        (Tree : Construct_Tree; Prof1, Prof2 : Construct_Tree_Iterator)
         return Boolean;
      --  Return true if the two profiles are equivalent, false otherwise.

      function Is_Equivalent_Category
        (Construct1, Construct2 : Simple_Construct_Information) return Boolean;
      --  Return true if the two categories are equivalent, which is larger
      --  than a strict equality (for example, a Cat_Type and a Cat_Record
      --  are equivalent, since they can denote the same object).

      procedure Compute_Scope
        (Tree       : Construct_Tree;
         Ada_Tree      : in out Ada_Construct_Tree;
         Base_Iter  : Construct_Tree_Iterator;
         Base_Scope : Construct_Tree_Iterator := Null_Construct_Tree_Iterator);
      --  Set the spec, public body and private body info for this iterator.
      --  If Base_Scope is null, then the search will start at the entity after
      --  Base_Iter, otherwise it will start at the first child of Base_Sope.

      --------------------
      -- Check_Profiles --
      --------------------

      function Check_Profiles
        (Tree : Construct_Tree; Prof1, Prof2 : Construct_Tree_Iterator)
         return Boolean
      is
         Iter1, Iter2 : Construct_Tree_Iterator;

         Ref1_Sloc_Start : Source_Location;
         Ref1_Sloc_End   : Source_Location;
         Ref1_Success    : Boolean;

         Ref2_Sloc_Start : Source_Location;
         Ref2_Sloc_End   : Source_Location;
         Ref2_Success    : Boolean;
      begin
         Iter1 := Next (Tree, Prof1, Jump_Into);
         Iter2 := Next (Tree, Prof2, Jump_Into);

         while Get_Parent_Scope (Tree, Iter1) = Prof1
           and then Get_Parent_Scope (Tree, Iter2) = Prof2
         loop
            if (Get_Construct (Prof1).Category = Cat_Parameter
                and then Get_Construct (Prof2).Category = Cat_Parameter
                and then
                  ((Get_Language_Context (Language).Case_Sensitive and then
                      To_Lower (Get_Construct (Prof1).Name.all) /=
                      To_Lower (Get_Construct (Prof2).Name.all))
                   or else Get_Construct (Prof1).Name.all /=
                     Get_Construct (Prof2).Name.all))
              or else (Get_Construct (Prof1).Category = Cat_Parameter
                       xor Get_Construct (Prof2).Category = Cat_Parameter)
            then
               return False;
            end if;

            Iter1 := Next (Tree, Iter1);
            Iter2 := Next (Tree, Iter2);
         end loop;

         --  In case of functions, we should also check return types here !

         Get_Referenced_Entity
           (Lang       => Language,
            Buffer     => Buffer,
            Construct  => Get_Construct (Prof1),
            Sloc_Start => Ref1_Sloc_Start,
            Sloc_End   => Ref1_Sloc_End,
            Success    => Ref1_Success);

         Get_Referenced_Entity
           (Lang       => Language,
            Buffer     => Buffer,
            Construct  => Get_Construct (Prof2),
            Sloc_Start => Ref2_Sloc_Start,
            Sloc_End   => Ref2_Sloc_End,
            Success    => Ref2_Success);

         if Ref1_Success xor Ref2_Success then
            return False;
         elsif Ref1_Success then
            return
              (Get_Language_Context (Language).Case_Sensitive and then
               To_Lower (Buffer (Ref1_Sloc_Start.Index .. Ref1_Sloc_End.Index))
               = To_Lower
                 (Buffer (Ref2_Sloc_Start.Index .. Ref2_Sloc_End.Index)))
              or else Buffer (Ref1_Sloc_Start.Index .. Ref1_Sloc_End.Index)
              = Buffer (Ref2_Sloc_Start.Index .. Ref2_Sloc_End.Index);
         else
            return True;
         end if;

      end Check_Profiles;

      ----------------------------
      -- Is_Equivalent_Category --
      ----------------------------

      function Is_Equivalent_Category
        (Construct1, Construct2 : Simple_Construct_Information) return Boolean
      is
      begin
         return Construct1.Category = Construct2.Category
           or else (Construct1.Category in Type_Category
                    and then Construct2.Category in Type_Category);
      end Is_Equivalent_Category;

      -------------------
      -- Compute_Scope --
      -------------------

      procedure Compute_Scope
        (Tree       : Construct_Tree;
         Ada_Tree      : in out Ada_Construct_Tree;
         Base_Iter  : Construct_Tree_Iterator;
         Base_Scope : Construct_Tree_Iterator := Null_Construct_Tree_Iterator)
      is
         Local_Iter  : Construct_Tree_Iterator;
         Local_Scope : Construct_Tree_Iterator := Null_Construct_Tree_Iterator;

         Spec_Index         : Natural := 0;
         First_Body_Index   : Natural := 0;
         Second_Body_Index  : Natural := 0;

      begin
         if Get_Construct (Base_Iter).Name = null then
            return;
         end if;

         if Base_Scope = Null_Construct_Tree_Iterator then
            Local_Iter := Next (Tree, Base_Iter, Jump_Over);
            Local_Scope := Get_Parent_Scope (Tree, Base_Iter);
         else
            Local_Iter := Next (Tree, Base_Scope, Jump_Into);
            Local_Scope := Base_Scope;
         end if;

         while Local_Iter /= Null_Construct_Tree_Iterator
           and then Get_Parent_Scope (Tree, Local_Iter) = Local_Scope
         loop
            if Get_Construct (Local_Iter).Name /= null
              and then Get_Construct (Local_Iter).Name.all
              = Get_Construct (Base_Iter).Name.all
              and then Is_Equivalent_Category
                (Get_Construct (Base_Iter), Get_Construct (Local_Iter))
            then
               if Get_Construct (Base_Iter).Category
               in Subprogram_Category
               then
                  if not Check_Profiles (Tree, Base_Iter, Local_Iter) then
                     return;
                  end if;
               end if;

               if Ada_Tree (Base_Iter.Index).Spec_Index = 0 then
                  if Get_Construct (Base_Iter).Sloc_Start.Index <
                    Get_Construct (Local_Iter).Sloc_Start.Index
                  then
                     Spec_Index := Base_Iter.Index;
                     First_Body_Index := Local_Iter.Index;
                  else
                     Spec_Index := Local_Iter.Index;
                     First_Body_Index := Base_Iter.Index;
                  end if;
               else
                  if Get_Construct (Base_Iter).Sloc_Start.Index <
                    Get_Construct (Local_Iter).Sloc_Start.Index
                  then
                     Spec_Index := Base_Iter.Index;
                     First_Body_Index := Ada_Tree (Base_Iter.Index)
                       .First_Body_Index;
                     Second_Body_Index := Local_Iter.Index;
                  else
                     Spec_Index := Local_Iter.Index;
                     First_Body_Index := Base_Iter.Index;
                     Second_Body_Index := Ada_Tree (Base_Iter.Index)
                       .First_Body_Index;
                  end if;
               end if;

               Ada_Tree (Spec_Index).Spec_Index := Spec_Index;
               Ada_Tree (Spec_Index).First_Body_Index := First_Body_Index;
               Ada_Tree (Spec_Index).Second_Body_Index := Second_Body_Index;

               Ada_Tree (First_Body_Index).Spec_Index := Spec_Index;
               Ada_Tree (First_Body_Index).First_Body_Index :=
                 First_Body_Index;
               Ada_Tree (First_Body_Index).Second_Body_Index :=
                 Second_Body_Index;

               if Second_Body_Index /= 0 then
                  Ada_Tree (Second_Body_Index).Spec_Index := Spec_Index;
                  Ada_Tree (Second_Body_Index).First_Body_Index :=
                    First_Body_Index;
                  Ada_Tree (Second_Body_Index).Second_Body_Index :=
                    Second_Body_Index;
               end if;

               exit;
            end if;

            Local_Iter := Next (Tree, Local_Iter, Jump_Over);
         end loop;

         --  Look in the other parts if any

         if Local_Scope /= Null_Construct_Tree_Iterator
           and then Ada_Tree (Local_Scope.Index).Spec_Index /= 0
           and then Local_Scope.Index
             /= Ada_Tree (Local_Scope.Index).Spec_Index
         then
            Compute_Scope
              (Tree,
               Ada_Tree,
               Base_Iter,
               (Tree.Contents (Ada_Tree (Local_Scope.Index).Spec_Index),
                Ada_Tree (Local_Scope.Index).Spec_Index));
         end if;

      end Compute_Scope;

      Ada_Tree : Ada_Construct_Tree (Tree.Contents'Range);
      Iter  : Construct_Tree_Iterator;
   begin
      Iter := First (Tree);

      while Iter /= Null_Construct_Tree_Iterator loop
         Compute_Scope (Tree, Ada_Tree, Iter);

         Iter := Next (Tree, Iter, Jump_Into);
      end loop;

      return Ada_Tree;
   end Generate_Ada_Construct_Tree;

   --------------
   -- Get_Spec --
   --------------

   function Get_Spec
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator)
      return Construct_Tree_Iterator
   is
   begin
      if Ada_Tree (Iter.Index).Spec_Index = 0 then
         return Iter;
      else
         return
           (Tree.Contents (Ada_Tree (Iter.Index).Spec_Index),
            Ada_Tree (Iter.Index).Spec_Index);
      end if;
   end Get_Spec;

   --------------------
   -- Get_First_Body --
   --------------------

   function Get_First_Body
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator)
      return Construct_Tree_Iterator
   is
   begin
      if Ada_Tree (Iter.Index).First_Body_Index = 0 then
         return Iter;
      else
         return
           (Tree.Contents (Ada_Tree (Iter.Index).First_Body_Index),
            Ada_Tree (Iter.Index).First_Body_Index);
      end if;
   end Get_First_Body;

   ---------------------
   -- Get_Second_Body --
   ---------------------

   function Get_Second_Body
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator)
      return Construct_Tree_Iterator
   is
   begin
      if Ada_Tree (Iter.Index).Second_Body_Index = 0 then
         return Iter;
      else
         return
           (Tree.Contents (Ada_Tree (Iter.Index).Second_Body_Index),
            Ada_Tree (Iter.Index).Second_Body_Index);
      end if;
   end Get_Second_Body;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      Ada_Tree   : Ada_Construct_Tree;
      Offset     : Natural;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False) return Construct_Tree_Iterator_Array
   is
   begin
      return
        Get_Visible_Constructs
          (Tree,
           Ada_Tree,
           Get_Last_Relevant_Construct (Tree, Offset),
           Name,
           Use_Wise,
           Is_Partial);
   end Get_Visible_Constructs;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree       : Construct_Tree;
      Ada_Tree   : Ada_Construct_Tree;
      From       : Construct_Tree_Iterator;
      Name       : String;
      Use_Wise   : Boolean := True;
      Is_Partial : Boolean := False) return Construct_Tree_Iterator_Array
   is
      Lower_Case_Name : constant String := To_Lower (Name);

      procedure Free (This : in out Construct_Tree_Iterator);

      package Construct_Iterator_List_Pckg is new Generic_List
        (Construct_Tree_Iterator);

      use Construct_Iterator_List_Pckg;

      Constructs_Found   : Construct_Iterator_List_Pckg.List;

      Seek_Iterator      : Construct_Tree_Iterator;
      Prev_Iterator      : Construct_Tree_Iterator;
      Initial_Parent     : Construct_Tree_Iterator;

      type Use_Record is record
         Name    : GNAT.Strings.String_Access;
         --  ??? Would it save computation to store a Composite_Identifier
         --  here ?

         Removed : Boolean;
      end record;

      procedure Free (This : in out Use_Record);
      --  Free the memory associated to the Use_Record given in parameter.

      package Use_List_Pckg is new Generic_List (Use_Record);

      Use_List           : Use_List_Pckg.List;

      use Use_List_Pckg;

      function Name_Match
        (Construct_Tested : Simple_Construct_Information) return Boolean;
      --  Return true if the name given in parameter matches the expected one.
      --  This takes into account the value of Is_Partial.

      procedure Look_In_Package
        (Package_Iterator : Construct_Tree_Iterator;
         Allow_Private    : Boolean);
      --  See if we find the seeked entity in the given package. This will
      --  not check any of the use clause nor any of the entities in the
      --  encolsed or enclosing scopes. When the seeked entity is found, it is
      --  added to the list if relevant.

      procedure Add_If_Visible (It : Construct_Tree_Iterator);
      --  Add the given iterator to Construct_Found if and only if the iterator
      --  it visible even with the ones already in the list. If Last is false,
      --  the iterator will be preempted instead of being appened.

      procedure Analyze_Used_Package
        (Pckg     : Construct_Tree_Iterator;
         Root_Use : String;
         Begin_It : Use_List_Pckg.List_Node);
      --  Analyze the package considering the use information. If Root_Use is
      --  not an empty string, this means that the use taken into account has
      --  to be applied to a nested package. Use clauses after Begin_It will
      --  also be checked to see if they match a nested package.

      function Get_First_Item (Name : String) return String;
      --  Return first element of the name given in parameter

      function Remove_First_Item (Name : String) return String;
      --  Return the name given in parameter without the first element

      function Simplify_Scope_Name
        (Current_Scope : Composite_Identifier;
         Checked_Scope : Composite_Identifier) return Composite_Identifier;
      --  Return Checked_Scope without the elements found in Current_Scope.
      --  This simplifies use clauses, e.g., in:
      --
      --  package A is
      --
      --     ...
      --
      --     use A.B
      --
      --  for the name A.B this function will only return the element B.

      procedure Handle_Use_For (Pckg : Construct_Tree_Iterator);
      --  Handle the use clauses for the package given in parameter, in order
      --  to resolve visibile entities in it.

      ----------
      -- Free --
      ----------

      procedure Free (This : in out Use_Record) is
         pragma Unreferenced (This);
      begin
         null;
      end Free;

      ---------------------
      -- Look_In_Package --
      ---------------------

      procedure Look_In_Package
        (Package_Iterator : Construct_Tree_Iterator;
         Allow_Private    : Boolean)
      is
         It : Construct_Tree_Iterator;
      begin
         It := Get_Last_Child (Tree, Package_Iterator);

         while Get_Parent_Scope (Tree, It) = Package_Iterator loop
            if Get_Construct (It).Category in Cat_Package .. Cat_Field
              and then
                (Allow_Private
                 or else Get_Construct (It).Visibility = Visibility_Public)
              and then Get_Construct (It).Name /= null
              and then Name_Match (Get_Construct (It))
            then
               Add_If_Visible (It);
            end if;

            It := Prev (Tree, It, Jump_Over);
         end loop;
      end Look_In_Package;

      ----------
      -- Free --
      ----------

      procedure Free (This : in out Construct_Tree_Iterator) is
         pragma Unreferenced (This);
      begin
         null;
      end Free;

      --------------------
      -- Add_If_Visible --
      --------------------

      procedure Add_If_Visible (It : Construct_Tree_Iterator) is
         Node : Construct_Iterator_List_Pckg.List_Node :=
           First (Constructs_Found);
      begin
         while Node /= Construct_Iterator_List_Pckg.Null_Node loop
            if Get_Construct (It).Name.all
              = Get_Construct (Data (Node)).Name.all
            then
               --  If we found the spec of an entity of wich we already have
               --  the body, then replace the body by the spec.

               if Get_Spec (Tree, Ada_Tree, Data (Node)) = It then
                  Set_Data (Node, It);
                  return;
               end if;

               --  If we found an other node wich is not a subprogram, then the
               --  one we found is not visible.

               if Get_Construct (Data (Node)).Category not in
                 Subprogram_Category
               then
                  return;
               end if;
            end if;

            Node := Next (Node);
         end loop;

         Append (Constructs_Found, It);
      end Add_If_Visible;

      ----------------
      -- Name_Match --
      ----------------

      function Name_Match
        (Construct_Tested : Simple_Construct_Information) return Boolean
      is
         Name_Tested : constant String :=
           Get_Name_Index (Ada_Tree_Lang, Construct_Tested);
      begin
         if Is_Partial then
            return Name_Tested'Length >= Lower_Case_Name'Length
              and then Name_Tested
                (Name_Tested'First
                 .. Name_Tested'First + Lower_Case_Name'Length - 1)
              = Lower_Case_Name;
         else
            return Lower_Case_Name = Name_Tested;
         end if;
      end Name_Match;

      --------------------------
      -- Analyze_Used_Package --
      --------------------------

      procedure Analyze_Used_Package
        (Pckg     : Construct_Tree_Iterator;
         Root_Use : String;
         Begin_It : Use_List_Pckg.List_Node)
      is
         Tree_It : Construct_Tree_Iterator;
         Use_It  : Use_List_Pckg.List_Node := Next (Begin_It);
      begin
         if Root_Use /= "" then
            --  See the sub entities of this Root_Use

            Tree_It := Next (Tree, Pckg, Jump_Into);

            while Get_Parent_Scope (Tree, Tree_It) = Pckg loop
               if Get_Construct (Tree_It).Category = Cat_Package
                 and then Get_Construct (Tree_It).Is_Declaration
               then
                  if Get_First_Item (Get_Construct (Tree_It).Name.all)
                    = Get_First_Item (Root_Use)
                  then
                     Analyze_Used_Package
                       (Tree_It,
                        Remove_First_Item (Root_Use),
                        Begin_It);
                  end if;
               end if;

               Tree_It := Next (Tree, Tree_It, Jump_Over);
            end loop;
         else
            --  Analyze the current package

            Look_In_Package (Pckg, False);
         end if;

         --  See the use that have been called after

         Tree_It := Next (Tree, Pckg, Jump_Into);

         while Get_Parent_Scope (Tree, Tree_It) = Pckg loop
            Use_It := Next (Begin_It);

            if Get_Construct (Tree_It).Category = Cat_Package
              and then Get_Construct (Tree_It).Is_Declaration
            then
               while Use_It /= Use_List_Pckg.Null_Node loop
                  if not Data (Use_It).Removed
                    and then Get_First_Item (Data (Use_It).Name.all)
                    = Get_First_Item (Get_Construct (Tree_It).Name.all)
                  then
                     Analyze_Used_Package
                       (Tree_It,
                        Remove_First_Item (Get_Construct (Tree_It).Name.all),
                        Use_It);

                     Data_Ref (Use_It).Removed := True;
                  end if;

                  Use_It := Next (Use_It);
               end loop;
            end if;

            Tree_It := Next (Tree, Tree_It, Jump_Over);
         end loop;

      end Analyze_Used_Package;

      --------------------
      -- Get_First_Item --
      --------------------

      function Get_First_Item (Name : String) return String is
         Index : Natural := Name'First;
      begin
         Skip_To_Char (Name, Index, '.');

         if Index > Name'Last then
            return Name;
         else
            return Name (Name'First .. Index - 1);
         end if;
      end Get_First_Item;

      -----------------------
      -- Remove_First_Item --
      -----------------------

      function Remove_First_Item (Name : String) return String is
         Index : Natural := Name'First;
      begin
         Skip_To_Char (Name, Index, '.');

         if Index > Name'Last then
            return "";
         else
            return Name (Index + 1 .. Name'Last);
         end if;
      end Remove_First_Item;

      -------------------------
      -- Simplify_Scope_Name --
      -------------------------

      function Simplify_Scope_Name
        (Current_Scope : Composite_Identifier;
         Checked_Scope : Composite_Identifier) return Composite_Identifier
      is
         Checked_Index   : Integer := 1;
         Has_To_Be_Equal : Boolean := False;
      begin
         for Current_Index in 1 .. Length (Current_Scope) loop
            if not Has_To_Be_Equal
              and then Get_Item (Current_Scope, Current_Index)
              = Get_Item (Checked_Scope, 1)
            then
               Has_To_Be_Equal := True;
               Checked_Index := 2;
            end if;

            if Has_To_Be_Equal then
               if Checked_Index > Length (Checked_Scope) or else
                 Get_Item (Current_Scope, Current_Index)
                 /= Get_Item (Checked_Scope, Checked_Index)
               then
                  return Checked_Scope;
               else
                  Checked_Index := Checked_Index + 1;
               end if;
            end if;
         end loop;

         return Get_Slice
           (Checked_Scope, Checked_Index, Length (Checked_Scope));
      end Simplify_Scope_Name;

      --------------------
      -- Handle_Use_For --
      --------------------

      procedure Handle_Use_For (Pckg : Construct_Tree_Iterator) is
         Full_Name : constant String := Get_Full_Name (Tree, Pckg);
         Use_It    : Use_List_Pckg.List_Node;
      begin
         Use_It := First (Use_List);

         while Use_It /= Use_List_Pckg.Null_Node loop
            if not Data (Use_It).Removed then
               if Get_First_Item (Data (Use_It).Name.all) =
                 Get_First_Item
                   (Get_Construct (Pckg).Name.all)
               then
                  --  In this case, the first element of the use clause matches

                  Analyze_Used_Package
                    (Pckg,
                     Remove_First_Item (Data (Use_It).Name.all),
                     Use_It);

                  Data_Ref (Use_It).Removed := True;
               else
                  --  Otherwise, check if by removing the eventual extra prefix
                  --  we find something

                  declare
                     Scope_Id   : constant Composite_Identifier :=
                       To_Composite_Identifier (Full_Name);
                     Use_Id     : constant Composite_Identifier :=
                       To_Composite_Identifier (Data (Use_It).Name.all);
                     New_Use_Id : constant Composite_Identifier :=
                       Simplify_Scope_Name (Scope_Id, Use_Id);
                  begin
                     if Length (New_Use_Id) > 0
                       and then Get_Item (New_Use_Id, 1)
                       = Get_First_Item (Get_Construct (Pckg).Name.all)
                     then
                        Analyze_Used_Package
                          (Pckg,
                           Remove_First_Item (To_String (New_Use_Id)),
                           Use_It);

                        Data_Ref (Use_It).Removed := True;
                     end if;
                  end;
               end if;
            end if;

            Use_It := Next (Use_It);
         end loop;

         --  Once we finished to analyze the package and its possible
         --  interresting entities trough the uses, we have to remove every use
         --  clause that have been resolved.

         declare
            It      : Use_List_Pckg.List_Node := First (Use_List);
            Garbage : Use_List_Pckg.List_Node;
         begin
            while It /= Use_List_Pckg.Null_Node loop
               if Data (It).Removed then
                  Garbage := It;
                  It := Next (It);
                  Remove_Nodes (Use_List, Garbage, Garbage);
               else
                  It := Next (It);
               end if;
            end loop;
         end;
      end Handle_Use_For;

   begin
      if From /= Null_Construct_Tree_Iterator then

         --  Look back to see if we find the entity

         Seek_Iterator := From;

         while Seek_Iterator /= Null_Construct_Tree_Iterator loop

            case Get_Construct (Seek_Iterator).Category is
               when Cat_Use =>

                  --  If we are on a use clause, then have a look at the
                  --  corresponding package and see if we find the
                  --  seeked entity

                  if Use_Wise then
                     Prepend
                       (Use_List,
                        (GNAT.Strings.String_Access
                           ((Get_Construct (Seek_Iterator).Name)),
                         False));
                  end if;

               when Cat_Package .. Cat_Field =>

                  --  If we are on a named construct, check if it's the one
                  --  we are actually looking for

                  if Get_Construct (Seek_Iterator).Name /= null
                    and then Name_Match
                      (Get_Construct (Seek_Iterator))
                  then
                     Add_If_Visible (Seek_Iterator);
                  end if;

                  if Get_Construct (Seek_Iterator).Category = Cat_Package
                    and then Use_Wise
                    and then Get_Construct (Seek_Iterator).Is_Declaration
                  then
                     Handle_Use_For (Seek_Iterator);
                  end if;

               when others =>
                  null;

            end case;

            Prev_Iterator := Prev (Tree, Seek_Iterator, Jump_Over);

            if Get_Parent_Scope (Tree, Seek_Iterator) /=
              Get_Parent_Scope (Tree, Prev_Iterator)
            then
               --  We are about to leave the current scope. First, have a look
               --  at the specification if any.

               Initial_Parent := Get_Parent_Scope (Tree, Seek_Iterator);

               if Ada_Tree (Initial_Parent.Index).Spec_Index /= 0
                 and then Ada_Tree (Initial_Parent.Index).Spec_Index
                   /= Initial_Parent.Index
               then
                  --  There is actually a spec somewhere. Get it and look for
                  --  possible entities in jump_over mode.

                  Initial_Parent :=
                    (Tree.Contents
                       (Ada_Tree (Initial_Parent.Index).Spec_Index),
                     Ada_Tree (Initial_Parent.Index).Spec_Index);

                  Look_In_Package (Initial_Parent, True);
               end if;
            end if;

            Seek_Iterator := Prev_Iterator;
         end loop;

      end if;

      Free (Use_List);

      declare
         Result : Construct_Tree_Iterator_Array
           (1 .. Length (Constructs_Found));

         Node : Construct_Iterator_List_Pckg.List_Node :=
           First (Constructs_Found);
      begin
         for J in 1 .. Length (Constructs_Found) loop
            Result (J) := Data (Node);

            Node := Next (Node);
         end loop;

         Free (Constructs_Found);

         return Result;
      end;
   end Get_Visible_Constructs;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree         : Construct_Tree;
      Ada_Tree     : Ada_Construct_Tree;
      Start_Entity : Construct_Tree_Iterator;
      Id           : Composite_Identifier;
      Use_Wise     : Boolean := True)
      return Construct_Tree_Iterator_Array
   is
      Current_Scope : Construct_Tree_Iterator;

      Visible_Constructs : constant Construct_Tree_Iterator_Array :=
        Get_Visible_Constructs
          (Tree, Ada_Tree, Start_Entity, Get_Item (Id, 1), Use_Wise);
   begin
      if Length (Id) = 1 then
         return Visible_Constructs;
      elsif Visible_Constructs'Length >= 1 then
         Current_Scope := Visible_Constructs (1);
      else
         return Null_Construct_Tree_Iterator_Array;
      end if;

      for J in 2 .. Length (Id) loop
         declare
            Name_Seeked        : constant String := Get_Item (Id, J);
            End_Of_Scope_Index : constant Natural :=
              Current_Scope.Index + Current_Scope.Node.Sub_Nodes_Length;
            End_Of_Scope       : constant Construct_Tree_Iterator :=
              (Tree.Contents (End_Of_Scope_Index), End_Of_Scope_Index);

            Visible_Constructs : constant Construct_Tree_Iterator_Array :=
              Get_Visible_Constructs
                (Tree, Ada_Tree, End_Of_Scope, Name_Seeked, Use_Wise);
         begin
            if J = Length (Id) then
               return Visible_Constructs;
            end if;

            if Visible_Constructs'Length >= 1 then
               --  ??? We do not handle cases where there are serveal
               --  possibilities here. Should we ?
               Current_Scope := Visible_Constructs (1);
            else
               return Null_Construct_Tree_Iterator_Array;
            end if;
         end;
      end loop;

      return Null_Construct_Tree_Iterator_Array;
   end Get_Visible_Constructs;

   ---------------------------
   -- Get_Visible_Construct --
   ---------------------------

   function Get_Visible_Constructs
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Offset   : Natural;
      Id       : Composite_Identifier;
      Use_Wise : Boolean := True)
      return Construct_Tree_Iterator_Array
   is
   begin
      return
        Get_Visible_Constructs
          (Tree,
           Ada_Tree,
           Get_Last_Relevant_Construct (Tree, Offset),
           Id,
           Use_Wise);
   end Get_Visible_Constructs;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Tree : access Ada_Tree_Language) return Language_Access
   is
      pragma Unreferenced (Tree);
   begin
      return Ada_Lang;
   end Get_Language;

   ---------------------
   -- Get_Parent_Tree --
   ---------------------

   function Get_Parent_Tree
     (Lang       : access Ada_Tree_Language;
      Left_Tree  : access Construct_Tree;
      Right_Tree : access Construct_Tree) return Get_Parent_Tree_Result
   is
      Left_It : constant Construct_Tree_Iterator :=
        Get_Unit_Construct (Lang, Left_Tree);
      Right_It  : constant Construct_Tree_Iterator :=
        Get_Unit_Construct (Lang, Right_Tree);

      function Is_Parent_Tree
        (Parent_Id, Child_Id : Composite_Identifier) return Boolean;

      function Is_Parent_Tree
        (Parent_Id, Child_Id : Composite_Identifier) return Boolean is
      begin
         if Length (Child_Id) = Length (Parent_Id) + 1 then
            for J in 1 .. Length (Parent_Id) loop
               if Get_Item (Parent_Id, J) /= Get_Item (Child_Id, J) then
                  return False;
               end if;
            end loop;

            return True;
         end if;

         return False;
      end Is_Parent_Tree;

      Right_Category, Left_Category : Language_Category;

   begin
      if Left_It /= Null_Construct_Tree_Iterator
        and then Right_It /= Null_Construct_Tree_Iterator
      then
         Right_Category := Get_Construct (Right_It).Category;
         Left_Category := Get_Construct (Left_It).Category;

         if Right_Category = Cat_Package
           or else Left_Category = Cat_Package
         then
            begin
               if Left_Category = Cat_Package
                 and then Is_Parent_Tree
                   (Get_Unit_Name (Lang, Left_Tree),
                    Get_Unit_Name (Lang, Right_Tree))
               then
                  return Left;
               elsif Right_Category = Cat_Package
                 and then Is_Parent_Tree
                   (Get_Unit_Name (Lang, Right_Tree),
                    Get_Unit_Name (Lang, Left_Tree))
               then
                  return Right;
               end if;
            end;
         end if;

         if Get_Unit_Name (Lang, Right_Tree)
           = Get_Unit_Name (Lang, Left_Tree)
         then
            if Get_Construct (Right_It).Is_Declaration then
               return Right;
            else
               return Left;
            end if;
         end if;

      end if;

      return None;
   end Get_Parent_Tree;

   ------------------------
   -- Get_Unit_Construct --
   ------------------------

   function Get_Unit_Construct
     (Lang : access Ada_Tree_Language;
      Tree : access Construct_Tree) return Construct_Tree_Iterator
   is
      pragma Unreferenced (Lang);
   begin
      if Tree.Unit_Index = 0 then
         return Null_Construct_Tree_Iterator;
      elsif Tree.Unit_Index /= -1 then
         return (Tree.Contents (Tree.Unit_Index), Tree.Unit_Index);
      else
         declare
            It : Construct_Tree_Iterator := First (Tree.all);
         begin
            while It /= Null_Construct_Tree_Iterator loop
               declare
                  Category : constant Language_Category :=
                    Get_Construct (It).Category;
               begin
                  if Category = Cat_Package
                    or else Category = Cat_Function
                    or else Category = Cat_Procedure
                  then
                     Tree.Unit_Index := It.Index;

                     return It;
                  end if;
               end;

               It := Next (Tree.all, It, Jump_Over);
            end loop;
         end;
      end if;

      Tree.Unit_Index := 0;

      return Null_Construct_Tree_Iterator;
   end Get_Unit_Construct;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name
     (Lang : access Ada_Tree_Language;
      Tree : access Construct_Tree) return Composite_Identifier is
   begin
      if Tree.Unit_Name = null then
         if Get_Unit_Construct (Lang, Tree)
           /= Null_Construct_Tree_Iterator
         then
            Tree.Unit_Name := new Composite_Identifier'
              (To_Composite_Identifier
                 (To_Lower
                    (Get_Construct
                       (Get_Unit_Construct (Lang, Tree)).Name.all)));
         else
            Tree.Unit_Name := new Composite_Identifier'
              (To_Composite_Identifier (""));
         end if;
      end if;

      return Tree.Unit_Name.all;
   end Get_Unit_Name;

   --------------------
   -- Get_Name_Index --
   --------------------

   function Get_Name_Index
     (Lang      : access Ada_Tree_Language;
      Construct : Simple_Construct_Information) return String
   is
      pragma Unreferenced (Lang);
   begin
      if Construct.Category = Cat_Package then
         declare
            Id : constant Composite_Identifier :=
              To_Composite_Identifier (Construct.Name.all);
         begin
            return To_Lower (Get_Item (Id, Length (Id)));
         end;
      else
         return To_Lower (Construct.Name.all);
      end if;
   end Get_Name_Index;

   ---------------------
   -- Get_Public_Tree --
   ---------------------

   function Get_Public_Tree
     (Lang      : access Ada_Tree_Language;
      Full_Tree : access Construct_Tree;
      Free_Tree : Boolean)
      return Construct_Tree
   is
      pragma Unreferenced (Lang);

      New_Tree : Construct_Tree (Full_Tree.Length);

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         New_Tree_Index  : in out Natural;
         Parameters_Only : Boolean := False);

      ---------------
      -- Add_Scope --
      ---------------

      procedure Add_Scope
        (Scope           : Construct_Tree_Iterator;
         New_Tree_Index  : in out Natural;
         Parameters_Only : Boolean := False)
      is
         It             : Construct_Tree_Iterator;
         Previous_Index : Natural := 0;
         Initial_Index  : constant Natural := New_Tree_Index;
      begin
         if Scope = Null_Construct_Tree_Iterator then
            It := First (Full_Tree.all);
         else
            It := Next (Full_Tree.all, Scope, Jump_Into);
         end if;

         if Get_Construct (Scope).Category = Cat_Package then
            if not Get_Construct (Scope).Is_Declaration then
               return;
            end if;
         end if;

         while It /= Null_Construct_Tree_Iterator
           and then Get_Parent_Scope (Full_Tree.all, It).Index = Scope.Index
         loop
            if Parameters_Only and then
              Get_Construct (It).Category /= Cat_Parameter
            then
               return;
            end if;

            if Get_Construct (It).Visibility = Visibility_Public then
               New_Tree.Contents (New_Tree_Index).Construct :=
                 It.Node.Construct;

               if Free_Tree then
                  --  If we are going to free the old tree, then we don't need
                  --  its name anymore.

                  Full_Tree.Contents (It.Index).Construct.Name := null;
               else
                  --  If we are not going to free the old tree, then we don't
                  --  want the two pointers to be on the same object, we have
                  --  to duplicate the name

                  New_Tree.Contents (New_Tree_Index).Construct.Name :=
                    new String'(It.Node.Construct.Name.all);
               end if;

               New_Tree.Contents (New_Tree_Index).Previous_Sibling_Index :=
                 Previous_Index;
               New_Tree.Contents (New_Tree_Index).Sub_Nodes_Length := 0;
               New_Tree.Contents (New_Tree_Index).Parent_Index :=
                 Initial_Index - 1;

               Previous_Index := New_Tree_Index;
               New_Tree_Index := New_Tree_Index + 1;

               if not Parameters_Only then
                  if Get_Construct (It).Category = Cat_Package
                    or else Get_Construct (It).Category
                      in Cat_Class .. Cat_Type
                  then
                     Add_Scope (It, New_Tree_Index);
                  elsif Get_Construct (It).Category
                      in Cat_Task .. Cat_Entry
                  then
                     Add_Scope (It, New_Tree_Index, True);
                  end if;
               end if;
            end if;

            It := Next (Full_Tree.all, It, Jump_Over);
         end loop;

         if Scope /= Null_Construct_Tree_Iterator then
            New_Tree.Contents (Initial_Index - 1).Sub_Nodes_Length :=
              New_Tree_Index - Initial_Index;
         end if;
      end Add_Scope;

      Index : Natural := 1;
   begin
      if Full_Tree.Contents'Length = 0 then
         return Full_Tree.all;
      end if;

      Add_Scope (Null_Construct_Tree_Iterator, Index);

      if Free_Tree then
         Free (Full_Tree.all);
      end if;

      return
        (Length      => Index - 1,
         Contents    => New_Tree.Contents (1 .. Index - 1),
         others => <>);
   end Get_Public_Tree;

end Language.Tree.Ada;
