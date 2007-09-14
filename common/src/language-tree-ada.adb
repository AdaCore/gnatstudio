-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2006-2007, AdaCore               --
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

with Glib.Convert; use Glib.Convert;

with Ada.Strings.Unbounded; use Ada.Strings;
with Ada.Unchecked_Deallocation; use Ada;

with Generic_List;
with String_Utils;      use String_Utils;

with Ada.Characters.Handling; use Ada.Characters.Handling;

with Language.Ada;           use Language.Ada;
with Language.Documentation; use Language.Documentation;

package body Language.Tree.Ada is

   use type GNAT.Strings.String_Access;

   ----------
   -- Free --
   ----------

   procedure Free
     (Tree : Construct_Tree; Ada_Tree : in out Ada_Construct_Tree)
   is
      procedure Internal is new Unchecked_Deallocation
        (Ada_Construct_Tree_Array, Ada_Construct_Tree);
      procedure Internal is new Unchecked_Deallocation
        (Ada_Construct_Relation, Relation_Ptr);

      Garbage : Relation_Ptr;
   begin
      if Ada_Tree = null then
         return;
      end if;

      for J in Ada_Tree'Range loop
         if Ada_Tree (J).Ada_Relation /= null then
            Garbage := Ada_Tree (J).Ada_Relation;

            if Garbage.Spec.Tree = Tree
              and then Garbage.Spec.Index = J
            then
               Garbage.Spec.Tree := null;
               Garbage.Spec.Index := 0;
            end if;

            if Garbage.First_Body.Tree = Tree
              and then Garbage.First_Body.Index = J
            then
               Garbage.First_Body.Tree := null;
               Garbage.First_Body.Index := 0;
            end if;

            if Garbage.Second_Body.Tree = Tree
              and then Garbage.Second_Body.Index = J
            then
               Garbage.Second_Body.Tree := null;
               Garbage.Second_Body.Index := 0;
            end if;

            if Garbage.Spec.Index = 0
              and then Garbage.First_Body.Index = 0
              and then Garbage.Second_Body.Index = 0
            then
               Internal (Garbage);
            end if;
         end if;
      end loop;

      Internal (Ada_Tree);
   end Free;

   ---------------------------------
   -- Generate_Ada_Construct_Tree --
   ---------------------------------

   function Generate_Ada_Construct_Tree
     (Tree     : Construct_Tree;
      Buffer   : String_Access) return Ada_Construct_Tree
   is
      Result, Dummy : Ada_Construct_Tree;
   begin
      Generate_Ada_Construct_Trees (Tree, null, Buffer, null, Result, Dummy);

      return Result;
   end Generate_Ada_Construct_Tree;

   ----------------------------------
   -- Generate_Ada_Construct_Trees --
   ----------------------------------

   procedure Generate_Ada_Construct_Trees
     (Spec_Tree, Body_Tree         : Construct_Tree;
      Spec_Buffer, Body_Buffer     : String_Access;
      Spec_Ada_Tree, Body_Ada_Tree : out Ada_Construct_Tree)
   is
      type Simple_Construct_Wrapper is record
         Tree     : Construct_Tree;
         Ada_Tree : Ada_Construct_Tree;
         Buffer   : String_Access;
         Index    : Integer;
      end record;

      type Simple_Construct_Array is array
        (Integer range <>) of Simple_Construct_Wrapper;

      type Simple_Construct_Array_Ptr is access all Simple_Construct_Array;

      procedure Free is new Unchecked_Deallocation
        (Simple_Construct_Array, Simple_Construct_Array_Ptr);

      procedure Insert_At
        (Dic_Index  : Natural;
         Tree       : Construct_Tree;
         Ada_Tree   : Ada_Construct_Tree;
         Buffer     : String_Access;
         Tree_Index : Natural);
      --  Insert the wrapper {Tree, Ada_Tree, Buffer, Tree_Index} in the
      --  dictionnary at the position Dic_index.

      Dictionnary : Simple_Construct_Array_Ptr;

      Dictionnary_Length : Integer := 0;

      procedure Process_Tree
        (Tree     : Construct_Tree;
         Ada_Tree : Ada_Construct_Tree;
         Buffer   : String_Access);
      --  Analyzes the tree, and set the relevant data in the dictionnary and
      --  in the Ada_Tree.

      procedure Add_In_Dictionnary
        (Tree     : Construct_Tree;
         Ada_Tree : Ada_Construct_Tree;
         Buffer   : String_Access;
         Index    : Natural);
      --  Adds the given element {Tree, Ada_Tree, Buffer, Index} in the
      --  dictionnary. Set the relevant data in the Ada_Tree if needed.

      procedure Establish_Relation (Left, Right : Simple_Construct_Wrapper);
      --  Establish the Spec, First_Body or Second_Body relationship between
      --  the two elements.

      ---------------
      -- Insert_At --
      ---------------

      procedure Insert_At
        (Dic_Index  : Natural;
         Tree       : Construct_Tree;
         Ada_Tree   : Ada_Construct_Tree;
         Buffer     : String_Access;
         Tree_Index : Natural) is
      begin
         Dictionnary_Length := Dictionnary_Length + 1;

         for J in reverse Dic_Index  + 1 .. Dictionnary_Length loop
            Dictionnary (J) := Dictionnary (J - 1);
         end loop;

         Dictionnary (Dic_Index) := (Tree, Ada_Tree, Buffer, Tree_Index);
      end Insert_At;

      ------------------
      -- Process_Tree --
      ------------------

      procedure Process_Tree
        (Tree     : Construct_Tree;
         Ada_Tree : Ada_Construct_Tree;
         Buffer   : String_Access) is
      begin
         for J in Tree.Contents'Range loop
            case Tree.Contents (J).Construct.Category is
               when Cat_Package .. Cat_Subtype | Cat_Variable =>
                  Add_In_Dictionnary (Tree, Ada_Tree, Buffer, J);
               when others =>
                  null;
            end case;
         end loop;
      end Process_Tree;

      ----------------------
      -- Add_In_Construct --
      ----------------------

      procedure Add_In_Dictionnary
        (Tree     : Construct_Tree;
         Ada_Tree : Ada_Construct_Tree;
         Buffer   : String_Access;
         Index    : Natural)
      is
         Looked_Index, Looked_Range      : Integer;
         New_Iterator,  Looked_Iterator  : Construct_Tree_Iterator;
      begin
         if Dictionnary_Length = 0 then
            Dictionnary (1) := (Tree, Ada_Tree, Buffer, Index);
            Dictionnary_Length := 1;

            return;
         else
            Looked_Range := Dictionnary_Length;

            Looked_Index := Looked_Range / 2 + (Looked_Range mod 2);

            New_Iterator := (Tree.Contents (Index), Index);

            loop
               Looked_Iterator :=
                 (Dictionnary (Looked_Index).Tree.Contents
                  (Dictionnary (Looked_Index).Index),
                  Dictionnary (Looked_Index).Index);

               case Compare_Entities
                 (Ada_Tree_Lang,
                  New_Iterator, Looked_Iterator,
                  Tree, Dictionnary (Looked_Index).Tree,
                  Buffer, Dictionnary (Looked_Index).Buffer)
               is
                  when Equals | Equivalent =>
                     Establish_Relation
                       (Dictionnary (Looked_Index),
                        (Tree, Ada_Tree, Buffer, Index));

                     exit;
                  when Greater_Than =>
                     if Looked_Range = 0 then
                        Insert_At
                          (Looked_Index + 1, Tree, Ada_Tree, Buffer, Index);
                        exit;
                     else
                        Looked_Range := Looked_Range / 2;

                        Looked_Index := Looked_Index +
                          Looked_Range / 2 + (Looked_Range mod 2);
                     end if;
                  when Lower_Than =>
                     if Looked_Range = 0 then
                        Insert_At
                          (Looked_Index, Tree, Ada_Tree, Buffer, Index);
                        exit;
                     else
                        Looked_Range := Looked_Range / 2
                          - (1 - Looked_Range mod 2);

                        if Looked_Range > 0 then
                           Looked_Index :=
                             Looked_Index - (Looked_Range / 2 + 1);
                        end if;
                     end if;
               end case;
            end loop;
         end if;
      end Add_In_Dictionnary;

      ------------------------
      -- Establish_Relation --
      ------------------------

      procedure Establish_Relation (Left, Right : Simple_Construct_Wrapper) is
         Relation : Relation_Ptr;
      begin
         if Left.Ada_Tree (Left.Index).Ada_Relation /= null then
            Relation := Left.Ada_Tree (Left.Index).Ada_Relation;

            Relation.Second_Body := (Right.Tree, Right.Index);
         else
            Relation := new Ada_Construct_Relation;
            Left.Ada_Tree (Left.Index).Ada_Relation := Relation;

            Relation.Spec := (Left.Tree, Left.Index);

            Relation.First_Body := (Right.Tree, Right.Index);
         end if;

         Right.Ada_Tree (Right.Index).Ada_Relation := Relation;
      end Establish_Relation;

   begin
      if Body_Tree /= null then
         Dictionnary := new Simple_Construct_Array
           (1 .. Spec_Tree.Contents'Length + Body_Tree.Contents'Length);
         Body_Ada_Tree :=
           new Ada_Construct_Tree_Array (Body_Tree.Contents'Range);
      else
         Dictionnary := new Simple_Construct_Array
           (1 .. Spec_Tree.Contents'Length);
      end if;

      Spec_Ada_Tree := new Ada_Construct_Tree_Array (Spec_Tree.Contents'Range);

      Process_Tree (Spec_Tree, Spec_Ada_Tree, Spec_Buffer);

      if Body_Tree /= null then
         Process_Tree (Body_Tree, Body_Ada_Tree, Body_Buffer);
      end if;

      Free (Dictionnary);
   end Generate_Ada_Construct_Trees;

   --------------
   -- Get_Spec --
   --------------

   function Get_Spec
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator)
      return Construct_Cell_Access
   is
   begin
      if Ada_Tree (Iter.Index).Ada_Relation = null then
         return (Tree, Iter.Index);
      else
         return Ada_Tree (Iter.Index).Ada_Relation.Spec;
      end if;
   end Get_Spec;

   --------------------
   -- Get_First_Body --
   --------------------

   function Get_First_Body
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator)
      return Construct_Cell_Access
   is
   begin
      if Ada_Tree (Iter.Index).Ada_Relation = null then
         return (Tree, Iter.Index);
      else
         return Ada_Tree (Iter.Index).Ada_Relation.First_Body;
      end if;
   end Get_First_Body;

   ---------------------
   -- Get_Second_Body --
   ---------------------

   function Get_Second_Body
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator)
      return Construct_Cell_Access
   is
   begin
      if Ada_Tree (Iter.Index).Ada_Relation = null
        or else Ada_Tree (Iter.Index).Ada_Relation.Second_Body
        = Null_Construct_Cell_Access
      then
         return (Tree, Iter.Index);
      else
         return Ada_Tree (Iter.Index).Ada_Relation.Second_Body;
      end if;
   end Get_Second_Body;

   ----------------------------
   -- Get_Most_Complete_View --
   ----------------------------

   function Get_Most_Complete_View
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Construct_Cell_Access is
   begin
      if Ada_Tree (Iter.Index).Ada_Relation = null then
         return (Tree, Iter.Index);
      elsif Ada_Tree (Iter.Index).Ada_Relation.Second_Body.Index /= 0 then
         return Ada_Tree (Iter.Index).Ada_Relation.Second_Body;
      else
         return Ada_Tree (Iter.Index).Ada_Relation.First_Body;
      end if;
   end Get_Most_Complete_View;

   ---------------------------
   -- Is_Most_Complete_View --
   ---------------------------

   function Is_Most_Complete_View
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Boolean is
   begin
      if Ada_Tree (Iter.Index).Ada_Relation = null then
         return True;
      elsif Ada_Tree (Iter.Index).Ada_Relation.Spec.Index = Iter.Index
        and then Ada_Tree (Iter.Index).Ada_Relation.Spec.Tree = Tree
      then
         return Ada_Tree (Iter.Index).Ada_Relation.First_Body.Index = 0;
      elsif Ada_Tree (Iter.Index).Ada_Relation.First_Body.Index = Iter.Index
        and then Ada_Tree (Iter.Index).Ada_Relation.First_Body.Tree = Tree
      then
         return Ada_Tree (Iter.Index).Ada_Relation.Second_Body.Index = 0;
      else
         return True;
      end if;
   end Is_Most_Complete_View;

   ------------------------
   -- Is_First_Occurence --
   ------------------------

   function Is_First_Occurence
     (Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Iter     : Construct_Tree_Iterator) return Boolean
   is
   begin
      return Ada_Tree (Iter.Index).Ada_Relation = null
        or else
          (Ada_Tree (Iter.Index).Ada_Relation.Spec.Index = Iter.Index
           and then Ada_Tree (Iter.Index).Ada_Relation.Spec.Tree = Tree);
   end Is_First_Occurence;

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
      --  enclosed or enclosing scopes. When the seeked entity is found, it is
      --  added to the list if relevant.

      function Is_Visible (It : Construct_Tree_Iterator) return Boolean;
      --  Return True if the object at the iterator position is visible.

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

      procedure Handle_Enumeration (Enum : Construct_Tree_Iterator);
      --  Handle the contents of an enumeration.

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
              and then Is_Visible (It)
            then
               Append (Constructs_Found, It);
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

      function Is_Visible (It : Construct_Tree_Iterator) return Boolean is
         Node : Construct_Iterator_List_Pckg.List_Node :=
           First (Constructs_Found);
      begin
         while Node /= Construct_Iterator_List_Pckg.Null_Node loop
            if Get_Construct (It).Name.all
              = Get_Construct (Data (Node)).Name.all
            then
               --  If we found the spec of an entity of wich we already have
               --  the body, then replace the body by the spec.

               if To_Construct_Tree_Iterator
                 (Get_Spec (Tree, Ada_Tree, Data (Node))) = It
               then
                  Set_Data (Node, It);
                  return False;
               end if;

               --  If we found an other node wich is not a subprogram, then the
               --  one we found is not visible.

               if Get_Construct (Data (Node)).Category not in
                 Subprogram_Category
               then
                  return False;
               end if;
            end if;

            Node := Next (Node);
         end loop;

         return True;
      end Is_Visible;

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

      ------------------------
      -- Handle_Enumeration --
      ------------------------

      procedure Handle_Enumeration (Enum : Construct_Tree_Iterator) is
         Child : Construct_Tree_Iterator := Next (Tree, Enum, Jump_Into);
      begin
         while Get_Parent_Scope (Tree, Child) = Enum loop
            if Name_Match (Get_Construct (Child)) then
               Append (Constructs_Found, Child);
            end if;

            Child := Next (Tree, Child, Jump_Over);
         end loop;
      end Handle_Enumeration;

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

                  if Is_Visible (Seek_Iterator) then
                     if Get_Construct (Seek_Iterator).Category = Cat_Type
                       and then Is_Enum_Type (Tree, Seek_Iterator)
                     then
                        Handle_Enumeration (Seek_Iterator);
                     end if;

                     --  If we are on a named construct, check if it's the one
                     --  we are actually looking for

                     if Get_Construct (Seek_Iterator).Name /= null
                       and then Name_Match (Get_Construct (Seek_Iterator))
                     then
                        Append (Constructs_Found, Seek_Iterator);
                     end if;
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

               if Initial_Parent /= Null_Construct_Tree_Iterator
                 and then Ada_Tree (Initial_Parent.Index).Ada_Relation /= null
                 and then
                   To_Construct_Tree_Iterator
                     (Ada_Tree (Initial_Parent.Index).Ada_Relation.Spec)
                 /= Initial_Parent
                 and then Ada_Tree
                   (Initial_Parent.Index).Ada_Relation.Spec.Tree = Tree
               --  ??? with the statment above, we explicitely say that
               --  we don't want to consider specs coming from possible foreign
               --  trees referenced in the Ada_Tree given in parameter (most
               --  commonly, the spec of a body or vice versa). Insead, we
               --  could want to handle both, the body and the spec, in this
               --  subprogram.
               then
                  --  There is actually a spec somewhere. Get it and look for
                  --  possible entities in jump_over mode.

                  Initial_Parent :=
                    To_Construct_Tree_Iterator
                     (Ada_Tree (Initial_Parent.Index).Ada_Relation.Spec);

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

   ----------------
   -- Is_Visible --
   ----------------

   function Is_Visible
     (Db       : access Construct_Database;
      Cell     : Construct_Cell_Access;
      Tree     : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;
      Offset   : Natural) return Boolean
   is
      pragma Unreferenced (Db);

      Local_Visible_Entities : constant Construct_Tree_Iterator_Array :=
        Get_Visible_Constructs
          (Tree       => Tree,
           Ada_Tree   => Ada_Tree,
           Offset     => Offset,
           Name       => Get_Construct
             (To_Construct_Tree_Iterator (Cell)).Name.all,
           Use_Wise   => True,
           Is_Partial => False);
   begin
      for J in Local_Visible_Entities'Range loop
         --  If we found the entity, then return it.

         if Is_Same_Construct
           (Cell, To_Construct_Access (Tree, Local_Visible_Entities (J)))
         then
            return True;
         end if;

         if Local_Visible_Entities (J).Node.Construct.Category
         in Data_Category
         then
            --  In this case, the data is hidden by a former declaration.
            return False;
         end if;
      end loop;

      --  If the construct has not been found, then it's not in the visible
      --  contents of the current file. Checks if the construct is either
      --  in a public library part, or in a private visible library part.

      declare
         Previous_It : Construct_Tree_Iterator :=
           To_Construct_Tree_Iterator (Cell);

         It : Construct_Tree_Iterator :=
           Get_Parent_Scope
             (Get_Tree (Cell), To_Construct_Tree_Iterator (Cell));

         Loc : constant Text_Location := (True, Offset);

         Tested_Scope : constant Construct_Tree_Iterator := Get_Iterator_At
           (Tree              => Tree,
            Location          => Loc,
            From_Type         => Start_Construct,
            Position          => Enclosing,
            Categories_Seeked => Null_Category_Array);
      begin
         while It /= Null_Construct_Tree_Iterator loop
            if Get_Construct (It).Category /= Cat_Package
              or else not Get_Construct (It).Is_Declaration
            then
               return False;
            end if;

            if Get_Construct (Previous_It).Visibility /= Visibility_Public
              and then not
                Has_Full_Visibility
                  ((Tree, Tested_Scope.Index), (Get_Tree (Cell), It.Index))
            then
               return False;
            end if;

            Previous_It := It;
            It := Get_Parent_Scope (Get_Tree (Cell), It);
         end loop;

         return True;
      end;

      --  ??? We could also check visibility against with / use clauses.
   end Is_Visible;

   -------------------------
   -- Has_Full_Visibility --
   -------------------------

   function Has_Full_Visibility
     (Construct : Construct_Cell_Access; Scope : Construct_Cell_Access)
      return Boolean
   is
      Scope_Path : constant Construct_Tree_Iterator_Array :=
        Full_Construct_Path (Scope);
      Construct_Path : constant Construct_Tree_Iterator_Array :=
        Full_Construct_Path (Construct);
   begin
      if Scope_Path'Length > Construct_Path'Length then
         return False;
      end if;

      for J in Scope_Path'Range loop
         if Get_Construct (Scope_Path (J)).Category /=
           Get_Construct (Construct_Path (J)).Category
           or else Get_Construct (Scope_Path (J)).Name = null
           or else Get_Construct (Construct_Path (J)).Name = null
           or else Get_Construct (Scope_Path (J)).Name.all
           /= Get_Construct (Construct_Path (J)).Name.all
         then
            return False;
         end if;
      end loop;

      return True;
   end Has_Full_Visibility;

   ------------------
   -- Is_Enum_Type --
   ------------------

   function Is_Enum_Type
     (Tree : Construct_Tree;
      It   : Construct_Tree_Iterator) return Boolean
   is
      First_Child : constant Construct_Tree_Iterator :=
        Next (Tree, It, Jump_Into);
   begin
      return Get_Construct (It).Category = Cat_Type
        and then Get_Construct (First_Child).Category = Cat_Literal;
   end Is_Enum_Type;

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
      Left_Tree  : Construct_Tree;
      Right_Tree : Construct_Tree) return Get_Parent_Tree_Result
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
      if Left_It = Right_It then
         return None;
      elsif Left_It /= Null_Construct_Tree_Iterator
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
      Tree : Construct_Tree) return Construct_Tree_Iterator
   is
      pragma Unreferenced (Lang);
   begin
      if Tree.Unit_Index = 0 then
         return Null_Construct_Tree_Iterator;
      elsif Tree.Unit_Index /= -1 then
         return (Tree.Contents (Tree.Unit_Index), Tree.Unit_Index);
      else
         declare
            It : Construct_Tree_Iterator := First (Tree);
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

               It := Next (Tree, It, Jump_Over);
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
      Tree : Construct_Tree) return Composite_Identifier is
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
      if Construct.Name = null then
         return "";
      end if;

      if Construct.Category = Cat_Package
        or else Construct.Category = Cat_Procedure
        or else Construct.Category = Cat_Function
      then
         --  If the construct may be a unit name, then we want to store only it
         --  last item (e.g. in Pckg.Child, we store only Child in the db).

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

   ----------------------
   -- Compare_Entities --
   ----------------------

   function Compare_Entities
     (Lang                      : access Ada_Tree_Language;
      Left_Iter, Right_Iter     : Construct_Tree_Iterator;
      Left_Tree, Right_Tree     : Construct_Tree;
      Left_Buffer, Right_Buffer : String_Access) return General_Order
   is
      pragma Unreferenced (Lang);

      function Check_Lowercase_Names
        (Left, Right : GNAT.Strings.String_Access) return General_Order;

      function Check_Lowercase_Names
        (Left, Right : String) return General_Order;

      function Check_Referenced_Entitites
        (Left, Right : Simple_Construct_Information) return General_Order;

      ---------------------------
      -- Check_Lowercase_Names --
      ---------------------------

      function Check_Lowercase_Names
        (Left, Right : GNAT.Strings.String_Access) return General_Order
      is
      begin
         if Left = null then
            if Right = null then
               return Equals;
            else
               return Greater_Than;
            end if;
         elsif Right = null then
            return Lower_Than;
         end if;

         return Check_Lowercase_Names (Left.all, Right.all);
      end Check_Lowercase_Names;

      function Check_Lowercase_Names
        (Left, Right : String) return General_Order
      is
         Smaller_Size            : Integer;
         Left_Index, Right_Index : Integer;
      begin
         if Left'Length < Right'Length then
            Smaller_Size := Left'Length;
         else
            Smaller_Size := Right'Length;
         end if;

         Left_Index := Left'First;
         Right_Index := Right'First;

         for J in 1 .. Smaller_Size loop
            declare
               Left_C, Right_C : Character;
            begin
               Left_C := To_Lower (Left (Left_Index));
               Right_C := To_Lower (Right (Right_Index));

               if Left_C < Right_C then
                  return Lower_Than;
               elsif Left_C > Right_C then
                  return Greater_Than;
               end if;
            end;

            Left_Index := Left_Index + 1;
            Right_Index := Right_Index + 1;
         end loop;

         if Left'Length < Right'Length then
            return Lower_Than;
         elsif Left'Length > Right'Length then
            return Greater_Than;
         else
            return Equals;
         end if;
      end Check_Lowercase_Names;

      -------------------------------
      -- Check_Referenced_Entities --
      -------------------------------

      function Check_Referenced_Entitites
        (Left, Right : Simple_Construct_Information) return General_Order
      is
         Left_Sloc_Start, Left_Sloc_End,
         Right_Sloc_Start, Right_Sloc_End : Source_Location;

         Left_Success, Right_Success      : Boolean;
      begin
         Get_Referenced_Entity
           (Ada_Lang,
            Left_Buffer.all,
            Left,
            Left_Sloc_Start,
            Left_Sloc_End,
            Left_Success);

         Get_Referenced_Entity
           (Ada_Lang,
            Right_Buffer.all,
            Right,
            Right_Sloc_Start,
            Right_Sloc_End,
            Right_Success);

         if not Left_Success then
            if not Right_Success then
               return Equals;
            else
               return Lower_Than;
            end if;
         elsif not Right_Success then
            return Greater_Than;
         end if;

         return Check_Lowercase_Names
           (Left_Buffer (Left_Sloc_Start.Index .. Left_Sloc_End.Index),
            Right_Buffer (Right_Sloc_Start.Index .. Right_Sloc_End.Index));
      end Check_Referenced_Entitites;

      Left_Category, Right_Category : Language_Category;

   begin
      --  First, checks the categories

      Left_Category := Left_Iter.Node.Construct.Category;
      Right_Category := Right_Iter.Node.Construct.Category;

      if Left_Category in Type_Category then
         Left_Category := Cat_Class;
      end if;

      if Right_Category in Type_Category then
         Right_Category := Cat_Class;
      end if;

      if Left_Category < Right_Category then
         return Lower_Than;
      elsif Left_Category > Right_Category then
         return Greater_Than;
      end if;

      --  Second, check the names of the identifiers

      declare
         Tmp_Result : constant General_Order := Check_Lowercase_Names
           (Left_Iter.Node.Construct.Name, Right_Iter.Node.Construct.Name);
      begin
         if Tmp_Result /= Equals then
            return Tmp_Result;
         end if;
      end;

      --  Third, check the scopes

      declare
         Left_Parent, Right_Parent : Construct_Tree_Iterator;
         Nb_Parents                : Integer := 0;
      begin
         Left_Parent := Left_Iter;
         Right_Parent := Right_Iter;

         --  First check the number of scopes

         loop
            Left_Parent := Get_Parent_Scope (Left_Tree, Left_Parent);
            Right_Parent := Get_Parent_Scope (Right_Tree, Right_Parent);

            if Left_Parent = Null_Construct_Tree_Iterator then
               if Right_Parent = Null_Construct_Tree_Iterator then
                  exit;
               else
                  return Lower_Than;
               end if;
            elsif Right_Parent = Null_Construct_Tree_Iterator then
               return Greater_Than;
            end if;

            Nb_Parents := Nb_Parents + 1;
         end loop;

         --  Second check the name of the scopes

         declare
            Left_Scopes, Right_Scopes : array
              (1 .. Nb_Parents) of Construct_Tree_Iterator;

            Tmp_Result                : General_Order;
         begin
            Left_Parent := Left_Iter;
            Right_Parent := Right_Iter;

            for J in reverse 1 .. Nb_Parents loop
               Left_Parent := Get_Parent_Scope (Left_Tree, Left_Parent);
               Right_Parent := Get_Parent_Scope (Right_Tree, Right_Parent);

               Left_Scopes (J) := Left_Parent;
               Right_Scopes (J) := Right_Parent;
            end loop;

            for J in 1 .. Nb_Parents loop
               Tmp_Result := Check_Lowercase_Names
                 (Left_Scopes (J).Node.Construct.Name,
                  Right_Scopes (J).Node.Construct.Name);

               if Tmp_Result /= Equals then
                  return Tmp_Result;
               end if;
            end loop;
         end;
      end;

      --  Fourth, check the profiles

      if Left_Category not in Subprogram_Category then
         --  If we are not working on subprogram, then nothing has to be
         --  cheked here

         return Equals;
      end if;

      declare
         Left_Param, Right_Param : Construct_Tree_Iterator;
         Tmp_Result              : General_Order;
      begin
         --  First check the number of parameters

         Left_Param := Next (Left_Tree, Left_Iter, Jump_Into);
         Right_Param := Next (Right_Tree, Right_Iter, Jump_Into);

         loop
            if Get_Parent_Scope (Left_Tree, Left_Param) /= Left_Iter
              or else Left_Param.Node.Construct.Category /= Cat_Parameter
            then
               if Get_Parent_Scope (Right_Tree, Right_Iter) /= Right_Iter
                 or else Right_Param.Node.Construct.Category
                   /= Cat_Parameter
               then
                  exit;
               else
                  return Lower_Than;
               end if;
            elsif Get_Parent_Scope (Right_Tree, Right_Param) /= Right_Iter
              or else Left_Param.Node.Construct.Category /= Cat_Parameter
            then
               return Greater_Than;
            end if;

            Left_Param := Next (Left_Tree, Left_Param, Jump_Over);
            Right_Param := Next (Right_Tree, Right_Param, Jump_Over);
         end loop;

         --  Then check the actual names and types

         Left_Param := Next (Left_Tree, Left_Iter, Jump_Into);
         Right_Param := Next (Right_Tree, Right_Iter, Jump_Into);

         loop
            exit when Get_Parent_Scope (Left_Tree, Left_Param) /= Left_Iter
              or else Left_Param.Node.Construct.Category /= Cat_Parameter;

            --  Checks the parameter names

            Tmp_Result := Check_Lowercase_Names
              (Left_Param.Node.Construct.Name.all,
               Right_Param.Node.Construct.Name.all);

            if Tmp_Result /= Equals then
               return Tmp_Result;
            end if;

            Tmp_Result := Check_Referenced_Entitites
              (Left_Param.Node.Construct, Right_Param.Node.Construct);

            if Tmp_Result /= Equals then
               return Tmp_Result;
            end if;

            Left_Param := Next (Left_Tree, Left_Param, Jump_Over);
            Right_Param := Next (Right_Tree, Right_Param, Jump_Over);
         end loop;

         if Left_Category = Cat_Function then
            Tmp_Result := Check_Referenced_Entitites
              (Left_Iter.Node.Construct, Right_Iter.Node.Construct);

            if Tmp_Result /= Equals then
               return Tmp_Result;
            end if;
         end if;
      end;

      --  ??? Handle the Equivalent case as well (iterators points to different
      --  parts of the same entity.

      return Equals;
   end Compare_Entities;

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

      New_Tree : Construct_Tree := new Construct_Tree_Record
        (Full_Tree.all.Contents'Length);

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

                  Full_Tree.all.Contents (It.Index).Construct.Name := null;
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
      if Full_Tree.all.Contents'Length = 0 then
         return new Construct_Tree_Record'(Full_Tree.all.all);
      end if;

      Add_Scope (Null_Construct_Tree_Iterator, Index);

      if Free_Tree then
         Free (Full_Tree.all);
      end if;

      declare
         Returned_Tree : constant Construct_Tree := new Construct_Tree_Record'
           (Length      => Index - 1,
            Contents    => New_Tree.Contents (1 .. Index - 1),
            others => <>);
      begin
         --  Aggregates are expanded into a copy on the stack, that's why we
         --  have to do the manual copy here. Otherwise, we get a SEGV here
         --  when calling this from a GNATbench thread.
         for J in New_Tree.Contents'Range loop
            New_Tree.Contents (J) := Null_Construct_Tree_Node;
         end loop;

         Free (New_Tree);
         return Returned_Tree;
      end;
   end Get_Public_Tree;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Lang   : access Ada_Tree_Language;
      Buffer : String;
      Tree   : Construct_Tree;
      Node   : Construct_Tree_Iterator) return String
   is
      Beginning, Current   : Natural;
      Result               : Unbounded.Unbounded_String;

      Type_Start, Type_End : Source_Location;
      Success              : Boolean;
      Language             : constant Language_Access :=
        Get_Language (Tree_Language'Class (Lang.all)'Access);

      function Attribute_Decoration
        (Construct  : Simple_Construct_Information;
         Default_In : Boolean) return String;

      function Get_Default_Value
        (Construct  : Simple_Construct_Information;
         Max_Length : Integer := 30) return String;

      function Attribute_Decoration
        (Construct  : Simple_Construct_Information;
         Default_In : Boolean) return String
      is
         Buffer : String (1 .. 30);
         Ind    : Integer := 1;
      begin
         if Construct.Attributes (Ada_In_Attribute)
           or else
             (Default_In and then not
                  (Construct.Attributes (Ada_Out_Attribute)
                   or else Construct.Attributes (Ada_Access_Attribute)))
         then
            Buffer (Ind .. Ind + 2) := "in ";
            Ind := Ind + 3;
         end if;

         if Construct.Attributes (Ada_Out_Attribute) then
            Buffer (Ind .. Ind + 3) := "out ";
            Ind := Ind + 4;
         end if;

         if Construct.Attributes (Ada_Not_Attribute) then
            Buffer (Ind .. Ind + 3) := "not ";
            Ind := Ind + 4;
         end if;

         if Construct.Attributes (Ada_Null_Attribute) then
            Buffer (Ind .. Ind + 4) := "null ";
            Ind := Ind + 5;
         end if;

         if Construct.Attributes (Ada_Access_Attribute) then
            Buffer (Ind .. Ind + 6) := "access ";
            Ind := Ind + 7;
         end if;

         if Construct.Attributes (Ada_Constant_Attribute) then
            Buffer (Ind .. Ind + 8) := "constant ";
            Ind := Ind + 9;
         end if;

         if Construct.Attributes (Ada_Aliased_Attribute) then
            Buffer (Ind .. Ind + 7) := "aliased ";
            Ind := Ind + 8;
         end if;

         return Buffer (1 .. Ind - 1);
      end Attribute_Decoration;

      ------------------------
      --  Get_Default_Value --
      ------------------------

      function Get_Default_Value
        (Construct  : Simple_Construct_Information;
         Max_Length : Integer := 30) return String
      is
         Result        : String (1 .. Max_Length);
         Current_Ind   : Integer := 0;
         Extract_Value : Boolean := False;
         Parent_Depth  : Integer := 0;

         function Append_Text (Str : String) return Boolean;

         function Token_Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean;

         -----------------
         -- Append_Text --
         -----------------

         function Append_Text (Str : String) return Boolean is
            Size_Taken : Integer := Str'Length;
            Stop       : Boolean := False;
         begin
            if Size_Taken + Current_Ind > Result'Length - 3 then
               Size_Taken := Result'Length - 3 - Current_Ind;
               Stop := True;
            end if;

            Result (Current_Ind + 1 .. Current_Ind + 1 + Size_Taken - 1) :=
              Str (Str'First .. Str'First + Size_Taken - 1);

            Current_Ind := Current_Ind + Size_Taken;

            if Stop then
               Result (Result'Last - 2 .. Result'Last) := "...";
               Current_Ind := Result'Last;
               return True;
            else
               return False;
            end if;
         end Append_Text;

         --------------------
         -- Token_Callback --
         --------------------

         function Token_Callback
           (Entity         : Language_Entity;
            Sloc_Start     : Source_Location;
            Sloc_End       : Source_Location;
            Partial_Entity : Boolean) return Boolean
         is
            pragma Unreferenced (Partial_Entity);

            Text : constant String :=
              Buffer (Sloc_Start.Index .. Sloc_End.Index);
         begin
            if Entity = Operator_Text and then Text = ";" then
               return True;
            end if;

            if not Extract_Value  then
               if Entity = Operator_Text
                 and then Text = ":="
               then
                  Extract_Value := True;
               end if;

               return False;
            else
               if Entity = Operator_Text then
                  if Text = "(" then
                     Parent_Depth := Parent_Depth + 1;

                     return Append_Text (" (");
                  elsif Text = ")" or else Text = "," then
                     if Text = ")" then
                        if Parent_Depth = 0 then
                           return True;
                        end if;

                        Parent_Depth := Parent_Depth - 1;
                     end if;

                     return Append_Text (Text);
                  else
                     return Append_Text (" " & Text);
                  end if;
               end if;

               return Append_Text (" " & Text);
            end if;
         end Token_Callback;
      begin
         Parse_Entities
           (Ada_Lang, Buffer (Construct.Sloc_Entity.Index .. Buffer'Last),
            Token_Callback'Unrestricted_Access);

         return Result (1 .. Current_Ind);
      end Get_Default_Value;

      Add_New_Line : Boolean := False;

   begin
      Get_Documentation_Before
        (Context       => Get_Language_Context (Language).all,
         Buffer        => Buffer,
         Decl_Index    => Get_Construct (Node).Sloc_Start.Index,
         Comment_Start => Beginning,
         Comment_End   => Current);

      if Beginning = 0 then
         Get_Documentation_After
           (Context       => Get_Language_Context (Language).all,
            Buffer        => Buffer,
            Decl_Index    => Get_Construct (Node).Sloc_End.Index,
            Comment_Start => Beginning,
            Comment_End   => Current);
      end if;

      if Beginning /= 0 then
         Unbounded.Append
           (Result,
            Escape_Text
              (Comment_Block
                 (Language,
                  Buffer (Beginning .. Current),
                  Comment => False,
                  Clean   => True)));

         Add_New_Line := True;
      end if;

      if Get_Construct (Node).Category in Subprogram_Category then
         declare
            Sub_Iter                  : Construct_Tree_Iterator :=
              Next (Tree, Node, Jump_Into);
            Has_Parameter                : Boolean := False;
            Biggest_Parameter_Name       : Integer := 0;
            Biggest_Decoration_Length    : Integer := 0;
            Biggest_Affected_Type_Length : Integer := 0;
            Current_Affected_Type_Length : Integer := 0;
         begin
            while Get_Parent_Scope (Tree, Sub_Iter) = Node loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if Get_Construct (Sub_Iter).Name'Length >
                    Biggest_Parameter_Name
                  then
                     Biggest_Parameter_Name :=
                       Get_Construct (Sub_Iter).Name'Length;
                  end if;

                  if Attribute_Decoration
                    (Get_Construct (Sub_Iter), True)'Length
                    > Biggest_Decoration_Length
                  then
                     Biggest_Decoration_Length :=
                       Attribute_Decoration
                         (Get_Construct (Sub_Iter), True)'Length;
                  end if;

                  if Get_Construct (Sub_Iter).Attributes
                    (Ada_Assign_Attribute)
                  then
                     Get_Referenced_Entity
                       (Language,
                        Buffer,
                        Get_Construct (Sub_Iter),
                        Type_Start,
                        Type_End,
                        Success);

                     Current_Affected_Type_Length :=
                       Type_End.Index - Type_Start.Index + 1;

                     if Get_Construct (Sub_Iter).Attributes
                       (Ada_Class_Attribute)
                     then
                        Current_Affected_Type_Length :=
                          Current_Affected_Type_Length + 6;
                        --  Addition of the 'Class attribute to the label
                     end if;

                     if Success
                       and then Current_Affected_Type_Length
                         > Biggest_Affected_Type_Length
                     then
                        Biggest_Affected_Type_Length :=
                          Current_Affected_Type_Length;
                     end if;
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;

            Sub_Iter := Next (Tree, Node, Jump_Into);

            while Get_Parent_Scope (Tree, Sub_Iter) = Node loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if not Has_Parameter then
                     if Add_New_Line then
                        Unbounded.Append (Result, ASCII.LF & ASCII.LF);
                     end if;

                     Unbounded.Append (Result, "<b>Parameters:</b>");
                     Has_Parameter := True;
                     Add_New_Line := True;
                  end if;

                  Unbounded.Append (Result, ASCII.LF);

                  Get_Referenced_Entity
                    (Language,
                     Buffer,
                     Get_Construct (Sub_Iter),
                     Type_Start,
                     Type_End,
                     Success);

                  if Get_Construct (Sub_Iter).Attributes
                    (Ada_Assign_Attribute)
                  then
                     Unbounded.Append
                       (Result, "<span foreground=""#555555"">[");
                  else
                     Unbounded.Append (Result, " ");
                  end if;

                  Current_Affected_Type_Length :=
                    Type_End.Index - Type_Start.Index + 1;

                  Unbounded.Append
                    (Result,
                     Escape_Text (Get_Construct (Sub_Iter).Name.all));

                  --  ??? These loops are highly inefficient. Consider
                  --  improving these

                  for J in Get_Construct (Sub_Iter).Name'Length + 1
                    .. Biggest_Parameter_Name
                  loop
                     Unbounded.Append (Result, " ");
                  end loop;

                  if Success then
                     Unbounded.Append
                       (Result,
                        " : <b>"
                        & Attribute_Decoration (Get_Construct (Sub_Iter), True)
                        & "</b>");

                     for J in
                       Attribute_Decoration
                         (Get_Construct (Sub_Iter), True)'Length + 1
                       .. Biggest_Decoration_Length
                     loop
                        Unbounded.Append (Result, " ");
                     end loop;

                     Unbounded.Append
                       (Result,
                        Escape_Text
                          (Buffer (Type_Start.Index .. Type_End.Index)));

                     if Get_Construct (Sub_Iter).Attributes
                       (Ada_Class_Attribute)
                     then
                        Unbounded.Append (Result, "'Class");
                        Current_Affected_Type_Length :=
                          Current_Affected_Type_Length + 6;
                     end if;
                  else
                     Unbounded.Append (Result, " : ???");
                  end if;

                  if Get_Construct (Sub_Iter).Attributes
                    (Ada_Assign_Attribute)
                  then
                     for J in Current_Affected_Type_Length + 1
                       .. Biggest_Affected_Type_Length
                     loop
                        Unbounded.Append (Result, " ");
                     end loop;

                     Unbounded.Append
                       (Result, " :="
                        & Escape_Text
                          (Get_Default_Value (Get_Construct (Sub_Iter)))
                        & "]</span>");
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;
         end;

         Get_Referenced_Entity
           (Language,
            Buffer,
            Get_Construct (Node),
            Type_Start,
            Type_End,
            Success);

         if Success then
            if Add_New_Line then
               Unbounded.Append (Result, ASCII.LF & ASCII.LF);
            end if;

            Unbounded.Append
              (Result,
               "<b>Return:</b>"
               & ASCII.LF & " <b>"
               & Attribute_Decoration (Get_Construct (Node), False)
               & "</b>"
               & Escape_Text (Buffer (Type_Start.Index .. Type_End.Index)));

            if Get_Construct (Node).Attributes (Ada_Class_Attribute) then
               Unbounded.Append (Result, "'Class");
            end if;
         end if;
      elsif Get_Construct (Node).Category in Data_Category then
         declare
            Var_Start, Var_End : Source_Location;
         begin
            Get_Referenced_Entity
              (Language,
               Buffer,
               Get_Construct (Node),
               Var_Start,
               Var_End,
               Success);

            if Success then
               if Add_New_Line then
                  Unbounded.Append (Result, ASCII.LF & ASCII.LF);
               end if;

               Unbounded.Append
                 (Result,
                  "<b>Type: "
                  & Attribute_Decoration (Get_Construct (Node), False)
                  & "</b>"
                  & Escape_Text (Buffer (Var_Start.Index .. Var_End.Index)));

               if Get_Construct (Node).Attributes (Ada_Class_Attribute) then
                  Unbounded.Append (Result, "'Class");
               end if;
            end if;
         end;
      end if;

      return Unbounded.To_String (Result);
   end Get_Documentation;

   ----------------
   -- Is_Body_Of --
   ----------------

   function Is_Body_Of
     (Body_Tree, Spec_Tree : Construct_Tree) return Boolean
   is
      Spec_Unit, Body_Unit : Construct_Tree_Iterator;
   begin
      Spec_Unit := Get_Unit_Construct (Ada_Tree_Lang, Spec_Tree);
      Body_Unit := Get_Unit_Construct (Ada_Tree_Lang, Body_Tree);

      if Spec_Unit = Null_Construct_Tree_Iterator
        or else Body_Unit = Null_Construct_Tree_Iterator
      then
         return False;
      end if;

      return To_Lower (Spec_Unit.Node.Construct.Name.all)
        = To_Lower (Body_Unit.Node.Construct.Name.all)
        and then Spec_Unit.Node.Construct.Is_Declaration
        and then not Body_Unit.Node.Construct.Is_Declaration;
   end Is_Body_Of;

end Language.Tree.Ada;
