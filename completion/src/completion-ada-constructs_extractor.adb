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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation; use Ada;

with Ada_Analyzer.Utils;     use Ada_Analyzer.Utils;
with Language.Ada;           use Language.Ada;
with Language.Documentation; use Language.Documentation;
with Language.Tree.Ada;      use Language.Tree.Ada;
with Basic_Types;            use Basic_Types;

package body Completion.Ada.Constructs_Extractor is

   use Completion_List_Pckg;
   use Completion_List_Extensive_Pckg;
   use Extensive_List_Pckg;

   ---------------------------------------
   -- New_Construct_Completion_Resolver --
   ---------------------------------------

   function New_Construct_Completion_Resolver
     (Construct_Db   : Construct_Database_Access;
      Current_File   : Virtual_File;
      Current_Buffer : GNAT.Strings.String_Access)
      return Construct_Completion_Resolver
   is
      Resolver : Construct_Completion_Resolver;
   begin
      Resolver.Manager := null;
      Resolver.Construct_Db := Construct_Db;
      Resolver.Current_File := Get_Or_Create
        (Construct_Db, Current_File, Ada_Tree_Lang);
      Resolver.Current_Buffer := Current_Buffer;

      return Resolver;
   end New_Construct_Completion_Resolver;

   --------------------
   -- Get_Completion --
   --------------------

   function Get_Completion
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      if Proposal.Is_All then
         return "all";
      elsif Get_Construct (Proposal.Tree_Node).Category = Cat_Package
        or else Get_Construct (Proposal.Tree_Node).Category = Cat_Function
        or else Get_Construct (Proposal.Tree_Node).Category = Cat_Procedure
      then
         declare
            Id : constant Composite_Identifier := To_Composite_Identifier
              (Get_Construct (Proposal.Tree_Node).Name.all);
         begin
            return Get_Item (Id, Length (Id));
         end;
      else
         return Get_Construct (Proposal.Tree_Node).Name.all;
      end if;
   end Get_Completion;

   --------------
   -- Get_Type --
   --------------

   function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category
   is
   begin
      if Proposal.Is_All then
         return Cat_Unknown;
      else
         return Get_Construct (Proposal.Tree_Node).Category;
      end if;
   end Get_Category;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin

         return Get_Documentation
           (Ada_Lang,
            Proposal.Buffer.all,
            Proposal.Tree,
            Proposal.Tree_Node);

   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Proposal : Construct_Completion_Proposal) return File_Location is
   begin
      return (Get_File_Path (Proposal.File),
              Get_Construct (Proposal.Tree_Node).Sloc_Start.Line,
              Basic_Types.Visible_Column_Type
                (Get_Construct (Proposal.Tree_Node).Sloc_Start.Column));
   end Get_Location;

   --------------------
   -- Get_Compositon --
   --------------------

   procedure Get_Composition
     (Proposal   : Construct_Completion_Proposal;
      Identifier : String;
      Offset     : Positive;
      Is_Partial : Boolean;
      Result     : in out Completion_List)
   is
      Tree  : Construct_Tree;
      Ada_Tree : Ada_Construct_Tree;

      Parent_Sloc_Start, Parent_Sloc_End : Source_Location;
      Parent_Found                       : Boolean;

      procedure Add_To_List
        (List      : in out Extensive_List_Pckg.List;
         Tree_Node : Construct_Tree_Iterator;
         Is_All    : Boolean := False;
         File      : Structured_File_Access;
         Tree      : Construct_Tree;
         Buffer    : GNAT.Strings.String_Access;
         Name      : String := "");

      procedure Add_Children_Of
        (Tree               : Construct_Tree;
         Scope              : Construct_Tree_Iterator;
         List               : in out Extensive_List_Pckg.List;
         Private_Visibility : Boolean);
      --  Add all the children of the scope given in parameter in the list.
      --  Consider private elements if Private_Visibility is true.

      procedure Add_To_List
        (List      : in out Extensive_List_Pckg.List;
         Tree_Node : Construct_Tree_Iterator;
         Is_All    : Boolean := False;
         File      : Structured_File_Access;
         Tree      : Construct_Tree;
         Buffer    : GNAT.Strings.String_Access;
         Name      : String := "")
      is
         P : constant Construct_Completion_Proposal :=
           (Show_Identifiers,
            Get_Resolver (Proposal),
            Tree_Node,
            File,
            Is_All,
            0,
            Tree,
            Proposal.Filter,
            Buffer);
      begin
         if Get_Construct (Tree_Node).Name /= null
           and then
             ((Name = "" and then Match (Identifier, Get_Id (P), Is_Partial))
              or else Match (Identifier, Name, Is_Partial))
         then
            Append (List, P);
         end if;
      end Add_To_List;

      ---------------------
      -- Add_Children_Of --
      ---------------------

      procedure Add_Children_Of
        (Tree               : Construct_Tree;
         Scope              : Construct_Tree_Iterator;
         List               : in out Extensive_List_Pckg.List;
         Private_Visibility : Boolean)
      is
         Child_Iterator : Construct_Tree_Iterator :=
           Next (Tree, Scope, Jump_Into);
      begin
         while Get_Parent_Scope (Tree, Child_Iterator) = Scope loop
            if Private_Visibility
              or else
                Get_Construct (Child_Iterator).Visibility
                = Visibility_Public
            then
               if (Proposal.Filter and All_Visible_Entities) /= 0
                 or else Get_Construct
                   (Child_Iterator).Category = Cat_Package
               then
                  Add_To_List
                    (List,
                     Child_Iterator,
                     False,
                     Proposal.File,
                     Proposal.Tree,
                     Proposal.Buffer);

                  if Is_Enum_Type (Tree, Child_Iterator) then
                     Add_Children_Of (Tree, Child_Iterator, List, True);
                  end if;
               end if;
            end if;

            Child_Iterator := Next (Tree, Child_Iterator, Jump_Over);
         end loop;
      end Add_Children_Of;

      List : Extensive_List_Pckg.List;

      Proposal_Is_Access : Boolean := False;
      Proposal_Is_All    : Boolean := False;
   begin
      Tree := Proposal.Tree;

      Ada_Tree := Generate_Ada_Construct_Tree
        (Proposal.Tree, Ada_Lang, Proposal.Buffer.all);

      case Get_Construct (Proposal.Tree_Node).Category is
         when Cat_Variable | Cat_Local_Variable | Cat_Field | Cat_Parameter
            | Cat_Class .. Cat_Subtype | Subprogram_Category =>

            if Get_Construct (Proposal.Tree_Node).Category in
              Subprogram_Category
              and then Proposal.Params_In_Expression
                > Get_Number_Of_Parameters (Proposal)
            then
               return;
            end if;

            Proposal_Is_Access := Is_Access
              (Proposal.Buffer.all,
               Get_Construct (Proposal.Tree_Node));

            if Proposal_Is_Access and then not Proposal.Is_All then
               declare
                  All_List : Extensive_List_Pckg.List;
               begin
                  Add_To_List
                    (All_List,
                     Proposal.Tree_Node,
                     True,
                     Proposal.File,
                     Proposal.Tree,
                     Proposal.Buffer,
                     "all");

                  Append (Result.List, To_Extensive_List (All_List));
                  --  We do that now because we want "all" to be displayed in
                  --  front of other possibilities.

                  Proposal_Is_All := True;
               end;
            end if;

            Get_Referenced_Entity
              (Ada_Lang,
               Proposal.Buffer.all,
               Get_Construct (Proposal.Tree_Node),
               Parent_Sloc_Start,
               Parent_Sloc_End,
               Parent_Found);

            if Parent_Found then
               declare
                  List : Completion_List := Get_Initial_Completion_List
                    (Get_Resolver (Proposal).Manager.all,
                     Proposal.Buffer.all,
                     Parent_Sloc_End.Index,
                     False);
                  Sub_Proposal_Is_Access : Boolean;

                  It : Completion_Iterator := First (List);
               begin
                  while not At_End (It) loop
                     if Get_Proposal (It)
                       in Construct_Completion_Proposal'Class
                     then
                        Sub_Proposal_Is_Access := Is_Access
                          (Construct_Completion_Proposal
                                (Get_Proposal (It)).Buffer.all,
                           Get_Construct
                             (Construct_Completion_Proposal
                                (Get_Proposal (It)).Tree_Node));

                        if not Proposal_Is_All
                          or else not Sub_Proposal_Is_Access
                        then
                           Get_Composition
                             (Get_Proposal (It),
                              Identifier,
                              1,
                              Is_Partial,
                              Result);
                        end if;
                     else
                        Get_Composition
                          (Get_Proposal (It),
                           Identifier,
                           1,
                           Is_Partial,
                           Result);
                     end if;

                     Next (It);
                  end loop;

                  Free (List);
               end;
            end if;

            if Get_Construct (Proposal.Tree_Node).Category
              in Cat_Class .. Cat_Subtype
              and then not Is_Enum_Type (Tree, Proposal.Tree_Node)
            then
               declare
                  Child_Iterator : Construct_Tree_Iterator;
                  Type_Iterator  : constant Construct_Tree_Iterator
                    := Proposal.Tree_Node;
               begin
                  Child_Iterator := Next (Tree, Type_Iterator, Jump_Into);

                  while
                    Get_Parent_Scope (Tree, Child_Iterator) = Type_Iterator
                  loop
                     Add_To_List
                       (List,
                        Child_Iterator,
                        False,
                        Proposal.File,
                        Proposal.Tree,
                        Proposal.Buffer);

                     Child_Iterator := Next
                       (Tree, Child_Iterator, Jump_Over);
                  end loop;
               end;
            end if;

            Append (Result.List, To_Extensive_List (List));

         when Cat_Package =>
            declare
               Spec_It         : Construct_Tree_Iterator;
               Body_It         : Construct_Tree_Iterator;
               Private_Spec_Visibility : Boolean;
               Body_Visibility : Boolean;
            begin
               Spec_It := Get_Spec
                 (Tree, Ada_Tree, Proposal.Tree_Node);
               Body_It := Get_First_Body
                 (Tree, Ada_Tree, Proposal.Tree_Node);

               Body_Visibility :=
                 Get_Construct (Body_It).Sloc_Start.Index <= Offset
                 and then Get_Construct
                   (Body_It).Sloc_End.Index >= Offset;

               Private_Spec_Visibility :=
                 Body_Visibility
                 or else (Get_Construct (Spec_It).Sloc_Start.Index <= Offset
                          and then Get_Construct
                            (Spec_It).Sloc_End.Index >= Offset);

               --  Add the entities from the specification

               Add_Children_Of (Tree, Spec_It, List, Private_Spec_Visibility);

               --  If we have to add the entries in the body if needed

               if Body_Visibility and then Body_It /= Spec_It then
                  Add_Children_Of (Tree, Body_It, List, True);
               end if;

               if Get_Unit_Construct
                 (Ada_Tree_Lang, Get_Public_Tree (Proposal.File))
                 = Proposal.Tree_Node
               then
                  --  If we are completing a unit name, then look for its
                  --  children

                  declare
                     use Language.Tree.Database.File_Set;

                     C : Language.Tree.Database.File_Set.Cursor :=
                       Start_File_Search
                          (Construct_Completion_Resolver
                             (Proposal.Resolver.all).Construct_Db.all);
                  begin
                     while C /= Language.Tree.Database.File_Set.No_Element loop
                        if Get_Parent_File (Element (C)) = Proposal.File
                          and then Get_Construct
                            (Get_Unit_Construct
                                 (Ada_Tree_Lang,
                                  Get_Public_Tree (Element (C))))
                          .Is_Declaration
                        then
                           Add_To_List
                             (List,
                              Get_Unit_Construct
                                (Ada_Tree_Lang,
                                 Get_Public_Tree (Element (C))),
                              False,
                              Element (C),
                              Get_Public_Tree (Element (C)),
                              Get_Buffer (Element (C)));
                        end if;

                        C := Next (C);
                     end loop;
                  end;
               end if;

               Append (Result.List, To_Extensive_List (List));

               return;
            end;

         when others =>
            null;
      end case;

      Free (Ada_Tree);
   end Get_Composition;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters
     (Proposal : Construct_Completion_Proposal) return Natural
   is
      Total : Integer := 0;
      Tree  : Construct_Tree;
      It    : Construct_Tree_Iterator;
   begin
      Tree := Proposal.Tree;

      It := Next (Tree, Proposal.Tree_Node, Jump_Into);

      while Get_Parent_Scope (Tree, It) = Proposal.Tree_Node loop
         if Get_Construct (It).Category = Cat_Parameter then
            Total := Total + 1;
         end if;

         It := Next (Tree, It, Jump_Over);
      end loop;

      return Total;
   end Get_Number_Of_Parameters;

   -----------------------
   -- Append_Expression --
   -----------------------

   procedure Append_Expression
     (Proposal             : in out Construct_Completion_Proposal;
      Number_Of_Parameters : Natural)
   is
   begin
      Proposal.Params_In_Expression := Number_Of_Parameters;
   end Append_Expression;

   ----------
   -- Free --
   ----------

   procedure Free (Proposal : in out Construct_Completion_Proposal) is
      pragma Unreferenced (Proposal);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Db_Wrapper) is
   begin
      Free (This.Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Tree_Iterator_Array_Access) is
      procedure Internal_Free is new
        Unchecked_Deallocation
          (Construct_Tree_Iterator_Array,
           Construct_Tree_Iterator_Array_Access);
   begin
      Internal_Free (This);
   end Free;

   -----------------------
   -- Get_Possibilities --
   -----------------------

   procedure Get_Possibilities
     (Resolver   : access Construct_Completion_Resolver;
      Identifier : String;
      Is_Partial : Boolean;
      Offset     : Integer;
      Filter     : Possibilities_Filter;
      Result     : in out Completion_List) is
   begin
      Result.Searched_Identifier := new String'(Identifier);

      if (Filter and All_Accessible_Units) /= 0 then
         --  Create an extensive list of all the accessible units with no
         --  parent.
         declare
            use File_Set;

            C : File_Set.Cursor := Start_File_Search
              (Resolver.Construct_Db.all);
            List : Extensive_List_Pckg.List;
            Construct_It : Construct_Tree_Iterator;

         begin
            while C /= File_Set.No_Element loop
               Construct_It := Get_Unit_Construct
                 (Ada_Tree_Lang,
                  Get_Public_Tree (Element (C)));

               if Construct_It /= Null_Construct_Tree_Iterator
                 and then Get_Construct (Construct_It).Is_Declaration
               then
                  declare
                     Name : constant Composite_Identifier :=
                       Get_Unit_Name
                         (Ada_Tree_Lang, Get_Public_Tree (Element (C)));
                  begin
                     if Match
                       (Identifier,
                        Get_Item (Name, Length (Name)),
                        Is_Partial)
                     then
                        Append
                          (List,
                           Construct_Completion_Proposal'
                             (Show_Identifiers,
                              Resolver,
                              Construct_It,
                              Element (C),
                              False,
                              0,
                              Get_Public_Tree (Element (C)),
                              Filter,
                              Get_Buffer (Element (C))));
                     end if;
                  end;
               end if;

               C := Next (C);
            end loop;

            Append (Result.List, To_Extensive_List (List));
         end;
      end if;

      if (Filter and All_Visible_Entities) /= 0 and then Offset > 0 then
         Append
           (Result.List,
            Construct_Db_Wrapper'
              (First_File   => Resolver.Current_File,
               First_Buffer => Resolver.Current_Buffer,
               Construct_Db => Resolver.Construct_Db,
               Name         => new String'(Identifier),
               Is_Partial   => Is_Partial,
               Offset       => Offset,
               Resolver     => Resolver,
               Filter       => Filter));
      end if;
   end Get_Possibilities;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Completion_Resolver) is
      C : Tree_List.Cursor := First (This.Tree_Collection);
   begin
      while C /= Tree_List.No_Element loop
         declare
            Tree : Construct_Tree := Element (C);
         begin
            Free (Tree);
         end;

         C := Next (C);
      end loop;
   end Free;

   -----------
   -- First --
   -----------

   function First
     (Db_Construct : Construct_Db_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      Result : Construct_Iterator_Wrapper;
      Tree   : constant Construct_Tree :=
        To_Construct_Tree (Db_Construct.First_Buffer, Ada_Lang);
   begin
      Result.Stage := Initial_File;
      Result.First_File := Db_Construct.First_File;
      Result.Name := Db_Construct.Name;
      Result.Construct_Db := Db_Construct.Construct_Db;
      Result.Resolver := Db_Construct.Resolver;
      Result.Is_Partial := Db_Construct.Is_Partial;
      Result.Filter := Db_Construct.Filter;
      Result.First_Buffer := Db_Construct.First_Buffer;
      Result.First_Tree := Tree;
      Result.First_Buffer := Db_Construct.First_Buffer;

      Append (Db_Construct.Resolver.Tree_Collection, Tree);

      declare
         Ada_Tree : Ada_Construct_Tree :=
           Generate_Ada_Construct_Tree
             (Result.First_Tree,
              Ada_Lang,
              Result.First_Buffer.all);
      begin
         Result.Visible_Constructs :=
           new Construct_Tree_Iterator_Array'
             (Get_Visible_Constructs
                  (Result.First_Tree,
                   Ada_Tree,
                   Db_Construct.Offset,
                   Db_Construct.Name.all,
                   Is_Partial => Db_Construct.Is_Partial));

         Free (Ada_Tree);
      end;

      if Result.Visible_Constructs'Length = 0 then
         Result.Visible_Index := 0;
         Next (Result);
      else
         Result.Visible_Index := Result.Visible_Constructs'First;
      end if;

      return Result;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Construct_Iterator_Wrapper) return Boolean is
   begin
      return It.Stage = Database and then
        (At_End (It.Db_Iterator)
         or else
           (not It.Is_Partial
            and then  Get_Construct
              (Get_Construct (It.Db_Iterator)).Name.all /= It.Name.all));
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Construct_Iterator_Wrapper) return Boolean is
   begin
      if At_End (It) then
         return True;
      end if;

      if It.Stage = Initial_File then
         return It.Visible_Constructs /= null
           and then It.Visible_Index <= It.Visible_Constructs'Last
           and then
             (Get_Construct (It.Visible_Constructs (It.Visible_Index))
              .Category /= Cat_Package
              or else
              Get_Construct (It.Visible_Constructs (It.Visible_Index))
              .Is_Declaration);
      elsif It.Stage = Parent_File then
         return It.Current_It /= Null_Construct_Tree_Iterator;
      elsif It.Stage = Database then
         declare
            File : Structured_File_Access := It.First_File;
         begin
            while File /= null loop
               if Get_File (It.Db_Iterator) = File then
                  return False;
               end if;

               File := Get_Parent_File (File);
            end loop;
         end;

         return True;
      end if;

      return True;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Construct_Iterator_Wrapper) is
   begin
      loop
         if It.Stage = Initial_File then
            --  Stage 1: We return the visible constructs from the current
            --  file.

            if It.Visible_Index < It.Visible_Constructs'Length then
               It.Visible_Index := It.Visible_Index + 1;
            else
               It.Stage := Parent_File;
               Free (It.Visible_Constructs);
               It.Visible_Index := 0;
               It.Current_It := Null_Construct_Tree_Iterator;
               It.Current_File := Get_Parent_File (It.First_File);
            end if;
         elsif It.Stage = Parent_File then
            --  Stage 2: We return the constructs from the parent files

            if It.Current_File = null then
               It.Stage := Database;
               It.Db_Iterator := Start
                 (It.Construct_Db, To_Lower (It.Name.all), It.Is_Partial);
            else
               if It.Current_It = Null_Construct_Tree_Iterator
                 and then It.Current_File /= null
               then
                  It.Current_It := First (Get_Full_Tree (It.Current_File));
               end if;

               while It.Current_It /= Null_Construct_Tree_Iterator loop
                  if Get_Construct (It.Current_It).Category = Cat_Package
                    or else Is_Enum_Type
                      (Get_Full_Tree (It.Current_File), It.Current_It)
                  then
                     It.Current_It :=
                       Next
                         (Get_Full_Tree
                              (It.Current_File), It.Current_It, Jump_Into);
                  else
                     It.Current_It :=
                       Next
                         (Get_Full_Tree
                              (It.Current_File), It.Current_It, Jump_Over);
                  end if;

                  declare
                     Constr : constant Simple_Construct_Information :=
                       Get_Construct (It.Current_It);
                  begin
                     exit when It.Current_It /= Null_Construct_Tree_Iterator
                       and then Constr.Name /= null
                       and then Constr.Category /= Cat_Use
                       and then Constr.Category /= Cat_With
                       and then Get_Parent_Scope
                         (Get_Full_Tree (It.Current_File), It.Current_It)
                       /= Null_Construct_Tree_Iterator
                       and then Match
                         (It.Name.all,
                          Constr.Name.all,
                          It.Is_Partial);
                  end;
               end loop;

               if It.Current_It = Null_Construct_Tree_Iterator then
                  It.Current_File := Get_Parent_File (It.Current_File);
                  It.Current_It := Null_Construct_Tree_Iterator;
               end if;
            end if;

         elsif It.Stage = Database then
            --  Stage 3: We return the files from the database

            if not At_End (It.Db_Iterator) then
               Next (It.Db_Iterator);
            end if;
         end if;

         exit when Is_Valid (It);
      end loop;

   end Next;

   ---------
   -- Get --
   ---------

   function Get
     (This : Construct_Iterator_Wrapper) return Completion_Proposal'Class is
   begin
      case This.Stage is
         when Initial_File =>
            return Construct_Completion_Proposal'
              (Mode                 => Show_Identifiers,
               Resolver             => This.Resolver,
               Tree_Node            =>
                  This.Visible_Constructs (This.Visible_Index),
               File                 => This.First_File,
               Is_All               => False,
               Params_In_Expression => 0,
               Tree                 => This.First_Tree,
               Buffer               => This.First_Buffer,
               Filter               => This.Filter);
         when Parent_File =>
            return Construct_Completion_Proposal'
              (Mode                 => Show_Identifiers,
               Resolver             => This.Resolver,
               Tree_Node            => This.Current_It,
               File                 => This.Current_File,
               Tree                 => Get_Full_Tree (This.Current_File),
               Buffer               => Get_Buffer (This.Current_File),
               Is_All               => False,
               Params_In_Expression => 0,
               Filter               => This.Filter);
         when Database =>
            return Construct_Completion_Proposal'
              (Mode                 => Show_Identifiers,
               Resolver             => This.Resolver,
               Tree_Node            =>
                  Get_Construct (This.Db_Iterator),
               File                 => Get_File (This.Db_Iterator),
               Is_All               => False,
               Params_In_Expression => 0,
               Tree                 => Get_Public_Tree
                 (Get_File (This.Db_Iterator)),
               Buffer               => Get_Buffer
                 (Get_File (This.Db_Iterator)),
               Filter               => This.Filter);
      end case;
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Iterator_Wrapper) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

end Completion.Ada.Constructs_Extractor;
