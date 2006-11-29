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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation; use Ada;

with GNAT.Strings;

with Glib.Unicode; use Glib.Unicode;

with Language.Ada;           use Language.Ada;
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
     (Proposal : Construct_Completion_Proposal) return UTF8_String
   is
      Comma         : Boolean := False;
      Max_Size_Name : Integer := 0;
      Nb_Params     : Integer := 0;
   begin
      if Proposal.Is_In_Profile then
         if Get_Construct (Proposal.Tree_Node).Category in Data_Category then
            return Get_Label (Proposal) & " => ";
         else
            if Proposal.Profile.Parameters'Length > 0 then
               for J in Proposal.Profile.Parameters'Range loop
                  if not Proposal.Profile.Parameters (J).Is_Written then
                     if Proposal.Profile.Parameters (J).Name'Length
                       > Max_Size_Name
                     then
                        Max_Size_Name :=
                          Proposal.Profile.Parameters (J).Name'Length;
                     end if;

                     Nb_Params := Nb_Params + 1;
                  end if;
               end loop;

               declare
                  Buffer : String
                    (1 .. Nb_Params * (Max_Size_Name + 6) - 1) :=
                    (others => ' ');

                  Index : Integer := 1;
               begin
                  for J in Proposal.Profile.Parameters'Range loop
                     if not Proposal.Profile.Parameters (J).Is_Written then
                        if Comma then
                           Buffer (Index .. Index + 1) := "," & ASCII.LF;
                           Index := Index + 2;
                        end if;

                        Buffer
                          (Index ..
                             Index
                           + Proposal.Profile.Parameters (J).Name'Length
                           - 1) := Proposal.Profile.Parameters (J).Name.all;

                        Index := Index + Max_Size_Name;

                        Buffer (Index .. Index + 3) := " => ";

                        Index := Index + 4;

                        Comma := True;
                     end if;
                  end loop;

                  Buffer (Buffer'Last .. Buffer'Last) := ")";

                  return Buffer;
               end;
            end if;

            return "";
         end if;
      else
         return Get_Label (Proposal);
      end if;
   end Get_Completion;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      if Proposal.Is_In_Profile and then
        Get_Construct (Proposal.Tree_Node).Category not in Data_Category
      then
         return "params of " & Get_Construct (Proposal.Tree_Node).Name.all;
      else
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
      end if;
   end Get_Label;

   ----------------------
   -- Get_Caret_Offset --
   ----------------------

   function Get_Caret_Offset
     (Proposal : Construct_Completion_Proposal)
      return Basic_Types.Character_Offset_Type
   is
      Max_Param_Length     : Glong := 0;
      Current_Param_Length : Glong := 0;
   begin
      if Proposal.Is_In_Profile and then
        Get_Construct (Proposal.Tree_Node).Category not in Data_Category
      then
         if Proposal.Profile.Parameters'Length > 0 then
            for J in Proposal.Profile.Parameters'Range loop
               Current_Param_Length :=
                 UTF8_Strlen (Proposal.Profile.Parameters (1).Name.all);

               if Current_Param_Length > Max_Param_Length then
                  Max_Param_Length := Current_Param_Length;
               end if;
            end loop;

            return Basic_Types.Character_Offset_Type (Max_Param_Length) + 4;
            --  4 is for the " => " string.
         else
            return 0;
         end if;
      else
         return Basic_Types.Character_Offset_Type
          (UTF8_Strlen
               (Get_Completion (Completion_Proposal'Class (Proposal))));
      end if;
   end Get_Caret_Offset;

   --------------
   -- Get_Type --
   --------------

   function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category is
   begin
      if Proposal.Is_All then
         return Cat_Literal;
      elsif Proposal.Is_In_Profile
        and then Get_Construct
          (Proposal.Tree_Node).Category not in Data_Category
      then
         return Cat_Unknown;
      else
         return Get_Construct (Proposal.Tree_Node).Category;
      end if;
   end Get_Category;

   --------------------
   -- Get_Visibility --
   --------------------

   function Get_Visibility
     (Proposal : Construct_Completion_Proposal) return Construct_Visibility is
   begin
      if Proposal.Is_All then
         return Visibility_Public;
      else
         return Get_Construct (Proposal.Tree_Node).Visibility;
      end if;
   end Get_Visibility;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      return Get_Documentation
        (Ada_Tree_Lang,
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
      Ada_Tree : Ada_Construct_Tree;

      procedure Add_To_List
        (List          : in out Extensive_List_Pckg.List;
         Tree_Node     : Construct_Tree_Iterator;
         Is_All        : Boolean := False;
         File          : Structured_File_Access;
         Tree          : Construct_Tree;
         Buffer        : GNAT.Strings.String_Access;
         Name          : String := "";
         Is_In_Profile : Boolean := False;
         Profile       : Profile_Manager_Access := null);

      procedure Add_Children_Of
        (Tree               : Construct_Tree;
         Scope              : Construct_Tree_Iterator;
         List               : in out Extensive_List_Pckg.List;
         Private_Visibility : Boolean);
      --  Add all the children of the scope given in parameter in the list.
      --  Consider private elements if Private_Visibility is true.

      procedure Handle_Referenced_Entity
        (Proposal        : Construct_Completion_Proposal;
         Cut_Access      : Boolean;
         Cut_Subprograms : Boolean);
      --  Adds the relevant content from the sub entity of the proposal. If
      --  Cut_Access is true, then accesses won't be appened to the result.

      procedure Add_To_List
        (List          : in out Extensive_List_Pckg.List;
         Tree_Node     : Construct_Tree_Iterator;
         Is_All        : Boolean := False;
         File          : Structured_File_Access;
         Tree          : Construct_Tree;
         Buffer        : GNAT.Strings.String_Access;
         Name          : String := "";
         Is_In_Profile : Boolean := False;
         Profile       : Profile_Manager_Access := null)
      is
         P : Construct_Completion_Proposal :=
           (Get_Resolver (Proposal),
            Profile,
            Tree_Node,
            File,
            Is_All,
            0,
            Tree,
            Proposal.Filter,
            Buffer,
            Is_In_Profile);
      begin
         if P.Profile = null then
            P.Profile := To_Profile_Manager (Tree, Tree_Node);
         end if;

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

      ------------------------------
      -- Handle_Referenced_Entity --
      ------------------------------

      procedure Handle_Referenced_Entity
        (Proposal        : Construct_Completion_Proposal;
         Cut_Access      : Boolean;
         Cut_Subprograms : Boolean)
      is
         Parent_Sloc_Start, Parent_Sloc_End : Source_Location;
         Parent_Found                       : Boolean;
      begin
         Get_Referenced_Entity
           (Ada_Lang,
            Proposal.Buffer.all,
            Get_Construct (Proposal.Tree_Node),
            Parent_Sloc_Start,
            Parent_Sloc_End,
            Parent_Found);

         if Parent_Found then
            Push_Excluded_Construct
              (Construct_Completion_Resolver (Proposal.Resolver.all)'Access,
               (Proposal.Tree_Node, Proposal.Tree, Proposal.Buffer));

            declare
               Context : constant Completion_Context :=
                 new Ada_Construct_Extractor_Context;
               List : Completion_List;

               It        : Completion_Iterator;
               Construct : Simple_Construct_Information;

               Completion_Proposal : Construct_Completion_Proposal;
            begin
               Context.Buffer := Proposal.Buffer;
               Context.Offset := Parent_Sloc_End.Index;
               Ada_Construct_Extractor_Context (Context.all).File :=
                 Proposal.File;

               Append (Get_Resolver (Proposal).Manager.Contexts, Context);

               List := Get_Initial_Completion_List
                 (Get_Resolver (Proposal).Manager.all,
                  Context,
                  False);

               It := First (List);

               while not At_End (It) loop
                  if Get_Proposal (It)
                  in Construct_Completion_Proposal'Class
                  then
                     Completion_Proposal := Construct_Completion_Proposal
                       (Get_Proposal (It));
                     Completion_Proposal.Params_In_Expression :=
                       Proposal.Params_In_Expression;

                     if not Is_Excluded
                       (Construct_Completion_Resolver
                          (Completion_Proposal.Resolver.all)'Access,
                        (Completion_Proposal.Tree_Node,
                         Completion_Proposal.Tree,
                         Completion_Proposal.Buffer))
                     then
                        Construct := Get_Construct
                          (Construct_Completion_Proposal
                             (Get_Proposal (It)).Tree_Node);

                        if (not Cut_Access
                            or else not
                              Construct.Attributes (Ada_Access_Attribute))
                          and then
                            (not Cut_Subprograms
                             or else
                               Construct.Category not in Subprogram_Category)
                        then
                           Get_Composition
                             (Completion_Proposal,
                              Identifier,
                              1,
                              Is_Partial,
                              Result);
                        end if;
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

            Pop_Excluded_Construct
              (Construct_Completion_Resolver (Proposal.Resolver.all)'Access);
         end if;
      end Handle_Referenced_Entity;

      List : Extensive_List_Pckg.List;

      Proposal_Is_All    : Boolean := False;
   begin
      --  If we are completing a parameter profile, then return all the
      --  possible remaining parameters.

      if Proposal.Profile /= null
        and then Proposal.Profile.Is_In_Profile
      then
         declare
            Param_List : Extensive_List_Pckg.List;

            Param : Construct_Tree_Iterator := Next
              (Proposal.Tree, Proposal.Tree_Node, Jump_Into);

            Actual_Param_It : Integer := 1;
         begin
            --  This first object is the subprogam itself. It will be used to
            --  add the full parameters list in one time.
            Add_To_List
              (Param_List,
               Proposal.Tree_Node,
               False,
               Proposal.File,
               Proposal.Tree,
               Proposal.Buffer,
               Is_In_Profile => True,
               Profile => new Profile_Manager'(Proposal.Profile.all));

            while Param /= Null_Construct_Tree_Iterator
              and then Get_Parent_Scope (Proposal.Tree, Param)
              = Proposal.Tree_Node
              and then Get_Construct (Param).Category = Cat_Parameter
            loop
               if not
                 Proposal.Profile.Parameters (Actual_Param_It).Is_Written
               then
                  Add_To_List
                   (Param_List,
                    Param,
                    False,
                    Proposal.File,
                    Proposal.Tree,
                    Proposal.Buffer,
                    Is_In_Profile => True);
               end if;

               Param := Next (Proposal.Tree, Param, Jump_Over);
               Actual_Param_It := Actual_Param_It + 1;
            end loop;

            Append (Result.List, To_Extensive_List (Param_List));

            return;
         end;
      end if;

      Ada_Tree := Generate_Ada_Construct_Tree
        (Proposal.Tree, Proposal.Buffer);

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

            if Get_Construct (Proposal.Tree_Node).
              Attributes (Ada_Array_Attribute)
              and then Proposal.Params_In_Expression < 1
            then
               --  We know that arrays have at least one dimension, so we don't
               --  complete if none is given. We could do something a bit more
               --  fancy at some point.

               return;
            end if;

            if Get_Construct (Proposal.Tree_Node).
              Attributes (Ada_Access_Attribute)
              and then not Proposal.Is_All
            then
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

            Handle_Referenced_Entity
              (Proposal,
               Proposal_Is_All,
               Get_Construct
                 (Proposal.Tree_Node).Category in Subprogram_Category);

            if Get_Construct (Proposal.Tree_Node).Category
              in Cat_Class .. Cat_Subtype
              and then not Is_Enum_Type (Proposal.Tree, Proposal.Tree_Node)
            then
               declare
                  Child_Iterator : Construct_Tree_Iterator;
                  Type_Iterator  : constant Construct_Tree_Iterator
                    := Proposal.Tree_Node;
               begin
                  Child_Iterator := Next
                    (Proposal.Tree, Type_Iterator, Jump_Into);

                  while
                    Get_Parent_Scope (Proposal.Tree, Child_Iterator)
                    = Type_Iterator
                  loop
                     Add_To_List
                       (List,
                        Child_Iterator,
                        False,
                        Proposal.File,
                        Proposal.Tree,
                        Proposal.Buffer);

                     Child_Iterator := Next
                       (Proposal.Tree, Child_Iterator, Jump_Over);
                  end loop;
               end;
            end if;

            Append (Result.List, To_Extensive_List (List));

         when Cat_Package =>
            Handle_Referenced_Entity (Proposal, False, False);

            declare
               Spec_It         : Construct_Tree_Iterator;
               Body_It         : Construct_Tree_Iterator;
               Private_Spec_Visibility : Boolean;
               Body_Visibility : Boolean;
            begin
               --  ??? Should we check that we are still in the same file ?
               Spec_It := To_Construct_Tree_Iterator
                 (Get_Spec (Proposal.Tree, Ada_Tree, Proposal.Tree_Node));
               Body_It := To_Construct_Tree_Iterator
                 (Get_First_Body
                    (Proposal.Tree, Ada_Tree, Proposal.Tree_Node));

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

               Add_Children_Of
                 (Proposal.Tree, Spec_It, List, Private_Spec_Visibility);

               --  If we have to add the entries in the body if needed

               if Body_Visibility and then Body_It /= Spec_It then
                  Add_Children_Of (Proposal.Tree, Body_It, List, True);
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

      Free (Proposal.Tree, Ada_Tree);
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
   begin
      Free (Proposal.Profile);
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
      Context    : Completion_Context;
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
                             (Resolver,
                              To_Profile_Manager
                                (Get_Public_Tree (Element (C)), Construct_It),
                              Construct_It,
                              Element (C),
                              False,
                              0,
                              Get_Public_Tree (Element (C)),
                              Filter,
                              Get_Buffer (Element (C)),
                              False));
                     end if;
                  end;
               end if;

               C := Next (C);
            end loop;

            Append (Result.List, To_Extensive_List (List));
         end;
      end if;

      if (Filter and All_Visible_Entities) /= 0 and then Offset > 0 then
         declare
            File : Structured_File_Access;
            Tree : aliased Construct_Tree;
         begin
            Tree := To_Construct_Tree (Context.Buffer.all, Ada_Lang);

            if Context.all in Ada_Construct_Extractor_Context then
               if not Is_In_Parents
                 (Ada_Construct_Extractor_Context (Context.all).File,
                  Construct_Completion_Resolver (Resolver.all).Current_File)
               then
                  Tree := Get_Public_Tree (Ada_Tree_Lang, Tree'Access, True);
               end if;

               File := Ada_Construct_Extractor_Context (Context.all).File;
            else
               File := Resolver.Current_File;
               Tree := To_Construct_Tree (Context.Buffer.all, Ada_Lang);
            end if;

            Append (Resolver.Tree_Collection, Tree);

            Append
              (Result.List,
               Construct_Db_Wrapper'
                 (First_File   => File,
                  First_Buffer => Context.Buffer,
                  First_Tree   => Tree,
                  Construct_Db => Resolver.Construct_Db,
                  Name         => new String'(Identifier),
                  Is_Partial   => Is_Partial,
                  Offset       => Offset,
                  Resolver     => Resolver,
                  Filter       => Filter));
         end;
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
      Result         : Construct_Iterator_Wrapper;
      Unit_Construct : Construct_Tree_Iterator;
   begin
      Result.Stage := Initial_File;
      Result.First_File := Db_Construct.First_File;
      Result.Name := Db_Construct.Name;
      Result.Construct_Db := Db_Construct.Construct_Db;
      Result.Resolver := Db_Construct.Resolver;
      Result.Is_Partial := Db_Construct.Is_Partial;
      Result.Filter := Db_Construct.Filter;
      Result.First_File := Db_Construct.First_File;
      Result.First_Tree := Db_Construct.First_Tree;
      Result.First_Buffer := Db_Construct.First_Buffer;

      Unit_Construct := Get_Unit_Construct (Ada_Tree_Lang, Result.First_Tree);

      if Get_Construct (Unit_Construct).Category = Cat_Package
        and then not Get_Construct (Unit_Construct).Is_Declaration
        and then Get_Parent_File (Result.First_File) /= null
        and then Is_Body_Of
          (Result.First_Tree,
           Get_Full_Tree (Get_Parent_File (Result.First_File)))
      then
         Generate_Ada_Construct_Trees
           (Spec_Tree     => Get_Full_Tree
              (Get_Parent_File (Result.First_File)),
            Body_Tree     => Result.First_Tree,
            Spec_Buffer   => Get_Buffer (Get_Parent_File (Result.First_File)),
            Body_Buffer   => Result.First_Buffer,
            Spec_Ada_Tree => Result.First_Spec_Ada_Tree,
            Body_Ada_Tree => Result.First_Ada_Tree);
      else
         Result.First_Ada_Tree := Generate_Ada_Construct_Tree
           (Result.First_Tree, Result.First_Buffer);
      end if;

      Result.Visible_Constructs :=
        new Construct_Tree_Iterator_Array'
          (Get_Visible_Constructs
               (Result.First_Tree,
                Result.First_Ada_Tree,
                Db_Construct.Offset,
                Db_Construct.Name.all,
                Is_Partial => Db_Construct.Is_Partial));

      if Result.Visible_Constructs'Length = 0 then
         Result.Visible_Index := 0;
         Next (Result);
      else
         Result.Visible_Index := Result.Visible_Constructs'First;
      end if;

      if not Is_Valid (Result) then
         Next (Result);
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
            and then not Match
              (It.Name.all,
               Get_Current_Id (It.Db_Iterator),
               False)));
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
              .Is_Declaration)
           and then Is_First_Occurence
             (It.First_Tree,
              It.First_Ada_Tree,
              It.Visible_Constructs (It.Visible_Index));
      elsif It.Stage = Parent_File then
         return It.Current_It /= Null_Construct_Tree_Iterator;
      elsif It.Stage = Database then
         return not Is_In_Parents (Get_File (It.Db_Iterator), It.First_File);
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

               if It.Current_File /= null then
                  if Is_Body_Of
                    (It.First_Tree, Get_Full_Tree (It.Current_File))
                  then
                     It.Is_Current_Spec := True;
                  else
                     It.Is_Current_Spec := False;
                  end if;
               end if;
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

                  if Is_In_Parents
                    (It.Current_File,
                     Construct_Completion_Resolver
                       (It.Resolver.all).Current_File)
                  then
                     It.Current_Tree := Get_Full_Tree (It.Current_File);
                  else
                     It.Current_Tree := Get_Public_Tree (It.Current_File);
                  end if;

                  if It.Is_Current_Spec then
                     Generate_Ada_Construct_Trees
                       (It.Current_Tree, It.First_Tree,
                        Get_Buffer (It.Current_File), It.First_Buffer,
                        It.Current_Ada_Tree, It.Current_Body_Tree);
                  else
                     It.Current_Ada_Tree := Generate_Ada_Construct_Tree
                       (It.Current_Tree, Get_Buffer (It.Current_File));
                  end if;
               end if;

               while It.Current_It /= Null_Construct_Tree_Iterator loop
                  if Get_Construct (It.Current_It).Category = Cat_Package
                    or else Is_Enum_Type (It.Current_Tree, It.Current_It)
                  then
                     It.Current_It :=
                       Next (It.Current_Tree, It.Current_It, Jump_Into);
                  else
                     It.Current_It :=
                       Next (It.Current_Tree, It.Current_It, Jump_Over);
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
                         (It.Current_Tree, It.Current_It)
                         /= Null_Construct_Tree_Iterator
                       and then Is_First_Occurence
                           (It.Current_Tree,
                            It.Current_Ada_Tree,
                            It.Current_It)
                       and then Match
                         (It.Name.all,
                          Constr.Name.all,
                          It.Is_Partial);
                  end;
               end loop;

               if It.Current_It = Null_Construct_Tree_Iterator then
                  if It.Current_Tree /= Null_Construct_Tree then
                     Free (It.Current_Tree, It.Current_Ada_Tree);
                     --  ??? Todo: fix the leak here, we should free the
                     --  current tree even when the iteration is interrupted.
                  end if;

                  if It.Is_Current_Spec then
                     Free (It.First_Tree, It.Current_Body_Tree);
                     It.Is_Current_Spec := False;
                  end if;

                  It.Current_File := Get_Parent_File (It.Current_File);
                  It.Current_It := Null_Construct_Tree_Iterator;
                  It.Current_Tree := Null_Construct_Tree;
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
     (This : Construct_Iterator_Wrapper) return Completion_Proposal'Class
   is
      Proposal  : Construct_Completion_Proposal;
      Full_Cell : Construct_Cell_Access;
   begin
      case This.Stage is
         when Initial_File =>
            Proposal := Construct_Completion_Proposal'
              (Resolver             => This.Resolver,
               Profile              => To_Profile_Manager
                 (This.First_Tree,
                  This.Visible_Constructs (This.Visible_Index)),
               Tree_Node            =>
                  This.Visible_Constructs (This.Visible_Index),
               File                 => This.First_File,
               Is_All               => False,
               Params_In_Expression => 0,
               Tree                 => This.First_Tree,
               Buffer               => This.First_Buffer,
               Filter               => This.Filter,
               Is_In_Profile        => False);

            Full_Cell := Get_Most_Complete_View
                (This.First_Tree,
                 This.First_Ada_Tree,
                 This.Visible_Constructs (This.Visible_Index));

            if Get_Tree (Full_Cell) = This.First_Tree then
               Proposal.Tree_Node := To_Construct_Tree_Iterator (Full_Cell);
            end if;

            return Proposal;
         when Parent_File =>
            Proposal :=
              (Resolver             => This.Resolver,
               Profile              => To_Profile_Manager
                 (Get_Full_Tree (This.Current_File), This.Current_It),
               Tree_Node            => This.Current_It,
               File                 => This.Current_File,
               Tree                 => This.Current_Tree,
               Buffer               => Get_Buffer (This.Current_File),
               Is_All               => False,
               Params_In_Expression => 0,
               Filter               => This.Filter,
               Is_In_Profile        => False);

            Full_Cell := Get_Most_Complete_View
                (This.Current_Tree, This.Current_Ada_Tree, This.Current_It);

            if Get_Tree (Full_Cell) = This.First_Tree then
               Proposal.Tree := Get_Tree (Full_Cell);
               Proposal.Tree_Node := To_Construct_Tree_Iterator (Full_Cell);
               Proposal.Buffer := This.First_Buffer;
               Proposal.File := This.First_File;
            elsif  To_Construct_Tree_Iterator (Full_Cell) /=
              Proposal.Tree_Node
            then
               Proposal.Tree_Node := To_Construct_Tree_Iterator (Full_Cell);
               Proposal.Tree := Get_Tree (Full_Cell);
            end if;

            return Proposal;
         when Database =>
            return Construct_Completion_Proposal'
              (Resolver             => This.Resolver,
               Profile              => To_Profile_Manager
                 (Get_Public_Tree (Get_File (This.Db_Iterator)),
                  Get_Construct (This.Db_Iterator)),
               Tree_Node            =>
                  Get_Construct (This.Db_Iterator),
               File                 => Get_File (This.Db_Iterator),
               Is_All               => False,
               Params_In_Expression => 0,
               Tree                 => Get_Public_Tree
                 (Get_File (This.Db_Iterator)),
               Buffer               => Get_Buffer
                 (Get_File (This.Db_Iterator)),
               Filter               => This.Filter,
               Is_In_Profile        => False);
      end case;
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Iterator_Wrapper) is
   begin
      if This.Current_Tree /= Null_Construct_Tree then
         Free (This.Current_Tree, This.Current_Ada_Tree);
      end if;

      if This.Is_Current_Spec then
         Free (This.First_Tree, This.Current_Body_Tree);
         This.Is_Current_Spec := False;
      end if;
   end Free;

   ------------------------
   -- To_Profile_Manager --
   ------------------------

   function To_Profile_Manager
     (Tree : Construct_Tree; It : Construct_Tree_Iterator)
      return Profile_Manager_Access
   is
      Profile : Profile_Manager_Access;
      Index   : Integer;
   begin
      if Get_Construct (It).Category in Subprogram_Category then
         declare
            Param     : Construct_Tree_Iterator := Next (Tree, It, Jump_Into);
            Nb_Params : Integer := 0;
         begin
            while Param /= Null_Construct_Tree_Iterator
              and then Get_Parent_Scope (Tree, Param) = It
              and then Get_Construct (Param).Category = Cat_Parameter
            loop
               Nb_Params := Nb_Params + 1;

               Param := Next (Tree, Param, Jump_Over);
            end loop;

            Profile := new Profile_Manager (Nb_Params);
            Profile.Case_Sensitive := False;

            Param :=  Next (Tree, It, Jump_Into);

            Index := 1;

            while Param /= Null_Construct_Tree_Iterator
              and then Get_Parent_Scope (Tree, Param) = It
              and then Get_Construct (Param).Category = Cat_Parameter
            loop
               Profile.Parameters (Index).Name :=
                 GNAT.Strings.String_Access (Get_Construct (Param).Name);

               Index := Index + 1;

               Param := Next (Tree, Param, Jump_Over);
            end loop;

            return Profile;
         end;
      else
         return null;
      end if;
   end To_Profile_Manager;

   -----------------------------
   -- Push_Excluded_Construct --
   -----------------------------

   procedure Push_Excluded_Construct
     (Resolver : access Construct_Completion_Resolver;
      Pointer  : Full_Construct_Cell) is
   begin
      Append (Resolver.Excluded_List, Pointer);
   end Push_Excluded_Construct;

   ----------------------------
   -- Pop_Excluded_Construct --
   ----------------------------

   procedure Pop_Excluded_Construct
     (Resolver : access Construct_Completion_Resolver) is
   begin
      Delete_Last (Resolver.Excluded_List);
   end Pop_Excluded_Construct;

   -----------------
   -- Is_Excluded --
   -----------------

   function Is_Excluded
     (Resolver : access Construct_Completion_Resolver;
      Pointer  : Full_Construct_Cell) return Boolean
   is
      It : Construct_List.Cursor := First (Resolver.Excluded_List);
      Relation : General_Order;
   begin
      while It /= Construct_List.No_Element loop
         Relation := Compare_Entities
           (Lang         => Ada_Tree_Lang,
            Left_Iter    => Element (It).It,
            Right_Iter   => Pointer.It,
            Left_Tree    => Element (It).Tree,
            Right_Tree   => Pointer.Tree,
            Left_Buffer  => Element (It).Buffer,
            Right_Buffer => Pointer.Buffer);

         if Relation = Equals or else Relation = Equivalent then
            return True;
         end if;

         It := Next (It);
      end loop;

      return False;
   end Is_Excluded;

end Completion.Ada.Constructs_Extractor;
