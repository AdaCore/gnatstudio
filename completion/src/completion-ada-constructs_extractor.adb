-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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

with Ada_Semantic_Tree.Visibility;      use Ada_Semantic_Tree.Visibility;
with Ada_Semantic_Tree.Dependency_Tree; use Ada_Semantic_Tree.Dependency_Tree;
with Basic_Types;                       use Basic_Types;
with Glib.Unicode;                      use Glib.Unicode;
with GNAT.Strings;
with Language.Tree.Ada;                 use Language.Tree.Ada;

package body Completion.Ada.Constructs_Extractor is

   use Completion_List_Pckg;
   use Completion_List_Extensive_Pckg;
   use Extensive_List_Pckg;

   Resolver_ID : constant String := "CNST_ADA";

   ---------------------------------------
   -- New_Construct_Completion_Resolver --
   ---------------------------------------

   function New_Construct_Completion_Resolver
     (Construct_Db   : Construct_Database_Access;
      Current_File   : Virtual_File;
      Current_Buffer : GNAT.Strings.String_Access)
      return Completion_Resolver_Access
   is
      Resolver_Acc : constant Completion_Resolver_Access :=
        new Construct_Completion_Resolver;
      Resolver : Construct_Completion_Resolver renames
        Construct_Completion_Resolver (Resolver_Acc.all);
   begin
      Resolver.Manager := null;
      Resolver.Construct_Db := Construct_Db;
      Resolver.Current_File := Get_Or_Create
        (Construct_Db, Current_File, Ada_Tree_Lang);
      Resolver.Current_Buffer := Current_Buffer;

      return Resolver_Acc;
   end New_Construct_Completion_Resolver;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (Left  : Stored_Construct_Completion_Proposal;
      Right : Stored_Proposal'Class)
      return Boolean
   is
   begin
      return Right in Stored_Construct_Completion_Proposal'Class
        and then
      Stored_Construct_Completion_Proposal (Right).Persistent_Entity
        = Left.Persistent_Entity;
   end Equal;

   --------------------------
   -- From_Stored_Proposal --
   --------------------------

   overriding function From_Stored_Proposal
     (Stored  : Stored_Construct_Completion_Proposal;
      Manager : Completion_Manager_Access;
      Context : Completion_Context)
      return Completion_Proposal_Access
   is
      pragma Unreferenced (Context);

      Result : constant Completion_Proposal_Access :=
        new Construct_Completion_Proposal;

      Constr_Result : Construct_Completion_Proposal renames
        Construct_Completion_Proposal (Result.all);

      Entity : Entity_Access;
   begin
      if not Exists (Stored.Persistent_Entity) then
         return null;
      end if;

      Entity := To_Entity_Access (Stored.Persistent_Entity);

      Constr_Result.Tree_Node := To_Construct_Tree_Iterator (Entity);
      Constr_Result.File := Get_File (Entity);
      Constr_Result.Is_All := Stored.Is_All;
      Constr_Result.Is_In_Call := Stored.Is_In_Call;
      Constr_Result.Resolver := Get_Resolver (Manager, Resolver_ID);

      if Constr_Result.Actual_Params /= null then
         Constr_Result.Actual_Params :=
           new Actual_Parameter_Resolver'(Stored.Actual_Params.all);
      end if;

      return Result;
   end From_Stored_Proposal;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Stored : in out Stored_Construct_Completion_Proposal)
   is
   begin
      Unref (Stored.Persistent_Entity);
   end Free;

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : Construct_Completion_Proposal) return Completion_Id
   is
      Id_Length : Integer := 0;

      It : Construct_Tree_Iterator := Proposal.Tree_Node;
      Construct : access Simple_Construct_Information;
   begin
      while It /= Null_Construct_Tree_Iterator loop
         Construct := Get_Construct (It);

         if Construct.Name /= null then
            Id_Length := Id_Length + Construct.Name'Length + 1;
         end if;

         It := Get_Parent_Scope (Get_Tree (Proposal.File), It);
      end loop;

      if Proposal.Is_All then
         Id_Length := Id_Length + 4;
      end if;

      declare
         Id        : String (1 .. Id_Length);
         Index     : Integer := Id'Length;
      begin
         It := Proposal.Tree_Node;

         while It /= Null_Construct_Tree_Iterator loop
            Construct := Get_Construct (It);

            if Construct.Name /= null then
               Id (Index - (Construct.Name'Length + 1 - 1)
                   .. Index) :=
                 Construct.Name.all & ".";
               Index := Index - (Construct.Name'Length + 1);
            end if;

            It := Get_Parent_Scope (Get_Tree (Proposal.File), It);
         end loop;

         if Proposal.Is_All then
            Id (Index - 3 .. Index) := "all.";
            Index := Index - 4;
         end if;

         Construct := Get_Construct (Proposal.Tree_Node);

         return
           (Id'Length - 1,
            Resolver_ID,
            Id (1 .. Id'Length  - 1), --  -1 to Remove the last dot
            Get_File_Path (Proposal.File),
            Construct.Sloc_Start.Line,
            Construct.Sloc_Start.Column);
      end;
   end To_Completion_Id;

   --------------------
   -- Get_Completion --
   --------------------

   overriding function Get_Completion
     (Proposal : Construct_Completion_Proposal) return UTF8_String
   is
      Comma         : Boolean := False;
      Max_Size_Name : Integer := 0;
   begin
      if Proposal.Is_In_Call then
         return Get_Label (Proposal) & " => ";
      elsif Proposal.Actual_Params /= null then
         declare
            Missing_Formals : constant Formal_Parameter_Array :=
              Get_Missing_Formals (Proposal.Actual_Params.all);
            Construct : access Simple_Construct_Information;
         begin
            if Missing_Formals'Length > 0 then
               for J in Missing_Formals'Range loop
                  Construct := Get_Construct (Missing_Formals (J));

                  if Construct.Name'Length > Max_Size_Name then
                     Max_Size_Name := Construct.Name'Length;
                  end if;
               end loop;

               declare
                  Buffer : String
                    (1 .. Missing_Formals'Length
                     * (Max_Size_Name + 6) - 1) :=
                    (others => ' ');

                  Index : Integer := 1;
               begin
                  for J in Missing_Formals'Range loop
                     Construct := Get_Construct (Missing_Formals (J));

                     if Comma then
                        Buffer (Index .. Index + 1) := "," & ASCII.LF;
                        Index := Index + 2;
                     end if;

                     Buffer
                       (Index ..
                          Index
                        + Construct.Name'Length - 1) := Construct.Name.all;

                     Index := Index + Max_Size_Name;

                     Buffer (Index .. Index + 3) := " => ";

                     Index := Index + 4;

                     Comma := True;
                  end loop;

                  Buffer (Buffer'Last .. Buffer'Last) := ")";

                  return Buffer;
               end;
            end if;
         end;

         return "";
      else
         return Get_Label (Proposal);
      end if;
   end Get_Completion;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Proposal : Construct_Completion_Proposal) return UTF8_String
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.Tree_Node);
   begin
      if Proposal.Actual_Params /= null then
         return "params of " & Construct.Name.all;
      else
         if Proposal.Is_All then
            return "all";
         elsif Construct.Category = Cat_Package
           or else Construct.Category = Cat_Function
           or else Construct.Category = Cat_Procedure
         then
            declare
               Id : constant Composite_Identifier := To_Composite_Identifier
                 (Construct.Name.all);
            begin
               return Get_Item (Id, Length (Id));
            end;
         else
            return Construct.Name.all;
         end if;
      end if;
   end Get_Label;

   ----------------------
   -- Get_Caret_Offset --
   ----------------------

   overriding function Get_Caret_Offset
     (Proposal : Construct_Completion_Proposal)
      return Basic_Types.Character_Offset_Type
   is
      Max_Param_Length     : Glong := 0;
      Current_Param_Length : Glong := 0;
      Construct            : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.Tree_Node);
   begin
      if Proposal.Actual_Params /= null
        and then Construct.Category not in Data_Category
      then
         declare
            Missing_Formals : constant Formal_Parameter_Array :=
              Get_Missing_Formals (Proposal.Actual_Params.all);
         begin
            if Missing_Formals'Length > 0 then
               for J in Missing_Formals'Range loop
                  Current_Param_Length :=
                    UTF8_Strlen
                      (Get_Construct (Missing_Formals (J)).Name.all);

                  if Current_Param_Length > Max_Param_Length then
                     Max_Param_Length := Current_Param_Length;
                  end if;
               end loop;

               return Basic_Types.Character_Offset_Type (Max_Param_Length) + 4;
               --  4 is for the " => " string.
            else
               return 0;
            end if;
         end;
      else
         return Basic_Types.Character_Offset_Type
          (UTF8_Strlen
               (Get_Completion (Completion_Proposal'Class (Proposal))));
      end if;
   end Get_Caret_Offset;

   --------------
   -- Get_Type --
   --------------

   overriding function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.Tree_Node);
   begin
      if Proposal.Is_All then
         return Cat_Literal;
      elsif Proposal.Actual_Params /= null
        and then Construct.Category not in Data_Category
      then
         return Cat_Unknown;
      else
         return Construct.Category;
      end if;
   end Get_Category;

   --------------------
   -- Get_Visibility --
   --------------------

   overriding function Get_Visibility
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

   overriding function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      return Get_Documentation
        (Ada_Tree_Lang,
         Get_Buffer (Proposal.File).all,
         Get_Tree (Proposal.File),
         Proposal.Tree_Node);
   end Get_Documentation;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Proposal : Construct_Completion_Proposal) return File_Location
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.Tree_Node);
   begin
      return (Get_File_Path (Proposal.File),
              Construct.Sloc_Start.Line,
              Basic_Types.Visible_Column_Type (Construct.Sloc_Start.Column));
   end Get_Location;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Proposal   : Construct_Completion_Proposal;
      Context    : Completion_Context;
      Offset     : Integer) return Boolean
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.Tree_Node);

      Ada_Context : Ada_Completion_Context;

      Entity     : Entity_Access;
      File       : Structured_File_Access;
      Resolver   : Construct_Completion_Resolver renames
        Construct_Completion_Resolver (Proposal.Resolver.all);
   begin
      if Context.all not in Ada_Completion_Context'Class then
         return False;
      end if;

      Ada_Context := Ada_Completion_Context (Context.all);

      if Construct.Name = null then
         return False;
      elsif Construct.Category = Cat_Field then
         return False;
      else
         declare
            Name : constant Composite_Identifier :=
              To_Composite_Identifier
                (Construct.Name.all);
         begin
            if not Match
              (Get_Name
                 (Ada_Context.Expression,
                  Token_List.Data
                    (Token_List.Last (Ada_Context.Expression.Tokens))),
               Get_Item (Name, Length (Name)),
               Token_List.Length (Ada_Context.Expression.Tokens) = 1)
            then
               return False;
            end if;

            Entity := To_Entity_Access (Proposal.File, Proposal.Tree_Node);

            if Is_Public_Library_Visible (Entity) then
               --  ??? We should probably check with / use visibility if
               --  needed...
               return True;
            end if;

            File := Resolver.Current_File;

            return Is_Locally_Visible
              (File     => File,
               Offset   => Offset,
               Entity   => Entity,
               Use_Wise => True);
            --  ??? Use-wise should be set according to preferences (context?)
         end;
      end if;
   end Match;

   ------------------------
   -- To_Stored_Proposal --
   ------------------------

   overriding function To_Stored_Proposal
     (Proposal : Construct_Completion_Proposal)
      return Stored_Proposal_Access
   is
      Result        : constant Stored_Proposal_Access :=
        new Stored_Construct_Completion_Proposal;
      Constr_Result : Stored_Construct_Completion_Proposal renames
        Stored_Construct_Completion_Proposal (Result.all);
   begin
      Constr_Result.Persistent_Entity := To_Entity_Persistent_Access
        (To_Entity_Access (Proposal.File, Proposal.Tree_Node));
      Ref (Constr_Result.Persistent_Entity);
      Constr_Result.Is_All := Proposal.Is_All;
      Constr_Result.Is_In_Call := Proposal.Is_In_Call;

      if Proposal.Actual_Params /= null then
         Constr_Result.Actual_Params :=
           new Actual_Parameter_Resolver'(Proposal.Actual_Params.all);
      end if;

      return Result;
   end To_Stored_Proposal;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Proposal : in out Construct_Completion_Proposal) is
   begin
      Free (Proposal.Actual_Params);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Construct_Db_Wrapper) is
   begin
      Free (This.List);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Tree_Iterator_Array_Access) is
      procedure Internal_Free is new
        Standard.Ada.Unchecked_Deallocation
          (Construct_Tree_Iterator_Array,
           Construct_Tree_Iterator_Array_Access);
   begin
      Internal_Free (This);
   end Free;

   -----------------------
   -- Get_Possibilities --
   -----------------------

   overriding procedure Get_Completion_Root
     (Resolver : access Construct_Completion_Resolver;
      Offset   : Integer;
      Context  : Completion_Context;
      Result   : in out Completion_List)
   is
      Visibility : Visibility_Context;
      Expression : Parsed_Expression;
   begin
      if Context.all in Ada_Completion_Context then
         Expression := Ada_Completion_Context (Context.all).Expression;

         if Token_List.Data
           (Token_List.Last (Expression.Tokens)).Tok_Type = Tok_Identifier
         then
            Result.Searched_Identifier := new String'
              (Get_Name
                 (Expression,
                  Token_List.Data (Token_List.Last (Expression.Tokens))));
         else
            Result.Searched_Identifier := new String'("");
         end if;
      else
         Expression := Null_Parsed_Expression;
      end if;

      Visibility.Offset := Offset;
      Visibility.Filter := Everything;
      Visibility.File := Resolver.Current_File;
      Visibility.Min_Visibility_Confidence := Public_Library_Visible;

      Append
        (Result.List,
         Construct_Db_Wrapper'
           (Visibility,
            Completion_Resolver_Access (Resolver),
            Find_Declarations
              ((From_File, Resolver.Current_File, Offset),
               From_Visibility           => Visibility,
               Expression                => Expression,
               Categories                => Null_Category_Array,
               Is_Partial                => True)));

      Free (Expression);
   end Get_Completion_Root;

   ------------
   -- Get_Id --
   ------------

   overriding function Get_Id
     (Resolver : Construct_Completion_Resolver) return String
   is
      pragma Unreferenced (Resolver);
   begin
      return Resolver_ID;
   end Get_Id;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Construct_Completion_Resolver) is
      pragma Unreferenced (This);
   begin
      null;
   end Free;

   -----------
   -- First --
   -----------

   overriding function First
     (Db_Construct : Construct_Db_Wrapper)
      return Completion_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      Result : Construct_Iterator_Wrapper;
   begin
      Result.Resolver := Completion_Resolver_Access (Db_Construct.Resolver);
      Result.Iter := First (Db_Construct.List);
      Result.Context := Db_Construct.Context;

      if Is_Valid (Result) and then not At_End (Result) then
         Result.Current_Decl := Get_View (Result.Iter);
      end if;

      return Result;
   end First;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (It : Construct_Iterator_Wrapper) return Boolean is
   begin
      return At_End (It.Iter);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid
     (It : Construct_Iterator_Wrapper) return Boolean
   is
      pragma Unreferenced (It);
   begin
      return True;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Construct_Iterator_Wrapper) is
   begin
      if Get_Actual_Parameters (It.Current_Decl) /= null
        or else It.Params_Array /= null
      then
         if It.Params_Array = null then
            It.Params_Array := new Formal_Parameter_Array'
              (Get_Missing_Formals
                 (Get_Actual_Parameters (It.Current_Decl).all));

            It.Params_It := 0;
         end if;

         It.Params_It := It.Params_It + 1;

         if It.Params_It > It.Params_Array'Last then
            Free (It.Params_Array);
         end if;
      end if;

      if It.Params_Array = null then
         Free (It.Current_Decl);
         Next (It.Iter);

         if Is_Valid (It) and then not At_End (It) then
            It.Current_Decl := Get_View (It.Iter);
         else
            It.Current_Decl := Null_Declaration_View;
         end if;
      else
         It.Current_Decl := To_Declaration
              (To_Entity_Access
                 (Get_File (Get_Entity (It.Iter)),
                  Get_Construct_Tree_Iterator
                    (It.Params_Array (It.Params_It))));
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This : Construct_Iterator_Wrapper) return Completion_Proposal'Class
   is
      Entity : constant Entity_Access := Get_Entity (This.Current_Decl);
      Actuals : Actual_Parameter_Resolver_Access;
   begin
      Actuals := Get_Actual_Parameters (This.Current_Decl);

      if Actuals /= null then
         Actuals := new Actual_Parameter_Resolver'(Actuals.all);
      end if;

      return Construct_Completion_Proposal'
        (Resolver      => This.Resolver,
         Tree_Node     => To_Construct_Tree_Iterator (Entity),
         File          => Get_File (This.Current_Decl),
         Is_All        => Is_All (This.Current_Decl),
         Actual_Params => Actuals,
         Is_In_Call    => This.Params_Array /= null);
   end Get;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Construct_Iterator_Wrapper) is
   begin
      Free (This.Params_Array);
      Free (This.Iter);
      Free (This.Current_Decl);
   end Free;

end Completion.Ada.Constructs_Extractor;
