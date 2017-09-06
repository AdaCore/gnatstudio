------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

with Language.Ada;                      use Language.Ada;
with Language.Profile_Formaters;        use Language.Profile_Formaters;
with Ada_Semantic_Tree.Visibility;      use Ada_Semantic_Tree.Visibility;
with Ada_Semantic_Tree.Dependency_Tree; use Ada_Semantic_Tree.Dependency_Tree;
with Glib.Unicode;                      use Glib.Unicode;
with GNAT.Strings;
with GNATCOLL.Symbols;               use GNATCOLL.Symbols;
with Ada_Semantic_Tree.Declarations; use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Generics;     use Ada_Semantic_Tree.Generics;
with Ada_Semantic_Tree.Lang;         use Ada_Semantic_Tree.Lang;

package body Completion.Ada.Constructs_Extractor is

   use Completion_List_Pckg;

   Resolver_ID : constant String := "CNST_ADA";

   procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
     (Actual_Parameter_Resolver, Actual_Parameter_Resolver_Access);

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
        (Construct_Db, Current_File);
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

      Constr_Result.View := To_Declaration (Entity);
      Set_Is_All (Constr_Result.View, Stored.Is_All);

      Constr_Result.Is_In_Call := Stored.Is_In_Call;
      Constr_Result.Resolver := Get_Resolver (Manager, Resolver_ID);

      if Constr_Result.Actual_Params /= null then
         Constr_Result.Actual_Params :=
           new Actual_Parameter_Resolver'(Stored.Actual_Params.all);
      end if;

      return Result;
   end From_Stored_Proposal;

   --------------
   -- Is_Valid --
   --------------

   overriding
   function Is_Valid
     (Stored : Stored_Construct_Completion_Proposal) return Boolean
   is
   begin
      return Exists (Stored.Persistent_Entity);
   end Is_Valid;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Stored : in out Stored_Construct_Completion_Proposal)
   is
   begin
      Unref (Stored.Persistent_Entity);
      Unchecked_Free (Stored.Actual_Params);
   end Free;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (Proposal : Construct_Completion_Proposal) return UTF8_String is
   begin
      return Proposal.View.Get_Documentation;
   end Get_Documentation;

   -------------------
   -- Is_Accessible --
   -------------------

   overriding
   function Is_Accessible
     (Proposal : Construct_Completion_Proposal)
      return Boolean
   is (Proposal.View.Is_Accessible);

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : Construct_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id
   is
      pragma Unreferenced (Db);

      Id_Length : Integer := 0;

      Entity : constant Entity_Access := Proposal.View.Get_Entity;
      It : Construct_Tree_Iterator := To_Construct_Tree_Iterator
        (Proposal.View);
      Construct : access Simple_Construct_Information;
   begin
      if It = Null_Construct_Tree_Iterator then
         --  In this case, we're on a view that doesn't corresponds to anything
         --  in the source, just use the name for id.

         declare
            Name : constant UTF8_String := Proposal.View.Get_Name;
         begin
            return
              (Id_Length   => Name'Length,
               Id          => Name,
               Resolver_ID => Resolver_ID,
               others      => <>);
         end;
      end if;

      while It /= Null_Construct_Tree_Iterator loop
         Construct := Get_Construct (It);

         if Construct.Name /= No_Symbol then
            Id_Length := Id_Length + Get (Construct.Name)'Length + 1;
         end if;

         It := Get_Parent_Scope (Get_Tree (Get_File (Entity)), It);
      end loop;

      if Is_All (Proposal.View) then
         Id_Length := Id_Length + 4;
      end if;

      declare
         Id        : String (1 .. Id_Length);
         Index     : Integer := Id'Length;
      begin
         It := To_Construct_Tree_Iterator (Proposal.View);

         while It /= Null_Construct_Tree_Iterator loop
            Construct := Get_Construct (It);

            if Construct.Name /= No_Symbol then
               Id (Index - (Get (Construct.Name)'Length + 1 - 1)
                   .. Index) :=
                 Get (Construct.Name).all & ".";
               Index := Index - (Get (Construct.Name)'Length + 1);
            end if;

            It := Get_Parent_Scope (Get_Tree (Get_File (Entity)), It);
         end loop;

         if Is_All (Proposal.View) then
            Id (Index - 3 .. Index) := "all.";
            Index := Index - 4;
         end if;

         Construct := Get_Construct (Proposal.View);

         return
           (Id'Length - 1,
            Resolver_ID,
            Id (1 .. Id'Length  - 1), --  -1 to Remove the last dot
            Get_File_Path (Get_File (Entity)),
            Construct.Sloc_Entity.Line,
            Construct.Sloc_Entity.Column);
      end;
   end To_Completion_Id;

   --------------------
   -- Get_Completion --
   --------------------

   overriding function Get_Completion
     (Proposal : Construct_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return UTF8_String
   is
      Comma         : Boolean := False;
      Max_Size_Name : Integer := 0;
   begin
      if Proposal.Is_In_Call then
         return Get_Label (Proposal, Db) & " => ";
      elsif Proposal.Actual_Params /= null then
         declare
            Missing_Formals : constant Formal_Parameter_Array :=
              Get_Missing_Formals (Proposal.Actual_Params.all);
            Construct : access Simple_Construct_Information;
            Aggregate_Extension : Entity_Access;
            Aggregate_Length : Integer := 0;
         begin
            if Missing_Formals'Length > 0 then
               Aggregate_Extension :=
                 Get_Aggregate_Parent
                   (Get_Profile (Proposal.Actual_Params.all));

               if Aggregate_Extension /= Null_Entity_Access then
                  Aggregate_Length :=
                    Get (Get_Construct (Aggregate_Extension).Name)'Length + 6;
               end if;

               for J in Missing_Formals'Range loop
                  Construct := Get_Construct (Missing_Formals (J));

                  if Get (Construct.Name)'Length > Max_Size_Name then
                     Max_Size_Name := Get (Construct.Name)'Length;
                  end if;
               end loop;

               declare
                  Buffer : String
                    (1 .. Missing_Formals'Length
                     * (Max_Size_Name + 6) + Aggregate_Length - 1) :=
                    (others => ' ');

                  Index : Integer := 1;
               begin
                  if Aggregate_Extension /= Null_Entity_Access then
                     Buffer
                       (Index .. Index + Aggregate_Length - 1) :=
                       Get (Get_Construct (Aggregate_Extension).Name).all
                       & " with" & ASCII.LF;

                     Index := Index + Aggregate_Length;
                  end if;

                  for J in Missing_Formals'Range loop
                     Construct := Get_Construct (Missing_Formals (J));

                     if Comma then
                        Buffer (Index .. Index + 1) := "," & ASCII.LF;
                        Index := Index + 2;
                     end if;

                     Buffer (Index .. Index + Get (Construct.Name)'Length - 1)
                       := Get (Construct.Name).all;

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
      elsif Proposal.From_Accept_Statement then
         declare
            Formater : aliased Text_Profile_Formater;
         begin
            Ada_Tree_Lang.Get_Profile
              (Entity       => Get_Entity (Proposal.View),
               Formater     => Formater'Access);
            return Get_Label (Proposal, Db)
              & " " & Formater.Get_Text
              & " do"
              & ASCII.LF
              & "null;"
              & ASCII.LF
              & "end " &  Get_Label (Proposal, Db) & ";";
         end;
      else
         return Get_Label (Proposal, Db);
      end if;
   end Get_Completion;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Proposal : Construct_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class) return UTF8_String
   is
      pragma Unreferenced (Db);
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.View);
   begin
      if Proposal.View.Get_Entity = Null_Entity_Access then
         return Proposal.View.Get_Name;
      elsif Proposal.Actual_Params /= null then
         return "params of " & Get (Construct.Name).all;
      elsif Is_All (Proposal.View) then
         return "all";
      elsif Construct.Category = Cat_Package
        or else Construct.Category = Cat_Function
        or else Construct.Category = Cat_Procedure
      then
         declare
            Id : constant Composite_Identifier := To_Composite_Identifier
              (Get (Construct.Name).all);
         begin
            return Get_Item (Id, Length (Id));
         end;
      else
         return Get (Construct.Name).all;
      end if;
   end Get_Label;

   ----------------------
   -- Get_Caret_Offset --
   ----------------------

   overriding function Get_Caret_Offset
     (Proposal : Construct_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return Basic_Types.Character_Offset_Type
   is
      use Glib;

      Max_Param_Length     : Glong := 0;
      Current_Param_Length : Glong := 0;
      Construct            : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.View);
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
                      (Get (Get_Construct (Missing_Formals (J)).Name).all);

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
               (Get_Completion (Completion_Proposal'Class (Proposal), Db)));
      end if;
   end Get_Caret_Offset;

   ------------------
   -- Get_Category --
   ------------------

   overriding function Get_Category
     (Proposal : Construct_Completion_Proposal) return Language_Category
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.View);
   begin
      if Proposal.View.Get_Entity = Null_Entity_Access then
         return Proposal.View.Get_Category;
      elsif Is_All (Proposal.View) then
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
      if Proposal.View.Get_Entity = Null_Entity_Access then
         return Visibility_Public;
      elsif Is_All (Proposal.View) then
         return Visibility_Public;
      else
         return Get_Construct (Proposal.View).Visibility;
      end if;
   end Get_Visibility;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Proposal : Construct_Completion_Proposal;
      Db : access Xref.General_Xref_Database_Record'Class)
      return File_Location
   is
      pragma Unreferenced (Db);
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.View);
      Entity : constant Entity_Access := Proposal.View.Get_Entity;
   begin
      if Entity /= Null_Entity_Access then
         return (Get_File_Path (Get_File (Entity)),
                 Construct.Sloc_Entity.Line,
                 Basic_Types.Visible_Column_Type
                   (Construct.Sloc_Entity.Column));
      else
         return (No_File, 0, 0);
      end if;
   end Get_Location;

   -----------
   -- Match --
   -----------

   overriding function Match
     (Proposal   : Construct_Completion_Proposal;
      Context    : Completion_Context;
      Offset     : String_Index_Type) return Boolean
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Proposal.View);

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

      if Construct.Name = No_Symbol then
         return False;
      elsif Construct.Category = Cat_Field then
         return False;
      else
         declare
            Name : constant Composite_Identifier :=
              To_Composite_Identifier (Get (Construct.Name).all);
         begin
            if not Match
              (Get_Name
                 (Ada_Context.Expression,
                  Ada_Context.Expression.Tokens.Last_Element),
               Get_Item (Name, Length (Name)),
               Token_List.Length (Ada_Context.Expression.Tokens) = 1)
            then
               return False;
            end if;

            Entity := Get_Entity (Proposal.View);

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
      if Get_Entity (Proposal.View) = Null_Entity_Access then
         return null;
      end if;

      Constr_Result.Persistent_Entity := To_Entity_Persistent_Access
        (Get_Entity (Proposal.View));
      Constr_Result.Is_All := Is_All (Proposal.View);
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
      Unchecked_Free (Proposal.Actual_Params);
      if Proposal.Should_Free_View then
         Free (Proposal.View);
      end if;
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   overriding function Deep_Copy
     (Proposal : Construct_Completion_Proposal)
      return Completion_Proposal'Class
   is
      Result : Construct_Completion_Proposal;
   begin
      Result :=
        (Resolver              => Proposal.Resolver,
         Actual_Params         => null,
         From_Accept_Statement => Proposal.From_Accept_Statement,
         View                  => Deep_Copy (Proposal.View),
         Is_In_Call            => Proposal.Is_In_Call,
         Should_Free_View      => True);
      if Proposal.Actual_Params /= null then
         Result.Actual_Params := new Actual_Parameter_Resolver'
           (Proposal.Actual_Params.all);
      end if;
      return Construct_Completion_Proposal'(Result);
   end Deep_Copy;

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

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver : access Construct_Completion_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List)
   is
      Visibility_Accessible : constant Visibility_Context :=
        (Offset => Offset,
         Filter => Everything,
         File   => Resolver.Current_File,
         Min_Visibility_Confidence => With_Visible);

      Visibility_Unreachable : constant Visibility_Context :=
        (Offset => Offset,
         Filter => Everything,
         File   => Resolver.Current_File,
         Min_Visibility_Confidence => Public_Library_Visible);

      Expression : Parsed_Expression;
   begin
      if Context.all in Ada_Completion_Context then
         Expression := Ada_Completion_Context (Context.all).Expression;

         if Expression.Tokens.Last_Element.Tok_Type = Tok_Identifier
         then
            Result.Searched_Identifier := new String'
              (Get_Name
                 (Expression,
                  Expression.Tokens.Last_Element));
         else
            Result.Searched_Identifier := new String'("");
         end if;
      else
         Expression := Null_Parsed_Expression;
      end if;

      Append
        (Result.List,
         Construct_Db_Wrapper'
           (Visibility_Accessible,
            Completion_Resolver_Access (Resolver),
            Find_Declarations
              ((From_File,
                Null_Instance_Info,
                Resolver.Current_File,
                Offset),
               From_Visibility => Visibility_Accessible,
               Expression      => Expression,
               Filter          => Null_Filter,
               Is_Partial      => True),
            Expression /= Null_Parsed_Expression
            and then Expression.Tokens.First_Element.Tok_Type = Tok_Accept));

      Append
        (Result.List,
         Construct_Db_Wrapper'
           (Visibility_Unreachable,
            Completion_Resolver_Access (Resolver),
            Find_Declarations
              ((From_File,
                Null_Instance_Info,
                Resolver.Current_File,
                Offset),
               From_Visibility => Visibility_Unreachable,
               Expression      => Expression,
               Filter          => Null_Filter,
               Is_Partial      => True),
            Expression /= Null_Parsed_Expression
            and then Expression.Tokens.First_Element.Tok_Type = Tok_Accept));

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
      Result.Resolver := Db_Construct.Resolver;
      Result.Iter := First (Db_Construct.List);
      Result.Context := Db_Construct.Context;
      Result.From_Accept_Statement := Db_Construct.From_Accept_Statement;

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
            It.Current_Decl := Null_Entity_View;
         end if;
      else
         --  We want to propagate the visibility of the parent construct
         --  to its arguments
         declare
            Entity : Entity_View;
         begin
            Entity := Get_View (It.Iter);
            It.Current_Decl := To_Declaration
              (To_Entity_Access (It.Params_Array (It.Params_It)),
               Is_Accessible => Entity.Is_Accessible);
            Free (Entity);
         end;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (This : in out Construct_Iterator_Wrapper)
      return Completion_Proposal'Class
   is
      Actuals : Actual_Parameter_Resolver_Access;
   begin
      if This.Proposal_Computed then
         Free (This.Proposal);
      end if;

      Actuals := Get_Actual_Parameters (This.Current_Decl);

      if Actuals /= null then
         Actuals := new Actual_Parameter_Resolver'(Actuals.all);
      end if;

      This.Proposal_Computed := True;
      This.Proposal := Construct_Completion_Proposal'
        (Resolver              => This.Resolver,
         View                  => Deep_Copy (This.Current_Decl),
         Actual_Params         => Actuals,
         Is_In_Call            => This.Params_Array /= null,
         From_Accept_Statement => This.From_Accept_Statement,
         Should_Free_View      => True);
      return This.Proposal;
   end Get;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Construct_Iterator_Wrapper) is
   begin
      Free (This.Params_Array);
      Free (This.Iter);
      Free (This.Current_Decl);
      if This.Proposal_Computed then
         Free (This.Proposal);
         This.Proposal_Computed := False;
      end if;
   end Free;

end Completion.Ada.Constructs_Extractor;
