------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Language.Cpp;     use Language.Cpp;
with Xref;             use Xref;

package body Completion.C.Constructs_Extractor is
   use Completion_List_Pckg;
   use Completion_List_Extensive_Pckg;
   use Extensive_List_Pckg;

   Resolver_ID : constant String := "CNST_C  ";

   function To_Language_Category
     (Db : access General_Xref_Database_Record'Class;
      E   : General_Entity) return Language_Category;
   --  Make a simple association between entity categories and construct
   --  categories. This association is known to be inaccurate.

   -----------------------------------------
   -- New_C_Construct_Completion_Resolver --
   -----------------------------------------

   function New_C_Construct_Completion_Resolver
     (Kernel       : Kernel_Handle;
      Current_File : Virtual_File) return Completion_Resolver_Access
   is
      pragma Unreferenced (Current_File);
   begin
      return
        new Construct_Completion_Resolver'
              (Manager     => null,
               Kernel      => Kernel);
   end New_C_Construct_Completion_Resolver;

   -------------------------
   -- Get_Completion_Root --
   -------------------------

   overriding procedure Get_Completion_Root
     (Resolver : access Construct_Completion_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List)
   is
      pragma Unreferenced (Offset);

      Db : constant General_Xref_Database := Resolver.Kernel.Databases;

      procedure Add_Proposal
        (To_List : in out Extensive_List_Pckg.List;
         E       : General_Entity);
      --  Append to the list To_List the proposal E

      procedure Add_Scope_Proposals
        (To_List      : in out Extensive_List_Pckg.List;
         Scope        : General_Entity;
         Prefix_Token : Token_Record);
      --  Append to To_List all the proposals defined in Scope which match
      --  the prefix available in Prefix_Token. If Prefix_Token is not an
      --  identifier then all the entities defined in Scope are appended
      --  to To_List.

      ------------------
      -- Add_Proposal --
      ------------------

      procedure Add_Proposal
        (To_List : in out Extensive_List_Pckg.List;
         E       : General_Entity)
      is
         Name     : aliased constant String := Db.Get_Name (E);
         Proposal : C_Completion_Proposal;

      begin
         Proposal :=
           (Resolver      => Resolver,
            Name          => new String'(Name),
            Category      => To_Language_Category (Db, E),
            Entity_Info   => E,
            With_Params   => False,
            Is_Param      => False);

         Append (To_List, Proposal);
      end Add_Proposal;

      procedure Add_Proposal_With_Params
        (To_List : in out Extensive_List_Pckg.List;
         E       : General_Entity);
      --  Append to the list To_List one proposal containing all the
      --  parameters needed to call E. In addition, in order to provide the
      --  same behavior of Ada completions, one proposal per parameter is
      --  added to To_List.

      function Is_Self_Referenced_Type
        (E : General_Entity) return Boolean;
      --  Return true if the type of E is itself

      ------------------------------
      -- Add_Proposal_With_Params --
      ------------------------------

      procedure Add_Proposal_With_Params
        (To_List : in out Extensive_List_Pckg.List;
         E       : General_Entity)
      is
         Name     : aliased constant String := Db.Get_Name (E);
         Proposal : C_Completion_Proposal;

      begin
         Proposal :=
           (Resolver      => Resolver,
            Name          => new String'(Name),
            Category      => To_Language_Category (Db, E),
            Entity_Info   => E,
            With_Params   => True,
            Is_Param      => False);
         Append (To_List, Proposal);

         declare
            Params : constant Xref.Parameter_Array := Db.Parameters (E);
            Param : General_Entity;

         begin
            for P in Params'Range loop
               Param := Params (P).Parameter;
               declare
                  Name : aliased constant String := Db.Get_Name (Param);
               begin
                  Proposal :=
                    (Resolver      => Resolver,
                     Name          => new String'(Name),
                     Category      => To_Language_Category (Db, Param),
                     Entity_Info   => Param,
                     With_Params   => False,
                     Is_Param      => True);

                  Append (To_List, Proposal);
               end;
            end loop;
         end;
      end Add_Proposal_With_Params;

      -------------------------
      -- Add_Scope_Proposals --
      -------------------------

      procedure Add_Scope_Proposals
        (To_List      : in out Extensive_List_Pckg.List;
         Scope        : General_Entity;
         Prefix_Token : Token_Record)
      is
--           GLI_Extension : constant Filesystem_String :=
--                           Get_ALI_Ext (ALI_Handler (Resolver.GLI_Handler));

         Prefix_Text   : constant String :=
                           Context.Buffer
                             (Natural (Prefix_Token.Token_First)
                                .. Natural (Prefix_Token.Token_Last));
         It : Calls_Iterator;
         E  : General_Entity;

      begin
         It := Db.Get_All_Called_Entities (Scope);
         while not At_End (It) loop
            E  := Get (It);

--            Decl := Db.Get_Declaration (E);

--              LI := Get_LI (Decl.Loc.File);
--
--              --  Skip entities associated with other languages
--
--              if LI /= null
--                and then File_Extension (Get_LI_Filename (LI))
--                          /= GLI_Extension
--              then
--                 null;

            --  Do not suggest types found in the scope of other types. Done to
            --  avoid suggesting in C/C++ types used in declaration of struct
            --  components. For example:
            --
            --        typedef struct {
            --           my_type_1 c1;
            --           my_type_2 c2;
            --        } my_type_3;
            --
            --  This ensures that we only suggest c1 and c2 for completion.

            if Db.Is_Type (Scope)
              and then Db.Is_Type (E)
            then
               null;

            --  Do not suggest subprogram parameters

            elsif Db.Is_Parameter_Of (E) /= No_General_Entity then
               null;

            --  The last token is a delimiter (dot, scope or dereference)

            elsif Prefix_Token.Tok_Type /= Tok_Identifier then
               Add_Proposal (To_List, E);

            --  The last token is an identifier. Check if the name of the
            --  entity is a valid prefix

            else
               declare
                  Nam : constant String := Db.Get_Name (E);

               begin
                  if Prefix_Text'Length <= Nam'Length
                    and then
                      Nam (Nam'First .. Nam'First + Prefix_Text'Length - 1)
                        = Prefix_Text
                  then
                     Add_Proposal (To_List, E);
                  end if;
               end;
            end if;

            Next (It);
         end loop;

         Destroy (It);
      end Add_Scope_Proposals;

      ------------------------
      -- Is_Self_Referenced --
      ------------------------

      function Is_Self_Referenced_Type
        (E : General_Entity) return Boolean is
      begin
         return
           E /= No_General_Entity
             and then Db.Is_Type (E)
             and then Db.Get_Type_Of (E) /= No_General_Entity
             and then Db.Get_Type_Of (E) = Db.Caller_At_Declaration (E);
      end Is_Self_Referenced_Type;

      --  Local variables

      C_Context  : C_Completion_Context;
      Expression : Parsed_Expression;
      E_List     : Extensive_List_Pckg.List;
      Iter       : Entities_In_Project_Cursor;
      Token      : Token_Record;

   --  Start of processing for Get_Completion_Root

   begin
      if Context.all not in C_Completion_Context'Class then
         return;
      end if;

      C_Context  := C_Completion_Context (Context.all);
      Expression := C_Context.Expression;

      case Token_List.Length (Expression.Tokens) is
         when 0 =>
            return;

         --  No context available: our candidates are all the the C/C++
         --  entities stored in the trie database whose prefix matches
         --  this one!

         when 1 =>
            Token := Token_List.Data (Token_List.First (Expression.Tokens));

            --  No action needed if the prefix is not an identifier. Thus we
            --  also avoid proposing internal names generated by the G++
            --  compiler when the prefix is just ".".

            if Token.Tok_Type /= Tok_Identifier then
               return;
            end if;

            declare
               Prefix : constant String :=
                          Context.Buffer
                            (Natural (Token.Token_First)
                               .. Natural (Token.Token_Last));

            begin
               Iter := Db.All_Entities_From_Prefix
                 (Prefix => Prefix, Is_Partial => True);
               while not At_End (Iter) loop
                  Add_Proposal (E_List, Get (Iter));
                  Next (Iter);
               end loop;
               Destroy (Iter);

               Append
                 (Result.List,
                  To_Extensive_List (E_List));

               --  We must initialize the value of Searched_Identifier to
               --  ensure that Smart_Complete() displaces the cursor backward
               --  to the beginning of the searched identifier. Otherwise it
               --  will be duplicated in the buffer.

               Result.Searched_Identifier := new String'(Prefix);
            end;

         --  Analyze the context to suggest better proposals

         when others =>
            declare
               Last_Token      : constant Token_Record :=
                                   Token_List.Data
                                     (Token_List.Last (Expression.Tokens));
               Last_Token_Text : constant String :=
                                   Context.Buffer
                                    (Natural (Last_Token.Token_First)
                                      .. Natural (Last_Token.Token_Last));

               Tok_Index  : Token_List.List_Node;
               Tok_Prev   : Token_List.List_Node;
               Token_Prev : Token_Record;

               use Token_List;

               procedure Prev_Token;
               --  Displace Tok_Index and Tok_Prev to reference the previous
               --  token

               procedure Prev_Token is
               begin
                  Tok_Index := Tok_Prev;

                  if Tok_Prev /= Token_List.Null_Node then
                     Token    := Data (Tok_Index);
                     Tok_Prev := Prev (Expression.Tokens, Tok_Index);

                     if Tok_Prev /= Token_List.Null_Node then
                        Token_Prev := Data (Tok_Prev);
                     else
                        Token_Prev := Null_Token;
                     end if;

                  else
                     Token      := Null_Token;
                     Token_Prev := Null_Token;
                  end if;
               end Prev_Token;

            begin
               Tok_Index := Last (Expression.Tokens);
               Tok_Prev  := Prev (Expression.Tokens, Tok_Index);
               Token     := Data (Tok_Index);
               Token_Prev := Data (Tok_Prev);

               if Token.Tok_Type = Tok_Identifier
                 and then (Token_Prev.Tok_Type = Tok_Dot
                             or else Token_Prev.Tok_Type = Tok_Dereference
                             or else Token_Prev.Tok_Type = Tok_Scope)
               then
                  Prev_Token;
               end if;

               if Token.Tok_Type = Tok_Left_Paren then
                  Prev_Token;

                  if Token.Tok_Type /= Tok_Identifier then
                     return;
                  end if;

                  --  Adding subprogram call proposals

                  declare
                     Prefix : constant String :=
                                Context.Buffer
                                  (Natural (Token.Token_First)
                                    .. Natural (Token.Token_Last));
                     E      : General_Entity;
                     Iter   : Entities_In_Project_Cursor :=
                       Db.All_Entities_From_Prefix
                         (Prefix => Prefix, Is_Partial => False);

                  begin
                     while not At_End (Iter) loop
                        E := Get (Iter);

                        if Db.Is_Subprogram (E) then
                           Add_Proposal_With_Params
                             (To_List => E_List,
                              E       => E);
                        end if;

                        Next (Iter);
                     end loop;

                     Destroy (Iter);

                     Completion_List_Pckg.Append
                       (Result.List,
                        To_Extensive_List (E_List));
                  end;

               elsif Token.Tok_Type = Tok_Dot
                 or else Token.Tok_Type = Tok_Dereference
                 or else Token.Tok_Type = Tok_Scope
               then
                  Prev_Token;

                  --  Skip array subscripts

                  if Token.Tok_Type = Tok_Right_Sq_Bracket then
                     declare
                        Expr_Depth : Natural := 0;

                     begin
                        loop
                           case Token.Tok_Type is
                              when Tok_Right_Sq_Bracket =>
                                 Expr_Depth := Expr_Depth + 1;

                              when Tok_Left_Sq_Bracket =>
                                 Expr_Depth := Expr_Depth - 1;

                              when others =>
                                 null;
                           end case;

                           Prev_Token;

                           exit when
                             (Expr_Depth = 0
                                and then
                                  Token.Tok_Type /= Tok_Right_Sq_Bracket)
                             or else Tok_Prev = Token_List.Null_Node;
                        end loop;
                     end;
                  end if;

                  --  Protect us against wrong sources

                  if Token = Null_Token then
                     return;
                  end if;

                  --  Adding proposals

                  declare
                     Prefix   : constant String :=
                                  Context.Buffer
                                    (Natural (Token.Token_First)
                                      .. Natural (Token.Token_Last));
                     E        : General_Entity;
                     Iter     : Entities_In_Project_Cursor :=
                       Db.All_Entities_From_Prefix
                         (Prefix => Prefix, Is_Partial => False);

                  begin
                     while not At_End (Iter) loop
                        E := Get (Iter);

                        if not Db.Is_Type (E) then
                           if Db.Is_Access (E) then
                              E := Db.Pointed_Type (E);

                           elsif Db.Is_Array (E) then
                              E := Db.Component_Type (E);

                           --  Class or record
                           elsif Db.Has_Methods (E) then
                              E := Db.Get_Type_Of (E);

                              --  Handle named typedef structs since the
                              --  compiler generates two entites in the LI
                              --  file with the same name. For example:
                              --
                              --     typedef struct {    // First_Entity
                              --       ...
                              --     } my_type;          // Second_Entity
                              --
                              --  When we declare an object of this type:
                              --
                              --     my_type obj;
                              --
                              --  The type of obj references Second_Entity,
                              --  whose (parent) type is First_Entity (which
                              --  is the entity needed for completion purposes)
                              --
                              --  ??? Should this be handled by the LI parser
                              --  instead.

                              if Is_Self_Referenced_Type (E) then
                                 E := Db.Get_Type_Of (E);
                              end if;

                           else
                              E := Db.Get_Type_Of (E);
                           end if;
                        end if;

                        if E /= No_General_Entity then

                           --  Class or record
                           if Db.Has_Methods (E) then
                              Add_Scope_Proposals
                                (To_List      => E_List,
                                 Scope        => E,
                                 Prefix_Token => Last_Token);
                           end if;
                        end if;

                        Next (Iter);
                     end loop;

                     Destroy (Iter);

                     Completion_List_Pckg.Append
                       (Result.List,
                        To_Extensive_List (E_List));

                     if Last_Token.Tok_Type = Tok_Identifier then
                        Result.Searched_Identifier :=
                          new String'(Last_Token_Text);
                     end if;
                  end;
               end if;
            end;
      end case;
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

   ----------------------
   -- To_Completion_Id --
   ----------------------

   overriding function To_Completion_Id
     (Proposal : C_Completion_Proposal;
      Db       : access Xref.General_Xref_Database_Record'Class)
      return Completion_Id
   is
      Id  : constant String := Proposal.Name.all;
      Loc : constant General_Location :=
                       Db.Get_Declaration (Proposal.Entity_Info).Loc;

   begin
      return (Id_Length   => Id'Length,
              Resolver_Id => Resolver_ID,
              Id          => Id,
              File        => Loc.File,
              Line        => Loc.Line,
              Column      => Integer (Loc.Column));
   end To_Completion_Id;

   ------------------
   -- Get_Category --
   ------------------

   overriding
   function Get_Category
     (Proposal : C_Completion_Proposal) return Language_Category is
   begin
      return Proposal.Category;
   end Get_Category;

   --------------------
   -- Get_Completion --
   --------------------

   overriding function Get_Completion
     (Proposal : C_Completion_Proposal;
      Db       : access General_Xref_Database_Record'Class)
      return UTF8_String
   is
      function Single_Param_Text (Param : General_Entity) return String;
      --  Generate the named notation associated with Param as a C comment.
      --  For example: "/* Param */"

      function All_Params_Text return String;
      --  Recursively traverse the whole list of parameters of the subprogram
      --  proposal and generate the named notation associated with all the
      --  parameters as C comments.

      -----------------------
      -- To_Named_Notation --
      -----------------------

      function Single_Param_Text (Param : General_Entity) return String is
      begin
         return "/* " & Db.Get_Name (Param) & " */";
      end Single_Param_Text;

      --------------------
      -- Get_All_Params --
      --------------------

      function All_Params_Text return String is
         Separator : constant String := "," & ASCII.LF;
         Spaces    : constant String := "  ";
         Params    : constant Xref.Parameter_Array :=
           Db.Parameters (Proposal.Entity_Info);
         Param     : General_Entity := No_General_Entity;

         function Next_Params
           (P : Integer; Prev_Params : String) return String;
         --  Prev_Params is used to accumulate the output.

         function Next_Params
           (P : Integer; Prev_Params : String) return String is
         begin
            if P > Params'Last then
               return Prev_Params;
            else
               return
                 Next_Params
                   (P + 1,
                    Prev_Params
                    & Separator
                    & Single_Param_Text (Param)
                    & Spaces);
            end if;
         end Next_Params;

      --  Start of processing for All_Params_Text

      begin
         if Params'Length /= 0 then
            Param := Params (Params'First).Parameter;
         end if;

         return Next_Params
           (Params'First + 1, Single_Param_Text (Param) & Spaces);
      end All_Params_Text;

   --  Start of processing for Get_Completion

   begin
      if Proposal.With_Params then
         return All_Params_Text & ")";

      elsif Proposal.Is_Param then
         return Single_Param_Text (Proposal.Entity_Info);

      else
         return Proposal.Name.all;
      end if;
   end Get_Completion;

   ---------------
   -- Get_Label --
   ---------------

   overriding function Get_Label
     (Proposal : C_Completion_Proposal;
      Db       : access General_Xref_Database_Record'Class)
      return UTF8_String
   is
   begin
      if not Proposal.With_Params then
         return Proposal.Name.all;
      else
         declare
            Params : constant Parameter_Array :=
              Db.Parameters (Proposal.Entity_Info);
         begin
            if Params'Length = 0 then
               return Proposal.Name.all & " without params";
            else
               return "params of " & Proposal.Name.all;
            end if;
         end;
      end if;
   end Get_Label;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Proposal : C_Completion_Proposal;
      Db       : access General_Xref_Database_Record'Class)
      return File_Location
   is
      Loc : constant General_Location :=
              Db.Get_Declaration (Proposal.Entity_Info).Loc;
   begin
      return (Loc.File, Loc.Line, Loc.Column);
   end Get_Location;

   overriding function Get_Visibility
     (Proposal : C_Completion_Proposal) return Construct_Visibility is
      pragma Unreferenced (Proposal);
   begin
      return Visibility_Public;
   end Get_Visibility;

   --------------------------
   -- To_Language_Category --
   --------------------------

   function To_Language_Category
     (Db : access General_Xref_Database_Record'Class;
      E   : General_Entity) return Language_Category
   is
   begin
      if Db.Has_Methods (E) then
         --  class or record
         return Cat_Class;
         --  return Cat_Structure;

      elsif Db.Is_Subprogram (E) then
         --  procedure or function
         return Cat_Function;
         --  return Cat_Procedure;

      elsif not Db.Is_Type (E) then
         return Cat_Variable;
      end if;

      return Cat_Unknown;

--           when Enumeration_Literal => return Cat_Literal;
--           when Include_File => return Cat_Include;
--           when Union => return Cat_Union;
   end To_Language_Category;

end Completion.C.Constructs_Extractor;
