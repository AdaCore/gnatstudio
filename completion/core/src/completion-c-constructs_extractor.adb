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

with ALI_Parser;       use ALI_Parser;
with Entities;         use Entities;
with Entities.Queries; use Entities.Queries;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;
with Language.Cpp;     use Language.Cpp;
with Xref;             use Xref;

package body Completion.C.Constructs_Extractor is
   use Completion_List_Pckg;
   use Completion_List_Extensive_Pckg;
   use Extensive_List_Pckg;

   Resolver_ID : constant String := "CNST_C  ";

   function To_Language_Category (Kind : E_Kind) return Language_Category;
   --  Make a simple association between entity categories and construct
   --  categories. This association is known to be inaccurate.

   -----------------------------------------
   -- New_C_Construct_Completion_Resolver --
   -----------------------------------------

   function New_C_Construct_Completion_Resolver
     (Kernel       : Kernel_Handle;
      Current_File : Virtual_File) return Completion_Resolver_Access is
   begin
      return
        new Construct_Completion_Resolver'
              (Manager     => null,
               Kernel      => Kernel,
               GLI_Handler => Get_LI_Handler
                                (Get_Database (Kernel), Current_File));
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
      Doc_Threshold : constant Natural := 100;
      --  The documentation associated with each completion proposal is only
      --  generated when the number of completion proposals is smaller than
      --  this value. This threshold is required to allow the use of Dynamic
      --  Smart Completion in projects composed of many C/C++ files.

      Under_Doc_Treshold : Boolean := True;
      --  True if the number of candidates is smaller than Doc_Treshold

      procedure Add_Proposal
        (To_List : in out Extensive_List_Pckg.List;
         E       : Entity_Information);
      --  Append to the list To_List the proposal E

      procedure Add_Scope_Proposals
        (To_List      : in out Extensive_List_Pckg.List;
         Scope        : Entity_Information;
         Prefix_Token : Token_Record);
      --  Append to To_List all the proposals defined in Scope which match
      --  the prefix available in Prefix_Token. If Prefix_Token is not an
      --  identifier then all the entities defined in Scope are appended
      --  to To_List.

      function Gen_Doc (E_Info : Entity_Information) return String_Access;
      --  Generate the documentation associated with E_Info

      ------------------
      -- Add_Proposal --
      ------------------

      procedure Add_Proposal
        (To_List : in out Extensive_List_Pckg.List;
         E       : Entity_Information)
      is
         Name     : aliased constant String := Get (Get_Name (E)).all;
         Proposal : C_Completion_Proposal;

      begin
         Proposal :=
           (Resolver      => Resolver,
            Name          => new String'(Name),
            Category      => To_Language_Category (Get_Kind (E)),
            Documentation => Gen_Doc (E),
            Entity_Info   => E,
            With_Params   => False,
            Is_Param      => False);

         Append (To_List, Proposal);
      end Add_Proposal;

      procedure Add_Proposal_With_Params
        (To_List : in out Extensive_List_Pckg.List;
         E       : Entity_Information);
      --  Append to the list To_List one proposal containing all the
      --  parameters needed to call E. In addition, in order to provide the
      --  same behavior of Ada completions, one proposal per parameter is
      --  added to To_List.

      ------------------------------
      -- Add_Proposal_With_Params --
      ------------------------------

      procedure Add_Proposal_With_Params
        (To_List : in out Extensive_List_Pckg.List;
         E       : Entity_Information)
      is
         Name     : aliased constant String := Get (Get_Name (E)).all;
         Proposal : C_Completion_Proposal;

      begin
         Proposal :=
           (Resolver      => Resolver,
            Name          => new String'(Name),
            Category      => To_Language_Category (Get_Kind (E)),
            Documentation => Gen_Doc (E),
            Entity_Info   => E,
            With_Params   => True,
            Is_Param      => False);
         Append (To_List, Proposal);

         declare
            It    : Subprogram_Iterator;
            Param : Entity_Information;

         begin
            It := Get_Subprogram_Parameters (E);
            Get (It, Param);

            while Param /= null loop
               declare
                  Name : aliased constant String :=
                           Get (Get_Name (Param)).all;
               begin
                  Proposal :=
                    (Resolver      => Resolver,
                     Name          => new String'(Name),
                     Category      => To_Language_Category (Get_Kind (Param)),
                     Documentation => Gen_Doc (Param),
                     Entity_Info   => Param,
                     With_Params   => False,
                     Is_Param      => True);

                  Append (To_List, Proposal);
               end;

               Next (It);
               Get (It, Param);
            end loop;
         end;
      end Add_Proposal_With_Params;

      -------------------------
      -- Add_Scope_Proposals --
      -------------------------

      procedure Add_Scope_Proposals
        (To_List      : in out Extensive_List_Pckg.List;
         Scope        : Entity_Information;
         Prefix_Token : Token_Record)
      is
         GLI_Extension : constant Filesystem_String :=
                           Get_ALI_Ext (ALI_Handler (Resolver.GLI_Handler));

         Prefix_Text   : constant String :=
                           Context.Buffer
                             (Natural (Prefix_Token.Token_First)
                                .. Natural (Prefix_Token.Token_Last));
         It : Calls_Iterator;
         E  : Entity_Information;

      begin
         It := Get_All_Called_Entities (Scope);
         while not At_End (It) loop
            E := Get (It);

            --  Skip entities associated with other languages

            if File_Extension
                (Get_LI_Filename (Get_LI (Get_Declaration_Of (E).File)))
                  /= GLI_Extension
            then
               null;

            --  The last token is a delimiter (dot, scope or dereference)

            elsif Prefix_Token.Tok_Type /= Tok_Identifier then
               Add_Proposal (To_List, E);

            --  The last token is an identifier. Check if the name of the
            --  entity is a valid prefix

            else
               declare
                  Nam : constant String := Get (Get_Name (E)).all;

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

      -------------
      -- Gen_Doc --
      -------------

      function Gen_Doc (E_Info : Entity_Information) return String_Access is

         function Doc_Header (E_Info : Entity_Information) return String;
         --  Generate the header of the documentation of each entity

         function Doc_Header (E_Info : Entity_Information) return String is
            K : constant E_Kinds := Get_Kind (E_Info).Kind;
         begin
            if K = Include_File then
               return "";
            else
               return
                 Attributes_To_String (Get_Attributes (E_Info))
                 & " " & Kind_To_String (Get_Kind (E_Info))
                 & ASCII.LF;
            end if;
         end Doc_Header;

      begin
         --  Generate a minimum documentation indicating the type of the entity
         --  and its location when the number of proposals passes the treshold.
         --  This improves the behavior of GPS under Dynamic Smart Completion
         --  since Gen_Completion_Root is invoked when the first letter is
         --  pressed (to generate the whole list of proposals) and subsequent
         --  letters are used to filter this list; that is, the list is not
         --  re-generated each time a letter is pressed. As a consequence, if
         --  the treshold is initially passed, this minimum documentation is
         --  the only documentation available when the list is filtered.

         if not Under_Doc_Treshold then
            return new String'(Doc_Header (E_Info));
         else
            return new String'
              (Doc_Header (E_Info)
               & Documentation
                 (Resolver.Kernel.Databases,
                  Resolver.Kernel.Get_Language_Handler,
                  General_Entity'(Old_Entity => E_Info, others => <>)));
         end if;
      end Gen_Doc;

      --  Local variables

      use Entities_Search_Tries;

      C_Context  : C_Completion_Context;
      Expression : Parsed_Expression;
      E_List     : Extensive_List_Pckg.List;
      Iter       : Vector_Trie_Iterator;
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
               Count  : Natural := 0;

            begin
               --  Check if we are under Doc_Threshold

               Iter :=
                 Start (Trie       => Get_Name_Index (Resolver.GLI_Handler),
                        Prefix     => Prefix,
                        Is_Partial => True);
               while not At_End (Iter) loop
                  Count := Count + 1;

                  if Count > Doc_Threshold then
                     Under_Doc_Treshold := False;

                     --  We don't need to count the exact number of proposals
                     --  that we have

                     exit;
                  end if;

                  Next (Iter);
               end loop;

               Iter :=
                 Start (Trie       => Get_Name_Index (Resolver.GLI_Handler),
                        Prefix     => Prefix,
                        Is_Partial => True);
               while not At_End (Iter) loop
                  Add_Proposal (E_List, Get (Iter));
                  Next (Iter);
               end loop;

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

               Tok_Index : Token_List.List_Node;
               Tok_Prev  : Token_List.List_Node;

               use Token_List;

               procedure Prev_Token;
               --  Displace Tok_Index and Tok_Prev to reference the previous
               --  token

               procedure Prev_Token is
               begin
                  Tok_Index := Tok_Prev;

                  if Tok_Prev /= Token_List.Null_Node then
                     Tok_Prev  := Prev (Expression.Tokens, Tok_Index);
                     Token     := Data (Tok_Index);
                  else
                     Token     := Null_Token;
                  end if;
               end Prev_Token;

            begin
               Tok_Index := Last (Expression.Tokens);
               Tok_Prev  := Prev (Expression.Tokens, Tok_Index);
               Token     := Data (Tok_Index);

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
                     E      : Entity_Information;
                     Iter   : Vector_Trie_Iterator;

                  begin
                     Iter :=
                       Start
                         (Trie       => Get_Name_Index (Resolver.GLI_Handler),
                          Prefix     => Prefix,
                          Is_Partial => False);

                     while not At_End (Iter) loop
                        E := Get (Iter);

                        if Is_Subprogram (E) then
                           Add_Proposal_With_Params
                             (To_List => E_List,
                              E       => E);
                        end if;

                        Next (Iter);
                     end loop;

                     Free (Iter);

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
                     E        : Entity_Information;
                     Ent_Kind : E_Kind;
                     Iter     : Vector_Trie_Iterator;

                  begin
                     Iter :=
                       Start
                         (Trie       => Get_Name_Index (Resolver.GLI_Handler),
                          Prefix     => Prefix,
                          Is_Partial => False);

                     while not At_End (Iter) loop
                        E := Get (Iter);

                        if Get_Category (E) = Object then
                           case Get_Kind (E).Kind is
                              when Access_Kind =>
                                 E := Pointed_Type (E);

                              when Array_Kind =>
                                 E := Array_Contents_Type (E);

                              when others =>
                                 E := Get_Type_Of (E);
                           end case;
                        end if;

                        if E /= null then
                           Ent_Kind := Get_Kind (E);

                           if Ent_Kind.Kind = Class
                             or else Ent_Kind.Kind = Record_Kind
                           then
                              Add_Scope_Proposals
                                (To_List      => E_List,
                                 Scope        => E,
                                 Prefix_Token => Last_Token);
                           end if;
                        end if;

                        Next (Iter);
                     end loop;

                     Free (Iter);

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
     (Proposal : C_Completion_Proposal) return Completion_Id
   is
      Id  : constant String := Proposal.Name.all;
      Loc : constant Entities.File_Location :=
                       Get_Declaration_Of (Proposal.Entity_Info);

   begin
      return (Id_Length   => Id'Length,
              Resolver_Id => Resolver_ID,
              Id          => Id,
              File        => Get_Filename (Loc.File),
              Line        => Get_Line (Loc),
              Column      => Integer (Get_Column (Loc)));
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

   overriding
   function Get_Completion
     (Proposal : C_Completion_Proposal) return UTF8_String
   is
      function Single_Param_Text (Param : Entity_Information) return String;
      --  Generate the named notation associated with Param as a C comment.
      --  For example: "/* Param */"

      function All_Params_Text return String;
      --  Recursively traverse the whole list of parameters of the subprogram
      --  proposal and generate the named notation associated with all the
      --  parameters as C comments.

      -----------------------
      -- To_Named_Notation --
      -----------------------

      function Single_Param_Text (Param : Entity_Information) return String is
      begin
         return "/* " & Get (Get_Name (Param)).all & " */";
      end Single_Param_Text;

      --------------------
      -- Get_All_Params --
      --------------------

      function All_Params_Text return String is
         Separator : constant String := "," & ASCII.LF;
         Spaces    : constant String := "  ";
         It        : Subprogram_Iterator;
         Param     : Entity_Information;

         function Next_Params (Prev_Params : String) return String;
         --  Prev_Params is used to accumulate the output.

         function Next_Params (Prev_Params : String) return String is
         begin
            Next (It);
            Get (It, Param);

            if Param = null then
               return Prev_Params;
            else
               return
                 Next_Params
                   (Prev_Params
                    & Separator
                    & Single_Param_Text (Param)
                    & Spaces);
            end if;
         end Next_Params;

      --  Start of processing for All_Params_Text

      begin
         It := Get_Subprogram_Parameters (Proposal.Entity_Info);
         Get (It, Param);

         if Param = null then
            return "";
         else
            return Next_Params (Single_Param_Text (Param) & Spaces);
         end if;
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

   overriding
   function Get_Label
     (Proposal : C_Completion_Proposal)  return UTF8_String is
   begin
      if not Proposal.With_Params then
         return Proposal.Name.all;
      else
         declare
            It    : Subprogram_Iterator;
            Param : Entity_Information;
         begin
            It := Get_Subprogram_Parameters (Proposal.Entity_Info);
            Get (It, Param);

            if Param = null then
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
     (Proposal : C_Completion_Proposal) return File_Location
   is
      Loc : constant Entities.File_Location :=
              Get_Declaration_Of (Proposal.Entity_Info);
   begin
      return (Get_Filename (Get_File (Loc)), Get_Line (Loc), Get_Column (Loc));
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

   function To_Language_Category (Kind : E_Kind) return Language_Category is
   begin
      case Kind.Kind is
         when Access_Kind
            | Array_Kind
            | Boolean_Kind
            | Decimal_Fixed_Point
            | Enumeration_Kind
            | Exception_Entity
            | Floating_Point
            | Modular_Integer
            | Ordinary_Fixed_Point
            | Private_Type
            | Signed_Integer
            | String_Kind
            | Named_Number =>
            return Cat_Variable;

         when Class_Wide
            | Class
            | Interface_Kind =>
            return Cat_Class;

         when Enumeration_Literal =>
            return Cat_Literal;

         when Function_Or_Operator =>
            return Cat_Function;

         when Include_File =>
            return Cat_Include;

         when Procedure_Kind =>
            return Cat_Procedure;

         when Record_Kind =>
            return Cat_Structure;

         when Union =>
            return Cat_Union;

         when Unresolved_Entity
            | Function_Macro
            | Macro
            | Reference =>
            return Cat_Unknown;

         --  Kinds exlusive of Ada

         when Overloaded_Entity
            | Entry_Or_Entry_Family
            | Label_On_Block
            | Label_On_Loop
            | Label_On_Statement
            | Package_Kind
            | Protected_Kind
            | Private_Object
            | Task_Kind =>
            return Cat_Unknown;

         --  Unused language categories:
         --    Cat_Type
         --    Cat_Subtype
         --    Cat_Local_Variable
         --    Cat_Parameter

         --    Cat_Case_Inside_Record
         --    Cat_Discriminant
         --    Cat_Representation_Clause

         --    Cat_Loop_Statement
         --    Cat_If_Statement
         --    Cat_Case_Statement
         --    Cat_Select_Statement
         --    Cat_Accept_Statement
         --    Cat_Declare_Block
         --    Cat_Return_Block
         --    Cat_Simple_Block

         --    Cat_Package
         --    Cat_Task

         --    Cat_Protected
         --    Cat_Entry

         --    Cat_Exception_Handler
         --    Cat_Pragma

         --    Cat_Custom

      end case;
   end To_Language_Category;

end Completion.C.Constructs_Extractor;
