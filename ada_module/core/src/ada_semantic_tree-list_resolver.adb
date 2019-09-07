------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Ada.Characters.Handling;     use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Symbols;            use GNATCOLL.Symbols;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with Language.Ada;                use Language.Ada;
with Ada_Semantic_Tree.Type_Tree; use Ada_Semantic_Tree.Type_Tree;
with Ada_Semantic_Tree.Parts;     use Ada_Semantic_Tree.Parts;

package body Ada_Semantic_Tree.List_Resolver is

   use Token_List;

   ---------------------------------
   -- Get_Construct_Tree_Iterator --
   ---------------------------------

   function Get_Construct_Tree_Iterator
     (Param : Formal_Parameter) return Construct_Tree_Iterator is
   begin
      return To_Construct_Tree_Iterator (Entity_Access (Param));
   end Get_Construct_Tree_Iterator;

   -------------------
   -- Get_Construct --
   -------------------

   overriding function Get_Construct
     (Param : Formal_Parameter)
      return access Simple_Construct_Information
   is
   begin
      return Get_Construct
        (To_Construct_Tree_Iterator (Entity_Access (Param)));
   end Get_Construct;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Param : Formal_Parameter) return Entity_Access is
      pragma Unreferenced (Param);
   begin
      return Null_Entity_Access;
   end Get_Type;

   ----------------------
   -- To_Entity_Access --
   ----------------------

   function To_Entity_Access (Param : Formal_Parameter) return Entity_Access is
   begin
      return Entity_Access (Param);
   end To_Entity_Access;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out List_Profile_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (List_Profile, List_Profile_Access);
   begin
      Internal (This);
   end Free;

   ----------------------------
   -- Is_Entity_With_Profile --
   ----------------------------

   function Is_Entity_With_Profile
     (Entity       : Entity_Access;
      Visible_From : Visibility_Context) return Boolean
   is
      Used_Entity : Entity_Access;
   begin
      if Get_Construct (Entity).Category in Subprogram_Category then
         return True;
      else
         Used_Entity := Get_Last_Visible_Declaration
           (Entity, Visible_From.File, Visible_From.Offset);

         return Get_Construct (Used_Entity).Category in Type_Category
           or else Get_Construct (Used_Entity).Attributes (Array_Attribute);
      end if;
   end Is_Entity_With_Profile;

   ----------------------
   -- Get_List_Profile --
   ----------------------

   function Get_List_Profile
     (Entity       : Entity_Access;
      Visible_From : Visibility_Context;
      Kind         : Profile_Kind := Regular_Profile)
      return List_Profile
   is
      function Compute_Entity_To_Analyse return Entity_Access;

      function Compute_Entity_To_Analyse return Entity_Access is
      begin
         if Get_Construct (Entity).Category in Type_Category then
            --  In the case of types, we only consider the most visible view
            --  in order to take into account possibly hidden fileds.

            return Get_Last_Visible_Declaration
              (Entity, Visible_From.File, Visible_From.Offset);
         else
            --  For other entities, the body is not visible, so consider only
            --  the given declaration

            return Entity;
         end if;
      end Compute_Entity_To_Analyse;

      Used_Entity : constant Entity_Access := Compute_Entity_To_Analyse;

      Tree  : constant Construct_Tree := Get_Tree (Get_File (Used_Entity));
      Scope : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Used_Entity);
      It    : Construct_Tree_Iterator := Next (Tree, Scope, Jump_Into);

      Result_Array : Entity_Array (1 .. Get_Child_Number (Scope));
      Index        : Integer := 1;
      Ada_Type     : Ada_Type_Access;
      Private_Root : Ada_Type_Access;
   begin
      if Get_Construct (Used_Entity).Category in Subprogram_Category
        or else Get_Construct (Used_Entity).Category = Cat_Package
      then
         if Kind = Regular_Profile then
            while It /= Null_Construct_Tree_Iterator
              and then Is_Parent_Scope (Scope, It)
            loop
               if Get_Construct (It).Is_Generic_Spec then
                  --  We skip potential generic parameters preceding real
                  --  parameters.
                  null;
               elsif Get_Construct (It).Category = Cat_Parameter then
                  Result_Array (Index) := To_Entity_Access
                    (Get_File (Used_Entity), It);
                  Index := Index + 1;
               else
                  exit;
               end if;

               It := Next (Tree, It, Jump_Over);
            end loop;
         else
            while It /= Null_Construct_Tree_Iterator
              and then Is_Parent_Scope (Scope, It)
            loop
               if Get_Construct (It).Is_Generic_Spec then
                  Result_Array (Index) := To_Entity_Access
                    (Get_File (Used_Entity), It);
                  Index := Index + 1;
               else
                  exit;
               end if;

               It := Next (Tree, It, Jump_Over);
            end loop;
         end if;

         declare
            Result : List_Profile (Index - 1);
         begin
            Result.Params := Result_Array (1 .. Index - 1);

            return Result;
         end;
      elsif Get_Construct (Entity).Attributes (Ada_Array_Attribute) then
         declare
            Result : List_Profile (1);
         begin
            Result.Extra_Params_Allowed := True;
            Result.Params (1) := Null_Entity_Access;

            return Result;
         end;
      elsif Get_Construct (Entity).Category in Type_Category then
         if Kind = Regular_Profile then
            Ada_Type := Get_Ada_Type (Used_Entity);

            if Ada_Type /= Null_Ada_Type_Access then
               --  This is a tagged type, use the hierarchy information

               Private_Root := First_Private_Parent
                 (Ada_Type, Visible_From);

               declare
                  Fields : constant Entity_Array :=
                    (Get_Fields_From (Ada_Type, Private_Root));
                  Result : List_Profile (Fields'Length);
               begin
                  Result.Params := Fields;
                  Result.Aggregate_Parent := Get_Entity (Private_Root);

                  return Result;
               end;
            else
               --  This is a simple record type, just traverse the tree

               for J in 1 .. Language.Tree.Get_Child_Number (Scope) loop
                  if Get_Construct (It).Category = Cat_Field
                    or else Get_Construct (It).Category = Cat_Discriminant
                  then
                     Result_Array (Index) := Get_First_Occurence
                       (To_Entity_Access
                          (Get_File (Used_Entity), It));
                     Index := Index + 1;
                  end if;

                  It := Next (Tree, It, Jump_Into);
               end loop;

               declare
                  Result : List_Profile (Index - 1);
               begin
                  Result.Params := Result_Array (1 .. Index - 1);

                  return Result;
               end;
            end if;
         end if;
      end if;

      declare
         Result : List_Profile (0);
      begin
         Result.Extra_Params_Allowed := True;

         return Result;
      end;
   end Get_List_Profile;

   -----------------
   -- Get_Formals --
   -----------------

   function Get_Formals (Profile : List_Profile) return Entity_Array is
   begin
      return Profile.Params;
   end Get_Formals;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Profile : List_Profile) return Entity_Access
   is
      pragma Unreferenced (Profile);
   begin
      return Null_Entity_Access;
   end Get_Entity;

   ---------------------------
   -- Get_Number_Of_Formals --
   ---------------------------

   function Get_Number_Of_Formals (Profile : List_Profile) return Integer is
   begin
      return Profile.Params'Length;
   end Get_Number_Of_Formals;

   --------------------------
   -- Get_Aggregate_Parent --
   --------------------------

   function Get_Aggregate_Parent
     (Params : List_Profile) return Entity_Access
   is
   begin
      return Params.Aggregate_Parent;
   end Get_Aggregate_Parent;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Actual_Parameter) is
   begin
      Free (This.Expression);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Actual_Parameter_Resolver_Access) is
      procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
        (Actual_Parameter_Resolver, Actual_Parameter_Resolver_Access);
   begin
      if This /= null then
         for J in This.Actual_Params'Range loop
            Free (This.Actual_Params (J));
         end loop;
      end if;

      Unchecked_Free (This);
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (This : Actual_Parameter_Resolver)
      return Actual_Parameter_Resolver
   is
      Result : Actual_Parameter_Resolver (This.Length);
   begin
      Result.Profile := This.Profile;
      Result.Params_Set := This.Params_Set;

      for J in This.Actual_Params'Range loop
         Result.Actual_Params (J) := This.Actual_Params (J);
         Result.Actual_Params (J).Expression.Tokens := Token_List.Empty_Vector;

         for Item of This.Actual_Params (J).Expression.Tokens loop
            Result.Actual_Params (J).Expression.Tokens.Append (Item);
         end loop;
      end loop;

      return Result;
   end Deep_Copy;

   -----------------------------------
   -- Get_Actual_Parameter_Resolver --
   -----------------------------------

   function Get_Actual_Parameter_Resolver
     (Profile : List_Profile)
      return Actual_Parameter_Resolver
   is
      Result : Actual_Parameter_Resolver (Profile.Nb_Params);
   begin
      Result.Profile := Profile;

      return Result;
   end Get_Actual_Parameter_Resolver;

   --------------------------
   -- Get_Actual_Parameter --
   --------------------------

   function Get_Actual_Parameter
     (Buffer      : access UTF8_String;
      Param_Start : String_Index_Type;
      Param_End   : String_Index_Type) return Actual_Parameter
   is
      use type Ada.Containers.Count_Type;

      Result : Actual_Parameter;
      It     : Token_List.Cursor;
   begin
      Result.Is_Named := False;

      Result.Expression := Parse_Expression_Backward
        (Buffer, Param_End, Param_Start, True);

      if Result.Expression.Tokens.Length >= 2 then
         It := First (Result.Expression.Tokens);
         It := Next (It);

         if Element (It).Tok_Type = Tok_Arrow then
            Result.Is_Named := True;
         end if;
      end if;

      return Result;
   end Get_Actual_Parameter;

   -------------------
   -- Append_Actual --
   -------------------

   procedure Append_Actual
     (Params      : in out Actual_Parameter_Resolver;
      Actual      : Actual_Parameter;
      Do_Semantic : Boolean;
      Param_Added : out Boolean;
      Success     : out Boolean)
   is
      pragma Unreferenced (Do_Semantic);
      --  ??? Do_Semantics is currently ignored.
   begin
      Param_Added := False;

      if Params.Profile.Extra_Params_Allowed then
         Success := True;
      else
         Success := False;
      end if;

      if Params.Params_Set = Params.Actual_Params'Length then
         return;
      end if;

      for J in Params.Actual_Params'Range loop
         if Params.Actual_Params (J) = Null_Actual_Parameter then
            if Actual.Is_Named
              and then Get_Construct
                (To_Construct_Tree_Iterator
                     (Params.Profile.Params (J))).Name /= No_Symbol
            then
               if Equal
                 (Get_Name
                    (Actual.Expression,
                     Element (Actual.Expression.Tokens.First)),
                  Get (Get_Construct
                    (To_Construct_Tree_Iterator
                       (Params.Profile.Params (J))).Name).all,
                  False)
               then
                  Param_Added := True;
                  Params.Actual_Params (J) := Actual;
                  Success := True;
                  Params.Params_Set := Params.Params_Set + 1;

                  return;
               end if;
            else
               Param_Added := True;
               Params.Actual_Params (J) := Actual;
               Success := True;
               Params.Params_Set := Params.Params_Set + 1;

               return;
            end if;
         end if;
      end loop;
   end Append_Actual;

   --------------------
   -- Append_Actuals --
   --------------------

   procedure Append_Actuals
     (Params     : in out Actual_Parameter_Resolver;
      Buffer     : String_Access;
      Start_Call : String_Index_Type;
      Success    : out Boolean)
   is
      Param_Start, Param_End : String_Index_Type := 0;
      Paren_Depth : Integer := 0;

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;

      function Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Entity, Partial_Entity);

         Word : constant String := Buffer (Sloc_Start.Index .. Sloc_End.Index);
         Param_Added : Boolean;
      begin
         if Paren_Depth = 0 then
            if Word = "(" then
               Paren_Depth := 1;
               Param_Start := String_Index_Type (Sloc_Start.Index + 1);
            end if;

         elsif Paren_Depth = 1 then
            declare
               Actual : Actual_Parameter;
            begin
               if Word = ")" or else Word = "," then
                  Param_End := String_Index_Type (Sloc_End.Index - 1);

                  Actual := Get_Actual_Parameter
                    (Buffer      => Buffer,
                     Param_Start => Param_Start,
                     Param_End   => Param_End);

                  Append_Actual
                    (Params      => Params,
                     Actual      => Actual,
                     Do_Semantic => False,
                     Param_Added => Param_Added,
                     Success     => Success);

                  if not Param_Added then
                     Free (Actual);
                  end if;

                  if not Success then
                     return True;
                  end if;

                  if Word = ")" then
                     return True;
                  else
                     Param_Start := String_Index_Type (Sloc_End.Index + 1);
                  end if;
               end if;
            end;

         else
            if Word = "(" then
               Paren_Depth := Paren_Depth + 1;
            elsif Word = ")" then
               Paren_Depth := Paren_Depth - 1;
            end if;
         end if;

         return False;
      end Callback;
   begin
      Success := True;

      Parse_Entities
        (Ada_Lang,
         Buffer (Integer (Start_Call) .. Buffer'Last),
         Callback'Unrestricted_Access);
   end Append_Actuals;

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete (Params : Actual_Parameter_Resolver) return Boolean is
   begin
      if Params.Params_Set = Params.Actual_Params'Length then
         return True;
      else
         for J in Params.Actual_Params'Range loop
            if Params.Actual_Params (J) = Null_Actual_Parameter
              and then not
                Get_Construct
                  (Params.Profile.Params (J)).Attributes (Ada_Assign_Attribute)
            then
               return False;
            end if;
         end loop;

         return True;
      end if;
   end Is_Complete;

   ------------------------------
   -- Get_Number_Of_Parameters --
   ------------------------------

   function Get_Number_Of_Parameters
     (Params : Actual_Parameter_Resolver) return Integer
   is
      pragma Unreferenced (Params);
   begin
      return 0;
   end Get_Number_Of_Parameters;

   -------------------------
   -- Get_Missing_Formals --
   -------------------------

   function Get_Missing_Formals
     (Params : Actual_Parameter_Resolver)
      return Formal_Parameter_Array
   is
      Result : Formal_Parameter_Array
        (1 .. Params.Profile.Params'Length - Params.Params_Set);
      Result_Index : Integer := 1;
   begin
      for J in Params.Profile.Params'Range loop
         if Params.Actual_Params (J) = Null_Actual_Parameter
           and then Params.Profile.Params (J) /= Null_Entity_Access
         then
            Result (Result_Index) :=
              Formal_Parameter (Params.Profile.Params (J));
            Result_Index := Result_Index + 1;
         end if;
      end loop;

      return Result (1 .. Result_Index - 1);
   end Get_Missing_Formals;

   ------------------------------
   -- Any_Named_Formal_Missing --
   ------------------------------

   function Any_Named_Formal_Missing
     (Params : Actual_Parameter_Resolver) return Boolean
   is
   begin
      for J in Params.Profile.Params'Range loop
         if Params.Actual_Params (J) = Null_Actual_Parameter
            and then Params.Profile.Params (J) /= Null_Entity_Access
         then
            return True;
         end if;
      end loop;

      return False;
   end Any_Named_Formal_Missing;

   -------------------------------
   -- Get_Expression_For_Formal --
   -------------------------------

   function Get_Expression_For_Formal
     (Params : Actual_Parameter_Resolver;
      Name   : String) return Parsed_Expression
   is
      Lower_Name : constant String := To_Lower (Name);
   begin
      for J in Params.Profile.Params'Range loop
         if To_Lower
           (Get (Get_Construct (Params.Profile.Params (J)).Name).all) =
           Lower_Name
         then
            return Params.Actual_Params (J).Expression;
         end if;
      end loop;

      return Null_Parsed_Expression;
   end Get_Expression_For_Formal;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Params : Actual_Parameter_Resolver) return List_Profile is
   begin
      return Params.Profile;
   end Get_Profile;

end Ada_Semantic_Tree.List_Resolver;
