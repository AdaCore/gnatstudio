-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2007-2008, AdaCore             --
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

with Ada.Unchecked_Deallocation;
with GNATCOLL.Utils;              use GNATCOLL.Utils;
with Language.Ada;                use Language.Ada;
with GNAT.Strings;                use GNAT.Strings;

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

   function Get_Construct
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

   ----------
   -- Free --
   ----------

   procedure Free (This : in out List_Profile_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (List_Profile, List_Profile_Access);
   begin
      Internal (This);
   end Free;

   ----------------------------------
   -- Get_Formal_Parameter_Profile --
   ----------------------------------

   function Get_List_Profile
     (Entity : Entity_Access) return List_Profile
   is
      Tree  : constant Construct_Tree := Get_Tree (Get_File (Entity));
      Scope : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Entity);
      It    : Construct_Tree_Iterator := Next (Tree, Scope, Jump_Into);

      Result_Array : Entity_Array (1 .. Get_Child_Number (Scope));
      Index        : Integer := 1;
   begin
      if Get_Construct (Entity).Category in Subprogram_Category then
         while It /= Null_Construct_Tree_Iterator
           and then Is_Parent_Scope (Scope, It)
         loop
            if Get_Construct (It).Category = Cat_Parameter then
               Result_Array (Index) := To_Entity_Access
                 (Get_File (Entity), It);
               Index := Index + 1;
            else
               exit;
            end if;

            It := Next (Tree, It, Jump_Over);
         end loop;

         declare
            Result : List_Profile (Index - 1);
         begin
            Result.Params := Result_Array (1 .. Index - 1);

            return Result;
         end;
      else
         if Get_Construct (Entity).Attributes (Ada_Array_Attribute) then
            declare
               Result : List_Profile (1);
            begin
               Result.Extra_Params_Allowed := True;
               Result.Params (1) := Null_Entity_Access;

               return Result;
            end;
         else
            declare
               Result : List_Profile (0);
            begin
               Result.Extra_Params_Allowed := True;

               return Result;
            end;
         end if;
      end if;

   end Get_List_Profile;

   --------------------
   -- Get_Subprogram --
   --------------------

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

   -----------------
   -- Get_Formals --
   -----------------

--     function Get_Formals
--       (Params : List_Profile)
--        return Formal_Parameter_Array
--     is
--     begin
--        return Formal_Parameter_Array (Params.Params);
--     end Get_Formals;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Actual_Parameter) is
   begin
      Token_List.Free (This.Expression.Tokens);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Actual_Parameter_Resolver_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Actual_Parameter_Resolver, Actual_Parameter_Resolver_Access);
   begin
      if This /= null then
         for J in This.Actual_Params'Range loop
            Free (This.Actual_Params (J));
         end loop;
      end if;

      Internal (This);
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy
     (This : Actual_Parameter_Resolver)
      return Actual_Parameter_Resolver
   is
      Result : Actual_Parameter_Resolver (This.Length);
      It     : Token_List.List_Node;
   begin
      Result.Profile := This.Profile;
      Result.Params_Set := This.Params_Set;

      for J in This.Actual_Params'Range loop
         Result.Actual_Params (J) := This.Actual_Params (J);
         Result.Actual_Params (J).Expression.Tokens := Token_List.Null_List;

         It := Token_List.First
           (This.Actual_Params (J).Expression.Tokens);

         while It /= Token_List.Null_Node loop
            Token_List.Append
              (Result.Actual_Params (J).Expression.Tokens,
               Token_List.Data (It));

            It := Next (It);
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

   ----------------------
   -- Get_Start_Offset --
   ----------------------

   function Get_Start_Offset (Param : Actual_Parameter) return Integer is
   begin
      return Data (First (Param.Expression.Tokens)).Token_First;
   end Get_Start_Offset;

   --------------------
   -- Get_End_Offset --
   --------------------

   function Get_End_Offset (Param : Actual_Parameter) return Integer is
   begin
      return Data (Last (Param.Expression.Tokens)).Token_Last;
   end Get_End_Offset;

   --------------------------
   -- Get_Actual_Parameter --
   --------------------------

   function Get_Actual_Parameter
     (Buffer      : UTF8_String_Access;
      Param_Start : Natural;
      Param_End   : Natural) return Actual_Parameter
   is
      Result : Actual_Parameter;
      It     : Token_List.List_Node;
   begin
      Result.Is_Named := False;

      Result.Expression := Parse_Current_List (Buffer, Param_End, Param_Start);

      if Length (Result.Expression.Tokens) >= 2 then
         It := First (Result.Expression.Tokens);
         It := Next (It);

         if Data (It).Tok_Type = Tok_Arrow then
            Result.Is_Named := True;
         end if;
      end if;

      return Result;
   end Get_Actual_Parameter;

   ---------------------------
   -- Get_Actual_Parameters --
   ---------------------------

   function Get_Actual_Parameters
     (Entity : Entity_Access) return Actual_Params_Array
   is
      Expression : Parsed_Expression;
   begin
      Expression := Parse_Current_List
        (UTF8_String_Access (Get_Buffer (Get_File (Entity))),
         Get_Construct (Entity).Sloc_End.Index,
         Get_Construct (Entity).Sloc_Start.Index);

      declare
         Actuals : Actual_Params_Array (1 .. Length (Expression.Tokens));
         Index   : Integer := 0;
         Node    : Token_List.List_Node := First (Expression.Tokens);
      begin
         while Node /= Token_List.Null_Node loop
            if Data (Node).Tok_Type = Tok_Expression then
               Index := Index + 1;

               --  ??? It's a bit of a shame to have to re-do an analysis while
               --  we already made a pass over the buffer - the parser should
               --  be enhanced to optionally retreive the sub expression in
               --  such a case.

               Actuals (Index) := Get_Actual_Parameter
                 (UTF8_String_Access (Get_Buffer (Get_File (Entity))),
                  Data (Node).Token_Last,
                  Data (Node).Token_First);
            end if;

            Node := Next (Node);
         end loop;

         return Actuals (1 .. Index);
      end;
   end Get_Actual_Parameters;

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
                     (Params.Profile.Params (J))).Name /= null
            then
               if Equal
                 (Get_Name
                    (Actual.Expression,
                     Data (First (Actual.Expression.Tokens))),
                  Get_Construct
                    (To_Construct_Tree_Iterator
                       (Params.Profile.Params (J))).Name.all,
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

   -------------------------------
   -- Any_Named_Formal_Missingg --
   -------------------------------

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

end Ada_Semantic_Tree.List_Resolver;
