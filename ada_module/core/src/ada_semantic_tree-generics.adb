------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Language.Ada;                    use Language.Ada;
with Ada_Semantic_Tree.Parts;         use Ada_Semantic_Tree.Parts;
with Ada_Semantic_Tree.Declarations;  use Ada_Semantic_Tree.Declarations;
with Ada_Semantic_Tree.Units;         use Ada_Semantic_Tree.Units;
with Ada_Semantic_Tree.Cache;         use Ada_Semantic_Tree.Cache;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Symbols;                use GNATCOLL.Symbols;
with GNATCOLL.Utils;                  use GNATCOLL.Utils;

package body Ada_Semantic_Tree.Generics is

   procedure Append_Context
     (Info  : Instance_Info;
      Added : Instance_Info);

   procedure Prepend_Context
     (Info  : Instance_Info;
      Added : Instance_Info);

   function Make_Generic_Instance (E : Entity_Access) return Instance_Info;

   type Instanciated_Package is new Cached_Information with record
      Generic_Package : Entity_Persistent_Access;
      Generic_Context : Persistent_Instance_Info;
   end record;

   overriding procedure Free (This : in out Instanciated_Package);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
   begin
      null;
   end Register_Assistant;

   ---------
   -- "&" --
   ---------

   function "&" (Left, Right : Instance_Info) return Instance_Info
   is
      Result : Instance_Info;
   begin
      if Left = Null_Instance_Info
        and then Right = Null_Instance_Info
      then
         return Null_Instance_Info;
      end if;

      if Left = Null_Instance_Info then
         return Right;
      end if;

      if Right = Null_Instance_Info then
         return Left;
      end if;

      Result := new Instance_Info_Record;

      Prepend_Context (Result, Left);
      Append_Context (Result, Right);

      return Result;
   end "&";

   ---------
   -- Ref --
   ---------

   procedure Ref (This : Instance_Info) is
   begin
      if This /= null then
         This.Refs := This.Refs + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (This : in out Instance_Info) is
      use Generic_Info_List;

      procedure Free is new Ada.Unchecked_Deallocation
        (Instance_Info_Record, Instance_Info);

   begin
      if This /= null then
         This.Refs := This.Refs - 1;

         if This.Refs <= 0 then
            for Item of This.Pre_Contexts loop
               Unref (Item);
            end loop;

            for Item of This.Post_Contexts loop
               Unref (Item);
            end loop;

            Free (This.Resolver);
            Free (This);
         end if;
      end if;
   end Unref;

   ---------------------------
   -- Make_Generic_Instance --
   ---------------------------

   function Make_Generic_Instance (E : Entity_Access) return Instance_Info is
      View : Entity_View;
      Generic_Package : Entity_Access;
      Result          : Instance_Info := null;
   begin
      View := Get_Generic_Entity (E);

      if View /= Null_Entity_View then
         Generic_Package := Get_First_Occurence (Get_Entity (View));

         declare
            Profile : constant List_Profile :=
              Get_List_Profile
                (Generic_Package,
                 Null_Visibility_Context,
                 Generic_Profile);
            Success : Boolean;
         begin
            Result := new Instance_Info_Record;

            Result.Instance_Package := E;
            Result.Generic_Package := Generic_Package;
            Result.Resolver := new Actual_Parameter_Resolver'
              (Get_Actual_Parameter_Resolver (Profile));

            if View.all in Declaration_View_Record'Class then
               if Declaration_View_Record
                 (View.all).Generic_Context
                 /= Null_Instance_Info
               then
                  Prepend_Context
                    (Result,
                     Declaration_View_Record
                       (View.all).Generic_Context);
               end if;
            end if;

            Append_Actuals
              (Result.Resolver.all,
               Get_Buffer (Get_File (E)),
               String_Index_Type (Get_Construct (E).Sloc_Start.Index),
               Success);

            Free (View);
         end;
      end if;

      return Result;
   end Make_Generic_Instance;

   -------------------------
   -- Is_Generic_Instance --
   -------------------------

   function Is_Generic_Instance (Entity : Entity_Access) return Boolean is
   begin
      return
        (Get_Construct (Entity).Category = Cat_Package
         or else Get_Construct (Entity).Category in Subprogram_Category)
        and then Get_Construct (Entity).Attributes (Ada_New_Attribute);
   end Is_Generic_Instance;

   --------------------------------------
   -- Get_Generic_Instance_Information --
   --------------------------------------

   function Get_Generic_Instance_Information
     (Entity : Entity_Access) return Instance_Info
   is
   begin
      if Is_Generic_Instance (Entity) then
         return Make_Generic_Instance (Entity);
      else
         return null;
      end if;
   end Get_Generic_Instance_Information;

   ----------------------------
   -- Is_Viewed_From_Generic --
   ----------------------------

   function Is_Viewed_From_Generic
     (Entity : Entity_Access; Visibility : Visibility_Context) return Boolean
   is
      pragma Unreferenced (Entity, Visibility);
   begin
      return False;
   end Is_Viewed_From_Generic;

   -----------------------
   -- Is_Generic_Entity --
   -----------------------

   function Is_Generic_Entity (Entity : Entity_Access) return Boolean is
      It : Construct_Tree_Iterator :=
        Get_Parent_Scope
          (Get_Tree (Get_File (Entity)),
           To_Construct_Tree_Iterator (Entity));
      Enclosing_Entity : Entity_Access;
   begin
      --  ??? This may be a bit of a slow computation - should we cache the
      --  information instead?

      Enclosing_Entity :=
        Get_First_Occurence (To_Entity_Access (Get_File (Entity), It));
      It := To_Construct_Tree_Iterator (Enclosing_Entity);

      while It /= Null_Construct_Tree_Iterator loop
         Enclosing_Entity :=
           Get_First_Occurence
             (To_Entity_Access (Get_File (Enclosing_Entity), It));

         if Get_Construct (Enclosing_Entity).Attributes
           (Ada_Generic_Attribute)
         then
            return True;
         end if;

         It := Get_Parent_Scope
           (Get_Tree (Get_File (Enclosing_Entity)),
            To_Construct_Tree_Iterator (Enclosing_Entity));
      end loop;

      return False;
   end Is_Generic_Entity;

   ----------------------------------
   -- Get_Actual_For_Generic_Param --
   ----------------------------------

   function Get_Actual_For_Generic_Param
     (Info   : Instance_Info; Formal : Entity_Access) return Entity_Access
   is
      use Generic_Info_List;

      Result : Entity_Access := Null_Entity_Access;
   begin
      for Item of Info.Pre_Contexts loop
         Result := Get_Actual_For_Generic_Param (Item, Formal);

         if Result /= Null_Entity_Access then
            return Result;
         end if;
      end loop;

      if Info.Resolver /= null then
         declare
            Formals : constant Entity_Array :=
              Get_Formals (Get_Profile (Info.Resolver.all));
         begin
            for J in Formals'Range loop
               if Formals (J) = Formal then
                  declare
                     Actual_Expression : constant Parsed_Expression :=
                       Get_Expression_For_Formal
                         (Info.Resolver.all,
                          Get (Get_Construct (Formals (J)).Name).all);
                     Actual_Resolution : Entity_List;
                     Actual_It         : Entity_Iterator;
                  begin
                     if Actual_Expression /= Null_Parsed_Expression then
                        Actual_Resolution :=
                          Find_Declarations
                            (Context           =>
                                 (From_File,
                                  Null_Instance_Info,
                                  Get_File (Info.Instance_Package),
                                  String_Index_Type
                                    (Get_Construct
                                       (Info.Instance_Package).
                                       Sloc_Start.Index)),
                             From_Visibility   =>
                               (File                 =>
                                  Get_File (Info.Instance_Package),
                                Offset               =>
                                  String_Index_Type
                                    (Get_Construct
                                         (Info.Instance_Package).
                                         Sloc_Start.Index),
                                Filter                    => Everything,
                                Min_Visibility_Confidence => Use_Visible),
                             Expression        => Actual_Expression,
                             Filter            => Filter_Types);
                     end if;

                     Actual_It := First (Actual_Resolution);

                     if not At_End (Actual_It) then
                        Result := Get_Entity (Actual_It);
                     end if;

                     Free (Actual_It);
                     Free (Actual_Resolution);
                  end;

                  exit;
               end if;
            end loop;
         end;
      end if;

      if Result /= Null_Entity_Access then
         return Result;
      end if;

      for Item of Info.Post_Contexts loop
         Result := Get_Actual_For_Generic_Param (Item, Formal);

         if Result /= Null_Entity_Access then
            return Result;
         end if;
      end loop;

      return Null_Entity_Access;
   end Get_Actual_For_Generic_Param;

   ------------------------
   -- Get_Generic_Entity --
   ------------------------

   function Get_Generic_Entity (Info : Instance_Info) return Entity_Access
   is
   begin
      if Info /= null then
         return Info.Generic_Package;
      else
         return Null_Entity_Access;
      end if;
   end Get_Generic_Entity;

   ------------------------
   -- Get_Generic_Entity --
   ------------------------

   function Get_Generic_Entity (Info : Entity_Access) return Entity_View is
      Result : Entity_View;
      Cached : Cache_Access := Get_Cache (Info);
   begin
      if Cached /= null then
         Result := new Declaration_View_Record;
         Declaration_View_Record (Result.all).Generic_Context :=
              To_Active
                (Instanciated_Package (Cached.all).Generic_Context);
         Result.Entity := To_Entity_Access
           (Instanciated_Package (Cached.all).Generic_Package);

         Ref (Declaration_View_Record (Result.all).Generic_Context);

         return Result;
      end if;

      declare
         Id : constant Cst_String_Access := Get (Get_Identifier
           (Get_Referenced_Identifiers
              (To_Construct_Tree_Iterator (Info))),
            Empty_If_Null => True);
         Expression : Parsed_Expression :=
           Parse_Expression_Backward
             (Id, String_Index_Type (Id'Last));
         Generic_Resolution : Entity_List;
         It                 : Entity_Iterator;
      begin
         Generic_Resolution := Find_Declarations
           (Context           =>
              (From_File,
               Null_Instance_Info,
               Get_File (Info),
               String_Index_Type (Get_Construct (Info).Sloc_Start.Index)),
            From_Visibility   =>
              (File                      => Get_File (Info),
               Offset                    =>
                 String_Index_Type (Get_Construct (Info).Sloc_Start.Index),
               Filter                    => Everything,
               Min_Visibility_Confidence => Use_Visible),
            Expression        => Expression,
            Filter            => Create
              ((1 => Get_Construct (Info).Category)));

         It := First (Generic_Resolution);
         Excluded_Entities.Append (It.Excluded_List.Entities, Info);

         --  We only consider the first matching resolution, ignore the other
         --  potential ones.

         if not At_End (It) then
            Result := Get_View (It);
         else
            Result := Null_Entity_View;
         end if;

         Free (It);
         Free (Generic_Resolution);
         Free (Expression);

         if Result /= null then
            Cached := new Instanciated_Package'
              (Cached_Information with
               Generic_Package => To_Entity_Persistent_Access
                 (Result.Get_Entity),
               Generic_Context => Null_Persistent_Instance_Info);

            if Result.all in Declaration_View_Record'Class then
               Instanciated_Package (Cached.all).Generic_Context :=
                 To_Persistent
                   (Declaration_View_Record (Result.all).Generic_Context);
            end if;

            Set_Cache (Info, Cached);
         end if;

         return Result;
      end;
   end Get_Generic_Entity;

   --------------------
   -- Append_Context --
   --------------------

   procedure Append_Context (Info  : Instance_Info; Added : Instance_Info)
   is
   begin
      if Added /= Null_Instance_Info then
         Info.Post_Contexts.Append (Added);
         Ref (Added);
      end if;
   end Append_Context;

   ---------------------
   -- Prepend_Context --
   ---------------------

   procedure Prepend_Context (Info  : Instance_Info; Added : Instance_Info) is
   begin
      if Added /= Null_Instance_Info then
         Info.Pre_Contexts.Prepend (Added);
         Ref (Added);
      end if;
   end Prepend_Context;

   -------------------
   -- To_Persistent --
   -------------------

   function To_Persistent
     (Instance : Instance_Info) return Persistent_Instance_Info
   is
      use Generic_Info_List;

      Result : Persistent_Instance_Info;
      Cur : Generic_Info_List.Cursor;
   begin
      if Instance = Null_Instance_Info then
         return null;
      end if;

      Result := new Persistent_Instance_Info_Record'
        (Instance_Package =>
           To_Entity_Persistent_Access (Instance.Instance_Package),
         Generic_Package  =>
           To_Entity_Persistent_Access (Instance.Generic_Package),
         others => <>);

      Cur := Instance.Pre_Contexts.First;

      while Has_Element (Cur) loop
         Result.Pre_Contexts.Append (To_Persistent (Element (Cur)));
         Cur := Next (Cur);
      end loop;

      Cur := Instance.Post_Contexts.First;

      while Has_Element (Cur) loop
         Result.Post_Contexts.Append (To_Persistent (Element (Cur)));
         Cur := Next (Cur);
      end loop;

      return Result;
   end To_Persistent;

   ---------------
   -- To_Active --
   ---------------

   function To_Active
     (Instance : Persistent_Instance_Info) return Instance_Info
   is
      use Persistent_Generic_Info_List;

      Result  : Instance_Info;
      Success : Boolean;
   begin
      if Instance = null then
         return Null_Instance_Info;
      end if;

      Result := new Instance_Info_Record'
        (Instance_Package =>
           To_Entity_Access (Instance.Instance_Package),
         Generic_Package  =>
           To_Entity_Access (Instance.Generic_Package),
         others => <>);

      Result.Resolver := new Actual_Parameter_Resolver'
        (Get_Actual_Parameter_Resolver
           (Get_List_Profile
              (Result.Generic_Package,
               Null_Visibility_Context,
               Generic_Profile)));

      if Result.Instance_Package /= Null_Entity_Access then
         Append_Actuals
           (Result.Resolver.all,
            Get_Buffer (Get_File (Result.Instance_Package)),
            String_Index_Type
              (Get_Construct (Result.Instance_Package).Sloc_Start.Index),
            Success);
      end if;

      for Item of Instance.Pre_Contexts loop
         Prepend_Context (Result, To_Active (Item));
      end loop;

      for Item of Instance.Post_Contexts loop
         Append_Context (Result, To_Active (Item));
      end loop;

      return Result;
   end To_Active;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date
     (This : Persistent_Instance_Info) return Boolean
   is
      use Persistent_Generic_Info_List;
   begin
      if not Exists (This.Instance_Package)
        or else not Exists (This.Generic_Package)
      then
         return False;
      end if;

      for Item of This.Pre_Contexts loop
         if not Is_Up_To_Date (Item) then
            return False;
         end if;
      end loop;

      for Item of This.Post_Contexts loop
         if not Is_Up_To_Date (Item) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Up_To_Date;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Persistent_Instance_Info) is
      use Persistent_Generic_Info_List;

      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Persistent_Instance_Info_Record,
         Persistent_Instance_Info);
   begin
      if This = null then
         return;
      end if;

      for Item of This.Pre_Contexts loop
         Free (Item);
      end loop;

      for Item of This.Post_Contexts loop
         Free (Item);
      end loop;

      Unref (This.Instance_Package);
      Unref (This.Generic_Package);

      Internal_Free (This);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (This : in out Instanciated_Package) is
   begin
      Free (This.Generic_Context);
      Unref (This.Generic_Package);
   end Free;

end Ada_Semantic_Tree.Generics;
