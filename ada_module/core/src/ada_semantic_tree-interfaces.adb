------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with GNATCOLL.Symbols;        use GNATCOLL.Symbols;
with GNATCOLL.Utils;          use GNATCOLL.Utils;

with Language.Ada;            use Language.Ada;

package body Ada_Semantic_Tree.Interfaces is

   Interfaces_Assistant_Id : constant String := "ADA_INTERFACES_ASSISTANT";

   package Exported_Entities is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Entity_Persistent_Access);

   type Interfaces_Db_Assistant is new Database_Assistant with record
      Import_Key : Construct_Annotations_Pckg.Annotation_Key;
      Exports    : Exported_Entities.Map;
   end record;

   use Construct_Annotations_Pckg;

   type Interface_Annotation_Record is new General_Annotation_Record
   with record
      Name       : String_Access;
      Convention : String_Access;
   end record;

   overriding procedure Free
     (Obj : in out Interface_Annotation_Record);

   overriding
   procedure File_Updated
     (Assistant : access Interfaces_Db_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind);

   overriding procedure Free (Assistant : in out Interfaces_Db_Assistant);

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant (Db : Construct_Database_Access) is
      Import_Key : Construct_Annotations_Pckg.Annotation_Key;

   begin
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Db).all,
         Import_Key);

      Register_Assistant
        (Db,
         Interfaces_Assistant_Id,
         new Interfaces_Db_Assistant'
           (Database_Assistant with
            Import_Key => Import_Key,
            others     => <>));
   end Register_Assistant;

   -------------------
   -- Get_Assistant --
   -------------------

   function Get_Assistant
     (Db : Construct_Database_Access) return Database_Assistant_Access
   is
   begin
      return Get_Assistant (Db, Interfaces_Assistant_Id);
   end Get_Assistant;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Obj : in out Interface_Annotation_Record)
   is
   begin
      Free (Obj.Name);
      Free (Obj.Convention);
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Assistant : in out Interfaces_Db_Assistant) is
      Value : Entity_Persistent_Access;
   begin
      for X of Assistant.Exports loop
         Value := X;
         Unref (Value);
      end loop;

      Assistant.Exports.Clear;
   end Free;

   ------------------
   -- File_Updated --
   ------------------

   function Lt_Nocase (Left, Right : String) return Boolean;

   function Lt_Nocase (Left, Right : String) return Boolean is
      Lc, Rc : Character;
   begin
      for J in 1 .. Left'Length loop
         if J > Right'Length then
            return False;
         end if;

         Lc := To_Lower (Left (Left'First + J - 1));
         Rc := To_Lower (Right (Right'First + J - 1));

         if Lc /= Rc then
            return Lc < Rc;
         end if;
      end loop;

      return Right'Length > Left'Length;
   end Lt_Nocase;

   type Interface_Kind is (Import, Export);

   type Interface_Pragma
     (Name_Length : Integer; Conv_Length : Integer)
      is record

         Name       : String (1 .. Name_Length);
         Convention : String (1 .. Conv_Length);
         Kind       : Interface_Kind;
   end record;

   package Name_Association is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Interface_Pragma, "<" => Lt_Nocase);

   ------------------
   -- File_Updated --
   ------------------

   overriding procedure File_Updated
     (Assistant : access Interfaces_Db_Assistant;
      File      : Structured_File_Access;
      Old_Tree  : Construct_Tree;
      Kind      : Update_Kind)
   is
      pragma Unreferenced (Kind, Old_Tree);

      Tree      : constant Construct_Tree := Get_Tree (File);
      It        : Construct_Tree_Iterator := Last (Tree);
      Buffer    : constant String_Access := Get_Buffer (File);

      Current_Associations : Name_Association.Map;

      function Parse_Pragma
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
        with Pre => Sloc_Start.Index > 0
          and Sloc_End.Index >= Sloc_Start.Index;

      type Arg_Rec is record
         Sloc_Start : Source_Location;
         Sloc_End   : Source_Location;
      end record;

      Null_Arg_Rec : constant Arg_Rec := ((0, 0, 0), (0, 0, 0));

      type Arg_Index is (Convention, Entity, External_Name, Link_Name);

      Parsed_Info : array (Arg_Index) of Arg_Rec := (others => Null_Arg_Rec);

      Current_Arg_Index : Arg_Index := Arg_Index'First;
      Last_Arg          : Arg_Rec := Null_Arg_Rec;
      Is_Positional     : Boolean := False;

      ------------------
      -- Parse_Pragma --
      ------------------

      function Parse_Pragma
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Entity, Partial_Entity);

         Contents    : constant String := Buffer
           (Sloc_Start.Index .. Sloc_End.Index);
      begin
         if Contents = "," then
            Parsed_Info (Current_Arg_Index) := Last_Arg;

            if not Is_Positional
              and then Current_Arg_Index < Arg_Index'Last
            then
               Current_Arg_Index := Arg_Index'Succ (Current_Arg_Index);
            end if;
         elsif Contents = "(" then
            Current_Arg_Index := Arg_Index'First;
            Last_Arg          := Null_Arg_Rec;
            Is_Positional     := False;
            Parsed_Info       := (others => Null_Arg_Rec);
         elsif Contents = ")" then
            Parsed_Info (Current_Arg_Index) := Last_Arg;
            return True;
         elsif Contents = ";" then
            return True;
         elsif Contents = "=>" then
            Is_Positional := True;

            Current_Arg_Index :=
              Arg_Index'Value
                (Buffer
                     (Last_Arg.Sloc_Start.Index .. Last_Arg.Sloc_End.Index));
         else
            Last_Arg := (Sloc_Start, Sloc_End);
         end if;

         return False;
      end Parse_Pragma;

      Import_Pragma : constant String := "import";
      Export_Pragma : constant String := "export";
      Current_Interface_Kind : Interface_Kind;

   begin
      while It /= Null_Construct_Tree_Iterator loop
         if Get_Construct (It).Category = Cat_Pragma then
            --  If this is a pragma, then extract the import / export
            --  information

            if Get_Construct (It).Name /= No_Symbol
              and then
                (Equal
                     (Get (Get_Construct (It).Name).all, Import_Pragma, False)
                 or else Equal
                   (Get (Get_Construct (It).Name).all, Export_Pragma, False))
            then
               if Equal
                 (Get (Get_Construct (It).Name).all, Import_Pragma, False)
               then
                  Current_Interface_Kind := Import;

                  Parse_Entities
                    (Ada_Lang,
                     Get_Buffer (File)
                     (Get_Construct (It).Sloc_Entity.Index
                        + Import_Pragma'Length
                        .. Get_Construct (It).Sloc_End.Index),
                    Parse_Pragma'Unrestricted_Access);
               else
                  Current_Interface_Kind := Export;

                  Parse_Entities
                    (Ada_Lang,
                     Get_Buffer (File)
                     (Get_Construct (It).Sloc_Entity.Index
                        + Export_Pragma'Length
                        .. Get_Construct (It).Sloc_End.Index),
                    Parse_Pragma'Unrestricted_Access);
               end if;

               if Parsed_Info (Entity) /= Null_Arg_Rec then
                  if Parsed_Info (External_Name) /= Null_Arg_Rec then
                     declare
                        Ext_Name : constant String :=
                          Buffer
                            (Parsed_Info (External_Name).Sloc_Start.Index ..
                               Parsed_Info (External_Name).Sloc_End.Index);
                        Ent_Name : constant String :=
                          Buffer
                            (Parsed_Info (Entity).Sloc_Start.Index ..
                               Parsed_Info (Entity).Sloc_End.Index);
                        Conv_Name : constant String :=
                          Buffer
                            (Parsed_Info (Convention).Sloc_Start.Index ..
                               Parsed_Info (Convention).Sloc_End.Index);
                     begin
                        if Ext_Name'Length >= 2
                          and then Ext_Name (Ext_Name'First) = '"'
                          and then not Current_Associations.Contains (Ent_Name)
                        then
                           --  We only manage real strings

                           Current_Associations.Insert
                             (Ent_Name,
                              (Ext_Name'Length - 2,
                               Conv_Name'Length,
                               Ext_Name
                                 (Ext_Name'First + 1 .. Ext_Name'Last - 1),
                               Conv_Name,
                               Current_Interface_Kind));
                        end if;
                     end;
                  else
                     declare
                        Ent_Name : constant String :=
                          Buffer
                            (Parsed_Info (Entity).Sloc_Start.Index ..
                               Parsed_Info (Entity).Sloc_End.Index);
                        Conv_Name : constant String :=
                          Buffer
                            (Parsed_Info (Convention).Sloc_Start.Index ..
                               Parsed_Info (Convention).Sloc_End.Index);
                     begin
                        if not Current_Associations.Contains (Ent_Name) then
                           Current_Associations.Insert
                             (Ent_Name,
                              (Ent_Name'Length,
                               Conv_Name'Length,
                               Ent_Name,
                               Conv_Name,
                               Current_Interface_Kind));
                        end if;
                     end;
                  end if;
               end if;
            end if;
         elsif Get_Construct (It).Name /= No_Symbol then
            --  If this is a named construct, then check if there is an
            --  applicable pragma

            --  ??? should check the nesting level...

            if Current_Associations.Contains
              (Get (Get_Construct (It).Name).all)
            then
               declare
                  P : constant Interface_Pragma := Current_Associations.Element
                    (Get (Get_Construct (It).Name).all);
                  Entity            : Entity_Access;
                  Entity_Persistent : Entity_Persistent_Access;
                  Annot             : Annotation;
               begin
                  Entity := To_Entity_Access (File, It);

                  case P.Kind is
                     when Import =>
                        Annot :=
                          (Other_Kind,
                           new Interface_Annotation_Record'
                             (General_Annotation_Record
                              with
                              Name       => new String'(P.Name),
                              Convention => new String'(P.Convention)));

                        Set_Annotation
                          (Get_Annotation_Container (Tree, It).all,
                           Assistant.Import_Key,
                           Annot);

                     when Export =>
                        if Assistant.Exports.Contains (P.Name) then
                           --  If there's already a reference, then remove it
                           --  from the base. It's either OBE or duplicate, and
                           --  we don't handle either of the two cases.

                           Entity_Persistent :=
                             Assistant.Exports.Element (P.Name);

                           Unref (Entity_Persistent);

                           Assistant.Exports.Delete (P.Name);
                        end if;

                        Entity_Persistent :=
                          To_Entity_Persistent_Access (Entity);
                        Assistant.Exports.Insert (P.Name, Entity_Persistent);
                  end case;
               end;

               Current_Associations.Delete
                 (Get (Get_Construct (It).Name).all);
            end if;
         end if;

         It := Prev (Tree, It, Jump_Into);
      end loop;
   end File_Updated;

   -------------------------
   -- Get_Exported_Entity --
   -------------------------

   function Get_Exported_Entity
     (Assistant : Database_Assistant_Access; Name : String)
      return Entity_Access
   is
      My_Assistant : Interfaces_Db_Assistant renames
        Interfaces_Db_Assistant (Assistant.all);

      Entity : Entity_Persistent_Access;
   begin
      if My_Assistant.Exports.Contains (Name) then
         Entity := My_Assistant.Exports.Element (Name);

         if not Exists (Entity) then
            Unref (Entity);
            My_Assistant.Exports.Delete (Name);

            return Null_Entity_Access;
         end if;
      end if;

      return To_Entity_Access (Entity);
   end Get_Exported_Entity;

   -------------------------
   -- Get_Imported_Entity --
   -------------------------

   function Get_Imported_Entity
     (Entity : Entity_Access) return Imported_Entity
   is
      Assistant : constant Database_Assistant_Access :=
        Get_Assistant (Get_Database (Get_File (Entity)));

      My_Assistant : Interfaces_Db_Assistant renames
        Interfaces_Db_Assistant (Assistant.all);
      Annot : Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (Entity)),
            To_Construct_Tree_Iterator (Entity)).all,
         My_Assistant.Import_Key,
         Annot);

      if Annot /= Null_Annotation then
         return
           (Name       =>
              Interface_Annotation_Record (Annot.Other_Val.all).Name,
            Convention =>
              Interface_Annotation_Record (Annot.Other_Val.all).Convention);
      else
         return Null_Imported_Entity;
      end if;
   end Get_Imported_Entity;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Entity : Imported_Entity) return String is
   begin
      return Entity.Name.all;
   end Get_Name;

   --------------------
   -- Get_Convention --
   --------------------

   function Get_Convention (Entity : Imported_Entity) return String is
   begin
      return Entity.Convention.all;
   end Get_Convention;

end Ada_Semantic_Tree.Interfaces;
