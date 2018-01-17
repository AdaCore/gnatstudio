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

with XML_Utils;   use XML_Utils;
with XML_Parsers; use XML_Parsers;
with Ada.Unchecked_Deallocation;
with Tries;
with GNATCOLL.Utils;  use GNATCOLL.Utils;

with Ada.Characters.Handling;
use Ada.Characters.Handling;

package body Ada_Semantic_Tree.Std_Entities is

   Std_Entities_Assistant_Id : constant String := "STD_ENTITIES_ASSISTANT";

   function Get_Index (Desc : Std_Description) return Cst_String_Access;

   procedure Free (This : in out Std_Description);

   package Std_Description_Tries is new Tries
     (Std_Description, null, Get_Index);

   type Std_Entities_Db is new Database_Assistant with record
      Aspects_Trie    : Std_Description_Tries.Trie_Tree_Access;
      Attributes_Trie : Std_Description_Tries.Trie_Tree_Access;
      Pragmas_Trie    : Std_Description_Tries.Trie_Tree_Access;
      Standard_Trie   : Std_Description_Tries.Trie_Tree_Access;
      ASCII_Trie      : Std_Description_Tries.Trie_Tree_Access;

      Standard_Entity : Std_Description;
   end record;

   overriding procedure Free (Assistant : in out Std_Entities_Db);

   type Std_Entities_Db_Assistant is access all Std_Entities_Db'Class;

   type Std_Entity_Record is new Entity_View_Record with record
      Desc : Std_Description;
      Db   : Construct_Database_Access;
   end record;

   overriding function Get_Documentation
     (E : access Std_Entity_Record) return String;
   --  Return the documentation associaded to this standard entity view.

   overriding function Get_Name
     (E : access Std_Entity_Record) return Basic_Types.UTF8_String;

   overriding function Get_Category
     (E : access Std_Entity_Record) return Language_Category;

   overriding procedure Fill_Children
     (E               : access Std_Entity_Record;
      From_Visibility : Visibility_Context;
      Name            : String;
      Is_Partial      : Boolean;
      Filter          : Entity_Filter;
      Result          : in out Entity_List);

   procedure Get_Possible_ASCII_Entities
     (Db     : Construct_Database_Access;
      Prefix : String;
      Result : in out Entity_List);
   --  Add to the entity list the list of entities coming from the ASCII
   --  standard package matching the prefix given in parameter.

   -----------------------------
   -- Virtual list facilities --
   -----------------------------

   type Std_Mode_Type is
     (Aspect_Mode, Pragma_Mode, Attribute_Mode, Standard_Mode, ASCII_Mode,
      Exceptions_Mode);

   type Std_List is new Entity_List_Pckg.Virtual_List_Component
   with record
      Db                       : Construct_Database_Access;
      Prefix                   : String_Access;
      Context                  : Context_Of_Use_Array;
      Mode                     : Std_Mode_Type;
      Exclude_Standard_Package : Boolean := False;
      Is_Partial               : Boolean;
   end record;

   overriding procedure Free (Component : in out Std_List);

   type Std_Iterator is new
     Entity_List_Pckg.Virtual_List_Component_Iterator
   with record
      It                       : Std_Description_Tries.Iterator;
      Db                       : Construct_Database_Access;
      Exclude_Standard_Package : Boolean;
      Is_Partial               : Boolean;
      Lowercased_Name          : String_Access;
      Exceptions_Only          : Boolean;
   end record;

   overriding function First
     (List : Std_List)
      return Entity_List_Pckg.Virtual_List_Component_Iterator'Class;

   overriding function At_End (It : Std_Iterator) return Boolean;

   overriding procedure Next (It : in out Std_Iterator);

   overriding function Get (It : in out Std_Iterator) return Entity_View;

   overriding procedure Free (It : in out Std_Iterator);

   function Is_Valid (It : Std_Iterator'Class) return Boolean;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant
     (Database : Construct_Database_Access; From_File : Virtual_File)
   is
      New_Assistant : constant Std_Entities_Db_Assistant :=
        new Std_Entities_Db;

      type Element_Kind is (An_Aspect, A_Pragma, An_Attribute,
                            A_Standard_Component);

      procedure Analyze_Element
        (Node :  XML_Utils.Node_Ptr; Kind : Element_Kind);

      ---------------------
      -- Analyze_Element --
      ---------------------

      procedure Analyze_Element
        (Node :  XML_Utils.Node_Ptr; Kind : Element_Kind)
      is
         Current : XML_Utils.Node_Ptr := Node.Child;

         New_Element : constant Std_Description :=
           new Std_Description_Record;

         Is_Standard_Ada : Boolean := False;
         Doc : String_Access;
         Category : Language_Category := Cat_Unknown;
      begin
         New_Element.Name := new String'(Get_Attribute (Node, "name"));

         if Get_Attribute (Node, "category") /= "" then
            Category := Language_Category'Value
              ("CAT_" & Get_Attribute (Node, "category"));
         end if;

         Is_Standard_Ada := Get_Attribute (Node, "origin") = "Ada RM";

         New_Element.Index := new String'
           (To_Lower (Get_Attribute (Node, "name")));

         while Current /= null loop
            if Current.Tag.all = "DOC" then
               Doc := String_Access (Current.Value);
            end if;

            Current := Current.Next;
         end loop;

         if Kind = A_Pragma or else Kind = An_Attribute
           or else Kind = An_Aspect
         then
            if Is_Standard_Ada then
               New_Element.Origin := Ada_Standard;
               New_Element.Documentation := new String'(Doc.all);
            else
               New_Element.Origin := GNAT_Specific;
               New_Element.Documentation := new String'(Doc.all);
            end if;
         end if;

         case Kind is
            when An_Aspect =>
               New_Element.Category := Cat_With;

               Std_Description_Tries.Insert
                 (New_Assistant.Aspects_Trie.all, New_Element);

            when A_Pragma =>
               New_Element.Category := Cat_Pragma;

               Std_Description_Tries.Insert
                 (New_Assistant.Pragmas_Trie.all, New_Element);

            when An_Attribute =>
               New_Element.Category := Category;

               Std_Description_Tries.Insert
                 (New_Assistant.Attributes_Trie.all, New_Element);

            when A_Standard_Component =>
               New_Element.Category := Category;

               Std_Description_Tries.Insert
                 (New_Assistant.Standard_Trie.all, New_Element);

         end case;
      end Analyze_Element;

      Root_Node : XML_Utils.Node_Ptr;
      Errors    : String_Access;

      Current : XML_Utils.Node_Ptr;

   begin
      New_Assistant.Aspects_Trie :=
        new Std_Description_Tries.Trie_Tree (True);
      New_Assistant.Attributes_Trie :=
        new Std_Description_Tries.Trie_Tree (True);
      New_Assistant.Pragmas_Trie :=
        new Std_Description_Tries.Trie_Tree (True);
      New_Assistant.Standard_Trie :=
        new Std_Description_Tries.Trie_Tree (True);
      New_Assistant.ASCII_Trie :=
        new Std_Description_Tries.Trie_Tree (True);

      Register_Assistant
        (Db        => Database,
         Name      => Std_Entities_Assistant_Id,
         Assistant => Database_Assistant_Access (New_Assistant));

      if From_File = No_File then
         return;
      end if;

      XML_Parsers.Parse (From_File, Root_Node, Errors);

      Current := Root_Node.Child;

      while Current /= null loop
         if Current.Tag.all = "ATTRIBUTE" then
            Analyze_Element (Current, An_Attribute);
         elsif Current.Tag.all = "ASPECT" then
            Analyze_Element (Current, An_Aspect);
         elsif Current.Tag.all = "PRAGMA" then
            Analyze_Element (Current, A_Pragma);
         elsif Current.Tag.all = "STANDARD" then
            Analyze_Element (Current, A_Standard_Component);

            if Get_Attribute (Current, "name") = "ASCII" then
               declare
                  Ascii_Node  : XML_Utils.Node_Ptr := Current.Child;
                  New_Element : Std_Description;
               begin
                  while Ascii_Node /= null loop
                     New_Element := new Std_Description_Record;
                     New_Element.Name := new String'
                       (Get_Attribute (Ascii_Node, "name"));
                     New_Element.Index := new String'
                       (To_Lower (Get_Attribute (Ascii_Node, "name")));
                     New_Element.Documentation := new String'
                       (Get_Attribute (Ascii_Node, "doc"));
                     New_Element.Category := Cat_Variable;

                     Std_Description_Tries.Insert
                       (New_Assistant.ASCII_Trie.all,
                        New_Element);

                     Ascii_Node := Ascii_Node.Next;
                  end loop;
               end;
            end if;
         end if;

         Current := Current.Next;
      end loop;

      New_Assistant.Standard_Entity := new Std_Description_Record'
        (Name          => new String'("Standard"),
         Documentation => null,
         Origin        => Ada_Standard,
         Index         => new String'("standard"),
         Category      => Cat_Package);

      Std_Description_Tries.Insert
           (New_Assistant.Standard_Trie.all,
            New_Assistant.Standard_Entity);

      Free (Root_Node);
   end Register_Assistant;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Desc : Std_Description) return Cst_String_Access is
   begin
      if Desc /= null then
         return Cst_String_Access (Desc.Index);
      else
         return null;
      end if;
   end Get_Index;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Std_Description) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Std_Description_Record, Std_Description);
   begin
      if This /= null then
         Free (This.Name);
         Free (This.Index);
         Free (This.Documentation);
         Unchecked_Free (This);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Component : in out Std_List) is
   begin
      Free (Component.Prefix);
   end Free;

   -----------
   -- First --
   -----------

   overriding function First
     (List : Std_List)
      return Entity_List_Pckg.Virtual_List_Component_Iterator'Class
   is
      Assistant : constant Std_Entities_Db_Assistant :=
        Std_Entities_Db_Assistant
          (Get_Assistant (List.Db, Std_Entities_Assistant_Id));
      It : Std_Iterator;
   begin
      case List.Mode is
         when Aspect_Mode =>
            It := Std_Iterator'
              (It => Std_Description_Tries.Start
                 (Assistant.Aspects_Trie, List.Prefix.all),
               Db => List.Db,
               Exclude_Standard_Package => List.Exclude_Standard_Package,
               Lowercased_Name => new String'(To_Lower (List.Prefix.all)),
               Is_Partial => List.Is_Partial,
               Exceptions_Only => False);

         when Pragma_Mode =>
            It := Std_Iterator'
              (It => Std_Description_Tries.Start
                 (Assistant.Pragmas_Trie, List.Prefix.all),
               Db => List.Db,
               Exclude_Standard_Package => List.Exclude_Standard_Package,
               Lowercased_Name => new String'(To_Lower (List.Prefix.all)),
               Is_Partial => List.Is_Partial,
               Exceptions_Only => False);

         when Attribute_Mode =>
            It := Std_Iterator'
              (It => Std_Description_Tries.Start
                 (Assistant.Attributes_Trie, List.Prefix.all),
               Db => List.Db,
               Exclude_Standard_Package => List.Exclude_Standard_Package,
               Lowercased_Name => new String'(To_Lower (List.Prefix.all)),
               Is_Partial => List.Is_Partial,
               Exceptions_Only => False);

         when Standard_Mode =>
            It := Std_Iterator'
              (It => Std_Description_Tries.Start
                 (Assistant.Standard_Trie, List.Prefix.all),
               Db => List.Db,
               Exclude_Standard_Package => List.Exclude_Standard_Package,
               Lowercased_Name => new String'(To_Lower (List.Prefix.all)),
               Is_Partial => List.Is_Partial,
               Exceptions_Only => False);

         when ASCII_Mode =>
            It := Std_Iterator'
              (It => Std_Description_Tries.Start
                 (Assistant.ASCII_Trie, List.Prefix.all),
               Db => List.Db,
               Exclude_Standard_Package => List.Exclude_Standard_Package,
               Lowercased_Name => new String'(To_Lower (List.Prefix.all)),
               Is_Partial => List.Is_Partial,
               Exceptions_Only => False);

         when Exceptions_Mode =>
            It := Std_Iterator'
              (It => Std_Description_Tries.Start
                 (Assistant.Standard_Trie, List.Prefix.all),
               Db => List.Db,
               Exclude_Standard_Package => List.Exclude_Standard_Package,
               Lowercased_Name => new String'(To_Lower (List.Prefix.all)),
               Is_Partial => List.Is_Partial,
               Exceptions_Only => True);

      end case;

      if not Is_Valid (It) then
         Next (It);
      end if;

      return It;
   end First;

   ------------
   -- At_End --
   ------------

   overriding function At_End (It : Std_Iterator) return Boolean is
   begin
      return Std_Description_Tries.At_End (It.It);
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Std_Iterator) is
   begin
      Std_Description_Tries.Next (It.It);

      if not Is_Valid (It) then
         Next (It);
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get (It : in out Std_Iterator) return Entity_View is
   begin
      return new Std_Entity_Record'
        (Entity_View_Record with
         Desc => Std_Description_Tries.Get (It.It),
         Db   => It.Db);
   end Get;

   ----------
   -- Free --
   ----------

   overriding procedure Free (It : in out Std_Iterator) is
   begin
      Std_Description_Tries.Free (It.It);
      Free (It.Lowercased_Name);
   end Free;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Std_Iterator'Class) return Boolean is
   begin
      if At_End (It) then
         return True;
      else
         declare
            Name : constant String :=
              Get_Index (Std_Description_Tries.Get (It.It)).all;
         begin
            if Name = "standard" then
               return not It.Exclude_Standard_Package;
            elsif It.Exceptions_Only then
               return Name'Length > 6
                 and then Name (Name'Last - 5 .. Name'Last) = "_error";
            elsif It.Is_Partial then
               return True;
            else
               return Name = It.Lowercased_Name.all;
            end if;
         end;
      end if;
   end Is_Valid;

   --------------------------
   -- Get_Possible_Aspects --
   --------------------------

   procedure Get_Possible_Aspects
     (Db      : Construct_Database_Access;
      Prefix  : String;
      Context : Context_Of_Use_Array;
      Result  : in out Entity_List)
   is
      New_List : Std_List;
   begin
      New_List.Db := Db;
      New_List.Prefix := new String'(Prefix);
      New_List.Context := Context;
      New_List.Mode := Aspect_Mode;
      New_List.Is_Partial := True;

      Entity_List_Pckg.Append (Result.Contents, New_List);
   end Get_Possible_Aspects;

   --------------------------
   -- Get_Possible_Pragmas --
   --------------------------

   procedure Get_Possible_Pragmas
     (Db      : Construct_Database_Access;
      Prefix  : String;
      Context : Context_Of_Use_Array;
      Result  : in out Entity_List)
   is
      New_List : Std_List;
   begin
      New_List.Db := Db;
      New_List.Prefix := new String'(Prefix);
      New_List.Context := Context;
      New_List.Mode := Pragma_Mode;
      New_List.Is_Partial := True;

      Entity_List_Pckg.Append (Result.Contents, New_List);
   end Get_Possible_Pragmas;

   -----------------------------
   -- Get_Possible_Attributes --
   -----------------------------

   procedure Get_Possible_Attributes
     (Db      : Construct_Database_Access;
      Prefix  : String;
      Context : Context_Of_Use_Array;
      Result  : in out Entity_List)
   is
      New_List : Std_List;
   begin
      New_List.Db := Db;
      New_List.Prefix := new String'(Prefix);
      New_List.Context := Context;
      New_List.Mode := Attribute_Mode;
      New_List.Is_Partial := True;

      Entity_List_Pckg.Append (Result.Contents, New_List);
   end Get_Possible_Attributes;

   ------------------------------------
   -- Get_Possible_Standard_Entities --
   ------------------------------------

   procedure Get_Possible_Standard_Entities
     (Db                       : Construct_Database_Access;
      Prefix                   : String;
      Is_Partial               : Boolean;
      Result                   : in out Entity_List;
      Exclude_Standard_Package : Boolean := False)
   is
      New_List : Std_List;
   begin
      New_List.Db := Db;
      New_List.Prefix := new String'(Prefix);
      New_List.Context := (others => True);
      New_List.Mode := Standard_Mode;
      New_List.Exclude_Standard_Package := Exclude_Standard_Package;
      New_List.Is_Partial := Is_Partial;

      Entity_List_Pckg.Append (Result.Contents, New_List);
   end Get_Possible_Standard_Entities;

   --------------------------------------
   -- Get_Possible_Standard_Exceptions --
   --------------------------------------

   procedure Get_Possible_Standard_Exceptions
     (Db                       : Construct_Database_Access;
      Prefix                   : String;
      Is_Partial               : Boolean;
      Result                   : in out Entity_List;
      Exclude_Standard_Package : Boolean := False)
   is
      New_List : Std_List;
   begin
      New_List.Db := Db;
      New_List.Prefix := new String'(Prefix);
      New_List.Context := (others => True);
      New_List.Mode := Exceptions_Mode;
      New_List.Exclude_Standard_Package := Exclude_Standard_Package;
      New_List.Is_Partial := Is_Partial;

      Entity_List_Pckg.Append (Result.Contents, New_List);
   end Get_Possible_Standard_Exceptions;

   ---------------------------------
   -- Get_Possible_ASCII_Entities --
   ---------------------------------

   procedure Get_Possible_ASCII_Entities
     (Db     : Construct_Database_Access;
      Prefix : String;
      Result : in out Entity_List)
   is
      New_List : Std_List;
   begin
      New_List.Db := Db;
      New_List.Prefix := new String'(Prefix);
      New_List.Context := (others => True);
      New_List.Mode := ASCII_Mode;
      New_List.Is_Partial := True;

      Entity_List_Pckg.Append (Result.Contents, New_List);
   end Get_Possible_ASCII_Entities;

   -----------------------
   -- Get_Documentation --
   -----------------------

   overriding function Get_Documentation
     (E : access Std_Entity_Record) return String is
   begin
      if E.Desc /= null and then E.Desc.Documentation /= null then
         return E.Desc.Documentation.all;
      else
         return "";
      end if;
   end Get_Documentation;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (E : access Std_Entity_Record) return Basic_Types.UTF8_String is
   begin
      return E.Desc.Name.all;
   end Get_Name;

   ------------------
   -- Get_Category --
   ------------------

   overriding function Get_Category
     (E : access Std_Entity_Record) return Language_Category is
   begin
      return E.Desc.Category;
   end Get_Category;

   -------------------
   -- Fill_Children --
   -------------------

   overriding procedure Fill_Children
     (E               : access Std_Entity_Record;
      From_Visibility : Visibility_Context;
      Name            : String;
      Is_Partial      : Boolean;
      Filter          : Entity_Filter;
      Result          : in out Entity_List)
   is
      pragma Unreferenced (From_Visibility);
   begin
      if Get_Index (E.Desc).all = "standard" then
         if Filter.Kind = Exceptions_Only then
            Get_Possible_Standard_Exceptions
              (E.Db, Name, Is_Partial, Result, True);
         else
            Get_Possible_Standard_Entities
              (E.Db, Name, Is_Partial, Result, True);
         end if;
      elsif Get_Index (E.Desc).all = "ascii" then
         Get_Possible_ASCII_Entities (E.Db, Name, Result);
      end if;
   end Fill_Children;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Assistant : in out Std_Entities_Db) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Std_Description_Tries.Trie_Tree,
         Std_Description_Tries.Trie_Tree_Access);
   begin
      Std_Description_Tries.Clear (Assistant.Aspects_Trie.all);
      Std_Description_Tries.Clear (Assistant.Attributes_Trie.all);
      Std_Description_Tries.Clear (Assistant.Pragmas_Trie.all);
      Std_Description_Tries.Clear (Assistant.Standard_Trie.all);
      Std_Description_Tries.Clear (Assistant.ASCII_Trie.all);

      Free (Assistant.Attributes_Trie);
      Free (Assistant.Pragmas_Trie);
      Free (Assistant.Standard_Trie);
      Free (Assistant.ASCII_Trie);
   end Free;

end Ada_Semantic_Tree.Std_Entities;
