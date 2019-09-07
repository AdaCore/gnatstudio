------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Gtkada.Style;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;

with GPS.Kernel.Xref;         use GPS.Kernel.Xref;
with Tooltips;
with Xref;                    use Xref;

package body Language.Abstract_Construct_Tree is

   ------------
   -- Create --
   ------------

   function Create (K : Kernel_Handle) return Semantic_Tree_Provider_Access
   is
   begin
      return new Construct_Tree_Provider'(Kernel => K);
   end Create;

   -----------------------
   -- Get_Tree_For_File --
   -----------------------

   overriding function Get_Tree_For_File
     (Self    : in out Construct_Tree_Provider;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class
   is
      pragma Unreferenced (Context);

      Struct_File : constant Structured_File_Access :=
        Get_Or_Create
          (Self.Kernel.Get_Construct_Database, File);
   begin
      Ref (Struct_File);
      return Abstract_Construct_Tree'
        (Construct_File => Struct_File, Kernel => Self.Kernel);
   end Get_Tree_For_File;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Self : Construct_Node) return access Simple_Construct_Information
   is
     (Get_Construct (Self.Entity));

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self : Construct_Node) return Semantic_Node'Class
   is
      It, Parent_It : Construct_Tree_Iterator;
   begin
      It := To_Construct_Tree_Iterator (Self.Entity);
      if It = Null_Construct_Tree_Iterator then
         return No_Semantic_Node;
      end if;
      Parent_It := Get_Parent_Scope (Get_Tree (Self.Construct_File), It);
      if Parent_It = Null_Construct_Tree_Iterator then
         return No_Semantic_Node;
      else
         return Construct_Node'(Construct_File => Self.Construct_File,
                          Entity         => To_Entity_Access
                            (Self.Construct_File, Parent_It),
                          Kernel => Self.Kernel);
      end if;
   end Parent;

   ----------------
   -- Root_Nodes --
   ----------------

   overriding function Root_Nodes
     (Self : Abstract_Construct_Tree) return Semantic_Node_Array'Class
   is
      It : Construct_Tree_Iterator;
      T : constant Construct_Tree := Get_Tree (Self.Construct_File);
   begin
      It := First (Get_Tree (Self.Construct_File));
      return A : Construct_Node_Array do
         while It /= Null_Construct_Tree_Iterator loop
            A.Nodes.Append
              (Construct_Node'(Construct_File => Self.Construct_File,
                         Entity => To_Entity_Access
                           (Self.Construct_File, It),
                         Kernel => Self.Kernel));
            It := Next (T, It, Jump_Over);
         end loop;
      end return;
   end Root_Nodes;

   -------------------
   -- Root_Iterator --
   -------------------

   overriding function Root_Iterator
     (Self : Abstract_Construct_Tree) return Semantic_Tree_Iterator'Class
   is
      It : constant Construct_Tree_Iterator :=
        First (Get_Tree (Self.Construct_File));

   begin
      return Abstract_Construct_Tree_Iterator'
        (It, Self.Kernel, Self.Construct_File);
   end Root_Iterator;

   -------------
   -- Node_At --
   -------------

   overriding function Node_At
     (Self : Abstract_Construct_Tree; Sloc : Sloc_T;
      Category_Filter : Category_Array := Null_Category_Array)
      return Semantic_Node'Class
   is
      It   : Construct_Tree_Iterator;
   begin
      It := Get_Iterator_At
        (Tree     => Get_Tree (Self.Construct_File),
         Location => To_Location (Sloc.Line, String_Index_Type (Sloc.Column)),
         Position => Enclosing,
         Categories_Seeked => Category_Filter);
      return Construct_Node'(Construct_File => Self.Construct_File,
                       Entity => To_Entity_Access (Self.Construct_File, It),
                       Kernel => Self.Kernel);
   end Node_At;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Abstract_Construct_Tree) return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Get_File_Path (Self.Construct_File);
   end File;

   ------------
   -- Update --
   ------------

   overriding procedure Update (Self : in out Abstract_Construct_Tree) is
   begin
      Update_Contents
        (Get_Construct_Database (Self.Kernel), Self.File);
   end Update;

   ------------------
   -- Update_Async --
   ------------------

   overriding procedure Update_Async (Self : in out Abstract_Construct_Tree) is
   begin
      Self.Update;
      Self.Kernel.Semantic_Tree_Updated (Self.File);
   end Update_Async;

   --------------
   -- Category --
   --------------

   overriding function Category
     (Self : Construct_Node) return Language_Category
   is
   begin
      return Get_Construct (Self).Category;
   end Category;

   ----------
   -- Sort --
   ----------

   overriding procedure Sort
     (Self : in out Construct_Node_Array;
      Less_Than : access function (L, R : Semantic_Node'Class) return Boolean)
   is
      function "<" (L, R : Construct_Node) return Boolean
      is (Less_Than (Semantic_Node'Class (L), Semantic_Node'Class (R)));

      package Gen_Sort is new Construct_Node_Vectors.Generic_Sorting;

   begin
      Gen_Sort.Sort (Self.Nodes);
   end Sort;

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self : Construct_Node)
      return Semantic_Node_Array'Class
   is
      Self_It, It : Construct_Tree_Iterator;
      T : constant Construct_Tree := Get_Tree (Self.Construct_File);
   begin
      Self_It := To_Construct_Tree_Iterator (Self.Entity);
      return A : Construct_Node_Array do
         It := Next (T, Self_It, Jump_Into);
         while It /= Null_Construct_Tree_Iterator
           and then Is_Parent_Scope (Self_It, It)
         loop
            A.Nodes.Append
              (Construct_Node'(Construct_File => Self.Construct_File,
                         Entity => To_Entity_Access
                           (Self.Construct_File, It),
                         Kernel => Self.Kernel));
            It := Next (T, It, Jump_Over);
         end loop;
      end return;
   end Children;

   -----------------
   -- First_Child --
   -----------------

   overriding function First_Child
     (Self : Construct_Node) return Semantic_Node'Class
   is
      It : Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Self.Entity);
   begin
      if It = Null_Construct_Tree_Iterator then
         return No_Semantic_Node;
      end if;

      It := Next (Get_Tree (Self.Construct_File), It, Jump_Into);

      if It = Null_Construct_Tree_Iterator then
         return No_Semantic_Node;
      end if;

      return Construct_Node'(Self.Construct_File,
                             To_Entity_Access (Self.Construct_File, It),
                             Self.Kernel);
   end First_Child;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Self : Construct_Node) return Symbol
   is
   begin
      return Get_Construct (Self).Name;
   end Name;

   ----------------
   -- Visibility --
   ----------------

   overriding function Visibility
     (Self : Construct_Node) return Construct_Visibility
   is
   begin
      return
        (case Get_Construct (Self).Visibility is
            when Visibility_Public => Visibility_Public,
            when Visibility_Private => Visibility_Private,
            when Visibility_Protected => Visibility_Protected);
   end Visibility;

   ---------------
   -- Unique_Id --
   ---------------

   overriding function Unique_Id
     (Self : Construct_Node) return GNATCOLL.Symbols.Symbol
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Self);
   begin
      if Construct.Unique_Id /= No_Symbol then
         return Construct.Unique_Id;
      end if;

      declare
         P : constant Semantic_Node'Class := Self.Parent;
         Base_Id : constant String :=
           To_Lower (Get (Self.Name).all)
           & To_Lower (Get (Self.Profile (Show_Param_Names => True)).all)
           & Self.Category'Img;
      begin
         if P = No_Semantic_Node
           or else Self.Entity = Construct_Node (P).Entity
         then
            return Self.Kernel.Symbols.Find (Base_Id);
         else
            return Self.Kernel.Symbols.Find (Get (P.Unique_Id).all & Base_Id);
         end if;
      end;
   end Unique_Id;

   --------------------
   -- Is_Declaration --
   --------------------

   overriding function Is_Declaration
     (Self : Construct_Node) return Boolean is
   begin
      return Get_Construct (Self).Is_Declaration;
   end Is_Declaration;

   -------------
   -- Profile --
   -------------

   overriding function Profile
     (Self             : Construct_Node;
      Show_Param_Names : Boolean) return Symbol
   is
      Construct : constant access Simple_Construct_Information :=
        Get_Construct (Self);
   begin
      if Construct.Name /= No_Symbol and then
        Construct.Category in Subprogram_Category
      then
         return Self.Kernel.Symbols.Find
           (Get_Profile
              (Lang             => Get_Tree_Language (Self.Construct_File),
               Entity           => Self.Entity,
               Show_Param_Names => Show_Param_Names));

      --  In case the language has defined a profile anyway
      elsif Construct.Profile /= No_Symbol then
         return Construct.Profile;

      else
         return Empty_String;
      end if;
   end Profile;

   ----------------
   -- Definition --
   ----------------

   overriding function Definition
     (Self : Construct_Node) return Semantic_Node'Class
   is
   begin
      if Get_Tree_Language (Self.Construct_File) /= null then
         return Construct_Node'
           (Construct_File => Self.Construct_File,
            Entity =>
              Get_Tree_Language (Self.Construct_File).Find_Next_Part
            (Self.Entity),
            Kernel => Self.Kernel);
      end if;

      return No_Semantic_Node;
   end Definition;

   ------------------------
   -- Documentation_Body --
   ------------------------

   overriding function Documentation_Body
     (Self : Construct_Node) return String
   is
      use Gtkada.Style;
   begin
      return
        (if Self.Sloc_Start = No_Sloc_T
         then ""
         else
            Documentation
           (Self    => Self.Kernel.Databases,
            Handler => Self.Kernel.Get_Language_Handler,
            Color_For_Optional_Param =>
              To_Hex (Shade_Or_Lighten (Tooltips.Tooltips_Foreground_Color)),
            Entity  => From_Constructs (Self.Kernel.Databases, Self.Entity)));
   end Documentation_Body;

   --------------------------
   -- Documentation_Header --
   --------------------------

   overriding function Documentation_Header
     (Self : Construct_Node) return String is
   begin
      return "<b>" & Get (Self.Name).all & "</b>";
   end Documentation_Header;

   ----------------
   -- Sloc_Start --
   ----------------

   overriding function Sloc_Start
     (Self : Construct_Node) return Sloc_T
   is
      Sloc_Start : constant Source_Location := Get_Construct (Self).Sloc_Start;
   begin
      return (if Sloc_Start.Line = 0
              then No_Sloc_T
              else (Line   => Sloc_Start.Line,
                    Column => To_Visible_Column
                      (Self.Construct_File, Sloc_Start.Line,
                       String_Index_Type (Sloc_Start.Column)),
                    Index  => Offset_T (Sloc_Start.Index)));
   end Sloc_Start;

   --------------
   -- Sloc_Def --
   --------------

   overriding function Sloc_Def
     (Self : Construct_Node) return Sloc_T
   is
      Sloc_Def : constant Source_Location := Get_Construct (Self).Sloc_Entity;
   begin
      return (if Sloc_Def.Line = 0
              then No_Sloc_T
              else (Line   => Sloc_Def.Line,
                    Column => To_Visible_Column
                      (Self.Construct_File, Sloc_Def.Line,
                       String_Index_Type (Sloc_Def.Column)),
                    Index  => Offset_T (Sloc_Def.Index)));
   end Sloc_Def;

   --------------
   -- Sloc_End --
   --------------

   overriding function Sloc_End
     (Self : Construct_Node) return Sloc_T
   is
      Sloc_End : constant Source_Location := Get_Construct (Self).Sloc_End;
   begin
      return (if Sloc_End.Line = 0
              then No_Sloc_T
              else (Line   => Sloc_End.Line,
                    Column => To_Visible_Column
                      (Self.Construct_File, Sloc_End.Line,
                       String_Index_Type (Sloc_End.Column)),
                    Index  => Offset_T (Sloc_End.Index)));
   end Sloc_End;

   --------------
   -- Get_Hash --
   --------------

   overriding function Get_Hash
     (Self : Construct_Node) return Hash_Type is
   begin
      return Hash (Self.Entity);
   end Get_Hash;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Construct_Node) return GNATCOLL.VFS.Virtual_File
   is
   begin
      return Get_File_Path (Self.Construct_File);
   end File;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : in out Abstract_Construct_Tree_Iterator)
   is
   begin
      if Self.It = Null_Construct_Tree_Iterator then
         return;
      end if;
      Self.It := Next (Get_Tree (Self.Construct_File), Self.It, Jump_Into);
   end Next;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Self : Abstract_Construct_Tree_Iterator)
      return Semantic_Node'Class is
   begin
      if Self.It = Null_Construct_Tree_Iterator then
         return No_Semantic_Node;
      else
         return Construct_Node'
           (Self.Construct_File,
            To_Entity_Access (Self.Construct_File, Self.It),
            Self.Kernel);
      end if;
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   overriding function Has_Element
     (Self : Abstract_Construct_Tree_Iterator) return Boolean is
   begin
      return (Self.It /= Null_Construct_Tree_Iterator);
   end Has_Element;

end Language.Abstract_Construct_Tree;
