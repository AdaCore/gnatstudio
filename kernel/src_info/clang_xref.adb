with String_Utils; use String_Utils;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Libclang.Index; use Libclang.Index;
with Language.Libclang_Tree; use Language.Libclang_Tree;

pragma Warnings (Off);
package body Clang_Xref is

   function Get_Clang_Node (E : Clang_Entity) return Semantic_Node'Class;

   function Get_Clang_Node (E : Clang_Entity) return Semantic_Node'Class is
   begin
      return E.Kernel.Get_Abstract_Tree_For_File (E.Loc.File).Node_At
        (Sloc_T'(E.Loc.Line, E.Loc.Column, 0));
   end Get_Clang_Node;

   function "+" (S : Semantic_Node'Class) return Clang_Cursor is
      (Clang_Node (S).Cursor);

   overriding function Get_Entity
     (Db : Clang_Database;
      General_Db : General_Xref_Database;
      Name : String;
      Loc : General_Location) return Root_Entity'Class
   is
   begin
      return Clang_Entity'(General_Db => General_Db,
                           Name       => +Name,
                           Loc        => Loc,
                           Kernel => Db.Kernel);
   end Get_Entity;

   overriding function Is_Fuzzy (Entity : Clang_Entity) return Boolean
   is (False);

   overriding function Get_Name
     (Entity : Clang_Entity) return String is
     (+Entity.Name);

   overriding function Get_Display_Kind
     (Entity : Clang_Entity) return String is ("");

   overriding function Qualified_Name
     (Entity : Clang_Entity) return String is (Entity.Get_Name);

   overriding function Hash
     (Entity : Clang_Entity) return Integer
   is
   begin
      return Integer (Get_Clang_Node (Entity).Hash);
   end Hash;

   function Cmp
     (Entity1, Entity2 : Root_Entity'Class) return Integer is (0);

   overriding function Get_Declaration
     (Entity : Clang_Entity) return General_Entity_Declaration
   is
      Def : Clang_Cursor := clang_getCursorReferenced
        (+Get_Clang_Node (Entity));
      Sloc : Sloc_T := To_Sloc_T (clang_getCursorLocation (Def));
   begin
--        General_Entity_Declaration'(Loc => ,
--                                    Name => Spelling (Def),
--      Body_Is_Full_Declaration => clang_isCursorDefinition (Def))
      return No_General_Entity_Declaration;
   end Get_Declaration;

   overriding function Caller_At_Declaration
     (Entity : Clang_Entity) return Root_Entity'Class is
   begin
      return No_Root_Entity;
   end Caller_At_Declaration;

   overriding function Get_Body
     (Entity : Clang_Entity;
      After  : General_Location := No_Location)
      return General_Location is
   begin
      return No_Location;
   end Get_Body;

   overriding function Get_Type_Of
     (Entity : Clang_Entity) return Root_Entity'Class is
   begin
      return No_Root_Entity;
   end Get_Type_Of;

   overriding function Returned_Type
     (Entity : Clang_Entity) return Root_Entity'Class is
   begin
      return No_Root_Entity;
   end Returned_Type;

   overriding function Parent_Package
     (Entity : Clang_Entity) return Root_Entity'Class is
   begin
      return No_Root_Entity;
   end Parent_Package;

   overriding function Pointed_Type
     (Entity : Clang_Entity) return Root_Entity'Class is
   begin
      return No_Root_Entity;
   end Pointed_Type;

   overriding function Renaming_Of
     (Entity : Clang_Entity) return Root_Entity'Class is
   begin
      return No_Root_Entity;
   end Renaming_Of;

   overriding function Is_Primitive_Of
     (Entity : Clang_Entity) return Entity_Array
   is
   begin
      return No_Entity_Array;
   end Is_Primitive_Of;

   overriding function Has_Methods (E : Clang_Entity) return Boolean is
   begin
      return False;
   end Has_Methods;

   overriding function Is_Access (E : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Access;

   overriding function Is_Abstract
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Abstract;

   overriding function Is_Array
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Array;

   overriding function Is_Printable_In_Debugger
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Printable_In_Debugger;

   overriding function Is_Type
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Type;

   overriding function Is_Subprogram
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Subprogram;

   overriding function Is_Container
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Container;

   overriding function Is_Generic
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Generic;

   overriding function Is_Global
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Global;

   overriding function Is_Static_Local
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Static_Local;

   overriding function Is_Predefined_Entity
     (E  : Clang_Entity) return Boolean is
   begin
      return False;
   end Is_Predefined_Entity;

   overriding procedure Documentation
     (Handler           : Language_Handlers.Language_Handler;
      Entity            : Clang_Entity;
      Formater          : access Profile_Formater'Class;
      Check_Constructs  : Boolean := True;
      Look_Before_First : Boolean := True) is
   begin
      null;
   end Documentation;

   overriding function End_Of_Scope
     (Entity : Clang_Entity) return General_Location is (No_Location);

   overriding function Is_Parameter_Of
     (Entity : Clang_Entity) return Root_Entity'Class is (No_Root_Entity);

   overriding function Overrides
     (Entity : Clang_Entity) return Root_Entity'Class is (No_Root_Entity);

   overriding function Instance_Of
     (Entity : Clang_Entity) return Root_Entity'Class is (No_Root_Entity);

   overriding function Methods
     (Entity            : Clang_Entity;
      Include_Inherited : Boolean) return Entity_Array is (No_Entity_Array);

   overriding function Fields
     (Entity            : Clang_Entity) return Entity_Array
   is
     (No_Entity_Array);

   overriding function Literals
     (Entity            : Clang_Entity) return Entity_Array
   is
     (No_Entity_Array);

   overriding function Formal_Parameters
     (Entity            : Clang_Entity) return Entity_Array
   is
     (No_Entity_Array);

   overriding function Discriminant_Of
     (Entity            : Clang_Entity) return Root_Entity'Class
   is
      (No_Root_Entity);

   overriding function Discriminants
     (Entity            : Clang_Entity) return Entity_Array
   is
     (No_Entity_Array);

   overriding function Component_Type
     (Entity : Clang_Entity) return Root_Entity'Class is (No_Root_Entity);

   overriding function Index_Types
     (Entity : Clang_Entity) return Entity_Array is (No_Entity_Array);

   overriding function Child_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array is (No_Entity_Array);

   overriding function Parent_Types
     (Entity    : Clang_Entity;
      Recursive : Boolean) return Entity_Array is (No_Entity_Array);

   overriding function Parameters
     (Entity : Clang_Entity) return Parameter_Array is (No_Parameters);

   overriding function Get_All_Called_Entities
     (Entity : Clang_Entity) return Abstract_Entities_Cursor'Class
     is (No_Entities_Cursor);

   overriding function Find_All_References
     (Entity                : Clang_Entity;
      In_File               : GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.No_File;
      In_Scope              : Root_Entity'Class := No_Root_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False;
      Include_Implicit      : Boolean := False;
      Include_All           : Boolean := False;
      Kind                  : String := "")
      return Root_Reference_Iterator'Class is (No_Reference_Iterator);

end Clang_Xref;
