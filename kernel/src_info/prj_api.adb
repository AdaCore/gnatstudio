-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  Gnat sources dependencies
with Prj;       use Prj;
with Prj.Util;  use Prj.Util;
with Prj.Pars;  use Prj.Pars;
with Prj.Tree;  use Prj.Tree;
with Snames;    use Snames;
with Namet;     use Namet;
with Types;     use Types;
with Csets;     use Csets;
with Stringt;   use Stringt;
with Sinput;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Unchecked_Deallocation;

package body Prj_API is

   function Internal_Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Array_Index : String_Id := No_String;
      Kind : Variable_Kind := List)
      return Project_Node_Id;
   --  Internal version for Get_Or_Create_Attribute. If Array_Index is not
   --  No_String, then the variable is defined for a specific index.

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project (Name, Path : String) return Project_Node_Id is
      Project : Project_Node_Id;
   begin
      Project_Nodes.Append (Default_Project_Node (N_Project));
      Project := Project_Nodes.Last;
      pragma Assert (Project /= Empty_Node);

      --  Adding the name of the project
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      Project_Nodes.Table (Project).Name := Name_Enter;

      --  Adding the project path
      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      Project_Nodes.Table (Project).Path_Name := Name_Enter;
      return Project;
   end Create_Project;

   -------------------------------
   -- Get_Or_Create_Declaration --
   -------------------------------

   function Get_Or_Create_Declaration (Project : Project_Node_Id)
      return Project_Node_Id
   is
      Decl : Project_Node_Id := Project_Nodes.Table (Project).Field2;
   begin
      if Decl = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Project_Declaration));
         Decl := Project_Nodes.Last;
         Project_Nodes.Table (Project).Field2 := Decl;
      end if;
      return Decl;
   end Get_Or_Create_Declaration;

   --------------------------------------
   -- Internal_Get_Or_Create_Attribute --
   --------------------------------------

   function Internal_Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Array_Index : String_Id := No_String;
      Kind : Variable_Kind := List)
      return Project_Node_Id
   is
      Decl, Decl_Item, Var : Project_Node_Id;
      N : Name_Id;
   begin
      pragma Assert
        (Project_Nodes.Table (Prj_Or_Pkg).Kind = N_Package_Declaration
         or else Project_Nodes.Table (Prj_Or_Pkg).Kind = N_Project);

      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      N := Name_Find;

      --  First step is to create the declarative item that will contain the
      --  variable. This is dependent on the kind of node for Prj_Or_Pkg

      case Project_Nodes.Table (Prj_Or_Pkg).Kind is
         when N_Project =>
            Decl := Get_Or_Create_Declaration (Prj_Or_Pkg);
            Decl_Item := Project_Nodes.Table (Decl).Field1;

         when N_Package_Declaration =>
            Decl := Empty_Node;
            Decl_Item := Project_Nodes.Table (Prj_Or_Pkg).Field2;

         when others =>
            null;
      end case;

      --  Check if the variable already exists.
      --  If it does, nothing to do, and we just exit.

      while Decl_Item /= Empty_Node loop
         Var := Project_Nodes.Table (Decl_Item).Field1;
         if Project_Nodes.Table (Var).Kind = N_Variable_Declaration
           and then Project_Nodes.Table (Var).Name = N
         then
            return Var;
         end if;
         Decl_Item := Project_Nodes.Table (Decl_Item).Field2;
      end loop;

      --  Otherwise create the declarative item

      Project_Nodes.Append (Default_Project_Node (N_Declarative_Item));
      Decl_Item := Project_Nodes.Last;

      --  Insert it in the appropriate list

      case Project_Nodes.Table (Prj_Or_Pkg).Kind is
         when N_Project =>
            Project_Nodes.Table (Decl_Item).Field2 :=
              Project_Nodes.Table (Decl).Field1;
            Project_Nodes.Table (Decl).Field1 := Decl_Item;

         when N_Package_Declaration =>
            Project_Nodes.Table (Decl_Item).Field2 :=
              Project_Nodes.Table (Prj_Or_Pkg).Field2;
            Project_Nodes.Table (Prj_Or_Pkg).Field2 := Decl_Item;

         when others =>
            null;
      end case;

      --  Create the variable

      Project_Nodes.Append
        (Default_Project_Node (N_Attribute_Declaration, Kind));
      Var := Project_Nodes.Last;
      Project_Nodes.Table (Decl_Item).Field1 := Var;

      Project_Nodes.Table (Var).Name := N;
      Project_Nodes.Table (Var).Value := Array_Index;

      if Kind = Prj.List then
         Project_Nodes.Append
           (Default_Project_Node (N_Literal_String_List, List));
         Project_Nodes.Table (Var).Field1 := Project_Nodes.Last;
      end if;
      return Var;
   end Internal_Get_Or_Create_Attribute;

   -----------------------------
   -- Get_Or_Create_Attribute --
   -----------------------------

   function Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Kind : Variable_Kind := List)
      return Project_Node_Id is
   begin
      return Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, No_String, Kind);
   end Get_Or_Create_Attribute;

   -----------------------------
   -- Get_Or_Create_Attribute --
   -----------------------------

   function Get_Or_Create_Attribute
     (Prj_Or_Pkg : Project_Node_Id;
      Name : String;
      Index_Name : String;
      Kind : Variable_Kind := List)
     return Project_Node_Id is
   begin
      Start_String;
      Store_String_Chars (Index_Name);
      return Internal_Get_Or_Create_Attribute
        (Prj_Or_Pkg, Name, End_String, Kind);
   end Get_Or_Create_Attribute;

   ---------------------------
   -- Get_Or_Create_Package --
   ---------------------------

   function Get_Or_Create_Package
     (Project : Project_Node_Id; Pkg : String) return Project_Node_Id
   is
      Decl : constant Project_Node_Id := Get_Or_Create_Declaration (Project);
      Decl_Item : Project_Node_Id;
      Pack : Project_Node_Id;
      N : Name_Id;
   begin
      Name_Len := Pkg'Length;
      Name_Buffer (1 .. Name_Len) := Pkg;
      N := Name_Find;

      --  Check if the package already exists
      --  ??? It seems that packages and variables should be stored in
      --  different lists
      Decl_Item := Project_Nodes.Table (Decl).Field1;
      while Decl_Item /= Empty_Node loop
         Pack := Project_Nodes.Table (Decl_Item).Field1;
         if Project_Nodes.Table (Pack).Kind = N_Package_Declaration
           and then Project_Nodes.Table (Pack).Name = N
         then
            return Pack;
         end if;
         Decl_Item := Project_Nodes.Table (Decl_Item).Field2;
      end loop;

      --  Otherwise create the declarative item
      Project_Nodes.Append (Default_Project_Node (N_Declarative_Item));
      Decl_Item := Project_Nodes.Last;
      Project_Nodes.Table (Decl_Item).Field2 :=
        Project_Nodes.Table (Decl).Field1;
      Project_Nodes.Table (Decl).Field1 := Decl_Item;

      --  Create the variable
      Project_Nodes.Append
        (Default_Project_Node (N_Package_Declaration, Undefined));
      Pack := Project_Nodes.Last;
      Project_Nodes.Table (Pack).Field3 := Project_Nodes.Table (Decl).Field1;
      Project_Nodes.Table (Decl_Item).Field1 := Pack;

      Project_Nodes.Table (Pack).Name := N;

      return Pack;
   end Get_Or_Create_Package;

   --------------------
   -- Append_To_List --
   --------------------

   procedure Append_To_List (Var : Project_Node_Id; Value : String) is
      List, Expr, Term, Str : Project_Node_Id;
   begin
      pragma Assert (Var /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Var).Expr_Kind = Prj.List);

      List := Project_Nodes.Table (Var).Field1;

      pragma Assert (List /= Empty_Node);
      pragma Assert (Project_Nodes.Table (List).Kind = N_Literal_String_List);

      --  Create a new expression
      Project_Nodes.Append (Default_Project_Node (N_Expression));
      Term := Project_Nodes.Last;

      --  Create a new list if required
      Expr := Project_Nodes.Table (List).Field1;
      if Expr = Empty_Node then
         Project_Nodes.Table (List).Field1 := Term;

      --  Else append at the end of list
      else
         while Project_Nodes.Table (Expr).Field2 /= Empty_Node loop
            Expr := Project_Nodes.Table (Expr).Field2;
         end loop;

         Project_Nodes.Table (Expr).Field2 := Term;
      end if;
      Expr := Term;

      Project_Nodes.Append (Default_Project_Node (N_Term));
      Term := Project_Nodes.Last;
      Project_Nodes.Table (Expr).Field1 := Term;

      Project_Nodes.Append (Default_Project_Node (N_Literal_String));
      Str := Project_Nodes.Last;
      Project_Nodes.Table (Term).Field1 := Str;

      Start_String;
      Store_String_Chars (Value);
      Project_Nodes.Table (Str).Value := End_String;
   end Append_To_List;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Var : Project_Node_Id; Value : String) is
      Expr, Term, Str : Project_Node_Id;
   begin
      pragma Assert (Var /= Empty_Node);
      pragma Assert (Project_Nodes.Table (Var).Expr_Kind = Prj.Single);

      --  Create the expression if required
      Expr := Project_Nodes.Table (Var).Field1;
      if Expr = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Expression));
         Expr := Project_Nodes.Last;
         Project_Nodes.Table (Var).Field1 := Expr;
      else
         pragma Assert (Project_Nodes.Table (Expr).Kind = N_Expression);
         null;
      end if;

      Project_Nodes.Table (Expr).Field2 := Empty_Node; --  No next in the list

      --  Create the term
      Term := Project_Nodes.Table (Expr).Field1;
      if Term = Empty_Node then
         Project_Nodes.Append (Default_Project_Node (N_Term));
         Term := Project_Nodes.Last;
         Project_Nodes.Table (Expr).Field1 := Term;
      else
         pragma Assert (Project_Nodes.Table (Term).Kind = N_Term);
         null;
      end if;

      Project_Nodes.Append (Default_Project_Node (N_Literal_String));
      Str := Project_Nodes.Last;
      Project_Nodes.Table (Term).Field1 := Str;
      Project_Nodes.Table (Term).Field2 := Empty_Node;

      Start_String;
      Store_String_Chars (Value);
      Project_Nodes.Table (Str).Value := End_String;
   end Set_Value;



   --  To be removed below --




   type Prj_Action is access procedure (Project : Project_Id);
   procedure For_All_Imported (Root : Project_Id; Action : Prj_Action);
   --  Execute Action for all projects imported by Root (but not Root itself).

   type Prj_Action_F is access function (Project : Project_Id) return Boolean;
   function For_All_Imported
     (Root : Project_Id; Action : Prj_Action_F) return Boolean;
   --  same as above, but stops the recursion as soon as one of the action
   --  procedure returns False.
   --  This function returns False if it was interrupted before completing the
   --  recursion.

   procedure Set_Value_Of
     (Name                   : Name_Id;
      Variable_Or_Array_Name : Name_Id;
      In_Decl                : in out Declarations;
      Value                  : String);
   --  Override the value stored in the array Variable_Or_Array_Name,
   --  at index Name, or, if the array doesn't exist, in the variable called
   --  Variable_Or_Array_Name.
   --  If neither the array nor the variable exist, a new array is created.
   --
   --  Note that Value is splitted on space characters before being added.

   function Get_Array_Index
     (Index : Name_Id;
      In_Array : Array_Element_Id) return Array_Element_Id;
   --  Find the index for Index in the list starting at In_Array.
   --  No_Array_Element is returned if Index wasn't found.

   function Get_Variable_Id
     (Variable_Name : Name_Id;
      In_Variables  : Variable_Id) return Variable_Id;
   --  Find the index for Variable_Name in the list starting at In_Variables.
   --  No_Variable is returned if the variable wasn't found.

   procedure Add_To_Value_Of
     (Variable_Name : Name_Id;
      In_Decl       : in out Declarations;
      Value         : String);
   --  Add Value to the contents of Variable_Name.

   procedure Add_To_Variable_Value
     (Add_To : in out Variable_Value;
      Str    : String;
      Split  : Boolean := False);
   --  Add Str at the end of Add_To.
   --  If Split is True, Str is first splitted into a list of values on each
   --  space character (as a Unix shell would).

   ----------------------
   -- For_All_Imported --
   ----------------------

   procedure For_All_Imported (Root : Project_Id; Action : Prj_Action) is
      Prj : Project_List := Projects.Table (Root).Imported_Projects;
   begin
      while Prj /= Empty_Project_List loop
         Action (Project_Lists.Table (Prj).Project);
         For_All_Imported (Project_Lists.Table (Prj).Project, Action);
         Prj := Project_Lists.Table (Prj).Next;
      end loop;
   end For_All_Imported;

   ----------------------
   -- For_All_Imported --
   ----------------------

   function For_All_Imported
     (Root : Project_Id; Action : Prj_Action_F) return Boolean
   is
      Prj : Project_List := Projects.Table (Root).Imported_Projects;
   begin
      while Prj /= Empty_Project_List loop
         if not Action (Project_Lists.Table (Prj).Project)
           or else not For_All_Imported
           (Project_Lists.Table (Prj).Project, Action)
         then
            return False;
         end if;
         Prj := Project_Lists.Table (Prj).Next;
      end loop;
      return True;
   end For_All_Imported;

   -------------------
   -- Parse_Project --
   -------------------

   function Parse_Project (Name : String) return Prj.Project_Id is
      Project : Project_Id;
   begin
      --  Needed in case Parse has an error
      Sinput.Main_Source_File := System_Source_File_Index;

      --  Parse all the subpackages, since we need to keep the information
      --  for all of them.
      Parse (Project, Project_File_Name => Name);
      return Project;
   end Parse_Project;

   --------------------------------
   -- Get_File_Specific_Switches --
   --------------------------------

   function Get_File_Specific_Switches
     (Project    : Prj.Project_Id;
      File_Name  : String;
      In_Package : Name_Id := Name_Compiler) return Prj.Variable_Value
   is
      Gnatmake_Pkg  : constant Prj.Package_Id :=
        Prj.Util.Value_Of
        (Name        => Name_Gnatmake,
         In_Packages => Projects.Table (Project).Decl.Packages);
      Compiler_Pkg : Prj.Package_Id;
   begin
      if Gnatmake_Pkg /= No_Package then
         Compiler_Pkg := Prj.Util.Value_Of
           (Name        => In_Package,
            In_Packages => Packages.Table (Gnatmake_Pkg).Decl.Packages);

         if Compiler_Pkg /= No_Package then
            Name_Len := File_Name'Length;
            Name_Buffer (1 .. Name_Len) := File_Name;
            return Prj.Util.Value_Of
              (Name                    => Name_Find,
               Attribute_Or_Array_Name => Name_Switches,
               In_Package              => Compiler_Pkg);
         end if;
      end if;
      return Nil_Variable_Value;
   end Get_File_Specific_Switches;

   --------------------------
   -- Get_Default_Switches --
   --------------------------

   function Get_Default_Switches
     (Project    : Prj.Project_Id;
      In_Package : Name_Id := Name_Compiler) return Prj.Variable_Value
   is
      Gnatmake_Pkg  : constant Prj.Package_Id :=
        Prj.Util.Value_Of
        (Name        => Name_Gnatmake,
         In_Packages => Projects.Table (Project).Decl.Packages);
      Compiler_Pkg : Prj.Package_Id;
   begin
      if Gnatmake_Pkg /= No_Package then
         Compiler_Pkg := Prj.Util.Value_Of
           (Name        => In_Package,
            In_Packages => Packages.Table (Gnatmake_Pkg).Decl.Packages);

         if Compiler_Pkg /= No_Package then
            return
              Prj.Util.Value_Of
              (Variable_Name => Name_Switches,
               In_Variables  => Packages.Table (Compiler_Pkg).Decl.Variables);
         end if;
      end if;
      return Variable_Value'
        (Kind => Undefined, Location => No_Location, Default => False);
   end Get_Default_Switches;

   ---------------------------
   -- Add_To_Variable_Value --
   ---------------------------

   procedure Add_To_Variable_Value
     (Add_To : in out Variable_Value;
      Str    : String;
      Split  : Boolean := False)
   is
      procedure Free is new Unchecked_Deallocation
        (Argument_List, Argument_List_Access);
      List : Argument_List_Access;
      Values, Last : String_List_Id := Nil_String;
   begin
      if Split then
         List := Argument_String_To_List (Str);
      end if;

      if List = null or else List'Length = 1 then
         if List /= null then
            Free (List (List'First));
            Free (List);
         end if;

         Start_String;
         Store_String_Chars (Str);

         case Add_To.Kind is
            when Undefined =>  --  Nil_Variable_Value
               Add_To := (Kind     => Single,
                          Location => No_Location,
                          Default  => False,
                          Value    => End_String);

            when Single =>
               String_Elements.Append
                 ((Value    => Add_To.Value,
                   Location => Add_To.Location,
                   Next     => Nil_String));
               String_Elements.Append
                 ((Value    => End_String,
                   Location => No_Location,
                   Next     => Nil_String));
               String_Elements.Table (String_Elements.Last - 1).Next :=
                 String_Elements.Last;
               Add_To := (Kind     => Prj.List,
                          Location => No_Location,
                          Default  => False,
                          Values   => String_Elements.Last - 1);

            when Prj.List =>
               Values := Add_To.Values;
               while String_Elements.Table (Values).Next /= Nil_String loop
                  Values := String_Elements.Table (Values).Next;
               end loop;

               String_Elements.Append
                 ((Value    => End_String,
                   Location => No_Location,
                   Next     => Nil_String));
               String_Elements.Table (Values).Next := String_Elements.Last;
         end case;

      else
         for J in List'Range loop
            Start_String;
            Store_String_Chars (List (J).all);
            Free (List (J));
            String_Elements.Append
              ((Value    => End_String,
                Location => No_Location,
                Next     => Nil_String));

            if Values = Nil_String then
               Values := String_Elements.Last;
            else
               String_Elements.Table (String_Elements.Last - 1).Next :=
                 String_Elements.Last;
            end if;
         end loop;

         Free (List);

         case Add_To.Kind is
            when Undefined =>  --  Nil_Variable_Value
               Add_To := (Kind     => Prj.List,
                          Location => No_Location,
                          Default  => False,
                          Values   => Values);

            when Single =>
               String_Elements.Append
                 ((Value    => Add_To.Value,
                   Location => Add_To.Location,
                   Next     => Values));
               Add_To := (Kind     => Prj.List,
                          Location => No_Location,
                          Default  => False,
                          Values   => String_Elements.Last);

            when Prj.List =>
               Last := Add_To.Values;
               while String_Elements.Table (Last).Next /= Nil_String loop
                  Last := String_Elements.Table (Last).Next;
               end loop;

               String_Elements.Table (Last).Next := Values;
         end case;
      end if;
   end Add_To_Variable_Value;

   --------------------------------
   -- Set_File_Specific_Switches --
   --------------------------------

   procedure Set_File_Specific_Switches
     (Project    : Prj.Project_Id;
      File_Name  : String;
      Switches   : String;
      In_Package : Name_Id := Name_Compiler)
   is
      Gnatmake_Pkg  : Prj.Package_Id := Prj.Util.Value_Of
        (Name_Gnatmake, Projects.Table (Project).Decl.Packages);
      Compiler_Pkg : Prj.Package_Id;
   begin

      --  Create the gnatmake package if necesary

      if Gnatmake_Pkg = No_Package then
         Packages.Append
           ((Name   => Name_Gnatmake,
             Decl   => No_Declarations,
             Parent => No_Package,
             Next   => Projects.Table (Project).Decl.Packages));
         Projects.Table (Project).Decl.Packages := Packages.Last;

         Gnatmake_Pkg := Packages.Last;
      end if;

      --  Get and create if necessary the compiler subpackage

      Compiler_Pkg := Prj.Util.Value_Of
        (In_Package, Packages.Table (Gnatmake_Pkg).Decl.Packages);

      if Compiler_Pkg = No_Package then
         Packages.Append
           ((Name   => In_Package,
             Decl   => No_Declarations,
             Parent => No_Package,
             Next   => Packages.Table (Gnatmake_Pkg).Decl.Packages));
         Packages.Table (Gnatmake_Pkg).Decl.Packages := Packages.Last;

         Compiler_Pkg := Packages.Last;
      end if;

      --  Set the value

      Name_Len := File_Name'Length;
      Name_Buffer (1 .. Name_Len) := File_Name;
      Set_Value_Of
        (Name                   => Name_Find,
         Variable_Or_Array_Name => Name_Switches,
         In_Decl                => Packages.Table (Compiler_Pkg).Decl,
         Value                  => Switches);
   end Set_File_Specific_Switches;

   -----------------------------
   -- Find_Project_Containing --
   -----------------------------

   function Find_Project_Containing (Root : Prj.Project_Id; File : String)
      return Prj.Project_Id
   is
      Found : Project_Id := No_Project;

      function Action (Prj : Project_Id) return Boolean;
      function Action (Prj : Project_Id) return Boolean is
         Str  : String_List_Id := Projects.Table (Prj).Sources;
         Elem : String_Element;
      begin
         while Str /= Nil_String loop
            Elem := String_Elements.Table (Str);
            String_To_Name_Buffer (Elem.Value);

            --  ??? Could we work directly on String_Id ???
            if Name_Buffer (1 .. Name_Len) = File then
               Found := Prj;
               return False;
            end if;
            Str := Elem.Next;
         end loop;

         return True;
      end Action;

      Tmp : constant Boolean := Action (Root)
        and then For_All_Imported (Root, Action'Unrestricted_Access);
      pragma Warnings (Off, Tmp);
   begin
      return Found;
   end Find_Project_Containing;

   ------------------
   -- Find_Project --
   ------------------

   function Find_Project (Name : String) return Prj.Project_Id  is
   begin
      for J in Projects.Table'First .. Projects.Last loop
         if Get_Name_String (Projects.Table (J).Name) = Name then
            return J;
         end if;
      end loop;
      return No_Project;
   end Find_Project;

   ------------------
   -- Set_Value_Of --
   ------------------

   procedure Set_Value_Of
     (Name                   : Name_Id;
      Variable_Or_Array_Name : Name_Id;
      In_Decl                : in out Declarations;
      Value                  : String)
   is
      The_Array    : Array_Element_Id := No_Array_Element;
      Var          : Variable_Id := No_Variable;
   begin
      if In_Decl /= No_Declarations then
         --  First, look if there is an array element that fits

         The_Array := Get_Array_Index
           (Index    => Name,
            In_Array => Value_Of (Name      => Variable_Or_Array_Name,
                                  In_Arrays => In_Decl.Arrays));

         if The_Array = No_Array_Element then
            Var := Get_Variable_Id
              (Variable_Name => Variable_Or_Array_Name,
               In_Variables  => In_Decl.Variables);
         end if;
      end if;

      if The_Array = No_Array_Element
        and then Var = No_Variable
      then
         Array_Elements.Append
           ((Index => Name,
             Value => Nil_Variable_Value,
             Next  => No_Array_Element));
         The_Array := Array_Elements.Last;
         Arrays.Append
           ((Name  => Variable_Or_Array_Name,
             Value => Array_Elements.Last,
             Next  => In_Decl.Arrays));
         In_Decl.Arrays := Arrays.Last;
      end if;

      if The_Array /= No_Array_Element then
         Array_Elements.Table (The_Array).Value := Nil_Variable_Value;
         Add_To_Variable_Value
           (Add_To => Array_Elements.Table (The_Array).Value,
            Str    => Value,
            Split  => True);
      else
         Variable_Elements.Table (Var).Value := Nil_Variable_Value;
         Add_To_Variable_Value
           (Add_To => Variable_Elements.Table (Var).Value,
            Str    => Value,
            Split  => True);
      end if;
   end Set_Value_Of;

   ---------------------
   -- Get_Array_Index --
   ---------------------

   function Get_Array_Index
     (Index    : Name_Id;
      In_Array : Array_Element_Id) return Array_Element_Id
   is
      Current : Array_Element_Id := In_Array;
   begin
      while Current /= No_Array_Element loop
         if Index = Array_Elements.Table (Current).Index then
            return Current;
         end if;
         Current := Array_Elements.Table (Current).Next;
      end loop;
      return No_Array_Element;
   end Get_Array_Index;

   ---------------------
   -- Get_Variable_Id --
   ---------------------

   function Get_Variable_Id
     (Variable_Name : Name_Id;
      In_Variables  : Variable_Id) return Variable_Id
   is
      Current : Variable_Id := In_Variables;
   begin
      while Current /= No_Variable loop
         if Variable_Name = Variable_Elements.Table (Current).Name then
            return Current;
         end if;
         Current := Variable_Elements.Table (Current).Next;
      end loop;
      return No_Variable;
   end Get_Variable_Id;

   ---------------------
   -- Add_To_Value_Of --
   ---------------------

   procedure Add_To_Value_Of
     (Variable_Name : Name_Id;
      In_Decl       : in out Declarations;
      Value         : String)
   is
      Var          : Variable_Id := No_Variable;
   begin
      if In_Decl /= No_Declarations then
         --  Does the variable already exist ?
         Var := Get_Variable_Id
           (Variable_Name => Variable_Name,
            In_Variables  => In_Decl.Variables);
      end if;

      --  Do we need to create the variable ?

      if Var = No_Variable then
         Variable_Elements.Append
           ((Next  => In_Decl.Variables,
             Name  => Variable_Name,
             Value => Nil_Variable_Value));
         In_Decl.Variables := Variable_Elements.Last;
         Var := In_Decl.Variables;
      end if;

      Add_To_Variable_Value
        (Add_To => Variable_Elements.Table (Var).Value,
         Str    => Value,
         Split  => False);
   end Add_To_Value_Of;

   --------------------
   -- Create_Project --
   --------------------

   function Create_Project (Name : String) return Prj.Project_Id is
      N, Fn : Name_Id;
      Data : Project_Data;
   begin
      Name_Len := Name'Length;
      Name_Buffer (1 .. Name_Len) := Name;
      N := Name_Find;

      Name_Len := Name'Length + 4;
      Name_Buffer (1 .. Name_Len) := Name & ".apr";
      Fn := Name_Find;

      Data.Name := N;
      Data.File_Name := Fn;
      Projects.Append (Data);
      return Projects.Last;
   end Create_Project;

   --------------------
   -- Import_Project --
   --------------------

   procedure Import_Project (Parent, Imported : Prj.Project_Id) is
      function Check (Prj : Project_Id) return Boolean;
      function Check (Prj : Project_Id) return Boolean is
      begin
         return Prj /= Imported;
      end Check;

   begin
      --  Check if it is already imported
      if Imported /= Parent
        and then For_All_Imported (Parent, Check'Unrestricted_Access)
      then
         if Projects.Table (Imported).First_Referred_By = No_Project then
            Projects.Table (Imported).First_Referred_By := Parent;
         end if;

         Project_Lists.Append
           ((Project => Imported,
             Next    => Projects.Table (Parent).Imported_Projects));
         Projects.Table (Parent).Imported_Projects := Project_Lists.Last;
      end if;
   end Import_Project;

   --------------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir (Project : Prj.Project_Id; Dir : String) is
   begin
      Add_To_Value_Of
        (Variable_Name => Name_Source_Dirs,
         In_Decl       => Projects.Table (Project).Decl,
         Value         => Dir);
   end Add_Source_Dir;

begin
   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;
   Prj.Initialize;
   Project_Nodes.Set_Last (Empty_Node);
end Prj_API;
