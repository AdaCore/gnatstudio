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

with Ada.Containers;          use Ada.Containers;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GPR.Env;                 use GPR.Env;
with GPR.Names;               use GPR.Names;
with GPR.Part;                use GPR.Part;
with GPR.PP;                  use GPR.PP;
with GPR.Osint;               use GPR.Osint;
with GPR.Sinput;              use GPR.Sinput;
with GPR.Tree;                use GPR.Tree;

package body Toolchains.Parsers is
   Me : constant Trace_Handle := Create ("GPS.TOOLCHAINS.PARSER");

   package Toolchain_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Toolchain);

   type Attribute_Kind is (Unknown_Kind, Tool_Kind, Compiler_Kind);

   type Attribute (Kind : Attribute_Kind := Unknown_Kind) is record
      Use_Var_Ref       : Boolean := False;
      String_Expression : String_Access;
      Error             : String_Access;

      case Kind is
         when Tool_Kind =>
            Tool : Tools := Unknown;
         when Compiler_Kind =>
            Lang : Language_Id;
         when Unknown_Kind =>
            null;
      end case;
   end record;

   procedure Free (This : in out Attribute);

   procedure Parse
     (This      : in out Toolchain_Parser_Record;
      Node_Data : GPR.Project_Node_Tree_Ref;
      IDE_Node  : Project_Node_Id);
   --  Parse the toolchain contained in the IDE node given in parameter

   function Get_Toolchains
     (This : Toolchain_Parser_Record) return Toolchain_Array;
   --  Return the toolchains red from the project file

   procedure Set_Toolchains
     (This       : in out Toolchain_Parser_Record;
      Toolchains : Toolchain_Array);
   --  Modifies the stucture of the project so that it supports the toolchains
   --  given in parameter

   function Is_Supported (This : Toolchain_Parser_Record) return Boolean;
   --  Return true if the toolchain definition is supported, false otherwise

   function Get_Parsed_Project
     (This : Toolchain_Parser_Record)
      return Parsed_Project;
   --  Return the parsed project from where this toolchain has been extracted

   function Get_Error_Message (This : Toolchain_Parser_Record) return String;
   --  Return the error message associated to the parsing of this toolchain,
   --  if any.

   -----------
   -- Parse --
   -----------

   procedure Parse
     (This      : in out Toolchain_Parser_Record;
      Node_Data : GPR.Project_Node_Tree_Ref;
      IDE_Node  : Project_Node_Id)
   is
      Manager : constant Toolchain_Manager := Get_Manager
        (This.Enclosing_Parser.all);

      procedure Handle_IDE_Package (IDE_Package_Node : Project_Node_Id);
      --  Analyses the IDE package and looks for known-patterns generated for
      --  the toolchain management. At the end of the process, we'll get either
      --  a comprehensive list of managed toolchains, or we'll know that the
      --  target information of the IDE package is manually managed by the
      --  user.

      function Handle_IDE_Case (Case_Id : Project_Node_Id) return Boolean;
      --  Analyses the element in a case statement of the IDE package. Return
      --  true if this package matched an ide package, false otherwise.

      function Handle_Attribute
        (Attribute_Id : Project_Node_Id) return Attribute;
      --  Analyze a single attribute supposed to represent a tool

      procedure Analyze_Variable_Reference
        (Attribute_Id : Project_Node_Id;
         Var_Ref      : Project_Node_Id;
         Result       : in out Attribute);
      --  Analyse the variable reference given in parameter. If no variable has
      --  been set yet for the computation of the targets, then it will be
      --  taken. Otherwise, we'll check that this variable is indeed the one
      --  that we previously identified as being the one for the targets.

      function Create_Error_Message
        (Node : Project_Node_Id; Message : String) return String_Access;
      --  Return an error message prepended with file:line:column format

      ------------------------
      -- Handle_IDE_Package --
      ------------------------

      procedure Handle_IDE_Package (IDE_Package_Node : Project_Node_Id) is
         Renamed_IDE_Project : Project_Node_Id;
         Decl_Id             : Project_Node_Id;
         Item_Id             : Project_Node_Id;
         Name                : Name_Id;

         Is_Case_Mode      : Boolean;
         Is_Attribute_Mode : Boolean;
         Unique_Toolchain  : Toolchain;
         Matching          : Boolean;
         Attr              : Attribute;

         Dummy_Project_Tree : Project_Tree_Access;
         Dummy_Project      : Project_Type := GNATCOLL.Projects.No_Project;
         --  These two variables are used to create a dummy project for the ide
         --  package, setting attributes

      begin
         --  The first thing we want to be able to do here is to list all
         --  targets, and determine on which variables they depend on. It's
         --  only possible if the targets are written in terms of
         --  discrete variables

         --  3 possibilities:
         --  I - only one target - manageable through GPS
         --  II - multiple targets following know scheme - manageable
         --  III - no scheme recognized - disable target selection

         --  First, check if that's a renamed package. In which case, the
         --  Renamed package Should Be Edited / Modified.

         Renamed_IDE_Project := Project_Of_Renamed_Package_Of
           (IDE_Package_Node, Node_Data);

         if Renamed_IDE_Project /= Empty_Project_Node then
            This.Project :=
              Get_Parsed_Project
                (This.Enclosing_Parser.all, Renamed_IDE_Project);

            Decl_Id := First_Declarative_Item_Of
              (Project_Declaration_Of (Renamed_IDE_Project, Node_Data),
               Node_Data);

            while Decl_Id /= Empty_Project_Node loop
               Item_Id := Current_Item_Node (Decl_Id, Node_Data);

               if Kind_Of (Item_Id, Node_Data) = N_Package_Declaration then
                  Name := Name_Of (Item_Id, Node_Data);

                  if Get_Name_String (Name) = "ide" then
                     Handle_IDE_Package (Item_Id);
                  end if;
               end if;

               Decl_Id := Next_Declarative_Item (Decl_Id, Node_Data);
            end loop;

            return;
         end if;

         --  Then analyze the IDE package

         This.IDE_Package := IDE_Package_Node;

         Decl_Id := First_Declarative_Item_Of (IDE_Package_Node, Node_Data);

         Is_Case_Mode := False;
         Is_Attribute_Mode := False;
         Unique_Toolchain := null;

         while Decl_Id /= Empty_Project_Node loop
            Item_Id := Current_Item_Node (Decl_Id, Node_Data);

            if Kind_Of (Item_Id, Node_Data) = N_Case_Construction then
               --  Look for a toolchain expressed in a case statement

               Matching := Handle_IDE_Case (Item_Id);

               if Matching then
                  if Is_Attribute_Mode then
                     This.Error := Create_Error_Message
                       (Item_Id,
                        "can't have both a case statement "
                        & "and toolchain attributes in IDE package");

                     return;
                  end if;

                  This.Toolchain_Case_Statement := Item_Id;
                  Is_Case_Mode := True;
               end if;

            elsif Kind_Of (Item_Id, Node_Data) = N_Attribute_Declaration then
               --  Look for a toolchain directly expressed in attributes

               if Is_Case_Mode then
                  This.Error := Create_Error_Message
                    (Item_Id,
                     "can't have both a case statement "
                     & "and toolchain attributes in IDE package");
               end if;

               Attr := Handle_Attribute (Item_Id);

               if Attr.Kind /= Unknown_Kind and then not Attr.Use_Var_Ref then
                  --  We found an attribute with a plain name

                  if Dummy_Project = GNATCOLL.Projects.No_Project then
                     Dummy_Project_Tree := new Project_Tree;
                     Dummy_Project_Tree.Load_Empty_Project;
                     Dummy_Project := Dummy_Project_Tree.Root_Project;
                  end if;

                  case Attr.Kind is
                     when Compiler_Kind =>
                        Dummy_Project.Set_Attribute
                          (Attribute => Build ("ide", "compiler_command"),
                           Value     => Attr.String_Expression.all,
                           Index     => Element (Attr.Lang));

                     when Tool_Kind =>
                        case Attr.Tool is
                           when Unknown =>
                              null;
                           when GNAT_Driver =>
                              Dummy_Project.Set_Attribute
                                (Attribute => Build ("ide", "gnat"),
                                 Value     => Attr.String_Expression.all);
                           when GNAT_List =>
                              Dummy_Project.Set_Attribute
                                (Attribute => Build ("ide", "gnatlist"),
                                 Value     => Attr.String_Expression.all);
                           when Debugger =>
                              Dummy_Project.Set_Attribute
                                (Attribute => Build
                                   ("ide", "debugger_command"),
                                 Value     => Attr.String_Expression.all);
                           when CPP_Filt =>
                              null;
                        end case;

                     when Unknown_Kind =>
                        null;
                  end case;
               end if;

               if Attr.Kind /= Unknown_Kind then
                  This.Attributes.Insert (Item_Id);

                  if Attr.Error /= null then
                     This.Error := new String'(Attr.Error.all);
                     Free (Attr);

                     return;
                  end if;

                  Is_Attribute_Mode := True;
                  Free (Attr);
               end if;
            end if;

            Decl_Id := Next_Declarative_Item (Decl_Id, Node_Data);
         end loop;

         if Dummy_Project /= GNATCOLL.Projects.No_Project then
            --  We managed to create a dummy project, create the corresponding
            --  toolchain.

            Dummy_Project_Tree.Recompute_View;
            Unique_Toolchain := Get_Toolchain
              (Manager, Dummy_Project_Tree.Root_Project);

            This.Toolchains.Insert
              (Get_Label (Unique_Toolchain), Unique_Toolchain);

            Free (Dummy_Project_Tree);
         elsif This.Variable_Node /= Empty_Project_Node then
            --  The toolchain is controlled by a scenario variable. Read all
            --  the possible values of the scenario and use it to extract
            --  potential toolchains

            declare
               String_Type_Node : constant Project_Node_Id := String_Type_Of
                 (This.Variable_Node, Node_Data);
               String_Node      : Project_Node_Id := First_Literal_String
                 (String_Type_Node, Node_Data);
            begin
               while String_Node /= Empty_Project_Node loop
                  declare
                     Label : constant String := Get_Name_String
                       (String_Value_Of (String_Node, Node_Data));
                  begin
                     if not This.Toolchains.Contains (Label) then
                        This.Toolchains.Insert
                          (Label, Get_Toolchain (Manager, Label));
                     end if;

                     String_Node := Next_Literal_String
                       (String_Node, Node_Data);
                  end;
               end loop;
            end;
         end if;
      end Handle_IDE_Package;

      ---------------------
      -- Handle_IDE_Case --
      ---------------------

      function Handle_IDE_Case (Case_Id : Project_Node_Id) return Boolean is
         Added_Toolchains   : Toolchain_Lists.List;
         Created_Toolchains : Toolchain_Lists.List;
         Cur_Case_Item      : Project_Node_Id := First_Case_Item_Of
           (Case_Id, Node_Data);
         Found_Toolchain_Attributes : Boolean := False;
         Ada_Toolchain : Toolchain;
         Choice_Id          : Project_Node_Id;
         Decl_Id            : Project_Node_Id;
         Item_Id            : Project_Node_Id;
         Error_Found        : String_Access;
         Attr               : Attribute;
      begin
         while Cur_Case_Item /= Empty_Project_Node loop
            Ada_Toolchain := null;

            --  Retreive the name of the toolchain if it's a named case

            Choice_Id := First_Choice_Of (Cur_Case_Item, Node_Data);

            if Choice_Id /= Empty_Project_Node then
               declare
                  Choice_Name : constant String := Get_Name_String
                    (String_Value_Of (Choice_Id, Node_Data));
               begin
                  Ada_Toolchain := Get_Toolchain (Manager, Choice_Name);

                  if Ada_Toolchain = null then
                     Ada_Toolchain := Create_Empty_Toolchain (Manager);
                     Ada_Toolchain.Name := new String'(Choice_Name);
                     Ada_Toolchain.Is_Native := False;
                     Ada_Toolchain.Is_Custom := True;
                     Ada_Toolchain.Is_Valid := False;

                     Created_Toolchains.Append (Ada_Toolchain);
                  end if;

                  Added_Toolchains.Append (Ada_Toolchain);
               end;
            end if;

            --  Analyze the attributes of the toolchain

            Decl_Id := First_Declarative_Item_Of (Cur_Case_Item, Node_Data);

            Error_Found := null;

            while Decl_Id /= Empty_Project_Node loop
               Item_Id := Current_Item_Node (Decl_Id, Node_Data);

               if Kind_Of (Item_Id, Node_Data) = N_Attribute_Declaration then
                  Attr := Handle_Attribute (Item_Id);

                  if Attr.Error /= null then
                     if Found_Toolchain_Attributes then
                        This.Error := new String'(Attr.Error.all);

                        --  The package matched but contained errors
                        return True;
                     else
                        Error_Found := new String'(Attr.Error.all);
                     end if;

                  elsif Attr.Kind /= Unknown_Kind then
                     if Error_Found /= null then
                        This.Error := Error_Found;

                        --  The package matched but contained errors
                        return True;
                     end if;

                     Found_Toolchain_Attributes := True;

                     if Ada_Toolchain /= null
                       and then Ada_Toolchain.Is_Custom
                     then
                        case Attr.Kind is
                           when Tool_Kind =>
                              Set_Command
                                (Ada_Toolchain, Attr.Tool,
                                 Attr.String_Expression.all,
                                 From_Project, False);

                           when Compiler_Kind =>
                              Set_Compiler
                                (Ada_Toolchain,
                                 Element (Attr.Lang),
                                 Attr.String_Expression.all);

                           when Unknown_Kind =>
                              null;
                        end case;
                     end if;
                  end if;

                  Free (Attr);
               else
                  if Found_Toolchain_Attributes then
                     --  If we found at least one toolchain attribute, this is
                     --  the toolchain package.

                     This.Error := Create_Error_Message
                       (Item_Id,
                        "toolchain case can only contain "
                        & "toolchain attributes");

                     return True;
                  else
                     return False;
                  end if;
               end if;

               Decl_Id := Next_Declarative_Item (Decl_Id, Node_Data);
            end loop;

            Cur_Case_Item := Next_Case_Item (Cur_Case_Item, Node_Data);
         end loop;

         if not Found_Toolchain_Attributes then
            --  If we reached that location without findingin toolchain
            --  attributes, we're not on a toolchain case

            return False;
         end if;

         --  If we managed to do the full analysis, then this is a correct
         --  toolchain case statement, so create & add the toolchains if
         --  needed.

         declare
            use Toolchain_Lists;

            Cur : Toolchain_Lists.Cursor;
         begin
            Cur := Created_Toolchains.First;

            while Cur /= Toolchain_Lists.No_Element loop
               Add_Toolchain (Manager, Element (Cur));

               Cur := Next (Cur);
            end loop;

            Cur := Added_Toolchains.First;

            while Cur /= Toolchain_Lists.No_Element loop
               if Element (Cur).Is_Native then
                  This.Toolchains.Insert ("native", Element (Cur));
               else
                  This.Toolchains.Insert
                    (Get_Label (Element (Cur)), Element (Cur));
               end if;

               Cur := Next (Cur);
            end loop;

            return True;
         end;
      end Handle_IDE_Case;

      ----------------------
      -- Handle_Attribute --
      ----------------------

      function Handle_Attribute
        (Attribute_Id : Project_Node_Id) return Attribute
      is
         Result      : Attribute;
         Attribute_Name : constant String := Get_Name_String
           (Name_Of (Attribute_Id, Node_Data));
         Exp_Id      : Project_Node_Id;
         Term_Id     : Project_Node_Id;
         Cur_Term_Id : Project_Node_Id;
         Var_Ref     : Project_Node_Id;
      begin
         Exp_Id := Expression_Of (Attribute_Id, Node_Data);
         Term_Id := First_Term (Exp_Id, Node_Data);
         Cur_Term_Id := Current_Term (Term_Id, Node_Data);
         Var_Ref := Empty_Project_Node;

         if Attribute_Name = "gnatlist" then
            Result :=
              (Kind => Tool_Kind, Tool => GNAT_List, others => <>);

         elsif Attribute_Name = "gnat" then
            Result :=
              (Kind => Tool_Kind, Tool => GNAT_Driver, others => <>);

         elsif Attribute_Name = "compiler_command" then
            declare
               Attribute_Index : constant String := Get_Name_String
                 (Associative_Array_Index_Of (Attribute_Id, Node_Data));
            begin
               if Attribute_Index = "ada" then
                  Result :=
                    (Kind   => Compiler_Kind,
                     Lang   => Manager.Get_Or_Create_Language ("ada"),
                     others => <>);

               elsif Attribute_Index = "c" then
                  Result :=
                    (Kind   => Compiler_Kind,
                     Lang   => Manager.Get_Or_Create_Language ("c"),
                     others => <>);

                  --  ??? what about C++ compiler ???
               end if;
            end;

         elsif Attribute_Name = "debugger_command" then
            Result :=
              (Kind => Tool_Kind, Tool => Debugger, others => <>);

         else
            Result.Error := Create_Error_Message
              (Attribute_Id,
               "attribute is not a toolchain attribute");

            return Result;
         end if;

         if Kind_Of (Cur_Term_Id, Node_Data) = N_Variable_Reference then
            --  We're on a case like:
            --     for GNAT use Target & "-gnat"

            Result.Use_Var_Ref := True;
            Var_Ref := Cur_Term_Id;

            Analyze_Variable_Reference (Attribute_Id, Var_Ref, Result);

            if Result.Error /= null then
               return Result;
            end if;

            Term_Id := Next_Term (Term_Id, Node_Data);

            if Term_Id = Empty_Project_Node then
               Result.Error := Create_Error_Message
                 (Attribute_Id,
                  "only '""tool""' or 'Target & ""suffix""' format supported");

               return Result;
            end if;

            Cur_Term_Id := Current_Term (Term_Id, Node_Data);
         end if;

         --  The only supported attributes are:
         --    String
         --    Var & String

         if Next_Term (Term_Id, Node_Data) /= Empty_Project_Node then
            Result.Error := Create_Error_Message
              (Attribute_Id,
               "only '""tool""' or 'Target & ""suffix""' format supported");

            return Result;
         end if;

         if Kind_Of (Cur_Term_Id, Node_Data) /= N_Literal_String then
            Result.Error := Create_Error_Message
              (Attribute_Id,
               "only '""tool""' or 'Target & ""suffix""' format supported");

            return Result;
         end if;

         Result.String_Expression := new String'
           (Get_Name_String (String_Value_Of (Cur_Term_Id, Node_Data)));

         return Result;
      end Handle_Attribute;

      --------------------------------
      -- Analyze_Variable_Reference --
      --------------------------------

      procedure Analyze_Variable_Reference
        (Attribute_Id : Project_Node_Id;
         Var_Ref      : Project_Node_Id;
         Result       : in out Attribute)
      is
         Var_Name : constant String := To_Lower
           (Get_Name_String (Name_Of (Var_Ref, Node_Data)));
         Var_Node : Project_Node_Id;
      begin
         Var_Node := Get_Variable (This.Project.all, Var_Name);

         if Var_Node = Empty_Project_Node then
            Result.Error := Create_Error_Message
              (Attribute_Id, "variable """ & Var_Name & """not found");

            return;
         end if;

         if This.Variable_Node = Empty_Project_Node then
            This.Variable_Node := Var_Node;

            --  We currently only support target variables declared in the
            --  enclosing package, and referenced that way

            if Package_Node_Of (Var_Ref, Node_Data) /= Empty_Project_Node then
               Result.Error := Create_Error_Message
                 (Attribute_Id,
                  "target variable and toolchain definition "
                  & "not in the same package");

               return;
            end if;

            if Project_Node_Of (Var_Ref, Node_Data) /= Empty_Project_Node then
               Result.Error := Create_Error_Message
                 (Attribute_Id,
                  "target variable and toolchain definition "
                  & "not in the same package");

               return;
            end if;
         else
            if Var_Node /= This.Variable_Node then
               Result.Error := Create_Error_Message
                 (Attribute_Id,
                  "only one variable can be used to describe targets");

               return;
            end if;
         end if;
      end Analyze_Variable_Reference;

      --------------------------
      -- Create_Error_Message --
      --------------------------

      function Create_Error_Message
        (Node : Project_Node_Id; Message : String) return String_Access
      is
         Location : constant Source_Ptr := Location_Of (Node, Node_Data);
         File     : constant String := Get_Name_String
           (Source_File.Table (Get_Source_File_Index (Location)).File_Name);
         Line     : constant Line_Number :=
           Get_Line_Number (Location);
         Col      : constant Column_Number := Get_Column_Number (Location);
      begin
         return new String'
           (File & ":" & Trim (Line'Img, Both)
            & ":" & Trim (Col'Img, Both) & ": " & Message);
      end Create_Error_Message;

   begin
      This.Node_Data := Node_Data;
      This.Project := Get_Root_Project (This.Enclosing_Parser.all);

      if IDE_Node /= Empty_Project_Node then
         Handle_IDE_Package (IDE_Node);
      end if;

      if This.Toolchains.Length = 0 then
         --  If no toolchain has been found, then we're on the native case

         This.Toolchains.Insert ("native", Get_Native_Toolchain (Manager));
      end if;
   end Parse;

   --------------------
   -- Set_Toolchains --
   --------------------

   procedure Set_Toolchains
     (This       : in out Toolchain_Parser_Record;
      Toolchains : Toolchain_Array)
   is
      procedure Remove_Previous_Toolchain;
      --  Remove the case statement used for the toolchain

      procedure Adjust_Target_Variable;
      --  Create the target variable if none, and then set the appropriate
      --  target values

      function Get_Name_Id (Name : String) return Name_Id;
      --  Return the name id of the string queried in parameter

      procedure Create_Target_Variable (Type_Declaration : Project_Node_Id);
      --   Create the target variable after the type given in paramter

      procedure Create_Attributes
        (Container     : Project_Node_Id;
         The_Toolchain : Toolchain);
      --  Create all the attributes for the toolchain given in parameter.
      --  If then toolchain is null, then the variable given in parameter will
      --  be used. This should not be called with both a variable Id and
      --  a toolchain.

      procedure Create_Attribute
        (Container      : Project_Node_Id;
         Attribute_Name : String;
         Index_Name     : String;
         Command        : String);
      --  Create the attribute in the container. If variableId is not null,
      --  then it will be prefixed to the command.

      procedure Create_Case_Toolchain;
      --  Create the "case" block for toolchain selection

      -------------------------------
      -- Remove_Previous_Toolchain --
      -------------------------------

      procedure Remove_Previous_Toolchain is
         Decl_Id      : Project_Node_Id := First_Declarative_Item_Of
           (This.IDE_Package, This.Node_Data);
         Prev_Decl_Id : Project_Node_Id := Empty_Project_Node;
         Item_Id      : Project_Node_Id;
      begin
         while Decl_Id /= Empty_Project_Node loop
            Item_Id := Current_Item_Node (Decl_Id, This.Node_Data);

            if Item_Id = This.Toolchain_Case_Statement
              or else This.Attributes.Contains (Item_Id)
            then
               if Prev_Decl_Id = Empty_Project_Node then
                  Set_First_Declarative_Item_Of
                    (This.IDE_Package,
                     This.Node_Data,
                     Next_Declarative_Item (Decl_Id, This.Node_Data));
               else
                  Set_Next_Declarative_Item
                    (Prev_Decl_Id,
                     This.Node_Data,
                     Next_Declarative_Item (Decl_Id, This.Node_Data));
               end if;
            else
               Prev_Decl_Id := Decl_Id;
            end if;

            Decl_Id := Next_Declarative_Item (Decl_Id, This.Node_Data);
         end loop;
      end Remove_Previous_Toolchain;

      ----------------------------
      -- Adjust_Target_Variable --
      ----------------------------

      procedure Adjust_Target_Variable is
         Type_Declaration : Project_Node_Id;

         Tail_Node, Decl_Type : Project_Node_Id;
         Prev_Node            : Project_Node_Id;
         Contains_Native   : Boolean := False;

         Native_Name : aliased constant String := "native";
         Ext_Default : Project_Node_Id;
      begin
         --  Create the type declaration and the variable if needed

         if This.Variable_Node = Empty_Project_Node then
            --  If there's already a variable called "target", it's most
            --  likely the correct one, so use it. Otherwise, create a new
            --  one.

            This.Variable_Node :=
              Get_Variable (This.Project.all, "target");

            if This.Variable_Node /= Empty_Project_Node then
               Type_Declaration := String_Type_Of
                 (This.Variable_Node, This.Node_Data);
            elsif Toolchains'Length <= 1 then
               --  If there's no variable needed, and no variable found, then
               --  don't create any and just return

               return;
            else
               Type_Declaration := Default_Project_Node
                 (This.Node_Data,
                  N_String_Type_Declaration,
                  List);

               Set_Name_Of
                 (Type_Declaration,
                  This.Node_Data,
                  Get_Name_Id ("Target_Type"));

               Tail_Node := First_Declarative_Item_Of
                 (Get_Project_Declaration (This.Project.all), This.Node_Data);
               Decl_Type := Default_Project_Node
                 (This.Node_Data,
                  N_Declarative_Item,
                  Single);

               Set_Current_Item_Node
                 (Decl_Type, This.Node_Data, Type_Declaration);

               Set_First_Declarative_Item_Of
                 (Get_Project_Declaration (This.Project.all),
                  This.Node_Data,
                  Decl_Type);

               Set_Next_Declarative_Item
                 (Decl_Type, This.Node_Data, Tail_Node);

               Create_Target_Variable (Type_Declaration);
            end if;

            Set_Name_Of
              (This.Variable_Node, This.Node_Data, Get_Name_Id ("Target"));

            Set_String_Type_Of
              (This.Variable_Node, This.Node_Data, Type_Declaration);
         else
            Type_Declaration := String_Type_Of
              (This.Variable_Node, This.Node_Data);
         end if;

         --  Set the toolchain list in the type

         Prev_Node := Empty_Project_Node;

         for J in Toolchains'Range loop
            declare
               Toolchain_Node    : Project_Node_Id;
            begin
               if Toolchains (J).Is_Native then
                  Contains_Native := True;
               end if;

               Toolchain_Node := Create_Literal_String
                 (Get_Name_Id (Get_Name (Toolchains (J))),
                  This.Node_Data);

               if Prev_Node = Empty_Project_Node then
                  Set_First_Literal_String
                    (Type_Declaration,
                     This.Node_Data,
                     Toolchain_Node);
               else
                  Set_Next_Literal_String
                    (Prev_Node,
                     This.Node_Data,
                     Toolchain_Node);
               end if;

               Prev_Node := Toolchain_Node;
            end;
         end loop;

         --  Set the default variable value
         --  Use the native as the default toolchain if it exists,
         --  first of the toolchains to add otherwise.

         if Contains_Native then
            Ext_Default :=
              Create_Literal_String
                (Get_Name_Id (Native_Name), This.Node_Data);
         else
            Ext_Default :=
              Create_Literal_String
                (Get_Name_Id (Get_Label (Toolchains (Toolchains'First))),
                 This.Node_Data);
         end if;

         declare
            Ext_Value   : constant Project_Node_Id :=
              Current_Term
                (First_Term
                     (Expression_Of (This.Variable_Node, This.Node_Data),
                      This.Node_Data),
                 This.Node_Data);
         begin
            Set_External_Default_Of (Ext_Value, This.Node_Data, Ext_Default);
         end;
      end Adjust_Target_Variable;

      -----------------
      -- Get_Name_Id --
      -----------------

      function Get_Name_Id (Name : String) return Name_Id is
      begin
         GPR.Names.Name_Buffer (1 .. Name'Length) := Name;
         GPR.Names.Name_Len := Name'Length;

         return GPR.Names.Name_Find;
      end Get_Name_Id;

      ----------------------------
      -- Create_Target_Variable --
      ----------------------------

      procedure Create_Target_Variable (Type_Declaration : Project_Node_Id) is
         Ext_Value : Project_Node_Id;
         Ext_Var   : Project_Node_Id;
         Decl_Var  : Project_Node_Id;
         Decl_Type : Project_Node_Id;
         Tail_Node : Project_Node_Id;
         Item_Node : Project_Node_Id;
      begin
         This.Variable_Node := Default_Project_Node
           (This.Node_Data,
            N_Typed_Variable_Declaration,
            Single);

         Set_Name_Of
           (This.Variable_Node, This.Node_Data, Get_Name_Id ("Target"));

         Set_String_Type_Of
           (This.Variable_Node, This.Node_Data, Type_Declaration);
         Ext_Value := Default_Project_Node
           (This.Node_Data, N_External_Value, Undefined);
         Ext_Var := Create_Literal_String
           (Get_Name_Id ("TARGET"), This.Node_Data);

         Set_External_Reference_Of (Ext_Value, This.Node_Data, Ext_Var);
         Set_Expression_Of
           (This.Variable_Node,
            This.Node_Data,
            Enclose_In_Expression (Ext_Value, This.Node_Data));

         Decl_Var := Default_Project_Node
           (This.Node_Data,
            N_Declarative_Item,
            Single);

         Set_Current_Item_Node
           (Decl_Var, This.Node_Data, This.Variable_Node);

         Decl_Type := First_Declarative_Item_Of
           (Get_Project_Declaration (This.Project.all), This.Node_Data);

         while Decl_Type /= Empty_Project_Node loop
            Item_Node := Current_Item_Node
              (Decl_Type, This.Node_Data);

            exit when Item_Node = Type_Declaration;

            Decl_Type := Next_Declarative_Item (Decl_Type, This.Node_Data);
         end loop;

         Tail_Node := Next_Declarative_Item
           (Decl_Type, This.Node_Data);

         Set_Next_Declarative_Item
           (Decl_Type, This.Node_Data, Decl_Var);
         Set_Next_Declarative_Item
           (Decl_Var, This.Node_Data, Tail_Node);
      end Create_Target_Variable;

      -----------------------
      -- Create_Attributes --
      -----------------------

      procedure Create_Attributes
        (Container     : Project_Node_Id;
         The_Toolchain : Toolchain)
      is
      begin
         if not The_Toolchain.Is_Native
           or else not Is_Default (The_Toolchain, GNAT_Driver)
         then
            Create_Attribute
              (Container, "gnat",             "",
               Get_Command (The_Toolchain, GNAT_Driver));
         end if;

         if not The_Toolchain.Is_Native
           or else not Is_Default (The_Toolchain, GNAT_List)
         then
            Create_Attribute
              (Container, "gnatlist",         "",
               Get_Command (The_Toolchain, GNAT_List));
         end if;

         if not The_Toolchain.Is_Native
           or else not Is_Default (The_Toolchain, Debugger)
         then
            Create_Attribute
              (Container, "debugger_command", "",
               Get_Command (The_Toolchain, Debugger));
         end if;

         if not Is_Default (The_Toolchain, "ada") then
            Create_Attribute
              (Container, "compiler_command", "ada",
               Get_Exe (Get_Compiler (The_Toolchain, "ada")));
         end if;

         if not Is_Default (The_Toolchain, "c") then
            Create_Attribute
              (Container, "compiler_command", "c",
               Get_Exe (Get_Compiler (The_Toolchain, "c")));
         end if;
      end Create_Attributes;

      ----------------------
      -- Create_Attribute --
      ----------------------

      procedure Create_Attribute
        (Container      : Project_Node_Id;
         Attribute_Name : String;
         Index_Name     : String;
         Command        : String)
      is
         Index_Id : Name_Id;
         Str_Val  : Project_Node_Id;
         Exp      : Project_Node_Id;
      begin
         if Index_Name = "" then
            Index_Id := No_Name;
         else
            Index_Id := Get_Name_Id (Index_Name);
         end if;

         Str_Val := Create_Literal_String
           (Get_Name_Id (Command), This.Node_Data);

         Exp := Str_Val;

         declare
            Dummy : Project_Node_Id :=
              GPR.Tree.Create_Attribute
                (This.Node_Data,
                 Container,
                 Get_Name_Id (Attribute_Name),
                 Index_Id,
                 Single,
                 0,
                 Exp);
            pragma Unreferenced (Dummy);
         begin
            null;
         end;
      end Create_Attribute;

      ---------------------------
      -- Create_Case_Toolchain --
      ---------------------------

      procedure Create_Case_Toolchain is
         Case_Construct_Node : Project_Node_Id;
         Ref                 : Project_Node_Id;
         Prev_Case_Node      : Project_Node_Id;
         Case_Node           : Project_Node_Id;
         Name_Str            : Project_Node_Id;
      begin
         Prev_Case_Node := Empty_Project_Node;

         Case_Construct_Node := Default_Project_Node
           (This.Node_Data,
            N_Case_Construction,
            Undefined);

         Add_At_End
           (This.Node_Data,
            This.IDE_Package,
            Case_Construct_Node,
            False,
            False);

         Ref := Default_Project_Node
           (This.Node_Data, N_Variable_Reference, Undefined);

         Set_Name_Of
           (Ref, This.Node_Data, Name_Of (This.Variable_Node, This.Node_Data));

         Set_Case_Variable_Reference_Of
           (Case_Construct_Node,
            This.Node_Data,
            Ref);

         --  Handle "non standard" toolchains, aamp, native, custom...

         for J in Toolchains'Range loop
            Case_Node := Default_Project_Node
              (This.Node_Data,
               N_Case_Item,
               Undefined);

            if Prev_Case_Node = Empty_Project_Node then
               Set_First_Case_Item_Of
                 (Case_Construct_Node, This.Node_Data, Case_Node);
            else
               Set_Next_Case_Item
                 (Prev_Case_Node, This.Node_Data, Case_Node);
            end if;

            Prev_Case_Node := Case_Node;

            if Toolchains (J).Is_Native then
               Name_Str := Create_Literal_String
                 (Get_Name_Id ("native"), This.Node_Data);
            else
               Name_Str := Create_Literal_String
                 (Get_Name_Id
                    (Get_Label (Toolchains (J))), This.Node_Data);
            end if;

            Set_First_Choice_Of (Case_Node, This.Node_Data, Name_Str);
            Create_Attributes (Case_Node, Toolchains (J));
         end loop;
      end Create_Case_Toolchain;

   begin
      --  First, Create the IDE package if there's none

      if This.IDE_Package = Empty_Project_Node then
         This.IDE_Package := Create_Package
           (This.Node_Data, Get_Project_Node (This.Project.all), "ide");
      end if;

      --  Second, remove the case statement representing the toolchains
      --  if any, or the attributes from the previous toolchain

      Remove_Previous_Toolchain;

      --  Third, create or update the target type variable

      Adjust_Target_Variable;

      --  Finally, create the ide package

      if Toolchains'Length = 0 then
         --  There's no toolchain indication, don't do anything, just
         --  leave an empty ide package
         null;
      elsif Toolchains'Length = 1 then
         --  If there's only one toolchain to take care of, then only
         --  generate a block of string values.

         Create_Attributes
           (This.IDE_Package, Toolchains (Toolchains'First));
      else
         --  If we've got multiple targets handling with custom names,
         --  then we need to use a case statement.

         Create_Case_Toolchain;
      end if;
   end Set_Toolchains;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Attribute) is
   begin
      Free (This.String_Expression);
      Free (This.Error);
   end Free;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported (This : Toolchain_Parser_Record) return Boolean is
   begin
      return This.Error = null;
   end Is_Supported;

   ------------------------
   -- Get_Parsed_Project --
   ------------------------

   function Get_Parsed_Project
     (This : Toolchain_Parser_Record) return Parsed_Project
   is
   begin
      return This.Project;
   end Get_Parsed_Project;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (This : Toolchain_Parser_Record) return String is
   begin
      if This.Error = null then
         return "";
      else
         return This.Error.all;
      end if;
   end Get_Error_Message;

   --------------------
   -- Get_Toolchains --
   --------------------

   function Get_Toolchains
     (This : Toolchain_Parser_Record) return Toolchain_Array
   is
      use Toolchain_Maps;

      Cur : Toolchain_Maps.Cursor := This.Toolchains.First;

      Result : Toolchain_Array (1 .. Integer (This.Toolchains.Length));
   begin
      for J in Result'Range loop
         Result (J) := Element (Cur);

         Cur := Next (Cur);
      end loop;

      return Result;
   end Get_Toolchains;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (This         : in out Project_Parser_Record;
      Manager      : Toolchain_Manager;
      Path         : Virtual_File;
      Project_Path : File_Array)
   is
      Decl_Id : Project_Node_Id;
      Item_Id : Project_Node_Id;
      Name    : Name_Id;
      Toolchain_Found : Boolean := False;
      Env     : GPR.Tree.Environment;
   begin
      if not Path.Is_Regular_File then
         raise Toolchain_Exception
           with """" & String (Path.Full_Name.all)
           & """ is not a regular file";
      end if;

      This.Manager := Manager;
      This.Tree_Data := new Project_Tree_Data;
      GPR.Initialize (This.Tree_Data);

      This.Node_Data := new Project_Node_Tree_Data;
      Initialize (This.Node_Data);

      Initialize
         (Env, Create_Flags
            (Report_Error               => null,
             When_No_Sources            => Silent,
             Require_Sources_Other_Lang => False,
             Allow_Duplicate_Basenames  => True,
             Compiler_Driver_Mandatory  => False,
             Error_On_Unknown_Language  => False,
             Require_Obj_Dirs           => Silent,
             Allow_Invalid_External     => Silent,
             Missing_Source_Files       => Silent));
      Add_Directories (Env.Project_Path, String (To_Path (Project_Path)));

      Parse (In_Tree                => This.Node_Data,
             Project                => This.Enclosing_Project_Node,
             Project_File_Name      => String (Path.Base_Name),
             Packages_To_Check      => All_Packages,
             Store_Comments         => True,
             Current_Directory      => String (Path.Dir_Name),
             Is_Config_File         => False,
             Target_Name            => "",
             Env                    => Env);
      Free (Env);

      This.Root_Project := new Parsed_Project_Record;
      Initialize
        (This.Root_Project,
         This,
         This.Node_Data,
         This.Enclosing_Project_Node);

      This.Root_Project_Node := Project_Declaration_Of
        (This.Enclosing_Project_Node, This.Node_Data);

      Decl_Id := First_Declarative_Item_Of
        (This.Root_Project_Node, This.Node_Data);

      --  Look for an ide package. Create a toolchain parser on this IDE
      --  package to extract the toolchain information for this project.

      while Decl_Id /= Empty_Project_Node loop
         Item_Id := Current_Item_Node (Decl_Id, This.Node_Data);

         if Kind_Of (Item_Id, This.Node_Data) = N_Package_Declaration then
            Name := Name_Of (Item_Id, This.Node_Data);

            if Get_Name_String (Name) = "ide" then
               Parse (This.Toolchain_Found, This.Node_Data, Item_Id);

               Toolchain_Found := True;
            end if;
         end if;

         Decl_Id := Next_Declarative_Item (Decl_Id, This.Node_Data);
      end loop;

      if not Toolchain_Found then
         Parse (This.Toolchain_Found, This.Node_Data, Empty_Project_Node);
      end if;

      This.Is_Valid := True;

   exception
      when E : others =>
         Trace (Me, E);
         This.Is_Valid := False;
   end Parse;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (This : Project_Parser_Record) return Toolchain_Manager
   is
   begin
      return This.Manager;
   end Get_Manager;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Project_Parser_Record) return Boolean is
   begin
      return This.Is_Valid;
   end Is_Valid;

   --------------------
   -- Get_Toolchains --
   --------------------

   function Get_Toolchains
     (This : Project_Parser_Record) return Toolchain_Array is
   begin
      return Get_Toolchains (This.Toolchain_Found);
   end Get_Toolchains;

   --------------------
   -- Set_Toolchains --
   --------------------

   procedure Set_Toolchains
     (This       : in out Project_Parser_Record;
      Toolchains : Toolchain_Array)
   is
   begin
      Set_Toolchains (This.Toolchain_Found, Toolchains);
   end Set_Toolchains;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported (This : Project_Parser_Record) return Boolean is
   begin
      return Is_Supported (This.Toolchain_Found);
   end Is_Supported;

   ------------------------
   -- Get_Parsed_Project --
   ------------------------

   function Get_Parsed_Project
     (This : Project_Parser_Record)
      return Parsed_Project
   is
   begin
      return Get_Parsed_Project (This.Toolchain_Found);
   end Get_Parsed_Project;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message (This : Project_Parser_Record) return String is
   begin
      return Get_Error_Message (This.Toolchain_Found);
   end Get_Error_Message;

   ----------------------
   -- Get_Root_Project --
   ----------------------

   function Get_Root_Project
     (This : Project_Parser_Record) return Parsed_Project
   is
   begin
      return This.Root_Project;
   end Get_Root_Project;

   ------------------------
   -- Get_Parsed_Project --
   ------------------------

   function Get_Parsed_Project
     (This : Project_Parser_Record;
      Node : Project_Node_Id) return Parsed_Project
   is
   begin
      return This.Parsed_Projects.Element (Node);
   end Get_Parsed_Project;

   ------------------
   -- Get_Variable --
   ------------------

   function Get_Variable
     (This : Parsed_Project_Record;
      Name : String) return Project_Node_Id
   is
   begin
      if This.Variables.Contains (Name) then
         return This.Variables.Element (Name);
      else
         return Empty_Project_Node;
      end if;
   end Get_Variable;

   ----------------------
   -- Get_Project_Node --
   ----------------------

   function Get_Project_Node
     (This : Parsed_Project_Record) return Project_Node_Id is
   begin
      return This.Project_Node;
   end Get_Project_Node;

   -----------------------------
   -- Get_Project_Declaration --
   -----------------------------

   function Get_Project_Declaration
     (This : Parsed_Project_Record) return Project_Node_Id is
   begin
      return Project_Declaration_Of (This.Project_Node, This.Node_Data);
   end Get_Project_Declaration;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This         : Parsed_Project;
      Parser       : in out Project_Parser_Record;
      Node_Data    : GPR.Project_Node_Tree_Ref;
      Project_Node : Project_Node_Id)
   is
      Source_File    : Source_File_Index;
      Item_Id        : Project_Node_Id;
      With_Id        : Project_Node_Id;
      With_Prj_Id    : Project_Node_Id;
      Decl_Id        : Project_Node_Id;
      Name           : Name_Id;
      Withed_Project : Parsed_Project;
   begin
      This.Is_Root := True;
      This.Node_Data := Node_Data;
      This.Project_Node := Project_Node;

      Source_File := Get_Source_File_Index
        (Location_Of (This.Project_Node, This.Node_Data));
      This.Path := Create
        (+Get_Name_String (Full_File_Name (Source_File)));

      Parser.Parsed_Projects.Insert (This.Project_Node, This);

      --  Creates a parser for all withed project recursively

      With_Id := First_With_Clause_Of (This.Project_Node, This.Node_Data);

      while With_Id /= Empty_Project_Node loop

         --  consider only non-limited with
         if Non_Limited_Project_Node_Of
           (With_Id, This.Node_Data) /= Empty_Project_Node
         then
            With_Prj_Id := Project_Node_Of (With_Id, This.Node_Data);

            if not Parser.Parsed_Projects.Contains (With_Prj_Id) then
               Withed_Project := new Parsed_Project_Record;
               Initialize
                 (Withed_Project,
                  Parser,
                  This.Node_Data,
                  With_Prj_Id);
               Withed_Project.Is_Root := False;
            end if;
         end if;

         With_Id := Next_With_Clause_Of (With_Id, This.Node_Data);
      end loop;

      --  Extract all scenario variables declared in this project

      Decl_Id := First_Declarative_Item_Of
        (Project_Declaration_Of
           (This.Project_Node, This.Node_Data), This.Node_Data);

      while Decl_Id /= Empty_Project_Node loop
         Item_Id := Current_Item_Node (Decl_Id, This.Node_Data);

         if Kind_Of (Item_Id, This.Node_Data)
           = N_Typed_Variable_Declaration
         then
            Name := Name_Of (Item_Id, This.Node_Data);
            This.Variables.Insert (Get_Name_String (Name), Item_Id);
         end if;

         Decl_Id := Next_Declarative_Item (Decl_Id, This.Node_Data);
      end loop;
   end Initialize;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Parsed_Project_Record) return Boolean is
   begin
      return This.Is_Root;
   end Is_Root;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (This : Parsed_Project_Record) return Virtual_File is
   begin
      return This.Path;
   end Get_Path;

   ----------
   -- Save --
   ----------

   procedure Save (This : Parsed_Project_Record) is
   begin
      Save (This, This.Path);
   end Save;

   ----------
   -- Save --
   ----------

   procedure Save (This : Parsed_Project_Record; To : Virtual_File) is
      File : Writable_File;

      procedure Write_Char_Ap (C : Character);
      procedure Write_Eol_Ap;
      procedure Write_Str_Ap (S : String);

      procedure Write_Char_Ap (C : Character) is
      begin
         Write (File, C & "");
      end Write_Char_Ap;

      procedure Write_Eol_Ap is
      begin
         Write (File, ASCII.LF & "");
      end Write_Eol_Ap;

      procedure Write_Str_Ap (S : String) is
      begin
         Write (File, S);
      end Write_Str_Ap;

   begin
      File := Write_File (To);

      Pretty_Print
        (Project                => This.Project_Node,
         In_Tree                => This.Node_Data,
         W_Char                 => Write_Char_Ap'Unrestricted_Access,
         W_Eol                  => Write_Eol_Ap'Unrestricted_Access,
         W_Str                  => Write_Str_Ap'Unrestricted_Access,
         Backward_Compatibility => False);

      Close (File);
   end Save;

end Toolchains.Parsers;
