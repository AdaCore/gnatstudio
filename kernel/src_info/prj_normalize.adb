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

with Prj_API;       use Prj_API;
with Prj_Scenarios; use Prj_Scenarios;

with Prj;      use Prj;
with Prj.PP;   use Prj.PP;
with Prj.Tree; use Prj.Tree;
with Namet;    use Namet;
with Types;    use Types;

with Text_IO;  use Text_IO;

package body Prj_Normalize is

   Internal_Project_Prefix : constant String := ".Internal";
   --  Prefix appended to the name of the root project to create the name of
   --  the internal project that contains the definition of all the scenarios.

   Scenario_Attribute_Name : constant String := "scenario";
   --  Name of the attribute used to store the name of the current
   --  configuration.

   type Project_Converter is record
      Common_Section : Project_Node_Id := Empty_Node;
      Case_Section   : Project_Node_Id := Empty_Node;
      Pkg_Section    : Project_Node_Id := Empty_Node;
   end record;

   procedure Recurse (Mgr              : in out Scenario_Manager;
                      Values           : in out String_Id_Array;
                      Converter        : in out Project_Converter;
                      Current_Project  : Project_Node_Id;
                      Internal_Project : Project_Node_Id;
                      Declarative_Item : Project_Node_Id);
   --  Convert the list of statements in Declarative_Item. Puts the
   --  normalized form in Project_Out.
   --  Internal_Project should be a reference to the internal project used
   --  internally to define scenarios.

   function Get_Or_Create_Case_Item
     (Name : String_Id; Case_Construction : Project_Node_Id)
      return Project_Node_Id;
   --  Return the case item in Case_Construction that matches Name.
   --  This case_item is created if it doesn't exist yet.

   function External_Variable_Name
     (Current_Project : Project_Node_Id; Ref : Project_Node_Id)
      return String_Id;
   --  Return the name of the external variable referenced by Ref.
   --  This subprogram looks at the variabel declaration, possibly in another
   --  package.

   procedure Recursive_Normalize
     (Project          : Project_Node_Id;
      Mgr              : in out Scenario_Manager;
      Internal_Project : Project_Node_Id);
   --  Recursively convert a project and all its imported projects.

   -----------------------------
   -- Get_Or_Create_Case_Item --
   -----------------------------

   function Get_Or_Create_Case_Item
     (Name : String_Id; Case_Construction : Project_Node_Id)
      return Project_Node_Id
   is
      Case_Item : Project_Node_Id := First_Case_Item_Of (Case_Construction);
      Choice : Project_Node_Id;
   begin
      while Case_Item /= Empty_Node loop
         Choice := First_Choice_Of (Case_Item);
         if Name = String_Value_Of (Choice) then
            return Case_Item;
         end if;
         Case_Item := Next_Case_Item (Case_Item);
      end loop;

      Case_Item := Default_Project_Node (N_Case_Item);
      Set_Next_Case_Item (Case_Item, First_Case_Item_Of (Case_Construction));
      Set_First_Case_Item_Of (Case_Construction, Case_Item);

      Choice := Default_Project_Node (N_Literal_String);
      Set_String_Value_Of (Choice, Name);
      Set_First_Choice_Of (Case_Item, Choice);

      return Case_Item;
   end Get_Or_Create_Case_Item;

   ----------------------------
   -- External_Variable_Name --
   ----------------------------

   function External_Variable_Name
     (Current_Project : Project_Node_Id; Ref : Project_Node_Id)
      return String_Id
   is
      N : constant Name_Id := Name_Of (Ref);
      Prj : Project_Node_Id := Current_Project;
      Decl, Item, Pkg : Project_Node_Id;
   begin
      if Project_Node_Of (Ref) /= Empty_Node then
         Prj := Project_Node_Of (Ref);
      end if;

      if Package_Node_Of (Ref) /= Empty_Node then
         Pkg := First_Package_Of (Prj);
         while Pkg /= Package_Node_Of (Ref) loop
            Pkg := Next_Package_In_Project (Pkg);
         end loop;
         Decl := First_Declarative_Item_Of (Pkg);
      else
         Decl := First_Declarative_Item_Of (Project_Declaration_Of (Prj));
      end if;

      while Decl /= Empty_Node loop
         Item := Current_Item_Node (Decl);
         if (Kind_Of (Item) = N_Variable_Declaration
             or else Kind_Of (Item) = N_Typed_Variable_Declaration)
           and then Name_Of (Item) = N
         then
            return External_Reference_Of (Item);
         end if;

         Decl := Next_Declarative_Item (Decl);
      end loop;

      return No_String;
   end External_Variable_Name;

   -------------
   -- Recurse --
   -------------

   procedure Recurse (Mgr              : in out Scenario_Manager;
                      Values           : in out String_Id_Array;
                      Converter        : in out Project_Converter;
                      Current_Project  : Project_Node_Id;
                      Internal_Project : Project_Node_Id;
                      Declarative_Item : Project_Node_Id)
   is
      Decl_Item : Project_Node_Id := Declarative_Item;
      Case_Item : Project_Node_Id;
      Var_Index : Natural;
      Parent_Decl   : Project_Node_Array_Access;
      Decl_Item_Out : Project_Node_Array_Access;

      procedure Append (Item : Project_Node_Id);
      --  Append Item at the end of the declaration list for all the
      --  Parent_Decl.

      procedure Find_Parent_Decl;
      --  Find the output declarative item to which statements read in the
      --  initial project should be added. If we have already seen a case
      --  statement, then this sets the appropriate list of case items that
      --  should be appended to.

      ------------
      -- Append --
      ------------

      procedure Append (Item : Project_Node_Id) is
      begin
         for D in Decl_Item_Out'Range loop
            if Decl_Item_Out (D) = Empty_Node then
               Decl_Item_Out (D) := Default_Project_Node (N_Declarative_Item);

               --  Can only happen when there is a single parent and were
               --  are in the common section
               if Parent_Decl (D) = Empty_Node then
                  Converter.Common_Section := Decl_Item_Out (D);
                  Parent_Decl (D) := Decl_Item_Out (D);
               else
                  Set_First_Declarative_Item_Of
                    (Parent_Decl (D), Decl_Item_Out (D));
               end if;
            else
               Set_Next_Declarative_Item
                 (Decl_Item_Out (D),
                  Default_Project_Node (N_Declarative_Item));
               Decl_Item_Out (D) := Next_Declarative_Item (Decl_Item_Out (D));
            end if;
            Set_Current_Item_Node (Decl_Item_Out (D), Item);
         end loop;
      end Append;

      ----------------------
      -- Find_Parent_Decl --
      ----------------------

      procedure Find_Parent_Decl is
         In_Case : Boolean;
         J       : Natural;
      begin
         --  First, a quick check to make sure that at least one item in Values
         --  is not null. Otherwise, we end up appending the same expression to
         --  all the nodes in the case statement.

         J := Values'First;
         if Converter.Case_Section = Empty_Node then
            while J <= Values'Last loop
               exit when Values (J) /= No_String;
               J := J + 1;
            end loop;
         end if;

         In_Case := (J <= Values'Last);

         --  Find the parent in which new declarations will be added

         if In_Case then
            if Converter.Case_Section = Empty_Node then
               Converter.Case_Section := Default_Project_Node
                 (N_Case_Construction);
               Set_Case_Variable_Reference_Of
                 (Converter.Case_Section,
                  Get_Scenario_Var_Reference (Mgr, Internal_Project));
            end if;
            declare
               Names : constant String_Id_Array :=
                 Get_Scenario_Names (Mgr, Values, Create => True);
            begin
               Parent_Decl := new Project_Node_Array (Names'Range);
               for A in Names'Range loop
                  Parent_Decl (A) := Get_Or_Create_Case_Item
                    (Names (A), Converter.Case_Section);
               end loop;
            end;
         else
            Parent_Decl := new Project_Node_Array (1 .. 1);
            Parent_Decl (1) := Converter.Common_Section;
         end if;

         --  Find the last declaration in that parent
         --  ??? Can we really call recurse twice with the same Parent.
         --  ??? If not, we don't need to look for the end, only insert in
         --  ??? front

         Decl_Item_Out := new Project_Node_Array (Parent_Decl'Range);

         for P in Parent_Decl'Range loop
            if Parent_Decl (P) /= Empty_Node then
               Decl_Item_Out (P) :=
                 First_Declarative_Item_Of (Parent_Decl (P));
               if Decl_Item_Out (P) /= Empty_Node then
                  while Next_Declarative_Item (Decl_Item_Out (P))
                    /= Empty_Node
                  loop
                     Decl_Item_Out (P) :=
                       Next_Declarative_Item (Decl_Item_Out (P));
                  end loop;
               end if;
            else
               Decl_Item_Out (P) := Empty_Node;
            end if;
         end loop;
      end Find_Parent_Decl;

   begin
      Find_Parent_Decl;

      --  Add the required nodes


      while Decl_Item /= Empty_Node loop
         case Kind_Of (Current_Item_Node (Decl_Item)) is
            when N_Case_Construction =>

               --  ??? Should test if current node is not an external variable
               --  ??? Need to get the name of the external references by
               --  N_Variable_Reference
               Var_Index := Variable_Index
                 (Mgr, External_Variable_Name
                  (Current_Project,
                   Case_Variable_Reference_Of
                   (Current_Item_Node (Decl_Item))));

               pragma Assert (Var_Index /= 0,
                              "Cannot import projects that have case "
                              & " statements not associated with"
                              & " typed and external variables");

               Case_Item := First_Case_Item_Of
                 (Current_Item_Node (Decl_Item));
               while Case_Item /= Empty_Node loop
                  --  Do we have a "when others" ?
                  if First_Choice_Of (Case_Item) = Empty_Node then
                     Values (Var_Index) := No_String;
                  else
                     Values (Var_Index) :=
                       String_Value_Of (First_Choice_Of (Case_Item));
                  end if;

                  Recurse (Mgr,
                           Values, Converter,
                           Current_Project,
                           Internal_Project,
                           First_Declarative_Item_Of (Case_Item));
                  Case_Item := Next_Case_Item (Case_Item);
               end loop;
               Values (Var_Index) := No_String;

               --  We then need to reevaluate the parent declaration, since all
               --  statements from now on need to be added to the case
               --  statement, otherwise we won't respect the order of commands
               --  given by the user

               Free (Parent_Decl);
               Free (Decl_Item_Out);
               Find_Parent_Decl;

            when N_Package_Declaration =>
               declare
                  Pkg_Converter : Project_Converter;
                  Val           : String_Id_Array (Values'Range) :=
                    (others => No_String);
                  Pkg, Decl : Project_Node_Id;
               begin
                  Recurse (Mgr,
                           Val, Pkg_Converter,
                           Current_Project,
                           Internal_Project,
                           First_Declarative_Item_Of
                           (Current_Item_Node (Decl_Item)));

                  Pkg := Default_Project_Node (N_Package_Declaration);
                  Set_Name_Of (Pkg, Name_Of (Current_Item_Node (Decl_Item)));
                  Set_Project_Of_Renamed_Package_Of
                    (Pkg, Project_Of_Renamed_Package_Of
                     (Current_Item_Node (Decl_Item)));

                  Decl := Default_Project_Node (N_Declarative_Item);
                  Set_Current_Item_Node (Decl, Pkg);
                  Set_Next_Declarative_Item (Decl, Converter.Pkg_Section);
                  Converter.Pkg_Section := Decl;

                  if Pkg_Converter.Common_Section /= Empty_Node then
                     Set_First_Declarative_Item_Of
                       (Pkg, Pkg_Converter.Common_Section);
                     if Pkg_Converter.Case_Section /= Empty_Node then
                        Decl := Pkg_Converter.Common_Section;
                        while Next_Declarative_Item (Decl) /= Empty_Node loop
                           Decl := Next_Declarative_Item (Decl);
                        end loop;
                        Set_Next_Declarative_Item
                          (Decl, Pkg_Converter.Case_Section);
                     end if;
                  elsif Pkg_Converter.Case_Section /= Empty_Node then
                     Set_First_Declarative_Item_Of
                       (Pkg, Pkg_Converter.Case_Section);
                  end if;
               end;

            when N_String_Type_Declaration =>
               --  A type declaration will only be added if it is used for a
               --  non-scenario variable
               null;

            when N_Typed_Variable_Declaration =>
               if not Is_External_Variable (Current_Item_Node (Decl_Item)) then
                  --  Output the type and the variable
                  --  ??? If the type is shared between several variables, this
                  --  ??? means it will be output several types. Howver, this
                  --  ??? is unlikely to happen in real case, and this can
                  --  ??? still be fixed manually
                  Append (String_Type_Of (Current_Item_Node (Decl_Item)));
                  Append (Current_Item_Node (Decl_Item));
               end if;

            when others =>
               Append (Current_Item_Node (Decl_Item));
         end case;

         Decl_Item := Next_Declarative_Item (Decl_Item);
      end loop;

      Free (Parent_Decl);
   end Recurse;

   -------------------------
   -- Recursive_Normalize --
   -------------------------

   procedure Recursive_Normalize
     (Project          : Project_Node_Id;
      Mgr              : in out Scenario_Manager;
      Internal_Project : Project_Node_Id)
   is
      Values : String_Id_Array (1 .. Variable_Count (Mgr)) :=
        (others => No_String);
      Converter : Project_Converter;
      Decl, Item : Project_Node_Id;
   begin
      Add_Imported_Project (Project, Internal_Project);
      Recurse (Mgr,
               Values,
               Converter,
               Project,
               Internal_Project,
               First_Declarative_Item_Of (Project_Declaration_Of (Project)));

      Set_First_Declarative_Item_Of (Get_Or_Create_Declaration (Project),
                                     Converter.Common_Section);
      Decl := Converter.Common_Section;

      Item := Default_Project_Node (N_Declarative_Item);
      Set_Current_Item_Node (Item, Converter.Case_Section);

      if Decl = Empty_Node then
         Set_First_Declarative_Item_Of
           (Get_Or_Create_Declaration (Project), Item);
         Decl := Converter.Case_Section;
      else
         while Next_Declarative_Item (Decl) /= Empty_Node loop
            Decl := Next_Declarative_Item (Decl);
         end loop;
         Set_Next_Declarative_Item (Decl, Item);
      end if;

      if Decl = Empty_Node then
         Set_First_Declarative_Item_Of (Get_Or_Create_Declaration (Project),
                                        Converter.Pkg_Section);
      else
         while Next_Declarative_Item (Decl) /= Empty_Node loop
            Decl := Next_Declarative_Item (Decl);
         end loop;
         Set_Next_Declarative_Item (Decl, Converter.Pkg_Section);
      end if;

      Pretty_Print (Project);
      New_Line;
      New_Line;

      Item := First_With_Clause_Of (Project);
      while Item /= Empty_Node loop
         --  ??? Should make sure we haven't converted that project yet.
         if Project_Node_Of (Item) /= Internal_Project then
            Recursive_Normalize
              (Project_Node_Of (Item), Mgr, Internal_Project);
         end if;
         Item := Next_With_Clause_Of (Item);
      end loop;
   end Recursive_Normalize;

   -----------------------
   -- Normalize_Project --
   -----------------------

   procedure Normalize_Project (Project : Project_Node_Id) is
      Mgr : Scenario_Manager;
      Vars : constant Project_Node_Array := Find_Scenario_Variables (Project);
      Internal_Project : Project_Node_Id;
   begin
      Put_Line ("Vars'Length= " & Vars'Length'Img);
      Initialize (Mgr, Vars);
      Internal_Project := Create_Project
        (Get_Name_String (Name_Of (Project)) & Internal_Project_Prefix, "");

      Recursive_Normalize (Project, Mgr, Internal_Project);

      Append_Declaration (Mgr, Internal_Project);


      Pretty_Print (Internal_Project);
   end Normalize_Project;

end Prj_Normalize;

