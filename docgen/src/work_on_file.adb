-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Src_Info.Queries;          use Src_Info.Queries;
with Test_Utils;                use Test_Utils;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Language_Handlers;         use Language_Handlers;
with Test_Utils;                use Test_Utils;
with Work_On_Source;            use Work_On_Source;
with Doc_Types;                 use Doc_Types;
with Language_Handlers.Glide;   use Language_Handlers.Glide;

package body Work_On_File is

   package TSFL renames Type_Source_File_List;
   package TEL  renames Type_Entity_List;
   package TRL  renames Type_Reference_List;

   Entity_List           : Type_Entity_List.List;
   Entity_Node           : Entity_List_Information;
   Subprogram_Index_List : Type_Entity_List.List;
   Type_Index_List       : Type_Entity_List.List;

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files
     (Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options)
   is
      Source_File_Node : Type_Source_File_List.List_Node;
      Source_Info_List : Src_Info.LI_File_List;
      Handler          : Language_Handler;
      Project_Tree     : Project_Node_Id;
      Project_View     : Project_Id;
      Doc_File         : File_Type;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
   begin

      --  sort the list of the files first
      Sort_List_Name (Source_File_List);

      Source_File_Node := TSFL.First (Source_File_List);

      --  get the handler
      Handler := Create_Lang_Handler;

      --  get Project_Tree and Project_View
      Reset (Source_Info_List);
      Load_Project (TSFL.Data (Source_File_Node).Prj_File_Name.all,
                       Handler, Project_Tree, Project_View);

      for J in 1 .. Type_Source_File_List.Length (Source_File_List) loop

         --  create the doc file from the package name for each(!) package
         if not Options.Doc_One_File then
            Create (Doc_File,
                    Out_File,
                    Get_Doc_File_Name (TSFL.Data
                                         (Source_File_Node).File_Name.all,
                                       Options.Doc_Directory.all,
                                       Options.Doc_Suffix.all));
            Put_Line ("from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
            Put_Line ("   create documentation to: " &
                           Get_Doc_File_Name (TSFL.Data
                                            (Source_File_Node).File_Name.all,
                                          Options.Doc_Directory.all,
                                          Options.Doc_Suffix.all));
            --  create the doc file from the project file name for
            --  all(!) packages
         elsif J = 1 and Options.Doc_One_File then
            Create (Doc_File,
                       Out_File,
                    Get_Doc_File_Name
                      (TSFL.Data
                         (Source_File_Node).Prj_File_Name.all,
                       Options.Doc_Directory.all,
                       Options.Doc_Suffix.all));
            Put_Line ("create documentation to: " &
                        Get_Doc_File_Name
                          (TSFL.Data
                               (Source_File_Node).Prj_File_Name.all,
                           Options.Doc_Directory.all,
                           Options.Doc_Suffix.all));
            Put_Line ("   from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
         else
            Put_Line ("   from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
         end if;

         --  find the next and the previous package name (used for TexInfo)
         if J = 1 then
            Prev_Package := new String '(" ");
         else
            Source_File_Node := TSFL.Prev (Source_File_List,
                                              Source_File_Node);
            Prev_Package :=
              new String '(TSFL.Data (Source_File_Node).Package_Name.all);
            Source_File_Node := TSFL.Next (Source_File_Node);
         end if;
         if J = Type_Source_File_List.Length (Source_File_List) then
            Next_Package := new String '(" ");
         else
            Source_File_Node := TSFL.Next (Source_File_Node);
            Next_Package :=
              new String '(TSFL.Data (Source_File_Node).Package_Name.all);
            Source_File_Node := TSFL.Prev (Source_File_List,
                                              Source_File_Node);
         end if;

         Process_One_File (Doc_File,
                           J = 1,
                           J = Type_Source_File_List.Length
                             (Source_File_List),
                           TSFL.Data (Source_File_Node).File_Name.all,
                           TSFL.Data (Source_File_Node).Package_Name.all,
                           Next_Package,
                           Prev_Package,
                           TSFL.Data (Source_File_Node).Def_In_Line,
                           Source_File_List,
                           Source_Info_List, Handler,
                           Project_Tree,
                           Project_View,
                           Options,
                           Options.Process_Body_Files and
                                TSFL.Data (Source_File_Node).Other_File_Found);

         Source_File_Node := TSFL.Next (Source_File_Node);

         --  close the doc file
         if J = Type_Source_File_List.Length (Source_File_List) or
         not Options.Doc_One_File then
            Close (Doc_File);
         end if;

         Free (Next_Package);
         Free (Prev_Package);
      end loop;

      if not Options.Doc_One_File then
         --  sort the type index list and the subprogram index list first
         Sort_List_Name (Subprogram_Index_List);
         Sort_List_Name (Type_Index_List);

         --  create the index doc files for the packages
         Process_Unit_Index       (Source_File_List, Options);
         Process_Subprogram_Index (Subprogram_Index_List, Options);
         Process_Type_Index       (Type_Index_List, Options);
      end if;
   end Process_Files;

   ----------------------
   -- Process_One_File --
   ----------------------

   procedure Process_One_File
     (Doc_File           : File_Type;
      First_File         : Boolean;
      Last_File          : Boolean;
      Source_Filename    : String;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Def_In_Line        : Integer;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Info_List   : in out Src_Info.LI_File_List;
      Handler            : in out Language_Handler;
      Project_Tree       : in out Project_Node_Id;
      Project_View       : in out Project_Id;
      Options            : All_Options;
      Process_Body_File  : Boolean)
   is
      LI_Unit                   : LI_File_Ptr;
      Entity_Iter               : Entity_Declaration_Iterator;
      Info                      : Entity_Information;

      function Source_File_In_List
        (Name : String) return Boolean;
      --  returns true if the file is found in the source file list

      function Is_Defined_In_Subprogram
        (Entity          : String;
         Short_Entity    : String;
         Package_Name    : String) return Boolean;
      --  returns true if the entity is defined within another entity and
      --  not dierctly within the package. The function only parses the
      --  full entity name to find it out, but nothing more is needed
      --  (especially with the additional Scope info it's enough).

      function Get_Full_Entity_Filename
        (Filename         : String) return String;
      --  tries to find the file in the list and if found, it returns it
      --  with all his path in front of the name

      function Search_Line_In_Body
        (Info : Entity_Information) return Natural;
         --  tries to find out the beginning of the subprogram in the
         --  body file. Returns the line number.

      procedure Process_Subprogram
        (Source_Filename : String;
         Entity_File     : String;
         Info            : Entity_Information);
      --  fills all the entity_node information with the information still
      --  needed AND adds them to the index list (so all other information
      --  must be already provided!)

      procedure Process_Type
        (Source_Filename : String;
         Entity_File     : String);
      --  fills all the entity_node information with the information still
      --  needed AND adds them to the index list (so all other information
      --  must be already provided!)

      procedure Add_Entity_To_Index_List
        (Index_List  : in out Type_Entity_List.List;
         Entity_Node : Entity_List_Information);
      --  creates a copy of the given Entity_Node and addes it to the index
      --  list. The Elements in the index lists must be a copy of the original
      --  entities, because the normal file lists will be freed while these
      --  elements should remain after all files were processed.

      -------------------------
      -- Source_File_In_List --
      -------------------------

      function Source_File_In_List
        (Name : String) return Boolean
      is
         Source_File_Node : Type_Source_File_List.List_Node;
         Found            : Boolean;
      begin
         Found := False;
         Source_File_Node := TSFL.First (Source_File_List);
         for J in 1 .. TSFL.Length (Source_File_List) loop
            if File_Name  (TSFL.Data (Source_File_Node).File_Name.all)
              = (Name) then
               Found := True;
            end if;
            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
         return Found;
      end Source_File_In_List;

      ------------------------------
      -- Is_Defined_In_Subprogram --
      ------------------------------

      function Is_Defined_In_Subprogram
        (Entity          : String;
         Short_Entity    : String;
         Package_Name    : String) return Boolean is
      begin
         --  check if the short name of the entity starts right
         --  after the package name followed by "."
         if not (Get_String_Index (Entity, 1, To_Lower (Package_Name)) +
                   Package_Name'Length + 1
           < Get_String_Index (Entity, 1, Short_Entity)) and
         --  and that it is really the name at the end of the
         --  entity name, followed by nothing
           Entity'Last = (Get_String_Index (Entity, 1, Short_Entity)) +
           Short_Entity'Last - 1
         then
            return False;
         else
            return True;
         end if;
      end Is_Defined_In_Subprogram;

      ------------------------------
      -- Get_Full_Entity_Filename --
      ------------------------------

      function Get_Full_Entity_Filename
        (Filename         : String) return String
      is
         Source_File_Node : Type_Source_File_List.List_Node;
      begin
         Source_File_Node := TSFL.First (Source_File_List);
         for J in 1 .. TSFL.Length (Source_File_List) loop
            if File_Name (TSFL.Data (Source_File_Node).File_Name.all) =
              Filename then
               return TSFL.Data (Source_File_Node).File_Name.all;
            end if;
            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
         --  exception later!?
         Put_Line ("!!!Error: File not found in List, cannot return" &
                   " the name with the path!");
         return "";
      end Get_Full_Entity_Filename;

      ------------------------------
      -- Add_Entity_To_Index_List --
      ------------------------------

      procedure Add_Entity_To_Index_List
        (Index_List  : in out Type_Entity_List.List;
         Entity_Node : Entity_List_Information) is

         Local_Node : Entity_List_Information;
      begin
         Local_Node.Kind := Entity_Node.Kind;
         Local_Node.Name := new String'(Entity_Node.Name.all);
         Local_Node.Short_Name := new String'(Entity_Node.Short_Name.all);
         Local_Node.File_Name := new String'(Entity_Node.File_Name.all);
         Local_Node.Column := Entity_Node.Column;
         Local_Node.Line := Entity_Node.Line;
         Local_Node.Is_Private := Entity_Node.Is_Private;

         Type_Entity_List.Append (Index_List, Local_Node);
      end Add_Entity_To_Index_List;

      ---------------------------
      --  Search_Line_In_Body  --
      ---------------------------

      function Search_Line_In_Body
        (Info : Entity_Information) return Natural is
         Local_Location       : File_Location;
         Local_Status         : Find_Decl_Or_Body_Query_Status;
      begin
         Find_Next_Body (LI_Unit,
                         Get_Declaration_File_Of (Info),
                         Get_Name (Info),
                         Get_Declaration_Line_Of (Info),
                         Get_Declaration_Column_Of (Info),
                         Get_LI_Handler_From_File
                           (Glide_Language_Handler (Handler),
                            Source_Filename,
                            Project_View),
                         Source_Info_List,
                         Project_View,
                         "",
                         "",
                         Local_Location,
                         Local_Status);
         if Local_Status = Success then
            return Get_Line (Local_Location);
            --  for example instantanations of generic entities don't
            --  need to have a declaration in the body => return 0
         else
            return 0;
         end if;
      end  Search_Line_In_Body;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Source_Filename : String;
         Entity_File     : String;
         Info            : Entity_Information) is

         Decl_Found                : Boolean;
         Reference_Iter            : Entity_Reference_Iterator;
         Reference_Scope_Tree      : Scope_Tree;
         Local_Ref_List            : Type_Reference_List.List;
         Local_Calls_List          : Type_Reference_List.List;
         Entity_Tree_Node          : Scope_Tree_Node;

         procedure Add_Calls_References
           (Parent_Node : Scope_Tree_Node);
         --  creates the list with the subprograms called in the current
         --  subprogram and passes them to Local_Calls_List

         procedure Tree_Called_Callback
           (Node        : Scope_Tree_Node;
            Is_Renaming : Boolean);
         --  the callback function is used to find the names of
         --  the referenced subprograms and add each to the
         --  Local_Ref_List (the subprograms, where the current
         --  subprogram is called)

         procedure Remove_Double_Nodes
           (List : in out TRL.List);
         --  remove all double nodes from the list,
         --  only one node of each will be left

         ----------------------------
         --  Add_Calls_References  --
         ----------------------------

         procedure Add_Calls_References
           (Parent_Node : Scope_Tree_Node) is

            Child_Iterator : Scope_Tree_Node_Iterator;
            Child_Node     : Scope_Tree_Node;
            Reference_Node : Reference_List_Information;
         begin
            Child_Iterator := Start (Parent_Node);
            Child_Node := Get (Child_Iterator);
            while Child_Node /= Null_Scope_Tree_Node loop

               --  Put_Line (Get_Name (Get_Entity (Child_Node))); for testing!

               if Is_Subprogram (Child_Node) and
                 File_Extension
                   (Get_Declaration_File_Of (Get_Entity (Child_Node)))
                    = ".ads" then
                  if Options.Info_Output then
                     Put_Line ("Reference found: " &
                               Get_Name (Get_Entity (Child_Node)));
                  end if;
                  Reference_Node.File_Name := new String'
                    (Get_Declaration_File_Of (Get_Entity (Child_Node)));
                  Reference_Node.Line :=
                    (Get_Declaration_Line_Of (Get_Entity (Child_Node)));
                  Reference_Node.Column :=
                    (Get_Declaration_Column_Of (Get_Entity (Child_Node)));
                  Reference_Node.Subprogram_Name := new String'
                    (Get_Name (Get_Entity (Child_Node)));
                  Reference_Node.Set_Link :=
                    Source_File_In_List (Reference_Node.File_Name.all) and
                    (Get_Scope (Get_Entity (Child_Node)) = Global_Scope or
                     Options.Show_Private);

                  Type_Reference_List.Append (Local_Calls_List,
                                              Reference_Node);

               end if;
               Next (Child_Iterator);
               Child_Node := Get (Child_Iterator);
            end loop;
         end Add_Calls_References;

         ----------------------------
         --  Tree_Called_Callback  --
         ----------------------------

         procedure Tree_Called_Callback
           (Node        : Scope_Tree_Node;
            Is_Renaming : Boolean) is

            Local_Tree_Node : Scope_Tree_Node;
            Reference_Node  : Reference_List_Information;
         begin
            if Is_Renaming then
               Put_Line ("Is_Renaming: ");    --  just for testing later!
            end if;

            --  get the name of the subprogram which calls the entity
            Local_Tree_Node := Get_Parent (Node);
            while not Is_Subprogram (Local_Tree_Node) loop
               Local_Tree_Node := Get_Parent (Local_Tree_Node);
            end loop;

            Reference_Node.File_Name  :=
              new String'(Get_File
                            (Get_Location (Get_Reference (Node))));
            Reference_Node.Line       :=
              Get_Line   (Get_Location (Get_Reference (Node)));
            Reference_Node.Column     :=
              Get_Column (Get_Location (Get_Reference (Node)));
            Reference_Node.Set_Link := Decl_Found;
            Reference_Node.Subprogram_Name :=
              new String'(Get_Name (Get_Entity (Local_Tree_Node)));

            --  add reference to the local list
            Type_Reference_List.Append (Local_Ref_List,
                                        Reference_Node);
         end Tree_Called_Callback;

         ---------------------------
         --  Remove_Double_Nodes  --
         ---------------------------

         procedure Remove_Double_Nodes
           (List : in out TRL.List) is
            Ref_Node_1, Ref_Node_2 : TRL.List_Node;
            use TRL;
         begin
            if not Is_Empty (List) then
               Sort_List_Name (List);

               Ref_Node_1 := First (List);
               while Ref_Node_1 /= Last (List) loop
                  Ref_Node_2 := Next (Ref_Node_1);
                  if Data (Ref_Node_1).Subprogram_Name.all =
                    Data (Ref_Node_2).Subprogram_Name.all and
                    Data (Ref_Node_1).File_Name.all =
                    Data (Ref_Node_2).File_Name.all and
                    Data (Ref_Node_1).Line =
                    Data (Ref_Node_2).Line then
                     Remove_Nodes (List,
                                   Ref_Node_1,
                                   Ref_Node_2);
                  else
                     Ref_Node_1 := Ref_Node_2;
                  end if;
               end loop;
            end if;
         end Remove_Double_Nodes;

      begin   --  Process_Subprogram

         --  only if the procedure is defined in this file AND
         --  the references are wished:
         if File_Name (Entity_Node.File_Name.all) =
           File_Name (Source_Filename)
         and Options.References then
               Entity_Node.Line_In_Body := Search_Line_In_Body (Info);

               Find_All_References (Project_Tree,
                                 Handler,
                                 Info,
                                 Source_Info_List,
                                 Reference_Iter,
                                 Project_View,
                                 True);

            --  1. find all subprograms called in the subprogram processed
            Reference_Scope_Tree :=
            Create_Tree (LI_Unit);
            Entity_Tree_Node :=
              Find_Entity_Scope (Reference_Scope_Tree, Info);
            if Entity_Tree_Node /= Null_Scope_Tree_Node then
               Add_Calls_References (Entity_Tree_Node);
            end if;
            Free (Reference_Scope_Tree);

            --  2. look for all references where this subprogram is called
            while Get (Reference_Iter) /= No_Reference loop

               --  set the global variable: is the file known, where the
               --  declaration of the reference can be found?
               if Source_File_In_List
                 (Get_File (Get_Location (Get (Reference_Iter)))) then
                  Decl_Found := True;
               else
                  Decl_Found := False;
               end if;

               --  for the rest use the scope tree
               Reference_Scope_Tree :=
                 Create_Tree (Get_LI (Reference_Iter));
               Find_Entity_References
                 (Reference_Scope_Tree,
                  Info,
                  Tree_Called_Callback'Unrestricted_Access);
               Entity_Node.Line_In_Body := Search_Line_In_Body (Info);
               Free (Reference_Scope_Tree);

               Next (Handler, Reference_Iter, Source_Info_List);
            end loop;

            --  pass the local lists to the entity_node lists
            Entity_Node.Called_List := Local_Ref_List;
            Remove_Double_Nodes (Local_Calls_List);
            Entity_Node.Calls_List  := Local_Calls_List;
         end if;

         --  if defined in a .ads file, add entity to the
         --  Subprogram_Index_List
         if  File_Extension (File_Name (Source_Filename)) = ".ads"
           and Source_Filename = Entity_File then
            Add_Entity_To_Index_List (Subprogram_Index_List, Entity_Node);
         end if;

      end Process_Subprogram;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type
        (Source_Filename : String;
         Entity_File     : String) is
      begin
         Entity_Node.Kind   := Type_Entity;

         --  if defined in a .ads file => add to the Type_Index_List
         if File_Extension (File_Name (Source_Filename)) = ".ads"
           and Source_Filename = Entity_File then
            Add_Entity_To_Index_List (Type_Index_List, Entity_Node);
         end if;

      end Process_Type;

      ----------------------
      -- Process_One_File --
      ----------------------

   begin
      Load_LI_File
        (Source_Info_List, Handler, Project_View,
         File_Name (Source_Filename),
         LI_Unit);

      if Options.Info_Output then
         Put_Line ("Find all possible declarations");
      end if;

      --  get all entities of the file
      if LI_Unit /= No_LI_File then
         Entity_Iter := Find_All_Possible_Declarations (LI_Unit, "");
      else
         Put_Line ("LI file not found");  --  later Exception?
      end if;

      --  get next entity from the file
      while not At_End (Entity_Iter) loop
         Info := Get (Entity_Iter);

         if Get_Scope (Info) = Global_Scope then
            Entity_Node.Is_Private := False;
         else
            Entity_Node.Is_Private := True;
         end if;

         --  check if the entity in defined within a subprogram.
         --  if true => ignore!
         if not Is_Defined_In_Subprogram (Get_Full_Name (Info, LI_Unit, "."),
                                          Get_Name (Info),
                                          Package_Name)
         --  AND check if the declaration of the entity is in one of the files
         --  which are in list, if false => no need for creating links
         --  => ignore!
           and Source_File_In_List (Get_Declaration_File_Of (Info))
         --  AND check if it's a private entity and if they should be processed
           and (Options.Show_Private or not Entity_Node.Is_Private)
         then

            --  Info_Output is set, if further information are wished
            if Options.Info_Output then
               Put_Line ("-----");
               Put_Line ("Entity found: "
                         & Get_Full_Name (Info, LI_Unit, ".") &
                         "  in File:  " & Source_Filename &
                         "  defined in  file: " &
                         Get_Full_Entity_Filename
                           (Get_Declaration_File_Of (Info)) &
                         ", in line: " &
                         Get_Declaration_Line_Of (Info)'Img &
                         ", in column: " &
                         Get_Declaration_Column_Of (Info)'Img);
            end if;

            --  get the parameters needed be all entities
            Entity_Node.Name            :=
              new String'(Get_Full_Name (Info, LI_Unit, "."));
            Entity_Node.Short_Name      :=
              new String'(Get_Name (Info));
            Entity_Node.File_Name       :=
              new String'(Get_Full_Entity_Filename
                            (Get_Declaration_File_Of (Info)));
            Entity_Node.Column          := Get_Declaration_Column_Of (Info);
            Entity_Node.Line            := Get_Declaration_Line_Of (Info);


            --  get the entity specific parameters
            --  these are the last parameters to gather, after the CASE no
            --  more changes are allowed, because the index lists are created
            --  in the subprograms used here, so all info must be avaiable.
            case Get_Kind (Info) is
               when Non_Generic_Function_Or_Operator |
                  Generic_Function_Or_Operator =>
                  Entity_Node.Kind := Function_Entity;
                  Process_Subprogram
                    (Source_Filename,
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)),
                     Info);
               when Non_Generic_Procedure |
                  Generic_Procedure =>
                  Entity_Node.Kind := Procedure_Entity;
                  Process_Subprogram
                    (Source_Filename,
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)),
                     Info);
               when Record_Type | Enumeration_Type |
                  Access_Type | Array_Type |
                  Boolean_Type | String_Type | Class_Wide_Type |
                  Decimal_Fixed_Point_Type |
                  Floating_Point_Type | Modular_Integer_Type |
                  Ordinary_Fixed_Point_Type |
                  Private_Type | Protected_Type |
                  Signed_Integer_Type | Task_Type
                  => Process_Type
                    (Source_Filename,
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)));
               when Exception_Entity =>
                  Entity_Node.Kind := Exception_Entity;
               when Array_Object | Boolean_Object | Enumeration_Object |
                  Floating_Point_Object |
                  Modular_Integer_Object |  Protected_Object |
                  Record_Object | Signed_Integer_Object |
                  String_Object | Task_Object |
                  Access_Object | Class_Wide_Object |
                  Ordinary_Fixed_Point_Object =>
                  Entity_Node.Kind := Var_Entity;
               when Generic_Package  =>
                  Entity_Node.Kind := Package_Entity;
               when  Non_Generic_Package =>
                  Entity_Node.Kind := Package_Entity;
               when others => Entity_Node.Kind := Other_Entity;
            end case;

            --  add to the entity list of this file
            Type_Entity_List.Append (Entity_List, Entity_Node);

         end if;
         --  get next entity in this file
         Next (Entity_Iter);
      end loop;

      Destroy (Entity_Iter);
      Destroy (Info);
      --  process the documentation of this file
      Process_Source (Doc_File,
                      First_File,
                      Last_File,
                      Next_Package,
                      Prev_Package,
                      Source_File_List,
                      Source_Filename,
                      Package_Name,
                      Def_In_Line,
                      Entity_List,
                      Process_Body_File,
                      Options);

   end Process_One_File;

end Work_On_File;
