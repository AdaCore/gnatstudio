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
with Src_Info.Queries;          use Src_Info.Queries;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Language_Handlers;         use Language_Handlers;
with Docgen.Work_On_Source;     use Docgen.Work_On_Source;
with Language_Handlers.Glide;   use Language_Handlers.Glide;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Glide_Intl;                use Glide_Intl;
with Docgen.ALI_Utils;          use Docgen.ALI_Utils;
with String_Utils;              use String_Utils;

package body Docgen.Work_On_File is

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
      Handler            : in out Language_Handler;
      Project_Tree       : in out Project_Node_Id;
      Project_View       : in out Project_Id;
      Source_Info_List   : in out Src_Info.LI_File_List;
      Options            : All_Options)
   is
      Source_File_Node : Type_Source_File_List.List_Node;
      Doc_File         : File_Type;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;

      function Find_Next_Package
        (Package_Nr : Natural) return String;
      --  Returns the name of the next package in the list
      --  (body files with the same package name are ignored)
      --  If next package doesn't exist, a "" ist returned.

      function Find_Prev_Package
        (Package_Nr : Natural) return String;
      --  Returns the name of the previous package in the list
      --  (body files with the same package name are ignored)
      --  If next package doesn't exist, a "" ist returned.

      -----------------------
      -- Find_Next_Package --
      -----------------------

      function Find_Next_Package
        (Package_Nr : Natural) return String is
         Local_Node : Type_Source_File_List.List_Node;
         use TSFL;
      begin
         if Package_Nr = Type_Source_File_List.Length (Source_File_List) then
            return "";
         else
            Local_Node := Source_File_Node;
            Local_Node := Next (Source_File_Node);
            if not Is_Spec_File (Data (Local_Node).File_Name.all) then
               Local_Node := Next (Local_Node);
            end if;
            if Local_Node = Null_Node then
               --  don't change this return, needed in Docgen-Texi_Output
               --  in Write_Not_Regular_Beginning!
               return "";
            else
               return Data (Local_Node).Package_Name.all;
            end if;
         end if;
      end Find_Next_Package;

      -----------------------
      -- Find_Prev_Package --
      -----------------------

      function Find_Prev_Package (Package_Nr : Natural) return String is
         Local_Node : Type_Source_File_List.List_Node;
         use TSFL;
      begin
         if Package_Nr = 1
           or else (Package_Nr = 2
                    and then not Is_Spec_File
                      (TSFL.Data (Source_File_Node).File_Name.all))
         then
            return "";
         else
            Local_Node := Prev (Source_File_List, Source_File_Node);

            if Is_Spec_File (Data (Local_Node).File_Name.all)
              and then Options.Process_Body_Files
            then
               Local_Node := Prev (Source_File_List, Local_Node);
            end if;

            if Local_Node = Null_Node then
               --  Don't change this return, needed in Docgen-Texi_Output
               --  in Write_Not_Regular_Beginning!
               return "";

            else
               return Data (Local_Node).Package_Name.all;
            end if;
         end if;
      end Find_Prev_Package;

   begin
      --  Sort the list of the files first

      Sort_List_Name (Source_File_List);
      Source_File_Node := TSFL.First (Source_File_List);

      for J in 1 .. Type_Source_File_List.Length (Source_File_List) loop
         --  Create the doc file from the package name for each package

         Create (Doc_File,
                 Out_File,
                 Get_Doc_File_Name
                   (TSFL.Data (Source_File_Node).File_Name.all,
                    Options.Doc_Directory.all,
                    Options.Doc_Suffix.all));
         Put_Line (-"from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
         Put_Line (-"   create documentation to: " &
                   Get_Doc_File_Name
                     (TSFL.Data (Source_File_Node).File_Name.all,
                      Options.Doc_Directory.all,
                      Options.Doc_Suffix.all));

         --  Find the next and the previous package name (used for TexInfo)

         Next_Package := new String'(Find_Next_Package (J));
         Prev_Package := new String'(Find_Prev_Package (J));

         Process_One_File
           (Doc_File,
            TSFL.Data (Source_File_Node).File_Name.all,
            TSFL.Data (Source_File_Node).Package_Name.all,
            Next_Package,
            Prev_Package,
            Source_File_List,
            Source_Info_List,
            Handler,
            Project_Tree,
            Project_View,
            Options,
            Options.Process_Body_Files and then
              TSFL.Data (Source_File_Node).Other_File_Found);

         Source_File_Node := TSFL.Next (Source_File_Node);

         --  close the doc file
         Close (Doc_File);

         Free (Next_Package);
         Free (Prev_Package);
      end loop;

      --  sort the type index list and the subprogram index list first
      Sort_List_Name (Subprogram_Index_List);
      Sort_List_Name (Type_Index_List);

      --  create the index doc files for the packages
      Process_Unit_Index       (Source_File_List, Options);
      Process_Subprogram_Index (Subprogram_Index_List, Options);
      Process_Type_Index       (Type_Index_List, Options);

      TEL.Free (Subprogram_Index_List);
      TEL.Free (Type_Index_List);
   end Process_Files;

   ----------------------
   -- Process_One_File --
   ----------------------

   procedure Process_One_File
     (Doc_File           : File_Type;
      Source_Filename    : String;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Info_List   : in out Src_Info.LI_File_List;
      Handler            : in out Language_Handler;
      Project_Tree       : in out Project_Node_Id;
      Project_View       : in out Project_Id;
      Options            : All_Options;
      Process_Body_File  : Boolean)
   is
      LI_Unit     : LI_File_Ptr;
      Tree        : Scope_Tree;
      Entity_Iter : Entity_Declaration_Iterator;
      Info        : Entity_Information;

      function Get_Full_Entity_Filename (Filename : String) return String;
      --  Search the file in the list and if found, return it
      --  with its full.

      function Search_Line_In_Body (Info : Entity_Information) return Natural;
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

      function Give_Copy_Of_Element
        (Old_Element : Entity_List_Information)
         return Entity_List_Information;
      --  creates a copy of the Entity. This is needed, because entity
      --  contains pointers, and as the file entity list will be freed after
      --  each file has been processed and the index lists will remain until
      --  the last file.

      ------------------------------
      -- Get_Full_Entity_Filename --
      ------------------------------

      function Get_Full_Entity_Filename (Filename : String) return String is
         Source_File_Node : Type_Source_File_List.List_Node;
      begin
         Source_File_Node := TSFL.First (Source_File_List);

         for J in 1 .. TSFL.Length (Source_File_List) loop
            if File_Name (TSFL.Data (Source_File_Node).File_Name.all) =
              Filename
            then
               return TSFL.Data (Source_File_Node).File_Name.all;
            end if;

            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;

         --  exception later ???
         Put_Line (-("!!!Error: File not found in List, cannot return" &
                   " the name with the path!"));
         return "";
      end Get_Full_Entity_Filename;

      ------------------------------
      -- Add_Entity_To_Index_List --
      ------------------------------

      procedure Add_Entity_To_Index_List
        (Index_List  : in out Type_Entity_List.List;
         Entity_Node : Entity_List_Information)
      is
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
        (Info : Entity_Information) return Natural
      is
         Local_Location : File_Location;
         Local_Status   : Find_Decl_Or_Body_Query_Status;
      begin
         Find_Next_Body
           (LI_Unit,
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

            --  For example instantiations of generic entities don't
            --  need to have a declaration in the body
            --  => return No_Body_Line_Needed = constant 0

         else
            return No_Body_Line_Needed;
         end if;
      end Search_Line_In_Body;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Source_Filename : String;
         Entity_File     : String;
         Info            : Entity_Information)
      is
         Decl_Found           : Boolean;
         Reference_Iter       : Entity_Reference_Iterator;
         Reference_Scope_Tree : Scope_Tree;
         Local_Ref_List       : Type_Reference_List.List;
         Local_Calls_List     : Type_Reference_List.List;
         Entity_Tree_Node     : Scope_Tree_Node;

         procedure Add_Calls_References (Parent_Node : Scope_Tree_Node);
         --  creates the list with the subprograms called in the current
         --  subprogram and passes them to Local_Calls_List

         procedure Tree_Called_Callback
           (Node        : Scope_Tree_Node;
            Is_Renaming : Boolean);
         --  the callback function is used to find the names of
         --  the referenced subprograms and add each to the
         --  Local_Ref_List (the subprograms, where the current
         --  subprogram is called)

         procedure Remove_Double_Nodes (List : in out TRL.List);
         --  remove all double nodes from the list,
         --  only one node of each will be left

         ----------------------------
         --  Add_Calls_References  --
         ----------------------------

         procedure Add_Calls_References (Parent_Node : Scope_Tree_Node) is
            Child_Iterator : Scope_Tree_Node_Iterator;
            Child_Node     : Scope_Tree_Node;
            Reference_Node : Reference_List_Information;
         begin
            Child_Iterator := Start (Parent_Node);
            Child_Node := Get (Child_Iterator);

            while Child_Node /= Null_Scope_Tree_Node loop
               if Is_Subprogram (Child_Node)
                 and then Is_Spec_File
                   (Get_Declaration_File_Of (Get_Entity (Child_Node)))
               then
                  if Options.Info_Output then
                     Put_Line (-"Reference found: " &
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
                    (Options.Link_All
                     or else Source_File_In_List
                       (Source_File_List, Reference_Node.File_Name.all))
                     and then
                       (Get_Scope (Get_Entity (Child_Node)) = Global_Scope
                        or else Options.Show_Private);

                  Type_Reference_List.Append
                    (Local_Calls_List, Reference_Node);
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
            Is_Renaming : Boolean)
         is
            pragma Unreferenced (Is_Renaming);

            Local_Tree_Node : Scope_Tree_Node;
            Reference_Node  : Reference_List_Information;
         begin
            --  Get the name of the subprogram which calls the entity

            Local_Tree_Node := Get_Parent (Node);

            --  ??? A function is not necessarily called from a subprogram

            while Local_Tree_Node /= Null_Scope_Tree_Node
              and then not Is_Subprogram (Local_Tree_Node)
            loop
               Local_Tree_Node := Get_Parent (Local_Tree_Node);
            end loop;

            if Local_Tree_Node /= Null_Scope_Tree_Node then
               Reference_Node.File_Name :=
                 new String'(Get_File (Get_Location (Get_Reference (Node))));
               Reference_Node.Line :=
                 Get_Line   (Get_Location (Get_Reference (Node)));
               Reference_Node.Column :=
                 Get_Column (Get_Location (Get_Reference (Node)));
               Reference_Node.Set_Link := Decl_Found;
               Reference_Node.Subprogram_Name :=
                 new String'(Get_Name (Get_Entity (Local_Tree_Node)));

               --  Add reference to the local list

               Type_Reference_List.Append (Local_Ref_List, Reference_Node);
            end if;
         end Tree_Called_Callback;

         ---------------------------
         --  Remove_Double_Nodes  --
         ---------------------------

         procedure Remove_Double_Nodes (List : in out TRL.List) is
            Ref_Node_1, Ref_Node_2 : TRL.List_Node;
            use TRL;
         begin
            if not Is_Empty (List) then
               Sort_List_Name (List);

               Ref_Node_1 := First (List);

               while Ref_Node_1 /= Last (List) loop
                  Ref_Node_2 := Next (Ref_Node_1);

                  if Data (Ref_Node_1).Subprogram_Name.all =
                     Data (Ref_Node_2).Subprogram_Name.all
                    and then Data (Ref_Node_1).File_Name.all =
                     Data (Ref_Node_2).File_Name.all
                    and then Data (Ref_Node_1).Line =
                     Data (Ref_Node_2).Line
                  then
                     Remove_Nodes (List, Ref_Node_1, Ref_Node_2);
                  else
                     Ref_Node_1 := Ref_Node_2;
                  end if;
               end loop;
            end if;
         end Remove_Double_Nodes;

      begin   --  Process_Subprogram
         --  Only if the procedure is defined in this file AND
         --  the references are wished:

         if File_Name (Entity_Node.File_Name.all) = File_Name (Source_Filename)
           and then Options.References
         then
            Entity_Node.Line_In_Body := Search_Line_In_Body (Info);
            Find_All_References
              (Project_Tree,
               Handler,
               Info,
               Source_Info_List,
               Reference_Iter,
               Project_View,
               True);

            --  1. Find all subprograms called in the subprogram processed

            Entity_Tree_Node := Find_Entity_Scope (Tree, Info);

            if Entity_Tree_Node /= Null_Scope_Tree_Node then
               Add_Calls_References (Entity_Tree_Node);
            end if;

            --  2. Look for all references where this subprogram is called

            while Get (Reference_Iter) /= No_Reference loop
               --  Set the global variable: is the file known, where the
               --  declaration of the reference can be found?

               if Source_File_In_List
                 (Source_File_List,
                  Get_File (Get_Location (Get (Reference_Iter))))
               then
                  Decl_Found := True;
               else
                  Decl_Found := False;
               end if;

               --  For the rest use the scope tree

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

            --  Pass the local lists to the entity_node lists

            Entity_Node.Called_List := Local_Ref_List;
            Remove_Double_Nodes (Local_Calls_List);
            Entity_Node.Calls_List  := Local_Calls_List;
         end if;

         --  If defined in a spec file, add entity to the
         --  Subprogram_Index_List

         if Is_Spec_File (Source_Filename)
           and then Source_Filename = Entity_File
         then
            Add_Entity_To_Index_List
              (Subprogram_Index_List,
               Give_Copy_Of_Element (Entity_Node));
         end if;
      end Process_Subprogram;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type
        (Source_Filename : String;
         Entity_File     : String) is
      begin
         Entity_Node.Kind := Type_Entity;

         --  if defined in a spec file => add to the Type_Index_List
         if Is_Spec_File (Source_Filename)
           and then Source_Filename = Entity_File
         then
            Add_Entity_To_Index_List
              (Type_Index_List,
               Give_Copy_Of_Element (Entity_Node));
         end if;
      end Process_Type;

      --------------------------
      -- Give_Copy_Of_Element --
      --------------------------

      function Give_Copy_Of_Element
        (Old_Element : Entity_List_Information)
         return Entity_List_Information
      is
         New_Entity : Entity_List_Information;
      begin
         New_Entity.Kind := Old_Element.Kind;
         New_Entity.Name := new String '(Old_Element.Name.all);
         New_Entity.Short_Name := new String '(Old_Element.Short_Name.all);
         New_Entity.File_Name := new String '(Old_Element.File_Name.all);
         New_Entity.Column := Old_Element.Column;
         New_Entity.Line := Old_Element.Line;
         New_Entity.Is_Private := Old_Element.Is_Private;

         --  the rest is not needed for the index files!

         return New_Entity;
      end Give_Copy_Of_Element;

      ----------------------
      -- Process_One_File --
      ----------------------

   begin
      Load_LI_File
        (Source_Info_List, Handler, Project_View,
         File_Name (Source_Filename),
         LI_Unit);

      --  If body file, no need to start working on ALI, the lists of the
      --  spec file can be used.

      if not Is_Spec_File (Source_Filename) then
         Process_Source
           (Doc_File,
            Next_Package,
            Prev_Package,
            Source_File_List,
            Source_Filename,
            Package_Name,
            Entity_List,
            Process_Body_File,
            LI_Unit,
            Options);

         --  now free the list
         TEL.Free (Entity_List);

         --  but if a spec file, you have to look in the ALI files
      else
         if Options.Info_Output then
            Put_Line (-"Find all possible declarations");
         end if;

         --  Get all entities of the file

         if LI_Unit /= No_LI_File then
            Entity_Iter := Find_All_Possible_Declarations (LI_Unit, "");
            Tree := Create_Tree (LI_Unit);
         else
            Put_Line (-"LI file not found");  --  later Exception?
         end if;

         --  Get next entity from the file

         while not At_End (Entity_Iter) loop
            Info := Get (Entity_Iter);

            if Get_Scope (Info) = Global_Scope then
               Entity_Node.Is_Private := False;
            else
               Entity_Node.Is_Private := True;
            end if;

            --  Check if the declaration of the entity is in one of the files
            --  which are in list, if false => no need for creating links
            --  Also check if it's a private entity and whether they should be
            --  processed.

            if Source_File_In_List
              (Source_File_List, Get_Declaration_File_Of (Info))
              and then (Options.Show_Private
                        or else not Entity_Node.Is_Private)
            then
               --  Info_Output is set, if further information are wished

               if Options.Info_Output then
                  Put_Line ("-----");
                  Put_Line
                    ("Entity found: "
                     & Get_Full_Name (Info, LI_Unit, ".", Tree) &
                     " in " & Source_Filename &
                     " defined at " &
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)) &
                     ":" & Image (Get_Declaration_Line_Of (Info)) &
                     ":" & Image (Get_Declaration_Column_Of (Info)));
               end if;

               --  Get the parameters needed by all entities

               Entity_Node.Name            :=
                 new String'(Get_Full_Name (Info, LI_Unit, ".", Tree));
               Entity_Node.Short_Name      :=
                 new String'(Get_Name (Info));
               Entity_Node.File_Name       :=
                 new String'(Get_Full_Entity_Filename
                                    (Get_Declaration_File_Of (Info)));
               Entity_Node.Column          := Get_Declaration_Column_Of (Info);
               Entity_Node.Line            := Get_Declaration_Line_Of (Info);

               --  For all entities which are not subprograms the ref lists
               --  must be set to null

               if Get_Kind (Info).Kind /= Function_Or_Operator
                 and then Get_Kind (Info).Kind /= Procedure_Kind
               then
                  Entity_Node.Called_List := TRL.Null_List;
                  Entity_Node.Calls_List  := TRL.Null_List;
               end if;

               --  Get the entity specific parameters
               --  these are the last parameters to gather, after the CASE no
               --  more changes are allowed, because the index lists are
               --  created in the subprograms used here, so all info must
               --  be avaiable.

               case Get_Kind (Info).Kind is
                  when Procedure_Kind | Function_Or_Operator =>
                     Entity_Node.Kind := Subprogram_Entity;
                     Process_Subprogram
                       (Source_Filename,
                        Get_Full_Entity_Filename
                          (Get_Declaration_File_Of (Info)),
                        Info);
                  when Record_Kind | Enumeration_Kind |
                       Access_Kind | Array_Kind |
                       Boolean_Kind | String_Kind | Class_Wide |
                       Decimal_Fixed_Point |
                       Floating_Point | Modular_Integer |
                       Ordinary_Fixed_Point |
                       Private_Type | Protected_Kind |
                       Signed_Integer =>
                     if Get_Kind (Info).Is_Type then
                        Process_Type
                          (Source_Filename,
                           Get_Full_Entity_Filename
                             (Get_Declaration_File_Of (Info)));
                     else
                        Entity_Node.Kind := Var_Entity;
                     end if;

                  when Exception_Entity =>
                     Entity_Node.Kind := Exception_Entity;
                  when Task_Kind =>
                     Entity_Node.Kind := Entry_Entity;
                  when Package_Kind  =>
                     Entity_Node.Kind := Package_Entity;
                  when others =>
                     Entity_Node.Kind := Other_Entity;
               end case;

               --  Add to the entity list of this file

               Type_Entity_List.Append (Entity_List, Entity_Node);

            end if;

            --  Get next entity in this file

            Next (Entity_Iter);
         end loop;

         Destroy (Entity_Iter);
         Destroy (Info);
         Free (Tree);

         --  Process the documentation of this file

         Process_Source
           (Doc_File,
            Next_Package,
            Prev_Package,
            Source_File_List,
            Source_Filename,
            Package_Name,
            Entity_List,
            Process_Body_File,
            LI_Unit,
            Options);

         --  If body files are not being processed, free directly here

         if not Options.Process_Body_Files then
            TEL.Free (Entity_List);
         end if;
      end if;
   end Process_One_File;

end Docgen.Work_On_File;
