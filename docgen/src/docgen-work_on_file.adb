-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Src_Info.Queries;          use Src_Info.Queries;
with Projects;                  use Projects;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Docgen.Work_On_Source;     use Docgen.Work_On_Source;
with Traces;                    use Traces;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Project;      use Glide_Kernel.Project;
with VFS;                       use VFS;
with Projects.Registry;         use Projects.Registry;

package body Docgen.Work_On_File is

   Me : constant Debug_Handle := Create ("Docgen");

   package TSFL renames Type_Source_File_List;
   package TEL  renames Type_Entity_List;
   package TRL  renames Type_Reference_List;

   procedure Process_One_File
     (B                  : Backend_Handle;
      Doc_File           : File_Type;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename    : Virtual_File;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Process_Body_File  : Boolean;
      Subprogram_Index_List : in out Type_Entity_List.List;
      Type_Index_List       : in out Type_Entity_List.List;
      Converter          : Docgen.Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String);
   --  called by Process_Files for each file from the given list
   --  will examine that file and call the function Work_On_Source
   --  from Docgen.Work_On_File.

   --  In the procedure Process_Files each file from the list will be passed
   --  to the procedure Process_One_File, while collecting information about
   --  types and subprograms of all spec files, to be able to create index
   --  lists of these entities by calling the procedures Process_Type_Index,
   --  Process_Subprogram_Index and Process_Unit_Index (the latter for the
   --  source file list) in the package Docgen.Work_On_File.

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files
     (B                : Backend_Handle;
      Source_File_List : in out Docgen.Type_Source_File_List.List;
      Kernel           : access Glide_Kernel.Kernel_Handle_Record'Class;
      Options          : Docgen.All_Options;
      Doc_Suffix       : String;
      Converter        : Docgen.Doc_Subprogram_Type)
   is
      Source_File_Node      : Type_Source_File_List.List_Node;
      Doc_File              : File_Type;
      Next_Package          : GNAT.OS_Lib.String_Access;
      Prev_Package          : GNAT.OS_Lib.String_Access;
      Subprogram_Index_List : Type_Entity_List.List;
      Type_Index_List       : Type_Entity_List.List;

      Doc_Directory_Root : constant String :=
           Get_Doc_Directory (B, Kernel_Handle (Kernel));

      function Find_Next_Package
        (Package_Nr : Natural) return String;
      --  Returns the name of the next package in the list
      --  (body files with the same package name are ignored)
      --  If next package doesn't exist, "" is returned.

      function Find_Prev_Package
        (Package_Nr : Natural) return String;
      --  Returns the name of the previous package in the list
      --  (body files with the same package name are ignored)
      --  If next package doesn't exist, "" is returned.

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
            if not Is_Spec_File (Kernel, Data (Local_Node).File_Name) then
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
                      (Kernel, TSFL.Data (Source_File_Node).File_Name))
         then
            return "";
         else
            Local_Node := Prev (Source_File_List, Source_File_Node);

            if Is_Spec_File (Kernel, Data (Local_Node).File_Name)
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

      for J in 1 .. TSFL.Length (Source_File_List) loop
         declare
            File_Name : constant String := Get_Doc_File_Name
              (TSFL.Data (Source_File_Node).File_Name,
               Doc_Directory_Root, Doc_Suffix);
         begin
            Trace (Me, "From file: " &
                   Full_Name (TSFL.Data (Source_File_Node).File_Name).all
                   & " create documentation to: " & File_Name);
            Create (Doc_File, Out_File, File_Name);

            --  Find the next and the previous package name (used for TexInfo)

            Next_Package := new String'(Find_Next_Package (J));
            Prev_Package := new String'(Find_Prev_Package (J));

            Process_One_File
              (B,
               Doc_File,
               Kernel,
               TSFL.Data (Source_File_Node).File_Name,
               TSFL.Data (Source_File_Node).Package_Name.all,
               Next_Package,
               Prev_Package,
               Source_File_List,
               Options,
               Options.Process_Body_Files and then
               TSFL.Data (Source_File_Node).Other_File_Found,
               Subprogram_Index_List,
               Type_Index_List,
               Converter,
               Doc_Directory_Root,
               Doc_Suffix);

            Source_File_Node := TSFL.Next (Source_File_Node);

            Close (Doc_File);

            Free (Next_Package);
            Free (Prev_Package);
         end;
      end loop;

      --  sort the type index list and the subprogram index list first
      Sort_List_Name (Subprogram_Index_List);
      Sort_List_Name (Type_Index_List);

      --  create the index doc files for the packages
         Process_Unit_Index
          (B, Kernel, Source_File_List, Options, Converter,
           Doc_Directory_Root, Doc_Suffix);
         Process_Subprogram_Index
          (B, Kernel, Subprogram_Index_List, Options, Converter,
           Doc_Directory_Root, Doc_Suffix);
         Process_Type_Index
          (B, Kernel, Type_Index_List, Options, Converter,
          Doc_Directory_Root, Doc_Suffix);

      TEL.Free (Subprogram_Index_List);
      TEL.Free (Type_Index_List);
   end Process_Files;

   ----------------------
   -- Process_One_File --
   ----------------------

   procedure Process_One_File
     (B                  : Backend_Handle;
      Doc_File           : File_Type;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename    : Virtual_File;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options;
      Process_Body_File  : Boolean;
      Subprogram_Index_List : in out Type_Entity_List.List;
      Type_Index_List       : in out Type_Entity_List.List;
      Converter          : Docgen.Doc_Subprogram_Type;
      Doc_Directory      : String;
      Doc_Suffix         : String)
   is
      LI_Unit     : LI_File_Ptr;
      Tree        : Scope_Tree;
      Entity_Iter : Entity_Declaration_Iterator;
      Info        : Entity_Information;
      Entity_Node : Entity_List_Information;
      Entity_List : Type_Entity_List.List;

      procedure Process_Subprogram
        (Source_Filename : Virtual_File;
         Entity_File     : Virtual_File;
         Info            : Entity_Information);
      --  Fills all the entity_node information with the information still
      --  needed AND adds them to the index list (so all other information
      --  must be already provided!)

      procedure Process_Type
        (Source_Filename : Virtual_File;
         Entity_File     : Virtual_File);
      --  Fills all the entity_node information with the information still
      --  needed AND adds them to the index list (so all other information
      --  must be already provided!)

      procedure Add_Calls_References
        (Calls_List  : in out Type_Reference_List.List;
         Parent_Node : Scope_Tree_Node);
      --  Append to Calls_List the list of subprograms called by Parent_Node.

      --------------------------
      -- Add_Calls_References --
      --------------------------

      procedure Add_Calls_References
        (Calls_List  : in out Type_Reference_List.List;
         Parent_Node : Scope_Tree_Node)
      is
         Child_Iterator : Scope_Tree_Node_Iterator;
         Child_Node     : Scope_Tree_Node;
         Entity         : Entity_Information;
      begin
         Child_Iterator := Start (Parent_Node);

         loop
            Child_Node := Get (Child_Iterator);
            exit when Child_Node = Null_Scope_Tree_Node;

            Entity := Get_Entity (Child_Node);

            if Is_Subprogram (Child_Node)
              and then Is_Spec_File (Kernel, Get_Declaration_File_Of (Entity))
            then
               Trace (Me, "Reference found: " & Get_Name (Entity));
               Type_Reference_List.Append
                 (Calls_List,
                  (Entity   => Entity,
                   Set_Link =>
                     (Options.Link_All
                      or else Source_File_In_List
                        (Source_File_List, Get_Declaration_File_Of (Entity)))
                   and then
                     (Get_Scope (Entity) = Global_Scope
                      or else Options.Show_Private)));
            else
               Destroy (Entity);
            end if;

            Next (Child_Iterator);
         end loop;
      end Add_Calls_References;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Source_Filename : Virtual_File;
         Entity_File     : Virtual_File;
         Info            : Entity_Information)
      is
         Decl_Found           : Boolean;
         Reference_Iter       : Entity_Reference_Iterator;
         Reference_Scope_Tree : Scope_Tree;
         Local_Ref_List       : Type_Reference_List.List;
         Local_Calls_List     : Type_Reference_List.List;
         Entity_Tree_Node     : Scope_Tree_Node;

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
         --  Tree_Called_Callback  --
         ----------------------------

         procedure Tree_Called_Callback
           (Node        : Scope_Tree_Node;
            Is_Renaming : Boolean)
         is
            pragma Unreferenced (Is_Renaming);
            Local_Tree_Node : Scope_Tree_Node := Get_Parent (Node);
         begin
            --  Get the name of the subprogram which calls the entity

            while Local_Tree_Node /= Null_Scope_Tree_Node
              and then not Is_Subprogram (Local_Tree_Node)
            loop
               Local_Tree_Node := Get_Parent (Local_Tree_Node);
            end loop;

            if Local_Tree_Node /= Null_Scope_Tree_Node then
               Type_Reference_List.Append
                 (Local_Ref_List,
                  (Entity   => Get_Entity (Local_Tree_Node),
                   Set_Link => Decl_Found));
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

                  if Is_Equal
                    (Data (Ref_Node_1).Entity, Data (Ref_Node_2).Entity)
                  then
                     Remove_Nodes (List, Ref_Node_1, Ref_Node_2);
                  else
                     Ref_Node_1 := Ref_Node_2;
                  end if;
               end loop;
            end if;
         end Remove_Double_Nodes;

         Status : Find_Decl_Or_Body_Query_Status;
      begin
         --  Only if the procedure is defined in this file AND
         --  the references are wished:

         if Get_Declaration_File_Of (Entity_Node.Entity) = Source_Filename
           and then Options.References
         then
            Find_Next_Body
              (Kernel, LI_Unit, Info, Entity_Node.Line_In_Body, Status);
            if Status /= Success then
               Entity_Node.Line_In_Body := Null_File_Location;
            end if;

            Find_All_References
                (Get_Root_Project (Get_Registry (Kernel)),
                 Get_Language_Handler (Kernel),
                 Info,
                 Get_LI_File_List (Kernel),
                 Reference_Iter,
                 No_Project,
                 True);

            --  1. Find all subprograms called in the subprogram processed

            Entity_Tree_Node := Find_Entity_Scope (Tree, Info);

            if Entity_Tree_Node /= Null_Scope_Tree_Node then
               Add_Calls_References (Local_Calls_List, Entity_Tree_Node);
            end if;

            --  2. Look for all references where this subprogram is called

            while Get (Reference_Iter) /= No_Reference loop
               --  Set the global variable: is the file known, where the
               --  declaration of the reference can be found?

               Decl_Found := Source_File_In_List
                 (Source_File_List,
                  Get_File (Get_Location
                              (Get (Reference_Iter))));

               --  For the rest use the scope tree

               Reference_Scope_Tree :=
                 Create_Tree (Get_LI (Reference_Iter));
               Find_Entity_References
                 (Reference_Scope_Tree,
                  Info,
                  Tree_Called_Callback'Unrestricted_Access);

               Find_Next_Body
                 (Kernel, LI_Unit, Info, Entity_Node.Line_In_Body, Status);
               if Status /= Success then
                  Entity_Node.Line_In_Body := Null_File_Location;
               end if;

               Free (Reference_Scope_Tree);
               Next (Get_Language_Handler (Kernel),
                     Reference_Iter,
                     Get_LI_File_List (Kernel));
            end loop;

            --  Pass the local lists to the entity_node lists

            Entity_Node.Called_List := Local_Ref_List;
            Remove_Double_Nodes (Local_Calls_List);
            Entity_Node.Calls_List  := Local_Calls_List;
         end if;

         --  If defined in a spec file, add entity to the
         --  Subprogram_Index_List

         if Is_Spec_File (Kernel, Source_Filename)
           and then Source_Filename = Entity_File
         then
            Type_Entity_List.Append
              (Subprogram_Index_List, Clone (Entity_Node));
         end if;
      end Process_Subprogram;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type
        (Source_Filename : Virtual_File;
         Entity_File     : Virtual_File) is
      begin
         Entity_Node.Kind := Type_Entity;

         --  if defined in a spec file => add to the Type_Index_List
         if Is_Spec_File (Kernel, Source_Filename)
           and then Source_Filename = Entity_File
         then
            Type_Entity_List.Append (Type_Index_List, Clone (Entity_Node));
         end if;
      end Process_Type;

   begin
      LI_Unit := Locate_From_Source_And_Complete (Kernel, Source_Filename);

      --  If body file, no need to start working on ALI, the lists of the
      --  spec file can be used.

      if not Is_Spec_File (Kernel, Source_Filename) then
         Process_Source
           (B,
            Kernel,
            Doc_File,
            Next_Package,
            Prev_Package,
            Source_File_List,
            Source_Filename,
            Package_Name,
            Entity_List,
            Process_Body_File,
            LI_Unit,
            Options,
            Converter,
            Doc_Directory,
            Doc_Suffix);

         --  now free the list
         TEL.Free (Entity_List);

         --  but if a spec file, you have to look in the ALI files
      else
         Trace (Me, "Find all possible declarations");

         --  Get all entities of the file

         if LI_Unit /= No_LI_File then
            Entity_Iter := Find_All_Possible_Declarations (LI_Unit, "");
            Tree := Create_Tree (LI_Unit);
         else
            Trace (Me, "LI file not found");  --  later Exception?
         end if;

         --  Get next entity from the file

         while not At_End (Entity_Iter) loop
            Info := Get (Entity_Iter);

            Entity_Node.Is_Private := Get_Scope (Info) /= Global_Scope;

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

               --  Get the parameters needed by all entities

               Entity_Node.Name   :=
                 new String'(Get_Full_Name (Info, LI_Unit, ".", Tree));
               Entity_Node.Entity := Copy (Info);

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
                        Get_Declaration_File_Of (Info),
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
                           Get_Declaration_File_Of (Info));
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
           (B,
            Kernel,
            Doc_File,
            Next_Package,
            Prev_Package,
            Source_File_List,
            Source_Filename,
            Package_Name,
            Entity_List,
            Process_Body_File,
            LI_Unit,
            Options,
            Converter,
            Doc_Directory,
            Doc_Suffix);

         --  If body files are not being processed, free directly here

         if not Options.Process_Body_Files then
            TEL.Free (Entity_List);
         end if;
      end if;
   end Process_One_File;

end Docgen.Work_On_File;
