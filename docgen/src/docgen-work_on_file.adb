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
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Intl;                use Glide_Intl;
with VFS;                       use VFS;

package body Docgen.Work_On_File is

   Me : constant Debug_Handle := Create ("Docgen");

   package TSFL renames Type_Source_File_List;
   package TEL  renames Type_Entity_List;
   package TRL  renames Type_Reference_List;

   procedure Process_One_File
     (B                             : Backend_Handle;
      Doc_File                      : File_Type;
      Kernel                        : access
        Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename               : Virtual_File;
      Package_Name                  : String;
      Next_Package                  : GNAT.OS_Lib.String_Access;
      Prev_Package                  : GNAT.OS_Lib.String_Access;
      Source_File_List              : in out Type_Source_File_List.List;
      Options                       : All_Options;
      Process_Body_File             : Boolean;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Type_Index_List               : in out Type_Entity_List.List;
      Tagged_Types_List             : in out Type_List_Tagged_Element.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Private_Tagged_Types_List     : in out Type_List_Tagged_Element.List;
      All_Tagged_Types_List         : in out List_Entity_Handle.List;
      Converter                     : Docgen.Doc_Subprogram_Type;
      Doc_Directory                 : String;
      Doc_Suffix                    : String;
      Still_Warned                  : in out Boolean;
      All_Scope_Tree : in out Tree_Htable.String_Hash_Table.HTable);
   --  Called by Process_Files for each file from the given list
   --  will examine that file and call the function Work_On_Source
   --  from Docgen.Work_On_File.

   --  In the procedure Process_Files each file from the list will be passed
   --  to the procedure Process_One_File, while collecting information about
   --  types and subprograms of all spec files, to be able to create index
   --  lists of these entities by calling the procedures Process_Type_Index,
   --  Process_Subprogram_Index and Process_Unit_Index (the latter for the
   --  source file list) in the package Docgen.Work_On_File.
   --  Source_File_List : list of source files that must be processed.
   --  Options          : options set by preferences.
   --  Process_Body_File: indicate if body files are also processed.
   --  Type_Index_List  : list of all public types contained in all files.
   --  Private_Type_Index_List  : list of all private types contained in all
   --  files.
   --  Subprogram_Index_List    : list of all public subprograms contained in
   --  all files.
   --  Private_Subprogram_Index_List : list of all private subprograms
   --  contained in all files.
   --  Tagged_Types_List        : list of all public tagged types contained in
   --  all files.
   --  Private_Tagged_Types_List: list of all private tagged types contained
   --  in all files.
   --  All_Tagged_Types_List    : contains all the tagged types. Those that
   --  are declared in the processed files but also the parents and the
   --  children even if they aren't declared in the processed files.
   --  Converter : indicates what format the documentation should be generated
   --  in.
   --  Still_Warned   : indicate that a message still has been put on the
   --  console.
   --  All_Scope_Tree : hash table which contains the scope trees built. This
   --  hash table is shared by all files and finally destroyed at the end of
   --  the documentation process in Process_Files.

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
      use Type_List_Tagged_Element;
      use List_Entity_Handle;
      use TSFL;

      J                             : Natural;
      Source_File_Node              : Type_Source_File_List.List_Node;
      Doc_File                      : File_Type;
      Next_Package                  : GNAT.OS_Lib.String_Access;
      Prev_Package                  : GNAT.OS_Lib.String_Access;
      Subprogram_Index_List         : Type_Entity_List.List;
      Type_Index_List               : Type_Entity_List.List;
      Tagged_Types_List             : Type_List_Tagged_Element.List;
      Private_Subprogram_Index_List : Type_Entity_List.List;
      Private_Type_Index_List       : Type_Entity_List.List;
      Private_Tagged_Types_List     : Type_List_Tagged_Element.List;
      All_Tagged_Types_List         : List_Entity_Handle.List;
      Unused                        : List_Reference_In_File.List;
      Doc_Directory_Root            : constant String :=
        Get_Doc_Directory (B, Kernel_Handle (Kernel));
      Still_Warned                  : Boolean;
      Level                         : Natural := 1;
      All_Scope_Tree                : Tree_Htable.String_Hash_Table.HTable;

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
        (Package_Nr : Natural) return String
      is
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

            if Local_Node = Type_Source_File_List.Null_Node then
               --  don't change this return, needed in Docgen.Texi_Output
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

            if Local_Node = Type_Source_File_List.Null_Node then
               --  Don't change this return, needed in Docgen-Texi_Output
               --  in Write_Not_Regular_Beginning!
               return "";

            else
               return Data (Local_Node).Package_Name.all;
            end if;
         end if;
      end Find_Prev_Package;

   begin

      Still_Warned := False;

      --  Sort the list of the files first
      Sort_List_Name (Source_File_List);
      Source_File_Node := TSFL.First (Source_File_List);

      J := 1;
      while Source_File_Node /= TSFL.Null_Node loop
         declare
            File_Name : constant String := Get_Doc_File_Name
              (TSFL.Data (Source_File_Node).File_Name,
               Doc_Directory_Root, Doc_Suffix);
         begin
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
               Tagged_Types_List,
               Private_Subprogram_Index_List,
               Private_Type_Index_List,
               Private_Tagged_Types_List,
               All_Tagged_Types_List,
               Converter,
               Doc_Directory_Root,
               Doc_Suffix,
               Still_Warned,
               All_Scope_Tree);

            Source_File_Node := TSFL.Next (Source_File_Node);
            J := J + 1;

            Close (Doc_File);
            Free (Next_Package);
            Free (Prev_Package);
         end;
      end loop;

      --  Sort the type index list and the subprogram index list first (both
      --  for private and public lists)
      Sort_List_Name (Subprogram_Index_List);
      Sort_List_Name (Type_Index_List);
      Sort_List_Name (Private_Subprogram_Index_List);
      Sort_List_Name (Private_Type_Index_List);

      --  Create the index doc files for the packages
      Process_Unit_Index
        (B, Kernel, Source_File_List, Unused, Options, Converter,
           Doc_Directory_Root, Doc_Suffix, Level);
      Process_Subprogram_Index
        (B, Kernel, Subprogram_Index_List, Private_Subprogram_Index_List,
         Unused, Options, Converter, Doc_Directory_Root, Doc_Suffix, Level);
      Process_Type_Index
        (B, Kernel, Type_Index_List, Private_Type_Index_List,
         Unused,  Options, Converter, Doc_Directory_Root, Doc_Suffix, Level);

      if Options.Tagged_Types then
         Sort_List_Name (Tagged_Types_List);
         if Options.Show_Private then
            Sort_List_Name (Private_Tagged_Types_List);
         end if;
         Process_Tagged_Type_Index
           (B, Kernel, Tagged_Types_List, Private_Tagged_Types_List,
            Unused, Source_File_List, Options, Converter, Doc_Directory_Root,
            Doc_Suffix, Level);
      end if;

      Tree_Htable.String_Hash_Table.Reset (All_Scope_Tree);
      --  Scope trees are deleted only when all files have been processed.
      TEL.Free (Subprogram_Index_List);
      TEL.Free (Type_Index_List);
      Type_List_Tagged_Element.Free (Tagged_Types_List);
      List_Entity_Handle.Free (All_Tagged_Types_List);

   end Process_Files;

   ----------------------
   -- Process_One_File --
   ----------------------

   procedure Process_One_File
     (B                     : Backend_Handle;
      Doc_File              : File_Type;
      Kernel                : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source_Filename       : Virtual_File;
      Package_Name          : String;
      Next_Package          : GNAT.OS_Lib.String_Access;
      Prev_Package          : GNAT.OS_Lib.String_Access;
      Source_File_List      : in out Type_Source_File_List.List;
      Options               : All_Options;
      Process_Body_File     : Boolean;
      Subprogram_Index_List : in out Type_Entity_List.List;
      Type_Index_List       : in out Type_Entity_List.List;
      Tagged_Types_List     : in out Type_List_Tagged_Element.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Private_Tagged_Types_List     : in out Type_List_Tagged_Element.List;
      All_Tagged_Types_List : in out List_Entity_Handle.List;
      Converter             : Docgen.Doc_Subprogram_Type;
      Doc_Directory         : String;
      Doc_Suffix            : String;
      Still_Warned          : in out Boolean;
      All_Scope_Tree : in out Tree_Htable.String_Hash_Table.HTable)
   is
      LI_Unit          : LI_File_Ptr;
      LI_List          : Src_Info.LI_File_List;
      Tree             : Scope_Tree;
      Entity_Iter      : Local_Entities_Iterator;
      Ref_In_File      : E_Reference;
      Info             : Entity_Information;
      Entity_Node      : Entity_List_Information;
      Entity_List      : Type_Entity_List.List;
      List_Ref_In_File : List_Reference_In_File.List;
      List_Ent_In_File : List_Entity_In_File.List;
      --  Used to clear space taken by Entity_Information which are
      --  pointed by the field Entity of the record Reference_In_File
      Ent_Handle       : Entity_Handle := null;
      Status           : Find_Decl_Or_Body_Query_Status;

      Field            : Entity_Information;
      Entity_Complete  : Entity_List_Information_Handle;
      Found_Private    : Boolean;
      Tree_Field       : Scope_Tree;
      Node_Field       : Scope_Tree_Node;
      Iter_Field       : Scope_Tree_Node_Iterator;
      --  The 6 variables above are used for private field of public types

      Me_Info          : Entity_Handle;
      Son_Info         : Entity_Handle;
      Father_Info      : Entity_Handle;
      Father           : Entity_Information;
      Son              : Entity_Information;
      Global_Son       : Entity_Information;
      Child            : Child_Type_Iterator;
      Global_Child     : Child_Type_Iterator;
      Parent           : Parent_Iterator;
      Children_Iter    : Entity_Reference_Iterator;
      Tag_Elem_Me      : Tagged_Element_Handle;
      Tag_Elem_Parent  : Tagged_Element_Handle;
      Tag_Elem_Child   : Tagged_Element_Handle;
      Entity_Is_Tagged : Boolean := False;
      Found_Global     : Boolean := False;
      LI               : LI_File_Ptr;
      --  The 16 variables above are used to find tagged types, their children
      --  and their parents

      Level : Natural;
      --  Stores the level of the current package in which we are
      --  processing types, subprograms...

      use List_Reference_In_File;

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

      procedure Global_Children
        (Found : in out Boolean;
         Iter  : in out Entity_Reference_Iterator);
      --  Found : it indicates if a tagged type has children declared in other
      --  ali files.
      --  Iter  : if found is True, it points on children declared in other
      --  ali files.

      procedure Is_Tagged_Type
        (Entity_Is_Tagged : in out Boolean;
         Found_Global     : in out Boolean;
         Iter             : in out Entity_Reference_Iterator;
         Info             : in Entity_Information;
         LI_Unit          : in LI_File_Ptr);
      --  Entity_Is_Tagged: indicate if Info is a tagged type.
      --  Found_Global    : if Info is a tagged type, it indicates if there is
      --  children declared in other ali files.
      --  Iter            : if found_global is True, it points on children
      --  declared in other ali files.

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

            if Get_Kind (Entity).Kind = Function_Or_Operator
              or else Get_Kind (Entity).Kind =  Procedure_Kind
            --  Is_Subprogram (Child_Node) was the previous instruction.
            --  ??? For this function package are subprograms.
            then
               Type_Reference_List.Append
                 (Calls_List,
                  (Entity   => Entity,
                   Set_Link =>
                     (Options.Link_All
                      or else Source_File_In_List
                        (Source_File_List, Get_Declaration_File_Of (Entity)))
                   and then
                     (Get_Scope (Entity) = Global_Scope
                      or else Options.Show_Private
                      or else (Options.Process_Body_Files
                               and then
                               not Is_Spec_File
                                 (Kernel,
                                  Get_Declaration_File_Of (Entity))))));
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
         LI_List              : Src_Info.LI_File_List;

         procedure Tree_Called_Callback
           (Node        : Scope_Tree_Node;
            Is_Renaming : Boolean);
         --  The callback function is used to find the names of
         --  the referenced subprograms and add each to the
         --  Local_Ref_List (the subprograms, where the current
         --  subprogram is called)

         procedure Remove_Double_Nodes (List : in out TRL.List);
         --  Remove all double nodes from the list,
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

      begin
         --  Only if the procedure is defined in this file AND
         --  the references are wished:

         if Entity_File = Source_Filename
           and then Options.References
         then
            LI_List := Get_LI_File_List (Kernel);
            Find_All_References
                (Get_Root_Project (Get_Registry (Kernel)),
                 Get_Language_Handler (Kernel),
                 Info,
                 LI_List,
                 Reference_Iter,
                 No_Project,
                 True,
                 VFS.No_File);

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
                  Get_File (Get_Location (Get (Reference_Iter))));

               --  Try to get the scope tree associated with the ali file
               Reference_Scope_Tree
                 := Tree_Htable.String_Hash_Table.Get
                   (All_Scope_Tree,
                    Base_Name (Get_LI_Filename (Get_LI (Reference_Iter))));
               --  First time, we met the ali file, we build the scope tree
               --  and store it in the hash table during all the process of
               --  docgen
               if Reference_Scope_Tree = Null_Scope_Tree then
                  Reference_Scope_Tree :=
                    Create_Tree (Get_LI (Reference_Iter));
                  Tree_Htable.String_Hash_Table.Set
                    (All_Scope_Tree,
                     Base_Name (Get_LI_Filename (Get_LI (Reference_Iter))),
                     Reference_Scope_Tree);
               end if;

               Find_Entity_References
                 (Reference_Scope_Tree,
                  Info,
                  Tree_Called_Callback'Unrestricted_Access);

               Next (Get_Language_Handler (Kernel),
                     Reference_Iter,
                     LI_List);
            end loop;

            --  Pass the local lists to the entity_node lists
            Remove_Double_Nodes (Local_Ref_List);
            Entity_Node.Called_List := Local_Ref_List;
            Remove_Double_Nodes (Local_Calls_List);
            Entity_Node.Calls_List  := Local_Calls_List;
         end if;

         --  If defined in a spec file, add entity to the
         --  Subprogram_Index_List

         if Is_Spec_File (Kernel, Source_Filename)
           and then Source_Filename = Entity_File
         then
            if Options.Show_Private and then
              Entity_Node.Is_Private
            then
               Type_Entity_List.Append
                 (Private_Subprogram_Index_List, Clone (Entity_Node, False));
            else
               Type_Entity_List.Append
                 (Subprogram_Index_List, Clone (Entity_Node, False));
            end if;
         end if;
      end Process_Subprogram;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type
        (Source_Filename : Virtual_File;
         Entity_File     : Virtual_File) is
      begin
         --  If defined in a spec file => add to the Type_Index_List
         Entity_Node.Kind := Type_Entity;
         if Is_Spec_File (Kernel, Source_Filename)
           and then Source_Filename = Entity_File
         then
            if not Entity_Node.Is_Private then
               Type_Entity_List.Append
                 (Type_Index_List, Clone (Entity_Node, False));
            else
               Type_Entity_List.Append
                 (Private_Type_Index_List, Clone (Entity_Node, False));
            end if;
         end if;
      end Process_Type;

      ---------------------
      -- Global_Children --
      ---------------------

      procedure Global_Children
        (Found : in out Boolean;
         Iter  : in out Entity_Reference_Iterator)
      is
         LI           : LI_File_Ptr;
         Child        : Child_Type_Iterator;
         C            : Entity_Information;
         Found_Global : Boolean := False;
      begin

         Find_All_References
           (Get_Root_Project (Get_Registry (Kernel)),
            Get_Language_Handler (Kernel),
            Info,
            LI_List,
            Iter,
            No_Project,
            True,
            VFS.No_File);

         if Get (Iter) = No_Reference then
            Found := False;
         else
            while Get (Iter) /= No_Reference loop
               LI := Get_LI (Iter);
               Child := Get_Children_Types (LI, Info);

               C := Get (Child);
               --  we don't make a loop in order to parse all the children
               --  We want to know if Info has at least one child
               if C /= No_Entity_Information then
                  Found_Global := True;
                  Destroy (C);
               end if;

               Destroy (Child);

               exit when Found_Global;
               Next (Get_Language_Handler (Kernel),
                     Iter,
                     LI_List);
            end loop;
            Found := Found_Global;
         end if;
      end Global_Children;

      --------------------
      -- Is_Tagged_Type --
      --------------------

      procedure Is_Tagged_Type
        (Entity_Is_Tagged : in out Boolean;
         Found_Global     : in out Boolean;
         Iter             : in out Entity_Reference_Iterator;
         Info             : in Entity_Information;
         LI_Unit          : in LI_File_Ptr) is
      begin
         if Get_Kind (Info).Is_Type
           and then
             (Get_Kind (Info).Kind = Record_Kind
               --  In Ada, tagged type are classified as Record
               --  The only way to distinguish them to classic
               --  record is to search for parent and children.
               --  ??? tagged types without child and without
               --  parent don't appear in the list
              or else Get_Kind (Info).Kind = Class
              or else Get_Kind (Info).Kind = Class_Wide)
         then

            Global_Children (Found_Global, Iter);

            if Get (Get_Parent_Types (LI_Unit, Info))
              /= No_Entity_Information
              or else
                Get (Get_Children_Types (LI_Unit, Info))
              /= No_Entity_Information
              or else
                Found_Global
            then
               Entity_Is_Tagged := True;
            else
               Entity_Is_Tagged := False;
            end if;
         else
            Entity_Is_Tagged := False;
         end if;
      end Is_Tagged_Type;

   begin
      LI_Unit := Locate_From_Source_And_Complete (Kernel, Source_Filename);
      LI_List := Get_LI_File_List (Kernel);
      Level := 1;

      Trace (Me, "File name: " & Base_Name (Source_Filename));
      --  All references of the current file are put in a list.
      --  In the case of a spec file, we used references which are also
      --  declarations. Before those changes, declarations were found by
      --  Find_All_Possible_Declaration.
      --  For spec and body files, we can use this list after during the
      --  linkage process instead of calling a subprogram.

      if not Is_Spec_File (Kernel, Source_Filename) then
         if LI_Unit /= No_LI_File then
            Entity_Iter :=
              Find_All_References_In_File (LI_Unit, Source_Filename);

            loop
               Ref_In_File := Get (Entity_Iter);

               if Ref_In_File = No_Reference then
                  --  New entity
                  Info := Get (Entity_Iter);
                  exit when Info = No_Entity_Information;

                  Ent_Handle := new Entity_Information'(Info);
                  List_Entity_In_File.Append (List_Ent_In_File, Info);

               else
                  --  New reference on the current entity

                  if Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '='
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '>'
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '+'
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '-'
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '*'
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '/'
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '<'
                    and then
                      Get_Name (Get (Entity_Iter))
                    (Get_Name (Get (Entity_Iter))'First) /= '&'
                    and then
                      Get_Name (Get (Entity_Iter)) /= "/="
                    and then
                      Get_Name (Get (Entity_Iter)) /= "and"
                    and then
                      Get_Name (Get (Entity_Iter)) /= "or"
                  then
                     --  ??? Temporary solution: operators are not added.
                     --  In fact, it seems that Parse_Entity don't return
                     --  them as identifiers. So, if we add them in the
                     --  references list, they won't be matched and as a
                     --  concequence all the following references also.
                     --  NB: for spec file, there isn't this problem because
                     --  we must search in the whole list of references: so
                     --  no link is lost (we only have "too much" reference
                     --  nodes).
                     --  If there's some operators missing in the test above
                     --  it will explain the lost of links for body files
                     --  Bug of GNAT: in src_info-queries.adb, a reference
                     --  "finalize" is returned instead of "Full_Name" line
                     --  1907: after this point, all links are lost for the
                     --  reason seen before.

                     List_Reference_In_File.Append
                       (List_Ref_In_File,
                        (Name   =>
                         new String'(Get_Name (Get (Entity_Iter))),
                         Line   => Get_Line (Get_Location (Ref_In_File)),
                         Column => Get_Column (Get_Location (Ref_In_File)),
                         Entity => Ent_Handle));
                  end if;
               end if;

               --  Get next entity (or reference) in this file
               Next (Entity_Iter);
            end loop;

            Sort_List_By_Line_And_Column (List_Ref_In_File);

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
               List_Ref_In_File,
               Tagged_Types_List,
               Private_Tagged_Types_List,
               Process_Body_File,
               LI_Unit,
               Options,
               Converter,
               Doc_Directory,
               Doc_Suffix,
               Level,
               All_Scope_Tree);
         else
            Trace (Me, "LI file not found");  --  later Exception?
         end if;

         TEL.Free (Entity_List);

      else
         --  If a spec file, you have to look in the ALI files

         if LI_Unit /= No_LI_File then
            Entity_Iter :=
              Find_All_References_In_File (LI_Unit, Source_Filename);

            --  Try to get the scope tree of the ali file.
            Tree := Tree_Htable.String_Hash_Table.Get
              (All_Scope_Tree, Base_Name (Get_LI_Filename (LI_Unit)));
            --  The scope tree has never been created, we build and store it.
            if Tree = Null_Scope_Tree then
               Tree :=
                 Create_Tree (LI_Unit);
               Tree_Htable.String_Hash_Table.Set
                 (All_Scope_Tree,
                  Base_Name (Get_LI_Filename (LI_Unit)),
                  Tree);
            end if;

            loop
               Ref_In_File := Get (Entity_Iter);

               if Ref_In_File = No_Reference then
                  --  New entity

                  Info := Get (Entity_Iter);
                  exit when Info = No_Entity_Information;

                  Ent_Handle := new Entity_Information'(Info);
                  List_Entity_In_File.Append (List_Ent_In_File, Info);

                  Entity_Node.Is_Private := Get_Scope (Info) /= Global_Scope;
                  Entity_Node.Public_Declaration := No_Entity_Information;

                  Find_Next_Body
                    (Kernel, LI_Unit,
                     Info, Entity_Node.Line_In_Body, Status);

                  if Status /= Success then
                     --  Should we really reset Line_In_Body when
                     --  Status = Fuzzy_Match ???

                     Entity_Node.Line_In_Body := Null_File_Location;
                     if not Still_Warned then
                        Console.Insert
                          (Kernel,
                             -("Docgen: xref information is not up-to-date, " &
                               "output may not be accurate"),
                           Mode => Verbose);
                        Still_Warned := True;
                        --  The message is print only one time.
                     end if;
                  end if;

                  --  Check if the declaration of the entity is in one of the
                  --  files which are in list, if false => no need for
                  --  creating links.
                  --  Also check if it's a private entity and whether they
                  --  should be processed.

                  if Source_File_In_List
                    (Source_File_List, Get_Declaration_File_Of (Info))
                    and then (Options.Show_Private
                              or else not Entity_Node.Is_Private)
                  then
                     --  Get the parameters needed by all entities

                     Entity_Node.Name :=
                       new String'(Get_Full_Name (Info, LI_Unit, ".", Tree));
                     Entity_Node.Entity := Copy (Info);

                     --  For all entities which are not subprograms the ref
                     --  lists must be set to null

                     if Get_Kind (Info).Kind /= Function_Or_Operator
                       and then Get_Kind (Info).Kind /= Procedure_Kind
                     then
                        Entity_Node.Called_List := TRL.Null_List;
                        Entity_Node.Calls_List  := TRL.Null_List;
                     end if;

                     --  Get the entity specific parameters.
                     --  These are the last parameters to gather, after the
                     --  CASE no more changes are allowed, because the
                     --  index lists are created in the subprograms used
                     --  here, so all info must be avaiable.
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
                             Signed_Integer
                        =>
                           if Get_Kind (Info).Is_Type then
                              Process_Type
                                (Source_Filename,
                                 Get_Declaration_File_Of (Info));

                              if Options.Show_Private
                                and then
                                   not Entity_Node.Is_Private
                              then
                                 --  For record/enum, we must search if they
                                 --  have private fields. In this case, we
                                 --  must create a new entity in order have
                                 --  documentation about it. In fact, the
                                 --  record itself is public and if this work
                                 --  isn't done, only the documentation
                                 --  "type X is record with private" is given.
                                 --  The private fiels are forgotten.
                                 Get_Scope_Tree (Kernel,
                                                 Info,
                                                 Tree_Field,
                                                 Node_Field,
                                                 True);
                                 Iter_Field := Start (Node_Field);
                                 Found_Private := False;
                                 loop
                                    Node_Field := Get (Iter_Field);
                                    exit when Node_Field
                                      = Null_Scope_Tree_Node;
                                    Field := Get_Entity (Node_Field);

                                    if not Is_Discriminant
                                      (Field, LI_Unit, Info) then
                                       if Get_Scope (Field)
                                         /= Global_Scope then
                                          Found_Private := True;
                                       end if;
                                       exit when Found_Private;
                                    end if;
                                    Destroy (Field);
                                    Next (Iter_Field);
                                 end loop;

                                 if Found_Private then
                                    Entity_Complete
                                      := new Entity_List_Information;
                                    Entity_Complete.all.Entity
                                      := Create
                                        (File   =>
                                      Get_File (Entity_Node.Line_In_Body),
                                         Line   =>
                                      Get_Line (Entity_Node.Line_In_Body),
                                         Column =>
                                           Get_Column
                                             (Entity_Node.Line_In_Body),
                                         Name   =>
                                           Get_Name (Entity_Node.Entity),
                                         Scope  =>
                                           Get_Scope (Entity_Node.Entity),
                                         Kind   =>
                                           Get_Kind (Entity_Node.Entity));
                                    Entity_Complete.Name :=
                                      new String'(Get_Full_Name
                                                    (Info,
                                                     LI_Unit, ".", Tree));
                                    Entity_Complete.Kind := Type_Entity;
                                    Entity_Complete.Is_Private := True;
                                    Entity_Complete.Public_Declaration :=
                                      Copy (Entity_Node.Entity);
                                    Entity_Complete.Line_In_Body
                                      := Entity_Node.Line_In_Body;
                                    Entity_Complete.Called_List
                                      := TRL.Null_List;
                                    Entity_Complete.Calls_List
                                      := TRL.Null_List;

                                    Type_Entity_List.Prepend
                                      (Entity_List, Entity_Complete.all);

                                    if Is_Spec_File (Kernel, Source_Filename)
                                      and then Source_Filename
                                        = Get_Declaration_File_Of
                                          (Entity_Complete.Entity)
                                    then
                                       --  Currently, we add the name of the
                                       --  record/enum type. So, this name is
                                       --  duplicated: it appears both in
                                       --  public and private part of the
                                       --  index list. For the future, it
                                       --  would be better to add the fields
                                       --  in the private part.
                                       Type_Entity_List.Append
                                         (Private_Type_Index_List,
                                          Clone (Entity_Complete.all, False));
                                    end if;
                                 end if;
                                 Free (Tree_Field);
                              end if;

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

                     if Options.Tagged_Types then
                        Entity_Is_Tagged := False;
                        Found_Global     := False;
                        Is_Tagged_Type (Entity_Is_Tagged,
                                        Found_Global,
                                        Children_Iter,
                                        Info, LI_Unit);
                        if Entity_Is_Tagged then

                           Me_Info := Find_In_List (All_Tagged_Types_List,
                                                    Info);
                           if Me_Info /= null then
                              --  This tagged type has still been met.
                              --  During the update of the lists, the local
                              --  son or parent which is added doesn't still
                              --  exist in the list

                              --  Search of fathers
                              Parent :=
                                Get_Parent_Types (LI_Unit, Me_Info.all);
                              loop
                                 Father := Get (Parent);
                                 exit when Father = No_Entity_Information;

                                 --  New father
                                 Father_Info := Find_In_List
                                   (All_Tagged_Types_List, Father);
                                 if Father_Info = null then
                                    --  Father seen for the first time
                                    Father_Info
                                      := new Entity_Information'(Father);
                                    List_Entity_Handle.Append
                                      (All_Tagged_Types_List, Father_Info);

                                    Tag_Elem_Parent := new Tagged_Element;
                                    Tag_Elem_Parent.Me := Father_Info;
                                    Tag_Elem_Parent.Number_Of_Children := 0;
                                    Tag_Elem_Parent.Number_Of_Parents := 0;

                                    if Source_File_In_List
                                      (Source_File_List,
                                       Get_Declaration_File_Of
                                         (Father_Info.all))then
                                       --  If this tagged type is declared, we
                                       --  indicate that we must print it in
                                       --  the doc file
                                       Tag_Elem_Parent.Print_Me := True;
                                    else
                                       Tag_Elem_Parent.Print_Me := False;
                                    end if;

                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Father_Info.all))
                                    then
                                       --  In this case, the parent won't be
                                       --  analised as an entity, so we must
                                       --  fill its list of children
                                       --  First time we met this father, so
                                       --  no need to check if Me_Info  still
                                       --  exist in list
                                       Tag_Elem_Parent.Number_Of_Children
                                         := 1;
                                       List_Entity_Handle.Append
                                         (Tag_Elem_Parent.My_Children,
                                          Me_Info);
                                    end if;

                                    if Options.Show_Private and then
                                      Get_Scope (Father_Info.all)
                                      /= Global_Scope
                                    then
                                       Type_List_Tagged_Element.Append
                                         (Private_Tagged_Types_List,
                                          Tag_Elem_Parent.all);
                                    else
                                       Type_List_Tagged_Element.Append
                                         (Tagged_Types_List,
                                          Tag_Elem_Parent.all);
                                    end if;
                                 else
                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Father_Info.all))
                                    then
                                       if Options.Show_Private and then
                                         Get_Scope (Father_Info.all)
                                         /= Global_Scope
                                       then
                                          Add_Child (Private_Tagged_Types_List,
                                                     Father_Info,
                                                     Me_Info);
                                       else
                                          Add_Child (Tagged_Types_List,
                                                     Father_Info,
                                                     Me_Info);
                                       end if;
                                    end if;
                                 end if;

                                 if Options.Show_Private and then
                                   Get_Scope (Me_Info.all)
                                   /= Global_Scope
                                 then
                                    Add_Parent
                                      (Private_Tagged_Types_List, Me_Info,
                                       Father_Info);
                                 else
                                    Add_Parent
                                      (Tagged_Types_List, Me_Info,
                                       Father_Info);
                                 end if;

                                 --  Update of the list of parents with the
                                 --  local parent.
                                 --  Update also the number of parents
                                 Next (Parent);
                              end loop;

                              --  Search for children

                              Child :=
                                Get_Children_Types (LI_Unit, Me_Info.all);

                              loop
                                 Son := Get (Child);
                                 exit when Son = No_Entity_Information;

                                 --  New child
                                 Son_Info := Find_In_List
                                   (All_Tagged_Types_List, Son);

                                 if Son_Info = null then
                                    --  Child seen for the first time
                                    Son_Info := new Entity_Information'(Son);
                                    List_Entity_Handle.Append
                                      (All_Tagged_Types_List, Son_Info);

                                    Tag_Elem_Child := new Tagged_Element;
                                    Tag_Elem_Child.Me := Son_Info;
                                    Tag_Elem_Child.Number_Of_Children := 0;
                                    Tag_Elem_Child.Number_Of_Parents := 0;

                                    if Source_File_In_List
                                      (Source_File_List,
                                       Get_Declaration_File_Of
                                         (Son_Info.all))then
                                       --  If this tagged type is declared, we
                                       --  indicate that we must print it in
                                       --  the doc file
                                       Tag_Elem_Child.Print_Me := True;
                                    else
                                       Tag_Elem_Child.Print_Me := False;
                                    end if;

                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Son_Info.all))
                                    then
                                       --  In this case, the son won't be
                                       --  analysed as an entity, so we must
                                       --  fill its list of parents
                                       --  First time we met this child, so
                                       --  no need to check if this Me_Info
                                       --  still put in the list
                                       Tag_Elem_Child.Number_Of_Parents
                                         := 1;
                                       List_Entity_Handle.Append
                                         (Tag_Elem_Child.My_Parents, Me_Info);
                                    end if;

                                    if Options.Show_Private and then
                                      Get_Scope (Son_Info.all) /= Global_Scope
                                    then
                                       Type_List_Tagged_Element.Append
                                         (Private_Tagged_Types_List,
                                          Tag_Elem_Child.all);
                                    else
                                       Type_List_Tagged_Element.Append
                                         (Tagged_Types_List,
                                          Tag_Elem_Child.all);
                                    end if;
                                 else
                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Son_Info.all))
                                    then
                                       if Options.Show_Private and then
                                         Get_Scope (Son_Info.all)
                                         /= Global_Scope
                                       then
                                          Add_Parent
                                            (Private_Tagged_Types_List,
                                             Son_Info,
                                             Me_Info);
                                       else
                                          Add_Parent (Tagged_Types_List,
                                                      Son_Info,
                                                      Me_Info);
                                       end if;
                                    end if;
                                 end if;

                                 if Options.Show_Private and then
                                   Get_Scope (Me_Info.all)
                                   /= Global_Scope
                                 then
                                    Add_Child
                                      (Private_Tagged_Types_List,
                                       Me_Info,
                                       Son_Info);
                                 else
                                    Add_Child
                                      (Tagged_Types_List, Me_Info, Son_Info);
                                 end if;
                                 --  Update of the list of children with the
                                 --  local child.
                                 --  Update also the number of children

                                 Next (Child);
                              end loop;

                              --  Search for all chidren (those who don't
                              --  appear in current file)
                              if Found_Global then

                                 while Get (Children_Iter) /= No_Reference loop
                                    LI := Get_LI (Children_Iter);

                                    Global_Child := Get_Children_Types (LI,
                                                                        Info);
                                    loop
                                       Global_Son := Get (Global_Child);
                                       exit when Global_Son
                                         = No_Entity_Information;

                                       --  New child
                                       Son_Info := Find_In_List
                                         (All_Tagged_Types_List, Global_Son);

                                       if Son_Info = null then
                                          --  Child seen for the first time
                                          Son_Info
                                            := new Entity_Information'
                                              (Global_Son);
                                          List_Entity_Handle.Append
                                            (All_Tagged_Types_List, Son_Info);

                                          Tag_Elem_Child := new Tagged_Element;
                                          Tag_Elem_Child.Me := Son_Info;
                                          Tag_Elem_Child.Number_Of_Children
                                            := 0;
                                          Tag_Elem_Child.Number_Of_Parents
                                            := 0;

                                          if Source_File_In_List
                                            (Source_File_List,
                                             Get_Declaration_File_Of
                                               (Son_Info.all))then
                                             --  This tagged type is declared,
                                             --  we indicate that we must print
                                             --  it in the doc file
                                             Tag_Elem_Child.Print_Me := True;
                                          else
                                             Tag_Elem_Child.Print_Me := False;
                                          end if;

                                          if Options.Process_Body_Files
                                            and then
                                              not Is_Spec_File
                                                (Kernel,
                                                 Get_Declaration_File_Of
                                                   (Son_Info.all))
                                          then
                                             --  In this case, the son won't
                                             --  be analysed as an entity, so
                                             --  we must fill its list of
                                             --  parents
                                             --  First time we met this child,
                                             --  so no need to check if
                                             --  Me_Info still put in the list
                                             Tag_Elem_Child.Number_Of_Parents
                                               := 1;
                                             List_Entity_Handle.Append
                                               (Tag_Elem_Child.My_Parents,
                                                Me_Info);
                                          end if;

                                          if Options.Show_Private and then
                                            Get_Scope (Son_Info.all)
                                            /= Global_Scope
                                          then
                                             Type_List_Tagged_Element.Append
                                               (Private_Tagged_Types_List,
                                                Tag_Elem_Child.all);
                                          else
                                             Type_List_Tagged_Element.Append
                                               (Tagged_Types_List,
                                                Tag_Elem_Child.all);
                                          end if;
                                       else
                                          if Options.Process_Body_Files
                                            and then
                                              not Is_Spec_File
                                                (Kernel,
                                                 Get_Declaration_File_Of
                                                   (Son_Info.all))
                                          then
                                             if Options.Show_Private and then
                                               Get_Scope (Son_Info.all)
                                               /= Global_Scope
                                             then
                                                Add_Parent
                                                  (Private_Tagged_Types_List,
                                                   Son_Info,
                                                   Me_Info);
                                             else
                                                Add_Parent (Tagged_Types_List,
                                                            Son_Info,
                                                            Me_Info);
                                             end if;

                                          end if;
                                       end if;

                                       if Options.Show_Private and then
                                         Get_Scope (Me_Info.all)
                                         /= Global_Scope
                                       then
                                          Add_Child
                                            (Private_Tagged_Types_List,
                                             Me_Info,
                                             Son_Info);
                                       else
                                          Add_Child
                                            (Tagged_Types_List, Me_Info,
                                             Son_Info);
                                       end if;
                                       --  Update of the list of children
                                       --  with the local child.
                                       --  Update also the number of children

                                       Next (Global_Child);
                                    end loop;

                                    Destroy (Global_Child);
                                    Next (Get_Language_Handler (Kernel),
                                          Children_Iter,
                                          LI_List);
                                 end loop;

                              end if;

                           else
                              --  First met with this tagged type.
                              --  So, we don't need to do an update!
                              Tag_Elem_Me := new Tagged_Element;
                              Me_Info :=  new Entity_Information'(Copy (Info));
                              List_Entity_Handle.Append
                                (All_Tagged_Types_List, Me_Info);
                              Tag_Elem_Me.Me := Me_Info;
                              Tag_Elem_Me.Number_Of_Children := 0;
                              Tag_Elem_Me.Number_Of_Parents := 0;

                              if Source_File_In_List
                                (Source_File_List,
                                 Get_Declaration_File_Of
                                   (Me_Info.all))then
                                 --  If this tagged type is declared, we
                                 --  indicate that we must print it in
                                 --  the doc file
                                 Tag_Elem_Me.Print_Me := True;
                              else
                                 Tag_Elem_Me.Print_Me := False;
                              end if;

                              --  Search of fathers
                              Parent :=
                                Get_Parent_Types (LI_Unit, Me_Info.all);
                              loop
                                 Father := Get (Parent);
                                 exit when Father = No_Entity_Information;

                                 --  New father
                                 Father_Info := Find_In_List
                                   (All_Tagged_Types_List, Father);
                                 if Father_Info = null then
                                    --  Father seen for the first time
                                    Father_Info
                                      := new Entity_Information'(Father);
                                    List_Entity_Handle.Append
                                      (All_Tagged_Types_List, Father_Info);

                                    Tag_Elem_Parent := new Tagged_Element;
                                    Tag_Elem_Parent.Me := Father_Info;
                                    Tag_Elem_Parent.Number_Of_Children := 0;
                                    Tag_Elem_Parent.Number_Of_Parents := 0;

                                    if Source_File_In_List
                                      (Source_File_List,
                                       Get_Declaration_File_Of
                                         (Father_Info.all))then
                                       --  If this tagged type is declared, we
                                       --  indicate that we must print it in
                                       --  the doc file
                                       Tag_Elem_Parent.Print_Me := True;
                                    else
                                       Tag_Elem_Parent.Print_Me := False;
                                    end if;

                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Father_Info.all))
                                    then
                                       --  In this case, the parent won't be
                                       --  analised as an entity, so we must
                                       --  fill its list of children
                                       --  First time we met this father, so
                                       --  no need to check if Me_Info
                                       --  still put in the list
                                       Tag_Elem_Parent.Number_Of_Children
                                         := 1;
                                       List_Entity_Handle.Append
                                         (Tag_Elem_Parent.My_Children,
                                          Me_Info);
                                    end if;

                                    if Options.Show_Private and then
                                      Get_Scope (Father_Info.all)
                                      /= Global_Scope
                                    then
                                       Type_List_Tagged_Element.Append
                                         (Private_Tagged_Types_List,
                                          Tag_Elem_Parent.all);
                                    else
                                       Type_List_Tagged_Element.Append
                                         (Tagged_Types_List,
                                          Tag_Elem_Parent.all);
                                    end if;

                                 else
                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Father_Info.all))
                                    then
                                       if Options.Show_Private and then
                                         Get_Scope (Father_Info.all)
                                         /= Global_Scope
                                       then
                                          Add_Child
                                            (Private_Tagged_Types_List,
                                             Father_Info,
                                             Me_Info);
                                       else
                                          Add_Child
                                            (Tagged_Types_List,
                                             Father_Info,
                                             Me_Info);
                                       end if;

                                    end if;
                                 end if;

                                 --  First time we met this tagged type, so
                                 --  no need to check if this father still
                                 --  put in the list
                                 List_Entity_Handle.Append
                                   (Tag_Elem_Me.My_Parents, Father_Info);
                                 --  Update of the list of parents with the
                                 --  local parent.

                                 Tag_Elem_Me.Number_Of_Parents :=
                                   Tag_Elem_Me.Number_Of_Parents + 1;
                                 --  Update of the number of parents

                                 Next (Parent);
                              end loop;

                              --  Search of children
                              Child :=
                                Get_Children_Types (LI_Unit, Me_Info.all);
                              loop
                                 Son := Get (Child);
                                 exit when Son = No_Entity_Information;

                                 --  New Child
                                 Son_Info := Find_In_List
                                   (All_Tagged_Types_List, Son);
                                 if Son_Info = null then
                                    --  Child seen for the first time
                                    Son_Info := new Entity_Information'(Son);
                                    List_Entity_Handle.Append
                                      (All_Tagged_Types_List, Son_Info);

                                    Tag_Elem_Child := new Tagged_Element;
                                    Tag_Elem_Child.Me := Son_Info;
                                    Tag_Elem_Child.Number_Of_Children := 0;
                                    Tag_Elem_Child.Number_Of_Parents := 0;

                                    if Source_File_In_List
                                      (Source_File_List,
                                       Get_Declaration_File_Of
                                         (Son_Info.all))then
                                       --  If this tagged type is declared, we
                                       --  indicate that we must print it in
                                       --  the doc file
                                       Tag_Elem_Child.Print_Me := True;
                                    else
                                       Tag_Elem_Child.Print_Me := False;
                                    end if;

                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Son_Info.all))
                                    then
                                       --  In this case, the child won't be
                                       --  analysed as an entity, so we must
                                       --  fill its list of parents
                                       --  First time we met this child, so
                                       --  no need to check if Me_Info
                                       --  still put in the list
                                       Tag_Elem_Child.Number_Of_Parents
                                         := 1;
                                       List_Entity_Handle.Append
                                         (Tag_Elem_Child.My_Parents, Me_Info);
                                    end if;

                                    if Options.Show_Private and then
                                      Get_Scope (Son_Info.all) /= Global_Scope
                                    then
                                       Type_List_Tagged_Element.Append
                                         (Private_Tagged_Types_List,
                                          Tag_Elem_Child.all);
                                    else
                                       Type_List_Tagged_Element.Append
                                         (Tagged_Types_List,
                                          Tag_Elem_Child.all);
                                    end if;
                                    --  We don't need to indicate now
                                    --  information about the father of this
                                    --  child (in fact, Me_Info is one)
                                    --  because this field will be update
                                    --  when the child will be studied as
                                    --  main tagged type (not as a son or
                                    --  parent)

                                 else
                                    if Options.Process_Body_Files
                                      and then
                                        not Is_Spec_File
                                          (Kernel, Get_Declaration_File_Of
                                                 (Son_Info.all))
                                    then
                                       if Options.Show_Private and then
                                         Get_Scope (Son_Info.all)
                                         /= Global_Scope
                                       then
                                          Add_Parent
                                            (Private_Tagged_Types_List,
                                             Son_Info,
                                             Me_Info);
                                       else
                                          Add_Parent (Tagged_Types_List,
                                                      Son_Info,
                                                      Me_Info);
                                       end if;
                                    end if;
                                 end if;

                                 List_Entity_Handle.Append
                                   (Tag_Elem_Me.My_Children, Son_Info);
                                 --  Update of the list of parents with the
                                 --  local parent.

                                 Tag_Elem_Me.Number_Of_Children :=
                                   Tag_Elem_Me.Number_Of_Children + 1;
                                 --  Update of the number of parents

                                 Next (Child);
                              end loop;

                              --  Search for all chidren (those who don't
                              --  appear in current file)
                              if Found_Global then

                                 while Get (Children_Iter) /= No_Reference loop
                                    LI := Get_LI (Children_Iter);

                                    Global_Child := Get_Children_Types (LI,
                                                                        Info);
                                    loop
                                       Global_Son := Get (Global_Child);
                                       exit when Global_Son
                                         = No_Entity_Information;

                                       --  New Child
                                       Son_Info := Find_In_List
                                         (All_Tagged_Types_List, Global_Son);
                                       if Son_Info = null then
                                          --  Child seen for the first time
                                          Son_Info
                                            := new Entity_Information'
                                              (Global_Son);
                                          List_Entity_Handle.Append
                                            (All_Tagged_Types_List, Son_Info);

                                          Tag_Elem_Child := new Tagged_Element;
                                          Tag_Elem_Child.Me := Son_Info;
                                          Tag_Elem_Child.Number_Of_Children
                                            := 0;
                                          Tag_Elem_Child.Number_Of_Parents
                                            := 0;

                                          if Source_File_In_List
                                            (Source_File_List,
                                             Get_Declaration_File_Of
                                               (Son_Info.all))then
                                             --  This tagged type is declared,
                                             --  we indicate that we must print
                                             --  it in the doc file
                                             Tag_Elem_Child.Print_Me := True;
                                          else
                                             Tag_Elem_Child.Print_Me := False;
                                          end if;

                                          if Options.Process_Body_Files
                                            and then
                                              not Is_Spec_File
                                                (Kernel,
                                                 Get_Declaration_File_Of
                                                   (Son_Info.all))
                                          then
                                             --  In this case, the child won't
                                             --  be analysed as an entity, so
                                             --  we must fill its list of
                                             --  parents
                                             --  First time we met this child,
                                             --  so no need to check if
                                             --  Me_Info still put in the list
                                             Tag_Elem_Child.Number_Of_Parents
                                               := 1;

                                             List_Entity_Handle.Append
                                               (Tag_Elem_Child.My_Parents,
                                                Me_Info);
                                          end if;

                                          if Options.Show_Private and then
                                            Get_Scope (Son_Info.all)
                                            /= Global_Scope
                                          then
                                             Type_List_Tagged_Element.Append
                                               (Private_Tagged_Types_List,
                                                Tag_Elem_Child.all);
                                          else
                                             Type_List_Tagged_Element.Append
                                               (Tagged_Types_List,
                                                Tag_Elem_Child.all);
                                          end if;

                                          --  We don't need to indicate now
                                          --  information about the father of
                                          --  this child (Me_Info is one)
                                          --  because this field will be update
                                          --  when the child will be studied as
                                          --  main tagged type (not as a son or
                                          --  parent)
                                       else
                                          if Options.Process_Body_Files
                                            and then
                                              not Is_Spec_File
                                                (Kernel,
                                                 Get_Declaration_File_Of
                                                   (Son_Info.all))
                                          then
                                             if Options.Show_Private and then
                                               Get_Scope (Son_Info.all)
                                               /= Global_Scope
                                             then
                                                Add_Parent
                                                  (Private_Tagged_Types_List,
                                                   Son_Info,
                                                   Me_Info);
                                             else
                                                Add_Parent (Tagged_Types_List,
                                                            Son_Info,
                                                            Me_Info);
                                             end if;
                                          end if;
                                       end if;

                                       List_Entity_Handle.Append
                                         (Tag_Elem_Me.My_Children, Son_Info);
                                       --  Update of the list of parents with
                                       --  the local parent.

                                       Tag_Elem_Me.Number_Of_Children :=
                                         Tag_Elem_Me.Number_Of_Children + 1;
                                       --  Update of the number of parents

                                       Next (Global_Child);
                                    end loop;

                                    Destroy (Global_Child);
                                    Next (Get_Language_Handler (Kernel),
                                          Children_Iter,
                                          LI_List);
                                 end loop;

                              end if;

                              if Options.Show_Private and then
                                Entity_Node.Is_Private
                              then
                                 Type_List_Tagged_Element.Append
                                   (Private_Tagged_Types_List,
                                    Tag_Elem_Me.all);
                              else
                                 Type_List_Tagged_Element.Append
                                   (Tagged_Types_List, Tag_Elem_Me.all);
                              end if;
                           end if;
                        end if;
                     end if;

                     --  Add to the entity list of this file
                     --  Now, Prepend is used instead of Append (cost o(1))
                     --  Besides the list Entity_List mustn't be sorted.
                     --  For instance, it's necessary in order to respect
                     --  declaration which have the following scheme:
                     --  type X;
                     --  type Y is access X;
                     --  type X is record ..... end record;
                     Type_Entity_List.Prepend (Entity_List, Entity_Node);
                  end if;
               else
                  --  New reference on the current entity

                  List_Reference_In_File.Append
                    (List_Ref_In_File,
                     (Name   =>
                      new String'(Get_Name (Get (Entity_Iter))),
                      Line   => Get_Line (Get_Location (Ref_In_File)),
                      Column => Get_Column (Get_Location (Ref_In_File)),
                      Entity => Ent_Handle));
               end if;

               --  Get next entity (or reference) in this file
               Next (Entity_Iter);
            end loop;

            Sort_List_By_Line_And_Column (List_Ref_In_File);

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
               List_Ref_In_File,
               Tagged_Types_List,
               Private_Tagged_Types_List,
               Process_Body_File,
               LI_Unit,
               Options,
               Converter,
               Doc_Directory,
               Doc_Suffix,
               Level,
               All_Scope_Tree);
         else
            Trace (Me, "LI file not found: "
                   & Base_Name (Source_Filename));  --  later Exception?
         end if;

         --  If body files are not being processed, free directly here
         if not Options.Process_Body_Files then
            TEL.Free (Entity_List);
         end if;
      end if;

      List_Entity_In_File.Free (List_Ent_In_File, True);
      List_Reference_In_File.Free (List_Ref_In_File, True);
   end Process_One_File;

   -------------------
   -- Free_All_Tree --
   -------------------

   procedure Free_All_Tree (X : in out Scope_Tree) is
   begin
      Free (X);
   end Free_All_Tree;

end Docgen.Work_On_File;
