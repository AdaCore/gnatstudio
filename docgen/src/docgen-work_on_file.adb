-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2001-2007, AdaCore                 --
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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with GNAT.OS_Lib;                   use GNAT.OS_Lib;

with Commands;                      use Commands;
with Commands.Generic_Asynchronous;
with Docgen.Backend;                use Docgen.Backend;
with Docgen.Work_On_Source;         use Docgen.Work_On_Source;
with Entities.Queries;              use Entities.Queries;
with GPS.Intl;                      use GPS.Intl;
with GPS.Kernel.Standard_Hooks;     use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Task_Manager;       use GPS.Kernel.Task_Manager;
with OS_Utils;                      use OS_Utils;
with Traces;                        use Traces;
with VFS;                           use VFS;

package body Docgen.Work_On_File is

   Me : constant Debug_Handle := Create ("Docgen.Work_On_File");

   package TEL  renames Type_Entity_List;

   type Process_One_File_Data is record
      Kernel                        : Kernel_Handle;
      Backend                       : Docgen.Backend.Backend_Handle;
      Options                       : Docgen.All_Options;
      Source_File_List              : Type_Source_File_Table.HTable;
      Source_File_Node              : Type_Source_File_Table.Iterator;
      Nb_Files                      : Natural;
      Nb_Processed                  : Natural;
      Subprogram_Index_List         : Type_Entity_List.List;
      Type_Index_List               : Type_Entity_List.List;
      Tagged_Types_List             : List_Entity_Information.List;
      Private_Subprogram_Index_List : Type_Entity_List.List;
      Private_Type_Index_List       : Type_Entity_List.List;
      Private_Tagged_Types_List     : List_Entity_Information.List;
   end record;

   procedure Free (D : in out Process_One_File_Data);
   --  Free memory associated with D

   procedure Process_One_File_Iterate
     (Data    : in out Process_One_File_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type);
   --  Perform an atomic one-file processing

   package Process_One_File_Commands is new Commands.Generic_Asynchronous
     (Data_Type => Process_One_File_Data,
      Free      => Free);
   --  Handle the one-file processing commands

   procedure Finalize_Files_Processing (Data : in out Process_One_File_Data);
   --  This procedure is in charge of the documentation generation phase
   --  that comes right after every single file has been processed.

   function Is_Tagged_Type (Info : Entity_Information) return Boolean;
   --  Whether Info is tagged type or a C++ class

   procedure Get_All_References_In_File
     (File                          : Source_File;
      File_Is_Spec                  : Boolean;
      List_Ref_In_File              : out List_Reference_In_File.List;
      Options                       : All_Options;
      Type_Index_List               : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Entity_List                   : in out Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Tagged_Types_List             : out List_Entity_Information.List;
      Private_Tagged_Types_List     : out List_Entity_Information.List);
   --  Get all entities references in File, and store their names and
   --  references in the appropriate file.
   --  Extra information is collected for each entity depending on its type.

   procedure Process_Subprogram
     (Entity_Node                   : in out Entity_List_Information;
      Options                       : All_Options;
      Current_File_Is_Spec          : Boolean;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Subprogram_Index_List         : in out Type_Entity_List.List);
   --  Fills all the entity_node information with the information still
   --  needed AND adds them to the index list (so all other information
   --  must be already provided!)

   procedure Process_Type
     (Entity_Node             : in out Entity_List_Information;
      Type_Index_List         : in out Type_Entity_List.List;
      Private_Type_Index_List : in out Type_Entity_List.List);
   --  Fills all the entity_node information with the information still
   --  needed AND adds them to the index list (so all other information
   --  must be already provided!)

   procedure Process_Private_Completion
     (Entity_Node             : in out Entity_List_Information;
      Options                 : All_Options;
      Private_Type_Index_List : in out Type_Entity_List.List;
      Entity_List             : in out Type_Entity_List.List);
   --  Process the completion of a private type

   procedure Process_Tagged_Types
     (Info                      : Entity_Information;
      Options                   : All_Options;
      Tagged_Types_List         : out List_Entity_Information.List;
      Private_Tagged_Types_List : out List_Entity_Information.List);
   --  Process tagged types in the current file

   procedure Process_New_Entity
     (Info                          : Entity_Information;
      Options                       : All_Options;
      Source_Filename               : Source_File;
      Source_Is_Spec                : Boolean;
      Type_Index_List               : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Entity_List                   : in out Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Tagged_Types_List             : out List_Entity_Information.List;
      Private_Tagged_Types_List     : out List_Entity_Information.List);
   --  Handle an entity referenced for the first time

   procedure Process_One_File
     (B                             : access Docgen.Backend.Backend'Class;
      Kernel                        : access Kernel_Handle_Record'Class;
      Result                        : in out Unbounded_String;
      Source_Filename               : Source_File;
      Unit_Name                     : String;
      Source_File_List              : in out Type_Source_File_Table.HTable;
      Options                       : All_Options;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Type_Index_List               : in out Type_Entity_List.List;
      Tagged_Types_List             : in out List_Entity_Information.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Private_Tagged_Types_List     : in out List_Entity_Information.List);
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
   --  All_Scope_Tree : hash table which contains the scope trees built. This
   --  hash table is shared by all files and finally destroyed at the end of
   --  the documentation process in Process_Files.

   --------------------
   -- Is_Tagged_Type --
   --------------------

   function Is_Tagged_Type (Info : Entity_Information) return Boolean is
      Kind      : constant E_Kind := Get_Kind (Info);
   begin
      if Kind.Is_Type then
         case Kind.Kind is
            when Class | Class_Wide =>
               return True;

            when Record_Kind =>
               --  In Ada, tagged types are classified as Record
               --  The only way to distinguish them to classic
               --  record is to search for parent and children.
               --  ??? tagged types without child and without
               --  parent don't appear in the list
               --  ??? Ada tagged types are of Class kind.

               return Get_Parent_Types (Info)'Length > 0
                 or else Get_Child_Types (Info)'Length > 0;

            when others =>
               return False;
         end case;
      else
         return False;
      end if;
   end Is_Tagged_Type;

   ------------------------
   -- Process_Subprogram --
   ------------------------

   procedure Process_Subprogram
     (Entity_Node                   : in out Entity_List_Information;
      Options                       : All_Options;
      Current_File_Is_Spec          : Boolean;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Subprogram_Index_List         : in out Type_Entity_List.List) is
   begin
      --  If defined in a spec file, add entity to the
      --  Subprogram_Index_List

      if Current_File_Is_Spec then
         if Options.Show_Private and then Entity_Node.Is_Private then
            Type_Entity_List.Append
              (Private_Subprogram_Index_List, Clone (Entity_Node));
         else
            Type_Entity_List.Append
              (Subprogram_Index_List, Clone (Entity_Node));
         end if;
      end if;
   end Process_Subprogram;

   ------------------
   -- Process_Type --
   ------------------

   procedure Process_Type
     (Entity_Node             : in out Entity_List_Information;
      Type_Index_List         : in out Type_Entity_List.List;
      Private_Type_Index_List : in out Type_Entity_List.List) is
   begin
      if not Entity_Node.Is_Private then
         Type_Entity_List.Append (Type_Index_List, Clone (Entity_Node));
      else
         Type_Entity_List.Append
           (Private_Type_Index_List, Clone (Entity_Node));
      end if;
   end Process_Type;

   --------------------------------
   -- Process_Private_Completion --
   --------------------------------

   procedure Process_Private_Completion
     (Entity_Node             : in out Entity_List_Information;
      Options                 : All_Options;
      Private_Type_Index_List : in out Type_Entity_List.List;
      Entity_List             : in out Type_Entity_List.List) is
   begin
      if Options.Show_Private
        and then not Entity_Node.Is_Private
      then
         --  For record/enum, we must search if they have
         --  private fields. In this case, we must create a new
         --  entity in order to generate its documentation. In
         --  fact, the record itself is public and if this work
         --  isn't done, only the documentation
         --  "type X is record with private" is given.
         --  The private fields are forgotten.

         declare
            Found_Private   : Boolean := False;
            Field           : Entity_Information;
            Entity_Complete : Entity_List_Information;
            Iter_Field      : Calls_Iterator :=
               Get_All_Called_Entities (Entity_Node.Entity);
         begin
            while not At_End (Iter_Field) loop
               Field := Get (Iter_Field);
               if In_Range (Get_Declaration_Of (Field), Entity_Node.Entity)
                 and then not Is_Discriminant (Field, Entity_Node.Entity)
                 and then not Get_Attributes (Field)(Global)
               then
                  Found_Private := True;
                  exit;
               end if;

               Next (Iter_Field);
            end loop;

            if Found_Private then
               Ref (Entity_Node.Entity);
               Ref (Entity_Node.Entity);
               Entity_Complete := Entity_List_Information'
                 (Kind               => Type_Entity,
                  Entity             => Entity_Node.Entity,
                  Is_Private         => True,
                  Line_In_Body       => Entity_Node.Line_In_Body,
                  Public_Declaration => Entity_Node.Entity,
                  Processed          => False);

--  ???
--             Entity_Complete.all.Entity := Create
--                (File   => Get_File (Entity_Node.Line_In_Body),
--                 Line   => Get_Line (Entity_Node.Line_In_Body),
--                 Column => Get_Column (Entity_Node.Line_In_Body),
--                 Name   => Get_Name (Entity_Node.Entity),
--                 Scope  => Get_Scope (Entity_Node.Entity),
--                 Kind   => Get_Kind (Entity_Node.Entity));

               Type_Entity_List.Prepend (Entity_List, Entity_Complete);

               --  Currently, we add the name of the record/enum type. So,
               --  this name is duplicated: it appears both in public and
               --  private part of the index list. For the future, it would
               --  be better to add the fields in the private part.

               Type_Entity_List.Append
                 (Private_Type_Index_List, Clone (Entity_Complete));
            end if;
         end;
      end if;
   end Process_Private_Completion;

   --------------------------------
   -- Get_All_References_In_File --
   --------------------------------

   procedure Get_All_References_In_File
     (File                          : Source_File;
      File_Is_Spec                  : Boolean;
      List_Ref_In_File              : out List_Reference_In_File.List;
      Options                       : All_Options;
      Type_Index_List               : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Entity_List                   : in out Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Tagged_Types_List             : out List_Entity_Information.List;
      Private_Tagged_Types_List     : out List_Entity_Information.List)
   is
      Entity_Iter : Entity_Iterator;
      Info        : Entity_Information;
      Refs        : Entity_Reference_Iterator;
      Ref         : Entity_Reference;
   begin
      Find_All_Entities_In_File (Iter => Entity_Iter, File => File);

      List_Ref_In_File := List_Reference_In_File.Null_List;

      while not At_End (Entity_Iter) loop
         Info := Get (Entity_Iter);

         if File_Is_Spec
           or else Options.Process_Body_Files
         then
            Process_New_Entity
              (Info                          => Info,
               Options                       => Options,
               Source_Filename               => File,
               Source_Is_Spec                => File_Is_Spec,
               Type_Index_List               => Type_Index_List,
               Private_Type_Index_List       => Private_Type_Index_List,
               Entity_List                   => Entity_List,
               Source_File_List              => Source_File_List,
               Private_Subprogram_Index_List => Private_Subprogram_Index_List,
               Subprogram_Index_List         => Subprogram_Index_List,
               Tagged_Types_List             => Tagged_Types_List,
               Private_Tagged_Types_List     => Private_Tagged_Types_List);

            Find_All_References
              (Iter    => Refs,
               Entity  => Info,
               In_File => File);

            while not At_End (Refs) loop
               Ref := Get (Refs);
               if Ref /= No_Entity_Reference
                 and then Get_Location (Ref).File = File
               --  Here we are only interested in references in File of
               --  entities that appear in File.
               then
                  List_Reference_In_File.Append
                    (List_Ref_In_File,
                     (Line   => Get_Line   (Get_Location (Ref)),
                      Column => Get_Column (Get_Location (Ref)),
                      Entity => Info));
               end if;
               Next (Refs);
            end loop;

            Destroy (Refs);
         end if;

         Next (Entity_Iter);
      end loop;

      Destroy (Entity_Iter);

      Sort_List_By_Line_And_Column (List_Ref_In_File);
   end Get_All_References_In_File;

   ------------------------
   -- Process_New_Entity --
   ------------------------

   procedure Process_New_Entity
     (Info                          : Entity_Information;
      Options                       : All_Options;
      Source_Filename               : Source_File;
      Source_Is_Spec                : Boolean;
      Type_Index_List               : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Entity_List                   : in out Type_Entity_List.List;
      Source_File_List              : Type_Source_File_Table.HTable;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Tagged_Types_List             : out List_Entity_Information.List;
      Private_Tagged_Types_List     : out List_Entity_Information.List)
   is
      Entity_Node : Entity_List_Information;
   begin
      --  Check if the declaration of the entity is in one of the
      --  files which are in list, if false => no need for
      --  creating links.
      --  Also check if it's a private entity and whether they
      --  should be processed.

      if (Options.Show_Private or else Get_Attributes (Info)(Global))
        and then Source_File_In_List
          (Source_File_List, Get_File (Get_Declaration_Of (Info)))
      then
         Ref (Info);
         Entity_Node := Entity_List_Information'
           (Kind               => Other_Entity,
            Entity             => Info,
            Is_Private         => not Get_Attributes (Info)(Global),
            Line_In_Body       => No_File_Location,
            Public_Declaration => null,
            Processed          => False);

         Find_Next_Body (Info, Location => Entity_Node.Line_In_Body);

         --  Get the entity specific parameters.
         --  These are the last parameters to gather, after the
         --  'case' no more changes are allowed, because the
         --  index lists are created in the subprograms used
         --  here, so all info must be avaiable.

         case Get_Kind (Info).Kind is
            when Procedure_Kind | Function_Or_Operator =>
               Entity_Node.Kind := Subprogram_Entity;

               if Source_Filename =
                 Get_File (Get_Declaration_Of (Info))
               then
                  Process_Subprogram
                    (Entity_Node           => Entity_Node,
                     Options               => Options,
                     Current_File_Is_Spec  => Source_Is_Spec,
                     Private_Subprogram_Index_List =>
                       Private_Subprogram_Index_List,
                     Subprogram_Index_List => Subprogram_Index_List);
               end if;

            when Record_Kind          | Enumeration_Kind
               | Access_Kind          | Array_Kind
               | Boolean_Kind         | String_Kind
               | Decimal_Fixed_Point  | Class_Wide
               | Floating_Point       | Modular_Integer
               | Ordinary_Fixed_Point | Private_Type
               | Protected_Kind       | Signed_Integer
               | Named_Number =>

               if Get_Kind (Info).Is_Type then
                  Entity_Node.Kind := Type_Entity;

                  if Source_Filename = Get_File (Get_Declaration_Of (Info))
                    and then Source_Is_Spec
                  then
                     Process_Type
                       (Entity_Node             => Entity_Node,
                        Type_Index_List         => Type_Index_List,
                        Private_Type_Index_List => Private_Type_Index_List);
                     Process_Private_Completion
                       (Entity_Node             => Entity_Node,
                        Options                 => Options,
                        Private_Type_Index_List => Private_Type_Index_List,
                        Entity_List             => Entity_List);
                  end if;
               else
                  Entity_Node.Kind := Var_Entity;
               end if;

            when Exception_Entity => Entity_Node.Kind := Exception_Entity;
            when Task_Kind        => Entity_Node.Kind := Entry_Entity;
            when Package_Kind     => Entity_Node.Kind := Package_Entity;
            when others           => Entity_Node.Kind := Other_Entity;
         end case;

         if Options.Tagged_Types
           and then Is_Tagged_Type (Info)
           and then Get_File (Get_Declaration_Of (Info)) = Source_Filename
         then
            Process_Tagged_Types
              (Info                      => Entity_Node.Entity,
               Options                   => Options,
               Tagged_Types_List         => Tagged_Types_List,
               Private_Tagged_Types_List => Private_Tagged_Types_List);
         end if;

         --  Prepend entities to the list, so that we properly handle
         --  the following scheme:
         --     type X;
         --     type Y is access X;
         --     type X is record ..... end record;
         Type_Entity_List.Prepend (Entity_List, Entity_Node);
      end if;
   end Process_New_Entity;

   --------------------------
   -- Process_Tagged_Types --
   --------------------------

   procedure Process_Tagged_Types
     (Info                      : Entity_Information;
      Options                   : All_Options;
      Tagged_Types_List         : out List_Entity_Information.List;
      Private_Tagged_Types_List : out List_Entity_Information.List) is
   begin
      if Options.Show_Private and then not Get_Attributes (Info)(Global) then
         List_Entity_Information.Append (Private_Tagged_Types_List, Info);
      else
         List_Entity_Information.Append (Tagged_Types_List, Info);
      end if;
   end Process_Tagged_Types;

   -------------------------------
   -- Finalize_Files_Processing --
   -------------------------------

   procedure Finalize_Files_Processing (Data : in out Process_One_File_Data) is
      Level : constant Natural := 1;
   begin
      --  Sort the type index list and the subprogram index list first (both
      --  for private and public lists)
      Sort_List_Name (Data.Subprogram_Index_List);
      Sort_List_Name (Data.Type_Index_List);
      Sort_List_Name (Data.Private_Subprogram_Index_List);
      Sort_List_Name (Data.Private_Type_Index_List);

      --  Create the index doc files for the packages
      Process_Unit_Index
        (Data.Backend,
         Data.Kernel,
         Data.Source_File_List,
         Data.Options,
         Level);
      Process_Subprogram_Index
        (Data.Backend,
         Data.Kernel,
         Data.Subprogram_Index_List,
         Data.Private_Subprogram_Index_List,
         Data.Source_File_List,
         Data.Options);
      Process_Type_Index
        (Data.Backend,
         Data.Kernel,
         Data.Type_Index_List,
         Data.Private_Type_Index_List,
         Data.Source_File_List,
         Data.Options);

      if Data.Options.Tagged_Types then
         Sort_List_Name (Data.Tagged_Types_List);

         if Data.Options.Show_Private then
            Sort_List_Name (Data.Private_Tagged_Types_List);
         end if;

         Process_Tagged_Type_Index
           (Data.Backend,
            Data.Kernel,
            Data.Tagged_Types_List,
            Data.Private_Tagged_Types_List,
            Data.Source_File_List,
            Data.Options);
      end if;

      TEL.Free (Data.Subprogram_Index_List);
      TEL.Free (Data.Type_Index_List);
      List_Entity_Information.Free (Data.Tagged_Types_List);

      Type_Source_File_Table.Reset (Data.Source_File_List);

      Open_Html
        (Data.Kernel,
         URL_Or_File => Get_Doc_Directory (Data.Backend, Data.Kernel)
            & "index" & Get_Extension (Data.Backend));

      Pop_State (Data.Kernel);
      Free (Data);
   end Finalize_Files_Processing;

   ------------------------------
   -- Process_One_File_Iterate --
   ------------------------------

   procedure Process_One_File_Iterate
     (Data    : in out Process_One_File_Data;
      Command : Command_Access;
      Result  : out Command_Return_Type)
   is
      use List_Entity_Information;
      use Type_Source_File_Table;

      Doc_Directory : constant String :=
                        Docgen.Backend.Get_Doc_Directory
                          (Data.Backend, Data.Kernel);

   begin
      if Get_Element (Data.Source_File_Node) /= No_Source_File_Information then
         declare
            Current_Unit   : constant GNAT.Strings.String_Access :=
                               Get_Element (Data.Source_File_Node).Unit_Name;
            File           : constant Source_File :=
                               Get_Key (Data.Source_File_Node);
            Doc_File       : constant File_Descriptor :=
                               Create_File
                                 (Doc_Directory &
                                  Get_Element
                                    (Data.Source_File_Node).Doc_File_Name.all,
                                  Binary);

            Process_Result : Unbounded_String;
         begin

            Process_One_File
              (Data.Backend,
               Data.Kernel,
               Process_Result,
               Source_Filename               => File,
               Unit_Name                     => Current_Unit.all,
               Source_File_List              => Data.Source_File_List,
               Options                       => Data.Options,
               Subprogram_Index_List         => Data.Subprogram_Index_List,
               Type_Index_List               => Data.Type_Index_List,
               Tagged_Types_List             => Data.Tagged_Types_List,
               Private_Subprogram_Index_List =>
                 Data.Private_Subprogram_Index_List,
               Private_Type_Index_List       => Data.Private_Type_Index_List,
               Private_Tagged_Types_List     =>
                 Data.Private_Tagged_Types_List);

            --  Write the result to the doc file.

            Put_Line (Doc_File, To_String (Process_Result));
            Close (Doc_File);

            if Get_Element (Data.Source_File_Node) /=
              No_Source_File_Information
            then
               Get_Next (Data.Source_File_List, Data.Source_File_Node);
            end if;
         end;

         Data.Nb_Processed := Data.Nb_Processed + 1;

         Set_Progress
           (Command,
            (Running,
             Data.Nb_Processed,
             Data.Nb_Files));

         Result := Execute_Again;
      else
         Finalize_Files_Processing (Data);
         Result := Success;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Data.Kernel);
         Result := Failure;
         return;
   end Process_One_File_Iterate;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Process_One_File_Data) is
      pragma Unreferenced (D);
   begin
      null;
   end Free;

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files
     (B                : access Docgen.Backend.Backend'Class;
      Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Source_File_List : in out Docgen.Type_Source_File_Table.HTable;
      Nb_Files         : Natural;
      Options          : Docgen.All_Options)
   is
      use List_Entity_Information;
      use Type_Source_File_Table;

      Source_File_Node              : Type_Source_File_Table.Iterator;
      Subprogram_Index_List         : Type_Entity_List.List;
      Type_Index_List               : Type_Entity_List.List;
      Tagged_Types_List             : List_Entity_Information.List;
      Private_Subprogram_Index_List : Type_Entity_List.List;
      Private_Type_Index_List       : Type_Entity_List.List;
      Private_Tagged_Types_List     : List_Entity_Information.List;

      C : Process_One_File_Commands.Generic_Asynchronous_Command_Access;

   begin
      Get_First (Source_File_List, Source_File_Node);

      Process_One_File_Commands.Create
        (C,
         -"Generating Doc",
         (Kernel                        => Kernel_Handle (Kernel),
          Backend                       => Backend.Backend_Handle (B),
          Options                       => Options,
          Source_File_List              => Source_File_List,
          Source_File_Node              => Source_File_Node,
          Nb_Files                      => Nb_Files,
          Nb_Processed                  => 0,
          Subprogram_Index_List         => Subprogram_Index_List,
          Type_Index_List               => Type_Index_List,
          Tagged_Types_List             => Tagged_Types_List,
          Private_Subprogram_Index_List => Private_Subprogram_Index_List,
          Private_Type_Index_List       => Private_Type_Index_List,
          Private_Tagged_Types_List     => Private_Tagged_Types_List),
         Process_One_File_Iterate'Access);

      Launch_Background_Command
        (Kernel, Command_Access (C), True, True, "Generating Doc");
   end Process_Files;

   ----------------------
   -- Process_One_File --
   ----------------------

   procedure Process_One_File
     (B                             : access Docgen.Backend.Backend'Class;
      Kernel                        : access Kernel_Handle_Record'Class;
      Result                        : in out Unbounded_String;
      Source_Filename               : Source_File;
      Unit_Name                     : String;
      Source_File_List              : in out Type_Source_File_Table.HTable;
      Options                       : All_Options;
      Subprogram_Index_List         : in out Type_Entity_List.List;
      Type_Index_List               : in out Type_Entity_List.List;
      Tagged_Types_List             : in out List_Entity_Information.List;
      Private_Subprogram_Index_List : in out Type_Entity_List.List;
      Private_Type_Index_List       : in out Type_Entity_List.List;
      Private_Tagged_Types_List     : in out List_Entity_Information.List)
   is
      Entity_List      : Type_Entity_List.List;
      List_Ref_In_File : List_Reference_In_File.List;
      Is_Spec          : constant Boolean :=
         Is_Spec_File (Kernel, Get_Filename (Source_Filename));

      Level            : Natural := 1;
      --  Stores the level of the current package in which we are
      --  processing types, subprograms...

      Reporter         : constant File_Error_Reporter :=
                           new Docgen_Error_Reporter_Record'
                             (Kernel_Handle (Kernel), False);
   begin
      Trace
        (Me,
         "Generating doc for "
         & Full_Name (Get_Filename (Source_Filename)).all);

      Update_Xref (Source_Filename, Reporter);

      Get_All_References_In_File
        (File                          => Source_Filename,
         File_Is_Spec                  => Is_Spec,
         List_Ref_In_File              => List_Ref_In_File,
         Options                       => Options,
         Type_Index_List               => Type_Index_List,
         Private_Type_Index_List       => Private_Type_Index_List,
         Entity_List                   => Entity_List,
         Source_File_List              => Source_File_List,
         Private_Subprogram_Index_List => Private_Subprogram_Index_List,
         Subprogram_Index_List         => Subprogram_Index_List,
         Tagged_Types_List             => Tagged_Types_List,
         Private_Tagged_Types_List     => Private_Tagged_Types_List);

      Process_Source
        (B, Kernel, Result,
         Source_File_List,
         Get_Filename (Source_Filename),
         Is_Spec,
         Unit_Name,
         Entity_List,
         List_Ref_In_File,
         Tagged_Types_List,
         Private_Tagged_Types_List,
         Options,
         Level);

      TEL.Free (Entity_List);
      List_Reference_In_File.Free (List_Ref_In_File, True);
   end Process_One_File;

end Docgen.Work_On_File;
