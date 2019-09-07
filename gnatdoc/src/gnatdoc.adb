------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Config;
with GNATCOLL.Iconv;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GPS.Intl;                use GPS.Intl;
with GPS.Messages_Windows;    use GPS.Messages_Windows;
with GNATdoc.Atree;           use GNATdoc.Atree;
with GNATdoc.Backend;         use GNATdoc.Backend;
with GNATdoc.Frontend;        use GNATdoc.Frontend;
with GNATdoc.Project_Environments;
with GNATdoc.Time;
with GNATdoc.Treepr;
with GNATdoc.Utils;           use GNATdoc.Utils;
with Language;                use Language;
with Language.Ada;
with Language.C;
with Templates_Parser;        use Templates_Parser;
with UTF8_Utils;
with Xref.Docgen;             use Xref.Docgen;

with GNAT.IO;

package body GNATdoc is
   Me : constant Trace_Handle := Create ("GNATdoc.1");

   Old_Documentation_Dir_Attribute : constant Attribute_Pkg_String :=
     Build (Ide_Package, "documentation_dir");

   Debug_Unit : constant String := " disabled";
   --  File name without any extension. Used to process only the given unit
   --  (spec and body).

   Debug_Unit_Disabled : constant Boolean := Debug_Unit (1) = ' ';
   --  Indicates if Debug_Unit is not set

   Resilient_Mode : constant Boolean := True;
   --  Continue processing files even in case of serious errors.

   Serious_Errors : Natural := 0;
   --  Number of serious errors reported

   Skipped_Files : Files_List.Vector;
   --  Files that could not be processed by GNATdoc

   -----------------------
   -- Local Subprograms --
   -----------------------

   function In_Ada_Lang
     (Context : access constant Docgen_Context;
      File    : Virtual_File) return Boolean;
   --  Return True if the file corresponds with an Ada source file.

   procedure Log_Line
     (Quiet_Mode : Boolean;
      Text       : String);
   --  Output Text if Quiet_Mode is False

   procedure Process_Files
     (Kernel              : Core_Kernel;
      Options             : Docgen_Options;
      Project             : Project_Type;
      Prj_Files           : in out Project_Files_List.Vector;
      Update_Global_Index : Boolean);
   --  This subprogram factorizes the functionality shared by routines
   --  Process_Single_File and Process_Project_Files. It processes all
   --  the files in Src_Files and generates their documentation.

   -----------------
   -- In_Ada_Lang --
   -----------------

   function In_Ada_Lang
     (Context : access constant Docgen_Context;
      File    : Virtual_File) return Boolean
   is
      Lang : constant Language_Access :=
               Get_Language_From_File (Context.Lang_Handler, File);
   begin
      return Lang.all in Language.Ada.Ada_Language'Class;
   end In_Ada_Lang;

   --------------
   -- Log_Line --
   --------------

   procedure Log_Line
     (Quiet_Mode : Boolean;
      Text       : String) is
   begin
      if not Quiet_Mode then
         GNAT.IO.Put_Line (Text);
      end if;
   end Log_Line;

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files
     (Kernel              : Core_Kernel;
      Options             : Docgen_Options;
      Project             : Project_Type;
      Prj_Files           : in out Project_Files_List.Vector;
      Update_Global_Index : Boolean)
   is
      Backend        : GNATdoc_Backend'Class :=
                         New_Backend (To_String (Options.Backend_Name));
      Database       : constant General_Xref_Database := Kernel.Databases;
      Lang_Handler   : constant Language_Handler := Kernel.Lang_Handler;
      Error_Reported : Boolean := False;

      procedure Build_Trees_And_Call_Backend
        (Context           : access constant Docgen_Context;
         All_Src_Files     : in out Files_List.Vector;
         All_Include_Files : in out Files_List.Vector);
      --  Build the tree of all the sources, set the inheritance depth level of
      --  all the tagged types and invoke the backend with each tree.

      procedure Check_Src_Files;
      --  Check the source files of Prj_Files and remove those files which can
      --  not be processed.

      function Collect_All_Src_Files
        (Prj_Files     : Project_Files_List.Vector;
         Include_Files : Files_List.Vector)
        return Files_List.Vector;
      --  Collect all the files of the project in a single list and append to
      --  the resulting list all the files in the Include_Files list.

      procedure Collect_C_Header_Files
        (Direct_Include_Files : access Files_List.Vector;
         All_Include_Files    : access Files_List.Vector);
      --  Collect in Direct_Include_Files all the C/C++ header files referenced
      --  directly by files of Prj_Files, and collect in All_Include_Files all
      --  the C/C++ header files which are transitively referenced by all the
      --  sources of Prj_Files.

      procedure Compute_Dependencies
        (File : Virtual_File; Files : in out Files_List.Vector);
      --  Compute the transitive list of dependencies of File using only the
      --  files contained in this list of files

      function Is_Large_Project
        (Files : in out Files_List.Vector) return Boolean;
      --  Return true if the project is large enough to enable additional
      --  messages.

      function Have_Files
        (Prj_Files : Project_Files_List.Vector) return Boolean;
      --  Return true if some project in Prj_Files has a file to be processed

      type Stages is
        (Unknown_Stage, Frontend_Stage, Backend_Stage, TreeOutput_Stage);

      procedure Report_Error
        (All_Src_Files : in out Files_List.Vector;
         File          : Virtual_File;
         Stage         : Stages);
      --  Output a banner describing the error detected processing File plus
      --  the list of dependencies of File.

      procedure Sort_Dependencies (Files : in out Files_List.Vector);
      --  Sort the list of files to ensure that dependent files are located
      --  in the resulting list after the files from which they depend.

      ----------------------------------
      -- Build_Trees_And_Call_Backend --
      ----------------------------------

      procedure Build_Trees_And_Call_Backend
        (Context           : access constant Docgen_Context;
         All_Src_Files     : in out Files_List.Vector;
         All_Include_Files : in out Files_List.Vector)
      is
         All_Trees          : Frontend.Tree_List.Vector;
         Files_Without_Tree : Files_List.Vector;

         type Categories is (Specs, Bodies);
         --  Files are grouped in these two categories: all Specs are processed
         --  before Bodies (to ensure that tree bodies have available all info
         --  required from the specs).

         procedure Build_Trees_In_Category (Category : Categories);
         --  Traverse All_Src_Files building the tree of files in the specified
         --  Category and storing them in All_Trees. Files_Without_Tree stores
         --  files of the given Category whose tree could not be built.

         procedure Call_Backend;
         --  Invoke the backend with All_Trees

         procedure Check_Duplicated_Full_Names;
         --  Check that compilation units of All_Trees have different full
         --  names

         procedure Check_Trees;
         --  Verify that trees stored in All_Trees have a minimum decoration.

         procedure Output_Trees_And_Comment_Files;
         --  Generate internal files containing the trees and the documentation
         --  extracted for each entity.

         procedure Remove_Files_Without_Tree;
         --  Remove from the All_Src_Files list those files on which we could
         --  not build the tree. Required to avoid reporting spurious warnings
         --  on entities not fully decorated.

         procedure Set_Tagged_Types_Idepth;
         --  Set the inheritance depth level of all the tagged types defined
         --  in the trees stored in All_Trees.

         function Spec_Tree_Of_Body (File : Virtual_File) return Tree_Type;
         --  Search in All_Trees for the tree containing the spec of the given
         --  File. Return No_Tree if not available.

         -----------------------------
         -- Build_Trees_In_Category --
         -----------------------------

         Count_Files : Natural := 0;
         --  Number of built trees. Used to generate the log displaying the
         --  evolution of GNATdoc when building the trees.

         procedure Build_Trees_In_Category (Category : Categories) is

            function In_Selected_Category
              (File : GNATCOLL.VFS.Virtual_File) return Boolean;
            function In_Selected_Category
              (File : GNATCOLL.VFS.Virtual_File) return Boolean is
            begin
               case Category is
                  when Specs =>
                     return Is_Spec_File (Kernel, File)
                       and then
                         (Debug_Unit_Disabled
                            or else (+File.Base_Name) = Debug_Unit & ".ads");
                  when Bodies =>
                     return not Is_Spec_File (Kernel, File)
                       and then In_Ada_Lang (Context, File)
                       and then
                         (Debug_Unit_Disabled
                            or else (+File.Base_Name) = Debug_Unit & ".adb");
               end case;
            end In_Selected_Category;

            --  Local variables

            P_Tree    : Project_Tree_Access renames Kernel.Registry.Tree;
            Num_Files : constant Natural := Natural (All_Src_Files.Length);

         --  Start of processing for Build_Trees_In_Category

         begin
            for File of All_Src_Files loop
               if In_Selected_Category (File) then
                  declare
                     Tree      : aliased Tree_Type;
                     Tree_Spec : aliased Tree_Type;
                     Spec_File : Virtual_File;

                  begin
                     Count_Files := Count_Files + 1;
                     Log_Line (Options.Quiet_Mode,
                       Count_Files'Img
                       & "/" & To_String (Num_Files)
                       & ": " & (+File.Base_Name));

                     case Category is
                        when Specs =>
                           Tree :=
                             Frontend.Build_Tree
                               (Context => Context,
                                File    => File);

                        when Bodies =>
                           Spec_File := P_Tree.Other_File (File);

                           if not Files_Without_Tree.Contains (Spec_File)
                             and then not Skipped_Files.Contains (Spec_File)
                           then
                              Tree_Spec := Spec_Tree_Of_Body (File);
                              Tree :=
                                Frontend.Build_Tree
                                  (Context   => Context,
                                   File      => File,
                                   Tree_Spec => Tree_Spec'Access);
                           end if;
                     end case;

                     if Tree /= No_Tree
                       and then not Tree.Is_Separate_Unit
                     then
                        All_Trees.Append (Tree);
                     else
                        Files_Without_Tree.Append (File);
                     end if;
                  exception
                     when E : others =>
                     Trace (Me, E);

                     if Resilient_Mode then
                        GNAT.IO.Put_Line
                          (">>> GNATdoc internal error: "
                             & "internal problem processing "
                             & (+File.Base_Name));
                        Files_Without_Tree.Append (File);
                     else
                        Report_Error (All_Src_Files, File, Frontend_Stage);
                        raise;
                     end if;
                  end;
               end if;
            end loop;
         end Build_Trees_In_Category;

         ------------------
         -- Call_Backend --
         ------------------

         procedure Call_Backend is
            Num_Trees : constant Natural := Natural (All_Trees.Length);
            Count     : Natural := 0;

         begin
            for Tree of All_Trees loop
               Count := Count + 1;

               if Num_Trees > 600 and then (Count mod 300) = 0 then
                  Log_Line (Options.Quiet_Mode,
                    Count'Img & "/" & To_String (Num_Trees) & " files");
               end if;

               begin
                  Backend.Process_File (Tree'Access);

               exception
                  when E : others =>
                     if Resilient_Mode then
                        GNAT.IO.Put_Line
                          (">>> GNATdoc internal error: "
                             & "internal problem processing "
                             & (+Tree.File.Base_Name));
                     else
                        Report_Error (All_Src_Files, Tree.File, Backend_Stage);
                        Trace (Me, E);
                        raise;
                     end if;
               end;
            end loop;
         end Call_Backend;

         ---------------------------------
         -- Check_Duplicated_Full_Names --
         ---------------------------------

         procedure Check_Duplicated_Full_Names is
            Units : EInfo_List.Vector;

            Cursor : EInfo_List.Cursor;
            Prev_E : Entity_Id;
            Entity : Entity_Id;
            use type Ada.Containers.Count_Type;
         begin
            for Tree of All_Trees loop
               declare
                  Root : Entity_Id renames Tree.Tree_Root;
               begin
                  if In_Ada_Lang (Context, Tree.File) then
                     pragma Assert (Is_Standard_Entity (Root));

                     for E of Get_Entities (Root).all loop
                        if Is_Compilation_Unit (E) then
                           Units.Append (E);
                           exit;
                        else
                           pragma Assert (Is_Generic_Formal (E));
                        end if;
                     end loop;
                  end if;
               end;
            end loop;

            --  No test needed if we have a single unit (or none in case of
            --  serious error)

            if Units.Length < 2 then
               return;
            end if;

            EInfo_Vector_Sort_Full.Sort (Units);

            Cursor := Units.First;
            Prev_E := EInfo_List.Element (Cursor);
            EInfo_List.Next (Cursor);

            while EInfo_List.Has_Element (Cursor) loop
               Entity := EInfo_List.Element (Cursor);

               pragma Assert (Get_Full_Name (Prev_E) /= Get_Full_Name (Entity)
                 or else
                   (Present (Get_Corresponding_Spec (Entity))
                      and then Get_Corresponding_Spec (Entity) = Prev_E)
                 or else
                   (Present (Get_Corresponding_Spec (Prev_E))
                      and then Get_Corresponding_Spec (Prev_E) = Entity));

               Prev_E := Entity;
               EInfo_List.Next (Cursor);
            end loop;
         end Check_Duplicated_Full_Names;

         -----------------
         -- Check_Trees --
         -----------------

         procedure Check_Trees is

            procedure Check_Tree (Tree : Tree_Type);
            --  Verify that Tree nodes have a minimum decoration.

            ----------------
            -- Check_Tree --
            ----------------

            procedure Check_Tree (Tree : Tree_Type) is

               function Check_Node
                 (Entity      : Entity_Id;
                  Scope_Level : Natural) return Traverse_Result;
               --  Check a single node

               ----------------
               -- Check_Node --
               ----------------

               function Check_Node
                 (Entity      : Entity_Id;
                  Scope_Level : Natural) return Traverse_Result
               is
                  pragma Unreferenced (Scope_Level);

                  procedure Check (Entity : Entity_Id);
                  procedure Check_List (List : EInfo_List.Vector);

                  -----------
                  -- Check --
                  -----------

                  procedure Check (Entity : Entity_Id) is

                     procedure Error_Msg (Msg : String);
                     procedure Error_Msg (Msg : String) is
                        Debug : constant Boolean := False;
                     begin
                        if not Skipped_Files.Contains
                             (LL.Get_Location (Entity).File)
                        then
                           GNAT.IO.Put_Line
                             ("Warning: " & Image (LL.Get_Location (Entity))
                                & ": may not be documented");

                           if Debug then
                              Serious_Errors := Serious_Errors + 1;

                              GNAT.IO.Put_Line
                                (Image (LL.Get_Location (Entity))
                                   & ":"
                                   & Get_Short_Name (Entity)
                                   & ":"
                                   & Msg);
                              Atree.pns (Entity);

                              if Present (Get_Full_View (Entity)) then
                                 Atree.pn (Get_Full_View (Entity));
                              end if;
                           end if;
                        end if;
                     end Error_Msg;

                  begin
                     --  We cannot verify nodes which are not fully decorated
                     --  because they are not part of the project (for example,
                     --  entities defined in the runtime of the compiler)

                     if not Is_Decorated (Entity) then

                        --  Skip this check on internal entities built by the
                        --  frontend to workaround misssing entities not
                        --  available through Xref

                        if Present (Get_Corresponding_Spec (Entity)) then
                           null;

                        elsif Is_Internal (Entity)
                          or else Is_Skipped (Entity)
                        then
                           null;

                        --  Report the error only on those files that are part
                        --  of the project.

                        elsif All_Src_Files.Contains
                                (LL.Get_Location (Entity).File)
                        then
                           Error_Msg ("missing full decoration");
                        end if;
                     else
                        if No (Get_Scope (Entity))
                          and then not Is_Standard_Entity (Entity)
                        then
                           --  Entities defined in package bodies are not fully
                           --  decorated

                           if not Is_Spec_File
                                    (Context.Kernel,
                                     LL.Get_Location (Entity).File)
                           then
                              null;
                           else
                              Error_Msg
                                ("missing scope decoration (decorated)");
                           end if;

                        elsif Present (Get_Alias (Entity))
                          and then No (Get_Scope (Get_Alias (Entity)))

                           --  Report the error only on those files that are
                           --  part of the project.

                          and then All_Src_Files.Contains
                                    (LL.Get_Location (Get_Alias (Entity)).File)
                        then
                           Error_Msg ("missing scope decoration in alias");

                        elsif Has_Duplicated_Entities
                                (LL.Get_Parent_Types (Entity).all)
                        then
                           Error_Msg ("duplicated entities (Parent_Types)");

                        elsif Has_Duplicated_Entities
                                (Get_Progenitors (Entity).all)
                        then
                           Error_Msg ("duplicated entities (Progenitors)");

                        elsif Has_Duplicated_Entities
                                (Get_Entities (Entity).all)
                        then
                           Error_Msg ("duplicated entities (Entities)");

                        elsif Has_Duplicated_Entities
                                (Get_Inherited_Methods (Entity).all)
                        then
                           Error_Msg
                             ("duplicated entities (Inherited_Methods)");

                        elsif Has_Duplicated_Entities
                                (Get_Methods (Entity).all)
                        then
                           Error_Msg ("duplicated entities (Methods)");

                        elsif Has_Duplicated_Entities
                                (Get_Direct_Derivations (Entity).all)
                        then
                           Error_Msg
                             ("duplicated entities (Direct derivations)");

                        elsif Has_Duplicated_Entities
                                (LL.Get_Child_Types (Entity).all)
                        then
                           Error_Msg ("duplicated entities (Child types)");
                        end if;
                     end if;
                  end Check;

                  ----------------
                  -- Check_List --
                  ----------------

                  procedure Check_List (List : EInfo_List.Vector) is
                     Cursor : EInfo_List.Cursor;
                  begin
                     if not EInfo_List.Has_Element (List.First) then
                        return;
                     end if;

                     Cursor := List.First;
                     while EInfo_List.Has_Element (Cursor) loop
                        Check (EInfo_List.Element (Cursor));
                        EInfo_List.Next (Cursor);
                     end loop;
                  end Check_List;

               begin
                  Check (Entity);

                  Check_List (LL.Get_Parent_Types (Entity).all);
                  Check_List (LL.Get_Child_Types (Entity).all);

                  return OK;
               end Check_Node;

               --  Local variables

               Root   : Entity_Id renames Tree.Tree_Root;
               Root_E : Entity_Id := Atree.No_Entity;

            --  Start of processing for Check_Tree

            begin
               --  At current stage we can check only trees of Ada sources

               if not In_Ada_Lang (Context, Tree.File) then
                  return;
               end if;

               declare
                  C : constant EInfo_List.Cursor := Get_Entities (Root).First;
               begin
                  if EInfo_List.Has_Element (C) then
                     Root_E := EInfo_List.Element (C);
                  end if;
               end;

               if No (Root_E) then
                  return;
               end if;

               Traverse_Tree (Root_E, Check_Node'Access);
            end Check_Tree;

         --  Start of processing for Check_Trees

         begin
            --  For large projects enable an extra output

            if Is_Large_Project (All_Src_Files) then
               Log_Line (Options.Quiet_Mode, "Checking consistency");
            end if;

            --  Verify the trees. This cannot be done before because as part of
            --  decorating a tree the frontend may complete the decoration of
            --  entities defined in other trees (for example, the decoration of
            --  a derived type completes the decoration of its parent type,
            --  which may be located in another file ---and hence in another
            --  tree).

            for Tree of All_Trees loop
               if not Skipped_Files.Contains (Tree.File) then
                  Check_Tree (Tree);
               end if;
            end loop;
         end Check_Trees;

         ------------------------------------
         -- Output_Trees_And_Comment_Files --
         ------------------------------------

         procedure Output_Trees_And_Comment_Files is
            Num_Files : constant Natural := Natural (All_Src_Files.Length);

            Cursor           : Tree_List.Cursor;
            Tree             : aliased Tree_Type;
            Is_C_Header_File : Boolean;
            Files_Count      : Natural := 0;
         begin
            Cursor := All_Trees.First;
            while Tree_List.Has_Element (Cursor) loop
               begin
                  Files_Count := Files_Count + 1;

                  if Num_Files > 600
                    and then (Files_Count mod 300) = 0
                  then
                     Log_Line (Options.Quiet_Mode,
                       Files_Count'Img
                       & "/"
                       & To_String (Num_Files)
                       & " files");
                  end if;

                  Tree := Tree_List.Element (Cursor);

                  --  We cannot rely on the service Get_Language_From_File
                  --  because it does not work well with C/C++ header files

                  Is_C_Header_File :=
                    All_Include_Files.Contains (Tree.File);

                  --  Target dependant include files generate supurious
                  --  output differences (even processing only the
                  --  files which are directly included from the C body
                  --  files referenced in the project ---available in
                  --  Direct_Include_Files). For this reason we disable
                  --  their output. More work will be needed in this area
                  --  when we decide to provide full support of gnatdoc for
                  --  C/C++ files.

                  if not Is_C_Header_File then
                     if Options.Tree_Output.Kind /= None then
                        if Options.Tree_Output.Kind = Short then
                           Treepr.Print_Short_Tree
                             (Context     => Context,
                              Tree        => Tree'Access,
                              With_Scopes => True);
                        else
                           Treepr.Print_Full_Tree
                             (Context     => Context,
                              Tree        => Tree'Access,
                              With_Scopes => True);
                        end if;
                     end if;

                     if Options.Output_Comments then
                        Treepr.Print_Comments
                          (Context => Context,
                           Tree    => Tree'Access);
                     end if;
                  end if;

               exception
                  when E : others =>
                     Report_Error (All_Src_Files, Tree.File, TreeOutput_Stage);
                     Trace (Me, E);
                     raise;
               end;

               Tree_List.Next (Cursor);
            end loop;
         end Output_Trees_And_Comment_Files;

         -------------------------------
         -- Remove_Files_Without_Tree --
         -------------------------------

         procedure Remove_Files_Without_Tree is
         begin
            for File of Files_Without_Tree loop
               declare
                  Cursor : Files_List.Cursor;
               begin
                  Cursor := All_Src_Files.Find (File);

                  if Files_List.Has_Element (Cursor) then
                     All_Src_Files.Delete (Cursor);
                  end if;
               end;
            end loop;
         end Remove_Files_Without_Tree;

         -----------------------------
         -- Set_Tagged_Types_Idepth --
         -----------------------------

         procedure Set_Tagged_Types_Idepth is
            procedure Set_Idepth (E : Entity_Id);
            procedure Set_Idepth (E : Entity_Id) is
            begin
               if Is_Tagged (E) then
                  Set_IDepth_Level (E);
               end if;
            end Set_Idepth;

         begin
            for Tree of All_Trees loop
               For_All (Tree.All_Entities, Set_Idepth'Access);
            end loop;
         end Set_Tagged_Types_Idepth;

         -----------------------
         -- Spec_Tree_Of_Body --
         -----------------------

         function Spec_Tree_Of_Body (File : Virtual_File) return Tree_Type is
            Root_E : Entity_Id;

         begin
            for Tree of All_Trees loop
               Root_E := Ada_Compilation_Unit (Tree);

               if Present (Root_E)
                 and then Present (LL.Get_Body_Loc (Root_E))
                 and then LL.Get_Body_Loc (Root_E).File = File
               then
                  return Tree;
               end if;
            end loop;

            return No_Tree;
         end Spec_Tree_Of_Body;

      --  Start of processing for Build_Trees_And_Call_Backend

      begin
         Trace (Me, "Build_Trees_And_Call_Backend");

         Build_Trees_In_Category (Specs);
         Build_Trees_In_Category (Bodies);
         Remove_Files_Without_Tree;

         if Tree_List.Has_Element (All_Trees.First) then
            Check_Trees;
            Output_Trees_And_Comment_Files;

            --  No need to continue if we found serious errors in the tree
            --  since the backend may crash!

            if Serious_Errors > 0 then
               return;
            end if;

            Check_Duplicated_Full_Names;

            --  Set the inheritance depth level of tagged types. This cannot
            --  be done before because files are not processed following
            --  their order of dependencies (and thus a file containing a
            --  child type may be processed by the frontend before the file
            --  containing its parent type).

            Set_Tagged_Types_Idepth;

            --  For large projects enable an extra output

            if Is_Large_Project (All_Src_Files) then
               Log_Line (Options.Quiet_Mode, "Generating documentation");
            end if;

            --  Invoke the backend with all the trees built by the frontend
            --  (stored in All_Trees).

            Call_Backend;
         end if;
      end Build_Trees_And_Call_Backend;

      ---------------------
      -- Check_Src_Files --
      ---------------------

      procedure Check_Src_Files is
         Ignored_Files : Files_List.Vector;

         procedure Init_Ignored_Files;
         --  Initialize the contents of the list of Ignored_Files

         function In_Ignored_Files (Base_Name : String) return Boolean;
         --  Search for Base_Name in the list of ignored files and return
         --  true if found.

         function Skip_File (File_Index : Files_List.Cursor) return Boolean;
         --  Return True if the file Src_Files (File_Index) cannot be
         --  processed.

         ------------------------
         -- Init_Ignored_Files --
         ------------------------

         procedure Init_Ignored_Files is
            use type GNAT.Strings.String_Access;
         begin
            if Options.Ignore_Files = null
              or else Options.Ignore_Files.all = ""
            then
               return;
            end if;

            --  Traverse the string containing the list of ignored files. The
            --  supported separators of the list are comma and space.

            declare
               S    : String renames Options.Ignore_Files.all;
               From : Natural;
               J    : Natural;

            begin
               J := S'First;
               while J < S'Last loop
                  while J <= S'Last
                    and then (S (J) = ' ' or else S (J) = ',')
                  loop
                     J := J + 1;
                  end loop;

                  exit when J > S'Last;

                  From := J;

                  while J <= S'Last
                    and then S (J) /= ' '
                    and then S (J) /= ','
                  loop
                     J := J + 1;
                  end loop;

                  Ignored_Files.Append
                    (GNATCOLL.VFS.Create (+S (From .. J - 1)));
               end loop;
            end;
         end Init_Ignored_Files;

         ----------------------
         -- In_Ignored_Files --
         ----------------------

         function In_Ignored_Files (Base_Name : String) return Boolean is
         begin
            for File of Ignored_Files loop
               if File.Base_Name = +Base_Name then
                  return True;
               end if;
            end loop;

            return False;
         end In_Ignored_Files;

         ---------------
         -- Skip_File --
         ---------------

         function Skip_File
           (File_Index : Files_List.Cursor) return Boolean
         is
            File : Virtual_File;
            Lang : Language_Access;
         begin
            if not Files_List.Element (File_Index).Is_Regular_File then
               if Options.Report_Errors = Errors_And_Warnings then
                  Kernel.Messages_Window.Insert
                    ((-"warning: the file ") &
                       Display_Full_Name
                       (Files_List.Element (File_Index)) &
                     (-" cannot be found. It will be skipped."),
                     Mode => GPS.Messages_Windows.Info);
               end if;

               return True;
            end if;

            File := Files_List.Element (File_Index);
            Lang := Get_Language_From_File (Lang_Handler, File);

            --  We don't support yet other parsers than Ada, C and C++

            if Lang.all not in Language.Ada.Ada_Language'Class
              and then Lang.all not in Language.C.C_Language'Class
            then
               if not Skipped_Files.Contains (File) then
                  Kernel.Messages_Window.Insert
                    (-("info: Documentation not generated for ") &
                       Display_Base_Name (File) &
                     (-" since this language is not supported."),
                     Mode => GPS.Messages_Windows.Info);

                  Skipped_Files.Append (File);
               end if;

               return True;
            end if;

            --  Verify that we have the LI file for this source file.

            if not Database.Is_Up_To_Date (File) then
               --  (C/C++) Do not report error on header files since the
               --  compiler does not generate their LI file.

               if Lang.all in Language.C.C_Language'Class
                 and then Is_Spec_File (Kernel, File)
               then
                  null;

               --  (Ada): We restrict the warning to specs since otherwise
               --  it would be reported with all the separate Ada units. This
               --  missing case is handled and reported by Build_Tree().

               elsif Is_Spec_File (Kernel, File) then
                  Report_Skipped_File (Kernel, File);
               end if;

               return True;
            end if;

            if Lang.all in Language.Ada.Ada_Language'Class then
               if not Options.Document_Bodies
                 and then not Is_Spec_File (Kernel, File)
               then
                  return True;
               elsif Is_GNAT_Binder_File (File) then
                  return True;
               end if;

            elsif Options.Skip_C_Files
              and then Lang.all in Language.C.C_Language'Class
            then
               return True;
            end if;

            if In_Ignored_Files (+File.Base_Name) then
               if not Options.Quiet_Mode then
                  Log_Line (Options.Quiet_Mode,
                    "- " & (+File.Base_Name) & " ignored");
               end if;

               return True;
            end if;

            return False;
         end Skip_File;

         --  Local variables

         File_Index : Files_List.Cursor;
         Prj_Index  : Project_Files_List.Cursor;
         Prj_Srcs   : Project_Files;

         Skipped_Files : aliased Files_List.Vector;
         File : GNATCOLL.VFS.Virtual_File;

         use type Ada.Containers.Count_Type;

      --  Start of processing for Check_Src_Files

      begin
         Init_Ignored_Files;

         Prj_Index := Prj_Files.First;
         while Project_Files_List.Has_Element (Prj_Index) loop
            Prj_Srcs := Project_Files_List.Element (Prj_Index);

            if Prj_Files.Length > 1 then
               Log_Line (Options.Quiet_Mode,
                 "- Project: " & Prj_Srcs.Project.Name);
            end if;

            File_Index := Prj_Srcs.Src_Files.First;
            while Files_List.Has_Element (File_Index) loop
               if Skip_File (File_Index) then
                  File := Files_List.Element (File_Index);

                  if not Files_List.Has_Element
                           (Skipped_Files.Find (File))
                  then
                     Skipped_Files.Append (File);

                     if Is_Spec_File (Kernel, File) then
                        declare
                           P_Tree    : Project_Tree_Access renames
                                         Kernel.Registry.Tree;
                           Body_File : GNATCOLL.VFS.Virtual_File;
                        begin
                           Body_File := P_Tree.Other_File (File);

                           if Body_File /= File then
                              Skipped_Files.Append (Body_File);
                           end if;
                        end;
                     end if;
                  end if;
               end if;

               Files_List.Next (File_Index);
            end loop;

            Project_Files_List.Next (Prj_Index);
         end loop;

         for File of Skipped_Files loop
            File_Index := Prj_Srcs.Src_Files.Find (File);

            if Files_List.Has_Element (File_Index) then
               Prj_Srcs.Src_Files.Delete (File_Index);
            end if;
         end loop;

         Ignored_Files.Clear;
      end Check_Src_Files;

      ---------------------------
      -- Collect_All_Src_Files --
      ---------------------------

      function Collect_All_Src_Files
        (Prj_Files     : Project_Files_List.Vector;
         Include_Files : Files_List.Vector)
         return Files_List.Vector
      is
         File_Index : Files_List.Cursor;
         Prj_Index  : Project_Files_List.Cursor;
         Prj_Srcs   : Project_Files;

         All_Files  : Files_List.Vector;
         File       : Virtual_File;

      begin
         Prj_Index := Prj_Files.First;
         while Project_Files_List.Has_Element (Prj_Index) loop
            Prj_Srcs := Project_Files_List.Element (Prj_Index);

            File_Index := Prj_Srcs.Src_Files.First;
            while Files_List.Has_Element (File_Index) loop
               File := Files_List.Element (File_Index);

               if not All_Files.Contains (File) then
                  All_Files.Append (File);
               end if;

               Files_List.Next (File_Index);
            end loop;

            Project_Files_List.Next (Prj_Index);
         end loop;

         File_Index := Include_Files.First;
         while Files_List.Has_Element (File_Index) loop
            File := Files_List.Element (File_Index);

            if not All_Files.Contains (File) then
               All_Files.Append (File);
            end if;

            Files_List.Next (File_Index);
         end loop;

         return All_Files;
      end Collect_All_Src_Files;

      ----------------------------
      -- Collect_C_Header_Files --
      ----------------------------

      procedure Collect_C_Header_Files
        (Direct_Include_Files : access Files_List.Vector;
         All_Include_Files    : access Files_List.Vector)
      is
         Prj_Srcs : Project_Files;

         procedure Collect_Include_Files
           (File   : Virtual_File;
            Result : access Files_List.Vector);

         procedure Collect_Include_Files
           (File   : Virtual_File;
            Result : access Files_List.Vector)
         is
            Cursor : Xref.Entities_In_File_Cursor;
            Loc    : General_Location;

         begin
            Cursor := Database.Entities_In_File (File, No_Project);
            while not At_End (Cursor) loop
               declare
                  E : Root_Entity'Class := Cursor.Get;
               begin
                  E   := Cursor.Get;
                  Loc := Get_Location (E);
                  if Xref.Get_Display_Kind (E) = "include file"
                    and then not Result.Contains (Loc.File)
                  then
                     Result.Append (Loc.File);
                  end if;
               end;

               Cursor.Next;
            end loop;
         end Collect_Include_Files;

         File_Index : Files_List.Cursor;
         Lang       : Language_Access;
         Prj_Index  : Project_Files_List.Cursor;

      begin
         Prj_Index := Prj_Files.First;
         while Project_Files_List.Has_Element (Prj_Index) loop
            Prj_Srcs := Project_Files_List.Element (Prj_Index);

            File_Index := Prj_Srcs.Src_Files.First;
            while Files_List.Has_Element (File_Index) loop
               Lang :=
                 Get_Language_From_File
                   (Lang_Handler, Files_List.Element (File_Index));

               if Lang.all in Language.C.C_Language'Class then
                  Collect_Include_Files
                    (File   => Files_List.Element (File_Index),
                     Result => Direct_Include_Files);
               end if;

               Files_List.Next (File_Index);
            end loop;

            Project_Files_List.Next (Prj_Index);
         end loop;

         --  At this stage Include_Files contains all the header files which
         --  are directly included by files of the project. Now we continue
         --  collecting all the headers which are transitively included in
         --  these header files.

         declare
            In_Files : aliased Files_List.Vector;
            Result   : aliased Files_List.Vector;

            use type Ada.Containers.Count_Type;
         begin
            Append_Unique_Files
              (Target => In_Files'Access,
               Source => Direct_Include_Files);

            while In_Files.Length > 0 loop
               Result.Clear;

               File_Index := In_Files.First;
               while Files_List.Has_Element (File_Index) loop
                  if not All_Include_Files.Contains
                    (Files_List.Element (File_Index))
                  then
                     All_Include_Files.Append
                       (Files_List.Element (File_Index));

                     Collect_Include_Files
                       (File   => Files_List.Element (File_Index),
                        Result => Result'Access);
                  end if;

                  Files_List.Next (File_Index);
               end loop;

               In_Files.Clear;

               Append_Unique_Files
                 (Target => In_Files'Access,
                  Source => Result'Access);
            end loop;
         end;
      end Collect_C_Header_Files;

      --------------------------
      -- Compute_Dependencies --
      --------------------------

      procedure Compute_Dependencies
        (File : Virtual_File; Files : in out Files_List.Vector)
      is
         Pending_Files : Files_List.Vector := Files.Copy;

         type Node;
         type Node_Ptr is access Node;

         package Childs_List is new Ada.Containers.Vectors
           (Index_Type => Natural, Element_Type => Node_Ptr);

         type Node is record
            File   : Virtual_File;
            Childs : Childs_List.Vector;
         end record;

         procedure Append (Subtree : Node_Ptr);
         --  Traverse the list of dependencies in post-order appending all the
         --  files to Files

         function Build_Dependencies_Tree
           (File : Virtual_File) return Node_Ptr;

         ------------
         -- Append --
         ------------

         procedure Append (Subtree : Node_Ptr) is
            Cursor : Childs_List.Cursor;
         begin
            Cursor := Subtree.Childs.First;
            while Childs_List.Has_Element (Cursor) loop
               Append (Childs_List.Element (Cursor));
               Childs_List.Next (Cursor);
            end loop;

            Files.Append (Subtree.File);
         end Append;

         -----------------------------
         -- Build_Dependencies_Tree --
         -----------------------------

         function Build_Dependencies_Tree
           (File : Virtual_File) return Node_Ptr
         is
            Result        : constant Node_Ptr := new Node;
            It            : File_Iterator;
            Dep_File      : Virtual_File;
            Dep_File_Info : Node_Ptr;
            Cursor        : Files_List.Cursor;
         begin
            Cursor := Pending_Files.Find (File);
            Pending_Files.Delete (Cursor);

            Result.File := File;

            It := Find_Dependencies (Database, File, No_Project);
            while It.Has_Element loop
               Dep_File := It.Element;

               if Pending_Files.Contains (Dep_File) then
                  Dep_File_Info := Build_Dependencies_Tree (Dep_File);
                  Result.Childs.Append (Dep_File_Info);
               end if;

               It.Next;
            end loop;

            return Result;
         end Build_Dependencies_Tree;

         Subtree : Node_Ptr;
      begin
         Files.Clear;
         Subtree := Build_Dependencies_Tree (File);
         Append (Subtree);
      end Compute_Dependencies;

      ----------------------
      -- Is_Large_Project --
      ----------------------

      function Is_Large_Project
        (Files : in out Files_List.Vector) return Boolean
      is
         use type Ada.Containers.Count_Type;
      begin
         return Files.Length > 400;
      end Is_Large_Project;

      ----------------
      -- Have_Files --
      ----------------

      function Have_Files
        (Prj_Files : Project_Files_List.Vector) return Boolean
      is
         File_Index : Files_List.Cursor;
         Prj_Index  : Project_Files_List.Cursor;
         Prj_Srcs   : Project_Files;

      begin
         Prj_Index := Prj_Files.First;
         while Project_Files_List.Has_Element (Prj_Index) loop
            Prj_Srcs := Project_Files_List.Element (Prj_Index);

            File_Index := Prj_Srcs.Src_Files.First;

            if Files_List.Has_Element (File_Index) then
               return True;
            end if;

            Project_Files_List.Next (Prj_Index);
         end loop;

         return False;
      end Have_Files;

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error
        (All_Src_Files : in out Files_List.Vector;
         File          : Virtual_File;
         Stage         : Stages)
      is
         Header_Eq : constant Natural := 25;
         Header    : constant String  := " GNATdoc BUG DETECTED ";
         Length    : constant Natural := 2 * Header_Eq + Header'Length;
         use GNAT.IO;

         procedure Write (C : Character; N : Natural := 1);
         --  Output N times character C

         procedure Write_Line (S : String := "");
         --  Write the text S delimited by characters '|'. The right margin
         --  is extended to put the right delimited at position Length - 1

         procedure Write_Line (S : String := "") is
            Prefix : constant String := "| " & S;
         begin
            Put (Prefix);
            Write (' ', Length - Prefix'Length + 1);
            Write ('|');
            New_Line;
         end Write_Line;

         procedure Write (C : Character; N : Natural := 1) is
         begin
            for J in 1 .. N loop
               Put (C);
            end loop;
         end Write;

         Is_GPL_Version : constant Boolean := False;

      begin
         Write ('+');
         Write ('=', Header_Eq);
         Put   (Header);
         Write ('=', Header_Eq);
         Write ('+');
         New_Line;

         case Stage is
            when Unknown_Stage =>
               Write_Line ("Code: 00");
            when Frontend_Stage =>
               Write_Line ("Code: 01");
            when Backend_Stage =>
               Write_Line ("Code: 02");
            when TreeOutput_Stage =>
               Write_Line ("Code: 03");
         end case;

         Write_Line ("Error detected processing file:");
         Write_Line ("- " & (+File.Base_Name));
         Write_Line;

         Write_Line
           ("Compiler version: "
            & GNATdoc.Project_Environments.GNATdoc_Project_Environment'Class
              (Kernel.Registry.Environment.all).Get_GNAT_Version);
         Write_Line
           (" GNATdoc version: " & To_String (Config.Version)
            &  " (" & Config.Source_Date & ")");
         Write_Line ("            Host: " & Config.Target);

         Write_Line;

         if Is_GPL_Version then
            Write_Line
              ("Please submit a bug report by email " &
               "to report@adacore.com.");
            Write_Line
              ("GAP members can alternatively use GNAT Tracker:");
            Write_Line
              ("http://www.adacore.com/ " &
               "section 'send a report'.");
            Write_Line
              ("See gnatinfo.txt for full info on procedure " &
               "for submitting bugs.");
         else
            Write_Line
              ("Please submit a bug report using GNAT Tracker:");
            Write_Line
              (" http://www.adacore.com/gnattracker/ " &
               "section 'send a report'.");
            Write_Line
              ("Alternatively submit a bug report by email " &
               "to report@adacore.com,");
            Write_Line
              ("including your customer number #nnn " &
               "in the subject line.");
         end if;

         Write_Line;

         Write_Line
           ("Use a subject line meaningful to you" &
            " and us to track the bug.");
         Write_Line
           ("Include the entire contents of this bug " &
            "box in the report.");
         Write_Line
           ("Include the exact command that you entered.");

         Write_Line;

         Write_Line
           ("Also include sources listed below in gnatchop format");
         Write_Line
           ("(concatenated together with no headers between files).");

         Write ('+');
         Write ('=', Length);
         Write ('+');
         New_Line;

         New_Line;
         Put_Line ("Please include these source files with error report.");
         New_Line;

         Put_Line ("  Note that list may not be accurate in some cases, ");
         Put_Line ("  so please double check that the problem can still ");
         Put_Line ("  be reproduced with the set of files listed.");
         New_Line;

         Compute_Dependencies (File, All_Src_Files);

         Print_Files (All_Src_Files, With_Full_Name => True);
         Error_Reported := True;
      end Report_Error;

      -----------------------
      -- Sort_Dependencies --
      -----------------------

      procedure Sort_Dependencies (Files : in out Files_List.Vector) is
         Num_Files     : constant Natural  := Natural (Files.Length);
         Pending_Files : Files_List.Vector := Files.Copy;

         type Node;
         type Node_Ptr is access Node;

         package Childs_List is new Ada.Containers.Vectors
           (Index_Type => Natural, Element_Type => Node_Ptr);

         type Node is record
            File   : Virtual_File;
            Childs : Childs_List.Vector;
         end record;

         procedure Append (Subtree : Node_Ptr);
         --  Traverse the list of dependencies in post-order appending all the
         --  files to Files

         function Build_Dependencies_Tree
           (File : Virtual_File) return Node_Ptr;

         ------------
         -- Append --
         ------------

         procedure Append (Subtree : Node_Ptr) is
            Cursor : Childs_List.Cursor;
         begin
            Cursor := Subtree.Childs.First;
            while Childs_List.Has_Element (Cursor) loop
               Append (Childs_List.Element (Cursor));
               Childs_List.Next (Cursor);
            end loop;

            Files.Append (Subtree.File);
         end Append;

         ----------------
         -- Build_Tree --
         ----------------

         Files_Count : Natural := 0;

         function Build_Dependencies_Tree
           (File : Virtual_File) return Node_Ptr
         is
            Result        : constant Node_Ptr := new Node;
            It            : File_Iterator;
            Dep_File      : Virtual_File;
            Dep_File_Info : Node_Ptr;
            Cursor        : Files_List.Cursor;
         begin
            Files_Count := Files_Count + 1;

            --  For large number of files we enable an extra output to
            --  confirm that the tool is not in an infinite loop

            if Num_Files > 600
              and then (Files_Count mod 300) = 0
            then
               Log_Line (Options.Quiet_Mode,
                 Files_Count'Img
                   & "/"
                   & To_String (Num_Files)
                   & " files");
            end if;

            Cursor := Pending_Files.Find (File);
            Pending_Files.Delete (Cursor);

            Result.File := File;

            It := Find_Dependencies (Database, File, No_Project);
            while It.Has_Element loop
               Dep_File := It.Element;

               if Pending_Files.Contains (Dep_File) then
                  Dep_File_Info := Build_Dependencies_Tree (Dep_File);
                  Result.Childs.Append (Dep_File_Info);
               end if;

               It.Next;
            end loop;

            return Result;
         end Build_Dependencies_Tree;

         Subtree    : Node_Ptr;
         File_Index : Files_List.Cursor;
      begin
         Files.Clear;

         loop
            File_Index := Pending_Files.First;
            exit when not Files_List.Has_Element (File_Index);

            --  Compute a file dependencies subtree

            Subtree :=
              Build_Dependencies_Tree (Files_List.Element (File_Index));

            --  Append this subtree to the output

            Append (Subtree);
         end loop;
      end Sort_Dependencies;

      ----------
      -- Free --
      ----------

      procedure Free is
        new Ada.Unchecked_Deallocation (Docgen_Context, Docgen_Context_Ptr);

      --  Local variables

      Context : Docgen_Context_Ptr :=
                  new Docgen_Context'
                        (Kernel, Database, Lang_Handler, Options,
                         Project, Prj_Files);

      Direct_Include_Files : aliased Files_List.Vector;
      --  C and C++ header files which are directly included by files of the
      --  current project.

      All_Src_Files        : aliased Files_List.Vector;
      --  All the source files of the project

      All_Include_Files    : aliased Files_List.Vector;
      --  All the C and C++ header files which transitively included by all
      --  the header files of the project

   --  Start of processing for Process_Files

   begin
      --  Register the database in the tree. Needed by internal routines
      --  which can be called directly from gdb

      Atree.Register_Database (Database);

      GNATdoc.Time.Reset;

      --  Initialize the Atree. Required to reset the internal counter used to
      --  generate the unique identifiers of the tree nodes (and thus associate
      --  the same id to repeated executions of this module; required when a
      --  breakpoint in set in node with a given id. For details on debugging
      --  see the comments in the body of gnatdoc-atree.adb).

      Atree.Initialize;

      --  Initialize the backend. Required to ensure that we create the
      --  destination directory with support files before processing the
      --  first file.

      begin
         Backend.Initialize (Context);
      exception
         when others =>
            GNAT.IO.Put_Line
              ("Cannot generate output in directory "
                 & (+Get_Doc_Directory (Kernel).Full_Name));
            return;
      end;

      --  Remove from the list those files which cannot be processed

      Log_Line (Options.Quiet_Mode, "Collecting source files");

      --  Check the source files of Prj_Files and remove those files which
      --  can not be processed. This action it not done at later stage (that
      --  is, checking directly the full list of source files All_Src_Files)
      --  because the backend needs this information to generate the list of
      --  files processed per project.

      Check_Src_Files;

      if not Have_Files (Prj_Files) then
         Log_Line (Options.Quiet_Mode, "No files to process");
         Trace (Me, "No files to process");
         return;
      end if;

      --  Collect in Direct_Include_Files all the C/C++ header files referenced
      --  directly by files of Prj_Files, and collect in All_Include_Files all
      --  the C/C++ header files which are transitively referenced by all the
      --  sources of Prj_Files.

      if not Context.Options.Skip_C_Files then
         Collect_C_Header_Files
           (Direct_Include_Files'Access, All_Include_Files'Access);
      end if;

      --  Collect all the files of the project in a single list and append to
      --  the resulting list all the files in the Include_Files list.

      All_Src_Files := Collect_All_Src_Files (Prj_Files, All_Include_Files);

      --  For large projects enable an extra output

      if Is_Large_Project (All_Src_Files) then
         Log_Line (Options.Quiet_Mode,
           All_Src_Files.Length'Img & " files to process");
      end if;

      Log_Line (Options.Quiet_Mode, "Computing file dependencies");

      if Debug_Unit_Disabled then
         Sort_Dependencies (All_Src_Files);
      end if;

      if Is_Large_Project (All_Src_Files) then
         Log_Line (Options.Quiet_Mode, "Processing files");
      end if;

      Build_Trees_And_Call_Backend (Context, All_Src_Files, All_Include_Files);

      --  For large projects enable an extra output

      if Is_Large_Project (All_Src_Files) then
         Log_Line (Options.Quiet_Mode, "Generating indexes");
      end if;

      Backend.Finalize (Update_Global_Index);

      Templates_Parser.Release_Cache;

      if not Options.Quiet_Mode then
         Kernel.Messages_Window.Insert
           (-("info: Documentation generated in ") &
              Get_Doc_Directory (Kernel).Display_Full_Name,
            Mode => Info);
      end if;

      if Options.Display_Time then
         Time.Print_Time (Context);
      end if;

      Free (Context);

   exception
      when E : others =>
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
         if not Error_Reported then
            Report_Error (All_Src_Files, No_File, Unknown_Stage);
            Trace (Me, E);
         end if;
   end Process_Files;

   ---------------------------
   -- Process_Project_Files --
   ---------------------------

   procedure Process_Project_Files
     (Kernel    : not null access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Options   : Docgen_Options;
      Project   : Project_Type;
      Recursive : Boolean := False;
      Filename  : String := "")
   is
      P         : Project_Type := Project;
      Src_Files : Files_List.Vector;
      Prj_Files : Project_Files_List.Vector;
      Prj_Srcs  : Project_Files;

      procedure Init_Ignored_Subprojects_Table;
      --  Initialize the list of ignored subprojects

      function Is_Ignored_Subproject (Prj_Name : String) return Boolean;
      --  True if Name is one of the ignored subprojects

      -----------------------------
      -- Init_Ignore_Subprojects --
      -----------------------------

      Ign_Prjs  : GNAT.Strings.String_List_Access;

      procedure Init_Ignored_Subprojects_Table is
         Ign_Attr  : constant Attribute_Pkg_List :=
                       Build (Package_Name   => Pkg_Name,
                              Attribute_Name => Ignored_Subprojects_Name);
      begin
         Ign_Prjs :=
           Kernel.Registry.Tree.Root_Project.Attribute_Value (Ign_Attr);
      end Init_Ignored_Subprojects_Table;

      ---------------------------
      -- Is_Ignored_Subproject --
      ---------------------------

      function Is_Ignored_Subproject (Prj_Name : String) return Boolean is
         use type GNAT.Strings.String_List_Access;
      begin
         if Ign_Prjs /= null then
            for J in Ign_Prjs'Range loop
               if To_Lower (Ign_Prjs (J).all) = To_Lower (Prj_Name) then
                  return True;
               end if;
            end loop;
         end if;

         return False;
      end Is_Ignored_Subproject;

   begin
      Trace (Me, "Process_Project_Files");

      if P = No_Project then
         P := Kernel.Registry.Tree.Root_Project;
      end if;

      Prj_Srcs.Src_Files := new Files_List.Vector;

      if not Recursive then
         declare
            Source_Files : File_Array_Access := P.Source_Files;
         begin
            Prj_Srcs.Project := P;

            for J in Source_Files'Range loop
               if Filename = "" then
                  Prj_Srcs.Src_Files.Append (Source_Files (J));
               else
                  if String (Source_Files (J).Base_Name) = Filename then
                     declare
                        Other_File : constant Virtual_File :=
                          Kernel.Registry.Tree.Other_File (Source_Files (J));
                     begin
                        Prj_Srcs.Src_Files.Append (Source_Files (J));
                        Prj_Srcs.Src_Files.Append (Other_File);
                     end;

                     exit;
                  end if;
               end if;
            end loop;

            Unchecked_Free (Source_Files);
            Prj_Files.Append (Prj_Srcs);
         end;
      else
         declare
            Prj_Iter : Project_Iterator;

         begin
            Init_Ignored_Subprojects_Table;

            Prj_Iter := P.Start_Reversed;
            Process_All_Projects : while Current (Prj_Iter) /= No_Project loop
               Prj_Srcs.Project := Current (Prj_Iter);

               if Is_Ignored_Subproject (Prj_Srcs.Project.Name) then
                  Log_Line (Options.Quiet_Mode,
                    "- Project: " & Prj_Srcs.Project.Name & " ignored");
               else
                  declare
                     Source_Files : File_Array_Access :=
                       Prj_Srcs.Project.Source_Files;
                  begin
                     for J in Source_Files'Range loop
                        if Filename = "" then
                           Prj_Srcs.Src_Files.Append (Source_Files (J));
                        else
                           if String (Source_Files (J).Base_Name)
                             = Filename
                           then
                              declare
                                 Other_File : constant Virtual_File :=
                                   Kernel.Registry.Tree.Other_File
                                     (Source_Files (J));
                              begin
                                 Prj_Srcs.Src_Files.Append (Source_Files (J));
                                 Prj_Srcs.Src_Files.Append (Other_File);
                              end;

                              Unchecked_Free (Source_Files);
                              Prj_Files.Append (Prj_Srcs);

                              exit Process_All_Projects;
                           end if;
                        end if;
                     end loop;

                     Unchecked_Free (Source_Files);
                  end;

                  Prj_Files.Append (Prj_Srcs);
               end if;

               Next (Prj_Iter);
            end loop Process_All_Projects;
         end;
      end if;

      --  Clear the internal structures of the frontend
      Frontend.Initialize;

      Process_Files
        (Kernel    => Core_Kernel (Kernel),
         Options   => Options,
         Project   => Project,
         Prj_Files => Prj_Files,
         Update_Global_Index => True);

      Src_Files.Clear; -- Free???

   exception
      when GNATdoc.Backend.Unknown_Backend =>
         GNAT.IO.Put_Line ("wrong value for switch --output");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Process_Project_Files;

   -----------
   -- Files --
   -----------

   package body Files is

      -------------------------
      -- Append_Unique_Files --
      -------------------------

      procedure Append_Unique_Files
        (Target : access Files_List.Vector;
         Source : access Files_List.Vector)
      is
         File_Index : Files_List.Cursor;
      begin
         File_Index := Source.First;
         while Files_List.Has_Element (File_Index) loop
            if not Target.Contains (Files_List.Element (File_Index)) then
               Target.Append (Files_List.Element (File_Index));
            end if;

            Files_List.Next (File_Index);
         end loop;
      end Append_Unique_Files;

      -----------------
      -- Print_Files --
      -----------------

      procedure Print_Files
        (Source         : Files_List.Vector;
         With_Full_Name : Boolean := False)
      is
         File_Index : Files_List.Cursor;
         File       : Virtual_File;
      begin
         File_Index := Source.First;
         while Files_List.Has_Element (File_Index) loop
            File := Files_List.Element (File_Index);

            if With_Full_Name then
               GNAT.IO.Put_Line (+File.Full_Name);
            else
               GNAT.IO.Put_Line (+File.Base_Name);
            end if;

            Files_List.Next (File_Index);
         end loop;
      end Print_Files;

      --------------
      -- Filename --
      --------------

      function Filename (File : Virtual_File) return Filesystem_String is
         Ext  : constant Filesystem_String := File.File_Extension;
         Base : constant Filesystem_String := File.Base_Name;
         Name : constant Filesystem_String :=
           Base (Base'First .. Base'Last - Ext'Length);
      begin
         return Name;
      end Filename;

      ---------------
      -- Less_Than --
      ---------------

      function Less_Than (Left, Right : Virtual_File) return Boolean is
      begin
         return To_Lower (+Base_Name (Left)) < To_Lower (+Base_Name (Right));
      end Less_Than;

      function Less_Than (Left, Right : Project_Files) return Boolean is
      begin
         return To_Lower (Name (Left.Project))
                  < To_Lower (Name (Right.Project));
      end Less_Than;

      --------------------
      -- Remove_Element --
      --------------------

      procedure Remove_Element
        (List   : in out Files_List.Vector;
         Cursor : in out Files_List.Cursor)
      is
         Prev : constant Files_List.Extended_Index :=
           Files_List.To_Index (Cursor);
      begin
         List.Delete (Cursor);
         Cursor := List.To_Cursor (Prev);
      end Remove_Element;

   end Files;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File
     (Context   : access constant Docgen_Context;
      Directory : Virtual_File;
      Filename  : Filesystem_String;
      Text      : access Unbounded_String)
   is
      Name   : Virtual_File;
      Output : Writable_File;

   begin
      if not Is_Directory (Directory) then
         Directory.Make_Dir;
      end if;

      Name   := Create_From_Dir (Directory, Filename);
      Output := Name.Write_File;

      if Output = Invalid_File then
         Context.Kernel.Messages_Window.Insert
           ("Could not create " & Name.Display_Full_Name,
            Mode => GPS.Messages_Windows.Error);
         return;
      end if;

      Write (Output, To_String (Text.all));
      Close (Output);
   end Write_To_File;

   -------------------
   -- Write_To_File --
   -------------------

   procedure Write_To_File
     (Context   : access constant Docgen_Context;
      Directory : Virtual_File;
      Filename  : Filesystem_String;
      Text      : String)
   is
      Name   : Virtual_File;
      Output : Writable_File;

   begin
      if not Is_Directory (Directory) then
         Directory.Make_Dir;
      end if;

      Name   := Create_From_Dir (Directory, Filename);
      Output := Name.Write_File;

      if Output = Invalid_File then
         Context.Kernel.Messages_Window.Insert
           ("Could not create " & Name.Display_Full_Name,
            Mode => GPS.Messages_Windows.Error);
         return;
      end if;

      Write (Output, Text);
      Close (Output);
   end Write_To_File;

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory
     (Kernel : access Core_Kernel_Record'Class) return Virtual_File
   is
      Project  : Project_Type renames Kernel.Registry.Tree.Root_Project;
      Attr     : constant String :=
        Project.Attribute_Value (Documentation_Dir_Attribute);
      Old_Attr : constant String :=
        Project.Attribute_Value (Old_Documentation_Dir_Attribute);
      Base_Dir : Virtual_File;

   begin
      if Attr /= "" then
         Base_Dir :=
           Create_From_Base (+Attr, Project.Project_Path.Get_Parent.Full_Name);
         Base_Dir.Ensure_Directory;

         return Base_Dir;

      elsif Old_Attr /= "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "attribute Documentation_Dir in package IDE is obsolete");
         Base_Dir :=
           Create_From_Base
             (+Old_Attr, Project.Project_Path.Get_Parent.Full_Name);
         Base_Dir.Ensure_Directory;

         return Base_Dir;
      end if;

      if Project.Object_Dir /= No_File then
         Base_Dir := Project.Object_Dir;
      else
         Base_Dir := Project.Project_Path.Get_Parent;
      end if;

      return Create_From_Dir (Base_Dir, +"gnatdoc/");
   end Get_Doc_Directory;

   ----------------------
   -- Read_Source_File --
   ----------------------

   function Read_Source_File
     (Context : access constant Docgen_Context;
      File    : GNATCOLL.VFS.Virtual_File) return GNAT.Strings.String_Access
   is
      use type GNAT.Strings.String_Access;

      Buffer  : GNAT.Strings.String_Access := File.Read_File;
      Aux     : GNAT.Strings.String_Access;
      Default : GNATCOLL.Iconv.Iconv_T;
      Last    : Natural;
      Index   : Positive;

   begin
      --  There are some heuristicts should be used to guess most appropriate
      --  encoding of the file:
      --
      --  1. Check whether UTF-8 is used for file. This is important to cover
      --  useful case when only ASCII characters are used for identifiers, but
      --  UTF-8 is used for string literals and comments.
      --
      --  2. Look for compiler switches (-gnati?) in ALI file to detect
      --  encoding of identifiers (and string literals and comments too). This
      --  is not implemented now.
      --
      --  3. Default encoding is used otherwise.

      if not UTF8_Utils.Validate_UTF_8 (Buffer.all) then
         Default := GNATCOLL.Iconv.Iconv_Open
           (GNATCOLL.Iconv.UTF8, Context.Options.Default_Encoding.all);

         if UTF8_Utils.Validate (Default, Buffer.all) then
            Aux := Buffer;
            Buffer := new String'(GNATCOLL.Iconv.Iconv (Default, Aux.all));
            GNAT.Strings.Free (Aux);
         end if;

         GNATCOLL.Iconv.Iconv_Close (Default);
      end if;

      --  Replace CR/LF pairs by singe LF character

      Index := Buffer'First;
      Last  := Index - 1;

      while Index <= Buffer'Last loop
         Last := Last + 1;

         if Buffer (Index) = CR
           and then Index < Buffer'Last
           and then Buffer (Index + 1) = LF
         then
            Buffer (Last) := LF;
            Index := Index + 1;

         elsif Index /= Last then
            Buffer (Last) := Buffer (Index);
         end if;

         Index := Index + 1;
      end loop;

      if Last /= Buffer'Last then
         Aux := Buffer;
         Buffer := new String'(Aux (Aux'First .. Last));
         GNAT.Strings.Free (Aux);
      end if;

      return Buffer;
   end Read_Source_File;

   -------------------------
   -- Report_Skipped_File --
   -------------------------

   procedure Report_Skipped_File
     (Kernel : Core_Kernel;
      File   : Virtual_File) is
   begin
      if not Skipped_Files.Contains (File) then
         Kernel.Messages_Window.Insert
           (-("warning: cross references for file ") &
              Display_Base_Name (File) &
           (-" are not up-to-date. Documentation not generated."),
           Mode => GPS.Messages_Windows.Error);

         Skipped_Files.Append (File);
      end if;
   end Report_Skipped_File;

end GNATdoc;
