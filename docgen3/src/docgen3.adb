------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GPS.Intl;                use GPS.Intl;
with GPS.Messages_Windows;    use GPS.Messages_Windows;
with Docgen3.Atree;           use Docgen3.Atree;
with Docgen3.Backend;         use Docgen3.Backend;
with Docgen3.Files;           use Docgen3.Files;
with Docgen3.Frontend;        use Docgen3.Frontend;
with Docgen3.Time;            use Docgen3.Time;
with Docgen3.Treepr;          use Docgen3.Treepr;
with Docgen3.Utils;           use Docgen3.Utils;
with Language;                use Language;
with Language.Ada;
with Language.C;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with Traces;
with Templates_Parser;        use Templates_Parser;
with Xref.Docgen;             use Xref.Docgen;

with GNAT.IO;  -- to be removed???

package body Docgen3 is
   Me : constant Traces.Debug_Handle := Create ("Docgen3.1");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Process_Files
     (Kernel              : Core_Kernel;
      Options             : Docgen_Options;
      Src_Files           : in out Files_List.Vector;
      Update_Global_Index : Boolean);
   --  This subprogram factorizes the functionality shared by routines
   --  Process_Single_File and Process_Project_Files. It processes all
   --  the files in Src_Files and generates their documentation.

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files
     (Kernel              : Core_Kernel;
      Options             : Docgen_Options;
      Src_Files           : in out Files_List.Vector;
      Update_Global_Index : Boolean)
   is
      Database     : constant General_Xref_Database := Kernel.Databases;
      Lang_Handler : constant Language_Handler := Kernel.Lang_Handler;

      procedure Check_Src_Files;
      --  Check the contents of Src_Files and remove from the list those files
      --  which can not be processed

      procedure Prepend_C_Header_Files;
      --  Prepend to Src_Files all the C and C++ files specified in "#include"
      --  directives.

      procedure Check_Src_Files is

         function Skip_File (File_Index : Files_List.Cursor) return Boolean;
         --  Return True if if the file Src_Files (File_Index) cannot be
         --  processed.

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
               Kernel.Messages_Window.Insert
                 ((-"warning: the file ") &
                    Display_Full_Name
                    (Files_List.Element (File_Index)) &
                  (-" cannot be found. It will be skipped."),
                  Mode => GPS.Messages_Windows.Info);

               return True;
            end if;

            File := Files_List.Element (File_Index);
            Lang := Get_Language_From_File
              (Lang_Handler, Files_List.Element (File_Index));

            --  We don't support yet other parsers than Ada, C and C++

            if Lang.all not in Language.Ada.Ada_Language'Class
              and then Lang.all not in Language.C.C_Language'Class
            then
               Kernel.Messages_Window.Insert
                 (-("info: Documentation not generated for ") &
                    Display_Base_Name (File) &
                  (-" since this language is not supported."),
                  Mode => GPS.Messages_Windows.Info);

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
               else
                  Kernel.Messages_Window.Insert
                    (-("warning: cross references for file ") &
                       Display_Base_Name (File) &
                     (-" are not up-to-date. Documentation not generated."),
                     Mode => GPS.Messages_Windows.Error);
               end if;

               return True;
            end if;

            if Lang.all in Language.Ada.Ada_Language'Class
              and then not Is_Spec_File (Kernel, File)
            then
               return True;

            elsif Options.Skip_C_Files
              and then Lang.all in Language.C.C_Language'Class
            then
               return True;
            end if;

            return False;
         end Skip_File;

         --  Local variables

         File_Index : Files_List.Cursor;
         Num_Files  : Natural := Natural (Src_Files.Length);

      --  Start of processing for Check_Files

      begin
         Trace (Me, "Initial number of files: " & Num_Files'Img);
         File_Index := Src_Files.First;
         while Files_List.Has_Element (File_Index) loop
            if Skip_File (File_Index) then
               Remove_Element (Src_Files, File_Index);
            else
               Files_List.Next (File_Index);
            end if;
         end loop;

         Num_Files := Natural (Src_Files.Length);
         Trace (Me, "Number of files to process: " & Num_Files'Img);
      end Check_Src_Files;

      ----------------------------
      -- Prepend_C_Header_Files --
      ----------------------------

      procedure Prepend_C_Header_Files is

         procedure Prepend_Include_Files (File : Virtual_File);
         procedure Prepend_Include_Files (File : Virtual_File) is
            Cursor : Xref.Entities_In_File_Cursor;
            E      : General_Entity;
            Loc    : General_Location;

         begin
            Cursor := Database.Entities_In_File (File);
            while not At_End (Cursor) loop
               E   := Cursor.Get;
               Loc := Get_Location (Database, E);

               if Xref.Get_Display_Kind (Database, E) = "include file"
                 and then not Src_Files.Contains (Loc.File)
               then
                  Src_Files.Prepend (Loc.File);
               end if;

               Cursor.Next;
            end loop;
         end Prepend_Include_Files;

         File_Index : Files_List.Cursor;
         Lang       : Language_Access;

      begin
         File_Index := Src_Files.First;
         while Files_List.Has_Element (File_Index) loop
            Lang :=
              Get_Language_From_File
                (Lang_Handler, Files_List.Element (File_Index));

            if Lang.all in Language.C.C_Language'Class then
               Prepend_Include_Files (Files_List.Element (File_Index));
            end if;

            Files_List.Next (File_Index);
         end loop;
      end Prepend_C_Header_Files;

      ----------
      -- Free --
      ----------

      procedure Free is
        new Ada.Unchecked_Deallocation (Docgen_Context, Docgen_Context_Ptr);

      --  Local variables

      Backend : Docgen3_Backend'Class := New_Backend;
      Context : aliased Docgen_Context_Ptr :=
                          new Docgen_Context'
                             (Kernel, Database, Lang_Handler, Options);

   --  Start of processing for Process_Files

   begin
      --  Register the database in the tree. Needed by internal routines
      --  which can be called directly from gdb

      Atree.Register_Database (Database);

      --  Remove from the list those files which cannot be processed

      Check_Src_Files;

      if Src_Files.Is_Empty then
         Trace (Me, "No files to process");
         return;
      end if;

      if not Context.Options.Skip_C_Files then
         Prepend_C_Header_Files;
      end if;

      Docgen3.Time.Reset;

      --  Initialize the Atree. Required to reset the internal counter used to
      --  generate the unique identifiers of the tree nodes (and thus associate
      --  the same id to repeated executions of this module; required when a
      --  breakpoint in set in node with a given id. For details on debugging
      --  see the comments in the body of docgen3-atree.adb).

      Atree.Initialize;

      --  Initialize the backend. Required to ensure that we create the
      --  destination directory with support files before processing the
      --  first file.

      Backend.Initialize (Context);

      --  Process all the files

      declare
         Num_Files  : constant Natural := Natural (Src_Files.Length);
         Count      : Natural := 0;
         File_Index : Files_List.Cursor;
         All_Trees  : Frontend.Tree_List.Vector;

      begin
         File_Index := Src_Files.First;
         while Files_List.Has_Element (File_Index) loop
            Count := Count + 1;

            declare
               Current_File  : Virtual_File
                                 renames Files_List.Element (File_Index);
               Tree          : aliased Tree_Type;

            begin
               --  Progress notification: currently using GNAT.IO but this
               --  must be improved???

               GNAT.IO.Put_Line
                 (Count'Img & "/" & To_String (Num_Files)
                  & ": "
                  & (+Current_File.Base_Name));

               Tree :=
                 Frontend.Build_Tree
                   (Context => Context,
                    File    => Current_File);

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

               All_Trees.Append (Tree);
            end;

            Files_List.Next (File_Index);
         end loop;

         if Tree_List.Has_Element (All_Trees.First) then

            --  Set the inheritance depth level of tagged types. This cannot
            --  be done before because files are not processed following their
            --  order of dependencies (and thus a file containing a child type
            --  may be processed by the frontend before the file containing
            --  its parent type).

            declare
               procedure Set_Idepth (E : Entity_Id);
               procedure Set_Idepth (E : Entity_Id) is
               begin
                  if Is_Tagged_Type (E) then
                     Set_IDepth_Level (E);
                  end if;
               end Set_Idepth;

               procedure Set_Alias (E : Entity_Id);
               procedure Set_Alias (E : Entity_Id) is
                  Alias : Entity_Id;
               begin
                  if Present (LL.Get_Alias (E)) then
                     pragma Assert (No (Get_Alias (E)));
                     Alias :=
                       Find_Unique_Entity
                         (Get_Location (Database, LL.Get_Alias (E)));
                     pragma Assert (Present (Alias));
                     Set_Alias (E, Alias);
                  end if;
               end Set_Alias;

               Cursor : Tree_List.Cursor;
               Tree   : aliased Tree_Type;

            begin
               Cursor := All_Trees.First;
               while Tree_List.Has_Element (Cursor) loop
                  Tree := Tree_List.Element (Cursor);

                  For_All (Tree.All_Entities, Set_Idepth'Access);
                  For_All (Tree.All_Entities, Set_Alias'Access);

                  Backend.Process_File (Tree'Access);

                  Tree_List.Next (Cursor);
               end loop;

               Kernel.Messages_Window.Insert
                 (-("info: Documentation generated in ") &
                    Get_Doc_Directory (Kernel).Display_Full_Name,
                  Mode => Info);
            end;
         end if;
      end;

      Backend.Finalize (Update_Global_Index);

      Templates_Parser.Release_Cache;

      if Options.Display_Time then
         Time.Print_Time (Context);
      end if;

      Free (Context);

   exception
      when E : others =>
         Free (Context);
         Trace (Traces.Exception_Handle, E);
   end Process_Files;

   ---------------------------
   -- Process_Project_Files --
   ---------------------------

   procedure Process_Project_Files
     (Kernel    : not null access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Options   : Docgen_Options;
      Project   : Project_Type;
      Recursive : Boolean := False)
   is
      P         : Project_Type := Project;
      Src_Files : Files_List.Vector;

   begin
      Trace (Me, "Process_Project_Files");

      if P = No_Project then
         P := Kernel.Registry.Tree.Root_Project;
      end if;

      declare
         Source_Files  : File_Array_Access := P.Source_Files (Recursive);
      begin
         for J in Source_Files'Range loop
            Src_Files.Append (Source_Files (J));
         end loop;

         Unchecked_Free (Source_Files);
      end;

      --  Clear the internal structures of the frontend
      Frontend.Initialize;

      Process_Files
        (Kernel    => Core_Kernel (Kernel),
         Options   => Options,
         Src_Files => Src_Files,
         Update_Global_Index => True);

      Src_Files.Clear;
   end Process_Project_Files;

   -------------------------
   -- Process_Single_File --
   -------------------------

   procedure Process_Single_File
     (Kernel  : not null access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Options : Docgen_Options;
      File    : GNATCOLL.VFS.Virtual_File)
   is
      Other_File : constant Virtual_File :=
                     Kernel.Registry.Tree.Other_File (File);
      Src_Files  : Files_List.Vector;
   begin
      Trace (Me, "Process_Single_File");
      Src_Files.Append (File);

      if Other_File /= File
        and then Is_Regular_File (Other_File)
      then
         Src_Files.Append (Other_File);
      end if;

      Process_Files
        (Kernel    => Core_Kernel (Kernel),
         Options   => Options,
         Src_Files => Src_Files,
         Update_Global_Index => False);

      Src_Files.Clear;
   end Process_Single_File;

end Docgen3;
