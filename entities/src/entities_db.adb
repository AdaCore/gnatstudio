with Ada.Calendar;      use Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Text_IO;       use Ada.Text_IO;
with Database;          use Database;
with Database_Enums;    use Database_Enums;
with GNATCOLL.Mmap;     use GNATCOLL.Mmap;
with GNATCOLL.SQL;      use GNATCOLL.SQL;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.Traces;   use GNATCOLL.Traces;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Orm;               use Orm;

package body Entities_Db is
   Me : constant Trace_Handle := Create ("ENTITIES", Off);
   Me_Error : constant Trace_Handle := Create ("ENTITIES.ERROR");
   Me_Debug : constant Trace_Handle := Create ("ENTITIES.DEBUG", Off);

   Query_Get_File : constant Files_Stmt :=
     Orm.All_Files
     .Filter (Database.Files.Path = Text_Param (1))
     .Limit (1)
     .Prepare (On_Server => True, Name => "get_file_by_path");
   --  Retrieve the info for a file given its path

   Query_Insert_File : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Files.Path = Text_Param (1))
             & (Database.Files.Timestamp = Time_Param (2))
             & (Database.Files.Language = Text_Param (3))),
        On_Server => True, Name => "insert_file");

   Query_Set_File_Dep : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.F2f.Fromfile = Integer_Param (1))
             & (Database.F2f.Tofile = Integer_Param (2))
             & (Database.F2f.Kind = F2f_Withs)),
        On_Server => True, Name => "set_file_dep");

   Query_Set_ALI : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.F2f.Fromfile = Integer_Param (1))
             & (Database.F2f.Tofile = Integer_Param (2))
             & (Database.F2f.Kind = F2f_Has_Ali)),
        On_Server => True, Name => "set_ali");

   Query_Delete_File_Dep : constant Prepared_Statement :=
     Prepare
       (SQL_Delete
            (From => Database.F2f,
             Where => Database.F2f.Fromfile = Integer_Param (1)),
        On_Server => True, Name => "delete_file_dep");

   Query_Insert_Entity : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entities.Name = Text_Param (1))
             & (Database.Entities.Kind = Text_Param (2))
             & (Database.Entities.Decl_File = Integer_Param (3))
             & (Database.Entities.Decl_Line = Integer_Param (4))
             & (Database.Entities.Decl_Column = Integer_Param (5))),
        On_Server => True, Name => "insert_entity");

   Query_Insert_Ref : constant Prepared_Statement :=
     Prepare
       (SQL_Insert
            ((Database.Entity_Refs.Entity   = Integer_Param (1))
             & (Database.Entity_Refs.File   = Integer_Param (2))
             & (Database.Entity_Refs.Line   = Integer_Param (3))
             & (Database.Entity_Refs.Column = Integer_Param (4))
             & (Database.Entity_Refs.Kind   = Text_Param (5))),
        On_Server => True, Name => "insert_ref");

   Query_Find_Entity_From_Decl : constant Prepared_Statement :=
     Prepare
       (SQL_Select
            (Database.Entities.Id,
             From => Database.Entities,
             Where => Database.Entities.Name = Text_Param (1)
             and Database.Entities.Decl_File = Integer_Param (2)
             and Database.Entities.Decl_Line = Integer_Param (3)
             and Database.Entities.Decl_Column = Integer_Param (4),
             Limit => 1),
        On_Server => True, Name => "entity_from_decl");
   --  Get an entity's id given the location of its declaration. In sqlite3,
   --  this is implemented as a single table lookup thanks to the multi-column
   --  covering index we created.

   package VFS_To_Ids is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => Integer,   --  Id in the files table
      Hash            => Full_Name_Hash,
      Equivalent_Keys => "=");
   use VFS_To_Ids;

   package Depid_To_Ids is new Ada.Containers.Vectors
     (Index_Type      => Positive,  --  index in the ALI file ("D" lines)
      Element_Type    => Integer);  --  Id in the files table
   use Depid_To_Ids;

   procedure Parse_LI
     (Session                   : Session_Type;
      Tree                      : Project_Tree;
      Library_File, Source_File : Virtual_File;
      VFS_To_Id                 : in out VFS_To_Ids.Map;
      Ignore_Ref_In_Other_Files : Boolean);
   --  Parse the contents of a single LI file.
   --  VFS_To_Id is a local cache for the entries in the files table.
   --
   --  Ignore_Ref_In_Other_Files should be set to True when we are parsing or
   --  updating *all* the LI files of the projects. Since we know we will see
   --  the same xref when parsing the LI file of the unit in which the entity
   --  is declared, we do not need to insert it. For instance, this applies to
   --  the parent types of entities, or the list of parameters for subprograms.
   --  This also avoids duplication in the database.

   --------------
   -- Parse_LI --
   --------------

   procedure Parse_LI
     (Session                   : Session_Type;
      Tree                      : Project_Tree;
      Library_File, Source_File : Virtual_File;
      VFS_To_Id                 : in out VFS_To_Ids.Map;
      Ignore_Ref_In_Other_Files : Boolean)
   is
      pragma Unreferenced (Source_File, Ignore_Ref_In_Other_Files);
      M      : Mapped_File;
      Str    : Str_Access;
      Last   : Integer;
      Index  : Integer;

      Start           : Integer;
      ALI_Id          : Integer := -1;
      Current_Unit_Id : Integer := -1;
      Dep_Id          : Integer;

      D_Line_Id       : Positive := 1;
      --  Current "D" line index

      Depid_To_Id     : Depid_To_Ids.Vector;

      Xref_File, Xref_Line, Xref_Col : Integer;
      Xref_Kind : Character;
      --  The current xref, result of Get_Xref

      Current_Entity : Integer;
      --  Id in "entities" table for the current entity

      procedure Skip_Spaces;
      pragma Inline (Skip_Spaces);
      --  Moves Index on the first character following the spaces.
      --  This doesn't check whether we go past the end-of-line or the last
      --  character in the file.

      procedure Skip_Word;
      pragma Inline (Skip_Word);
      --  Moves Index to the first whitespace character following the current
      --  word

      procedure Skip_Instance_Info;
      --  Skip any instantiation info "[file|line[fil2|line[...]]]"

      procedure Skip_Import_Info;
      --  Skip any information about imports, in references:
      --      65b<c,gnatcoll_munmap>22

      procedure Next_Line;
      pragma Inline (Next_Line);
      --  Moves Index to the beginning of the next line

      function Get_Natural return Natural;
      pragma Inline (Get_Natural);
      --  Read an integer at the current position, and moves Index after it.

      function Get_Char return Character;
      pragma Inline (Get_Char);
      --  Return the current character, and move forward

      procedure Get_Ref (With_Col : Boolean := True);
      --  Parse a "file|line kind col" reference (the file is optional,
      --  and left untouched if unspecified). Sets the Xref_* variables
      --  accordingly.
      --  If With_Col is False, no "kind col" is expected

      function Get_Ref_Or_Predefined
        (Endchar  : Character;
         With_Col : Boolean := True) return Boolean;
      --  Parse a "file|line kind col" reference, or the name of a predefined
      --  entity. After this ref or name, we expect to see Endchar.
      --  Returns False if there is an error.

      function Insert_File
        (Basename : String;
         Language : String;
         Clear    : Boolean := False) return Integer;
      --  Retrieves the id for the file in the database, or create a new entry
      --  for it.
      --  Returns -1 if the file is not known in the project.
      --  If Clear is true, this clears all known relationships from this
      --  file to any other (Known file dependencies, ALI files,...)

      procedure Process_Entity (Current_X_File : Integer);
      --  Process the current line when it is an entity declaration and its
      --  references in the current file.

      -----------------
      -- Get_Natural --
      -----------------

      function Get_Natural return Natural is
         V : Natural := 0;
      begin
         if Str (Index) not in '0' .. '9' then
            Trace (Me_Error, "Expected a natural, got "
                   & String (Str (Index .. Integer'Min (Index + 20, Last))));
            return 0;  --  Error in ALI file
         end if;

         loop
            V := V * 10 + (Character'Pos (Str (Index)) - Character'Pos ('0'));
            Index := Index + 1;
            exit when Index > Last
              or else Str (Index) not in '0' .. '9';
         end loop;

         return V;
      end Get_Natural;

      --------------
      -- Get_Char --
      --------------

      function Get_Char return Character is
         C : constant Character := Str (Index);
      begin
         Index := Index + 1;
         return C;
      end Get_Char;

      -------------
      -- Get_Ref --
      -------------

      procedure Get_Ref (With_Col : Boolean := True) is
      begin
         Xref_Line := Get_Natural;

         if Str (Index) = '|' then
            Xref_File := Depid_To_Id.Element (Xref_Line);
            Index := Index + 1;  --  Skip '|'
            Xref_Line := Get_Natural;
         end if;

         if With_Col then
            Xref_Kind := Get_Char;
            Skip_Import_Info;
            Xref_Col := Get_Natural;
         end if;
      end Get_Ref;

      ---------------------------
      -- Get_Ref_Or_Predefined --
      ---------------------------

      function Get_Ref_Or_Predefined
        (Endchar  : Character;
         With_Col : Boolean := True) return Boolean is
      begin
         if Str (Index) not in '0' .. '9' then
            --  a predefined entity
            while Str (Index) /= Endchar loop
               Index := Index + 1;
            end loop;

         else
            Get_Ref (With_Col => With_Col);
         end if;

         --  ??? TBD: insert in db

         --  There could be extra information regarding instantiations. Ignore
         --  these for now.

         Skip_Instance_Info;

         if Get_Char /= Endchar then
            if Active (Me_Error) then
               Trace (Me_Error, "Error: expected "
                      & Character'Image (Endchar) & ", got '"
                      & String
                        (Str (Index - 1 .. Integer'Min (Index + 20, Last)))
                      & "' at index" & Index'Img);
            end if;
            return False;
         end if;

         return True;
      end Get_Ref_Or_Predefined;

      ---------------
      -- Next_Line --
      ---------------

      procedure Next_Line is
      begin
         while Index <= Last
           and then Str (Index) /= ASCII.LF
         loop
            Index := Index + 1;
         end loop;

         Index := Index + 1;  --  Skip ASCII.LF
      end Next_Line;

      ------------------------
      -- Skip_Instance_Info --
      ------------------------

      procedure Skip_Instance_Info is
         Nesting : Natural := 1;
      begin
         if Str (Index) = '[' then
            while Nesting > 0 loop
               Index := Index + 1;
               if Str (Index) = '[' then
                  Nesting := Nesting + 1;
               elsif Str (Index) = ']' then
                  Nesting := Nesting - 1;
               end if;
            end loop;
            Index := Index + 1;
         end if;
      end Skip_Instance_Info;

      ----------------------
      -- Skip_Import_Info --
      ----------------------

      procedure Skip_Import_Info is
      begin
         if Str (Index) = '<' then
            while Str (Index) /= '>' loop
               Index := Index + 1;
            end loop;
            Index := Index + 1;
         end if;
      end Skip_Import_Info;

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces is
      begin
         while Str (Index) = ' ' or else Str (Index) = ASCII.HT loop
            Index := Index + 1;
         end loop;
      end Skip_Spaces;

      ---------------
      -- Skip_Word --
      ---------------

      procedure Skip_Word is
      begin
         while Index <= Last
           and then Str (Index) /= ' '
           and then Str (Index) /= ASCII.LF
           and then Str (Index) /= ASCII.HT
         loop
            Index := Index + 1;
         end loop;
      end Skip_Word;

      -----------------
      -- Insert_File --
      -----------------

      function Insert_File
        (Basename : String;
         Language : String;
         Clear    : Boolean := False) return Integer
      is
         --  Unfortunately we have to copy the name of the string, there is
         --  no way to directly use the access type...

         File : constant Virtual_File :=
           Tree.Create
             (Name            => +Basename,
              Use_Object_Path => False);

         Name  : aliased String := +File.Full_Name (Normalize => True).all;
         Files : File_List;
         R     : Forward_Cursor;
         Found : VFS_To_Ids.Cursor;
         Id    : Integer;
      begin
         if File = GNATCOLL.VFS.No_File then
            if Active (Me) then
               Trace (Me, "File not found in project: " & Basename);
            end if;
            return -1;
         end if;

         Found := VFS_To_Id.Find (File);
         if Has_Element (Found) then
            Id := Element (Found);
            if Clear then
               Session.DB.Execute
                 (Query_Delete_File_Dep, Params => (1 => +Id));
            end if;
            return Id;
         end if;

         Files := Query_Get_File.Get
           (Session, Params => (1 => +Name'Access));
         if Files.Has_Row then
            Id := Files.Element.Id;
            VFS_To_Id.Insert (File, Id);

            if Clear then
               Session.DB.Execute
                 (Query_Delete_File_Dep, Params => (1 => +Id));
            end if;

            return Id;
         end if;

         R.Fetch
           (Session.DB,
            Query_Insert_File,
            Params => (1  => +Name'Access,
                       2  => +File.File_Time_Stamp,
                       3  => +Language'Unrestricted_Access));

         Id := R.Last_Id (Session.DB, Database.Files.Id);
         VFS_To_Id.Insert (File, Id);
         return Id;
      end Insert_File;

      --------------------
      -- Process_Entity --
      --------------------

      procedure Process_Entity (Current_X_File : Integer) is
         Is_Library_Level : Boolean;
         Name_Start, Name_End : Integer;
         pragma Unreferenced (Is_Library_Level);
      begin
         if Str (Index) = '.' then
            --  Same entity as before, so we do not change current entity

            Index := Index + 2;  --  First ref on that line

         else
            Get_Ref;
            Xref_File        := Current_X_File;
            Is_Library_Level := Get_Char = '*';

            Name_Start := Index;
            Skip_Word;

            Name_End := Name_Start + 1;

            --  Operators are quoted

            if Str (Name_Start) = '"' then
               while Str (Name_End) /= '"' loop
                  Name_End := Name_End + 1;
               end loop;
               Name_End := Name_End + 1;

            else
               --  Entity names can contain extra information, like
               --  pointed type,... So we need to extract the name
               --  itself and will store the extra information in a
               --  second step

               while Name_End < Index
                 and then Str (Name_End) /= '{'
                 and then Str (Name_End) /= '['
                 and then Str (Name_End) /= '<'
                 and then Str (Name_End) /= '('
               loop
                  Name_End := Name_End + 1;
               end loop;
            end if;

            declare
               Name : aliased String :=
                 String (Str (Name_Start .. Name_End - 1));
               R : Forward_Cursor;
            begin
               --  Check if we already know that entity.

               R.Fetch
                 (Session.DB,
                  Query_Find_Entity_From_Decl,
                  Params =>
                    (1 => +Name'Unrestricted_Access,
                     2 => +Current_X_File,
                     3 => +Xref_Line,
                     4 => +Xref_Col));

               if R.Has_Row then
                  Current_Entity := R.Integer_Value (0);

               elsif Perform_Entity_Insert then
                  --  Save the current entity

                  R.Fetch
                    (Session.DB,
                     Query_Insert_Entity,
                     Params =>
                       (1 => +Name'Unrestricted_Access,
                        2 => +Xref_Kind,       --  kind
                        3 => +Current_X_File,  --  decl_file
                        4 => +Xref_Line,       --  decl_line
                        5 => +Xref_Col));      --  decl_column

                  Current_Entity :=
                    R.Last_Id (Session.DB, Database.Entities.Id);

               else
                  Current_Entity := 1;
               end if;
            end;

            --  Process the extra information we had (pointed type,...)

            if Str (Name_End) = '=' then
               --  First, renaming info, as in
               --     17p4 S=17:30{83|45P9} 34r10
               --  Difficulty here is that after '=' we have the location of
               --  a reference, so we need to find the corresponding entity
               --  before we can insert in the database. We'll do that once we
               --  have inserted all other refs.
               --  ??? TBD

               Index := Name_End + 1;
               Get_Ref;
               Name_End := Index;
            end if;

            loop
               Index := Name_End + 1;
               Xref_File := Current_X_File;

               case Str (Name_End) is
                  when '[' =>
                     --  Instantiation reference, as in
                     --     5K12 G[1|3] 7r24 8r8 11r4
                     --  No column information

                     if not Get_Ref_Or_Predefined
                       (Endchar => ']', With_Col => False)
                     then
                        return;
                     end if;

                  when '<' =>
                     --  Points to the parent types as in
                     --     7I9 My_Integer<integer> 8r28
                     --     9R9*My_Tagged<7|2R9><8R9> 9e69
                     --  For an array, this is the index type (can be
                     --     duplicated when there are multiple indexes)
                     --  For an overriding operation, this points to the
                     --     overridden operation.

                     if not Get_Ref_Or_Predefined (Endchar => '>') then
                        return;
                     end if;

                  when '(' =>
                     --  Points to designated type or component type for array
                     --     6A9*My_Array(4I9)<3I9>
                     --  where 4I9 is component type, and 3I9 is index type

                     if not Get_Ref_Or_Predefined (Endchar => ')') then
                        return;
                     end if;

                  when '{' =>
                     --  Points to ancestor type for subtypes
                     --  Points to result type for functions
                     --  Points to enum type for enumeration literal
                     --  Points to type for objects and components

                     if not Get_Ref_Or_Predefined (Endchar => '}') then
                        return;
                     end if;

                  when ' ' =>
                     exit;

                  when ASCII.LF =>
                     --  For the next call to Next_Line
                     Index := Name_End;
                     return;

                  when others =>
                     if Active (Me_Error) then
                        Trace
                          (Me_Error, "Unexpected character in ALI: "
                           & Character'Image (Str (Name_End))
                           & " in '"
                           & String
                             (Str (Name_End
                                   .. Integer'Min (Name_End + 20, Last)))
                           & "'");
                     end if;

                     return;
               end case;

               Name_End := Index;
            end loop;

            Index := Name_End;
            Xref_File := Current_X_File;

            while Index <= Last
              and then Str (Index) /= ASCII.LF
            loop
               Skip_Spaces;
               Get_Ref;
               Skip_Instance_Info;

               if Perform_Entity_Insert then
                  Session.DB.Execute
                    (Query_Insert_Ref,
                     Params => (1 => +Current_Entity,
                                2 => +Xref_File,
                                3 => +Xref_Line,
                                4 => +Xref_Col,
                                5 => +Xref_Kind));
               end if;
            end loop;
         end if;

         --  Index now points to the first ref on the current line.
      end Process_Entity;

   begin
      if Active (Me_Debug) then
         Trace (Me_Debug, "Parse LI "
                & Library_File.Display_Full_Name);
      end if;

      M := Open_Read
        (Filename              => +Library_File.Full_Name.all,
         Use_Mmap_If_Available => True);
      Read (M);

      Str := Data (M);
      Last := GNATCOLL.Mmap.Last (M);
      Index := Str'First;

      ALI_Id := Insert_File
        (Basename => Library_File.Display_Full_Name,
         Language => "li");
      --  ??? Should also clear all known xref from this file, or perhaps from
      --  its units ?

      loop
         Next_Line;
         exit when Index > Last;

         case Str (Index) is
            when 'U' =>
               --  Describes a unit associated with the LI file

               Index := Index + 2;
               Skip_Word;  --  Skip unit name
               Skip_Spaces;
               Start := Index;
               Skip_Word;

               Current_Unit_Id := Insert_File
                 (Basename => String (Str (Start .. Index - 1)),
                  Language => "ada",
                  Clear    => True);

               if Current_Unit_Id /= -1 then
                  Session.DB.Execute
                    (Query_Set_ALI,
                     Params => (1 => +Current_Unit_Id,
                                2 => +ALI_Id));
               end if;

            when 'W' =>
               --  Describes a "with" dependency with the last seen U line

               Index := Index + 2;
               Skip_Word;
               Skip_Spaces;
               Start := Index;
               Skip_Word;

               Dep_Id := Insert_File
                 (Basename => String (Str (Start .. Index - 1)),
                  Language => "ada");

               if Dep_Id /= -1 and then Current_Unit_Id /= -1 then
                  Session.DB.Execute
                    (Query_Set_File_Dep,
                     Params => (1 => +Current_Unit_Id, 2 => +Dep_Id));
               end if;

            when 'D' =>
               --  All dependencies for all units (used as indexes in xref)

               Index := Index + 2;
               Start := Index;
               Skip_Word;

               Dep_Id := Insert_File
                 (Basename => String (Str (Start .. Index - 1)),
                  Language => "ada");

               Depid_To_Id.Set_Length (Ada.Containers.Count_Type (D_Line_Id));
               Depid_To_Id.Replace_Element
                 (Index    => D_Line_Id,
                  New_Item => Dep_Id);

               D_Line_Id := D_Line_Id + 1;

            when 'X' =>
               --  From now on till first empty line, only xref info

               Index := Index + 2;

               declare
                  Current_X_File : constant Integer :=
                      Depid_To_Id.Element (Get_Natural);
                  --  "files" id for the current X section

               begin
                  --  Now parse all entities for this file
                  loop
                     Next_Line;

                     --  Do we still have a reference within that X section ?
                     --  If not, move back to previous line, because the outer
                     --  loop already moves to the next line.

                     if Index > Last
                       or else (Str (Index) not in '0' .. '9'
                                and then Str (Index) /= '.')
                     then
                        Index := Index - 2;
                        exit;
                     end if;

                     --  If we know the source file in which the entity is
                     --  declared, we can insert it and all its refs. Otherwise
                     --  we just ignore it.

                     if Current_X_File /= -1 then
                        Process_Entity (Current_X_File);
                     end if;
                  end loop;
               end;

            when others =>
               null;
         end case;
      end loop;

      Close (M);
   end Parse_LI;

   ------------------------
   -- Parse_All_LI_Files --
   ------------------------

   procedure Parse_All_LI_Files
     (Session : Session_Type;
      Tree    : Project_Tree;
      Project : Project_Type)
   is
      use Library_Info_Lists;
      LI_Files  : Library_Info_Lists.List;
      Start     : Time := Clock;
      VFS_To_Id : VFS_To_Ids.Map;
      Has_Pragma : Boolean := True;

   begin
      --  Disable checks for foreign keys. This saves a bit of time when
      --  inserting the new references. At worse we could end up with an
      --  entity or a reference whose kind does not match an entry in the
      --  *_kind tables, and the xref will not show later on in query, but
      --  that's easily fixed by adding the new entry in the *_kind table (that
      --  is when the ALI file has changed format)
      --  Since this is sqlite specific, we test whether the backend supports
      --  this.

      Session.DB.Execute ("PRAGMA foreign_keys=OFF");
      if not Session.DB.Success then
         Has_Pragma := False;
         Session.Rollback;
      end if;

      if Has_Pragma then
         Session.DB.Execute ("PRAGMA synchronous=OFF");
         --  Session.DB.Execute ("PRAGMA count_changes=OFF");  --  Deprecated
         Session.DB.Execute ("PRAGMA journal_mode=MEMORY");
         Session.DB.Execute ("PRAGMA temp_store=MEMORY");
      end if;

      Project.Library_Files
        (Recursive => True, Xrefs_Dirs => True, Including_Libraries => True,
         ALI_Ext => ".ali", List => LI_Files);
      Project.Library_Files
        (Recursive => True, Xrefs_Dirs => True, Including_Libraries => True,
         ALI_Ext => ".gli", List => LI_Files);

      Put_Line ("Number of .ali + .gli files:" & Length (LI_Files)'Img
                & " (" & Duration'Image (Clock - Start) & " seconds)");

      Start := Clock;
      for Lib_Info of LI_Files loop
         Parse_LI (Session                   => Session,
                   Tree                      => Tree,
                   Library_File              => Lib_Info.Library_File,
                   Source_File               => Lib_Info.Source_File,
                   VFS_To_Id                 => VFS_To_Id,
                   Ignore_Ref_In_Other_Files => True);
      end loop;

      --  It might be safer to commit after each LI file, but this is much
      --  slower. At worse, if there is an error parsing one file, we are not
      --  going to load anything, and the files will be loaded on demand later.
      Session.Commit;

      if Has_Pragma then
         Session.DB.Execute ("PRAGMA foreign_keys=ON");

         --  The default would be FULL, but we do not need to prevent against
         --  system crashes in this application.
         Session.DB.Execute ("PRAGMA synchronous=NORMAL");

         --  The default would be DELETE, but we do not care enough about
         --  data integrity
         Session.DB.Execute ("PRAGMA journal_mode=MEMORY");

         --  We can store temporary tables in memory
         Session.DB.Execute ("PRAGMA temp_store=MEMORY");

         --  Gather statistic to speed up the query optimizer
         Session.DB.Execute ("ANALYZE");
         Session.Commit;
      end if;

      Put_Line
        ("Done parsing files:" & Duration'Image (Clock - Start) & " seconds");
   end Parse_All_LI_Files;

end Entities_Db;
