------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Containers;            use Ada.Containers;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Basic_Types;               use Basic_Types;
with Language_Handlers;         use Language_Handlers;
with Language;                  use Language;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with Traces;

with Entities.Construct_Assistant; use Entities.Construct_Assistant;

package body Entities.Queries is

   Me     : constant Trace_Handle := Create ("Entities.Queries", Off);
   Callers_Me : constant Trace_Handle := Create ("Entities.Callers", Off);
   Ref_Me : constant Trace_Handle := Create ("Entities.Ref", Off);
   Constructs_Heuristics : constant Trace_Handle :=
     Create ("Entities.Constructs", On);

   Num_Columns_Per_Line : constant := 250;
   --  The number of columns in each line, when computing the proximity of a
   --  match. This is an approximate number, for efficiency. Big values mean
   --  that we give advantage to matches on the same line rather than on the
   --  same column.

   Use_Approximate_Overriding_Algorithm : constant Boolean := False;
   --  If set to True, GPS tries to find the list of overriding or overriden
   --  entities in Find_All_References through an approximate algorithm: it
   --  searchs for parent or children types, and for their primitive operations
   --  with the same name as the current entity.
   --  If set to False, we assume that GNAT itself provides an accurate
   --  information and that calls to Overriden_Entity returns the exact
   --  information.
   --  This should be set to True for versions of GNAT <= 20050718

   use Entities_Hash;
   use Entity_Information_Arrays;
   use Entity_File_Maps;
   use Entities_In_File_Sets;
   use Source_File_Arrays;
   use Dependency_Arrays;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LI_Information_Iterator'Class, LI_Information_Iterator_Access);

   procedure Find
     (EL              : Entity_Information_List_Access;
      File            : Source_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information;
      Closest_Ref     : in out Entity_Reference);
   --  Check in EL the entities which has a reference as close as possible
   --  to (Line, Column). Distance is the initial closest distance known, and
   --  is changed to reflect the result of the find. It is set to 0 if an
   --  exact match was found.
   --  If Check_Decl_Only is True, then it only tries to match a declaration,
   --  and doesn't check references to the entity.

   procedure Find
     (Source                 : Source_File;
      Normalized_Entity_Name : Symbol := No_Symbol;
      Line                   : Integer;
      Column                 : Basic_Types.Visible_Column_Type;
      Check_Decl_Only        : Boolean;
      Entity                 : out Entity_Information;
      Closest_Ref            : out Entity_Reference;
      Status                 : out Find_Decl_Or_Body_Query_Status);
   --  Find the closest entity to (Line, Column) in Source

   procedure Find_Any_Entity
     (File            : Source_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information;
      Closest_Ref     : in out Entity_Reference);
   --  Find the entity in File which is referenced at the given location

   procedure Find_Any_Entity
     (Trie            : Entities_Hash.Instance;
      File            : Source_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information;
      Closest_Ref     : in out Entity_Reference);
   --  Same as above, but restricted to a subset of the entities only

   function Get_Start_Of_Scope_In_File
     (Entity     : Entity_Information;
      File       : Source_File;
      Force_Spec : Boolean) return File_Location;
   function Get_End_Of_Scope_In_File
     (Entity     : Entity_Information;
      File       : Source_File;
      Force_Spec : Boolean) return File_Location;
   --  Get the range of lines that Entity encloses in File.
   --  If Entity doesn't have the notion of enclosed lines (a simple integer
   --  for instance), then No_File_Location is returned.
   --  If Force_Spec is True, then the scope of the spec is returned, otherwise
   --  the scope of the body is returned.

   type Scope_Tree;
   type Scope_Tree_Access is access Scope_Tree;
   type Scope_Tree is record
      Sibling     : Scope_Tree_Access;
      First_Child : Scope_Tree_Access;

      Entity      : Entity_Information;
      Start_Line  : Integer;
      End_Line    : Integer;
   end record;
   --  A structure that contains entities and their enclosed lines (scopes)

   procedure Free (Tree : in out Scope_Tree_Access);
   --  Free the memory occupied by the tree

   procedure Add_To_Tree
     (Tree           : in out Scope_Tree_Access;
      Entity         : Entity_Information;
      Start_Of_Scope : File_Location;
      End_Of_Scope   : File_Location);
   --  Add a new entity to Tree, given its scope

   function In_Range
     (Ref        : File_Location;
      In_File    : Source_File;
      Start_Line : Integer;
      Last_Line  : Integer) return Boolean;
   pragma Inline (In_Range);
   --  True if Ref is in the scope given by the other parameters

   procedure Compute_Callers_And_Called (File : Source_File);
   --  Compute all the calls/called by relationships in the File

   function Get_Entity_From_Ref
     (Entity : Entity_Information; Ref : E_Reference)
      return Entity_Information;
   --  Return the actual entity corresponding to a specific reference to
   --  Entity.

   procedure Setup_For_Entity
     (Iter   : in out Entity_Reference_Iterator;
      Entity : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_Scope : Entity_Information := null);
   --  Setup Iter to search for references to Entity

   procedure Add_Overriding_Subprograms
     (Iter                   : in out Entity_Reference_Iterator;
      Overriding_Subprograms : Boolean := True;
      Overridden_Subprograms : Boolean := True);
   --  Setup Iter so that it will also return references to all entities
   --  overriden or overriding by Iter.Entity

   function To_String (Location : File_Location) return String;
   --  For debugging purposes only

   function Get_Parent_Or_Child_Types
     (Entity    : Entity_Information;
      Parents   : Boolean;
      Recursive : Boolean) return Entity_Information_Array;
   --  This subprogram returns either the parents or the children of an
   --  entity. This factorizes code between Get_Parent_Types and
   --  Get_Child_Types.

   procedure Add_Child_Types
     (Iter   : in out Children_Iterator;
      Entity : Entity_Information);
   --  Add all child types of Entity to the results of Iter

   procedure Dump (T : Scope_Tree_Access; Prefix : String);
   --  ???

   ---------------
   -- To_String --
   ---------------

   function To_String (Location : File_Location) return String is
   begin
      if Location = No_File_Location then
         return "<no loc>";
      else
         return (+Full_Name (Get_Filename (Location.File)))
           & ':' & Location.Line'Img
           & ':' & Location.Column'Img;
      end if;
   end To_String;

   ----------
   -- Find --
   ----------

   procedure Find
     (EL              : Entity_Information_List_Access;
      File            : Source_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information;
      Closest_Ref     : in out Entity_Reference)
   is
      Prox : Integer;
      E   : Entity_Information;
      Ref : E_Reference;
   begin
      if EL /= null then
         For_Each_Entity :
         for Ent in Entity_Information_Arrays.First .. Last (EL.all) loop
            E := EL.Table (Ent);

            if E.LI_Declaration.File = File then
               Prox := Natural (abs (E.LI_Declaration.Column - Column)) +
               abs (E.LI_Declaration.Line - Line) * Num_Columns_Per_Line;

               if Prox < Distance then
                  Closest := E;
                  Closest_Ref := Declaration_As_Reference (E);
                  Distance := Prox;
                  exit For_Each_Entity when Distance = 0;
               end if;
            end if;

            if not Check_Decl_Only then
               declare
                  It : Entity_Reference_Cursor := First (E.References);
               begin
                  while It /= Null_Entity_Reference_Cursor loop
                     Ref := Element (It);

                     if Is_Real_Reference (Ref.Kind)
                       and then Ref.Location.File = File
                     then
                        Prox := Natural (abs (Ref.Location.Column - Column)) +
                        abs (Ref.Location.Line - Line) * Num_Columns_Per_Line;

                        if Prox < Distance then
                           Closest := E;
                           Closest_Ref := (E, Index (It));
                           Distance := Prox;

                           exit For_Each_Entity when Distance = 0;
                        end if;
                     end if;

                     It := Next (It);
                  end loop;
               end;
            end if;
         end loop For_Each_Entity;

      else
         Trace (Me, "Find: no Entity List");
      end if;
   end Find;

   ---------------------
   -- Find_Any_Entity --
   ---------------------

   procedure Find_Any_Entity
     (Trie            : Entities_Hash.Instance;
      File            : Source_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information;
      Closest_Ref     : in out Entity_Reference)
   is
      Iter : Entities_Hash.Cursor;
      UEI  : Entity_Informations;
   begin
      Get_First (Trie, Iter);

      loop
         UEI := Get_Element (Iter);
         exit when UEI = null;

         Find (UEI.List, File, Line, Column,
               Check_Decl_Only, Distance, Closest, Closest_Ref);
         exit when Distance = 0;

         Get_Next (Trie, Iter);
      end loop;
   end Find_Any_Entity;

   ---------------------
   -- Find_Any_Entity --
   ---------------------

   procedure Find_Any_Entity
     (File            : Source_File;
      Line            : Integer;
      Column          : Basic_Types.Visible_Column_Type;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information;
      Closest_Ref     : in out Entity_Reference) is
   begin
      Find_Any_Entity
        (File.Entities, File, Line, Column, Check_Decl_Only,
         Distance, Closest, Closest_Ref);

      if Distance /= 0 and then not Check_Decl_Only then
         Find_Any_Entity
           (File.All_Entities, File, Line, Column,
            Check_Decl_Only, Distance, Closest, Closest_Ref);
      end if;
   end Find_Any_Entity;

   ----------
   -- Find --
   ----------

   procedure Find
     (Source                 : Source_File;
      Normalized_Entity_Name : Symbol := No_Symbol;
      Line                   : Integer;
      Column                 : Basic_Types.Visible_Column_Type;
      Check_Decl_Only        : Boolean;
      Entity                 : out Entity_Information;
      Closest_Ref            : out Entity_Reference;
      Status                 : out Find_Decl_Or_Body_Query_Status)
   is
      Case_Sensitive : constant Boolean :=
                         not Case_Insensitive_Identifiers (Source.Handler);
      Distance       : Integer := Integer'Last;
      Closest        : Entity_Information;
      UEI            : Entity_Informations;
      Iter           : Entity_Iterator;

   begin
      if Active (Me) then
         Trace (Me, "Find name=" & Get (Normalized_Entity_Name, True).all
                & " Source=" & Display_Full_Name (Get_Filename (Source))
                & " line=" & Line'Img & " column=" & Column'Img
                & " check_decl=" & Check_Decl_Only'Img);
      end if;

      Closest_Ref := No_Entity_Reference;

      if Normalized_Entity_Name = No_Symbol then
         Find_Any_Entity
           (Source, Line, Column, Check_Decl_Only, Distance, Closest,
            Closest_Ref);
      else
         UEI := Get (Source.Entities,
                     (Str => Normalized_Entity_Name,
                      Case_Sensitive => Case_Sensitive));
         if UEI /= null then
            Find
              (UEI.List,
               Source, Line,
               Column, Check_Decl_Only, Distance, Closest, Closest_Ref);
            Trace (Me, "After find in entities: distance=" & Distance'Img);
         end if;

         if Distance /= 0 and then not Check_Decl_Only then
            UEI := Get (Source.All_Entities,
                        (Str => Normalized_Entity_Name,
                         Case_Sensitive => Case_Sensitive));
            if UEI /= null then
               Find (UEI.List,
                     Source, Line, Column, Check_Decl_Only, Distance, Closest,
                     Closest_Ref);
               Trace (Me, "After find in all entities: distance="
                      & Distance'Img);
            end if;
         end if;
      end if;

      if Distance = 0 then
         Status := Success;
         Entity := Closest;

      elsif Distance < Num_Columns_Per_Line then
         --  If we found an entity on the same line, assume this is the right
         --  entity, even if there are overloadings.
         --  Improves the situation, in particular with C/C++ xrefs where
         --  column info is not always accurate.

         Status := Fuzzy_Match;
         Entity := Closest;

      elsif Distance = Integer'Last then
         Status      := Entity_Not_Found;
         Entity      := null;
         Closest_Ref := No_Entity_Reference;

      else
         --  How many entities with this name do we have ?

         Find_All_Entities_In_File
           (Iter, File => Source,
            Name => Get (Normalized_Entity_Name, True).all);

         if Get (Iter) /= null then
            Next (Iter);

            if Get (Iter) = null then
               Status := Fuzzy_Match;
            else
               Status := Overloaded_Entity_Found;
            end if;

         else
            Status := Fuzzy_Match;
         end if;

         Entity := Closest;
      end if;
   end Find;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : GNATCOLL.VFS.Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False)
   is
      Ref : Entity_Reference;
   begin
      Find_Declaration
        (Db, File_Name, Entity_Name, Line, Column, Entity, Ref, Status,
         Check_Decl_Only);
   end Find_Declaration;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : GNATCOLL.VFS.Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out Entity_Information;
      Closest_Ref     : out Entity_Reference;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False)
   is
      Handler : constant LI_Handler := Get_LI_Handler (Db, File_Name);
      Source  : Source_File;
   begin
      Status := Entity_Not_Found;
      Entity := null;

      if Handler /= null then
         --  Get a Source_File, but do not update its LI information, since
         --  that will be done in the call to Find_Declaration below. Therefore
         --  use Get_Or_Create instead of Get_Source_Info

         Source := Get_Or_Create
           (Db           => Db,
            File         => File_Name,
            LI           => null,
            Handler      => Handler,
            Allow_Create => True);

         if Source /= null then
            Find_Declaration
              (Db, Source, Entity_Name, Line, Column, Entity, Closest_Ref,
               Status, Check_Decl_Only, Handler);
            return;
         end if;
      end if;

      Trace (Me, "No such file registered: " & Display_Full_Name (File_Name));
   end Find_Declaration;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Db              : Entities_Database;
      Source          : Source_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Basic_Types.Visible_Column_Type;
      Entity          : out Entity_Information;
      Closest_Ref     : out Entity_Reference;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False;
      Handler         : LI_Handler := null;
      Fuzzy_Expected  : Boolean := False)
   is
      S_Entity_Name : Symbol := No_Symbol;
      H       : LI_Handler := Handler;
      Updated : Source_File;
      New_Entity : Entity_Information;
      New_Location : File_Location;
   begin
      if Entity_Name /= "" then
         S_Entity_Name := Db.Symbols.Find (Entity_Name);
      end if;

      if Active (Me) then
         if Source /= null then
            Trace (Me, "Find_Declaration entity=" & Entity_Name
                   & " source=" & Display_Full_Name (Get_Filename (Source))
                   & " line=" & Line'Img
                   & " column=" & Column'Img);
         else
            Trace (Me, "Find_Declaration entity=" & Entity_Name
                   & " source=<null>"
                   & " line=" & Line'Img
                   & " column=" & Column'Img);
         end if;
      end if;

      if Source = null then
         Status := Entity_Not_Found;
         Entity := null;
         Trace (Me, "Entity not found");
         return;
      end if;

      if Source = Get_Predefined_File (Db, Source.Handler) then
         Entity := Get_Or_Create
           (Name         => S_Entity_Name,
            File         => Source,
            Line         => Line,
            Column       => Column,
            Allow_Create => True);
         Status := Success;
         Trace (Me, "Found in predefined package");
         return;
      end if;

      if H = null then
         H := Get_LI_Handler (Db, Get_Filename (Source));
      end if;

      Status := Entity_Not_Found;
      Entity := null;

      if H /= null then
         --  Updates LI information
         Updated := Get_Source_Info (H, Get_Filename (Source));

         if Updated /= null then
            Find (Source, S_Entity_Name, Line, Column, Check_Decl_Only,
                  Entity, Closest_Ref, Status);
         end if;

         Trace (Me, "Result=" & Status'Img);
      else
         Status := Entity_Not_Found;
         Entity := null;
         Trace (Me, "Entity not found");
      end if;

      if Active (Constructs_Heuristics)
        and then Db.Construct_Db_Locks = 0
        and then
          (Status = Entity_Not_Found
           or else
             ((Status = Fuzzy_Match
               or else Status = Overloaded_Entity_Found)
              and then not Fuzzy_Expected))
      then
         declare
            Tree_Lang : constant Tree_Language_Access :=
                         Get_Tree_Language_From_File
                           (Language_Handler (Db.Lang), Source.Name);
            S_File : constant Structured_File_Access :=
              Get_Or_Create
                (Db   => Db.Construct_Db,
                 File => Source.Name);

            Result    : Entity_Access;
         begin
            if not Is_Null (S_File) then
               --  In some cases, the references are extracted from a place
               --  where there is still an ALI file, but no more source file.
               --  This will issue a null Structured_File_Access, which is what
               --  we're protecting the following code by the above condition.

               Update_Contents (S_File);

               Result := Tree_Lang.Find_Declaration
                 (S_File, Line, To_Line_String_Index (S_File, Line, Column));

               if Result /= Null_Entity_Access then
                  --  First, try to see if there's already a similar entity in
                  --  the database. If that's the case, it's better to use it
                  --  than the dummy one created from the construct.

                  New_Location :=
                    (File     => Get_Or_Create
                       (Db    => Db,
                        File  => Get_File_Path (Get_File (Result))),
                     Line   => Get_Construct (Result).Sloc_Entity.Line,
                     Column => To_Visible_Column
                       (Get_File (Result),
                        Get_Construct (Result).Sloc_Entity.Line,
                        String_Index_Type
                          (Get_Construct (Result).Sloc_Entity.Column)));

                  New_Entity := Get_Or_Create
                    (Name         => Get_Construct (Result).Name,
                     File         => New_Location.File,
                     Line         => New_Location.Line,
                     Column       => New_Location.Column,
                     Allow_Create => False);

                  if New_Entity /= null then
                     --  If we found an updated ALI entity, use it.

                     Entity := New_Entity;
                  elsif Entity /= null then
                     --  Else, if we had a fuzzy entity, use it to initalize
                     --  the LI entity from the construct database.

                     Entity.Live_Declaration := New_Location;
                  else
                     --  If we have no entity to connect to, then create one
                     --  from the construct database.

                     Entity := To_LI_Entity (Result);
                  end if;

                  Status := Fuzzy_Match;
               end if;
            end if;
         end;
      elsif Entity /= null then
         --  If we found an accurate match without the construct database, it
         --  means that the LI declaration is the accurate live declaration.
         --  Update it accordingly, in case it has been moved previously.

         Entity.Live_Declaration := Entity.LI_Declaration;
      end if;
   end Find_Declaration;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Entity               : Entity_Information;
      Current_Location     : File_Location := No_File_Location;
      Location             : out File_Location;
      No_Location_If_First : Boolean := False)
   is

      function Is_Expected_Construct
        (Location : File_Location) return Boolean;
      --  Return true if the location given in parameter indeed corresponds to
      --  a declaration construct, false otherwise, typically when the file has
      --  been modified and the ali retreived is not up to date.
      --  Note that if the construct database is deactivated, this will always
      --  return true (we're always on the expected construct, we don't expect
      --  anything in particular).

      function Extract_Next_By_Heuristics return File_Location;
      --  Return the next location using the construct heuristics

      function Get_Entity_At_Location
        (Loc : File_Location) return Entity_Access;
      --  Return the construct entity found at the location given in parameter.

      --------------------------------
      -- Extract_Next_By_Heuristics --
      --------------------------------

      function Extract_Next_By_Heuristics return File_Location is
         Result : File_Location;

         C_Entity, New_Entity : Entity_Access := Null_Entity_Access;
         Db : constant Entities_Database :=
           Entity.Live_Declaration.File.Db;

      begin
         --  In order to locate the reference to look from, we check if there
         --  is a file associated to the input location. In certain cases, this
         --  location is computed from a context that does not have file
         --  information, so for safety purpose, we check that the file exist
         --  (there's nothing we can do at the completion level without a
         --  file). If there's no file, then the context has been partially
         --  provided (or not at all) so we start from the declaration of the
         --  Entity.

         if Active (Constructs_Heuristics)
           and then Db.Construct_Db_Locks = 0
         then
            if Current_Location.File /= null then
               C_Entity := Get_Entity_At_Location (Current_Location);
            end if;

            if C_Entity = Null_Entity_Access then
               C_Entity := Get_Entity_At_Location (Entity.Live_Declaration);
            end if;

            if C_Entity /= Null_Entity_Access then
               declare
                  S_File : constant Structured_File_Access :=
                    Get_File (C_Entity);

                  Tree_Lang : constant Tree_Language_Access :=
                    Get_Tree_Language_From_File
                      (Language_Handler (Db.Lang), Get_File_Path (S_File));
               begin
                  New_Entity := Tree_Lang.Find_Next_Part (C_Entity);

                  --  If we're initializing a loop, e.g. the current location
                  --  is no location, then return the result. Otherwise, don't
                  --  return it if we got back to the initial body and the
                  --  caller doesn't want to loop back.

                  if Current_Location /= No_File_Location
                    and then No_Location_If_First
                    and then C_Entity = Tree_Lang.Find_First_Part (C_Entity)
                  then
                     return No_File_Location;
                  end if;

                  if New_Entity /= C_Entity then
                     Result.File :=
                       Get_Or_Create
                         (Db           => Db,
                          File         =>
                            Get_File_Path (Get_File (New_Entity)));
                     Result.Line :=
                       Get_Construct (New_Entity).Sloc_Entity.Line;

                     Result.Column := To_Visible_Column
                       (Get_File (New_Entity),
                        Get_Construct (New_Entity).Sloc_Entity.Line,
                        String_Index_Type
                          (Get_Construct (New_Entity).Sloc_Entity.Column));

                     return Result;
                  end if;
               end;
            end if;
         end if;

         return No_File_Location;
      end Extract_Next_By_Heuristics;

      ---------------------------
      -- Is_Expected_Construct --
      ---------------------------

      function Is_Expected_Construct
        (Location : File_Location) return Boolean
      is
         C_Entity : Entity_Access;
         Db       : constant Entities_Database  := Location.File.Db;
      begin
         if Active (Constructs_Heuristics)
           and then Location.File.Db.Construct_Db_Locks = 0
         then
            C_Entity := Get_Entity_At_Location (Location);

            --  Return true if we found a construct here and if it's of the
            --  appropriate name.

            return C_Entity /= Null_Entity_Access
              and then Get_Identifier (C_Entity) = Find_Normalized
              (Get_Symbols (Db), Get (Entity.Name).all);
         end if;

         return True;
      end Is_Expected_Construct;

      ----------------------------
      -- Get_Entity_At_Location --
      ----------------------------

      function Get_Entity_At_Location
        (Loc : File_Location)
            return Entity_Access
      is
         Db : constant Entities_Database := Loc.File.Db;
         S_File : constant Structured_File_Access :=
           Get_Or_Create
             (Db   => Db.Construct_Db,
              File => Loc.File.Name);
         Construct : Construct_Tree_Iterator;
      begin
         Update_Contents (S_File);

         Construct :=
           Get_Iterator_At
             (Tree      => Get_Tree (S_File),
              Location  => To_Location
                (Loc.Line,
                 To_Line_String_Index
                   (S_File,
                    Loc.Line,
                    Loc.Column)),
              From_Type => Start_Name);

         if Construct /= Null_Construct_Tree_Iterator then
            return To_Entity_Access (S_File, Construct);
         else
            return Null_Entity_Access;
         end if;
      end Get_Entity_At_Location;

      Ref          : E_Reference;
      First_Entity : Entity_Reference_Cursor :=
                       Null_Entity_Reference_Cursor;
      Return_Next  : Boolean := Current_Location = No_File_Location;
      It           : Entity_Reference_Cursor;

      Bodies_Found : Integer := 0;
      H_Loc : File_Location;

   begin
      if Entity = null then
         Location := No_File_Location;
         return;
      end if;

      if Active (Me) then
         Trace (Me, "Find_Next_Body for "
                & Debug_Name (Entity)
                & " current=" & To_String (Current_Location));
      end if;

      --  First, try to get the next entity using the ali files

      Update_Xref (Entity.LI_Declaration.File);

      It := First (Entity.References);

      while It /= Null_Entity_Reference_Cursor loop
         Ref := Element (It);

         if Ref.Kind = Body_Entity
           or else Ref.Kind = Completion_Of_Private_Or_Incomplete_Type
         then
            Bodies_Found := Bodies_Found + 1;

            if Entity.Is_Imported then
               Location := Ref.Location;
               return;

            elsif Return_Next then
               Location := Ref.Location;

               if not Is_Expected_Construct (Location) then
                  H_Loc := Extract_Next_By_Heuristics;

                  if H_Loc /= No_File_Location then
                     Location := H_Loc;
                  end if;
               end if;

               return;
            end if;

            if First_Entity = Null_Entity_Reference_Cursor then
               First_Entity := It;
            end if;
         end if;

         if Ref.Location = Current_Location then
            Return_Next := True;
         end if;

         It := Next (It);
      end loop;

      --  If no next body has been found at this stage, try to see what we can
      --  do using the construct database.

      H_Loc := Extract_Next_By_Heuristics;

      if H_Loc /= No_File_Location then
         if First_Entity = Null_Entity_Reference_Cursor then
            --  Case 1, we didn't found anything at all, so just use the
            --  information coming from the construct database

            Location := H_Loc;
            return;
         else
            --  Case 2, we did find 'First_Entity'. Check if it's at the
            --  expected location and that it's OK to return the first entity.

            if not No_Location_If_First
              and then not Is_Expected_Construct
                (Element (First_Entity).Location)
            then

               Location := H_Loc;
               return;
            end if;
         end if;
      end if;

      --  If we don't have any more information to extract from the construct
      --  database, then return the first entity if allowed by the flags, or
      --  null.

      if No_Location_If_First
        or else First_Entity = Null_Entity_Reference_Cursor
      then
         Location := No_File_Location;
      else
         Location := Element (First_Entity).Location;
      end if;
   end Find_Next_Body;

   ---------------------
   -- Is_Discriminant --
   ---------------------

   function Is_Discriminant
     (Discr, Entity : Entity_Information) return Boolean
   is
      Iter : Entity_Reference_Iterator;
   begin
      Find_All_References
        (Iter    => Iter,
         Entity  => Entity,
         In_File => Get_File (Get_Declaration_Of (Entity)),
         Filter  => (Discriminant => True, others => False));

      while not At_End (Iter) loop
         if Get_Entity (Iter) = Discr then
            Destroy (Iter);
            return True;
         end if;
         Next (Iter);
      end loop;

      Destroy (Iter);
      return False;
   end Is_Discriminant;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Loc : File_Location; Entity : Entity_Information) return Boolean
   is
      Bod         : File_Location := No_File_Location;
      Start, Last : Integer;
   begin
      --  An entity is never in its own range
      if Loc = Get_Declaration_Of (Entity) then
         return False;
      end if;

      if Entity.LI_Declaration.File = Loc.File then
         Start := Get_Start_Of_Scope_In_File
           (Entity, Loc.File, Force_Spec => True).Line;
         Last  := Get_End_Of_Scope_In_File
           (Entity, Loc.File, Force_Spec => True).Line;
         if In_Range (Loc, Entity.LI_Declaration.File, Start, Last) then
            return True;
         end if;
      end if;

      loop
         Find_Next_Body
           (Entity, Bod, Location => Bod, No_Location_If_First => True);
         exit when Bod = No_File_Location
           or else Bod.File = Loc.File;
      end loop;

      if Bod /= No_File_Location then
         Start := Get_Start_Of_Scope_In_File
           (Entity, Loc.File, Force_Spec => False).Line;
         Last  := Get_End_Of_Scope_In_File
           (Entity, Loc.File, Force_Spec => False).Line;
         if In_Range (Loc, Bod.File, Start, Last) then
            return True;
         end if;
      end if;

      return False;
   end In_Range;

   ----------------------
   -- Setup_For_Entity --
   ----------------------

   procedure Setup_For_Entity
     (Iter                  : in out Entity_Reference_Iterator;
      Entity                : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_Scope              : Entity_Information := null)
   is
      Deps : Dependency_Iterator;
   begin
      if Active (Me) then
         Increase_Indent
           (Me, "Setup_For_Entity " & Debug_Name (Entity)
            & " declared at "
            & (+Base_Name
              (Get_Filename (Get_Declaration_Of (Entity).File))
              & Get_Declaration_Of (Entity).Line'Img));
      end if;

      if Iter.In_File = null then
         Find_Ancestor_Dependencies
           (Deps,
            File                  => Get_File (Get_Declaration_Of (Entity)),
            File_Has_No_LI_Report => File_Has_No_LI_Report,
            Include_Self          => True);
      else
         Find_Ancestor_Dependencies
           (Deps,
            File                  => Iter.In_File,
            File_Has_No_LI_Report => File_Has_No_LI_Report,
            Include_Self          => True,
            Single_Source_File    => True);
      end if;

      Iter.Entity := Entity;

      Iter.Files_It := Entity.References.First;

      Iter.Files_Analyzed.Clear;

      --  ??? We are looking at potentially outdated references. Therefore we
      --  need to refresh the corresponding ALI files, but that will also be
      --  done later on when looking for the ancestor dependencies. Shouldn't
      --  we simply start by updating all LI files, and then return the
      --  references ?
      --  However, disabling this code breaks refactoring (refactoring.1)

      if Iter.Files_It /= Entity_File_Maps.No_Element then
         declare
            Prev_Timestamp : Integer;
         begin
            loop
               Prev_Timestamp := Entity.File_Timestamp_In_References;

               --  ??? We might update the same file multiple times if there
               --  are multiple references ?
               Update_Xref (Element (Iter.Files_It).File);

               if Prev_Timestamp = Entity.File_Timestamp_In_References then
                  --  If we didn't add any file, then we can start extracting
                  --  references.

                  Iter.Files_Analyzed.Insert (Key (Iter.Files_It));
                  Iter.Entity_It := Element (Iter.Files_It).Refs.First;

                  exit;

               else
                  --  Otherwise, we need to retreive the first file because
                  --  the list has changed and the iterator is no longer valid,
                  --  and redo the xrefs update on that new first entry if
                  --  it's not the same.

                  Iter.Files_It := Entity.References.First;

                  if Iter.Files_It = Entity_File_Maps.No_Element then
                     --  All files may have been removed at this stage, so exit
                     --  the loop if that's the case.

                     exit;
                  end if;
               end if;
            end loop;
         end;
      end if;

      Iter.Decl_Returned := not Iter.Filter (Declaration)
        or else
          (In_Scope /= null
           and then not In_Range
             (Iter.Entity.LI_Declaration,
              Iter.In_File, Iter.Start_Line, Iter.Last_Line));

      if not Iter.Decl_Returned
        and then (Iter.In_File = null
                  or else Iter.In_File = Iter.Entity.LI_Declaration.File)
      then
         Update_Xref (Iter.Entity.LI_Declaration.File);
      end if;

      Iter.Deps := Deps;

      if Active (Me) then
         Decrease_Indent (Me, "Done Setup_For_Entity");
      end if;

      --  If the first reference in the list is not correct, move to next one

      if Iter.Decl_Returned
        and then
          (Length (Entity.References) = 0
           or else Iter.Entity_It = Entities_In_File_Sets.No_Element
           or else not Iter.Filter (Element (Iter.Entity_It).Kind)
           or else not In_Range
             (Element (Iter.Entity_It).Location,
              Iter.In_File, Iter.Start_Line, Iter.Last_Line))
      then
         Next (Iter);
      end if;
   end Setup_For_Entity;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Iter                  : out Entity_Reference_Iterator;
      Entity                : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_File               : Source_File := null;
      In_Scope              : Entity_Information := null;
      Filter                : Reference_Kind_Filter := Real_References_Filter;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False)
   is
      F           : Source_File := In_File;
      Loc         : File_Location := No_File_Location;
      Start, Last : Integer;
   begin
      if Active (Me) then
         Assert (Me, Entity /= null,
                 "No Entity specified to Find_All_References");

         Increase_Indent
           (Me, "Find_All_References to " & Debug_Name (Entity)
            & ' ' & To_String (Entity.Live_Declaration)
            & " in_file=" & Boolean'Image (In_File /= null));
      end if;

      if In_Scope /= null then
         loop
            Find_Next_Body (In_Scope, Loc, Location => Loc,
                            No_Location_If_First => True);
            if Loc = No_File_Location then
               F := In_Scope.LI_Declaration.File;
               Start := Get_Start_Of_Scope_In_File
                 (In_Scope, F, Force_Spec => True).Line;
               Last  := Get_End_Of_Scope_In_File
                 (In_Scope, F, Force_Spec => True).Line;
               exit;
            else
               F := Loc.File;
               Start := Get_Start_Of_Scope_In_File
                 (In_Scope, F, Force_Spec => False).Line;
               Last  := Get_End_Of_Scope_In_File
                 (In_Scope, F, Force_Spec => False).Line;
               exit when Last /= 0;
            end if;
         end loop;

      else
         Start := 1;
         Last  := Integer'Last;
      end if;

      Iter.In_File              := F;
      Iter.Start_Line           := Start;
      Iter.Last_Line            := Last;
      Iter.Filter               := Filter;
      Iter.Include_Overriding   := Include_Overriding;
      Iter.Include_Overridden   := Include_Overridden;
      Iter.Extra_Entities       := Entity_Information_Arrays.Empty_Instance;
      Iter.Extra_Entities_Index := Entity_Information_Arrays.First;
      Iter.Files_Analyzed       := new File_Analyzed_Set.Set;

      Setup_For_Entity (Iter, Entity, File_Has_No_LI_Report, In_Scope);
   end Find_All_References;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   function Get_Current_Progress
     (Iter : Entity_Reference_Iterator) return Integer is
   begin
      return Get_Current_Progress (Iter.Deps);
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   function Get_Total_Progress
     (Iter : Entity_Reference_Iterator) return Integer is
   begin
      return Get_Total_Progress (Iter.Deps);
   end Get_Total_Progress;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Entity_Reference_Iterator) return Boolean is
      Result : Boolean;
   begin
      Result := Iter.Decl_Returned
        and then not Iter.Include_Overriding
        and then not Iter.Include_Overridden
        and then
          (Iter.Extra_Entities = Entity_Information_Arrays.Empty_Instance
           or else Iter.Extra_Entities_Index > Last (Iter.Extra_Entities))
        and then At_End (Iter.Deps)
        and then Iter.Files_It = Entity_File_Maps.No_Element
        and then Iter.Entity_It = Entities_In_File_Sets.No_Element;

      if Result and then Active (Me) then
         Decrease_Indent (Me, "Done searching for all references");
      end if;

      return Result;
   end At_End;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Ref        : File_Location;
      In_File    : Source_File;
      Start_Line : Integer;
      Last_Line  : Integer) return Boolean is
   begin
      return In_File = null
        or else (Ref.File = In_File
                 and then Ref.Line >= Start_Line
                 and then Ref.Line <= Last_Line);
   end In_Range;

   --------------------------------
   -- Add_Overriding_Subprograms --
   --------------------------------

   procedure Add_Overriding_Subprograms
     (Iter                   : in out Entity_Reference_Iterator;
      Overriding_Subprograms : Boolean := True;
      Overridden_Subprograms : Boolean := True)
   is
      Param_Of        : constant Entity_Information :=
                          Is_Parameter_Of (Iter.Entity);
      Subprogram      : Entity_Information;
      Toplevel_Entity : Entity_Information;

      procedure Add_Prims_Of_Entity
        (Entity            : Entity_Information;
         Processing_Parent : Boolean);
      --  Add the relevant primitive operations of Entity.
      --  Processing_Parent must be True if we are analyzing parents
      --  of the initial type. In such a case, we need to do a slightly more
      --  expensive search: for children, we only look for primitive operations
      --  that are overriding (since otherwise there is no chance they do
      --  override the current entity of Iter). For parent, however, we cannot
      --  speed up the search this way.

      procedure Add_Children_Of (Entity : Entity_Information);
      --  Process recursively the children of Entity, and check whether they
      --  have some matching primitive.

      function Is_Same_Entity (Entity : Entity_Information) return Boolean;
      pragma Inline (Is_Same_Entity);
      --  Return True if Entity is in the same chain of overriding entities
      --  as Iter.Entity

      --------------------
      -- Is_Same_Entity --
      --------------------

      function Is_Same_Entity (Entity : Entity_Information) return Boolean is
         Top : Entity_Information := Entity;
      begin
         if Use_Approximate_Overriding_Algorithm then
            return Entity.Name = Subprogram.Name;
         else
            --  The test is smarter here: we make sure that it is correct even
            --  when there are two primitive operations with the same name.

            while Overriden_Entity (Top) /= null loop
               Top := Overriden_Entity (Top);
            end loop;
            return Top = Toplevel_Entity;
         end if;
      end Is_Same_Entity;

      -------------------------
      -- Add_Prims_Of_Entity --
      -------------------------

      procedure Add_Prims_Of_Entity
        (Entity            : Entity_Information;
         Processing_Parent : Boolean)
      is
         Prim      : Primitive_Operations_Iterator;
         Primitive : Entity_Information;
         SIter     : Subprogram_Iterator;
         Param     : Entity_Information;
      begin
         if Entity /= null then
            Find_All_Primitive_Operations
              (Prim, Entity,
               Only_If_Overriding => not Processing_Parent,
               Include_Inherited  => False);

            while not At_End (Prim) loop
               Primitive := Get (Prim);

               if Is_Same_Entity (Primitive) then
                  --  Primitive is a subprogram that override Subprogram.
                  --  The latter is either the entity we are searching itself,
                  --  or the subprogram for which that entity is a parameter

                  if Param_Of = null then
                     Append (Iter.Extra_Entities, Primitive);
                  else
                     --  Search the parameter that correspond to our entity
                     SIter := Get_Subprogram_Parameters (Primitive);
                     loop
                        Get (SIter, Param);
                        exit when Param = null;

                        if Param.Name = Iter.Entity.Name then
                           Append (Iter.Extra_Entities, Param);
                           exit;
                        end if;

                        Next (SIter);
                     end loop;
                  end if;
               end if;

               Next (Prim);
            end loop;

            Destroy (Prim);
         end if;
      end Add_Prims_Of_Entity;

      ---------------------
      -- Add_Children_Of --
      ---------------------

      procedure Add_Children_Of (Entity : Entity_Information) is
         Children : Children_Iterator;
         Deps     : Dependency_Iterator;
      begin
         --  Trick here: we first look for all the files that depend, even
         --  indirectly, on the file declaring Entity. That gives us all the
         --  files that might potentially contain child types of Entity. Once
         --  they have been loaded in memory, we can freeze the entities
         --  database to speed things up. This saves a lot of system calls, and
         --  speeds up the search

         if Frozen (Iter.Deps.Db) = Create_And_Update then
            --  ??? This should be done in the background, since it can freeze
            --  the GPS API for a while.
            Find_Ancestor_Dependencies
              (Iter => Deps,
               File => Get_Declaration_Of (Entity).File);
            while not At_End (Deps) loop
               Next (Deps);
            end loop;
         end if;

         Freeze (Iter.Deps.Db);

         Children := Get_Child_Types
           (Entity,
            Recursive   => True,
            Update_Xref => False);

         while not At_End (Children) loop
            if Get (Children) /= null then
               Add_Prims_Of_Entity (Get (Children), False);
            end if;
            Next (Children);
         end loop;

         Thaw (Iter.Deps.Db);

         Destroy (Children);
      exception
         when E : others =>
            Trace (Traces.Exception_Handle, E);
            Thaw (Iter.Deps.Db);
      end Add_Children_Of;

      Prim_Of         : Entity_Information;

   begin
      --  If we have a parameter of subprogram A, we look for subprograms that
      --  override A.

      if Param_Of /= null then
         Subprogram := Param_Of;
      else
         Subprogram := Iter.Entity;
      end if;

      if not Use_Approximate_Overriding_Algorithm then
         Toplevel_Entity := Subprogram;
         while Overriden_Entity (Toplevel_Entity) /= null loop
            Toplevel_Entity := Overriden_Entity (Toplevel_Entity);
         end loop;
      end if;

      Prim_Of := Is_Primitive_Operation_Of (Subprogram);

      if Prim_Of /= null then
         if Overriding_Subprograms then
            Add_Children_Of (Prim_Of);
         end if;

         if Overridden_Subprograms then
            declare
               Parents : constant Entity_Information_Array :=
                 Get_Parent_Types (Prim_Of, Recursive => True);
            begin
               for P in Parents'Range loop
                  Add_Prims_Of_Entity (Parents (P), True);
               end loop;
            end;
         end if;
      end if;
   end Add_Overriding_Subprograms;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Entity_Reference_Iterator) is
      Repeat : Boolean := True;

      procedure Safe_Update_Xref (File : Source_File);
      --  Update the file given in parameter, and reset the file iterator if
      --  needed, in order to keep consistent data structures. If the update
      --  didn't corrupt and iterator, then Iter is placed at the beginning
      --  of that file.

      procedure Move_To_Next_Entity;
      --  Move the iterator to the next entity in the entity list, moving
      --  the file cursor as well if needed.

      ----------------------
      -- Safe_Update_Xref --
      ----------------------

      procedure Safe_Update_Xref (File : Source_File) is
         Prev_Timestamp : constant Integer :=
                            Iter.Entity.File_Timestamp_In_References;
      begin
         Update_Xref (File);

         if Prev_Timestamp
           = Iter.Entity.File_Timestamp_In_References
         then
            --  If we didn't load any new file, then we can analyze
            --  that file.

            Iter.Entity_It := Element (Iter.Files_It).Refs.First;
            Iter.Files_Analyzed.Insert (Key (Iter.Files_It));
         else
            --  Otherwise, the File_It iterator is corrupted, reset it and look
            --  for new files.

            Iter.Files_It := First (Iter.Entity.References);

            --  If the first file has not already been analysed, then we need
            --  to perform the analysis.

            if not Iter.Files_Analyzed.Contains (Key (Iter.Files_It)) then
               Safe_Update_Xref (Element (Iter.Files_It).File);
            end if;
         end if;
      end Safe_Update_Xref;

      -------------------------
      -- Move_To_Next_Entity --
      -------------------------

      procedure Move_To_Next_Entity is
      begin
         if Iter.Entity_It /= Entities_In_File_Sets.No_Element then
            Iter.Entity_It := Next (Iter.Entity_It);
         end if;

         if Iter.Entity_It = Entities_In_File_Sets.No_Element
           and then Iter.Files_It /= Entity_File_Maps.No_Element
         then
            if Iter.Entity_It = Entities_In_File_Sets.No_Element
              and then Iter.Files_It /= Entity_File_Maps.No_Element
            then
               while Iter.Entity_It = Entities_In_File_Sets.No_Element
                 and then Iter.Files_It /= Entity_File_Maps.No_Element
                 and then Iter.Files_Analyzed.Contains (Key (Iter.Files_It))
               loop
                  Iter.Files_It := Next (Iter.Files_It);

                  if Iter.Files_It /= Entity_File_Maps.No_Element
                    and then not
                      Iter.Files_Analyzed.Contains (Key (Iter.Files_It))
                  then
                     Safe_Update_Xref (Element (Iter.Files_It).File);
                  end if;
               end loop;
            end if;
         end if;
      end Move_To_Next_Entity;

      Lock : Construct_Heuristics_Lock :=
        Lock_Construct_Heuristics (Iter.Entity.LI_Declaration.File.Db);
   begin
      --  We always return the declaration first

      if not Iter.Decl_Returned then
         Iter.Decl_Returned := True;
      else
         Move_To_Next_Entity;
      end if;

      while Repeat loop
         --  Return the references we already know about (from inspecting
         --  prior files or parsing them in memory).

         while Iter.Entity_It /= Entities_In_File_Sets.No_Element loop
            if Iter.Filter (Element (Iter.Entity_It).Kind)
              and then In_Range
                (Element (Iter.Entity_It).Location,
                 Iter.In_File, Iter.Start_Line, Iter.Last_Line)
            then
               --  Special case here: if the entity has no separate
               --  declaration, then the location of the body and the spec
               --  are the same. Avoid duplicates with the following test.
               --  This test is further complicated because Decl_Returned is
               --  set to true if the filters are such that the declaration
               --  will never be returned anyway. So if the user explicitly
               --  requested bodies, we show them.

               if Iter.Decl_Returned
                 and then Iter.Filter (Declaration)
                 and then Element (Iter.Entity_It).Kind =
                 Body_Entity
                 and then Element (Iter.Entity_It).Location
                 = Iter.Entity.LI_Declaration
               then
                  Next (Iter);
               end if;

               --  We found an entity to return, exit from the subprogram.

               return;
            end if;

            Move_To_Next_Entity;
         end loop;

         --  Parse the current file on the list

         if At_End (Iter.Deps) then
            if (Iter.Include_Overriding or else Iter.Include_Overridden)
              and then
                Iter.Extra_Entities = Entity_Information_Arrays.Empty_Instance
            then
               Add_Overriding_Subprograms
                 (Iter,
                  Overriding_Subprograms => Iter.Include_Overriding,
                  Overridden_Subprograms => Iter.Include_Overridden);
               Iter.Include_Overriding := False;
               Iter.Include_Overridden := False;
            end if;

            if Iter.Extra_Entities = Entity_Information_Arrays.Empty_Instance
              or else Iter.Extra_Entities_Index > Last (Iter.Extra_Entities)
            then
               return;
            end if;

            --  Increment counter first, to avoid infinite recursion
            Iter.Extra_Entities_Index := Iter.Extra_Entities_Index + 1;
            Setup_For_Entity
              (Iter,
               Iter.Extra_Entities.Table (Iter.Extra_Entities_Index - 1));

         elsif Iter.Filter (Get_Kind (Get (Iter))) then
            Repeat := False;
         end if;

         --  Move to the next file that might contain references to the entity,
         --  but only if we are looking for actual references

         Next (Iter.Deps);

         if not At_End (Iter.Deps)
           and then Get (Iter.Deps) /= null
           and then not Iter.Files_Analyzed.Contains
             (Get (Iter.Deps).Ordered_Index)
           and then Iter.Entity.References.Contains
             (Get (Iter.Deps).Ordered_Index)
         then
            Iter.Entity_It :=
              First
                (Iter.Entity.References.Element
                     (Get (Iter.Deps).Ordered_Index).Refs);
            Iter.Files_Analyzed.Insert (Get (Iter.Deps).Ordered_Index);
         end if;
      end loop;

      Lock.Unlock_Construct_Heuristics;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Entity_Reference_Iterator) return Entity_Reference is
   begin
      if not Iter.Decl_Returned then
         return (Entity => Iter.Entity,
                 Index  => (Is_Declaration => True, others => <>));

      elsif Iter.Entity_It = Entities_In_File_Sets.No_Element then
         return No_Entity_Reference;

      else
         return
           (Entity => Iter.Entity,
            Index  => (Element (Iter.Entity_It).Location,
                       Element (Iter.Entity_It).Is_Declaration));
      end if;
   end Get;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Iter : Entity_Reference_Iterator) return Entity_Information is
   begin
      if not Iter.Decl_Returned then
         return Iter.Entity;

      elsif Iter.Entity_It = Entities_In_File_Sets.No_Element then
         return null;

      else
         return Get_Entity_From_Ref (Iter.Entity, Element (Iter.Entity_It));
      end if;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Reference_Iterator) is
   begin
      Destroy (Iter.Deps);
      Free (Iter.Files_Analyzed);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Reference_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Reference_Iterator, Entity_Reference_Iterator_Access);
   begin
      if Iter /= null then
         Destroy (Iter.all);
         Unchecked_Free (Iter);
      end if;
   end Destroy;

   --------------------------------
   -- Find_Ancestor_Dependencies --
   --------------------------------

   procedure Find_Ancestor_Dependencies
     (Iter                  : out Dependency_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Include_Self          : Boolean := False;
      Single_Source_File    : Boolean := False)
   is
      Importing : Project_Iterator;
      Project   : Project_Type;
      Extended  : Project_Type;
      Count     : Natural;
      pragma Unreferenced (Count);
   begin
      if Active (Me) then
         Trace (Me, "Find_Ancestor_Dependencies: "
                & Display_Full_Name (Get_Filename (File))
                & " Self=" & Boolean'Image (Include_Self)
                & " Single=" & Boolean'Image (Single_Source_File));
      end if;

      --  Algorithm:
      --  For each project importing File's project:
      --     Parse_All_LI_Information from disk
      --  Then return all files found on File.Depended_On
      --
      --  Unfortunately, we cannot do
      --  For each project importing File's project:
      --     while parse_all_li_information:
      --         if there are new entries on File.Depended_On, return them
      --  because the files already on File.Depended_On at startup might no
      --  longer be valid, and the only way to check is to parse their ALI
      --  file.
      --
      --  This means that nothing will happen while we parse all LI information
      --  whereas it would be nice to return results as soon as possible.

      if Single_Source_File then
         Iter := (LI_Iter               => <>,  --  unused because Handler=null
                  Db                    => File.Db,
                  Include_Self          => Include_Self,
                  File_Has_No_LI_Report => File_Has_No_LI_Report,
                  Single_Source_File    => True,
                  Handler               => null,
                  Total_Progress        => 1,
                  Current_Progress      => 0,
                  Dep_Index             => Dependency_Arrays.First,
                  Source_File_Index     => 0,
                  File                  => File);

      else
         Project := File.Db.Registry.Tree.Info (Get_Filename (File)).Project;

         if Project = No_Project then
            --  Project not found ? We'll have to parse all projects, since
            --  it might be from the GNAT runtime.
            Project := File.Db.Registry.Tree.Root_Project;
            Importing := Start (Project, Recursive => True);
         else
            --  We need to look at the projects that might be using the
            --  extended project (if any). Otherwise, we will not search for
            --  ancestors dependencies for a source from an extending project
            --  elsewhere than in the extending project itself, since none of
            --  the other projects can be importing it).

            loop
               Extended := Project.Extended_Project;
               exit when Extended = No_Project;
               Project := Extended;
            end loop;

            Importing := Find_All_Projects_Importing
              (Project, Include_Self => True);
         end if;

         Iter := (LI_Iter               => <>,
                  Db                    => File.Db,
                  Include_Self          => Include_Self,
                  File_Has_No_LI_Report => File_Has_No_LI_Report,
                  Single_Source_File    => False,
                  Handler               => Get_LI_Handler_From_File
                    (Language_Handler (File.Db.Lang),
                     Get_Filename (File)),
                  Total_Progress        => 1,
                  Current_Progress      => 0,
                  Dep_Index             => Dependency_Arrays.First,
                  Source_File_Index     => 0,
                  File                  => File);

         Start (Iter.LI_Iter,
                Handler   => Language_Handler (File.Db.Lang),
                Project   => Importing);
      end if;
   end Find_Ancestor_Dependencies;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   function Get_Current_Progress (Iter : Dependency_Iterator) return Integer is
   begin
      return Iter.Current_Progress;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   function Get_Total_Progress (Iter : Dependency_Iterator) return Integer is
   begin
      return Iter.Total_Progress;
   end Get_Total_Progress;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Dependency_Iterator) return Boolean is
   begin
      if Iter.Single_Source_File then
         --  We only have one item to return, File itself
         return Iter.Dep_Index > Dependency_Arrays.First;

      else
         return Iter.Handler = null
           and then Iter.Dep_Index > Last (Iter.File.Depended_On);
      end if;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Dependency_Iterator) is
   begin
      if Iter.Handler = null then
         --  Iterate over File.Depended_On
         Iter.Dep_Index := Iter.Dep_Index + 1;

      else
         --  Keep parsing all LI information from disk.
         --  We never enter here if Iter.Single_Source_File

         Next (Iter.LI_Iter,
               Steps => 20,   --  ??? hard-coded for now
               Count => Iter.Current_Progress,
               Total => Iter.Total_Progress);

         if Iter.Current_Progress >= Iter.Total_Progress then
            --  We have finished parsing for all needed projects. We will now
            --  start returning the files stored in File.Depended_On

            Free (Iter.LI_Iter);
            Iter.Handler := null;
         end if;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Dependency_Iterator) return Source_File is
   begin
      if Iter.Handler /= null then
         --  Not available yet
         return null;

      elsif Iter.Single_Source_File then
         return Iter.File;

      else
         return Iter.File.Depended_On.Table (Iter.Dep_Index).File;
      end if;
   end Get;

   -----------------
   -- Is_Explicit --
   -----------------

   function Is_Explicit (Iter : Dependency_Iterator) return Boolean is
   begin
      if Iter.Handler /= null then
         --  Not available yet
         return False;
      else
         return Iter.File.Depended_On.Table (Iter.Dep_Index).Explicit;
      end if;
   end Is_Explicit;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Dependency_Iterator) is
   begin
      Free (Iter.LI_Iter);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Dependency_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Dependency_Iterator, Dependency_Iterator_Access);
   begin
      if Iter /= null then
         Destroy (Iter.all);
         Unchecked_Free (Iter);
      end if;
   end Destroy;

   -----------------------
   -- Find_Dependencies --
   -----------------------

   procedure Find_Dependencies
     (Iter                  : out File_Dependency_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null)
   is
   begin
      Update_Xref (File, File_Has_No_LI_Report);
      Iter := (Dep_Index => Dependency_Arrays.First,
               File      => File);
   end Find_Dependencies;

   -----------------
   -- Is_Explicit --
   -----------------

   function Is_Explicit (Iter : File_Dependency_Iterator) return Boolean is
   begin
      return Iter.File.Depends_On.Table (Iter.Dep_Index).Explicit;
   end Is_Explicit;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out File_Dependency_Iterator) is
   begin
      Iter.Dep_Index := Iter.Dep_Index + 1;
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : File_Dependency_Iterator) return Boolean is
   begin
      return Iter.File = null
        or else Iter.Dep_Index > Last (Iter.File.Depends_On);
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : File_Dependency_Iterator) return Source_File is
   begin
      return Iter.File.Depends_On.Table (Iter.Dep_Index).File;
   end Get;

   -------------------------------
   -- Get_Subprogram_Parameters --
   -------------------------------

   function Get_Subprogram_Parameters
     (Subprogram            : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null)
      return Subprogram_Iterator
   is
      Iter : Subprogram_Iterator;
   begin
      Update_Xref
        (Get_File (Get_Declaration_Of (Subprogram)), File_Has_No_LI_Report);
      Iter := (It            => First (Subprogram.References),
               Entity        => Subprogram,
               Cache_Current => null);

      if Length (Iter.Entity.References) > 0
        and then not Is_Parameter_Reference (Element (Iter.It).Kind)
      then
         Next (Iter);
      end if;

      return Iter;
   end Get_Subprogram_Parameters;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Subprogram_Iterator) is
   begin
      loop
         Iterator.It := Next (Iterator.It);
         exit when Iterator.It = Null_Entity_Reference_Cursor
           or else Is_Parameter_Reference
             (Element (Iterator.It).Kind);
      end loop;
      Iterator.Cache_Current := null;
   end Next;

   ---------
   -- Get --
   ---------

   procedure Get
     (Iterator  : in out Subprogram_Iterator;
      Parameter : out Entity_Information)
   is
      Entity : Entity_Information;
      Loc    : File_Location;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      if Iterator.Cache_Current = null
        and then Iterator.It /= Null_Entity_Reference_Cursor
      then
         Loc := Element (Iterator.It).Location;

         Find_Declaration
           (Db          => Get_Database
              (Get_File (Get_Declaration_Of (Iterator.Entity))),
            File_Name   => Get_Filename (Get_File (Loc)),
            Entity_Name => "",
            Line        => Get_Line (Loc),
            Column      => Get_Column (Loc),
            Check_Decl_Only => True,
            Entity      => Entity,
            Status      => Status);

         --  ??? If there was an error above, this will set the current to
         --  null, and thus we will no longer return the remaining parameters.
         --  In fact, we might not be able to compute them anyway...
         Iterator.Cache_Current := Entity;
      end if;

      Parameter := Iterator.Cache_Current;
   end Get;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Iterator : Subprogram_Iterator) return Parameter_Type is
   begin
      case Element (Iterator.It).Kind is
         when Subprogram_In_Parameter     => return In_Parameter;
         when Subprogram_In_Out_Parameter => return In_Out_Parameter;
         when Subprogram_Out_Parameter    => return Out_Parameter;
         when Subprogram_Access_Parameter => return Access_Parameter;
         when others =>
            Assert (Me, False, "We should have had a parameter ?");
            return In_Parameter;
      end case;
   end Get_Type;

   -----------
   -- Image --
   -----------

   function Image (Kind : Parameter_Type) return String is
   begin
      case Kind is
         when In_Parameter     => return "in";
         when Out_Parameter    => return "out";
         when In_Out_Parameter => return "in out";
         when Access_Parameter => return "access";
      end case;
   end Image;

   ----------------------------
   -- Get_Generic_Parameters --
   ----------------------------

   function Get_Generic_Parameters
     (Generic_Entity        : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null)
      return Generic_Iterator
   is
      Iter : Generic_Iterator;
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Generic_Entity)),
                   File_Has_No_LI_Report);
      Iter := (It            => First (Generic_Entity.References),
               Entity        => Generic_Entity,
               Cache_Current => null);

      if Iter.Entity.References.Length > 0
        and then Element (Iter.It).Kind /= Formal_Generic_Parameter
      then
         Next (Iter);
      end if;

      return Iter;
   end Get_Generic_Parameters;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Generic_Iterator) is
   begin
      loop
         Iterator.It := Next (Iterator.It);
         exit when Iterator.It = Null_Entity_Reference_Cursor
           or else Element (Iterator.It).Kind = Formal_Generic_Parameter;
      end loop;
      Iterator.Cache_Current := null;
   end Next;

   ---------
   -- Get --
   ---------

   procedure Get
     (Iterator  : in out Generic_Iterator;
      Parameter : out Entity_Information)
   is
      Entity : Entity_Information;
      Loc    : File_Location;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      if Iterator.Cache_Current = null
        and then Iterator.It /= Null_Entity_Reference_Cursor
      then
         Loc := Element (Iterator.It).Location;

         Find_Declaration
           (Db          => Get_Database
              (Get_File (Get_Declaration_Of (Iterator.Entity))),
            File_Name   => Get_Filename (Get_File (Loc)),
            Entity_Name => "",
            Line        => Get_Line (Loc),
            Column      => Get_Column (Loc),
            Check_Decl_Only => True,
            Entity      => Entity,
            Status      => Status);

         --  ??? If there was an error above, this will set the current to
         --  null, and thus we will no longer return the remaining parameters.
         --  In fact, we might not be able to compute them anyway...
         Iterator.Cache_Current := Entity;
      end if;

      Parameter := Iterator.Cache_Current;
   end Get;

   -------------------------
   -- Is_Instantiation_Of --
   -------------------------

   function Is_Instantiation_Of
     (Entity : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Entity)));
      return Entity.Instantiation_Of;
   end Is_Instantiation_Of;

   -----------------
   -- Renaming_Of --
   -----------------

   function Renaming_Of
     (Entity : Entity_Information) return Entity_Information is
   begin
      return Entity.Rename;
   end Renaming_Of;

   --------------------------------
   -- Get_Start_Of_Scope_In_File --
   --------------------------------

   function Get_Start_Of_Scope_In_File
     (Entity     : Entity_Information;
      File       : Source_File;
      Force_Spec : Boolean) return File_Location
   is
      Loc : File_Location := No_File_Location;
      Ref : E_Reference;
      It  : Entity_Reference_Cursor;
   begin
      if Force_Spec then
         It := First (Entity.References);

         while It /= Null_Entity_Reference_Cursor loop
            Ref := Element (It);

            --  Use this as the "declaration", since otherwise the
            --  End_Of_Spec reference is meaningless.
            if Ref.Kind = Completion_Of_Private_Or_Incomplete_Type
              and then Get_File (Ref.Location) = File
            then
               return Ref.Location;
            end if;

            It := Next (It);
         end loop;

         if Entity.LI_Declaration.File = File then
            return Entity.LI_Declaration;
         else
            return No_File_Location;
         end if;

      else
         loop
            Find_Next_Body
              (Entity               => Entity,
               Current_Location     => Loc,
               Location             => Loc,
               No_Location_If_First => True);
            exit when Loc = No_File_Location
              or else Loc.File = File;
         end loop;

         return Loc;
      end if;
   end Get_Start_Of_Scope_In_File;

   ------------------------------
   -- Get_End_Of_Scope_In_File --
   ------------------------------

   function Get_End_Of_Scope_In_File
     (Entity     : Entity_Information;
      File       : Source_File;
      Force_Spec : Boolean) return File_Location
   is
      Ref          : E_Reference;
      Max          : File_Location := No_File_Location;
      Ref_Is_Body  : Boolean := False;
      Body_Seen    : Boolean := False;
      It           : Entity_Reference_Cursor;
   begin
      if Entity.End_Of_Scope.Location.File = File
        and then ((Force_Spec and then Entity.End_Of_Scope.Kind = End_Of_Spec)
                  or else (not Force_Spec
                           and then Entity.End_Of_Scope.Kind = End_Of_Body))
      then
         return Entity.End_Of_Scope.Location;

      else
         It := First (Entity.References);

         while It /= Null_Entity_Reference_Cursor loop
            Ref := Element (It);

            if Ref.Location.File = File then
               if (Force_Spec and then Ref.Kind = End_Of_Spec)
                   or else (not Force_Spec and then Ref.Kind = End_Of_Body)
               then
                  return Ref.Location;
               end if;

               if Ref.Kind = Body_Entity then
                  Body_Seen := True;

                  if Ref.Location = Entity.LI_Declaration then
                     Ref_Is_Body := True;
                  end if;
               end if;

               --  Subprograms sometimes have no end-of-spec reference,
               --  although they should so that the parameters are correctly
               --  associated to them. We simulate these by considering the
               --  end-of-spec is the location of the last parameter
               --  declaration. This however fails with some languages (C...)
               --  where the backend might generate the parameters declaration
               --  associated with the body. We need to protect against that.

               if Force_Spec
                 and then not Body_Seen
                 and then Is_Parameter_Reference (Ref.Kind)
               then
                  Max := Ref.Location;
                  Max.Column := Max.Column + 1;
               end if;
            end if;

            It := Next (It);
         end loop;

         if Ref_Is_Body then
            return No_File_Location;
         else
            return Max;
         end if;
      end if;
   end Get_End_Of_Scope_In_File;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Scope_Tree_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scope_Tree, Scope_Tree_Access);
      T : Scope_Tree_Access;
   begin
      while Tree /= null loop
         T := Tree.Sibling;
         Free (Tree.First_Child);
         Unchecked_Free (Tree);
         Tree := T;
      end loop;
   end Free;

   -----------------
   -- Add_To_Tree --
   -----------------

   procedure Add_To_Tree
     (Tree           : in out Scope_Tree_Access;
      Entity         : Entity_Information;
      Start_Of_Scope : File_Location;
      End_Of_Scope   : File_Location)
   is
      T        : Scope_Tree_Access;
      Previous : Scope_Tree_Access;
      Sib      : Scope_Tree_Access;
   begin
      if Tree = null then
         Tree := new Scope_Tree'
           (Sibling     => null,
            First_Child => null,
            Entity      => Entity,
            Start_Line  => Start_Of_Scope.Line,
            End_Line    => End_Of_Scope.Line);

      else
         T := Tree;
         while T /= null loop
            --  Case 1: the entity is fully before the current tree entity
            if T.Start_Line > End_Of_Scope.Line then
               if Previous = null then
                  Tree  := new Scope_Tree'
                    (Sibling     => Tree,
                     First_Child => null,
                     Entity      => Entity,
                     Start_Line  => Start_Of_Scope.Line,
                     End_Line    => End_Of_Scope.Line);
               else
                  Previous.Sibling := new Scope_Tree'
                    (Sibling     => Previous.Sibling,
                     First_Child => null,
                     Entity      => Entity,
                     Start_Line  => Start_Of_Scope.Line,
                     End_Line    => End_Of_Scope.Line);
               end if;
               return;

            --  Case 2: entity full after the current tree entity
            elsif T.End_Line < Start_Of_Scope.Line then
               null;

            --  Case 3: the entity is in the scope of the current tree entity
            elsif Start_Of_Scope.Line >= T.Start_Line then
               Add_To_Tree
                 (Tree           => T.First_Child,
                  Entity         => Entity,
                  Start_Of_Scope => Start_Of_Scope,
                  End_Of_Scope   => End_Of_Scope);
               return;

            --  Case 4: the entity contains the current tree entity and any
            --  number of the sibling tree entities.
            else
               Sib := T;
               while Sib.Sibling /= null
                 and then Sib.Sibling.Start_Line <= End_Of_Scope.Line
               loop
                  Sib := Sib.Sibling;
               end loop;

               if Previous = null then
                  Tree := new Scope_Tree'
                    (Sibling     => Sib.Sibling,
                     First_Child => T,
                     Entity      => Entity,
                     Start_Line  => Start_Of_Scope.Line,
                     End_Line    => End_Of_Scope.Line);
               else
                  Previous.Sibling := new Scope_Tree'
                    (Sibling     => Sib.Sibling,
                     First_Child => T,
                     Entity      => Entity,
                     Start_Line  => Start_Of_Scope.Line,
                     End_Line    => End_Of_Scope.Line);
               end if;
               Sib.Sibling := null;
               return;
            end if;

            Previous := T;
            T := T.Sibling;
         end loop;

         Previous.Sibling := new Scope_Tree'
           (Sibling     => null,
            First_Child => null,
            Entity      => Entity,
            Start_Line  => Start_Of_Scope.Line,
            End_Line    => End_Of_Scope.Line);
      end if;
   end Add_To_Tree;

   ----------
   -- Dump --
   ----------

   procedure Dump (T : Scope_Tree_Access; Prefix : String) is
      T2 : Scope_Tree_Access := T;
   begin
      while T2 /= null loop
         Trace
           (Me, Prefix
            & Debug_Name (T2.Entity)
            & T2.Start_Line'Img & T2.End_Line'Img);
         Dump (T2.First_Child, Prefix & "  ");
         T2 := T2.Sibling;
      end loop;
   end Dump;

   -----------------------------
   -- Compute_All_Call_Graphs --
   -----------------------------

   procedure Compute_All_Call_Graphs (Db : Entities_Database) is
      use Files_HTable;
      Iter : Files_HTable.Cursor;
      File : Source_File_Item;
   begin
      Get_First (Db.Files, Iter);
      loop
         File := Get_Element (Iter);
         exit when File = null;

         Compute_Callers_And_Called (File.File);

         Get_Next (Db.Files, Iter);
      end loop;
   end Compute_All_Call_Graphs;

   --------------------
   -- Compute_Scopes --
   --------------------

   function Compute_Scopes (File : Source_File) return Lines_To_Scope is
      procedure Add_To_Tree
        (Tree : in out Scope_Tree_Access;
         EL   : Entity_Information_List_Access);
      --  Add all entities from EL to the tree

      procedure Fill_Table
        (Tree             : Scope_Tree_Access;
         Line_Start       : Natural;
         Enclosing_Entity : Entity_Information;
         Line_Last        : out Natural;
         Info             : in out Entity_Info_Array;
         Info_For_Decl    : in out Entity_Info_Array);
      --  Set the information in Info based on Tree

      -----------------
      -- Add_To_Tree --
      -----------------

      procedure Add_To_Tree
        (Tree : in out Scope_Tree_Access;
         EL   : Entity_Information_List_Access)
      is
         Entity         : Entity_Information;
         End_Of_Scope   : File_Location;
         Start_Of_Scope : File_Location;
      begin
         for E in Entity_Information_Arrays.First .. Last (EL.all) loop
            Entity := EL.Table (E);

            --  Ignore labels on loops, which create a local scope that we do
            --  not want to show in the callgraph

            if Get_Kind (Entity).Kind /= Label_On_Loop then
               --  Add the spec too, since if it is on multiple lines, we
               --  want the parameters to be associated with that subprogram,
               --  and not to the caller.

               End_Of_Scope := Get_End_Of_Scope_In_File
                 (Entity, File, Force_Spec => True);
               if End_Of_Scope /= No_File_Location then
                  Start_Of_Scope := Get_Start_Of_Scope_In_File
                    (Entity, File, Force_Spec => True);
                  if Start_Of_Scope /= No_File_Location then
                     Add_To_Tree
                       (Tree           => Tree,
                        Entity         => Entity,
                        Start_Of_Scope => Start_Of_Scope,
                        End_Of_Scope   => End_Of_Scope);
                  end if;
               end if;

               End_Of_Scope := Get_End_Of_Scope_In_File
                 (Entity, File, Force_Spec => False);

               if End_Of_Scope /= No_File_Location then
                  Start_Of_Scope := Get_Start_Of_Scope_In_File
                    (Entity, File, Force_Spec => False);
                  if Start_Of_Scope /= No_File_Location then
                     Add_To_Tree
                       (Tree           => Tree,
                        Entity         => Entity,
                        Start_Of_Scope => Start_Of_Scope,
                        End_Of_Scope   => End_Of_Scope);
                  end if;
               end if;
            end if;
         end loop;
      end Add_To_Tree;

      ----------------
      -- Fill_Table --
      ----------------

      procedure Fill_Table
        (Tree             : Scope_Tree_Access;
         Line_Start       : Natural;
         Enclosing_Entity : Entity_Information;
         Line_Last        : out Natural;
         Info             : in out Entity_Info_Array;
         Info_For_Decl    : in out Entity_Info_Array)
      is
         T    : Scope_Tree_Access := Tree;
         Line : Natural := Line_Start;
      begin
         Line_Last := Line_Start;

         while T /= null loop
            if T.Start_Line <= Info'Last then
               Info (Line .. T.Start_Line - 1) := (others => Enclosing_Entity);
               Info_For_Decl (T.Start_Line)    := Enclosing_Entity;
            end if;

            if T.First_Child = null then
               Line := T.Start_Line;

            else
               Fill_Table
                 (Tree             => T.First_Child,
                  Line_Start       => T.Start_Line,
                  Enclosing_Entity => T.Entity,
                  Line_Last        => Line,
                  Info             => Info,
                  Info_For_Decl    => Info_For_Decl);
               Line := Line + 1;
            end if;

            if Is_Container (Get_Kind (T.Entity).Kind) then
               if T.End_Line <= Info'Last then
                  Info (Line .. T.End_Line) := (others => T.Entity);
               end if;

               Line := T.End_Line + 1;
            else
               if T.End_Line - 1 <= Info'Last then
                  if T.End_Line = T.Start_Line then
                     Info (Line) := Enclosing_Entity;
                  else
                     Info (Line .. T.End_Line - 1) := (others => T.Entity);
                  end if;
               end if;

               Line := T.End_Line;
            end if;

            Line_Last := T.End_Line;
            T := T.Sibling;
         end loop;
      end Fill_Table;

      Tree     : Scope_Tree_Access;
      Line_Max : Integer := 0;
      T        : Scope_Tree_Access;
      Iter     : Entities_Hash.Cursor;
      UEI      : Entity_Informations;

   begin
      if File = null then
         return Lines_To_Scope'
           (Line_Max => 0, Line_Info => <>, Info_For_Decl => <>);
      end if;

      Update_Xref (File);

      Get_First (File.Entities, Iter);

      loop
         UEI := Get_Element (Iter);
         exit when UEI = null;
         Add_To_Tree (Tree, UEI.List);
         Get_Next (File.Entities, Iter);
      end loop;

      Get_First (File.All_Entities, Iter);

      loop
         UEI := Get_Element (Iter);
         exit when UEI = null;
         Add_To_Tree (Tree, UEI.List);
         Get_Next (File.All_Entities, Iter);
      end loop;

      if Active (Callers_Me) then
         Dump (Tree, "");
      end if;

      if Tree /= null then
         T := Tree;
         while T.Sibling /= null loop
            T := T.Sibling;
         end loop;

         if T.Sibling /= null then
            Line_Max := T.Sibling.End_Line;
         else
            Line_Max := T.End_Line;
         end if;

         declare
            Result : Lines_To_Scope (Line_Max);
            Last          : Integer;
         begin
            Fill_Table
              (Tree             => Tree,
               Line_Start       => 1,
               Enclosing_Entity => null,
               Line_Last        => Last,
               Info             => Result.Line_Info,
               Info_For_Decl    => Result.Info_For_Decl);

            Free (Tree);

            if Active (Me) then
               Trace (Me, "Compute_Scopes for "
                      & Display_Full_Name (Get_Filename (File)));
               for L in Result.Line_Info'Range loop
                  if Result.Line_Info (L) /= null then
                     if Result.Info_For_Decl (L) /= null then
                        Trace (Callers_Me, "Line" & L'Img & " "
                               & Debug_Name (Result.Line_Info (L))
                               & " Decl="
                               & Debug_Name (Result.Info_For_Decl (L)));
                     else
                        Trace (Callers_Me, "Line" & L'Img & " "
                               & Debug_Name (Result.Line_Info (L))
                               & " Decl=<null>");
                     end if;
                  end if;
               end loop;
            end if;

            return Result;
         end;
      end if;

      return Lines_To_Scope'
        (Line_Max => 0, Line_Info => <>, Info_For_Decl => <>);
   end Compute_Scopes;

   ---------------
   -- Get_Scope --
   ---------------

   function Get_Scope
     (Scopes : Lines_To_Scope; Line : Integer) return Entity_Information is
   begin
      if Line in Scopes.Line_Info'Range then
         return Scopes.Line_Info (Line);
      else
         return null;
      end if;
   end Get_Scope;

   --------------------------------
   -- Compute_Callers_And_Called --
   --------------------------------

   procedure Compute_Callers_And_Called (File : Source_File) is
      procedure Process_All_Entities_Refs
        (Info           : Entity_Info_Array;
         Info_For_Decl  : Entity_Info_Array;
         For_Entities   : Entity_Information_List_Access;
         Add_Deps       : Boolean);
      --  We now have in Lines the inner-most entity at that scope, used
      --  for computing the parent for specific references.
      --  Traverse all the entities in For_Entities, for all
      --  references in File, and set their caller.
      --  If Add_Deps is True, then a dependency is added between File and
      --  the declaration file of the entities.

      procedure Process_All_Refs
        (Info          : Entity_Info_Array;
         Entity        : Entity_Information;
         Info_For_Decl : Entity_Info_Array);
      --  Process a list of references as in Process_All_Entities_Refs

      -------------------------------
      -- Process_All_Entities_Refs --
      -------------------------------

      procedure Process_All_Entities_Refs
        (Info           : Entity_Info_Array;
         Info_For_Decl  : Entity_Info_Array;
         For_Entities   : Entity_Information_List_Access;
         Add_Deps       : Boolean)
      is
         Caller : Entity_Information;
      begin
         for E in Entity_Information_Arrays.First ..
           Last (For_Entities.all)
         loop
            if Add_Deps then
               Add_Depends_On
                 (For_Entities.Table (E).LI_Declaration.File, File);
            end if;

            if For_Entities.Table (E).LI_Declaration.File = File
              and then For_Entities.Table (E).LI_Declaration.Line
                in Info'Range
            then
               Caller := Info (For_Entities.Table (E).LI_Declaration.Line);

               if Caller = For_Entities.Table (E) then
                  Caller := Info_For_Decl
                    (For_Entities.Table (E).LI_Declaration.Line);
               end if;

               --  An entity should not be considered its own caller. Here we
               --  reset the caller to make sure this never happens.

               if Caller = For_Entities.Table (E) then
                  Caller := null;
               end if;

               Set_Caller_At_Declaration (For_Entities.Table (E), Caller);

               if Caller /= null then
                  Add_Called (Caller, For_Entities.Table (E));
               end if;
            end if;

            Process_All_Refs (Info, For_Entities.Table (E), Info_For_Decl);
         end loop;
      end Process_All_Entities_Refs;

      ----------------------
      -- Process_All_Refs --
      ----------------------

      procedure Process_All_Refs
        (Info          : Entity_Info_Array;
         Entity        : Entity_Information;
         Info_For_Decl : Entity_Info_Array)
      is
         Refs   : Entity_Reference_List renames Entity.References;
         Caller : Entity_Information;
         It     : Entity_Reference_Cursor := First (Refs);
         E_Ref  : E_Reference;
      begin
         while It /= Null_Entity_Reference_Cursor loop
            E_Ref := Element (It);

            if E_Ref.Location.File = File
              and then Is_Real_Reference (E_Ref.Kind)
              and then E_Ref.Kind /= Label
              and then E_Ref.Location.Line <= Info'Last
            then
               declare
                  Line : constant Integer := E_Ref.Location.Line;
               begin
                  if Line >= Info'First then
                     Caller := Info (Line);
                  else
                     Caller := null;
                     Trace (Me, "Line out of range: " & Integer'Image (Line));
                  end if;
               end;

               if Caller = Entity then
                  Caller := Info_For_Decl (E_Ref.Location.Line);
               end if;

               if Caller = Entity then
                  Caller := null;
               end if;

               E_Ref.Caller := Caller;
               Replace (Refs, Index (It), E_Ref);

               if Caller /= null then
                  Trace (Ref_Me, "Ref " & Debug_Name (Caller)
                         & " since caller for a location");
                  Ref (Caller);

                  Add_Called (Caller, Entity);
               end if;
            end if;

            It := Next (It);
         end loop;
      end Process_All_Refs;

      use type LI_File_List;
   begin
      if File = null or else File.Scope_Tree_Computed then
         return;
      end if;

      if File.LI_Files = Null_LI_File_List then
         --  If there is no LI known, we should not compute the call tree:
         --  otherwise, Scope_Tree_Computed is set to True. When the ALI file
         --  is finally parsed later on (perhaps it now exists on the disk)
         --  it will not reset the source file (not associated with it yet)
         --  and as a result it will not reset Scope_Tree_Computed. Thus it
         --  will not compute the actual call tree.

         Trace (Me, "Compute_Callers_And_Called: nothing to do, no LI for "
                & File.Name.Display_Full_Name);
         return;
      end if;

      declare
         Info : constant Lines_To_Scope := Compute_Scopes (File);
         Iter : Entities_Hash.Cursor;
         UEI  : Entity_Informations;

      begin
         Get_First (File.Entities, Iter);

         loop
            UEI := Get_Element (Iter);
            exit when UEI = null;
            Process_All_Entities_Refs
              (Info.Line_Info, Info.Info_For_Decl,
               UEI.List, Add_Deps => False);
            Get_Next (File.Entities, Iter);
         end loop;

         Get_First (File.All_Entities, Iter);

         loop
            UEI := Get_Element (Iter);
            exit when UEI = null;
            Process_All_Entities_Refs
              (Info.Line_Info, Info.Info_For_Decl,
               UEI.List, Add_Deps => True);
            Get_Next (File.All_Entities, Iter);
         end loop;
      end;

      File.Scope_Tree_Computed := True;
   end Compute_Callers_And_Called;

   -------------------------
   -- Get_Entity_From_Ref --
   -------------------------

   function Get_Entity_From_Ref
     (Entity : Entity_Information;
      Ref    : E_Reference) return Entity_Information
   is
      Result : Entity_Information;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      if Is_Real_Reference (Ref.Kind) then
         return Entity;

      else
         Find_Declaration
           (Db        => Entity.LI_Declaration.File.Db,
            File_Name => Get_Filename (Get_File (Ref.Location)),
            Line      => Get_Line (Ref.Location),
            Column    => Get_Column (Ref.Location),
            Entity    => Result,
            Status    => Status);

         if Status = Success or else Status = Fuzzy_Match then
            return Result;
         else
            return null;
         end if;
      end if;
   end Get_Entity_From_Ref;

   ------------------------
   -- Get_Parent_Package --
   ------------------------

   function Get_Parent_Package
     (Pkg : Entity_Information; Force_Load_Xrefs : Boolean := True)
      return Entity_Information
   is
      It : Entity_Reference_Cursor;
   begin
      if Force_Load_Xrefs then
         Update_Xref (Pkg.LI_Declaration.File);
      end if;

      It := First (Pkg.References);

      while It /= Null_Entity_Reference_Cursor loop
         if Element (It).Kind = Parent_Package then
            return Get_Entity_From_Ref (Pkg, Element (It));
         end if;

         It := Next (It);
      end loop;
      return null;
   end Get_Parent_Package;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name (Entity : Entity_Information) return String is
      Max_Level     : constant := 128;
      --  Limit number of names, to avoid e.g. infinite loop in case of
      --  incorrect/confusing xref info.

      E             : Entity_Information := Entity;
      Last_Not_Null : Entity_Information := Entity;
      Result        : Unbounded_String;
      Separator     : constant String :=
                        Scope_Separator
                          (Entity.LI_Declaration.File.Db.Lang.
                            Get_Language_From_File
                              (Entity.LI_Declaration.File.Name));

   begin
      --  For efficiency, build the name in reverse order, to avoid freeing
      --  and allocating many strings, and revert it before exit.

      for J in 1 .. Max_Level loop
         exit when E = null;

         Append (Result, Get (E.Name).all);
         Update_Xref (E.LI_Declaration.File);
         Compute_Callers_And_Called (E.LI_Declaration.File);
         Last_Not_Null := E;
         E := E.Caller_At_Declaration;

         if E /= null then
            Append (Result, Separator);
         end if;
      end loop;

      --  If there is no more caller in the tree, we still need to
      --  look for possible parent packages.

      E := Last_Not_Null;
      for J in 1 .. Max_Level loop
         E := Get_Parent_Package (E);
         exit when E = null;
         Append (Result, Separator);
         Append (Result, Get (E.Name).all);
      end loop;

      return Revert (To_String (Result), Separator);
   end Get_Full_Name;

   ----------------
   -- Get_Caller --
   ----------------

   function Get_Caller (Ref : Entity_Reference) return Entity_Information is
   begin
      if Ref.Entity = null then
         return null;

      elsif Ref.Index.Is_Declaration then
         Compute_Callers_And_Called (Ref.Entity.LI_Declaration.File);
         return Ref.Entity.Caller_At_Declaration;

      else
         Compute_Callers_And_Called
           (Element (Ref.Entity.References, Ref.Index).Location.File);
         return Element (Ref.Entity.References, Ref.Index).Caller;
      end if;
   end Get_Caller;

   -------------------------------
   -- For_Each_Dispatching_Call --
   -------------------------------

   procedure For_Each_Dispatching_Call
     (Entity    : Entity_Information;
      Ref       : Entity_Reference;
      On_Callee : access function
        (Callee, Primitive_Of : Entity_Information) return Boolean;
      Filter    : Reference_Kind_Filter := Entity_Has_Declaration;
      Policy    : Dispatching_Menu_Policy)
   is
      Db    : Entities_Database;
      Iter  : Entity_Reference_Iterator;
      E, E2 : Entity_Information;
   begin
      if Entity /= null
        and then Policy /= Never
        and then Get_Kind (Ref) = Dispatching_Call
      then
         if Policy = From_Memory then
            Db := Get_Database (Get_File (Get_Declaration_Of (Entity)));
            Freeze (Db);
         end if;

         Find_All_References
           (Iter                  => Iter,
            Entity                => Entity,
            File_Has_No_LI_Report => null,
            In_File               => null,
            Filter                => Filter,
            Include_Overriding    => True,
            Include_Overridden    => False);

         while not At_End (Iter) loop
            E := Get_Entity (Iter);
            if E /= null then
               E2 := Is_Primitive_Operation_Of (E);
               if E2 /= null then
                  exit when not On_Callee (Callee => E, Primitive_Of => E2);
               end if;
            end if;

            Next (Iter);
         end loop;

         Destroy (Iter);

         if Policy = From_Memory then
            Thaw (Db);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, "Unexpected exception: "
                & Exception_Information (E));
         if Policy = From_Memory then
            Thaw (Db);
         end if;
   end For_Each_Dispatching_Call;

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   function Get_All_Called_Entities
     (Entity : Entity_Information) return Calls_Iterator
   is
      Loc     : File_Location := No_File_Location;
      Old_Loc : File_Location;
   begin
      if not Is_Container (Entity.Kind.Kind) then
         return (Entity => Entity,
                 Index  => Entity_Information_Arrays.Index_Type'Last);
      end if;

      loop
         Old_Loc := Loc;
         Find_Next_Body
           (Entity, Loc, Location => Loc, No_Location_If_First => True);

         if Loc = No_File_Location then
            Compute_Callers_And_Called (Entity.LI_Declaration.File);
            exit;
         else
            Compute_Callers_And_Called (Loc.File);
         end if;

         exit when Old_Loc = Loc;
         --  Old_Loc should not be equal to Loc but it appears to be the case
         --  with some C++ entities (constructor that has the same name as
         --  the class in which it is defined.
      end loop;

      return (Entity => Entity,
              Index  => Entity_Information_Arrays.First);
   end Get_All_Called_Entities;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Calls_Iterator) is
      pragma Unreferenced (Iter);
   begin
      null;
   end Destroy;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Calls_Iterator) return Boolean is
   begin
      return Iter.Index >
        Entity_Information_Arrays.Last (Iter.Entity.Called_Entities);
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : Calls_Iterator) return Entity_Information is
   begin
      return Iter.Entity.Called_Entities.Table (Iter.Index);
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Calls_Iterator) is
   begin
      Iter.Index := Iter.Index + 1;
   end Next;

   -------------------------------
   -- Find_All_Entities_In_File --
   -------------------------------

   procedure Find_All_Entities_In_File
     (Iter                  : out Entity_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Name                  : String := "")
   is
      S_Name : Symbol;
      UEI : Entity_Informations;
   begin
      Update_Xref (File, File_Has_No_LI_Report);

      Iter.Case_Sensitive := not Case_Insensitive_Identifiers (File.Handler);

      if Name = "" then
         Get_First (File.Entities, Iter.SIter);
         UEI := Get_Element (Iter.SIter);

         Get_First (File.All_Entities, Iter.Iter);
         Iter.Name := No_Symbol;

      else
         S_Name := File.Db.Symbols.Find (Name);
         UEI := Get (File.Entities,
                     (Str => S_Name,
                      Case_Sensitive => Iter.Case_Sensitive));
         Iter.Name := S_Name;
      end if;

      if UEI /= null then
         Iter.EL := UEI.List;
      end if;

      Iter.File := File;
      Iter.Index_In_EL := Entity_Information_Arrays.First;
      Iter.Processing_Entities := True;

      if Iter.EL = null then
         Next (Iter);
      end if;
   end Find_All_Entities_In_File;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Entity_Iterator) return Boolean is
   begin
      return not Iter.Processing_Entities
        and then (Iter.EL = null
                    or else Iter.Index_In_EL > Last (Iter.EL.all));
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : Entity_Iterator) return Entity_Information is
   begin
      if Iter.EL /= null then
         return Iter.EL.Table (Iter.Index_In_EL);
      else
         return null;
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Entity_Iterator) is
      EIS : Entity_Informations;
      UEI : Entity_Informations;
   begin
      Iter.Index_In_EL := Iter.Index_In_EL + 1;

      if Iter.EL = null
        or else Iter.Index_In_EL > Last (Iter.EL.all)
      then
         if Iter.Processing_Entities then
            Iter.Index_In_EL := Entity_Information_Arrays.First;

            if Iter.Name = No_Symbol then
               Get_Next (Iter.File.Entities, Iter.SIter);
               EIS := Get_Element (Iter.SIter);
               if EIS /= null then
                  Iter.EL := EIS.List;
               else
                  Iter.EL := null;
               end if;

               if Iter.EL = null then
                  Iter.Processing_Entities := False;
                  UEI := Get_Element (Iter.Iter);
                  if UEI /= null then
                     Iter.EL := UEI.List;
                  end if;
               end if;

            else
               Iter.Processing_Entities := False;
               UEI := Get (Iter.File.All_Entities,
                           (Str => Iter.Name,
                            Case_Sensitive => Iter.Case_Sensitive));
               if UEI /= null then
                  Iter.EL := UEI.List;
               else
                  Iter.EL := null;
               end if;
            end if;

         elsif Iter.Name = No_Symbol then
            Iter.Index_In_EL := Entity_Information_Arrays.First;
            Get_Next (Iter.File.All_Entities, Iter.Iter);
            UEI := Get_Element (Iter.Iter);

            if UEI = null then
               Iter.EL := null;
            else
               Iter.EL := UEI.List;
            end if;

         else
            Iter.EL := null;
         end if;
      end if;
   end Next;

   -----------------------
   -- Get_Variable_Type --
   -----------------------

   function Get_Variable_Type
     (Entity : Entity_Information) return Entity_Information is
   begin
      if Entity.Parent_Types = Null_Entity_Information_List then
         return null;
      else
         --  For a variable, the first parent is the variable's type
         return Entity.Parent_Types.Table (Entity_Information_Arrays.First);
      end if;
   end Get_Variable_Type;

   -------------------------
   -- Array_Contents_Type --
   -------------------------

   function Array_Contents_Type
     (Entity : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Entity)));
      if Is_Subprogram (Entity) then
         return null;
      else
         return Entity.Pointed_Type;
      end if;
   end Array_Contents_Type;

   ------------------
   -- Pointed_Type --
   ------------------

   function Pointed_Type
     (Entity : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Entity)));
      if Is_Subprogram (Entity) then
         return null;
      else
         return Entity.Pointed_Type;
      end if;
   end Pointed_Type;

   ----------------------
   -- Overriden_Entity --
   ----------------------

   function Overriden_Entity
     (Entity : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Entity)));
      if Is_Subprogram (Entity) then
         return Entity.Pointed_Type;
      else
         return null;
      end if;
   end Overriden_Entity;

   -------------------
   -- Returned_Type --
   -------------------

   function Returned_Type
     (Entity : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Entity)));
      return Entity.Returned_Type;
   end Returned_Type;

   ----------------
   -- Is_Subtype --
   ----------------

   function Is_Subtype (Entity : Entity_Information) return Boolean is
   begin
      if not Entity.Kind.Is_Type then
         return False;
      end if;

      --  ??? Unimplemented
      return False;
   end Is_Subtype;

   -----------------------------------
   -- Find_All_Primitive_Operations --
   -----------------------------------

   procedure Find_All_Primitive_Operations
     (Iter               : out Primitive_Operations_Iterator;
      Entity             : Entity_Information;
      Include_Inherited  : Boolean;
      Only_If_Overriding : Boolean := False) is
   begin
      --  If we only want the overriding primitives, the info is given directly
      --  from the ALI files, so we do not need to analyze the parent types as
      --  well.
      if Include_Inherited and then not Only_If_Overriding then
         Iter.Parents := new Entity_Information_Array'
           (Entity & Get_Parent_Types (Entity, Recursive => True));
      else
         Iter.Parents := new Entity_Information_Array'(1 => Entity);
      end if;

      Iter.Current_Parent    := Iter.Parents'First;
      Iter.Current_Primitive := Entity_Information_Arrays.First;
      Iter.Overriding_Only   := Only_If_Overriding;

      --  No primitive op for current parent ? Move to next parent

      if Iter.Current_Primitive >
        Last (Iter.Parents (Iter.Current_Parent).Primitive_Subprograms)
      then
         Next (Iter);
      end if;

      --  Current primitive is not good ? Move to next

      if not At_End (Iter)
        and then Iter.Overriding_Only
        and then Overriden_Entity (Get (Iter)) = null
      then
         Next (Iter);
      end if;
   end Find_All_Primitive_Operations;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Primitive_Operations_Iterator) return Boolean is
   begin
      return Iter.Current_Parent > Iter.Parents'Last;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Primitive_Operations_Iterator) is
   begin
      if Iter.Current_Parent <= Iter.Parents'Last then
         Iter.Current_Primitive := Iter.Current_Primitive + 1;

         loop
            --  Move to the next matching primitive op for current parent
            declare
               P : Entity_Information renames
                 Iter.Parents (Iter.Current_Parent);
            begin
               if Iter.Overriding_Only then
                  while Iter.Current_Primitive <=
                    Last (P.Primitive_Subprograms)
                    and then Overriden_Entity
                      (P.Primitive_Subprograms.Table (Iter.Current_Primitive))
                    = null
                  loop
                     Iter.Current_Primitive := Iter.Current_Primitive + 1;
                  end loop;
               end if;

               --  Stop searching if we have found one
               exit when Iter.Current_Primitive <=
                 Last (P.Primitive_Subprograms);
            end;

            --  No more primitive op for the current parent, move to next

            Iter.Current_Parent := Iter.Current_Parent + 1;
            exit when Iter.Current_Parent > Iter.Parents'Last;

            Iter.Current_Primitive := Entity_Information_Arrays.First;
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get
     (Iter : Primitive_Operations_Iterator) return Entity_Information is
   begin
      return Iter.Parents (Iter.Current_Parent).Primitive_Subprograms.Table
        (Iter.Current_Primitive);
   end Get;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Primitive_Operations_Iterator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Information_Array, Entity_Information_Array_Access);
   begin
      Unchecked_Free (Iter.Parents);
   end Destroy;

   -----------------------
   -- Array_Index_types --
   -----------------------

   function Array_Index_Types
     (Entity : Entity_Information) return Entity_Information_Array
   is
      Result : Entity_Information_Array
        (1 .. Integer (Last (Entity.Called_Entities)));
   begin
      for P in
        Entity_Information_Arrays.First .. Last (Entity.Called_Entities)
      loop
         Result (Integer (P)) := Entity.Called_Entities.Table (P);
      end loop;

      return Result;
   end Array_Index_Types;

   -------------------------------
   -- Get_Parent_Or_Child_Types --
   -------------------------------

   function Get_Parent_Or_Child_Types
     (Entity    : Entity_Information;
      Parents   : Boolean;
      Recursive : Boolean) return Entity_Information_Array
   is
      Result  : Entity_Information_List;
      Members : Entity_Information_List;

      procedure Process_Recursive (Entity  : Entity_Information);
      --  Add Entity and possibly all its parents/children to Result

      -----------------------
      -- Process_Recursive --
      -----------------------

      procedure Process_Recursive (Entity  : Entity_Information) is
         Members : Entity_Information_List;
      begin
         Append (Result, Entity);

         if Recursive then
            --  ??? Not efficient when using interfaces and multiple entities
            --  are defined in the same file.
            Update_Xref (Get_File (Get_Declaration_Of (Entity)));

            if Parents then
               Members := Entity.Parent_Types;
            else
               Members := Entity.Child_Types;
            end if;

            for P in
              Entity_Information_Arrays.First .. Last (Members)
            loop
               Process_Recursive (Members.Table (P));
            end loop;
         end if;
      end Process_Recursive;

   begin
      if Parents then
         Members := Entity.Parent_Types;
      else
         Members := Entity.Child_Types;
      end if;

      for P in
        Entity_Information_Arrays.First .. Last (Members)
      loop
         Process_Recursive (Members.Table (P));
      end loop;

      declare
         R : Entity_Information_Array
           (Integer (Entity_Information_Arrays.First) ..
              Integer (Last (Result)));
      begin
         for E in R'Range loop
            R (E) := Result.Table (Entity_Information_Arrays.Index_Type (E));
         end loop;

         Free (Result);
         return R;
      end;
   end Get_Parent_Or_Child_Types;

   ----------------------
   -- Get_Parent_Types --
   ----------------------

   function Get_Parent_Types
     (Entity    : Entity_Information;
      Recursive : Boolean := False) return Entity_Information_Array is
   begin
      return Get_Parent_Or_Child_Types (Entity, True, Recursive);
   end Get_Parent_Types;

   ---------------------
   -- Get_Child_Types --
   ---------------------

   function Get_Child_Types
     (Entity      : Entity_Information;
      Recursive   : Boolean := False;
      Update_Xref : Boolean := True) return Children_Iterator
   is
      Deps : Dependency_Iterator;
      Iter : Children_Iterator;
      Update : constant Boolean := Update_Xref
        and then Frozen (Get_File (Get_Declaration_Of (Entity)).Db) =
           Create_And_Update;
   begin
      --  Algorithm is the following:
      --  - Find all child types for Entity:
      --    * Find all files that depend on the file that declares the entity
      --    * Parse those files. This will automatically
      --      update Entity.Child_Types
      --  - Recurse if necessary

      if Update then
         Find_Ancestor_Dependencies
           (Deps,
            File         => Get_File (Get_Declaration_Of (Entity)),
            Include_Self => True);
      end if;

      Iter := Children_Iterator'
        (Entity      => Entity,
         Recursive   => Recursive,
         Update_Xref => Update,
         Deps        => Deps,
         Results     => Null_Entity_Information_List,
         Current     => Entity_Information_Arrays.First);

      if not Update then
         Add_Child_Types (Iter, Iter.Entity);
      end if;

      return Iter;
   end Get_Child_Types;

   ---------------------
   -- Add_Child_Types --
   ---------------------

   procedure Add_Child_Types
     (Iter   : in out Children_Iterator;
      Entity : Entity_Information)
   is
      Members : constant Entity_Information_List := Entity.Child_Types;
   begin
      for P in Entity_Information_Arrays.First .. Last (Members) loop
         Append (Iter.Results, Members.Table (P));
      end loop;
   end Add_Child_Types;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Children_Iterator) return Boolean is
   begin
      return (not Iter.Update_Xref or else At_End (Iter.Deps))
        and then Iter.Current > Last (Iter.Results);
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : Children_Iterator) return Entity_Information is
      Entity : Entity_Information;
   begin
      if not Iter.Update_Xref or else At_End (Iter.Deps) then
         Entity := Iter.Results.Table (Iter.Current);
         return Entity;
      else
         return null;
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Children_Iterator) is
   begin
      if Iter.Update_Xref and then not At_End (Iter.Deps) then
         Next (Iter.Deps);
         if At_End (Iter.Deps) then
            --  When we just finished processing an entity, add all its
            --  children to the results.
            Add_Child_Types (Iter, Iter.Entity);
         else
            --  We haven't finished looking for all files that potentially
            --  contain extensions of the entity, so we'll keep looking.
            null;
         end if;

      else
         --  We are not searching for possible extensions, so return the
         --  current results.

         if Iter.Recursive
           and then Iter.Current <= Last (Iter.Results)
         then
            --  Start searching for children of the child type. These will
            --  be ultimately appended to Iter.Results
            Iter.Entity := Iter.Results.Table (Iter.Current);

            if Iter.Update_Xref then
               Find_Ancestor_Dependencies
                 (Iter.Deps,
                  File         => Get_File (Get_Declaration_Of (Iter.Entity)),
                  Include_Self => True);

            else
               Add_Child_Types (Iter, Iter.Entity);
            end if;
         end if;

         Iter.Current := Iter.Current + 1;
      end if;
   end Next;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Children_Iterator) is
   begin
      Destroy (Iter.Deps);
      Free (Iter.Results);
   end Destroy;

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   function Is_Parameter_Of
     (Entity : Entity_Information) return Entity_Information
   is
      Caller     : constant Entity_Information :=
                     Get_Caller (Declaration_As_Reference (Entity));
      Param_Iter : Subprogram_Iterator;
      Param      : Entity_Information;
   begin
      if Caller /= null then
         Param_Iter := Get_Subprogram_Parameters (Caller);

         loop
            Get (Param_Iter, Param);
            exit when Param = null;

            if Param = Entity then
               return Caller;
            end if;

            Next (Param_Iter);
         end loop;
      end if;

      return null;
   end Is_Parameter_Of;

   -----------
   -- Start --
   -----------

   procedure Start
     (Iter      : out Recursive_LI_Information_Iterator;
      Handler   : access Language_Handlers.Language_Handler_Record'Class;
      Project   : GNATCOLL.Projects.Project_Iterator;
      Filter    : Language_Filter := null) is
   begin
      Iter.Project := Project;
      Iter.Handler      := Language_Handler (Handler);
      Iter.Lang_Count   := LI_Handlers_Count (Iter.Handler);
      Iter.Filter       := Filter;
      Iter.Current_Lang := 1;
      Iter.Count        := 0;
      Iter.Total        := 0;
      Iter.LI_Count     := 0;
      Iter.LI_Total     := 0;
      Iter.Start        := Ada.Calendar.Clock;
   end Start;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Iter  : in out Recursive_LI_Information_Iterator;
      Steps : Natural := Natural'Last;
      Count : out Natural;
      Total : out Natural)
   is
      function Process return Boolean;
      --  parse the next LI files, and return True if we should exit

      function Process return Boolean is
         Tmp : Natural;
      begin
         if Iter.LI /= null then
            Next (Iter.LI.all, Steps => Steps,
                      Count => Iter.LI_Count,
                      Total => Iter.LI_Total);
            if Iter.LI_Count < Iter.LI_Total then
               --  Will keep processing next time
               Count := Iter.Count + Iter.LI_Count;
               Total := Iter.Total + Iter.LI_Total;
               return True;
            end if;

            --  We know the total for LI will not change anymore
            --  In case the iter did not correctly predict the total,
            --  we now assume that all files have been parsed

            Tmp := Natural'Max (Iter.LI_Count, Iter.LI_Total);
            Iter.Count := Iter.Count + Tmp;
            Iter.LI_Count := 0;
            Iter.Total := Iter.Total + Tmp;
            Iter.LI_Total := 0;

            Free (Iter.LI.all);
            Unchecked_Free (Iter.LI);

            --  We finished iterating for this project and language.
            --  Move to next language

            Iter.Current_Lang := Iter.Current_Lang + 1;
         end if;

         return False;
      end Process;

      P  : Project_Type := Current (Iter.Project);
      LI : LI_Handler;
   begin
      --  If we are already processing a list of files, continue

      if Process then
         return;
      end if;

      --  Move to next project or language

      while P /= No_Project loop
         while Iter.Current_Lang <= Iter.Lang_Count loop
            declare
               Lang : constant String :=
                        Get_Nth_Language (Iter.Handler, Iter.Current_Lang);
            begin
               --  Nothing to do if the language is not used for the project
               --  or we don't want to include this language.

               if (Has_Language (P, Lang)
                     --  Given that C and C++ share the LI handler (see
                     --  CPP_Module.Register_Module), we must take care
                     --  of C++ specifically.

                     or else
                       (Lang = "c"
                          and then Has_Language (P, "C++")))
                 and then (Iter.Filter = null or else Iter.Filter (Lang))
               then
                  LI := Get_Nth_Handler (Iter.Handler, Iter.Current_Lang);
                  if LI /= null then
                     Iter.LI := new LI_Information_Iterator'Class'
                       (Parse_All_LI_Information (LI, P));

                     if Process then
                        return;
                     end if;
                  end if;
               else
                  Iter.Current_Lang := Iter.Current_Lang + 1;
               end if;
            end;
         end loop;

         --  We finished all languages for this project, move to next project

         Iter.Current_Lang := 1;
         Next (Iter.Project);
         P := Current (Iter.Project);

         if P /= No_Project then
            Trace (Me, "Parse all LI information: switching to project "
                   & P.Name);
         end if;
      end loop;

      --  Nothing else to process
      Trace (Me, "Parsed" & Iter.Count'Img & " LI files in "
             & Duration'Image (Ada.Calendar.Clock - Iter.Start) & " seconds");

      Count := Iter.Count + Iter.LI_Count;
      Total := Iter.Total + Iter.LI_Total;
   end Next;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Iter : in out Recursive_LI_Information_Iterator) is
   begin
      if Iter.LI /= null then
         Free (Iter.LI.all);
         Unchecked_Free (Iter.LI);
      end if;
   end Free;

end Entities.Queries;
