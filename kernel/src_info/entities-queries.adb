-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
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

with VFS;                     use VFS;
with Traces;                  use Traces;
with Projects;                use Projects;
with Projects.Registry;       use Projects.Registry;
with Language_Handlers.Glide; use Language_Handlers.Glide;

with Glib.Unicode;            use Glib.Unicode;

with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;             use GNAT.OS_Lib;

package body Entities.Queries is
   Me : constant Debug_Handle := Create ("Entities.Queries", Off);

   Find_Deps_File_Granularity : constant Debug_Handle :=
     Create ("Entities.Queries_File_Granularity", On);
   --  Whether the search for dependencies parses all the files from a project
   --  at one time, or one file at a time.
   --  The latter provides more accuracy for the progress bar, but takes
   --  slightly more time.

   Num_Columns_Per_Line : constant := 250;
   --  The number of columns in each line, when computing the proximity of a
   --  match. This is an approximate number, for efficiency. Big values mean
   --  that we give advantage to matches on the same line rather than on the
   --  same column.

   use Entities_Tries;
   use Entity_Information_Arrays;
   use Entity_Reference_Arrays;
   use Source_File_Arrays;
   use Dependency_Arrays;

   procedure Find
     (EL              : Entity_Information_List_Access;
      File            : Source_File;
      Line            : Integer;
      Column          : Integer;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information);
   --  Check in EL the entities which has a reference as close as possible
   --  to (Line, Column). Distance is the initial closest distance known, and
   --  is changed to reflect the result of the find. It is set to 0 if an
   --  exact match was found.
   --  If Check_Decl_Only is True, then it only tries to match a declaration,
   --  and doesn't check references to the entity.

   procedure Find
     (Source                 : Source_File;
      Normalized_Entity_Name : String := "";
      Line                   : Integer;
      Column                 : Integer;
      Check_Decl_Only        : Boolean;
      Entity                 : out Entity_Information;
      Status                 : out Find_Decl_Or_Body_Query_Status);
   --  Find the closest entity to (Line, Column) in Source

   procedure Find_Any_Entity
     (File            : Source_File;
      Line            : Integer;
      Column          : Integer;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information);
   --  Find the entity in File which is referenced at the given location

   procedure Find_Any_Entity
     (Trie            : Entities_Tries.Trie_Tree;
      File            : Source_File;
      Line            : Integer;
      Column          : Integer;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information);
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
   --  A structure that contains entities and their enclosed lines (scopes).

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

   function To_String (Location : File_Location) return String;
   --  For debugging purposes only

   ---------------
   -- To_String --
   ---------------

   function To_String (Location : File_Location) return String is
   begin
      if Location = No_File_Location then
         return "<no loc>";
      else
         return Full_Name (Get_Filename (Location.File)).all
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
      Column          : Integer;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information)
   is
      Prox : Integer;
      E    : Entity_Information;
      Ref  : E_Reference;
   begin
      if EL /= null then
         For_Each_Entity :
         for Ent in Entity_Information_Arrays.First .. Last (EL.all) loop
            E := EL.Table (Ent);

            if E.Declaration.File = File then
               Prox := abs (Natural (E.Declaration.Column) - Column) +
               abs (E.Declaration.Line - Line) * Num_Columns_Per_Line;

               if Prox < Distance then
                  Closest := E;
                  Distance := Prox;
                  exit For_Each_Entity when Distance = 0;
               end if;
            end if;

            if not Check_Decl_Only then
               for R in
                 Entity_Reference_Arrays.First .. Last (E.References)
               loop
                  Ref := E.References.Table (R);

                  if Is_Real_Reference (Ref.Kind)
                    and then Ref.Location.File = File
                  then
                     Prox := abs (Natural (Ref.Location.Column) - Column) +
                        abs (Ref.Location.Line - Line) * Num_Columns_Per_Line;

                     if Prox < Distance then
                        Closest := E;
                        Distance := Prox;

                        exit For_Each_Entity when Distance = 0;
                     end if;
                  end if;
               end loop;
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
     (Trie            : Entities_Tries.Trie_Tree;
      File            : Source_File;
      Line            : Integer;
      Column          : Integer;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information)
   is
      Iter   : Entities_Tries.Iterator := Start (Trie, "");
      EL     : Entity_Information_List_Access;
   begin
      loop
         EL := Get (Iter);
         exit when EL = null;

         Find (EL, File, Line, Column, Check_Decl_Only, Distance, Closest);
         exit when Distance = 0;

         Next (Iter);
      end loop;

      Free (Iter);
   end Find_Any_Entity;

   ---------------------
   -- Find_Any_Entity --
   ---------------------

   procedure Find_Any_Entity
     (File            : Source_File;
      Line            : Integer;
      Column          : Integer;
      Check_Decl_Only : Boolean;
      Distance        : in out Integer;
      Closest         : in out Entity_Information) is
   begin
      Find_Any_Entity
        (File.Entities, File, Line, Column, Check_Decl_Only,
         Distance, Closest);

      if Distance /= 0 and then not Check_Decl_Only then
         Find_Any_Entity
           (File.All_Entities, File, Line, Column,
            Check_Decl_Only, Distance, Closest);
      end if;
   end Find_Any_Entity;

   ----------
   -- Find --
   ----------

   procedure Find
     (Source                 : Source_File;
      Normalized_Entity_Name : String := "";
      Line                   : Integer;
      Column                 : Integer;
      Check_Decl_Only        : Boolean;
      Entity                 : out Entity_Information;
      Status                 : out Find_Decl_Or_Body_Query_Status)
   is
      Distance : Integer := Integer'Last;
      Closest  : Entity_Information;
   begin
      Trace (Me, "Find name=" & Normalized_Entity_Name
             & " Source=" & Full_Name (Get_Filename (Source)).all
             & " line=" & Line'Img & " column=" & Column'Img
             & " check_decl=" & Check_Decl_Only'Img);

      if Normalized_Entity_Name = "" then
         Find_Any_Entity
           (Source, Line, Column, Check_Decl_Only, Distance, Closest);
      else
         Find
           (Get (Source.Entities, Normalized_Entity_Name), Source, Line,
            Column, Check_Decl_Only, Distance, Closest);

         Trace (Me, "After find in entities: distance=" & Distance'Img);

         if Distance /= 0 and then not Check_Decl_Only then
            Find (Get (Source.All_Entities, Normalized_Entity_Name),
                  Source, Line, Column, Check_Decl_Only, Distance, Closest);
            Trace (Me, "After find in all entities: distance=" & Distance'Img);
         end if;
      end if;

      if Distance = 0 then
         Status := Success;
         Entity := Closest;
      elsif Distance = Integer'Last then
         Status := Entity_Not_Found;
         Entity := null;
      else
         Status := Fuzzy_Match;
         Entity := Closest;
      end if;
   end Find;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : VFS.Virtual_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Natural;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False)
   is
      Handler  : constant LI_Handler := Get_LI_Handler (Db, File_Name);
      Source   : Source_File;
   begin
      if Handler = null then
         Trace (Me, "No such file registered: "
                & Full_Name (File_Name).all);
         Status := Entity_Not_Found;
         Entity := null;
      else
         Source := Get_Source_Info (Handler, File_Name);

         if Source /= null then
            Find_Declaration
              (Db, Source, Entity_Name, Line, Column, Entity, Status,
               Check_Decl_Only, Handler);
         end if;
      end if;
   end Find_Declaration;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Db              : Entities_Database;
      Source          : Source_File;
      Entity_Name     : String := "";
      Line            : Natural;
      Column          : Natural;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False;
      Handler         : LI_Handler := null)
   is
      H       : LI_Handler := Handler;
      Updated : Source_File;
      pragma Unreferenced (Updated);
   begin
      Trace (Me, "Find_Declaration entity=" & Entity_Name
             & " source=" & Full_Name (Get_Filename (Source)).all
             & " line=" & Line'Img
             & " column=" & Column'Img);
      if Source = Get_Predefined_File (Db) then
         Entity := Get_Or_Create
           (Name         => Entity_Name,
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

      if H /= null then
         Updated := Get_Source_Info (H, Get_Filename (Source));

         if Case_Insensitive_Identifiers (H) then
            Find (Source, UTF8_Strdown (Entity_Name), Line, Column,
                  Check_Decl_Only, Entity, Status);
         else
            Find (Source, Entity_Name, Line, Column, Check_Decl_Only,
                  Entity, Status);
         end if;
         Trace (Me, "Result=" & Status'Img);
      else
         Status := Entity_Not_Found;
         Entity := null;
         Trace (Me, "Entity not found");
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
      Ref     : E_Reference;
      First   : Entity_Reference_Arrays.Index_Type :=
        Entity_Reference_Arrays.First - 1;
      Return_Next : Boolean := Current_Location = No_File_Location;
   begin
      Trace (Me, "Find_Next_Body for "
             & Get_Name (Entity).all
             & " current=" & To_String (Current_Location));

      Update_Xref (Entity.Declaration.File);

      for R in Entity_Reference_Arrays.First .. Last (Entity.References) loop
         Ref := Entity.References.Table (R);
         if Ref.Kind = Body_Entity
           or else Ref.Kind = Completion_Of_Private_Or_Incomplete_Type
         then
            if Return_Next then
               Location := Ref.Location;
               return;
            end if;

            if First = Entity_Reference_Arrays.First - 1 then
               First := R;
            end if;
         end if;

         if Ref.Location = Current_Location then
            Return_Next := True;
         end if;
      end loop;

      if No_Location_If_First
        or else First = Entity_Reference_Arrays.First - 1
      then
         Location := No_File_Location;
      else
         Location := Entity.References.Table (First).Location;
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
      Bod : File_Location := No_File_Location;
      Start, Last : Integer;
   begin
      --  An entity is never in its own range
      if Loc = Get_Declaration_Of (Entity) then
         return False;
      end if;

      if Entity.Declaration.File = Loc.File then
         Start := Get_Start_Of_Scope_In_File
           (Entity, Loc.File, Force_Spec => True).Line;
         Last  := Get_End_Of_Scope_In_File
           (Entity, Loc.File, Force_Spec => True).Line;
         if In_Range (Loc, Entity.Declaration.File, Start, Last) then
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

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Iter                  : out Entity_Reference_Iterator;
      Entity                : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_File               : Source_File := null;
      In_Scope              : Entity_Information := null;
      Filter                : Reference_Kind_Filter := Real_References_Filter)
   is
      Deps : Dependency_Iterator;
      F    : Source_File := In_File;
      Loc  : File_Location := No_File_Location;
      Start, Last : Integer;
      Had_Old_Refs : constant Boolean := Length (Entity.References) /= 0;
   begin
      Assert (Me, Entity /= null,
              "No Entity specified to Find_All_References");

      Trace (Me, "Find_All_References to " & Get_Name (Entity).all
             & ' ' & To_String (Entity.Declaration)
             & " in_file=" & Boolean'Image (In_File /= null));

      if In_Scope /= null then
         loop
            Find_Next_Body (In_Scope, Loc, Location => Loc,
                            No_Location_If_First => True);
            if Loc = No_File_Location then
               F := In_Scope.Declaration.File;
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

      if F = null then
         Find_Ancestor_Dependencies
           (Deps,
            File                  => Get_File (Get_Declaration_Of (Entity)),
            File_Has_No_LI_Report => File_Has_No_LI_Report,
            Include_Self          => True);
      else
         Find_Ancestor_Dependencies
           (Deps,
            File                  => F,
            File_Has_No_LI_Report => File_Has_No_LI_Report,
            Include_Self          => True,
            Single_Source_File    => True);
      end if;

      Iter :=
        (Index                => Entity_Reference_Arrays.First,
         Decl_Returned        => not Filter (Declaration)
           or else
             (In_Scope /= null
              and then not In_Range (Entity.Declaration, F, Start, Last)),
         Returning_Existing_Refs => Had_Old_Refs,
         Last_Returned_File   => null,
         Entity               => Entity,
         Deps                 => Deps,
         In_File              => F,
         Start_Line           => Start,
         Filter               => Filter,
         Last_Line            => Last);

      --  If the first reference in the list is not correct, move to next one
      if Iter.Decl_Returned
        and then
          (Length (Entity.References) = 0
           or else not Iter.Filter (Entity.References.Table (Iter.Index).Kind)
           or else not In_Range
             (Entity.References.Table (Iter.Index).Location,
              Iter.In_File, Iter.Start_Line, Iter.Last_Line))
      then
         Next (Iter);
      end if;
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
   begin
      return Iter.Decl_Returned
        and then At_End (Iter.Deps)
        and then Iter.Index > Last (Iter.Entity.References);
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

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Entity_Reference_Iterator) is
   begin
      --  We always return the declaration first
      if not Iter.Decl_Returned then
         Iter.Decl_Returned := True;
      elsif Iter.Index <= Last (Iter.Entity.References) then
         Iter.Index := Iter.Index + 1;
      end if;

      while Iter.Index <= Last (Iter.Entity.References) loop
         if Iter.Returning_Existing_Refs
           and then Iter.Entity.References.Table (Iter.Index).Location.File /=
             Iter.Last_Returned_File
         then
            --  Are we still parsing the list of references that were there
            --  before the call to Find_All_References ? If yes, we need to
            --  check whether the file is still up-to-date

            Iter.Last_Returned_File :=
              Iter.Entity.References.Table (Iter.Index).Location.File;
            Update_Xref (Iter.Last_Returned_File);
            --  Do not move the index forward now, since parsing the file
            --  might have removed the reference from the list.

         else
            if Iter.Filter (Iter.Entity.References.Table (Iter.Index).Kind)
              and then In_Range
                (Iter.Entity.References.Table (Iter.Index).Location,
                 Iter.In_File, Iter.Start_Line, Iter.Last_Line)
            then
               return;
            end if;

            Iter.Index := Iter.Index + 1;
         end if;
      end loop;

      Iter.Returning_Existing_Refs := False;

      --  Parse the current file on the list

      if At_End (Iter.Deps) then
         return;
      end if;

      Next (Iter.Deps);

      --  The next time Next is called, Index will be incremented, so we have
      --  to make sure not to lose a reference here.
      if not At_End (Iter.Deps)
        and then Get (Iter.Deps) /= null
        and then Iter.Index <= Last (Iter.Entity.References)
      then
         Iter.Index := Iter.Index - 1;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Entity_Reference_Iterator) return Entity_Reference is
   begin
      if not Iter.Decl_Returned then
         return (Entity => Iter.Entity,
                 Index  => Entity_Reference_Arrays.Index_Type'Last);
      elsif Iter.Index > Last (Iter.Entity.References) then
         return No_Entity_Reference;
      else
         return (Entity => Iter.Entity, Index => Iter.Index);
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
      elsif Iter.Index > Last (Iter.Entity.References) then
         return null;
      else
         return Get_Entity_From_Ref
           (Iter.Entity,
            Iter.Entity.References.Table (Iter.Index));
      end if;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Reference_Iterator) is
   begin
      Destroy (Iter.Deps);
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
      Importing : Imported_Project_Iterator;
      Project   : Project_Type;
      Count     : Natural;
      pragma Unreferenced (Count);
   begin
--        Trace (Me, "Find_Ancestor_Dependencies: "
--               & Full_Name (Get_Filename (File)).all
--               & " Self=" & Boolean'Image (Include_Self)
--               & " Single=" & Boolean'Image (Single_Source_File));

      Update_Xref (File, File_Has_No_LI_Report);

      Project := Get_Project_From_File
        (File.Db.Registry.all, Get_Filename (File),
         Root_If_Not_Found => False);

      if Single_Source_File then
         if Project = No_Project then
            Project := Get_Root_Project (File.Db.Registry.all);
         end if;

         Iter := (Importing             => Start (Project, Recursive => False),
                  Db                    => File.Db,
                  Include_Self          => Include_Self,
                  File_Has_No_LI_Report => File_Has_No_LI_Report,
                  Single_Source_File    => Single_Source_File,
                  Handler               => null,
                  Total_Progress        => 1,
                  Current_Progress      => 0,
                  Dep_Index             => Dependency_Arrays.First,
                  Source_File_Index     => 0,
                  File                  => File);
         Next (Iter);
      else
         if Project = No_Project then
            --  Project not found ? We'll have to parse all projects, since
            --  it might be from the GNAT runtime
            Importing := Start
              (Get_Root_Project (File.Db.Registry.all), Recursive => True);
         else
            Importing := Find_All_Projects_Importing
              (Project, Include_Self => True);
         end if;

         Iter := (Importing             => Importing,
                  Db                    => File.Db,
                  Include_Self          => Include_Self,
                  File_Has_No_LI_Report => File_Has_No_LI_Report,
                  Single_Source_File    => Single_Source_File,
                  Handler               => Get_LI_Handler_From_File
                    (Glide_Language_Handler (File.Db.Lang),
                     Get_Filename (File)),
                  Total_Progress        => 0,
                  Current_Progress      => 1,
                  Dep_Index             => Dependency_Arrays.First,
                  Source_File_Index     => 0,
                  File                  => File);

         while Current (Importing) /= No_Project loop
            if Active (Find_Deps_File_Granularity) then
               Iter.Total_Progress := Iter.Total_Progress
                 + Direct_Sources_Count (Current (Importing));
            else
               Iter.Total_Progress := Iter.Total_Progress + 1;
            end if;
            Next (Importing);
         end loop;

         if Iter.Handler /= null then
            if not Active (Find_Deps_File_Granularity) then
               Count := Parse_All_LI_Information
                 (Handler   => Iter.Handler,
                  Project   => Current (Iter.Importing),
                  Recursive => False);
            end if;
         end if;
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
      return Iter.Handler = null
        and then Iter.Dep_Index > Last (Iter.File.Depended_On)
        and then (Iter.Single_Source_File
                  or else Current (Iter.Importing) = No_Project);
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Dependency_Iterator) is
      Source : Source_File;
      Count  : Natural;
      VF     : VFS.Virtual_File := VFS.No_File;
      pragma Unreferenced (Source, Count);
   begin
      if Iter.Handler = null then
         Iter.Dep_Index := Iter.Dep_Index + 1;
      else
         if Active (Find_Deps_File_Granularity) then
            Iter.Source_File_Index := Iter.Source_File_Index + 1;
            VF := Get_Source_Files
              (Current (Iter.Importing), Iter.Source_File_Index);
            if VF /= VFS.No_File then
               Iter.Current_Progress := Iter.Current_Progress + 1;
               Source := Get_Source_Info (Iter.Handler, VF);
            end if;
         end if;

         if VF = VFS.No_File then
            Next (Iter.Importing);
            if Current (Iter.Importing) = No_Project then
               Iter.Handler := null;
            else
               if not Active (Find_Deps_File_Granularity) then
                  Iter.Current_Progress := Iter.Current_Progress + 1;
                  Count := Parse_All_LI_Information
                    (Handler   => Iter.Handler,
                     Project   => Current (Iter.Importing),
                     Recursive => False);
               else
                  Iter.Source_File_Index := 0;
               end if;
            end if;
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
      pragma Unreferenced (Iter);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Dependency_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Dependency_Iterator, Dependency_Iterator_Access);
   begin
      Destroy (Iter.all);
      Unchecked_Free (Iter);
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
      return Iter.Dep_Index > Last (Iter.File.Depends_On);
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
      Iter := (Index         => Entity_Reference_Arrays.First,
               Entity        => Subprogram,
               Cache_Current => null);
      if Length (Iter.Entity.References) > 0
        and then not Is_Parameter_Reference
          (Iter.Entity.References.Table (Iter.Index).Kind)
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
         Iterator.Index := Iterator.Index + 1;
         exit when Iterator.Index > Last (Iterator.Entity.References)
           or else Is_Parameter_Reference
             (Iterator.Entity.References.Table (Iterator.Index).Kind);
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
        and then Iterator.Index <= Last (Iterator.Entity.References)
      then
         Loc := Iterator.Entity.References.Table (Iterator.Index).Location;

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
      case Iterator.Entity.References.Table (Iterator.Index).Kind is
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
   begin
      if Force_Spec then
         for R in
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            Ref := Entity.References.Table (R);

            --  Use this as the "declaration", since otherwise the
            --  End_Of_Spec reference is meaningless
            if Ref.Kind = Completion_Of_Private_Or_Incomplete_Type
              and then Get_File (Ref.Location) = File
            then
               return Ref.Location;
            end if;
         end loop;

         if Entity.Declaration.File = File then
            return Entity.Declaration;
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
   begin
      if Entity.End_Of_Scope.Location.File = File
        and then ((Force_Spec and then Entity.End_Of_Scope.Kind = End_Of_Spec)
                  or else (not Force_Spec
                           and then Entity.End_Of_Scope.Kind = End_Of_Body))
      then
         return Entity.End_Of_Scope.Location;
      else
         for R in
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            Ref := Entity.References.Table (R);
            if ((Force_Spec and then Ref.Kind = End_Of_Spec)
                or else (not Force_Spec and then Ref.Kind = End_Of_Body))
              and then Ref.Location.File = File
            then
               return Ref.Location;
            end if;
         end loop;

         return No_File_Location;
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
            --  number of the sibling tree entities
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

   procedure Dump (T : Scope_Tree_Access; Prefix : String);
   procedure Dump (T : Scope_Tree_Access; Prefix : String) is
      T2 : Scope_Tree_Access := T;
   begin
      while T2 /= null loop
         Trace
           (Me, Prefix
            & Get_Name (T2.Entity).all
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
      Iter : Files_HTable.Iterator;
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

   --------------------------------
   -- Compute_Callers_And_Called --
   --------------------------------

   procedure Compute_Callers_And_Called (File : Source_File) is
      type Entity_Info_Array is array (Natural range <>) of Entity_Information;
      --  Matches a line number with the inner-most enclosing entity

      procedure Add_To_Tree
        (Tree : in out Scope_Tree_Access;
         Trie : Entities_Tries.Trie_Tree);
      --  Add all entities from Trie to the tree

      procedure Fill_Table
        (Tree             : Scope_Tree_Access;
         Line_Start       : Natural;
         Enclosing_Entity : Entity_Information;
         Line_Last        : out Natural;
         Info             : in out Entity_Info_Array;
         Info_For_Decl    : in out Entity_Info_Array);
      --  Set the information in Info based on Tre

      procedure Process_All_Entities_Refs
        (Info           : Entity_Info_Array;
         Info_For_Decl  : Entity_Info_Array;
         For_Entities   : Entities_Tries.Trie_Tree;
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

      -----------------
      -- Add_To_Tree --
      -----------------

      procedure Add_To_Tree
        (Tree : in out Scope_Tree_Access;
         Trie : Entities_Tries.Trie_Tree)
      is
         Iter   : Entities_Tries.Iterator := Start (Trie, "");
         EL     : Entity_Information_List_Access;
         Entity : Entity_Information;
         End_Of_Scope   : File_Location;
         Start_Of_Scope : File_Location;
      begin
         loop
            EL := Get (Iter);
            exit when EL = null;

            for E in Entity_Information_Arrays.First .. Last (EL.all) loop
               Entity := EL.Table (E);

               --  Add the spec too, since if it is on multiple lines, we
               --  want the parameters to be associated with that subprogram,
               --  and not to the caller

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
            end loop;

            Next (Iter);
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
               Info_For_Decl (T.Start_Line) := Enclosing_Entity;
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

            if T.End_Line - 1 <= Info'Last then
               Info (Line .. T.End_Line - 1) := (others => T.Entity);
            end if;

            Line := T.End_Line;

            Line_Last := T.End_Line;
            T := T.Sibling;
         end loop;
      end Fill_Table;

      -------------------------------
      -- Process_All_Entities_Refs --
      -------------------------------

      procedure Process_All_Entities_Refs
        (Info           : Entity_Info_Array;
         Info_For_Decl  : Entity_Info_Array;
         For_Entities   : Entities_Tries.Trie_Tree;
         Add_Deps       : Boolean)
      is
         Iter   : Entities_Tries.Iterator := Start (For_Entities, "");
         EL     : Entity_Information_List_Access;
         Caller : Entity_Information;
      begin
         loop
            EL := Get (Iter);
            exit when EL = null;

            for E in Entity_Information_Arrays.First .. Last (EL.all) loop
               if Add_Deps then
                  Add_Depends_On (EL.Table (E).Declaration.File, File);
               end if;

               if EL.Table (E).Declaration.File = File
                 and then EL.Table (E).Declaration.Line in Info'Range
               then
                  Caller := Info (EL.Table (E).Declaration.Line);

                  if Caller = EL.Table (E) then
                     Caller := Info_For_Decl (EL.Table (E).Declaration.Line);
                  end if;

                  EL.Table (E).Caller_At_Declaration := Caller;

                  if Caller /= null then
                     Add (Caller.Called_Entities,
                          EL.Table (E),
                          Check_Duplicates => True);
                  end if;
               end if;

               Process_All_Refs (Info, EL.Table (E), Info_For_Decl);
            end loop;

            Next (Iter);
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
         Refs   : constant Entity_Reference_List := Entity.References;
         Caller : Entity_Information;
      begin
         for R in Entity_Reference_Arrays.First .. Last (Refs) loop
            if Refs.Table (R).Location.File = File
              and then Is_Real_Reference (Refs.Table (R).Kind)
              and then Refs.Table (R).Kind /= Label
              and then Refs.Table (R).Location.Line <= Info'Last
            then
               Caller := Info (Refs.Table (R).Location.Line);

               if Caller = Entity then
                  Caller := Info_For_Decl (Refs.Table (R).Location.Line);
               end if;

               Refs.Table (R).Caller := Caller;

               if Caller /= null then
                  Add (Caller.Called_Entities,
                       Entity,
                       Check_Duplicates => True);
               end if;
            end if;
         end loop;
      end Process_All_Refs;

      Tree   : Scope_Tree_Access;
      Line_Max : Integer := 0;
      T      : Scope_Tree_Access;
   begin
      if File = null or else File.Scope_Tree_Computed then
         return;
      end if;

      Update_Xref (File);

      Add_To_Tree (Tree, File.Entities);
      Add_To_Tree (Tree, File.All_Entities);

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
            --  We need two tables to memorize the entity enclosing a given
            --  line: in the case of "procedure A (B : Integer)", the caller
            --  of A is the package, whereas the caller of B is A itself.

            Line_Info     : Entity_Info_Array (1 .. Line_Max);
            Info_For_Decl : Entity_Info_Array (1 .. Line_Max);
            Last          : Integer;
         begin
            Fill_Table
              (Tree             => Tree,
               Line_Start       => 1,
               Enclosing_Entity => null,
               Line_Last        => Last,
               Info             => Line_Info,
               Info_For_Decl    => Info_For_Decl);

            Free (Tree);

--              for L in Line_Info'Range loop
--                 if Line_Info (L) /= null then
--                    Trace (Me, "Line" & L'Img & " "
--                           & Get_Name (Line_Info (L)).all);
--                 end if;
--              end loop;

            Process_All_Entities_Refs
              (Line_Info, Info_For_Decl, File.Entities, Add_Deps => False);
            Process_All_Entities_Refs
              (Line_Info, Info_For_Decl, File.All_Entities, Add_Deps => True);
         end;
      end if;

      File.Scope_Tree_Computed := True;
   end Compute_Callers_And_Called;

   -------------------------
   -- Get_Entity_From_Ref --
   -------------------------

   function Get_Entity_From_Ref
     (Entity : Entity_Information;
      Ref    : E_Reference)
      return Entity_Information
   is
      Result : Entity_Information;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      if Is_Real_Reference (Ref.Kind) then
         return Entity;
      else
         Find_Declaration
           (Db        => Entity.Declaration.File.Db,
            File_Name => Get_Filename (Get_File (Ref.Location)),
            Line      => Get_Line (Ref.Location),
            COlumn    => Get_Column (Ref.Location),
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
     (Pkg : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Pkg.Declaration.File);
      for R in Entity_Reference_Arrays.First .. Last (Pkg.References) loop
         if Pkg.References.Table (R).Kind = Parent_Package then
            return Get_Entity_From_Ref (Pkg, Pkg.References.Table (R));
         end if;
      end loop;
      return null;
   end Get_Parent_Package;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name
     (Entity    : Entity_Information;
      Separator : String := ".") return String
   is
      E : Entity_Information := Entity;
      Last_Not_Null : Entity_Information := Entity;
      Length : Natural := 0;
   begin
      while E /= null loop
         Length := Length + E.Name'Length + Separator'Length;
         Compute_Callers_And_Called (E.Declaration.File);
         Last_Not_Null := E;
         E := E.Caller_At_Declaration;
      end loop;

      --  If there is no more caller in the tree, we still need to
      --  look for possible parent packages

      E := Last_Not_Null;
      loop
         E := Get_Parent_Package (E);
         exit when E = null;
         Length := Length + E.Name'Length + Separator'Length;
      end loop;


      declare
         Result : String (1 .. Length);
         Index  : Natural := Result'Last;
      begin
         E := Entity;
         while E /= null loop
            Result (Index - Separator'Length + 1 .. Index) := Separator;
            Index := Index - Separator'Length;
            Result (Index - E.Name'Length + 1 .. Index) := E.Name.all;
            Index := Index - E.Name'Length;

            E := E.Caller_At_Declaration;
         end loop;

         E := Last_Not_Null;
         loop
            E := Get_Parent_Package (E);
            exit when E = null;
            Result (Index - Separator'Length + 1 .. Index) := Separator;
            Index := Index - Separator'Length;
            Result (Index - E.Name'Length + 1 .. Index) := E.Name.all;
            Index := Index - E.Name'Length;
         end loop;

         return Result (Result'First .. Result'Last - 1);
      end;
   end Get_Full_Name;

   ----------------
   -- Get_Caller --
   ----------------

   function Get_Caller (Ref : Entity_Reference) return Entity_Information is
   begin
      if Ref.Entity = null then
         return null;
      elsif Ref.Index = Entity_Reference_Arrays.Index_Type'Last then
         Compute_Callers_And_Called (Ref.Entity.Declaration.File);
         return Ref.Entity.Caller_At_Declaration;
      else
         Compute_Callers_And_Called
           (Ref.Entity.References.Table (Ref.Index).Location.File);
         return Ref.Entity.References.Table (Ref.Index).Caller;
      end if;
   end Get_Caller;

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   function Get_All_Called_Entities
     (Entity : Entity_Information) return Calls_Iterator
   is
      Loc : File_Location := No_File_Location;
      Iter : Entities_Tries.Iterator;
   begin
      loop
         Find_Next_Body
           (Entity, Loc, Location => Loc, No_Location_If_First => True);
         if Loc = No_File_Location then
            Compute_Callers_And_Called (Entity.Declaration.File);
            exit;
         else
            Compute_Callers_And_Called (Loc.File);
         end if;
      end loop;

      Iter := Start (Entity.Called_Entities, "");

      return (Entity => Entity,
              Iter   => Iter,
              EL     => Get (Iter),
              Index  => Entity_Information_Arrays.First);
   end Get_All_Called_Entities;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Calls_Iterator) is
   begin
      Free (Iter.Iter);
   end Destroy;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Calls_Iterator) return Boolean is
   begin
      return Iter.EL = null;
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : Calls_Iterator) return Entity_Information is
   begin
      return Iter.EL.Table (Iter.Index);
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Calls_Iterator) is
   begin
      Iter.Index := Iter.Index + 1;
      if Iter.Index > Last (Iter.EL.all) then
         Next (Iter.Iter);
         Iter.EL := Get (Iter.Iter);
         Iter.Index := Entity_Information_Arrays.First;
      end if;
   end Next;

   -------------------------------
   -- Find_All_Entities_In_File --
   -------------------------------

   procedure Find_All_Entities_In_File
     (Iter                  : out Entity_Iterator;
      File                  : Source_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Prefix                : String := "")
   is
   begin
      Update_Xref (File, File_Has_No_LI_Report);
      Iter.Iter := Start (File.Entities, Prefix);
      Iter.Prefix := new String'(Prefix);
      Iter.File := File;

      Iter.EL := Get (Iter.Iter);

      if Iter.EL = null then
         Iter.Processing_Entities := False;
         Next (Iter);
      else
         Iter.Index_In_EL := Entity_Information_Arrays.First;
         Iter.Processing_Entities := True;
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
      return Iter.EL.Table (Iter.Index_In_EL);
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Entity_Iterator) is
   begin
      Iter.Index_In_EL := Iter.Index_In_EL + 1;

      if Iter.EL = null or else Iter.Index_In_EL > Last (Iter.EL.all) then
         Next (Iter.Iter);
         Iter.EL := Get (Iter.Iter);
         Iter.Index_In_EL := Entity_Information_Arrays.First;

         if Iter.EL = null
           and then Iter.Processing_Entities
         then
            Free (Iter.Iter);
            Iter.Iter := Start (Iter.File.All_Entities, Iter.Prefix.all);
            Iter.EL   := Get (Iter.Iter);
            Iter.Processing_Entities := False;
         end if;
      end if;
   end Next;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Iterator) is
   begin
      Free (Iter.Iter);
      Free (Iter.Prefix);
   end Destroy;

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
      return Entity.Pointed_Type;
   end Array_Contents_Type;

   ------------------
   -- Pointed_Type --
   ------------------

   function Pointed_Type
     (Entity : Entity_Information) return Entity_Information is
   begin
      Update_Xref (Get_File (Get_Declaration_Of (Entity)));
      return Entity.Pointed_Type;
   end Pointed_Type;

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
     (Iter              : out Primitive_Operations_Iterator;
      Entity            : Entity_Information;
      Include_Inherited : Boolean)
   is
   begin
      if Include_Inherited then
         Iter.Parents := new Entity_Information_Array'
           (Entity & Get_Parent_Types (Entity, Recursive => True));
      else
         Iter.Parents := new Entity_Information_Array'(1 => Entity);
      end if;

      Iter.Current_Parent    := Iter.Parents'First;
      Iter.Current_Primitive := Entity_Information_Arrays.First;

      Find_All_References
        (Iter.Refs,
         Entity => Entity,
         In_File => Get_File (Get_Declaration_Of (Entity)),
         Filter => (Primitive_Operation => True,
                    others              => False));

      if Iter.Current_Primitive >
        Last (Iter.Parents (Iter.Current_Parent).Primitive_Subprograms)
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

         while Iter.Current_Primitive >
           Last (Iter.Parents (Iter.Current_Parent).Primitive_Subprograms)
         loop
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
      Destroy (Iter.Refs);
      Unchecked_Free (Iter.Parents);
   end Destroy;

   ----------------------
   -- Get_Parent_Types --
   ----------------------

   function Get_Parent_Types
     (Entity    : Entity_Information;
      Recursive : Boolean := False) return Entity_Information_Array
   is
      Result : Entity_Information_List;

      procedure Process_Recursive (Entity : Entity_Information);
      --  Add Entity and possibly all its parents to Result

      -----------------------
      -- Process_Recursive --
      -----------------------

      procedure Process_Recursive (Entity : Entity_Information) is
      begin
         Append (Result, Entity);

         if Recursive then
            Update_Xref (Get_File (Get_Declaration_Of (Entity)));

            for P in
              Entity_Information_Arrays.First .. Last (Entity.Parent_Types)
            loop
               Process_Recursive (Entity.Parent_Types.Table (P));
            end loop;
         end if;
      end Process_Recursive;

   begin
      for P in
        Entity_Information_Arrays.First .. Last (Entity.Parent_Types)
      loop
         Process_Recursive (Entity.Parent_Types.Table (P));
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
   end Get_Parent_Types;

   ---------------------
   -- Get_Child_Types --
   ---------------------

   procedure Get_Child_Types
     (Iter   : out Child_Type_Iterator;
      Entity : Entity_Information) is
   begin
      --  Parse all the files that might contain a reference to the entity
      Find_Ancestor_Dependencies
        (Iter         => Iter.Dep,
         File         => Get_File (Get_Declaration_Of (Entity)),
         Include_Self => True);
      Iter.Entity := Entity;
      Iter.Index  := Entity_Information_Arrays.First;
   end Get_Child_Types;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Child_Type_Iterator) return Boolean is
   begin
      return At_End (Iter.Dep)
        and then Iter.Index > Last (Iter.Entity.Child_Types);
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Child_Type_Iterator) is
   begin
      if At_End (Iter.Dep) then
         Iter.Index := Iter.Index + 1;
      else
         Next (Iter.Dep);
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Child_Type_Iterator) return Entity_Information is
   begin
      if At_End (Iter.Dep) then
         return Iter.Entity.Child_Types.Table (Iter.Index);
      else
         return null;
      end if;
   end Get;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Child_Type_Iterator) is
   begin
      Destroy (Iter.Dep);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Child_Type_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Child_Type_Iterator, Child_Type_Iterator_Access);
   begin
      if Iter /= null then
         Destroy (Iter.all);
         Unchecked_Free (Iter);
      end if;
   end Destroy;

end Entities.Queries;
