with VFS;      use VFS;
with Traces;   use Traces;
with Projects; use Projects;

with Glib.Unicode; use Glib.Unicode;

with Ada.Unchecked_Deallocation;

package body Entities.Queries is
   Me : constant Debug_Handle := Create ("Entities.Queries");

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
   --  Find the closest entity to (Line, Column) in Source.

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
     (Entity : Entity_Information; File : Source_File) return File_Location;
   function Get_End_Of_Scope_In_File
     (Entity : Entity_Information; File : Source_File) return File_Location;
   --  Get the range of lines that Entity encloses in File.
   --  If Entity doesn't have the notion of enclosed lines (a simple integer
   --  for instance), then No_File_Location is returned

   type Scope_Tree;
   type Scope_Tree_Access is access Scope_Tree;
   type Scope_Tree is record
      Sibling     : Scope_Tree_Access;
      Parent      : Scope_Tree_Access;
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
      Parent         : Scope_Tree_Access;
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
               Prox := abs (E.Declaration.Column - Column) +
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
                     Prox := abs (Ref.Location.Column - Column) +
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
      if Normalized_Entity_Name = "" then
         Find_Any_Entity
           (Source, Line, Column, Check_Decl_Only, Distance, Closest);
      else
         Find
           (Get (Source.Entities, Normalized_Entity_Name), Source, Line,
            Column, Check_Decl_Only, Distance, Closest);

         if Distance /= 0 and then not Check_Decl_Only then
            Find (Get (Source.All_Entities, Normalized_Entity_Name),
                  Source, Line, Column, Check_Decl_Only, Distance, Closest);
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
      Line            : Positive;
      Column          : Positive;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status;
      Check_Decl_Only : Boolean := False)
   is
      Handler  : constant LI_Handler := Get_LI_Handler (Db, File_Name);
      Source   : constant Source_File := Get_Source_Info (Handler, File_Name);
   begin
      if Source = null then
         Trace (Me, "No such file registered: " & Full_Name (File_Name).all);
         Status := Entity_Not_Found;
         Entity := null;

      elsif Case_Insensitive_Identifiers (Handler) then
         Find (Source, UTF8_Strdown (Entity_Name), Line, Column,
               Check_Decl_Only, Entity, Status);
      else
         Find (Source, Entity_Name, Line, Column, Check_Decl_Only,
               Entity, Status);
      end if;
   end Find_Declaration;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Entity           : Entity_Information;
      Current_Location : File_Location := No_File_Location;
      Location         : out File_Location)
   is
      Ref     : E_Reference;
      First   : Entity_Reference_Arrays.Index_Type :=
        Entity_Reference_Arrays.First - 1;
      Return_Next : Boolean := Current_Location = No_File_Location;
   begin
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

      if First = Entity_Reference_Arrays.First - 1 then
         Location := No_File_Location;
      else
         Location := Entity.References.Table (First).Location;
      end if;
   end Find_Next_Body;

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Iter                  : out Entity_Reference_Iterator;
      Entity                : Entity_Information;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      In_File               : Source_File := null;
      In_Scope              : Entity_Information := null)
   is
      Deps : Dependency_Iterator;
      F    : Source_File := In_File;
      Loc  : File_Location;
      Start, Last : Integer;
   begin
      Assert (Me, Entity /= null,
              "No Entity specified to Find_All_References");

      if In_Scope /= null then
         Find_Next_Body (In_Scope, Location => Loc);
         if Loc = No_File_Location then
            F := Entity.Declaration.File;
         else
            F := Loc.File;
         end if;

         Start := Get_Start_Of_Scope_In_File (In_Scope, F).Line;
         Last  := Get_End_Of_Scope_In_File   (In_Scope, F).Line;
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
        (Need_To_Update_Files => F = null,
         Index                => Entity_Reference_Arrays.First,
         Decl_Returned        => In_Scope /= null
            and then not In_Range (Entity.Declaration, F, Start, Last),
         Entity               => Entity,
         Deps                 => Deps,
         In_File              => F,
         Start_Line           => Start,
         Last_Line            => Last);

      if Iter.Entity.References /= Null_Entity_Reference_List then
         if (In_Scope /= null
             and then not In_Range
               (Iter.Entity.References.Table (Iter.Index).Location,
                Iter.In_File, Iter.Start_Line, Iter.Last_Line))
           or else not Is_Real_Reference
             (Iter.Entity.References.Table (Iter.Index).Kind)
         then
            Next (Iter);
         end if;
      end if;
   end Find_All_References;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Entity_Reference_Iterator) return Boolean is
   begin
      return Iter.Decl_Returned
        and then not Iter.Need_To_Update_Files
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
      if Iter.Need_To_Update_Files then
         if At_End (Iter.Deps)
           or else Get (Iter.Deps) /= null
         then
            Iter.Need_To_Update_Files := False;
         else
            Next (Iter.Deps);
         end if;
      elsif Iter.Decl_Returned then
         loop
            Iter.Index := Iter.Index + 1;
            exit when Iter.Index > Last (Iter.Entity.References)
              or else
                (Is_Real_Reference
                     (Iter.Entity.References.Table (Iter.Index).Kind)
                 and then In_Range
                   (Iter.Entity.References.Table (Iter.Index).Location,
                    Iter.In_File, Iter.Start_Line, Iter.Last_Line));
         end loop;
      else
         Iter.Decl_Returned := True;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Entity_Reference_Iterator) return Entity_Reference is
   begin
      if Iter.Need_To_Update_Files then
         return No_Entity_Reference;
      elsif not Iter.Decl_Returned then
         return (Entity => Iter.Entity,
                 Index  => Entity_Reference_Arrays.Index_Type'Last);
      else
         return (Entity => Iter.Entity, Index => Iter.Index);
      end if;
   end Get;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Reference_Iterator) is
   begin
      Destroy (Iter.Deps);
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
   begin
      Trace (Me, "Find_Ancestor_Dependencies: "
             & Full_Name (Get_Filename (File)).all
             & " Self=" & Boolean'Image (Include_Self)
             & " Single=" & Boolean'Image (Single_Source_File));

      Update_Xref (File, File_Has_No_LI_Report);

      Assert (Me, File.LI /= null, "No LI file known");

      if Single_Source_File then
         Iter := (Importing             =>
                    Start (Get_Project (File.LI), Recursive => False),
                  Source_Files          =>
                    new File_Array'(1 => Get_Filename (File)),
                  Current_File          => 0,
                  Include_Self          => Include_Self,
                  File_Has_No_LI_Report => File_Has_No_LI_Report,
                  Single_Source_File    => Single_Source_File,
                  Total_Progress        => 1,
                  Current_Progress      => 0,
                  Dep_Index             => Dependency_Arrays.First,
                  File                  => File);
      else
         Importing := Find_All_Projects_Importing
           (Get_Project (File.LI), Include_Self => True);
         Iter := (Importing             => Importing,
                  Source_Files          => Get_Source_Files
                    (Current (Importing), Recursive => False),
                  Current_File          => 0,
                  Include_Self          => Include_Self,
                  File_Has_No_LI_Report => File_Has_No_LI_Report,
                  Single_Source_File    => Single_Source_File,
                  Total_Progress        => 0,
                  Current_Progress      => 0,
                  Dep_Index             => Dependency_Arrays.First,
                  File                  => File);
         Iter.Current_File := Iter.Source_Files'First - 1;

         while Current (Importing) /= No_Project loop
            Iter.Total_Progress := Iter.Total_Progress
              + Direct_Sources_Count (Current (Importing));
            Next (Importing);
         end loop;
      end if;

      Next (Iter);
   end Find_Ancestor_Dependencies;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Dependency_Iterator) return Boolean is
   begin
      return Iter.Source_Files = null
        and then Iter.Dep_Index > Last (Iter.File.Depended_On);
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Dependency_Iterator) is
      Source : Source_File;
      pragma Unreferenced (Source);
   begin
      if Iter.Source_Files = null then
         Iter.Dep_Index := Iter.Dep_Index + 1;
      else
         loop
            loop
               Iter.Current_File := Iter.Current_File + 1;
               exit when Iter.Current_File > Iter.Source_Files'Last;

               if Iter.Include_Self
                 or else Iter.Source_Files (Iter.Current_File) /=
                    Get_Filename (Iter.File)
               then
                  Iter.Current_Progress := Iter.Current_Progress + 1;
                  Source := Get_Source_Info
                    (Get_LI_Handler
                       (Iter.File.Db, Iter.Source_Files (Iter.Current_File)),
                     Iter.Source_Files (Iter.Current_File),
                     Iter.File_Has_No_LI_Report);
                  return;
               end if;
            end loop;

            Unchecked_Free (Iter.Source_Files);
            Next (Iter.Importing);
            if Current (Iter.Importing) = No_Project then
               return;
            end if;

            Iter.Source_Files := Get_Source_Files
              (Current (Iter.Importing), Recursive => False);
            Iter.Current_File := Iter.Source_Files'First - 1;
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Dependency_Iterator) return Source_File is
   begin
      if Iter.Source_Files /= null then
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
      if Iter.Source_Files /= null then
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
      Unchecked_Free (Iter.Source_Files);
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
      Iter := (Dep_Index             => Dependency_Arrays.First,
               File                  => File);
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
     (Entity : Entity_Information; File : Source_File) return File_Location
   is
      Loc : File_Location := No_File_Location;
   begin
      if Entity.Declaration.File = File then
         return Entity.Declaration;
      else
         loop
            Find_Next_Body
              (Entity           => Entity,
               Current_Location => Loc,
               Location         => Loc);
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
     (Entity : Entity_Information; File : Source_File) return File_Location
   is
      Ref : E_Reference;
   begin
      if Entity.End_Of_Scope.Location.File = File then
         return Entity.End_Of_Scope.Location;
      else
         for R in
           Entity_Reference_Arrays.First .. Last (Entity.References)
         loop
            Ref := Entity.References.Table (R);
            if (Ref.Kind = End_Of_Spec or else Ref.Kind = End_Of_Body)
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
      Parent         : Scope_Tree_Access;
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
            Parent      => Parent,
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
                     Parent      => Tree.Parent,
                     First_Child => null,
                     Entity      => Entity,
                     Start_Line  => Start_Of_Scope.Line,
                     End_Line    => End_Of_Scope.Line);
               else
                  Previous.Sibling := new Scope_Tree'
                    (Sibling     => Previous.Sibling,
                     Parent      => Tree.Parent,
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
                  Parent         => Tree,
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
                     Parent      => Tree.Parent,
                     First_Child => T,
                     Entity      => Entity,
                     Start_Line  => Start_Of_Scope.Line,
                     End_Line    => End_Of_Scope.Line);
               else
                  Previous.Sibling := new Scope_Tree'
                    (Sibling     => Sib.Sibling,
                     Parent      => Tree.Parent,
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
            Parent      => Tree,
            First_Child => null,
            Entity      => Entity,
            Start_Line  => Start_Of_Scope.Line,
            End_Line    => End_Of_Scope.Line);
      end if;
   end Add_To_Tree;

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
        (Tree       : Scope_Tree_Access;
         Line_Start : Natural;
         Enclosing_Entity : Entity_Information;
         Line_Last  : out Natural;
         Info       : in out Entity_Info_Array);
      --  Set the information in Info based on Tre

      procedure Process_All_Entities_Refs
        (Info         : Entity_Info_Array;
         For_Entities : Entities_Tries.Trie_Tree;
         Add_Deps     : Boolean);
      --  We now have in Lines the inner-most entity at that scope, used
      --  for computing the parent for specific references.
      --  Traverse all the entities in For_Entities, for all
      --  references in File, and set their caller.
      --  If Add_Deps is True, then a dependency is added between File and
      --  the declaration file of the entities.

      procedure Process_All_Refs
        (Info         : Entity_Info_Array;
         Entity       : Entity_Information);
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

               End_Of_Scope := Get_End_Of_Scope_In_File (Entity, File);

               if End_Of_Scope /= No_File_Location then
                  Start_Of_Scope := Get_Start_Of_Scope_In_File (Entity, File);
                  Add_To_Tree
                    (Tree           => Tree,
                     Parent         => null,
                     Entity         => Entity,
                     Start_Of_Scope => Start_Of_Scope,
                     End_Of_Scope   => End_Of_Scope);
               end if;
            end loop;

            Next (Iter);
         end loop;
      end Add_To_Tree;

      ----------------
      -- Fill_Table --
      ----------------

      procedure Fill_Table
        (Tree       : Scope_Tree_Access;
         Line_Start : Natural;
         Enclosing_Entity : Entity_Information;
         Line_Last  : out Natural;
         Info       : in out Entity_Info_Array)
      is
         T    : Scope_Tree_Access := Tree;
         Line : Natural := Line_Start;
      begin
         Line_Last := Line_Start;

         while T /= null loop
            Info (Line .. T.Start_Line) := (others => Enclosing_Entity);

            if T.First_Child = null then
               Line := T.Start_Line + 1;
            else
               Fill_Table
                 (Tree             => Tree.First_Child,
                  Line_Start       => T.Start_Line + 1,
                  Enclosing_Entity => T.Entity,
                  Line_Last        => Line,
                  Info             => Info);
               Line := Line + 1;
            end if;

            Info (Line .. T.End_Line - 1) := (others => T.Entity);
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

               if EL.Table (E).Declaration.Line in Info'Range then
                  Caller := Info (EL.Table (E).Declaration.Line);
                  EL.Table (E).Caller_At_Declaration := Caller;

                  if Caller /= null then
                     Add (Caller.Called_Entities,
                          EL.Table (E),
                          Check_Duplicates => True);
                  end if;
               end if;


               Process_All_Refs (Info, EL.Table (E));
            end loop;

            Next (Iter);
         end loop;
      end Process_All_Entities_Refs;

      ----------------------
      -- Process_All_Refs --
      ----------------------

      procedure Process_All_Refs
        (Info         : Entity_Info_Array;
         Entity       : Entity_Information)
      is
         Refs   : Entity_Reference_List := Entity.References;
         Caller : Entity_Information;
      begin
         for R in Entity_Reference_Arrays.First .. Last (Refs) loop
            if Refs.Table (R).Location.File = File
              and then Is_Real_Reference (Refs.Table (R).Kind)
              and then Refs.Table (R).Location.Line <= Info'Last
            then
               Caller := Info (Refs.Table (R).Location.Line);
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

      Trace (Me, "Computing scope tree for "
             & Full_Name (Get_Filename (File)).all);

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
            Line_Info : Entity_Info_Array (1 .. Line_Max);
            Last      : Integer;
         begin
            Fill_Table
              (Tree             => Tree,
               Line_Start       => 1,
               Enclosing_Entity => null,
               Line_Last        => Last,
               Info             => Line_Info);
            Free (Tree);

            Process_All_Entities_Refs
              (Line_Info, File.Entities, Add_Deps => False);
            Process_All_Entities_Refs
              (Line_Info, File.All_Entities, Add_Deps => True);
         end;
      end if;

      File.Scope_Tree_Computed := True;
   end Compute_Callers_And_Called;

   ----------------
   -- Get_Caller --
   ----------------

   function Get_Caller (Ref : Entity_Reference) return Entity_Information is
   begin
      if Ref.Index = Entity_Reference_Arrays.Index_Type'Last then
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
      Loc : File_Location;
      Iter : Entities_Tries.Iterator;
   begin
      Find_Next_Body (Entity, Location => Loc);
      if Loc = No_File_Location then
         Compute_Callers_And_Called (Entity.Declaration.File);
      else
         Compute_Callers_And_Called (Loc.File);
      end if;

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

end Entities.Queries;
