with VFS;      use VFS;
with Traces;   use Traces;
with Projects; use Projects;

with Glib.Unicode; use Glib.Unicode;

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
      Ref  : Entity_Reference;
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
      Ref     : Entity_Reference;
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
      In_File               : Source_File := null)
   is
      Deps : Dependency_Iterator;
   begin
      Assert (Me, Entity /= null,
              "No Entity specified to Find_All_References");

      if In_File = null then
         Find_Ancestor_Dependencies
           (Deps,
            File                  => Get_File (Get_Declaration_Of (Entity)),
            File_Has_No_LI_Report => File_Has_No_LI_Report,
            Include_Self          => True);
      else
         Find_Ancestor_Dependencies
           (Deps,
            File                  => In_File,
            File_Has_No_LI_Report => File_Has_No_LI_Report,
            Include_Self          => True,
            Single_Source_File    => True);
      end if;

      Iter :=
        (Need_To_Update_Files => In_File = null,
         Index                => Entity_Reference_Arrays.First,
         Entity               => Entity,
         Deps                 => Deps,
         In_File              => In_File);
   end Find_All_References;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Entity_Reference_Iterator) return Boolean is
   begin
      return not Iter.Need_To_Update_Files
        and then Iter.Index > Last (Iter.Entity.References);
   end At_End;

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
      else
         loop
            Iter.Index := Iter.Index + 1;
            exit when Iter.Index > Last (Iter.Entity.References)
              or else Is_Real_Reference
                (Iter.Entity.References.Table (Iter.Index).Kind);
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Entity_Reference_Iterator) return File_Location is
   begin
      if Iter.Need_To_Update_Files then
         return No_File_Location;
      else
         return Iter.Entity.References.Table (Iter.Index).Location;
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

end Entities.Queries;
