package body Src_Info.Queries is

   function Location_Matches
     (Location  : File_Location;
      File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Boolean;
   --  Return True if the given File_Location is pointing to the same
   --  Line, Column, and Filename. The filename comparison is done after
   --  comparing the position for better performance.

   procedure Find_Spec_Or_Body
     (Decl            : E_Declaration_Info_List;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive);
   --  ??? Document...

   ----------------------
   -- Location_Matches --
   ----------------------

   function Location_Matches
     (Location  : File_Location;
      File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Boolean is
   begin
      return Location.Line = Line
        and then Location.Column = Column
        and then Get_Source_Filename (Location.File) = File_Name;
   end Location_Matches;

   -----------------------
   -- Find_Spec_Or_Body --
   -----------------------

   procedure Find_Spec_Or_Body
     (Decl            : E_Declaration_Info_List;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive)
   is
      Current_Decl : E_Declaration_Info_List := Decl;
      Current_Ref  : E_Reference_List;
   begin
      Decl_Loop :
      while Current_Decl /= null loop

         --  Check the entity name to limit a bit the search in the
         --  Xref lists
         if Current_Decl.Value.Declaration.Name.all = Entity_Name then

            --  ??? For the moment, just implement the search declaration
            --  ??? of the job. The rest should be added fairly easily...
            Current_Ref := Current_Decl.Value.References;
            Ref_Loop :
            while Current_Ref /= null loop

               if Location_Matches
                    (Current_Ref.Value.Location, File_Name, Line, Column)
               then
                  File_Name_Found := new String'
                    (Get_Source_Filename
                      (Current_Decl.Value.Declaration.Location.File));
                  Start_Line := Current_Decl.Value.Declaration.Location.Line;
                  Start_Column :=
                    Current_Decl.Value.Declaration.Location.Column;
                  End_Line := Start_Line;
                  End_Column := Start_Column + Entity_Name'Length;
                  exit Decl_Loop;
               end if;

               Current_Ref := Current_Ref.Next;
            end loop Ref_Loop;
         end if;
         Current_Decl := Current_Decl.Next;
      end loop Decl_Loop;

      --  Set the value of File_Name_Found if no matching entity has been
      --  found.
      if Current_Decl = null then
         File_Name_Found := null;
      end if;
   end Find_Spec_Or_Body;

   ------------------------------
   -- Find_Declaration_Or_Body --
   ------------------------------

   procedure Find_Declaration_Or_Body
     (Lib_Info        : LI_File_Ptr;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive)
   is
      Current_Sep : File_Info_Ptr_List;
      Current_Dep : Dependency_File_Info_List;
   begin
      --  Assumption: if the Lib_Info structure is up-to-date, then the casing
      --  of the entity we are searching (here Entity_Name) is identical
      --  to the casing inside the Lib_Info, in which case we do not need
      --  to do case-insensitive string matching. This is important to avoid
      --  breaking the support for case-sensitive languages such as C for
      --  instance.

      --  Search a matching entity declaration in the Spec
      if Lib_Info.Spec_Info /= null
        and then Lib_Info.Spec_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.Spec_Info.Declarations,
            File_Name, Entity_Name, Line, Column,
            File_Name_Found, Start_Line, Start_Column, End_Line, End_Column);
         if File_Name_Found /= null then
            return;
         end if;
      end if;

      --  Search in the Body
      if Lib_Info.Body_Info /= null
        and then Lib_Info.Body_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.Body_Info.Declarations,
            File_Name, Entity_Name, Line, Column,
            File_Name_Found, Start_Line, Start_Column, End_Line, End_Column);
         if File_Name_Found /= null then
            return;
         end if;
      end if;

      --  Search in the separates
      Current_Sep := Lib_Info.Separate_Info;
      while Current_Sep /= null loop
         if Current_Sep.Value.Declarations /= null then
            Find_Spec_Or_Body
              (Current_Sep.Value.Declarations,
               File_Name, Entity_Name, Line, Column,
               File_Name_Found, Start_Line, Start_Column,
               End_Line, End_Column);
            if File_Name_Found /= null then
               return;
            end if;
         end if;
         Current_Sep := Current_Sep.Next;
      end loop;

      --  Search in the list of dependencies, if any
      Current_Dep := Lib_Info.Dependencies_Info;
      while Current_Dep /= null loop
         if Current_Dep.Value.Declarations /= null then
            Find_Spec_Or_Body
              (Current_Dep.Value.Declarations,
               File_Name, Entity_Name, Line, Column,
               File_Name_Found, Start_Line, Start_Column,
               End_Line, End_Column);
            if File_Name_Found /= null then
               return;
            end if;
         end if;
         Current_Dep := Current_Dep.Next;
      end loop;

      --  If we reach this point, that means we did not find the entity in
      --  our list of declarations.
      File_Name_Found := null;
      Start_Line := 1;
      Start_Column := 1;
      End_Line := 1;
      End_Column := 1;
   end Find_Declaration_Or_Body;

end Src_Info.Queries;
