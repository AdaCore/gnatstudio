package body Src_Info.Queries is

   function Search_Is_Completed (Status : Query_Status) return Boolean;
   --  Return False unless Status is equal to Entity_Not_Found. The idea
   --  implemented behind this function is to have a single function to decide,
   --  given an xref query, whether the results from a sub-query should be
   --  presented to the end-user, or if some more search, when possible, should
   --  be performed.

   function Location_Matches
     (Location  : File_Location;
      File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Boolean;
   --  Return True if the given File_Location is pointing to the same
   --  Line, Column, and Filename. The filename comparison is done after
   --  comparing the position for better performance.

   procedure Find_Next_Body_Ref
     (Decl            : E_Declaration_Info;
      Ref             : E_Reference_List := null;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Query_Status);
   --  Search for the body reference to the given declaration immediately
   --  following the given reference. If there is none then return the
   --  location of the declaration.
   --
   --  As a special case, if Ref is null, we search for the first body
   --  reference. If none, then return an xref failure (null File_Name_Found,
   --  etc).

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
      End_Column      : out Positive;
      Status          : out Query_Status);
   --  ??? Document...

   -------------------------
   -- Search_Is_Completed --
   -------------------------

   function Search_Is_Completed (Status : Query_Status) return Boolean is
   begin
      case Status is
         when Entity_Not_Found =>
            return False;
            --  We should continue the search if we can.
         when Internal_Error =>
            return True;
            --  ??? We don't want to ignore internal errors at the moment,
            --  ??? so we stop the query, and report and error to the end-user.
            --  ??? we may want to change this at release time if we want
            --  ??? to provide a better fault tolerant product (by changing
            --  ??? the value returned to False, the net effect is to ignore
            --  ??? the internal error while taking our chance by continuing
            --  ??? the search).
         when No_Body_Entity_Found |
              Success =>
            return True;
            --  Obviously, we have completed our query.
      end case;
   end Search_Is_Completed;

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

   ------------------------
   -- Find_Next_Body_Ref --
   ------------------------

   procedure Find_Next_Body_Ref
     (Decl            : E_Declaration_Info;
      Ref             : E_Reference_List := null;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Query_Status)
   is
      Last_Body_Ref : E_Reference_List;
      Current_Ref   : E_Reference_List;
      Entity_Name   : String renames Decl.Declaration.Name.all;
   begin
      --  Search the body reference immediately placed after the given
      --  Ref. Note that the references are stored in _reverse_ order...
      Current_Ref := Decl.References;
      while Current_Ref /= Ref and then Current_Ref /= null loop
         --  The test against null is just a guard against programing errors,
         --  just in case we are given a ref which is not part of the reference
         --  list of Decl...
         if Current_Ref.Value.Kind = Body_Entity then
            Last_Body_Ref := Current_Ref;
         end if;
         Current_Ref := Current_Ref.Next;
      end loop;

      if Last_Body_Ref = null and then Ref /= null then
         --  Case where we are on one of the body entities references and did
         --  not find a next body entity reference, so return the location
         --  of the declaration...
         File_Name_Found := new String'
           (Get_Source_Filename (Decl.Declaration.Location.File));
         Start_Line := Decl.Declaration.Location.Line;
         Start_Column := Decl.Declaration.Location.Column;
         End_Line := Start_Line;
         End_Column := Start_Column + Entity_Name'Length;
         Status := Success;

      elsif Last_Body_Ref /= null then
         --  Case where we found a body entity reference. Return its location.
         File_Name_Found := new String'
           (Get_Source_Filename (Last_Body_Ref.Value.Location.File));
         Start_Line   := Last_Body_Ref.Value.Location.Line;
         Start_Column := Last_Body_Ref.Value.Location.Column;
         End_Line     := Start_Line;
         End_Column   := Start_Column + Entity_Name'Length;
         Status := Success;

      else
         --  Case where we are located at the declaration itself, and could
         --  not find any body reference. Return no location.
         File_Name_Found := null;
         Status := No_Body_Entity_Found;
      end if;
   end Find_Next_Body_Ref;

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
      End_Column      : out Positive;
      Status          : out Query_Status)
   is
      Current_Decl : E_Declaration_Info_List := Decl;
      Current_Ref  : E_Reference_List;
   begin
      --  Initialize the value of the returned parameters
      File_Name_Found := null;
      Start_Line := 1;
      Start_Column := 1;
      End_Line := 1;
      End_Column := 1;
      Status := Entity_Not_Found;

      --  Search the entity in the list of declarations
      Decl_Loop :
      while Current_Decl /= null loop

         --  Check the entity name to limit a bit the search in the
         --  Xref lists
         if Current_Decl.Value.Declaration.Name.all = Entity_Name then

            --  Check if the location corresponds to the declaration,
            --  in which case we need to jump to the first body.
            if Location_Matches
              (Current_Decl.Value.Declaration.Location,
               File_Name, Line, Column)
            then
               Find_Next_Body_Ref
                 (Current_Decl.Value, null, File_Name_Found,
                  Start_Line, Start_Column, End_Line, End_Column, Status);
               exit Decl_Loop;
            end if;

            --  Search in the list of references.
            Current_Ref := Current_Decl.Value.References;
            Ref_Loop :
            while Current_Ref /= null loop

               if Location_Matches
                    (Current_Ref.Value.Location, File_Name, Line, Column)
               then
                  --  If this is a body reference, then we try to navigate
                  --  to the next body reference.
                  if Current_Ref.Value.Kind = Body_Entity then
                     Find_Next_Body_Ref
                       (Current_Decl.Value, Current_Ref, File_Name_Found,
                        Start_Line, Start_Column, End_Line, End_Column,
                        Status);
                  else
                     --  This is a non-body entity reference, so jump to the
                     --  declaration
                     File_Name_Found := new String'
                       (Get_Source_Filename
                         (Current_Decl.Value.Declaration.Location.File));
                     Start_Line :=
                       Current_Decl.Value.Declaration.Location.Line;
                     Start_Column :=
                       Current_Decl.Value.Declaration.Location.Column;
                     End_Line := Start_Line;
                     End_Column := Start_Column + Entity_Name'Length;
                     Status := Success;
                  end if;
                  exit Decl_Loop;
               end if;

               Current_Ref := Current_Ref.Next;
            end loop Ref_Loop;
         end if;
         Current_Decl := Current_Decl.Next;
      end loop Decl_Loop;

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
      End_Column      : out Positive;
      Status          : out Query_Status)
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
            File_Name_Found, Start_Line, Start_Column, End_Line, End_Column,
            Status);

         if Search_Is_Completed (Status) then
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
            File_Name_Found, Start_Line, Start_Column, End_Line, End_Column,
            Status);
         if Search_Is_Completed (Status) then
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
               End_Line, End_Column, Status);

            if Search_Is_Completed (Status) then
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
               End_Line, End_Column, Status);

            if Search_Is_Completed (Status) then
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
      Status := Entity_Not_Found;
   exception
      when others =>
         --  Trap all exceptions for better robustness, and report an
         --  internal error
         File_Name_Found := null;
         Start_Line := 1;
         Start_Column := 1;
         End_Line := 1;
         End_Column := 1;
         Status := Internal_Error;
   end Find_Declaration_Or_Body;

end Src_Info.Queries;
