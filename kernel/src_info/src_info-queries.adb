-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Unchecked_Deallocation;
with Src_Info.ALI;              use Src_Info.ALI;
with Prj.Env;                   use Prj.Env;

package body Src_Info.Queries is

   procedure Free is new
     Unchecked_Deallocation (Dependency_Node, Dependency_List);

   function Search_Is_Completed
     (Status : Find_Decl_Or_Body_Query_Status)
      return Boolean;
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
      Status          : out Find_Decl_Or_Body_Query_Status);
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
      Status          : out Find_Decl_Or_Body_Query_Status);
   --  ??? Document...

   procedure Destroy (Dep : in out Dependency);
   --  Deallocates the memory associated with the given Dependency record.

   ----------------------
   -- Find_Object_File --
   ----------------------

   function Find_Object_File
     (Project_View    : Prj.Project_Id;
      Short_File_Name : String;
      Object_Path     : String)
      return String
   is
      Path : String_Access;
   begin
      --  First, try on the project object path
      Path := Locate_Regular_File
        (Short_File_Name, Ada_Objects_Path (Project_View).all);

      if Path /= null then
         declare
            Full_Path : constant String := Path.all;
         begin
            Free (Path);
            return Full_Path;
         end;
      end if;

      --  Fallback, try on the Object_Path (only if Use_Object_Path is set)
      if Object_Path /= "" then
         Path := Locate_Regular_File (Short_File_Name, Object_Path);
         if Path /= null then
            declare
               Full_Path : constant String := Path.all;
            begin
               Free (Path);
               return Full_Path;
            end;
         end if;
      end if;

      --  Object file not found anywhere, return the empty string
      return "";
   end Find_Object_File;

   -------------------------
   -- Search_Is_Completed --
   -------------------------

   function Search_Is_Completed
     (Status : Find_Decl_Or_Body_Query_Status)
      return Boolean is
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
      Status          : out Find_Decl_Or_Body_Query_Status)
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
      Status          : out Find_Decl_Or_Body_Query_Status)
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
      Status          : out Find_Decl_Or_Body_Query_Status)
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

   -------------------
   -- Get_Unit_Name --
   -------------------

   procedure Get_Unit_Name
     (Source_Info_List : in out Src_Info.LI_File_List;
      Project          : Prj.Project_Id;
      Source_Path      : String;
      Object_Path      : String;
      Dep              : in out Dependency;
      Unit_Name        : out String_Access) is
   begin
      if Dep.Unit_Name = null then
         --  To find the Unit_Name, we need to parse the corresponding ALI
         --  file.  There is no other choice, because the function that
         --  associates the filename to a given unit is not bijective.
         declare
            Short_ALI_Filename : constant String := ALI_Filename_From_Source
              (Dep.Filename.all, Project, Source_Path);
            --  ??? Seems unefficient to search the path again, since the
            --  ??? function above just did it.
            Path : constant String :=
              Find_Object_File (Project, Short_ALI_Filename, Object_Path);
            Unit : LI_File_Ptr;
            Success : Boolean;
         begin
            Parse_ALI_File
              (Path, Project, Source_Path,
               Source_Info_List, Unit, Success);

            if Success then
               Dep.Unit_Name := new String'(Unit.Spec_Info.Unit_Name.all);
            end if;
         end;
      end if;

      Unit_Name := Dep.Unit_Name;
   end Get_Unit_Name;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Dep : Dependency) return String is
   begin
      return Dep.Filename.all;
   end Get_Filename;

   ---------------------------
   -- Get_Depends_From_Spec --
   ---------------------------

   function Get_Depends_From_Spec (Dep : Dependency) return Boolean is
   begin
      return Dep.Depends_From_Spec;
   end Get_Depends_From_Spec;

   ---------------------------
   -- Get_Depends_From_Body --
   ---------------------------

   function Get_Depends_From_Body (Dep : Dependency) return Boolean is
   begin
      return Dep.Depends_From_Body;
   end Get_Depends_From_Body;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Dep : in out Dependency) is
   begin
      Free (Dep.Unit_Name);
      Free (Dep.Unit_Name);
   end Destroy;

   procedure Destroy (List : in out Dependency_List) is
      Current_Dep : Dependency_List renames List;
      Next_Dep    : Dependency_List;
   begin
      while Current_Dep /= null loop
         Next_Dep := Current_Dep.Next;
         Destroy (Current_Dep.Value);
         Free (Current_Dep);
         Current_Dep := Next_Dep;
      end loop;
   end Destroy;

   -----------------------
   -- Find_Dependencies --
   -----------------------

   procedure Find_Dependencies
     (Lib_Info     : LI_File_Ptr;
      Dependencies : out Dependency_List;
      Status       : out Dependencies_Query_Status)
   is
      Current_Dep : Dependency_File_Info_List;
   begin
      if Lib_Info = null then
         Dependencies := null;
         Status := Internal_Error;
         return;
      end if;

      Current_Dep  := Lib_Info.Dependencies_Info;
      Dependencies := null;

      while Current_Dep /= null loop
         declare
            FI : constant File_Info_Ptr :=
              Get_File_Info (Current_Dep.Value.File);
         begin
            if FI = null or else FI.Source_Filename = null then
               Destroy (Dependencies);
               Status := Internal_Error;
            end if;

            Dependencies := new Dependency_Node'
              (Value =>
                (Unit_Name => null,
                 Filename  => new String'(FI.Source_Filename.all),
                 Depends_From_Spec => Current_Dep.Value.Depends_From_Spec,
                 Depends_From_Body => Current_Dep.Value.Depends_From_Body),
               Next  => Dependencies);
            if FI.Unit_Name /= null then
               Dependencies.Value.Unit_Name := new String'(FI.Unit_Name.all);
            end if;
            Current_Dep := Current_Dep.Next;
         end;
      end loop;

      Status := Success;
   end Find_Dependencies;

end Src_Info.Queries;
