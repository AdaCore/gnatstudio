-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                       Copyright (C) 2001-2003                     --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Unicode;            use Glib.Unicode;
with Unchecked_Deallocation;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with System.Assertions;       use System.Assertions;
with Traces;                  use Traces;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Language_Handlers.Glide; use Language_Handlers.Glide;

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with Projects;                use Projects;
with Projects.Registry;       use Projects.Registry;
with VFS;                     use VFS;

package body Src_Info.Queries is

   pragma Suppress (All_Checks);
   --  For efficiency

   Me : constant Debug_Handle := Create ("SRC_INFO");

   type E_Kind_To_Boolean_Map is array (E_Kinds) of Boolean;
   pragma Pack (E_Kind_To_Boolean_Map);

   Is_Scope_Entity : constant E_Kind_To_Boolean_Map :=
     (Overloaded_Entity     => False,
      Unresolved_Entity     => False,
      Access_Kind           => False,
      Array_Kind            => False,
      Boolean_Kind          => False,
      Class_Wide            => True,
      Class                 => True,
      Decimal_Fixed_Point   => False,
      Entry_Or_Entry_Family => False,
      Enumeration_Literal   => False,
      Enumeration_Kind      => False,
      Exception_Entity      => False,
      Floating_Point        => False,
      Function_Or_Operator  => True,
      Package_Kind          => True,
      Procedure_Kind        => True,
      Label_On_Block        => False,
      Label_On_Loop         => False,
      Label_On_Statement    => False,
      Modular_Integer       => False,
      Named_Number          => False,
      Ordinary_Fixed_Point  => False,
      Private_Type          => False,
      Protected_Kind        => False,
      Record_Kind           => True,
      Signed_Integer        => False,
      String_Kind           => False,
      Task_Kind             => True);
   --  This table should contain true if the corresponding element should
   --  appear when computing the full scope name for an entity (see
   --  Get_Full_Name).

   Is_Subprogram_Entity : constant E_Kind_To_Boolean_Map :=
     (Procedure_Kind        => True,
      Function_Or_Operator  => True,
      Entry_Or_Entry_Family => True,
      Task_Kind             => True,
      Package_Kind          => True,
      Overloaded_Entity     => True,
      --  ??? Should we check that at least one of the possible
      --  completions is a subprogram
      others                => False);
   --  This table should contain true if the corresponding element is
   --  considered as a subprogram (see Is_Subprogram)

   Is_Label_Entity : constant E_Kind_To_Boolean_Map :=
     (Label_On_Loop      => True,
      Label_On_Statement => True,
      Label_On_Block     => True,
      others             => False);
   --  This table should contain true if the corresponding element is a label
   --  (see Is_Label)

   use Name_Htable.String_Hash_Table;

   procedure Free is new
     Unchecked_Deallocation (Dependency_Node, Dependency_List);

   function Search_Is_Completed
     (Status : Find_Decl_Or_Body_Query_Status) return Boolean;
   --  Return False unless Status is equal to Entity_Not_Found. The idea
   --  implemented behind this function is to have a single function to decide,
   --  given an xref query, whether the results from a sub-query should be
   --  presented to the end-user, or if some more search, when possible, should
   --  be performed.

   function Location_Matches
     (Location  : File_Location;
      File_Name : Virtual_File;
      Line      : Positive;
      Column    : Positive) return Integer;
   --  Return 0 if the given File_Location is pointing to the same
   --  Line, Column, and Filename. The filename comparison is done after
   --  comparing the position for better performance.
   --  Otherwise, the distance from Location to (Line, Column) is returned. In
   --  the end, the closest match will be used.
   --  Integer'Last should be returned if the entity doesn't match (different
   --  file for instance).

   function Find_Next_Body_Ref
     (Decl : E_Declaration_Info;
      Ref  : E_Reference_List := null) return E_Reference_List;
   --  Search for the body reference to the given declaration immediately
   --  following the given reference. If there is none then return the
   --  location of the declaration.
   --
   --  As a special case, if Ref is null, we search for the first body
   --  reference. If none, then return an xref failure (null File_Name_Found,
   --  etc).

   procedure Find_Spec_Or_Body
     (Decl            : E_Declaration_Info_List;
      File_Name       : Virtual_File;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Check_References : Boolean := True;
      Proximity       : in out Integer;
      Entity_Decl     : in out E_Declaration_Info;
      Ref             : in out E_Reference_List;
      Status          : in out Find_Decl_Or_Body_Query_Status);
   --  Same as Internal_Find_Declaration_Or_Body, but for a specific
   --  declaration list.  Entity_Name must be all lower-cases if the language
   --  is case insensitive.
   --  If Check_References, (Line, Column) is only searched in the list of
   --  declarations, not in the list of references.
   --  If Entity_Name is the empty string, no matching is done on the name,
   --  only on the line and column
   --
   --  Status is not reset.
   --
   --  If no exact match is found for the entity, the closest match will be
   --  returned (e.g the LI file wasn't up-to-date), and Proximity will be set
   --  to the distance. Status will be set to Fuzzy_Match.

   procedure Trace_Dump
     (Handler : Debug_Handle;
      Scope : Scope_List;
      Prefix : String;
      Subprograms_Pkg_Only : Boolean;
      Display_Siblings : Boolean := True);
   --  Dump Scope to Handler, printing Prefix at the beginning of each line.
   --  If Display_Siblings is True, then the siblings of the node will be
   --  displayed. Otherwise, only the node itself and its children will be
   --  displayed.

   function Dump (L : Scope_List) return String;
   --  Return a string representation of L

   procedure Free (Scope : in out Scope_List);
   --  Free the memory occupied by Scope.

   function Find_Entity_Scope
     (Scope : Scope_List; Entity : Entity_Information) return Scope_Tree_Node;
   --  Find the declaration for Name in Scope.
   --  Name must be all lower-case if the language is case-insensitive

   procedure Find_Entity_References
     (Node     : Scope_List;
      Entity   : Entity_Information;
      Callback : Node_Callback);
   --  Find the references to Decl in Node

   function Is_Same_Entity
     (Decl : E_Declaration; Entity : Entity_Information) return Boolean;
   --  True if Entity1 and Entity2 represent the same entity. You can not use a
   --  direct equality test

   procedure Internal_Find_Declaration_Or_Body
     (Lib_Info      : LI_File_Ptr;
      File_Name     : Virtual_File;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Check_References : Boolean := True;
      Decl          : out E_Declaration_Info;
      Ref           : out E_Reference_List;
      Status        : out Find_Decl_Or_Body_Query_Status);
   --  Internal version of Find_Declaration and Find_Next_Body.
   --  Decl might point to an E_Declaration whose E_Kind is Overloaded_Entity.
   --  In that case, the caller should search in Lib_Info all the possible
   --  declarations for an entity with the same name, and the user will be
   --  asked to choose.
   --  Ref points to the E_Reference in the list of Decl that corresponds to
   --  Line, Column.
   --  If Check_References, (Line, Column) is only searched in the list of
   --  declarations, not in the list of references.
   --  If Entity_Name is the empty string, no matching is done on the name,
   --  only on the line and column

   function Get_Declarations_From_File
     (Lib_Info : LI_File_Ptr; File_Name : VFS.Virtual_File)
      return E_Declaration_Info_List;
   --  Return the list of declarations for a specific source file.

   function Find_Declaration
     (List : E_Declaration_Info_List;
      Entity : Entity_Information) return E_Declaration_Info_List;
   --  Return the specific declaration for Entity in List, or null if no such
   --  declaration was found.

   function Find_Declaration_In_LI
     (Lib_Info : LI_File_Ptr; Entity : Entity_Information)
      return E_Declaration_Info_List;
   --  Same as above, but search in all the source files associated with
   --  Lib_Info.

   function Find_Declaration_In_LI_Or_Dependencies
     (Lib_Info : LI_File_Ptr; Location : File_Location)
      return Entity_Information;
   --  Create an entity information by looking first in the direct source files
   --  of Lib_Info or in the list of their dependencies.
   --  In some cases, this should be used instead of Find_Declaration_In_LI,
   --  since the actual LI file where the entity is
   --  defined might not have been parsed yet, although we have reference to
   --  entities defined in it. For instance, the type of a parameter should be
   --  search through this subprogram.

   function Create
     (Lib_Info : LI_File_Ptr; Ref : E_Reference) return Entity_Information;
   --  Create an entity information from Ref.
   --  Lib_Info should be the file in which the declaration associated with Ref
   --  is found. Most of the time, this will be the LI file that used to
   --  compute Ref in the first place.
   --  The returned entity must be freed by the user.

   function Create (Decl : E_Declaration) return Entity_Information;
   --  Create an entity from a declaration.
   --  The returned entity must be freed by the user.

   function Process_Parents
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information;
      Kind     : Parent_Kind) return Entity_Information;
   --  Process the list of parent locations for a given entity. Only the first
   --  parent location whose kind matches is processed.

   procedure Internal_Next (Iter : in out Child_Type_Iterator);
   --  Internal version of Next, which doesn't move Iter.Current if it
   --  matches.

   function Next_Real_Reference
     (Ref : E_Reference_List) return E_Reference_List;
   --  Return the next element in Ref that is a real reference to the entity.
   --  This can include Ref itself

   -------------------------
   -- Search_Is_Completed --
   -------------------------

   function Search_Is_Completed
     (Status : Find_Decl_Or_Body_Query_Status)
      return Boolean is
   begin
      case Status is
         when Entity_Not_Found | Fuzzy_Match =>
            return False;
            --  We should continue the search if we can.

         when Internal_Error =>
            return True;
            --  ??? We don't want to ignore internal errors at the moment,
            --  so we stop the query, and report and error to the end-user.
            --  We may want to change this at release time if we want
            --  to provide a better fault tolerant product (by changing
            --  the value returned to False, the net effect is to ignore
            --  the internal error while taking our chance by continuing
            --  the search).

         when No_Body_Entity_Found | Success | Overloaded_Entity_Found =>
            return True;
            --  Obviously, we have completed our query.
      end case;
   end Search_Is_Completed;

   ----------------------
   -- Location_Matches --
   ----------------------

   function Location_Matches
     (Location  : File_Location;
      File_Name : Virtual_File;
      Line      : Positive;
      Column    : Positive)
      return Integer
   is
      Num_Columns_Per_Line : constant := 250;
      --  The number of columns in each line, when computing the proximity of a
      --  match. This is an approximate number, for efficiency. Big values mean
      --  that we give advantage to matches on the same line rather than on the
      --  same column.

   begin
      --  ??? Could be faster by avoiding the function returning unconstrained
      --  type.

      if Location.File = No_Source_File
        or else Get_Source_Filename (Location.File) /= File_Name
      then
         return Integer'Last;

      elsif Location.Line = Line
        and then (Location.Column = 0 or else Location.Column = Column)
      then
         return 0;

      else
         return (Line - Location.Line) * Num_Columns_Per_Line
           + (Column - Location.Column);
      end if;
   end Location_Matches;

   ------------------------
   -- Find_Next_Body_Ref --
   ------------------------

   function Find_Next_Body_Ref
     (Decl : E_Declaration_Info;
      Ref  : E_Reference_List := null) return E_Reference_List
   is
      Current_Ref : E_Reference_List;
      Result      : E_Reference_List;
   begin
      --  Search the body reference immediately placed after the given
      --  Ref. Note that the references are stored in _reverse_ order...
      Current_Ref := Decl.References;

      while Current_Ref /= null loop
         --  Completion of private types is also considered as a body, since
         --  they don't have an actual implementation anyway.
         if Current_Ref.Value.Kind = Body_Entity
           or else Current_Ref.Value.Kind =
              Completion_Of_Private_Or_Incomplete_Type
         then
            Result := Current_Ref;
         end if;

         exit when Result /= null and then Current_Ref = Ref;

         Current_Ref := Current_Ref.Next;
      end loop;

      return Result;
   end Find_Next_Body_Ref;

   -----------------------
   -- Find_Spec_Or_Body --
   -----------------------

   procedure Find_Spec_Or_Body
     (Decl            : E_Declaration_Info_List;
      File_Name       : Virtual_File;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Check_References : Boolean := True;
      Proximity       : in out Integer;
      Entity_Decl     : in out E_Declaration_Info;
      Ref             : in out E_Reference_List;
      Status          : in out Find_Decl_Or_Body_Query_Status)
   is
      Current_Decl : E_Declaration_Info_List := Decl;
      Current_Ref  : E_Reference_List;
      Prox         : Integer;
   begin
      --  Search the entity in the list of declarations

      Decl_Loop :
      while Current_Decl /= null loop
         --  Check the entity name to limit a bit the search in the
         --  Xref lists
         if Entity_Name = ""
           or else Current_Decl.Value.Declaration.Name.all = Entity_Name
         then
            --  Check if the location corresponds to the declaration,
            --  in which case we need to jump to the first body.

            Prox := Location_Matches
              (Current_Decl.Value.Declaration.Location,
               File_Name, Line, Column);

            if Prox = 0 then
               Entity_Decl := Current_Decl.Value;
               Ref         := null;
               Status      := Success;
               Proximity   := 0;
               return;

            elsif abs (Prox) < abs (Proximity) then
               Entity_Decl := Current_Decl.Value;
               Ref         := null;
               Status      := Fuzzy_Match;
               Proximity   := Prox;
            end if;

            --  Search in the list of references

            if Check_References then
               Current_Ref := Current_Decl.Value.References;

               Ref_Loop :
               while Current_Ref /= null loop
                  Prox := Location_Matches
                    (Current_Ref.Value.Location, File_Name, Line, Column);

                  if Prox = 0 then
                     Entity_Decl := Current_Decl.Value;
                     Ref         := Current_Ref;
                     Status      := Success;
                     return;

                  elsif abs (Prox) < abs (Proximity) then
                     Entity_Decl := Current_Decl.Value;
                     Ref         := Current_Ref;
                     Status      := Fuzzy_Match;
                     Proximity   := Prox;
                  end if;

                  Current_Ref := Current_Ref.Next;
               end loop Ref_Loop;
            end if;
         end if;
         Current_Decl := Current_Decl.Next;
      end loop Decl_Loop;

      --  Do not change Entity_Decl, Ref, Status, since these might already
      --  have values if we have searched the specs or bodies first (see
      --  Internal_Find_Declaration_Or_Body).
   end Find_Spec_Or_Body;

   ---------------------------------------
   -- Internal_Find_Declaration_Or_Body --
   ---------------------------------------

   procedure Internal_Find_Declaration_Or_Body
     (Lib_Info      : LI_File_Ptr;
      File_Name     : Virtual_File;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Check_References : Boolean := True;
      Decl          : out E_Declaration_Info;
      Ref           : out E_Reference_List;
      Status        : out Find_Decl_Or_Body_Query_Status)
   is
      Current_Sep : File_Info_Ptr_List;
      Current_Dep : Dependency_File_Info_List;
      E_Name      : GNAT.OS_Lib.String_Access;
      Proximity   : Integer := Integer'Last - 1;
      --  Not Integer'Last, so that if Location_Matches returns Integer'Last,
      --  this is not considered as a match.

   begin
      if Case_Insensitive_Identifiers (Lib_Info.LI.Handler) then
         E_Name := new String'(UTF8_Strdown (Entity_Name));
      else
         E_Name := new String'(Entity_Name);
      end if;

      Decl   := No_Declaration_Info;
      Ref    := null;
      Status := Entity_Not_Found;

      --  Search a matching entity declaration in the Spec
      if Lib_Info.LI.Spec_Info /= null
        and then Lib_Info.LI.Spec_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.LI.Spec_Info.Declarations,
            File_Name, E_Name.all, Line, Column, Check_References,
            Proximity, Decl, Ref, Status);
      end if;

      --  Search in the Body
      if not Search_Is_Completed (Status)
        and then Lib_Info.LI.Body_Info /= null
        and then Lib_Info.LI.Body_Info.Declarations /= null
      then
         Find_Spec_Or_Body
           (Lib_Info.LI.Body_Info.Declarations,
            File_Name, E_Name.all, Line, Column, Check_References,
            Proximity, Decl, Ref, Status);
      end if;

      --  Search in the separates
      if not Search_Is_Completed (Status) then
         Current_Sep := Lib_Info.LI.Separate_Info;
         while Current_Sep /= null loop
            if Current_Sep.Value.Declarations /= null then
               Find_Spec_Or_Body
                 (Current_Sep.Value.Declarations,
                  File_Name, E_Name.all, Line, Column, Check_References,
                  Proximity, Decl, Ref, Status);

               exit when Search_Is_Completed (Status);
            end if;

            Current_Sep := Current_Sep.Next;
         end loop;
      end if;

      --  Search in the list of dependencies, if any
      if not Search_Is_Completed (Status) then
         Current_Dep := Lib_Info.LI.Dependencies_Info;
         while Current_Dep /= null loop
            if Current_Dep.Value.Declarations /= null then
               Find_Spec_Or_Body
                 (Current_Dep.Value.Declarations,
                  File_Name, E_Name.all, Line, Column, Check_References,
                  Proximity, Decl, Ref, Status);
               if Search_Is_Completed (Status) then
                  exit;
               end if;
            end if;

            Current_Dep := Current_Dep.Next;
         end loop;
      end if;

      Free (E_Name);
   end Internal_Find_Declaration_Or_Body;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Lib_Info      : LI_File_Ptr;
      File_Name     : VFS.Virtual_File;
      Entity_Name   : String;
      Line          : Positive;
      Column        : Positive;
      Entity        : out Entity_Information;
      Status        : out Find_Decl_Or_Body_Query_Status)
   is
      Ref         : E_Reference_List;
      Decl        : E_Declaration_Info;
   begin
      Internal_Find_Declaration_Or_Body
        (Lib_Info         => Lib_Info,
         File_Name        => File_Name,
         Entity_Name      => Entity_Name,
         Line             => Line,
         Column           => Column,
         Check_References => True,
         Decl             => Decl,
         Ref              => Ref,
         Status           => Status);

      if Status = Success or else Status = Fuzzy_Match then
         if Decl.Declaration.Kind.Kind /= Overloaded_Entity then
            Entity := Get_Entity (Decl.Declaration);
         else
            Entity := No_Entity_Information;
            Status := Overloaded_Entity_Found;
         end if;
      else
         Entity := No_Entity_Information;
         Trace (Me, "Couldn't find a valid xref for " & Entity_Name
                & " line=" & Line'Img & " Column=" & Column'Img
                & " file=" & Full_Name (File_Name).all
                & " Status=" & Status'Img);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         --  Trap all exceptions for better robustness, and report an
         --  internal error
         Entity := No_Entity_Information;
         Status := Internal_Error;
   end Find_Declaration;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Lib_Info               : LI_File_Ptr;
      File_Name              : VFS.Virtual_File;
      Entity_Name            : String;
      Line                   : Positive;
      Column                 : Positive;
      Handler                : access LI_Handler_Record'Class;
      Source_Info_List       : LI_File_List;
      Project                : Project_Type;
      Location               : out File_Location;
      Status                 : out Find_Decl_Or_Body_Query_Status)
   is
      Body_LI : LI_File_Ptr;
      Ref     : E_Reference_List;
      Decl    : E_Declaration_Info;
      Entity  : Entity_Information;
   begin
      --  First, find the declaration of the entity, since we need to parse the
      --  LI file that really defines the entity, or we won't find the body.

      Internal_Find_Declaration_Or_Body
        (Lib_Info         => Lib_Info,
         File_Name        => File_Name,
         Entity_Name      => Entity_Name,
         Line             => Line,
         Column           => Column,
         Check_References => True,
         Decl             => Decl,
         Ref              => Ref,
         Status           => Status);

      if (Status = Success or else Status = Fuzzy_Match)
        and then Decl.Declaration.Kind.Kind = Overloaded_Entity
      then
         Status := Overloaded_Entity_Found;
         Location := Null_File_Location;
         return;
      end if;

      if Status = Success or else Status = Fuzzy_Match then

         --  If we need to parse the LI File that contains the body
         --  information. Note that we have to change the line,column
         --  information, or the entity will not match in the LI file.

         if Decl.Declaration.Location.File.LI /= Lib_Info then
            Entity := Get_Entity (Decl.Declaration);
            Create_Or_Complete_LI
              (Handler                => Handler,
               File                   => Body_LI,
               Source_Filename        => Get_Declaration_File_Of (Entity),
               List                   => Source_Info_List,
               Project                => Project);

            Internal_Find_Declaration_Or_Body
              (Lib_Info         => Body_LI,
               File_Name        => Get_Declaration_File_Of (Entity),
               Entity_Name      => Entity_Name,
               Line             => Get_Declaration_Line_Of (Entity),
               Column           => Get_Declaration_Column_Of (Entity),
               Check_References => True,
               Decl             => Decl,
               Ref              => Ref,
               Status           => Status);
            Destroy (Entity);
         end if;
      end if;

      --  We have now found a reference. Now we must decide whether we want to
      --  get the reference to the declaration, one of the bodies,...
      --  Check if the location corresponds to the declaration,
      --  in which case we need to jump to the first body.
      --  Otherwise, if this is a body reference, then we try to navigate
      --  to the next body reference.

      if Status = Success or else Status = Fuzzy_Match then
         Ref := Find_Next_Body_Ref (Decl, Ref);

         if Ref /= null then
            Location := Ref.Value.Location;
         else
            --  Return the location of the reference, since there is no body.
            --  It happens with Ada for instance when a subprogram has no
            --  separate body (BA28-004)
            Location := Decl.Declaration.Location;
         end if;
      else
         Trace (Me, "Couldn't find a valid xref for " & Entity_Name
                & " line=" & Line'Img & " Column=" & Column'Img
                & " file=" & Base_Name (File_Name));
      end if;

   exception
      when E : others =>
         --  Trap all exceptions for better robustness, and report an
         --  internal error.

         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Status   := Internal_Error;
         Location := Null_File_Location;
   end Find_Next_Body;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (List : in out Dependency_List) is
      Current_Dep : Dependency_List renames List;
      Next_Dep    : Dependency_List;
   begin
      while Current_Dep /= null loop
         Next_Dep := Current_Dep.Next;
         Free (Current_Dep);
         Current_Dep := Next_Dep;
      end loop;
   end Destroy;

   -----------------------
   -- Find_Dependencies --
   -----------------------

   procedure Find_Dependencies
     (Lib_Info        : LI_File_Ptr;
      Source_Filename : Virtual_File;
      Dependencies    : out Dependency_List;
      Status          : out Dependencies_Query_Status)
   is
      Current_Dep : Dependency_File_Info_List;
      FI          : File_Info_Ptr;
      List        : File_Info_Ptr_List;
      Part        : Unit_Part;
      B, S        : Boolean;
      Base        : constant String := Base_Name (Source_Filename);
   begin
      if Lib_Info = null then
         Dependencies := null;
         Status := Internal_Error;
         Trace (Me, "No Lib_Info specified for Find_Dependencies");
         return;
      end if;

      Part := Get_Unit_Part (Lib_Info, Source_Filename);

      Current_Dep  := Lib_Info.LI.Dependencies_Info;
      Dependencies := null;

      --  The body and separates always depends on the spec
      --  The body always depends on the separates
      --  The separates also depend on the other separates

      if Part /= Unit_Spec then
         if Lib_Info.LI.Spec_Info /= null then
            FI := Lib_Info.LI.Spec_Info;
            Dependencies := new Dependency_Node'
              (Value =>
                 (File =>
                    (File_Name => Create
                       (FI.Source_Filename.all, Lib_Info.LI.Project,
                        Use_Object_Path => False),
                     LI_Name   => Lib_Info.LI.LI_Filename),
                  Dep  => (Depends_From_Spec => False,
                           Depends_From_Body => True)),
               Next  => Dependencies);
         end if;

         List := Lib_Info.LI.Separate_Info;
         while List /= null loop
            if List.Value.Source_Filename.all /= Base then
               Dependencies := new Dependency_Node'
                 (Value =>
                    (File =>
                       (File_Name => Create
                          (List.Value.Source_Filename.all,
                           Lib_Info.LI.Project, Use_Object_Path => False),
                        LI_Name   => Lib_Info.LI.LI_Filename),
                     Dep  => (Depends_From_Spec => False,
                              Depends_From_Body => True)),
                  Next  => Dependencies);
            end if;
            List := List.Next;
         end loop;

         if Part = Unit_Separate
           and then Lib_Info.LI.Body_Info /= null
         then
            FI := Lib_Info.LI.Body_Info;
            Dependencies := new Dependency_Node'
              (Value =>
                 (File =>
                    (File_Name => Create
                       (FI.Source_Filename.all,
                        Lib_Info.LI.Project, Use_Object_Path => False),
                     LI_Name   => Lib_Info.LI.LI_Filename),
                  Dep  => (Depends_From_Spec => False,
                           Depends_From_Body => True)),
               Next  => Dependencies);
         end if;
      end if;

      while Current_Dep /= null loop
         B := Get_Depends_From_Body (Current_Dep.Value.Dep_Info);
         S := Get_Depends_From_Spec (Current_Dep.Value.Dep_Info);

         if (Part = Unit_Spec and then S)
           or else (Part = Unit_Body and then B)
         then
            FI := Get_File_Info (Current_Dep.Value.File);
            if FI = null or else FI.Source_Filename = null then
               Destroy (Dependencies);
               Dependencies := null;
               Status := Internal_Error;
               Trace (Me, "Couldn't find the File_Info_Ptr for "
                        & Full_Name (Get_LI_Filename (Lib_Info)).all);
               return;
            end if;

            Dependencies := new Dependency_Node'
              (Value =>
                 (File =>
                    (File_Name => Create
                       (FI.Source_Filename.all,
                        Current_Dep.Value.File.LI.LI.Project,
                        Use_Object_Path => False),
                     LI_Name   => Current_Dep.Value.File.LI.LI.LI_Filename),
                  Dep  => Current_Dep.Value.Dep_Info),
               Next  => Dependencies);
         end if;

         Current_Dep := Current_Dep.Next;
      end loop;

      Status := Success;
   end Find_Dependencies;

   ----------------------
   -- File_Information --
   ----------------------

   function File_Information (Dep : Dependency) return Internal_File is
   begin
      return Dep.File;
   end File_Information;

   ----------------------------
   -- Dependency_Information --
   ----------------------------

   function Dependency_Information (Dep : Dependency) return Dependency_Info is
   begin
      return Dep.Dep;
   end Dependency_Information;

   ----------
   -- Dump --
   ----------

   function Dump (L : Scope_List) return String is
   begin
      case L.Typ is
         when Declaration =>
            if L.Decl.End_Of_Scope /= No_Reference then
               return "Decl:""" & L.Decl.Name.all
                 & L.Start_Of_Scope.Line'Img
                 & L.Start_Of_Scope.Column'Img
                 & " ->"
                 & L.Decl.End_Of_Scope.Location.Line'Img
                 & L.Decl.End_Of_Scope.Location.Column'Img
                 & ", decl at"
                 & L.Decl.Location.Line'Img
                 & L.Decl.Location.Column'Img
                 & """";
            else
               return "Decl:""" & L.Decl.Name.all
                 & L.Start_Of_Scope.Line'Img
                 & L.Start_Of_Scope.Column'Img
                 & ", decl at"
                 & L.Decl.Location.Line'Img
                 & L.Decl.Location.Column'Img
                 & """";
            end if;
         when Reference =>
            return "Ref:""" & L.Decl.Name.all
              & L.Ref.Location.Line'Img
              & L.Ref.Location.Column'Img & """";
      end case;
   end Dump;

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump
     (Handler : Debug_Handle;
      Scope : Scope_List;
      Prefix : String;
      Subprograms_Pkg_Only : Boolean;
      Display_Siblings : Boolean := True)
   is
      L : Scope_List := Scope;
   begin
      while L /= null loop
         if not Subprograms_Pkg_Only
           or else Is_Subprogram (Scope_Tree_Node (L))
           or else L.Decl.Kind.Kind = Package_Kind
         then
            Trace (Handler, Prefix & Dump (L));
            if L.Typ = Declaration then
               Trace_Dump
                 (Handler, L.Contents, Prefix & "  ", Subprograms_Pkg_Only);
            end if;
         end if;

         if Display_Siblings then
            L := L.Sibling;
         else
            L := null;
         end if;
      end loop;
   end Trace_Dump;

   ----------------
   -- Trace_Dump --
   ----------------

   procedure Trace_Dump
     (Handler              : Traces.Debug_Handle;
      Tree                 : Scope_Tree;
      Node                 : Scope_Tree_Node := Null_Scope_Tree_Node;
      Subprograms_Pkg_Only : Boolean := True) is
   begin
      if Tree /= Null_Scope_Tree then
         Trace (Handler, "Scope tree for " & Base_Name (Tree.LI_Filename));
         if Node = Null_Scope_Tree_Node then
            Trace (Handler, "Part= BODY");
            Trace_Dump (Handler, Tree.Body_Tree, "",
                        Subprograms_Pkg_Only);
            Trace (Handler, "Part= SPEC");
            Trace_Dump (Handler, Tree.Spec_Tree, "",
                        Subprograms_Pkg_Only);
            for P in Tree.Separate_Trees'Range loop
               Trace (Handler, "Part=" & P'Img);
               Trace_Dump (Handler, Tree.Separate_Trees (P), "",
                           Subprograms_Pkg_Only);
            end loop;
         else
            Trace_Dump
              (Handler, Scope_List (Node), "",
               Subprograms_Pkg_Only, Display_Siblings => False);
         end if;
      else
         Trace (Handler, "Null scope tree");
      end if;
   end Trace_Dump;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Node : Scope_Tree_Node) return Boolean is
   begin
      return Is_Subprogram_Entity (Node.Decl.Kind.Kind)
        and then (Node.Typ = Declaration
                  or else not Is_End_Reference (Node.Ref.Kind));
   end Is_Subprogram;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram (Entity : Entity_Information) return Boolean is
   begin
      return Is_Subprogram_Entity (Entity.Kind.Kind);
   end Is_Subprogram;

   --------------
   -- Is_Label --
   --------------

   function Is_Label (Node : Scope_Tree_Node) return Boolean is
   begin
      return Is_Label_Entity (Node.Decl.Kind.Kind);
   end Is_Label;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Node : Scope_Tree_Node) return E_Kind is
   begin
      return Node.Decl.Kind;
   end Get_Kind;

   ----------
   -- Free --
   ----------

   procedure Free (Scope : in out Scope_List) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Scope_Node, Scope_List);
      L : Scope_List;
   begin
      while Scope /= null loop
         L     := Scope;
         Scope := Scope.Sibling;

         if L.Typ = Declaration then
            Free (L.Contents);
         end if;

         Unchecked_Free (L);
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Tree : in out Scope_Tree) is
   begin
      Free (Tree.Body_Tree);
      Free (Tree.Spec_Tree);

      if Tree.Separate_Trees /= null then
         for P in Tree.Separate_Trees'Range loop
            Free (Tree.Separate_Trees (P));
         end loop;

         Free (Tree.Separate_Trees);
      end if;
   end Free;

   -----------------
   -- Create_Tree --
   -----------------

   function Create_Tree
     (Lib_Info : LI_File_Ptr; Declarations_Only : Boolean := False)
      return Scope_Tree
   is

      procedure Add_Declarations
        (Decl : E_Declaration_Info_List; T : in out Scope_Tree);
      --  Add the declarations from Decl into L.

      procedure Add_Single_Entity
        (Decl   : in out Scope_List;
         Node   : in out Scope_List;
         Parent : Scope_List);
      procedure Add_Single_Entity
        (Decl : in out Scope_List; T : in out Scope_Tree);
      --  Add a single reference in the tree (either starting at the top-level
      --  of the tree if Node is null, or at Node otherwise).

      procedure Add_References
        (Decl       : E_Declaration_Info;
         T          : in out Scope_Tree;
         Decl_Start : File_Location);
      --  Add all the references to the entity declared in Decl.
      --  Decl_Start is the starting location of the scope for the entity.

      function "<" (L1, L2 : File_Location) return Boolean;
      pragma Inline ("<");
      --  True if L1 is before L2 (line and column)

      function In_Range (Decl : Scope_List; Loc : Scope_List) return Integer;
      --  Whether Loc is in the range of Decl.
      --  -1 is returned if Decl is before Loc, 0 if within, 1 if after, or
      --  2 if Loc is contained in the scope of Decl.
      --  Both Decl and Loc must reference the same source file, or no
      --  comparison can be done.

      function Is_Contained (Loc, Decl : Scope_List) return Boolean;
      --  True if Loc is contained in the scope of Decl.
      --  This is a simplified (and more efficient) version of In_Range.

      procedure Add_In_List
        (L        : in out Scope_List;
         Previous : in out Scope_List;
         Parent   : Scope_List;
         New_Item : Scope_List);
      --  Add New_Item in the list L, after Previous (or at the beginning if
      --  Previous is null.

      procedure Compute_Scope (L : in out Scope_List; Ref : E_Reference_List);
      --  Compute the beginning and end of scope for the declaration in L.

      -------
      -- < --
      -------

      function "<" (L1, L2 : File_Location) return Boolean is
      begin
         return L1.Line < L2.Line
           or else (L1.Line = L2.Line and then L1.Column < L2.Column);
      end "<";

      -------------------
      -- Compute_Scope --
      -------------------

      procedure Compute_Scope
        (L : in out Scope_List; Ref : E_Reference_List)
      is
         R : E_Reference_List := Ref;
      begin
         L.Start_Of_Scope := L.Decl.Location;

         while R /= null loop
            --  There might be multiple bodies for a given entity (for
            --  instance, in Ada for a rename-as-body, both the "rename" and
            --  the real body are referenced as bodies.
            --  We need to find the proper one

            if Is_Start_Reference (R.Value.Kind)
              and then (L.Decl.End_Of_Scope = No_Reference
                        or else L.Decl.End_Of_Scope.Location.File =
                          R.Value.Location.File)
            then
               L.Start_Of_Scope := R.Value.Location;
               return;
            end if;

            R := R.Next;
         end loop;
      end Compute_Scope;

      --------------
      -- In_Range --
      --------------

      function In_Range (Decl : Scope_List; Loc : Scope_List) return Integer is
      begin
         case Decl.Typ is
            when Declaration =>
               case Loc.Typ is
                  when Declaration =>
                     if Decl.Decl.End_Of_Scope /= No_Reference then
                        if Decl.Decl.End_Of_Scope.Location <
                          Loc.Start_Of_Scope
                        then
                           return -1;
                        elsif Decl.Start_Of_Scope < Loc.Start_Of_Scope then
                           --  Entities are necessarily comprised within one
                           --  another

                           return 2;

                        elsif Loc.Decl.End_Of_Scope /= No_Reference
                          and then Decl.Start_Of_Scope <
                            Loc.Decl.End_Of_Scope.Location
                        then
                           return 0;
                        else
                           return 1;
                        end if;

                     elsif Loc.Decl.End_Of_Scope /= No_Reference then
                        if Decl.Start_Of_Scope < Loc.Start_Of_Scope then
                           return -1;
                        elsif Decl.Start_Of_Scope <
                          Loc.Decl.End_Of_Scope.Location
                        then
                           return 0;
                        else
                           return 1;
                        end if;

                     elsif Decl.Start_Of_Scope < Loc.Start_Of_Scope then
                        return -1;
                     else
                        return 1;
                     end if;

                  when Reference =>
                     if Decl.Decl.End_Of_Scope /= No_Reference then
                        if Decl.Decl.End_Of_Scope.Location <
                          Loc.Ref.Location
                        then
                           return -1;
                        elsif Decl.Start_Of_Scope < Loc.Ref.Location then
                           return 2;
                        else
                           return 1;
                        end if;

                     elsif Decl.Start_Of_Scope < Loc.Ref.Location then
                        return -1;
                     else
                        return 1;
                     end if;
               end case;

            when Reference =>
               case Loc.Typ is
                  when Declaration =>
                     if Decl.Ref.Location < Loc.Start_Of_Scope then
                        return -1;
                     elsif Loc.Decl.End_Of_Scope /= No_Reference
                       and then Decl.Ref.Location <
                         Loc.Decl.End_Of_Scope.Location
                     then
                        return 0;
                     else
                        return 1;
                     end if;

                  when Reference =>
                     if Decl.Ref.Location < Loc.Ref.Location then
                        return -1;
                     else
                        return 1;
                     end if;
               end case;
         end case;
      end In_Range;

      ------------------
      -- Is_Contained --
      ------------------

      function Is_Contained (Loc, Decl : Scope_List) return Boolean is
      begin
         if Decl.Typ = Declaration then
            case Loc.Typ is
               when Declaration =>
                  return Decl.Decl.End_Of_Scope /= No_Reference
                    and then not (Decl.Decl.End_Of_Scope.Location <
                                    Loc.Start_Of_Scope)
                    and then Decl.Start_Of_Scope < Loc.Start_Of_Scope;

               when Reference =>
                  return Decl.Decl.End_Of_Scope /= No_Reference
                    and then not (Decl.Decl.End_Of_Scope.Location <
                                    Loc.Ref.Location)
                    and then Decl.Start_Of_Scope < Loc.Ref.Location;
            end case;
         end if;

         return False;
      end Is_Contained;

      -----------------
      -- Add_In_List --
      -----------------

      procedure Add_In_List
        (L        : in out Scope_List;
         Previous : in out Scope_List;
         Parent   : Scope_List;
         New_Item : Scope_List) is
      begin
         Assert (Me, New_Item.Sibling = null,
                 "Inserting item with existing sibling");
         New_Item.Parent := Parent;

         if Previous = null then
            New_Item.Sibling := L;
            L := New_Item;
         else
            New_Item.Sibling := Previous.Sibling;
            Previous.Sibling := New_Item;
         end if;
      end Add_In_List;

      -----------------------
      -- Add_Single_Entity --
      -----------------------

      procedure Add_Single_Entity
        (Decl   : in out Scope_List;
         Node   : in out Scope_List;
         Parent : Scope_List)
      is
         List     : Scope_List := Node;
         Previous : Scope_List := null;
         Save     : Scope_List;

      begin
         while List /= null loop
            case In_Range (Decl, List) is
               when -1 =>
                  Add_In_List (Node, Previous, Parent, Decl);
                  return;

               when 0 =>
                  Add_Single_Entity (Decl, List.Contents, List);
                  return;

               when 1 =>
                  null;

               when 2 =>
                  Add_In_List (Node, Previous, Parent, Decl);

                  loop
                     Save := List.Sibling;
                     List.Sibling := null;
                     Add_Single_Entity (List, Decl.Contents, Decl);
                     Decl.Sibling := Save;
                     List := Save;

                     exit when List = null
                       or else not Is_Contained (Loc => List, Decl => Decl);
                  end loop;

                  return;

               when others =>
                  null;
            end case;

            Previous := List;
            List := List.Sibling;
         end loop;

         Add_In_List (Node, Previous, Parent, Decl);
      end Add_Single_Entity;

      procedure Add_Single_Entity
        (Decl   : in out Scope_List;
         T      : in out Scope_Tree)
      is
         P               : Unit_Part;
         File_List       : File_Info_Ptr_List;
         Num             : Positive := 1;
         Source_Filename : GNAT.OS_Lib.String_Access;

      begin
         if Decl.Typ = Declaration then
            if Decl.Start_Of_Scope.File.LI /= Lib_Info then
               Free (Decl);
               return;
            end if;

            P := Decl.Start_Of_Scope.File.Part;
            Source_Filename := Decl.Start_Of_Scope.File.Source_Filename;

         else
            if Decl.Ref.Location.File.LI /= Lib_Info then
               Free (Decl);
               return;
            end if;

            P := Decl.Ref.Location.File.Part;
            Source_Filename := GNAT.OS_Lib.String_Access
              (Decl.Ref.Location.File.Source_Filename);
         end if;

         case P is
            when Unit_Body =>
               Add_Single_Entity (Decl, T.Body_Tree, null);

            when Unit_Spec =>
               Add_Single_Entity (Decl, T.Spec_Tree, null);

            when Unit_Separate =>
               File_List := Lib_Info.LI.Separate_Info;

               while File_List /= null loop
                  exit when File_List.Value.Source_Filename.all =
                    Source_Filename.all;

                  Num := Num + 1;
                  File_List := File_List.Next;
               end loop;

               if Num <= T.Separate_Trees'Last then
                  Add_Single_Entity (Decl, T.Separate_Trees (Num), null);
               end if;
         end case;
      end Add_Single_Entity;

      --------------------
      -- Add_References --
      --------------------

      procedure Add_References
        (Decl : E_Declaration_Info;
         T : in out Scope_Tree;
         Decl_Start : File_Location)
      is
         R : E_Reference_List := Decl.References;
         New_Item : Scope_List;
      begin
         while R /= null loop
            --  Do not add labels to the scope tree, since these only bring
            --  syntactic information, and do not impact the code.

            if R.Value.Kind /= Label
              and then R.Value.Location /= Decl_Start
              and then R.Value.Location /=
                Decl.Declaration.End_Of_Scope.Location
            then
               New_Item := new Scope_Node'
                 (Typ         => Reference,
                  Sibling     => null,
                  Parent      => null,
                  Decl        => Decl.Declaration'Unrestricted_Access,
                  Ref         => R.Value'Unrestricted_Access);
               Add_Single_Entity (New_Item, T);
            end if;

            R := R.Next;
         end loop;
      end Add_References;

      ----------------------
      -- Add_Declarations --
      ----------------------

      procedure Add_Declarations
        (Decl : E_Declaration_Info_List; T : in out Scope_Tree)
      is
         List     : E_Declaration_Info_List := Decl;
         New_Item : Scope_List;

      begin
         while List /= null loop
            New_Item := new Scope_Node'
              (Typ            => Declaration,
               Decl           => List.Value.Declaration'Unrestricted_Access,
               Contents       => null,
               Start_Of_Scope => Null_File_Location,
               Parent         => null,
               Sibling        => null);

            --  Add the body into the tree

            Compute_Scope (New_Item, List.Value.References);

            declare
               Start_Of_Scope : constant File_Location :=
                 New_Item.Start_Of_Scope;
            begin
               --  Try to insert the declaration in the tree. Note that this
               --  might actually delete New_Item if the declaration doesn't
               --  fit in the tree.

               Add_Single_Entity (New_Item, T);

               if not Declarations_Only then
                  Add_References (List.Value, T, Start_Of_Scope);
               end if;

               --  If the entity has an End_Of_Spec reference, and is a package
               --  (the one in which we are), we need to add it to the spec
               --  tree as well.
               --  It might already be there however, if the package has no
               --  body

               if List.Value.Declaration.End_Of_Scope /= No_Reference
                 and then List.Value.Declaration.Kind.Kind = Package_Kind
                 and then Start_Of_Scope.File.Part /= Unit_Spec
               then
                  New_Item := new Scope_Node'
                    (Typ         => Declaration,
                     Decl        => List.Value.Declaration'Unrestricted_Access,
                     Contents    => null,
                     Start_Of_Scope => List.Value.Declaration.Location,
                     Parent      => null,
                     Sibling     => null);
                  Add_Single_Entity (New_Item, T);
               end if;
            end;

            List := List.Next;
         end loop;
      end Add_Declarations;

      T             : Scope_Tree;
      File_List     : File_Info_Ptr_List;
      Dep           : Dependency_File_Info_List;
      Num_Separates : Natural := 0;

   begin
      Assert (Me, Lib_Info /= null, "Create_Tree: LI file is null");
      Assert (Me, Lib_Info.LI.Parsed, "Create_Tree: LI file wasn't parsed");

      File_List := Lib_Info.LI.Separate_Info;

      while File_List /= null loop
         Num_Separates := Num_Separates + 1;
         File_List := File_List.Next;
      end loop;

      T := (Lib_Info    => Lib_Info,
            LI_Filename => Lib_Info.LI.LI_Filename,
            Time_Stamp  => 0,
            Body_Tree   => null,
            Spec_Tree   => null,
            Separate_Trees => new Scope_List_Array (1 .. Num_Separates));

      if Lib_Info.LI.Spec_Info /= null then
         Add_Declarations (Lib_Info.LI.Spec_Info.Declarations, T);
      end if;

      if Lib_Info.LI.Body_Info /= null then
         Add_Declarations (Lib_Info.LI.Body_Info.Declarations, T);
      end if;

      File_List := Lib_Info.LI.Separate_Info;

      while File_List /= null loop
         Add_Declarations (File_List.Value.Declarations, T);
         File_List := File_List.Next;
      end loop;

      Dep := Lib_Info.LI.Dependencies_Info;

      while Dep /= null loop
         Add_Declarations (Dep.Value.Declarations, T);
         Dep := Dep.Next;
      end loop;

      return T;

   exception
      when E : Constraint_Error | Assert_Failure =>
         Assert (Me, False, "Unexpected exception in Create_Tree: "
                & Exception_Information (E));
         Free (T);
         return Null_Scope_Tree;
   end Create_Tree;

   -----------------------
   -- Find_Entity_Scope --
   -----------------------

   function Find_Entity_Scope
     (Scope : Scope_List; Entity : Entity_Information) return Scope_Tree_Node
   is
      L : Scope_List := Scope;
      Result : Scope_Tree_Node;
   begin
      while L /= null loop
         if L.Typ = Declaration then
            if Is_Same_Entity (L.Decl.all, Entity) then
               return Scope_Tree_Node (L);
            end if;

            Result := Find_Entity_Scope (L.Contents, Entity);
            if Result /= null then
               return Result;
            end if;
         end if;

         L := L.Sibling;
      end loop;
      return null;
   end Find_Entity_Scope;

   -----------------------
   -- Find_Entity_Scope --
   -----------------------

   function Find_Entity_Scope
     (Tree : Scope_Tree; Entity : Entity_Information)
      return Scope_Tree_Node
   is
      Result : Scope_Tree_Node;
   begin
      Result := Find_Entity_Scope (Tree.Body_Tree, Entity);
      if Result = null then
         Result := Find_Entity_Scope (Tree.Spec_Tree, Entity);

         if Result = null then
            for P in Tree.Separate_Trees'Range loop
               Result := Find_Entity_Scope
                 (Tree.Separate_Trees (P), Entity);
               exit when Result /= null;
            end loop;
         end if;
      end if;

      return Result;
   end Find_Entity_Scope;

   ----------------------------
   -- Find_Entity_References --
   ----------------------------

   procedure Find_Entity_References
     (Node     : Scope_List;
      Entity : Entity_Information;
      Callback : Node_Callback)
   is
      L : Scope_List := Node;
   begin
      while L /= null loop
         case L.Typ is
            when Declaration =>
               if Is_Renaming_Of (Entity, Scope_Tree_Node (L)) then
                  Callback (Scope_Tree_Node (L), Is_Renaming => True);
               end if;
               Find_Entity_References (L.Contents, Entity, Callback);

            when Reference =>
               if Is_Same_Entity (L.Decl.all, Entity)
                 and then Is_Real_Reference (L.Ref.Kind)
               then
                  Callback (Scope_Tree_Node (L), Is_Renaming => False);
               end if;
         end case;
         L := L.Sibling;
      end loop;
   end Find_Entity_References;

   ----------------------------
   -- Find_Entity_References --
   ----------------------------

   procedure Find_Entity_References
     (Tree : Scope_Tree;
      Entity : Entity_Information;
      Callback : Node_Callback) is
   begin
      Find_Entity_References (Tree.Body_Tree, Entity, Callback);
      Find_Entity_References (Tree.Spec_Tree, Entity, Callback);
      for P in Tree.Separate_Trees'Range loop
         Find_Entity_References (Tree.Separate_Trees (P), Entity, Callback);
      end loop;
   end Find_Entity_References;

   -----------
   -- Start --
   -----------

   function Start (Node : Scope_Tree_Node) return Scope_Tree_Node_Iterator is
   begin
      if Node = Null_Scope_Tree_Node then
         return null;
      else
         return Scope_Tree_Node_Iterator (Node.Contents);
      end if;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Scope_Tree_Node_Iterator) is
   begin
      if Iter /= null then
         Iter := Scope_Tree_Node_Iterator (Iter.Sibling);
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Scope_Tree_Node_Iterator) return Scope_Tree_Node is
   begin
      return Scope_Tree_Node (Iter);
   end Get;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Node : Scope_Tree_Node) return Scope_Tree_Node is
   begin
      return Scope_Tree_Node (Node.Parent);
   end Get_Parent;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Entity : Entity_Information) return String is
   begin
      if Entity.Name = null then
         return "";
      end if;
      return Entity.Name.all;
   end Get_Name;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Node : Scope_Tree_Node) return Entity_Information is
   begin
      return Get_Entity (Node.Decl.all);
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity (Decl : E_Declaration) return Entity_Information is
   begin
      if Decl.Location /= Null_File_Location then
         return Create
           (Name   => Decl.Name.all,
            Line   => Decl.Location.Line,
            Column => Decl.Location.Column,
            Scope  => Decl.Scope,
            Kind   => Decl.Kind,
            File   => Get_Source_Filename (Decl.Location.File));
      else
         return Create
           (Name   => Decl.Name.all,
            Line   => Decl.Location.Line,
            Column => Decl.Location.Column,
            Scope  => Decl.Scope,
            Kind   => Decl.Kind,
            File   => VFS.No_File);
      end if;
   end Get_Entity;

   ----------
   -- Copy --
   ----------

   function Copy (Entity : Entity_Information) return Entity_Information is
   begin
      if Entity = No_Entity_Information then
         return No_Entity_Information;
      else
         return Create
           (Name   => Entity.Name.all,
            Line   => Entity.Decl_Line,
            Column => Entity.Decl_Column,
            File   => Entity.Decl_File,
            Scope  => Entity.Scope,
            Kind   => Entity.Kind);
      end if;
   end Copy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Entity : in out Entity_Information) is
   begin
      Free (Entity.Name);
      Entity := No_Entity_Information;
   end Destroy;

   -----------------------------
   -- Get_Declaration_Line_Of --
   -----------------------------

   function Get_Declaration_Line_Of (Entity : Entity_Information)
      return Positive is
   begin
      return Entity.Decl_Line;
   end Get_Declaration_Line_Of;

   -------------------------------
   -- Get_Declaration_Column_Of --
   -------------------------------

   function Get_Declaration_Column_Of (Entity : Entity_Information)
      return Natural is
   begin
      return Entity.Decl_Column;
   end Get_Declaration_Column_Of;

   -----------------------------
   -- Get_Declaration_File_Of --
   -----------------------------

   function Get_Declaration_File_Of
     (Entity : Entity_Information) return VFS.Virtual_File is
   begin
      if Is_Predefined_Entity (Entity) then
         Trace (Me, "Get_Declaration_File_Of: Predefined_Entity");
         return VFS.No_File;
      else
         return Entity.Decl_File;
      end if;
   end Get_Declaration_File_Of;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind (Entity : Entity_Information) return E_Kind is
   begin
      return Entity.Kind;
   end Get_Kind;

   ---------------
   -- Get_Scope --
   ---------------

   function Get_Scope (Entity : Entity_Information) return E_Scope is
   begin
      return Entity.Scope;
   end Get_Scope;

   --------------------
   -- Is_Same_Entity --
   --------------------

   function Is_Same_Entity
     (Decl : E_Declaration; Entity : Entity_Information) return Boolean is
   begin
      return Decl.Location.Line         = Entity.Decl_Line
        and then Decl.Location.Column   = Entity.Decl_Column
        and then Decl.Name.all          = Entity.Name.all
        and then
          ((Decl.Location = Predefined_Entity_Location
            and then Entity.Decl_File = VFS.No_File)
           or else Get_Source_Filename (Decl.Location.File) =
             Entity.Decl_File);
   end Is_Same_Entity;

   -------------------
   -- Get_Full_Name --
   -------------------

   function Get_Full_Name
     (Entity    : Entity_Information;
      Decl_File : LI_File_Ptr;
      Separator : String := ".";
      Scope     : Scope_Tree := Null_Scope_Tree) return String
   is
      Tree            : Scope_Tree;
      Node, Tmp, Tmp2 : Scope_Tree_Node;
      Full_Name       : Unbounded_String;
      T, T2           : Entity_Information;

   begin
      if Decl_File = null then
         return Get_Name (Entity);
      end if;

      if Scope = Null_Scope_Tree then
         Tree := Create_Tree (Decl_File);
      else
         Tree := Scope;
      end if;

      if Tree = Null_Scope_Tree then
         return Get_Name (Entity);
      end if;

      Node := Find_Entity_Scope (Tree, Entity);

      if Node = Null_Scope_Tree_Node then
         if Scope = Null_Scope_Tree then
            Free (Tree);
         end if;

         return Get_Name (Entity);
      end if;

      Tmp := Node;
      while Tmp /= Null_Scope_Tree_Node loop
         --  Only include the names that are actually part of a scope (the
         --  entity itself is automatically part of the scope, whatever its
         --  kind).

         if Tmp = Node or else Is_Scope_Entity (Get_Kind (Tmp).Kind) then
            T := Get_Entity (Tmp);

            if Length (Full_Name) /= 0 then
               Full_Name := Get_Name (T) & Separator & Full_Name;
            else
               Full_Name := To_Unbounded_String (Get_Name (T));
            end if;

            Tmp2 := Get_Parent (Tmp);

            --  If there is no more parent node in the tree, we still need to
            --  look for possible parent packages
            if Tmp2 = Null_Scope_Tree_Node then
               loop
                  T2 := Get_Parent_Package (Decl_File, T);
                  exit when T2 = No_Entity_Information;

                  Full_Name := Get_Name (T2) & Separator & Full_Name;

                  Destroy (T);
                  T := T2;
               end loop;
            end if;

            Destroy (T);
            Tmp := Tmp2;

         else
            Tmp := Get_Parent (Tmp);
         end if;
      end loop;

      if Scope = Null_Scope_Tree then
         Free (Tree);
      end if;

      return To_String (Full_Name);
   end Get_Full_Name;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out Entity_Reference_Iterator) is
   begin
      Destroy (Iterator.Entity);
      Destroy (Iterator.Decl_Iter);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out Entity_Reference_Iterator_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Entity_Reference_Iterator, Entity_Reference_Iterator_Access);
   begin
      Destroy (Iterator.all);
      Unchecked_Free (Iterator);
   end Destroy;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Entity_Reference_Iterator) return E_Reference is
   begin
      if Iterator.References = null then
         return No_Reference;
      else
         return Iterator.References.Value;
      end if;
   end Get;

   ------------
   -- Get_LI --
   ------------

   function Get_LI (Iterator : Entity_Reference_Iterator) return LI_File_Ptr is
   begin
      return Get (Iterator.Decl_Iter);
   end Get_LI;

   ----------------------
   -- Find_Declaration --
   ----------------------

   function Find_Declaration
     (List   : E_Declaration_Info_List;
      Entity : Entity_Information) return E_Declaration_Info_List is
   begin
      --  Check the declaration in the list. All the declarations have the same
      --  source file, so we can simply check the first one to save some time.
      if List = null
        or else Get_Source_Filename (List.Value.Declaration.Location.File) /=
        Get_Declaration_File_Of (Entity)
      then
         return null;

      else
         return Get_Declaration
           (List, Entity.Decl_Line, Entity.Decl_Column, Get_Name (Entity));
      end if;
   end Find_Declaration;

   ----------------------------
   -- Find_Declaration_In_LI --
   ----------------------------

   function Find_Declaration_In_LI
     (Lib_Info : LI_File_Ptr; Entity : Entity_Information)
      return E_Declaration_Info_List
   is
      D : constant E_Declaration_Info_List := Get_Declarations_From_File
        (Lib_Info, Get_Declaration_File_Of (Entity));
   begin
      if D = null then
         return null;
      else
         return Find_Declaration (D, Entity);
      end if;
   end Find_Declaration_In_LI;

   --------------------------------------------
   -- Find_Declaration_In_LI_Or_Dependencies --
   --------------------------------------------

   function Find_Declaration_In_LI_Or_Dependencies
     (Lib_Info : LI_File_Ptr; Location : File_Location)
      return Entity_Information
   is
      Dep : Dependency_File_Info_List := Lib_Info.LI.Dependencies_Info;
      Typ : E_Declaration_Info_List;
   begin
      Typ := Get_Declarations_From_File (Lib_Info, Get_File (Location));
      if Typ /= null then
         Typ := Get_Declaration (Typ,
                                 Location.Line,
                                 Location.Column,
                                 "");
         if Typ = null then
            return No_Entity_Information;
         else
            return Create (Typ.Value.Declaration);
         end if;
      end if;

      while Dep /= null loop
         if Dep.Value.File = Location.File then
            Typ := Get_Declaration
              (List        => Dep.Value.Declarations,
               Decl_Line   => Location.Line,
               Decl_Column => Location.Column,
               Entity_Name => "");
            if Typ /= null then
               return Create (Typ.Value.Declaration);
            else
               return Create
                 (File   => Get_File (Location),
                  Line   => Location.Line,
                  Column => Location.Column,
                  Name   => "???",
                  Scope  => Global_Scope,
                  Kind   => (Procedure_Kind, False, False, False));
            end if;
         end if;
         Dep := Dep.Next;
      end loop;

      return No_Entity_Information;
   end Find_Declaration_In_LI_Or_Dependencies;

   -------------------------
   -- Next_Real_Reference --
   -------------------------

   function Next_Real_Reference
     (Ref : E_Reference_List)
      return E_Reference_List
   is
      R : E_Reference_List := Ref;
   begin
      while R /= null loop
         if Is_Real_Reference (R.Value.Kind) then
            return R;
         end if;

         R := R.Next;
      end loop;

      return null;
   end Next_Real_Reference;

   ----------
   -- Next --
   ----------

   procedure Next
     (Lang_Handler : Language_Handlers.Language_Handler;
      Iterator     : in out Entity_Reference_Iterator;
      List         : LI_File_List)
   is
      function Check_Declarations (Declarations : E_Declaration_Info_List)
         return E_Reference_List;
      --  Return the list of references to the entity in Declarations, or null
      --  if there is none.

      function Check_LI (LI : LI_File_Ptr) return E_Reference_List;
      --  Set the iterator to examine the declarations in LI

      function Check_Decl_File return E_Reference_List;
      --  Check the next declaration list in Iterator.LI.

      ------------------------
      -- Check_Declarations --
      ------------------------

      function Check_Declarations (Declarations : E_Declaration_Info_List)
         return E_Reference_List
      is
         D : E_Declaration_Info_List;
      begin
         if Declarations = null then
            return null;
         else
            D := Find_Declaration (Declarations, Iterator.Entity);

            if D /= null then
               return Next_Real_Reference (D.Value.References);
            end if;
         end if;

         return null;
      end Check_Declarations;

      --------------
      -- Check_LI --
      --------------

      function Check_LI (LI : LI_File_Ptr) return E_Reference_List is
      begin
         --  If this is the LI file for Decl, we need to parse the
         --  body and spec infos. Otherwise, only the dependencies
         if LI = Iterator.Decl_Iter.Decl_LI then
            Iterator.Part := None;
            return Check_Decl_File;

         --  Otherwise, check all the dependencies to see if we
         --  have the entity.
         else
            return Check_Declarations
              (Iterator.Decl_Iter.Current_Decl.Value.Declarations);
         end if;
      end Check_LI;

      ---------------------
      -- Check_Decl_File --
      ---------------------

      function Check_Decl_File return E_Reference_List is
         Ref : E_Reference_List;
         LI  : constant LI_File_Ptr := Get (Iterator.Decl_Iter);
      begin
         if Iterator.Part = None then
            Iterator.Part := Unit_Spec;
            if LI.LI.Spec_Info /= null then
               Ref := Check_Declarations (LI.LI.Spec_Info.Declarations);
               if Ref /= null then
                  return Ref;
               end if;
            end if;
         end if;

         --  Were we checking the declarations from the spec ?
         if Iterator.Part = Unit_Spec then
            Iterator.Part := Unit_Body;
            if LI.LI.Body_Info /= null then
               Ref := Check_Declarations (LI.LI.Body_Info.Declarations);
               if Ref /= null then
                  return Ref;
               end if;
            end if;
         end if;

         --  Were we checking the declarations from the body ?
         if Iterator.Part = Unit_Body then
            Iterator.Part := Unit_Separate;
            Iterator.Current_Separate := LI.LI.Separate_Info;
         end if;

         --  Are we currently checking the separates
         if Iterator.Part = Unit_Separate then
            while Iterator.Current_Separate /= null loop
               Ref := Check_Declarations
                 (Iterator.Current_Separate.Value.Declarations);
               Iterator.Current_Separate := Iterator.Current_Separate.Next;

               if Ref /= null then
                  return Ref;
               end if;
            end loop;
            Iterator.Part := Unit_Dependency;
            Iterator.Current_Dependency := LI.LI.Dependencies_Info;
         end if;

         if Iterator.Part = Unit_Dependency then
            while Iterator.Current_Dependency /= null loop
               Ref := Check_Declarations
                 (Iterator.Current_Dependency.Value.Declarations);
               Iterator.Current_Dependency := Iterator.Current_Dependency.Next;

               if Ref /= null then
                  return Ref;
               end if;
            end loop;
         end if;

         return null;
      end Check_Decl_File;

      First_Next : constant Boolean := Iterator.References = null;
      --  True if this is the first time we call Next

   begin
      --  If necessary, force skipping to the next LI file
      if Iterator.LI_Once then
         Iterator.References := null;
         Iterator.Part := Unit_Dependency;
         Iterator.Current_Dependency := null;
      end if;

      --  If there are still some references
      if Iterator.References /= null then
         Iterator.References := Next_Real_Reference (Iterator.References.Next);
      end if;

      if Iterator.References = null then
         --  Were we processing the LI file for the source file that contains
         --  the initial declaration ?

         if Get (Iterator.Decl_Iter) /= null
           and then Get (Iterator.Decl_Iter) = Iterator.Decl_Iter.Decl_LI
         then
            --  ??? Iterator.Part is uninitialized here
            Iterator.References := Check_Decl_File;
            if Iterator.References /= null then
               return;
            end if;
         end if;

         if not First_Next then
            Next (Lang_Handler, Iterator.Decl_Iter, List);
         end if;

         while Get (Iterator.Decl_Iter) /= null loop
            Iterator.References := Check_LI (Get (Iterator.Decl_Iter));
            exit when Iterator.References /= null;

            Next (Lang_Handler, Iterator.Decl_Iter, List);
            Iterator.Part := None;
         end loop;
      end if;
   end Next;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   function Get_Total_Progress
     (Iterator : Entity_Reference_Iterator) return Natural is
   begin
      return Iterator.Decl_Iter.Total_Progress;
   end Get_Total_Progress;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   function Get_Current_Progress
     (Iterator : Entity_Reference_Iterator) return Natural is
   begin
      return Iterator.Decl_Iter.Current_Progress;
   end Get_Current_Progress;

   -------------------------
   -- Find_All_References --
   -------------------------
   --  Algorithm:

   --  For efficiency, we only look in the direct sources of all the projects
   --  that import, even indirectly the project that contains the declaration
   --  source file. No files in other projects can reference that entity, since
   --  it is not visible.
   --  We also only look in the files that are in the same language.

   procedure Find_All_References
     (Root_Project           : Project_Type;
      Lang_Handler           : Language_Handlers.Language_Handler;
      Entity                 : Entity_Information;
      List                   : LI_File_List;
      Iterator               : out Entity_Reference_Iterator;
      Project                : Project_Type := No_Project;
      LI_Once                : Boolean := False;
      In_File                : VFS.Virtual_File := VFS.No_File) is
   begin
      Iterator.Entity := Copy (Entity);
      Iterator.LI_Once := LI_Once;

      if In_File = VFS.No_File then
         Find_Ancestor_Dependencies
           (Root_Project, Lang_Handler,
            Get_Declaration_File_Of (Entity), List,
            Iterator.Decl_Iter,
            Project                => Project,
            Include_Self           => True,
            Indirect_Imports       => True,
            LI_Once                => True);
      else
         Find_Ancestor_Dependencies
           (Root_Project, Lang_Handler, In_File, List,
            Iterator.Decl_Iter,
            Project                => Project,
            Include_Self           => True,
            LI_Once                => True,
            Indirect_Imports       => True,
            Single_Source_File     => True);
      end if;

      Next (Lang_Handler, Iterator, List);
   end Find_All_References;

   ---------------------------------
   -- Find_All_References_In_File --
   ---------------------------------

   function Find_All_References_In_File
     (Lib_Info    : LI_File_Ptr;
      Source_File : VFS.Virtual_File) return Local_Entities_Iterator
   is
      Iter : Local_Entities_Iterator;
   begin
      Iter.Part := Unit_Spec;
      Iter.File := Source_File;
      Iter.New_Decl := False;
      Iter.LI := Lib_Info;
      Next (Iter);
      return Iter;
   end Find_All_References_In_File;

   ---------
   -- Get --
   ---------

   function Get
     (Iterator : Local_Entities_Iterator) return Entity_Information is
   begin
      if Iterator.Current_Decl = null then
         return No_Entity_Information;
      else
         return Get_Entity (Iterator.Current_Decl.Value.Declaration);
      end if;
   end  Get;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Local_Entities_Iterator) return E_Reference is
   begin
      if Iterator.New_Decl or else Iterator.Current_Decl = null then
         return No_Reference;
      elsif Iterator.Reference = null then
         return (Location => Iterator.Current_Decl.Value.Declaration.Location,
                 Kind     => Reference);
      else
         return Iterator.Reference.Value;
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Local_Entities_Iterator) is
      procedure Next_File;
      --  Move to the next file to analyze

      function Next_Reference (Ref : E_Reference_List) return E_Reference_List;
      --  Move to the next reference

      procedure Next_Decl;
      --  Move to the next declaration

      function Check_File (File : File_Info_Ptr) return Boolean;
      --  Return whether File matches the iterator

      ----------------
      -- Check_File --
      ----------------

      function Check_File (File : File_Info_Ptr) return Boolean is
      begin
         if File /= null
           and then File.Declarations /= null
         then
            Iterator.Current_Decl := File.Declarations;
            return True;
         end if;

         return False;
      end Check_File;

      ---------------
      -- Next_File --
      ---------------

      procedure Next_File is
      begin
         if Iterator.Part = Unit_Spec then
            Iterator.Part := Unit_Body;

            if Check_File (Iterator.LI.LI.Spec_Info) then
               return;
            end if;
         end if;

         if Iterator.Part = Unit_Body then
            Iterator.Part := Unit_Separate;
            if Check_File (Iterator.LI.LI.Body_Info) then
               return;
            end if;
         end if;

         if Iterator.Part = Unit_Separate then
            if Iterator.Current_Separate = null then
               Iterator.Current_Separate := Iterator.LI.LI.Separate_Info;
            else
               Iterator.Current_Separate := Iterator.Current_Separate.Next;
            end if;

            while Iterator.Current_Separate /= null loop
               if Check_File (Iterator.Current_Separate.Value) then
                  return;
               end if;

               Iterator.Current_Separate := Iterator.Current_Separate.Next;
            end loop;

            Iterator.Part := Unit_Dependency;
         end if;

         if Iterator.Part = Unit_Dependency then
            if Iterator.Current_Dep = null then
               Iterator.Current_Dep := Iterator.LI.LI.Dependencies_Info;
            else
               Iterator.Current_Dep := Iterator.Current_Dep.Next;
            end if;

            while Iterator.Current_Dep /= null loop
               if Get_Source_Filename (Iterator.Current_Dep.Value.File) =
                   Iterator.File
                 and then Iterator.Current_Dep.Value.Declarations /= null
               then
                  Iterator.Current_Decl :=
                    Iterator.Current_Dep.Value.Declarations;
                  return;
               end if;

               Iterator.Current_Dep := Iterator.Current_Dep.Next;
            end loop;
         end if;

         Iterator.Part := None;
         Iterator.Current_Decl := null;
      end Next_File;

      ---------------
      -- Next_Decl --
      ---------------

      procedure Next_Decl is
      begin
         Iterator.Current_Decl := Iterator.Current_Decl.Next;
         Iterator.Reference    := null;
         Iterator.New_Decl     := True;

         if Iterator.Current_Decl = null then
            Next_File;
         end if;
      end Next_Decl;

      --------------------
      -- Next_Reference --
      --------------------

      function Next_Reference
        (Ref : E_Reference_List) return E_Reference_List
      is
         E : E_Reference_List := Ref;
      begin
         loop
            exit when E = null
              or else Get_File (Get_Location (E.Value)) = Iterator.File;
            E := Next_Real_Reference (E.Next);
         end loop;

         if E = null then
            Next_Decl;
         end if;

         return E;
      end Next_Reference;

   begin
      --  No more entities to display in the current ilfe ?

      if Iterator.Current_Decl = null then
         Next_File;

      elsif Iterator.Reference /= null then
         Iterator.Reference := Next_Reference (Iterator.Reference.Next);

      else
         if Iterator.New_Decl then
            Iterator.New_Decl := False;

            if Get_Source_Filename
              (Iterator.Current_Decl.Value.Declaration.Location.File) =
              Iterator.File
            then
               --  We'll return a reference to the declaration
               return;
            end if;
         end if;

         --  Move to first reference
         Iterator.Reference :=
           Next_Reference (Iterator.Current_Decl.Value.References);

         if Iterator.Reference = null then
            Next_Decl;
         end if;
      end if;
   end Next;

   ----------
   -- Next --
   ----------

   procedure Next
     (Lang_Handler : Language_Handlers.Language_Handler;
      Iterator : in out Dependency_Iterator;
      List     : LI_File_List)
   is
      function Check_File return Dependency_File_Info_List;
      --  Check the current file in the iterator, and return the list of
      --  references that matches the declaration (or null if there is none).

      function Check_LI (LI : LI_File_Ptr) return Dependency_File_Info_List;
      --  Set the iterator to examine the declarations in LI

      procedure Next_Separate;
      --  Select the next separate in the current LI, which should be examined.

      ----------------
      -- Check_File --
      ----------------

      function Check_File return Dependency_File_Info_List is
         LI      : LI_File_Ptr;
         Handler : LI_Handler;
         Sep_List : File_Info_Ptr_List;

      begin
         if not Get
           (Iterator.Examined,
            Base_Name (Iterator.Source_Files (Iterator.Current_File)))
         then
            Handler := Get_LI_Handler_From_File
              (Glide_Language_Handler (Lang_Handler),
               Iterator.Source_Files (Iterator.Current_File));

            --  For now, we do not have inter-language cross-references, so we
            --  do nothing if the file doesn't have the same language.

            if Handler = Iterator.Decl_LI.LI.Handler then
               LI := Locate_From_Source
                 (List, Iterator.Source_Files (Iterator.Current_File));

               --  Do nothing if the file is not the same language
               if LI = null
                 or else LI.LI.Handler = Handler
               then
                  Create_Or_Complete_LI
                    (Handler                => Handler,
                     File                   => LI,
                     Source_Filename        =>
                       Iterator.Source_Files (Iterator.Current_File),
                     List                   => List,
                     Project                => Current (Iterator.Importing));

                  Assert
                    (Me, LI = null or else LI.LI.Parsed,
                     "Unparsed LI returned for file "
                     & Base_Name
                       (Iterator.Source_Files (Iterator.Current_File)));
               end if;

               if LI /= null then
                  --  Memorize the list of files that have been examined, to
                  --  avoid parsing again the same LI file
                  if LI.LI.Spec_Info /= null then
                     Set
                       (Iterator.Examined,
                        LI.LI.Spec_Info.Source_Filename.all,
                        True);
                  end if;

                  if LI.LI.Body_Info /= null then
                     Set
                       (Iterator.Examined,
                        LI.LI.Body_Info.Source_Filename.all,
                        True);
                  end if;

                  Sep_List := LI.LI.Separate_Info;
                  while Sep_List /= null loop
                     if Sep_List.Value /= null then
                        Set
                          (Iterator.Examined,
                           Sep_List.Value.Source_Filename.all,
                           True);
                     end if;
                     Sep_List := Sep_List.Next;
                  end loop;

                  return Check_LI (LI);
               end if;
            end if;
         end if;

         Iterator.LI := null;
         return null;
      end Check_File;

      -------------------
      -- Next_Separate --
      -------------------

      procedure Next_Separate is
      begin
         if Iterator.Current_Separate = null then
            Iterator.Current_Separate := Iterator.LI.LI.Separate_Info;
         else
            Iterator.Current_Separate := Iterator.Current_Separate.Next;
         end if;

         if Iterator.Current_Separate /= null
           and then Iterator.Current_Separate.Value.Source_Filename.all =
             Base_Name (Iterator.Source_Filename)
         then
            Iterator.Current_Separate := Iterator.Current_Separate.Next;
         end if;
      end Next_Separate;

      --------------
      -- Check_LI --
      --------------

      function Check_LI (LI : LI_File_Ptr) return Dependency_File_Info_List is
         Dep : Dependency_File_Info_List := LI.LI.Dependencies_Info;
      begin
         Iterator.LI := LI;

         if LI = Iterator.Decl_LI  then
            --  If we shouldn't include any file from the declaration LI

            Iterator.Current_Separate := null;

            --  The only case where we might happen to return the spec is when
            --  the source file itself was the spec (the spec never depends on
            --  the body or the separates).

            if Iterator.Include_Self
              and then Iterator.LI.LI.Spec_Info /= null
              and then Iterator.LI.LI.Spec_Info.Source_Filename.all =
                Base_Name (Iterator.Source_Filename)
            then
               Trace (Me, "Check_LI: spec matches");
               Iterator.Current_Part := Unit_Spec;
               return null;
            end if;

            Iterator.Current_Part := Unit_Body;

            if Iterator.LI.LI.Body_Info /= null
              and then (Iterator.Include_Self
                        or else Iterator.LI.LI.Body_Info.Source_Filename.all /=
                          Base_Name (Iterator.Source_Filename))
            then
               Trace (Me, "Check_LI: body matches "
                      & Iterator.LI.LI.Body_Info.Source_Filename.all);
               return null;
            end if;

            Next_Separate;

            if Iterator.Current_Separate = null then
               --  No dependency for this LI file
               Trace (Me, "Check_LI: no separate or body matches");
               Iterator.LI := null;
            else
               Trace (Me, "Check_LI: found separate: "
                      & Iterator.Current_Separate.Value.Source_Filename.all);
            end if;

            return null;
         end if;

         while Dep /= No_Dependencies loop
            exit when
              (Iterator.Indirect_Imports
               or else Dep.Value.Dep_Info.Depends_From_Spec
               or else Dep.Value.Dep_Info.Depends_From_Body)
              and then Dep.Value.File.LI = Iterator.Decl_LI
              and then Get_File_Info (Dep.Value.File).Source_Filename.all =
                Base_Name (Iterator.Source_Filename);
            Dep := Dep.Next;
         end loop;

         if Dep /= null then
            --  Order is important here. Check the specs first
            if Dep.Value.Dep_Info.Depends_From_Spec then
               Iterator.Current_Part := Unit_Spec;
               --  body will be checked on next call to Next
            else
               Iterator.Current_Part := Unit_Body;
            end if;
         end if;

         return Dep;
      end Check_LI;

   begin
      --  Check the separate units if necessary. Whenever we are returning a
      --  body, we are returning the separates as well.

      if not Iterator.LI_Once then
         if Iterator.Current_Part = Unit_Body then
            Next_Separate;
            if Iterator.Current_Separate /= null then
               Trace (Me, "Next: returning next separate "
                      & Iterator.Current_Separate.Value.Source_Filename.all);
               return;
            end if;

            --  If we were depending on the specs, it is possible that we also
            --  depend on the body.

         elsif (Iterator.Current_Decl /= null and then
                Iterator.Current_Decl.Value.Dep_Info.Depends_From_Body)
           or else Iterator.LI = Iterator.Decl_LI
         then
            Iterator.Current_Separate := null;
            Iterator.Current_Part := Unit_Body;
            Trace (Me, "Next: now analyzing body");
            return;
         end if;
      end if;

      loop
         --  Move to the next file in the current project
         loop
            Iterator.Current_File := Iterator.Current_File + 1;
            exit when Iterator.Current_File > Iterator.Source_Files'Last;
            Iterator.Current_Progress := Iterator.Current_Progress + 1;
            Iterator.Current_Decl := Check_File;
            if Iterator.Current_Decl /= null
              or else Iterator.LI = Iterator.Decl_LI
            then
               return;
            end if;
         end loop;

         --  Move to the next project

         Unchecked_Free (Iterator.Source_Files);
         Reset (Iterator.Examined);

         Next (Iterator.Importing);
         if Current (Iterator.Importing) = No_Project then
            Iterator.Current_Decl := null;
            Iterator.LI := No_LI_File;
            return;
         end if;

         Iterator.Source_Files := Get_Source_Files
           (Current (Iterator.Importing),
            Recursive => False,
            Full_Path => True);
         Iterator.Current_File := Iterator.Source_Files'First - 1;
      end loop;

   exception
      when E : others =>
         Trace (Me, "Next: unexpected exception " & Exception_Message (E));
   end Next;

   --------------------------------
   -- Find_Ancestor_Dependencies --
   --------------------------------

   procedure Find_Ancestor_Dependencies
     (Root_Project    : Project_Type;
      Lang_Handler : Language_Handlers.Language_Handler;
      Source_Filename : VFS.Virtual_File;
      List            : LI_File_List;
      Iterator        : out Dependency_Iterator;
      Project         : Project_Type := No_Project;
      Include_Self    : Boolean := False;
      LI_Once         : Boolean := False;
      Indirect_Imports : Boolean := False;
      Single_Source_File : Boolean := False)
   is
      Decl_Project : Project_Type := Project;
      Iterator_Decl_Project : Project_Type := Project;
      Handler      : LI_Handler;
      Tmp          : Projects.Imported_Project_Iterator;

   begin
      Trace (Me, "Find_Ancestor_Dependencies: "
             & Full_Name (Source_Filename).all
             & " self="   & Boolean'Image (Include_Self)
             & " single=" & Boolean'Image (Single_Source_File));

      if Decl_Project = No_Project then
         Decl_Project := Get_Project_From_File
           (Project_Registry'Class (Get_Registry (Root_Project)),
            Source_Filename,
            Root_If_Not_Found => False);
         Iterator_Decl_Project := Decl_Project;

         if Decl_Project = No_Project then
            Decl_Project := Root_Project;
         end if;
      end if;

      Iterator.LI               := No_LI_File;
      Iterator.LI_Once          := LI_Once;
      Iterator.Current_Separate := null;
      Iterator.Current_Part     := Unit_Spec;
      Iterator.Decl_LI          := Locate_From_Source (List, Source_Filename);
      Iterator.Source_Filename  := Source_Filename;
      Iterator.Include_Self     := Include_Self;
      Iterator.Indirect_Imports := Indirect_Imports;

      Handler := Get_LI_Handler_From_File
        (Glide_Language_Handler (Lang_Handler), Source_Filename);

      if Handler = null then
         Trace (Me, "Find_Ancestor_Dependencies: Unsupported language");
         Destroy (Iterator);
         return;
      end if;

      Create_Or_Complete_LI
        (Handler         => Handler,
         File            => Iterator.Decl_LI,
         Source_Filename => Source_Filename,
         List            => List,
         Project         => Decl_Project);

      Assert (Me,
              Iterator.Decl_LI /= null,
              "LI file not found for " & Base_Name (Source_Filename));

      if Single_Source_File then
         Iterator.Importing      := Start (Decl_Project, Recursive => False);
         Iterator.Source_Files   := new File_Array'(1 => Source_Filename);
         Iterator.Total_Progress := Iterator.Source_Files'Length;

      else
         Iterator.Importing := Find_All_Projects_Importing
           (Root_Project, Iterator_Decl_Project,
            Include_Self => True);
         Iterator.Source_Files := Get_Source_Files
           (Current (Iterator.Importing),
            Recursive => False,
            Full_Path => True);

         Iterator.Total_Progress := 0;
         Tmp := Iterator.Importing;

         while Current (Tmp) /= No_Project loop
            Iterator.Total_Progress := Iterator.Total_Progress
              + Direct_Sources_Count (Current (Tmp));
            Next (Tmp);
         end loop;
      end if;

      Iterator.Current_File := Iterator.Source_Files'First - 1;
      Iterator.Current_Progress := 0;

      Next (Lang_Handler, Iterator, List);

   exception
      when Assert_Failure =>
         Destroy (Iterator);

      when E : others =>
         Trace (Me, "Find_Ancestor_Dependencies: unexpected exception "
                & Exception_Message (E));
   end Find_Ancestor_Dependencies;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Dependency_Iterator)
      return Dependency
   is
      S : Virtual_File;
   begin
      Assert (Me, not Iterator.Indirect_Imports,
              "Get cannot be used when searching indirect imports");

      --  Is this a dependency from a separate ?
      if Iterator.Current_Separate /= null then
         return
           (File => Internal_File'
              (File_Name => Create
                 (Iterator.Current_Separate.Value.Source_Filename.all,
                  Iterator.LI.LI.Project, Use_Object_Path => False),
               LI_Name   => Iterator.LI.LI.LI_Filename),
            Dep => (False, True));
      end if;

      if Iterator.Current_Part = Unit_Spec then
         S := Create
           (Iterator.LI.LI.Spec_Info.Source_Filename.all,
            Iterator.LI.LI.Project, Use_Object_Path => False);
      else
         S := Create
           (Iterator.LI.LI.Body_Info.Source_Filename.all,
            Iterator.LI.LI.Project, Use_Object_Path => False);
      end if;

      if Iterator.LI = Iterator.Decl_LI then
         return
           (File => Internal_File'
              (File_Name => S, LI_Name => Iterator.LI.LI.LI_Filename),
            Dep  => (False, True));
      else
         return
           (File => Internal_File'
              (File_Name => S, LI_Name => Iterator.LI.LI.LI_Filename),
            Dep  => Iterator.Current_Decl.Value.Dep_Info);
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Dependency_Iterator) return LI_File_Ptr is
   begin
      return Iterator.LI;
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Dependency_Iterator) return Project_Type is
   begin
      return Current (Iterator.Importing);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Entity_Reference_Iterator) return Project_Type is
   begin
      return Get (Iterator.Decl_Iter);
   end Get;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out Dependency_Iterator) is
   begin
      Unchecked_Free (Iterator.Source_Files);
      Reset (Iterator.Examined);
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Iterator : in out Dependency_Iterator_Access)
   is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Dependency_Iterator, Dependency_Iterator_Access);
   begin
      Destroy (Iterator.all);
      Unchecked_Free (Iterator);
   end Destroy;

   -----------------------
   -- Get_Other_File_Of --
   -----------------------

   function Get_Other_File_Of
     (Lib_Info : LI_File_Ptr; Source_Filename : Virtual_File)
      return Virtual_File
   is
      Part : constant Unit_Part := Get_Unit_Part (Lib_Info, Source_Filename);
   begin
      Assert (Me, not Is_Incomplete (Lib_Info), "LI file is not up-to-date");

      case Part is
         when Unit_Body | Unit_Separate =>
            if Lib_Info.LI.Spec_Info /= null then
               return Create
                 (Lib_Info.LI.Spec_Info.Source_Filename.all,
                  Lib_Info.LI.Project, Use_Object_Path => False);
            end if;

         when Unit_Spec =>
            if Lib_Info.LI.Body_Info /= null then
               return Create
                 (Lib_Info.LI.Body_Info.Source_Filename.all,
                  Lib_Info.LI.Project, Use_Object_Path => False);
            end if;
      end case;
      return VFS.No_File;
   end Get_Other_File_Of;

   --------------------
   -- Is_Renaming_Of --
   --------------------

   function Is_Renaming_Of
     (Entity : Entity_Information; Node : Scope_Tree_Node) return Boolean is
   begin
      return Node.Decl.Rename /= Null_File_Location
        and then not Is_Predefined_Entity (Entity)
        and then Location_Matches
        (Node.Decl.Rename, Entity.Decl_File,
         Entity.Decl_Line, Entity.Decl_Column) = 0;
   end Is_Renaming_Of;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (List : E_Declaration_Info_List;
      Decl_Line, Decl_Column : Natural; Entity_Name : String := "")
      return E_Declaration_Info_List
   is
      Decl : E_Declaration_Info_List := List;
   begin
      while Decl /= null loop
         if Decl.Value.Declaration.Location.Line = Decl_Line
           and then Decl.Value.Declaration.Location.Column = Decl_Column
           and then (Entity_Name = ""
                     or else Decl.Value.Declaration.Name.all = Entity_Name)
         then
            return Decl;
         end if;

         Decl := Decl.Next;
      end loop;
      return null;
   end Get_Declaration;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (List : LI_File_List; Entity : Entity_Information) return E_Declaration is
   begin
      return Get_Declaration
        (Get_Declaration_Location (List, Entity), Get_Name (Entity));
   end Get_Declaration;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Location : File_Location; Entity_Name : String := "")
      return E_Declaration
   is
      File : constant File_Info_Ptr := Get_File_Info (Location.File);
      Decl : E_Declaration_Info_List;
   begin
      Decl := Get_Declaration
        (File.Declarations, Location.Line, Location.Column, Entity_Name);
      if Decl /= null then
         return Decl.Value.Declaration;
      else
         return No_Declaration;
      end if;
   end Get_Declaration;

   ------------------------------
   -- Get_Declaration_Location --
   ------------------------------

   function Get_Declaration_Location
     (List : LI_File_List; Entity : Entity_Information) return File_Location is
   begin
      return File_Location'(File   => Get_Source_File (List, Entity),
                            Line   => Get_Declaration_Line_Of (Entity),
                            Column => Get_Declaration_Column_Of (Entity));
   end Get_Declaration_Location;

   ---------------------
   -- Get_Source_File --
   ---------------------

   function Get_Source_File
     (List : LI_File_List; Entity : Entity_Information) return Source_File
   is
      LI   : LI_File_Ptr;
      Part : Unit_Part;
   begin
      LI   := Locate_From_Source (List, Get_Declaration_File_Of (Entity));
      Part := Get_Unit_Part (LI, Get_Declaration_File_Of (Entity));

      if Part /= Unit_Separate then
         return Source_File'(LI => LI, Part => Part, Source_Filename => null);
      else
         return Source_File'
           (LI => LI, Part => Part,
            Source_Filename => new String'
              (Base_Name (Get_Declaration_File_Of (Entity))));
      end if;
   end Get_Source_File;

   -----------------
   -- Renaming_Of --
   -----------------

   procedure Renaming_Of
     (List           : LI_File_List;
      Entity         : Entity_Information;
      Is_Renaming    : out Boolean;
      Renamed_Entity : out Entity_Information)
   is
      Decl : E_Declaration := Get_Declaration (List, Entity);
      Renamed_Location : constant File_Location := Decl.Rename;
   begin
      Is_Renaming := Renamed_Location /= Null_File_Location;

      if Is_Renaming then
         Decl := Get_Declaration (Renamed_Location);
         if Decl /= No_Declaration then
            Renamed_Entity := Get_Entity (Decl);
         else
            Trace (Me, Get_Name (Entity) & " is a renaming, but couldn't"
                     & " find LI file for renamed entity");
            Renamed_Entity := No_Entity_Information;
         end if;
      else
         Renamed_Entity := No_Entity_Information;
      end if;
   end Renaming_Of;

   --------------------------------
   -- Get_Declarations_From_File --
   --------------------------------

   function Get_Declarations_From_File
     (Lib_Info : LI_File_Ptr; File_Name : VFS.Virtual_File)
      return E_Declaration_Info_List
   is
      Base : constant String := Base_Name (File_Name);
      Sep : File_Info_Ptr_List;
   begin
      if Lib_Info.LI.Body_Info /= null
        and then Lib_Info.LI.Body_Info.Source_Filename.all = Base
      then
         return Lib_Info.LI.Body_Info.Declarations;
      end if;

      if Lib_Info.LI.Spec_Info /= null
        and then Lib_Info.LI.Spec_Info.Source_Filename.all = Base
      then
         return Lib_Info.LI.Spec_Info.Declarations;
      end if;

      Sep := Lib_Info.LI.Separate_Info;
      while Sep /= null loop
         if Sep.Value.Source_Filename.all = Base then
            return Sep.Value.Declarations;
         end if;
         Sep := Sep.Next;
      end loop;

      return null;
   end Get_Declarations_From_File;

   ------------------------------------
   -- Find_All_Possible_Declarations --
   ------------------------------------

   function Find_All_Possible_Declarations
     (Lib_Info    : LI_File_Ptr;
      Entity_Name : String := "";
      In_Source_File : Virtual_File := VFS.No_File)
      return Entity_Declaration_Iterator
   is
      Iter : Entity_Declaration_Iterator;
   begin
      Assert (Me, Lib_Info /= null and then Lib_Info.LI.Parsed,
              "Unparsed LI_File_Ptr in Find_All_Possible_Declarations");

      if Entity_Name /= "" then
         if Case_Insensitive_Identifiers (Lib_Info.LI.Handler) then
            Iter.Entity_Name := new String'(To_Lower (Entity_Name));
         else
            Iter.Entity_Name := new String'(Entity_Name);
         end if;
      end if;

      Iter.Lib_Info    := Lib_Info;
      Iter.Current_Dep := Lib_Info.LI.Dependencies_Info;
      Iter.Part := None;
      Iter.Current     := null;
      Iter.Sep_Source  := null;

      if In_Source_File /= VFS.No_File then
         Iter.Current := Get_Declarations_From_File (Lib_Info, In_Source_File);
         Iter.Uniq_List := True;
      else
         Iter.Uniq_List := False;
         Next (Iter);
      end if;

      return Iter;
   end Find_All_Possible_Declarations;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Entity_Declaration_Iterator)
      return Entity_Information is
   begin
      if Iterator.Current = null then
         return No_Entity_Information;
      else
         return Get_Entity (Iterator.Current.Value.Declaration);
      end if;
   end Get;

   ------------
   -- At_End --
   ------------

   function At_End (Iterator : Entity_Declaration_Iterator) return Boolean is
   begin
      return Iterator.Current = null
        and then Iterator.Lib_Info = null;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Entity_Declaration_Iterator) is
      File   : File_Info_Ptr;
   begin
      if Iterator.Current /= null then
         Iterator.Current := Iterator.Current.Next;
      end if;

      loop
         --  While there remains some declarations to analyze in the current
         --  file
         while Iterator.Current /= null loop
            if Iterator.Current.Value.Declaration.Kind.Kind /=
              Overloaded_Entity
              and then
              (Iterator.Entity_Name = null
               or else (Iterator.Current.Value.Declaration.Name /= null
                        and then Iterator.Current.Value.Declaration.Name.all =
                        Iterator.Entity_Name.all))
            then
               --  We have found a matching entity
               return;
            end if;
            Iterator.Current := Iterator.Current.Next;
         end loop;

         --  Are we done yet ?
         if Iterator.Uniq_List then
            Iterator.Lib_Info := null;
            Destroy (Iterator);
            return;
         end if;

         pragma Assert (Iterator.Current = null);

         if Iterator.Lib_Info /= null then
            --  Move to the next part of the original file

            if Iterator.Part = None then
               Iterator.Part := Unit_Spec;
               File := Iterator.Lib_Info.LI.Spec_Info;

            elsif Iterator.Part = Unit_Spec then
               Iterator.Part := Unit_Body;
               File := Iterator.Lib_Info.LI.Body_Info;

            else
               if Iterator.Part = Unit_Body then
                  Iterator.Part := Unit_Separate;
                  Iterator.Sep_Source :=
                    Iterator.Lib_Info.LI.Separate_Info;
               else
                  Iterator.Sep_Source := Iterator.Sep_Source.Next;
               end if;

               if Iterator.Sep_Source /= null then
                  File := Iterator.Sep_Source.Value;
               else
                  --  No more files to analyze, move to next LI
                  File := null;
                  Iterator.Lib_Info := null;
               end if;
            end if;

            if File /= null then
               Iterator.Current := File.Declarations;
            end if;
         end if;

         --  Do not use "else", this might have been set to null above
         if Iterator.Lib_Info = null then
            if Iterator.Current_Dep = null then
               Iterator.Lib_Info := null;
               Destroy (Iterator);
               return;
            end if;

            Iterator.Current := Iterator.Current_Dep.Value.Declarations;
            Iterator.Current_Dep := Iterator.Current_Dep.Next;
            Iterator.Part        := None;
         end if;
      end loop;
   end Next;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out Entity_Declaration_Iterator) is
   begin
      Iterator.Lib_Info := null;
      Free (Iterator.Entity_Name);
      Iterator.Current := null;
   end Destroy;

   ------------
   -- Create --
   ------------

   function Create
     (File   : VFS.Virtual_File;
      Line   : Positive;
      Column : Natural;
      Name   : String;
      Scope  : E_Scope;
      Kind   : E_Kind) return Entity_Information is
   begin
      return Entity_Information'
        (Name        => new String'(Name),
         Decl_Line   => Line,
         Decl_Column => Column,
         Decl_File   => File,
         Scope       => Scope,
         Kind        => Kind);
   end Create;

   ------------------
   -- Free_Boolean --
   ------------------

   procedure Free_Boolean (X : in out Boolean) is
      pragma Unreferenced (X);
   begin
      null;
   end Free_Boolean;

   ------------------------
   -- Get_Parent_Package --
   ------------------------

   function Get_Parent_Package
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Entity_Information
   is
      Decl   : E_Declaration_Info;
      Ref    : E_Reference_List;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      Internal_Find_Declaration_Or_Body
        (Lib_Info         => Lib_Info,
         File_Name        => Get_Declaration_File_Of (Entity),
         Entity_Name      => Get_Name (Entity),
         Line             => Get_Declaration_Line_Of (Entity),
         Column           => Get_Declaration_Column_Of (Entity),
         Check_References => False,
         Decl             => Decl,
         Ref              => Ref,
         Status           => Status);

      if Status = Success then
         Ref := Decl.References;
         while Ref /= null loop
            if Ref.Value.Kind = Parent_Package then
               return Create (Lib_Info, Ref.Value);
            end if;

            Ref := Ref.Next;
         end loop;
      end if;

      return No_Entity_Information;
   end Get_Parent_Package;

   -------------------------------
   -- Get_Subprogram_Parameters --
   -------------------------------

   function Get_Subprogram_Parameters
     (Lib_Info : LI_File_Ptr; Subprogram : Entity_Information)
      return Subprogram_Iterator
   is
      Iter : Subprogram_Iterator;
      D    : E_Declaration_Info_List;
   begin
      Assert (Me, Lib_Info /= null, "Get_Subprogram_Parameters: null LI");

      Iter.Lib_Info := Lib_Info;
      D := Find_Declaration_In_LI (Lib_Info, Subprogram);

      if D /= null then
         Iter.Current  := D.Value.References;
      end if;

      if Iter.Current /= null
        and then Iter.Current.Value.Kind /= Subprogram_In_Parameter
        and then Iter.Current.Value.Kind /= Subprogram_In_Out_Parameter
        and then Iter.Current.Value.Kind /= Subprogram_Out_Parameter
        and then Iter.Current.Value.Kind /= Subprogram_Access_Parameter
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
      if Iterator.Current /= null then
         loop
            Iterator.Current := Iterator.Current.Next;

            exit when Iterator.Current = null;

            exit when Iterator.Current.Value.Kind = Subprogram_In_Parameter
              or else Iterator.Current.Value.Kind = Subprogram_In_Out_Parameter
              or else Iterator.Current.Value.Kind = Subprogram_Out_Parameter
              or else Iterator.Current.Value.Kind =
                Subprogram_Access_Parameter;
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iterator : Subprogram_Iterator) return Entity_Information is
   begin
      if Iterator.Current = null then
         return No_Entity_Information;
      else
         return Create (Iterator.Lib_Info, Iterator.Current.Value);
      end if;
   end Get;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Iterator : Subprogram_Iterator) return Parameter_Type is
   begin
      if Iterator.Current = null then
         return In_Parameter;
      else
         case Iterator.Current.Value.Kind is
            when Subprogram_In_Parameter     => return In_Parameter;
            when Subprogram_Out_Parameter    => return Out_Parameter;
            when Subprogram_In_Out_Parameter => return In_Out_Parameter;
            when Subprogram_Access_Parameter => return Access_Parameter;
            when others                      => return In_Parameter;
         end case;
      end if;
   end Get_Type;

   -----------
   -- Image --
   -----------

   function Image (Param : Parameter_Type) return String is
   begin
      case Param is
         when In_Parameter     => return "in";
         when Out_Parameter    => return "out";
         when In_Out_Parameter => return "in out";
         when Access_Parameter => return "access";
      end case;
   end Image;

   ------------
   -- Create --
   ------------

   function Create  (Decl : E_Declaration) return Entity_Information is
   begin
      if Decl.Name = null then
         Trace (Me, "Decl.Name = null");
      end if;

      return Create
        (File   => Get_File (Decl.Location),
         Line   => Get_Line (Decl.Location),
         Column => Get_Column (Decl.Location),
         Name   => Decl.Name.all,
         Scope  => Decl.Scope,
         Kind   => Decl.Kind);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Lib_Info : LI_File_Ptr;
      Ref      : E_Reference) return Entity_Information
   is
      Decl   : E_Declaration_Info;
      R      : E_Reference_List;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      Internal_Find_Declaration_Or_Body
        (Lib_Info         => Lib_Info,
         File_Name        => Get_File (Ref.Location),
         Entity_Name      => "",
         Line             => Get_Line (Ref.Location),
         Column           => Get_Column (Ref.Location),
         Check_References => False,
         Decl             => Decl,
         Ref              => R,
         Status           => Status);

      if Status /= Success then
         return No_Entity_Information;

      else
         return Create (Decl.Declaration);
      end if;
   end Create;

   -------------------
   -- Get_Reference --
   -------------------

   function Get_Reference (Node : Scope_Tree_Node) return E_Reference is
   begin
      case Node.Typ is
         when Declaration =>
            return (Location => Node.Decl.Location,
                    Kind     => Label);

         when Reference =>
            return Node.Ref.all;
      end case;
   end Get_Reference;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Entity1, Entity2 : Entity_Information) return Boolean is
   begin
      return Entity1.Decl_Line = Entity2.Decl_Line
        and then Entity1.Decl_Column = Entity2.Decl_Column
        and then Entity1.Decl_File = Entity2.Decl_File
        and then Entity1.Name.all = Entity2.Name.all;
      --  In fact, we don't really need to test the names, since there is only
      --  one possible entity at any given location. However, the source files
      --  might have changed between the creation of Entity1 and Entity2, so we
      --  do this as an extra check.
   end Is_Equal;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   function Is_Predefined_Entity
     (Entity : Entity_Information) return Boolean is
   begin
      return Entity.Decl_File = VFS.No_File;
   end Is_Predefined_Entity;

   ---------------------
   -- Process_Parents --
   ---------------------

   function Process_Parents
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information;
      Kind     : Parent_Kind) return Entity_Information
   is
      Decl : constant E_Declaration_Info_List := Find_Declaration_In_LI
        (Lib_Info, Entity);
      Parent : File_Location_List;
   begin
      if Decl /= null then
         Parent := Decl.Value.Declaration.Parent_Location;
         while Parent /= null
           and then Parent.Kind /= Kind
         loop
            Trace (Me, "Got parent kind=" & Parent.Kind'Img & " (looking for "
                   & Kind'Img & ')');
            Parent := Parent.Next;
         end loop;

         if Parent /= null then
            if Parent.Value = Predefined_Entity_Location then
               Trace
                 (Me, "Process_Parents: Found a predefined entity: "
                  & Get_String (Parent.Predefined_Entity_Name));
               return Create
                 (File   => VFS.No_File,
                  Line   => 1,
                  Column => 1,
                  Name => Get_String (Parent.Predefined_Entity_Name),
                  Scope  => Global_Scope,
                  Kind   => Unresolved_Entity_Kind);
            else
               --  The type of the variable might be either directly in the
               --  source files of Lib_Info or in one of the imported source
               --  files.
               return Find_Declaration_In_LI_Or_Dependencies
                 (Lib_Info, Parent.Value);
            end if;
         end if;
      end if;

      Trace (Me, "Process_Parents: Declaration not found for "
             & Get_Name (Entity) & ' '
             & Base_Name (Get_Declaration_File_Of (Entity))
             & Get_Declaration_Line_Of (Entity)'Img
             & Get_Declaration_Column_Of (Entity)'Img
             & ' ' & Full_Name (Get_LI_Filename (Lib_Info)).all);
      return No_Entity_Information;
   end Process_Parents;

   -----------------------
   -- Get_Variable_Type --
   -----------------------

   function Get_Variable_Type
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Entity_Information is
   begin
      return Process_Parents (Lib_Info, Entity, Container_Type);
   end Get_Variable_Type;

   ------------------
   -- Pointed_Type --
   ------------------

   function Pointed_Type
     (Lib_Info   : LI_File_Ptr;
      Access_Type : Entity_Information) return Entity_Information is
   begin
      return Process_Parents (Lib_Info, Access_Type, Pointed_Type);
   end Pointed_Type;

   -------------------
   -- Returned_Type --
   -------------------

   function Returned_Type
     (Lib_Info        : LI_File_Ptr;
      Subprogram_Type : Entity_Information) return Entity_Information is
   begin
      return Process_Parents (Lib_Info, Subprogram_Type, Returned_Type);
   end Returned_Type;

   ----------------------
   -- Get_Parent_Types --
   ----------------------

   function Get_Parent_Types
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Parent_Iterator
   is
      Decl : constant E_Declaration_Info_List := Find_Declaration_In_LI
        (Lib_Info, Entity);
      Parent : File_Location_List;
   begin
      if Decl = null then
         return (Lib_Info => Lib_Info, Current => null);
      else
         Parent := Decl.Value.Declaration.Parent_Location;
         while Parent /= null
           and then Parent.Kind /= Parent_Type
           and then Parent.Kind /= Container_Type
         loop
            Parent := Parent.Next;
         end loop;

         return (Lib_Info => Lib_Info, Current => Parent);
      end if;
   end Get_Parent_Types;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Parent_Iterator) is
   begin
      if Iter.Current /= null then
         Iter.Current := Iter.Current.Next;
         while Iter.Current /= null
           and then Iter.Current.Kind /= Parent_Type
           and then Iter.Current.Kind /= Container_Type
         loop
            Iter.Current := Iter.Current.Next;
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Parent_Iterator) return Entity_Information is
   begin
      if Iter.Current = null then
         return No_Entity_Information;
      else
         return Find_Declaration_In_LI_Or_Dependencies
           (Iter.Lib_Info, Iter.Current.Value);
      end if;
   end Get;

   ------------------------------
   -- Get_Primitive_Operations --
   ------------------------------

   function Get_Primitive_Operations
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Primitive_Iterator
   is
      Decl : constant E_Declaration_Info_List := Find_Declaration_In_LI
        (Lib_Info, Entity);
   begin
      if Decl = null then
         return (Lib_Info => Lib_Info,
                 Kind     => Primitive_Operation,
                 Current  => null);
      else
         return (Lib_Info => Lib_Info,
                 Kind     => Primitive_Operation,
                 Current  => Decl.Value.Declaration.Primitive_Subprograms);
      end if;
   end Get_Primitive_Operations;

   -----------------------
   -- Get_Discriminants --
   -----------------------

   function Get_Discriminants
     (Lib_Info : LI_File_Ptr;
      Entity   : Entity_Information) return Discriminant_Iterator
   is
      Decl : constant E_Declaration_Info_List := Find_Declaration_In_LI
        (Lib_Info, Entity);
      C    : E_Reference_List;
   begin
      if Decl = null then
         return (Lib_Info => Lib_Info,
                 Kind     => Discriminant,
                 Current  => null);
      else
         C := Decl.Value.References;
         while C /= null and then C.Value.Kind /= Discriminant loop
            C := C.Next;
         end loop;

         return (Lib_Info => Lib_Info,
                 Kind     => Discriminant,
                 Current  => C);
      end if;
   end Get_Discriminants;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Special_Iterator) is
   begin
      if Iter.Current /= null then
         loop
            Iter.Current := Iter.Current.Next;
            exit when Iter.Current = null
              or else Iter.Current.Value.Kind = Iter.Kind;
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Special_Iterator) return Entity_Information is
   begin
      if Iter.Current = null then
         return No_Entity_Information;
      else
         return Find_Declaration_In_LI_Or_Dependencies
           (Iter.Lib_Info, Iter.Current.Value.Location);
      end if;
   end Get;

   ------------
   -- Length --
   ------------

   function Length (Iter : Special_Iterator) return Natural is
      L : Natural := 0;
      C : E_Reference_List := Iter.Current;
   begin
      while C /= null loop
         if C.Value.Kind = Iter.Kind then
            L := L + 1;
         end if;
         C := C.Next;
      end loop;
      return L;
   end Length;

   --------------------
   -- Is_Declaration --
   --------------------

   function Is_Declaration (Node : Scope_Tree_Node) return Boolean is
   begin
      return Node.Typ = Declaration;
   end Is_Declaration;

   ------------------------
   -- Get_Children_Types --
   ------------------------

   function Get_Children_Types
     (Lib_Info : LI_File_Ptr; Entity : Entity_Information)
      return Child_Type_Iterator
   is
      Iter : Child_Type_Iterator;
   begin
      Iter.Lib_Info := Lib_Info;
      Iter.File     := null;
      Iter.Entity   := Copy (Entity);

      if Lib_Info.LI.Spec_Info /= null
        and then Lib_Info.LI.Spec_Info.Declarations /= null
      then
         Iter.Part     := Unit_Spec;
         Iter.Current  := Lib_Info.LI.Spec_Info.Declarations;

      elsif Lib_Info.LI.Body_Info /= null
        and then Lib_Info.LI.Body_Info.Declarations /= null
      then
         Iter.Part     := Unit_Body;
         Iter.Current  := Lib_Info.LI.Body_Info.Declarations;

      else
         Iter.Part := Unit_Separate;
         Iter.File := Lib_Info.LI.Separate_Info;
         while Iter.File /= null
           and then Iter.File.Value.Declarations = null
         loop
            Iter.File := Iter.File.Next;
         end loop;

         if Iter.File /= null then
            Iter.Current := Iter.File.Value.Declarations;
         end if;
      end if;
      Internal_Next (Iter);
      return Iter;
   end Get_Children_Types;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Child_Type_Iterator) is
   begin
      Destroy (Iter.Entity);
   end Destroy;

   -------------------
   -- Internal_Next --
   -------------------

   procedure Internal_Next (Iter : in out Child_Type_Iterator) is
      Parent : File_Location_List;
   begin
      if Iter.Current /= null then
         loop
            if Iter.Current /= null then
               if Iter.Current.Value.Declaration.Kind.Is_Type then
                  --  Any parent matches ?
                  Parent := Iter.Current.Value.Declaration.Parent_Location;
                  while Parent /= null loop
                     if (Parent.Kind = Parent_Type
                         or else Parent.Kind = Container_Type)
                       and then Location_Matches
                       (Parent.Value,
                        Get_Declaration_File_Of (Iter.Entity),
                        Get_Declaration_Line_Of (Iter.Entity),
                        Get_Declaration_Column_Of (Iter.Entity)) = 0
                     then
                        return;
                     end if;

                     Parent := Parent.Next;
                  end loop;
               end if;

               Iter.Current := Iter.Current.Next;
            end if;

            if Iter.Current = null then
               case Iter.Part is
                  when Unit_Spec =>
                     Iter.Part := Unit_Body;
                     if Iter.Lib_Info.LI.Body_Info /= null then
                        Iter.Current :=
                          Iter.Lib_Info.LI.Body_Info.Declarations;
                     end if;

                  when Unit_Body =>
                     Iter.Part := Unit_Separate;
                     Iter.File := Iter.Lib_Info.LI.Separate_Info;
                     if Iter.File /= null then
                        Iter.Current := Iter.File.Value.Declarations;
                     else
                        return;
                     end if;

                  when Unit_Separate =>
                     Iter.File := Iter.File.Next;
                     if Iter.File /= null then
                        Iter.Current := Iter.File.Value.Declarations;
                     else
                        return;
                     end if;
               end case;
            end if;
         end loop;
      end if;
   end Internal_Next;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Child_Type_Iterator) is
   begin
      if Iter.Current /= null then
         Iter.Current := Iter.Current.Next;
         Internal_Next (Iter);
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Child_Type_Iterator) return Entity_Information is
   begin
      if Iter.Current = null then
         return No_Entity_Information;
      else
         return Create (Iter.Current.Value.Declaration);
      end if;
   end Get;

   ----------------
   -- Is_Subtype --
   ----------------

   function Is_Subtype
     (Decl_File : LI_File_Ptr;
      Entity    : Entity_Information) return Boolean
   is
      Decl : constant E_Declaration_Info_List := Find_Declaration_In_LI
        (Decl_File, Entity);
      Parent : File_Location_List;
   begin
      if Decl = null
        or else not Decl.Value.Declaration.Kind.Is_Type
      then
         return False;
      end if;

      Parent := Decl.Value.Declaration.Parent_Location;
      while Parent /= null loop
         if Parent.Kind = Container_Type then
            return True;
         end if;
         Parent := Parent.Next;
      end loop;

      return False;
   end Is_Subtype;

   ---------------------
   -- Is_Discriminant --
   ---------------------

   function Is_Discriminant
     (Discr            : Entity_Information;
      Lib_Info_For_Typ : LI_File_Ptr;
      Typ              : Entity_Information) return Boolean
   is
      Iter : Discriminant_Iterator := Get_Discriminants
        (Lib_Info_For_Typ, Typ);
      D    : Entity_Information;
   begin
      --  ??? Not efficient. Since this is used in the context of scope_tree,
      --  should we improve the scope trees instead ?
      loop
         D := Get (Iter);
         exit when D = No_Entity_Information;
         if Is_Equal (D, Discr) then
            return True;
         end if;

         Destroy (D);
         Next (Iter);
      end loop;
      return False;
   end Is_Discriminant;

   ------------------------------
   -- Create_Predefined_Entity --
   ------------------------------

   function Create_Predefined_Entity (Name : String; Kind : E_Kind)
      return Entity_Information
   is
   begin
      return Create
        (File   => VFS.No_File,
         Line   => 1,
         Column => 1,
         Name   => Name,
         Scope  => Global_Scope,
         Kind   => Kind);
   end Create_Predefined_Entity;

end Src_Info.Queries;
