-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Src_Info.Queries;          use Src_Info.Queries;
with Test_Utils;                use Test_Utils;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Src_Info;                  use Src_Info;
with Src_Info.Queries;          use Src_Info.Queries;
with Language_Handlers;         use Language_Handlers;
with Test_Utils;                use Test_Utils;
with Work_On_Source;            use Work_On_Source;
with Doc_Types;                 use Doc_Types;


package body Work_on_File is

   package TSFL renames Type_Source_File_List;
   package TEL  renames Type_Entity_List;
   package ASU  renames Ada.Strings.Unbounded;

   Entity_List           : Type_Entity_List.List;
   Entity_Node           : Entity_List_Information;
   Subprogram_Index_List : Type_Entity_List.List;
   Type_Index_List       : Type_Entity_List.List;

   -------------------
   -- Process_Files --
   -------------------

   procedure Process_Files
     (Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options)
   is
      Source_File_Node : Type_Source_File_List.List_Node;
      Source_Info_List : Src_Info.LI_File_List;
      Handler          : Language_Handler;
      Project_Tree     : Project_Node_Id;
      Project_View     : Project_Id;
      Doc_File         : File_Type;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
   begin

      --  sort the list of the files first
      Sort_List_Name (Source_File_List);

      Source_File_Node := TSFL.First (Source_File_List);

      --  get the handler
      Handler := Create_Lang_Handler;

      --  get Project_Tree and Project_View
      Reset (Source_Info_List);
      Load_Project (TSFL.Data (Source_File_Node).Prj_File_Name.all,
                       Handler, Project_Tree, Project_View);


      for J in 1 .. Type_Source_File_List.Length (Source_File_List) loop


         --  create the doc file from the package name for each package
         if not Options.Doc_One_File then
            Create (Doc_File,
                       Out_File,
                       Get_Doc_File_Name (TSFL.Data
                                           (Source_File_Node).File_Name.all,
                                         Options.Doc_Directory.all,
                                         Options.Doc_Suffix.all));
            Put_Line ("from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
            Put_Line ("   create documentation to: " &
                           Get_Doc_File_Name (TSFL.Data
                                            (Source_File_Node).File_Name.all,
                                          Options.Doc_Directory.all,
                                          Options.Doc_Suffix.all));
            --  create the doc file from the project file name for all packages
         elsif J = 1 and Options.Doc_One_File then
            Create (Doc_File,
                       Out_File,
                    Get_Doc_File_Name
                      (TSFL.Data
                         (Source_File_Node).Prj_File_Name.all,
                       Options.Doc_Directory.all,
                       Options.Doc_Suffix.all));
            Put_Line ("create documentation to: " &
                        Get_Doc_File_Name
                          (TSFL.Data
                               (Source_File_Node).Prj_File_Name.all,
                           Options.Doc_Directory.all,
                           Options.Doc_Suffix.all));
            Put_Line ("   from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
         else
            Put_Line ("   from file: " &
                     TSFL.Data (Source_File_Node).File_Name.all);
         end if;


         --  find the next and the previous package name (used for TexInfo)
         if J = 1 then
            Prev_Package := new String '(" ");
         else
            Source_File_Node := TSFL.Prev (Source_File_List,
                                              Source_File_Node);
            Prev_Package :=
              new String '(TSFL.Data (Source_File_Node).Package_Name.all);
            Source_File_Node := TSFL.Next (Source_File_Node);
         end if;
         if J = Type_Source_File_List.Length (Source_File_List) then
            Next_Package := new String '(" ");
         else
            Source_File_Node := TSFL.Next (Source_File_Node);
            Next_Package :=
              new String '(TSFL.Data (Source_File_Node).Package_Name.all);
            Source_File_Node := TSFL.Prev (Source_File_List,
                                              Source_File_Node);
         end if;

         Process_One_File (Doc_File,
                              J = 1,
                              J = Type_Source_File_List.Length
                                (Source_File_List),
                              TSFL.Data (Source_File_Node).File_Name.all,
                              TSFL.Data (Source_File_Node).Package_Name.all,
                              Next_Package,
                              Prev_Package,
                              TSFL.Data (Source_File_Node).Def_In_Line,
                              Source_File_List,
                              Source_Info_List, Handler,
                              Project_Tree, Project_View,
                              Options,
                              Options.Process_Body_Files and
                                TSFL.Data (Source_File_Node).Other_File_Found);

         Source_File_Node := TSFL.Next (Source_File_Node);

         --  destroy the doc file
         if J = Type_Source_File_List.Length (Source_File_List) or
         not Options.Doc_One_File then
            Close (Doc_File);
         end if;

         Free (Next_Package);
         Free (Prev_Package);
      end loop;

      if not Options.Doc_One_File then
         --  sort the type list and the subprogram list first
         Sort_List_Name (Subprogram_Index_List);
         Sort_List_Name (Type_Index_List);

         --  create the index files for the packages
         Process_Unit_Index       (Source_File_List, Options);
         Process_Subprogram_Index (Subprogram_Index_List, Options);
         Process_Type_Index       (Type_Index_List, Options);
      end if;
   end Process_Files;



   ----------------------
   -- Process_One_File --
   ----------------------

   procedure Process_One_File
     (Doc_File           : File_Type;
      First_File         : Boolean;
      Last_File          : Boolean;
      Source_Filename    : String;
      Package_Name       : String;
      Next_Package       : GNAT.OS_Lib.String_Access;
      Prev_Package       : GNAT.OS_Lib.String_Access;
      Def_In_Line        : Integer;
      Source_File_List   : in out Type_Source_File_List.List;
      Source_Info_List   : in out Src_Info.LI_File_List;
      Handler            : in out Language_Handler;
      Project_Tree       : in out Project_Node_Id;
      Project_View       : in out Project_Id;
      Options            : All_Options;
      Process_Body_File  : Boolean)

   is
      LI_Unit                   : LI_File_Ptr;
      Entity_Iter               : Entity_Declaration_Iterator;
      Info                      : Entity_Information;

      function Source_File_In_List
        (Name : String) return Boolean;
      --  returns true if the file is found in the source file list

      function Is_Defined_In_Subprogram
        (Entity          : String;
         Short_Entity    : String;
         Package_Name    : String) return Boolean;
      --  returns true if the entity is defined within another entity and
      --  not dierctly within the package

      function Get_Full_Entity_Filename
        (Filename         : String) return String;
      --  tries to find the file in the list and if found, it returns it
      --  with all his path in front of the name

      function Is_Constant_Or_Named_Number
        (Header : String) return Boolean;

      function Get_Whole_Header
        (Source_Filename : String;
         Line            : Natural;
         Is_Generic      : Boolean) return String;
         --  returns one string with all the header of the entity

      procedure Process_Subprogram
        (Source_Filename : String;
         Entity_File     : String;
         Info            : Entity_Information);

      procedure Process_Type
        (Source_Filename : String;
         Entity_File     : String);


      -------------------------
      -- Source_File_In_List --
      -------------------------

      function Source_File_In_List
        (Name : String) return Boolean
      is
         Source_File_Node : Type_Source_File_List.List_Node;
         Found            : Boolean;
      begin
         Found := False;
         Source_File_Node := TSFL.First (Source_File_List);
         for J in 1 .. TSFL.Length (Source_File_List) loop
            if File_Name  (TSFL.Data (Source_File_Node).File_Name.all)
              = (Name) then
               Found := True;
            end if;
            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
         return Found;
      end Source_File_In_List;

      ------------------------------
      -- Is_Defined_In_Subprogram --
      ------------------------------

      function Is_Defined_In_Subprogram
        (Entity          : String;
         Short_Entity    : String;
         Package_Name    : String) return Boolean is
      begin
         --  check if the short name of the entity starts right
         --  after the package name + "."
         if not (Get_String_Index (Entity, 1, To_Lower (Package_Name)) +
                   Package_Name'Length + 1
           < Get_String_Index (Entity, 1, Short_Entity)) and
         --  and that it is really the name at the end of the
         --  entity name, followed by nothing
           Entity'Last = (Get_String_Index (Entity, 1, Short_Entity)) +
           Short_Entity'Last - 1
         then
            return False;
         else
            return True;
         end if;
      end Is_Defined_In_Subprogram;

      ------------------------------
      -- Get_Full_Entity_Filename --
      ------------------------------

      function Get_Full_Entity_Filename
        (Filename         : String) return String
      --  tries to find the file in the list and if found, it returns it
      --  with all his path in front of the name
      is
         Source_File_Node : Type_Source_File_List.List_Node;
      begin
         Source_File_Node := TSFL.First (Source_File_List);
         for J in 1 .. TSFL.Length (Source_File_List) loop
            if File_Name (TSFL.Data (Source_File_Node).File_Name.all) =
              Filename then
               return TSFL.Data (Source_File_Node).File_Name.all;
            end if;
            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
         Put_Line ("!!!Error: File not found in List, cannot return" &
                   " the name with the path!");
         return "";
      end Get_Full_Entity_Filename;

      ----------------------------------
      --  Is_Constant_Or_Named_Number --
      ----------------------------------

      function Is_Constant_Or_Named_Number
        (Header : String) return Boolean is
         --  returns true if the found header is really a constant or named
         --  number definition: is a ":=" can be found
      begin
         if Get_String_Index (Header, 1, ":=") > 0 then
            return True;
         else
            return False;
         end if;
      end Is_Constant_Or_Named_Number;

      ----------------------
      -- Get_Whole_Header --
      ----------------------

      function Get_Whole_Header
        (Source_Filename : String;
         Line            : Natural;
         Is_Generic      : Boolean) return String is
         --  returns one string with all the header of the entity

         Line_Nr     : Natural;
         Open_Braces : Integer;
         Column_Nr   : Natural;
         Result      : ASU.Unbounded_String;
         New_Line    : ASU.Unbounded_String;
         Is_Record   : Boolean;
      begin
         Line_Nr     := Line;
         Is_Record   := False;
         Open_Braces := 0;
         Result      := ASU.To_Unbounded_String ("");

         --  process the lines of the subprogram header
         while True loop
            New_Line := ASU.To_Unbounded_String (Get_Line_From_File
                                                   (Source_Filename,
                                                    Line_Nr));

            if Is_Generic then
               Column_Nr := Get_String_Index (To_Lower
                                             (ASU.To_String (New_Line)), 1,
                                              "generic");
               if Column_Nr > 0 and
                 (ASU.Length (New_Line) = Column_Nr + 6)
               then
                  if (Get_String_Index (To_Lower (ASU.To_String (New_Line)),
                                        1,
                                        "--") = 0) or
                    (Get_String_Index (To_Lower (ASU.To_String (New_Line)),
                                        1,
                                        "--") > Column_Nr) then
                     ASU.Append (New_Line, ASCII.LF);
                     ASU.Append (New_Line, Result);
                     Result := New_Line;
                     return ASU.To_String (Result);
                  end if;
               elsif Column_Nr > 0 and
                  (ASU.Element (New_Line, Column_Nr + 7) = ' '
                  or ASU.Element (New_Line, Column_Nr + 7) = ASCII.HT
                  or ASU.Element (New_Line, Column_Nr + 7) = ASCII.LF
                  or ASU.Element (New_Line, Column_Nr + 7) = ASCII.CR) then
                  if (Get_String_Index (To_Lower (ASU.To_String (New_Line)),
                                        1,
                                        "--") = 0) or
                    (Get_String_Index (To_Lower (ASU.To_String (New_Line)),
                                        1,
                                        "--") > Column_Nr) then
                     ASU.Append (New_Line, ASCII.LF);
                     ASU.Append (New_Line, Result);
                     Result := New_Line;
                     return ASU.To_String (Result);
                  end if;
               end if;
               Line_Nr := Line_Nr - 1;
               ASU.Append (New_Line, ASCII.LF);
               ASU.Append (New_Line, Result);
               Result := New_Line;
                  --  if not generic
                  --  check if "record_" found (with a space after!)
            else

               if Get_String_Index (To_Lower (ASU.To_String (New_Line)),
                                    1,
                                     " record") > 0
               then
                  if Is_Record and
                    Get_String_Index (To_Lower (ASU.To_String (New_Line)),
                                      1,
                                      " end") > 0 then
                     ASU.Append (Result, New_Line);
                     return ASU.To_String (Result);
                  else
                     Is_Record := True;
                  end if;
               end if;

               if not Is_Record then
                  for J in 1 .. ASU.Length (New_Line) loop
                     if ASU.Element (New_Line, J) = '(' then
                        Open_Braces := Open_Braces + 1;
                     elsif ASU.Element (New_Line, J) = ')' then
                        Open_Braces := Open_Braces - 1;
                     elsif ASU.Element (New_Line, J) = ';' then
                        if Open_Braces < 1 then
                           ASU.Append (Result, New_Line);
                           return ASU.To_String (Result);
                        end if;
                     end if;
                  end loop;
               end if;

               Line_Nr := Line_Nr + 1;
               ASU.Append (Result, New_Line);
               ASU.Append (Result, ASCII.LF);
            end if;
         end loop;
         return "";
      end Get_Whole_Header;

      ------------------------
      -- Process_Subprogram --
      ------------------------

      procedure Process_Subprogram
        (Source_Filename : String;
         Entity_File     : String;
         Info            : Entity_Information) is

         Local_Ref_List  : Type_Reference_List.List;
         Reference_Iter  : Entity_Reference_Iterator;
         Reference_Node  : Reference_List_Information;
         Line            : ASU.Unbounded_String;

      begin

         Entity_Node.Line_In_Body := 1;

         --  only if the procedure is defined in this file:
         --  find all positions where the procedure is called
         if File_Name (Entity_Node.File_Name.all) =
           File_Name (Source_Filename)
         and Options.References
         then
            Find_All_References (Project_Tree,
                                 Handler,
                                 Info,
                                 Source_Info_List,
                                 Reference_Iter,
                                 Project_View,
                                 False);

            --  loop for all references found for this subprogram
            while Get (Reference_Iter) /= No_Reference loop

               --  if the file, where the declaration of the reference
               --  can be found is known =>
               if Source_File_In_List
                 (Get_File (Get_Location (Get (Reference_Iter)))) then

                  --  get the line with this reference entity from its
                  --  source file
                  Line :=
                    ASU.To_Unbounded_String
                      (To_Lower
                           (Get_Line_From_File
                                (Get_Full_Entity_Filename
                                     (Get_File
                                          (Get_Location
                                             (Get (Reference_Iter)))),
                                 Get_Line (Get_Location
                                             (Get (Reference_Iter))))));

                  --  check if the reference found is not a call of the
                  --  the subprogram but its definition in the body
                  --  for this look for the words "procedure" or "function"
                  --  in this line (better function needed?)
                  if ASU.Index (Line, "procedure") > 0 or
                    ASU.Index (Line, "function") > 0 then

                     --  add the line where the subprogram can be
                     --  found in the body file
                     Entity_Node.Line_In_Body :=
                       Get_Line (Get_Location (Get (Reference_Iter)));

                     --  if it is only the identifier behind the "end"
                     --  of its definition  => ignore!
                     --  if no "end" found => it is really a call of it
                     --  => add reference to list
                  elsif ASU.Index (Line, "end") = 0 then

                     if Options.Info_Output then
                        Put_Line ("Ref="
                                    & Get_File
                                      (Get_Location
                                           (Get (Reference_Iter)))
                                    & Get_Line
                                      (Get_Location
                                           (Get (Reference_Iter)))'Img
                                    & Get_Column
                                      (Get_Location
                                           (Get (Reference_Iter)))'Img);
                     end if;

                     Reference_Node.File_Name  :=
                       new String'(Get_File
                                     (Get_Location (Get (Reference_Iter))));
                     Reference_Node.Line       :=
                       Get_Line   (Get_Location (Get (Reference_Iter)));
                     Reference_Node.Column     :=
                       Get_Column (Get_Location (Get (Reference_Iter)));
                     Reference_Node.File_Found := True;

                     Type_Reference_List.Append (Local_Ref_List,
                                                 Reference_Node);
                  end if;
               --  if the file is not known: add without testing,
               --  but set File_Found := false
               else
                  Reference_Node.File_Name  :=
                    new String'(Get_File
                                  (Get_Location (Get (Reference_Iter))));
                  Reference_Node.Line       :=
                    Get_Line   (Get_Location (Get (Reference_Iter)));
                  Reference_Node.Column     :=
                    Get_Column (Get_Location (Get (Reference_Iter)));
                  Reference_Node.File_Found := False;

                  Type_Reference_List.Append (Local_Ref_List, Reference_Node);
               end if;
               Next (Handler, Reference_Iter, Source_Info_List);
            end loop;

            Entity_Node.Ref_List := Local_Ref_List;

         end if;

         --  if defined in a .ads file => add entity to the Type_Index_List
         if  File_Extension (File_Name (Source_Filename)) = ".ads"
           and Source_Filename = Entity_File then
            Type_Entity_List.Append (Subprogram_Index_List, Entity_Node);
         end if;

      end Process_Subprogram;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type
        (Source_Filename : String;
         Entity_File     : String) is
      begin

         Entity_Node.Kind   := Type_Entity;

         --  if defined in a .ads file => add to the Type_Index_List
         if File_Extension (File_Name (Source_Filename)) = ".ads"
           and Source_Filename = Entity_File then
            Type_Entity_List.Append (Type_Index_List, Entity_Node);
         end if;

      end Process_Type;

      ----------------------
      -- Process_One_File --
      ----------------------

   begin
      Load_LI_File
        (Source_Info_List, Handler, Project_View,
         File_Name (Source_Filename),
         LI_Unit);

      if Options.Info_Output then
         Put_Line ("Find all possible declarations");
      end if;

      --  get all entities of the file
      if LI_Unit /= No_LI_File then
         Entity_Iter := Find_All_Possible_Declarations (LI_Unit, "");
      else
         Put_Line ("LI file not found");  --  later Exception?
      end if;

      --  get next entity from the file
      while not At_End (Entity_Iter) loop
         Info := Get (Entity_Iter);

         if Get_Scope (Info) = Global_Scope then
            Entity_Node.Is_Private := False;
         else
            Entity_Node.Is_Private := True;
         end if;

         --  check if the entity in defined within a subprogram.
         --  if true => ignore!
         if not Is_Defined_In_Subprogram (Get_Full_Name (Info, LI_Unit, "."),
                                          Get_Name (Info),
                                          Package_Name)
         --  AND check if the declaration of the entity is in one of the files
         --  which are in list, if false => no need for creating links
         --  => ignore!
           and Source_File_In_List (Get_Declaration_File_Of (Info))
         --  AND check if it's a private entity and if they should be processed
           and (Options.Show_Private or not Entity_Node.Is_Private)
         then

            --  Info_Output is set, if further information are wished
            if Options.Info_Output then
               Put_Line ("-----");
               Put_Line ("Entity found: " &
                          Kind_To_String (Get_Kind (Info)) &
                        ": " & Get_Full_Name (Info, LI_Unit, ".") &
                        "In File: " & Source_Filename &
                        "  defined in  file: " &
                          Get_Full_Entity_Filename
                             (Get_Declaration_File_Of (Info)) &
                         ", in line: " &
                           Get_Declaration_Line_Of (Info)'Img &
                         ", in column: " &
                          Get_Declaration_Column_Of (Info)'Img);
            end if;

            --  get the parameters needed be all entities
            Entity_Node.Name            :=
              new String'(Get_Full_Name (Info, LI_Unit, "."));
            Entity_Node.Short_Name      :=
              new String'(Get_Name (Info));
            Entity_Node.File_Name       :=
              new String'(Get_Full_Entity_Filename
                             (Get_Declaration_File_Of (Info)));
            Entity_Node.Column          := Get_Declaration_Column_Of (Info);
            Entity_Node.Line            := Get_Declaration_Line_Of (Info);
            if Get_Kind (Info) = Generic_Function_Or_Operator or
              Get_Kind (Info) = Generic_Procedure then
               Entity_Node.Header := new String '(Get_Whole_Header
                                                    (Entity_Node.File_Name.all,
                                                     Entity_Node.Line,
                                                     True));
            else
               Entity_Node.Header := new String '(Get_Whole_Header
                                                    (Entity_Node.File_Name.all,
                                                     Entity_Node.Line,
                                                     False));
            end if;
            Entity_Node.Header_Lines    :=
              Count_Lines (Entity_Node.Header.all);
            Entity_Node.File_Found      := True;

            --  get the entity specific parameters
            case Get_Kind (Info) is
               when Non_Generic_Function_Or_Operator |
                    Generic_Function_Or_Operator =>
                  Entity_Node.Kind := Function_Entity;
                  Process_Subprogram
                    (Source_Filename,
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)),
                     Info);
               when Non_Generic_Procedure |
                    Generic_Procedure =>
                  Entity_Node.Kind := Procedure_Entity;
                  Process_Subprogram
                    (Source_Filename,
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)),
                     Info);
               when Record_Type | Enumeration_Type |
                    Access_Type | Array_Type |
                      Boolean_Type | String_Type | Class_Wide_Type |
                        Decimal_Fixed_Point_Type |
                          Floating_Point_Type | Modular_Integer_Type |
                            Ordinary_Fixed_Point_Type |
                              Private_Type | Protected_Type |
                                Signed_Integer_Type | Task_Type
                  => Process_Type
                    (Source_Filename,
                     Get_Full_Entity_Filename
                       (Get_Declaration_File_Of (Info)));
               when Exception_Entity =>
                  Entity_Node.Kind := Exception_Entity;
               when Array_Object | Boolean_Object | Enumeration_Object |
                        Floating_Point_Object |
                        Modular_Integer_Object |  Protected_Object |
                        Record_Object | Signed_Integer_Object |
                        String_Object | Task_Object |
                        Access_Object | Class_Wide_Object |
                        Ordinary_Fixed_Point_Object =>
                  if Is_Constant_Or_Named_Number (Entity_Node.Header.all) then
                     Entity_Node.Kind := Var_Entity;
                  else
                     Entity_Node.Kind := Other_Entity;
                  end if;
               when Generic_Package  =>
                  Entity_Node.Kind := Package_Entity;
               when  Non_Generic_Package =>
                  Entity_Node.Kind := Package_Entity;
               when others => Entity_Node.Kind := Other_Entity;
            end case;

            --  add to the entity list of this file
            Type_Entity_List.Append (Entity_List, Entity_Node);

         end if;
         --  get next entity in this file
         Next (Entity_Iter);
      end loop;

      Destroy (Entity_Iter);
      Destroy (Info);
      --  process the documentation of this file
      Process_Source (Doc_File,
                      First_File,
                      Last_File,
                      Next_Package,
                      Prev_Package,
                      Source_File_List,
                      Source_Filename,
                      Package_Name,
                      Def_In_Line,
                      Entity_List,
                      Process_Body_File,
                      Options);
   end Process_One_File;

   --------------------
   -- Kind_To_String --
   --------------------

   function Kind_To_String (Kind : Src_Info.E_Kind) return String is
   begin
      --  ??? Would be nice to do it as a primitive subprogram of the
      --  LI_Handlers, unfortunately they currently don't have access to
      --  Glide_Intl for proper translations.

      case Kind is
         when Overloaded_Entity            => return "???";
         when Unresolved_Entity            => return "unknown";
         when Access_Object                =>
            return "access variable / pointer";
         when Access_Type                  => return "access type / pointer";
         when Array_Object                 => return "array";
         when Array_Type                   => return "array type";
         when Boolean_Object               => return "boolean";
         when Boolean_Type                 => return "boolean type";
         when Class_Wide_Object            => return "class wide";
         when Class_Wide_Type              => return "class wide type";
         when Decimal_Fixed_Point_Object   => return "decimal fixed point";
         when Decimal_Fixed_Point_Type     =>
            return "decimal fixed point type";
         when Entry_Or_Entry_Family        => return "entry or entry family";
            --  later
         when Enumeration_Literal          => return "enumeration literal";
            --  not used
         when Enumeration_Object           => return "enumeration";
         when Enumeration_Type             => return "enumeration type";
         when Exception_Entity             => return "exception";
         when Floating_Point_Object        => return "floating point";
         when Floating_Point_Type          => return "floating point type";
         when Generic_Class                => return "generic class";
            --  later
         when Generic_Function_Or_Operator => return "generic function";
         when Generic_Package              => return "generic package";
            --  later
         when Generic_Procedure            => return "generic procedure";
         when Label_On_Block               => return "label on block";
            --  not used
         when Label_On_Loop                => return "label on loop";
            --  not used
         when Label_On_Statement           => return "label on statement";
            --  not used
         when Modular_Integer_Object       => return "modular integer";
         when Modular_Integer_Type         => return "modular integer type";
         when Named_Number                 => return "named number";
         when Non_Generic_Function_Or_Operator => return "function";
         when Non_Generic_Package          => return "package";
            --  later
         when Non_Generic_Procedure        => return "procedure";
         when Ordinary_Fixed_Point_Object  => return "fixed point";
         when Ordinary_Fixed_Point_Type    => return "fixed point type";
         when Private_Type                 => return "private type";
         when Protected_Object             => return "protected object";
         when Protected_Type               => return "protected type";
         when Record_Object                => return "record / struct";
         when Record_Type                  => return "record type / struct";
         when Signed_Integer_Object        => return "signed integer";
         when Signed_Integer_Type          => return "signed integer type";
         when String_Object                => return "string";
         when String_Type                  => return "string type";
         when Task_Object                  => return "task";
         when Task_Type                    => return "task type";
      end case;
   end Kind_To_String;

end Work_on_File;
