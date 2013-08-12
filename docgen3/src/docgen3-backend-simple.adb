------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Docgen3.Comment;         use Docgen3.Comment;
with Docgen3.Time;            use Docgen3.Time;
with Docgen3.Utils;           use Docgen3.Utils;
with Language;                use Language;
with Language.Ada;            use Language.Ada;
with Language.C;
with Language.Cpp;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with String_Utils;            use String_Utils;
with Templates_Parser;        use Templates_Parser;
with Traces;                  use Traces;
with Xref.Docgen;             use Xref.Docgen;

package body Docgen3.Backend.Simple is
   Me : constant Debug_Handle := Create ("Docgen3.1-Backend");

   type Template_Kind is
     (Tmpl_Entities,        --  Entities index
      Tmpl_Files_Index,     --  Files index
      Tmpl_Global_Index     --  Global index
     );

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Clear (Entities : in out Collected_Entities);
   --  Clear all the lists used to classify the tree nodes in categories

   function Get_Name
     (E : Entity_Id; Use_Full_Name : Boolean := False) return String;
   --  Return the name of an entity. When Use_Full_Name is false, for Ada
   --  operators return the name enclosed in string quotes (ie. "+").

   function Get_Unique_Name (E : Entity_Id) return String;
   --  For types return the full name of E; for subprograms return the full
   --  name of E concatenated with the line where E is defined (to handle
   --  overloaded entities defined in other files ---for example, entities
   --  inherited from a parent type defined in another file).

   function Get_Template
     (System_Dir : Virtual_File;
      Kind       : Template_Kind) return Virtual_File;

   function To_Destination_Name
     (Basename : Filesystem_String) return Filesystem_String;

   ----------
   -- ReST --
   ----------

   --  Local package which generates the ReST output

   package ReST is

      procedure Append
        (Printout : access Unbounded_String;
         Text     : String);
      --  Append Text to Printout

      procedure Append_Line
        (Printout : access Unbounded_String;
         Text     : String);
      --  Append Text to Printout plus ASCII.LF

      procedure ReST_Append_Comment
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the comment of E

      procedure ReST_Append_Label
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the reStructured output of a label for E

      type List_Filter_Kind is (None, Lang_Ada, Lang_C_CPP);

      procedure ReST_Append_List
        (Printout      : access Unbounded_String;
         List          : EInfo_List.Vector;
         Header        : String;
         Use_Full_Name : Boolean := False;
         Filter        : List_Filter_Kind := None);
      --  Append to Printout the Header plus the reStructured Text of all the
      --  elements of List whose language is covered by the Filter (if the
      --  filter is none then all entities are appended to Printout). If no
      --  entity of List is covered by Filter then the Header is not added
      --  to Printout.

      procedure ReST_Append_Record_Type_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the reStructured output of a record type

      procedure ReST_Append_Reference
        (Printout : access Unbounded_String;
         Entity   : Entity_Id;
         Prefix   : String;
         Suffix   : String := "");
      --  Append to Printout a reference to entity E

      procedure ReST_Append_Simple_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the reStructured output of a variable or a type

      procedure ReST_Append_Src
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the source of E

      procedure ReST_Append_Subprogram
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the reStructured output of a subprogram

      function ReST_Label
        (E : Entity_Id) return String;
      --  Returns the unique reStructured label associated with E

      function ReST_Label
        (Filename : String) return String;
      --  Returns the reStructured label associated with Filename

      function To_ReST
        (Comment : Structured_Comment) return Unbounded_String;
      --  Convert the structured comment to the reStructured output

   end ReST;

   ----------
   -- ReST --
   ----------

   package body ReST is

      function Append_Tab (Text : Unbounded_String) return Unbounded_String;
      --  Append spaces after each ASCII.LF

      ------------
      -- Append --
      ------------

      procedure Append
        (Printout : access Unbounded_String;
         Text     : String) is
      begin
         Printout.all := Printout.all & Text;
      end Append;

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line
        (Printout : access Unbounded_String;
         Text     : String) is
      begin
         Append (Printout, Text & ASCII.LF);
      end Append_Line;

      ----------------
      -- Append_Tab --
      ----------------

      function Append_Tab (Text : Unbounded_String) return Unbounded_String
      is
         Tab    : constant String := "   ";
         Result : Unbounded_String;
         C      : Character;
      begin
         Append (Result, Tab);

         for J in 1 .. Length (Text) loop
            C := Element (Text, J);
            Append (Result, C);

            if C = ASCII.LF then
               Append (Result, Tab);
            end if;
         end loop;

         return Result;
      end Append_Tab;

      -------------------------
      -- ReST_Append_Comment --
      -------------------------

      procedure ReST_Append_Comment
        (Printout : access Unbounded_String;
         E        : Entity_Id)
      is
         Comment_Str : constant String :=
                         To_String (To_ReST (Get_Comment (E)));
      begin
         if Comment_Str /= "" then
            Append_Line (Printout, "");
            Append_Line (Printout, Comment_Str);
         end if;
      end ReST_Append_Comment;

      -----------------------
      -- ReST_Append_Label --
      -----------------------

      procedure ReST_Append_Label
        (Printout : access Unbounded_String;
         E        : Entity_Id) is
      begin
         Append_Line (Printout, ReST_Label (E));
         Append_Line (Printout, "");
      end ReST_Append_Label;

      ----------------------
      -- ReST_Append_List --
      ----------------------

      procedure ReST_Append_List
        (Printout      : access Unbounded_String;
         List          : EInfo_List.Vector;
         Header        : String;
         Use_Full_Name : Boolean  := False;
         Filter        : List_Filter_Kind := None)
      is
         Cursor : EInfo_List.Cursor;
         E      : Entity_Id;
         Found  : Boolean;

      begin
         if not EInfo_List.Has_Element (List.First) then
            return;
         end if;

         --  Check if there is some entity passing the filter; otherwise no
         --  output is generated.

         if Filter /= None then
            Found := False;
            Cursor := List.First;
            while EInfo_List.Has_Element (Cursor) loop
               E := EInfo_List.Element (Cursor);

               if (Filter = Lang_Ada and then In_Ada_Language (E))
                 or else
                   (Filter = Lang_C_CPP and then In_C_Or_CPP_Language (E))
               then
                  Found := True;
                  exit;
               end if;

               EInfo_List.Next (Cursor);
            end loop;

            if not Found then
               return;
            end if;
         end if;

         Append_Line (Printout, "- " & Header);

         Cursor := List.First;
         while EInfo_List.Has_Element (Cursor) loop
            E := EInfo_List.Element (Cursor);

            if Filter = None
              or else (Filter = Lang_Ada and then In_Ada_Language (E))
              or else (Filter = Lang_C_CPP and then In_C_Or_CPP_Language (E))
            then
               Append (Printout, "   * :ref:`");
               Append (Printout, Get_Name (E, Use_Full_Name));

               Append_Line (Printout,
                            " <"
                            & Get_Unique_Name (E)
                            & ">` "
                            & Image (LL.Get_Location (E)));
            end if;

            EInfo_List.Next (Cursor);
         end loop;

         Append_Line (Printout, "");
      end ReST_Append_List;

      -----------------------------------------
      -- ReST_Append_Record_Type_Declaration --
      -----------------------------------------

      procedure ReST_Append_Record_Type_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id)
      is
         Name   : constant String := Get_Short_Name (E);
         Header : constant String (Name'Range) := (others => '=');

      begin
         ReST_Append_Label (Printout, E);

         Append_Line (Printout, Name);
         Append_Line (Printout, Header);
         Append_Line (Printout, "");

         if not Is_Partial_View (E) then
            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

         else
            Append_Line (Printout, "**Partial View:**");

            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

            Append_Line (Printout, "**Full View:**");
            Append_Line (Printout, "");
            Append_Line (Printout, ".. code-block:: ada");
            Append_Line (Printout, "");
            Append_Line (Printout,
                         To_String (Append_Tab (Get_Full_View_Src (E))));
            Append_Line (Printout, "");

            declare
               Comment_Str : constant String :=
                 To_String (To_ReST (Get_Full_View_Comment (E)));
            begin
               if Comment_Str /= "" then
                  Append_Line (Printout, "");
                  Append_Line (Printout, Comment_Str);
               end if;
            end;
         end if;

         if Is_Tagged_Type (E)
           or else Get_Kind (E) = E_Class
         then
            if In_Ada_Language (E) then
               if Present (Get_Parent (E)) then
                  Append_Line (Printout, "- Parent");

                  ReST_Append_Reference
                    (Printout => Printout,
                     Entity   => Get_Parent (E),
                     Prefix   => "   * ");

                  Append_Line (Printout, "");
               end if;

               ReST_Append_List
                 (Printout => Printout,
                  List     => Get_Progenitors (E).all,
                  Header   => "Progenitors");

            else
               ReST_Append_List
                 (Printout => Printout,
                  List     => LL.Get_Parent_Types (E).all,
                  Header   => "Parent types");
            end if;
         end if;

         ReST_Append_List
           (Printout => Printout,
            List     => LL.Get_Child_Types (E).all,
            Header   => "Child types");

         if In_Ada_Language (E)
           and then LL.Has_Methods (E)
         then
            EInfo_Vector_Sort_Short.Sort (Get_Inherited_Methods (E).all);

            ReST_Append_List
              (Printout => Printout,
               List     => Get_Inherited_Methods (E).all,
               Header   => "Inherited dispatching subprograms");

            EInfo_Vector_Sort_Short.Sort (Get_Methods (E).all);

            ReST_Append_List
              (Printout => Printout,
               List     => Get_Methods (E).all,
               Header   => "New and overridden dispatching subprograms");
         end if;
      end ReST_Append_Record_Type_Declaration;

      ---------------------------
      -- ReST_Append_Reference --
      ---------------------------

      procedure ReST_Append_Reference
        (Printout : access Unbounded_String;
         Entity   : Entity_Id;
         Prefix   : String;
         Suffix   : String := "") is
      begin
         Append (Printout, Prefix & " :ref:`");
         Append (Printout,
           Get_Name (Entity,
             Use_Full_Name =>
               Get_Language (Entity).all in Language.Cpp.Cpp_Language'Class));

         Append_Line (Printout,
           " <"
           & Get_Unique_Name (Entity)
           & ">` "
           & Image (LL.Get_Location (Entity))
           & Suffix);
      end ReST_Append_Reference;

      ------------------------------------
      -- ReST_Append_Simple_Declaration --
      ------------------------------------

      procedure ReST_Append_Simple_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id)
      is
         Name   : constant String := Get_Short_Name (E);
         Header : constant String (Name'Range) := (others => '=');

      begin
         ReST_Append_Label (Printout, E);

         Append_Line (Printout, Name);
         Append_Line (Printout, Header);
         Append_Line (Printout, "");

         ReST_Append_Src (Printout, E);
         ReST_Append_Comment (Printout, E);
      end ReST_Append_Simple_Declaration;

      ---------------------
      -- ReST_Append_Src --
      ---------------------

      procedure ReST_Append_Src
        (Printout : access Unbounded_String;
         E        : Entity_Id)
      is
      begin
         if Get_Src (E) /= Null_Unbounded_String then
            Append_Line (Printout, "");

            if In_Ada_Language (E) then
               Append_Line (Printout, ".. code-block:: ada");

            --  We cannot reliably differentiate C/C++ sources in header files.
            --  Hence we use C++ for both cases.

            elsif In_C_Or_CPP_Language (E) then
               Append_Line (Printout, ".. code-block:: c++");
            end if;

            Append_Line (Printout, "");
            Append_Line (Printout, To_String (Append_Tab (Get_Src (E))));
            Append_Line (Printout, "");
         end if;
      end ReST_Append_Src;

      ----------------------------
      -- ReST_Append_Subprogram --
      ----------------------------

      procedure ReST_Append_Subprogram
        (Printout : access Unbounded_String;
         E        : Entity_Id)
      is
         Name   : constant String :=
                    Get_Name (E,
                      Use_Full_Name =>
                        In_C_Or_CPP_Language (E) and then LL.Is_Primitive (E));
         Header : constant String (Name'Range) := (others => '=');

      begin
         if Get_Src (E) /= Null_Unbounded_String then
            ReST_Append_Label (Printout, E);

            Append_Line (Printout, Name);
            Append_Line (Printout, Header);
            Append_Line (Printout, "");

            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);
         end if;
      end ReST_Append_Subprogram;

      -------------
      -- To_ReST --
      -------------

      function To_ReST
        (Comment : Structured_Comment) return Unbounded_String
      is
         Result : Unbounded_String := Null_Unbounded_String;

         procedure Add (Tag_Info : Tag_Info_Ptr);
         --  Append to Result the contents of Tag_Info

         ---------
         -- Add --
         ---------

         procedure Add (Tag_Info : Tag_Info_Ptr) is
         begin
            if Tag_Info.Tag = Null_Unbounded_String then
               Result := Result
                 & Trim (Reduce (To_String (Tag_Info.Text)),
                         Ada.Strings.Left) & ASCII.LF
                 & ASCII.LF;
            else
               if Tag_Info.Tag = "seealso" then
                  Result := Result
                    & "**" & Tag_Info.Tag & "**" & ASCII.LF
                    & " :ref:`" & Tag_Info.Attr & "`" & ASCII.LF
                    & ASCII.LF;

               elsif Tag_Info.Tag = "param" then
                  if Tag_Info.Text /= Null_Unbounded_String then
                     Result := Result
                       & "**" & Tag_Info.Tag & "** "
                       & Tag_Info.Attr & ASCII.LF
                       & ASCII.LF
                       & Trim (Reduce (To_String (Tag_Info.Text)),
                               Ada.Strings.Left) & ASCII.LF
                       & ASCII.LF;
                  end if;

               else
                  Result := Result
                    & "**" & Tag_Info.Tag & "** "
                    & Tag_Info.Attr & ASCII.LF
                    & ASCII.LF
                    & Trim (Reduce (To_String (Tag_Info.Text)),
                            Ada.Strings.Left) & ASCII.LF
                    & ASCII.LF;
               end if;
            end if;
         end Add;

         --  Local variables

         C        : Tag_Cursor;
         Tag_Info : Tag_Info_Ptr;

      --  Start of processing for To_ReST

      begin
         if No (Comment) then
            return Null_Unbounded_String;
         end if;

         C := New_Cursor (Comment);
         while not At_End (C) loop
            Tag_Info := Get (C);
            Add (Tag_Info);
            Next (C);
         end loop;

         return Result;
      end To_ReST;

      ----------------
      -- ReST_Label --
      ----------------

      function ReST_Label
        (E : Entity_Id) return String is
      begin
         return ".. _" & Get_Unique_Name (E) & ":";
      end ReST_Label;

      ----------------
      -- ReST_Label --
      ----------------

      function ReST_Label
        (Filename : String) return String is
      begin
         return ".. _" & Filename & ":";
      end ReST_Label;

   end ReST;
   use ReST;

   -----------
   -- Clear --
   -----------

   procedure Clear (Entities : in out Collected_Entities) is
   begin
      Entities.Pkgs.Clear;
      Entities.Variables.Clear;
      Entities.Access_Types.Clear;
      Entities.Simple_Types.Clear;
      Entities.Record_Types.Clear;
      Entities.Tagged_Types.Clear;
      Entities.Interface_Types.Clear;
      Entities.Subprgs.Clear;
      Entities.Methods.Clear;

      Entities.CPP_Classes.Clear;
      Entities.CPP_Constructors.Clear;
   end Clear;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Backend : in out Simple_Backend;
      Update_Global_Index : Boolean)
   is
      procedure Generate_Global_Indexes;

      -----------------------------
      -- Generate_Global_Indexes --
      -----------------------------

      procedure Generate_Global_Indexes is

         procedure Generate_Entities_Index
           (Filename : String;
            Header   : String;
            Filter   : List_Filter_Kind);
         --  Generate the ReST file Filename with this Header containing an
         --  index with all the entities of the project which pass the Filter.

         procedure Generate_Files_Index
           (Filename  : String;
            Header    : String;
            Src_Files : Files_List.Vector);
         --  Generate the ReST file Filename with this Header containing an
         --  index with all the files of Src_Files.

         procedure Generate_Instances_Index
           (List      : access EInfo_List.Vector;
            Filename  : String;
            Header    : String);
         --  Generate the ReST file Filename with this Header containing an
         --  index with the tree of dependencies of Ada tagged types.

         procedure Generate_Tagged_Types_Tree_Index
           (Filename  : String;
            Header    : String);
         --  Generate the ReST file Filename with this Header containing an
         --  index with the tree of dependencies of Ada tagged types.

         function Get_Ada_Src_Files return Files_List.Vector;
         --  Return the list of Ada source files of the project

         function Get_C_And_CPP_Src_Files return Files_List.Vector;
         --  Return the list of C & C++ source files of the project

         function Has_Instances (List : EInfo_List.Vector) return Boolean;
         --  Return true if List has the instance of some generic

         -----------------------
         -- Generate_Entities --
         -----------------------

         procedure Generate_Entities_Index
           (Filename : String;
            Header   : String;
            Filter   : List_Filter_Kind)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
              Get_Template
                (Get_Share_Dir (Backend.Context.Kernel), Tmpl_Entities);

         begin
            Printout :=
              Printout
              & Header   & ASCII.LF
              & Header_U & ASCII.LF
              & ASCII.LF;

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Pkgs,
               "Packages",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Variables,
               "Constants & variables",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Simple_Types,
               "Simple types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Access_Types,
               "Access types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Record_Types,
               "Record types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Subprgs,
               "Subprograms",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Tagged_Types,
               "Tagged types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.CPP_Classes,
               "C++ Classes",
               Filter => Filter);

            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_Destination_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Entities_Index;

         --------------------
         -- Generate_Files --
         --------------------

         procedure Generate_Files_Index
           (Filename  : String;
            Header    : String;
            Src_Files : Files_List.Vector)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Printout_H  : aliased Unbounded_String;
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
                            Get_Template
                              (Get_Share_Dir (Backend.Context.Kernel),
                               Tmpl_Files_Index);
            File        : GNATCOLL.VFS.Virtual_File;
            File_Index  : Files_List.Cursor;

         begin
            Printout_H :=
              Printout
              & Header & ASCII.LF
              & Header_U & ASCII.LF;

            File_Index := Src_Files.First;
            while Files_List.Has_Element (File_Index) loop
               File := Files_List.Element (File_Index);

               Printout :=
                 Printout
                 & "   " & (+File.Base_Name) & ASCII.LF;

               Files_List.Next (File_Index);
            end loop;

            Insert
              (Translation, Assoc ("HEADER", Printout_H));
            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_Destination_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Files_Index;

         ------------------------------
         -- Generate_Instances_Index --
         ------------------------------

         procedure Generate_Instances_Index
           (List     : access EInfo_List.Vector;
            Filename : String;
            Header   : String)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Tmpl        : constant Virtual_File :=
                            Get_Template
                              (Get_Share_Dir (Backend.Context.Kernel),
                               Tmpl_Entities);
            Cursor1     : EInfo_List.Cursor;
            E1          : Entity_Id;
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;

         begin
            Append_Line (Printout'Access, Header);
            Append_Line (Printout'Access, Header_U);
            Append_Line (Printout'Access, "");

            Cursor1 := List.First;
            while EInfo_List.Has_Element (Cursor1) loop
               E1 := EInfo_List.Element (Cursor1);

               --  Locate a generic

               if LL.Is_Generic (E1) then
                  ReST_Append_Reference
                    (Printout => Printout'Access,
                     Entity   => E1,
                     Prefix   => "- ");
                  Append_Line (Printout'Access, "");

                  --  Append all its instantiations

                  declare
                     LL_E1   : constant General_Entity := LL.Get_Entity (E1);
                     Cursor2 : EInfo_List.Cursor := List.First;
                     E2      : Entity_Id;
                  begin
                     while EInfo_List.Has_Element (Cursor2) loop
                        E2 := EInfo_List.Element (Cursor2);

                        if Present (LL.Get_Instance_Of (E2))
                          and then LL.Get_Instance_Of (E2) = LL_E1
                        then
                           ReST_Append_Reference
                             (Printout => Printout'Access,
                              Entity   => E2,
                              Prefix   => "  - ");
                           Append_Line (Printout'Access, "");
                        end if;

                        EInfo_List.Next (Cursor2);
                     end loop;
                  end;
               end if;

               EInfo_List.Next (Cursor1);
            end loop;

            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_Destination_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Instances_Index;

         --------------------------------
         -- Generate_Tagged_Types_Tree --
         --------------------------------

         procedure Generate_Tagged_Types_Tree_Index
           (Filename : String;
            Header   : String)
         is
            Header_U : constant String (Header'Range) := (others => '=');
            Printout : aliased Unbounded_String;

            function Collect_Tagged_Types_And_Interfaces
              return EInfo_List.Vector;
            --  Returns a list containing all the tagged types and interfaces
            --  of the project

            function Get_Root_Types
              (List : EInfo_List.Vector) return EInfo_List.Vector;
            --  Return the root types of this list of tagged types

            procedure ReST_Append (E : Entity_Id; Prefix : String);
            --  Append to Printout the Prefix followed by E

            ---------------------------
            -- Get_Tagged_Type_Roots --
            ---------------------------

            function Collect_Tagged_Types_And_Interfaces
              return EInfo_List.Vector
            is
               Result : EInfo_List.Vector;
               Cursor : EInfo_List.Cursor;

            begin
               Cursor := Backend.Entities.Tagged_Types.First;
               while EInfo_List.Has_Element (Cursor) loop
                  Result.Append (EInfo_List.Element (Cursor));
                  EInfo_List.Next (Cursor);
               end loop;

               Cursor := Backend.Entities.Interface_Types.First;
               while EInfo_List.Has_Element (Cursor) loop
                  Result.Append (EInfo_List.Element (Cursor));
                  EInfo_List.Next (Cursor);
               end loop;

               pragma Assert (not Has_Duplicated_Entities (Result));
               return Result;
            end Collect_Tagged_Types_And_Interfaces;

            --------------------
            -- Get_Root_Types --
            --------------------

            function Get_Root_Types
              (List : EInfo_List.Vector) return EInfo_List.Vector
            is
               Root_Level : constant Natural := 0;
               Root_Types : EInfo_List.Vector;
               Cursor : EInfo_List.Cursor;
               E      : Entity_Id;

            begin
               --  First round: Collect root types available in the input list

               Cursor := List.First;
               while EInfo_List.Has_Element (Cursor) loop
                  E := EInfo_List.Element (Cursor);
                  pragma Assert (Is_Tagged_Type (E));

                  if Get_IDepth_Level (E) = Root_Level then
                     Root_Types.Append (E);
                  end if;

                  EInfo_List.Next (Cursor);
               end loop;

               --  Second round: Collect root types of other list components

               Cursor := List.First;
               while EInfo_List.Has_Element (Cursor) loop
                  E := EInfo_List.Element (Cursor);

                  if Get_IDepth_Level (E) /= Root_Level then
                     while Present (Get_Parent (E)) loop
                        E := Get_Parent (E);
                     end loop;

                     if not Root_Types.Contains (E) then
                        Root_Types.Append (E);
                     end if;
                  end if;

                  EInfo_List.Next (Cursor);
               end loop;

               return Root_Types;
            end Get_Root_Types;

            -----------------
            -- ReST_Append --
            -----------------

            procedure ReST_Append (E : Entity_Id; Prefix : String) is

               function Gen_Suffix return String;
               function Gen_Suffix return String is
               begin
                  if Get_Kind (E) = E_Interface then
                     return " *(Interface)*";
                  elsif LL.Is_Abstract (E) then
                     return " *(abstract)*";
                  else
                     return "";
                  end if;
               end Gen_Suffix;

               --  Local variables

               Suffix : constant String := Gen_Suffix;
               Cursor : EInfo_List.Cursor;
               Entity : Entity_Id;
            begin
               ReST_Append_Reference
                 (Printout => Printout'Access,
                  Entity   => E,
                  Prefix   => Prefix,
                  Suffix   => Suffix);
               Append_Line (Printout'Access, "");

               Cursor := Get_Derivations (E).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Entity := EInfo_List.Element (Cursor);
                  ReST_Append (Entity, "  " & Prefix);

                  EInfo_List.Next (Cursor);
               end loop;
            end ReST_Append;

            --  Local variables

            Tmpl   : constant Virtual_File :=
                       Get_Template
                         (Get_Share_Dir (Backend.Context.Kernel),
                          Tmpl_Entities);

            Cursor      : EInfo_List.Cursor;
            E           : Entity_Id;
            Root_Types  : EInfo_List.Vector;
            Translation : Translate_Set;

         begin
            Append_Line (Printout'Access, Header);
            Append_Line (Printout'Access, Header_U);
            Append_Line (Printout'Access, "");

            Root_Types := Get_Root_Types (Collect_Tagged_Types_And_Interfaces);

            EInfo_Vector_Sort_Short.Sort (Root_Types);

            Cursor := Root_Types.First;
            while EInfo_List.Has_Element (Cursor) loop
               E := EInfo_List.Element (Cursor);
               ReST_Append (E, "- ");
               EInfo_List.Next (Cursor);
            end loop;

            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_Destination_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Tagged_Types_Tree_Index;

         -----------------------
         -- Get_Ada_Src_Files --
         -----------------------

         function Get_Ada_Src_Files return Files_List.Vector is
            File       : GNATCOLL.VFS.Virtual_File;
            File_Index : Files_List.Cursor;
            Lang       : Language_Access;
            Result     : Files_List.Vector;

         begin
            File_Index := Backend.Src_Files.First;

            while Files_List.Has_Element (File_Index) loop
               File := Files_List.Element (File_Index);
               Lang :=
                 Backend.Context.Lang_Handler.Get_Language_From_File (File);

               if Lang.all in Language.Ada.Ada_Language'Class then
                  Result.Append (File);
               end if;

               Files_List.Next (File_Index);
            end loop;

            return Result;
         end Get_Ada_Src_Files;

         -----------------------------
         -- Get_C_And_CPP_Src_Files --
         -----------------------------

         function Get_C_And_CPP_Src_Files return Files_List.Vector is
            File       : GNATCOLL.VFS.Virtual_File;
            File_Index : Files_List.Cursor;
            Lang       : Language_Access;
            Result     : Files_List.Vector;

         begin
            File_Index := Backend.Src_Files.First;

            while Files_List.Has_Element (File_Index) loop
               File := Files_List.Element (File_Index);
               Lang :=
                 Backend.Context.Lang_Handler.Get_Language_From_File (File);

               if Lang.all in Language.C.C_Language'Class then
                  Result.Append (File);
               end if;

               Files_List.Next (File_Index);
            end loop;

            return Result;
         end Get_C_And_CPP_Src_Files;

         -------------------
         -- Has_Instances --
         -------------------

         function Has_Instances
           (List : EInfo_List.Vector) return Boolean
         is
            Cursor : EInfo_List.Cursor;
            E      : Entity_Id;

         begin
            Cursor := List.First;
            while EInfo_List.Has_Element (Cursor) loop
               E := EInfo_List.Element (Cursor);

               if Present (LL.Get_Instance_Of (E)) then
                  return True;
               end if;

               EInfo_List.Next (Cursor);
            end loop;

            return False;
         end Has_Instances;

         --  Local variables

         Printout      : aliased Unbounded_String;
         Translation   : Translate_Set;
         Tmpl          : constant Virtual_File :=
                           Get_Template
                             (Get_Share_Dir (Backend.Context.Kernel),
                              Tmpl_Global_Index);
         Src_Files     : aliased Files_List.Vector;
         My_Delay      : Delay_Time;
         Has_Ada_Files : Boolean := False;
         Has_C_Files   : Boolean := False;

         --  Start of processing for Generate_Global_Index

      begin
         Start (My_Delay);
         Trace (Me, "Generate_Global_Index");

         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Pkgs);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Variables);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Simple_Types);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Record_Types);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Subprgs);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Tagged_Types);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.CPP_Classes);

         Src_Files := Get_Ada_Src_Files;
         Has_Ada_Files := Natural (Src_Files.Length) > 0;

         if Has_Ada_Files then
            declare
               Filename : constant String := "ada_files_idx";
            begin
               Files_Vector_Sort.Sort (Src_Files);
               Generate_Files_Index
                 (Src_Files => Src_Files,
                  Filename  => Filename,
                  Header    => "Ada source files");
               Append_Line (Printout'Access, "   " & Filename);
            end;

            declare
               Filename : constant String := "ada_entities_idx";
            begin
               Generate_Entities_Index
                 (Filename => Filename,
                  Header   => "Ada entities",
                  Filter   => Lang_Ada);
               Append_Line (Printout'Access, "   " & Filename);
            end;

            if EInfo_List.Has_Element
                 (Backend.Entities.Tagged_Types.First)
            then
               declare
                  Filename : constant String := "ada_dep_tree_idx";
               begin
                  Generate_Tagged_Types_Tree_Index
                    (Filename => Filename,
                     Header   => "Ada derivations tree (tagged types)");
                  Append_Line (Printout'Access, "   " & Filename);
               end;
            end if;

            if Has_Instances (Backend.Entities.Pkgs) then
               declare
                  Filename : constant String := "ada_pkg_instances_idx";
               begin
                  Generate_Instances_Index
                    (List     => Backend.Entities.Pkgs'Access,
                     Filename => Filename,
                     Header   =>
                       "Ada generic packages and their instantiations");
                  Append_Line (Printout'Access, "   " & Filename);
               end;
            end if;

            if Has_Instances (Backend.Entities.Subprgs) then
               declare
                  Filename : constant String := "ada_subp_instances_idx";
               begin
                  Generate_Instances_Index
                    (List     => Backend.Entities.Subprgs'Access,
                     Filename => Filename,
                     Header   =>
                       "Ada generic subprograms and their instantiations");
                  Append_Line (Printout'Access, "   " & Filename);
               end;
            end if;

            Printout := Printout & ASCII.LF;
         end if;

         Src_Files := Get_C_And_CPP_Src_Files;
         Has_C_Files := Natural (Src_Files.Length) > 0;

         if Has_C_Files then
            declare
               Filename : constant String := "c_files_idx";
            begin
               Files_Vector_Sort.Sort (Src_Files);
               Generate_Files_Index
                 (Src_Files => Src_Files,
                  Filename  => Filename,
                  Header    => "C & C++ source files");
               Append_Line (Printout'Access, "   " & Filename);
            end;

            declare
               Filename : constant String := "c_entities_idx";
            begin
               Generate_Entities_Index
                 (Filename => Filename,
                  Header   => "C & C++ entities",
                  Filter   => Lang_C_CPP);
               Append_Line (Printout'Access, "   " & Filename);
            end;

            Printout := Printout & ASCII.LF;
         end if;

         if Has_Ada_Files and Has_C_Files then
            Src_Files := Backend.Src_Files;
            if Natural (Src_Files.Length) > 0 then
               declare
                  Filename : constant String := "all_files_idx";
               begin
                  Files_Vector_Sort.Sort (Src_Files);
                  Generate_Files_Index
                    (Src_Files => Src_Files,
                     Filename  => Filename,
                     Header    => "All source files");
                  Append_Line (Printout'Access, "   " & Filename);
               end;
            end if;

            declare
               Filename : constant String := "all_entities_idx";
            begin
               Generate_Entities_Index
                 (Filename => Filename,
                  Header   => "All entities",
                  Filter   => None);
               Append_Line (Printout'Access, "   " & Filename);
            end;
         end if;

         Printout := Printout & ASCII.LF;

         Insert (Translation, Assoc ("PRINTOUT", Printout));

         Write_To_File
           (Context   => Backend.Context,
            Directory => Get_Doc_Directory (Backend.Context.Kernel),
            Filename  => To_Destination_Name ("index"),
            Text =>
              Parse (+Tmpl.Full_Name, Translation, Cached => True));

         Stop (My_Delay, Generate_Global_Index_Time);
      end Generate_Global_Indexes;

   --  Start of processing for Finalize

   begin
      if Update_Global_Index then
         Generate_Global_Indexes;
      end if;
   end Finalize;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (E : Entity_Id;
      Use_Full_Name : Boolean := False) return String is
   begin
      if Use_Full_Name then
         return Get_Full_Name (E);

      elsif not LL.Is_Subprogram (E)
        or else In_C_Or_CPP_Language (E)
      then
         return Get_Short_Name (E);

      --  Handle Ada operators

      else
         declare
            Name : constant String := Get_Short_Name (E);
         begin
            if Name = "+"
              or else Name = "-"
              or else Name = "*"
              or else Name = "/"
              or else Name = "&"
              or else Name = "="
              or else Name = "<"
              or else Name = ">"
              or else Name = "<="
              or else Name = ">="
              or else Name = "**"
              or else Name = "not"
              or else Name = "and"
              or else Name = "or"
              or else Name = "xor"
              or else Name = "mod"
              or else Name = "rem"
              or else Name = "abs"
            then
               return """" & Name & """";
            else
               return Name;
            end if;
         end;
      end if;
   end Get_Name;

   ---------------------
   -- Get_Unique_Name --
   ---------------------

   function Get_Unique_Name (E : Entity_Id) return String is
   begin
      if LL.Is_Type (E) then
         return To_Lower (Get_Full_Name (E));
      else
         return
           To_Lower (Get_Full_Name (E) & To_String (LL.Get_Location (E).Line));
      end if;
   end Get_Unique_Name;

   ------------------
   -- Get_Template --
   ------------------

   function Get_Template
     (System_Dir : Virtual_File;
      Kind       : Template_Kind) return Virtual_File is
   begin
      case Kind is
         when Tmpl_Entities =>
            return Create_From_Dir
              (System_Dir, "docgen3/entities.tmpl");
         when Tmpl_Files_Index =>
            return Create_From_Dir
              (System_Dir, "docgen3/files_index.tmpl");
         when Tmpl_Global_Index =>
            return Create_From_Dir
              (System_Dir, "docgen3/index.tmpl");
      end case;
   end Get_Template;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Backend : in out Simple_Backend;
      Context : access constant Docgen_Context)
   is
      procedure Generate_Support_Files
        (Kernel : Core_Kernel);
      --  Generate support files in destination directory

      ----------------------------
      -- Generate_Support_Files --
      ----------------------------

      procedure Generate_Support_Files
        (Kernel : Core_Kernel)
      is
         Src_Dir : constant GNATCOLL.VFS.Virtual_File :=
           Create_From_Dir
             (Get_Share_Dir (Kernel), "docgen3/support/");
         Dst_Dir : constant GNATCOLL.VFS.Virtual_File :=
           Get_Doc_Directory (Kernel);
         Success : Boolean;

      begin
         if not Is_Directory (Dst_Dir) then
            Dst_Dir.Make_Dir;
         end if;

         --  Copy the "support" directory into the target directory

         Src_Dir.Copy (Dst_Dir.Full_Name, Success);

         pragma Assert (Success);
      end Generate_Support_Files;

   begin
      Backend.Context := Context;
      Generate_Support_Files (Context.Kernel);
   end Initialize;

   ------------------
   -- Process_File --
   ------------------

   overriding procedure Process_File
     (Backend : in out Simple_Backend;
      Tree    : access Tree_Type)
   is
      Tmpl    : constant Virtual_File :=
                  Get_Template
                   (Get_Share_Dir (Backend.Context.Kernel), Tmpl_Entities);
      Translation : Translate_Set;

      procedure For_All
        (Vector   : in out EInfo_List.Vector;
         Printout : access Unbounded_String;
         Process  : access procedure (Printout : access Unbounded_String;
                                      E_Info   : Entity_Id));
      --  Call subprogram Process for all the elements of Vector

      -------------
      -- For_All --
      -------------

      procedure For_All
        (Vector   : in out EInfo_List.Vector;
         Printout : access Unbounded_String;
         Process  : access procedure (Printout : access Unbounded_String;
                                      E_Info   : Entity_Id))
      is
         Cursor  : EInfo_List.Cursor;

      begin
         Cursor := Vector.First;
         while EInfo_List.Has_Element (Cursor) loop
            Process (Printout, EInfo_List.Element (Cursor));
            EInfo_List.Next (Cursor);
         end loop;
      end For_All;

      ------------------
      -- Process_Node --
      ------------------

      Root_Level : constant Natural := 0;

      procedure Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural);

      procedure Process_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural)
      is
         Entities : aliased Collected_Entities;

         procedure Classify_Entity (E : Entity_Id);
         --  Classify the entity in one of the following categories: Method,
         --  subprogram, tagged type, record type, type, variable or package.

         ---------------------
         -- Classify_Entity --
         ---------------------

         procedure Classify_Entity (E : Entity_Id) is
         begin
            if Is_Package (E) then
               Entities.Pkgs.Append (E);
               Backend.Entities.Pkgs.Append (E);

            elsif Get_Kind (E) = E_Variable then
               Entities.Variables.Append (E);
               Backend.Entities.Variables.Append (E);

            elsif LL.Is_Subprogram (E) then

               --  C/C++ macros unsupported yet???

               if Get_Kind (E) = E_Macro then
                  null;

               elsif Get_Kind (Entity) = E_Class
                 and then LL.Is_Primitive (E)
               then
                  --  This is not fully correct since we should check that
                  --  it is NOT defined as "void" (but this information is
                  --  not available in Xref ???)

                  if Get_Kind (E) = E_Procedure
                    and then Get_Short_Name (E) = Get_Short_Name (Entity)
                  then
                     Entities.CPP_Constructors.Append (E);
                     Backend.Entities.CPP_Constructors.Append (E);
                  else
                     Append_Unique_Elmt (Entities.Methods, E);
                     Append_Unique_Elmt (Backend.Entities.Methods, E);
                  end if;

               elsif In_Ada_Language (E) then
                  if LL.Is_Primitive (E) then
                     Append_Unique_Elmt (Entities.Methods, E);
                     Append_Unique_Elmt (Backend.Entities.Methods, E);
                  else
                     Entities.Subprgs.Append (E);
                     Backend.Entities.Subprgs.Append (E);
                  end if;

               else
                  Entities.Subprgs.Append (E);
                  Backend.Entities.Subprgs.Append (E);
               end if;

            elsif LL.Is_Type (E) then
               if Get_Kind (E) = E_Class then
                  Entities.CPP_Classes.Append (E);
                  Backend.Entities.CPP_Classes.Append (E);

               elsif Is_Tagged_Type (E) then
                  if Get_Kind (E) = E_Interface then
                     Entities.Interface_Types.Append (E);
                     Backend.Entities.Interface_Types.Append (E);
                  else
                     Entities.Tagged_Types.Append (E);
                     Backend.Entities.Tagged_Types.Append (E);
                  end if;

               elsif Is_Class_Or_Record_Type (E) then
                  Entities.Record_Types.Append (E);
                  Backend.Entities.Record_Types.Append (E);

               elsif LL.Is_Access (E) then
                  Entities.Access_Types.Append (E);
                  Backend.Entities.Access_Types.Append (E);

               else
                  Entities.Simple_Types.Append (E);
                  Backend.Entities.Simple_Types.Append (E);
               end if;

            end if;
         end Classify_Entity;

         --  Local variable

         Printout : aliased Unbounded_String;
         use type Ada.Containers.Count_Type;

      --  Start of processing for Process_Node

      begin
         if In_Ada_Language (Entity) then
            if Is_Package (Entity) then
               if not Backend.Entities.Pkgs.Contains (Entity) then
                  Backend.Entities.Pkgs.Append (Entity);
               end if;

            elsif LL.Is_Subprogram (Entity) then
               if not Backend.Entities.Subprgs.Contains (Entity) then
                  Backend.Entities.Subprgs.Append (Entity);
               end if;
            end if;
         end if;

         --  Classify the tree nodes in categories

         For_All (Get_Entities (Entity).all, Classify_Entity'Access);

         if Entities.Variables.Length > 0
           or else Entities.Simple_Types.Length > 0
           or else Entities.Access_Types.Length > 0
           or else Entities.Record_Types.Length > 0
           or else Entities.Interface_Types.Length > 0
           or else Entities.Tagged_Types.Length > 0
           or else Entities.CPP_Classes.Length > 0
           or else Entities.Subprgs.Length > 0
           or else Entities.Methods.Length > 0
           or else Entities.CPP_Constructors.Length > 0
           or else Entities.Pkgs.Length > 0
         then
            Append_Line (Printout'Access, "Entities");
            Append_Line (Printout'Access, "========");
            Append_Line (Printout'Access, "");

            ReST_Append_List
              (Printout'Access, Entities.Variables, "Constants & variables");
            ReST_Append_List
              (Printout'Access, Entities.Simple_Types, "Simple Types");
            ReST_Append_List
              (Printout'Access, Entities.Access_Types, "Access Types");
            ReST_Append_List
              (Printout'Access, Entities.Record_Types, "Record Types");
            ReST_Append_List
              (Printout'Access, Entities.Interface_Types, "Interface types");
            ReST_Append_List
              (Printout'Access, Entities.Tagged_Types, "Tagged types");
            ReST_Append_List
              (Printout'Access, Entities.CPP_Classes, "C++ Classes");
            ReST_Append_List
              (Printout'Access, Entities.Subprgs, "Subprograms");

            if In_Ada_Language (Entity) then
               ReST_Append_List
                 (Printout'Access, Entities.Methods,
                  "Dispatching subprograms");
            else
               ReST_Append_List
                 (Printout'Access, Entities.CPP_Constructors, "Constructors",
                  Use_Full_Name => True);
               ReST_Append_List
                 (Printout'Access, Entities.Methods, "Methods",
                  Use_Full_Name => True);
            end if;

            if In_Ada_Language (Entity) then
               ReST_Append_List
                 (Printout'Access, Entities.Pkgs, "Nested packages");
            end if;

            --  Generate full documentation

            For_All
              (Vector   => Entities.Variables,
               Printout => Printout'Access,
               Process  => ReST_Append_Simple_Declaration'Access);
            For_All
              (Entities.Simple_Types,
               Printout'Access,
               ReST_Append_Simple_Declaration'Access);
            For_All
              (Entities.Access_Types,
               Printout'Access,
               ReST_Append_Simple_Declaration'Access);
            For_All
              (Entities.Record_Types,
               Printout'Access,
               ReST_Append_Record_Type_Declaration'Access);

            if In_Ada_Language (Entity) then
               For_All
                 (Entities.Interface_Types,
                  Printout'Access,
                  ReST_Append_Record_Type_Declaration'Access);
               For_All
                 (Entities.Tagged_Types,
                  Printout'Access,
                  ReST_Append_Record_Type_Declaration'Access);
               For_All
                 (Entities.Methods,
                  Printout'Access,
                  ReST_Append_Subprogram'Access);

            else
               For_All
                 (Entities.CPP_Constructors,
                  Printout'Access,
                  ReST_Append_Subprogram'Access);
               For_All
                 (Entities.Methods,
                  Printout'Access,
                  ReST_Append_Subprogram'Access);
            end if;

            For_All
              (Entities.Subprgs,
               Printout'Access,
               ReST_Append_Subprogram'Access);
         end if;

         declare
            function Get_Filename return String;
            --  Return the filename associatd with the output of this node

            function Get_Filename return String is
            begin
               if Scope_Level = 0 then
                  return +Tree.File.Base_Name;

               elsif Get_Kind (Entity) = E_Class then
                  return Get_Short_Name (Entity);

               else
                  return Get_Full_Name (Entity);
               end if;
            end Get_Filename;

            Doc_Dir     : constant Virtual_File :=
                            Get_Doc_Directory (Backend.Context.Kernel);
            Filename    : constant String := To_Lower (Get_Filename);
            ReST_Header : constant String (Filename'Range) := (others => '*');
            ReST_File   : constant Filesystem_String :=
                            To_Destination_Name (Filesystem_String (Filename));
            Labels      : Unbounded_String;
            Header      : aliased Unbounded_String;
         begin
            if In_Ada_Language (Entity) then
               Labels :=
                 To_Unbounded_String (ReST_Label (Filename))
                 & ASCII.LF;

               --  For subprograms and instantiations we do not add here its
               --  label to avoid generating the label twice since we will
               --  append its profile (and label). See bellow the call to
               --  ReST_Append_Subprogram.

               if Is_Package (Entity)
                 and then not Present (LL.Get_Instance_Of (Entity))
               then
                  Labels := Labels
                    & ReST_Label (Entity)
                    & ASCII.LF;
               end if;

            elsif Get_Kind (Entity) = E_Class then
               Labels :=
                 To_Unbounded_String (ReST_Label (Entity)) & ASCII.LF;
            end if;

            Header :=
              Labels
              & ASCII.LF
              & ReST_Header & ASCII.LF
              & Filename    & ASCII.LF
              & ReST_Header & ASCII.LF;

            if In_Ada_Language (Entity) then
               Header :=
                 Header
                 & ASCII.LF
                 & To_ReST (Get_Comment (Entity))
                 & ASCII.LF;

               if Present (LL.Get_Instance_Of (Entity)) then
                  ReST_Append_Simple_Declaration (Printout'Access, Entity);

               elsif LL.Is_Subprogram (Entity) then
                  ReST_Append_Subprogram (Printout'Access, Entity);
               end if;

            elsif Get_Kind (Entity) = E_Class then
               ReST_Append_Src (Header'Access, Entity);
               ReST_Append_Comment (Header'Access, Entity);
            end if;

            Printout :=
              Header & ASCII.LF
              & Printout & ASCII.LF;

            Insert (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Doc_Dir,
               Filename  => ReST_File,
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));

            --  Append files of nested Ada packages and C++ classes to the
            --  list of files of the global index

            if Scope_Level > Root_Level then
               declare
                  File : constant Virtual_File :=
                           Create_From_Dir (Doc_Dir, ReST_File);
               begin
                  Backend.Extra_Files.Append (File);
               end;
            end if;
         end;

         --  (Ada) Handle nested packages

         if In_Ada_Language (Entity) then
            declare
               Cursor : EInfo_List.Cursor;

            begin
               Cursor := Entities.Pkgs.First;
               while EInfo_List.Has_Element (Cursor) loop
                  Process_Node (EInfo_List.Element (Cursor), Scope_Level + 1);
                  EInfo_List.Next (Cursor);
               end loop;
            end;

         --  (C++) Handle C++ nested classes

         else
            declare
               Cursor : EInfo_List.Cursor;

            begin
               Cursor := Entities.CPP_Classes.First;
               while EInfo_List.Has_Element (Cursor) loop
                  Process_Node (EInfo_List.Element (Cursor), Scope_Level + 1);
                  EInfo_List.Next (Cursor);
               end loop;
            end;
         end if;

         Clear (Entities);
      end Process_Node;

      --  Local variables

      Lang         : constant Language_Access :=
                       Get_Language_From_File
                        (Backend.Context.Lang_Handler, Tree.File);
      In_Ada_Lang  : constant Boolean :=
                       Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang    : constant Boolean := not In_Ada_Lang;
      Root_Id      : Entity_Id renames Tree.Tree_Root;
      Current_Unit : Entity_Id;
      My_Delay     : Delay_Time;

   --  Start of processing for Process_File

   begin
      if No (Root_Id) then
         return;
      elsif In_C_Lang and then Backend.Context.Options.Skip_C_Files then
         return;
      end if;

      Trace (Me, "Process_File " & (+Tree.File.Base_Name));

      if In_Ada_Lang then
         Current_Unit := Get_Entities (Tree.Tree_Root).First_Element;
      else
         Current_Unit := Tree.Tree_Root;
      end if;

      Start (My_Delay);

      Process_Node (Current_Unit, Scope_Level => Root_Level);
      Backend.Src_Files.Append (Tree.File);

      Stop (My_Delay, Generate_Doc_Time);
   end Process_File;

   -------------------------
   -- To_Destination_Name --
   -------------------------

   function To_Destination_Name
     (Basename : Filesystem_String) return Filesystem_String
   is
   begin
      return Basename & ".rst";
   end To_Destination_Name;

end Docgen3.Backend.Simple;
