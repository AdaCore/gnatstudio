------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2017, AdaCore                     --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Basic_Types;              use Basic_Types;
with GNATdoc.Comment;          use GNATdoc.Comment;
with GNATdoc.Frontend;         use GNATdoc.Frontend;
with GNATdoc.Time;             use GNATdoc.Time;
with GNATdoc.Utils;            use GNATdoc.Utils;
with GNAT.Strings;             use GNAT.Strings;
with Language;                 use Language;
with Language.Ada;             use Language.Ada;
with Language.C;
with Language.Cpp;
with Language.Tree;            use Language.Tree;
with Language.Tree.Database;   use Language.Tree.Database;
with String_Utils;             use String_Utils;
with Templates_Parser;         use Templates_Parser;
with GNATCOLL.Traces;          use GNATCOLL.Traces;

package body GNATdoc.Backend.Simple is
   use type EInfo_List.Vector;

   Me : constant Trace_Handle := Create ("GNATdoc.1-Backend");

   type Template_Kind is
     (Tmpl_Entities,        --  Entities index
      Tmpl_Files_Index,     --  Files index
      Tmpl_Global_Index,    --  Global index
      Tmpl_Prj_Files_Index, --  Project files index
      Tmpl_Src_File         --  Source file
     );

   -----------------------
   -- Local Subprograms --
   -----------------------

   function File_Containing (E : Entity_Id) return String;
   --  Returns the name of the backend file containing the documentation of E

   function Get_Entities
     (Context : access constant Docgen_Context;
      Entity  : Entity_Id) return access EInfo_List.Vector;

   function Get_Inherited_Methods
     (Context : access constant Docgen_Context;
      Entity  : Entity_Id) return access EInfo_List.Vector;

   function Get_Methods
     (Context : access constant Docgen_Context;
      Entity  : Entity_Id) return access EInfo_List.Vector;

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
     (Self : Simple_Backend'Class;
      Kind : Template_Kind) return Virtual_File;

   function To_Html_Name
     (Basename : Filesystem_String) return Filesystem_String;

   function To_Listing_Name (Basename : String) return String;

   function To_ReST_Name
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

      procedure ReST_Append_Concurrent_Type_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id;
         Context  : access constant Docgen_Context);
      --  Append to Printout the reStructured output of a concurrent type or
      --  object

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
         E        : Entity_Id;
         Context  : access constant Docgen_Context);
      --  Append to Printout the reStructured output of a record type

      procedure ReST_Append_Reference
        (Printout : access Unbounded_String;
         Entity   : Entity_Id;
         Prefix   : String;
         Suffix   : String := "");
      --  Append to Printout a reference to entity E

      procedure ReST_Append_Simple_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id;
         Context  : access constant Docgen_Context);
      --  Append to Printout the reStructured output of a variable or a type

      procedure ReST_Append_Src
        (Printout : access Unbounded_String;
         E        : Entity_Id);
      --  Append to Printout the source of E

      procedure ReST_Append_Subprogram
        (Printout : access Unbounded_String;
         E        : Entity_Id;
         Context  : access constant Docgen_Context);
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

      ---------------------------------------------
      -- ReST_Append_Concurrent_Type_Declaration --
      ---------------------------------------------

      procedure ReST_Append_Concurrent_Type_Declaration
        (Printout : access Unbounded_String;
         E        : Entity_Id;
         Context  : access constant Docgen_Context)
      is
         Name   : constant String := Get_Short_Name (E);
         Header : constant String (Name'Range) := (others => '=');

         Discriminants : constant EInfo_List.Vector := Get_Discriminants (E);
         Components    : constant EInfo_List.Vector := Get_Components (E);
         Entries       : constant EInfo_List.Vector := Get_Entries (E);
         Subprograms   : constant EInfo_List.Vector := Get_Subprograms (E);

      begin
         ReST_Append_Label (Printout, E);

         Append_Line (Printout, Name);
         Append_Line (Printout, Header);
         Append_Line (Printout, "");

         if not Is_Partial_View (E)
           or else not Context.Options.Show_Private
         then
            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

         else
            if Is_Incomplete (E) then
               Append_Line (Printout, "**Incomplete View:**");
            else
               Append_Line (Printout, "**Partial View:**");
            end if;

            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

            Append_Line (Printout, "**Full View:**");
            Append_Line (Printout, "");
            Append_Line (Printout, ".. code-block:: ada");
            Append_Line (Printout, "");
            Append_Line (Printout,
                         To_String (Append_Tab (Get_Src (Get_Full_View (E)))));
            Append_Line (Printout, "");

            declare
               Comment_Str : constant String :=
                 To_String (To_ReST (Get_Comment (Get_Full_View (E))));
            begin
               if Comment_Str /= "" then
                  Append_Line (Printout, "");
                  Append_Line (Printout, Comment_Str);
               end if;
            end;
         end if;

         ReST_Append_List
           (Printout => Printout,
            List     => Discriminants,
            Header   => "Discriminants");

         ReST_Append_List
           (Printout => Printout,
            List     => Components,
            Header   => "Components");

         ReST_Append_List
           (Printout => Printout,
            List     => Entries,
            Header   => "Entries");

         ReST_Append_List
           (Printout => Printout,
            List     => Subprograms,
            Header   => "Subprograms");

         --  Append documentation of discriminants

         declare
            Cursor : EInfo_List.Cursor;
            Entity : Entity_Id;
         begin
            Cursor := Discriminants.First;
            while EInfo_List.Has_Element (Cursor) loop
               Entity := EInfo_List.Element (Cursor);
               ReST_Append_Simple_Declaration (Printout, Entity, Context);

               EInfo_List.Next (Cursor);
            end loop;
         end;

         --  Append documentation of components

         declare
            Cursor : EInfo_List.Cursor;
            Entity : Entity_Id;
         begin
            Cursor := Components.First;
            while EInfo_List.Has_Element (Cursor) loop
               Entity := EInfo_List.Element (Cursor);
               ReST_Append_Simple_Declaration (Printout, Entity, Context);

               EInfo_List.Next (Cursor);
            end loop;
         end;

         --  Append documentation of entries

         declare
            Cursor : EInfo_List.Cursor;
            Entity : Entity_Id;
         begin
            Cursor := Entries.First;
            while EInfo_List.Has_Element (Cursor) loop
               Entity := EInfo_List.Element (Cursor);
               ReST_Append_Subprogram (Printout, Entity, Context);

               EInfo_List.Next (Cursor);
            end loop;
         end;

         --  Append documentation of subprograms

         declare
            Cursor : EInfo_List.Cursor;
            Entity : Entity_Id;
         begin
            Cursor := Subprograms.First;
            while EInfo_List.Has_Element (Cursor) loop
               Entity := EInfo_List.Element (Cursor);
               ReST_Append_Subprogram (Printout, Entity, Context);

               EInfo_List.Next (Cursor);
            end loop;
         end;
      end ReST_Append_Concurrent_Type_Declaration;

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
         function Gen_Suffix (E : Entity_Id) return String;
         function Gen_Suffix (E : Entity_Id) return String is
            Private_Suffix : constant String :=
              (if In_Private_Part (E) then " *(private)*"
                                      else "");
         begin
            if Is_Generic (E) then
               return " *(generic)*" & Private_Suffix;
            elsif Is_Generic_Formal (E) then
               return " *(generic formal)*" & Private_Suffix;
            elsif Get_Kind (E) = E_Interface then
               return " *(interface)*" & Private_Suffix;
            elsif LL.Is_Abstract (E) then
               return " *(abstract)*" & Private_Suffix;
            else
               return Private_Suffix;
            end if;
         end Gen_Suffix;

         Cursor : EInfo_List.Cursor;
         E      : Entity_Id;
         Found  : Boolean;

      begin
         if not EInfo_List.Has_Element (List.First) then
            return;
         end if;

         --  Check if there is some entity to be printed; otherwise no
         --  output is generated.

         Found := False;
         Cursor := List.First;
         while EInfo_List.Has_Element (Cursor) loop
            E := EInfo_List.Element (Cursor);

            if Filter = None
              or else
                (Filter = Lang_Ada and then In_Ada_Language (E))
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

               Append_Line
                 (Printout,
                  " <"
                  & Get_Unique_Name (E)
                  & ">` "
                  & Image (LL.Get_Location (E))
                  & Gen_Suffix (E));
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
         E        : Entity_Id;
         Context  : access constant Docgen_Context)
      is
         Name   : constant String := Get_Short_Name (E);
         Header : constant String (Name'Range) := (others => '=');

      begin
         ReST_Append_Label (Printout, E);

         --  Append labels to all the components. Required to facilitate
         --  navigation from the annotated sources listing.

         declare
            Cursor : EInfo_List.Cursor;
         begin
            Cursor := Get_Entities (Context, E).First;
            while EInfo_List.Has_Element (Cursor) loop
               ReST_Append_Label (Printout, EInfo_List.Element (Cursor));
               EInfo_List.Next (Cursor);
            end loop;
         end;

         Append_Line (Printout, Name);
         Append_Line (Printout, Header);
         Append_Line (Printout, "");

         if not Is_Partial_View (E)
           or else not Context.Options.Show_Private
         then
            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

         else
            if Is_Incomplete (E) then
               Append_Line (Printout, "**Incomplete View:**");
            else
               Append_Line (Printout, "**Partial View:**");
            end if;

            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

            Append_Line (Printout, "**Full View:**");
            Append_Line (Printout, "");
            Append_Line (Printout, ".. code-block:: ada");
            Append_Line (Printout, "");
            Append_Line (Printout,
                         To_String (Append_Tab (Get_Src (Get_Full_View (E)))));
            Append_Line (Printout, "");

            declare
               Comment_Str : constant String :=
                 To_String (To_ReST (Get_Comment (Get_Full_View (E))));
            begin
               if Comment_Str /= "" then
                  Append_Line (Printout, "");
                  Append_Line (Printout, Comment_Str);
               end if;
            end;
         end if;

         if Is_Tagged (E)
           or else Get_Kind (E) = E_Class
         then
            if In_Ada_Language (E) then
               if Present (Get_Parent (E))
                 or else (Is_Partial_View (E)
                            and then Present (Get_Parent (Get_Full_View (E))))
               then
                  if not Has_Private_Parent (E)
                    or else Context.Options.Show_Private
                  then
                     Append_Line (Printout, "- Parent");

                     ReST_Append_Reference
                       (Printout => Printout,
                        Entity   =>
                          (if not Is_Partial_View (E) then Get_Parent (E)
                           else Get_Parent (Get_Full_View (E))),
                        Prefix   => "   * ");

                     Append_Line (Printout, "");
                  end if;
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
            if not Has_Private_Parent (E)
              or else Context.Options.Show_Private
            then
               ReST_Append_List
                 (Printout => Printout,
                  List     => Get_Inherited_Methods (Context, E).all,
                  Header   => "Inherited dispatching subprograms");
            end if;

            ReST_Append_List
              (Printout => Printout,
               List     => Get_Methods (Context, E).all,
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
         E        : Entity_Id;
         Context  : access constant Docgen_Context)
      is
         Name   : constant String := Get_Short_Name (E);
         Header : constant String (Name'Range) := (others => '=');

      begin
         ReST_Append_Label (Printout, E);

         Append_Line (Printout, Name);
         Append_Line (Printout, Header);
         Append_Line (Printout, "");

         if not Is_Partial_View (E)
           or else not Context.Options.Show_Private
         then
            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

         else
            Append_Line (Printout, "**Incomplete View:**");

            ReST_Append_Src (Printout, E);
            ReST_Append_Comment (Printout, E);

            Append_Line (Printout, "**Full View:**");
            ReST_Append_Src (Printout, Get_Full_View (E));
            ReST_Append_Comment (Printout, Get_Full_View (E));
         end if;
      end ReST_Append_Simple_Declaration;

      ---------------------
      -- ReST_Append_Src --
      ---------------------

      procedure ReST_Append_Src
        (Printout : access Unbounded_String;
         E        : Entity_Id)
      is
      begin
         if Present (Get_Src (E)) then
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
         E        : Entity_Id;
         Context  : access constant Docgen_Context)
      is
         Name   : constant String :=
                    Get_Name (E,
                      Use_Full_Name =>
                        In_C_Or_CPP_Language (E) and then LL.Is_Primitive (E));
         Header : constant String (Name'Range) := (others => '=');

      begin
         if Present (Get_Src (E)) then
            ReST_Append_Label (Printout, E);

            --  Append labels to all the formals. Required to facilitate
            --  navigation from the annotated sources listing.

            declare
               Cursor : EInfo_List.Cursor;
               Formal : Entity_Id;
            begin
               Cursor := Get_Entities (Context, E).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Formal := EInfo_List.Element (Cursor);
                  exit when Get_Kind (Formal) /= E_Formal;

                  ReST_Append_Label (Printout, Formal);
                  EInfo_List.Next (Cursor);
               end loop;
            end;

            Append_Line (Printout, Name);
            Append_Line (Printout, Header);
            Append_Line (Printout, "");

            ReST_Append_Src (Printout, E);

            if Is_Generic_Formal (E) then
               Append_Line (Printout, "Generic formal.");
               Append_Line (Printout, "");
            end if;

            if Present (Get_Alias (E)) then
               ReST_Append_Reference (Printout,
                 Entity => Get_Alias (E),
                 Prefix => "Alias: ");
            end if;

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
            if No (Tag_Info.Tag) then
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
                  if Present (Tag_Info.Text) then
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

   ---------------------
   -- File_Containing --
   ---------------------

   function File_Containing (E : Entity_Id) return String is
      S : Entity_Id;

   begin
      if In_Ada_Language (E) then

         --  For inherited primitives defined in other files/scopes we could
         --  not set their scope (cf. Decorate_Record_Type).

         if No (Get_Scope (E)) then
            return "";

         elsif Is_Standard_Entity (Get_Scope (E)) then
            return To_Lower (+LL.Get_Location (E).File.Base_Name);

         elsif Get_Kind (E) = E_Package then
            return To_Lower (Get_Full_Name (E));

         else
            S := Get_Scope (E);

            while Present (S)
              and then Get_Kind (S) /= E_Package
            loop
               S := Get_Scope (S);
            end loop;

            if No (S)
              or else Is_Standard_Entity (S)
              or else No (Get_Scope (S))
              or else Is_Standard_Entity (Get_Scope (S))
            then
               return To_Lower (+LL.Get_Location (E).File.Base_Name);
            else
               return To_Lower (Get_Full_Name (S));
            end if;
         end if;

      --  C/C++ language. In this case we are not covering all cases since
      --  we still don't support C/C++ annotated sources.

      else
         if Get_Kind (E) = E_Class then
            return To_Lower (Get_Short_Name (E));
         else
            return To_Lower (+LL.Get_Location (E).File.Base_Name);
         end if;
      end if;
   end File_Containing;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Backend : in out Simple_Backend;
      Update_Global_Index : Boolean)
   is
      procedure Generate_Annotated_Ada_Sources;
      --  Generate html annotated Ada sources

      procedure Generate_Global_Indexes;
      --  Generate ???

      function Get_Ada_Src_Files return Files_List.Vector;
      --  Return the list of Ada source files of the project

      -----------------------
      -- Get_Ada_Src_Files --
      -----------------------

      function Get_Ada_Src_Files return Files_List.Vector is
         Lang   : Language_Access;
         Result : Files_List.Vector;

      begin
         for File of Backend.Src_Files loop
            Lang :=
              Backend.Context.Lang_Handler.Get_Language_From_File (File);

            if Lang.all in Language.Ada.Ada_Language'Class then
               Result.Append (File);
            end if;
         end loop;

         return Result;
      end Get_Ada_Src_Files;

      ------------------------------------
      -- Generate_Annotated_Ada_Sources --
      ------------------------------------

      procedure Generate_Annotated_Ada_Sources is

         procedure Process_File (File : GNATCOLL.VFS.Virtual_File);
         procedure Process_File (File : GNATCOLL.VFS.Virtual_File) is
            Lang : constant Language_Access :=
              Get_Language_From_File (Backend.Context.Lang_Handler, File);

            Printout : aliased Unbounded_String;
            Buffer   : GNAT.Strings.String_Access;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean;
            --  Callback for entity parser

            function To_Label (S : String) return String;
            --  Html labels generated by Sphynx replace several characters
            --  This routine takes care of performing the same changes to
            --  generate references to the correct labels.

            function To_Label (S : String) return String is
               Result : String := S;
            begin
               for J in Result'Range loop
                  if Result (J) = '.'
                    or Result (J) = '_'
                  then
                     Result (J) := '-';
                  end if;
               end loop;

               return Result;
            end To_Label;

            --------
            -- CB --
            --------

            Last_Idx : Natural := 0;

            function CB
              (Entity         : Language_Entity;
               Sloc_Start     : Source_Location;
               Sloc_End       : Source_Location;
               Partial_Entity : Boolean) return Boolean
            is
               pragma Unreferenced (Partial_Entity);

               --  use Basic_Types;
            begin
               --  Print all text between previous call and current one
               if Last_Idx /= 0 then
                  Append (Printout'Access,
                    Buffer (Last_Idx + 1 .. Sloc_Start.Index - 1));
               end if;

               Last_Idx := Sloc_End.Index;

               --  For all entities that are not identifiers, print them
               --  directly

               if Entity = Keyword_Text then
                  Append (Printout'Access,
                    "<b>"
                    & Buffer (Sloc_Start.Index .. Sloc_End.Index)
                    & "</b>");

               elsif Entity = Comment_Text then
                  Append (Printout'Access,
                    "<i>"
                    & Buffer (Sloc_Start.Index .. Sloc_End.Index)
                    & "</i>");

               elsif Entity not in Identifier_Entity then
                  Append (Printout'Access,
                    Buffer (Sloc_Start.Index .. Sloc_End.Index));

               --  If entity is an identifier or a partial identifier, then try
               --  to find its corresponding Entity_Info
               else
                  declare
                     Str_Quote : constant String := """";
                     Loc : constant General_Location :=
                       (File    => File,
                        Project => No_Project,  --  ??? unknown
                        Line    => Sloc_Start.Line,
                        Column  => Visible_Column_Type (Sloc_Start.Column));
                     E   : Entity_Id;
                  begin
                     E := Find_Unique_Entity (Loc, In_References => True);

                     --  Cannot generate the reference if we don't know the
                     --  file where the entity is defined.

                     if Present (E)
                       and then File_Containing (E) /= ""
                     then
                        Append (Printout'Access,
                          "<a href="
                          & Str_Quote
                          & File_Containing (E) & ".html"
                          & "#"
                          & To_Label (Get_Unique_Name (E))
                          & Str_Quote
                          & ">"
                          & Buffer (Sloc_Start.Index .. Sloc_End.Index)
                          & "</a>");

                     else
                        Append (Printout'Access,
                          Buffer (Sloc_Start.Index .. Sloc_End.Index));
                     end if;
                  end;
               end if;

               return False;
            end CB;

            Filename    : constant String := "hsrc_" & (+File.Base_Name);
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
                            Backend.Get_Template (Tmpl_Src_File);

         begin
            Append_Line (Printout'Access, "<!DOCTYPE html>");
            Append_Line (Printout'Access, "<html>");
            Append_Line (Printout'Access, "<body>");
            Append_Line (Printout'Access, "<pre>");

            Buffer := File.Read_File;

            Parse_Entities
              (Lang, Buffer.all (Buffer'First .. Buffer'Last),
               CB'Unrestricted_Access);

            Free (Buffer);

            Append_Line (Printout'Access, "</pre>");
            Append_Line (Printout'Access, "</body>");
            Append_Line (Printout'Access, "</html>");

            Insert (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_Html_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));

            --  Generate the associated ReST file

            declare
               Html_Filename : constant String :=
                 String (To_Html_Name (Filesystem_String (Filename)));
               Fname  : constant String := To_Listing_Name (+File.Base_Name);
               Header : constant String (Fname'Range) := (others => '*');
            begin
               Printout := Null_Unbounded_String;

               Append_Line (Printout'Access, Header);
               Append_Line (Printout'Access, Fname);
               Append_Line (Printout'Access, Header);
               Append_Line (Printout'Access, "");
               Append_Line (Printout'Access, ".. raw:: html");
               Append_Line (Printout'Access, "      :file: " & Html_Filename);
               Append_Line (Printout'Access, "");

               Insert (Translation, Assoc ("PRINTOUT", Printout));

               Write_To_File
                 (Context   => Backend.Context,
                  Directory => Get_Doc_Directory (Backend.Context.Kernel),
                  Filename  => To_ReST_Name (Filesystem_String (Fname)),
                  Text =>
                    Parse (+Tmpl.Full_Name, Translation, Cached => True));
            end;
         end Process_File;

         Src_Files  : aliased Files_List.Vector;
         File       : GNATCOLL.VFS.Virtual_File;
         File_Index : Files_List.Cursor;
      begin
         Src_Files := Get_Ada_Src_Files;

         File_Index := Src_Files.First;
         while Files_List.Has_Element (File_Index) loop
            File := Files_List.Element (File_Index);
            Process_File (File);

            Files_List.Next (File_Index);
         end loop;
      end Generate_Annotated_Ada_Sources;

      -----------------------------
      -- Generate_Global_Indexes --
      -----------------------------

      procedure Generate_Global_Indexes is

         procedure Generate_Annotated_Files_Index
           (Filename  : String;
            Header    : String;
            Src_Files : Files_List.Vector);
         --  Generate the ReST file Filename with this Header containing an
         --  index with all the annotated files correspoding to the Src_Files.

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
         procedure Generate_Instances_Index
           (Generics  : EInfo_List.Vector;
            Instances : EInfo_List.Vector;
            Filename  : String;
            Header    : String);
         --  Generate the ReST file Filename with this Header containing an
         --  index with the tree of dependencies of Ada tagged types.

         procedure Generate_Project_Files_Index
           (Filename  : String;
            Header    : String);
         --  Generate the ReST file Filename with this Header containing an
         --  index with all the files of Src_Files.

         procedure Generate_Tagged_Types_Tree_Index
           (Filename  : String;
            Header    : String);
         --  Generate the ReST file Filename with this Header containing an
         --  index with the tree of dependencies of Ada tagged types.

         function Get_C_And_CPP_Src_Files return Files_List.Vector;
         --  Return the list of C & C++ source files of the project

         function Has_Instances (List : EInfo_List.Vector) return Boolean;
         --  Return true if List has the instance of some generic

         ------------------------------------
         -- Generate_Annotated_Files_Index --
         ------------------------------------

         procedure Generate_Annotated_Files_Index
           (Filename  : String;
            Header    : String;
            Src_Files : Files_List.Vector)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Printout_H  : aliased Unbounded_String;
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
                            Backend.Get_Template (Tmpl_Files_Index);
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
                 & "   " & To_Listing_Name (+File.Base_Name) & ASCII.LF;

               Files_List.Next (File_Index);
            end loop;

            Insert
              (Translation, Assoc ("HEADER", Printout_H));
            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Annotated_Files_Index;

         -----------------------------
         -- Generate_Entities_Index --
         -----------------------------

         procedure Generate_Entities_Index
           (Filename : String;
            Header   : String;
            Filter   : List_Filter_Kind)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
                            Backend.Get_Template (Tmpl_Entities);

         begin
            Printout :=
              Printout
              & Header   & ASCII.LF
              & Header_U & ASCII.LF
              & ASCII.LF;

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
               Backend.Entities.Interface_Types,
               "Interface types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Tagged_Types,
               "Tagged types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Tasks,
               "Tasks & Task types",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Protected_Objects,
               "Protected objects",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.CPP_Classes,
               "C++ Classes",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Subprgs,
               "Subprograms",
               Filter => Filter);

            ReST_Append_List
              (Printout'Access,
               Backend.Entities.Pkgs,
               "Packages",
               Filter => Filter);

            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Entities_Index;

         --------------------------
         -- Generate_Files_Index --
         --------------------------

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
                            Backend.Get_Template (Tmpl_Files_Index);
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
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
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
                            Backend.Get_Template (Tmpl_Entities);
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

               if Is_Generic (E1) then
                  ReST_Append_Reference
                    (Printout => Printout'Access,
                     Entity   => E1,
                     Prefix   => "- ");
                  Append_Line (Printout'Access, "");

                  --  Append all its instantiations

                  declare
                     LL_E1   : constant Root_Entity'Class :=
                       LL.Get_Entity (E1);
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
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Instances_Index;

         ------------------------------
         -- Generate_Instances_Index --
         ------------------------------

         procedure Generate_Instances_Index
           (Generics  : EInfo_List.Vector;
            Instances : EInfo_List.Vector;
            Filename  : String;
            Header    : String)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Tmpl        : constant Virtual_File :=
              Backend.Get_Template (Tmpl_Entities);
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;

         begin
            Append_Line (Printout'Access, Header);
            Append_Line (Printout'Access, Header_U);
            Append_Line (Printout'Access, "");

            for Generic_Entity of Generics loop

               --  Locate a generic

               if Is_Generic (Generic_Entity) then
                  ReST_Append_Reference
                    (Printout => Printout'Access,
                     Entity   => Generic_Entity,
                     Prefix   => "- ");
                  Append_Line (Printout'Access, "");

                  --  Append all its instantiations

                  declare
                     LL_Generic_Entity : constant Root_Entity'Class :=
                       LL.Get_Entity (Generic_Entity);

                  begin
                     for Instance_Entity of Instances loop
                        if LL.Get_Instance_Of (Instance_Entity)
                          = LL_Generic_Entity
                        then
                           ReST_Append_Reference
                             (Printout => Printout'Access,
                              Entity   => Instance_Entity,
                              Prefix   => "  - ");
                           Append_Line (Printout'Access, "");
                        end if;
                     end loop;
                  end;
               end if;
            end loop;

            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Instances_Index;

         ----------------------------------
         -- Generate_Project_Files_Index --
         ----------------------------------

         procedure Generate_Project_Files_Index
           (Filename  : String;
            Header    : String)
         is
            Header_U    : constant String (Header'Range) := (others => '=');
            Printout_H  : aliased Unbounded_String;
            Printout    : aliased Unbounded_String;
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
                            Backend.Get_Template (Tmpl_Prj_Files_Index);
            Prj_Index   : Project_Files_List.Cursor;
            Prj_Srcs    : Project_Files;

         begin
            Printout_H :=
              Printout
              & Header & ASCII.LF
              & Header_U & ASCII.LF;

            Prj_Index := Backend.Context.Prj_Files.First;
            while Project_Files_List.Has_Element (Prj_Index) loop
               Prj_Srcs := Project_Files_List.Element (Prj_Index);

               declare
                  Prj_Name : constant String := Name (Prj_Srcs.Project);
                  Filename : constant String := "prj__" & To_Lower (Prj_Name);
               begin
                  Files_Vector_Sort.Sort (Prj_Srcs.Src_Files.all);
                  Generate_Files_Index
                    (Src_Files => Prj_Srcs.Src_Files.all,
                     Filename  => Filename,
                     Header    => Prj_Name);
                  Append_Line (Printout'Access, "   " & Filename);
               end;

               Project_Files_List.Next (Prj_Index);
            end loop;

            Insert
              (Translation, Assoc ("HEADER", Printout_H));
            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Backend.Context,
               Directory => Get_Doc_Directory (Backend.Context.Kernel),
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Project_Files_Index;

         --------------------------------------
         -- Generate_Tagged_Types_Tree_Index --
         --------------------------------------

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

            -----------------------------------------
            -- Collect_Tagged_Types_And_Interfaces --
            -----------------------------------------

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
                  pragma Assert (Is_Tagged (E));

                  if Get_IDepth_Level (E) = Root_Level
                    and then not Is_Full_View (E)
                  then
                     Root_Types.Append (E);
                  end if;

                  EInfo_List.Next (Cursor);
               end loop;

               --  Second round: Collect root types of other list components

               Cursor := List.First;
               while EInfo_List.Has_Element (Cursor) loop
                  E := EInfo_List.Element (Cursor);

                  if Get_IDepth_Level (E) /= Root_Level then
                     if Is_Partial_View (E) then
                        E := Get_Full_View (E);
                     end if;

                     while Present (Get_Parent (E)) loop
                        E := Get_Parent (E);

                        if Is_Partial_View (E) then
                           E := Get_Full_View (E);
                        end if;
                     end loop;

                     if Is_Full_View (E) then
                        E := Get_Partial_View (E);
                     end if;

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
                  if Is_Generic_Formal (E) then
                     return " *(generic formal)*";
                  elsif Get_Kind (E) = E_Interface then
                     return " *(interface)*";
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

               Cursor := Get_Direct_Derivations (E).First;
               while EInfo_List.Has_Element (Cursor) loop
                  Entity := EInfo_List.Element (Cursor);
                  ReST_Append (Entity, "  " & Prefix);

                  EInfo_List.Next (Cursor);
               end loop;
            end ReST_Append;

            --  Local variables

            Tmpl   : constant Virtual_File :=
                       Backend.Get_Template (Tmpl_Entities);

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
               Filename  => To_ReST_Name (Filesystem_String (Filename)),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Tagged_Types_Tree_Index;

         -----------------------------
         -- Get_C_And_CPP_Src_Files --
         -----------------------------

         function Get_C_And_CPP_Src_Files return Files_List.Vector is
            Lang   : Language_Access;
            Result : Files_List.Vector;

         begin
            for File of Backend.Src_Files loop
               Lang :=
                 Backend.Context.Lang_Handler.Get_Language_From_File (File);

               if Lang.all in Language.C.C_Language'Class then
                  Result.Append (File);
               end if;
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

         -----------------------
         -- New_Index_Section --
         -----------------------

         procedure New_Index_Section (Printout : access Unbounded_String);
         procedure New_Index_Section (Printout : access Unbounded_String) is
         begin
            Append_Line (Printout, ".. toctree::");
            Append_Line (Printout, "   :maxdepth: 1");
            Append_Line (Printout, "");
         end New_Index_Section;

         --  Local variables

         Printout      : aliased Unbounded_String;
         Translation   : Translate_Set;
         Tmpl          : constant Virtual_File :=
                           Backend.Get_Template (Tmpl_Global_Index);
         Src_Files     : aliased Files_List.Vector;
         My_Delay      : Delay_Time;
         Has_Ada_Files : Boolean := False;
         Has_C_Files   : Boolean := False;

         --  Start of processing for Generate_Global_Index

      begin
         Start (My_Delay);
         Trace (Me, "Generate_Global_Index");

         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Pkgs);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Pkgs_Instances);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Variables);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Simple_Types);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Record_Types);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Subprgs);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.Tagged_Types);
         EInfo_Vector_Sort_Short.Sort (Backend.Entities.CPP_Classes);

         Src_Files := Get_Ada_Src_Files;
         Has_Ada_Files := Natural (Src_Files.Length) > 0;

         if Has_Ada_Files then
            New_Index_Section (Printout'Access);

            Files_Vector_Sort.Sort (Src_Files);

            declare
               Filename : constant String := "ada_lst_files";
            begin
               Generate_Annotated_Files_Index
                 (Src_Files => Src_Files,
                  Filename  => Filename,
                  Header    => "Ada annotated sources");
               Append_Line (Printout'Access, "   " & Filename);
            end;

            declare
               Filename : constant String := "ada_files_idx";
            begin
               Generate_Files_Index
                 (Src_Files => Src_Files,
                  Filename  => Filename,
                  Header    => "Ada entities per file");
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
                     Header   => "Ada tagged types (derivations tree)");
                  Append_Line (Printout'Access, "   " & Filename);
               end;
            end if;

            if not Backend.Entities.Pkgs_Instances.Is_Empty then
               declare
                  Filename : constant String := "ada_pkg_instances_idx";

               begin
                  Generate_Instances_Index
                    (Generics  => Backend.Entities.Pkgs,
                     Instances => Backend.Entities.Pkgs_Instances,
                     Filename  => Filename,
                     Header    =>
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

            declare
               Filename : constant String := "ada_entities_idx";
            begin
               Generate_Entities_Index
                 (Filename => Filename,
                  Header   => "All Ada entities",
                  Filter   => Lang_Ada);
               Append_Line (Printout'Access, "   " & Filename);
            end;

            Printout := Printout & ASCII.LF;
         end if;

         Src_Files := Get_C_And_CPP_Src_Files;
         Has_C_Files := Natural (Src_Files.Length) > 0;

         if Has_C_Files then
            New_Index_Section (Printout'Access);

            declare
               Filename : constant String := "c_files_idx";
            begin
               Files_Vector_Sort.Sort (Src_Files);
               Generate_Files_Index
                 (Src_Files => Src_Files,
                  Filename  => Filename,
                  Header    => "C & C++ entities per file");
               Append_Line (Printout'Access, "   " & Filename);
            end;

            declare
               Filename : constant String := "c_entities_idx";
            begin
               Generate_Entities_Index
                 (Filename => Filename,
                  Header   => "All C & C++ entities",
                  Filter   => Lang_C_CPP);
               Append_Line (Printout'Access, "   " & Filename);
            end;

            Printout := Printout & ASCII.LF;
         end if;

         New_Index_Section (Printout'Access);

         declare
            Filename : constant String := "prj_files_idx";
         begin
            Generate_Project_Files_Index
              (Filename  => Filename,
               Header    => "Projects source files");
            Append_Line (Printout'Access, "   " & Filename);
         end;

         if Has_Ada_Files and Has_C_Files then
            Src_Files := Backend.Src_Files;
            if Natural (Src_Files.Length) > 0 then
               declare
                  Filename : constant String := "ada_files_idx";
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
            Filename  => To_ReST_Name ("index"),
            Text =>
              Parse (+Tmpl.Full_Name, Translation, Cached => True));

         Stop (My_Delay, Generate_Global_Index_Time);
      end Generate_Global_Indexes;

   --  Start of processing for Finalize

   begin
      Generate_Annotated_Ada_Sources;

      if Update_Global_Index then
         Generate_Global_Indexes;
      end if;
   end Finalize;

   ------------------
   -- Get_Entities --
   ------------------

   function Get_Entities
     (Context : access constant Docgen_Context;
      Entity  : Entity_Id) return access EInfo_List.Vector is
   begin
      if Is_Partial_View (Entity) and then Context.Options.Show_Private then
         return Get_Entities (Get_Full_View (Entity));
      else
         return Get_Entities (Entity);
      end if;
   end Get_Entities;

   ---------------------------
   -- Get_Inherited_Methods --
   ---------------------------

   function Get_Inherited_Methods
     (Context : access constant Docgen_Context;
      Entity  : Entity_Id) return access EInfo_List.Vector is
   begin
      if Is_Partial_View (Entity) and then Context.Options.Show_Private then
         EInfo_Vector_Sort_Short.Sort
           (Get_Inherited_Methods (Get_Full_View (Entity)).all);
         return Get_Inherited_Methods (Get_Full_View (Entity));
      else
         EInfo_Vector_Sort_Short.Sort (Get_Inherited_Methods (Entity).all);
         return Get_Inherited_Methods (Entity);
      end if;
   end Get_Inherited_Methods;

   -----------------
   -- Get_Methods --
   -----------------

   function Get_Methods
     (Context : access constant Docgen_Context;
      Entity  : Entity_Id) return access EInfo_List.Vector is
   begin
      if Is_Partial_View (Entity) and then Context.Options.Show_Private then
         EInfo_Vector_Sort_Short.Sort
           (Get_Methods (Get_Full_View (Entity)).all);
         return Get_Methods (Get_Full_View (Entity));
      else
         EInfo_Vector_Sort_Short.Sort (Get_Methods (Entity).all);
         return Get_Methods (Entity);
      end if;
   end Get_Methods;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (E : Entity_Id;
      Use_Full_Name : Boolean := False) return String is
   begin
      if Use_Full_Name then
         return Get_Full_Name (E);

      elsif not Is_Subprogram_Or_Entry (E)
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
              or else Name = "/="
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
     (Self : Simple_Backend'Class;
      Kind : Template_Kind) return Virtual_File is
   begin
      case Kind is
         when Tmpl_Entities =>
            return Self.Get_Resource_File ("entities.tmpl");
         when Tmpl_Files_Index =>
            return Self.Get_Resource_File ("files_index.tmpl");
         when Tmpl_Global_Index =>
            return Self.Get_Resource_File ("index.tmpl");
         when Tmpl_Prj_Files_Index =>
            return Self.Get_Resource_File ("prj_index.tmpl");
         when Tmpl_Src_File =>
            return Self.Get_Resource_File ("src.tmpl");
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
           Backend.Get_Resource_File ("support/");
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
      GNATdoc.Backend.Base.Base_Backend (Backend).Initialize (Context);

      Generate_Support_Files (Context.Kernel);
   end Initialize;

   ----------
   -- Name --
   ----------

   overriding function Name (Self : Simple_Backend) return String is
      pragma Unreferenced (Self);

   begin
      return "simple";
   end Name;

   ---------------------------------
   -- Generate_Lang_Documentation --
   ---------------------------------

   overriding procedure Generate_Lang_Documentation
     (Backend     : in out Simple_Backend;
      Tree        : access Tree_Type;
      Entity      : Entity_Id;
      Entities    : Base.Collected_Entities;
      Scope_Level : Natural)
   is
      Root_Level  : constant Natural := 0;
      Tmpl        : constant Virtual_File :=
                      Backend.Get_Template (Tmpl_Entities);
      Translation : Translate_Set;

      procedure For_All
        (Vector   : EInfo_List.Vector;
         Printout : access Unbounded_String;
         Process  : access procedure
                             (Printout : access Unbounded_String;
                              E_Info   : Entity_Id;
                              Context  : access constant Docgen_Context));
      --  Call subprogram Process for all the elements of Vector

      procedure Append_Generic_Formal
        (Printout : access Unbounded_String;
         E        : Entity_Id;
         Context  : access constant Docgen_Context);
      --  Append to Printout the reStructured output of a generic formal

      -------------
      -- For_All --
      -------------

      procedure For_All
        (Vector   : EInfo_List.Vector;
         Printout : access Unbounded_String;
         Process  : access procedure
                             (Printout : access Unbounded_String;
                              E_Info   : Entity_Id;
                              Context  : access constant Docgen_Context))
      is
         Cursor  : EInfo_List.Cursor;

      begin
         Cursor := Vector.First;
         while EInfo_List.Has_Element (Cursor) loop
            Process (Printout,
              E_Info  => EInfo_List.Element (Cursor),
              Context => Backend.Context);
            EInfo_List.Next (Cursor);
         end loop;
      end For_All;

      ---------------------------
      -- Append_Generic_Formal --
      ---------------------------

      procedure Append_Generic_Formal
        (Printout : access Unbounded_String;
         E        : Entity_Id;
         Context  : access constant Docgen_Context) is
      begin
         if Is_Package (E) then
            null;  --  unsupported yet???

         elsif Get_Kind (E) = E_Variable then
            ReST_Append_Simple_Declaration (Printout, E, Backend.Context);

         elsif LL.Is_Type (E) then
            if Is_Class_Or_Record_Type (E) then
               ReST_Append_Record_Type_Declaration
                 (Printout, E, Backend.Context);
            else
               ReST_Append_Simple_Declaration (Printout, E, Backend.Context);
            end if;

         elsif Is_Subprogram (E) then
            ReST_Append_Subprogram (Printout, E, Context);

            --  Here we cover generic formals which are not fully decorated
            --  We assume that the output associated with these missing
            --  cases is simple. More work needed here???

         else
            ReST_Append_Simple_Declaration (Printout, E, Backend.Context);
         end if;
      end Append_Generic_Formal;

      --  Local variables

      My_Delay : Delay_Time;
      Printout : aliased Unbounded_String;
      use type Ada.Containers.Count_Type;

   --  Start of processing for Process_File

   begin
      Trace (Me, "Process_File " & (+Tree.File.Base_Name));
      Start (My_Delay);

      if In_Ada_Language (Entity) then
         if Is_Package (Entity) then
            if not Backend.Entities.Pkgs.Contains (Entity) then
               Backend.Entities.Pkgs.Append (Entity);
            end if;

         elsif Is_Subprogram (Entity) then
            if not Backend.Entities.Subprgs.Contains (Entity) then
               Backend.Entities.Subprgs.Append (Entity);
            end if;

         elsif Is_Concurrent_Type_Or_Object (Entity) then
            --  Concurrent types and objects are handled "twice" for purposes
            --  of HTML backend. Simple backend generates documentation for
            --  them at time of processing of enclosing package.

            return;
         end if;
      end if;

      if Entities.Access_Types.Length > 0
        or else Entities.CPP_Classes.Length > 0
        or else Entities.CPP_Constructors.Length > 0
        or else Entities.Generic_Formals.Length > 0
        or else Entities.Interface_Types.Length > 0
        or else Entities.Methods.Length > 0
        or else Entities.Pkgs.Length > 0
        or else Entities.Pkgs_Instances.Length > 0
        or else Entities.Record_Types.Length > 0
        or else Entities.Simple_Types.Length > 0
        or else Entities.Subprgs.Length > 0
        or else Entities.Tagged_Types.Length > 0
        or else Entities.Variables.Length > 0
        or else Entities.Tasks.Length > 0
        or else Entities.Protected_Objects.Length > 0
      then
         Append_Line (Printout'Access, "Entities");
         Append_Line (Printout'Access, "========");
         Append_Line (Printout'Access, "");

         ReST_Append_List
           (Printout'Access, Entities.Generic_Formals, "Generic formals");
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
           (Printout'Access, Entities.Tasks, "Tasks & task types types");
         ReST_Append_List
           (Printout'Access, Entities.Protected_Objects, "Protected objects");
         ReST_Append_List
           (Printout'Access, Entities.CPP_Classes, "C++ Classes");
         ReST_Append_List
           (Printout'Access, Entities.Subprgs, "Subprograms");

         if In_Ada_Language (Entity) then
            ReST_Append_List
              (Printout'Access, Entities.Methods, "Dispatching subprograms");
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
              (Printout'Access,
               Entities.Pkgs & Entities.Pkgs_Instances,
               "Nested packages");
         end if;

         --  Generate full documentation

         For_All
           (Vector   => Entities.Generic_Formals,
            Printout => Printout'Access,
            Process  => Append_Generic_Formal'Access);
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
           (Entities.Tasks,
            Printout'Access,
            ReST_Append_Concurrent_Type_Declaration'Access);

         For_All
           (Entities.Protected_Objects,
            Printout'Access,
            ReST_Append_Concurrent_Type_Declaration'Access);

         For_All
           (Entities.Subprgs,
            Printout'Access,
            ReST_Append_Subprogram'Access);
      end if;

      declare
         Doc_Dir     : constant Virtual_File :=
           Get_Doc_Directory (Backend.Context.Kernel);
         Filename    : constant String := File_Containing (Entity);
         ReST_Header : constant String (Filename'Range) := (others => '*');
         ReST_File   : constant Filesystem_String :=
           To_ReST_Name (Filesystem_String (Filename));
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
            if Get_Kind (Entity) = E_Generic_Package then
               Header :=
                 Header
                 & ASCII.LF
                 & "Generic package."
                 & ASCII.LF
                 & ASCII.LF
                 & To_ReST (Get_Comment (Entity))
                 & ASCII.LF;
            else
               Header :=
                 Header
                 & ASCII.LF
                 & To_ReST (Get_Comment (Entity))
                 & ASCII.LF;
            end if;

            if Present (LL.Get_Instance_Of (Entity)) then
               ReST_Append_Simple_Declaration
                 (Printout'Access, Entity, Backend.Context);

            elsif Is_Subprogram (Entity) then
               ReST_Append_Subprogram
                 (Printout'Access, Entity, Backend.Context);
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

      Stop (My_Delay, Generate_Doc_Time);
   end Generate_Lang_Documentation;

   ---------------------
   -- To_Listing_Name --
   ---------------------

   function To_Listing_Name (Basename : String) return String is
   begin
      return "lst_" & Basename;
   end To_Listing_Name;

   ------------------
   -- To_Html_Name --
   ------------------

   function To_Html_Name
     (Basename : Filesystem_String) return Filesystem_String
   is
   begin
      return Basename & ".html";
   end To_Html_Name;

   ------------------
   -- To_ReST_Name --
   ------------------

   function To_ReST_Name
     (Basename : Filesystem_String) return Filesystem_String
   is
   begin
      return Basename & ".rst";
   end To_ReST_Name;

end GNATdoc.Backend.Simple;
