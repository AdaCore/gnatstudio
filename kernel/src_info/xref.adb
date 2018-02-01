------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers;            use Ada.Containers;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.SQL.Sqlite;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Scripts.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNAT.SHA1;
with GNAT.Strings;              use GNAT.Strings;
with Language_Handlers;         use Language_Handlers;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with Language; use Language;

package body Xref is
   Me : constant Trace_Handle := Create ("GPS.KERNEL.Xref");

   Force_Local_Database : constant Trace_Handle := Create
     ("GPS.INTERNAL.FORCE_LOCAL_DB", Off);
   --  Whether to use a DB in the temporary directory

   Constructs_Heuristics : constant Trace_Handle :=
     Create ("GPS.INTERNAL.Entities_Constructs", On);

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Root_Entity'Class, Root_Entity_Access);

   ---------------------------
   --  Note for development --
   ---------------------------

   --  A lot of functions defined here use either the new system
   --  (GNATCOLL.Xref) or the legacy database (Entities.*), and
   --  sometimes fallback on the constructs database.

   package Entity_Lists is new Ada.Containers.Doubly_Linked_Lists
      (General_Entity);
   use Entity_Lists;

   function Get_Location
     (Ref : Entity_Reference) return General_Location;
   --  Return the General Location of a GNATCOLL reference

   procedure Node_From_Entity
     (Self        : access General_Xref_Database_Record'Class;
      Handler     : access Abstract_Language_Handler_Record'Class;
      Decl        : General_Location;
      Ent         : out Entity_Access;
      Tree_Lang   : out Tree_Language_Access;
      Name        : String := "");
   --  Returns the constructs data for a given entity. Name is optional. If it
   --  is given, this will perform a search by name in the construct database.
   --  If the result is unique, then it will return it.

   function Construct_From_Entity
     (Self   : access General_Xref_Database_Record'Class;
      Entity : General_Entity) return access Simple_Construct_Information;
   --  Returns Construct_Information from an Entity. This access shouldn't be
   --  kept because it will be invalid next time the constructs database is
   --  updated

   function To_String (Loc : General_Location) return String;
   --  Display Loc

   function To_General_Entity
     (Db : access General_Xref_Database_Record'Class;
      E  : Entity_Information) return General_Entity;
   --  Convert Xref.Entity_Information to General_Entity

   procedure Fill_Entity_Array
     (Db   : General_Xref_Database;
      Curs : in out Entities_Cursor'Class;
      Arr  : in out Entity_Lists.List);
   --  Store all entities returned by the cursor into the array

   function To_Entity_Array (Arr : Entity_Lists.List) return Entity_Array;
   --  Creates an entity array.
   --  ??? This is not very efficient

   function Get_Entity_At_Location
     (Db  : access General_Xref_Database_Record'Class;
      Loc : General_Location) return Entity_Access;
   --  Return the construct entity found at the location given in parameter.

   procedure Reference_Iterator_Get_References
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : in out References_Cursor'Class);
   --  Wraps GNATCOLL.Xref.References to pass correct parameters

   procedure Close_Database (Self   : General_Xref_Database);
   --  Close the database connection (and perhaps remove the sqlite database
   --  if we were using a temporary project).

   procedure Open_Database (Self   : General_Xref_Database;
                            Tree   : Project_Tree_Access);
   --  Open the database connection

   ----------------
   -- Assistants --
   ----------------
   --  These types are used for the constructs database.

   LI_Assistant_Id : constant String := "LI_ASSISTANT";

   type LI_Db_Assistant is new Database_Assistant with record
      LI_Key : Construct_Annotations_Pckg.Annotation_Key;
      Db     : General_Xref_Database;
   end record;

   type LI_Db_Assistant_Access is access all LI_Db_Assistant'Class;

   type LI_Annotation is new
     Construct_Annotations_Pckg.General_Annotation_Record
   with record
      Entity : General_Entity;
   end record;

   overriding procedure Free (Obj : in out LI_Annotation);

   function To_LI_Entity
     (Self : access General_Xref_Database_Record'Class;
      E    : Entity_Access) return General_Entity;
   --  Return an LI entity based on a construct entity. Create one if none.

   ---------------
   -- To_String --
   ---------------

   function To_String (Loc : General_Location) return String is
   begin
      if Loc = No_Location then
         return "<no_loc>";
      else
         return Loc.File.Display_Base_Name & ':'
           & Image (Loc.Line, Min_Width => 1) & ':'
           & Image (Integer (Loc.Column), Min_Width => 1);
      end if;
   end To_String;

   -------------------
   -- Documentation --
   -------------------

   overriding procedure Documentation
     (Handler           : Language_Handlers.Language_Handler;
      Entity            : General_Entity;
      Formater          : access Profile_Formater'Class;
      Check_Constructs  : Boolean := True;
      Look_Before_First : Boolean := True)
   is
      function Doc_From_Constructs return Boolean;
      procedure Doc_From_LI;

      Decl : constant General_Location :=
        Get_Declaration (Entity).Loc;
      Context : constant Language.Language_Context_Access :=
        Language.Get_Language_Context
          (Get_Language_From_File (Handler, Source_Filename => Decl.File));

      -------------------------
      -- Doc_From_Constructs --
      -------------------------

      function Doc_From_Constructs return Boolean is
         Ent       : Entity_Access;
         Tree_Lang : Tree_Language_Access;
         Buffer    : GNAT.Strings.String_Access;
         Node      : Construct_Tree_Iterator;
      begin
         Node_From_Entity (Entity.Db, Handler, Decl, Ent, Tree_Lang);

         if Ent = Null_Entity_Access then
            return False;
         end if;

         Buffer := Get_Buffer (Get_File (Ent));
         Node   := To_Construct_Tree_Iterator (Ent);

         --  If the constructs have been properly loaded
         if Get_Construct (Node).Sloc_Start.Index /= 0 then
            declare
               Comment : constant String :=
                 Extract_Comment
                   (Buffer            => Buffer.all,
                    Decl_Start_Index  => Get_Construct (Node).Sloc_Start.Index,
                    Decl_End_Index    => Get_Construct (Node).Sloc_End.Index,
                    Language          => Context.Syntax,
                    Look_Before_First => Look_Before_First);
            begin
               Get_Profile (Tree_Lang, Ent, Formater, With_Aspects => True);

               if Comment /= "" then
                  Formater.Add_Comments (Comment);
               end if;

               return True;
            end;
         else
            return False;
         end if;
      end Doc_From_Constructs;

      -----------------
      -- Doc_From_LI --
      -----------------

      procedure Doc_From_LI is
      begin
         if Entity.Entity /= No_Entity then
            Formater.Add_Comments
              (Ada.Strings.Fixed.Trim
                 (Entity.Db.Xref.Comment (Entity.Entity, Context.Syntax)
                  & ASCII.LF
                  & Entity.Db.Xref.Text_Declaration (Entity.Entity),
                  Left  => Ada.Strings.Maps.Null_Set,
                  Right => Ada.Strings.Maps.To_Set
                    (' ' & ASCII.HT & ASCII.LF & ASCII.CR)));
         end if;
      end Doc_From_LI;

   --  Start of processing for Documentation

   begin
      if not Check_Constructs then
         Doc_From_LI;
      elsif not Doc_From_Constructs then
         Doc_From_LI;
      end if;

      --  If still not found, we used to default to also searching just before
      --  the body. But when there is a separate spec, the doc should be there
      --  and when we don't have a separate spec the "declaration" is the
      --  location of the body.
   end Documentation;

   -------------------------------
   -- For_Each_Dispatching_Call --
   -------------------------------

   overriding procedure For_Each_Dispatching_Call
     (Ref       : General_Entity_Reference;
      On_Callee : access function (Callee : Root_Entity'Class) return Boolean;
      Filter    : Reference_Kind_Filter := null)
   is
      Prim_Ent  : General_Entity;
   begin
      --  Handle cases in which no action is needed

      if Ref.Db = null
        or else not Ref.Is_Dispatching_Call
      then
         return;
      end if;

      declare
         function Should_Show (E : Entity_Information) return Boolean;
         --  Whether we should display E

         -----------------
         -- Should_Show --
         -----------------

         function Should_Show (E : Entity_Information) return Boolean is
            R : References_Cursor;
         begin
            if Filter = null then
               return True;
            end if;

            Ref.Db.Xref.References (E, R);
            while R.Has_Element loop
               if Filter
                 (General_Entity_Reference'
                    (Ref => R.Element, Db => Ref.Db))
               then
                  return True;
               end if;
               R.Next;
            end loop;
            return False;
         end Should_Show;

         Cursor : Recursive_Entities_Cursor;
         Prim   : Entity_Information;

      begin
         Prim     := Ref.Ref.Entity;
         Prim_Ent := To_General_Entity (Ref.Db, Prim);

         if Should_Show (Prim)
           and then not On_Callee (Callee => Prim_Ent)
         then
            return;
         end if;

         Recursive
           (Self    => Ref.Db.Xref,
            Entity  => Ref.Ref.Entity,
            Compute => Overridden_By'Unrestricted_Access,
            Cursor  => Cursor);

         while Cursor.Has_Element loop
            Prim     := Cursor.Element;
            Prim_Ent := To_General_Entity (Ref.Db, Prim);

            exit when Should_Show (Prim_Ent.Entity)
              and then not On_Callee
                (Callee       => Prim_Ent);

            Cursor.Next;
         end loop;

      exception
         when E : others =>
            Trace (Me, "Unexpected exception: "
                   & Exception_Information (E));
      end;
   end For_Each_Dispatching_Call;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Ref : General_Entity_Reference) return Root_Entity'Class
   is
      E : General_Entity;
   begin
      --  Attempt to use the sqlite system

      if Ref.Ref /= No_Entity_Reference then
         E.Entity := Ref.Ref.Entity;
         E.Db     := Ref.Db;
      else
         E := No_General_Entity;
      end if;

      return E;
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Db   : access General_Xref_Database_Record;
      Name : String;
      Loc  : General_Location;
      Approximate_Search_Fallback : Boolean := True;
      Closest_Ref  : out Root_Entity_Reference_Ref) return Root_Entity'Class
   is
      Lang_Name : constant String :=
        Db.Lang_Handler.Get_Language_From_File (Loc.File).Get_Name;
      package LDB renames Lang_Specific_Databases_Maps;
      use type LDB.Cursor;

      Cursor : constant LDB.Cursor :=
        Db.Lang_Specific_Databases.Find (Lang_Name);
   begin
      if Cursor /= LDB.No_Element then
         Closest_Ref :=
           Root_Entity_Reference_Refs.To_Holder (No_Root_Entity_Reference);
         return LDB.Element (Cursor).Get_Entity
           (General_Xref_Database (Db), Name, Loc);
      end if;

      return Find_Declaration_Or_Overloaded
        (General_Xref_Database (Db),
         Loc               => Loc,
         Entity_Name       => Name,
         Ask_If_Overloaded => False,
         Closest_Ref       => Closest_Ref,
         Approximate_Search_Fallback => Approximate_Search_Fallback);
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Db   : access General_Xref_Database_Record;
      Name : String;
      Loc  : General_Location;
      Approximate_Search_Fallback : Boolean := True) return Root_Entity'Class
   is
      Ref : Root_Entity_Reference_Ref;
   begin
      return Get_Entity (Db, Name, Loc, Approximate_Search_Fallback, Ref);
   end Get_Entity;

   ------------------------------------
   -- Find_Declaration_Or_Overloaded --
   ------------------------------------

   function Find_Declaration_Or_Overloaded
     (Self              : access General_Xref_Database_Record;
      Loc               : General_Location;
      Entity_Name       : String;
      Ask_If_Overloaded : Boolean := False;
      Closest_Ref       : out Root_Entity_Reference_Ref;
      Approximate_Search_Fallback : Boolean := True) return Root_Entity'Class
   is
      Fuzzy : Boolean;
      Closest_General_Ref : access General_Entity_Reference;

      function Internal_No_Constructs
        (Name : String; Loc : General_Location) return General_Entity;
      --  Search for the entity, without a fallback to the constructs db

      function Internal_No_Constructs
        (Name : String; Loc : General_Location) return General_Entity
      is
         Entity  : General_Entity := No_General_Entity;
         Set     : File_Info_Set;
         P       : Project_Type;
      begin
         if Loc = No_Location then
            --  predefined entities
            Closest_General_Ref.Ref := Self.Xref.Get_Entity
              (Name    => Name,
               File    => Predefined_Entity,
               Project => No_Project,
               Approximate_Search_Fallback => Approximate_Search_Fallback);
         else
            if Loc.Project_Path = No_File then
               Set := Self.Registry.Tree.Info_Set (Loc.File);
               declare
                  F_Info : constant File_Info'Class :=
                    File_Info'Class (Set.First_Element);
               begin
                  P := F_Info.Project;
               end;
            else
               P := GNATCOLL.Scripts.Projects
                 .Project_Tree.Project_From_Path (Loc.Project_Path);
            end if;

            --  Already handles the operators
            Closest_General_Ref.Ref := Self.Xref.Get_Entity
              (Name    => Name,
               File    => Loc.File,
               Line    => Loc.Line,
               Project => P,
               Column  => Loc.Column,
               Approximate_Search_Fallback => Approximate_Search_Fallback);
         end if;

         Entity.Entity := Closest_General_Ref.Ref.Entity;
         Fuzzy :=
         --  Multiple possible files ?
           (Loc.Project_Path = No_File and then Set.Length > 1)

           or else
             (Entity.Entity /= No_Entity and then
                (Is_Fuzzy_Match (Entity.Entity)
                     --  or else not Self.Xref.Is_Up_To_Date (Loc.File)
                ));

         declare
            ELoc : constant Entity_Reference :=
              Self.Xref.Declaration (Entity.Entity).Location;
         begin
            if ELoc /= No_Entity_Reference then
               Entity.Loc := (Line         => ELoc.Line,
                              Project_Path => ELoc.Project.Project_Path,
                              Column       => ELoc.Column,
                              File         => ELoc.File);
            else
               Entity.Loc := No_Location;
            end if;
         end;

         Entity.Is_Fuzzy := Fuzzy;
         return Entity;
      end Internal_No_Constructs;

      Entity : Root_Entity'Class := General_Entity'Class (No_General_Entity);
      type General_Entity_Reference_Access is
        access all General_Entity_Reference;
   begin
      Closest_Ref.Replace_Element
        (General_Entity_Reference'
           (Db => General_Xref_Database (Self), others => <>));

      Closest_General_Ref := General_Entity_Reference_Access
        (Closest_Ref.Reference.Element);

      if Entity_Name = "" then
         Entity := No_Root_Entity;
         return Entity;
      end if;

      if Active (Me) then
         Increase_Indent (Me, "Find_Declaration of " & Entity_Name
                          & " file=" & Loc.File.Display_Base_Name
                          & " projec_path=" &
                            Loc.Project_Path.Display_Base_Name
                          & " line=" & Loc.Line'Img
                          & " col=" & Loc.Column'Img);
      end if;

      Entity := General_Entity'Class
        (Internal_No_Constructs (Entity_Name, Loc));
      --  also sets Fuzzy

      if Fuzzy and then Ask_If_Overloaded then
         Entity := Select_Entity_Declaration
           (Self => Self,
            File   => Loc.File,
            Project => Get_Project (Loc),
            Entity => Entity);

         if Active (Me) then
            Decrease_Indent (Me);
         end if;
         General_Entity (Entity).Db := General_Xref_Database (Self);
         return Entity;
      end if;

      --  Fallback on constructs

      if (Entity = No_Root_Entity or else Fuzzy)
        and then Active (Constructs_Heuristics)
        and then Loc /= No_Location   --  Nothing for predefined entities
      then
         declare
            Tree_Lang : Tree_Language_Access;
            Result       : Entity_Access;
            Result_Loc   : Source_Location;
            New_Location : General_Location;
            New_Entity   : General_Entity := No_General_Entity;

         begin
            Trace (Me, "Searching entity declaration in constructs");
            Node_From_Entity
              (Self,
               Handler   => Self.Lang_Handler,
               Decl      => Loc,
               Ent       => Result,
               Tree_Lang => Tree_Lang,
               Name      => Entity_Name);

            if Result /= Null_Entity_Access
               and then Get (Get_Construct (Result).Name).all = Entity_Name
            then
               Result_Loc := Get_Construct (Result).Sloc_Entity;

               --  First, try to see if there's already a similar entity in
               --  the database. If that's the case, it's better to use it
               --  than the dummy one created from the construct.

               if Result_Loc.Line > 0 then
                  New_Location :=
                    (File    => Get_File_Path (Get_File (Result)),
                     Project_Path => Get_Project (Loc).Create_From_Project
                       (Get_File_Path (Get_File (Result)).Full_Name.all)
                       .Project.Project_Path,
                     Line    => Result_Loc.Line,
                     Column  => To_Visible_Column
                       (Get_File (Result),
                        Result_Loc.Line,
                        String_Index_Type (Result_Loc.Column)));

                  New_Entity := Internal_No_Constructs
                    (Name  => Get (Get_Construct (Result).Name).all,
                     Loc   => (File    => New_Location.File,
                               Project_Path => New_Location.Project_Path,
                               Line    => New_Location.Line,
                               Column  => New_Location.Column));
               end if;

               if New_Entity /= No_General_Entity
                 and then not Is_Fuzzy (New_Entity)
               then
                  --  If we found an updated ALI entity, use it.
                  Entity := General_Entity'Class (New_Entity);

               elsif Entity /= No_Root_Entity then
                  --  Reuse the ALI entity, since that gives us a chance to
                  --  query its references as well.
                  General_Entity'Class (Entity).Loc := New_Location;

               else
                  --  If we have no entity to connect to, then create one
                  --  from the construct database.

                  Entity := General_Entity'Class (To_LI_Entity (Self, Result));
               end if;

               General_Entity'Class (Entity).Is_Fuzzy := True;
            end if;
         end;
      end if;

      if Active (Me) then
         Decrease_Indent (Me);
      end if;

      General_Entity (Entity).Db := General_Xref_Database (Self);
      return Entity;
   end Find_Declaration_Or_Overloaded;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Entity : General_Entity) return String is
   begin
      if Entity.Entity /= No_Entity then
         return To_String
           (Declaration (Entity.Db.Xref.all, Entity.Entity).Name);
      elsif Entity.Loc /= No_Location then
         declare
            C : constant access Simple_Construct_Information :=
              Construct_From_Entity (Entity.Db, Entity);
         begin
            if C = null then
               return "";
            end if;

            return Get (C.Name).all;
         end;
      end if;
      return "";
   end Get_Name;

   --------------------
   -- Qualified_Name --
   --------------------

   overriding function Qualified_Name
     (Entity : General_Entity) return String
   is
   begin
      if Entity.Entity /= No_Entity then
         return Entity.Db.Xref.Qualified_Name (Entity.Entity);
      end if;
      return "";
   end Qualified_Name;

   ------------------
   -- Get_Location --
   ------------------

   overriding function Get_Location
     (Ref : General_Entity_Reference) return General_Location is
   begin
      if Ref.Ref /= No_Entity_Reference then
         return Get_Location (Ref.Ref);
      end if;
      return No_Location;
   end Get_Location;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Ref : Entity_Reference) return General_Location is
   begin
      if Ref = No_Entity_Reference then
         return No_Location;
      else
         return
           (File    => Ref.File,
            Project_Path => Ref.Project.Project_Path,
            Line    => Ref.Line,
            Column  => Visible_Column_Type (Ref.Column));
      end if;
   end Get_Location;

   ---------------------------
   -- Caller_At_Declaration --
   ---------------------------

   overriding function Caller_At_Declaration
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return General_Entity'
        (Entity => Entity.Db.Xref.Declaration
           (Entity.Entity).Location.Scope,
         Db     => Entity.Db,
         others => <>);
   end Caller_At_Declaration;

   ---------------------
   -- Get_Declaration --
   ---------------------

   overriding function Get_Declaration
     (Entity : General_Entity) return General_Entity_Declaration is
   begin
      if Entity.Loc /= No_Location then
         declare
            Result    : Entity_Access;
            Tree_Lang : Tree_Language_Access;
            Decl      : Entity_Access;
            Node      : Construct_Tree_Iterator;
            Cat       : Language_Category;
            Project   : Project_Type;
         begin
            Node_From_Entity
              (Entity.Db,
               Entity.Db.Lang_Handler,
               Entity.Loc, Result, Tree_Lang);

            if Result /= Null_Entity_Access then
               Decl := Get_Declaration
                  (Get_Tree_Language (Get_File (Result)), Result);
               Node := To_Construct_Tree_Iterator (Decl);
               Cat := Get_Construct (Node).Category;

               --  Find the project that controls the file (in the case of
               --  aggregate projects)
               Project := Get_Project (Entity.Loc).Create_From_Project
                 (Get_File_Path (Get_File (Decl)).Full_Name.all)
                 .Project;

               return (Loc => (File    => Get_File_Path (Get_File (Decl)),
                               Project_Path => Project.Project_Path,
                               Line   => Get_Construct (Node).Sloc_Entity.Line,
                               Column => Visible_Column_Type
                                 (Get_Construct (Node).Sloc_Entity.Column)),
                       Body_Is_Full_Declaration =>
                         Cat = Cat_Type or else Cat = Cat_Class,
                       Name =>
                         To_Unbounded_String
                           (Get (Get_Construct (Node).Name).all));
            end if;
         end;
      end if;

      if Entity.Entity /= No_Entity then
         declare
            Ref : constant Entity_Declaration :=
              Entity.Db.Xref.Declaration (Entity.Entity);
         begin
            if Ref /= No_Entity_Declaration then
               return (Loc    => (File    => Ref.Location.File,
                                  Project_Path =>
                                    Ref.Location.Project.Project_Path,
                                  Line    => Ref.Location.Line,
                                  Column  => Ref.Location.Column),
                       Body_Is_Full_Declaration =>
                         Ref.Flags.Body_Is_Full_Declaration,
                       Name   => Ref.Name);
            end if;
         end;
      end if;

      return No_General_Entity_Declaration;
   end Get_Declaration;

   ----------------------------
   -- Get_Entity_At_Location --
   ----------------------------

   function Get_Entity_At_Location
     (Db     : access General_Xref_Database_Record'Class;
      Loc : General_Location) return Entity_Access
   is
      S_File : constant Structured_File_Access :=
        Get_Or_Create
          (Db   => Db.Constructs,
           File => Loc.File);
      Construct : Construct_Tree_Iterator;
   begin
      Update_Contents (S_File);

      Construct :=
        Get_Iterator_At
          (Tree      => Get_Tree (S_File),
           Location  => To_Location
             (Loc.Line,
              To_Line_String_Index
                (S_File,
                 Loc.Line,
                 Loc.Column)),
           From_Type => Start_Name);

      if Construct /= Null_Construct_Tree_Iterator then
         return To_Entity_Access (S_File, Construct);
      else
         return Null_Entity_Access;
      end if;
   end Get_Entity_At_Location;

   --------------
   -- Get_Body --
   --------------

   overriding function Get_Body
     (Entity : General_Entity;
      After  : General_Location := No_Location) return General_Location
   is
      Block_Me : constant Block_Trace_Handle := Create
         (Me,
          (if Active (Me)
           then Get_Name (Entity) & " fuzzy=" & Is_Fuzzy (Entity)'Img
           else "")) with Unreferenced;

      function Extract_Next_By_Heuristics return General_Location;
      --  Return the next body location using the construct heuristics

      function Is_Location_For_Entity
        (Location : General_Location) return Boolean;
      --  Return true if the location given in parameter indeed corresponds to
      --  a declaration construct, false otherwise, typically when the file has
      --  been modified and the ali retreived is not up to date.
      --  Note that if the construct database is deactivated, this will always
      --  return true (we're always on the expected construct, we don't expect
      --  anything in particular).

      ----------------------------
      -- Is_Location_For_Entity --
      ----------------------------

      function Is_Location_For_Entity
        (Location : General_Location) return Boolean
      is
         C_Entity : Entity_Access;
      begin
         if Active (Constructs_Heuristics) then
            C_Entity := Get_Entity_At_Location (Entity.Db, Location);

            --  Return true if we found a construct here and if it's of the
            --  appropriate name.

            return C_Entity /= Null_Entity_Access
              and then Get (Get_Identifier (C_Entity)).all =
              Get_Name (Entity);
         end if;

         return True;
      end Is_Location_For_Entity;

      --------------------------------
      -- Extract_Next_By_Heuristics --
      --------------------------------

      function Extract_Next_By_Heuristics return General_Location is
         C_Entity, New_Entity : Entity_Access := Null_Entity_Access;
         Loc : General_Location;
         P   : Project_Type;

      begin
         --  In order to locate the reference to look from, we check if there
         --  is a file associated to the input location. In certain cases, this
         --  location is computed from a context that does not have file
         --  information, so for safety purpose, we check that the file exist
         --  (there's nothing we can do at the completion level without a
         --  file). If there's no file, then the context has been partially
         --  provided (or not at all) so we start from the declaration of the
         --  Entity.

         if Active (Constructs_Heuristics) then
            if After /= No_Location then
               C_Entity := Get_Entity_At_Location (Entity.Db, After);
            end if;

            if C_Entity = Null_Entity_Access then
               if Entity.Loc /= No_Location then
                  C_Entity := Get_Entity_At_Location (Entity.Db, Entity.Loc);
               else
                  Loc := Get_Declaration (Entity).Loc;
                  if Loc /= No_Location then
                     C_Entity := Get_Entity_At_Location (Entity.Db, Loc);
                  end if;
               end if;
            end if;

            if C_Entity /= Null_Entity_Access then
               declare
                  S_File : constant Structured_File_Access :=
                    Get_File (C_Entity);

                  Tree_Lang : constant Tree_Language_Access :=
                    Get_Tree_Language_From_File
                      (Entity.Db.Lang_Handler, Get_File_Path (S_File));
               begin
                  New_Entity := Tree_Lang.Find_Next_Part (C_Entity);

                  --  If we're initializing a loop, e.g. the current location
                  --  is no location, then return the result. Otherwise, don't
                  --  return it if we got back to the initial body and the
                  --  caller doesn't want to loop back.

                  if New_Entity /= C_Entity then
                     P := Get_Project (Loc).Create_From_Project
                       (Get_File_Path (Get_File (New_Entity)).Full_Name.all)
                       .Project;

                     return
                       (File    => Get_File_Path (Get_File (New_Entity)),
                        Project_Path => P.Project_Path,
                        Line    => Get_Construct (New_Entity).Sloc_Entity.Line,
                        Column  =>
                          To_Visible_Column
                            (Get_File (New_Entity),
                             Get_Construct (New_Entity).Sloc_Entity.Line,
                             String_Index_Type
                               (Get_Construct (New_Entity).Sloc_Entity.Column
                               )));
                  end if;
               end;
            end if;
         end if;

         return No_Location;
      end Extract_Next_By_Heuristics;

      Candidate : General_Location := No_Location;
      Decl_Loc : constant General_Location := Get_Declaration (Entity).Loc;

   begin
      if After = No_Location
        or else After = Decl_Loc
      then
         declare
            H_Loc : constant General_Location := Extract_Next_By_Heuristics;
         begin
            if Active (Me) then
               Trace (Me, "Body computed from constructs at "
                      & To_String (H_Loc));
            end if;

            if H_Loc /= No_Location and then
            --  If we found nothing, use the information from the constructs.
              (Candidate = No_Location

               --  it's OK to return the first entity.
               or else not Is_Location_For_Entity (Candidate))

            then
               Candidate := H_Loc;

               --  else if the candidate is at the expected location and if
               if Active (Me) then
                  Trace (Me, "Use body from constructs");
               end if;
            end if;
         end;
      end if;

      if Candidate /= No_Location then
         return Candidate;
      end if;

      if Entity.Entity /= No_Entity then
         declare
            C   : References_Cursor;
            Ref : Entity_Reference;
            Matches : Boolean := After = No_Location;
            First  : General_Location := No_Location;
            Is_First : Boolean := True;
         begin
            Bodies (Entity.Db.Xref.all, Entity.Entity, Cursor => C);
            while Has_Element (C) loop
               Ref := Element (C);

               if Ref /= No_Entity_Reference then
                  if Is_First then
                     Is_First := False;
                     First := (File         => Ref.File,
                               Project_Path => Ref.Project.Project_Path,
                               Line         => Ref.Line,
                               Column => Visible_Column_Type (Ref.Column));
                  end if;

                  if Matches then
                     Candidate :=
                       (File    => Ref.File,
                        Project_Path => Ref.Project.Project_Path,
                        Line    => Ref.Line,
                        Column  => Visible_Column_Type (Ref.Column));
                     exit;
                  else
                     Matches := Ref.Line = After.Line
                       and then Ref.Column = After.Column
                       and then Ref.File = After.File;
                  end if;
               end if;

               Next (C);
            end loop;

            if Candidate = No_Location then
               --  The "After" parameter did not correspond to a body
               Candidate := First;
            end if;
         end;
      end if;

      return Candidate;
   end Get_Body;

   -----------------
   -- Get_Type_Of --
   -----------------

   overriding function Get_Type_Of
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New (Entity.Db, Entity.Db.Xref.Type_Of (Entity.Entity));
   end Get_Type_Of;

   -------------------
   -- Returned_Type --
   -------------------

   overriding function Returned_Type
     (Entity : General_Entity) return Root_Entity'Class is
   begin
      return From_New (Entity.Db, Entity.Db.Xref.Type_Of (Entity.Entity));
   end Returned_Type;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   overriding function Is_Predefined_Entity
     (E  : General_Entity) return Boolean is
   begin
      return Is_Predefined_Entity (Declaration (E.Db.Xref.all, E.Entity));
   end Is_Predefined_Entity;

   ----------------------
   -- Node_From_Entity --
   ----------------------

   procedure Node_From_Entity
     (Self        : access General_Xref_Database_Record'Class;
      Handler     : access Abstract_Language_Handler_Record'Class;
      Decl        : General_Location;
      Ent         : out Entity_Access;
      Tree_Lang   : out Tree_Language_Access;
      Name        : String := "")
   is
      Data_File   : Structured_File_Access;
   begin
      Ent := Null_Entity_Access;
      Tree_Lang := Get_Tree_Language_From_File (Handler, Decl.File, False);
      Data_File := Language.Tree.Database.Get_Or_Create
        (Db   => Self.Constructs,
         File => Decl.File);
      Update_Contents (Data_File);

      --  In some cases, the references are extracted from a place
      --  where there is still an ALI file, but no more source file.
      --  This will issue a null Structured_File_Access, which is why
      --  we're protecting the following code with the above condition

      if not Is_Null (Data_File) then
         --  Find_Declaration does more than Get_Iterator_At, so use it.
         Ent := Tree_Lang.Find_Declaration
            (Data_File, Decl.Line,
             To_Line_String_Index (Data_File, Decl.Line, Decl.Column));
      end if;

      if Ent = Null_Entity_Access
        and then Name /= ""
      then
         declare
            It : Construct_Db_Iterator := Self.Constructs.Start (Name, True);
         begin
            if not At_End (It) then
               Ent := Get (It);
               Next (It);
            end if;

            --  Return a null result if there is more than one result

            if not At_End (It) then
               Ent := Null_Entity_Access;
            end if;
         end;
      end if;

   end Node_From_Entity;

   ---------------------------
   -- Construct_From_Entity --
   ---------------------------

   function Construct_From_Entity
     (Self   : access General_Xref_Database_Record'Class;
      Entity : General_Entity) return access Simple_Construct_Information
   is
      Result    : Entity_Access;
      Tree_Lang : Tree_Language_Access;
      Decl      : Entity_Access;
      Node      : Construct_Tree_Iterator;

   begin
      Node_From_Entity
        (Self, Self.Lang_Handler, Entity.Loc, Result, Tree_Lang);

      if Result /= Null_Entity_Access then
         Decl := Get_Declaration
           (Get_Tree_Language (Get_File (Result)), Result);
         Node := To_Construct_Tree_Iterator (Decl);

         return Get_Construct (Node);
      end if;

      return null;
   end Construct_From_Entity;

   ------------------
   -- Pointed_Type --
   ------------------

   overriding function Pointed_Type
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New (Entity.Db, Entity.Db.Xref.Pointed_Type (Entity.Entity));
   end Pointed_Type;

   -----------------------
   -- To_General_Entity --
   -----------------------

   function To_General_Entity
     (Db : access General_Xref_Database_Record'Class;
      E  : Entity_Information) return General_Entity
   is
      Decl : constant Entity_Declaration := Declaration (Db.Xref.all, E);
      Loc  : General_Location;

   begin
      Loc :=
        (File    => Decl.Location.File,
         Project_Path => Decl.Location.Project.Project_Path,
         Line    => Decl.Location.Line,
         Column  => Visible_Column_Type (Decl.Location.Column));

      return General_Entity (Get_Entity
                             (Db   => Db,
                              Name => To_String (Decl.Name),
                              Loc  => Loc));
   end To_General_Entity;

   -----------------
   -- Renaming_Of --
   -----------------

   overriding function Renaming_Of
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return General_Entity'
        (Entity => Entity.Db.Xref.Renaming_Of (Entity.Entity),
         Db     => Entity.Db,
         others => <>);
   end Renaming_Of;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Ref1, Ref2 : General_Entity_Reference) return Boolean is
   begin
      return Ref1.Ref = Ref2.Ref;
   end "=";

   ---------
   -- "=" --
   ---------

   overriding function "=" (E1, E2 : General_Entity) return Boolean is
   begin
      if E1.Entity = No_Entity and then E2.Entity = No_Entity then
         return E1.Loc = E2.Loc;
      else
         return E1.Entity = E2.Entity;
      end if;
   end "=";

   -------------------------
   -- Find_All_References --
   -------------------------

   function Find_All_References
      (Self     : access General_Xref_Database_Record;
       File     : GNATCOLL.VFS.Virtual_File;
       Kind     : String := "";
       Sort     : References_Sort := GNATCOLL.Xref.By_Location)
     return Root_Reference_Iterator'Class
   is
      Iter : Entity_Reference_Iterator;
   begin
      Iter.Db := General_Xref_Database (Self);
      Self.Xref.References
        (File => File, Cursor => Iter.Iter, Kind => Kind, Sort => Sort);
      return Iter;
   end Find_All_References;

   ---------------------------------------
   -- Reference_Iterator_Get_References --
   ---------------------------------------

   procedure Reference_Iterator_Get_References
     (Self   : Xref_Database'Class;
      Entity : Entity_Information;
      Cursor : in out References_Cursor'Class)
   is
      C : constant GPS_Recursive_References_Cursor :=
         GPS_Recursive_References_Cursor (Cursor);
   begin
      Self.References
        (Entity, Cursor,
         Include_Implicit => C.Include_Implicit,
         Include_All      => C.Include_All,
         Kinds            => To_String (C.Kind));
   end Reference_Iterator_Get_References;

   -------------------------
   -- Find_All_References --
   -------------------------

   overriding function Find_All_References
     (Entity             : General_Entity;
      In_File            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      In_Scope           : Root_Entity'Class := No_Root_Entity;
      Include_Overriding : Boolean := False;
      Include_Overridden : Boolean := False;
      Include_Implicit   : Boolean := False;
      Include_All        : Boolean := False;
      Include_Renames    : Boolean := True;
      Kind               : String := "")
      return Root_Reference_Iterator'Class
   is
      Iter   : Entity_Reference_Iterator;
   begin
      Iter.Db := Entity.Db;
      Iter.Iter.Include_Implicit := Include_Implicit;
      Iter.Iter.Include_All := Include_All;
      Iter.Iter.Kind := To_Unbounded_String (Kind);
      Entity.Db.Xref.Recursive
        (Entity          => Entity.Entity,
         Compute         => Reference_Iterator_Get_References'Access,
         Cursor          => Iter.Iter,
         From_Overriding => Include_Overriding,
         From_Overridden => Include_Overridden,
         From_Renames    => Include_Renames);
      Iter.In_File  := In_File;
      Iter.In_Scope := General_Entity (In_Scope);

      while Has_Element (Iter.Iter)
        and then
          ((Iter.In_File /= No_File
            and then Iter.Iter.Element.File /= Iter.In_File)
           or else
             (Iter.In_Scope /= No_General_Entity
              and then Iter.Iter.Element.Scope /= Iter.In_Scope.Entity))
      loop
         Iter.Iter.Next;
      end loop;
      return Iter;
   end Find_All_References;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Iter : Entity_Reference_Iterator) return Boolean is
   begin
      return not Has_Element (Iter.Iter);
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Entity_Reference_Iterator) is
   begin
      Next (Iter.Iter);

      while Has_Element (Iter.Iter)
        and then
          ((Iter.In_File /= No_File
            and then Iter.Iter.Element.File /= Iter.In_File)
           or else
             (Iter.In_Scope /= No_General_Entity
              and then Iter.Iter.Element.Scope /= Iter.In_Scope.Entity))
      loop
         Iter.Iter.Next;
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Entity_Reference_Iterator) return Root_Entity_Reference'Class
   is
   begin
      return General_Entity_Reference'
        (Db      => Iter.Db,
         Ref     => Iter.Iter.Element);
   end Get;

   ----------------
   -- Get_Entity --
   ----------------

   overriding function Get_Entity
     (Iter : Entity_Reference_Iterator) return Root_Entity'Class is
   begin
      return General_Entity'(Entity     => Iter.Iter.Element.Entity,
                             Db         => Iter.Db,
                             others     => <>);
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Iter : in out Entity_Reference_Iterator) is
   begin
      null;
   end Destroy;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   overriding function Get_Current_Progress
     (Iter : Entity_Reference_Iterator) return Integer
   is
      pragma Unreferenced (Iter);
   begin
      return 1;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   overriding function Get_Total_Progress
     (Iter : Entity_Reference_Iterator) return Integer
   is
      pragma Unreferenced (Iter);
   begin
      --  precomputing the number of references is expensive (basically
      --  requires doing the query twice), and won't be needed anymore when
      --  we get rid of the asynchronous iterators.
      return 1;
   end Get_Total_Progress;

   -----------------------
   -- Show_In_Callgraph --
   -----------------------

   overriding function Show_In_Callgraph
     (Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Ref.Db.Xref.Show_In_Callgraph (Ref.Ref);
   end Show_In_Callgraph;

   ----------------
   -- Get_Caller --
   ----------------

   overriding function Get_Caller
     (Ref : General_Entity_Reference) return Root_Entity'Class
   is
   begin
      return General_Entity'
        (Entity => Ref.Ref.Scope,
         Db => Ref.Db,
         others => <>);
   end Get_Caller;

   -------------------
   -- Is_Subprogram --
   -------------------

   overriding function Is_Subprogram
     (E  : General_Entity) return Boolean
   is
   begin
      if E.Entity /= No_Entity then
         return E.Db.Xref.Declaration (E.Entity).Flags.Is_Subprogram;

      elsif E.Loc /= No_Location then
         declare
            C : constant access Simple_Construct_Information :=
              Construct_From_Entity (E.Db, E);

         begin
            return
              C /= null and then C.Category in Cat_Function | Cat_Procedure;
         end;
      end if;

      return False;
   end Is_Subprogram;

   ------------------
   -- Is_Container --
   ------------------

   overriding function Is_Container
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Container;
   end Is_Container;

   ----------------
   -- Is_Generic --
   ----------------

   overriding function Is_Generic
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Generic;
   end Is_Generic;

   ---------------
   -- Is_Global --
   ---------------

   overriding function Is_Global
     (E  : General_Entity) return Boolean is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Global;
   end Is_Global;

   ---------------------
   -- Is_Static_Local --
   ---------------------

   overriding function Is_Static_Local
     (E  : General_Entity) return Boolean is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Static_Local;
   end Is_Static_Local;

   -------------
   -- Is_Type --
   -------------

   overriding function Is_Type
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Type;
   end Is_Type;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   overriding function Is_Dispatching_Call
     (Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Ref.Db.Xref.Is_Dispatching_Call (Ref.Ref);
   end Is_Dispatching_Call;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Base_Entities_Cursor) return Boolean is
   begin
      return not Has_Element (Iter.Iter);
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Base_Entities_Cursor) return Root_Entity'Class is
   begin
      return General_Entity'
        (Entity => Element (Iter.Iter),
         Db     => Iter.Db,
         others => <>);
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Base_Entities_Cursor) is
   begin
      Next (Iter.Iter);
   end Next;

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   overriding function Get_All_Called_Entities
     (Entity : General_Entity) return Abstract_Entities_Cursor'Class
   is
      Result : Calls_Iterator;
   begin
      Result.Db := Entity.Db;
      Entity.Db.Xref.Calls (Entity.Entity, Result.Iter);
      return Result;
   end Get_All_Called_Entities;

   ----------------------
   -- Entities_In_File --
   ----------------------

   function Entities_In_File
     (Self    : access General_Xref_Database_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Name    : String := "") return Entities_In_File_Cursor
   is
      Result  : Entities_In_File_Cursor;
   begin
      Result.Db := General_Xref_Database (Self);

      if Name = "" then
         Self.Xref.Referenced_In (File, Project, Cursor => Result.Iter);
      else
         Self.Xref.Referenced_In
           (File, Project, Name, Cursor => Result.Iter);
      end if;
      return Result;
   end Entities_In_File;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Iter : Entities_In_File_Cursor) return Boolean is
   begin
      return At_End (Base_Entities_Cursor (Iter));
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Entities_In_File_Cursor) return Root_Entity'Class is
   begin
      return Get (Base_Entities_Cursor (Iter));
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Iter : in out Entities_In_File_Cursor) is
   begin
      Next (Base_Entities_Cursor (Iter));
   end Next;

   ------------------------------
   -- All_Entities_From_Prefix --
   ------------------------------

   function All_Entities_From_Prefix
     (Self       : access General_Xref_Database_Record'Class;
      Prefix     : String;
      Is_Partial : Boolean := True) return Entities_In_Project_Cursor
   is
      Block_Me : constant Block_Trace_Handle := Create
         (Me, (if Active (Me)
                 then " prefix=" & Prefix & " partial=" & Is_Partial'Img
                 else ""))
         with Unreferenced;
      Result : Entities_In_Project_Cursor;
   begin
      Result.Db := General_Xref_Database (Self);
      Self.Xref.From_Prefix
        (Prefix     => Prefix,
         Is_Partial => Is_Partial,
         Cursor     => Result.Iter);
      return Result;
   end All_Entities_From_Prefix;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Iter : in out Entities_In_Project_Cursor) is
   begin
      null;
   end Destroy;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Iter : Entities_In_Project_Cursor) return Boolean
   is
   begin
      return not Has_Element (Iter.Iter);
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Entities_In_Project_Cursor) return Root_Entity'Class
   is
   begin
      return From_New (Iter.Db, Element (Iter.Iter));
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Entities_In_Project_Cursor) is
   begin
      Next (Iter.Iter);
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Calls_Iterator) return Boolean is
   begin
      return At_End (Base_Entities_Cursor (Iter));
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Calls_Iterator) return Root_Entity'Class is
   begin
      return Get (Base_Entities_Cursor (Iter));
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Calls_Iterator) is
   begin
      Next (Base_Entities_Cursor (Iter));
   end Next;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Iter : in out Calls_Iterator) is
   begin
      null;
   end Destroy;

   --------------
   -- From_New --
   --------------

   function From_New
     (Db     : General_Xref_Database;
      Entity : GNATCOLL.Xref.Entity_Information) return General_Entity is
   begin
      return (Entity => Entity, Db => Db, others => <>);
   end From_New;

   ----------------
   -- Parameters --
   ----------------

   overriding function Parameters
     (Entity : General_Entity) return Parameter_Array
   is
      --  We can't use a Vector in the specs, since the size of
      --  General_Parameter is unknown at that point. So we have to use an
      --  array and grow it.

      type Parameter_Array_Access is access all Parameter_Array;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
         (Parameter_Array, Parameter_Array_Access);

      All_Params : Parameter_Array_Access := new Parameter_Array (1 .. 100);
      Tmp        : Parameter_Array_Access;
      Count      : Integer := All_Params'First - 1;
      Curs : Parameters_Cursor :=
        Entity.Db.Xref.Parameters (Entity.Entity);
   begin
      while Curs.Has_Element loop
         Count := Count + 1;
         if Count > All_Params'Last then
            Tmp := All_Params;
            All_Params := new Parameter_Array (1 .. All_Params'Last * 2);
            All_Params (Tmp'Range) := Tmp.all;
            Unchecked_Free (Tmp);
         end if;

         All_Params (Count) :=
           (Kind => Curs.Element.Kind,
            Parameter => new General_Entity'
               (From_New (Entity.Db, Curs.Element.Parameter)));
         Curs.Next;
      end loop;

      return R : constant Parameter_Array :=
         All_Params (All_Params'First .. Count)
      do
         Unchecked_Free (All_Params);
      end return;
   end Parameters;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Parameter_Array) is
   begin
      for J in Self'Range loop
         Unchecked_Free (Self (J).Parameter);
      end loop;
   end Free;

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   overriding function Is_Parameter_Of
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New (Entity.Db, Entity.Db.Xref.Parameter_Of (Entity.Entity));
   end Is_Parameter_Of;

   ---------------------
   -- Is_Primitive_Of --
   ---------------------

   overriding function Is_Primitive_Of
     (Entity : General_Entity) return Entity_Array
   is
      Result : Entity_Lists.List;
      Curs   : Entities_Cursor;
   begin
      Entity.Db.Xref.Method_Of (Entity.Entity, Curs);
      Fill_Entity_Array (Entity.Db, Curs, Result);
      return To_Entity_Array (Result);
   end Is_Primitive_Of;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date
     (Self : access General_Xref_Database_Record;
      File : Virtual_File) return Boolean
   is
   begin
      return Self.Xref.Is_Up_To_Date (File);
   end Is_Up_To_Date;

   -----------------
   -- Has_Methods --
   -----------------

   overriding function Has_Methods
     (E  : General_Entity) return Boolean
   is
   begin
      if E.Entity /= No_Entity then
         return E.Db.Xref.Declaration (E.Entity).Flags.Has_Methods;
      end if;

      --  ??? Fallback on constructs
      return False;
   end Has_Methods;

   ---------------
   -- Is_Access --
   ---------------

   overriding function Is_Access
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Access;
   end Is_Access;

   -----------------
   -- Is_Abstract --
   -----------------

   overriding function Is_Abstract
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Abstract;
   end Is_Abstract;

   --------------
   -- Is_Array --
   --------------

   overriding function Is_Array
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db.Xref.Declaration (E.Entity).Flags.Is_Array;
   end Is_Array;

   ------------------------------
   -- Is_Printable_In_Debugger --
   ------------------------------

   overriding function Is_Printable_In_Debugger
     (E  : General_Entity) return Boolean
   is
   begin
      return E.Db /= null
         and then E.Db.Xref.Declaration (E.Entity).Flags.Is_Printable_In_Gdb;
   end Is_Printable_In_Debugger;

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   overriding function Get_Display_Kind
     (Entity : General_Entity) return String
   is
   begin
      return To_String (Entity.Db.Xref.Declaration (Entity.Entity).Kind);
   end Get_Display_Kind;

   ------------------------------
   -- Reference_Is_Declaration --
   ------------------------------

   overriding function Reference_Is_Declaration
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Ref.Ref.Kind_Id = Kind_Id_Declaration;
   end Reference_Is_Declaration;

   -----------------------
   -- Reference_Is_Body --
   -----------------------

   overriding function Reference_Is_Body
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Ref.Ref.Kind = "body";
   end Reference_Is_Body;

   -----------------------
   -- Is_Read_Reference --
   -----------------------

   overriding function Is_Read_Reference
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Ref.Db.Xref.Is_Read_Reference (Ref.Ref);
   end Is_Read_Reference;

   --------------------------------------------
   -- Is_Read_Or_Write_Or_Implicit_Reference --
   --------------------------------------------

   overriding function Is_Read_Or_Write_Or_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Is_Read_Or_Write_Reference (Ref)
        or else Is_Implicit_Reference (Ref);
   end Is_Read_Or_Write_Or_Implicit_Reference;

   -----------------------------------
   -- Is_Read_Or_Implicit_Reference --
   -----------------------------------

   overriding function Is_Read_Or_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Is_Read_Reference (Ref)
        or else Is_Implicit_Reference (Ref);
   end Is_Read_Or_Implicit_Reference;

   ---------------------------
   -- Is_Implicit_Reference --
   ---------------------------

   overriding function Is_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Ref.Db.Xref.Is_Implicit_Reference (Ref.Ref);
   end Is_Implicit_Reference;

   -----------------------
   -- Is_Real_Reference --
   -----------------------

   overriding function Is_Real_Reference
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Ref.Db.Xref.Is_Real_Reference (Ref.Ref);
   end Is_Real_Reference;

   -----------------------------------
   -- Is_Real_Or_Implicit_Reference --
   -----------------------------------

   overriding function Is_Real_Or_Implicit_Reference
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Is_Real_Reference (Ref)
        or else Is_Implicit_Reference (Ref);
   end Is_Real_Or_Implicit_Reference;

   ------------------------
   -- Is_Write_Reference --
   ------------------------

   overriding function Is_Write_Reference
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Ref.Db.Xref.Is_Write_Reference (Ref.Ref);
   end Is_Write_Reference;

   --------------------------------
   -- Is_Read_Or_Write_Reference --
   --------------------------------

   overriding function Is_Read_Or_Write_Reference
     (Ref : General_Entity_Reference) return Boolean is
   begin
      return Ref.Db.Xref.Is_Read_Or_Write_Reference (Ref.Ref);
   end Is_Read_Or_Write_Reference;

   -------------------
   -- Open_Database --
   -------------------

   procedure Open_Database
     (Self   : General_Xref_Database; Tree   : Project_Tree_Access)
   is
      Working_Xref_File : Virtual_File;
      Error : GNAT.Strings.String_Access;
   begin
      Self.Working_Xref_Db := GNATCOLL.VFS.No_File;
      Working_Xref_File := Xref_Database_Location (Self);

      Self.Xref_Db_Is_Temporary := Tree.Status /= From_File;

      Trace (Me, "Set up xref database: " &
             (+Working_Xref_File.Full_Name.all));

      if Self.Disable_SQL_Queries then
         --  Just to avoid errors because we are accessing a non-existing db
         Self.DB := GNATCOLL.SQL.Sqlite.Setup
            (Database => ":memory:", Errors   => Self.Errors);
         Self.Xref.Setup_DB
           (DB    => Self.DB,
            Tree  => Tree,
            Error => Error);

      else
         begin
            Self.DB :=  GNATCOLL.SQL.Sqlite.Setup
              (Database => +Working_Xref_File.Full_Name.all,
               Errors   => Self.Errors);
            Self.Xref.Setup_DB
              (DB    => Self.DB,
               Tree  => Tree,
               Error => Error);
         exception
            when E : others =>
               --  Catch a corrupted database here and stop propagating
               --  the exception, so as not to block the project loading,
               --  the splash screen, etc
               Trace (Me, "Exception received in Setup_DB: "
                      & Exception_Information (E));
         end;
      end if;

      --  Not interested in schema version errors, gnatinspect already
      --  displays them on the console
      Free (Error);
   end Open_Database;

   --------------------
   -- Close_Database --
   --------------------

   procedure Close_Database (Self   : General_Xref_Database) is
      Success : Boolean;
   begin
      Trace (Me, "Closing xref database, temporary="
             & Self.Xref_Db_Is_Temporary'Img);
      Self.Xref.Free;

      --  If we were already working on a database, first copy the working
      --  database to the database saved between sessions, for future use

      if Self.Xref_Db_Is_Temporary then
         --  This database does not need saving, so we are deleting it
         Trace (Me, "Database was temporary, not saving");

         if Self.Working_Xref_Db /= No_File then
            Self.Working_Xref_Db.Delete (Success);

            if not Success then
               Trace
                 (Me, "Warning: could not delete temporary database file");
            end if;
         end if;
      end if;

      GNATCOLL.SQL.Exec.Free (Self.DB);
   end Close_Database;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out General_Xref_Database) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (General_Xref_Database_Record'Class, General_Xref_Database);
   begin
      Close_Database (Self);
      Self.Xref := null;
      Free (Self.Constructs);
      Unchecked_Free (Self);
   end Destroy;

   ------------------
   -- End_Of_Scope --
   ------------------

   overriding function End_Of_Scope
     (Entity : General_Entity) return General_Location
   is
      Iter : References_Cursor;
      Ref  : Entity_Reference;
   begin
      Entity.Db.Xref.References
        (Entity.Entity, Cursor => Iter,
         Include_Implicit => True,
         Include_All => True,
         Kinds       => "");

      while Has_Element (Iter) loop
         Ref := Element (Iter);
         if Ref.Is_End_Of_Scope then
            return (File    => Ref.File,
                    Project_Path => Ref.Project.Project_Path,
                    Line    => Ref.Line,
                    Column  => Ref.Column);
         end if;

         Next (Iter);
      end loop;
      return No_Location;
   end End_Of_Scope;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self         : access General_Xref_Database_Record;
      Lang_Handler :
         access Language.Tree.Database.Abstract_Language_Handler_Record'Class;
      Symbols      : GNATCOLL.Symbols.Symbol_Table_Access;
      Registry     : Projects.Project_Registry_Access;
      Errors       : access GNATCOLL.SQL.Exec.Error_Reporter'Class := null)
   is
      use Construct_Annotations_Pckg;
      LI_Entity_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Self.Constructs := new Language.Tree.Database.Construct_Database;
      Self.Lang_Handler := Abstract_Language_Handler (Lang_Handler);
      Set_Symbols (Self.Constructs, Symbols);

      Self.Symbols := Symbols;
      Self.Registry := Registry;
      Self.Errors := Errors;

      Language.Tree.Database.Initialize
        (Db         => Self.Constructs,
         Lg_Handler => Abstract_Language_Handler (Lang_Handler));
      Get_Annotation_Key
        (Get_Construct_Annotation_Key_Registry (Self.Constructs).all,
         LI_Entity_Key);

      Register_Assistant
        (Self.Constructs,
         LI_Assistant_Id,
         new LI_Db_Assistant'
           (Database_Assistant with
            LI_Key => LI_Entity_Key,
            Db     => General_Xref_Database (Self)));

      if Self.Xref = null then
         Self.Xref := new Extended_Xref_Database;
      end if;
   end Initialize;

   ------------------
   -- To_LI_Entity --
   ------------------

   function To_LI_Entity
     (Self : access General_Xref_Database_Record'Class;
      E    : Entity_Access) return General_Entity
   is
      use Construct_Annotations_Pckg;
      Entity : General_Entity;

      Assistant : constant LI_Db_Assistant_Access := LI_Db_Assistant_Access
        (Get_Assistant (Self.Constructs, LI_Assistant_Id));

      Construct_Annotation : Construct_Annotations_Pckg.Annotation;
      Loc : General_Location;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (E)), To_Construct_Tree_Iterator (E)).all,
         Assistant.LI_Key,
         Construct_Annotation);

      if Construct_Annotation = Construct_Annotations_Pckg.Null_Annotation then
         Loc := (File    => Get_File_Path (Get_File (E)),
                 Project_Path => No_File,   --  ??? unknown
                 Line    => Get_Construct (E).Sloc_Entity.Line,
                 Column  => To_Visible_Column
                  (Get_File (E),
                   Get_Construct (E).Sloc_Entity.Line,
                   String_Index_Type (Get_Construct (E).Sloc_Entity.Column)));

         --  Create a new LI entity

         Entity := No_General_Entity;
         Entity.Loc := Loc;
         Construct_Annotation := (Other_Kind, Other_Val => new LI_Annotation);
         LI_Annotation (Construct_Annotation.Other_Val.all).Entity := Entity;
         Set_Annotation
           (Get_Annotation_Container
              (Get_Tree (Get_File (E)), To_Construct_Tree_Iterator (E)).all,
            Assistant.LI_Key,
            Construct_Annotation);
      else
         null;
      end if;

      return LI_Annotation (Construct_Annotation.Other_Val.all).Entity;
   end To_LI_Entity;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Obj : in out LI_Annotation) is
   begin
      null;
   end Free;

   ----------
   -- Hash --
   ----------

   overriding function Hash
     (Entity : General_Entity) return Integer is
   begin
      --  Use directly the sqlite internal id.
      return GNATCOLL.Xref.Internal_Id (Entity.Entity);
   end Hash;

   ---------
   -- Cmp --
   ---------

   function Cmp
     (Entity1, Entity2 : Root_Entity'Class) return Integer
   is
      Id1, Id2 : Integer;
   begin
      if not (Entity1 in General_Entity'Class
        and then Entity2 in General_Entity'Class)
      then
         --  Two entities are not generic entities: compare their name
         declare
            Name1 : constant String := Entity1.Get_Name;
            Name2 : constant String := Entity2.Get_Name;
         begin
            if Name1 < Name2 then
               return -1;
            elsif Name1 = Name2 then
               return 0;
            else
               return 1;
            end if;
         end;
      end if;

      Id1 := GNATCOLL.Xref.Internal_Id (General_Entity (Entity1).Entity);
      Id2 := GNATCOLL.Xref.Internal_Id (General_Entity (Entity2).Entity);
      if Id1 < Id2 then
         return -1;
      elsif Id1 = Id2 then
         return 0;
      else
         return 1;
      end if;
   end Cmp;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Iter : File_Iterator) return Boolean is
   begin
      return Has_Element (Iter.Iter);
   end Has_Element;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out File_Iterator) is
   begin
      Next (Iter.Iter);
   end Next;

   -------------
   -- Element --
   -------------

   function Element (Iter : File_Iterator) return Virtual_File is
   begin
      return Element (Iter.Iter);
   end Element;

   -------------
   -- Project --
   -------------

   function Project
     (Iter : File_Iterator;
      Tree : GNATCOLL.Projects.Project_Tree'Class)
      return GNATCOLL.Projects.Project_Type is
   begin
      return Project (Iter.Iter, Tree);
   end Project;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out File_Iterator) is
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out File_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (File_Iterator'Class, File_Iterator_Access);
   begin
      if Iter /= null then
         Destroy (Iter.all);
         Unchecked_Free (Iter);
      end if;
   end Destroy;

   -----------------------
   -- Find_Dependencies --
   -----------------------

   function Find_Dependencies
     (Self    : access General_Xref_Database_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type) return File_Iterator
   is
      Iter    : File_Iterator;
   begin
      Iter.Iter := Self.Xref.Imports (File, Project);
      return Iter;
   end Find_Dependencies;

   --------------------------------
   -- Find_Ancestor_Dependencies --
   --------------------------------

   function Find_Ancestor_Dependencies
     (Self    : access General_Xref_Database_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type) return File_Iterator
   is
      Iter    : File_Iterator;
   begin
      Iter.Iter := Self.Xref.Imported_By (File, Project);
      return Iter;
   end Find_Ancestor_Dependencies;

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   overriding function Get_Display_Kind
     (Ref  : General_Entity_Reference) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Ref.Ref.Kind);
   end Get_Display_Kind;

   ------------------------------
   -- All_Real_Reference_Kinds --
   ------------------------------

   function All_Real_Reference_Kinds
     (Db  : access General_Xref_Database_Record)
      return GNAT.Strings.String_List
   is
   begin
      return Db.Xref.All_Real_Reference_Kinds;
   end All_Real_Reference_Kinds;

   --------------
   -- Is_Fuzzy --
   --------------

   overriding function Is_Fuzzy (Entity : General_Entity) return Boolean is
   begin
      return Entity.Is_Fuzzy;
   end Is_Fuzzy;

   ---------------------
   -- From_Constructs --
   ---------------------

   function From_Constructs
     (Db  : General_Xref_Database;
      Entity : Language.Tree.Database.Entity_Access) return General_Entity
   is
      Loc : General_Location;
   begin
      Loc :=
        (File    => Get_File_Path (Get_File (Entity)),
         Project_Path => No_File,  --  ambiguous
         Line    => Get_Construct (Entity).Sloc_Entity.Line,
         Column  => To_Visible_Column
            (Get_File (Entity),
             Get_Construct (Entity).Sloc_Entity.Line,
             String_Index_Type (Get_Construct (Entity).Sloc_Entity.Column)));
      return (Loc => Loc, Db => Db, others => <>);
   end From_Constructs;

   -----------------
   -- Instance_Of --
   -----------------

   overriding function Instance_Of
      (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New
        (Entity.Db,
         Entity.Db.Xref.Instance_Of (Entity.Entity));
   end Instance_Of;

   --------------------
   -- From_Instances --
   --------------------

   overriding function From_Instances
     (Ref : General_Entity_Reference) return Entity_Array
   is
      R : constant GNATCOLL.Xref.Entity_Array :=
        Ref.Db.Xref.From_Instances (Ref.Ref);
      Result : Entity_Array (R'Range);
   begin
      for A in R'Range loop
         Result (A) := new General_Entity'
           (From_New (Ref.Db, R (A)));
      end loop;
      return Result;
   end From_Instances;

   -----------------------
   -- Fill_Entity_Array --
   -----------------------

   procedure Fill_Entity_Array
     (Db   : General_Xref_Database;
      Curs : in out Entities_Cursor'Class;
      Arr  : in out Entity_Lists.List)
   is
   begin
      while Curs.Has_Element loop
         Arr.Append (From_New (Db, Curs.Element));
         Curs.Next;
      end loop;
   end Fill_Entity_Array;

   ---------------------
   -- To_Entity_Array --
   ---------------------

   function To_Entity_Array
     (Arr : Entity_Lists.List) return Entity_Array
   is
      Result : Entity_Array (1 .. Integer (Arr.Length));
      C      : Entity_Lists.Cursor := Arr.First;
   begin
      for R in Result'Range loop
         Result (R) := new General_Entity'(Element (C));
         Entity_Lists.Next (C);
      end loop;
      return Result;
   end To_Entity_Array;

   ---------------------
   -- Discriminant_Of --
   ---------------------

   overriding function Discriminant_Of
      (Entity            : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New
        (Entity.Db,
         Entity.Db.Xref.Discriminant_Of (Entity.Entity));
   end Discriminant_Of;

   -------------------
   -- Discriminants --
   -------------------

   overriding function Discriminants
     (Entity : General_Entity) return Entity_Array
   is
      Arr  : Entity_Lists.List;
      Curs : Entities_Cursor;
   begin
      Entity.Db.Xref.Discriminants (Entity.Entity, Cursor => Curs);
      Fill_Entity_Array (Entity.Db, Curs, Arr);
      return To_Entity_Array (Arr);
   end Discriminants;

   -----------------------
   -- Formal_Parameters --
   -----------------------

   overriding function Formal_Parameters
      (Entity : General_Entity) return Entity_Array
   is
      Arr  : Entity_Lists.List;
      Curs : Entities_Cursor;
   begin
      Entity.Db.Xref.Formal_Parameters (Entity.Entity, Cursor => Curs);
      Fill_Entity_Array (Entity.Db, Curs, Arr);
      return To_Entity_Array (Arr);
   end Formal_Parameters;

   --------------
   -- Literals --
   --------------

   overriding function Literals
     (Entity : General_Entity) return Entity_Array
   is
      Block_Me : constant Block_Trace_Handle := Create
         (Me, (if Active (Me) then Get_Name (Entity) else ""))
         with Unreferenced;
      Arr  : Entity_Lists.List;
      Curs : Entities_Cursor;
   begin
      Entity.Db.Xref.Literals (Entity.Entity, Cursor => Curs);
      Fill_Entity_Array (Entity.Db, Curs, Arr);
      return To_Entity_Array (Arr);
   end Literals;

   -----------------
   -- Child_Types --
   -----------------

   overriding function Child_Types
      (Entity    : General_Entity;
       Recursive : Boolean) return Entity_Array
   is
      Arr : Entity_Lists.List;
      Curs : Entities_Cursor;
      Rec  : Recursive_Entities_Cursor;
   begin
      if Recursive then
         Entity.Db.Xref.Recursive
           (Entity  => Entity.Entity,
            Compute => GNATCOLL.Xref.Child_Types'Access,
            Cursor  => Rec);
         Fill_Entity_Array (Entity.Db, Rec, Arr);
      else
         Entity.Db.Xref.Child_Types (Entity.Entity, Cursor => Curs);
         Fill_Entity_Array (Entity.Db, Curs, Arr);
      end if;

      return To_Entity_Array (Arr);
   end Child_Types;

   ------------------
   -- Parent_Types --
   ------------------

   overriding function Parent_Types
      (Entity    : General_Entity;
       Recursive : Boolean) return Entity_Array
   is
      Arr : Entity_Lists.List;
      Curs : Entities_Cursor;
      Rec  : Recursive_Entities_Cursor;
   begin
      if Recursive then
         Entity.Db.Xref.Recursive
           (Entity  => Entity.Entity,
            Compute => GNATCOLL.Xref.Parent_Types'Access,
            Cursor  => Rec);
         Fill_Entity_Array (Entity.Db, Rec, Arr);
      else
         Entity.Db.Xref.Parent_Types (Entity.Entity, Cursor => Curs);
         Fill_Entity_Array (Entity.Db, Curs, Arr);
      end if;

      return To_Entity_Array (Arr);
   end Parent_Types;

   ------------
   -- Fields --
   ------------

   overriding function Fields
      (Entity            : General_Entity) return Entity_Array
   is
      Arr  : Entity_Lists.List;
      Curs : Entities_Cursor;
   begin
      Entity.Db.Xref.Fields (Entity.Entity, Cursor => Curs);
      Fill_Entity_Array (Entity.Db, Curs, Arr);
      return To_Entity_Array (Arr);
   end Fields;

   -------------
   -- Methods --
   -------------

   overriding function Methods
      (Entity            : General_Entity;
       Include_Inherited : Boolean) return Entity_Array
   is
      Result : Entity_Lists.List;
      Curs   : Entities_Cursor;
   begin
      Entity.Db.Xref.Methods
        (Entity.Entity,
         Cursor            => Curs,
         Include_Inherited => Include_Inherited);
      Fill_Entity_Array (Entity.Db, Curs, Result);
      return To_Entity_Array (Result);
   end Methods;

   --------------------
   -- Component_Type --
   --------------------

   overriding function Component_Type
      (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New
        (Entity.Db, Entity.Db.Xref.Component_Type (Entity.Entity));
   end Component_Type;

   --------------------
   -- Parent_Package --
   --------------------

   overriding function Parent_Package
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New
        (Entity.Db, Entity.Db.Xref.Parent_Package (Entity.Entity));
   end Parent_Package;

   -----------------
   -- Index_Types --
   -----------------

   overriding function Index_Types
      (Entity : General_Entity) return Entity_Array
   is
      Curs : Entities_Cursor;
      Arr  : Entity_Lists.List;
   begin
      Entity.Db.Xref.Index_Types (Entity.Entity, Cursor => Curs);
      Fill_Entity_Array (Entity.Db, Curs, Arr);
      return To_Entity_Array (Arr);
   end Index_Types;

   ---------------
   -- Overrides --
   ---------------

   overriding function Overrides
     (Entity : General_Entity) return Root_Entity'Class
   is
   begin
      return From_New (Entity.Db, Entity.Db.Xref.Overrides (Entity.Entity));
   end Overrides;

   -------------------------------
   -- Select_Entity_Declaration --
   -------------------------------

   function Select_Entity_Declaration
     (Self    : access General_Xref_Database_Record;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : Project_Type;
      Entity  : Root_Entity'Class) return Root_Entity'Class
   is
      pragma Unreferenced (Self, File, Project);
   begin
      return Entity;
   end Select_Entity_Declaration;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : access General_Xref_Database_Record) is
   begin
      null;
   end Reset;

   ---------------------
   -- Project_Changed --
   ---------------------

   procedure Project_Changed (Self : General_Xref_Database) is
      Error : GNAT.Strings.String_Access;
   begin
      --  Create an initial empty database. It will never be filled, and
      --  will be shortly replaced in Project_View_Changed, but it ensures
      --  that GPS does not raise exceptions if some action is performed
      --  while the project has not been computed (like loading of the
      --  desktop for instance).
      --  ??? We really should not be doing anything until the project has
      --  been computed.

      if Self.Xref /= null then
         Trace (Me, "Closing previous version of the database");
         Close_Database (Self);
      end if;

      Trace (Me, "Set up xref database: :memory:");
      Self.Working_Xref_Db := GNATCOLL.VFS.No_File;
      Self.Xref_Db_Is_Temporary := True;
      Self.DB := GNATCOLL.SQL.Sqlite.Setup
         (Database => ":memory:", Errors   => Self.Errors);
      Self.Xref.Setup_DB
        (DB    => Self.DB,
         Tree  => Self.Registry.Tree,
         Error => Error);

      --  not interested in schema version errors, gnatinspect will
      --  already display those for the user.
      Free (Error);
   end Project_Changed;

   ----------------------------
   -- Xref_Database_Location --
   ----------------------------

   function Xref_Database_Location
     (Self    : not null access General_Xref_Database_Record)
      return GNATCOLL.VFS.Virtual_File is
   begin
      if Self.Working_Xref_Db = GNATCOLL.VFS.No_File then
         declare
            Project : constant Project_Type := Self.Registry.Tree.Root_Project;
            Attr : constant String :=
              Project.Attribute_Value
                (Build ("IDE", "Xref_Database"),
                 Default => "",
                 Use_Extended => True);
         begin
            if Attr = "" then
               if Active (Force_Local_Database) then
                  declare
                     Hash : constant String := GNAT.SHA1.Digest
                       (+Project.Project_Path.Full_Name (Normalize => True));
                  begin
                     Self.Working_Xref_Db :=
                       Get_Tmp_Directory / (+("gnatinspect-" & Hash & ".db"));
                  end;
               else
                  Self.Working_Xref_Db :=
                    Project.Artifacts_Dir / (+"gnatinspect.db");
               end if;
            else
               Self.Working_Xref_Db := Create_From_Base
                 (Base_Name => +Attr,
                  Base_Dir  => Project.Project_Path.Dir_Name);
            end if;

            Trace
              (Me, "project db file: " &
                 Self.Working_Xref_Db.Display_Full_Name);

            Self.Disable_SQL_Queries :=
              not Create (Self.Working_Xref_Db.Dir_Name).Is_Writable
              or else
                (Self.Working_Xref_Db.Is_Regular_File
                 and then not Self.Working_Xref_Db.Is_Writable);
         end;
      end if;

      return Self.Working_Xref_Db;
   end Xref_Database_Location;

   -------------------
   -- Allow_Queries --
   -------------------

   function Allow_Queries
     (Self : not null access General_Xref_Database_Record) return Boolean
   is
   begin
      return not Self.Disable_SQL_Queries;
   end Allow_Queries;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed
     (Self   : General_Xref_Database;
      Tree   : Project_Tree_Access)
   is
   begin
      if Self.Xref /= null then
         Trace (Me, "Closing previous version of the database");
         Close_Database (Self);
      end if;

      --  Self.Xref was initialized in Project_Changed.
      Self.Xref.Free;
      Self.Working_Xref_Db := No_File;

      Open_Database (Self, Tree);

      --  ??? Now would be a good opportunity to update the cross-references
      --  rather than wait for the next compilation.
   end Project_View_Changed;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Entity_Array) is
   begin
      for J in X'Range loop
         Unchecked_Free (X (J));
      end loop;
   end Free;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (G : General_Location) return Project_Type is
      T : constant Project_Tree_Access :=
        GNATCOLL.Scripts.Projects.Project_Tree;
   begin
      if T = null then
         return No_Project;
      else
         return T.Project_From_Path (G.Project_Path);
      end if;
   end Get_Project;

end Xref;
