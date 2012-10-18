------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

pragma Ada_2012;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Maps;          use Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;
with ALI_Parser;
with Dynamic_Arrays;
with Glib.Convert;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNAT.Strings;              use GNAT.Strings;
with Language_Handlers;         use Language_Handlers;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with String_Utils;
with Traces;

package body Xref is
   Me : constant Trace_Handle := Create ("Xref");
   Constructs_Heuristics : constant Trace_Handle :=
     Create ("Entities.Constructs", On);

   ---------------------------
   --  Note for development --
   ---------------------------

   --  A lot of functions defined here use either the new system
   --  (GNATCOLL.Xref) or the legacy database (Entities.*), and
   --  sometimes fallback on the constructs database.

   use type Old_Entities.Entity_Information;
   use type Old_Entities.File_Location;

   type Hash_Type is range 0 .. 2 ** 20 - 1;
   function Hash is new String_Utils.Hash (Hash_Type);

   package Entity_Arrays is new Dynamic_Arrays (General_Entity);
   use Entity_Arrays;

   function Get_Location
     (Ref : Entity_Reference) return General_Location;
   --  Return the General Location of a GNATCOLL reference

   procedure Node_From_Entity
     (Self        : access General_Xref_Database_Record'Class;
      Handler     : Language_Handlers.Language_Handler;
      Decl        : General_Location;
      Ent         : out Entity_Access;
      Tree_Lang   : out Tree_Language_Access);
   --  Returns the constructs data for a given entity.

   function To_General_Entity
     (E : Old_Entities.Entity_Information) return General_Entity;
   function To_General_Entity
     (Db : access General_Xref_Database_Record'Class;
      E  : Entity_Information) return General_Entity;
   --  Convert Xref.Entity_Information to General_Entity

   procedure Fill_Entity_Array
     (Curs : in out Entities_Cursor'Class;
      Arr  : in out Entity_Arrays.Instance);
   --  Store all entities returned by the cursor into the array

   function To_Entity_Array (Arr : Entity_Arrays.Instance) return Entity_Array;
   --  Creates an entity array.
   --  ??? This is not very efficient

   function Get_Entity_At_Location
     (Db  : access General_Xref_Database_Record'Class;
      Loc : General_Location) return Entity_Access;
   --  Return the construct entity found at the location given in parameter.

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

   function To_LI_Entity (E : Entity_Access) return General_Entity;
   --  Return an LI entity based on a construct entity. Create one if none.

   -------------------
   -- Documentation --
   -------------------

   function Documentation
     (Self             : access General_Xref_Database_Record;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : General_Entity;
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String
   is
      function Doc_From_Constructs return String;
      function Doc_From_LI return String;

      Decl : constant General_Location :=
        Get_Declaration (Self, Entity).Loc;
      Context : constant Language.Language_Context_Access :=
        Language.Get_Language_Context
          (Get_Language_From_File (Handler, Source_Filename => Decl.File));

      Form : constant Formatting :=
        (if Raw_Format then Text else HTML);

      -------------------------
      -- Doc_From_Constructs --
      -------------------------

      function Doc_From_Constructs return String is
         Ent       : Entity_Access;
         Tree_Lang : Tree_Language_Access;
         Buffer    : GNAT.Strings.String_Access;
         Node      : Construct_Tree_Iterator;
      begin
         if Entity.Node = Null_Entity_Access then
            Node_From_Entity (Self, Handler, Decl, Ent, Tree_Lang);
         else
            Ent := Entity.Node;
            Tree_Lang := Get_Tree_Language (Get_File (Ent));
         end if;

         Buffer := Get_Buffer (Get_File (Ent));
         Node   := To_Construct_Tree_Iterator (Ent);

         declare
            Comment : constant String :=
              Extract_Comment
                (Buffer            => Buffer.all,
                 Decl_Start_Index  => Get_Construct (Node).Sloc_Start.Index,
                 Decl_End_Index    => Get_Construct (Node).Sloc_End.Index,
                 Language          => Context.Syntax,
                 Format            => Form);
            Profile : constant String :=
              Get_Profile (Tree_Lang, Ent, Raw_Format => Raw_Format);

         begin
            if Comment /= "" then
               if Profile /= "" then
                  return Glib.Convert.Escape_Text (Comment)
                    & ASCII.LF & ASCII.LF & Profile;
               else
                  return Glib.Convert.Escape_Text (Comment);
               end if;
            else
               return Profile;
            end if;
         end;
      end Doc_From_Constructs;

      -----------------
      -- Doc_From_LI --
      -----------------

      function Doc_From_LI return String is
         Buffer : GNAT.Strings.String_Access;
         Loc    : Old_Entities.File_Location;
         Result : Unbounded_String;
      begin
         if Active (SQLITE) then
            if Entity.Entity /= No_Entity then
               Append
                 (Result,
                  Glib.Convert.Escape_Text
                    (Self.Xref.Comment (Entity.Entity, Context.Syntax, Form))
                  & ASCII.LF
                  & Self.Xref.Text_Declaration (Entity.Entity, Form)
                  & ASCII.LF);

               return To_String
                 (Ada.Strings.Unbounded.Trim
                    (Result,
                     Left => Ada.Strings.Maps.Null_Set,
                     Right => Ada.Strings.Maps.To_Set
                       (' ' & ASCII.HT & ASCII.LF & ASCII.CR)));
            end if;
         else
            Buffer := Decl.File.Read_File;

            if Buffer = null then
               return "";
            end if;

            Result := To_Unbounded_String
              (Glib.Convert.Escape_Text
                 (Extract_Comment
                    (Buffer            => Buffer.all,
                     Decl_Start_Line   => Decl.Line,
                     Decl_Start_Column => Integer (Decl.Column),
                     Language          => Context.Syntax,
                     Format            => Form)));

            if Result = "" and then Entity.Old_Entity /= null then
               Find_Next_Body
                 (Entity.Old_Entity,
                  Location => Loc,
                  No_Location_If_First => True);

               if Loc /= Old_Entities.No_File_Location then
                  Free (Buffer);
                  Buffer := Old_Entities.Get_Filename (Loc.File).Read_File;
                  Result := To_Unbounded_String
                    (Extract_Comment
                       (Buffer            => Buffer.all,
                        Decl_Start_Line   => Loc.Line,
                        Decl_Start_Column => Integer (Loc.Column),
                        Language          => Context.Syntax,
                        Format            => Form));
               end if;
            end if;

            Free (Buffer);
            return To_String (Result);
         end if;

         return "";
      end Doc_From_LI;

   --  Start of processing for Documentation

   begin
      if not Check_Constructs then
         return Doc_From_LI;
      else
         declare
            R : constant String := Doc_From_Constructs;
         begin
            if R = "" then
               return Doc_From_LI;
            end if;
            return R;
         end;
      end if;

      --  If still not found, we used to default to also searching just before
      --  the body. But when there is a separate spec, the doc should be there
      --  and when we don't have a separate spec the "declaration" is the
      --  location of the body.
   end Documentation;

   -------------------------------
   -- For_Each_Dispatching_Call --
   -------------------------------

   procedure For_Each_Dispatching_Call
     (Dbase     : access General_Xref_Database_Record;
      Entity    : General_Entity;
      Ref       : General_Entity_Reference;
      On_Callee : access function
        (Callee, Primitive_Of : General_Entity) return Boolean;
      Filter    : Reference_Kind_Filter := null;
      Policy    : Dispatching_Menu_Policy)

   is
      use type Old_Entities.Reference_Kind;

      Prim_Ent  : General_Entity;
      Typ_Ent   : General_Entity;

   begin
      --  Handle cases in which no action is needed

      if Entity = No_General_Entity
        or else Policy = Never
          or else not Dbase.Is_Dispatching_Call (Ref)
      then
         return;
      end if;

      if Active (SQLITE) then
         declare
            function Tagged_Type (E : Entity_Information)
               return Entity_Information;

            function Tagged_Type (E : Entity_Information)
               return Entity_Information is
            begin
               return Dbase.Xref.Declaration
                 (Dbase.Xref.Method_Of (E)).Location.Entity;
            end Tagged_Type;

            Cursor : Recursive_Entities_Cursor;
            Prim   : Entity_Information;
            Show   : Boolean;
            R      : References_Cursor;

         begin
            Prim     := Entity.Entity;
            Prim_Ent := To_General_Entity (Dbase, Prim);
            Typ_Ent  := To_General_Entity (Dbase, Tagged_Type (Prim));

            if On_Callee (Callee => Prim_Ent, Primitive_Of => Typ_Ent) then
               Recursive
                 (Self    => Dbase.Xref,
                  Entity  => Entity.Entity,
                  Compute => Overridden_By'Unrestricted_Access,
                  Cursor  => Cursor);

               while Cursor.Has_Element loop
                  Prim     := Cursor.Element;
                  Prim_Ent := To_General_Entity (Dbase, Prim);
                  Typ_Ent  := To_General_Entity (Dbase, Tagged_Type (Prim));

                  Show := Filter = null;
                  if not Show then
                     Dbase.Xref.References (Typ_Ent.Entity, R);
                     while R.Has_Element loop
                        Show := Filter
                          (Dbase, (Ref => R.Element, others => <>));
                        exit when Show;
                        R.Next;
                     end loop;
                  end if;

                  exit when Show and then not On_Callee
                    (Callee       => Prim_Ent,
                     Primitive_Of => Typ_Ent);

                  Cursor.Next;
               end loop;
            end if;

         exception
            when E : others =>
               Trace (Traces.Exception_Handle, "Unexpected exception: "
                      & Exception_Information (E));
         end;

      --  Legacy functionality

      else
         declare
            function Proxy
              (Callee, Primitive_Of : Old_Entities.Entity_Information)
               return Boolean;
            function Proxy_Filter
              (R : Old_Entities.Entity_Reference) return Boolean;

            function Proxy
              (Callee, Primitive_Of : Old_Entities.Entity_Information)
               return Boolean is
            begin
               return On_Callee (From_Old (Callee), From_Old (Primitive_Of));
            end Proxy;

            function Proxy_Filter
              (R : Old_Entities.Entity_Reference) return Boolean is
            begin
               return Filter (Dbase, (Old_Ref => R, others => <>));
            end Proxy_Filter;

            P : Old_Entities.Queries.Reference_Filter_Function := null;
            Need_Bodies : constant Boolean :=
              Filter = Reference_Is_Body'Access;

         begin

            if Filter /= null then
               P := Proxy_Filter'Unrestricted_Access;
            end if;

            Old_Entities.Queries.For_Each_Dispatching_Call
              (Entity    => Entity.Old_Entity,
               Ref       => Ref.Old_Ref,
               On_Callee => Proxy'Access,
               Filter    => P,
               Need_Bodies => Need_Bodies,
               Policy    => Policy);
         end;
      end if;
   end For_Each_Dispatching_Call;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Ref : General_Entity_Reference) return General_Entity
   is
      E : General_Entity;
   begin
      --  Attempt to use the sqlite system

      if Active (SQLITE)
        and then Ref.Ref /= No_Entity_Reference
      then
         E.Entity := Ref.Ref.Entity;
      end if;

      --  Fall back on the old system

      E.Old_Entity := Old_Entities.Get_Entity (Ref.Old_Ref);

      return E;
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Db   : access General_Xref_Database_Record;
      Name : String;
      Loc  : General_Location) return General_Entity
   is
      Entity : General_Entity;
      Ref    : General_Entity_Reference;
   begin
      Find_Declaration_Or_Overloaded
        (General_Xref_Database (Db),
         Loc               => Loc,
         Entity_Name       => Name,
         Ask_If_Overloaded => False,
         Entity            => Entity,
         Closest_Ref       => Ref);
      return Entity;
   end Get_Entity;

   ------------------------------------
   -- Find_Declaration_Or_Overloaded --
   ------------------------------------

   procedure Find_Declaration_Or_Overloaded
     (Self              : access General_Xref_Database_Record;
      Loc               : General_Location;
      Entity_Name       : String;
      Ask_If_Overloaded : Boolean := False;
      Entity            : out General_Entity;
      Closest_Ref       : out General_Entity_Reference)
   is
      Fuzzy : Boolean;

      function Internal_No_Constructs
        (Name : String; Loc : General_Location) return General_Entity;
      --  Search for the entity, without a fallback to the constructs db

      function Internal_No_Constructs
        (Name : String; Loc : General_Location) return General_Entity
      is
         Entity : General_Entity;
      begin
         if Active (SQLITE) then
            if Loc = No_Location then
               --  predefined entities
               Closest_Ref.Ref := Self.Xref.Get_Entity
                 (Name   => Name,
                  File   => No_File);

            else
               --  Already handles the operators
               Closest_Ref.Ref := Self.Xref.Get_Entity
                 (Name   => Name,
                  File   => Loc.File,
                  Line   => Loc.Line,
                  Column => Loc.Column);
            end if;

            Entity.Entity := Closest_Ref.Ref.Entity;
            Fuzzy := Entity.Entity /= No_Entity and then
              (Is_Fuzzy_Match (Entity.Entity)
                  --  or else not Self.Xref.Is_Up_To_Date (Loc.File)
              );

         else
            declare
               Status : Find_Decl_Or_Body_Query_Status;
               Source : Old_Entities.Source_File;
            begin
               --  ??? Should have a pref for the handling of fuzzy matches:
               --  - consider it as a no match: set Status to Entity_Not_Found
               --  - consider it as overloaded entity: same as below;
               --  - use the closest match: nothing to do.

               if Loc = No_Location then
                  --  A predefined entity
                  --  ??? Should not hard-code False here

                  Source := Old_Entities.Get_Predefined_File
                    (Self.Entities, Case_Sensitive => False);
                  Find_Declaration
                    (Db             => Self.Entities,
                     Source         => Source,
                     Entity_Name    => Name,
                     Line           => Loc.Line,  --  irrelevant
                     Column         => Loc.Column,  --  irrelevant
                     Entity         => Entity.Old_Entity,
                     Closest_Ref    => Closest_Ref.Old_Ref,
                     Status         => Status);

               else
                  Source := Old_Entities.Get_Or_Create
                    (Self.Entities, Loc.File, Allow_Create => True);
                  Find_Declaration
                    (Db             => Self.Entities,
                     Source         => Source,
                     Entity_Name    => Name,
                     Line           => Loc.Line,
                     Column         => Loc.Column,
                     Entity         => Entity.Old_Entity,
                     Closest_Ref    => Closest_Ref.Old_Ref,
                     Status         => Status);
               end if;

               Fuzzy := Status = Overloaded_Entity_Found
                 or else Status = Fuzzy_Match;
               --  or else not Old_Entities.Is_Up_To_Date (Source);

               if Status = Entity_Not_Found
                 and then Name /= ""
                 and then Name (Name'First) = '"'
               then
                  --  Try without the quotes
                  Find_Declaration_Or_Overloaded
                    (Self              => Self,
                     Loc               => Loc,
                     Entity_Name       => Entity_Name
                       (Entity_Name'First + 1 .. Entity_Name'Last - 1),
                     Ask_If_Overloaded => Ask_If_Overloaded,
                     Entity            => Entity,
                     Closest_Ref       => Closest_Ref);
               end if;
            end;
         end if;

         Entity.Is_Fuzzy := Fuzzy;
         return Entity;
      end Internal_No_Constructs;

   begin
      Closest_Ref := No_General_Entity_Reference;

      if Active (Me) then
         Increase_Indent (Me, "Find_Declaration of " & Entity_Name);
      end if;

      Entity := Internal_No_Constructs (Entity_Name, Loc);  --  also sets Fuzzy

      if Fuzzy and then Ask_If_Overloaded then
         Entity := Select_Entity_Declaration
           (Self   => General_Xref_Database_Record'Class (Self.all)'Access,
            File   => Loc.File,
            Entity => Entity);

         if Active (Me) then
            Decrease_Indent (Me);
         end if;
         return;
      end if;

      --  Fallback on constructs

      if (Entity = No_General_Entity or else Fuzzy)
        and then Active (Constructs_Heuristics)
        and then Loc /= No_Location   --  Nothing for predefined entities
      then
         declare
            Tree_Lang : constant Tree_Language_Access :=
                         Get_Tree_Language_From_File
                           (Self.Lang_Handler, Loc.File);
            S_File : constant Structured_File_Access :=
              Get_Or_Create
                (Db   => Self.Constructs,
                 File => Loc.File);

            Result       : Entity_Access;
            New_Location : General_Location;
            New_Entity   : General_Entity;

         begin
            if not Is_Null (S_File) then
               Trace (Me, "Find_Declaration: fallback on constructs");

               --  In some cases, the references are extracted from a place
               --  where there is still an ALI file, but no more source file.
               --  This will issue a null Structured_File_Access, which is why
               --  we're protecting the following code with the above condition

               Update_Contents (S_File);

               Result := Tree_Lang.Find_Declaration
                 (S_File, Loc.Line,
                  To_Line_String_Index (S_File, Loc.Line, Loc.Column));

               if Result /= Null_Entity_Access
                 and then
                   (Entity_Name = "" or else
                    Get (Get_Construct (Result).Name).all = Entity_Name)
               then
                  --  First, try to see if there's already a similar entity in
                  --  the database. If that's the case, it's better to use it
                  --  than the dummy one created from the construct.

                  New_Location :=
                    (File   => Get_File_Path (Get_File (Result)),
                     Line   => Get_Construct (Result).Sloc_Entity.Line,
                     Column => To_Visible_Column
                       (Get_File (Result),
                        Get_Construct (Result).Sloc_Entity.Line,
                        String_Index_Type
                          (Get_Construct (Result).Sloc_Entity.Column)));

                  New_Entity := Internal_No_Constructs
                    (Name  => Get (Get_Construct (Result).Name).all,
                     Loc   => (File   => New_Location.File,
                               Line   => New_Location.Line,
                               Column => New_Location.Column));

                  if New_Entity /= No_General_Entity then
                     --  If we found an updated ALI entity, use it.
                     Entity := New_Entity;
                     Entity.Node := Result;

                  elsif Entity /= No_General_Entity then
                     null;

                  else
                     --  If we have no entity to connect to, then create one
                     --  from the construct database.

                     Entity := To_LI_Entity (Result);
                  end if;

                  Entity.Is_Fuzzy := True;
               end if;
            end if;
         end;
      end if;

      if Active (Me) then
         Decrease_Indent (Me);
      end if;
   end Find_Declaration_Or_Overloaded;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return String is
   begin
      if Active (SQLITE) then
         if Entity.Entity /= No_Entity then
            return To_String
              (Declaration (Db.Xref.all, Entity.Entity).Name);
         end if;
      else
         if Entity.Old_Entity /= null then
            return Get (Old_Entities.Get_Name (Entity.Old_Entity)).all;
         end if;
      end if;

      return "";
   end Get_Name;

   --------------------
   -- Qualified_Name --
   --------------------

   function Qualified_Name
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return String
   is
   begin
      if Active (SQLITE) then
         if Entity.Entity /= No_Entity then
            return Self.Xref.Qualified_Name (Entity.Entity);
         end if;
      else
         if Entity.Old_Entity /= null then
            return Old_Entities.Queries.Get_Full_Name (Entity.Old_Entity);
         end if;
      end if;

      return "";
   end Qualified_Name;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Ref : General_Entity_Reference) return General_Location is
   begin
      if Active (SQLITE) then
         if Ref.Ref /= No_Entity_Reference then
            return Get_Location (Ref.Ref);
         end if;

      else
         declare
            use Old_Entities;
            Loc : constant Old_Entities.File_Location :=
              Old_Entities.Get_Location (Ref.Old_Ref);
         begin
            if Loc.File /= null then
               return (File => Old_Entities.Get_Filename (Loc.File),
                       Line => Loc.Line,
                       Column => Loc.Column);
            end if;
         end;
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
           (File => Ref.File,
            Line => Ref.Line,
            Column => Visible_Column_Type (Ref.Column));
      end if;
   end Get_Location;

   ---------------------------
   -- Caller_At_Declaration --
   ---------------------------

   function Caller_At_Declaration
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return (Entity => Db.Xref.Declaration (Entity.Entity).Location.Scope,
                 others => <>);
      else
         return (Old_Entity =>
                   Old_Entities.Queries.Get_Caller
                     (Old_Entities.Declaration_As_Reference
                          (Entity.Old_Entity)),
                 others => <>);
      end if;
   end Caller_At_Declaration;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity_Declaration is
   begin
      if Active (SQLITE) then
         if Entity.Entity /= No_Entity then
            declare
               Ref : constant Entity_Declaration :=
                 Db.Xref.Declaration (Entity.Entity);
            begin
               if Ref /= No_Entity_Declaration then
                  return (Loc    => (File => Ref.Location.File,
                                     Line => Ref.Location.Line,
                                     Column => Ref.Location.Column),
                          Body_Is_Full_Declaration =>
                            Ref.Flags.Body_Is_Full_Declaration,
                          Name   => Ref.Name);
               end if;
            end;
         end if;

      else
         if Entity.Old_Entity /= null then
            declare
               Loc : constant Old_Entities.File_Location :=
                 Old_Entities.Get_Declaration_Of (Entity.Old_Entity);
            begin
               return (Loc => (File   => Old_Entities.Get_Filename (Loc.File),
                               Line   => Loc.Line,
                               Column => Loc.Column),
                       Body_Is_Full_Declaration =>
                         Old_Entities.Body_Is_Full_Declaration
                           (Old_Entities.Get_Kind (Entity.Old_Entity).Kind),
                       Name => To_Unbounded_String
                         (Get
                            (Old_Entities.Get_Name (Entity.Old_Entity)).all));
            end;
         end if;
      end if;

      if Entity.Node /= Null_Entity_Access then
         declare
            Decl : constant Entity_Access :=
              Get_Declaration
                (Get_Tree_Language (Get_File (Entity.Node)),
                 Entity.Node);
            Node : constant Construct_Tree_Iterator :=
              To_Construct_Tree_Iterator (Decl);

         begin
            return (Loc => (File   => Get_File_Path (Get_File (Decl)),
                            Line   => Get_Construct (Node).Sloc_Start.Line,
                            Column => Visible_Column_Type
                              (Get_Construct (Node).Sloc_Start.Column)),
                    Body_Is_Full_Declaration => False,
                    Name => Null_Unbounded_String); --  ??? How to get the name
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

   function Get_Body
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity;
      After  : General_Location := No_Location) return General_Location
   is
      No_Location_If_First : constant Boolean := False;

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
            C_Entity := Get_Entity_At_Location (Db, Location);

            --  Return true if we found a construct here and if it's of the
            --  appropriate name.

            return C_Entity /= Null_Entity_Access
              and then Get (Get_Identifier (C_Entity)).all =
              Db.Get_Name (Entity);
         end if;

         return True;
      end Is_Location_For_Entity;

      --------------------------------
      -- Extract_Next_By_Heuristics --
      --------------------------------

      function Extract_Next_By_Heuristics return General_Location is
         C_Entity, New_Entity : Entity_Access := Null_Entity_Access;
         Loc : General_Location;

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
               C_Entity := Get_Entity_At_Location (Db, After);
            end if;

            if C_Entity = Null_Entity_Access then
               if Entity.Node /= Null_Entity_Access then
                  C_Entity := Entity.Node;

               else
                  Loc :=  Db.Get_Declaration (Entity).Loc;
                  if Loc /= No_Location then
                     C_Entity := Get_Entity_At_Location (Db, Loc);
                  end if;
               end if;
            end if;

            if C_Entity /= Null_Entity_Access then
               declare
                  S_File : constant Structured_File_Access :=
                    Get_File (C_Entity);

                  Tree_Lang : constant Tree_Language_Access :=
                    Get_Tree_Language_From_File
                      (Db.Lang_Handler, Get_File_Path (S_File));
               begin
                  New_Entity := Tree_Lang.Find_Next_Part (C_Entity);

                  --  If we're initializing a loop, e.g. the current location
                  --  is no location, then return the result. Otherwise, don't
                  --  return it if we got back to the initial body and the
                  --  caller doesn't want to loop back.

                  if After /= No_Location
                    and then No_Location_If_First
                    and then C_Entity = Tree_Lang.Find_First_Part (C_Entity)
                  then
                     return No_Location;
                  end if;

                  if New_Entity /= C_Entity then
                     return
                       (File   => Get_File_Path (Get_File (New_Entity)),
                        Line   => Get_Construct (New_Entity).Sloc_Entity.Line,
                        Column =>
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

   begin
      if Active (Me) then
         Increase_Indent (Me, "Get_Body of "
                          & Db.Get_Name (Entity)
                          & " fuzzy=" & Is_Fuzzy (Entity)'Img);
      end if;

      if Active (SQLITE) then
         if Entity.Entity /= No_Entity then
            declare
               C   : References_Cursor;
               Ref : Entity_Reference;
               Matches : Boolean := After = No_Location;
            begin
               Bodies (Db.Xref.all, Entity.Entity, Cursor => C);
               while Has_Element (C) loop
                  Ref := Element (C);

                  if Ref /= No_Entity_Reference then
                     if Matches then
                        Candidate :=
                          (File => Ref.File,
                           Line => Ref.Line,
                           Column => Visible_Column_Type (Ref.Column));
                        exit;
                     else
                        Matches := Ref.Line = After.Line
                          and then Ref.Column = After.Column
                          and then Ref.File = After.File;
                     end if;
                  end if;

                  Next (C);
               end loop;
            end;
         end if;

      else
         if Entity.Old_Entity /= null then
            declare
               Loc : Old_Entities.File_Location;
            begin
               if After /= No_Location then
                  Find_Next_Body
                    (Entity           => Entity.Old_Entity,
                     Current_Location =>
                       (File   => Old_Entities.Get_Or_Create
                          (Db.Entities, After.File, Allow_Create => True),
                        Line   => After.Line,
                        Column => After.Column),
                     Location         => Loc);
               else
                  Find_Next_Body
                    (Entity           => Entity.Old_Entity,
                     Current_Location => Old_Entities.No_File_Location,
                     Location         => Loc);
               end if;

               if Loc = Old_Entities.No_File_Location then
                  Loc := Old_Entities.Get_Declaration_Of (Entity.Old_Entity);
               end if;

               if Loc /= Old_Entities.No_File_Location then
                  Candidate :=
                    (File => Old_Entities.Get_Filename (Loc.File),
                     Line => Loc.Line,
                     Column => Loc.Column);
               end if;
            end;
         end if;
      end if;

      --  If no next body has been found at this stage, try to see what we can
      --  do using the construct database.

      declare
         H_Loc : constant General_Location := Extract_Next_By_Heuristics;
      begin
         if H_Loc /= No_Location and then
            --  If we found nothing, use the information from the constructs.
             (Candidate = No_Location

              --  else if the candidate is at the expected location and if it's
              --  OK to return the first entity.
              or else (not No_Location_If_First
                       and then not Is_Location_For_Entity (Candidate)))

         then
            Trace (Me, "Body computed from constructs");
            Candidate := H_Loc;

         --  If we don't have any more information to extract from the
         --  construct database, then return the first entity if allowed by
         --  the flags, or null.

         elsif No_Location_If_First then
            Candidate := No_Location;
         end if;
      end;

      if Active (Me) then
         Decrease_Indent (Me);
      end if;

      return Candidate;
   end Get_Body;

   -----------------
   -- Get_Type_Of --
   -----------------

   function Get_Type_Of
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if not Active (SQLITE) then
         declare
            E : constant Old_Entities.Entity_Information :=
                  Old_Entities.Get_Type_Of (Entity.Old_Entity);
         begin
            return From_Old (E);
         end;
      else
         return From_New (Db.Xref.Type_Of (Entity.Entity));
      end if;
   end Get_Type_Of;

   -------------------
   -- Returned_Type --
   -------------------

   function Returned_Type
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity is
   begin
      if Active (SQLITE) then
         return From_New (Db.Xref.Type_Of (Entity.Entity));
      else
         return From_Old (Old_Entities.Get_Returned_Type (Entity.Old_Entity));
      end if;
   end Returned_Type;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   function Is_Predefined_Entity
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean is
   begin
      if not Active (SQLITE) then
         return Old_Entities.Is_Predefined_Entity (E.Old_Entity);
      else
         return Is_Predefined_Entity (Declaration (Db.Xref.all, E.Entity));
      end if;
   end Is_Predefined_Entity;

   ----------------------
   -- Node_From_Entity --
   ----------------------

   procedure Node_From_Entity
     (Self        : access General_Xref_Database_Record'Class;
      Handler     : Language_Handlers.Language_Handler;
      Decl        : General_Location;
      Ent         : out Entity_Access;
      Tree_Lang   : out Tree_Language_Access)
   is
      Data_File   : Structured_File_Access;
      Node        : Construct_Tree_Iterator;
   begin
      Ent       := Null_Entity_Access;
      Tree_Lang := Get_Tree_Language_From_File (Handler, Decl.File, False);
      Data_File := Language.Tree.Database.Get_Or_Create
        (Db   => Self.Constructs,
         File => Decl.File);

      if Data_File /= null then
         Node := Get_Iterator_At
           (Tree        => Get_Tree (Data_File),
            Location    =>
              (Absolute_Offset => False,
               Line            => Decl.Line,
               Line_Offset     => To_Line_String_Index
                 (Data_File, Decl.Line, Decl.Column)),
            From_Type   => Start_Name);

         if Node /= Null_Construct_Tree_Iterator then
            Ent := To_Entity_Access (Data_File, Node);
         end if;
      end if;
   end Node_From_Entity;

   ------------------
   -- Pointed_Type --
   ------------------

   function Pointed_Type
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Db.Xref.Pointed_Type (Entity.Entity));
      else
         return From_Old
           (Old_Entities.Queries.Pointed_Type (Entity.Old_Entity));
      end if;
   end Pointed_Type;

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : General_Entity) is
   begin
      Old_Entities.Ref (Entity.Old_Entity);
   end Ref;

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
      pragma Assert (Active (SQLITE));

      Loc :=
        (File   => Decl.Location.File,
         Line   => Decl.Location.Line,
         Column => Visible_Column_Type (Decl.Location.Column));

      return Get_Entity
        (Db   => Db,
         Name => To_String (Decl.Name),
         Loc  => Loc);
   end To_General_Entity;

   -----------------------
   -- To_General_Entity --
   -----------------------

   function To_General_Entity
     (E  : Old_Entities.Entity_Information) return General_Entity is
   begin
      pragma Assert (not Active (SQLITE));

      if E = null then
         return No_General_Entity;
      else
         return General_Entity'(Old_Entity => E, others => <>);
      end if;
   end To_General_Entity;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out General_Entity) is
   begin
      Old_Entities.Unref (Entity.Old_Entity);
   end Unref;

   -----------------
   -- Renaming_Of --
   -----------------

   function Renaming_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return (Entity => Self.Xref.Renaming_Of (Entity.Entity),
                 others => <>);
      else
         return (Old_Entity =>
                   Old_Entities.Queries.Renaming_Of (Entity.Old_Entity),
                 others     => <>);
      end if;
   end Renaming_Of;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Ref1, Ref2 : General_Entity_Reference) return Boolean
   is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         return Ref1.Ref = Ref2.Ref;
      else
         return Ref1.Old_Ref = Ref2.Old_Ref;
      end if;
   end "=";

   ---------
   -- "=" --
   ---------

   overriding function "=" (E1, E2 : General_Entity) return Boolean is
   begin
      if Active (SQLITE) then
         if E1.Entity = No_Entity then
            return E2.Entity = No_Entity
              and then E2.Node = Null_Entity_Access;
         elsif E2.Entity = No_Entity then
            return E1.Entity = No_Entity
              and then E1.Node = Null_Entity_Access;
         else
            return E1.Entity = E2.Entity;
         end if;
      else
         if E1.Old_Entity = null then
            return E2.Old_Entity = null
              and then E2.Node = Null_Entity_Access;
         elsif E2.Old_Entity = null then
            return E1.Old_Entity = null
              and then E1.Node = Null_Entity_Access;
         else
            return E1.Old_Entity = E2.Old_Entity;
         end if;
      end if;
   end "=";

   -------------------------
   -- Find_All_References --
   -------------------------

   procedure Find_All_References
     (Self                  : access General_Xref_Database_Record;
      Iter                  : out Entity_Reference_Iterator;
      Entity                : General_Entity;
      File_Has_No_LI_Report : Basic_Types.File_Error_Reporter := null;
      In_File              : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      In_Scope              : General_Entity := No_General_Entity;
      Include_Overriding    : Boolean := False;
      Include_Overridden    : Boolean := False)
   is
      F      : Old_Entities.Source_File;
   begin
      if Active (SQLITE) then
         --  File_Has_No_LI_Report voluntarily ignored.

         Self.Xref.Recursive
           (Entity          => Entity.Entity,
            Compute         => GNATCOLL.Xref.References'Access,
            Cursor          => Iter.Iter,
            From_Overriding => Include_Overriding,
            From_Overridden => Include_Overridden,
            From_Renames    => True);
         Iter.In_File  := In_File;
         Iter.In_Scope := In_Scope;

         while Has_Element (Iter.Iter)
           and then (Iter.In_File = No_File
                     or else Iter.Iter.Element.File /= Iter.In_File)
           and then (Iter.In_Scope = No_General_Entity
                     or else Iter.Iter.Element.Scope /= Iter.In_Scope.Entity)
         loop
            Iter.Iter.Next;
         end loop;

      else
         declare
            use Old_Entities;
         begin
            if In_File /= No_File then
               F := Old_Entities.Get_Or_Create
                 (Db    => Self.Entities,
                  File  => In_File,
                  Allow_Create => True);
            end if;

            Old_Entities.Queries.Find_All_References
              (Iter.Old_Iter, Entity.Old_Entity,
               File_Has_No_LI_Report, F, In_Scope.Old_Entity,
               Include_Overriding => Include_Overriding,
               Include_Overridden => Include_Overridden);

            --  Skip cases where No_Entity_Reference is returned.
            while not At_End (Iter.Old_Iter)
              and then Get (Iter.Old_Iter) = Old_Entities.No_Entity_Reference
            loop
               Next (Iter.Old_Iter);
            end loop;
         end;
      end if;
   end Find_All_References;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Entity_Reference_Iterator) return Boolean is
   begin
      if Active (SQLITE) then
         return not Has_Element (Iter.Iter);
      else
         return At_End (Iter.Old_Iter);
      end if;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Entity_Reference_Iterator) is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         Next (Iter.Iter);

         while Has_Element (Iter.Iter)
           and then (Iter.In_File = No_File
                     or else Iter.Iter.Element.File /= Iter.In_File)
           and then (Iter.In_Scope = No_General_Entity
                     or else Iter.Iter.Element.Scope /= Iter.In_Scope.Entity)
         loop
            Iter.Iter.Next;
         end loop;

      else
         Next (Iter.Old_Iter);
         while not At_End (Iter.Old_Iter)
           and then Get (Iter.Old_Iter) = Old_Entities.No_Entity_Reference
         loop
            Next (Iter.Old_Iter);
         end loop;
      end if;
   end Next;

   ---------
   -- Get --
   ---------

   function Get
     (Iter : Entity_Reference_Iterator) return General_Entity_Reference
   is
   begin
      if Active (SQLITE) then
         return (Old_Ref => Old_Entities.No_Entity_Reference,
                 Ref     => Iter.Iter.Element);
      else
         return (Old_Ref => Get (Iter.Old_Iter),
                 Ref     => No_Entity_Reference);
      end if;
   end Get;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Iter : Entity_Reference_Iterator) return General_Entity is
   begin
      if Active (SQLITE) then
         return (Entity     => Iter.Iter.Element.Entity,
                 others     => <>);
      else
         return To_General_Entity (Get_Entity (Iter.Old_Iter));
      end if;
   end Get_Entity;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Reference_Iterator) is
   begin
      if Active (SQLITE) then
         null;
      else
         Destroy (Iter.Old_Iter);
      end if;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entity_Reference_Iterator_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Entity_Reference_Iterator,
         Entity_Reference_Iterator_Access);
   begin
      if Iter /= null then
         Destroy (Iter.all);
         Unchecked_Free (Iter);
      end if;
   end Destroy;

   --------------------------
   -- Get_Current_Progress --
   --------------------------

   function Get_Current_Progress
     (Iter : Entity_Reference_Iterator) return Integer is
   begin
      if Active (SQLITE) then
         return 1;
      else
         return Get_Current_Progress (Iter.Old_Iter);
      end if;
   end Get_Current_Progress;

   ------------------------
   -- Get_Total_Progress --
   ------------------------

   function Get_Total_Progress
     (Iter : Entity_Reference_Iterator) return Integer is
   begin
      if Active (SQLITE) then
         --  precomputing the number of references is expensive (basically
         --  requires doing the query twice), and won't be needed anymore when
         --  we get rid of the asynchronous iterators.
         return 1;
      else
         return Get_Total_Progress (Iter.Old_Iter);
      end if;
   end Get_Total_Progress;

   -----------------------
   -- Show_In_Callgraph --
   -----------------------

   function Show_In_Callgraph
     (Db  : access General_Xref_Database_Record;
      Ref : General_Entity_Reference) return Boolean
   is
   begin
      if Active (SQLITE) then
         return Db.Xref.Show_In_Callgraph (Ref.Ref);
      else
         return Old_Entities.Show_In_Call_Graph
           (Db.Entities, Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Show_In_Callgraph;

   ----------------
   -- Get_Caller --
   ----------------

   function Get_Caller
     (Ref : General_Entity_Reference) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return (Entity => Ref.Ref.Scope, others => <>);
      else
         return (Old_Entity => Old_Entities.Queries.Get_Caller (Ref.Old_Ref),
                 others => <>);
      end if;
   end Get_Caller;

   -------------------
   -- Is_Subprogram --
   -------------------

   function Is_Subprogram
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Subprogram;
      else
         return Old_Entities.Is_Subprogram (E.Old_Entity);
      end if;
   end Is_Subprogram;

   ------------------
   -- Is_Container --
   ------------------

   function Is_Container
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Container;
      else
         return Old_Entities.Is_Container
           (Old_Entities.Get_Kind (E.Old_Entity).Kind);
      end if;
   end Is_Container;

   ----------------
   -- Is_Generic --
   ----------------

   function Is_Generic
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Generic;
      else
         return Old_Entities.Get_Kind (E.Old_Entity).Is_Generic;
      end if;
   end Is_Generic;

   ---------------
   -- Is_Global --
   ---------------

   function Is_Global
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean is
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Global;
      else
         return Old_Entities.Get_Attributes
           (E.Old_Entity)(Old_Entities.Global);
      end if;
   end Is_Global;

   ---------------------
   -- Is_Static_Local --
   ---------------------

   function Is_Static_Local
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean is
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Static_Local;
      else
         return Old_Entities.Get_Attributes
           (E.Old_Entity)(Old_Entities.Static_Local);
      end if;
   end Is_Static_Local;

   -------------
   -- Is_Type --
   -------------

   function Is_Type
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Type;
      else
         return Old_Entities.Get_Category (E.Old_Entity) =
           Old_Entities.Type_Or_Subtype;
      end if;
   end Is_Type;

   -------------------------
   -- Is_Dispatching_Call --
   -------------------------

   function Is_Dispatching_Call
     (Db  : access General_Xref_Database_Record;
      Ref : General_Entity_Reference) return Boolean
   is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         return Db.Xref.Is_Dispatching_Call (Ref.Ref);
      else
         return Old_Entities.Get_Kind (Ref.Old_Ref) =
           Old_Entities.Dispatching_Call;
      end if;
   end Is_Dispatching_Call;

   ------------
   -- At_End --
   ------------

   function At_End (Iter : Base_Entities_Cursor) return Boolean is
   begin
      if Active (SQLITE) then
         return not Has_Element (Iter.Iter);
      else
         raise Program_Error;
      end if;
   end At_End;

   ---------
   -- Get --
   ---------

   function Get (Iter : Base_Entities_Cursor) return General_Entity is
   begin
      if Active (SQLITE) then
         return (Entity => Element (Iter.Iter), others => <>);
      else
         raise Program_Error;
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Base_Entities_Cursor) is
   begin
      if Active (SQLITE) then
         Next (Iter.Iter);
      else
         raise Program_Error;
      end if;
   end Next;

   -----------------------------
   -- Get_All_Called_Entities --
   -----------------------------

   function Get_All_Called_Entities
     (Self   : access General_Xref_Database_Record'Class;
      Entity : General_Entity) return Calls_Iterator
   is
      Result : Calls_Iterator;
   begin
      if Active (SQLITE) then
         Self.Xref.Calls (Entity.Entity, Result.Iter);
      else
         Result.Old_Iter := Old_Entities.Queries.Get_All_Called_Entities
           (Entity.Old_Entity);
      end if;
      return Result;
   end Get_All_Called_Entities;

   ----------------------
   -- Entities_In_File --
   ----------------------

   function Entities_In_File
     (Self   : access General_Xref_Database_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Name   : String := "") return Entities_In_File_Cursor
   is
      Result : Entities_In_File_Cursor;
      F      : Old_Entities.Source_File;
   begin
      if Active (SQLITE) then
         if Name = "" then
            Self.Xref.Referenced_In (File, Cursor => Result.Iter);
         else
            Self.Xref.Referenced_In (File, Name, Cursor => Result.Iter);
         end if;

      else
         F := Old_Entities.Get_Or_Create
           (Self.Entities, File, Allow_Create => True);
         Old_Entities.Queries.Find_All_Entities_In_File
           (Iter        => Result.Old_Iter,
            File        => F,
            Name        => Name);
      end if;
      return Result;
   end Entities_In_File;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Iter : Entities_In_File_Cursor) return Boolean is
   begin
      if Active (SQLITE) then
         return At_End (Base_Entities_Cursor (Iter));
      else
         return At_End (Iter.Old_Iter);
      end if;
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Entities_In_File_Cursor) return General_Entity is
   begin
      if Active (SQLITE) then
         return Get (Base_Entities_Cursor (Iter));
      else
         return (Old_Entity => Get (Iter.Old_Iter), others => <>);
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Iter : in out Entities_In_File_Cursor) is
   begin
      if Active (SQLITE) then
         Next (Base_Entities_Cursor (Iter));
      else
         Next (Iter.Old_Iter);
      end if;
   end Next;

   ------------------------------
   -- All_Entities_From_Prefix --
   ------------------------------

   function All_Entities_From_Prefix
     (Self       : access General_Xref_Database_Record'Class;
      Prefix     : String;
      Is_Partial : Boolean := True) return Entities_In_Project_Cursor
   is
      use Old_Entities.Entities_Search_Tries;
      Result : Entities_In_Project_Cursor;
   begin
      if Active (SQLITE) then
         null;
      else
         Result.Old_Iter :=
           Start (Trie     => Old_Entities.Get_Name_Index
                     (Old_Entities.Get_LI_Handler (Self.Entities)),
                  Prefix   => Prefix,
                  Is_Partial => Is_Partial);
      end if;
      return Result;
   end All_Entities_From_Prefix;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Entities_In_Project_Cursor) is
   begin
      if Active (SQLITE) then
         null;
      else
         Old_Entities.Entities_Search_Tries.Free (Iter.Old_Iter);
      end if;
   end Destroy;

   ------------
   -- At_End --
   ------------

   overriding function At_End
     (Iter : Entities_In_Project_Cursor) return Boolean
   is
      use Old_Entities.Entities_Search_Tries;
   begin
      if Active (SQLITE) then
         return True;
      else
         return At_End (Iter.Old_Iter);
      end if;
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Iter : Entities_In_Project_Cursor) return General_Entity
   is
      use Old_Entities.Entities_Search_Tries;
   begin
      if Active (SQLITE) then
         return No_General_Entity;
      else
         return From_Old (Get (Iter.Old_Iter));
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Entities_In_Project_Cursor) is
      use Old_Entities.Entities_Search_Tries;
   begin
      if Active (SQLITE) then
         null;
      else
         Next (Iter.Old_Iter);
      end if;
   end Next;

   ------------
   -- At_End --
   ------------

   overriding function At_End (Iter : Calls_Iterator) return Boolean is
   begin
      if Active (SQLITE) then
         return At_End (Base_Entities_Cursor (Iter));
      else
         return At_End (Iter.Old_Iter);
      end if;
   end At_End;

   ---------
   -- Get --
   ---------

   overriding function Get (Iter : Calls_Iterator) return General_Entity is
   begin
      if Active (SQLITE) then
         return Get (Base_Entities_Cursor (Iter));
      else
         return (Old_Entity => Get (Iter.Old_Iter), others => <>);
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   overriding procedure Next (Iter : in out Calls_Iterator) is
   begin
      if Active (SQLITE) then
         Next (Base_Entities_Cursor (Iter));
      else
         Next (Iter.Old_Iter);
      end if;
   end Next;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out Calls_Iterator) is
   begin
      if not Active (SQLITE) then
         Destroy (Iter.Old_Iter);
      end if;
   end Destroy;

   ------------
   -- To_Old --
   ------------

   function To_Old
     (Entity : General_Entity) return Old_Entities.Entity_Information is
   begin
      return Entity.Old_Entity;
   end To_Old;

   function From_Old
     (Entity : Old_Entities.Entity_Information) return General_Entity is
   begin
      return (Old_Entity => Entity, others => <>);
   end From_Old;

   ------------
   -- To_New --
   ------------

   function To_New
     (Entity : General_Entity) return GNATCOLL.Xref.Entity_Information is
   begin
      return Entity.Entity;
   end To_New;

   --------------
   -- From_New --
   --------------

   function From_New
     (Entity : GNATCOLL.Xref.Entity_Information) return General_Entity is
   begin
      return (Entity => Entity, others => <>);
   end From_New;

   ----------------
   -- Parameters --
   ----------------

   function Parameters
     (Dbase  : access General_Xref_Database_Record;
      Entity : General_Entity) return Parameter_Array
   is
      All_Params : Parameter_Array (1 .. 100);
      Count      : Integer := All_Params'First - 1;
   begin
      if Active (SQLITE) then
         declare
            Curs : Parameters_Cursor := Dbase.Xref.Parameters (Entity.Entity);
         begin
            while Curs.Has_Element loop
               Count := Count + 1;
               All_Params (Count) :=
                 (Kind => Curs.Element.Kind,
                  Parameter => From_New (Curs.Element.Parameter));
               Curs.Next;
            end loop;
         end;

      else
         declare
            Iter : Old_Entities.Queries.Subprogram_Iterator :=
              Old_Entities.Queries.Get_Subprogram_Parameters
                (Entity.Old_Entity);
            E    : Old_Entities.Entity_Information;
         begin
            loop
               Old_Entities.Queries.Get (Iter, E);
               exit when E = null;

               Count := Count + 1;
               All_Params (Count).Parameter := From_Old (E);
               case Old_Entities.Queries.Get_Type (Iter) is
                  when Old_Entities.Queries.In_Parameter =>
                     All_Params (Count).Kind := In_Parameter;
                  when Old_Entities.Queries.Out_Parameter =>
                     All_Params (Count).Kind := Out_Parameter;
                  when Old_Entities.Queries.In_Out_Parameter =>
                     All_Params (Count).Kind := In_Out_Parameter;
                  when Old_Entities.Queries.Access_Parameter =>
                     All_Params (Count).Kind := Access_Parameter;
               end case;

               Old_Entities.Queries.Next (Iter);
            end loop;
         end;
      end if;

      return All_Params (All_Params'First .. Count);
   end Parameters;

   ---------------------
   -- Is_Parameter_Of --
   ---------------------

   function Is_Parameter_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Parameter_Of (Entity.Entity));
      else
         return From_Old
           (Old_Entities.Queries.Is_Parameter_Of (Entity.Old_Entity));
      end if;
   end Is_Parameter_Of;

   ---------------------
   -- Is_Primitive_Of --
   ---------------------

   function Is_Primitive_Of
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Method_Of (Entity.Entity));
      else
         return From_Old
           (Old_Entities.Is_Primitive_Operation_Of (Entity.Old_Entity));
      end if;
   end Is_Primitive_Of;

   -------------------
   -- Is_Up_To_Date --
   -------------------

   function Is_Up_To_Date
     (Self : access General_Xref_Database_Record;
      File : Virtual_File) return Boolean
   is
      Source : Old_Entities.Source_File;
   begin
      if Active (SQLITE) then
         return Self.Xref.Is_Up_To_Date (File);
      else
         Source := Old_Entities.Get_Or_Create
           (Self.Entities, File, Allow_Create => True);
         return Old_Entities.Is_Up_To_Date (Source);
      end if;
   end Is_Up_To_Date;

   -----------------
   -- Has_Methods --
   -----------------

   function Has_Methods
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
      use Old_Entities;
      K  : Old_Entities.E_Kinds;
   begin
      if Active (SQLITE) then
         if E.Entity /= No_Entity then
            return Db.Xref.Declaration (E.Entity).Flags.Has_Methods;
         end if;

      else
         if E.Old_Entity /= null then
            K := Old_Entities.Get_Kind (E.Old_Entity).Kind;
            return K = Old_Entities.Class
              or else K = Record_Kind
              or else K = Old_Entities.Interface_Kind;
         end if;
      end if;

      --  ??? Fallback on constructs
      return False;
   end Has_Methods;

   ---------------
   -- Is_Access --
   ---------------

   function Is_Access
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Access;

      else
         return E.Old_Entity /= null
           and then Old_Entities.Get_Kind (E.Old_Entity).Kind =
              Old_Entities.Access_Kind;
      end if;
   end Is_Access;

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Abstract;

      else
         return E.Old_Entity /= null
           and then Old_Entities.Get_Kind (E.Old_Entity).Is_Abstract;
      end if;
   end Is_Abstract;

   --------------
   -- Is_Array --
   --------------

   function Is_Array
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
      use Old_Entities;
      Is_Array_E : constant array (E_Kinds) of Boolean :=
        (Overloaded_Entity => True,
         Unresolved_Entity => True,
         Array_Kind        => True,
         String_Kind       => True,
         others            => False);
   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Array;

      else
         return E.Old_Entity /= null
           and then Is_Array_E (Old_Entities.Get_Kind (E.Old_Entity).Kind);
      end if;
   end Is_Array;

   ------------------------------
   -- Is_Printable_In_Debugger --
   ------------------------------

   function Is_Printable_In_Debugger
     (Db : access General_Xref_Database_Record;
      E  : General_Entity) return Boolean
   is
      use Old_Entities;
      Is_Printable_Entity : constant array (E_Kinds) of Boolean :=
        (Overloaded_Entity    => True,
         Unresolved_Entity    => True,
         Access_Kind          => True,
         Array_Kind           => True,
         Boolean_Kind         => True,
         Class_Wide           => True,
         Class                => True,
         Decimal_Fixed_Point  => True,
         Enumeration_Literal  => True,
         Enumeration_Kind     => True,
         Exception_Entity     => True,
         Floating_Point       => True,
         Modular_Integer      => True,
         Named_Number         => True,
         Ordinary_Fixed_Point => True,
         Record_Kind          => True,
         Signed_Integer       => True,
         String_Kind          => True,
         others               => False);

   begin
      if Active (SQLITE) then
         return Db.Xref.Declaration (E.Entity).Flags.Is_Printable_In_Gdb;

      else
         return E.Old_Entity /= null
           and then Is_Printable_Entity
             (Old_Entities.Get_Kind (E.Old_Entity).Kind);
      end if;
   end Is_Printable_In_Debugger;

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   function Get_Display_Kind
     (Db     : access General_Xref_Database_Record;
      Entity : General_Entity) return String
   is
   begin
      if Active (SQLITE) then
         return To_String (Db.Xref.Declaration (Entity.Entity).Kind);
      else
         return Old_Entities.Kind_To_String
           (Old_Entities.Get_Kind (Entity.Old_Entity));
      end if;
   end Get_Display_Kind;

   ------------------------------
   -- Reference_Is_Declaration --
   ------------------------------

   function Reference_Is_Declaration
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean
   is
      pragma Unreferenced (Db);
   begin
      if Active (SQLITE) then
         return Ref.Ref.Kind_Id = Kind_Id_Declaration;
      else
         return Old_Entities.Queries.Reference_Is_Declaration
           (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Reference_Is_Declaration;

   -----------------------
   -- Reference_Is_Body --
   -----------------------

   function Reference_Is_Body
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean
   is
      pragma Unreferenced (Db);
   begin
      if Active (SQLITE) then
         return Ref.Ref.Kind = "body";
      else
         return Old_Entities.Queries.Reference_Is_Body
           (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Reference_Is_Body;

   -----------------------
   -- Is_Read_Reference --
   -----------------------

   function Is_Read_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean is
   begin
      if Active (SQLITE) then
         return Db.Xref.Is_Read_Reference (Ref.Ref);
      else
         return Old_Entities.Is_Read_Reference
           (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Is_Read_Reference;

   --------------------------------------------
   -- Is_Or_Read_Write_Or_Implicit_Reference --
   --------------------------------------------

   function Is_Read_Or_Write_Or_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Is_Read_Or_Write_Reference (Db, Ref)
        or else Is_Implicit_Reference (Db, Ref);
   end Is_Read_Or_Write_Or_Implicit_Reference;

   -----------------------------------
   -- Is_Read_Or_Implicit_Reference --
   -----------------------------------

   function Is_Read_Or_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean
   is
   begin
      return Is_Read_Reference (Db, Ref)
        or else Is_Implicit_Reference (Db, Ref);
   end Is_Read_Or_Implicit_Reference;

   ---------------------------
   -- Is_Implicit_Reference --
   ---------------------------

   function Is_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean
   is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         return Db.Xref.Is_Implicit_Reference (Ref.Ref);
      else
         return Old_Entities.Get_Kind (Ref.Old_Ref) = Old_Entities.Implicit;
      end if;
   end Is_Implicit_Reference;

   -----------------------
   -- Is_Real_Reference --
   -----------------------

   function Is_Real_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean is
   begin
      if Active (SQLITE) then
         return Db.Xref.Is_Real_Reference (Ref.Ref);
      else
         return Old_Entities.Is_Real_Reference
           (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Is_Real_Reference;

   -----------------------------------
   -- Is_Real_Or_Implicit_Reference --
   -----------------------------------

   function Is_Real_Or_Implicit_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean is
   begin
      return Is_Real_Reference (Db, Ref)
        or else Is_Implicit_Reference (Db, Ref);
   end Is_Real_Or_Implicit_Reference;

   ------------------------
   -- Is_Write_Reference --
   ------------------------

   function Is_Write_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean is
   begin
      if Active (SQLITE) then
         return Db.Xref.Is_Write_Reference (Ref.Ref);
      else
         return Old_Entities.Is_Write_Reference
           (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Is_Write_Reference;

   --------------------------------
   -- Is_Read_Or_Write_Reference --
   --------------------------------

   function Is_Read_Or_Write_Reference
     (Db  : access General_Xref_Database_Record'Class;
      Ref : General_Entity_Reference) return Boolean is
   begin
      if Active (SQLITE) then
         return Db.Xref.Is_Read_Or_Write_Reference (Ref.Ref);
      else
         return Old_Entities.Is_Write_Reference
           (Old_Entities.Get_Kind (Ref.Old_Ref))
           or else Old_Entities.Is_Read_Reference
             (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Is_Read_Or_Write_Reference;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out General_Xref_Database) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (General_Xref_Database_Record'Class, General_Xref_Database);
   begin
      if Active (SQLITE) then
         Self.Xref.Free;
         Self.Xref := null;
      else
         Old_Entities.Destroy (Self.Entities);
      end if;

      Free (Self.Constructs);

      Unchecked_Free (Self);
   end Destroy;

   ------------
   -- Freeze --
   ------------

   function Freeze
     (Self : access General_Xref_Database_Record) return Database_Lock is
   begin
      if not Active (SQLITE) then
         Old_Entities.Freeze (Self.Entities);
      end if;

      return (Constructs =>
                Old_Entities.Lock_Construct_Heuristics (Self.Entities));
   end Freeze;

   ----------
   -- Thaw --
   ----------

   procedure Thaw
     (Self : access General_Xref_Database_Record;
      Lock : in out Database_Lock) is
   begin
      if not Active (SQLITE) then
         Old_Entities.Thaw (Self.Entities);
      end if;

      Old_Entities.Unlock_Construct_Heuristics (Lock.Constructs);
   end Thaw;

   ------------------
   -- End_Of_Scope --
   ------------------

   function End_Of_Scope
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Location
   is
      Iter : References_Cursor;
      Ref  : Entity_Reference;
   begin
      if Active (SQLITE) then
         Self.Xref.References (Entity.Entity, Cursor => Iter);
         while Has_Element (Iter) loop
            Ref := Element (Iter);
            if Ref.Is_End_Of_Scope then
               return (File   => Ref.File,
                       Line   => Ref.Line,
                       Column => Ref.Column);
            end if;

            Next (Iter);
         end loop;

      else
         declare
            use Old_Entities;
            Kind  : Old_Entities.Reference_Kind;
            Loc   : Old_Entities.File_Location;
         begin
            Old_Entities.Get_End_Of_Scope (Entity.Old_Entity, Loc, Kind);
            if Loc /= Old_Entities.No_File_Location then
               return (File => Get_Filename (Loc.File),
                       Line => Get_Line (Loc),
                       Column => Get_Column (Loc));
            end if;
         end;
      end if;
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
      Subprogram_Ref_Is_Call : Boolean := False)
   is
      use Construct_Annotations_Pckg;
      LI_Entity_Key : Construct_Annotations_Pckg.Annotation_Key;
   begin
      Self.Constructs := new Language.Tree.Database.Construct_Database;
      Self.Lang_Handler := Abstract_Language_Handler (Lang_Handler);
      Set_Symbols (Self.Constructs, Symbols);

      Self.Symbols := Symbols;

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

      if Active (SQLITE) then
         if Self.Xref = null then
            Self.Xref := new Extended_Xref_Database;
         end if;

      else
         Self.Entities := Old_Entities.Create
           (Registry,
            Self.Constructs,
            Normal_Ref_In_Call_Graph => Subprogram_Ref_Is_Call);

         Old_Entities.Set_Symbols (Self.Entities, Symbols);
         Old_Entities.Register_Language_Handler (Self.Entities, Lang_Handler);
         Old_Entities.Set_LI_Handler
           (Self.Entities, ALI_Parser.Create_ALI_Handler
              (Db           => Self.Entities,
               Registry     => Registry.all,
               Lang_Handler => Lang_Handler));
      end if;
   end Initialize;

   ------------------
   -- To_LI_Entity --
   ------------------

   function To_LI_Entity (E : Entity_Access) return General_Entity is
      use Construct_Annotations_Pckg;

      Assistant : constant LI_Db_Assistant_Access := LI_Db_Assistant_Access
        (Get_Assistant (Get_Database (Get_File (E)), LI_Assistant_Id));

      Construct_Annotation : Construct_Annotations_Pckg.Annotation;
   begin
      Get_Annotation
        (Get_Annotation_Container
           (Get_Tree (Get_File (E)), To_Construct_Tree_Iterator (E)).all,
         Assistant.LI_Key,
         Construct_Annotation);

      if Construct_Annotation = Construct_Annotations_Pckg.Null_Annotation then
         --  Create a new LI entity

         Construct_Annotation := (Other_Kind, Other_Val => new LI_Annotation);

         if not Active (SQLITE) then
            declare
               use Old_Entities, Language;
               New_Entity  : Old_Entities.Entity_Information;
               Declaration : Old_Entities.File_Location;
               K           : E_Kinds;
               Is_Type     : Boolean := False;
            begin
               Declaration :=
                 (Get_Or_Create
                    (Db    => Assistant.Db.Entities,
                     File  => Get_File_Path (Get_File (E))),
                  Get_Construct (E).Sloc_Entity.Line,
                  To_Visible_Column
                    (Get_File (E),
                     Get_Construct (E).Sloc_Entity.Line,
                     String_Index_Type
                       (Get_Construct (E).Sloc_Entity.Column)));

               --  Make a simple association between construct categories
               --  and entity categories. This association is known to be
               --  inaccurate, but is helpful when trying to categorize
               --  entities.

               case Get_Construct (E).Category is
               when Cat_Package | Cat_Namespace => K := Package_Kind;
               when Cat_Task
                  | Cat_Procedure
                  | Cat_Function
                  | Cat_Method
                  | Cat_Constructor
                  | Cat_Destructor
                  | Cat_Protected
                  | Cat_Entry =>

                  K := Procedure_Kind;

               when Cat_Class
                  | Cat_Structure
                  | Cat_Case_Inside_Record
                  | Cat_Union
                  | Cat_Type
                  | Cat_Subtype =>

                  K := Class;
                  Is_Type := True;

               when Cat_Variable
                  | Cat_Local_Variable
                  | Cat_Parameter
                  | Cat_Discriminant
                  | Cat_Field =>

                  K := Signed_Integer;

               when Cat_Literal =>

                  K := Enumeration_Literal;

               when Cat_With
                  | Cat_Use
                  | Cat_Include =>

                  K := Include_File;

               when Cat_Unknown
                  | Cat_Representation_Clause
                  | Cat_Loop_Statement
                  | Cat_If_Statement
                  | Cat_Case_Statement
                  | Cat_Select_Statement
                  | Cat_Accept_Statement
                  | Cat_Declare_Block
                  | Cat_Return_Block
                  | Cat_Simple_Block
                  | Cat_Exception_Handler
                  | Cat_Pragma
                  | Cat_Custom =>

                  K := Unresolved_Entity;
               end case;

               New_Entity := Old_Entities.Create_Dummy_Entity
                 (Name    => Get_Construct (E).Name,
                  Decl    => Declaration,
                  Kind    => K,
                  Is_Type => Is_Type);

               LI_Annotation (Construct_Annotation.Other_Val.all).Entity :=
                 From_Old (New_Entity);
               Set_Annotation
                 (Get_Annotation_Container
                    (Get_Tree
                       (Get_File (E)), To_Construct_Tree_Iterator (E)).all,
                  Assistant.LI_Key,
                  Construct_Annotation);
            end;
         end if;

      else
         --  Update the entity in case it has moved
         null;

--           LI_Annotation (Construct_Annotation.Other_Val.all).
--             LI_Entity.Live_Declaration.Line :=
--               Get_Construct (E).Sloc_Entity.Line;
--           LI_Annotation (Construct_Annotation.Other_Val.all).
--             LI_Entity.Live_Declaration.Column := To_Visible_Column
--               (Get_File (E),
--                Get_Construct (E).Sloc_Entity.Line,
--                String_Index_Type (Get_Construct (E).Sloc_Entity.Column));
      end if;

      return LI_Annotation (Construct_Annotation.Other_Val.all).Entity;
   end To_LI_Entity;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Obj : in out LI_Annotation) is
   begin
      Unref (Obj.Entity);
   end Free;

   ----------
   -- Hash --
   ----------

   function Hash
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return Integer
   is

      Decl : Entity_Declaration;
   begin
      if Active (SQLITE) then
         Decl := Self.Xref.Declaration (Entity.Entity);
         return Integer
           (Hash (To_String (Decl.Name)
            & Decl.Location.File.Display_Full_Name
            & Decl.Location.Line'Img
            & Decl.Location.Column'Img));

      elsif Entity.Old_Entity /= null then
         declare
            use Old_Entities;
            Loc : constant File_Location :=
              Get_Declaration_Of (Entity.Old_Entity);
         begin
            return Integer
              (Hash
                 (Get (Get_Name (Entity.Old_Entity)).all
                  & (+Full_Name (Get_Filename (Get_File (Loc))))
                  & Get_Line (Loc)'Img
                  & Get_Column (Loc)'Img));
         end;
      end if;

      return 0;
   end Hash;

   ---------
   -- Cmp --
   ---------

   function Cmp
     (Self   : access General_Xref_Database_Record;
      Entity1, Entity2 : General_Entity) return Integer
   is
   begin
      if Entity1 = No_General_Entity then
         if Entity2 = No_General_Entity then
            return 0;
         else
            return -1;
         end if;

      elsif Entity2 = No_General_Entity then
         return 1;

      else
         declare
            Name1 : constant String := Self.Get_Name (Entity1);
            Name2 : constant String := Self.Get_Name (Entity2);
         begin
            if Name1 < Name2 then
               return -1;

            elsif Name1 = Name2 then
               declare
                  File1 : constant Virtual_File :=
                    Self.Get_Declaration (Entity1).Loc.File;
                  File2 : constant Virtual_File :=
                    Self.Get_Declaration (Entity2).Loc.File;
               begin
                  if File1 < File2 then
                     return -1;
                  elsif File1 = File2 then
                     return 0;
                  else
                     return 1;
                  end if;
               end;

            else
               return 1;
            end if;
         end;
      end if;
   end Cmp;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Iter : File_Iterator) return Boolean is
   begin
      if Active (SQLITE) then
         return Has_Element (Iter.Iter);
      else
         if Iter.Is_Ancestor then
            return not Old_Entities.Queries.At_End (Iter.Old_Ancestor_Iter);
         else
            return not Old_Entities.Queries.At_End (Iter.Old_Iter);
         end if;
      end if;
   end Has_Element;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out File_Iterator) is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         Next (Iter.Iter);
      else
         if Iter.Is_Ancestor then
            Old_Entities.Queries.Next (Iter.Old_Ancestor_Iter);
         else
            Old_Entities.Queries.Next (Iter.Old_Iter);
            while not Old_Entities.Queries.At_End (Iter.Old_Iter)
              and then
                (Get (Iter.Old_Iter) = null
                 or else not Old_Entities.Queries.Is_Explicit (Iter.Old_Iter))
            loop
               Old_Entities.Queries.Next (Iter.Old_Iter);
            end loop;
         end if;
      end if;
   end Next;

   -------------
   -- Element --
   -------------

   function Element (Iter : File_Iterator) return Virtual_File is
   begin
      if Active (SQLITE) then
         return Element (Iter.Iter);
      else
         if Iter.Is_Ancestor then
            return Old_Entities.Get_Filename
              (Old_Entities.Queries.Get (Iter.Old_Ancestor_Iter));
         else
            return Old_Entities.Get_Filename
              (Old_Entities.Queries.Get (Iter.Old_Iter));
         end if;
      end if;
   end Element;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iter : in out File_Iterator) is
   begin
      if not Active (SQLITE) then
         if Iter.Is_Ancestor then
            Old_Entities.Queries.Destroy (Iter.Old_Ancestor_Iter);
         end if;
      end if;
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
     (Self : access General_Xref_Database_Record'Class;
      File : GNATCOLL.VFS.Virtual_File) return File_Iterator
   is
      use Old_Entities;
      Iter : File_Iterator;
   begin
      Iter.Is_Ancestor := False;

      if Active (SQLITE) then
         Iter.Iter := Self.Xref.Imports (File);
      else
         Old_Entities.Queries.Find_Dependencies
           (Iter => Iter.Old_Iter,
            File => Old_Entities.Get_Or_Create
              (Self.Entities, File, Allow_Create => True));

         while not Old_Entities.Queries.At_End (Iter.Old_Iter)
           and then
             (Get (Iter.Old_Iter) = null
              or else not Old_Entities.Queries.Is_Explicit (Iter.Old_Iter))
         loop
            Old_Entities.Queries.Next (Iter.Old_Iter);
         end loop;
      end if;
      return Iter;
   end Find_Dependencies;

   --------------------------------
   -- Find_Ancestor_Dependencies --
   --------------------------------

   function Find_Ancestor_Dependencies
     (Self                  : access General_Xref_Database_Record'Class;
      File                  : GNATCOLL.VFS.Virtual_File) return File_Iterator
   is
      use Old_Entities;
      Iter : File_Iterator;
   begin
      Iter.Is_Ancestor := True;

      if Active (SQLITE) then
         Iter.Iter := Self.Xref.Imported_By (File);
      else
         Old_Entities.Queries.Find_Ancestor_Dependencies
           (Iter               => Iter.Old_Ancestor_Iter,
            File               => Old_Entities.Get_Or_Create
              (Self.Entities, File, Allow_Create => True),
            Include_Self       => False,
            Single_Source_File => False);

         while not Old_Entities.Queries.At_End (Iter.Old_Ancestor_Iter)
           and then
             (Get (Iter.Old_Ancestor_Iter) = null
              or else
                not Old_Entities.Queries.Is_Explicit (Iter.Old_Ancestor_Iter))
         loop
            Old_Entities.Queries.Next (Iter.Old_Ancestor_Iter);
         end loop;
      end if;
      return Iter;
   end Find_Ancestor_Dependencies;

   ----------------------
   -- Get_Display_Kind --
   ----------------------

   function Get_Display_Kind
     (Ref  : General_Entity_Reference) return String is
   begin
      if Active (SQLITE) then
         return Ada.Strings.Unbounded.To_String (Ref.Ref.Kind);
      else
         return Old_Entities.Kind_To_String
           (Old_Entities.Get_Kind (Ref.Old_Ref));
      end if;
   end Get_Display_Kind;

   ------------------------------
   -- All_Real_Reference_Kinds --
   ------------------------------

   function All_Real_Reference_Kinds
     (Db  : access General_Xref_Database_Record)
      return GNAT.Strings.String_List
   is
      use Old_Entities;
   begin
      if Active (SQLITE) then
         return Db.Xref.All_Real_Reference_Kinds;
      else
         return Result : String_List
           (Reference_Kind'Pos (Reference_Kind'First) + 1 ..
              Reference_Kind'Pos (Reference_Kind'Last) + 1)
         do
            for R in Reference_Kind'Range loop
               Result (Reference_Kind'Pos (R) + 1) :=
                 new String'(Kind_To_String (R));
            end loop;
         end return;
      end if;
   end All_Real_Reference_Kinds;

   --------------
   -- Is_Fuzzy --
   --------------

   function Is_Fuzzy (Entity : General_Entity) return Boolean is
   begin
      return Entity.Is_Fuzzy;
   end Is_Fuzzy;

   ---------------------
   -- From_Constructs --
   ---------------------

   function From_Constructs
     (Entity : Language.Tree.Database.Entity_Access) return General_Entity is
   begin
      return (Node => Entity, others => <>);
   end From_Constructs;

   -----------------
   -- Instance_Of --
   -----------------

   function Instance_Of
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Instance_Of (Entity.Entity));
      else
         return From_Old
           (Old_Entities.Queries.Is_Instantiation_Of (Entity.Old_Entity));
      end if;
   end Instance_Of;

   --------------------
   -- From_Instances --
   --------------------

   function From_Instances
     (Self   : access General_Xref_Database_Record;
      Ref    : General_Entity_Reference) return Entity_Array
   is
   begin
      if Active (SQLITE) then
         declare
            R : constant GNATCOLL.Xref.Entity_Array :=
              Self.Xref.From_Instances (Ref.Ref);
            Result : Entity_Array (R'Range);
         begin
            for A in R'Range loop
               Result (A) := From_New (R (A));
            end loop;
            return Result;
         end;

      else
         declare
            use Old_Entities;
            Inst : constant Entity_Instantiation :=
              Old_Entities.From_Instantiation_At (Ref.Old_Ref);
            Current : Entity_Instantiation := Inst;
            Count : Natural := 0;
         begin
            while Current /= No_Instantiation loop
               Count := Count + 1;
               Current := Generic_Parent (Current);
            end loop;

            declare
               Result : Entity_Array (1 .. Count);
            begin
               Count := Result'First;
               Current := Inst;
               while Current /= No_Instantiation loop
                  Result (Count) := From_Old (Get_Entity (Current));
                  Count := Count + 1;
                  Current := Generic_Parent (Current);
               end loop;

               return Result;
            end;
         end;
      end if;
   end From_Instances;

   -----------------------
   -- Fill_Entity_Array --
   -----------------------

   procedure Fill_Entity_Array
     (Curs : in out Entities_Cursor'Class;
      Arr  : in out Entity_Arrays.Instance)
   is
   begin
      while Curs.Has_Element loop
         Append (Arr, From_New (Curs.Element));
         Curs.Next;
      end loop;
   end Fill_Entity_Array;

   ---------------------
   -- To_Entity_Array --
   ---------------------

   function To_Entity_Array
     (Arr : Entity_Arrays.Instance) return Entity_Array
   is
      Last : constant Integer := Integer (Entity_Arrays.Last (Arr));
      Result : Entity_Array (Integer (Entity_Arrays.First) .. Last);
   begin
      for R in Result'Range loop
         Result (R) := Arr.Table (Entity_Arrays.Index_Type (R));
      end loop;
      return Result;
   end To_Entity_Array;

   ---------------------
   -- Discriminant_Of --
   ---------------------

   function Discriminant_Of
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Discriminant_Of (Entity.Entity));
      else
         --  ??? Not implemented.
         --  Old_Entities.Queries.Is_Discriminant requires knowning the record
         --  itself before we event start.

         return No_General_Entity;
      end if;
   end Discriminant_Of;

   -------------------
   -- Discriminants --
   -------------------

   function Discriminants
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array
   is
      Arr : Entity_Arrays.Instance;
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
         begin
            Self.Xref.Discriminants (Entity.Entity, Cursor => Curs);
            Fill_Entity_Array (Curs, Arr);
         end;

      else
         declare
            Iter  : Old_Entities.Queries.Entity_Reference_Iterator;
            Discr : Old_Entities.Entity_Information;
         begin
            Old_Entities.Queries.Find_All_References
              (Iter, Entity.Old_Entity,
               Filter => (Old_Entities.Discriminant => True, others => False));

            while not At_End (Iter) loop
               Discr := Get_Entity (Iter);
               if Discr /= null then
                  Append (Arr, From_Old (Discr));
               end if;

               Next (Iter);
            end loop;

            Destroy (Iter);
         end;
      end if;

      return R : constant Entity_Array := To_Entity_Array (Arr) do
         Free (Arr);
      end return;
   end Discriminants;

   -----------------------
   -- Formal_Parameters --
   -----------------------

   function Formal_Parameters
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return Entity_Array
   is
      use Old_Entities;
      Arr : Entity_Arrays.Instance;
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
         begin
            Self.Xref.Formal_Parameters (Entity.Entity, Cursor => Curs);
            Fill_Entity_Array (Curs, Arr);
         end;

      else
         declare
            Param : Old_Entities.Entity_Information;
            Iter  : Old_Entities.Queries.Generic_Iterator :=
              Get_Generic_Parameters (Entity.Old_Entity);
         begin
            loop
               Get (Iter, Param);
               exit when Param = null;

               Append (Arr, From_Old (Param));
               Next (Iter);
            end loop;
         end;
      end if;

      return R : constant Entity_Array := To_Entity_Array (Arr) do
         Free (Arr);
      end return;
   end Formal_Parameters;

   --------------
   -- Literals --
   --------------

   function Literals
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array
   is
      use Old_Entities;
      Arr : Entity_Arrays.Instance;
   begin
      if Active (Me) then
         Increase_Indent
           (Me, "Retrieving literals of " & Self.Get_Name (Entity));
      end if;

      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
         begin
            Self.Xref.Literals (Entity.Entity, Cursor => Curs);
            Fill_Entity_Array (Curs, Arr);
         end;

      elsif Get_Kind (Entity.Old_Entity).Kind = Enumeration_Kind then
         declare
            Field : Old_Entities.Entity_Information;
            Iter  : Old_Entities.Queries.Calls_Iterator :=
              Get_All_Called_Entities (Entity.Old_Entity);
         begin
            while not At_End (Iter) loop
               Field := Get (Iter);

               if Active (Me) then
                  Trace
                    (Me, "Old: candidate: "
                     & Self.Get_Name (From_Old (Field))
                     & " range="
                     & In_Range (Old_Entities.Get_Declaration_Of (Field),
                       Entity.Old_Entity)'Img
                     & " cat=" & Get_Category (Field)'Img);
               end if;

               if In_Range (Old_Entities.Get_Declaration_Of (Field),
                            Entity.Old_Entity)
                 and then Get_Category (Field) = Literal
               then
                  Append (Arr, From_Old (Field));
               end if;

               Next (Iter);
            end loop;

            Destroy (Iter);
         end;
      end if;

      if Active (Me) then
         Decrease_Indent (Me);
      end if;

      return R : constant Entity_Array := To_Entity_Array (Arr) do
         Free (Arr);
      end return;
   end Literals;

   -----------------
   -- Child_Types --
   -----------------

   function Child_Types
      (Self      : access General_Xref_Database_Record;
       Entity    : General_Entity;
       Recursive : Boolean) return Entity_Array
   is
      use Old_Entities;
      Arr : Entity_Arrays.Instance;
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
            Rec  : Recursive_Entities_Cursor;
         begin
            if Recursive then
               Self.Xref.Recursive
                 (Entity  => Entity.Entity,
                  Compute => GNATCOLL.Xref.Child_Types'Access,
                  Cursor  => Rec);
               Fill_Entity_Array (Rec, Arr);
            else
               Self.Xref.Child_Types (Entity.Entity, Cursor => Curs);
               Fill_Entity_Array (Curs, Arr);
            end if;
         end;

      else
         declare
            Children : Children_Iterator :=
              Get_Child_Types (Entity.Old_Entity, Recursive => Recursive);
         begin
            while not At_End (Children) loop
               if Get (Children) /= null then
                  Append (Arr, From_Old (Get (Children)));
               end if;
               Next (Children);
            end loop;
            Destroy (Children);
         end;
      end if;

      return R : constant Entity_Array := To_Entity_Array (Arr) do
         Free (Arr);
      end return;
   end Child_Types;

   ------------------
   -- Parent_Types --
   ------------------

   function Parent_Types
      (Self      : access General_Xref_Database_Record;
       Entity    : General_Entity;
       Recursive : Boolean) return Entity_Array
   is
      use Old_Entities;
      Arr : Entity_Arrays.Instance;
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
            Rec  : Recursive_Entities_Cursor;
         begin
            if Recursive then
               Self.Xref.Recursive
                 (Entity  => Entity.Entity,
                  Compute => GNATCOLL.Xref.Parent_Types'Access,
                  Cursor  => Rec);
               Fill_Entity_Array (Rec, Arr);
            else
               Self.Xref.Parent_Types (Entity.Entity, Cursor => Curs);
               Fill_Entity_Array (Curs, Arr);
            end if;
         end;

      else
         declare
            Parents : constant Entity_Information_Array :=
              Get_Parent_Types (Entity.Old_Entity, Recursive);
         begin
            for P in Parents'Range loop
               Append (Arr, From_Old (Parents (P)));
            end loop;
         end;
      end if;

      return R : constant Entity_Array := To_Entity_Array (Arr) do
         Free (Arr);
      end return;
   end Parent_Types;

   ------------
   -- Fields --
   ------------

   function Fields
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity) return Entity_Array
   is
      use Old_Entities;
      Arr : Entity_Arrays.Instance;
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
         begin
            Self.Xref.Fields (Entity.Entity, Cursor => Curs);
            Fill_Entity_Array (Curs, Arr);
         end;

      --  Ignore for enumerations
      elsif Get_Kind (Entity.Old_Entity).Kind /= Enumeration_Kind then
         declare
            Field : Old_Entities.Entity_Information;
            Iter  : Old_Entities.Queries.Calls_Iterator :=
              Get_All_Called_Entities (Entity.Old_Entity);
         begin
            while not At_End (Iter) loop
               Field := Get (Iter);

               --  Hide discriminants and subprograms (would happen in C++,
               --  but these are primitive operations in this case)

               if In_Range (Old_Entities.Get_Declaration_Of (Field),
                            Entity.Old_Entity)
                 and then not Is_Discriminant (Field, Entity.Old_Entity)
                 and then not Old_Entities.Is_Subprogram (Field)
                 and then Get_Category (Field) /= Type_Or_Subtype
               then
                  Append (Arr, From_Old (Field));
               end if;

               Next (Iter);
            end loop;

            Destroy (Iter);
         end;
      end if;

      return R : constant Entity_Array := To_Entity_Array (Arr) do
         Free (Arr);
      end return;
   end Fields;

   -------------
   -- Methods --
   -------------

   function Methods
      (Self              : access General_Xref_Database_Record;
       Entity            : General_Entity;
       Include_Inherited : Boolean) return Entity_Array
   is
      Result : Entity_Arrays.Instance;
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
            Rec_Curs : Recursive_Entities_Cursor;
         begin
            if Include_Inherited then
               Self.Xref.Recursive
                 (Entity  => Entity.Entity,
                  Compute => GNATCOLL.Xref.Methods'Unrestricted_Access,
                  Cursor  => Rec_Curs);
               Fill_Entity_Array (Rec_Curs, Result);
            else
               Self.Xref.Methods (Entity.Entity, Cursor => Curs);
               Fill_Entity_Array (Curs, Result);
            end if;
         end;
      else
         declare
            use Old_Entities;
            Prim : Primitive_Operations_Iterator;
         begin
            Find_All_Primitive_Operations
              (Iter              => Prim,
               Entity            => Entity.Old_Entity,
               Include_Inherited => Include_Inherited);

            while not At_End (Prim) loop
               Append (Result, From_Old (Get (Prim)));
               Next (Prim);
            end loop;

            Destroy (Prim);
         end;
      end if;

      return R : constant Entity_Array := To_Entity_Array (Result) do
         Free (Result);
      end return;
   end Methods;

   --------------------
   -- Component_Type --
   --------------------

   function Component_Type
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Component_Type (Entity.Entity));
      else
         return From_Old (Old_Entities.Queries.Array_Contents_Type
                            (Entity.Old_Entity));
      end if;
   end Component_Type;

   --------------------
   -- Parent_Package --
   --------------------

   function Parent_Package
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Parent_Package (Entity.Entity));
      else
         return From_Old
           (Old_Entities.Queries.Get_Parent_Package (Entity.Old_Entity));
      end if;
   end Parent_Package;

   -----------------
   -- Index_Types --
   -----------------

   function Index_Types
      (Self   : access General_Xref_Database_Record;
       Entity : General_Entity) return Entity_Array
   is
   begin
      if Active (SQLITE) then
         declare
            Curs : Entities_Cursor;
            Arr  : Entity_Arrays.Instance;
         begin
            Self.Xref.Index_Types (Entity.Entity, Cursor => Curs);
            Fill_Entity_Array (Curs, Arr);

            return R : constant Entity_Array := To_Entity_Array (Arr) do
               Free (Arr);
            end return;
         end;
      else
         declare
            Indexes : constant Old_Entities.Entity_Information_Array :=
              Old_Entities.Queries.Array_Index_Types (Entity.Old_Entity);
            Result : Entity_Array (Indexes'Range);
         begin
            for R in Result'Range loop
               Result (R) := From_Old (Indexes (R));
            end loop;
            return Result;
         end;
      end if;
   end Index_Types;

   ---------------
   -- Overrides --
   ---------------

   function Overrides
     (Self   : access General_Xref_Database_Record;
      Entity : General_Entity) return General_Entity
   is
   begin
      if Active (SQLITE) then
         return From_New (Self.Xref.Overrides (Entity.Entity));
      else
         return From_Old (Old_Entities.Queries.Overriden_Entity
                          (Entity.Old_Entity));
      end if;
   end Overrides;

   -------------------------------
   -- Select_Entity_Declaration --
   -------------------------------

   function Select_Entity_Declaration
     (Self   : access General_Xref_Database_Record;
      File   : GNATCOLL.VFS.Virtual_File;
      Entity : General_Entity) return General_Entity
   is
      pragma Unreferenced (Self, File);
   begin
      return Entity;
   end Select_Entity_Declaration;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : access General_Xref_Database_Record) is
   begin
      if not Active (SQLITE) then
         Old_Entities.Reset (Self.Entities);
      end if;
   end Reset;

end Xref;
