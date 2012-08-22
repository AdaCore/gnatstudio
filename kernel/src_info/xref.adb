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
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Glib.Convert;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNAT.Strings;              use GNAT.Strings;
with Language_Handlers;         use Language_Handlers;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with Traces;

package body Xref is
   Me : constant Trace_Handle := Create ("Xref", Off);

   ---------------------------
   --  Note for development --
   ---------------------------

   --  A lot of functions defined here are first attempting to use the
   --  new system (GNATCOLL.Xref) and fallback on the legacy database
   --  (Entities.*).
   --
   --  The plan is to transition all user calls to Entities made in GPS
   --  to use this API, and then to get rid of the legacy database.

   use type Entities.Entity_Information;
   use type Entities.File_Location;

   function Get_Location
     (Ref : Entity_Reference) return General_Location;
   --  Return the General Location of a GNATCOLL reference

   function Is_Type_Declaration
     (Db : General_Xref_Database;
      E  : Entity_Information) return Boolean;
   --  True if E is a type declaration

   procedure Node_From_Entity
     (Self        : General_Xref_Database;
      Handler     : Language_Handlers.Language_Handler;
      Decl        : General_Location;
      Ent         : out Entity_Access;
      Tree_Lang   : out Tree_Language_Access);
   --  Returns the constructs data for a given entity.

   function To_File_Location
     (Db  : General_Xref_Database;
      Loc : General_Location) return Entities.File_Location;
   --  Convert General_Location to File_Location

   function To_General_Entity
     (Db : General_Xref_Database;
      E  : Entity_Information) return General_Entity;
   --  Convert Xref.Entity_Information to General_Entity

   function To_General_Location
     (Loc : Entities.File_Location) return General_Location;
   --  Convert File_Location to General_Location

   function To_String (Location : General_Location) return String;
   --  For debugging purposes only

   -------------------
   -- Documentation --
   -------------------

   function Documentation
     (Self             : General_Xref_Database;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : General_Entity;
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String
   is
      function Doc_From_Constructs return String;
      function Doc_From_LI return String;

      Decl : constant General_Location := Get_Declaration (Self, Entity);
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
         Loc    : Entities.File_Location;
         Result : Unbounded_String;
      begin
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

               if Loc /= Entities.No_File_Location then
                  Free (Buffer);
                  Buffer := Entities.Get_Filename (Loc.File).Read_File;
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

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Dbase                : General_Xref_Database;
      Entity               : General_Entity;
      Current_Location     : General_Location := No_Location;
      Location             : out General_Location;
      No_Location_If_First : Boolean := False)
   is
   begin
      --  Legacy functionality

      if not Active (Entities.SQLITE) then
         declare
            Loc : Entities.File_Location;
         begin
            Entities.Queries.Find_Next_Body
              (Entity               => Entity.Old_Entity,
               Current_Location     => To_File_Location (Dbase,
                                         Current_Location),
               Location             => Loc,
               No_Location_If_First => No_Location_If_First);
            Location := To_General_Location (Loc);
            return;
         end;
      end if;

      --  New functionality

      if Entity = No_General_Entity then
         Location := No_Location;
         return;
      end if;

      if Active (Me) then
         Trace (Me, "Find_Next_Body for "
                & Get_Name (Dbase, Entity)
                & " current=" & To_String (Current_Location));
      end if;

      declare
         Cursor        : References_Cursor;
         Return_Next   : Boolean := Current_Location = No_Location;
         First_Ref_Loc : General_Location := No_Location;

      begin
         References
           (Self    => Dbase.Xref.all,
            Entity  => Entity.Entity,
            Cursor  => Cursor);

         while Cursor.Has_Element loop
            declare
               Ref  : constant Entity_Reference := Cursor.Element;
               Kind : constant String := To_String (Ref.Kind);
               Loc  : constant General_Location := Get_Location (Ref);

            begin
               if First_Ref_Loc = No_Location then
                  First_Ref_Loc := Loc;
               end if;

               if Kind = "body"
                 or else Kind = "full declaration"
               then
                  --  Missing support for Is_Imported???

                  if Return_Next then
                     Location := Loc;
                     return;
                  end if;
               end if;

               if Loc = Current_Location then
                  Return_Next := True;
               end if;
            end;

            Cursor.Next;
         end loop;

         --  If we don't have any more information to extract from the
         --  construct database, then return the first entity if allowed by
         --  the flags, or null.

         if No_Location_If_First then
            Location := No_Location;
         else
            Location := First_Ref_Loc;
         end if;
      end;
   end Find_Next_Body;

   -------------------------------
   -- For_Each_Dispatching_Call --
   -------------------------------

   procedure For_Each_Dispatching_Call
     (Dbase     : General_Xref_Database;
      Entity    : General_Entity;
      Ref       : General_Entity_Reference;
      On_Callee : access function
                    (Callee, Primitive_Of : General_Entity) return Boolean;
      Filter    : Entities.Reference_Kind_Filter := Entity_Has_Declaration;
      Policy    : Dispatching_Menu_Policy)

   is
      use type Entities.Reference_Kind;

      Prim_Ent  : General_Entity;
      Typ_Ent   : General_Entity;

   begin
      --  Handle cases in which no action is needed

      if Entity = No_General_Entity
        or else Policy = Never
      then
         return;

      elsif not Active (Entities.SQLITE)
        and then Entities.Get_Kind (Ref.Old_Ref)
                   /= Entities.Dispatching_Call
      then
         return;

      elsif Active (Entities.SQLITE)
        and then Ref.Ref.Kind /= "dispatching call"
      then
         return;
      end if;

      --  New functionality

      if Active (Entities.SQLITE) then
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

                  exit when not On_Callee
                    (Callee => Prim_Ent,
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
            Iter : Entities.Queries.Entity_Reference_Iterator;
            Db   : Entities.Entities_Database;

         begin
            if Policy = From_Memory then
               Db :=
                 Entities.Get_Database
                   (Entities.Get_File
                        (Entities.Get_Declaration_Of (Entity.Old_Entity)));
               Entities.Freeze (Db);
            end if;

            Entities.Queries.Find_All_References
              (Iter                  => Iter,
               Entity                => Entity.Old_Entity,
               File_Has_No_LI_Report => null,
               In_File               => null,
               Filter                => Filter,
               Include_Overriding    => True,
               Include_Overridden    => False);

            declare
               E, Typ : Entities.Entity_Information;

            begin
               while not At_End (Iter) loop
                  E := Get_Entity (Iter);

                  if E /= null then
                     Typ := Entities.Is_Primitive_Operation_Of (E);

                     if Typ /= null then
                        Prim_Ent := To_General_Entity (E);
                        Typ_Ent  := To_General_Entity (Typ);

                        exit when not On_Callee
                          (Callee => Prim_Ent,
                           Primitive_Of => Typ_Ent);
                     end if;
                  end if;

                  Next (Iter);
               end loop;
            end;

            Destroy (Iter);

            if Policy = From_Memory then
               Entities.Thaw (Db);
            end if;

         exception
            when E : others =>
               Trace (Traces.Exception_Handle, "Unexpected exception: "
                      & Exception_Information (E));
               if Policy = From_Memory then
                  Entities.Thaw (Db);
               end if;
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

      if Active (Entities.SQLITE)
        and then Ref.Ref /= No_Entity_Reference
      then
         E.Entity := Ref.Ref.Entity;
      end if;

      --  Fall back on the old system

      E.Old_Entity := Entities.Get_Entity (Ref.Old_Ref);

      return E;
   end Get_Entity;

   ----------------
   -- Get_Entity --
   ----------------

   function Get_Entity
     (Db   : General_Xref_Database;
      Name : String;
      Loc  : General_Location) return General_Entity
   is
      use Entities;
      Old_Entity : Entities.Entity_Information;
      Status     : Find_Decl_Or_Body_Query_Status;
   begin
      if Active (Entities.SQLITE) then
         return General_Entity'
           (Entity => Db.Xref.Get_Entity
              (Name => Name,
               File => Loc.File,
               Line => Loc.Line,
               Column => Visible_Column (Loc.Column)).Entity,
            others => <>);
      else
         Find_Declaration
           (Db          => Db.Entities,
            File_Name   => Loc.File,
            Entity_Name => Name,
            Line        => Loc.Line,
            Column      => Loc.Column,
            Entity      => Old_Entity,
            Status      => Status);

         return To_General_Entity (Old_Entity);
      end if;
   end Get_Entity;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Db     : General_Xref_Database;
      Entity : General_Entity) return String is
   begin
      if Active (Entities.SQLITE)
        and then Entity.Entity /= No_Entity
      then
         return To_String
           (Declaration (Db.Xref.all, Entity.Entity).Name);
      end if;

      if Entity.Old_Entity /= null then
         return Get (Entities.Get_Name (Entity.Old_Entity)).all;
      end if;

      return "";
   end Get_Name;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (Ref : General_Entity_Reference) return General_Location is
   begin
      if Active (Entities.SQLITE)
        and then Ref.Ref /= No_Entity_Reference
      then
         return Get_Location (Ref.Ref);
      end if;

      declare
         Loc : constant Entities.File_Location :=
           Entities.Get_Location (Ref.Old_Ref);
      begin
         return (File => Entities.Get_Filename (Loc.File),
                 Line => Loc.Line,
                 Column => Loc.Column);
      end;
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

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location is
   begin
      if Active (Entities.SQLITE)
        and then Entity.Entity /= No_Entity
      then
         declare
            Ref : constant Entity_Reference :=
              Db.Xref.Declaration (Entity.Entity).Location;
         begin
            if Ref /= No_Entity_Reference then
               return (File   => Ref.File,
                       Line   => Ref.Line,
                       Column => Visible_Column_Type (Ref.Column));
            end if;
         end;
      end if;

      if Entity.Old_Entity /= null then
         declare
            Loc : constant Entities.File_Location :=
              Entities.Get_Declaration_Of (Entity.Old_Entity);
         begin
            return (File   => Entities.Get_Filename (Loc.File),
                    Line   => Loc.Line,
                    Column => Loc.Column);
         end;
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
            return (File   => Get_File_Path (Get_File (Decl)),
                    Line   => Get_Construct (Node).Sloc_Start.Line,
                    Column => Visible_Column_Type
                      (Get_Construct (Node).Sloc_Start.Column));
         end;
      end if;

      return No_Location;
   end Get_Declaration;

   --------------
   -- Get_Body --
   --------------

   function Get_Body
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Location is
   begin
      if Active (Entities.SQLITE)
        and then Entity.Entity /= No_Entity
      then
         declare
            C   : References_Cursor;
            Ref : Entity_Reference;
         begin
            Bodies (Db.Xref.all, Entity.Entity, Cursor => C);
            if Has_Element (C) then
               Ref := Element (C);

               if Ref /= No_Entity_Reference then
                  return (File => Ref.File,
                          Line => Ref.Line,
                          Column => Visible_Column_Type (Ref.Column));
               end if;
            end if;
         end;
      end if;

      if Entity.Old_Entity /= null then
         declare
            Loc : Entities.File_Location;
         begin
            Find_Next_Body
              (Entity           => Entity.Old_Entity,
               Current_Location => Entities.No_File_Location,
               Location         => Loc);

            if Loc = Entities.No_File_Location then
               Loc := Entities.Get_Declaration_Of (Entity.Old_Entity);
            end if;

            if Loc /= Entities.No_File_Location then
               return (File => Entities.Get_Filename (Loc.File),
                       Line => Loc.Line,
                       Column => Loc.Column);
            end if;
         end;
      end if;

      return No_Location;
   end Get_Body;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Db     : General_Xref_Database;
      Entity : General_Entity) return Entities.Entity_Category
   is
      K : constant Entities.E_Kind := Get_Kind (Db, Entity);
      use Entities;

   begin
      case K.Kind is
         when Overloaded_Entity | Unresolved_Entity | Macro =>
            return Unknown;

         when Procedure_Kind | Function_Or_Operator |
              Entry_Or_Entry_Family | Function_Macro =>
            return Subprogram;

         when Package_Kind =>
            return Package_Or_Namespace;

         when Label_On_Block | Label_On_Loop | Label_On_Statement =>
            return Label;

         when Enumeration_Literal | Named_Number =>
            return Literal;

         when others =>
            if K.Is_Type then
               return Type_Or_Subtype;
            else
               return Object;
            end if;
      end case;
   end Get_Category;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind
     (Db     : General_Xref_Database;
      Entity : General_Entity) return Entities.E_Kind
   is
   begin
      if not Active (Entities.SQLITE) then
         return Entities.Get_Kind (Entity.Old_Entity);
      else
         declare
            use Entities;

            Decl   : constant Entity_Declaration :=
                       Declaration (Db.Xref.all, Entity.Entity);
            Kind   : constant String := To_String (Decl.Kind);
            Last   : Natural := Kind'Last;
            Result : Entities.E_Kind :=
                       (Kind => Unresolved_Entity,
                        Is_Generic  => False,
                        Is_Type     => False,
                        Is_Abstract => False);
         begin
            if Is_Type_Declaration (Db, Entity.Entity) then
               Result.Is_Type := True;
               Last := Last - 5;
            end if;

            --  Source: gnatlib/src/generated/gnatcoll-xref-database.adb

            declare
               K : constant String := Kind (Kind'First .. Last);

            begin
               if K = "pointer"
                 or else K = "access"
                 or else K = "access parameter"
               then
                  Result.Kind := Access_Kind;

               elsif K = "array" then
                  Result.Kind := Array_Kind;

               elsif K = "boolean" then
                  Result.Kind := Boolean_Kind;

               elsif K = "class wide" then
                  Result.Kind := Class_Wide;

               elsif K = "class"
                 or else K = "class instance"
               then
                  Result.Kind := Class;

               elsif K = "decimal fixed point" then
                  Result.Kind := Decimal_Fixed_Point;

               elsif K = "entry" then
                  Result.Kind := Entry_Or_Entry_Family;

               elsif K = "enumeration literal" then
                  Result.Kind := Enumeration_Literal;

               elsif K = "enumeration" then
                  Result.Kind := Enumeration_Kind;

               elsif K = "exception" then
                  Result.Kind := Exception_Entity;

               elsif K = "floating point" then
                  Result.Kind := Floating_Point;

               elsif K = "function macro" then
                  Result.Kind := Function_Macro;

               elsif K = "include file" then
                  Result.Kind := Include_File;

               elsif K = "interface" then
                  Result.Kind := Interface_Kind;

               elsif K = "statement label" then
                  Result.Kind := Label_On_Statement;

               elsif K = "block label" then
                  Result.Kind := Label_On_Block;

               elsif K = "loop label" then
                  Result.Kind := Label_On_Loop;

               elsif K = "macro" then
                  Result.Kind := Macro;

               --  Modular_Integer missing???

               elsif K = "named number" then
                  Result.Kind := Named_Number;

               elsif K = "function" then
                  Result.Kind := Function_Or_Operator;

               elsif K = "abstract function" then
                  Result.Kind := Function_Or_Operator;
                  Result.Is_Abstract := True;

               elsif K = "generic function" then
                  Result.Kind := Function_Or_Operator;
                  Result.Is_Generic := True;

               elsif K = "generic package" then
                  Result.Kind := Package_Kind;
                  Result.Is_Generic := True;

               elsif K = "package" then
                  Result.Kind := Package_Kind;

               elsif K = "parent package" then
                  Result.Kind := Package_Kind;

               elsif K = "procedure" then
                  Result.Kind := Procedure_Kind;

               elsif K = "abstract procedure" then
                  Result.Kind := Procedure_Kind;
                  Result.Is_Abstract := True;

               elsif K = "generic procedure" then
                  Result.Kind := Procedure_Kind;
                  Result.Is_Generic := True;

               elsif K = "fixed point" then
                  Result.Kind := Ordinary_Fixed_Point;

               --  Private_Type missing???

               elsif K = "private object" then
                  Result.Kind := Private_Object;

               elsif K = "protected object"
                 or else K = "protected"
               then
                  Result.Kind := Protected_Kind;

               --  Reference,    --  C++'s & operator (untested???)

               elsif K = "record" then
                  Result.Kind := Record_Kind;
                  --  Is_Abstract (not available in record types???)

               elsif K = "integer"
                 or else K = "short_integer"
                 or else K = "short_short_integer"
                 or else K = "long_integer"
                 or else K = "long_long_integer"
               then
                  Result.Kind := Signed_Integer;

               elsif K = "string" then
                  Result.Kind := String_Kind;

               elsif K = "task" then
                  Result.Kind := Task_Kind;

               --  Union missing???

               else
                  if K = "generic formal"
                    or else K = "formal generic parameter"
                  then
                     Result.Is_Generic := True;

                  elsif K = "abstract" then
                     Result.Is_Abstract := True;

                  --  Unsupported???
                  else
                     raise Program_Error;
                  end if;
               end if;

               return Result;
            exception
               when E : others =>
                  Trace (Traces.Exception_Handle, "Unexpected exception: "
                         & Exception_Information (E));
                  return Result;
            end;
         end;
      end if;
   end Get_Kind;

   -----------------
   -- Get_Type_Of --
   -----------------

   function Get_Type_Of
     (Db     : General_Xref_Database;
      Entity : General_Entity) return General_Entity
   is
   begin
      if not Active (Entities.SQLITE) then
         declare
            E : constant Entities.Entity_Information :=
                  Entities.Get_Type_Of (Entity.Old_Entity);
         begin
            return To_General_Entity (E);
         end;

      else
         return To_General_Entity (Db, Type_Of (Db.Xref.all, Entity.Entity));
      end if;
   end Get_Type_Of;

   --------------------------
   -- Is_Predefined_Entity --
   --------------------------

   function Is_Predefined_Entity
     (Db : General_Xref_Database;
      E  : General_Entity) return Boolean is
   begin
      if not Active (Entities.SQLITE) then
         return Entities.Is_Predefined_Entity (E.Old_Entity);
      else
         return Is_Predefined_Entity (Declaration (Db.Xref.all, E.Entity));
      end if;
   end Is_Predefined_Entity;

   -------------------------
   -- Is_Type_Declaration --
   -------------------------

   function Is_Type_Declaration
     (Db : General_Xref_Database;
      E  : Entity_Information) return Boolean
   is
      Decl : constant Entity_Declaration := Declaration (Db.Xref.all, E);
      Kind : constant String := To_String (Decl.Kind);

   begin
      return Kind'Length > 4
         and then Kind (Kind'Last - 4 .. Kind'Last) = " type";
   end Is_Type_Declaration;

   ----------------------
   -- Node_From_Entity --
   ----------------------

   procedure Node_From_Entity
     (Self        : General_Xref_Database;
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
     (Dbase  : General_Xref_Database;
      Entity : General_Entity) return General_Entity is
   begin
      if not Active (Entities.SQLITE) then
         return To_General_Entity (Pointed_Type (Entity.Old_Entity));
      else
         return Pointed_Type (Dbase, Entity);
      end if;
   end Pointed_Type;

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : General_Entity) is
   begin
      Entities.Ref (Entity.Old_Entity);
   end Ref;

   ----------------------
   -- To_File_Location --
   ----------------------

   function To_File_Location
     (Db  : General_Xref_Database;
      Loc : General_Location) return Entities.File_Location
   is
   begin
      if Loc = No_Location then
         return Entities.No_File_Location;
      else
         return
           (File   => Entities.Get_Or_Create (Db.Entities, Loc.File),
            Line   => Loc.Line,
            Column => Loc.Column);
      end if;
   end To_File_Location;

   -----------------------
   -- To_General_Entity --
   -----------------------

   function To_General_Entity
     (Db : General_Xref_Database;
      E  : Entity_Information) return General_Entity
   is
      Decl : constant Entity_Declaration := Declaration (Db.Xref.all, E);
      Loc  : General_Location;

   begin
      pragma Assert (Active (Entities.SQLITE));

      Loc :=
        (File   => Decl.Location.File,
         Line   => Decl.Location.Line,
         Column => Visible_Column_Type (Decl.Location.Column));

      return
        Get_Entity
          (Db   => Db,
           Name => To_String (Decl.Name),
           Loc  => Loc);
   end To_General_Entity;

   -----------------------
   -- To_General_Entity --
   -----------------------

   function To_General_Entity
     (E  : Entities.Entity_Information) return General_Entity is
   begin
      pragma Assert (not Active (Entities.SQLITE));

      if E = null then
         return No_General_Entity;
      else
         return General_Entity'(Old_Entity => E, others => <>);
      end if;
   end To_General_Entity;

   -------------------------
   -- To_General_Location --
   -------------------------

   function To_General_Location
     (Loc : Entities.File_Location) return General_Location is
   begin
      return
        (File   => Entities.Get_Filename (Loc.File),
         Line   => Loc.Line,
         Column => Loc.Column);
   end To_General_Location;

   ---------------
   -- To_String --
   ---------------

   function To_String (Location : General_Location) return String is
   begin
      if Location = No_Location then
         return "<no loc>";
      else
         return (Display_Full_Name (Location.File))
           & ':' & Location.Line'Img
           & ':' & Location.Column'Img;
      end if;
   end To_String;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out General_Entity) is
   begin
      Entities.Unref (Entity.Old_Entity);
   end Unref;

end Xref;
