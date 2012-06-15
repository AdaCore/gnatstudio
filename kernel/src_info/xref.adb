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

with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Entities.Queries;          use Entities.Queries;
with Glib.Convert;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNAT.Strings;              use GNAT.Strings;
with Language_Handlers;         use Language_Handlers;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;

package body Xref is

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

   procedure Node_From_Entity
     (Self        : General_Xref_Database;
      Handler     : Language_Handlers.Language_Handler;
      Decl        : General_Location;
      Ent         : out Entity_Access;
      Tree_Lang   : out Tree_Language_Access);
   --  Returns the constructs data for a given entity.

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
      Source : Entities.Source_File;
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
         Source := Entities.Get_Or_Create
           (Db           => Db.Entities,
            File         => Loc.File,
            Allow_Create => False);
         if Source = null then
            return No_General_Entity;
         else
            return General_Entity'
              (Old_Entity =>
                 Entities.Get_Or_Create
                   (Name  => Entities.Get_Symbols (Db.Entities).Find (Name),
                    File         => Source,
                    Line         => Loc.Line,
                    Column       => Loc.Column,
                    Allow_Create => False),
               others => <>);
         end if;
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
         return (File => Ref.Ref.File,
                 Line => Ref.Ref.Line,
                 Column => Visible_Column_Type (Ref.Ref.Column));
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

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : General_Entity) is
   begin
      Entities.Ref (Entity.Old_Entity);
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out General_Entity) is
   begin
      Entities.Unref (Entity.Old_Entity);
   end Unref;

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
                (Buffer     => Buffer.all,
                 Decl_Start => Get_Construct (Node).Sloc_Start.Index,
                 Decl_End   => Get_Construct (Node).Sloc_End.Index,
                 Language   => Context.Syntax,
                 Format     => Form);
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
      begin
         if Entity.Entity /= No_Entity then
            return GNATCOLL.Xref.Documentation
              (Self.Xref.all,
               Entity.Entity,
               Context.Syntax,
               Format => Form);
         else
            Buffer := Decl.File.Read_File;
            return Result : constant String := Extract_Comment
              (Buffer     => Buffer.all,
               Decl_Start => Decl.Line,
               Decl_End   => Decl.Line,
               Language   => Context.Syntax,
               Format     => Form)
            do
               Free (Buffer);
            end return;
         end if;
      end Doc_From_LI;

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

end Xref;
