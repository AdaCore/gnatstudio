------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with Ada.Strings.Less_Case_Insensitive;
with GNAT.Heap_Sort_G;
with Ada.Strings.Equal_Case_Insensitive;

package body Code_Analysis is

   function Get
     (File_Node : File_Access;
      Key       : String) return Subprogram_Access;
   --  Like other Get subprogram in the specification, but declared here
   --  because it is not used outside of this package body.

   ----------
   -- Less --
   ----------

   function Less (V1, V2 : String) return Boolean is
   begin
      return Ada.Strings.Less_Case_Insensitive (V1, V2);
   end Less;

   function Less (V1, V2 : Virtual_File) return Boolean is
   begin
      return Ada.Strings.Less_Case_Insensitive
        (V1.Display_Base_Name, V2.Display_Base_Name);
   end Less;

   function Less
     (V1, V2 : Projects.Views.Project_View_Reference) return Boolean is
   begin
      return Ada.Strings.Less_Case_Insensitive (V1.Name, V2.Name);
   end Less;

   ---------
   -- Equ --
   ---------

   function Equ  (V1, V2 : Subprogram_Access) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive
        (V1.Name.all, V2.Name.all);
   end Equ;

   function Equ  (V1, V2 : File_Access) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive
        (V1.Name.Display_Base_Name, V2.Name.Display_Base_Name);
   end Equ;

   function Equ  (V1, V2 : Project_Access) return Boolean is
   begin
      return Ada.Strings.Equal_Case_Insensitive (V1.View.Name, V2.View.Name);
   end Equ;

   -------------------------
   -- Clear_Code_Analysis --
   -------------------------

   procedure Clear_Code_Analysis (Projects : Code_Analysis_Tree) is
      use Project_Maps;
      Cur          : Cursor := Projects.First;
      Project_Node : Project_Access;

   begin
      loop
         exit when Cur = No_Element;
         Project_Node := Element (Cur);
         Project_Maps.Delete (Projects.all, Cur);
         Free_Project (Project_Node);
      end loop;
   end Clear_Code_Analysis;

   ----------------------
   -- Sort_Subprograms --
   ----------------------

   procedure Sort_Subprograms (Nodes : in out Subprogram_Array) is

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Instanciation of Heap_Sort_G

      procedure Move (From, To : Natural);
      --  Instanciation of Heap_Sort_G

      Tmp : Subprogram_Access;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean
      is begin
         if Op1 = 0 then
            return Tmp.Name.all < Nodes (Op2).Name.all;
         elsif Op2 = 0 then
            return Nodes (Op1).Name.all < Tmp.Name.all;
         else
            return Nodes (Op1).Name.all < Nodes (Op2).Name.all;
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From, To : Natural) is
      begin
         if From = 0 then
            Nodes (To) := Tmp;
         elsif To = 0 then
            Tmp := Nodes (From);
         else
            Nodes (To) := Nodes (From);
         end if;
      end Move;

      package Sort is new GNAT.Heap_Sort_G
        (Move, Lt);
   begin
      Sort.Sort (Natural (Nodes'Length));
   end Sort_Subprograms;

   ----------------
   -- Sort_Files --
   ----------------

   procedure Sort_Files (Nodes : in out File_Array) is

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Instanciation of Heap_Sort_G

      procedure Move (From, To : Natural);
      --  Instanciation of Heap_Sort_G

      Tmp : File_Access;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean
      is begin
         if Op1 = 0 then
            return Base_Name (Tmp.Name) < Base_Name (Nodes (Op2).Name);
         elsif Op2 = 0 then
            return Base_Name (Nodes (Op1).Name) < Base_Name (Tmp.Name);
         else
            return Base_Name (Nodes (Op1).Name) < Base_Name (Nodes (Op2).Name);
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From, To : Natural) is
      begin
         if From = 0 then
            Nodes (To) := Tmp;
         elsif To = 0 then
            Tmp := Nodes (From);
         else
            Nodes (To) := Nodes (From);
         end if;
      end Move;

      package Sort is new GNAT.Heap_Sort_G
        (Move, Lt);
   begin
      Sort.Sort (Natural (Nodes'Length));
   end Sort_Files;

   -------------------
   -- Sort_Projects --
   -------------------

   procedure Sort_Projects (Nodes : in out Project_Array) is

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Instanciation of Heap_Sort_G

      procedure Move (From, To : Natural);
      --  Instanciation of Heap_Sort_G

      Tmp : Project_Access;

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean
      is begin
         if Op1 = 0 then
            return Tmp.View.Name < Nodes (Op2).View.Name;
         elsif Op2 = 0 then
            return Nodes (Op1).View.Name < Tmp.View.Name;
         else
            return Nodes (Op1).View.Name < Nodes (Op2).View.Name;
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From, To : Natural) is
      begin
         if From = 0 then
            Nodes (To) := Tmp;
         elsif To = 0 then
            Tmp := Nodes (From);
         else
            Nodes (To) := Nodes (From);
         end if;
      end Move;

      package Sort is new GNAT.Heap_Sort_G
        (Move, Lt);
   begin
      Sort.Sort (Natural (Nodes'Length));
   end Sort_Projects;

   ---------
   -- Get --
   ---------

   function Get
     (File_Node : File_Access;
      Key       : String) return Subprogram_Access
   is
      Position : constant Subprogram_Maps.Cursor :=
                   File_Node.Subprograms.Find (Key);

   begin
      if Subprogram_Maps.Has_Element (Position) then
         return Subprogram_Maps.Element (Position);

      else
         return null;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Project_Node : Project_Access;
      File_Name    : GNATCOLL.VFS.Virtual_File) return File_Access
   is
      Position : constant File_Maps.Cursor :=
                   Project_Node.Files.Find (File_Name);

   begin
      if File_Maps.Has_Element (Position) then
         return File_Maps.Element (Position);

      else
         return null;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Projects     : Code_Analysis_Tree;
      Project_View : Standard.Projects.Views.Project_View_Reference)
      return Project_Access
   is
      Position : constant Project_Maps.Cursor := Projects.Find (Project_View);

   begin
      if Project_Maps.Has_Element (Position) then
         return Project_Maps.Element (Position);

      else
         return null;
      end if;
   end Get;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (File_Node : File_Access;
      Key       : String) return not null Subprogram_Access
   is
      Subprogram_Node : Subprogram_Access := Get (File_Node, Key);

   begin
      if Subprogram_Node = null then
         Subprogram_Node := new Subprogram;
         File_Node.Subprograms.Insert (Key, Subprogram_Node);
      end if;

      return Subprogram_Node;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Project_Node : Project_Access;
      File_Name    : GNATCOLL.VFS.Virtual_File) return not null File_Access
   is
      File_Node : File_Access := Get (Project_Node, File_Name);

   begin
      if File_Node = null then
         File_Node := new File;
         File_Node.Name := File_Name;
         Project_Node.Files.Insert (File_Name, File_Node);
      end if;

      return File_Node;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Projects     : Code_Analysis_Tree;
      Project_View : Standard.Projects.Views.Project_View_Reference)
      return not null Project_Access
   is
      Project_Node : Project_Access := Get (Projects, Project_View);

   begin
      if Project_Node = null then
         Project_Node :=
           new Project'(Analysis_Data => <>,
                        View          => Project_View,
                        Files         => <>);
         Projects.Insert (Project_View, Project_Node);
      end if;

      return Project_Node;
   end Get_Or_Create;

   -------------------
   -- Free_Analysis --
   -------------------

   procedure Free_Analysis (Analysis_Id : in out Analysis) is
   begin
      if Analysis_Id.Coverage_Data /= null then
         Unchecked_Free (Analysis_Id.Coverage_Data);
      end if;

      if Analysis_Id.CodePeer_Data /= null then
         Analysis_Id.CodePeer_Data.Finalize;
         Unchecked_Free (Analysis_Id.CodePeer_Data);
      end if;
   end Free_Analysis;

   ---------------
   -- Free_Line --
   ---------------

   procedure Free_Line (Line_Node : in out Line'Class) is
   begin
      if Line_Node.Contents /= null then
         Free (Line_Node.Contents);
      end if;

      Free_Analysis (Line_Node.Analysis_Data);
   end Free_Line;

   ---------------------
   -- Free_Subprogram --
   ---------------------

   procedure Free_Subprogram (Sub_Node : in out Subprogram_Access) is
   begin
      Free_Analysis (Sub_Node.Analysis_Data);
      Unchecked_Free (Sub_Node.Name);
      Unchecked_Free (Sub_Node);
   end Free_Subprogram;

   ---------------
   -- Free_File --
   ---------------

   procedure Free_File (File_Node : in out File_Access) is

      procedure Free_From_Cursor (Cursor : Subprogram_Maps.Cursor);
      --  To be used by Idefinite_Hashed_Maps.Iterate subprogram

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (Cursor : Subprogram_Maps.Cursor) is
         Sub_Node : Subprogram_Access := Subprogram_Maps.Element (Cursor);
      begin
         Free_Subprogram (Sub_Node);
      end Free_From_Cursor;

   begin
      if File_Node.Lines /= null then
         for J in File_Node.Lines'Range loop
            Free_Line (File_Node.Lines (J));
         end loop;

         Unchecked_Free (File_Node.Lines);
      end if;

      File_Node.Subprograms.Iterate (Free_From_Cursor'Access);
      Free_Analysis (File_Node.Analysis_Data);
      Unchecked_Free (File_Node);
   end Free_File;

   ------------------
   -- Free_Project --
   ------------------

   procedure Free_Project (Project_Node : in out Project_Access) is

      procedure Free_From_Cursor (Cursor : File_Maps.Cursor);
      --  To be used by Idefinite_Hashed_Maps.Iterate subprogram

      ----------------------
      -- Free_From_Cursor --
      ----------------------

      procedure Free_From_Cursor (Cursor : File_Maps.Cursor) is
         File_Node : File_Access := File_Maps.Element (Cursor);
      begin
         Free_File (File_Node);
      end Free_From_Cursor;

   begin
      Project_Node.Files.Iterate (Free_From_Cursor'Access);
      Free_Analysis (Project_Node.Analysis_Data);
      Unchecked_Free (Project_Node);
   end Free_Project;

   ------------------------
   -- Free_Code_Analysis --
   ------------------------

   procedure Free_Code_Analysis (Projects : in out Code_Analysis_Tree) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Maps.Map, Code_Analysis_Tree);
   begin
      Clear_Code_Analysis (Projects);
      Unchecked_Free (Projects);
   end Free_Code_Analysis;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Self : File_Coverage) return Boolean is
   begin
      return Self.Status = Valid;
   end Is_Valid;

   overriding function Is_Valid (Self : Project_Coverage) return Boolean is
   begin
      return Self.Status = Valid;
   end Is_Valid;

   overriding function Is_Valid (Self : Subprogram_Coverage) return Boolean is
   begin
      return Self.Status = Valid;
   end Is_Valid;

end Code_Analysis;
