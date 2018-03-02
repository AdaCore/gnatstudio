------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;

with GNATCOLL.SQL.Sessions;
with GNATCOLL.Symbols;
with GNATCOLL.SQL.Sqlite;

with Basic_Types;
with Commands;

with GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;

with Database.Orm;
with GNAThub.Messages;
with Language.Abstract_Language_Tree;

package body GNAThub.Loader is

   Empty_Severity_Id : constant := 0;
   --  Code for unspecified (NULL) severity

   procedure Cleanup (Self : in out Loader'Class);
   --  Stop loading (when necessary) and cleanup resources.

   procedure Load_Severities (Self : in out Loader'Class);
   --  Loads list of severities from the database

   procedure Load_Tools_And_Rules (Self : in out Loader'Class);
   --  Loads list of tools and their rules.

   procedure Load_Resources (Self : in out Loader'Class);
   --  Loads list of resources.

   procedure Load_Messages
     (Self          : in out Loader'Class;
      Resource_Id   : Natural;
      Resource_Name : String);
   --  Loads messages for resource.

   type Loader_Command
     (Loader : not null access GNAThub.Loader.Loader'Class) is
     new Commands.Root_Command with null record;
   overriding function Execute
     (Self : access Loader_Command) return Commands.Command_Return_Type;
   --  Loads next portion of data.

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Loader'Class) is

      use type GPS.Scripts.Commands.Scheduled_Command_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation (Resource_Record, Resource_Access);

      Resource : Resource_Access;

   begin
      if Self.Command /= null then
         GPS.Kernel.Task_Manager.Interrupt_Queue
           (Self.Module.Get_Kernel, Self.Command);
         Self.Command := null;
      end if;

      while not Self.Resources.Is_Empty loop
         Resource := Self.Resources.First_Element;
         Self.Resources.Delete_First;
         Free (Resource);
      end loop;

      Self.Severities.Clear;
      Self.Rules.Clear;
      GNATCOLL.Projects.Free (Self.Source_Files);

      GNATCOLL.SQL.Sessions.Free;
   end Cleanup;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Loader_Command) return Commands.Command_Return_Type is
   begin
      Self.Loader.Load_Messages
        (Resource_Maps.Key (Self.Loader.Current),
         To_String (Resource_Maps.Element (Self.Loader.Current).Name));

      Resource_Maps.Next (Self.Loader.Current);

      if Resource_Maps.Has_Element (Self.Loader.Current) then
         return Commands.Execute_Again;

      else
         GNATCOLL.Projects.Free (Self.Loader.Source_Files);
         Self.Loader.Command := null;
         Self.Loader.Cleanup;

         return Commands.Success;
      end if;

   exception
      when others =>
         Self.Loader.Command := null;
         Self.Loader.Cleanup;

         return Commands.Failure;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Loader'Class) is
   begin
      null;
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load
     (Self     : in out Loader'Class;
      Database : GNATCOLL.VFS.Virtual_File)
   is
      Aux : Commands.Command_Access;
      --  New command is assigned to this variable to create object with
      --  necessary accessibility level, otherwise acccessibility check will
      --  fail inside Launch_Background_Command subprogram.

      Id  : Natural := 1;

   begin
      Self.Cleanup;

      GNATCOLL.SQL.Sessions.Setup
        (Descr        =>
           GNATCOLL.SQL.Sqlite.Setup (String (Database.Full_Name.all)),
         Max_Sessions => 2);
      Self.Load_Severities;
      Self.Load_Tools_And_Rules;
      Self.Load_Resources;

      if not Self.Resources.Is_Empty then
         Self.Source_Files := GNATCOLL.Projects.Source_Files
           (GPS.Kernel.Project.Get_Project
              (Self.Module.Get_Kernel), True, True);

         for Item of Self.Module.Severities loop
            Self.Module.Severities_Id.Include (Item, Id);
            Id := Id + 1;
         end loop;

         Aux := new Loader_Command (Self'Unchecked_Access);
         Self.Current := Self.Resources.First;
         Self.Command :=
           GPS.Kernel.Task_Manager.Launch_Background_Command
             (Kernel   => Self.Module.Get_Kernel,
              Command  => Aux,
              Active   => True,
              Show_Bar => False);

      elsif Self.Resources.Is_Empty then
         Self.Cleanup;
      end if;
   end Load;

   -------------------
   -- Load_Messages --
   -------------------

   procedure Load_Messages
     (Self          : in out Loader'Class;
      Resource_Id   : Natural;
      Resource_Name : String)
   is
      use GNATCOLL.Projects;
      use type GNATCOLL.VFS.Virtual_File;

      File            : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Create_From_UTF8 (Resource_Name);
      Session         : constant GNATCOLL.SQL.Sessions.Session_Type :=
                         GNATCOLL.SQL.Sessions.Get_New_Session;
      List            : Database.Orm.Resource_Message_List :=
                         Database.Orm.Filter
                          (Database.Orm.All_Resources_Messages,
                           Resource_Id => Resource_Id).Get (Session);
      R               : Database.Orm.Resource_Message;
      M               : Database.Orm.Message;
      Message         : GNAThub.Messages.Message_Access;
      Visible         : Boolean;
      Rule            : GNAThub.Rule_Access;
      Severity        : GNAThub.Severity_Access;
      Severity_Id     : Positive;
      Position        : GNAThub.Severity_Natural_Maps.Cursor;

      Project         : GNATCOLL.Projects.Project_Type;
      Tree_Project    : GNAThub_Project_Access;
      Tree_File       : GNAThub_File_Access;
      Tree_Subprogram : GNAThub_Subprogram_Access;

      function Insert
        (Project : GNATCOLL.Projects.Project_Type)
        return GNAThub_Project_Access;

      function Insert
        (Project : GNAThub_Project_Access;
         File    : GNATCOLL.VFS.Virtual_File)
         return GNAThub_File_Access;

      function Find_Subprogram return GNAThub_Subprogram_Access;

      ---------------------
      -- Find_Subprogram --
      ---------------------

      function Find_Subprogram return GNAThub_Subprogram_Access
      is
         use Language.Abstract_Language_Tree;
         use GNAT.Strings;

         Tree   : constant Semantic_Tree'Class :=
           Self.Module.Get_Kernel.Get_Abstract_Tree_For_File ("GNATHUB", File);
         Result : GNAThub_Subprogram_Access := null;

      begin
         if Tree = No_Semantic_Tree then
            return Result;
         end if;

         declare
            Iter : Semantic_Tree_Iterator'Class := Root_Iterator (Tree);
         begin
            while Has_Element (Iter) loop
               declare
                  Node : Semantic_Node'Class := Element (Iter);
               begin
                  if Node.Is_Valid
                    and then Node.Category in
                      Language.Cat_Procedure .. Language.Cat_Destructor
                    and then Node.Sloc_Start.Line <= R.Line
                    and then Node.Sloc_End.Line >= R.Line
                    and then
                      (Result = null
                       or else Result.Line < Node.Sloc_Start.Line)
                  then
                     if Result = null then
                        Result := new GNAThub_Subprogram'
                          (Analysis_Data => (null, null),
                           Name          => null,
                           Counts_Size   => Natural
                             (Self.Module.Severities.Length) + 1,
                           Counts        => (others => 0),
                           Messages      =>
                             Messages_Vectors.Empty_Vector,
                           others => 0);
                     end if;

                     if Result.Name /= null then
                        Free (Result.Name);
                     end if;

                     Result.Name := new String'
                       (GNATCOLL.Symbols.Get (Node.Name).all);
                     Result.Line   := Node.Sloc_Start.Line;
                     Result.Column := Natural (Node.Sloc_Start.Column);
                  end if;
               end;
               Next (Iter);
            end loop;
         end;

         return Result;
      end Find_Subprogram;

      ------------
      -- Insert --
      ------------

      function Insert
        (Project : GNATCOLL.Projects.Project_Type)
         return GNAThub_Project_Access
      is
         use Code_Analysis.Project_Maps;

         Cursor : Code_Analysis.Project_Maps.Cursor;
         Result : GNAThub_Project_Access;
      begin
         Cursor := Self.Module.Tree.Find (Project);
         if Has_Element (Cursor) then
            Result := GNAThub_Project_Access (Element (Cursor));
         else
            Result := new GNAThub_Project'
              (Analysis_Data => (null, null),
               Name          => Project,
               Files         => Code_Analysis.File_Maps.Empty_Map,
               Counts_Size   => Natural (Self.Module.Severities.Length) + 1,
               Counts        => (others => 0));
            Self.Module.Tree.Include
              (Project, Code_Analysis.Project_Access (Result));
         end if;

         if Visible then
            Result.Counts (Severity_Id) := Result.Counts (Severity_Id) + 1;
            Result.Counts (Result.Counts'Last) :=
              Result.Counts (Result.Counts'Last) + 1;
         end if;

         return Result;
      end Insert;

      ------------
      -- Insert --
      ------------

      function Insert
        (Project : GNAThub_Project_Access;
         File    : GNATCOLL.VFS.Virtual_File)
         return GNAThub_File_Access
      is
         use Code_Analysis.File_Maps;

         Cursor : Code_Analysis.File_Maps.Cursor;
         Result : GNAThub_File_Access;
      begin
         Cursor := Project.Files.Find (File);
         if Has_Element (Cursor) then
            Result := GNAThub_File_Access (Element (Cursor));
         else
            Result := new GNAThub_File'
              (Analysis_Data => (null, null),
               Name          => File,
               Subprograms   => Code_Analysis.Subprogram_Maps.Empty_Map,
               Lines         => null,
               Line_Commands => Commands.Command_Lists.Empty_List,
               Counts_Size   => Natural (Self.Module.Severities.Length) + 1,
               Counts        => (others => 0),
               Messages      => Messages_Vectors.Empty_Vector);
            Project.Files.Include (File, Code_Analysis.File_Access (Result));
         end if;

         if Visible then
            Result.Counts (Severity_Id) := Result.Counts (Severity_Id) + 1;
            Result.Counts (Result.Counts'Last) :=
              Result.Counts (Result.Counts'Last) + 1;
         end if;

         return Result;
      end Insert;

      Filter_Result : GPS.Kernel.Messages.Filter_Result;
   begin
      while List.Has_Row loop
         R := List.Element;
         M := Database.Orm.Filter
           (Database.Orm.All_Messages, Id => R.Message_Id)
           .Get (Session).Element;

         if Self.Rules.Contains (M.Rule_Id) then
            --  This is message

            --  Use Empty_Severity_Id if M.Category_Id is NULL
            Severity := Self.Severities
              (Natural'Max (Empty_Severity_Id, M.Category_Id));
            Rule     := Self.Rules (M.Rule_Id);
            Position := Rule.Count.Find (Severity);

            if Severity_Natural_Maps.Has_Element (Position) then
               Rule.Count.Replace_Element
                 (Position, Severity_Natural_Maps.Element (Position) + 1);

            else
               Rule.Count.Insert (Severity, 1);
            end if;

            Message := new GNAThub.Messages.Message;
            GNAThub.Messages.Initialize
              (Self      => Message,
               Container =>
                 Self.Module.Get_Kernel.Get_Messages_Container,
               Severity  => Severity,
               Rule      => Rule,
               Text      => To_Unbounded_String (Database.Orm.Data (M)),
               File      => File,
               Line      => R.Line,
               Column    => Basic_Types.Visible_Column_Type (R.Col_Begin));

            --  Update module's tree

            Severity_Id := Self.Module.Severities_Id.Element (Severity);

            Filter_Result := Self.Module.Filter.Apply (Message.all);
            Visible := not Filter_Result.Non_Applicable
              and then Filter_Result.Flags (GPS.Kernel.Messages.Locations);

            for Index in Self.Source_Files'Range loop
               if Self.Source_Files (Index).File = File then
                  Project := Self.Source_Files (Index).Project;
                  exit;
               end if;
            end loop;

            Tree_Project := Insert (Project);
            Tree_File    := Insert (Tree_Project, File);

            if Project /= No_Project then
               Tree_Subprogram := Find_Subprogram;
            else
               Tree_Subprogram := null;
            end if;

            if Tree_Subprogram /= null then
               declare
                  use Subprogram_Maps;
                  Cursor : constant Subprogram_Maps.Cursor :=
                    Tree_File.Subprograms.Find
                      (Tree_Subprogram.Name.all);

                  procedure Unchecked_Free is new
                    Ada.Unchecked_Deallocation
                      (GNAThub_Subprogram, GNAThub_Subprogram_Access);

               begin
                  if Has_Element (Cursor) then
                     GNAT.Strings.Free (Tree_Subprogram.Name);
                     Unchecked_Free (Tree_Subprogram);
                     Tree_Subprogram := GNAThub_Subprogram_Access
                       (Subprogram_Maps.Element (Cursor));
                  else
                     Tree_File.Subprograms.Include
                       (Tree_Subprogram.Name.all,
                        Subprogram_Access (Tree_Subprogram));
                  end if;
               end;

               Tree_Subprogram.Messages.Append
                 (GPS.Kernel.Messages.References.Create
                    (GPS.Kernel.Messages.Message_Access (Message)));

               if Visible then
                  Tree_Subprogram.Counts (Severity_Id) :=
                    Tree_Subprogram.Counts (Severity_Id) + 1;
                  Tree_Subprogram.Counts (Tree_Subprogram.Counts'Last) :=
                    Tree_Subprogram.Counts (Tree_Subprogram.Counts'Last) + 1;
               end if;

            else
               Tree_File.Messages.Append
                 (GPS.Kernel.Messages.References.Create
                    (GPS.Kernel.Messages.Message_Access (Message)));
            end if;
         end if;

         List.Next;
      end loop;
   end Load_Messages;

   --------------------
   -- Load_Resources --
   --------------------

   procedure Load_Resources (Self : in out Loader'Class) is
      Session  : constant GNATCOLL.SQL.Sessions.Session_Type :=
                   GNATCOLL.SQL.Sessions.Get_New_Session;
      List     : Database.Orm.Resource_List := Database.Orm.Filter
         (Database.Orm.All_Resources, Kind => 2).Get (Session);
      R        : Database.Orm.Resource;
      Resource : Resource_Access;

   begin
      while List.Has_Row loop
         R := List.Element;
         Resource :=
           new Resource_Record'(Name => To_Unbounded_String (R.Name));
         Self.Resources.Insert (R.Id, Resource);
         List.Next;
      end loop;
   end Load_Resources;

   ---------------------
   -- Load_Severities --
   ---------------------

   procedure Load_Severities (Self : in out Loader'Class) is
      Session  : constant GNATCOLL.SQL.Sessions.Session_Type :=
                   GNATCOLL.SQL.Sessions.Get_New_Session;
      List     : Database.Orm.Category_List       :=
                   Database.Orm.All_Categories.Get (Session);
      S        : Database.Orm.Category;
      Severity : Severity_Access;

   begin
      while List.Has_Row loop
         S := List.Element;
         Severity :=
           Self.Module.New_Severity
             (Name       => To_Unbounded_String (S.Label),
              On_Sidebar => S.On_Side);
         Self.Severities.Insert (S.Id, Severity);
         List.Next;
      end loop;

      if not Self.Severities.Contains (Empty_Severity_Id) then
         Severity :=
           Self.Module.New_Severity
             (Name       => To_Unbounded_String ("Unspecified"),
              On_Sidebar => True);
         Self.Severities.Insert (Empty_Severity_Id, Severity);
      end if;
   end Load_Severities;

   --------------------------
   -- Load_Tools_And_Rules --
   --------------------------

   procedure Load_Tools_And_Rules (Self : in out Loader'Class) is
      Session : constant GNATCOLL.SQL.Sessions.Session_Type :=
                  GNATCOLL.SQL.Sessions.Get_New_Session;
      TL      : Database.Orm.Tool_List := Database.Orm.All_Tools.Get (Session);
      T       : Database.Orm.Tool;
      RL      : Database.Orm.Rule_List;
      R       : Database.Orm.Rule;

      Tool    : Tool_Access;
      Rule    : Rule_Access;

   begin
      while TL.Has_Row loop
         T    := TL.Element;
         Tool := Self.Module.New_Tool (To_Unbounded_String (T.Name));

         RL := Database.Orm.Filter (T.Tool_Rules, Kind => 0).Get (Session);

         while RL.Has_Row loop
            R := RL.Element;
            Rule :=
              Self.Module.New_Rule
                (Tool       => Tool,
                 Name       => To_Unbounded_String (R.Name),
                 Identifier => To_Unbounded_String (R.Identifier));
            Self.Rules.Insert (R.Id, Rule);
            RL.Next;
         end loop;

         TL.Next;
      end loop;
   end Load_Tools_And_Rules;

end GNAThub.Loader;
