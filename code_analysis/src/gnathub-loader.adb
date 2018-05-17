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

with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Strings;

with GNATCOLL.SQL.Sessions;
with GNATCOLL.Symbols;
with GNATCOLL.SQL.Sqlite;

with Basic_Types;
with Commands;

with GPS.Kernel.Messages;
with GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;

with Database.Orm;
with GNAThub.Messages;
with Language.Abstract_Language_Tree;

package body GNAThub.Loader is

   procedure Cleanup (Self : in out Loader'Class; Full : Boolean := False);
   --  Stop loading (when necessary) and cleanup resources.
   --  If Full, then also cleans the analysis data stored in memory.

   procedure Load_Severities (Self : in out Loader'Class);
   --  Loads list of severities from the database

   procedure Load_Tools_And_Rules (Self : in out Loader'Class);
   --  Loads list of tools and their rules.

   procedure Load_Resources (Self : in out Loader'Class);
   --  Loads list of resources.

   procedure Load_File_Messages
     (Self          : in out Loader'Class;
      Resource_Id   : Natural;
      Resource_Name : String);
   --  Loads messages for file resource.

   procedure Load_Project_Messages
     (Self          : in out Loader'Class;
      Resource_Id   : Natural;
      Resource_Name : String);
   --  Loads messages for project resource.

   function Get_Or_Create
     (Self        : Loader'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Visible     : Boolean;
      Severity_Id : Integer)
      return GNAThub_Project_Access;
   --  Retrieve the gnathub project or create it and update its message count.

   function Get_Or_Create
     (Self        : Loader'Class;
      Project     : GNAThub_Project_Access;
      File        : GNATCOLL.VFS.Virtual_File;
      Visible     : Boolean;
      Severity_Id : Integer)
      return GNAThub_File_Access;
   --  Retrieve the gnathub file or create it and update its message count.

   function Mangle_Subprogram_Name
     (Subprogram : GNAThub_Subprogram_Access)
      return String;
   --  Mangle the key using the line and column so we can add multiple
   --  entities with the same name in the subprograms map

   type Project_Loader_Command
     (Loader : not null access GNAThub.Loader.Loader'Class) is
     new Commands.Root_Command with null record;
   overriding function Execute
     (Self : access Project_Loader_Command)
      return Commands.Command_Return_Type;
   --  Loads next project messages

   type Dir_Loader_Command
     (Loader : not null access GNAThub.Loader.Loader'Class) is
     new Commands.Root_Command with null record;
   overriding function Execute
     (Self : access Dir_Loader_Command) return Commands.Command_Return_Type;
   --  Loads next directory messages

   type File_Loader_Command
     (Loader : not null access GNAThub.Loader.Loader'Class) is
     new Commands.Root_Command with null record;
   overriding function Execute
     (Self : access File_Loader_Command) return Commands.Command_Return_Type;
   --  Loads next file messages

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Loader'Class; Full : Boolean := False) is

      use type GPS.Scripts.Commands.Scheduled_Command_Access;

      procedure Free is
        new Ada.Unchecked_Deallocation (Resource_Record, Resource_Access);

      procedure Clean_Map (Map : in out Resource_Maps.Map);

      ---------------
      -- Clean_Map --
      ---------------

      procedure Clean_Map (Map : in out Resource_Maps.Map)
      is
         Resource : Resource_Access;
      begin
         while not Map.Is_Empty loop
            Resource := Map.First_Element;
            Map.Delete_First;
            Free (Resource);
         end loop;
      end Clean_Map;
   begin
      if Self.Command /= null then
         GPS.Kernel.Task_Manager.Interrupt_Queue
           (Self.Module.Get_Kernel, Self.Command);
         Self.Command := null;
      end if;

      if Full then
         Clear_Code_Analysis (Self.Module.Tree);
      end if;

      Clean_Map (Self.Project_Resources);
      Clean_Map (Self.Dir_Resources);
      Clean_Map (Self.File_Resources);

      Self.Severities.Clear;
      Self.Rules.Clear;
      Self.Metrics.Clear;
      GNATCOLL.Projects.Free (Self.Source_Files);

      GNATCOLL.SQL.Sessions.Free;
   end Cleanup;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Project_Loader_Command) return Commands.Command_Return_Type
   is
      Aux : Commands.Command_Access;
   begin
      Self.Loader.Load_Project_Messages
        (Resource_Maps.Key (Self.Loader.Current),
         To_String (Resource_Maps.Element (Self.Loader.Current).Name));
      Resource_Maps.Next (Self.Loader.Current);

      if Resource_Maps.Has_Element (Self.Loader.Current) then
         return Commands.Execute_Again;

      else
         Aux := new Dir_Loader_Command (Self.Loader);
         Self.Loader.Current := Self.Loader.Dir_Resources.First;
         Self.Loader.Command :=
           GPS.Kernel.Task_Manager.Launch_Background_Command
             (Kernel   => Self.Loader.Module.Get_Kernel,
              Command  => Aux,
              Active   => True,
              Show_Bar => False);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Dir_Loader_Command) return Commands.Command_Return_Type
   is
      Aux : Commands.Command_Access;
   begin
      --  Do nothing for the dir because they are unsupported for now
      --  in the code_analysis_tree
      Aux := new File_Loader_Command (Self.Loader);
      Self.Loader.Current := Self.Loader.File_Resources.First;
      Self.Loader.Command :=
        GPS.Kernel.Task_Manager.Launch_Background_Command
          (Kernel   => Self.Loader.Module.Get_Kernel,
           Command  => Aux,
           Active   => True,
           Show_Bar => False);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access File_Loader_Command) return Commands.Command_Return_Type is
   begin
      Self.Loader.Load_File_Messages
        (Resource_Maps.Key (Self.Loader.Current),
         To_String (Resource_Maps.Element (Self.Loader.Current).Name));
      Resource_Maps.Next (Self.Loader.Current);

      if Resource_Maps.Has_Element (Self.Loader.Current) then
         return Commands.Execute_Again;

      else
         GNATCOLL.Projects.Free (Self.Loader.Source_Files);
         Self.Loader.Command := null;
         Self.Loader.Cleanup;
         GNAThub.Module.Update_Report (Self.Loader.Module.all);
         return Commands.Success;
      end if;
   end Execute;

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
      Self.Cleanup (Full => True);

      GNATCOLL.SQL.Sessions.Setup
        (Descr        =>
           GNATCOLL.SQL.Sqlite.Setup (String (Database.Full_Name.all)),
         Max_Sessions => 2);
      Self.Load_Severities;
      Self.Load_Tools_And_Rules;
      Self.Load_Resources;

      if not Self.File_Resources.Is_Empty then
         Self.Source_Files := GNATCOLL.Projects.Source_Files
           (GPS.Kernel.Project.Get_Project
              (Self.Module.Get_Kernel), True, True);

         for Item of Self.Module.Severities loop
            Self.Module.Severities_Id.Include (Item, Id);
            Id := Id + 1;
         end loop;

         Aux := new Project_Loader_Command (Self'Unchecked_Access);
         Self.Current := Self.Project_Resources.First;
         Self.Command :=
           GPS.Kernel.Task_Manager.Launch_Background_Command
             (Kernel   => Self.Module.Get_Kernel,
              Command  => Aux,
              Active   => True,
              Show_Bar => False);

      elsif Self.File_Resources.Is_Empty then
         Self.Cleanup;
      end if;
   end Load;

   ------------------------
   -- Load_File_Messages --
   ------------------------

   procedure Load_File_Messages
     (Self          : in out Loader'Class;
      Resource_Id   : Natural;
      Resource_Name : String)
   is
      use GNATCOLL.Projects;
      use type GNATCOLL.VFS.Virtual_File;
      use Subprogram_Maps;

      File             : constant GNATCOLL.VFS.Virtual_File :=
                         GNATCOLL.VFS.Create_From_UTF8 (Resource_Name);
      Session          : constant GNATCOLL.SQL.Sessions.Session_Type :=
                         GNATCOLL.SQL.Sessions.Get_New_Session;
      Message_List     : Database.Orm.Resource_Message_List :=
                         Database.Orm.Filter
                          (Database.Orm.All_Resources_Messages,
                           Resource_Id => Resource_Id).Get (Session);
      Entities         : Database.Orm.Entity_List :=
                         Database.Orm.Filter
                          (Database.Orm.All_Entities,
                           Resource_Id => Resource_Id).Get (Session);
      R                : Database.Orm.Resource_Message;
      Entity           : Database.Orm.Entity;
      Sub_Message      : Database.Orm.Entity_Message;
      Sub_Message_List : Database.Orm.Entity_Message_List;
      Data_Sub_List    : Database.Orm.Message_List;
      M                : Database.Orm.Message;
      Message          : GNAThub.Messages.Message_Access;
      Metric_Message   : Metric;
      Visible          : Boolean;
      Rule             : GNAThub.Rule_Access;
      Severity         : GNAThub.Severity_Access;
      Severity_Id      : Positive;
      Position         : GNAThub.Severity_Natural_Maps.Cursor;

      Project          : GNATCOLL.Projects.Project_Type;
      Tree_Project     : GNAThub_Project_Access;
      Tree_File        : GNAThub_File_Access;
      Tree_Subprogram  : GNAThub_Subprogram_Access;

      Cursor           : Subprogram_Maps.Cursor;
      Filter_Result    : GPS.Kernel.Messages.Filter_Result;

      procedure Unchecked_Free is new
        Ada.Unchecked_Deallocation
          (GNAThub_Subprogram, GNAThub_Subprogram_Access);

      procedure Create_Data (Is_File_Data : Boolean);
      --  Create and add a message or a metric in the analysis tree
      --  If Is_File_Data then use Smart_Tree_Update when adding the data

      procedure Smart_Tree_Update;
      --  Via a semantic analysis: update the code_analysis tree
      --  by adding the projects, files and subprograms.

      -----------------
      -- Create_Data --
      -----------------

      procedure Create_Data (Is_File_Data : Boolean) is
      begin
         Severity := Self.Severities (M.Ranking);
         Severity_Id := Self.Module.Severities_Id.Element (Severity);

         --  Load a message
         if Self.Rules.Contains (M.Rule_Id) then
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

            Filter_Result := Self.Module.Filter.Apply (Message.all);
            Visible := not Filter_Result.Non_Applicable
              and then Filter_Result.Flags (GPS.Kernel.Messages.Locations);

            if Is_File_Data then
               Smart_Tree_Update;
            else
               Project :=
                 GPS.Kernel.Project.Get_Project (Self.Module.Get_Kernel);
               Tree_Project :=
                 Get_Or_Create (Self, Project, Visible, Severity_Id);
               Tree_File :=
                 Get_Or_Create
                   (Self, Tree_Project, File, Visible, Severity_Id);
            end if;

            if Tree_Subprogram /= null then
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

            --  Load a metric
         elsif Self.Metrics.Contains (M.Rule_Id) then
            Rule := Self.Metrics (M.Rule_Id);
            Metric_Message := new Metric_Record'(Severity => Severity,
                                                 Rule => Rule,
                                                 Value =>
                                                   Float'Value
                                                     (Database.Orm.Data (M)));
            Visible := False;
            if Is_File_Data then
               Smart_Tree_Update;
            else
               Project :=
                 GPS.Kernel.Project.Get_Project (Self.Module.Get_Kernel);
               Tree_Project :=
                 Get_Or_Create (Self, Project, Visible, Severity_Id);
               Tree_File :=
                 Get_Or_Create
                   (Self, Tree_Project, File, Visible, Severity_Id);
            end if;

            if Tree_Subprogram /= null then
               if not (Tree_Subprogram.Metrics.Contains
                       (Metric_Message.Rule.Tool.Name))
               then
                  Tree_Subprogram.Metrics.Include
                    (Metric_Message.Rule.Tool.Name,
                     Metrics_Ordered_Sets.Empty_Set);
               end if;
               Tree_Subprogram.Metrics
                 (Metric_Message.Rule.Tool.Name).Insert (Metric_Message);
            else
               if not (Tree_File.Metrics.Contains
                       (Metric_Message.Rule.Tool.Name))
               then
                  Tree_File.Metrics.Include
                    (Metric_Message.Rule.Tool.Name,
                     Metrics_Ordered_Sets.Empty_Set);
               end if;
               Tree_File.Metrics
                 (Metric_Message.Rule.Tool.Name).Insert (Metric_Message);
            end if;
         end if;
      end Create_Data;

      -----------------------
      -- Smart_Tree_Update --
      -----------------------

      procedure Smart_Tree_Update
      is
         use Language.Abstract_Language_Tree;
         use GNAT.Strings;

         Tree   : constant Semantic_Tree'Class :=
           Self.Module.Get_Kernel.Get_Abstract_Tree_For_File ("GNATHUB", File);
         Result : GNAThub_Subprogram_Access := null;

      begin
         if Tree = No_Semantic_Tree then
            Tree_Subprogram := null;
            return;
         end if;

         --  If a file's message is located in a entity then stores it
         --  as an entity's message.
         declare
            Iter : Semantic_Tree_Iterator'Class := Root_Iterator (Tree);
         begin
            --  Search for the most nested entity
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
                           Messages      => Messages_Vectors.Empty_Vector,
                           Metrics       => Metric_Tool_Maps.Empty_Map,
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

         for Index in Self.Source_Files'Range loop
            if Self.Source_Files (Index).File = File then
               Project := Self.Source_Files (Index).Project;
               exit;
            end if;
         end loop;

         Tree_Project :=
           Get_Or_Create (Self, Project, Visible, Severity_Id);
         Tree_File :=
           Get_Or_Create (Self, Tree_Project, File, Visible, Severity_Id);

         if Result /= null and then Project /= No_Project then
            Tree_Subprogram := Result;

            Cursor :=
              Tree_File.Subprograms.Find
                (Mangle_Subprogram_Name (Tree_Subprogram));
            if Has_Element (Cursor) then
               GNAT.Strings.Free (Tree_Subprogram.Name);
               Unchecked_Free (Tree_Subprogram);
               Tree_Subprogram := GNAThub_Subprogram_Access
                 (Subprogram_Maps.Element (Cursor));
            else
               Tree_File.Subprograms.Include
                 (Mangle_Subprogram_Name (Tree_Subprogram),
                  Subprogram_Access (Tree_Subprogram));
            end if;

         else
            Tree_Subprogram := null;
         end if;
      end Smart_Tree_Update;

   begin
      --  The messages/metrics of the file
      while Message_List.Has_Row loop
         R := Message_List.Element;
         M := Database.Orm.Filter
           (Database.Orm.All_Messages, Id => R.Message_Id)
           .Get (Session).Element;
         Create_Data (Is_File_Data => True);
         Message_List.Next;
      end loop;

      --  The entities of the file
      while Entities.Has_Row loop
         Entity := Entities.Element;
         Sub_Message_List := Database.Orm.Filter
           (Database.Orm.All_Entities_Messages, Entity_Id => Entity.Id)
           .Get (Session);

         Tree_Subprogram := new GNAThub_Subprogram'
           (Analysis_Data => (null, null),
            Name          => null,
            Counts_Size   => Natural
              (Self.Module.Severities.Length) + 1,
            Counts        => (others => 0),
            Messages      => Messages_Vectors.Empty_Vector,
            Metrics       => Metric_Tool_Maps.Empty_Map,
            others        => 0);

         Tree_Subprogram.Name := new String'(Entity.Name);
         Tree_Subprogram.Line := Entity.Line;
         Tree_Subprogram.Column := Natural (Entity.Col_Begin);

         Cursor :=
           Tree_File.Subprograms.Find
             (Mangle_Subprogram_Name (Tree_Subprogram));
         if Has_Element (Cursor) then
            GNAT.Strings.Free (Tree_Subprogram.Name);
            Unchecked_Free (Tree_Subprogram);
            Tree_Subprogram := GNAThub_Subprogram_Access
              (Subprogram_Maps.Element (Cursor));
         else
            Tree_File.Subprograms.Include
              (Mangle_Subprogram_Name (Tree_Subprogram),
               Subprogram_Access (Tree_Subprogram));
         end if;

         --  The messages/metrics of the current entity
         while Sub_Message_List.Has_Row loop
            Sub_Message := Sub_Message_List.Element;
            Data_Sub_List := Database.Orm.Filter
              (Database.Orm.All_Messages, Id => Sub_Message.Message_Id)
              .Get (Session);

            while Data_Sub_List.Has_Row loop
               M := Data_Sub_List.Element;
               Create_Data (Is_File_Data => False);
               Data_Sub_List.Next;
            end loop;
            Sub_Message_List.Next;
         end loop;

         Entities.Next;
      end loop;
   end Load_File_Messages;

   ---------------------------
   -- Load_Project_Messages --
   ---------------------------

   procedure Load_Project_Messages
     (Self          : in out Loader'Class;
      Resource_Id   : Natural;
      Resource_Name : String)
   is
      Session        : constant GNATCOLL.SQL.Sessions.Session_Type :=
        GNATCOLL.SQL.Sessions.Get_New_Session;
      List           : Database.Orm.Resource_Message_List :=
        Database.Orm.Filter
          (Database.Orm.All_Resources_Messages,
           Resource_Id => Resource_Id).Get (Session);
      R              : Database.Orm.Resource_Message;
      M              : Database.Orm.Message;
      Metric_Message : Metric;
      Rule           : GNAThub.Rule_Access;
      Severity       : GNAThub.Severity_Access;
      Severity_Id    : Positive;

      Project        : GNATCOLL.Projects.Project_Type;
      Tree_Project   : GNAThub_Project_Access;
   begin
      --  Only retrieve the metric for the project
      --  A message has no meaning for a project
      while List.Has_Row loop
         R := List.Element;
         M := Database.Orm.Filter
           (Database.Orm.All_Messages, Id => R.Message_Id)
           .Get (Session).Element;
         Severity := Self.Severities (M.Ranking);
         Severity_Id := Self.Module.Severities_Id.Element (Severity);

         if Self.Metrics.Contains (M.Rule_Id) then
            Rule := Self.Metrics (M.Rule_Id);
            Metric_Message := new Metric_Record'(Severity => Severity,
                                                 Rule => Rule,
                                                 Value =>
                                                   Float'Value
                                                     (Database.Orm.Data (M)));

            --  Retrieve the project
            Project :=
              GPS.Kernel.Project.Get_Project_Tree
                (Self.Module.Get_Kernel).Project_From_Name (Resource_Name);
            Tree_Project :=
              Get_Or_Create (Self, Project, False, Severity_Id);

            --  Add the metric
            if not (Tree_Project.Metrics.Contains
                    (Metric_Message.Rule.Tool.Name))
            then
               Tree_Project.Metrics.Include
                 (Metric_Message.Rule.Tool.Name,
                  Metrics_Ordered_Sets.Empty_Set);
            end if;
            Tree_Project.Metrics
              (Metric_Message.Rule.Tool.Name).Insert (Metric_Message);
         end if;
         List.Next;
      end loop;
   end Load_Project_Messages;

   --------------------
   -- Load_Resources --
   --------------------

   procedure Load_Resources (Self : in out Loader'Class) is
      Session  : constant GNATCOLL.SQL.Sessions.Session_Type :=
        GNATCOLL.SQL.Sessions.Get_New_Session;

      procedure Retrieve_Kind (Map : in out Resource_Maps.Map; Kind : Integer);
      --  There are two types of ressources:
      --    Project_Kind = 0
      --    Directory_Kind = 1
      --    File_Kind = 2

      -------------------
      -- Retrieve_Kind --
      -------------------

      procedure Retrieve_Kind (Map : in out Resource_Maps.Map; Kind : Integer)
      is
         List     : Database.Orm.Resource_List := Database.Orm.Filter
           (Database.Orm.All_Resources, Kind => Kind).Get (Session);
         R        : Database.Orm.Resource;
         Resource : Resource_Access;
      begin
         while List.Has_Row loop
            R := List.Element;
            Resource :=
              new Resource_Record'(Name => To_Unbounded_String (R.Name));
            Map.Insert (R.Id, Resource);
            List.Next;
         end loop;
      end Retrieve_Kind;
   begin
      Retrieve_Kind (Self.Project_Resources, Kind => 0);
      Retrieve_Kind (Self.Dir_Resources, Kind => 1);
      Retrieve_Kind (Self.File_Resources, Kind => 2);
   end Load_Resources;

   ---------------------
   -- Load_Severities --
   ---------------------

   procedure Load_Severities (Self : in out Loader'Class) is
      Severity : Severity_Access;
   begin
      for S in Analysis_Message_Category loop
         Severity := Self.Module.New_Severity (S);
         Self.Severities.Insert (Analysis_Message_Category'Pos (S), Severity);
      end loop;
   end Load_Severities;

   --------------------------
   -- Load_Tools_And_Rules --
   --------------------------

   procedure Load_Tools_And_Rules (Self : in out Loader'Class) is
      Session : constant GNATCOLL.SQL.Sessions.Session_Type :=
        GNATCOLL.SQL.Sessions.Get_New_Session;
      TL      : Database.Orm.Tool_List := Database.Orm.All_Tools.Get (Session);
      T       : Database.Orm.Tool;
      Tool    : Tool_Access;

      procedure Retrieve_Kind (M : in out Rule_Maps.Map; Kind : Integer);
      --  There are two types of rules:
      --    Rule_Kind = 0: its messages are stored as String in the database
      --    Metric_Kind = 1: its messages are stored as Float in the database

      ------------------
      -- Retieve_Kind --
      ------------------

      procedure Retrieve_Kind (M : in out Rule_Maps.Map; Kind : Integer)
      is
         RL   : Database.Orm.Rule_List;
         R    : Database.Orm.Rule;
         Rule : Rule_Access;
      begin
         RL := Database.Orm.Filter (T.Tool_Rules, Kind => Kind).Get (Session);

         while RL.Has_Row loop
            R := RL.Element;
            Rule :=
              Self.Module.New_Rule
                (Tool       => Tool,
                 Name       => To_Unbounded_String (R.Name),
                 Identifier => To_Unbounded_String (R.Identifier));
            M.Insert (R.Id, Rule);
            RL.Next;
         end loop;
      end Retrieve_Kind;
   begin
      while TL.Has_Row loop
         T := TL.Element;
         Tool := Self.Module.New_Tool (To_Unbounded_String (T.Name));
         Retrieve_Kind (Self.Rules, Kind => 0);
         Retrieve_Kind (Self.Metrics, Kind => 1);
         TL.Next;
      end loop;
   end Load_Tools_And_Rules;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Self        : Loader'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Visible     : Boolean;
      Severity_Id : Integer)
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
            Counts        => (others => 0),
            Metrics       => Metric_Tool_Maps.Empty_Map);
         Self.Module.Tree.Include
           (Project, Code_Analysis.Project_Access (Result));
      end if;

      if Visible then
         Result.Counts (Severity_Id) := Result.Counts (Severity_Id) + 1;
         Result.Counts (Result.Counts'Last) :=
           Result.Counts (Result.Counts'Last) + 1;
      end if;

      return Result;
   end Get_Or_Create;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Self        : Loader'Class;
      Project     : GNAThub_Project_Access;
      File        : GNATCOLL.VFS.Virtual_File;
      Visible     : Boolean;
      Severity_Id : Integer)
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
            Messages      => Messages_Vectors.Empty_Vector,
            Metrics       => Metric_Tool_Maps.Empty_Map);
         Project.Files.Include (File, Code_Analysis.File_Access (Result));
      end if;

      if Visible then
         Result.Counts (Severity_Id) := Result.Counts (Severity_Id) + 1;
         Result.Counts (Result.Counts'Last) :=
           Result.Counts (Result.Counts'Last) + 1;
      end if;

      return Result;
   end Get_Or_Create;

   ----------------------------
   -- Mangle_Subprogram_Name --
   ----------------------------

   function Mangle_Subprogram_Name
     (Subprogram : GNAThub_Subprogram_Access)
      return String is
   begin
      return (Integer'Image (Subprogram.Line)
              & "_"
              & Integer'Image (Subprogram.Column)
              & "_"
              & Subprogram.Name.all);
   end Mangle_Subprogram_Name;

end GNAThub.Loader;
