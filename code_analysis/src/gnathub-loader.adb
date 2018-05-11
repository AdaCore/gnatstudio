------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with Ada.Unchecked_Deallocation;
with GNAT.Strings;
with GNATCOLL.Symbols;

with Commands;
with GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;
with Language.Abstract_Language_Tree;

package body GNAThub.Loader is

   type Loader_Command
     (Loader : not null access Loader_Type'Class) is
     new Commands.Root_Command with null record;
   overriding function Execute
     (Self : access Loader_Command)
      return Commands.Command_Return_Type;
   --  Used to load GNAThub messages in the background

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Loader_Type;
      Module : not null GNAThub.Module.GNAThub_Module_Id) is
   begin
      Self.Module := Module;
   end Initialize;

   ----------
   -- Load --
   ----------

   function Load (Self : in out Loader_Type'Class) return Boolean is
      Aux : Commands.Command_Access;
   begin
      Self.Cleanup;
      Self.Prepare_Loading;

      if not Self.Has_Data_To_Load then
         Self.Cleanup;
         return False;
      end if;

      Self.Source_Files := GNATCOLL.Projects.Source_Files
        (GPS.Kernel.Project.Get_Project
           (Self.Module.Get_Kernel), True, True);

      Aux := new Loader_Command (Self'Unchecked_Access);
      Self.Command :=
        GPS.Kernel.Task_Manager.Launch_Background_Command
          (Kernel   => Self.Module.Get_Kernel,
           Command  => Aux,
           Active   => True,
           Show_Bar => False);

      return True;
   end Load;

   ---------------------
   -- Remove_Messages --
   ---------------------

   procedure Remove_Messages (Self : in out Loader_Type) is
      M_Ref   : Message_Reference;
      Message : GPS.Kernel.Messages.Message_Access;
   begin
      while not Self.Messages.Is_Empty loop
         M_Ref := Self.Messages.First_Element;

         if not M_Ref.Is_Empty then
            Message := M_Ref.Message;
            Self.Module.Kernel.Get_Messages_Container.Remove_Message
              (Message   => Message,
               Flags     => Message.Get_Flags,
               Recursive => True);
         end if;

         Self.Messages.Delete_First;
      end loop;
   end Remove_Messages;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup (Self : in out Loader_Type) is
      use type GPS.Scripts.Commands.Scheduled_Command_Access;
   begin
      --  Interrupt the loading comand that runs in background if needed
      if Self.Command /= null then
         GPS.Kernel.Task_Manager.Interrupt_Queue
           (Self.Module.Get_Kernel, Self.Command);
         Self.Command := null;
      end if;

      --  Free the project source files
      GNATCOLL.Projects.Free (Self.Source_Files);
   end Cleanup;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Loader_Command)
      return Commands.Command_Return_Type is
   begin
      Self.Loader.Load_Data;

      if Self.Loader.Has_Data_To_Load then
         return Commands.Execute_Again;

      else
         GNATCOLL.Projects.Free (Self.Loader.Source_Files);
         Self.Loader.Command := null;
         Self.Loader.Cleanup;
         Self.Loader.Module.Update_Report;

         return Commands.Success;
      end if;
   end Execute;

   ---------------------
   -- Find_Subprogram --
   ---------------------

   function Find_Subprogram
     (Self : in out Loader_Type'Class;
      File : GNATCOLL.VFS.Virtual_File;
      Line : Integer)
      return GNAThub_Subprogram_Access
   is
      use Language.Abstract_Language_Tree;
      use GNAT.Strings;

      Tree   : constant Semantic_Tree'Class :=
                 Self.Module.Get_Kernel.Get_Abstract_Tree_For_File
                   ("GNATHUB", File);
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
                   and then Node.Sloc_Start.Line <= Line
                   and then Node.Sloc_End.Line >= Line
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
                        Metrics       => Metric_Tool_Maps.Empty_Map,
                        others        => 0);
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
     (Self        : in out Loader_Type'Class;
      Project     : GNATCOLL.Projects.Project_Type;
      Severity_Id : Natural;
      Visible     : Boolean)
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
            Metrics       => Metric_Tool_Maps.Empty_Map,
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
     (Self        : in out Loader_Type'Class;
      Project     : GNAThub_Project_Access;
      File        : GNATCOLL.VFS.Virtual_File;
      Severity_Id : Natural;
      Visible     : Boolean)
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
            Metrics       => Metric_Tool_Maps.Empty_Map,
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

   --------------------
   -- Insert_Message --
   --------------------

   procedure Insert_Message
     (Self    : in out Loader_Type'Class;
      Message : GNAThub.Messages.Message_Access)
   is
      use GNATCOLL.Projects;
      use type GNATCOLL.VFS.Virtual_File;

      File            : constant GNATCOLL.VFS.Virtual_File :=
                          Message.Get_File;
      Severity_Id     : constant Natural := Self.Module.Severities_Id
        (Message.Get_Severity);
      Project         : GNATCOLL.Projects.Project_Type;
      Tree_Project    : GNAThub_Project_Access;
      Tree_File       : GNAThub_File_Access;
      Tree_Subprogram : GNAThub_Subprogram_Access;
      Filter_Result   : GPS.Kernel.Messages.Filter_Result;
      M_Ref           : Message_Reference;
      Visible         : Boolean;
   begin
      Filter_Result := Self.Module.Filter.Apply (Message.all);
      Visible := not Filter_Result.Non_Applicable
        and then Filter_Result.Flags (GPS.Kernel.Messages.Locations);

      for Index in Self.Source_Files'Range loop
         if Self.Source_Files (Index).File = File then
            Project := Self.Source_Files (Index).Project;
            exit;
         end if;
      end loop;

      Tree_Project := Insert
        (Self        => Self,
         Project     => Project,
         Severity_Id => Severity_Id,
         Visible     => Visible);

      Tree_File    := Insert
        (Self        => Self,
         Project     => Tree_Project,
         File        => File,
         Severity_Id => Severity_Id,
         Visible     => Visible);

      if Project /= No_Project then
         Tree_Subprogram := Find_Subprogram
           (Self => Self,
            File => File,
            Line => Message.Get_Line);
      else
         Tree_Subprogram := null;
      end if;

      M_Ref := GPS.Kernel.Messages.References.Create
        (GPS.Kernel.Messages.Message_Access (Message));

      Self.Messages.Append (M_Ref);

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

         Tree_Subprogram.Messages.Append (M_Ref);

         if Visible then
            Tree_Subprogram.Counts (Severity_Id) :=
              Tree_Subprogram.Counts (Severity_Id) + 1;
            Tree_Subprogram.Counts (Tree_Subprogram.Counts'Last) :=
              Tree_Subprogram.Counts (Tree_Subprogram.Counts'Last) + 1;
         end if;

      else
         Tree_File.Messages.Append (M_Ref);
      end if;
   end Insert_Message;

   -------------------
   -- Insert_Metric --
   -------------------

   procedure Insert_Metric
     (Self   : in out Loader_Type'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Line   : Natural;
      Metric : Metric_Access)
   is
      use GNATCOLL.Projects;
      use type GNATCOLL.VFS.Virtual_File;

      Project         : GNATCOLL.Projects.Project_Type;
      Tree_Project    : GNAThub_Project_Access;
      Tree_File       : GNAThub_File_Access;
      Tree_Subprogram : GNAThub_Subprogram_Access;
   begin
      for Index in Self.Source_Files'Range loop
         if Self.Source_Files (Index).File = File then
            Project := Self.Source_Files (Index).Project;
            exit;
         end if;
      end loop;

      Tree_Project := Insert
        (Self        => Self,
         Project     => Project,
         Severity_Id => 0,
         Visible     => False);

      Tree_File    := Insert
        (Self        => Self,
         Project     => Tree_Project,
         File        => File,
         Severity_Id => 0,
         Visible     => False);

      if Project /= No_Project then
         Tree_Subprogram := Find_Subprogram
           (Self => Self,
            File => File,
            Line => Line);
      else
         Tree_Subprogram := null;
      end if;

      if Tree_Subprogram /= null then
         if not (Tree_Subprogram.Metrics.Contains
                 (Metric.Rule.Tool.Name))
         then
            Tree_Subprogram.Metrics.Include
              (Metric.Rule.Tool.Name,
               Metrics_Ordered_Sets.Empty_Set);
         end if;

         Tree_Subprogram.Metrics
           (Metric.Rule.Tool.Name).Insert (Metric);
      else
         if not (Tree_File.Metrics.Contains
                 (Metric.Rule.Tool.Name))
         then
            Tree_File.Metrics.Include
              (Metric.Rule.Tool.Name,
               Metrics_Ordered_Sets.Empty_Set);
         end if;

         Tree_File.Metrics
           (Metric.Rule.Tool.Name).Insert (Metric);
      end if;
   end Insert_Metric;

end GNAThub.Loader;
