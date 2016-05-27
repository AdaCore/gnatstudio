------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

with GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;

with Basic_Types;
with Commands;
with GPS.Kernel.Task_Manager;

with GNAThub.Messages;
with Orm;

package body GNAThub.Loader is

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

      Self.Loader.Module.Message_Loaded;

      if Resource_Maps.Has_Element (Self.Loader.Current) then
         return Commands.Execute_Again;

      else
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
      use type Commands.Command_Access;

      Aux : Commands.Command_Access;
      --  New command is assigned to this variable to create object with
      --  necessary accessibility level, otherwise acccessibility check will
      --  fail inside Launch_Background_Command subprogram.

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
      File     : constant GNATCOLL.VFS.Virtual_File :=
                   GNATCOLL.VFS.Create_From_UTF8 (Resource_Name);
      Session  : constant GNATCOLL.SQL.Sessions.Session_Type :=
                   GNATCOLL.SQL.Sessions.Get_New_Session;
      List     : Orm.Resource_Message_List :=
                   Orm.All_Resources_Messages.Filter
                     (Resource_Id => Resource_Id).Get (Session);
      R        : Orm.Resource_Message;
      M        : Orm.Message;
      Message  : GNAThub.Messages.Message_Access;
      Rule     : GNAThub.Rule_Access;
      Severity : GNAThub.Severity_Access;
      Position : GNAThub.Severity_Natural_Maps.Cursor;

   begin
      while List.Has_Row loop
         R := List.Element;
         M :=
           Orm.All_Messages.Filter (Id => R.Message_Id).Get (Session).Element;

         if Self.Rules.Contains (M.Rule_Id) then
            --  This is message

            Severity := Self.Severities (M.Category_Id);
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
               Text      => To_Unbounded_String (Orm.Data (M)),
               File      => File,
               Line      => R.Line,
               Column    => Basic_Types.Visible_Column_Type (R.Col_Begin));
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
      List     : Orm.Resource_List :=
                   Orm.All_Resources.Filter (Kind => 2).Get (Session);
      R        : Orm.Resource;
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
      List     : Orm.Category_List       := Orm.All_Categories.Get (Session);
      S        : Orm.Category;
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
   end Load_Severities;

   --------------------------
   -- Load_Tools_And_Rules --
   --------------------------

   procedure Load_Tools_And_Rules (Self : in out Loader'Class) is
      Session : constant GNATCOLL.SQL.Sessions.Session_Type :=
                  GNATCOLL.SQL.Sessions.Get_New_Session;
      TL      : Orm.Tool_List := Orm.All_Tools.Get (Session);
      T       : Orm.Tool;
      RL      : Orm.Rule_List;
      R       : Orm.Rule;

      Tool    : Tool_Access;
      Rule    : Rule_Access;

   begin
      while TL.Has_Row loop
         T    := TL.Element;
         Tool := Self.Module.New_Tool (To_Unbounded_String (T.Name));

         RL := T.Tool_Rules.Filter (Kind => 0).Get (Session);

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
