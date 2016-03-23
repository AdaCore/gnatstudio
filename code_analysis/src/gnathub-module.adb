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

with GNATCOLL.VFS;
with GPS.Intl;              use GPS.Intl;
with GPS.Kernel.Actions;
with GPS.Kernel.Messages;

with GNAThub.Actions;
with GNAThub.Filters_Views;
with GNAThub.Loader;

package body GNAThub.Module is

   Module_Id : GNAThub_Module_Id;

   ------------------
   -- Display_Data --
   ------------------

   procedure Display_Data (Self : in out GNAThub_Module_Id_Record'Class) is
      Database : constant GNATCOLL.VFS.Virtual_File :=
                   Self.Get_Kernel.Get_Project_Tree.Root_Project.Object_Dir
                     .Create_From_Dir ("gnathub")
                     .Create_From_Dir ("gnathub.db");

      Ignore : Boolean;

   begin
      if Database.Is_Regular_File then
         Self.Loader.Load (Database);

         Ignore := GPS.Kernel.Actions.Execute_Action
           (Self.Get_Kernel, "open gnathub filters_view");

      else
         Self.Get_Kernel.Insert
           (Database.Display_Full_Name &
            (-" does not exist. Analysis information is absent."),
            Mode => GPS.Kernel.Error);
      end if;
   end Display_Data;

   --------------------
   -- Message_Loaded --
   --------------------

   procedure Message_Loaded (Self : in out GNAThub_Module_Id_Record'Class)
   is
      Ignore : Boolean;
   begin
      Ignore := GPS.Kernel.Actions.Execute_Action
        (Self.Get_Kernel, "update gnathub filters_view");
   end Message_Loaded;

   --------------
   -- New_Rule --
   --------------

   function New_Rule
     (Self       : in out GNAThub_Module_Id_Record'Class;
      Tool       : not null Tool_Access;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Identifier : Ada.Strings.Unbounded.Unbounded_String)
      return Rule_Access is
   begin
      return Rule : constant Rule_Access :=
        new Rule_Record'
          (Name       => Name,
           Identifier => Identifier,
           Tool       => Tool,
           Count      => <>)
      do
         Tool.Rules.Insert (Rule);
         Self.Rules.Insert (Rule);
      end return;
   end New_Rule;

   ------------------
   -- New_Severity --
   ------------------

   function New_Severity
     (Self       : in out GNAThub_Module_Id_Record'Class;
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      On_Sidebar : Boolean) return Severity_Access is
   begin
      return Severity : constant Severity_Access :=
        new Severity_Record'
          (Name       => Name,
           On_Sidebar => On_Sidebar)
      do
         Self.Severities.Insert (Severity);
      end return;
   end New_Severity;

   --------------
   -- New_Tool --
   --------------

   function New_Tool
     (Self : in out GNAThub_Module_Id_Record'Class;
      Name : Ada.Strings.Unbounded.Unbounded_String) return Tool_Access is
   begin
      return Tool : constant Tool_Access :=
        new Tool_Record'(Name => Name, Rules => <>)
      do
         Self.Tools.Insert (Tool);
      end return;
   end New_Tool;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Module_Id := new GNAThub_Module_Id_Record;
      Module_Id.Register_Module (Kernel, "GNAThub");
      GNAThub.Actions.Register_Actions (Module_Id);
      Module_Id.Loader := new GNAThub.Loader.Loader (Module_Id);
      Module_Id.Loader.Initialize;
      Module_Id.Filter := new GNAThub.Filters.Message_Filter;
      GNAThub.Filters_Views.Register_Module (Kernel, Module_Id);
      GPS.Kernel.Messages.Register_Filter
        (GPS.Kernel.Messages.Get_Messages_Container (Kernel),
         GPS.Kernel.Messages.Message_Filter_Access (Module_Id.Filter));
   end Register_Module;

end GNAThub.Module;
