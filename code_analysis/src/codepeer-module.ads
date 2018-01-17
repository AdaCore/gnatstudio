------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

--  ??? missing description of this package

private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Strings.Hash;

with Gdk.Event;
with Gdk.RGBA;
with Gtk.Menu;
with Gtk.Widget;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

private with Commands;
private with Default_Preferences;

with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Style_Manager; use GPS.Kernel.Style_Manager;

with Code_Analysis;

private with CodePeer.Bridge;
private with CodePeer.Listeners;
private with CodePeer.Reports;

package CodePeer.Module is

   type Module_Id_Record
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
     new GPS.Kernel.Modules.Module_ID_Record with private;

   type CodePeer_Module_Id is access all Module_Id_Record'Class;
   --  ??? missing comments on all types and procedures of this package

   procedure Load
     (Self             : access Module_Id_Record'Class;
      Inspection_File  : Virtual_File;
      Status_File      : Virtual_File;
      Bts_Directory    : Virtual_File;
      Output_Directory : Virtual_File);
   --  Load code review results from file, creates CodePeer Report window
   --  and display loaded results.

   procedure Review_Messages
     (Self     : access Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector);

   procedure Review_Messages
     (Self     : access Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector;
      File     : Virtual_File);

   type Submenu_Factory_Record
     (Module : access Module_Id_Record'Class) is
     new GPS.Kernel.Modules.UI.Submenu_Factory_Record with null record;
   overriding procedure Append_To_Menu
     (Factory : access Submenu_Factory_Record;
      Context : GPS.Kernel.Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module

   function Get_Color
     (Ranking : CodePeer.Message_Ranking_Level) return Gdk.RGBA.Gdk_RGBA;

   CodePeer_Category_Name  : constant String := "CodePeer: messages";
   Race_Condition_Category : constant String := "race condition";

   type CodePeer_Build_Mode
     (Kernel : not null access Kernel_Handle_Record'Class) is private;
   --  Type used to automatically set build mode to "codepeer" on entry
   --  and reset it to the previous mode on exit. Also takes care of
   --  freezing the xref db.

   function Create_CodePeer_Message
     (Id               : Natural;
      File             : Code_Analysis.File_Access;
      Subprogram       : Code_Analysis.Subprogram_Access;
      Merged           : Natural_Sets.Set;
      Lifeage          : Lifeage_Kinds;
      Line             : Positive;
      Column           : Positive;
      Category         : Message_Category_Access;
      Is_Check         : Boolean;
      Ranking          : Message_Ranking_Level;
      Text             : String;
      From_File        : GNATCOLL.VFS.Virtual_File;
      From_Line        : Positive;
      From_Column      : Positive;
      Checks           : Message_Category_Sets.Set;
      Vns              : Natural_Sets.Set;
      CWEs             : CWE_Category_Sets.Set)
      return Message_Access;

private

   type Message_Ranking_Color_Preference_Array is
     array (CodePeer.Message_Ranking_Level)
       of Default_Preferences.Color_Preference;

   type Message_Ranking_Style_Array is
     array (CodePeer.Message_Ranking_Level)
       of GPS.Kernel.Style_Manager.Style_Access;

   type CodePeer_Action is
     (None, Load_UI, Audit_Trail, Load_Bridge_Results, Load_CSV);
   --  Actions related to codepeer handling:
   --   - None: no action registered
   --   - Load_UI: load CodePeer UI
   --   - Audit_Trail: load audit trail after gps_codepeer_bridge has run
   --   - Load_Bridge_Results: load codepeer messages after gps_codepeer_bridge
   --   - Load_CSV: load CSV file after codepeer run

   package String_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets (String, Ada.Strings.Hash, "=");

   type Codepeer_Child_Record is new GPS_MDI_Child_Record with null record;
   type Codepeer_Child is access all Codepeer_Child_Record'Class;
   overriding function Build_Context
     (Self  : not null access Codepeer_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   type Module_Id_Record
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
     new GPS.Kernel.Modules.Module_ID_Record with record
      Version_Limit          : Format_Version := Format_Version'Last;
      --  Limit of maximum supported version of interchange format to
      --  workaround bug in CodePeer up to 3.1.1 (gps_codepeer_bridge reports
      --  error when GPS asks newer version when supported instead of fallback
      --  to latest supported version).
      Version                : Supported_Format_Version;
      --  Used version of format of interchange files.
      Output_Directory       : GNATCOLL.VFS.Virtual_File;
      --  CodePeer's output directory for root project.
      Tree                   : Code_Analysis.Code_Analysis_Tree;
      Messages               : CodePeer.Message_Maps.Map;
      --  Maps of message identifiers to messages.
      Annotation_Categories  : CodePeer.Bridge.Annotation_Category_Maps.Map;
      --  Map of identifiers to annotation categories to be used for lazy
      --  annotations loading.
      Race_Category          : CodePeer.Message_Category_Access;
      Has_Backtraces         : Boolean := False;
      Report_Subwindow       : Codepeer_Child;
      Report                 : CodePeer.Reports.Report;
      Annotations_Style      : GPS.Kernel.Style_Manager.Style_Access;
      Message_Colors         : Message_Ranking_Color_Preference_Array;
      Removed_Message_Color  : Default_Preferences.Color_Preference;
      Message_Styles         : Message_Ranking_Style_Array;
      Import_Annotations     : Default_Preferences.Boolean_Preference;
      --  Control import of CodePeer annotations
      Listener               : CodePeer.Listeners.Listener_Access;

      --  Global messages filter

      Filter_Criteria  : CodePeer.Message_Filter_Criteria;

      Action           : CodePeer_Action := None;
      --  Indicate possible action to be performed when a build target is
      --  finished.

      Inspection_File : Virtual_File;
      Status_File     : Virtual_File;
      Bts_Directory   : Virtual_File;
      --  Files is used to communicate with gps_codepeer_bridge

      Bridge_Messages : CodePeer.Message_Vectors.Vector;
      --  Messages used to communicate with gps_codepere_bridge

      Display_Values : Boolean := True;
      --  Display values tooltip

      Review_Command : Commands.Command_Access;
      --  Shared command to review selected message(s).

      Filter           : GPS.Kernel.Messages.Message_Filter_Access;
      --  Filter to update visibility of the messages.
   end record;

   overriding function Tooltip_Handler
     (Module  : access Module_Id_Record;
      Context : Selection_Context) return Gtk.Widget.Gtk_Widget;
   --  Create tooltip when backtraces data is available.

   procedure Load_Annotations
     (Self : access Module_Id_Record'Class;
      File : in out Code_Analysis.File'Class);
   --  Load annotations for subprograms of given source file.

   function Codepeer_Database_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  Return directory which is used by CodePeer for store SQLite database
   --  files.

   function CodePeer_Object_Directory
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  Return object directory in 'codepeer' mode.

   function Codepeer_Output_Directory
     (Kernel : not null access Kernel_Handle_Record'Class)
      return GNATCOLL.VFS.Virtual_File;
   --  Return directory which is used by CodePeer for output inspection
   --  results.

   function Codepeer_Message_Patterns
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  Return the project attribute CodePeer'Message_Patterns or No_File.

   function Codepeer_Additional_Patterns
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File;
   --  Return the project attribute CodePeer'Additional_Patterns or No_File.

   --  Style names to be used by children packages
   Informational_Probability_Style_Name : constant String
     := "CodePeer informational messages";

   Module : CodePeer_Module_Id;
   --  Global variable for store CodePeer plugin module. Used in the main menu
   --  callbacks.

   type CodePeer_Build_Mode
     (Kernel : not null access Kernel_Handle_Record'Class) is
     new Ada.Finalization.Controlled with
   record
      Switch_Mode : Boolean := False;
      Mode        : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   overriding procedure Initialize (Self : in out CodePeer_Build_Mode);
   overriding procedure Finalize (Self : in out CodePeer_Build_Mode);

   procedure Review
     (Module       : not null access Module_Id_Record'Class;
      Force        : Boolean;
      Build_Target : String);
   --  Launch CodePeer review using the specified build target.
   --  If Force is True, no dialog is displayed to change codepeer switches.

end CodePeer.Module;
