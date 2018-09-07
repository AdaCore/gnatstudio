------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014-2018, AdaCore                   --
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
private with Commands.Interactive;

private package CodePeer.Module.Actions is

   procedure Register_Actions (Module : not null CodePeer_Module_Id);
   --  Registers interactive commands of CodePeer module.

private

   use Standard.Commands;
   use Standard.Commands.Interactive;

   type CodePeer_Interactive_Command
     (Module : not null CodePeer_Module_Id) is
     abstract new Interactive_Command with null record;

   type Analyze_Command is new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self : access Analyze_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze..." menu item is activated

   type Analyze_All_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Analyze_All_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze All" menu item is activated

   type Analyze_File_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Analyze_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze File" menu item is activated

   type Analyze_File_By_File_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Analyze_File_By_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Analyze File By File" menu item is activated

   type Display_Code_Review_Command
      is new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Display_Code_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Display code review" menu item is activated

   type Display_HTML_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Display_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "HTML Report" menu item is activated

   type Generate_CSV_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Generate_CSV_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Generate CSV Report" menu item is activated

   type Generate_HTML_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Generate_HTML_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Generate HTML Report" menu item is activated

   type Generate_SCIL_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Generate_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Generate SCIL" menu item is activated

   type Log_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Log_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Edit CodePeer Log" menu item is activated

   type Regenerate_Report_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Regenerate_Report_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Regenerate Report" menu item is activated

   type Remove_Lock_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_Lock_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Remove Lock" menu item is activated

   type Remove_SCIL_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_SCIL_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Remove SCIL" menu item is activated

   type Remove_SCIL_DB_Command is new Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_SCIL_DB_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Remove SCIL & DB" menu item is activated

   type Remove_XML_Review_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_XML_Review_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Advanced->Remove XML Code Review" menu item is activated

   type Show_Annotations_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Show_Annotations_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called on "show annotations" item of contextual menu

   type Hide_Annotations_Command is
     new CodePeer_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Hide_Annotations_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called on "hide annotations" item of contextual menu

   -------------
   -- Filters --
   -------------

   type Is_Hide_Annotations_Filter
     (Module : not null CodePeer.Module.CodePeer_Module_Id) is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Hide_Annotations_Filter;
      Context : Selection_Context) return Boolean;
   --  Controls availability of "hide annotaions" contextual menu

   type Is_Show_Annotations_Filter
     (Module : not null CodePeer.Module.CodePeer_Module_Id) is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Show_Annotations_Filter;
      Context : Selection_Context) return Boolean;
   --  Controls availability of "show annotaions" contextual menu

   type Is_Local_Mode_Filter
     (Module : not null CodePeer.Module.CodePeer_Module_Id) is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Self    : access Is_Local_Mode_Filter;
      Context : Selection_Context) return Boolean;
   --  Controls availability of actions which is possible in local mode only.

end CodePeer.Module.Actions;
