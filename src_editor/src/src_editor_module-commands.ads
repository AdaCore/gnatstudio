------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

--  This package defines interactive commands for operating on source editors

with Commands.Interactive; use Commands.Interactive;
with Commands;             use Commands;
with GPS.Kernel;           use GPS.Kernel;
with Glib.Object; use Glib.Object;

package Src_Editor_Module.Commands is

   type In_Line_Numbers_Area_Filter is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Line_Numbers_Area_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the event currently processed was in an editor's line numbers
   --  area

   type Has_Body_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Body_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a body

   type Has_Type_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a type

   type Has_Parent_Type_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Parent_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a parent type (and thus is itself a type)

   type Is_Access_Type_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Access_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity is an access type.

   type Is_Dispatching_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Dispatching_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a type

   type Has_Other_File_Filter is new GPS.Kernel.Action_Filter_Record
     with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current file has a spec/body

   type Goto_Line_Command is new Interactive_Command with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Goto_Line_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Ask the user on which line to jump to

   type Goto_Other_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Other_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the file spec/body, depending on what is currently open

   type Goto_Declaration_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Declaration_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the declaration of the entity in the context

   type Goto_Next_Body_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Next_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the body of the entity in the context

   type Goto_Type_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Type_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Go to the type declaration of the entity in the context

   type Type_Hierarchy_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Type_Hierarchy_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Output type hierarchy into the location view

   procedure On_Goto_Line
     (Widget  : access GObject_Record'Class;
      Kernel  : access Kernel_Handle_Record'Class);
   --  Callback for the "Goto Line" contextual menu

end Src_Editor_Module.Commands;
