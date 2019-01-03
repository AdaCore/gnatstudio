------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

   type In_Line_Numbers_Area_Filter is new Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access In_Line_Numbers_Area_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the event currently processed was in an editor's line numbers
   --  area

   type Has_Specification_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Specification_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a specification

   type Has_Body_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Body_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a body

   type Has_Type_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a type

   type Has_Parent_Type_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Parent_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a parent type (and thus is itself a type)

   type Is_Access_Type_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Access_Type_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity is an access type.

   type Is_Dispatching_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Is_Dispatching_Filter;
      Context : GPS.Kernel.Selection_Context) return Boolean;
   --  True if the current entity has a type

   type Goto_Line_Command is new Interactive_Command with null record;
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

   Open_Command_Name : constant String := "open file";
   type Open_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Open menu

   type Open_Remote_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Open_Remote_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Open Remote menu

   type New_View_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access New_View_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->New View menu

   New_File_Command_Name : constant String := "new file";
   type New_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access New_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->New menu

   Save_Command_Name : constant String := "save";
   type Save_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Save_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Save menu

   type Save_As_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Save_As_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Save As... menu

   type Print_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Print_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  File->Print menu

   type Print_Selection_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Print_Selection_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Selection->Print Selection menu

   type Goto_Body_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Goto_Body_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Navigate->Goto Body menu
   --  Goto the next body of the entity under the cursor in the current
   --  editor.

   type Close_Command_Mode is (Close_One, Close_All, Close_All_Except_Current);
   type Close_Command is new Interactive_Command with record
      Mode      : Close_Command_Mode;
   end record;
   overriding function Execute
     (Command : access Close_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close the current window (or all windows if Close_All is True)

   type Comment_Lines_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Comment_Lines_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Comment Lines menu

   type Uncomment_Lines_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Uncomment_Lines_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Uncomment Lines menu

   type Fold_All_Blocks_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Fold_All_Blocks_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Fold all blocks menu

   type Unfold_All_Blocks_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Unfold_All_Blocks_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Unfold all blocks menu

   type Refill_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Refill_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Edit->Refill

   type Edit_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc for inherited subprogram
   --  Edit a file (from a contextual menu)

   type Editor_Properties_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Editor_Properties_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  See doc for inherited subprogram
   --  Edit the properties of a file (from a contextual menu)

   procedure Save_To_File
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name    : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Success : out Boolean);
   --  Save the current editor to Name, or its associated filename if Name is
   --  null.

   procedure New_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new view for the current editor and add it in the MDI.
   --  The current editor is the focus child in the MDI. If the focus child
   --  is not an editor, nothing happens.

end Src_Editor_Module.Commands;
