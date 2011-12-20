------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This package provides a general widget for editing naming scheme editors.
--  It is only suitable for languages that do not distinguish between spec
--  and bodies

with Gtk.Box;
with GNAT.Strings;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with Gtk.GEntry;
with Gtk.Tree_View;
with Gtk.Tree_Store;

package Naming_Exceptions is

   type Exceptions_Editor_Record is new Gtk.Box.Gtk_Box_Record with private;
   type Exceptions_Editor is access all Exceptions_Editor_Record'Class;

   procedure Gtk_New
     (Editor   : out Exceptions_Editor;
      Language : String);
   --  Create a new editor for the Language

   function Create_Project_Entry
     (Editor             : access Exceptions_Editor_Record;
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   procedure Show_Project_Settings
     (Editor             : access Exceptions_Editor_Record;
      Project            : Project_Type);
   --  See doc for homonym subprograms in naming_editors.ads

private
   type Exceptions_Editor_Record is new Gtk.Box.Gtk_Box_Record with record
      Language        : GNAT.Strings.String_Access;
      Filename_Entry  : Gtk.GEntry.Gtk_Entry;
      Exceptions_List : Gtk.Tree_View.Gtk_Tree_View;
      Exceptions      : Gtk.Tree_Store.Gtk_Tree_Store;
   end record;
end Naming_Exceptions;
