-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Basic_Types;
with Generic_List;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Regexp;
with GNAT.Regpat;
with Glide_Kernel;
with Gtk.Widget;
with Gtk.Combo;
with Gtk.Frame;
with Find_Utils;   use Find_Utils;
with Files_Extra_Info_Pkg;

package Src_Contexts is

   type Scope_Selector_Record is new Gtk.Frame.Gtk_Frame_Record with private;
   type Scope_Selector is access all Scope_Selector_Record'Class;
   --  The widget used to ask the extra information for the search algorithms
   --  in source files

   procedure Gtk_New
     (Selector : out Scope_Selector;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new scope selector

   type Files_Extra_Scope_Record is new
     Files_Extra_Info_Pkg.Files_Extra_Info_Record with private;
   type Files_Extra_Scope is access all Files_Extra_Scope_Record'Class;
   --  A widget that groups the Files_Extra_Info widget and a combo box to
   --  select the scope

   procedure Gtk_New
     (Extra  : out Files_Extra_Scope;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new widget

   ------------------
   -- File context --
   ------------------

   type File_Search_Context is abstract new Search_Context with private;

   type Current_File_Context is new File_Search_Context with private;
   type Current_File_Context_Access is access all Current_File_Context'Class;
   --  A special context for searching interactively in the current file.
   --  It doesn't support All_Occurrences.

   function Current_File_Factory
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Current File". A Files_Project_Context is returned if
   --  searching for All_Occurrences

   -------------------
   -- Files context --
   -------------------

   type Files_Context is new File_Search_Context with private;
   type Files_Context_Access is access all Files_Context'Class;
   --  A special context for searching in a specific list of files

   procedure Set_File_List
     (Context       : access Files_Context;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : String  := "";
      Recurse       : Boolean := False);
   --  Set the list of files to search

   function Files_Factory
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Files..."

   --------------------------------
   -- Files From Project context --
   --------------------------------

   type Files_Project_Context is new File_Search_Context with private;
   type Files_Project_Context_Access is access all Files_Project_Context'Class;

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : Basic_Types.String_Array_Access);
   --  Set the list of files to search.
   --  No copy of Files is made, the memory will be freed automatically.

   function Files_From_Project_Factory
     (Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurrences    : Boolean;
      Extra_Information  : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Files From Project"

private

   function Search
     (Context         : access Current_File_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Current File"

   function Replace
     (Context         : access Current_File_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Search_Backward : Boolean) return Boolean;
   --  Replace function for "Current File"

   type Search_Scope is
     (Whole,
      Comments_Only,
      Comments_And_Strings,
      Strings_Only,
      All_But_Comments);
   --  Scope wanted for the search.
   --  Whole scope means never use any context (i.e. files are whole scanned).
   --  This scope mostly applies to source files.
   --  Warning: do not change the contents or order of this type without
   --  synchronizing with vsearch.glade and Scan_Buffer.

   type Recognized_Lexical_States is
     (Statements, Strings, Mono_Comments, Multi_Comments);
   --  Current lexical state of the currently parsed file.
   --
   --  Statements      all but comments and strings
   --  Strings         string literals
   --  Mono_Comments   end of line terminated comments
   --  Multi_Comments  (possibly) multi-line comments

   type Match_Array_Access is access GNAT.Regpat.Match_Array;

   type Match_Result_Access is access Match_Result;
   type Match_Result_Array is array (Positive range <>) of Match_Result_Access;
   type Match_Result_Array_Access is access all Match_Result_Array;

   type Dir_Data is record
      Name : GNAT.OS_Lib.String_Access;
      Dir  : GNAT.Directory_Operations.Dir_Type;
   end record;
   type Dir_Data_Access is access Dir_Data;
   procedure Free (D : in out Dir_Data_Access);
   package Directory_List is new Generic_List (Dir_Data_Access);

   type File_Search_Context is abstract new Search_Context with record
      Replace_Valid : Boolean := False;
      --  Whether the current search item that the context refers to
      --  is acceptable for a replace operation.

      Begin_Line, Begin_Column, End_Line, End_Column : Natural := 0;
      --  Begin_Line is set to 0 if no match was found

      All_Occurrences : Boolean := False;
      Scope           : Search_Scope := Whole;

      Current_Lexical : Recognized_Lexical_States := Statements;
      --  The current scope when parsing the current file. This needs to be
      --  saved so that when we continue the search we restart in the proper
      --  state
   end record;

   type Current_File_Context is new File_Search_Context with null record;

   type Abstract_Files_Context is abstract new File_Search_Context
     with null record;

   --  Base context for all contexts that search in multiple files (possibly
   --  not opened in an editor)
   type Abstract_Files_Context_Access is access all
     Abstract_Files_Context'Class;

   function Search
     (Context         : access Abstract_Files_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Files From Project"

   function Replace
     (Context         : access Abstract_Files_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Search_Backward : Boolean) return Boolean;
   --  Replace function for "Files From Project"

   function Current_File
     (Context : access Abstract_Files_Context) return String is abstract;
   --  Return the current file.
   --  Return the empty string if there are no more files to examine

   procedure Move_To_Next_File
     (Context : access Abstract_Files_Context) is abstract;
   --  Move to the next file in the list.

   type Files_Context is new Abstract_Files_Context with record
      Files_Pattern : GNAT.Regexp.Regexp;
      Recurse       : Boolean                   := False;
      Dirs          : Directory_List.List;
      Current_File  : GNAT.OS_Lib.String_Access;

      Directory     : GNAT.OS_Lib.String_Access := null;
      --  Set to null at the end of the search
   end record;

   type Files_Project_Context is new Abstract_Files_Context with record
      Files         : Basic_Types.String_Array_Access := null;
      Current_File  : Natural;
   end record;

   function Current_File
     (Context : access Files_Project_Context) return String;
   procedure Move_To_Next_File (Context : access Files_Project_Context);
   procedure Free (Context : in out Files_Project_Context);

   function Current_File (Context : access Files_Context) return String;
   procedure Move_To_Next_File (Context : access Files_Context);
   procedure Free (Context : in out Files_Context);

   type Scope_Selector_Record is new Gtk.Frame.Gtk_Frame_Record with record
      Combo : Gtk.Combo.Gtk_Combo;
   end record;

   type Files_Extra_Scope_Record is new
     Files_Extra_Info_Pkg.Files_Extra_Info_Record with
   record
      Combo : Gtk.Combo.Gtk_Combo;
   end record;

end Src_Contexts;
