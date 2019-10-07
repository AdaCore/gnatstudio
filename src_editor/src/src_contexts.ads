------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Strings.Unbounded;
with GNAT.Regexp;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

with Gtk.Combo_Box_Text;
with Gtk.Text_Iter;
with Gtk.Text_Mark;         use Gtk.Text_Mark;
with Gtk.Widget;
with Gtkada.MDI;            use Gtkada.MDI;

with Basic_Types;           use Basic_Types;
with Files_Extra_Info_Pkg;
with Find_Utils;            use Find_Utils;
with GPS.Editors;           use GPS.Editors;
with GPS.Kernel;
with GPS.Search;
with GPS.Search.Replaces;   use GPS.Search.Replaces;
with Language_Handlers;

private with GPS_Vectors;

package Src_Contexts is

   type Simple_Scope_Selector_Record is
     new Scope_Selector_Interface with private;
   type Simple_Scope_Selector is access all Simple_Scope_Selector_Record'Class;
   --  Type used to create a simple scope selector widget

   type Files_Extra_Scope_Record is
     new Simple_Scope_Selector_Record with private;
   type Files_Extra_Scope is access all Files_Extra_Scope_Record'Class;
   --  Type used to create a scope selector widget with advanced options
   --  regarding files.

   type Search_Scope is
     (Whole,
      Comments_Only,
      Comments_And_Strings,
      Strings_Only,
      All_But_Comments);
   pragma Convention (C, Search_Scope);
   --  Scope wanted for the search.
   --  Whole scope means never use any context (i.e. files are whole scanned).
   --  This scope mostly applies to source files.
   --  Warning: do not change the contents or order of this type without
   --  synchronizing with Initialize_Scope_Combo and Scan_Buffer in the body
   --  of this package.

   type Source_Search_Occurrence_Record is
     new Search_Occurrence_Record with private;
   type Source_Search_Occurrence is
     access all Source_Search_Occurrence_Record'Class;
   --  Type used to represent a search occurence for source files

   overriding function Is_Equal
     (Left  : not null access Source_Search_Occurrence_Record;
      Right : not null access Source_Search_Occurrence_Record) return Boolean;
   --  Return True when the Left and Right occurrences are equal (i.e: same
   --  location).

   ------------------
   -- File context --
   ------------------

   type File_Search_Context is abstract new Root_Search_Context with private;

   type Current_File_Search_Module is
     new Search_Module_Type with private;

   overriding function Create_Context
     (Module          : not null access Current_File_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Current File". A Files_Project_Context is returned if
   --  searching for All_Occurrences
   --  This only works from the GUI, and shouldn't be used for text mode

   type Current_File_Context is new File_Search_Context with private;
   type Current_File_Context_Access is access all Current_File_Context'Class;
   --  A special context for searching interactively in the current file.
   --  It doesn't support All_Occurrences.

   overriding function Context_Look_In
     (Self : Current_File_Context) return String;
   --  See inherited documentation

   function Current_File_Factory
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences   : Boolean;
      Scope             : Search_Scope := Whole)
      return Root_Search_Context_Access;
   --  Same as above, but takes the scope directly in parameter

   type Editor_Coordinates is record
      Line : Editable_Line_Type;
      Col  : Character_Offset_Type;
   end record;

   type Search_Failure_Response is (None, Dialog, Informational_Popup);
   --  Type enumerating the different possibilities that can be used when
   --  a search fails to find another occurence.
   --
   --  . None: No response at all
   --
   --  . Dialog: a dialog warning the user that the search failed is displayed
   --
   --  . Informational_Popup: an ephemeral informatiional popup displaying a
   --    loopback icon is displayed.

   procedure Search_In_Editor
     (Context          : access Current_File_Context;
      Start_At         : Gtk.Text_Iter.Gtk_Text_Iter;
      Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward  : Boolean;
      Match_From       : out Editor_Coordinates;
      Match_Up_To      : out Editor_Coordinates;
      Found            : out Boolean;
      Start_Line       : Editable_Line_Type := 1;
      Start_Column     : Character_Offset_Type := 1;
      End_Line         : Editable_Line_Type := 0;
      End_Column       : Character_Offset_Type := 0;
      Failure_Response : Search_Failure_Response := Informational_Popup);
   --  Search for Context in an editor. The search starts at the given
   --  location and only applies to that buffer.
   --
   --  If Found is set to False on exit, then no match was found and the
   --  value of Match_From .. Match_Up_To is irrelevant.
   --
   --  Restrict search to given range (if specified):
   --  Start_Line:Start_Column .. End_Line:End_Column
   --
   --  Failure_Response is used to select which type of response is displayed
   --  in case of failure.

   overriding function Get_Terminate_Message
     (Context : access Current_File_Context;
      Kind    : Operation_Kind) return String;

   --------------------------------
   --  Current Selection Context --
   --------------------------------

   type Current_Selection_Search_Module is
     new Current_File_Search_Module with private;

   overriding function Create_Context
     (Module          : not null access Current_Selection_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Current Selection".

   type Current_Selection_Context is new Current_File_Context with private;

   overriding function Search
     (Context               : access Current_Selection_Context;
      Kernel                : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward       : Boolean;
      From_Selection_Start  : Boolean;
      Give_Focus            : Boolean;
      Found                 : out Boolean;
      Continue              : out Boolean;
      Display_Matched_Only : Boolean := False) return Search_Occurrence;
   --  Search function for "Current Selection"

   overriding function Replace
     (Context         : access Current_Selection_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean;

   overriding function Context_Look_In
     (Self : Current_Selection_Context) return String;

   ----------------------------
   -- Abstract files context --
   ----------------------------
   --  This context groups the common behavior for all the searches that are
   --  done on a set of files. It is mostly exposed so that it can be reused by
   --  the automatic testsuite

   type Abstract_Files_Context is abstract new
     File_Search_Context with private;

   function Search
     (Context              : access Abstract_Files_Context;
      Handler              : access
        Language_Handlers.Language_Handler_Record'Class;
      Kernel               : GPS.Kernel.Kernel_Handle;
      Callback             : Scan_Callback;
      Display_Matched_Only : Boolean := False)
      return Boolean;
   --  Search either the next match or all the occurrences, depending on the
   --  parameter All_Occurrences. For each one of them, Callback is called.
   --  This function returns True if there are potentially more matches in the
   --  set of files, and False if either there are no more matches or Callback
   --  itself has returned False.
   --  If Kernel is not null, then this subprogram will first check whether
   --  there exists an open editor in GPS for the current file.
   --
   --  Handler is needed for support of the regression testsuite (where Kernel
   --  is null)

   function Current_File
     (Context : access Abstract_Files_Context)
      return GNATCOLL.VFS.Virtual_File is abstract;
   --  Return the current file.
   --  Return No_File if there are no more files to examine

   procedure Move_To_Next_File
     (Context : access Abstract_Files_Context) is abstract;
   --  Move to the next file in the list.

   procedure Move_To_First_File
     (Context : access Abstract_Files_Context) is abstract;
   --  Move to the first file in the list.

   -------------------
   -- Files context --
   -------------------

   type Files_Search_Module is
     new Search_Module_Type with private;

   overriding function Create_Context
     (Module          : not null access Files_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Files..."

   type Files_Context is new Abstract_Files_Context with private;
   type Files_Context_Access is access all Files_Context'Class;
   --  A special context for searching in a specific list of files

   overriding function Context_Look_In (Self : Files_Context) return String;

   overriding function Get_Current_Progress
     (Context : access Files_Context) return Integer;
   overriding function Get_Total_Progress
     (Context : access Files_Context) return Integer;
   --  Get the current/total search progress.

   procedure Set_File_List
     (Context       : access Files_Context;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : GNATCOLL.VFS.Virtual_File := No_File;
      Recurse       : Boolean := False);
   --  Set the list of files to search

   function Files_Factory
     (All_Occurrences : Boolean;
      Scope           : Search_Scope) return Files_Context_Access;
   --  Same as above, but independent from a GUI. This is mostly used for the
   --  testsuite.
   --  No list of files is set, you need to call Set_File_List appropriately

   overriding
   function Get_Terminate_Message
     (Context : access Files_Context;
      Kind    : Operation_Kind) return String;

   --------------------------------
   -- Files From Project context --
   --------------------------------

   type Files_From_Project_Search_Module
   is new Search_Module_Type with private;

   overriding function Create_Context
     (Module        : not null access Files_From_Project_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Files From Project".
   --  The list of files is automatically set to the files of the root project
   --  and its imported projects

   type Files_Project_Context is new Abstract_Files_Context with private;
   type Files_Project_Context_Access is access all Files_Project_Context'Class;
   --  Context used to search in all files from the project

   function Files_From_Project_Factory
     (Scope           : Search_Scope;
      All_Occurrences : Boolean) return Files_Project_Context_Access;
   --  Same as calling Create_Context, but suitable for use outside the GUI.
   --  No file is set, you need to call Set_File_List explicitely

   overriding function Context_Look_In
     (Self : Files_Project_Context) return String;

   overriding function Get_Current_Progress
     (Context : access Files_Project_Context) return Integer;
   overriding function Get_Total_Progress
     (Context : access Files_Project_Context) return Integer;
   --  Get the current/total search progress.

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : Basic_Types.File_Sets.Set);
   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : GNATCOLL.VFS.File_Array_Access);
   --  Set the list of files to search.
   --  No copy of Files is made, and it will be freed when the context no
   --  longer needs it.

   type Files_From_Root_Project_Search_Module is
     new Search_Module_Type with private;

   overriding function Create_Context
     (Module          : not null access Files_From_Root_Project_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Files From Current Project".
   --  The list of files is automatically set to the files of the root project
   --  without imported projects

   overriding
   function Get_Terminate_Message
     (Context : access Files_Project_Context;
      Kind    : Operation_Kind) return String;

   ---------------------------
   -- Runtime Files context --
   ---------------------------

   type Runtime_Files_Search_Module
   is new Search_Module_Type with private;

   overriding function Create_Context
     (Module          : not null access Runtime_Files_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Files From Runtime".
   --  The list of files is automatically set to the *.ads files from
   --  Predefined_Source_Path

   type Runtime_Files_Context is new Files_Project_Context with private;

   overriding function Context_Look_In
     (Self : Runtime_Files_Context) return String;

   ------------------------
   -- Open Files context --
   ------------------------

   type Open_Files_Search_Module
   is new Search_Module_Type with private;

   overriding function Create_Context
     (Module          : not null access Open_Files_Search_Module;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      All_Occurrences : Boolean;
      Selector        : Scope_Selector)
      return Root_Search_Context_Access;
   --  Factory for "Open Files".
   --  The list of files is automatically set to the currently opend files

   type Open_Files_Context is new Abstract_Files_Context with private;
   type Open_Files_Context_Access is access all Open_Files_Context'Class;
   --  Context used to search in all files current edited

   overriding function Context_Look_In
     (Self : Open_Files_Context) return String;

   overriding function Get_Current_Progress
     (Context : access Open_Files_Context) return Integer;
   overriding function Get_Total_Progress
     (Context : access Open_Files_Context) return Integer;
   --  Get the current/total search progress.

   procedure Set_File_List
     (Context : access Open_Files_Context;
      Files   : GNATCOLL.VFS.File_Array_Access);
   procedure Set_File_List
     (Context : access Open_Files_Context;
      Files   : Basic_Types.File_Sets.Set);
   --  Set the list of files to search.
   --  No copy of Files is made, and it will be freed when the context no
   --  longer needs it.

   overriding
   function Get_Terminate_Message
     (Context : access Open_Files_Context;
      Kind    : Operation_Kind) return String;

private

   type Source_Search_Occurrence_Record is new Search_Occurrence_Record with
   record
      Editor_Child : MDI_Child;
      --  The editor in which the occurrence has been matched

      Match_From   : Editor_Coordinates;
      --  The editor coordinates for the match's start

      Match_Up_To  : Editor_Coordinates;
      --  The editor coordinates for the match's end
   end record;

   overriding function Search
     (Context              : access Current_File_Context;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean;
      Display_Matched_Only : Boolean := False) return Search_Occurrence;
   --  Search function for "Current File"

   overriding function Replace
     (Context         : access Current_File_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean;
   --  Replace function for "Current File"

   overriding procedure Highlight_Occurrence
     (Module     : not null access Current_File_Search_Module;
      Occurrence : not null access Search_Occurrence_Record'Class);
   overriding procedure Give_Focus_To_Occurrence
     (Module     : not null access Current_File_Search_Module;
      Occurrence : not null access Search_Occurrence_Record'Class);

   type Recognized_Lexical_States is
     (Statements, Strings, Mono_Comments, Multi_Comments);
   --  Current lexical state of the currently parsed file.
   --
   --  Statements      all but comments and strings
   --  Strings         string literals
   --  Mono_Comments   end of line terminated comments
   --  Multi_Comments  (possibly) multi-line comments

   type Dir_Data is record
      Name  : Virtual_File;
      Files : File_Array_Access;
      F_Idx : Natural;
   end record;
   type Dir_Data_Access is access Dir_Data;

   procedure Free (D : in out Dir_Data_Access);

   package Directory_List is new GPS_Vectors (Dir_Data_Access);

   type File_Search_Context is abstract new Root_Search_Context with record
      Replace_Valid      : Boolean := False;
      --  Whether the current search item that the context refers to
      --  is acceptable for a replace operation.

      Current            : GPS.Search.Search_Context := GPS.Search.No_Match;
      --  Information about the last match

      Scope              : Search_Scope := Whole;

      Current_Lexical    : Recognized_Lexical_States := Statements;
      --  The current scope when parsing the current file. This needs to be
      --  saved so that when we continue the search we restart in the proper
      --  state

      Replacement        : Replacement_Pattern;
      --  Cached replacement pattern

      Nb_Of_Replacements : Natural := 0;
      --  The number of replacements made with this context.
   end record;

   overriding procedure Reset
     (Context : access File_Search_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  See inherited documentation

   type Current_File_Search_Module is
     new Search_Module_Type with null record;

   type Current_File_Context is new File_Search_Context with record
      Current_File : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Abstract_Files_Context is abstract new File_Search_Context
     with null record;

   --  Base context for all contexts that search in multiple files (possibly
   --  not opened in an editor)
   type Abstract_Files_Context_Access is access all
     Abstract_Files_Context'Class;

   overriding procedure Reset
     (Context : access Abstract_Files_Context;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  See inherited documentation

   overriding function Search
     (Context              : access Abstract_Files_Context;
      Kernel               : access GPS.Kernel.Kernel_Handle_Record'Class;
      Search_Backward      : Boolean;
      From_Selection_Start : Boolean;
      Give_Focus           : Boolean;
      Found                : out Boolean;
      Continue             : out Boolean;
      Display_Matched_Only : Boolean := False) return Search_Occurrence;
   --  Search function for "Files From Project" and "Open_Files"

   overriding function Replace
     (Context         : access Abstract_Files_Context;
      Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Replace_String  : String;
      Case_Preserving : Boolean;
      Search_Backward : Boolean;
      Give_Focus      : Boolean) return Boolean;
   --  Replace function for "Files From Project" and "Open_Files"

   type Files_Search_Module is
     new Search_Module_Type with null record;

   type Files_Context is new Abstract_Files_Context with record
      Files_Pattern : GNAT.Regexp.Regexp;
      Recurse       : Boolean                   := False;
      Dirs          : Directory_List.Vector;
      Current_File  : GNATCOLL.VFS.Virtual_File;

      Directory     : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;

      At_End        : Boolean := False;
      --  Set to true at the end of the search

      Total_Dirs    : Natural := 0;
      Current_Dir   : Natural := 0;
   end record;

   type Files_From_Project_Search_Module
   is new Search_Module_Type with null record;

   type Files_From_Root_Project_Search_Module is
     new Search_Module_Type with null record;

   type Files_Project_Context is new Abstract_Files_Context with record
      Files        : GNATCOLL.VFS.File_Array_Access;
      Current_File : Integer;
   end record;

   type Open_Files_Search_Module
   is new Search_Module_Type with null record;

   type Open_Files_Context is new Abstract_Files_Context with record
      Files        : GNATCOLL.VFS.File_Array_Access := null;
      Current_File : Natural := 0;
   end record;

   overriding function Current_File
     (Context : access Files_Project_Context) return GNATCOLL.VFS.Virtual_File;
   overriding
   procedure Move_To_Next_File (Context : access Files_Project_Context);
   overriding
   procedure Move_To_First_File (Context : access Files_Project_Context);
   overriding
   procedure Free (Context : in out Files_Project_Context);

   overriding function Current_File (Context : access Files_Context)
     return GNATCOLL.VFS.Virtual_File;
   overriding procedure Move_To_Next_File (Context : access Files_Context);
   overriding procedure Move_To_First_File (Context : access Files_Context);
   overriding procedure Free (Context : in out Files_Context);

   overriding function Current_File
     (Context : access Open_Files_Context) return GNATCOLL.VFS.Virtual_File;
   overriding
   procedure Move_To_Next_File (Context : access Open_Files_Context);
   overriding
   procedure Move_To_First_File (Context : access Open_Files_Context);
   overriding procedure Free (Context : in out Open_Files_Context);

   type Simple_Scope_Selector_Record is
     new  Scope_Selector_Interface with record
      Combo : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
   end record;

   overriding procedure Initialize
     (Selector : not null access Simple_Scope_Selector_Record;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   overriding function Get_Scope_Combo
     (Selector : not null access Simple_Scope_Selector_Record)
      return Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
   overriding function Get_Optional_Widget
     (Selector : not null access Simple_Scope_Selector_Record)
      return Gtk.Widget.Gtk_Widget;

   type Files_Extra_Scope_Record is
     new Simple_Scope_Selector_Record with record
      File_Info_Widget : Files_Extra_Info_Pkg.Files_Extra_Info_Access;
   end record;

   overriding procedure Initialize
     (Selector : not null access Files_Extra_Scope_Record;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   overriding function Get_Optional_Widget
     (Selector : not null access Files_Extra_Scope_Record)
      return Gtk.Widget.Gtk_Widget;

   type Runtime_Files_Search_Module is
     new Search_Module_Type with null record;

   type Runtime_Files_Context is new Files_Project_Context with null record;

   type Current_Selection_Search_Module is
     new Current_File_Search_Module with null record;

   type Current_Selection_Context is new Current_File_Context with record
      Selection_From : Gtk_Text_Mark;
      Selection_To   : Gtk_Text_Mark;
   end record;

   type Current_Selection_Context_Access is
     access all Current_Selection_Context'Class;

end Src_Contexts;
