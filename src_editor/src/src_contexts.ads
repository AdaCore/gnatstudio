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
with Gtkada.MDI;
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

   type Current_File_Context is new Search_Context with private;
   type Current_File_Context_Access is access all Current_File_Context'Class;
   --  A special context for searching in the current file

   procedure Free (Context : in out Current_File_Context);
   --  Free the memory allocated for the context

   function Search
     (Context         : access Current_File_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Current File"

   -------------------
   -- Files context --
   -------------------

   type Files_Context is new Search_Context with private;
   type Files_Context_Access is access all Files_Context'Class;
   --  A special context for searching in a specific list of files

   procedure Set_File_List
     (Context       : access Files_Context;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : String  := "";
      Recurse       : Boolean := False);
   --  Set the list of files to search

   procedure Free (Context : in out Files_Context);
   --  Free the memory allocated for the context

   function Search
     (Context         : access Files_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Files..."

   --------------------------------
   -- Files From Project context --
   --------------------------------

   type Files_Project_Context is new Search_Context with private;
   type Files_Project_Context_Access is access all Files_Project_Context'Class;

   procedure Set_File_List
     (Context : access Files_Project_Context;
      Files   : Basic_Types.String_Array_Access);
   --  Set the list of files to search.
   --  No copy of Files is made, the memory will be freed automatically.

   procedure Free (Context : in out Files_Project_Context);
   --  Free the memory allocated for the context

   function Search
     (Context         : access Files_Project_Context;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Search_Backward : Boolean) return Boolean;
   --  Search function for "Files From Project"

   -----------------------------
   -- Standard search support --
   -----------------------------

   function Current_File_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Current File"

   function Files_From_Project_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Files From Project"

   function Files_Factory
     (Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class;
      All_Occurences    : Boolean;
      Extra_Information : Gtk.Widget.Gtk_Widget)
      return Search_Context_Access;
   --  Factory for "Files..."

private

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

   type Current_File_Context is new Search_Context with record
      Scope                : Search_Scope              := Whole;
      Child                : Gtkada.MDI.MDI_Child;
      All_Occurences       : Boolean;
      Next_Matches_In_File : Match_Result_Array_Access := null;
      Last_Match_Returned  : Natural                   := 0;
      --  These two fields are used to memorize the list of matches in the
      --  current file. It is always faster to search the whole file at once,
      --  and memorize the matches so that each call to Search only
      --  returns one match.
   end record;

   --  No additional data is needed for searching in the current file.

   type Files_Context is new Search_Context with record
      Scope         : Search_Scope              := Whole;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : GNAT.OS_Lib.String_Access := null;
      Recurse       : Boolean                   := False;
      Dirs          : Directory_List.List;

      Current_File         : GNAT.OS_Lib.String_Access;
      Next_Matches_In_File : Match_Result_Array_Access := null;
      Last_Match_Returned  : Natural                   := 0;
   end record;

   type Files_Project_Context is new Search_Context with record
      Scope        : Search_Scope                    := Whole;
      Files        : Basic_Types.String_Array_Access := null;
      Current_File : Natural;

      Next_Matches_In_File : Match_Result_Array_Access := null;
      Last_Match_Returned  : Natural                   := 0;
   end record;

   type Scope_Selector_Record is new Gtk.Frame.Gtk_Frame_Record with record
      Combo : Gtk.Combo.Gtk_Combo;
   end record;

   type Files_Extra_Scope_Record is new
     Files_Extra_Info_Pkg.Files_Extra_Info_Record with
   record
      Combo : Gtk.Combo.Gtk_Combo;
   end record;

end Src_Contexts;
