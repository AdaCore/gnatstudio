------------------------------------------------------------------------------
--                                                                          --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GNAT.Regexp;       use GNAT.Regexp;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.IO;           use GNAT.IO;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Symbols;  use GNATCOLL.Symbols;
with GNATCOLL.Traces;
with GPS.Search;        use GPS.Search;

with String_Utils;      use String_Utils;
with Find_Utils;        use Find_Utils;
with Src_Contexts;      use Src_Contexts;
with Language;          use Language;
with Language.Ada;      use Language.Ada;
with Language.C;        use Language.C;
with Language.Cpp;      use Language.Cpp;
with Language.Java;     use Language.Java;
with Language_Handlers; use Language_Handlers;
with GPS.Kernel;        use GPS.Kernel;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Projects;          use Projects;

procedure Run_Vsearch is

   Symbols : constant Symbol_Table_Access := GNATCOLL.Symbols.Allocate;

   procedure Print_Help;
   --  Print help about command line switches.

   procedure Wrapper_Search
     (Handler       : access Language_Handler_Record'Class;
      Look_For      : String;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : String;
      Recurse       : Boolean;
      Case_Sensitive : Boolean;
      Whole_Word     : Boolean;
      Regexp         : Boolean;
      Scope         : Search_Scope);
   --  Execute a search.
   --  The callback mustn't be null.
   --  Any exception is handled by printing its information on Standard_Error.

   procedure Report_Error (Line : String);
   --  Called to report error while parsing the project

   CL_Print_Matches     : Boolean  := True;
   CL_Max_Matches       : Natural  := Natural'Last;

   ----------------
   -- Print_Help --
   ----------------

   procedure Print_Help is
   begin
      Put_Line ("Usage : $0 Look_For {Files_List | Files_Pattern"
        & " [Selection_Options]}");
      Put_Line ("           [Callback_Options] [General_Options]");
      New_Line;
      Put_Line ("  -P project               name of the project file to load");
      New_Line;
      Put_Line (" Look_For:");
      Put_Line ("  -look_for       string   word or pattern to search");
      New_Line;
      Put_Line (" Files_Pattern:");
      Put_Line ("  -files_pattern  string   pattern to select files.");
      New_Line;
      Put_Line (" Selection_Options:");
      Put_Line ("  -directory      string   directory where to select files");
      Put_Line ("                           """" means current dir [""""]");
      Put_Line ("  -recurse        boolean  recurse in subdirectories [0]");
      New_Line;
      Put_Line (" Callback_Options:");
      Put_Line ("  -print_matches  boolean  whether to print matches     [1]");
      Put_Line ("  -print_files    boolean  whether to print files       [1]");
      Put_Line ("  -max_matches    number   non-aborting matches [0]");
      New_Line;
      Put_Line (" General_Options:");
      Put_Line ("  -case           boolean  match case           [0]");
      Put_Line ("  -whole          boolean  whole word           [0]");
      Put_Line ("  -regexp         boolean  look_for is a regexp [0]");
      Put_Line ("  -scope          string   searching scope [whole]");
      New_Line;
      Put_Line ("Boolean is '1' for true, other for false (eg '0').");
      Put_Line ("Default values are given enclosed by [].");
      Put_Line ("Switches may be given in any order.");
      Put_Line ("Files_List has priority over Files_Pattern.");
      Put_Line ("Matches are printed so: 'file:line:text'.");
      Put_Line ("Files are printed so: 'Done file'.");
      Put_Line ("Max_... is the limit of consecutive successful ...s.");
      Put_Line ("A 0 maximum means unlimited.");
      New_Line;
      Put_Line ("   Scope     Description");
      Put_Line ("------------ --------------");
      Put_Line ("whole        never use context");
      Put_Line ("comm_only    scan only comments");
      Put_Line ("str_only     scan only strings");
      Put_Line ("comm_str     scan strings & comments");
      Put_Line ("all_but_comm scan everything but comments");
   end Print_Help;

   --------------------
   -- Wrapper_Search --
   --------------------

   procedure Wrapper_Search
     (Handler       : access Language_Handler_Record'Class;
      Look_For      : String;
      Files_Pattern : GNAT.Regexp.Regexp;
      Directory     : String;
      Recurse       : Boolean;
      Case_Sensitive : Boolean;
      Whole_Word     : Boolean;
      Regexp         : Boolean;
      Scope         : Search_Scope)
   is
      Context : Files_Context_Access;
      Remaining_Matches : Natural := CL_Max_Matches;

      function General_Callback
         (Match : Search_Context; Text : String) return Boolean;
      --  General callback used in the testsuite

      function General_Callback
         (Match : Search_Context; Text : String) return Boolean
      is
         Location : constant String := +Base_Name (Current_File (Context))
           & ':' & Image (Match.Start.Line)
           & ':' & Image (Integer (Match.Start.Column));

      begin
         if CL_Print_Matches then
            Put_Line (Location & ':' & Text);
            --  ??? Would be nice to print the submatches as well (parenthesis
            --  pairs)
         end if;

         Remaining_Matches := Remaining_Matches - 1;
         return Remaining_Matches /= 0;
      end General_Callback;

   begin
      Context := Files_Factory
        (All_Occurrences => True,
         Scope           => Scope);
      Context.Set_Pattern
         (Look_For, Case_Sensitive => Case_Sensitive,
          Whole_Word => Whole_Word,
          Kind => (if Regexp then GPS.Search.Regexp else Full_Text));

      --  ??? What if we have a specific list of files
      Set_File_List
        (Context,
         Files_Pattern => Files_Pattern,
         Directory     => Create (+Directory),
         Recurse       => Recurse);

      while Search
        (Context,
         Handler  => Handler,
         Kernel   => null,
         Callback => General_Callback'Unrestricted_Access)
      loop
         null;
      end loop;

      Free (Root_Search_Context_Access (Context));
   end Wrapper_Search;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error (Line : String) is
   begin
      Put_Line ("Error when parsing project file: " & Line);
   end Report_Error;

   --  Start of processing for Run

   CL_Look_For        : String_Access  := null;
   CL_Files_Pattern   : String_Access  := new String'("*");
   CL_Directory       : String_Access  := new String'("");
   CL_Recurse         : Boolean        := False;
   CL_Whole_Word      : Boolean := False;
   CL_Case_Sensitive  : Boolean := False;
   CL_Regexp          : Boolean := False;
   CL_Scope           : String_Access  := new String'("");
   Project_Name       : String_Access := null;
   Scope              : Search_Scope       := Whole;
   Handler            : Language_Handler;
   Tree               : constant Project_Tree_Access := new Project_Tree;
   Registry           : constant Project_Registry_Access := Create (Tree);
   Project            : Project_Type;
   True_Value         : constant String := "1";
   Switches           : constant String :=
     "case: directory: files_pattern: help look_for:"
     & " max_matches: print_matches: "
     & " recurse: regexp: scope: whole: P:";

begin
   GNATCOLL.Traces.Parse_Config_File (Create_From_Base (".gnatdebug"));

   loop
      case Getopt (Switches) is
         when ASCII.NUL =>
            exit;

         when 'c' =>
            CL_Case_Sensitive := Parameter = True_Value;

         when 'd' =>
            Free (CL_Directory);
            CL_Directory := new String'(Parameter);

         when 'f' =>
            Free (CL_Files_Pattern);

            declare
               Original_Param : constant String := Parameter;
            begin
               if Index (Original_Param, "-asterix-") /= 0 then
                  CL_Files_Pattern := new String'
                    (Replace_Slice
                       (Original_Param, Index (Original_Param, "-asterix-"),
                        9, "*"));
               else
                  CL_Files_Pattern := new String'
                    (Original_Param);
               end if;
            end;
         when 'h' =>
            Print_Help;
            return;

         when 'l' =>
            Free (CL_Look_For);
            CL_Look_For := new String'(Parameter);

         when 'm' =>
            CL_Max_Matches := Natural'Value (Parameter);

         when 'P' =>
            Free (Project_Name);
            Project_Name := new String'(Parameter);

         when 'p' =>
            if Full_Switch = "print_matches" then
               CL_Print_Matches := Parameter = True_Value;
            end if;

         when 'r' =>
            if Full_Switch = "recurse" then
               CL_Recurse := Parameter = True_Value;

            else
               CL_Regexp := Parameter = True_Value;
            end if;

         when 's' =>
            Free (CL_Scope);
            CL_Scope := new String'(Parameter);

         when 'w' =>
            CL_Whole_Word := Parameter = True_Value;

         when others =>
            raise Program_Error;
      end case;
   end loop;

   if CL_Look_For = null then
      Put_Line (Standard_Error, "Need argument 'look_for' !");
      Put_Line (Standard_Error, "See -help.");
      return;
   end if;

   if CL_Scope.all = "" then
      null;
   elsif CL_Scope.all = "whole" then
      Scope := Whole;
   elsif CL_Scope.all = "comm_only" then
      Scope := Comments_Only;
   elsif CL_Scope.all = "str_only" then
      Scope := Strings_Only;
   elsif CL_Scope.all = "comm_str" then
      Scope := Comments_And_Strings;
   elsif CL_Scope.all = "all_but_comm" then
      Scope := All_But_Comments;
   else
      Put_Line (Standard_Error, "Bad argument for 'scope' !");
      return;
   end if;

   Create_Handler (Handler, Symbols);

   Set_Registry (Handler, Registry);

   Register_Language (Handler, Ada_Lang, null);
   --  Prj.Register_Default_Naming_Scheme
   --  (Get_String ("ada"), Get_String (".ads"), Get_String (".adb"),
   --   Get_Tree (Project));

   Register_Language (Handler, C_Lang, null);
   --  Prj.Register_Default_Naming_Scheme
   --    (Get_String ("c"), Get_String (".h"), Get_String (".c"),
   --     Get_Tree (Project));

   Register_Language (Handler, Cpp_Lang, null);
   --  Prj.Register_Default_Naming_Scheme
   --    (Get_String ("c++"), Get_String (".hh"), Get_String (".cpp"),
   --     Get_Tree (Project));

   Register_Language (Handler, Java_Lang, null);
   --  Prj.Register_Default_Naming_Scheme
   --    (Get_String ("java"), Get_String (".java"), Get_String (".java"),
   --     Get_Tree (Project));

   if Project_Name = null then
      Tree.Load_Empty_Project (Registry.Environment);
   else
      Tree.Load (Create (+Project_Name.all),
                 Env    => Registry.Environment,
                 Errors => Report_Error'Unrestricted_Access);
   end if;
   Project := Tree.Root_Project;

   Wrapper_Search
     (Handler,
      CL_Look_For.all,
      Compile (CL_Files_Pattern.all, Glob => True),
      CL_Directory.all,
      CL_Recurse,
      Scope  => Scope,
      Whole_Word     => CL_Whole_Word,
      Case_Sensitive => CL_Case_Sensitive,
      Regexp         => CL_Regexp);

   Free (CL_Look_For);
   Free (CL_Files_Pattern);
   Free (CL_Directory);
   Free (CL_Scope);
end Run_Vsearch;
