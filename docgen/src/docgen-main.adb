-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

--  This file contains the procedure Docgen_Main, the main procedure of
--  the program for the creation of project documentation.

--  The procedure examines the command line options, creates the list of
--  source files to be processed and calls the procedure Process_File
--  from the package Docgen.Work_On_File. The project ALI variables are
--  created here too, as their are needed to get the unit names of
--  the source files.

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Docgen.Work_On_File;       use Docgen.Work_On_File;
with Docgen;                    use Docgen;
with Docgen.Html_Output;        use Docgen.Html_Output;
with Docgen.Texi_Output;        use Docgen.Texi_Output;
with Docgen.ALI_Utils;          use Docgen.ALI_Utils;
with Glide_Intl;                use Glide_Intl;
with Src_Info;                  use Src_Info;
with Language_Handlers;         use Language_Handlers;
with Projects.Registry;         use Projects, Projects.Registry;

procedure Docgen.Main is

   package TSFL renames Type_Source_File_List;

   Help_Requested, Command_Line_Error : exception;
   Project_Not_First                  : exception;

   Source_File_List   : Type_Source_File_List.List;
   Prj_File_Name      : String_Access;
   Handler            : Language_Handler;
   Project            : Project_Type;
   Source_Info_List   : Src_Info.LI_File_List;
   Options            : All_Options;
   Registry           : aliased Project_Registry;

   procedure Handle_Command_Line
     (Quit : out Boolean);
   --  takes the arguments from the command line and sets the options

   procedure Add_Body_Files
     (Source_File_List : in out Type_Source_File_List.List);
   --  Add to the list the body files of the spec files in the list

   function Get_Source_Dir
     (Source_File_List : TSFL.List) return String;
   --  return the path of the first source file in the list

   -------------------------
   -- Handle_Command_Line --
   -------------------------

   procedure Handle_Command_Line
     (Quit : out Boolean)
   is
      use Ada.Command_Line;
      J                : Natural;
      N                : constant Natural := Argument_Count;
      Source_File_Node : Source_File_Information;
      LI_Unit          : LI_File_Ptr;
   begin
      --  ??? Should use GNAT.Command_Line instead
      J := 1;

      if N = 0 then
         raise Help_Requested;
      end if;
      Quit := False;

      --  if options for other output formats, define here!
      Options.Doc_Subprogram := Doc_HTML_Create'Access;
      Options.Doc_Suffix     := new String'(".htm");

      while J <= N loop
         declare
            S : constant String := Argument (J);
         begin

            if S = "-h"    or else S = "-?" or else
              S = "-help" or else S = "--help" then
               raise Help_Requested;

               --  the project file must be the first argement
            elsif J = 1 then
               if File_Extension (S) = ".gpr" then
                  Prj_File_Name := new String'(S);
                  Options.Project_Name :=
                    new String'(File_Name_Without_Suffix (S));
                  Load_Project (S, Registry, Project);
               else
                  raise Project_Not_First;
               end if;
            elsif S = "-texi" then
               Options.Doc_Subprogram := Doc_TEXI_Create'Access;
               Options.Doc_Suffix     := new String'(".texi");
            elsif S = "-onetexi" then
               Options.Doc_Subprogram := Doc_TEXI_Create'Access;
               Options.Doc_Suffix     := new String'(".texi");
               Options.One_Doc_File   := True;
            elsif S = "-body" then
               Options.Process_Body_Files := True;
            elsif S = "-ic" then
               Options.Ignorable_Comments := True;
            elsif S = "-above" then
               Options.Comments_Above := True;
            elsif S = "-verbose" then
               Options.Info_Output := True;
            elsif S = "-ref" then
               Options.References := True;
            elsif S = "-linkall" then
               Options.Link_All := True;
            elsif S = "-private" then
               Options.Show_Private := True;
            elsif S'Last > 9 and then S (1 .. 9) = "-docpath=" then
                  Options.Doc_Directory := new String '(S (10 .. S'Last));
            elsif S'Length > 5 and then Is_Spec_File (S) then
               Load_LI_File
                 (Source_Info_List, Handler, Registry, S, LI_Unit);
               Source_File_Node.File_Name := new String'(S);
               Source_File_Node.Prj_File_Name := Prj_File_Name;
               Source_File_Node.Package_Name :=
                 new String'(Get_Unit_Name (LI_Unit, File_Name (S)));
               Source_File_Node.Other_File_Found := True;
               Type_Source_File_List.Append (Source_File_List,
                                             Source_File_Node);
            else
               raise Command_Line_Error;
            end if;
         end;
         J := J + 1;
      end loop;

      --  check if at least one source_file_name exist
      if Type_Source_File_List.Length (Source_File_List) = 0 then
         raise Command_Line_Error;
      end if;

   exception
      when Command_Line_Error =>
         --  check the remaining arguments if there's a help option
         --  somewhere. If so, translate this into a Help_Requested.

         J := J + 1;

         while J <= N loop
            declare
               S : constant String := Argument (J);
            begin
               exit when S = "-?"    or else S = "-h" or else
               S = "-help" or else S = "--help";
            end;
            J := J + 1;
         end loop;

         if J <= N then
            raise Help_Requested;
         else
            raise;
         end if;
   end Handle_Command_Line;

   --------------------
   -- Add_Body_Files --
   --------------------

   procedure Add_Body_Files
     (Source_File_List : in out Type_Source_File_List.List)
   is
      Source_File_Node : Type_Source_File_List.List_Node;
      New_Node         : Source_File_Information;
      New_Filename     : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
   begin
      Source_File_Node := TSFL.First (Source_File_List);

      for J in 1 .. Type_Source_File_List.Length (Source_File_List) loop
         --  ??? ADD: check if the body file really exists

         if TSFL.Data (Source_File_Node).Other_File_Found then

            New_Filename := new String'
              (Other_File_Name (TSFL.Data (Source_File_Node).File_Name.all));
            if Options.Info_Output then
               Put_Line (-"Adding file: " & New_Filename.all);
            end if;
            Load_LI_File
              (Source_Info_List, Handler, Registry, New_Filename.all, LI_Unit);
            New_Node.File_Name         :=
              new String'(New_Filename.all);
            New_Node.Prj_File_Name     :=
              new String'(TSFL.Data (Source_File_Node).Prj_File_Name.all);
            New_Node.Package_Name :=
              new String'(Get_Unit_Name
                            (LI_Unit, File_Name (New_Filename.all)));
            Type_Source_File_List.Append (Source_File_List, New_Node);
            Free (New_Filename);
         end if;

         Source_File_Node := TSFL.Next (Source_File_Node);
      end loop;

      if Options.Info_Output then
         Put_Line (-"The files in the file list are now: ");
         Source_File_Node := TSFL.First (Source_File_List);

         for J in 1 .. Type_Source_File_List.Length (Source_File_List) loop
            Put_Line (File_Name (TSFL.Data (Source_File_Node).File_Name.all));
            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
      end if;
   end Add_Body_Files;

   --------------------
   -- Get_Source_Dir --
   --------------------

   function Get_Source_Dir
     (Source_File_List   : TSFL.List) return String is
      --  returns the path of the first source file in the list

      Node : TSFL.List_Node;
   begin
      Node := TSFL.First (Source_File_List);
      return Dir_Name (TSFL.Data (Node).File_Name.all);
   end Get_Source_Dir;


begin --  DocGen

   --  get the language handler
   Handler := Create_Lang_Handler (Registry'Unchecked_Access);
   --  get Project_Tree and Project_View
   Reset (Source_Info_List);
   --  the project is loaded in Command Line, as soon as the project
   --  file is known. With this information the unit names of the other
   --  files can be obtained directly.

   Parse_Command_Line :
   declare
      Quit : Boolean;
   begin
      Quit := False;
      Handle_Command_Line (Quit);
      if Quit then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
         return;
      end if;
   end Parse_Command_Line;

   if Options.Info_Output then
      Put_Line (-"--------------------------------------------------------");
      Put_Line (-"--------------------------------------------------------");
      Put_Line (-"--------------------------------------------------------");
   end if;

   if not TSFL.Is_Empty (Source_File_List) then

      --  if Process_Body (-body) option was set, add all body
      --  files to the list
      if Options.Process_Body_Files then
         Add_Body_Files (Source_File_List);
      end if;

      --  if not path for the doc was set, take the path of the sources
      if Options.Doc_Directory = null then
         Options.Doc_Directory :=
           new String '(Get_Source_Dir (Source_File_List));
      end if;

      --  Process all files listed or all files from the project file
      Process_Files (Source_File_List,
                     Registry,
                     Handler,
                     Project,
                     Source_Info_List,
                     Options);

      TSFL.Free (Source_File_List);
   end if;

exception
   when Command_Line_Error =>
      New_Line (Current_Error);
      Put_Line (Current_Error,
                  -"Type ""docgen -?"" for more information.");

   when Project_Not_First =>
      New_Line (Current_Error);
      Put_Line (Current_Error, -"The project file "".gpr"" must be" &
                "first argument");

   when Help_Requested =>
      New_Line (Current_Error);
      Put_Line (Current_Error, -"NAME");
      New_Line (Current_Error);
      Put_Line (Current_Error, -"   Docgen - Ada95 documentation generator");
      New_Line (Current_Error);
      Put_Line (Current_Error, -"SYNOPSIS");
      New_Line (Current_Error);
      Put_Line (Current_Error, -"   docgen (-h | -help | --help | -?)");
      Put_Line (Current_Error, -("   docgen  .gpr-file  spec-file " &
                "{ spec-files }"));
      Put_Line (Current_Error, -("   [ -info ] [ -ic ] [ -above]" &
                "[ -private] [ -texi ] "));
      Put_Line (Current_Error, -"  [ -ref] [ -docpath=DIR ] [-linkall]");
      New_Line (Current_Error);
      Put_Line (Current_Error, -"DESCRIPTION");
      New_Line (Current_Error);
      New_Line (Current_Error);
      Put_Line (Current_Error, -"OPTIONS");
      New_Line (Current_Error);
      Put_Line (Current_Error, -"   -help     Shows information about Docgen");
      Put_Line (Current_Error, -("   -verbose  Gives further information" &
                " while processing"));
      Put_Line (Current_Error, -("   -body     Create also output files" &
                " for the body files"));
      Put_Line (Current_Error, -("   -ic       Comment starting with ""--!""" &
                " should be ignored "));
      Put_Line (Current_Error, -("   -above    use comments above" &
                " the entity headers "));
      Put_Line (Current_Error, -("   -private  Process also the entites" &
                " declared as Private"));
      Put_Line (Current_Error, -("   -texi     The output format should" &
                                 " be TexInfo."));
      Put_Line (Current_Error, -("   -onetexi  The output format should" &
                                 " be TexInfo."));
      Put_Line (Current_Error, -("             Here a file project.text is" &
                "  created and"));
      Put_Line (Current_Error, -("             the package files can be" &
                                 "  included."));
      Put_Line (Current_Error, -"   -ref      Search also for the references");
      Put_Line (Current_Error, -"   -linkall  Link even declaration file not" &
                " being processed");
      Put_Line (Current_Error, -("   -docpath=DIR   the subdirectory for the" &
                " doc files"));
      Put_Line (Current_Error, -("                  The last character is" &
                """/"" or ""\"""));
      New_Line (Current_Error);

end Docgen.Main;
