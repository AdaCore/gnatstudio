-----------------------------------------------------------------------
--                               G P S                               --
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

with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Language.Ada;              use Language.Ada;

package body Docgen.Texi_Output is

   package TEL  renames Type_Entity_List;
   package TSFL renames Type_Source_File_List;

   procedure Doc_TEXI_Create
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_TEXI_Open (File, Info);
         when Close_Info            => Doc_TEXI_Close (File, Info);
         when Subtitle_Info         => Doc_TEXI_Subtitle (File, Info);
         when Package_Desc_Info     => Doc_TEXI_Pack_Desc (File, Info);
         when With_Info             => Doc_TEXI_With (File, Info);
         when Package_Info          => Doc_TEXI_Package (File, Info);
         when Var_Info              => Doc_TEXI_Var (File, Info);
         when Entry_Info            => Doc_TEXI_Entry (File, Info);
         when Subprogram_Info       => Doc_TEXI_Subprogram (File, Info);
         when Type_Info             => Doc_TEXI_Type (File, Info);
         when Exception_Info        => Doc_TEXI_Exception (File, Info);
         when Unit_Index_Info       => Doc_TEXI_Unit_Index_Header (File, Info);
         when Subprogram_Index_Info => Doc_TEXI_Sub_Index_Header  (File, Info);
         when Type_Index_Info       => Doc_TEXI_Type_Index_Header (File, Info);
         when Index_Item_Info       => Doc_TEXI_Index_Item (File, Info);
         when End_Of_Index_Info     => Doc_TEXI_Index_End  (File, Info);
         when Body_Line_Info        => Doc_TEXI_Body (File, Info);
         when others => null;
      end case;

   end Doc_TEXI_Create;

   -------------------
   -- Doc_TEXI_Open --
   -------------------

   procedure Doc_TEXI_Open
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      procedure Write_Regular_Beginning;
      --  Write to the File a beginning of a regular texi file.
      --  This file can be used alone later.

      procedure Write_Not_Regular_Beginning;
      --  Write to File a short beginning of a text file which will
      --  be included by the project.texi file later.

      -----------------------------
      -- Write_Regular_Beginning --
      -----------------------------

      procedure Write_Regular_Beginning is
      begin
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "\input texinfo");
         Ada.Text_IO.Put_Line (File, "@c start of header");
         Ada.Text_IO.Put_Line (File, "@setfilename " &
                            Get_Doc_File_Name
                              (Info.Open_File.all,
                               "",
                                  ".info"));
         if Is_Spec_File (Info.Open_File.all) then
            Ada.Text_IO.Put_Line (File, "@settitle "
                                  & Info.Open_Title.all);
            Ada.Text_IO.Put_Line (File, "@c end of header");
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "@titlepage");
            Ada.Text_IO.Put_Line (File, "@title "
                                  & Info.Open_Title.all);
                     Ada.Text_IO.Put_Line (File, "@end titlepage");
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "@c    Node  Next Last  Father");
            Ada.Text_IO.Put_Line (File, "@node " &
                               Info.Open_Title.all &
                               ",     ,     , (dir)");
         else
            Ada.Text_IO.Put_Line (File, "@settitle Body of "
                                  & Info.Open_Title.all);
            Ada.Text_IO.Put_Line (File, "@c end of header");
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "@titlepage");
            Ada.Text_IO.Put_Line (File, "@title Body of "
                                  & Info.Open_Title.all);
            Ada.Text_IO.Put_Line (File, "@end titlepage");
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "@c    Node  Next Last  Father");
            Ada.Text_IO.Put_Line (File, "@node Body of " &
                               Info.Open_Title.all &
                               ",     ,     , (dir)");
         end if;
      end Write_Regular_Beginning;

      ---------------------------------
      -- Write_Not_Regular_Beginning --
      ---------------------------------

      procedure Write_Not_Regular_Beginning is
         Node : Type_Source_File_List.List_Node;
      begin
         Ada.Text_IO.Put_Line (File, "@c    Node  Next Last  Father");
         --  work on spec files
         if Is_Spec_File (Info.Open_File.all) then
            --  check if the last spec and no body files processed
            if Info.Open_Package_Next.all /= "" or
            not Info.Doc_Info_Options.Process_Body_Files then
               --  check if link to the unit index should be set
               if Info.Open_Package_Next.all = "" then
                  Ada.Text_IO.Put_Line (File, "@node " &
                                        Info.Open_Title.all & ", " &
                                        " Unit Index, " &
                                        Info.Open_Package_Prev.all & ", Top");
               else
                  Ada.Text_IO.Put_Line (File, "@node " &
                                        Info.Open_Title.all & ", " &
                                        Info.Open_Package_Next.all & ", " &
                                        Info.Open_Package_Prev.all & ", Top");
               end if;
            else
               --  check if the last spec and body files will be processed:
               --  link to the first body package
               Node := Type_Source_File_List.First (Info.Doc_File_List);
               Ada.Text_IO.Put_Line (File, "@node " &
                                     Info.Open_Title.all & ", Body of " &
                                     Type_Source_File_List.Data
                                       (Node).Package_Name.all & ", " &
                                     Info.Open_Package_Prev.all & ", Top");
            end if;
            Ada.Text_IO.Put_Line (File, "@chapter " &
                                  Info.Open_Title.all);
            --  work on body files
         else
            Ada.Text_IO.Put (File, "@node Body of " &
                             Info.Open_Title.all);
            if Info.Open_Package_Next.all /= "" then
               Ada.Text_IO.Put (File, ", Body of " &
                                Info.Open_Package_Next.all);
            else
               Ada.Text_IO.Put (File, ", Unit Index");
               --  Info.Open_Package_Next.all);
            end if;
            if Info.Open_Package_Prev.all /= "" then
               Ada.Text_IO.Put (File, ", Body of " &
                                Info.Open_Package_Prev.all & ", Top");
            else
               --  link to the last spec package
               Node := Type_Source_File_List.Last (Info.Doc_File_List);
               Ada.Text_IO.Put (File, ", " &
                                Type_Source_File_List.Data
                                       (Node).Package_Name.all & ", Top");
            end if;
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "@chapter Body of " &
                                  Info.Open_Title.all);
         end if;
      end Write_Not_Regular_Beginning;

   begin
      --  check if the beginning of the texi file should be written
      --  if not, the texi file can be included in the project.texi
      --  later and a project documentation can be created
      if Info.Doc_Info_Options.One_Doc_File then
         Write_Not_Regular_Beginning;

         --  if yes, the text file will have its own beginning and
         --  it will be possible to create a documentation of the
         --  package only.
      else
         Write_Regular_Beginning;
      end if;

      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Open;

   --------------------
   -- Doc_TEXI_Close --
   --------------------

   procedure Doc_TEXI_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      --  write "bye" if this doc file not for using in project.texi
      if Info.Doc_Info_Options.One_Doc_File then
         --  check if a body file => if to write the index's
         if Is_Spec_File (Info.Close_File_Name.all) then
            Ada.Text_IO.New_Line (File);
            Ada.Text_IO.Put_Line (File, "@printindex fn");
            Ada.Text_IO.Put_Line (File, "@printindex tp");
         end if;
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "@bye");
      end if;
   end Doc_TEXI_Close;

   -----------------------
   -- Doc_TEXI_Subtitle --
   -----------------------

   procedure Doc_TEXI_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

   begin
      --  if the order of the information is changed in Docgen-Work_On_Source:
      --  Process_Source, this contents of the strings must be changed too!
      --  and replace the last type also in Doc_TEXI_Close

      --  the current implemented order of the subtitles like given in
      --  Docgen-Work_On_Source.Process_Source is:
      --
      --  Package Description
      --  With Clause
      --  Packgages
      --  Variables
      --  Exceptions
      --  Types
      --  Entries
      --  Subprograms

      Ada.Text_IO.Put_Line (File, "@c -------------------- SECTION " &
                            "-------------------");

      case Info.Subtitle_Kind is
         when Package_Desc_Info     =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                    "_description, " &
                                    Info.Subtitle_Package.all &
                                    "_withs, " &
                                    Info.Subtitle_Package.all &
                                    ", " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                    "_description");
         when With_Info             =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                     "_withs, " &
                                    Info.Subtitle_Package.all &
                                    "_packages, " &
                                    Info.Subtitle_Package.all &
                                    "_description, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                     "_withs");
         when Package_Info          =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_packages, " &
                                    Info.Subtitle_Package.all &
                                  "_vars, " &
                                    Info.Subtitle_Package.all &
                                  "_withs, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_packages");
         when Var_Info              =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_vars, " &
                                    Info.Subtitle_Package.all &
                                  "_exceptions, " &
                                    Info.Subtitle_Package.all &
                                  "_packages, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_vars");
         when Exception_Info        =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_exceptions, " &
                                    Info.Subtitle_Package.all &
                                  "_types, " &
                                    Info.Subtitle_Package.all &
                                  "_vars, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_exceptions");
         when Type_Info             =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_types, " &
                                    Info.Subtitle_Package.all &
                                  "_entries, " &
                                  Info.Subtitle_Package.all &
                                    "_exceptions, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_types");
         when Entry_Info             =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_entries, " &
                                    Info.Subtitle_Package.all &
                                  "_subprograms, " &
                                  Info.Subtitle_Package.all &
                                    "_exceptions, " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_entries");
         when Subprogram_Info       =>
            Ada.Text_IO.Put_Line (File, "@node " &
                                    Info.Subtitle_Package.all &
                                  "_subprograms,   , " &
                                    Info.Subtitle_Package.all &
                                      "_entries,   , " &
                                    Info.Subtitle_Package.all);
            Ada.Text_IO.Put_Line (File, "@section " &
                                    Info.Subtitle_Package.all &
                                  "_subprograms");
         when others => null;
      end case;

      Ada.Text_IO.Put_Line (File, "@c ----------------------" &
                            "--------------------------");
      Ada.Text_IO.New_Line (File);

   end Doc_TEXI_Subtitle;

   ------------------------
   -- Doc_TEXI_Pack_Desc --
   ------------------------

   procedure Doc_TEXI_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, Info.Package_Desc_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Pack_Desc;

   ----------------------
   -- Doc_TEXI_Package --
   ----------------------

   procedure Doc_TEXI_Package
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      if Info.Package_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");

      Format_TEXI (File,
                   Info.Package_Header.all,
                   Info.Package_Entity.File_Name.all,
                   Info.Package_Entity.Short_Name.all,
                   Info.Package_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "}");

      Ada.Text_IO.Put_Line (File, Info.Package_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Package;

   -------------------
   -- Doc_TEXI_With --
   -------------------

   procedure Doc_TEXI_With
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.With_Header.all,
                   Info.With_File.all,
                   "",
                   0,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   False);
      Ada.Text_IO.Put_Line (File, "}");
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_With;

   ------------------
   -- Doc_TEXI_Var --
   ------------------

   procedure Doc_TEXI_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      if Info.Var_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.Var_Header.all,
                   Info.Var_Entity.File_Name.all,
                   Info.Var_Entity.Short_Name.all,
                   Info.Var_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
      Ada.Text_IO.Put_Line (File, "}");
      Ada.Text_IO.Put_Line (File, Info.Var_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Var;

   ------------------------
   -- Doc_TEXI_Exception --
   ------------------------

   procedure Doc_TEXI_Exception
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      if Info.Exception_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.Exception_Header.all,
                   Info.Exception_Entity.File_Name.all,
                   Info.Exception_Entity.Short_Name.all,
                   Info.Exception_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
      Ada.Text_IO.Put_Line (File, "}");
      Ada.Text_IO.Put_Line (File, Info.Exception_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Exception;

   -------------------
   -- Doc_TEXI_Type --
   -------------------

   procedure Doc_TEXI_Type
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "@dindex " &
                            Info.Type_Entity.Name.all);
      if Info.Type_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.Type_Header.all,
                   Info.Type_Entity.File_Name.all,
                   Info.Type_Entity.Short_Name.all,
                   Info.Type_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
      Ada.Text_IO.Put_Line (File, "}");
      Ada.Text_IO.Put_Line (File, Info.Type_Description.all);
      Ada.Text_IO.New_Line (File);
   end Doc_TEXI_Type;

   --------------------
   -- Doc_TEXI_Entry --
   --------------------

   procedure Doc_TEXI_Entry
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      if Info.Entry_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.Entry_Header.all,
                   Info.Entry_Entity.File_Name.all,
                   Info.Entry_Entity.Short_Name.all,
                   Info.Entry_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
      Ada.Text_IO.Put_Line (File, "@code}");
      Ada.Text_IO.Put_Line (File, Info.Entry_Description.all);
   end Doc_TEXI_Entry;

   -------------------------
   -- Doc_TEXI_Subprogram --
   -------------------------

   procedure Doc_TEXI_Subprogram
     (File      : in Ada.Text_IO.File_Type;
      Info      : Doc_Info) is

      package TRL renames Type_Reference_List;

      procedure Print_Ref_List
        (Local_List    : TRL.List;
         Called_Subp   : Boolean);
      --  Processes the Ref_List to the output file.
      --  If Called_Subp is True, the list of the subprograms
      --  calling the current subprogram will be printed,
      --  if False, the list of the subprograms called within it.

      ----------------------
      --  Print_Ref_List  --
      ----------------------

      procedure Print_Ref_List
        (Local_List    : TRL.List;
         Called_Subp   : Boolean) is
         Node      : TRL.List_Node;
         Suffix    : GNAT.OS_Lib.String_Access;
      begin
         if not TRL.Is_Empty (Local_List) then
            if Called_Subp then
               Ada.Text_IO.Put_Line (File,
                                     "@strong{Subprogram is called by:}");
            else
               Ada.Text_IO.Put_Line (File,
                                     "@strong{Subprogram calles:}");
            end if;
            Ada.Text_IO.Put_Line (File, "@itemize @bullet");

            Node := TRL.First (Local_List);

            --  for every reference found write the information to doc file
            for J in 1 .. TRL.Length (Local_List) loop

               Ada.Text_IO.Put_Line (File, "@item");

               --  check if the creating of a link is possible
               --  ??? right now no links are set, so this "if"
               --  is not needed, but this can be changed.
               if TRL.Data (Node).Set_Link then

                  --  if a called subprogram => link to spec
                  if Called_Subp then
                     Suffix :=
                       new String'("_" &
                                   Body_Suffix (TRL.Data (Node).File_Name.all)
                                   & ".htm");
                  else
                     Suffix :=
                       new String'("_" &
                                   Spec_Suffix (TRL.Data (Node).File_Name.all)
                                   & ".htm");
                  end if;

                  Ada.Text_IO.Put_Line
                    (File,
                     "@code{"
                     & TRL.Data
                       (Node).Subprogram_Name.all
                     & "}   in   @file{"
                     & TRL.Data (Node).File_Name.all
                     & "},   line: "
                     & Integer'Image (TRL.Data (Node).Line)
                     & ",   column: "
                     & Integer'Image (TRL.Data (Node).Column));
               --  no link at all
               else Ada.Text_IO.Put_Line
                       (File,
                        "@code{"
                        & TRL.Data
                          (Node).Subprogram_Name.all
                        & "}   in   @file{"
                        & TRL.Data (Node).File_Name.all
                        & "},   line: "
                        & Integer'Image (TRL.Data (Node).Line)
                        & ",   column: "
                        & Integer'Image (TRL.Data (Node).Column));
               end if;
               Node := TRL.Next (Node);
            end loop;
               Ada.Text_IO.Put_Line (File, "@end itemize");
         end if;
      end Print_Ref_List;

   begin  --  Doc_TEXI_Subprogram

      if Info.Subprogram_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.Subprogram_Header.all,
                   Info.Subprogram_Entity.File_Name.all,
                   Info.Subprogram_Entity.Short_Name.all,
                   Info.Subprogram_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
      Ada.Text_IO.Put_Line (File, "}");
      --  write the description to doc file
      Ada.Text_IO.Put_Line (File, Info.Subprogram_Description.all);
      Print_Ref_List (Info.Subprogram_Entity.Called_List, True);
      Print_Ref_List (Info.Subprogram_Entity.Calls_List, False);
   end Doc_TEXI_Subprogram;

   -----------------
   -- Format_TEXI --
   -----------------

   procedure Format_TEXI
     (File          : Ada.Text_IO.File_Type;
      Text          : String;
      File_Name     : String;
      Entity_Name   : String;
      Entity_Line   : Natural;
      Is_Body       : Boolean;
      Process_Body  : Boolean;
      Do_Checks     : Boolean) is
      pragma Unreferenced (File_Name, Entity_Name, Entity_Line,
                           Is_Body, Process_Body, Do_Checks);

      --  global variables for the callback function
      Last_Index, Last_Line : Natural;

      function TEXI_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  the callback function to add TEXI code to the text being parsed.
      --  for each possible kind of parsed entity some TEXI code will
      --  be added to the Result_Line and the Already_Added_Chars
      --  increased by the number of the added chars in order to keep
      --  track of the current position in the original string.

      -------------------
      -- TEXI_Callback --
      -------------------

      function TEXI_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is
         pragma Unreferenced (Partial_Entity);

         TEXI_Comment_Prefix    : constant String := "";
         TEXI_Comment_Suffix    : constant String := "";
         TEXI_Keyword_Prefix    : constant String := "@strong{";
         TEXI_Keyword_Suffix    : constant String := "}";
         TEXI_String_Prefix     : constant String := "";
         TEXI_String_Suffix     : constant String := "";
         TEXI_Char_Prefix       : constant String := "";
         TEXI_Char_Suffix       : constant String := "";
         TEXI_Identifier_Prefix : constant String := "@emph{";
         TEXI_Identifier_Suffix : constant String := "}";

         procedure Callback_Output
           (Start_Index : Natural;
            End_Index   : Natural;
            Prefix     : String;
            Suffix     : String;
            Check_Tags : Boolean);
         --  Write the formatted text since the last output to doc file
         --  Prefix and Suffix are the TEXI code to be put around the
         --  parsed entity. The both index values are needed, as for comment
         --  lines the ASCII.LF at the line should be ignored, so you can't
         --  used always the Sloc_Index values.

         procedure Replace_TEXI_Tags
           (Input_Text : String);
         --  replaces all "@"  which are by "@@"
         --  and writes the output to the doc file.

         ---------------------
         -- Callback_Output --
         ---------------------

         procedure Callback_Output
           (Start_Index : Natural;
            End_Index   : Natural;
            Prefix      : String;
            Suffix      : String;
            Check_Tags  : Boolean) is
         begin
            Ada.Text_IO.Put (File, Text (Last_Index .. Start_Index - 1));

            if Check_Tags then
               Ada.Text_IO.Put (File, Prefix);
               Replace_TEXI_Tags (Text (Start_Index .. End_Index));
               Ada.Text_IO.Put (File, Suffix);
            else
               Ada.Text_IO.Put (File,
                                Prefix &
                                  Text (Start_Index .. End_Index) &
                                  Suffix);
            end if;
            Last_Index := End_Index + 1;
            Last_Line  := Sloc_End.Line;
         end Callback_Output;

         -----------------------
         -- Replace_TEXI_Tags --
         -----------------------

         procedure Replace_TEXI_Tags
           (Input_Text : String) is

            Last_Index : Natural;
         begin
            Last_Index := Input_Text'First;

            for J in Input_Text'First .. Input_Text'Last - 1 loop
               if Input_Text (J) = '@' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "@@");
                  Last_Index := J + 1;
               end if;
            end loop;
            Ada.Text_IO.Put (File, Input_Text (Last_Index .. Input_Text'Last));
         end Replace_TEXI_Tags;

      begin  -- TEXI_Callback

         case Entity is
            when Comment_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index - 1,
                  TEXI_Comment_Prefix, TEXI_Comment_Suffix, False);
            when Keyword_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  TEXI_Keyword_Prefix, TEXI_Keyword_Suffix, False);
            when String_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  TEXI_String_Prefix, TEXI_String_Suffix, True);
            when Character_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  TEXI_Char_Prefix, TEXI_Char_Suffix, False);
            when Identifier_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  TEXI_Identifier_Prefix, TEXI_Identifier_Suffix, False);
            when others => null;
         end case;
         return False;
      end TEXI_Callback;

   begin  --  Format_TEXI

      --  set the global varibales
      Last_Index := 1;
      Last_Line  := 0;

      --  parse the entities in Text
      Parse_Entities (Ada_Lang,
                      Text,
                      TEXI_Callback'Unrestricted_Access);

      --  write the rest of the text, since the last found parsed entity
      if Last_Index < Text'Last then
         Ada.Text_IO.Put (File, Text (Last_Index .. Text'Last));
      end if;
   end Format_TEXI;

   --------------------------------
   -- Doc_TEXI_Unit_Index_Header --
   --------------------------------

   procedure Doc_TEXI_Unit_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Node  : TSFL.List_Node;

      procedure Write_List_Of_Files
        (File      : Ada.Text_IO.File_Type;
         Doc_Specs : Boolean);
      --  Writes to the doc file a list of all files from the list being
      --  either spec or body files, as determined by Doc_Specs.

      procedure Create_TEXI_Project_Doc_File;
      --  Create the project.texi file containing the documentation of
      --  all processed files of the project by useing @include.

      -------------------------
      -- Write_List_Of_Files --
      -------------------------

      procedure Write_List_Of_Files
        (File      : Ada.Text_IO.File_Type;
         Doc_Specs : Boolean) is
      begin
         Node := TSFL.First (Info.Unit_File_List);
         for J in 1 .. TSFL.Length (Info.Unit_File_List) loop
            if (Is_Spec_File (TSFL.Data (Node).File_Name.all) and
                  Doc_Specs) or
              (not Is_Spec_File (TSFL.Data (Node).File_Name.all) and
               not Doc_Specs) then
               Ada.Text_IO.Put_Line
                 (File,
                  "@include " &
                  Base_Name
                    (Get_Doc_File_Name
                       (TSFL.Data
                          (Node).File_Name.all,
                        Info.Doc_Info_Options.Doc_Directory.all,
                        Info.Doc_Info_Options.Doc_Suffix.all)));
            end if;
            Node := TSFL.Next (Node);
         end loop;
      end Write_List_Of_Files;

      ----------------------------------
      -- Create_TEXI_Project_Doc_File --
      ----------------------------------

      procedure Create_TEXI_Project_Doc_File is
         Project_Doc_File : File_Type;
      begin
         --  create the file
         Create (Project_Doc_File,
                 Out_File,
                 Info.Doc_Info_Options.Doc_Directory.all & "project.texi");

         --  create the header
         Ada.Text_IO.New_Line (Project_Doc_File);
         Ada.Text_IO.Put_Line (Project_Doc_File, "\input texinfo");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@c start of header");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@setfilename "
                            & "project.info");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@settitle Project "
                            & Info.Unit_Project_Name.all);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@c end of header");
         Ada.Text_IO.New_Line (Project_Doc_File);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@titlepage");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@title Project "
                            & Info.Unit_Project_Name.all);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@end titlepage");
         Ada.Text_IO.New_Line (Project_Doc_File);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@ifinfo");
         Node := TSFL.First (Info.Unit_File_List);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@node Top, " &
                               TSFL.Data (Node).Package_Name.all &
                               ",   , (dir)");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@top Master Menu");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@end ifinfo");
         Ada.Text_IO.New_Line (Project_Doc_File);

         --  include all files wanted
         --  ???
         Write_List_Of_Files (Project_Doc_File, True);
         if Info.Doc_Info_Options.Process_Body_Files then
            Ada.Text_IO.New_Line (Project_Doc_File);
            Write_List_Of_Files (Project_Doc_File, False);
         end if;

         Ada.Text_IO.New_Line (Project_Doc_File);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@include index_unit.texi");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@include index_sub.texi");
         Ada.Text_IO.Put_Line (Project_Doc_File, "@include index_type.texi");

         Ada.Text_IO.New_Line (Project_Doc_File);
         Ada.Text_IO.Put_Line (Project_Doc_File, "@bye");
         Close (Project_Doc_File);
      end Create_TEXI_Project_Doc_File;

   begin   --  Doc_TEXI_Unit_Index_Header

      --  check if the project.texi file should also be created
      --  and if the index file will be included later
      if Info.Doc_Info_Options.One_Doc_File then
         Create_TEXI_Project_Doc_File;
         Node := TSFL.Last (Info.Unit_File_List);
         Ada.Text_IO.Put (File, "@node Unit Index, Subprogram Index, ");
         if Info.Doc_Info_Options.Process_Body_Files then
            Ada.Text_IO.Put (File, "Body of " &
                             TSFL.Data (Node).Package_Name.all);
         else
            Ada.Text_IO.Put (File, TSFL.Data (Node).Package_Name.all);
         end if;
         Ada.Text_IO.Put_Line (File, ",  Top");
         Ada.Text_IO.Put_Line (File, "@chapter Unit Index");
      else
         --  create the header for the self-standing unit index file
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "\input texinfo");
         Ada.Text_IO.Put_Line (File, "@c start of header");
         Ada.Text_IO.Put_Line (File, "@setfilename unit_index.info");
         Ada.Text_IO.Put_Line (File, "@settitle "
                            & "Unit Index");
         Ada.Text_IO.Put_Line (File, "@c end of header");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "@titlepage");
         Ada.Text_IO.Put_Line (File, "@title "
                               & "Unit Index");
         Ada.Text_IO.Put_Line (File, "@end titlepage");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "@node Unit Index,  ,   , (dir)");
      end if;
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "@itemize @bullet");
   end Doc_TEXI_Unit_Index_Header;

   --------------------------------
   -- Doc_TEXI_Sub_Index_Header --
   --------------------------------

   procedure Doc_TEXI_Sub_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin

      --  check if the project.texi file should also be created
      --  and if the index file will be included later
      if Info.Doc_Info_Options.One_Doc_File then
         Ada.Text_IO.Put_Line (File, "@node Subprogram Index,  Type Index," &
                               " Unit Index,  Top");
         Ada.Text_IO.Put_Line (File, "@chapter Subprogram Index");
      else
         --  create the header for the self-standing unit index file
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "\input texinfo");
         Ada.Text_IO.Put_Line (File, "@c start of header");
         Ada.Text_IO.Put_Line (File, "@setfilename sub_index.info");
         Ada.Text_IO.Put_Line (File, "@settitle "
                            & "Subprogram Index");
         Ada.Text_IO.Put_Line (File, "@c end of header");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "@titlepage");
         Ada.Text_IO.Put_Line (File, "@title "
                            & "Subprogram Index");
         Ada.Text_IO.Put_Line (File, "@end titlepage");
      end if;

      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "@itemize @bullet");
   end Doc_TEXI_Sub_Index_Header;

   --------------------------------
   -- Doc_TEXI_Type_Index_Header --
   --------------------------------

   procedure Doc_TEXI_Type_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin

      --  check if the project.texi file should also be created
      --  and if the index file will be included later
      if Info.Doc_Info_Options.One_Doc_File then
         Ada.Text_IO.Put_Line (File,
                               "@node Type Index,   , Subprogram Index, Top");
         Ada.Text_IO.Put_Line (File, "@chapter Type Index");
      else
         --  create the header for the self-standing unit index file
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "\input texinfo");
         Ada.Text_IO.Put_Line (File, "@c start of header");
         Ada.Text_IO.Put_Line (File, "@setfilename type_index.info");
         Ada.Text_IO.Put_Line (File, "@settitle "
                            & "Type Index");
         Ada.Text_IO.Put_Line (File, "@c end of header");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "@titlepage");
         Ada.Text_IO.Put_Line (File, "@title "
                            & "Type Index");
         Ada.Text_IO.Put_Line (File, "@end titlepage");
         Ada.Text_IO.New_Line (File);
         Ada.Text_IO.Put_Line (File, "@c    Node  Next Last  Father");
         Ada.Text_IO.Put_Line (File, "@node Top,   ,   ,   (dir)");
      end if;

      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "@itemize @bullet");
   end Doc_TEXI_Type_Index_Header;

   -------------------------
   -- Doc_TEXI_Index_Item --
   -------------------------

   procedure Doc_TEXI_Index_Item
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "@item");
      Ada.Text_IO.Put (File, Info.Item_Name.all);
      Ada.Text_IO.Put_Line (File, "     in @file{" &
                            Info.Item_File.all & "}");
   end Doc_TEXI_Index_Item;

   ------------------------
   -- Doc_TEXI_Index_End --
   ------------------------

   procedure Doc_TEXI_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin
      Ada.Text_IO.Put_Line (File, "@end itemize");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "@bye");
   end Doc_TEXI_Index_End;

   -------------------
   -- Doc_TEXI_Body --
   -------------------

   procedure Doc_TEXI_Body
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File,
                   Info.Body_Text.all,
                   Info.Body_File.all,
                   "",
                   0,
                   True,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
      Ada.Text_IO.Put_Line (File, "}");
   end Doc_TEXI_Body;

   ------------------------
   -- Get_TEXI_File_Name --
   ------------------------

   function Get_Texi_File_Name
     (File : String) return String is
   begin
      if Is_Spec_File (File) then
         return File_Name_Without_Suffix (File) & "_" &
                Spec_Suffix (File) & ".texi";
      else
         return File_Name_Without_Suffix (File) & "_" &
                Body_Suffix (File) & ".texi";
      end if;
   end Get_Texi_File_Name;

end Docgen.Texi_Output;
