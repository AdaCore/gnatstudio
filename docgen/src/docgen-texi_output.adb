-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Glide_Kernel;              use Glide_Kernel;
with Src_Info.Queries;          use Src_Info.Queries;
with VFS;                       use VFS;
with Projects;                  use Projects;

package body Docgen.Texi_Output is

   package TEL  renames Type_Entity_List;
   package TSFL renames Type_Source_File_List;

   procedure Doc_TEXI_Open
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Called each time a new file has been created

   procedure Doc_TEXI_Close
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Called each time the file should be closed

   procedure Doc_TEXI_Subtitle
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subtitle for the entity type to the documentation

   procedure Doc_TEXI_Entry
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add aa entry or entry family to the documentation

   procedure Doc_TEXI_Subprogram
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a subprogram to the documentation

   procedure Doc_TEXI_Pack_Desc
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the package description to the documentation

   procedure Doc_TEXI_Package
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the renamed and instantiated package to the documentation

   procedure Doc_TEXI_With
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the dependencies to the documentation

   procedure Doc_TEXI_Var
     (File    : Ada.Text_IO.File_Type;
      Info    : Doc_Info);
   --  Add a constant or named number to the documentation

   procedure Doc_TEXI_Exception
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an exception to the documentation

   procedure Doc_TEXI_Type
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add a type to the documentation

   procedure Format_TEXI
     (File          : Ada.Text_IO.File_Type;
      Text          : String);
   --  Formated Text as TEXI code and write to the docfile.

   procedure Doc_TEXI_Unit_Index_Header
     (Kernel        : access Kernel_Handle_Record'Class;
      File          : Ada.Text_IO.File_Type;
      Info          : Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String);
   --  Create the header of the index of all packages
   --  and also create the whole index.htm for the frames

   procedure Doc_TEXI_Sub_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all subprograms

   procedure Doc_TEXI_Type_Index_Header
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Create the header of the index of all types

   procedure Doc_TEXI_Index_Item
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add an item to an index, used for all 3 index types

   procedure Doc_TEXI_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info);
   --  Add the footer to the index, used for all 3 indes files

   procedure Doc_TEXI_Body
     (File   : Ada.Text_IO.File_Type;
      Info   : in out Doc_Info);
   --  Format the body by calling Format_TEXI for the whole body file
   --  and write it to the doc file

   ---------------------
   -- Doc_TEXI_Create --
   ---------------------

   procedure Doc_TEXI_Create
     (Kernel        : access Glide_Kernel.Kernel_Handle_Record'Class;
      File          : in Ada.Text_IO.File_Type;
      Info          : in out Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_TEXI_Open (Kernel, File, Info);
         when Close_Info            => Doc_TEXI_Close (Kernel, File, Info);
         when Subtitle_Info         => Doc_TEXI_Subtitle (File, Info);
         when Package_Desc_Info     => Doc_TEXI_Pack_Desc (File, Info);
         when With_Info             => Doc_TEXI_With (File, Info);
         when Package_Info          => Doc_TEXI_Package (File, Info);
         when Var_Info              => Doc_TEXI_Var (File, Info);
         when Entry_Info            => Doc_TEXI_Entry (File, Info);
         when Subprogram_Info       =>
            Doc_TEXI_Subprogram (Kernel, File, Info);
         when Type_Info             => Doc_TEXI_Type (File, Info);
         when Exception_Info        => Doc_TEXI_Exception (File, Info);
         when Unit_Index_Info       => Doc_TEXI_Unit_Index_Header
              (Kernel, File, Info, Doc_Directory, Doc_Suffix);
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
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info)
   is
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
         New_Line (File);
         Put_Line (File, "\input texinfo");
         Put_Line (File, "@c start of header");
         Put_Line (File, "@setfilename " &
                   Get_Doc_File_Name (Info.Open_File, "", ".info"));
         if Is_Spec_File (Kernel, Info.Open_File) then
            Put_Line (File, "@settitle " & Info.Open_Title.all);
            Put_Line (File, "@c end of header");
            New_Line (File);
            Put_Line (File, "@titlepage");
            Put_Line (File, "@title " & Info.Open_Title.all);
            Put_Line (File, "@end titlepage");
            New_Line (File);
            Put_Line (File, "@c    Node  Next Last  Father");
            Put_Line (File, "@node " & Info.Open_Title.all &
                      ",     ,     , (dir)");
         else
            Put_Line (File, "@settitle Body of " & Info.Open_Title.all);
            Put_Line (File, "@c end of header");
            New_Line (File);
            Put_Line (File, "@titlepage");
            Put_Line (File, "@title Body of " & Info.Open_Title.all);
            Put_Line (File, "@end titlepage");
            New_Line (File);
            Put_Line (File, "@c    Node  Next Last  Father");
            Put_Line (File, "@node Body of " & Info.Open_Title.all &
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
         if Is_Spec_File (Kernel, Info.Open_File) then
            --  check if the last spec and no body files processed
            if Info.Open_Package_Next.all /= "" or
            not Info.Doc_Info_Options.Process_Body_Files then
               --  check if link to the unit index should be set
               if Info.Open_Package_Next.all = "" then
                  Put_Line (File, "@node " &
                            Info.Open_Title.all & ", " &
                            " Unit Index, " &
                            Info.Open_Package_Prev.all & ", Top");
               else
                  Put_Line (File, "@node " &
                            Info.Open_Title.all & ", " &
                            Info.Open_Package_Next.all & ", " &
                            Info.Open_Package_Prev.all & ", Top");
               end if;
            else
               --  check if the last spec and body files will be processed:
               --  link to the first body package
               Node := Type_Source_File_List.First (Info.Doc_File_List);
               Put_Line (File, "@node " &
                         Info.Open_Title.all & ", Body of " &
                         Type_Source_File_List.Data
                           (Node).Package_Name.all & ", " &
                         Info.Open_Package_Prev.all & ", Top");
            end if;
            Put_Line (File, "@chapter " & Info.Open_Title.all);
            --  work on body files

         else
            Put (File, "@node Body of " & Info.Open_Title.all);

            if Info.Open_Package_Next.all /= "" then
               Put (File, ", Body of " & Info.Open_Package_Next.all);
            else
               Put (File, ", Unit Index");
               --  Info.Open_Package_Next.all);
            end if;

            if Info.Open_Package_Prev.all /= "" then
               Put (File, ", Body of " &
                    Info.Open_Package_Prev.all & ", Top");
            else
               --  link to the last spec package
               Node := Type_Source_File_List.Last (Info.Doc_File_List);
               Put (File, ", " &
                    Type_Source_File_List.Data
                      (Node).Package_Name.all & ", Top");
            end if;
            New_Line (File);
            Put_Line (File, "@chapter Body of " & Info.Open_Title.all);
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

      New_Line (File);
   end Doc_TEXI_Open;

   --------------------
   -- Doc_TEXI_Close --
   --------------------

   procedure Doc_TEXI_Close
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      --  write "bye" if this doc file not for using in project.texi
      if Info.Doc_Info_Options.One_Doc_File then
         --  check if a body file => if to write the index's
         if Is_Spec_File (Kernel, Info.Close_File_Name) then
            New_Line (File);
            Put_Line (File, "@printindex fn");
            Put_Line (File, "@printindex tp");
         end if;
         New_Line (File);
         Put_Line (File, "@bye");
      end if;
   end Doc_TEXI_Close;

   -----------------------
   -- Doc_TEXI_Subtitle --
   -----------------------

   procedure Doc_TEXI_Subtitle
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
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

      Put_Line (File, "@c -------------------- SECTION -------------------");

      case Info.Subtitle_Kind is
         when Package_Desc_Info     =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all &
               "_description, " & Info.Subtitle_Package.all &
               "_withs, " & Info.Subtitle_Package.all &
               ", " & Info.Subtitle_Package.all);
            Put_Line
              (File, "@section " & Info.Subtitle_Package.all & "_description");

         when With_Info             =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all &
               "_withs, " & Info.Subtitle_Package.all &
               "_packages, " & Info.Subtitle_Package.all &
               "_description, " & Info.Subtitle_Package.all);
            Put_Line
              (File, "@section " & Info.Subtitle_Package.all & "_withs");

         when Package_Info          =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all &
               "_packages, " & Info.Subtitle_Package.all &
               "_vars, " & Info.Subtitle_Package.all &
               "_withs, " & Info.Subtitle_Package.all);
            Put_Line
              (File, "@section " & Info.Subtitle_Package.all & "_packages");

         when Var_Info              =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all &
               "_vars, " & Info.Subtitle_Package.all &
               "_exceptions, " & Info.Subtitle_Package.all &
               "_packages, " & Info.Subtitle_Package.all);
            Put_Line (File, "@section " & Info.Subtitle_Package.all & "_vars");

         when Exception_Info        =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all &
               "_exceptions, " & Info.Subtitle_Package.all &
               "_types, " & Info.Subtitle_Package.all &
               "_vars, " & Info.Subtitle_Package.all);
            Put_Line
              (File, "@section " & Info.Subtitle_Package.all & "_exceptions");

         when Type_Info             =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all &
               "_types, " & Info.Subtitle_Package.all &
               "_entries, " & Info.Subtitle_Package.all &
               "_exceptions, " & Info.Subtitle_Package.all);
            Put_Line
              (File, "@section " & Info.Subtitle_Package.all & "_types");

         when Entry_Info             =>
            Put_Line
              (File, "@node " & Info.Subtitle_Package.all
               & "_entries, " & Info.Subtitle_Package.all
               & "_subprograms, " & Info.Subtitle_Package.all
               & "_exceptions, " & Info.Subtitle_Package.all);
            Put_Line
              (File, "@section " & Info.Subtitle_Package.all & "_entries");

         when Subprogram_Info       =>
            Put_Line
              (File, "@node "       & Info.Subtitle_Package.all &
               "_subprograms,   , " & Info.Subtitle_Package.all &
               "_entries,   , "     & Info.Subtitle_Package.all);
            Put_Line
              (File,
               "@section " & Info.Subtitle_Package.all & "_subprograms");
         when others => null;
      end case;

      Put_Line
        (File, "@c ------------------------------------------------");
      New_Line (File);
   end Doc_TEXI_Subtitle;

   ------------------------
   -- Doc_TEXI_Pack_Desc --
   ------------------------

   procedure Doc_TEXI_Pack_Desc
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, Info.Package_Desc_Description.all);
      New_Line (File);
   end Doc_TEXI_Pack_Desc;

   ----------------------
   -- Doc_TEXI_Package --
   ----------------------

   procedure Doc_TEXI_Package
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      if Info.Package_Entity.Is_Private then
         Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Put_Line (File, "@code{");
      Format_TEXI (File, Info.Package_Header.all);
      Put_Line (File, "}");
      Put_Line (File, Info.Package_Description.all);
      New_Line (File);
   end Doc_TEXI_Package;

   -------------------
   -- Doc_TEXI_With --
   -------------------

   procedure Doc_TEXI_With
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, "@code{");
      Format_TEXI (File, Info.With_Header.all);
      Put_Line (File, "}");
      New_Line (File);
   end Doc_TEXI_With;

   ------------------
   -- Doc_TEXI_Var --
   ------------------

   procedure Doc_TEXI_Var
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      if Info.Var_Entity.Is_Private then
         Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Put_Line (File, "@code{");
      Format_TEXI (File, Info.Var_Header.all);
      Put_Line (File, "}");
      Put_Line (File, Info.Var_Description.all);
      New_Line (File);
   end Doc_TEXI_Var;

   ------------------------
   -- Doc_TEXI_Exception --
   ------------------------

   procedure Doc_TEXI_Exception
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      if Info.Exception_Entity.Is_Private then
         Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Put_Line (File, "@code{");
      Format_TEXI (File, Info.Exception_Header.all);
      Put_Line (File, "}");
      Put_Line (File, Info.Exception_Description.all);
      New_Line (File);
   end Doc_TEXI_Exception;

   -------------------
   -- Doc_TEXI_Type --
   -------------------

   procedure Doc_TEXI_Type
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      Put_Line (File, "@dindex " & Info.Type_Entity.Name.all);
      if Info.Type_Entity.Is_Private then
         Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Put_Line (File, "@code{");
      Format_TEXI (File, Info.Type_Header.all);
      Put_Line (File, "}");
      Put_Line (File, Info.Type_Description.all);
      New_Line (File);
   end Doc_TEXI_Type;

   --------------------
   -- Doc_TEXI_Entry --
   --------------------

   procedure Doc_TEXI_Entry
     (File : in Ada.Text_IO.File_Type; Info : Doc_Info) is
   begin
      if Info.Entry_Entity.Is_Private then
         Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;
      Ada.Text_IO.Put_Line (File, "@code{");
      Format_TEXI (File, Info.Entry_Header.all);
      Put_Line (File, "@code}");
      Put_Line (File, Info.Entry_Description.all);
   end Doc_TEXI_Entry;

   -------------------------
   -- Doc_TEXI_Subprogram --
   -------------------------

   procedure Doc_TEXI_Subprogram
     (Kernel : access Kernel_Handle_Record'Class;
      File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info)
   is
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
         Called_Subp   : Boolean)
      is
         Node      : TRL.List_Node;
         Suffix    : GNAT.OS_Lib.String_Access;
         pragma Unreferenced (Suffix);
      begin
         if not TRL.Is_Empty (Local_List) then
            if Called_Subp then
               Put_Line (File, "@strong{Subprogram is called by:}");
            else
               Put_Line (File, "@strong{Subprogram calles:}");
            end if;

            Put_Line (File, "@itemize @bullet");

            Node := TRL.First (Local_List);

            --  for every reference found write the information to doc file
            for J in 1 .. TRL.Length (Local_List) loop
               Put_Line (File, "@item");

               --  check if the creating of a link is possible
               --  ??? right now no links are set, so this "if"
               --  is not needed, but this can be changed.
               if TRL.Data (Node).Set_Link then

                  --  if a called subprogram => link to spec
                  if Called_Subp then
                     Suffix := new String'
                       ("_" & Body_Suffix
                          (Kernel,
                           Get_Declaration_File_Of (TRL.Data (Node).Entity))
                        & ".htm");
                  else
                     Suffix := new String'
                       ("_" & Spec_Suffix
                          (Kernel,
                           Get_Declaration_File_Of (TRL.Data (Node).Entity))
                        & ".htm");
                  end if;

                  Put_Line
                    (File,
                     "@code{" & Get_Name (TRL.Data (Node).Entity)
                     & "}   in   @file{"
                     & Full_Name
                       (Get_Declaration_File_Of (TRL.Data (Node).Entity)).all
                     & "},   line: "
                     & Integer'Image
                       (Get_Declaration_Line_Of (TRL.Data (Node).Entity))
                     & ",   column: "
                     & Integer'Image
                       (Get_Declaration_Column_Of (TRL.Data (Node).Entity)));
               --  no link at all
               else
                  Put_Line
                    (File,
                     "@code{" & Get_Name (TRL.Data (Node).Entity)
                     & "}   in   @file{"
                     & Full_Name
                       (Get_Declaration_File_Of (TRL.Data (Node).Entity)).all
                     & "},   line: "
                     & Integer'Image
                       (Get_Declaration_Line_Of (TRL.Data (Node).Entity))
                     & ",   column: "
                     & Integer'Image
                       (Get_Declaration_Column_Of (TRL.Data (Node).Entity)));
               end if;
               Node := TRL.Next (Node);
            end loop;
               Ada.Text_IO.Put_Line (File, "@end itemize");
         end if;
      end Print_Ref_List;

   begin  --  Doc_TEXI_Subprogram
      if Info.Subprogram_Entity.Is_Private then
         Put_Line (File, "@emph{private:}" & ASCII.LF);
      end if;

      Put_Line (File, "@code{");
      Format_TEXI (File, Info.Subprogram_Header.all);
      Put_Line (File, "}");
      --  write the description to doc file
      Put_Line (File, Info.Subprogram_Description.all);
      Print_Ref_List (Info.Subprogram_Entity.Called_List, True);
      Print_Ref_List (Info.Subprogram_Entity.Calls_List, False);
   end Doc_TEXI_Subprogram;

   -----------------
   -- Format_TEXI --
   -----------------

   procedure Format_TEXI
     (File          : Ada.Text_IO.File_Type;
      Text          : String)
   is
      --  global variables for the callback function
      Last_Index, Last_Line : Natural;
      pragma Unreferenced (Last_Line);

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
         Put (File, Text (Last_Index .. Text'Last));
      end if;
   end Format_TEXI;

   --------------------------------
   -- Doc_TEXI_Unit_Index_Header --
   --------------------------------

   procedure Doc_TEXI_Unit_Index_Header
     (Kernel        : access Kernel_Handle_Record'Class;
      File          : Ada.Text_IO.File_Type;
      Info          : Doc_Info;
      Doc_Directory : String;
      Doc_Suffix    : String)
   is
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
         Doc_Specs : Boolean)
      is
         Is_Spec : Boolean;
      begin
         Node := TSFL.First (Info.Unit_File_List);
         for J in 1 .. TSFL.Length (Info.Unit_File_List) loop
            Is_Spec := Is_Spec_File (Kernel, TSFL.Data (Node).File_Name);

            if (Is_Spec and then Doc_Specs)
              or else (not Is_Spec and then not Doc_Specs)
            then
               Put_Line
                 (File,
                  "@include " &
                  Base_Name
                    (Get_Doc_File_Name
                       (TSFL.Data (Node).File_Name,
                        Doc_Directory,
                        Doc_Suffix)));
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
                 Dir_Name (Name (File)) & "project.texi");

         --  create the header
         New_Line (Project_Doc_File);
         Put_Line (Project_Doc_File, "\input texinfo");
         Put_Line (Project_Doc_File, "@c start of header");
         Put_Line (Project_Doc_File, "@setfilename " & "project.info");
         Put_Line (Project_Doc_File, "@settitle Project "
                   & Project_Name (Info.Unit_Project_Name));
         Put_Line (Project_Doc_File, "@c end of header");
         New_Line (Project_Doc_File);
         Put_Line (Project_Doc_File, "@titlepage");
         Put_Line (Project_Doc_File, "@title Project "
                   & Project_Name (Info.Unit_Project_Name));
         Put_Line (Project_Doc_File, "@end titlepage");
         New_Line (Project_Doc_File);
         Put_Line (Project_Doc_File, "@ifinfo");
         Node := TSFL.First (Info.Unit_File_List);
         Put_Line (Project_Doc_File, "@node Top, " &
                   TSFL.Data (Node).Package_Name.all &
                   ",   , (dir)");
         Put_Line (Project_Doc_File, "@top Master Menu");
         Put_Line (Project_Doc_File, "@end ifinfo");
         New_Line (Project_Doc_File);

         --  include all required files

         Write_List_Of_Files (Project_Doc_File, True);

         if Info.Doc_Info_Options.Process_Body_Files then
            New_Line (Project_Doc_File);
            Write_List_Of_Files (Project_Doc_File, False);
         end if;

         New_Line (Project_Doc_File);
         Put_Line (Project_Doc_File, "@include index_unit.texi");
         Put_Line (Project_Doc_File, "@include index_sub.texi");
         Put_Line (Project_Doc_File, "@include index_type.texi");

         New_Line (Project_Doc_File);
         Put_Line (Project_Doc_File, "@bye");
         Close (Project_Doc_File);
      end Create_TEXI_Project_Doc_File;

   begin   --  Doc_TEXI_Unit_Index_Header

      --  check if the project.texi file should also be created
      --  and if the index file will be included later
      if Info.Doc_Info_Options.One_Doc_File then
         Create_TEXI_Project_Doc_File;
         Node := TSFL.Last (Info.Unit_File_List);
         Put (File, "@node Unit Index, Subprogram Index, ");
         if Info.Doc_Info_Options.Process_Body_Files then
            Put (File, "Body of " & TSFL.Data (Node).Package_Name.all);
         else
            Put (File, TSFL.Data (Node).Package_Name.all);
         end if;
         Put_Line (File, ",  Top");
         Put_Line (File, "@chapter Unit Index");
      else
         --  create the header for the self-standing unit index file
         New_Line (File);
         Put_Line (File, "\input texinfo");
         Put_Line (File, "@c start of header");
         Put_Line (File, "@setfilename unit_index.info");
         Put_Line (File, "@settitle Unit Index");
         Put_Line (File, "@c end of header");
         New_Line (File);
         Put_Line (File, "@titlepage");
         Put_Line (File, "@title Unit Index");
         Put_Line (File, "@end titlepage");
         New_Line (File);
         Put_Line (File, "@node Unit Index,  ,   , (dir)");
      end if;
      New_Line (File);
      Put_Line (File, "@itemize @bullet");
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
         Put_Line
           (File, "@node Subprogram Index,  Type Index, Unit Index,  Top");
         Put_Line (File, "@chapter Subprogram Index");
      else
         --  create the header for the self-standing unit index file
         New_Line (File);
         Put_Line (File, "\input texinfo");
         Put_Line (File, "@c start of header");
         Put_Line (File, "@setfilename sub_index.info");
         Put_Line (File, "@settitle Subprogram Index");
         Put_Line (File, "@c end of header");
         New_Line (File);
         Put_Line (File, "@titlepage");
         Put_Line (File, "@title Subprogram Index");
         Put_Line (File, "@end titlepage");
      end if;

      New_Line (File);
      Put_Line (File, "@itemize @bullet");
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
         Put_Line (File, "@node Type Index,   , Subprogram Index, Top");
         Put_Line (File, "@chapter Type Index");
      else
         --  create the header for the self-standing unit index file
         New_Line (File);
         Put_Line (File, "\input texinfo");
         Put_Line (File, "@c start of header");
         Put_Line (File, "@setfilename type_index.info");
         Put_Line (File, "@settitle Type Index");
         Put_Line (File, "@c end of header");
         New_Line (File);
         Put_Line (File, "@titlepage");
         Put_Line (File, "@title Type Index");
         Put_Line (File, "@end titlepage");
         New_Line (File);
         Put_Line (File, "@c    Node  Next Last  Father");
         Put_Line (File, "@node Top,   ,   ,   (dir)");
      end if;

      New_Line (File);
      Put_Line (File, "@itemize @bullet");
   end Doc_TEXI_Type_Index_Header;

   -------------------------
   -- Doc_TEXI_Index_Item --
   -------------------------

   procedure Doc_TEXI_Index_Item
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Put_Line (File, "@item");
      Put (File, Info.Item_Name.all);
      Put_Line (File, "     in @file{" & Full_Name (Info.Item_File).all & "}");
   end Doc_TEXI_Index_Item;

   ------------------------
   -- Doc_TEXI_Index_End --
   ------------------------

   procedure Doc_TEXI_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info)
   is
      pragma Unreferenced (Info);
   begin
      Put_Line (File, "@end itemize");
      New_Line (File);
      Put_Line (File, "@bye");
   end Doc_TEXI_Index_End;

   -------------------
   -- Doc_TEXI_Body --
   -------------------

   procedure Doc_TEXI_Body
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      Put_Line (File, "@code{");
      Format_TEXI (File, Info.Body_Text.all);
      Put_Line (File, "}");
   end Doc_TEXI_Body;

end Docgen.Texi_Output;
