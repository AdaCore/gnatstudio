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
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Language.Ada;              use Language.Ada;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with OS_Utils;                  use OS_Utils;

package body Docgen.Work_On_Source is

   package TSFL renames Type_Source_File_List;
   package TEL renames Type_Entity_List;

   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source
     (Doc_File          : File_Type;
      Next_Package      : GNAT.OS_Lib.String_Access;
      Prev_Package      : GNAT.OS_Lib.String_Access;
      Source_File_List  : in out Type_Source_File_List.List;
      Source_Filename   : String;
      Package_Name      : String;
      Entity_List       : in out Type_Entity_List.List;
      Process_Body_File : Boolean;
      LI_Unit           : LI_File_Ptr;
      Options           : All_Options)
   is
      File_Text         : GNAT.OS_Lib.String_Access;
      Parsed_List       : Construct_List;

   begin

      --  parse the source file and create the Parsed_List
      File_Text := Read_File (Source_Filename);

      Process_Open_File (Doc_File,
                         Source_Filename,
                         Next_Package,
                         Prev_Package,
                         Package_Name,
                         Source_File_List,
                         Options);
      Process_Header (Doc_File,
                      Package_Name,
                      Source_Filename,
                      Process_Body_File,
                      Options);

      --  different ways of process for .ads and .adb files
      if File_Extension (File_Name (Source_Filename)) = ".ads" then

         Parse_Constructs (Ada_Lang,
                           File_Text.all,
                           Parsed_List);

         Sort_List_Name (Entity_List);

         --  the order of the following procedure calls can't be changed
         --  without changing the order in texi_output!
         --  See: Doc_TEXI_Subtitle !!!

         Process_Package_Description (Doc_File,
                                      Package_Name,
                                      File_Text.all,
                                      Options);
         Process_With_Clause (Doc_File,
                              Source_Filename,
                              Package_Name,
                              Parsed_List,
                              File_Text,
                              LI_Unit,
                              Source_File_List,
                              Options);
         Process_Packages     (Doc_File,
                               Entity_List,
                               Source_Filename,
                               Package_Name,
                               Parsed_List,
                               File_Text,
                               LI_Unit,
                               Source_File_List,
                               Options);
         Process_Vars        (Doc_File,
                              Entity_List,
                              Source_Filename,
                              Package_Name,
                              Parsed_List,
                              File_Text,
                              LI_Unit,
                              Source_File_List,
                              Options);
         Process_Exceptions  (Doc_File,
                              Entity_List,
                              Source_Filename,
                              Package_Name,
                              Parsed_List,
                              File_Text,
                              LI_Unit,
                              Source_File_List,
                              Options);
         Process_Types       (Doc_File,
                              Entity_List,
                              Source_Filename,
                              Package_Name,
                              Parsed_List,
                              File_Text,
                              LI_Unit,
                              Source_File_List,
                              Options);
         Process_Entries     (Doc_File,
                              Entity_List,
                              Source_Filename,
                              Process_Body_File,
                              Package_Name,
                              Parsed_List,
                              File_Text,
                              LI_Unit,
                              Source_File_List,
                              Options);
         Process_Subprograms (Doc_File,
                              Entity_List,
                              Source_Filename,
                              Process_Body_File,
                              Package_Name,
                              Parsed_List,
                              File_Text,
                              LI_Unit,
                              Source_File_List,
                              Options);
         Free (Parsed_List);

      else
         Process_One_Body_File (Doc_File,
                                Source_Filename,
                                File_Text,
                                LI_Unit,
                                Source_File_List,
                                Options);
      end if;
      Process_Footer (Doc_File,
                      Source_Filename,
                      Options);

      Process_Close_File (Doc_File, Source_Filename, Options);

      Free (File_Text);
   end Process_Source;

   -----------------------
   -- Process_Open_File --
   -----------------------

   procedure Process_Open_File
     (Doc_File         : File_Type;
      Package_File     : String;
      Next_Package     : GNAT.OS_Lib.String_Access;
      Prev_Package     : GNAT.OS_Lib.String_Access;
      Package_Name     : String;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options)
   is
      Data_Open   : Doc_Info (Info_Type => Open_Info);
   begin
      --  initialise the Doc_Info data
      Data_Open := Doc_Info'
        (Open_Info,
         Doc_Info_Options => Options,
         Doc_File_List => Source_File_List,
         Open_Title => new String'(Package_Name),
         Open_File  => new String'(Package_File),
         Open_Package_Next => Next_Package,
         Open_Package_Prev => Prev_Package,
         Doc_LI_Unit => No_LI_File);

      --  call the documentation procedure
      Options.Doc_Subprogram (Doc_File, Data_Open);

      Free (Data_Open.Open_Title);
      Free (Data_Open.Open_File);
   end Process_Open_File;

   ------------------------
   -- Process_Close_File --
   ------------------------

   procedure Process_Close_File
     (Doc_File      : File_Type;
      File_Name     : String;
      Options       : All_Options)
   is
      Data_Close    : Doc_Info (Info_Type => Close_Info);
   begin
      --  initialise the Doc_Info data
      Data_Close := Doc_Info'
        (Close_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         Close_File_Name =>
         new String'(File_Name));

      --  call the documentation procedure
      Options.Doc_Subprogram (Doc_File, Data_Close);

      Free (Data_Close.Close_File_Name);
   end Process_Close_File;

   ---------------------------
   -- Process_One_Body_File --
   ---------------------------

   procedure Process_One_Body_File
     (Doc_File           : File_Type;
      Source_File        : String;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options)
   is
      Data_Line          : Doc_Info (Info_Type => Body_Line_Info);
   begin
      --  initialise the Doc_Info data
      Data_Line := Doc_Info'
        (Body_Line_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => LI_Unit,
         Body_Text      => File_Text,
         Body_File      =>
         new String'(Source_File),
         Doc_File_List => Source_File_List);

      --  call the documentation procedure
      Options.Doc_Subprogram (Doc_File, Data_Line);

      Free (Data_Line.Body_Text);
      Free (Data_Line.Body_File);
   end Process_One_Body_File;

   ------------------------
   -- Process_Unit_Index --
   ------------------------

   procedure Process_Unit_Index
     (Source_File_List : Type_Source_File_List.List;
      Options          : All_Options)
   is
      Source_Filename  : GOL.String_Access;
      Package_Name     : GOL.String_Access;
      Source_File_Node : Type_Source_File_List.List_Node;
      Index_File       : File_Type;
      Data_Package     : Doc_Info (Info_Type => Unit_Index_Info);
      Data_Item        : Doc_Info (Info_Type => Index_Item_Info);
      Data_End         : Doc_Info (Info_Type => End_Of_Index_Info);

      package TSFL renames Type_Source_File_List;

      One_Ready        : Integer;
      --  how many files already examined BEFORE the loop

      Doc_File_Name    : constant String := "index_unit";
   begin
      Create (Index_File,
              Out_File,
              Options.Doc_Directory.all &
              Doc_File_Name &
              Options.Doc_Suffix.all);

      if not TSFL.Is_Empty (Source_File_List) then
         One_Ready := 0;
         Source_File_Node := TSFL.First (Source_File_List);
         Source_Filename  := TSFL.Data (Source_File_Node).File_Name;

         --  if first file .adb, take the next one, which must be .ads

         if File_Extension (File_Name (Source_Filename.all)) = ".adb" then
            Source_File_Node := TSFL.Next (Source_File_Node);
            Source_Filename := TSFL.Data (Source_File_Node).File_Name;
            One_Ready := 1;
         end if;

         Data_Package := Doc_Info'
           (Unit_Index_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Unit_Project_Name => Options.Project_Name,
            Unit_Index_File_Name => new String'(Doc_File_Name),
            Unit_File_List => Source_File_List);

         --  Create the upper part of the unit index

         Options.Doc_Subprogram (Index_File, Data_Package);

         for J in 1 .. Type_Source_File_List.Length (Source_File_List) -
           One_Ready loop
            Source_Filename := TSFL.Data (Source_File_Node).File_Name;

            --  add unit, but only if from a spec file
            --  ??? Should not compare with ".ads", since other naming scheme
            --  may be used

            if File_Extension (File_Name (Source_Filename.all)) = ".ads" then
               Package_Name := TSFL.Data (Source_File_Node).Package_Name;
               Data_Item := Doc_Info'
                 (Index_Item_Info,
                  Doc_Info_Options => Options,
                  Doc_LI_Unit => No_LI_File,
                  Doc_File_List => TSFL.Null_List,
                  Item_Name => Package_Name,
                  Item_File =>
                  new String'(File_Name (Source_Filename.all)),
                  Item_Line => First_File_Line,
                  Item_Doc_File =>
                  new String'(Base_Name
                                (Get_Doc_File_Name
                                   (Source_Filename.all,
                                    Options.Doc_Directory.all,
                                    Options.Doc_Suffix.all))));
               Options.Doc_Subprogram (Index_File, Data_Item);
            end if;

            Source_File_Node := TSFL.Next (Source_File_Node);
         end loop;
      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         End_index_Title => new String'("End of Index"));
      Options.Doc_Subprogram (Index_File, Data_End);

      Free (Data_Package.Unit_Index_File_Name);
      Free (Data_Item.Item_File);
      Free (Data_Item.Item_Doc_File);
      Free (Data_End.End_Index_Title);

      Close (Index_File);
   end Process_Unit_Index;

   ------------------------------
   -- Process_Subprogram_Index --
   ------------------------------

   procedure Process_Subprogram_Index
     (Subprogram_Index_List : Type_Entity_List.List;
      Options               : All_Options)
   is
      Source_Filename       : GOL.String_Access;
      Subprogram_Index_Node : Type_Entity_List.List_Node;
      Index_File       : File_Type;
      Data_Subprogram  : Doc_Info (Info_Type => Subprogram_Index_Info);
      Data_Item        : Doc_Info (Info_Type => Index_Item_Info);
      Data_End         : Doc_Info (Info_Type => End_Of_Index_Info);

      Doc_File_Name    : constant String := "index_sub";
   begin
      Create (Index_File,
              Out_File,
              Options.Doc_Directory.all &
              Doc_File_Name &
              Options.Doc_Suffix.all);

      Data_Subprogram := Doc_Info'
        (Subprogram_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         Subprogram_Index_File_Name => new String'(Doc_File_Name));
      Options.Doc_Subprogram (Index_File, Data_Subprogram);

      if not TEL.Is_Empty (Subprogram_Index_List) then
         Subprogram_Index_Node := TEL.First (Subprogram_Index_List);

         for J in 1 .. Type_Entity_List.Length (Subprogram_Index_List) loop
            Source_Filename := TEL.Data (Subprogram_Index_Node).File_Name;
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit => No_LI_File,
               Doc_File_List => TSFL.Null_List,
               Item_Name => TEL.Data
                 (Subprogram_Index_Node).Short_Name,
               Item_File => new String'(File_Name (Source_Filename.all)),
               Item_Line => TEL.Data (Subprogram_Index_Node).Line,
               Item_Doc_File => new String'
                 (Base_Name
                    (Get_Doc_File_Name
                       (Source_Filename.all,
                        Options.Doc_Directory.all,
                        Options.Doc_Suffix.all))));
            Options.Doc_Subprogram (Index_File, Data_Item);

            Subprogram_Index_Node := TEL.Next (Subprogram_Index_Node);
         end loop;

      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         End_Index_Title => new String'("End of Index"));
      Options.Doc_Subprogram (Index_File, Data_End);

      Free (Data_Item.Item_File);
      Free (Data_Item.Item_Doc_File);
      Free (Data_Subprogram.Subprogram_Index_File_Name);
      Free (Data_End.End_Index_Title);

      Close (Index_File);
   end Process_Subprogram_Index;

   ------------------------
   -- Process_Type_Index --
   ------------------------

   procedure Process_Type_Index
     (Type_Index_List : Type_Entity_List.List;
      Options          : All_Options)
   is
      Source_Filename  : GOL.String_Access;
      Type_Index_Node  : Type_Entity_List.List_Node;

      Index_File       : File_Type;

      Data_Type        : Doc_Info (Info_Type => Type_Index_Info);
      Data_Item        : Doc_Info (Info_Type => Index_Item_Info);
      Data_End         : Doc_Info (Info_Type => End_Of_Index_Info);

      Doc_File_Name    : constant String := "index_type";
   begin

      Create (Index_File,
              Out_File,
              Options.Doc_Directory.all &
              Doc_File_Name &
              Options.Doc_Suffix.all);
      Data_Type := Doc_Info'
        (Type_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         Type_Index_File_Name => new String'(Doc_File_Name));
      Options.Doc_Subprogram (Index_File, Data_Type);

      if not TEL.Is_Empty (Type_Index_List) then
         Type_Index_Node := TEL.First (Type_Index_List);

         for J in 1 .. Type_Entity_List.Length (Type_Index_List) loop
            Source_Filename := TEL.Data (Type_Index_Node).File_Name;
            Data_Item := Doc_Info'
              (Index_Item_Info,
               Doc_Info_Options => Options,
               Doc_LI_Unit => No_LI_File,
               Doc_File_List => TSFL.Null_List,
               Item_Name => TEL.Data (Type_Index_Node).Short_Name,
               Item_File => new String'(File_Name (Source_Filename.all)),
               Item_Line => TEL.Data (Type_Index_Node).Line,
               Item_Doc_File => new String'
                 (Base_Name
                    (Get_Doc_File_Name
                       (Source_Filename.all,
                        Options.Doc_Directory.all,
                        Options.Doc_Suffix.all))));
               Options.Doc_Subprogram (Index_File, Data_Item);

               Type_Index_Node := TEL.Next (Type_Index_Node);
         end loop;

      end if;

      Data_End := Doc_Info'
        (End_Of_Index_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         End_Index_Title => new String'("End of Index"));
      Options.Doc_Subprogram (Index_File, Data_End);

      Free (Data_Item.Item_File);
      Free (Data_Item.Item_Doc_File);
      Free (Data_End.End_Index_Title);
      Free (Data_Type.Type_Index_File_Name);

      Close (Index_File);
   end Process_Type_Index;

   --------------------
   -- Process_Header --
   --------------------

   procedure Process_Header
     (Doc_File           : File_Type;
      Package_Name       : String;
      Package_File       : String;
      Process_Body_File  : Boolean;
      Options            : All_Options)
   is
      Data_Header   : Doc_Info (Info_Type => Header_Info);

   begin

      Data_Header := Doc_Info'
        (Header_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         Header_Package => new String'(Package_Name),
         Header_File  => new String'(Package_File),
         Header_Line  => First_File_Line,
         Header_Link  => Process_Body_File);
      Options.Doc_Subprogram (Doc_File, Data_Header);

      Free (Data_Header.Header_Package);
      Free (Data_Header.Header_File);
   end Process_Header;


   --------------------
   -- Process_Footer --
   --------------------

   procedure Process_Footer
     (Doc_File      : File_Type;
      Package_File  : String;
      Options       : All_Options)
   is
      Data_Footer   : Doc_Info (Info_Type => Footer_Info);
   begin
      Data_Footer := Doc_Info'
        (Footer_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => No_LI_File,
         Doc_File_List => TSFL.Null_List,
         Footer_Title => new String'("Docgen"),
         Footer_File  => new String'(Package_File));
      Options.Doc_Subprogram (Doc_File, Data_Footer);

      Free (Data_Footer.Footer_Title);
      Free (Data_Footer.Footer_File);
   end Process_Footer;

   ---------------------------------
   -- Process_Package_Description --
   ---------------------------------

   procedure Process_Package_Description
     (Doc_File        : File_Type;
      Package_Name    : String;
      Text            : String;
      Options         : All_Options)
   is
      Data_Subtitle    : Doc_Info (Info_Type => Subtitle_Info);
      Data_Package     : Doc_Info (Info_Type => Package_Desc_Info);

      Description_Found, Start_Found : Boolean;
      Line                           : Natural;
      Max_Lines                      : constant Natural :=
        Count_Lines (Text);
      Description                    : GNAT.OS_Lib.String_Access;

   begin
      --  tries to find the first line of the description of the package
      --  if something else is found than a comment line => no description
      Description_Found := False;
      Start_Found       := False;
      Line              := 1;
      while not Start_Found and Line < Max_Lines + 1 loop
         if Line_Is_Comment (Get_Line_From_String (Text, Line)) then
            Description_Found := True;
            Start_Found       := True;
         elsif not Line_Is_Empty
           (Get_Line_From_String (Text, Line)) then
            Start_Found       := True;
         else
            Line := Line + 1;
         end if;
      end loop;

      --  if package description found
      if Description_Found then

         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Description"),
            Subtitle_Kind => Package_Desc_Info,
            Subtitle_Package => new String'(Package_Name));
         Options.Doc_Subprogram (Doc_File, Data_Subtitle);
         Description := Extract_Comment
           (Text,
            Line,
            0,
            True,
            Options);
         Data_Package := Doc_Info'
           (Package_Desc_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Package_Desc_Description => Description);
         Options.Doc_Subprogram (Doc_File, Data_Package);
         Free (Description);
      end if;
   end Process_Package_Description;

   --------------------------
   -- Process_With_Clauses --
   --------------------------

   procedure Process_With_Clause
     (Doc_File         : File_Type;
      Source_Filename  : String;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options)
   is
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_With       : Doc_Info (Info_Type => With_Info);

      Old_Line, New_Line : GNAT.OS_Lib.String_Access;
      Parse_Node         : Construct_Access;
      Parsed_List_End    : Boolean;
      First_With_Line    : Natural;

   begin
      New_Line        := new String'("  ");
      Parse_Node      := Parsed_List.First;
      Parsed_List_End := False;
      First_With_Line := 0;

      --  exception if no paresed entities found: later

      while not Parsed_List_End loop

         if Parse_Node.Category = Cat_With then
            Old_Line := New_Line;
            New_Line := new String'
              (New_Line.all & ASCII.LF &
               File_Text (Parse_Node.Sloc_Start.Index ..
                          Parse_Node.Sloc_End.Index));
            Free (Old_Line);

            if First_With_Line = 0 then
               First_With_Line := Parse_Node.Sloc_Start.Line;
            end if;
         end if;

         if Parse_Node = Parsed_List.Last or
           Parse_Node.Category = Cat_Procedure or
           Parse_Node.Category = Cat_Function then
            Parsed_List_End := True;
         else
            Parse_Node := Parse_Node.Next;
         end if;
      end loop;

      if New_Line.all'Length > 0 then

         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Dependencies"),
            Subtitle_Kind => With_Info,
            Subtitle_Package => new String'(Package_Name));
         Options.Doc_Subprogram (Doc_File, Data_Subtitle);
      end if;

      Data_With := Doc_Info'
        (With_Info,
         Doc_Info_Options => Options,
         Doc_LI_Unit => LI_Unit,
         Doc_File_List => Source_File_List,
         With_Header_Line => First_With_Line,
         With_File  => new String '(Source_Filename),
         With_Header => New_Line);
      Options.Doc_Subprogram (Doc_File, Data_With);

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
      Free (New_Line);
   end Process_With_Clause;

   ----------------------
   -- Process_Packages --
   ----------------------

   procedure Process_Packages
     (Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : String;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Package    : Doc_Info (Info_Type => Package_Info);

      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Packages"),
            Subtitle_Kind => Package_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop

            --  check if the entity is a package
            if TEL.Data (Entity_Node).Kind = Package_Entity
            --  but NOT the package itself
              and then To_Lower (TEL.Data (Entity_Node).Name.all) /=
                To_Lower (Package_Name)
            --  check if defined in this file, the others used only for bodys!
              and then TEL.Data (Entity_Node).File_Name.all = Source_Filename
            then

               --  check if the subtitle has been set already.
               --  Can't be set before the "if"
               if not First_Already_Set then
                  Options.Doc_Subprogram (Doc_File, Data_Subtitle);
                  First_Already_Set := True;
               end if;

               Header :=
                 Get_Whole_Header (File_Text.all,
                                   Parsed_List,
                                   TEL.Data (Entity_Node).Short_Name.all,
                                   TEL.Data (Entity_Node).Line);

               Description := Extract_Comment
                 (File_Text.all,
                  TEL.Data (Entity_Node).Line,
                  Count_Lines (Header.all),
                  False,
                  Options);
               Data_Package := Doc_Info'
                 (Package_Info,
                  Doc_Info_Options => Options,
                  Doc_LI_Unit => LI_Unit,
                  Doc_File_List => Source_File_List,
                  Package_Entity      => TEL.Data (Entity_Node),
                  Package_Description => Description,
                  Package_Header => Header,
                  Package_Header_Line => TEL.Data (Entity_Node).Line);
               Options.Doc_Subprogram (Doc_File, Data_Package);
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
            Free (Data_Package.Package_Header);
         end loop;
         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;
   end Process_Packages;

   ------------------
   -- Process_Vars --
   ------------------

   procedure Process_Vars
     (Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : String;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Var        : Doc_Info (Info_Type => Var_Info);

      First_Already_Set : Boolean;
   begin

      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Constants and Named Numbers"),
            Subtitle_Kind => Var_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a variable

            if TEL.Data (Entity_Node).Kind = Var_Entity
            --  Check if defined in this file, the others used only for bodys!
              and then TEL.Data (Entity_Node).File_Name.all = Source_Filename
            then
               Header :=
                 Get_Whole_Header (File_Text.all,
                                   Parsed_List,
                                   TEL.Data (Entity_Node).Short_Name.all,
                                   TEL.Data (Entity_Node).Line);

               --  check if it was a entity with its own header
               if Header /= null then

                  --  check if the subtitle "Constand and Named Numbers:"
                  --  has been set already.
                  --  Can't be set before the "if"
                  if not First_Already_Set then
                     Options.Doc_Subprogram (Doc_File, Data_Subtitle);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     TEL.Data (Entity_Node).Line,
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Var := Doc_Info'
                    (Var_Info,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit => LI_Unit,
                     Doc_File_List => Source_File_List,
                     Var_Entity      => TEL.Data (Entity_Node),
                     Var_Description => Description,
                     Var_Header     => Header,
                     Var_Header_Line => TEL.Data (Entity_Node).Line);
                  Options.Doc_Subprogram (Doc_File, Data_Var);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
            Free (Data_Var.Var_Header);
         end loop;

         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;
   end Process_Vars;

   ------------------------
   -- Process_Exceptions --
   ------------------------

   procedure Process_Exceptions
     (Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : String;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Exception  : Doc_Info (Info_Type => Exception_Info);
      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Exceptions"),
            Subtitle_Kind => Exception_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  If not a renamed exception...!  ***change this later!!!***

            --  Check if the entity is a exception
            if TEL.Data (Entity_Node).Kind = Exception_Entity
            --  Check if defined in this file, the others used only for bodys!
              and then TEL.Data (Entity_Node).File_Name.all = Source_Filename
            then
               Header :=
                 Get_Whole_Header (File_Text.all,
                                   Parsed_List,
                                   TEL.Data (Entity_Node).Short_Name.all,
                                   TEL.Data (Entity_Node).Line);

               --  check if it was a entity with its own header
               if Header /= null then

                  --  check if the subtitle "Exceptions:" has been set already.
                  --  Can't be set before the "if"
                  if not First_Already_Set then
                     Options.Doc_Subprogram (Doc_File, Data_Subtitle);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     TEL.Data (Entity_Node).Line,
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Exception := Doc_Info'
                    (Exception_Info,
                     Doc_Info_Options      => Options,
                     Doc_LI_Unit           => LI_Unit,
                     Doc_File_List         => Source_File_List,
                     Exception_Entity      => TEL.Data (Entity_Node),
                     Exception_Description => Description,
                     Exception_Header      => Header,
                     Exception_Header_Line => TEL.Data (Entity_Node).Line);
                  Options.Doc_Subprogram (Doc_File, Data_Exception);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
            Free (Data_Exception.Exception_Header);
         end loop;

         Free (Data_Subtitle.Subtitle_Name);
         Free (Data_Subtitle.Subtitle_Package);
      end if;
   end Process_Exceptions;

   -------------------
   -- Process_Types --
   -------------------

   procedure Process_Types
     (Doc_File         : File_Type;
      Entity_List      : in out Type_Entity_List.List;
      Source_Filename  : String;
      Package_Name     : String;
      Parsed_List      : Construct_List;
      File_Text        : GNAT.OS_Lib.String_Access;
      LI_Unit          : LI_File_Ptr;
      Source_File_List : in out Type_Source_File_List.List;
      Options          : All_Options)
   is
      Entity_Node     : Type_Entity_List.List_Node;
      Description     : GNAT.OS_Lib.String_Access;
      Header          : GNAT.OS_Lib.String_Access;
      Data_Subtitle   : Doc_Info (Info_Type => Subtitle_Info);
      Data_Type       : Doc_Info (Info_Type => Type_Info);

      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Types"),
            Subtitle_Kind => Type_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a type

            if TEL.Data (Entity_Node).Kind = Type_Entity
            --  Check if defined in this file (the rest of entities
            --  only for the body documentation)
              and then TEL.Data (Entity_Node).File_Name.all = Source_Filename
            then
               Header :=
                 Get_Whole_Header (File_Text.all,
                                   Parsed_List,
                                   TEL.Data (Entity_Node).Short_Name.all,
                                   TEL.Data (Entity_Node).Line);


               --  Check if it was a entity with its own header

               if Header /= null then
                  --  check if still the subtitle "Types:" has to be set.
                  --  Can't be set before the "if"

                  if not First_Already_Set then
                     Options.Doc_Subprogram (Doc_File, Data_Subtitle);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     TEL.Data (Entity_Node).Line,
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Type := Doc_Info'
                    (Type_Info,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit => LI_Unit,
                     Doc_File_List => Source_File_List,
                     Type_Entity      => TEL.Data (Entity_Node),
                     Type_Description => Description,
                     Type_Header => Header,
                     Type_Header_Line => TEL.Data (Entity_Node).Line);
                  Options.Doc_Subprogram (Doc_File, Data_Type);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
            Free (Data_Type.Type_Header);
         end loop;
      end if;

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Types;

   ---------------------
   -- Process_Entries --
   ---------------------

   procedure Process_Entries
     (Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : String;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options)
   is
      Entity_Node             : Type_Entity_List.List_Node;
      Description             : GNAT.OS_Lib.String_Access;
      Header                  : GNAT.OS_Lib.String_Access;
      Data_Subtitle           : Doc_Info (Info_Type => Subtitle_Info);
      Data_Entry              : Doc_Info (Info_Type => Entry_Info);

      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Tasks, Entries and Entry Families"),
            Subtitle_Kind => Entry_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  Check if the entity is a entry or entry family

            if (TEL.Data (Entity_Node).Kind = Entry_Entity)
            --  Check if defined in this file (the rest of
            --  entities only for the body documentation)
              and then TEL.Data (Entity_Node).File_Name.all = Source_Filename
            then
               Header :=
                 Get_Whole_Header (File_Text.all,
                                   Parsed_List,
                                   TEL.Data (Entity_Node).Short_Name.all,
                                   TEL.Data (Entity_Node).Line);

               --  Check if it was a entity with its own header

               if Header /= null then
                  --  Check if still the subtitle has to be set.
                  --  Can be set before the "if"

                  if not First_Already_Set then
                     Options.Doc_Subprogram (Doc_File, Data_Subtitle);
                     First_Already_Set := True;
                  end if;

                  Description := Extract_Comment
                    (File_Text.all,
                     TEL.Data (Entity_Node).Line,
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Entry := Doc_Info'
                    (Entry_Info,
                     Doc_Info_Options  => Options,
                     Doc_LI_Unit       => LI_Unit,
                     Doc_File_List     => Source_File_List,
                     Entry_Entity      => TEL.Data (Entity_Node),
                     Entry_Description => Description,
                     Entry_Link        => Process_Body_File,
                     Entry_Header      => Header,
                     Entry_Header_Line => TEL.Data (Entity_Node).Line);
                  Options.Doc_Subprogram (Doc_File, Data_Entry);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
            Free (Data_Entry.Entry_Header);
         end loop;
      end if;

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Entries;

   -------------------------
   -- Process_Subprograms --
   -------------------------

   procedure Process_Subprograms
     (Doc_File           : File_Type;
      Entity_List        : in out Type_Entity_List.List;
      Source_Filename    : String;
      Process_Body_File  : Boolean;
      Package_Name       : String;
      Parsed_List        : Construct_List;
      File_Text          : GNAT.OS_Lib.String_Access;
      LI_Unit            : LI_File_Ptr;
      Source_File_List   : in out Type_Source_File_List.List;
      Options            : All_Options)
   is
      Entity_Node             : Type_Entity_List.List_Node;
      Description             : GNAT.OS_Lib.String_Access;
      Header                  : GNAT.OS_Lib.String_Access;
      Data_Subtitle           : Doc_Info (Info_Type => Subtitle_Info);
      Data_Subprogram         : Doc_Info (Info_Type => Subprogram_Info);

      First_Already_Set : Boolean;

   begin
      if not TEL.Is_Empty (Entity_List) then
         First_Already_Set := False;
         Data_Subtitle := Doc_Info'
           (Subtitle_Info,
            Doc_Info_Options => Options,
            Doc_LI_Unit => No_LI_File,
            Doc_File_List => TSFL.Null_List,
            Subtitle_Name => new String'("Subprograms"),
            Subtitle_Kind => Subprogram_Info,
            Subtitle_Package => new String'(Package_Name));
         Entity_Node := TEL.First (Entity_List);

         for J in 1 .. TEL.Length (Entity_List) loop
            --  check if the entity is a procedure or a function

            if (TEL.Data (Entity_Node).Kind = Subprogram_Entity)
            --  check if defined in this file (the rest of
            --  entities only for the body documentation)
              and then TEL.Data (Entity_Node).File_Name.all = Source_Filename
            then
               --  Check if still the subtitle "Subprograms:"
               --  has to be set. Can be set before the "if"

               if not First_Already_Set then
                  Options.Doc_Subprogram (Doc_File, Data_Subtitle);
                  First_Already_Set := True;
               end if;

               Header :=
                 Get_Whole_Header (File_Text.all,
                                   Parsed_List,
                                   TEL.Data (Entity_Node).Short_Name.all,
                                   TEL.Data (Entity_Node).Line);


               --  check if it was a entity with its own header
               if Header /= null then
                  Description := Extract_Comment
                    (File_Text.all,
                     TEL.Data (Entity_Node).Line,
                     Count_Lines (Header.all),
                     False,
                     Options);

                  Data_Subprogram := Doc_Info'
                    (Subprogram_Info,
                     Doc_Info_Options => Options,
                     Doc_LI_Unit => LI_Unit,
                     Doc_File_List => Source_File_List,
                     Subprogram_Entity      => TEL.Data (Entity_Node),
                     Subprogram_Description => Description,
                     Subprogram_Link        => Process_Body_File,
                     Subprogram_List        => Entity_List,
                     Subprogram_Header => Header,
                     Subprogram_Header_Line => TEL.Data (Entity_Node).Line);
                  Options.Doc_Subprogram (Doc_File, Data_Subprogram);
               end if;
            end if;

            Entity_Node := TEL.Next (Entity_Node);
            Free (Description);
            Free (Header);
            Free (Data_Subprogram.Subprogram_Header);
         end loop;
      end if;

      Free (Data_Subtitle.Subtitle_Name);
      Free (Data_Subtitle.Subtitle_Package);
   end Process_Subprograms;

   ---------------------
   -- Line_Is_Comment --
   ---------------------

   function Line_Is_Comment
     (Line : String) return Boolean is
   begin
      if Line'Length > 5 then
         for J in Line'First .. Line'Last - 3 loop
            if Line (J) = '-' and Line (J + 1) = '-' then
               return True;
            elsif Line (J) /= ' '
              and Line (J) /= ASCII.HT
              and Line (J) /= ASCII.LF
              and Line (J) /= ASCII.CR
            then return False;
            end if;
         end loop;
      end if;
      return False;
   end Line_Is_Comment;

   -------------------
   -- Line_Is_Empty --
   -------------------

   function Line_Is_Empty
     (Line : String) return Boolean is
   begin
      for J in Line'First .. Line'Last loop
         if    Line (J) /= ' '
           and Line (J) /= ASCII.HT
           and Line (J) /= ASCII.LF
           and Line (J) /= ASCII.CR
         then
            return False;
         end if;
      end loop;
      return True;
   end Line_Is_Empty;

   --------------------------
   -- Is_Ignorable_Comment --
   --------------------------

   function Is_Ignorable_Comment
     (Comment_Line : String) return Boolean is
   begin
      if Comment_Line'Length > 5 then
         for J in Comment_Line'First .. Comment_Line'Last - 3 loop
            if Comment_Line (J) = '-' and Comment_Line (J + 1) = '-' then
               if Comment_Line (J + 2) = '!' then
                  return True;
               else
                  return False;
               end if;
            end if;
         end loop;
      end if;
      return False;
   end Is_Ignorable_Comment;

   -----------------
   -- Kill_Prefix --
   -----------------

   function Kill_Prefix
     (Comment_Line : String) return String is
      J : Natural;
   begin
      J := Comment_Line'First;
      while (Comment_Line (J) /= '-' and Comment_Line (J + 1) /= '-') loop
         J := J + 1;
      end loop;
      return Comment_Line (J + 3 .. Comment_Line'Last);
   end Kill_Prefix;

   -----------------------
   --  Extract_Comment  --
   -----------------------

   function Extract_Comment
     (File_Text           : String;
      Line                : Natural;
      Header_Lines        : Natural;
      Package_Description : Boolean;
      Options             : All_Options) return GNAT.OS_Lib.String_Access
   is
      New_Line, Old_Line  : GNAT.OS_Lib.String_Access;
      Result_Line         : GNAT.OS_Lib.String_Access;
      J                   : Natural;

   begin
      Result_Line := new String'("");

      --  The comments are under the header of the entity

      if Options.Comments_Under or Package_Description then
         J := Line + Header_Lines;

         --  the comments are above the header of the entity

      else
         J := Line - 1;
      end if;

      New_Line := new String'
        (Get_Line_From_String (File_Text, J));

      while Line_Is_Comment (New_Line.all) loop
         if Options.Comments_Under or Package_Description then
            J := J + 1;

            if not (Options.Ignorable_Comments and then
                      Is_Ignorable_Comment (New_Line.all)) then
               if Package_Description then
                  Result_Line := new String'(Result_Line.all & New_Line.all);
               else
                  Result_Line := new String'(Result_Line.all & ' ' &
                    Kill_Prefix (New_Line.all));
               end if;
            end if;
         else
            J := J - 1;

            if not (Options.Ignorable_Comments and then
                      Is_Ignorable_Comment (New_Line.all)) then
               if Package_Description then
                  Result_Line := new String'(New_Line.all &
                    Result_Line.all);
               else
                  Result_Line := new String'((Kill_Prefix (New_Line.all)) &
                    ' ' & Result_Line.all);
               end if;
            end if;
         end if;

         Old_Line := New_Line;
         New_Line := new String'(Get_Line_From_String (File_Text, J));
         Free (Old_Line);
      end loop;

      Free (New_Line);
      return Result_Line;
   end Extract_Comment;

   --------------------------
   -- Get_Line_From_String --
   --------------------------

   function Get_Line_From_String
     (Text    : String;
      Line_Nr : Natural) return String
   is
      Lines, Index_Start, Index_End : Natural;
   begin
      Lines       := 1;
      Index_Start := 1;

      if Line_Nr > 1 then
         while Index_Start < Text'Length and Lines < Line_Nr loop
            if Text (Index_Start) = ASCII.LF then
               Lines := Lines + 1;
            end if;

            Index_Start := Index_Start + 1;
         end loop;
      end if;

      Index_End := Index_Start;

      while Index_End < Text'Length and then Text (Index_End) /=  ASCII.LF loop
         Index_End := Index_End + 1;
      end loop;

      return Text (Index_Start .. Index_End);
   end Get_Line_From_String;

   ------------------------
   --  Get_Whole_Header  --
   ------------------------

   function Get_Whole_Header
     (File_Text   : String;
      Parsed_List : Construct_List;
      Entity_Name : String;
      Entity_Line : Natural) return GNAT.OS_Lib.String_Access
   is
      Parse_Node         : Construct_Access;
      Parsed_List_End    : Boolean;
      Result             : GNAT.OS_Lib.String_Access;
   begin
      Parse_Node      := Parsed_List.First;
      Parsed_List_End := False;

      --  exception if no paresed entities found: later

      while not Parsed_List_End loop
         if To_Lower (Parse_Node.Name.all) =
           To_Lower (Entity_Name) and then
           Parse_Node.Sloc_Start.Line = Entity_Line
         then
            Result := new String (1 .. Parse_Node.Sloc_End.Index -
                                    Parse_Node.Sloc_Start.Index + 1);
            Result.all := File_Text (Parse_Node.Sloc_Start.Index ..
                                             Parse_Node.Sloc_End.Index);
            return Result;
         end if;

         if Parse_Node = Parsed_List.Last then
            Parsed_List_End := True;
         else
            Parse_Node := Parse_Node.Next;
         end if;
      end loop;

      return null;
   end Get_Whole_Header;

end Docgen.Work_On_Source;
