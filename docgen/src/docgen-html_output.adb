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
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Language;                  use Language;
with Language.Ada;              use Language.Ada;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Language.Ada;              use Language.Ada;

package body Docgen.Html_Output is

   package TEL renames Type_Entity_List;

   procedure Doc_HTML_Create
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      case Info.Info_Type is
         when Open_Info             => Doc_HTML_Open (File, Info);
         when Close_Info            => Doc_HTML_Close (File, Info);
         when Header_Info           => Doc_HTML_Header (File, Info);
         when Footer_Info           => Doc_HTML_Footer (File, Info);
         when Subtitle_Info         => Doc_HTML_Subtitle (File, Info);
         when Package_Desc_Info     => Doc_HTML_Pack_Desc (File, Info);
         when With_Info             => Doc_HTML_With (File, Info);
         when Package_Info          => Doc_HTML_Package (File, Info);
         when Var_Info              => Doc_HTML_Var (File, Info);
         when Entry_Info            => Doc_HTML_Entry (File, Info);
         when Subprogram_Info       => Doc_HTML_Subprogram (File, Info);
         when Type_Info             => Doc_HTML_Type (File, Info);
         when Exception_Info        => Doc_HTML_Exception (File, Info);
         when Unit_Index_Info       => Doc_HTML_Unit_Index_Header (File, Info);
         when Subprogram_Index_Info => Doc_HTML_Sub_Index_Header  (File, Info);
         when Type_Index_Info       => Doc_HTML_Type_Index_Header (File, Info);
         when Index_Item_Info       => Doc_HTML_Index_Item (File, Info);
         when End_Of_Index_Info     => Doc_HTML_Index_End  (File, Info);
         when Body_Line_Info        => Doc_HTML_Body (File, Info);
         when others => null;
      end case;

   end Doc_HTML_Create;

   -------------------
   -- Doc_HTML_Open --
   -------------------

   procedure Doc_HTML_Open
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "<HTML>");
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<TITLE>" &
                            Info.Open_Title.all &
                            "</TITLE>");
      Ada.Text_IO.Put_Line (File, "<META name=""generator"" " &
                            "CONTENT=""DocGen ");
      Ada.Text_IO.Put_Line (File, "<META http-equiv=""Content-" &
                            "Type"" content=""" &
                            "text/html; charset=" & "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
   end Doc_HTML_Open;

   --------------------
   -- Doc_HTML_Close --
   --------------------

   procedure Doc_HTML_Close
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);

   begin
      Ada.Text_IO.Put_Line (File, "</BODY>");
      Ada.Text_IO.Put_Line (File, "</HTML>");
   end Doc_HTML_Close;

   -----------------------
   -- Doc_HTML_Subtitle --
   -----------------------

   procedure Doc_HTML_Subtitle
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, "<BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD> ");
      Ada.Text_IO.Put_Line (File, "<H3> "  &
                            Info.Subtitle_Name.all &
                            " </H3>");
      Ada.Text_IO.Put_Line (File, "</TD></TR></TABLE>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Subtitle;

   ------------------------
   -- Doc_HTML_Pack_Desc --
   ------------------------

   procedure Doc_HTML_Pack_Desc
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      Ada.Text_IO.Put_Line (File, "<h4><PRE> "  &
                            Info.Package_Desc_Description.all &
                            " </PRE></H4>");

      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Pack_Desc;

   ----------------------
   -- Doc_HTML_Package --
   ----------------------

   procedure Doc_HTML_Package
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Package_Entity.Line)
                            & """></A>  <BR>");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Package_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Package_List,
                   Info.Package_Header.all,
                   Info.Package_Entity.File_Name.all,
                   Info.Package_Entity.Short_Name.all,
                   Info.Package_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Package_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Package;

   -------------------
   -- Doc_HTML_With --
   -------------------

   procedure Doc_HTML_With
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      Format_HTML (File,
                   Info.With_List,
                   Info.With_Header.all,
                   Info.With_File.all,
                   "",
                   0,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   False);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_With;

   ------------------
   -- Doc_HTML_Var --
   ------------------

   procedure Doc_HTML_Var
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Var_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Var_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Var_List,
                   Info.Var_Header.all,
                   Info.Var_Entity.File_Name.all,
                   Info.Var_Entity.Short_Name.all,
                   Info.Var_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Var_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Var;

   ------------------------
   -- Doc_HTML_Exception --
   ------------------------

   procedure Doc_HTML_Exception
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Exception_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Exception_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Exception_List,
                   Info.Exception_Header.all,
                   Info.Exception_Entity.File_Name.all,
                   Info.Exception_Entity.Short_Name.all,
                   Info.Exception_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Exception_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Exception;

   -------------------
   -- Doc_HTML_Type --
   -------------------

   procedure Doc_HTML_Type
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Type_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Type_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Type_List,
                   Info.Type_Header.all,
                   Info.Type_Entity.File_Name.all,
                   Info.Type_Entity.Short_Name.all,
                   Info.Type_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Type_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");

   end Doc_HTML_Type;

   --------------------
   -- Doc_HTML_Entry --
   --------------------

   procedure Doc_HTML_Entry
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin

      --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Entry_Entity.Line)
                            & """></A>  <BR>");

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Entry_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Entry_List,
                   Info.Entry_Header.all,
                   Info.Entry_Entity.File_Name.all,
                   Info.Entry_Entity.Short_Name.all,
                   Info.Entry_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, Info.Entry_Description.all);
      Ada.Text_IO.Put_Line (File, "<HR> ");

   end Doc_HTML_Entry;

   -------------------------
   -- Doc_HTML_Subprogram --
   -------------------------

   procedure Doc_HTML_Subprogram
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
                                     "<H5> Subprogram is called by: </H5>");
            else
               Ada.Text_IO.Put_Line (File,
                                     "<H5> Subprogram calls: </H5>");
            end if;
            Ada.Text_IO.Put_Line (File, "<TABLE>");

            Node := TRL.First (Local_List);

            --  for every reference found write the information to doc file
            for J in 1 .. TRL.Length (Local_List) loop

               --  check if the creating of a link is possible
               if TRL.Data (Node).Set_Link then

                  --  if a called subprogram => link to spec
                  if Called_Subp then
                     Suffix := new String'("_adb.htm");
                  else
                     Suffix := new String'("_ads.htm");
                  end if;
                  declare
                     Body_File : constant String :=
                       TRL.Data (Node).File_Name.all
                       (TRL.Data (Node).File_Name'First ..
                          TRL.Data (Node).File_Name'Last - 4)
                     & Suffix.all;
                  begin
                     Ada.Text_IO.Put_Line
                       (File,
                        "<TR><TD><A href="""
                        & Body_File & "#"
                        & Integer'Image (TRL.Data (Node).Line)
                        & """>"
                        & TRL.Data
                          (Node).Subprogram_Name.all
                        & "</A></TD><TD> in &nbsp&nbsp<I>"
                        & TRL.Data (Node).File_Name.all
                        & "</I></TD><TD>, line: "
                        & Integer'Image (TRL.Data (Node).Line)
                        & "</TD><TD>, column: "
                        & Integer'Image (TRL.Data (Node).Column)
                        & "</TD><TR>");
                  end;
               --  no link at all
               else Ada.Text_IO.Put_Line (File,
                                          "<TR><TD>"
                                          & TRL.Data (Node).Subprogram_Name.all
                                          & "</TD><TD> in &nbsp&nbsp<I>"
                                          & TRL.Data (Node).File_Name.all
                                          & "</I></TD><TD>, line: "
                                          & Integer'Image
                                            (TRL.Data (Node).Line)
                                          & "</TD><TD>, column: "
                                          & Integer'Image
                                            (TRL.Data (Node).Column)
                                          & "</TD></TR>");
               end if;
               Node := TRL.Next (Node);
            end loop;
               Ada.Text_IO.Put_Line (File, "</TABLE>");
         end if;
      end Print_Ref_List;

   begin  --  Doc_HTML_Subprogram

   --  the mark for the HMTL browser to be able to find the position
      Ada.Text_IO.Put_Line (File, "  <A name="""
                            & Integer'Image (Info.Subprogram_Entity.Line)
                            & """></A>  <BR> ");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#DDDDDD"" " &
                            "width=""100%""><TR><TD> <PRE>");

      if Info.Subprogram_Entity.Is_Private then
         Ada.Text_IO.Put_Line (File, "<i> private: </i>" & ASCII.LF);
      end if;

      Format_HTML (File,
                   Info.Subprogram_List,
                   Info.Subprogram_Header.all,
                   Info.Subprogram_Entity.File_Name.all,
                   Info.Subprogram_Entity.Short_Name.all,
                   Info.Subprogram_Entity.Line,
                   False,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);

      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");

      --  write the description to doc file
      Ada.Text_IO.Put_Line (File, Info.Subprogram_Description.all);
      Ada.Text_IO.Put_Line (File, " <BR>  ");

      Print_Ref_List (Info.Subprogram_Entity.Called_List, True);
      Print_Ref_List (Info.Subprogram_Entity.Calls_List, False);

      Ada.Text_IO.Put_Line (File, "<HR> ");
   end Doc_HTML_Subprogram;

   -----------------
   -- Format_HTML --
   -----------------

   procedure Format_HTML
     (File          : Ada.Text_IO.File_Type;
      Entity_List   : Type_Entity_List.List;
      Text          : String;
      File_Name     : String;
      Entity_Name   : String;
      Entity_Line   : Natural;
      Is_Body       : Boolean;
      Process_Body  : Boolean;
      Do_Checks     : Boolean) is

      --  global variables for the callback function
      Last_Index, Last_Line : Natural;

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  the callback function to add HTML code to the text being parsed.
      --  for each possible kind of parsed entity some HTML code will
      --  be added to the Result_Line and the Already_Added_Chars
      --  increased by the number of the added chars in order to keep
      --  track of the current position in the original string.

      -------------------
      -- HTML_Callback --
      -------------------

      function HTML_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean is
         pragma Unreferenced (Partial_Entity);

         Entity_Node      : TEL.List_Node;

         HTML_Name_Head   : constant String := "<A name=""";
         HTML_Name_Middle : constant String := """>";
         HTML_Name_End    : constant String := "</A>";

         HTML_Comment_Prefix : constant String := "<FONT color=""green"">";
         HTML_Comment_Suffix : constant String := "</FONT>";
         HTML_Keyword_Prefix : constant String := "<B>";
         HTML_Keyword_Suffix : constant String := "</B>";
         HTML_String_Prefix  : constant String := "<FONT color=""red"">";
         HTML_String_Suffix  : constant String := "</FONT>";
         HTML_Char_Prefix    : constant String := "<FONT color=""red"">";
         HTML_Char_Suffix    : constant String := "</FONT>";

         procedure Callback_Output
           (Start_Index : Natural;
            End_Index   : Natural;
            Prefix     : String;
            Suffix     : String;
            Check_Tags : Boolean);
         --  Write the formatted text since the last output to doc file
         --  Prefix and Suffix are the HTML code to be put around the
         --  parsed entity. The both index values are needed, as for comment
         --  lines the ASCII.LF at the line should be ignored, so you can't
         --  used always the Sloc_Index values.

         procedure Create_Regular_Link;
         --  will create a regular link to the entity, links to both spec
         --  and body files are possible.

         procedure Create_Special_Link_To_Body;
         --  will create a link to the reference of the entity in the body

         function Link_Should_Be_Set return Boolean;
         --  check if a link to that entity should be set

         function Special_Link_Should_Be_Set return Boolean;
         --  check if a special link to the body should be set
         --  (a special link, because it doesn't link to the declaration
         --  of the entity, but to a reference somewhere in the body)

         function Regular_Link_Should_Be_Set return Boolean;
         --  check if a regular link to the body should be set
         --  (a regular link is a link to the entity's declaration)

         function Is_Called_Reference_Found
           (Line_Nr     : Natural;
            Source_File : String;
            Called_Node :  Entity_List_Information) return Boolean;
         --  looks in the Called_List if the entity is called called in the
         --  given file at the given line.

         procedure Replace_HTML_Tags
           (Input_Text : String);
         --  replaces all "<"  which are by "&lt;" and all ">" by "&gt;"
         --  and writes the output to the doc file.

         procedure Set_Name_Tags
           (Input_Text : String);
         --  sets a "<A name="lind_number"> <A>" in front of each line in the
         --  given strings (if in body file) and writes it to the doc file.

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
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Start_Index - 1));
            else
               Ada.Text_IO.Put (File, Text (Last_Index .. Start_Index - 1));
            end if;

            if Check_Tags then
               Ada.Text_IO.Put (File, Prefix);
               Replace_HTML_Tags (Text (Start_Index .. End_Index));
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

         -------------------
         -- Set_Name_Tags --
         -------------------

         procedure Set_Name_Tags
           (Input_Text : String) is
            Last_Written : Natural;
         begin
            Last_Written := Input_Text'First - 1;
            if Is_Body then
               for J in Input_Text'First .. Input_Text'Last loop
                  if Input_Text (J) = ASCII.LF then
                     Last_Line := Last_Line + 1;
                     Ada.Text_IO.Put (File, Input_Text
                                        (Last_Written + 1 .. J) &
                                        HTML_Name_Head &
                                          Integer'Image (Last_Line) &
                                          HTML_Name_Middle &
                                            HTML_Name_End);
                     Last_Written := J;
                  end if;
               end loop;
            end if;
            Ada.Text_IO.Put (File,
                             Input_Text
                               (Last_Written + 1 .. Input_Text'Last));
         end Set_Name_Tags;

         ---------------------------------
         -- Create_Special_Link_To_Body --
         ---------------------------------

         procedure Create_Special_Link_To_Body is
            Spec_File : constant String :=
              GNAT.Directory_Operations.File_Name
                (TEL.Data (Entity_Node).File_Name.all);
         begin
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Sloc_Start.Index - 1));
            else
               Ada.Text_IO.Put (File,
                                Text (Last_Index .. Sloc_Start.Index - 1));
            end if;

            Ada.Text_IO.Put (File,
                             "<A href=""" &
                             Spec_File (Spec_File'First
                                          .. Spec_File'Last - 4) &
                             "_adb.htm" &
                             "#" &
                             Integer'Image
                               (TEL.Data (Entity_Node).Line_In_Body) &
                             """>" &
                             Text (Sloc_Start.Index ..  Sloc_End.Index) &
                             "</A>");

            Last_Index := Sloc_End.Index + 1;
         end Create_Special_Link_To_Body;

         -------------------------
         -- Create_Regular_Link --
         -------------------------

         procedure Create_Regular_Link is
         begin
            if Sloc_Start.Line > Last_Line then
               Set_Name_Tags (Text (Last_Index .. Sloc_Start.Index - 1));
            else
               Ada.Text_IO.Put (File,
                                Text (Last_Index .. Sloc_Start.Index - 1));
            end if;

            Ada.Text_IO.Put (File,
                             "<A href=""" &
                             GNAT.Directory_Operations.File_Name
                               (Get_Html_File_Name
                                  (TEL.Data (Entity_Node).File_Name.all)) &
                             "#" &
                             Integer'Image (TEL.Data (Entity_Node).Line) &
                             """>" &
                             Text (Sloc_Start.Index .. Sloc_End.Index) &
                             "</A>");

            Last_Index := Sloc_End.Index + 1;
         end Create_Regular_Link;

         ------------------------
         -- Link_Should_Be_Set --
         ------------------------

         function Link_Should_Be_Set return Boolean is
         begin
            return
            --  check if the entity name is also the identiefier name
              TEL.Data (Entity_Node).Short_Name.all
              = To_Lower (Text (Sloc_Start.Index ..
                                           Sloc_End.Index))
            --  and the lines for the entity found in the list
            --  are the same as for the entity passed to Format_HTML,
            --  in the spec of in the body (in the latter case
            --  it's important to look at the declaration line in the
            --  body but also all references where it is called.
            --  Or No Checks should be made.
              and ((not Is_Body and
                      TEL.Data (Entity_Node).Line =
                      Entity_Line)
                   or (Is_Body and
                         TEL.Data (Entity_Node).Line_In_Body =
                         Sloc_Start.Line)
                   or (Is_Body and
                         (TEL.Data (Entity_Node).Kind =
                            Procedure_Entity or
                              TEL.Data (Entity_Node).Kind
                              = Function_Entity or
                              TEL.Data (Entity_Node).Kind
                              = Entry_Entity) and
                             Is_Called_Reference_Found
                               (Sloc_Start.Line,
                                File_Name,
                                TEL.Data (Entity_Node)))
                   or not Do_Checks)
            --  and the kind of the entity is not Other_Entity
            --  ONLY possible: subprograms, exceptions,tasks,
            --  types and packages
              and TEL.Data (Entity_Node).Kind /= Other_Entity
              and TEL.Data (Entity_Node).Kind /= Var_Entity
            --  and this line is not the declararion of the entity
            --  (allowed for subprograms + tasks)
              and (not (TEL.Data (Entity_Node).Line
                          = Sloc_Start.Line and
                            TEL.Data (Entity_Node).File_Name.all
                            = File_Name) or
                         TEL.Data (Entity_Node).Kind =
                         Procedure_Entity or
                           TEL.Data (Entity_Node).Kind = Function_Entity);
         end Link_Should_Be_Set;

         --------------------------------
         -- Special_Link_Should_Be_Set --
         --------------------------------

         function Special_Link_Should_Be_Set return Boolean is
         begin
            --  checks if a special link, a link not to a declaration,
            --  should be set.
            return
            not Is_Body and
              Process_Body and
              TEL.Data (Entity_Node).Line_In_Body > 0 and
              (TEL.Data (Entity_Node).Kind = Procedure_Entity or
                 TEL.Data (Entity_Node).Kind = Function_Entity

               or TEL.Data (Entity_Node).Kind = Entry_Entity
                 );
         end Special_Link_Should_Be_Set;

         --------------------------------
         -- Regular_Link_Should_Be_Set --
         --------------------------------

         function Regular_Link_Should_Be_Set return Boolean is
         begin
            --  a regular link is a link to the entity's declaration
            return
            --  no subprograms/tasks are processed here,
            --  if working on a spec file
            (not (TEL.Data (Entity_Node).Kind =
                    Procedure_Entity or
                    TEL.Data (Entity_Node).Kind =
                    Entry_Entity or
                     TEL.Data (Entity_Node).Kind
                     = Function_Entity) or Is_Body) and
            --  and don't link if it is the entity itself, which
            --  was found in its header => except for subprograms/tasks
            --  (above) which are linked to the body, no linking
            --  here.
              To_Lower (TEL.Data (Entity_Node).Short_Name.all) /=
              To_Lower (Entity_Name);
         end Regular_Link_Should_Be_Set;

         ---------------------------------
         --  Is_Called_Reference_Found  --
         ---------------------------------

         function Is_Called_Reference_Found
           (Line_Nr     : Natural;
            Source_File : String;
            Called_Node : Entity_List_Information) return Boolean is

            use Type_Reference_List;

            Ref_Node : List_Node;
         begin
            if not Is_Empty (Called_Node.Called_List) then
               Ref_Node := First (Called_Node.Called_List);
               for J in 1 .. Length (Called_Node.Called_List) loop
                  if Data (Ref_Node).Line = Line_Nr and
                    Data (Ref_Node).File_Name.all =
                    GNAT.Directory_Operations.File_Name (Source_File) then
                     return True;
                  end if;
                  Ref_Node := Next (Ref_Node);
               end loop;
            end if;
            return False;
         end Is_Called_Reference_Found;

         -----------------------
         -- Replace_HTML_Tags --
         -----------------------

         procedure Replace_HTML_Tags
           (Input_Text : String) is

            Last_Index : Natural;
         begin
            Last_Index := Input_Text'First;

            for J in Input_Text'First .. Input_Text'Last - 1 loop
               if Input_Text (J) = '<' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "&lt;");
                  Last_Index := J + 1;
               elsif Input_Text (J) = '>' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "&glt;");
                  Last_Index := J + 1;
               elsif Input_Text (J) = '&' then
                  Ada.Text_IO.Put (File, Input_Text (Last_Index .. J - 1) &
                                "&amp;");
                  Last_Index := J + 1;
               end if;
            end loop;
            Ada.Text_IO.Put (File, Input_Text (Last_Index .. Input_Text'Last));
         end Replace_HTML_Tags;

      begin  -- HTML_Callback

         case Entity is
            when Comment_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index - 1,
                  HTML_Comment_Prefix, HTML_Comment_Suffix, False);
            when Keyword_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_Keyword_Prefix, HTML_Keyword_Suffix, False);
            when String_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_String_Prefix, HTML_String_Suffix, True);
            when Character_Text => Callback_Output
                 (Sloc_Start.Index, Sloc_End.Index,
                  HTML_Char_Prefix, HTML_Char_Suffix, False);
            when Identifier_Text =>   --  perhaps links can be set:

               --  look in the list, if the identifier is there
               --  if found => try to  make a link;
               --  if not found => ignore!
               if not TEL.Is_Empty (Entity_List) then

                  Entity_Node := TEL.First (Entity_List);
                  for J in 1 .. TEL.Length (Entity_List) loop

                     if Link_Should_Be_Set then
                        if Special_Link_Should_Be_Set then
                           Create_Special_Link_To_Body;
                        elsif Regular_Link_Should_Be_Set then
                           Create_Regular_Link;
                        end if;
                        return False;
                     end if;

                     Entity_Node := TEL.Next (Entity_Node);
                  end loop;

               end if;
            when others => null;
         end case;
         return False;
      end HTML_Callback;

   begin  --  Format_HTML

      --  set the global varibales
      Last_Index := 1;
      Last_Line  := 0;

      --  parse the entities in Text
      Parse_Entities (Ada_Lang,
                      Text,
                      HTML_Callback'Unrestricted_Access);

      --  write the rest of the text, since the last found parsed entity
      if Last_Index < Text'Last then
         Ada.Text_IO.Put (File, Text (Last_Index .. Text'Last));
      end if;
   end Format_HTML;

   ---------------------
   -- Doc_HTML_Header --
   ---------------------

   procedure Doc_HTML_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Body_File_Name : GNAT.OS_Lib.String_Access;
   begin

      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD>");
      Ada.Text_IO.Put_Line (File, " <H1>  Package <I>");
      Ada.Text_IO.Put_Line (File, " <A NAME=""" &
                            Integer'Image (Info.Header_Line)
                            & """>");

      --  check if should set a link to the body file
      if Info.Header_Link and
        File_Extension (Info.Header_File.all) = ".ads" then
         Body_File_Name :=
           new String '(Info.Header_File.all (Info.Header_File'First
                                                .. Info.Header_File'Last - 4)
                           & "_adb.htm");

         Ada.Text_IO.Put_Line (File, "<A HREF=""" &
                               File_Name (Body_File_Name.all) &
                               """> ");
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A></A></I></H1>");
      --  check if should set a link to the spec file
      elsif File_Extension (Info.Header_File.all) = ".adb" then
         Body_File_Name :=
           new String '(Info.Header_File.all (Info.Header_File'First
                                                .. Info.Header_File'Last - 4)
                           & "_ads.htm");

         Ada.Text_IO.Put_Line (File, "<A href=""" &
                               File_Name (Body_File_Name.all) &
                               """> ");
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A></A></I></H1> ");
      else
         Ada.Text_IO.Put_Line (File, Info.Header_Package.all &
                               "</A> </I> </H1>");
      end if;

      Ada.Text_IO.Put_Line (File, "</TD></TR></TABLE>");
      if File_Extension (Info.Header_File.all) = ".adb" then
         Ada.Text_IO.Put_Line (File, "<PRE>");
      end if;
      Ada.Text_IO.Put_Line (File, "<HR>");
   end Doc_HTML_Header;

   ---------------------
   -- Doc_HTML_Footer --
   ---------------------

   procedure Doc_HTML_Footer
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
   begin
      if File_Extension (Info.Footer_File.all) = ".adb" then
         Ada.Text_IO.Put_Line (File, "</PRE>");
      end if;
   end Doc_HTML_Footer;

   --------------------------------
   -- Doc_HTML_Unit_Index_Header --
   --------------------------------

   procedure Doc_HTML_Unit_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is

      Frame_File       : File_Type;
      Source_File_Node : Type_Source_File_List.List_Node;
   begin

      Source_File_Node :=
        Type_Source_File_List.First (Info.Unit_File_List);

      --  create the main frame file
      Create (Frame_File,
              Out_File,
              Info.Doc_Info_Options.Doc_Directory.all & "index.htm");
      Ada.Text_IO.Put_Line (Frame_File, "<HTML> ");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<HEAD>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<TITLE> Index </TITLE>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</HEAD>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<FRAMESET cols=""30%,70%"">");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<FRAME src=""index_unit.htm"" " &
                            "NAME=""index"" >");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<FRAME src=""" &
                            Base_Name
                              (Get_Doc_File_Name
                                 (Type_Source_File_List.Data
                                    (Source_File_Node).File_Name.all,
                                  Info.Doc_Info_Options.Doc_Directory.all,
                                  Info.Doc_Info_Options.Doc_Suffix.all)) &
                            """ name=""main"" >");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</FRAMESET>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<NOFRAMES>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "<BODY>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</BODY>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</NOFRAMES>");
      Ada.Text_IO.New_Line (Frame_File);
      Ada.Text_IO.Put_Line (Frame_File, "</HTML>");
      Ada.Text_IO.New_Line (Frame_File);
      Close (Frame_File);

      --  create the header for the unit index file
      Ada.Text_IO.Put_Line (File, "<HTML> ");
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<BASE target=""main"">");
      Ada.Text_IO.Put_Line (File, "<META http-equiv=""Content-Type"" " &
                            "content=""" &
                            "text/html; charset=" &
                            "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                                  "width=""100%""><TR><TD> <PRE>");
      Ada.Text_IO.Put_Line (File, "<H2> Unit Index </H2> ");
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.Put_Line (File, "<H4> <a href=""index_sub.htm"" " &
                            "target=""index""> Subprogram " &
                            "Index </a> <br>");
      Ada.Text_IO.Put_Line (File, " <A href=""index_type.htm"" " &
                              "target=""index""> Type Index </A> </H4><BR>");
      Ada.Text_IO.Put_Line (File, "<HR> <BR>");
   end Doc_HTML_Unit_Index_Header;

   --------------------------------
   -- Doc_HTML_Sub_Index_Header --
   --------------------------------

   procedure Doc_HTML_Sub_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin
      Ada.Text_IO.Put_Line (File, "<HTML> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<BASE target=""main"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<META http-equiv=""Content-" &
                            "Type"" content=""" &
                            "text/html; charset=" &
                            "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                            "width=""100%""><TR><TD> <PRE>");
      Ada.Text_IO.Put_Line (File, "<H2> Subprogram Index </H2> ");
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<H4> <A href=""index_unit.htm""  " &
                            "target=""index""> Unit Index </A> <BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, " <A href=""index_type.htm"" " &
                            "target=""index""> Type Index </A> </H4><BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HR> <BR>");
      Ada.Text_IO.New_Line (File);

   end Doc_HTML_Sub_Index_Header;

   --------------------------------
   -- Doc_HTML_Type_Index_Header --
   --------------------------------

   procedure Doc_HTML_Type_Index_Header
     (File   : in Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin

      Ada.Text_IO.Put_Line (File, "<HTML> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HEAD>");
      Ada.Text_IO.Put_Line (File, "<BASE target=""main"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<META http-equiv" &
                            "=""Content-Type"" content=""" &
                            "text/html; charset=" & "ISO-8859-1" & """>");
      Ada.Text_IO.Put_Line (File, "</HEAD>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BODY bgcolor=""white"">");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<TABLE  bgcolor=""#9999FF"" " &
                              "width=""100%""><TR><TD> <PRE>");
      Ada.Text_IO.Put_Line (File, "<H2> Type Index </H2> ");
      Ada.Text_IO.Put_Line (File, "</PRE></TD></TR></TABLE>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<H4> <A href=""index_unit.htm"" " &
                            "target = ""index""> Unit Index </A> <BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, " <A href=""index_sub.htm"" " &
                            "target=""index""> Subprogram " &
                            "Index </A></H4> <BR>");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<HR> <BR>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Type_Index_Header;

   -------------------------
   -- Doc_HTML_Index_Item --
   -------------------------

   procedure Doc_HTML_Index_Item
     (File    : in Ada.Text_IO.File_Type;
      Info    : Doc_Info) is
   begin
      Ada.Text_IO.Put_Line (File, " <A href=""" & Info.Item_Doc_File.all
                            & "#" & Integer'Image (Info.Item_Line)
                            & """ target=""main""> "
                            & Info.Item_Name.all & "</A>");

      Ada.Text_IO.Put_Line (File, " <BR> &nbsp&nbsp&nbsp&nbsp&nbsp in " &
                            Info.Item_File.all);
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "<BR>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Index_Item;

   ------------------------
   -- Doc_HTML_Index_End --
   ------------------------

   procedure Doc_HTML_Index_End
     (File   : Ada.Text_IO.File_Type;
      Info   : Doc_Info) is
      pragma Unreferenced (Info);
   begin
      Ada.Text_IO.Put_Line (File, "</BODY> ");
      Ada.Text_IO.New_Line (File);
      Ada.Text_IO.Put_Line (File, "</HTML>");
      Ada.Text_IO.New_Line (File);
   end Doc_HTML_Index_End;

   -------------------
   -- Doc_HTML_Body --
   -------------------

   procedure Doc_HTML_Body
     (File   : in Ada.Text_IO.File_Type;
      Info   : in out Doc_Info) is
   begin
      Format_HTML (File,
                   Info.Body_Entity_List,
                   Info.Body_Text.all,
                   Info.Body_File.all,
                   "",
                   0,
                   True,
                   Info.Doc_Info_Options.Process_Body_Files,
                   True);
   end Doc_HTML_Body;

   ------------------------
   -- Get_Html_File_Name --
   ------------------------

   function Get_Html_File_Name
     (File : String) return String is
   begin
      if File_Extension (File) = ".ads" then
         return File (File'First .. File'Last - 4) & "_ads.htm";
      else
         return File (File'First .. File'Last - 4) & "_adb.htm";
      end if;
   end Get_Html_File_Name;

end Docgen.Html_Output;
