-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2005-2007, AdaCore                 --
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

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Entities.Queries;          use Entities.Queries;
with File_Utils;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with OS_Utils;                  use OS_Utils;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with String_Utils;              use String_Utils;
with Templates_Parser;          use Templates_Parser;

package body Docgen.Backend.Text is

   --------------------
   -- Template Files --
   --------------------

   Cache : constant Boolean := True;
   --  Control the templates parser cache option

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Callback_Output
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity      : Entities_Kind;
      Entity_Line : Natural);
   --  Write the formatted text since the last output to doc file.
   --  Prefix and Suffix are the Text code to be put around the
   --  parsed entity. Both index values are needed, as for comment
   --  lines the ASCII.LF at the line should be ignored, so you can't
   --  always use the Sloc_Index values.

   function Get_Text_File_Name
     (B         : access Backend;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) return String;
   --  Create file name from the full path of the source file using the
   --  extension provided in the backend.
   --  for ex.: from util/src/docgen.adb the name docgen_adb.htm is created
   --  for an HTML backend for which extension is "htm".

   procedure Set_Name_Tags
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Input_Text  : String;
      Entity_Line : Natural);
   --  Set a "<a name="line_number"> <a>" in front of each line in the
   --  given strings (if in body file) and writes it to the doc file.

   procedure Output_Entity
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Level             : Natural;
      Options           : All_Options;
      Entity            : Entity_Information;
      Processed_Sources : Type_Source_File_Table.HTable);
   --  Print a reference to a specific entity, possibly with an hyper-link

   function Get_Template_File_Name
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : Entities_Kind) return String;
   --  Returns the template filename to use to render the entity

   --------------
   -- Doc_Open --
   --------------

   procedure Doc_Open
     (B          : access Backend;
      Kernel     : access Kernel_Handle_Record'Class;
      Result     : in out Unbounded_String;
      Open_Title : String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse (Get_Template_File_Name (B, Kernel, File_Header_Kind),
                   (1 => Assoc ("TITLE", Open_Title)), Cache)));
   end Doc_Open;

   ---------------
   -- Doc_Close --
   ---------------

   procedure Doc_Close
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse (Get_Template_File_Name (B, Kernel, File_Footer_Kind),
                   Cached => Cache)));
   end Doc_Close;

   ------------------
   -- Doc_Subtitle --
   ------------------

   procedure Doc_Subtitle
     (B             : access Backend;
      Kernel        : access Kernel_Handle_Record'Class;
      Result        : in out Unbounded_String;
      Level         : Natural;
      Subtitle_Name : String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Subtitle_Kind),
               (Assoc ("LEVEL", Level),
                Assoc ("SUBTITLE", Subtitle_Name),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Subtitle;

   ----------------------
   -- Doc_Package_Desc --
   ----------------------

   procedure Doc_Package_Desc
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Level       : Natural;
      Description : String)
   is
      pragma Unreferenced (Level);
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse (Get_Template_File_Name (B, Kernel, Package_Desc_Kind),
                   (1 => Assoc ("DESCRIPTION", Description)), Cache)));
   end Doc_Package_Desc;

   -----------------
   -- Doc_Package --
   -----------------

   procedure Doc_Package
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Package_Entity   : Entity_Information;
      Package_Header   : String)
   is
      Name  : constant String :=
                Image (Get_Line (Get_Declaration_Of (Package_Entity)));
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Package_Header,
         Get_Filename
           (Get_File (Get_Declaration_Of (Package_Entity))),
         Get_Line (Get_Declaration_Of (Package_Entity)),
         No_Body_Line_Needed,
         False,
         Options,
         Source_File_List,
         Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Package_Kind),
               (Assoc ("NAME", Name),
                Assoc ("BLOCK", Block),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Package;

   ----------------------------
   -- Doc_Package_Open_Close --
   ----------------------------

   procedure Doc_Package_Open_Close
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      List_Ref_In_File  : in out List_Reference_In_File.List;
      Source_File_List  : Type_Source_File_Table.HTable;
      Options           : All_Options;
      Level             : Natural;
      Entity            : Entity_Information;
      Header            : String)
   is
      Name  : constant String :=
                Image (Get_Line (Get_Declaration_Of (Entity)));
      Block : Unbounded_String;
   begin
      --  This package contains declarations.
      --  Here we print either the header (package ... is)
      --  or the footer (end ...;)

      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Header,
         Get_Filename (Get_File (Get_Declaration_Of (Entity))),
         Get_Line (Get_Declaration_Of (Entity)),
         No_Body_Line_Needed,
         False, Options, Source_File_List, Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Package_Kind),
               (Assoc ("NAME", Name),
                Assoc ("BLOCK", Block),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Package_Open_Close;

   --------------
   -- Doc_With --
   --------------

   procedure Doc_With
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      With_Header      : String;
      With_File        : GNATCOLL.VFS.Virtual_File;
      With_Header_Line : Natural)
   is
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         With_Header,
         With_File,
         With_Header_Line,
         No_Body_Line_Needed,
         False, Options, Source_File_List, Level, Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, With_Kind),
               (Assoc ("BLOCK", Block),
                Assoc ("INDENT", Get_Indent (B.all))),
               Cache)));
   end Doc_With;

   -------------
   -- Doc_Var --
   -------------

   procedure Doc_Var
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String)
   is
      Line  : constant String :=
                Image (Get_Line (Get_Declaration_Of (Entity)));
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Header,
         Get_Filename (Get_File (Get_Declaration_Of (Entity))),
         Get_Line (Get_Declaration_Of (Entity)),
         No_Body_Line_Needed,
         False,
         Options,
         Source_File_List,
         Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Variable_Kind),
               (Assoc ("LINE", Line),
                Assoc ("BLOCK", Block),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Var;

   -------------------
   -- Doc_Exception --
   -------------------

   procedure Doc_Exception
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String)
   is
      Line  : constant String :=
                Image (Get_Line (Get_Declaration_Of (Entity)));
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Header,
         Get_Filename (Get_File (Get_Declaration_Of (Entity))),
         Get_Line (Get_Declaration_Of (Entity)),
         No_Body_Line_Needed,
         False, Options, Source_File_List, Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Exception_Kind),
               (Assoc ("DECL_LINE", Line),
                Assoc ("BLOCK", Block),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Exception;

   --------------
   -- Doc_Type --
   --------------

   procedure Doc_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String)
   is
      Line  : constant String :=
                Image (Get_Line (Get_Declaration_Of (Entity)));
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Header,
         Get_Filename (Get_File (Get_Declaration_Of (Entity))),
         Get_Line (Get_Declaration_Of (Entity)),
         No_Body_Line_Needed,
         False, Options, Source_File_List, Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Type_Kind),
               (Assoc ("LINE", Line),
                Assoc ("BLOCK", Block),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Type;

   ---------------------
   -- Doc_Tagged_Type --
   ---------------------

   procedure Doc_Tagged_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Level            : Natural;
      Entity           : Entity_Information)
   is
      type T_Entities is record
         Ref_File    : Tag;     -- ref file name
         Name        : Tag;     -- entity name
         Decl_File   : Tag;     -- entity declaration file
         Decl_Line   : Tag;     -- entity declaration line
         Decl_Column : Tag;     -- entity declaration column
      end record;

      procedure Output_Entity
        (Entity : Entity_Information; E_Data : in out T_Entities);
      --  Output Text info related to Entity.

      Parents  : constant Entity_Information_Array :=
        Get_Parent_Types (Entity);
      Children : Children_Iterator := Get_Child_Types (Entity);

      Has_Parents  : constant Boolean := Parents'Length > 0;
      Has_Children : Boolean := False;

      C_Entities   : T_Entities;
      P_Entities   : T_Entities;

      -------------------
      -- Output_Entity --
      -------------------

      procedure Output_Entity
        (Entity : Entity_Information; E_Data : in out T_Entities)
      is
         F    : constant Source_File := Get_File (Get_Declaration_Of (Entity));
         Info : constant Source_File_Information :=
                  Type_Source_File_Table.Get (Source_File_List, F);
         Line : constant String :=
                  Image (Get_Line (Get_Declaration_Of (Entity)));
         Col  : constant String :=
                  Image (Integer (Get_Column (Get_Declaration_Of (Entity))));
      begin
         if Info /= No_Source_File_Information then
            E_Data.Ref_File := E_Data.Ref_File & Info.Doc_File_Name.all;
         else
            E_Data.Ref_File := E_Data.Ref_File & "";
         end if;

         E_Data.Name        := E_Data.Name & Get_Name (Entity).all;
         E_Data.Decl_File   := E_Data.Decl_File & Base_Name (Get_Filename (F));
         E_Data.Decl_Line   := E_Data.Decl_Line & Line;
         E_Data.Decl_Column := E_Data.Decl_Column & Col;
      end Output_Entity;

   begin
      --  Parents

      if Has_Parents then
         for P in Parents'Range loop
            Output_Entity (Parents (P), P_Entities);
         end loop;
      end if;

      --  Children

      while not At_End (Children) loop
         if Get (Children) /= null then
            Has_Children := True;
            Output_Entity (Get (Children), C_Entities);
         end if;

         Next (Children);
      end loop;

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Tagged_Type_Kind),
               (Assoc ("HAS_PARENTS", Has_Parents),
                Assoc ("P_REF_FILE", P_Entities.Ref_File),
                Assoc ("P_NAME", P_Entities.Name),
                Assoc ("P_DECL_FILE", P_Entities.Decl_File),
                Assoc ("P_DECL_LINE", P_Entities.Decl_Line),
                Assoc ("P_DECL_COLUMN", P_Entities.Decl_Column),
                Assoc ("HAS_CHILDREN", Has_Children),
                Assoc ("C_REF_FILE", C_Entities.Ref_File),
                Assoc ("C_NAME", C_Entities.Name),
                Assoc ("C_DECL_FILE", C_Entities.Decl_File),
                Assoc ("C_DECL_LINE", C_Entities.Decl_Line),
                Assoc ("C_DECL_COLUMN", C_Entities.Decl_Column),
                Assoc ("INDENT", Level * Get_Indent (B.all))), Cache)));
   end Doc_Tagged_Type;

   ---------------
   -- Doc_Entry --
   ---------------

   procedure Doc_Entry
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_Information;
      Header           : String)
   is
      Line  : constant String :=
                Image (Get_Line (Get_Declaration_Of (Entity)));
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Header,
         Get_Filename (Get_File (Get_Declaration_Of (Entity))),
         Get_Line (Get_Declaration_Of (Entity)),
         No_Body_Line_Needed,
         False, Options, Source_File_List, Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Type_Kind),
               (Assoc ("LINE", Line),
                Assoc ("BLOCK", Block),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Entry;

   ---------------------------
   -- Doc_Caller_References --
   ---------------------------

   procedure Doc_Caller_References
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Options           : All_Options;
      Level             : Natural;
      Callers           : Entities.Entity_Information_Arrays.Instance;
      Processed_Sources : Type_Source_File_Table.HTable)
   is
      use Entity_Information_Arrays;
      Count : constant Index_Type :=
                Entity_Information_Arrays.Length (Callers);
      Block : Unbounded_String;
   begin
      if Count /= 0 then
         for C in Entity_Information_Arrays.First .. Last (Callers) loop
            Output_Entity
              (B, Kernel, Block, Level, Options, Callers.Table (C),
               Processed_Sources);
         end loop;
      end if;

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Caller_References_Kind),
               (Assoc ("BLOCK", Block),
                Assoc ("COUNT", Natural (Count)),
                Assoc ("INDENT", Level * Get_Indent (B.all))), Cache)));
   end Doc_Caller_References;

   --------------------------
   -- Doc_Calls_References --
   --------------------------

   procedure Doc_Calls_References
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Options           : All_Options;
      Level             : Natural;
      Calls             : Entities.Entity_Information_Arrays.Instance;
      Processed_Sources : Type_Source_File_Table.HTable)
   is
      use Entity_Information_Arrays;
      Count : constant Index_Type :=
                Entity_Information_Arrays.Length (Calls);
      Block : Unbounded_String;
   begin
      if Count /= 0 then
         for C in Entity_Information_Arrays.First .. Last (Calls) loop
            Output_Entity
              (B, Kernel, Block, Level, Options, Calls.Table (C),
               Processed_Sources);
         end loop;
      end if;

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Calls_References_Kind),
               (Assoc ("BLOCK", Block),
                Assoc ("COUNT", Natural (Count)),
                Assoc ("INDENT", Level * Get_Indent (B.all))), Cache)));
   end Doc_Calls_References;

   -------------------
   -- Output_Entity --
   -------------------

   procedure Output_Entity
     (B                 : access Backend;
      Kernel            : access Kernel_Handle_Record'Class;
      Result            : in out Unbounded_String;
      Level             : Natural;
      Options           : All_Options;
      Entity            : Entity_Information;
      Processed_Sources : Type_Source_File_Table.HTable)
   is
      F              : constant Virtual_File :=
                         Get_Filename (Get_File (Get_Declaration_Of (Entity)));
      Set_Link       : Boolean;
      Source_Visible : Boolean;
      T_Set          : Translate_Set;
   begin
      Set_Link := Options.Link_All
        or else Source_File_In_List
          (Processed_Sources, Get_File (Get_Declaration_Of (Entity)));

      if Set_Link then
         Source_Visible :=
           (Get_Attributes (Entity) (Global) or else Options.Show_Private)
           and then
             (Options.Process_Body_Files or else Type_Source_File_Table.Get
                  (Processed_Sources,
                   Get_File (Get_Declaration_Of (Entity))).Is_Spec);
      end if;

      if Set_Link and then Source_Visible then
         Insert
           (T_Set,
            Assoc ("REF_FILE",
                   Get_Text_File_Name (B, Kernel, Full_Name (F).all)));
      end if;

      Insert (T_Set, Assoc ("NAME", Get_Name (Entity).all));
      Insert (T_Set, Assoc ("DECL_FILE", Base_Name (F)));
      Insert (T_Set, Assoc ("INDENT", Level * Get_Indent (B.all)));
      Insert
        (T_Set,
         Assoc ("DECL_LINE",
                Image (Get_Line (Get_Declaration_Of (Entity)))));
      Insert
        (T_Set,
         Assoc ("DECL_COLUMN",
                Image (Integer (Get_Column (Get_Declaration_Of (Entity))))));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Entity_Kind),
               T_Set, Cache)));
   end Output_Entity;

   --------------------
   -- Doc_Subprogram --
   --------------------

   procedure Doc_Subprogram
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Entity           : Entity_List_Information;
      Header           : String)
   is
      Block : Unbounded_String;
   begin
      Format_Code
        (B, Kernel, Block,
         List_Ref_In_File,
         Header,
         Get_Filename (Get_File (Get_Declaration_Of (Entity.Entity))),
         Get_Line (Get_Declaration_Of (Entity.Entity)),
         Get_Line (Entity.Line_In_Body),
         False, Options, Source_File_List, Level,
         Get_Indent (B.all));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Subprogram_Kind),
               (Assoc ("BLOCK", Block),
                Assoc ("LINE", Image
                         (Get_Line (Get_Declaration_Of (Entity.Entity)))),
                Assoc ("INDENT", Level * Get_Indent (B.all))),
               Cache)));
   end Doc_Subprogram;

   ----------------
   -- Doc_Header --
   ----------------

   procedure Doc_Header
     (B              : access Backend;
      Kernel         : access Kernel_Handle_Record'Class;
      Result         : in out Unbounded_String;
      Header_File    : Virtual_File;
      Header_Package : String;
      Header_Line    : Natural;
      Header_Link    : Boolean)
   is
      T_Set : Translate_Set;
   begin
      if Header_Link then
         Insert
           (T_Set,
            Assoc
              ("HEADER_LINK",
               Get_Text_File_Name
                 (B, Kernel,
                  Other_File_Base_Name
                    (Get_Project_From_File
                       (Project_Registry (Get_Registry (Kernel).all),
                        Header_File),
                     Header_File))));
      end if;

      Insert (T_Set, Assoc ("FIRST_FILE_LINE", First_File_Line));
      Insert (T_Set, Assoc ("LINE", Header_Line));
      Insert (T_Set, Assoc ("HEADER_PACKAGE", Header_Package));

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Header_Kind),
               T_Set,
               Cache)));
   end Doc_Header;

   ------------------------
   -- Doc_Header_Private --
   ------------------------

   procedure Doc_Header_Private
     (B            : access Backend;
      Kernel       : access Kernel_Handle_Record'Class;
      Result       : in out Unbounded_String;
      Header_Title : String;
      Level        : Natural) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Private_Header_Kind),
               (Assoc ("LEVEL", Level),
                Assoc ("INDENT", Level * Get_Indent (B.all)),
                Assoc ("HEADER_TITLE", Header_Title)), Cache)));
   end Doc_Header_Private;

   ----------------
   -- Doc_Footer --
   ----------------

   procedure Doc_Footer
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse (Get_Template_File_Name (B, Kernel, Footer_Kind),
                   Cached => Cache)));
   end Doc_Footer;

   --------------------
   -- Doc_Unit_Index --
   --------------------

   procedure Doc_Unit_Index
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Doc_Directory    : String)
   is
      pragma Unreferenced (Level);
      use Type_Source_File_Table;
      Frame_File : File_Descriptor;
      Node       : Type_Source_File_Table.Iterator;
      FInfo      : Source_File_Information;

   begin
      --  ??? Should get the first one in alphabetical order
      Get_First (Source_File_List, Node);
      FInfo := Type_Source_File_Table.Get (Source_File_List, Get_Key (Node));

      --  Create the main frame file

      Frame_File := Create_File
        (Doc_Directory & "index." & B.Output_Description.Extension.all,
         Binary);

      Put_Line
        (Frame_File,
         Parse
           (Get_Template_File_Name (B, Kernel, Main_Frame_Kind),
            (1 => Assoc ("FRAME_REF", Base_Name (FInfo.Doc_File_Name.all)))));

      Close (Frame_File);

      --  Create the header for the unit index file

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Unit_Index_Header_Kind),
               (1 => Assoc ("TAGGED_TYPE_INDEX", Options.Tagged_Types)),
               Cache)));
   end Doc_Unit_Index;

   --------------------------
   -- Doc_Subprogram_Index --
   --------------------------

   procedure Doc_Subprogram_Index
     (B       : access Backend;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Options : All_Options) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name
                 (B, Kernel, Subprogram_Index_Header_Kind),
               (1 => Assoc ("TAGGED_TYPE_INDEX", Options.Tagged_Types)),
               Cache)));
   end Doc_Subprogram_Index;

   --------------------
   -- Doc_Type_Index --
   --------------------

   procedure Doc_Type_Index
     (B       : access Backend;
      Kernel  : access Kernel_Handle_Record'Class;
      Result  : in out Unbounded_String;
      Options : All_Options) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Type_Index_Header_Kind),
               (1 => Assoc ("TAGGED_TYPE_INDEX", Options.Tagged_Types)),
               Cache)));
   end Doc_Type_Index;

   ---------------------------
   -- Doc_Tagged_Type_Index --
   ---------------------------

   procedure Doc_Tagged_Type_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name
                 (B, Kernel, Tagged_Type_Index_Header_Kind),
               Cached => Cache)));
   end Doc_Tagged_Type_Index;

   ---------------------------
   -- Doc_Index_Tagged_Type --
   ---------------------------

   procedure Doc_Index_Tagged_Type
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Source_File_List : Type_Source_File_Table.HTable;
      Entity           : Entity_Information;
      Family           : Family_Type)
   is
      FInfo : Source_File_Information;
      T_Set : Translate_Set;
   begin
      Insert (T_Set, Assoc ("FAMILY", Family_Type'Image (Family)));

      case Family is
         when Main | Parent_With_Link | Child_With_Link =>
            FInfo := Type_Source_File_Table.Get
              (Source_File_List, Get_File (Get_Declaration_Of (Entity)));

            Insert (T_Set, Assoc ("REF_FILE", FInfo.Doc_File_Name.all));
            Insert
              (T_Set,
               Assoc ("LINE", Image (Get_Line (Get_Declaration_Of (Entity)))));
            Insert (T_Set, Assoc ("NAME", Get_Name (Entity).all));

         when No_Parent | No_Child =>
            null;

         when Parent_Without_Link | Child_Without_Link =>
            --  The parent/child of the tagged type is not declared in the
            --  processed files. Link can't be made.
            Insert (T_Set, Assoc ("NAME", Get_Name (Entity).all));
      end case;

      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Tagged_Type_Index_Kind),
               T_Set, Cache)));
   end Doc_Index_Tagged_Type;

   --------------------
   -- Doc_Index_Item --
   --------------------

   procedure Doc_Index_Item
     (B         : access Backend;
      Kernel    : access Kernel_Handle_Record'Class;
      Result    : in out Unbounded_String;
      Name      : String;
      Item_File : Entities.Source_File;
      Line      : Natural;
      Doc_File  : String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Item_Index_Kind),
               (Assoc ("REF_FILE", Doc_File),
                Assoc ("DECL_LINE", Line),
                Assoc ("NAME", Name),
                Assoc ("DECL_FILE", Base_Name (Get_Filename (Item_File)))),
               Cache)));
   end Doc_Index_Item;

   -----------------------
   -- Doc_Private_Index --
   -----------------------

   procedure Doc_Private_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Title  : String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Private_Index_Header_Kind),
               (1 => Assoc ("TITLE", Title)), Cache)));
   end Doc_Private_Index;

   ----------------------
   -- Doc_Public_Index --
   ----------------------

   procedure Doc_Public_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String;
      Title  : String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Public_Index_Header_Kind),
               (1 => Assoc ("TITLE", Title)), Cache)));
   end Doc_Public_Index;

   ----------------------
   -- Doc_End_Of_Index --
   ----------------------

   procedure Doc_End_Of_Index
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Result : in out Unbounded_String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Index_Footer_Kind),
               Cached => Cache)));
   end Doc_End_Of_Index;

   --------------
   -- Doc_Body --
   --------------

   procedure Doc_Body
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Source_File_List : Type_Source_File_Table.HTable;
      Options          : All_Options;
      Level            : Natural;
      Body_File        : GNATCOLL.VFS.Virtual_File;
      Body_Text        : String) is
   begin
      Format_Code
        (B, Kernel, Result,
         List_Ref_In_File,
         Body_Text,
         Body_File,
         First_File_Line,
         No_Body_Line_Needed,
         Is_Body          => True,
         Options          => Options,
         Source_File_List => Source_File_List,
         Level            => Level,
         Indent           => Get_Indent (B.all));
   end Doc_Body;

   ---------------------
   -- Doc_Description --
   ---------------------

   procedure Doc_Description
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Level       : Natural;
      Description : String) is
   begin
      Append
        (Result,
         Unbounded_String'
           (Parse
              (Get_Template_File_Name (B, Kernel, Description_Kind),
               (Assoc ("DESCRIPTION", Description),
                Assoc ("INDENT", Level * Get_Indent (B.all))), Cache)));
   end Doc_Description;

   ------------------------
   -- Get_Text_File_Name --
   ------------------------

   function Get_Text_File_Name
     (B         : access Backend;
      Kernel    : access Kernel_Handle_Record'Class;
      File_Name : String) return String
   is
      pragma Unreferenced (Kernel);
      Ext  : constant String := File_Extension (File_Name);
      Temp : constant String := Base_Name (File_Name, Ext) & '_' &
                                  Ext (Ext'First + 1 .. Ext'Last);
   begin
      return Temp & '.' & B.Output_Description.Extension.all;
   end Get_Text_File_Name;

   ----------------------------
   -- Get_Template_File_Name --
   ----------------------------

   function Get_Template_File_Name
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : Entities_Kind) return String
   is
      S_Prefix   : constant String := "share/gps/docgen/";
      --  System prefix
      H_Prefix   : constant String := "docgen/";
      --  Home prefix
   begin
      if B.Output_Description.Entities_Templates (Entity) = null then
         return "";

      else
         declare
            Des      : Output_Description renames B.Output_Description.all;
            Filename : constant String := Des.Entities_Templates (Entity).all;
         begin
            --  First we look for the template file in the home directory

            if Is_Regular_File
              (Get_Home_Dir (Kernel) & H_Prefix & Filename)
            then
               return Get_Home_Dir (Kernel) & H_Prefix & Filename;
            else
               return Get_System_Dir (Kernel) & S_Prefix & Filename;
            end if;
         end;
      end if;
   end Get_Template_File_Name;

   ---------------------
   -- Callback_Output --
   ---------------------

   procedure Callback_Output
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity      : Entities_Kind;
      Entity_Line : Natural) is
   begin
      if Sloc_Start.Line > Get_Last_Line (B.all) then
         Set_Name_Tags
           (B, Kernel, Result,
            Text (Get_Last_Index (B.all) .. Sloc_Start.Index - 1),
            Entity_Line);
      else
         Append (Result,
                 Text (Get_Last_Index (B.all) .. Sloc_Start.Index - 1));
      end if;

      --  Write entity

      declare
         Entity_Name : constant String :=
           Text (Sloc_Start.Index .. Sloc_End.Index);
      begin
         Append
           (Result,
            Unbounded_String'
              (Parse (Get_Template_File_Name (B, Kernel, Entity),
                      (1 => Assoc ("TEXT", Entity_Name)))));
      end;

      Set_Last_Index (B.all, Sloc_End.Index + 1);
      Set_Last_Line (B.all, Sloc_End.Line);
   end Callback_Output;

   -------------------
   -- Set_Name_Tags --
   -------------------

   procedure Set_Name_Tags
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Input_Text  : String;
      Entity_Line : Natural)
   is
      Last_Written : Natural := Input_Text'First - 1;
   begin
      --  Flush current line

      for J in Input_Text'Range loop
         --  ??? shouldn't we be using the last index as set in B
         if Input_Text (J) = ASCII.LF or else J = Input_Text'Last then
            Set_Last_Line (B.all, Get_Last_Line (B.all) + 1);
            Append (Result, Input_Text (Last_Written + 1 .. J));
            Last_Written := J;
            exit;
         end if;
      end loop;

      --  Output all following lines with a line number in front

      for J in Last_Written + 1 .. Input_Text'Last loop
         --  ??? shouldn't we be using the last index as set in B
         if Input_Text (J) = ASCII.LF or else J = Input_Text'Last then
            Set_Last_Line (B.all, Get_Last_Line (B.all) + 1);
            Append
              (Result,
               Unbounded_String'
                 (Parse
                    (Get_Template_File_Name (B, Kernel, Block_Kind),
                     (Assoc ("LINE",
                             Image (Get_Last_Line (B.all) + Entity_Line - 1)),
                      Assoc ("BLOCK", Input_Text (Last_Written + 1 .. J))),
                     Cache)));
            Last_Written := J;
         end if;
      end loop;
   end Set_Name_Tags;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (B : access Backend; Text : String) is
   begin
      Set_Last_Line (B.all, 0);
      Set_Last_Index (B.all, Text'First);
   end Initialize;

   --------------------
   -- Format_Comment --
   --------------------

   procedure Format_Comment
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B, Kernel, Result,
         Text,
         Sloc_Start,
         Sloc_End,
         Comment_Kind,
         Entity_Line);
   end Format_Comment;

   --------------------
   -- Format_Keyword --
   --------------------

   procedure Format_Keyword
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B, Kernel, Result,
         Text,
         Sloc_Start,
         Sloc_End,
         Keyword_Kind,
         Entity_Line);
   end Format_Keyword;

   -------------------
   -- Format_String --
   -------------------

   procedure Format_String
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B, Kernel, Result,
         Text,
         Sloc_Start,
         Sloc_End,
         String_Kind,
         Entity_Line);
   end Format_String;

   ----------------------
   -- Format_Character --
   ----------------------

   procedure Format_Character
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Sloc_Start  : Source_Location;
      Sloc_End    : Source_Location;
      Entity_Line : Natural) is
   begin
      Callback_Output
        (B, Kernel, Result,
         Text,
         Sloc_Start,
         Sloc_End,
         Char_Kind,
         Entity_Line);
   end Format_Character;

   -----------------------
   -- Format_Identifier --
   -----------------------

   procedure Format_Identifier
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      List_Ref_In_File : in out List_Reference_In_File.List;
      Text             : String;
      Sloc_Start       : Source_Location;
      Sloc_End         : Source_Location;
      File_Name        : GNATCOLL.VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Level            : Natural;
      Indent           : Natural)
   is
      Line_Body : constant Natural := Line_In_Body;
   begin
      --  In Text, each identifier may have a link,
      --  Each link is made by the subprogram Format_Link (see just below).
      --  But before this step, we must search for declaration: this is done
      --  in Format_All_Link (whose body contains the call to Format_Link.

      Format_All_Link
        (B, Kernel, Result,
         List_Ref_In_File,
         Text,
         Sloc_Start,
         Sloc_End,
         File_Name,
         Entity_Line,
         Line_Body,
         Source_File_List,
         Link_All,
         Is_Body,
         Process_Body,
         Level,
         Indent);
   end Format_Identifier;

   -----------------
   -- Format_Link --
   -----------------

   procedure Format_Link
     (B                : access Backend;
      Kernel           : access Kernel_Handle_Record'Class;
      Result           : in out Unbounded_String;
      Text             : String;
      Sloc_Start       : Source_Location;
      Sloc_End         : Source_Location;
      File_Name        : GNATCOLL.VFS.Virtual_File;
      Entity_Line      : Natural;
      Line_In_Body     : Natural;
      Source_File_List : Type_Source_File_Table.HTable;
      Link_All         : Boolean;
      Is_Body          : Boolean;
      Process_Body     : Boolean;
      Loc_End          : Natural;
      Loc_Start        : Natural;
      Entity_Info      : Entity_Information;
      Entity_Abstract  : in out Boolean)
   is
      pragma Unreferenced (Sloc_End);

      procedure Create_Regular_Link;
      --  will create a regular link to the entity, links to both spec
      --  and body files are possible.

      procedure Create_Special_Link_To_Body;
      --  Create a link to the reference of the entity in the body

      function Link_Should_Be_Set return Boolean;
      --  Check if a link to that entity should be set

      function Special_Link_Should_Be_Set return Boolean;
      --  Check if a special link to the body should be set
      --  (a special link, because it doesn't link to the declaration
      --  of the entity, but to a reference somewhere in the body)

      function Regular_Link_Should_Be_Set return Boolean;
      --  Check if a regular link to the body should be set
      --  (a regular link is a link to the entity's declaration)

      ---------------------------------
      -- Create_Special_Link_To_Body --
      ---------------------------------

      procedure Create_Special_Link_To_Body is
         Decl_File : constant Virtual_File := Get_Filename
            (Get_File (Get_Declaration_Of (Entity_Info)));
      begin
         if Sloc_Start.Line > Get_Last_Line (B.all) then
            Set_Name_Tags
              (B, Kernel, Result,
               Text (Get_Last_Index (B.all) .. Loc_Start - 1),
               Entity_Line);
         else
            Append (Result, Text (Get_Last_Index (B.all) .. Loc_Start - 1));
         end if;

         Append
           (Result,
            Unbounded_String'
              (Parse
                 (Get_Template_File_Name (B, Kernel, Link_Kind),
                  (Assoc ("REF_FILE",
                          Get_Text_File_Name
                            (B, Kernel,
                             Other_File_Base_Name
                               (Get_Project_From_File
                                  (Project_Registry
                                     (Get_Registry (Kernel).all), Decl_File),
                                Decl_File))),
                   Assoc ("DECL_LINE", Image (Line_In_Body)),
                   Assoc ("NAME", Text (Loc_Start .. Loc_End))),
                  Cache)));
         Set_Last_Index (B.all, Loc_End + 1);
      end Create_Special_Link_To_Body;

      -------------------------
      -- Create_Regular_Link --
      -------------------------

      procedure Create_Regular_Link is
         Line_To_Use : Natural;
      begin
         if Sloc_Start.Line > Get_Last_Line (B.all) then
            Set_Name_Tags
              (B, Kernel, Result,
               Text (Get_Last_Index (B.all) .. Loc_Start - 1),
               Entity_Line);
         else
            Append (Result, Text (Get_Last_Index (B.all) .. Loc_Start - 1));
         end if;

         Line_To_Use := Get_Line (Get_Declaration_Of (Entity_Info));

         Append
           (Result,
            Unbounded_String'
              (Parse
                 (Get_Template_File_Name (B, Kernel, Link_Kind),
                  (Assoc
                     ("REF_FILE",
                      Get_Text_File_Name
                        (B, Kernel, Full_Name
                           (Get_Filename
                              (Get_File
                                 (Get_Declaration_Of (Entity_Info)))).all)),
                   Assoc ("DECL_LINE", Image (Line_To_Use)),
                   Assoc ("NAME", Text (Loc_Start .. Loc_End))),
                  Cache)));
         Set_Last_Index (B.all, Loc_End + 1);
      end Create_Regular_Link;

      ------------------------
      -- Link_Should_Be_Set --
      ------------------------

      function Link_Should_Be_Set return Boolean is
      begin
         --  If no links should be set to entities declared in not
         --  processed source files => filter them out

         return
           (not Entity_Abstract
            and then
              (Link_All
               or else Source_File_In_List
                 (Source_File_List,
                  Get_File (Get_Declaration_Of (Entity_Info))))
         --  create no links if it is the declaration line itself;
         --  only if it's a subprogram or entry in a spec sometimes
         --  a link can be created to it body, so don't filter these ones.
            and then
              (Get_Filename (Get_File (Get_Declaration_Of (Entity_Info))) /=
                 File_Name
              or else Get_Line (Get_Declaration_Of (Entity_Info)) /=
                Sloc_Start.Line + Entity_Line - 1
              or else Special_Link_Should_Be_Set));
      end Link_Should_Be_Set;

      --------------------------------
      -- Special_Link_Should_Be_Set --
      --------------------------------

      function Special_Link_Should_Be_Set return Boolean is
         Entity_Kind : constant E_Kinds := Get_Kind (Entity_Info).Kind;
      begin
         return not Is_Body
           and then
             (Entity_Kind = Entry_Or_Entry_Family
              or else Entity_Kind = Procedure_Kind
              or else Entity_Kind = Function_Or_Operator)
           and then (Process_Body or else Link_All);
      end Special_Link_Should_Be_Set;

      --------------------------------
      -- Regular_Link_Should_Be_Set --
      --------------------------------

      function Regular_Link_Should_Be_Set return Boolean is
         Entity_Kind : constant E_Kinds := Get_Kind (Entity_Info).Kind;
      begin
         --  No subprograms/tasks are processed here, if working on a spec
         --  file
         return Is_Body
           or else not
             (Entity_Kind = Entry_Or_Entry_Family
              or else Entity_Kind = Procedure_Kind
              or else Entity_Kind = Function_Or_Operator);
      end Regular_Link_Should_Be_Set;

   begin  --  Format_Link
      if Link_Should_Be_Set then
         if Special_Link_Should_Be_Set then
            Create_Special_Link_To_Body;
         elsif Regular_Link_Should_Be_Set then
            Create_Regular_Link;
         end if;
      end if;
   end Format_Link;

   ------------
   -- Finish --
   ------------

   procedure Finish
     (B           : access Backend;
      Kernel      : access Kernel_Handle_Record'Class;
      Result      : in out Unbounded_String;
      Text        : String;
      Entity_Line : Natural) is
   begin
      if Get_Last_Index (B.all) <= Text'Last then
         Set_Name_Tags
           (B, Kernel, Result,
            Text (Get_Last_Index (B.all) .. Text'Last),
            Entity_Line);
      end if;
   end Finish;

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory
     (B      : access Backend;
      Kernel : access Kernel_Handle_Record'Class) return String is
   begin
      return File_Utils.Name_As_Directory
        (Object_Path
           (Get_Root_Project (Get_Registry (Kernel).all), False))
        & B.Output_Description.Extension.all & '/';
   end Get_Doc_Directory;

end Docgen.Backend.Text;
