-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Calendar.Formatting;   use Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;   use Ada.Calendar.Time_Zones;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

with Basic_Types;               use Basic_Types;

with DB_API;                    use DB_API;
with Entities;                  use Entities;
with Entities.Queries;          use Entities.Queries;
with File_Utils;                use File_Utils;
with Language;                  use Language;
with Language_Handlers;         use Language_Handlers;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with SN;                        use SN;
with SN.Browse;                 use SN.Browse;
with SN.DB_Structures;          use SN.DB_Structures;
with SN.Find_Fns;               use SN.Find_Fns;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body CPP_Parser is

   Me : constant Debug_Handle := Create ("CPP");

   Xref_Suffix : constant String := ".xref";
   DBIMP       : constant String := "dbimp";    --  SN database engine
   CBrowser    : constant String := "cbrowser"; --  SN C and C++ parser

   Base_Time : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of
     (1970, 1, 1, 0, 0, 0);
   --  All timestamps in the SN db are the number of seconds since that time
   --  (The Unix Epoch)

   Unresolved : constant E_Kind :=
     (Entities.Unresolved_Entity,
      Is_Type     => False,
      Is_Generic  => False,
      Is_Abstract => False);
   Modular_Integer_Entity : constant E_Kind :=
     (Modular_Integer,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Signed_Integer_Entity : constant E_Kind :=
     (Signed_Integer,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Floating_Point_Entity : constant E_Kind :=
     (Floating_Point,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   String_Entity : constant E_Kind :=
     (String_Kind,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Class_Entity : constant E_Kind :=
     (Class,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Macro_Entity : constant E_Kind :=
     (Macro,
      Is_Type     => False,
      Is_Generic  => False,
      Is_Abstract => False);
   Access_Entity_Type : constant E_Kind :=
     (Access_Kind,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Reference_Entity_Type : constant E_Kind :=
     (Reference,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Array_Entity_Type : constant E_Kind :=
     (Array_Kind,
      Is_Type     => True,
      Is_Generic  => False,
      Is_Abstract => False);
   Function_Entity : constant E_Kind :=
     (Function_Or_Operator,
      Is_Type     => False,
      Is_Generic  => False,
      Is_Abstract => False);
   Procedure_Entity : constant E_Kind :=
     (Procedure_Kind,
      Is_Type     => False,
      Is_Generic  => False,
      Is_Abstract => False);
   Void_Entity : constant E_Kind :=
     (Private_Type,
      Is_Type     => False,
      Is_Generic  => False,
      Is_Abstract => False);

   subtype Symbols_With_Tables is Symbol_Type
   range CL .. Symbol_Type'Pred (Undef);

   type SN_Table_Array is array (Table_Type) of DB_File;
   type Boolean_Table_Array is array (Symbols_With_Tables) of Boolean;
   type Segment_Array is array (Natural range <>) of Segment;

   All_Types_Table : constant Boolean_Table_Array :=
     (T | CL | UN | E | TA => True,
      MD | MI              => False,
      others               => False);
   --  All tables that contain type declarations

   type CPP_Handler_Record is new LI_Handler_Record with record
      Db            : Entities_Database;
      Registry      : Project_Registry;
      SN_Table      : SN_Table_Array;
      DBIMP_Path    : GNAT.Strings.String_Access;
      CBrowser_Path : GNAT.Strings.String_Access;
   end record;
   type CPP_Handler is access all CPP_Handler_Record'Class;

   function Get_Name (LI : access CPP_Handler_Record) return String;
   function Get_Source_Info
     (Handler               : access CPP_Handler_Record;
      Source_Filename       : VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null) return Source_File;
   function Case_Insensitive_Identifiers
     (Handler         : access CPP_Handler_Record) return Boolean;
   function Parse_All_LI_Information
     (Handler   : access CPP_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return Integer;
   function Generate_LI_For_Project
     (Handler      : access CPP_Handler_Record;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project      : Projects.Project_Type;
      Errors       : Projects.Error_Report;
      Recursive    : Boolean := False) return LI_Handler_Iterator'Class;
   procedure Parse_File_Constructs
     (Handler   : access CPP_Handler_Record;
      Languages : access Abstract_Language_Handler_Record'Class;
      File_Name : VFS.Virtual_File;
      Result    : out Language.Construct_List);
   --  See doc for inherited subprograms

   type Iterator_State_Type is
     (Analyze_Files, --  Parsing the files with cbrowser
      Process_Xrefs, --  Processing xrefs for all files with dbimp
      Skip_Project,  --  No C/C++ source files in that project
      Done);         --  Updating done

   type CPP_Handler_Iterator is new LI_Handler_Iterator with record
      State           : Iterator_State_Type := Done;
      Handler         : CPP_Handler;
      Project         : Project_Type;
      Process_Running : Boolean := False;
      PD              : GNAT.Expect.TTY.TTY_Process_Descriptor;
      Prj_Iterator    : Projects.Imported_Project_Iterator;
      List_Filename   : GNAT.Strings.String_Access;
      Current_Files   : VFS.File_Array_Access;
      Current_File    : Natural;
      Lang_Handler    : Language_Handler;

      Do_Dbimp        : Boolean := True;
      --  Whether we should call Dbimp in the end

      Tmp_Filename    : GNAT.OS_Lib.Temp_File_Name;
      --  The name of a temporary file created by dbimp, which needs to be
      --  freed on completion
   end record;
   procedure Continue
     (Iterator : in out CPP_Handler_Iterator;
      Errors   : Projects.Error_Report;
      Finished : out Boolean);
   procedure Destroy (Iterator : in out CPP_Handler_Iterator);
   --  See doc for inherited subprograms

   procedure Browse_Project
     (Project     : Project_Type;
      Iterator    : in out CPP_Handler_Iterator'Class;
      Errors      : Projects.Error_Report;
      Single_File : VFS.Virtual_File := VFS.No_File);
   --  Runs cbrowser for all source files of Project.
   --  If Single_File is specified, then only this information for this file
   --  will be generated.

   function Generate_LI_For_Source
     (Handler      : access CPP_Handler_Record'Class;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project      : Project_Type;
      Errors       : Projects.Error_Report;
      File         : VFS.Virtual_File) return LI_Handler_Iterator'Class;
   --  Generate the cbrowser information for a single file. This means that
   --  we will mostly know about entity declaration and bodies, but not about
   --  references. On the other hand, it is extremely fast, and suitable when
   --  displaying the list of entities declared in a file

   procedure Parse_File
     (Handler : access CPP_Handler_Record'Class;
      Project : Project_Type;
      Source  : Source_File);
   --  Do the actual parsing of the database to get information for Source

   procedure Open_DB_Files
     (Handler : access CPP_Handler_Record'Class);
   --  Opens all existing DB files, located in specified directories list.
   --  Returns array of DB_Files (indexed by Symbol_Type).
   --  This needs to be called only once for the whole project tree

   procedure Close_DB_Files
     (Handler : access CPP_Handler_Record'Class);
   --  Reclaim the memory used by the various databases.

   function Get_DB_Dir (Project : Projects.Project_Type) return String;
   pragma Inline (Get_DB_Dir);
   --  Return the directory that contains the source navigator files
   --  for specified project

   function Get_DB_Dirs
     (Project : Project_Type) return GNAT.Strings.String_List_Access;
   --  Return the list of database directories that should be inspected
   --  when looking for cross-references (basically, the db directories for
   --  all imported projects).
   --  Result must be freed by user.

   function Create_Directory_If_Not_Exist (Dir : String) return Boolean;
   --  Create the directory Dir if it doesn't exist yet.
   --  Return True if the directory existed or could be created successfully.

   function Table_Extension (Table : Table_Type) return String;
   --  Given a table type, return the associated file extension, or "" if
   --  there is none.

   function Entity_From_FIL
     (Sym    : FIL_Table;
      Source : Source_File) return Entity_Information;
   --  Create an entity from the information in Sym

   procedure Parse_FIL_Table
     (Handler : access CPP_Handler_Record'Class;
      Project : Project_Type;
      Source  : Source_File);
   --  Parse the FIL table for information about Source

   procedure Parse_GV_Table
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      Sym     : FIL_Table);
   procedure Parse_T_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_CL_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_MD_Table
     (Handler : access CPP_Handler_Record'Class;
      Sym     : FIL_Table;
      Source  : Source_File);
   procedure Parse_MI_Table
     (Handler : access CPP_Handler_Record'Class;
      Sym     : FIL_Table;
      Source  : Source_File);
   procedure Parse_CON_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_FU_Table
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      Sym     : FIL_Table);
   function Parse_FD_Table
     (Handler : access CPP_Handler_Record'Class;
      Sym     : FIL_Table) return Entity_Information;
   procedure Parse_TA_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_E_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_EC_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_TO_Table
     (Handler       : access CPP_Handler_Record'Class;
      Sym_Name      : String;
      Sym_Class     : String;
      Sym_Arg_Types : String);
   procedure Parse_LV_Table
     (Handler          : access CPP_Handler_Record'Class;
      Entity           : Entity_Information;
      Entity_File_Name : String;
      Entity_Class     : String;
      Entity_Arg_Types : String;
      Params           : String;
      Parsed_Params    : Segment_Array;
      Source           : Source_File);
   procedure Parse_IV_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table);
   procedure Parse_IU_Table
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      Sym     : FIL_Table);
   --  Search for more information about Sym in the various tables.

   procedure Parse_FU_Table_From_FD
     (Handler    : access CPP_Handler_Record'Class;
      Entity     : Entity_Information;
      Args       : String;
      Body_Known : Boolean;
      Body_Info  : FU_Table);
   --  The forward declaration of an entity has been parsed, we now search
   --  for its body in the FU table.

   procedure Parse_Method_Table_Internal
     (Handler      : access CPP_Handler_Record'Class;
      Entity       : out Entity_Information;
      Entity_Name  : String;
      Entity_Start : Point;
      Return_Type  : String;
      Entity_File  : String;
      Entity_Class : String;
      Arg_Types    : String;
      Arg_Names    : String;
      Source       : Source_File;
      Class        : out Entity_Information);
   --  Insert a method declaration and profile in the database.
   --  Return the Class entity

   procedure Process_Local_Variables_And_Parameters
     (Handler          : access CPP_Handler_Record'Class;
      Entity           : Entity_Information;
      Entity_File_Name : String;
      Entity_Class     : String;
      Entity_Arg_Types : String;
      Params           : String;
      Source           : Source_File);
   --  Analyze all the local variables declared in Sym, and check whether they
   --  are parameters to the subprogram Sym.

   procedure Cleanup_Entity_Name (Name : String; Name_Start : out Natural);
   --  Compute the position of the first character in Name that indicates its
   --  actual name, thus eliminating prefixes like "const" and "volatile".

   procedure Find
     (Handler : access CPP_Handler_Record'Class;
      Class   : String;
      Name    : String;
      Args    : String;
      M       : out MI_Table;
      Success : out Boolean);
   procedure Find
     (Handler : access CPP_Handler_Record'Class;
      Class   : String;
      Name    : String;
      Args    : String;
      D       : out MD_Table;
      Success : out Boolean);
   pragma Inline (Find);
   --  Find the declaration or body of a method in the tables. The argument
   --  types must match

   function Get_If_Predefined
     (Handler    : access CPP_Handler_Record'Class;
      Clean_Name : String) return Entity_Information;
   --  Return Clean_Name if it is a predefined entity, null otherwise.

   function Lookup_Entity_In_Tables
     (Handler                        : access CPP_Handler_Record'Class;
      Name                           : String;
      Current_Source                 : Source_File;
      Tables                         : Boolean_Table_Array := All_Types_Table;
      Check_Predefined               : Boolean := True;
      Check_Template_Arguments       : Boolean := True;
      Check_Class_Template_Arguments : Boolean := True;
      Class_Or_Function              : String := Invalid_String;
      Args                           : String := Invalid_String)
      return Entity_Information;
   --  Search the declaration of the entity in some of the SN tables
   --  (as indicated by Tables), or in the list of predefined entities.
   --  The entity is not searched in the already parsed memory.

   function Lookup_Non_Overloaded_Entity
     (In_File : Source_File;
      Name    : String) return Entity_Information;
   pragma Unreferenced (Lookup_Non_Overloaded_Entity);
   --  In there is only one possible declaration for Name so far, return it.
   --  Otherwise, this is an overloaded entity, and null is returned.

   procedure Set_Parent
     (Handler          : access CPP_Handler_Record'Class;
      Entity           : Entity_Information;
      Parent_Name      : String;
      Parent_Reference : Point := Invalid_Point;
      Entity_Is_A_Type : Boolean := False;
      Tables           : Boolean_Table_Array := All_Types_Table;
      Class_Or_Function : String := Invalid_String);
   --  Set the parent information for Entity.
   --  Parent_Reference is also registered as a reference to the parent

   procedure Set_Subprogram_Return_Type
     (Handler     : access CPP_Handler_Record'Class;
      Entity      : Entity_Information;
      Return_Type : String);
   --  Set the return type for a subprogram or method entity

   function Find_Forward_Declaration_And_Body
     (Handler    : access CPP_Handler_Record'Class;
      Name       : String;
      Filename   : String;
      Args       : String;
      Body_Known : Boolean;
      Body_Info  : FU_Table := No_FU_Table) return Entity_Information;
   --  Return the first possible forward declaration for the subprogram Name.
   --  Returns null if there is no matching forward declaration.
   --  This also looks for possible information about the location of the body
   --  for this entity. To prevent a lookup in the FU table, the information
   --  can be passed in parameter in Body_Known and Body_Info

   function Match_With_Joker (Str1, Str2_With_Joker : String) return Boolean;
   --  True if Str1 = Str2_With_Joker (but the latter can be '#' to force a
   --  match

   procedure Set_Kind_From_Table_If_Not_Set
     (Entity : Entity_Information; Table : Symbol_Type);
   --  Guess the kind of the entity from the table in which it is declared

   function Get_Static_Field
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      G       : GV_Table) return Entity_Information;
   --  Return the declaration for the static class field defined in G.
   --  G must have a class name defined.

   procedure Skip_Brackets
     (Name    : String;
      Index   : in out Natural;
      Opening : Character;
      Closing : Character);
   --  Skip to the matching opening or closing bracket. If Index currently
   --  points to an opening bracket, moves forward until it finds a matching
   --  closing one.
   --  If Index currently points to a closing bracket, moves backward.
   --  Nested brackets are properly handled
   --
   --  ??? Move to string_utils.ads

   function Database_Timestamp (Project : Project_Type) return Time;
   --  Return the timestamp of the database for Project.

   function Time_Stamp_From_DB
     (Project   : Project_Type;
      File_Name : VFS.Virtual_File) return Time;
   --  Return the timestamp of File_Name when the database was last created.
   --  This is a GMT time

   ---------------------
   -- Table_Extension --
   ---------------------

   function Table_Extension (Table : Table_Type) return String is
   begin
      case Table is
         when FIL    => return ".fil";
         when F      => return ".f";
         when FD     => return ".fd";
         when FU     => return ".fu";
         when T      => return ".t";
         when CL     => return ".cl";
         when GV     => return ".gv";
         when E      => return ".e";
         when EC     => return ".ec";
         when TO     => return ".to";
         when IV     => return ".iv";
         when MI     => return ".mi";
         when MD     => return ".md";
         when SN_IN  => return ".in";
         when UN     => return ".un";
         when MA     => return ".ma";
         when CON    => return ".con";
         when LV     => return ".lv";
         when TA     => return ".ta";
         when BY     => return ".by";
         when SN_REM => return ".rem";
         when COM    => return ".com";
         when COV    => return ".cov";
         when FR     => return ".fr";
         when IU     => return ".iu";
         when SU     => return ".su";
         when UD     => return ".ud";
      end case;
   end Table_Extension;

   -----------------------------------
   -- Create_Directory_If_Not_Exist --
   -----------------------------------

   function Create_Directory_If_Not_Exist (Dir : String) return Boolean is
   begin
      if Dir /= "" and then not GNAT.OS_Lib.Is_Directory (Dir) then
         --  It might happen that the Directory exists but is not readable.
         --  A bug in the GNAT runtime has Is_Directory return False in this
         --  case, and Make_Dir will of course fail.
         Make_Dir (Dir);
      end if;
      return True;

   exception
      when GNAT.Directory_Operations.Directory_Error =>
         return False;
   end Create_Directory_If_Not_Exist;

   ----------------
   -- Get_DB_Dir --
   ----------------

   function Get_DB_Dir (Project : Projects.Project_Type) return String is
      Obj_Dir : constant String := Object_Path (Project, False);
   begin
      if Obj_Dir = "" then
         return "";
      else
         return Name_As_Directory (Obj_Dir)
           & Name_As_Directory (SN.Browse.DB_Dir_Name);
      end if;
   end Get_DB_Dir;

   -----------------
   -- Get_DB_Dirs --
   -----------------

   function Get_DB_Dirs
     (Project : Project_Type) return GNAT.Strings.String_List_Access
   is
      Obj_Dir : constant String := Object_Path (Project, True);
      Db_Dir  : constant String := Name_As_Directory (DB_Dir_Name);
      Iter    : Path_Iterator := Start (Obj_Dir);
      Length  : Natural := 0;
      List    : GNAT.Strings.String_List_Access;

   begin
      --  ??? Should we avoid duplicates in the list of object directories ?

      while not At_End (Obj_Dir, Iter) loop
         Length := Length + 1;
         Iter := Next (Obj_Dir, Iter);
      end loop;

      List := new GNAT.Strings.String_List (1 .. Length);
      Iter := Start (Obj_Dir);

      for F in List'Range loop
         List (F) := new String'
           (Name_As_Directory (Current (Obj_Dir, Iter)) & Db_Dir);
         Iter := Next (Obj_Dir, Iter);
      end loop;

      return List;
   end Get_DB_Dirs;

   -------------------
   -- Open_DB_Files --
   -------------------

   procedure Open_DB_Files
     (Handler      : access CPP_Handler_Record'Class)
   is
      Obj_Dir : constant String := Object_Path
        (Get_Root_Project (Handler.Registry), True);
      Db_Dir  : constant String := Name_As_Directory (SN.Browse.DB_Dir_Name);
      Iter    : Path_Iterator := Start (Obj_Dir);
      Length  : Natural := 0;
      Success : Boolean;
   begin
      Trace (Me, "Opening the database for C and C++");

      --  ??? Should we avoid duplicates in the list of object directories ?

      while not At_End (Obj_Dir, Iter) loop
         Length := Length + 1;
         Iter := Next (Obj_Dir, Iter);
      end loop;

      declare
         Files : chars_ptr_array (1 .. Interfaces.C.size_t (Length));
      begin
         for Table in Table_Type loop
            Iter := Start (Obj_Dir);

            for D in Files'Range loop
               Files (D) := New_String
                 (Name_As_Directory (Current (Obj_Dir, Iter)) & Db_Dir
                  & SN.Browse.DB_File_Name & Table_Extension (Table));
               Iter := Next (Obj_Dir, Iter);
            end loop;

            DB_API.Open (Handler.SN_Table (Table), Files, Success);

            for D in Files'Range loop
               Free (Files (D));
            end loop;
         end loop;
      end;
   end Open_DB_Files;

   --------------------
   -- Close_DB_Files --
   --------------------

   procedure Close_DB_Files
     (Handler : access CPP_Handler_Record'Class)
   is
      Success : Boolean;
   begin
      for Table in Table_Type loop
         Close (Handler.SN_Table (Table), Success);
      end loop;
   end Close_DB_Files;

   -------------------------
   -- Cleanup_Entity_Name --
   -------------------------

   procedure Cleanup_Entity_Name
     (Name : String; Name_Start : out Natural)
   is
      Volatile_Str : constant String := "volatile ";
      Const_Str    : constant String := "const ";
      Virtual_Str  : constant String := "virtual ";
   begin
      if Looking_At (Name, Name'First, Substring => Volatile_Str) then
         Name_Start := Name'First + Volatile_Str'Length;
      elsif Looking_At (Name, Name'First, Substring => Const_Str) then
         Name_Start := Name'First + Const_Str'Length;
      elsif Looking_At (Name, Name'First, Substring => Virtual_Str) then
         Name_Start := Name'First + Virtual_Str'Length;
      else
         Name_Start := Name'First;
      end if;
   end Cleanup_Entity_Name;

   -----------------------
   -- Get_If_Predefined --
   -----------------------

   function Get_If_Predefined
     (Handler    : access CPP_Handler_Record'Class;
      Clean_Name : String) return Entity_Information
   is
      Signed_Str   : constant String := "signed ";
      Unsigned_Str : constant String := "unsigned ";

      Has_Unsigned : Boolean := False;
      Name_Start   : Natural := Clean_Name'First;
      Entity       : Entity_Information;
      Kind         : E_Kind;
   begin
      --  Do we already have a predefined entity with this name ?
      Entity := Get_Or_Create
        (Name         => Clean_Name,
         File         => Get_Predefined_File (Handler.Db, Handler),
         Line         => Predefined_Line,
         Column       => Predefined_Column,
         Allow_Create => False);

      if Entity = null then
         if Looking_At (Clean_Name, Name_Start, Signed_Str) then
            Name_Start := Name_Start + Signed_Str'Length;

         elsif Looking_At (Clean_Name, Name_Start, Unsigned_Str) then
            Name_Start := Name_Start + Unsigned_Str'Length;
            Has_Unsigned := True;
         end if;

         if Clean_Name (Name_Start .. Clean_Name'Last) = "char"
           or else Clean_Name (Name_Start .. Clean_Name'Last) = "int"
           or else Clean_Name (Name_Start .. Clean_Name'Last) = "long"
           or else Clean_Name (Name_Start .. Clean_Name'Last) = "long long"
           or else Clean_Name (Name_Start .. Clean_Name'Last) = "short"
         then
            if Has_Unsigned then
               Kind := Modular_Integer_Entity;
            else
               Kind := Signed_Integer_Entity;
            end if;

         elsif Clean_Name (Name_Start .. Clean_Name'Last) = "uint"
           or else Clean_Name (Name_Start .. Clean_Name'Last) = "uchar"
         then
            Kind := Modular_Integer_Entity;

         elsif Clean_Name (Name_Start .. Clean_Name'Last) = "float"
           or else Clean_Name (Name_Start .. Clean_Name'Last) = "double"
         then
            Kind := Floating_Point_Entity;

         elsif Clean_Name (Name_Start .. Clean_Name'Last) = "char *" then
            Kind := String_Entity;

         elsif Clean_Name (Name_Start .. Clean_Name'Last) = "void" then
            Kind := Void_Entity;

         elsif Clean_Name (Name_Start .. Clean_Name'Last) = "class" then
            Kind := Class_Entity;

         else
            return null;
         end if;

         Kind.Is_Type := True;

         Entity := Get_Or_Create
           (Name         => Clean_Name,
            File         => Get_Predefined_File (Handler.Db, Handler),
            Line         => Predefined_Line,
            Column       => Predefined_Column,
            Allow_Create => True);
         Set_Kind (Entity, Kind);
         Set_Attributes (Entity, (Global => True, others => False));
      end if;

      return Entity;
   end Get_If_Predefined;

   ------------------------------------
   -- Set_Kind_From_Table_If_Not_Set --
   ------------------------------------

   procedure Set_Kind_From_Table_If_Not_Set
     (Entity : Entity_Information;
      Table  : Symbol_Type)
   is
      Kind : E_Kind;
   begin
      if Get_Kind (Entity).Kind = Unresolved_Entity then
         Kind.Is_Type     := True;
         Kind.Is_Generic  := False;
         Kind.Is_Abstract := False;

         case Table is
            when CL | FR => Kind.Kind := Class;
            when IV  =>
               Kind.Kind    := Class;
               Kind.Is_Type := False;
            when E   => Kind.Kind := Enumeration_Kind;
            when EC  => Kind.Kind := Enumeration_Literal;
            when FD | MI | FU | MD | SU => Kind.Kind := Function_Or_Operator;
            when MA  => Kind.Kind := Macro;
            when UN  => Kind.Kind := Union;
            when Undef | CON | GV | T | IU | COM | COV
               | SN_IN | LV | TA | UD =>
               Kind.Kind := Unresolved_Entity;
         end case;
         Set_Kind (Entity, Kind);
      end if;
   end Set_Kind_From_Table_If_Not_Set;

   -------------------
   -- Skip_Brackets --
   -------------------

   procedure Skip_Brackets
     (Name    : String;
      Index   : in out Natural;
      Opening : Character;
      Closing : Character)
   is
      Num_Brackets : Natural := 1;
   begin
      if Name (Index) = Opening then
         while Index < Name'Last and then Num_Brackets /= 0 loop
            Index := Index + 1;
            if Name (Index) = Opening then
               Num_Brackets := Num_Brackets + 1;
            elsif Name (Index) = Closing then
               Num_Brackets := Num_Brackets - 1;
            end if;
         end loop;

      elsif Name (Index) = Closing then
         while Index > Name'First and then Num_Brackets /= 0 loop
            Index := Index - 1;
            if Name (Index) = Opening then
               Num_Brackets := Num_Brackets - 1;
            elsif Name (Index) = Closing then
               Num_Brackets := Num_Brackets + 1;
            end if;
         end loop;
      end if;
   end Skip_Brackets;

   -----------------------------
   -- Lookup_Entity_In_Tables --
   -----------------------------

   function Lookup_Entity_In_Tables
     (Handler                        : access CPP_Handler_Record'Class;
      Name                           : String;
      Current_Source                 : Source_File;
      Tables                         : Boolean_Table_Array := All_Types_Table;
      Check_Predefined               : Boolean := True;
      Check_Template_Arguments       : Boolean := True;
      Check_Class_Template_Arguments : Boolean := True;
      Class_Or_Function              : String := Invalid_String;
      Args                           : String := Invalid_String)
      return Entity_Information
   is
      Source               : Source_File;
      Entity, Real_Entity  : Entity_Information;
      Key                  : Entity_Key;
      Key2                 : Entity_Class_Key;
      Key3                 : Entity_Function_Key;
      Success              : Boolean;
      Last                 : Natural := Name'Last;
      P                    : Pair;
      F                    : FU_Table;

      procedure Handle_FU_Table (Table_Type : Symbol_Type);
      procedure Handle_CL_Table (Table_Type : Symbol_Type);
      procedure Handle_MI_Table (Table_Type : Symbol_Type);
      procedure Handle_LV_Table (Table_Type : Symbol_Type);
      pragma Inline (Handle_FU_Table, Handle_CL_Table, Handle_MI_Table,
                     Handle_LV_Table);
      --  Handle various kinds of tables when searching for an entity

      ---------------------
      -- Handle_FU_Table --
      ---------------------

      procedure Handle_FU_Table (Table_Type : Symbol_Type) is
      begin
         Set_Cursor_At
           (Handler.SN_Table (Table_Type), Name => Name (Name'First .. Last));
         loop
            Get_Pair (Handler.SN_Table (Table_Type),
                      Next_By_Key, Result => P);
            exit when P = No_Pair;

            --  Handling of overloaded entities: check the arguments
            --  of the entities, especially in the FU table
            Parse_Pair (P, F);
            exit when Args = Invalid_String
              or else F.Data (F.Arg_Types.First .. F.Arg_Types.Last) = Args;
         end loop;

         Release_Cursor (Handler.SN_Table (Table_Type));

         if P /= No_Pair then
            Source := Get_Or_Create
              (Handler.Db, F.Key (F.File_Name.First .. F.File_Name.Last),
               Handler);
            Entity := Get_Or_Create
              (Name   => Name (Name'First .. Last),
               File   => Source,
               Line   => F.Start_Position.Line,
               Column => F.Start_Position.Column);
            Set_Kind_From_Table_If_Not_Set (Entity, Table_Type);
            Set_Attributes
              (Entity, (Global => (F.Attributes and SN_STATIC) = 0,
                      others => False));
         end if;
      end Handle_FU_Table;

      ---------------------
      -- Handle_CL_Table --
      ---------------------

      procedure Handle_CL_Table (Table_Type : Symbol_Type) is
      begin
         Find_Key (Handler.SN_Table (Table_Type),
                   Name    => Name (Name'First .. Last),
                   Key     => Key,
                   Success => Success);

         if Success then
            Source := Get_Or_Create
              (Handler.Db,
               Key.Key (Key.File_Name.First .. Key.File_Name.Last),
               Handler);
            Entity := Get_Or_Create
              (Name   => Name (Name'First .. Last),
               File   => Source,
               Line   => Key.Start_Position.Line,
               Column => Key.Start_Position.Column);
            Set_Kind_From_Table_If_Not_Set (Entity, Table_Type);
         end if;
      end Handle_CL_Table;

      ---------------------
      -- Handle_MI_Table --
      ---------------------

      procedure Handle_MI_Table (Table_Type : Symbol_Type) is
      begin
         if Class_Or_Function /= ""
           and then Class_Or_Function /= "#"
         then
            Find_Key (Handler.SN_Table (Table_Type),
                      Class   => Class_Or_Function,
                      Name    => Name (Name'First .. Last),
                      Key     => Key2,
                      Success => Success);

            if Success then
               Source := Get_Or_Create
                 (Handler.Db,
                  Key2.Key (Key2.File_Name.First .. Key2.File_Name.Last),
                  Handler);
               Entity := Get_Or_Create
                 (Name   => Name (Name'First .. Last),
                  File   => Source,
                  Line   => Key2.Start_Position.Line,
                  Column => Key2.Start_Position.Column);
               Set_Kind_From_Table_If_Not_Set (Entity, Table_Type);
            end if;
         end if;
      end Handle_MI_Table;

      ---------------------
      -- Handle_LV_Table --
      ---------------------

      procedure Handle_LV_Table (Table_Type : Symbol_Type) is
      begin
         Find_Key (Handler.SN_Table (Table_Type),
                   Function_Name => Class_Or_Function,
                   Name          => Name (Name'First .. Last),
                   Key           => Key3,
                   Success       => Success);

         if Success then
            Source := Get_Or_Create
              (Handler.Db,
               Key3.Key (Key3.File_Name.First .. Key3.File_Name.Last),
               Handler);
            Entity := Get_Or_Create
              (Name   => Name (Name'First .. Last),
               File   => Source,
               Line   => Key3.Start_Position.Line,
               Column => Key3.Start_Position.Column);
            Set_Kind_From_Table_If_Not_Set (Entity, Table_Type);
         end if;
      end Handle_LV_Table;

   begin
      --  Handling of pointers, arrays,... => create anonymous types

      while (Name (Last) = '*' and then Name (Name'First .. Last) /= "char *")
        or else Name (Last) = '&'
        or else Name (Last) = '['
        or else Name (Last) = ']'
        or else Name (Last) = ' '
        or else Name (Last) = '>'
      loop
         --  Special case for "operator>>"
         if Name (Last) = '>' then
            if Name (Last - 1) /= '>' then
               Skip_Brackets (Name, Last, '<', '>');
            else
               Last := Last - 1;
            end if;
         end if;

         Last := Last - 1;
      end loop;

      if Check_Predefined then
         Entity := Get_If_Predefined (Handler, Name (Name'First .. Last));
      end if;

      if Entity = null then
         for Table_Type in Tables'Range loop
            if Tables (Table_Type)
              and then Is_Open (Handler.SN_Table (Table_Type))
            then
               case Table_Type is
                  when FU | FD =>
                     Handle_FU_Table (Table_Type);
                     exit when Entity /= null;

                  when CL | CON | E | EC | FR | GV | MA | T | UN =>
                     Handle_CL_Table (Table_Type);
                     exit when Entity /= null;

                  when IV | MI | MD | TA =>
                     Handle_MI_Table (Table_Type);
                     exit when Entity /= null;

                  when LV =>
                     Handle_LV_Table (Table_Type);
                     exit when Entity /= null;

                  when IU | COM | COV | SN_IN | SU | UD =>
                     if Active (Me) then
                        Trace (Me, "Do not know how to lookup entities in "
                               & Table_Type'Img & " table");
                     end if;
               end case;
            end if;
         end loop;
      end if;

      --  src_info-type_utils.adb:292
      if Entity = null and then Check_Template_Arguments then
         null;
      end if;

      --  src_info-type_utils.adb:315
      if Entity = null and then Check_Class_Template_Arguments then
         null;
      end if;

      --  Do we have a pointer ?

      if Entity /= null then
         loop
            Last := Last + 1;
            exit when Last > Name'Last;

            if Name (Last) = '<' then
               Skip_Brackets (Name, Last, '<', '>');
               Last := Last + 1;
               exit when Last > Name'Last;
            elsif Name (Last) = '[' then
               Last := Last + 1;
            end if;

            if Name (Last) /= ' ' then
               if Is_Predefined_Entity (Entity) then
                  Real_Entity := Get_Or_Create
                    (Name   => Name (Name'First .. Last),
                     File   => Get_Predefined_File (Handler.Db, Handler),
                     Line   => Predefined_Line,
                     Column => Predefined_Column);
               else
                  Real_Entity := Get_Or_Create
                    (Name   => Name (Name'First .. Last),
                     File   => Current_Source,
                     Line   => Predefined_Line,
                     Column => Predefined_Column);
               end if;

               if Name (Last) = '*' then
                  Set_Kind (Real_Entity, Access_Entity_Type);
                  Set_Pointed_Type (Real_Entity, Entity);

               elsif Name (Last) = '&' then
                  Set_Kind (Real_Entity, Reference_Entity_Type);
                  Set_Pointed_Type (Real_Entity, Entity);

               elsif Name (Last) = ']' then
                  Set_Kind (Real_Entity, Array_Entity_Type);
                  Set_Pointed_Type (Real_Entity, Entity);

                  Last := Last + 1;
               end if;

               Entity := Real_Entity;
            end if;
         end loop;
      end if;

      return Entity;
   end Lookup_Entity_In_Tables;

   ----------------------------------
   -- Lookup_Non_Overloaded_Entity --
   ----------------------------------

   function Lookup_Non_Overloaded_Entity
     (In_File : Source_File;
      Name    : String) return Entity_Information
   is
      Iter   : Entity_Iterator;
      Count  : Natural := 0;
      Entity : Entity_Information;
   begin
      Find_All_Entities_In_File (Iter, In_File, null, Name);
      while not At_End (Iter) loop
         if Get_Name (Get (Iter)).all = Name then
            Count := Count + 1;
            Entity := Get (Iter);
            exit when Count >= 2;
         end if;
         Next (Iter);
      end loop;

      if Count /= 1 then
         Entity := null;
      end if;

      Destroy (Iter);
      return Entity;
   end Lookup_Non_Overloaded_Entity;

   ----------------------
   -- Get_Static_Field --
   ----------------------

   function Get_Static_Field
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      G       : GV_Table) return Entity_Information
   is
      Entity, Class  : Entity_Information;
      Var     : IV_Table;
      Success : Boolean;
   begin
      Find (DB            => Handler.SN_Table (IV),
            Class         => G.Data (G.Class.First .. G.Class.Last),
            Variable_Name => G.Key  (G.Name.First .. G.Name.Last),
            Tab           => Var,
            Success       => Success);

      if Success then
         Entity := Get_Or_Create
           (Name   => G.Key (G.Name.First .. G.Name.Last),
            File   => Get_Or_Create
              (Handler.Db, Var.Key (Var.File_Name.First .. Var.File_Name.Last),
               Handler),
            Line   => Var.Start_Position.Line,
            Column => Var.Start_Position.Column);
         Add_Reference
           (Entity   => Entity,
            Location => (File   => Source,
                         Line   => G.Start_Position.Line,
                         Column => G.Start_Position.Column),
            Kind     => Body_Entity);

         --  Also register a new reference for the type (int in "int A::s;").
         --  We shouldn't set the parent through Set_Parent, since this is
         --  already done at the declaration of the field.

         Class := Get_Type_Of (Entity);
         if Class /= null then
            Add_Reference
              (Entity   => Class,
               Location =>
                 (File   => Source,
                  Line   => G.Type_Start_Position.Line,
                  Column => G.Type_Start_Position.Column),
               Kind     => Reference);
         end if;
      end if;

      return Entity;
   end Get_Static_Field;

   --------------------
   -- Parse_GV_Table --
   --------------------
   --  The following information is not used currently:
   --     End_Position, Attributes, Template_Parameters

   procedure Parse_GV_Table
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      Sym     : FIL_Table)
   is
      Entity  : Entity_Information;
      G       : GV_Table;
      Success : Boolean;
      Class   : Entity_Information;
      Length  : Natural;
   begin
      Find (DB       => Handler.SN_Table (GV),
            Name     => Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
            Position => Sym.Start_Position,
            Filename => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
            Tab      => G,
            Success  => Success);

      if Success then

         --  If we have a class name, this means that we are providing the
         --  actual declaration for a class static variable, as in
         --     "int A::my_static = 0"
         --  In this case, we need to reference the actual declaration

         Length := G.Class.Last - G.Class.First + 1;

         if Length > 0 then
            Entity := Get_Static_Field (Handler, Source, G);
         end if;

         if Entity = null then
            Entity := Entity_From_FIL (Sym, Source);

            Set_Parent
              (Handler          => Handler,
               Entity           => Entity,
               Parent_Name      =>
                 G.Data (G.Value_Type.First .. G.Value_Type.Last),
               Parent_Reference => G.Type_Start_Position,
               Tables           => All_Types_Table,
               Entity_Is_A_Type => False);
         end if;

         --  Register an approximate reference to the class (workaround
         --  limitation in SN)

         if Length > 0
           and then Sym.Start_Position.Column - 2 -
             Visible_Column_Type (Length) > 0
         then
            Class := Lookup_Entity_In_Tables
              (Handler,
               G.Data (G.Class.First .. G.Class.Last),
               Tables         => (CL => True, others => False),
               Current_Source => Get_File (Get_Declaration_Of (Entity)));

            if Class /= null then
               Add_Reference
                 (Entity   => Class,
                  Location =>
                    (File   => Get_File (Get_Declaration_Of (Entity)),
                     Line   => Sym.Start_Position.Line,
                     Column => Sym.Start_Position.Column - 2 -
                       Visible_Column_Type (Length)),
                  Kind     => Reference);
            end if;
         end if;
      end if;
   end Parse_GV_Table;

   --------------------
   -- Parse_IU_Table --
   --------------------

   procedure Parse_IU_Table
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      Sym     : FIL_Table)
   is
      Dep : constant Source_File := Get_Or_Create
        (Handler.Db,
         Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Handler);
   begin
      Add_Depends_On (Source, Dep, Explicit_Dependency => True);
   end Parse_IU_Table;

   -------------------
   -- Parse_T_Table --
   -------------------
   --  The following information is not used currently:
   --     End_Position, Attributes, Class_Name

   procedure Parse_T_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      Original : Entity_Information;
      Kind     : E_Kind;
      Success  : Boolean;
      V        : T_Table;
      Index    : Natural;
   begin
      Find (DB       => Handler.SN_Table (T),
            Name     => Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
            Position => Sym.Start_Position,
            Filename => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
            Tab      => V,
            Success  => Success);

      if Success then
         Index := V.Original.First;
         Skip_To_Char (String (V.Data), Index, ':');

         --  Name is of the form A::type
         if Index < V.Original.Last then
            Original := Lookup_Entity_In_Tables
              (Handler,
               V.Data (Index + 2 .. V.Original.Last),
               Current_Source    => Get_File (Get_Declaration_Of (Entity)),
               Tables => (IV | T => True, others => False),
               Class_Or_Function => V.Data (V.Original.First .. Index - 1));
         else
            Original := Lookup_Entity_In_Tables
              (Handler,
               V.Data (V.Original.First .. V.Original.Last),
               Tables           => (T | CL | UN | E => True, others => False),
               Current_Source   => Get_File (Get_Declaration_Of (Entity)));
         end if;

         if Original /= null then
            Set_Is_Renaming_Of (Entity, Renaming_Of => Original);
            Kind := Get_Kind (Original);
            Kind.Is_Type := True;
            Set_Kind           (Entity, Kind);

            --  Register an approximate reference to the renamed entity. At
            --  least, the "look around" algorithms in the queries will know
            --  to look for a reference in the neighborood.
            Add_Reference
              (Entity   => Original,
               Location =>
                 (File   => Get_File (Get_Declaration_Of (Entity)),
                  Line   => Get_Line (Get_Declaration_Of (Entity)),
                  Column => Get_Column (Get_Declaration_Of (Entity))),
               Kind     => Reference);
         end if;
      end if;
   end Parse_T_Table;

   --------------------
   -- Parse_IV_Table --
   --------------------
   --  The following information is not used currently:
   --     End_Position, Attributes

   procedure Parse_IV_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      Success  : Boolean;
      Var      : IV_Table;
   begin
      Find
        (DB             => Handler.SN_Table (IV),
         Class          => Sym.Key (Sym.Class.First .. Sym.Class.Last),
         Variable_Name  =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Start_Position => Sym.Start_Position,
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Tab            => Var,
         Success        => Success);

      if Success then
         --  Register a reference to the parent where the entity is defined.
         --  This is not accurate, but at least will provide enough information
         --  for the "look around" algorithms in the queries
         Set_Parent
           (Handler     => Handler,
            Entity      => Entity,
            Parent_Name =>
              Var.Data (Var.Value_Type.First .. Var.Value_Type.Last),
            Parent_Reference => Sym.Start_Position,
            Tables           => All_Types_Table,
            Class_Or_Function => Sym.Key (Sym.Class.First .. Sym.Class.Last));
      end if;
   end Parse_IV_Table;

   --------------------
   -- Parse_MD_Table --
   --------------------
   --  The following information is not used currently:
   --     Attributes, Template_Parameters

   procedure Parse_MD_Table
     (Handler : access CPP_Handler_Record'Class;
      Sym     : FIL_Table;
      Source  : Source_File)
   is
      Entity, Class : Entity_Information;
      Var      : MD_Table;
      Success  : Boolean;
      M        : MI_Table;
      P        : Pair;
      S        : Source_File;
   begin
      Find
        (DB             => Handler.SN_Table (MD),
         Class          => Sym.Key (Sym.Class.First .. Sym.Class.Last),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Start_Position => Sym.Start_Position,
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Tab            => Var,
         Success        => Success);

      if Success then
         Parse_Method_Table_Internal
           (Handler, Entity,
            Return_Type  =>
              Var.Data (Var.Return_Type.First .. Var.Return_Type.Last),
            Entity_Name  => Var.Key (Var.Name.First .. Var.Name.Last),
            Entity_Start => Var.Start_Position,
            Entity_File => Var.Key (Var.File_Name.First .. Var.File_Name.Last),
            Entity_Class => Var.Key (Var.Class.First .. Var.Class.Last),
            Arg_Types => Var.Data (Var.Arg_Types.First .. Var.Arg_Types.Last),
            Arg_Names => Var.Data (Var.Arg_Names.First .. Var.Arg_Names.Last),
            Source    => Source,
            Class     => Class);

         Set_End_Of_Scope
           (Entity,
            Location => (File   => Source,
                         Line   => Var.End_Position.Line,
                         Column => Var.End_Position.Column),
            Kind     => End_Of_Spec);

         Set_Attributes
           (Entity, (Global => (Var.Attributes and SN_STATIC) = 0,
                     others => False));

         --  Search the location of the body, so that the contextual menu
         --  "Go to body" is also activated.

         Set_Cursor_At
           (Handler.SN_Table (MI), Name => Get_Name (Entity).all);
         loop
            Get_Pair (Handler.SN_Table (MI), Next_By_Key, Result => P);
            exit when P = No_Pair;

            Parse_Pair (P, M);

            exit when M.Data (M.Arg_Types.First .. M.Arg_Types.Last) =
                Var.Data (Var.Arg_Types.First .. Var.Arg_Types.Last);
         end loop;

         Release_Cursor (Handler.SN_Table (MI));

         if P /= No_Pair then
            S := Get_Or_Create
              (Handler.Db, M.Key (M.File_Name.First .. M.File_Name.Last),
               Handler);
            Add_Reference
              (Entity   => Entity,
               Location => (File   => S,
                            Line   => M.Start_Position.Line,
                            Column => M.Start_Position.Column),
               Kind     => Body_Entity);
            Set_End_Of_Scope
              (Entity   => Entity,
               Location => (File   => S,
                            Line   => M.End_Position.Line,
                            Column => M.End_Position.Column),
               Kind     => End_Of_Body);
         end if;
      end if;
   end Parse_MD_Table;

   ---------------------------------
   -- Parse_Method_Table_Internal --
   ---------------------------------

   procedure Parse_Method_Table_Internal
     (Handler      : access CPP_Handler_Record'Class;
      Entity       : out Entity_Information;
      Entity_Name  : String;
      Entity_Start : Point;
      Return_Type  : String;
      Entity_File  : String;
      Entity_Class : String;
      Arg_Types    : String;
      Arg_Names    : String;
      Source       : Source_File;
      Class        : out Entity_Information) is
   begin
      Entity := Get_Or_Create
        (Name   => Entity_Name,
         File   => Source,
         Line   => Entity_Start.Line,
         Column => Entity_Start.Column);

      --  If the entity wasn't already known:
      if Get_Returned_Type (Entity) = null then
         Set_Subprogram_Return_Type (Handler, Entity, Return_Type);
         Process_Local_Variables_And_Parameters
           (Handler,
            Entity           => Entity,
            Entity_File_Name => Entity_File,
            Entity_Class     => Entity_Class,
            Entity_Arg_Types => Arg_Types,
            Params           => Arg_Names,
            Source           => Source);
         Parse_TO_Table
           (Handler       => Handler,
            Sym_Name      => Entity_Name,
            Sym_Class     => Entity_Class,
            Sym_Arg_Types => Arg_Types);
         Class := Lookup_Entity_In_Tables
           (Handler,
            Entity_Class,
            Current_Source                 => Source,
            Tables                         => (CL => True, others => False),
            Check_Predefined               => False,
            Check_Template_Arguments       => False,
            Check_Class_Template_Arguments => False,
            Class_Or_Function              => Invalid_String);

         if Class /= null then
            Add_Primitive_Subprogram (Class, Primitive => Entity);
         else
            Trace (Me, "Class not found in CL: " & Entity_Class);
         end if;
      end if;
   end Parse_Method_Table_Internal;

   ----------
   -- Find --
   ----------

   procedure Find
     (Handler : access CPP_Handler_Record'Class;
      Class   : String;
      Name    : String;
      Args    : String;
      D       : out MD_Table;
      Success : out Boolean)
   is
      P : Pair;
   begin
      if Is_Open (Handler.SN_Table (MD)) then
         Set_Cursor_At (Handler.SN_Table (MD), Class => Class, Name => Name);
         loop
            Get_Pair (Handler.SN_Table (MD), Next_By_Key, P);
            exit when P = No_Pair;

            Parse_Pair (P, D);
            exit when D.Data (D.Arg_Types.First .. D.Arg_Types.Last) = Args;
         end loop;

         Release_Cursor (Handler.SN_Table (MD));
         Success := P /= No_Pair;
      else
         Success := False;
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (Handler : access CPP_Handler_Record'Class;
      Class   : String;
      Name    : String;
      Args    : String;
      M       : out MI_Table;
      Success : out Boolean)
   is
      P : Pair;
   begin
      Set_Cursor_At (Handler.SN_Table (MI), Class => Class, Name => Name);
      loop
         Get_Pair (Handler.SN_Table (MI), Next_By_Key, P);
         exit when P = No_Pair;

         Parse_Pair (P, M);
         exit when M.Data (M.Arg_Types.First .. M.Arg_Types.Last) = Args;
      end loop;

      Release_Cursor (Handler.SN_Table (MI));
      Success := P /= No_Pair;
   end Find;

   --------------------
   -- Parse_MI_Table --
   --------------------
   --  The following information is not used currently:
   --     Attributes, Template_Parameters

   procedure Parse_MI_Table
     (Handler : access CPP_Handler_Record'Class;
      Sym     : FIL_Table;
      Source  : Source_File)
   is
      D             : MD_Table;
      M             : MI_Table;
      SuccessD      : Boolean;
      SuccessI      : Boolean;
      Class_Length  : Natural;
      Entity, Class : Entity_Information;
   begin
      --  Find and parse the corresponding method declaration.
      --  We cannot search the FIL table, since only one cursor can exist
      --  at any time, and we are called from Parse_FIL_Table.

      Find (Handler,
            Class   => Sym.Key (Sym.Class.First .. Sym.Class.Last),
            Name    => Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
            Args    => Sym.Data
              (Sym.Types_Of_Arguments.First .. Sym.Types_Of_Arguments.Last),
            D       => D,
            Success => SuccessD);
      Find (Handler,
            Class   => Sym.Key (Sym.Class.First .. Sym.Class.Last),
            Name    => Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
            Args    => Sym.Data
              (Sym.Types_Of_Arguments.First .. Sym.Types_Of_Arguments.Last),
            M       => M,
            Success => SuccessI);

      if SuccessD then
         --  Insert the declaration. Extra info will be/has been inserted when
         --  the MD table is parsed.
         Entity := Get_Or_Create
           (Name   => D.Key (D.Name.First .. D.Name.Last),
            File   => Get_Or_Create
              (Handler.Db, D.Key (D.File_Name.First .. D.File_Name.Last),
               Handler),
            Line   => D.Start_Position.Line,
            Column => D.Start_Position.Column);

         if SuccessI then
            Add_Reference
              (Entity   => Entity,
               Location => (File   => Source,
                            Line   => M.Start_Position.Line,
                            Column => M.Start_Position.Column),
               Kind     => Body_Entity);
         end if;

      else
         if SuccessI then
            Parse_Method_Table_Internal
              (Handler,
               Entity       => Entity,
               Return_Type  =>
                 M.Data (M.Return_Type.First .. M.Return_Type.Last),
               Entity_Name  => M.Key (M.Name.First .. M.Name.Last),
               Entity_Start => M.Start_Position,
               Entity_File  => M.Key (M.File_Name.First .. M.File_Name.Last),
               Entity_Class => M.Key (M.Class.First .. M.Class.Last),
               Arg_Types    => M.Data (M.Arg_Types.First .. M.Arg_Types.Last),
               Arg_Names    => M.Data (M.Arg_Names.First .. M.Arg_Names.Last),
               Source       => Source,
               Class        => Class);
         else
            Entity := Get_Or_Create
              (Name   => Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
               File   => Source,
               Line   => Sym.Start_Position.Line,
               Column => Sym.Start_Position.Column);
         end if;
      end if;

      if SuccessI then
         --  The class might not have been computed if the entity was already
         --  in the table. Compute it now in that case.
         if Class = null then
            Class := Lookup_Entity_In_Tables
              (Handler,
               M.Key (M.Class.First .. M.Class.Last),
               Current_Source                 => Source,
               Tables                         => (CL => True, others => False),
               Check_Predefined               => False,
               Check_Template_Arguments       => False,
               Check_Class_Template_Arguments => False,
               Class_Or_Function              => Invalid_String);
         end if;

         --  Add a reference for the class name.
         --  Since SN doesn't give us this information, we'll assume that the
         --  name of the method is always "class::name", ie no space, and
         --  then compute the start of "class".
         --  Do not insert a new reference to the class is the MI and MD are
         --  at the same location

         Class_Length := M.Class.Last - M.Class.First + 1;

         if Class /= null
           and then M.Start_Position.Column - 2 -
             Visible_Column_Type (Class_Length) > 0
           and then (M.Start_Position.Line /=
                       Get_Line (Get_Declaration_Of (Entity))
                     or else M.Start_Position.Column /=
                       Get_Column (Get_Declaration_Of (Entity)))
         then
            Add_Reference
              (Class,
               Location =>
                 (File   => Source,
                  Line   => M.Start_Position.Line,
                  Column => M.Start_Position.Column - 2 -
                    Visible_Column_Type (Class_Length)),
               Kind     => Reference);
         end if;

         Set_End_Of_Scope
           (Entity,
            Location => (File   => Source,
                         Line   => M.End_Position.Line,
                         Column => M.End_Position.Column),
            Kind => End_Of_Body);
      end if;
   end Parse_MI_Table;

   --------------------
   -- Parse_CL_Table --
   --------------------
   --  The following information is not used currently:
   --     Template_Parameters

   procedure Parse_CL_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      C       : CL_Table;
      Kind    : E_Kind := Class_Entity;
      Base    : IN_Table;
      Parent  : Entity_Information;
      Success : Boolean;
      P       : Pair;
   begin
      Find
        (DB             => Handler.SN_Table (CL),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Start_Position => Sym.Start_Position,
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Tab            => C,
         Success        => Success);

      if Success then
         Kind.Is_Generic := (C.Attributes and SN_TEMPLATE) /= 0;
         Set_Kind (Entity, Kind);

         Set_End_Of_Scope
           (Entity   => Entity,
            Location => (File   => Get_File (Get_Declaration_Of (Entity)),
                         Line   => C.End_Position.Line,
                         Column => C.End_Position.Column),
            Kind     => End_Of_Spec);

         --  Add base classes

         if Is_Open (Handler.SN_Table (SN_IN)) then
            Set_Cursor
              (Handler.SN_Table (SN_IN),
               By_Key,
               String (C.Key (C.Name.First .. C.Name.Last)) & Field_Sep,
               Exact_Match => False);

            loop
               Get_Pair (Handler.SN_Table (SN_IN), Next_By_Key, Result => P);
               exit when P = No_Pair;

               Parse_Pair (P, Base);

               Parent := Lookup_Entity_In_Tables
                 (Handler,
                  Base.Key (Base.Base_Class.First .. Base.Base_Class.Last),
                  Current_Source     => Get_File (Get_Declaration_Of (Entity)),
                  Tables                      => (CL => True, others => False),
                  Check_Predefined               => False,
                  Check_Template_Arguments       => False,
                  Check_Class_Template_Arguments => False,
                  Class_Or_Function              => Invalid_String);

               if Parent /= null then
                  Set_Type_Of (Entity, Parent);
                  Add_Reference
                    (Parent,
                     Location =>
                       (File   => Get_File (Get_Declaration_Of (Entity)),
                        Line   => Base.Start_Position.Line,
                        Column => Base.Start_Position.Column),
                     Kind => Reference);
               end if;
            end loop;

            Release_Cursor (Handler.SN_Table (SN_IN));
         end if;
      end if;
   end Parse_CL_Table;

   --------------------
   -- Parse_TA_Table --
   --------------------
   --  The following information is not used currently:
   --    Attributes, Template_Parameters, Class_Name

   procedure Parse_TA_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      T : TA_Table;
      Success : Boolean;
   begin
      Find
        (DB             => Handler.SN_Table (TA),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Start_Position => Sym.Start_Position,
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Class          => Sym.Key (Sym.Class.First .. Sym.Class.Last),
         Tab            => T,
         Success        => Success);

      if Success then
         Set_Parent
           (Handler        => Handler,
            Entity         => Entity,
            Parent_Name    => T.Data (T.Value_Type.First .. T.Value_Type.Last),
            Parent_Reference => T.Type_Position,
            Tables           => All_Types_Table,
            Entity_Is_A_Type => True);
      end if;
   end Parse_TA_Table;

   --------------------
   -- Parse_EC_Table --
   --------------------
   --  The following information is not used currently:
   --     End_Position, Attributes

   procedure Parse_EC_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      C       : EC_Table;
      Success : Boolean;
      Parent  : Entity_Information;
   begin
      Find
        (DB             => Handler.SN_Table (EC),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Start_Position => Sym.Start_Position,
         Tab            => C,
         Success        => Success);

      if Success then
         Parent := Lookup_Entity_In_Tables
           (Handler => Handler,
            Name    => C.Data
              (C.Enumeration_Name.First .. C.Enumeration_Name.Last),
            Current_Source    => Get_File (Get_Declaration_Of (Entity)),
            Tables                         => (E => True, others => False),
            Check_Predefined               => False,
            Check_Template_Arguments       => False,
            Check_Class_Template_Arguments => False);

         if Parent /= null then
            Set_Type_Of (Entity, Is_Of_Type => Parent);
         end if;
      end if;

      Set_Kind_From_Table_If_Not_Set (Entity, EC);
   end Parse_EC_Table;

   -------------------
   -- Parse_E_Table --
   -------------------
   --  The following information is not used currently:
   --     Attributes

   procedure Parse_E_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      ET      : E_Table;
      Success : Boolean;
   begin
      Find
        (DB             => Handler.SN_Table (E),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Start_Position => Sym.Start_Position,
         Tab            => ET,
         Success        => Success);

      if Success then
         Set_End_Of_Scope
           (Entity   => Entity,
            Location => (File   => Get_File (Get_Declaration_Of (Entity)),
                         Line   => ET.End_Position.Line,
                         Column => ET.End_Position.Column),
            Kind     => End_Of_Spec);
      end if;
   end Parse_E_Table;

   ---------------------
   -- Parse_CON_Table --
   ---------------------
   --  The following information is not used currently:
   --     End_Position, Attributes

   procedure Parse_CON_Table
     (Handler : access CPP_Handler_Record'Class;
      Entity  : Entity_Information;
      Sym     : FIL_Table)
   is
      C       : CON_Table;
      Success : Boolean;
   begin
      Find
        (DB             => Handler.SN_Table (CON),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Start_Position => Sym.Start_Position,
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Tab            => C,
         Success        => Success);

      if Success then
         Set_Parent
           (Handler          => Handler,
            Entity           => Entity,
            Parent_Name      =>
              C.Data (C.Declared_Type.First .. C.Declared_Type.Last),
            Parent_Reference => C.Type_Start_Position,
            Tables           => All_Types_Table,
            Entity_Is_A_Type => False);
      end if;
   end Parse_CON_Table;

   ---------------------------------------
   -- Find_Forward_Declaration_And_Body --
   ---------------------------------------
   --  The following information is not used currently:
   --    Attributes, Return_Type, Arg_Types, Template_Parameters

   function Find_Forward_Declaration_And_Body
     (Handler    : access CPP_Handler_Record'Class;
      Name       : String;
      Filename   : String;
      Args       : String;
      Body_Known : Boolean;
      Body_Info  : FU_Table := No_FU_Table) return Entity_Information
   is
      P      : Pair;
      FD_Tab : FD_Table;
      Decl   : Entity_Information;
      S      : Source_File;
   begin
      if Is_Open (Handler.SN_Table (FD)) then
         Set_Cursor
           (Handler.SN_Table (FD), By_Key, Name & Field_Sep, False);
         loop
            Get_Pair (Handler.SN_Table (FD), Next_By_Key, Result => P);
            exit when P = No_Pair;

            Parse_Pair (P, FD_Tab);

            exit when
              (Filename = ""
               or else Filename =
                 FD_Tab.Key (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last))
              and then Args =
                FD_Tab.Data (FD_Tab.Arg_Types.First .. FD_Tab.Arg_Types.Last);
            --  We compare prototypes, even though we have no garantee that the
            --  forward declaration includes the full prototype. In this case,
            --  we will have to separate entities, and no way to reconcile
            --  them. This is better than accepting any forward decl, since
            --  it might match too often, and would give incorrect results.
         end loop;

         Release_Cursor (Handler.SN_Table (FD));

         if P /= No_Pair then
            S := Get_Or_Create
              (Handler.Db,
               FD_Tab.Key (FD_Tab.File_Name.First .. FD_Tab.File_Name.Last),
               Handler);

            Decl := Get_Or_Create
              (Name         => Name,
               File         => S,
               Line         => FD_Tab.Start_Position.Line,
               Column       => FD_Tab.Start_Position.Column,
               Allow_Create => False);

            if Decl = null then
               Decl := Get_Or_Create
                 (Name   => Name,
                  File   => S,
                  Line   => FD_Tab.Start_Position.Line,
                  Column => FD_Tab.Start_Position.Column);

               Set_Subprogram_Return_Type
                 (Handler     => Handler,
                  Entity      => Decl,
                  Return_Type => FD_Tab.Data
                    (FD_Tab.Return_Type.First .. FD_Tab.Return_Type.Last));
               Set_Attributes
                 (Decl, (Global => (FD_Tab.Attributes and SN_STATIC) = 0,
                         others => False));

            --  ??? The following will add duplicate references in some cases
--              Add_Reference
--                (Decl,
--                 Location => (File   => Source,
--                              Line   => FD_Tab.End_Position.Line,
--                              Column => FD_Tab.End_Position.Column),
--                 Kind     => End_Of_Spec);

               --  Get the body info, since we want to be able to show
               --  "go to body" in the contextual menus
               Trace (Me, "Find_Forward_Declaration_And_Body " & Name
                      & " " & Args);
               Parse_FU_Table_From_FD
                 (Handler    => Handler,
                  Entity     => Decl,
                  Args       => Args,
                  Body_Known => Body_Known,
                  Body_Info  => Body_Info);
            end if;
            return Decl;
         end if;
      end if;

      return null;
   end Find_Forward_Declaration_And_Body;

   --------------------------------
   -- Set_Subprogram_Return_Type --
   --------------------------------

   procedure Set_Subprogram_Return_Type
     (Handler     : access CPP_Handler_Record'Class;
      Entity      : Entity_Information;
      Return_Type : String)
   is
      Parent : Entity_Information;
      Start  : Natural;
   begin
      Cleanup_Entity_Name (Return_Type, Start);

      if Return_Type (Start .. Return_Type'Last) = "void" then
         Set_Kind (Entity, Procedure_Entity);

      else
         Set_Kind (Entity, Function_Entity);
         Parent := Lookup_Entity_In_Tables
           (Handler, Return_Type (Start .. Return_Type'Last),
            Tables           => All_Types_Table,
            Current_Source   => Get_File (Get_Declaration_Of (Entity)));

         if Parent /= null then
            Set_Returned_Type (Entity, Parent);
         end if;
      end if;
   end Set_Subprogram_Return_Type;

   --------------------
   -- Parse_FD_Table --
   --------------------

   function Parse_FD_Table
     (Handler : access CPP_Handler_Record'Class;
      Sym     : FIL_Table) return Entity_Information
   is
   begin
      return Find_Forward_Declaration_And_Body
        (Handler,
         Name       => Sym.Key  (Sym.Identifier.First .. Sym.Identifier.Last),
         Filename   => Sym.Key  (Sym.File_Name.First .. Sym.File_Name.Last),
         Args       => Sym.Data (Sym.Types_Of_Arguments.First ..
                                 Sym.Types_Of_Arguments.Last),
         Body_Known => False);
   end Parse_FD_Table;

   ----------------------------
   -- Parse_FU_Table_From_FD --
   ----------------------------

   procedure Parse_FU_Table_From_FD
     (Handler    : access CPP_Handler_Record'Class;
      Entity     : Entity_Information;
      Args       : String;
      Body_Known : Boolean;
      Body_Info  : FU_Table)
   is
      P      : Pair;
      F      : FU_Table;
      Source : Source_File;
      Body_Found : Boolean;
   begin
      if Body_Known then
         F := Body_Info;
         Body_Found := True;
      else
         Set_Cursor_At
           (Handler.SN_Table (FU), Name => Get_Name (Entity).all);
         loop
            Get_Pair (Handler.SN_Table (FU), Next_By_Key, Result => P);
            exit when P = No_Pair;

            --  Handling of overloaded entities: check the arguments
            --  of the entities, especially in the FU table
            Parse_Pair (P, F);

            exit when Args = Invalid_String
              or else F.Data (F.Arg_Types.First .. F.Arg_Types.Last) = Args;
         end loop;

         Release_Cursor (Handler.SN_Table (FU));
         Body_Found := P /= No_Pair;
      end if;

      if Body_Found then
         Source := Get_Or_Create
           (Handler.Db,
            F.Key (F.File_Name.First .. F.File_Name.Last),
            Handler);

         Add_Reference
           (Entity   => Entity,
            Location =>
              (File   => Source,
               Line   => F.Start_Position.Line,
               Column => F.Start_Position.Column),
            Kind     => Body_Entity);
         Set_End_Of_Scope
           (Entity   => Entity,
            Location => (File   => Source,
                         Line   => F.End_Position.Line,
                         Column => F.End_Position.Column),
            Kind     => End_Of_Body);
      end if;
   end Parse_FU_Table_From_FD;

   --------------------
   -- Parse_FU_Table --
   --------------------
   --  The following information is not used currently:
   --     Attributes, Arg_Types, Template_Parameters

   procedure Parse_FU_Table
     (Handler : access CPP_Handler_Record'Class;
      Source  : Source_File;
      Sym     : FIL_Table)
   is
      C       : FU_Table;
      Success : Boolean;
      S       : Source_File := Source;
      Entity  : Entity_Information;
      End_Of_Scope_Kind : Reference_Kind;
   begin
      Find
        (DB             => Handler.SN_Table (FU),
         Name           =>
           Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
         Start_Position => Sym.Start_Position,
         Filename       => Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
         Tab            => C,
         Success        => Success);

      if Success then
         --  Find forward declaration if any, possibly in another file
         Entity := Find_Forward_Declaration_And_Body
           (Handler,
            Name       =>
              Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last),
            Filename   => "",
            Args       => C.Data (C.Arg_Types.First .. C.Arg_Types.Last),
            Body_Known => True,
            Body_Info  => C);

         if Full_Name (Get_Filename (Source)).all /=
           C.Key (C.File_Name.First  .. C.File_Name.Last)
         then
            S := Get_Or_Create
              (Handler.Db, C.Key (C.File_Name.First  .. C.File_Name.Last),
               Handler);
         end if;

         if Entity = null then
            Entity := Entity_From_FIL (Sym, S);
            End_Of_Scope_Kind := End_Of_Spec;
         else
            --  Check if we already have the body reference, which might be
            --  the case if we first parsed a .h file containing the forward
            --  declaration, and then parse the .c file containing the body

            --  Sym is the body in this case
            --  No need to set the body, since it should already be set
            --  when looking for the forward declaration anyway
--              Add_Reference
--                (Entity   => Entity,
--                 Location =>
--                   (File   => S,
--                    Line   => C.Start_Position.Line,
--                    Column => Column_Type (C.Start_Position.Column)),
--                 Kind     => Body_Entity);
            End_Of_Scope_Kind := End_Of_Body;
         end if;

         Set_End_Of_Scope
           (Entity   => Entity,
            Location => (File   => S,
                         Line   => C.End_Position.Line,
                         Column => C.End_Position.Column),
            Kind     => End_Of_Scope_Kind);

         Set_Subprogram_Return_Type
           (Handler, Entity,
            C.Data (C.Return_Type.First .. C.Return_Type.Last));

         Process_Local_Variables_And_Parameters
           (Handler,
            Entity           => Entity,
            Entity_File_Name =>
              Sym.Key (Sym.File_Name.First .. Sym.File_Name.Last),
            Entity_Class     => Sym.Key (Sym.Class.First .. Sym.Class.Last),
            Entity_Arg_Types => C.Data (C.Arg_Types.First .. C.Arg_Types.Last),
            Params           => C.Data (C.Arg_Names.First .. C.Arg_Names.Last),
            Source           => S);

         Parse_TO_Table
           (Handler     => Handler,
            Sym_Name    => C.Key (C.Name.First .. C.Name.Last),
            Sym_Class   => C.Key (C.Class.First .. C.Class.Last),
            Sym_Arg_Types => C.Data (C.Arg_Types.First .. C.Arg_Types.Last));
      end if;
   end Parse_FU_Table;

   --------------------
   -- Parse_TO_Table --
   --------------------
   --  The following information is not used currently:
   --     Referred_Argument_Types

   procedure Parse_TO_Table
     (Handler       : access CPP_Handler_Record'Class;
      Sym_Name      : String;
      Sym_Class     : String;
      Sym_Arg_Types : String)
   is
      P          : Pair;
      R          : TO_Table;
      Ref        : Entity_Information;
      Kind       : Reference_Kind;
      Arr        : Boolean_Table_Array;
      Ref_Source : Source_File;
   begin
      if Is_Open (Handler.SN_Table (TO)) then
         if Sym_Class'Length = 0 then
            Set_Cursor
              (Handler.SN_Table (TO),
               Position    => By_Key,
               Key         =>
               '#' & Field_Sep & Sym_Name & Field_Sep & "fu" & Field_Sep,
               Exact_Match => False);
         else
            Set_Cursor
              (Handler.SN_Table (TO),
               Position    => By_Key,
               Key         => Sym_Class & Field_Sep & Sym_Name & Field_Sep
               & "mi" & Field_Sep,
               Exact_Match => False);
         end if;

         loop
            Get_Pair (Handler.SN_Table (TO), Next_By_Key, Result => P);
            exit when P = No_Pair;

            Parse_Pair (P, R);

            if Sym_Arg_Types = R.Data
              (R.Caller_Argument_Types.First .. R.Caller_Argument_Types.Last)
              and then R.Referred_Symbol /= Undef
            then
               case R.Referred_Symbol is
                  when FU | FD => Arr := (FU | FD => True, others => False);
                  when MI | MD => Arr := (MI | MD => True, others => False);
                  when others =>
                     Arr := (others => False);
                     Arr (R.Referred_Symbol) := True;
               end case;

               Ref_Source := Get_Or_Create
                 (Handler.Db,
                  R.Key (R.File_Name.First .. R.File_Name.Last),
                  Handler);

               --  An undefined entity ?
               if R.Referred_Symbol = UD then

                  --  We can't really be smarter than SN, especially since we
                  --  might be referencing a field of a record. Perhaps we
                  --  could do this only if we have a function, but even then
                  --  this is doubtful.
--                    Ref := Lookup_Non_Overloaded_Entity
--                      (In_File => Sym_File,
--                       Name    => R.Key
--                         (R.Referred_Symbol_Name.First
--                          .. R.Referred_Symbol_Name.Last));

                  Ref := Get_Or_Create
                    (Name         => R.Key
                       (R.Referred_Symbol_Name.First
                        .. R.Referred_Symbol_Name.Last),
                     File         => Ref_Source,
                     Line         => Predefined_Line,
                     Column       => Predefined_Column,
                     Allow_Create => True);
                  Set_Kind (Ref, Unresolved);

               elsif R.Referred_Symbol = TA then
                  --  Bug in SN: the class for a TA is left to "#", so we
                  --  use the name of the generic entity instead
                  Ref := Lookup_Entity_In_Tables
                    (Handler => Handler,
                     Name    => R.Key
                       (R.Referred_Symbol_Name.First
                        .. R.Referred_Symbol_Name.Last),
                     Current_Source    => Ref_Source,
                     Class_Or_Function => Sym_Name,
                     Tables                         => Arr,
                     Check_Predefined               => False,
                     Check_Template_Arguments       => False,
                     Check_Class_Template_Arguments => False);

               else
                  Ref := Lookup_Entity_In_Tables
                    (Handler => Handler,
                     Name    => R.Key
                       (R.Referred_Symbol_Name.First
                        .. R.Referred_Symbol_Name.Last),
                     Current_Source    => Ref_Source,
                     Class_Or_Function => R.Key
                       (R.Referred_Class.First .. R.Referred_Class.Last),
                     Tables                         => Arr,
                     Args                           => R.Data
                       (R.Referred_Argument_Types.First
                        .. R.Referred_Argument_Types.Last),
                     Check_Predefined               => False,
                     Check_Template_Arguments       => False,
                     Check_Class_Template_Arguments => False);
               end if;

               if Ref /= null then
                  case R.Key (R.Access_Type.First) is
                     when 'w' => Kind := Modification;
                     when 'p' =>
                        --  Passed as a parameter
                        Kind := Modification;
                     when 'r' => Kind := Reference;
                     when others =>
                        Trace (Me, "Unknown access_type in TO table: "
                               & R.Key (R.Access_Type.First));
                        Kind := Reference;
                  end case;

                  --  Parameters declaration are also visible in the TO
                  --  table, but we shouldn't list these as a reference.

                  if R.Key (R.Access_Type.First) /= 'p'
                    or else Get_Line (Get_Declaration_Of (Ref)) /=
                    R.Position.Line
                    or else Get_Column (Get_Declaration_Of (Ref)) /=
                    R.Position.Column
                  then
                     Add_Reference
                       (Entity   => Ref,
                        Location =>
                          (File   => Ref_Source,
                           Line   => R.Position.Line,
                           Column => R.Position.Column),
                        Kind     => Kind);
                  end if;
               else
                  Trace (Me, "Entity not found from TO table: "
                         & R.Key (R.Referred_Symbol_Name.First
                                  .. R.Referred_Symbol_Name.Last)
                         & " in " & R.Referred_Symbol'Img);
               end if;
            end if;
         end loop;

         Release_Cursor (Handler.SN_Table (TO));
      end if;
   end Parse_TO_Table;

   --------------------
   -- Parse_LV_Table --
   --------------------
   --  The following information is not used currently:
   --    End_Position, Attributes, Type_Start_Position

   procedure Parse_LV_Table
     (Handler          : access CPP_Handler_Record'Class;
      Entity           : Entity_Information;
      Entity_File_Name : String;
      Entity_Class     : String;
      Entity_Arg_Types : String;
      Params           : String;
      Parsed_Params    : Segment_Array;
      Source           : Source_File)
   is
      P     : Pair;
      Var   : LV_Table;
      Local : Entity_Information;
   begin
      if Is_Open (Handler.SN_Table (LV)) then
         Set_Cursor
           (Handler.SN_Table (LV),
            By_Key,
            Get_Name (Entity).all & Field_Sep,
            Exact_Match => False);

         loop
            Get_Pair (Handler.SN_Table (LV), Next_By_Key, Result => P);
            exit when P = No_Pair;

            Parse_Pair (P, Var);

            if Match_With_Joker
              (Var.Data (Var.Class.First .. Var.Class.Last), Entity_Class)
              and then Var.Key (Var.File_Name.First .. Var.File_Name.Last) =
              Entity_File_Name
              and then Var.Data (Var.Arg_Types.First .. Var.Arg_Types.Last) =
              Entity_Arg_Types
            then
               Local := Get_Or_Create
                 (Name   => Var.Key (Var.Name.First .. Var.Name.Last),
                  File   => Source,
                  Line   => Var.Start_Position.Line,
                  Column => Var.Start_Position.Column);

               for P in Parsed_Params'Range loop
                  if Params (Parsed_Params (P).First .. Parsed_Params (P).Last)
                    = Var.Key (Var.Name.First .. Var.Name.Last)
                  then
                     Add_Reference
                       (Entity   => Entity,
                        Location =>
                          (File   => Source,
                           Line   => Var.Start_Position.Line,
                           Column => Var.Start_Position.Column),
                        Kind     => Subprogram_In_Parameter);
                  end if;
               end loop;

               --  Do not store a reference to the parent, since this is also
               --  mentionned in the TO table, and thus would be duplicated
               Set_Parent
                 (Handler     => Handler,
                  Entity      => Local,
                  Tables      => All_Types_Table,
                  Parent_Name =>
                    Var.Data (Var.Value_Type.First .. Var.Value_Type.Last));
            end if;
         end loop;

         Release_Cursor (Handler.SN_Table (LV));
      end if;
   end Parse_LV_Table;

   ----------------------
   -- Match_With_Joker --
   ----------------------

   function Match_With_Joker (Str1, Str2_With_Joker : String) return Boolean is
   begin
      return Str2_With_Joker = "#"
        or else Str1 = Str2_With_Joker;
   end Match_With_Joker;

   --------------------------------------------
   -- Process_Local_Variables_And_Parameters --
   --------------------------------------------

   procedure Process_Local_Variables_And_Parameters
     (Handler          : access CPP_Handler_Record'Class;
      Entity           : Entity_Information;
      Entity_File_Name : String;
      Entity_Class     : String;
      Entity_Arg_Types : String;
      Params           : String;
      Source           : Source_File)
   is
      Params_Count : Natural := 1;
   begin
      if Params'Length = 0 then
         Parse_LV_Table
           (Handler, Entity,
            Entity_File_Name, Entity_Class, Entity_Arg_Types,
            Params,
            Segment_Array'(1 .. 0 => Invalid_Segment),
            Source);

      else
         --  Params has the following format: "Param1, Param2, Param3"
         for P in Params'Range loop
            if Params (P) = ',' then
               Params_Count := Params_Count + 1;
            end if;
         end loop;

         declare
            Parsed_Params : Segment_Array (1 .. Params_Count);
         begin
            Parsed_Params (1).First := Params'First;
            Params_Count := 2;

            for P in Params'Range loop
               if Params (P) = ',' then
                  Parsed_Params (Params_Count - 1).Last :=  P - 1;
                  Parsed_Params (Params_Count).First :=  P + 1;
                  Params_Count := Params_Count + 1;
               end if;
            end loop;

            Parsed_Params (Parsed_Params'Last).Last := Params'Last;

            Parse_LV_Table
              (Handler, Entity,
               Entity_File_Name, Entity_Class, Entity_Arg_Types,
               Params, Parsed_Params, Source);
         end;
      end if;
   end Process_Local_Variables_And_Parameters;

   ----------------
   -- Set_Parent --
   ----------------

   procedure Set_Parent
     (Handler           : access CPP_Handler_Record'Class;
      Entity            : Entity_Information;
      Parent_Name       : String;
      Parent_Reference  : Point := Invalid_Point;
      Entity_Is_A_Type  : Boolean := False;
      Tables            : Boolean_Table_Array := All_Types_Table;
      Class_Or_Function : String := Invalid_String)
   is
      Parent     : Entity_Information;
      Name_Start : Integer;
      Kind       : E_Kind;
   begin
      Cleanup_Entity_Name (Parent_Name, Name_Start);

      Parent := Lookup_Entity_In_Tables
        (Handler, Parent_Name (Name_Start .. Parent_Name'Last),
         Current_Source    => Get_File (Get_Declaration_Of (Entity)),
         Tables            => Tables,
         Class_Or_Function => Class_Or_Function);

      if Parent /= null then
         Set_Type_Of (Entity, Is_Of_Type => Parent);

         Kind := Get_Kind (Parent);
         Kind.Is_Type := Entity_Is_A_Type;
         Set_Kind (Entity, Kind);
      end if;

      if Parent /= null and then Parent_Reference /= Invalid_Point then
         Add_Reference
           (Entity   => Parent,
            Location => File_Location'
              (File   => Get_File (Get_Declaration_Of (Entity)),
               Line   => Parent_Reference.Line,
               Column => Parent_Reference.Column),
            Kind     => Reference);
      end if;
   end Set_Parent;

   ---------------------
   -- Entity_From_FIL --
   ---------------------

   function Entity_From_FIL
     (Sym     : FIL_Table;
      Source  : Source_File) return Entity_Information is
   begin
      return Get_Or_Create
        (Name   => String
           (Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last)),
         File   => Source,
         Line   => Sym.Start_Position.Line,
         Column => Sym.Start_Position.Column);
   end Entity_From_FIL;

   ---------------------
   -- Parse_FIL_Table --
   ---------------------

   procedure Parse_FIL_Table
     (Handler : access CPP_Handler_Record'Class;
      Project : Project_Type;
      Source  : Source_File)
   is
      P      : Pair;
      Sym    : FIL_Table;
      Entity : Entity_Information;
      DB_Dir           : constant String := Get_DB_Dir (Project);
      TO_File_Name     : constant Virtual_File :=
        Create (Full_Filename => DB_Dir & SN.Browse.DB_File_Name & ".to");
      S      : Source_File;
   begin
      if Is_Open (Handler.SN_Table (FIL)) then
         Set_Cursor
           (Handler.SN_Table (FIL),
            Position    => By_Key,
            Key         => Full_Name (Get_Filename (Source)).all & Field_Sep,
            Exact_Match => False);

         loop
            Get_Pair (Handler.SN_Table (FIL), Next_By_Key, Result => P);
            exit when P = No_Pair;
            Parse_Pair (P, Sym);

            case Sym.Symbol is
               when GV =>
                  Parse_GV_Table (Handler, Source, Sym);

               when IU =>
                  Parse_IU_Table (Handler, Source, Sym);

               when T =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_T_Table (Handler, Entity, Sym);

               when CL =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_CL_Table (Handler, Entity, Sym);

               when CON =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_CON_Table (Handler, Entity, Sym);

               when MA =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Set_Kind (Entity, Macro_Entity);

               when FU =>
                  Parse_FU_Table (Handler, Source, Sym);

               when IV =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_IV_Table (Handler, Entity, Sym);

               when MD =>
                  Parse_MD_Table (Handler, Sym, Source);

               when MI =>
                  Parse_MI_Table (Handler, Sym, Source);

               when TA =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_TA_Table (Handler, Entity, Sym);

               when FD    =>
                  Entity := Parse_FD_Table (Handler, Sym);
                  --  Do something for cpp_ellipsis1 (we have one warning
                  --  in FD)

               when SN_IN => null; --  Parsed when handling CL

               when E =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_E_Table (Handler, Entity, Sym);

               when EC =>
                  Entity := Entity_From_FIL (Sym, Source);
                  Parse_EC_Table (Handler, Entity, Sym);

               when UN | Undef | COM | COV | FR | LV | SU | UD =>
                  Trace
                    (Me, "Parse_FIL_Table: "
                     & Base_Name (Get_Filename (Source))
                     & " has unparsed " & Sym.Symbol'Img & " for "
                     & Sym.Key (Sym.Identifier.First .. Sym.Identifier.Last));
            end case;
         end loop;

         Release_Cursor (Handler.SN_Table (FIL));

         --  Assign a new LI to the source, so that we know when the database
         --  was generated
         S := Get_Or_Create
           (Handler.Db,
            File => Get_Filename (Source),
            LI   => Get_Or_Create (Handler.Db, TO_File_Name, Project),
            Handler => LI_Handler (Handler));
         Set_Time_Stamp (Get_LI (S), File_Time_Stamp (TO_File_Name));
         Set_Time_Stamp (S, Time_Stamp_From_DB (Project, Get_Filename (S)));
      end if;
   end Parse_FIL_Table;

   ----------------
   -- Parse_File --
   ----------------

   procedure Parse_File
     (Handler : access CPP_Handler_Record'Class;
      Project : Project_Type;
      Source  : Source_File) is
   begin
      --  Do not call Reset (Source) at this point: parsing the .cpp file after
      --  the .h file would result in losing references found in the TO table
      --  while parsing the .h file. For instance, most references are found
      --  in the .cpp file when analyzing the TO table for entities declared in
      --  the .h file

      Trace (Me, "Parse_File " & Full_Name (Get_Filename (Source)).all);
      Parse_FIL_Table (Handler, Project, Source);

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Release_Cursor (Handler.SN_Table (FIL));
         --  Free (Module_Typedefs);
   end Parse_File;

   ---------------------
   -- Get_Source_Info --
   ---------------------

   function Get_Source_Info
     (Handler               : access CPP_Handler_Record;
      Source_Filename       : VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null) return Source_File
   is
      function Load_File
        (File : VFS.Virtual_File; File_Project : Project_Type := No_Project)
         return Source_File;
      --  Load the information for one specific file in memory

      ---------------
      -- Load_File --
      ---------------

      function Load_File
        (File : VFS.Virtual_File; File_Project : Project_Type := No_Project)
         return Source_File
      is
         Project : Project_Type;
         Source  : Source_File;
      begin
         Source := Get_Or_Create
           (DB      => Handler.Db,
            File    => File,
            Handler => LI_Handler (Handler),
            LI      => null);
         if Source = null then
            if File_Has_No_LI_Report /= null then
               Entities.Error
                 (File_Has_No_LI_Report.all, Source);
            end if;

            Trace (Me, "Couldn't create Source_File for "
                   & Full_Name (File).all);
            return null;
         end if;

         --  If the database has changed on disk, update the in-memory
         --  contents. Likewise if the object directory itself has changed when
         --  the project view has changed.
         if File_Project = No_Project then
            Project := Get_Project_From_File (Handler.Registry, File);
         else
            Project := File_Project;
         end if;

         if Get_Time_Stamp (Source) = VFS.No_Time
           or else Get_LI (Source) = null
           or else Database_Timestamp (Project) /=
             Get_Timestamp (Get_LI (Source))
           or else Get_DB_Dir (Get_Project (Get_LI (Source)))
             /= Dir_Name (Get_LI_Filename (Get_LI (Source))).all
         then
            Parse_File (Handler, Project, Source);
         end if;

         return Source;
      end Load_File;

      Other_File_Name     : VFS.Virtual_File;
      Source, Other_File  : Source_File;
      Project             : Project_Type;
      pragma Unreferenced (Other_File);

   begin
      --  Do nothing if the database doesn't exist or hasn't been parsed
      if not Is_Open (Handler.SN_Table (FIL)) then
         Open_DB_Files (Handler);
      end if;

      Trace (Me, "Get_Source_Info " & Full_Name (Source_Filename).all);

      Project := Get_Project_From_File (Handler.Registry, Source_Filename);
      Source := Load_File (Source_Filename, Project);

      if Source /= null then
         Other_File_Name := Create
           (Other_File_Base_Name (Project, Source_Filename),
            Handler.Registry);
         Trace (Me, "Get_Source_Info " & Full_Name (Other_File_Name).all);

         if Other_File_Name /= Source_Filename then
            Other_File := Load_File (Other_File_Name);
         end if;
      end if;

      return Source;
   end Get_Source_Info;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   function Case_Insensitive_Identifiers
     (Handler : access CPP_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Case_Insensitive_Identifiers;

   -----------------------------
   -- On_Project_View_Changed --
   -----------------------------

   procedure On_Project_View_Changed
     (Handler : access Entities.LI_Handler_Record'Class) is
   begin
      if Is_Open (CPP_Handler (Handler).SN_Table (FIL)) then
         Close_DB_Files (CPP_Handler (Handler));
      end if;
   end On_Project_View_Changed;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   function Parse_All_LI_Information
     (Handler   : access CPP_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return Integer
   is
      DB_Dir  : constant String := Name_As_Directory (SN.Browse.DB_Dir_Name);
      Iter    : Imported_Project_Iterator :=
                  Start (Project, Recursive => Recursive);
      P       : Project_Type;
      Count   : Natural := 0;
      Files   : chars_ptr_array (1 .. 1);
      Table   : DB_File;
      F_Pair  : Pair;
      F_Data  : F_Table;
      Success : Boolean;
      Source  : Source_File;
      pragma Unreferenced (Source);

   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         Files (1) := New_String
           (Name_As_Directory (Object_Path (P, Recursive => False)) & DB_Dir
            & SN.Browse.DB_File_Name & Table_Extension (F));
         DB_API.Open (Table, Files, Success);
         Free (Files (1));

         if Success then
            Set_Cursor_At (Table, Filename => Invalid_String);
            loop
               Get_Pair (Table, Next_By_Key, Result => F_Pair);
               exit when F_Pair = No_Pair;

               Parse_Pair (F_Pair, F_Data);

               Source := Get_Source_Info
                 (Handler,
                  Create
                    (Full_Filename => F_Data.Key
                       (F_Data.File_Name.First .. F_Data.File_Name.Last)));
               Count := Count + 1;
            end loop;

            Release_Cursor (Table);
         end if;

         Next (Iter);

         DB_API.Close (Table, Success);
      end loop;

      return Count;
   end Parse_All_LI_Information;

   ------------------------
   -- Create_CPP_Handler --
   ------------------------

   function Create_CPP_Handler
     (Db       : Entities.Entities_Database;
      Registry : Projects.Registry.Project_Registry) return Entities.LI_Handler
   is
      CPP : constant CPP_Handler := new CPP_Handler_Record;
   begin
      CPP.Db            := Db;
      CPP.Registry      := Registry;
      CPP.DBIMP_Path    := null;
      CPP.CBrowser_Path := null;
      return LI_Handler (CPP);
   end Create_CPP_Handler;

   ------------------------
   -- Database_Timestamp --
   ------------------------

   function Database_Timestamp (Project : Project_Type) return Time is
      DB_Dir       : constant String := Get_DB_Dir (Project);
      TO_File_Name : constant Virtual_File := Create
        (Full_Filename => DB_Dir & SN.Browse.DB_File_Name & ".to");
   begin
      return File_Time_Stamp (TO_File_Name);
   end Database_Timestamp;

   --------------------
   -- Browse_Project --
   --------------------

   procedure Browse_Project
     (Project     : Project_Type;
      Iterator    : in out CPP_Handler_Iterator'Class;
      Errors      : Projects.Error_Report;
      Single_File : VFS.Virtual_File := VFS.No_File)
   is
      DB_Dir       : constant String := Get_DB_Dir (Project);
      TO_Timestamp : constant Time := Database_Timestamp (Project);
      Num_C_Files  : Natural := 0;
      Tmp_File     : File_Type;
      Success      : Boolean;
      Recompute_TO : Boolean := False;

   begin
      --  Prepare the list of files

      Trace (Me, "Computing the C and C++ sources list for "
             & Project_Name (Project));

      Unchecked_Free (Iterator.Current_Files);

      if Single_File = VFS.No_File then
         Iterator.Current_Files := Get_Source_Files
           (Project   => Project,
            Recursive => False);
      else
         Iterator.Current_Files := new File_Array'(1 => Single_File);
      end if;

      Iterator.Current_File := Iterator.Current_Files'First;

      --  If there is at least one source file, make sure the database
      --  directory exists.

      if Iterator.Current_File <= Iterator.Current_Files'Last then
         Success := Create_Directory_If_Not_Exist (DB_Dir);
         if not Success then
            if Errors /= null then
               Errors ("Could not access or create the directory "
                       & DB_Dir);
            end if;
            Iterator.State := Skip_Project;
            return;
         end if;

         --  Create the list of files that need to be analyzed.

         Iterator.List_Filename := new String'(DB_Dir & "gps_list");
         Create (Tmp_File, Out_File, Name => Iterator.List_Filename.all);

      else
         Iterator.State := Skip_Project;
         return;
      end if;

      for F in Iterator.Current_Files'Range loop
         declare
            File           : constant Virtual_File :=
                               Iterator.Current_Files (F);
            Lang           : constant String := Get_Language_From_File
              (Iterator.Lang_Handler, File);
            Xref_File_Name : constant String :=
                               DB_Dir & Base_Name (File) & Xref_Suffix;
         begin
            if Lang = C_String or else Lang = Cpp_String then

               if File_Time_Stamp (Xref_File_Name) <
                 File_Time_Stamp (File)
               then
                  Num_C_Files := Num_C_Files + 1;

                  --  Remove the current xref file if it exists, since
                  --  cbrowser opens it in append mode.

                  if GNAT.OS_Lib.Is_Regular_File (Xref_File_Name) then
                     GNAT.OS_Lib.Delete_File (Xref_File_Name, Success);
                  end if;

                  Put_Line (Tmp_File, "@" & Xref_File_Name);
                  Put_Line (Tmp_File, Full_Name (File, True).all);
                  Recompute_TO := True;

               elsif not Recompute_TO
                 and then File_Time_Stamp (Xref_File_Name) /= TO_Timestamp
               then
                  Recompute_TO := True;
               end if;
            end if;
         end;
      end loop;

      Close (Tmp_File);

      if Num_C_Files > 0 then
         Iterator.State := Analyze_Files;
         Close_DB_Files (Iterator.Handler);
         SN.Browse.Browse
           (File_Name     => Iterator.List_Filename.all,
            DB_Directory  => DB_Dir,
            DBIMP_Path    => Iterator.Handler.DBIMP_Path.all,
            Cbrowser_Path => Iterator.Handler.CBrowser_Path.all,
            PD            => Iterator.PD);
         Iterator.Process_Running := True;

      elsif Recompute_TO then
         Iterator.State := Analyze_Files;

      else
         GNAT.OS_Lib.Delete_File (Iterator.List_Filename.all, Success);
         GNAT.Strings.Free (Iterator.List_Filename);
         Iterator.State := Skip_Project;
      end if;
   end Browse_Project;

   ----------------------------
   -- Generate_LI_For_Source --
   ----------------------------

   function Generate_LI_For_Source
     (Handler      : access CPP_Handler_Record'Class;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project      : Project_Type;
      Errors       : Projects.Error_Report;
      File         : VFS.Virtual_File) return LI_Handler_Iterator'Class
   is
      Iter : CPP_Handler_Iterator;
   begin
      Iter.Handler      := CPP_Handler (Handler);
      Iter.Project      := Project;
      Iter.Prj_Iterator := Start (Project, Recursive => False);
      Iter.Lang_Handler := Language_Handler (Lang_Handler);
      Iter.Do_Dbimp     := False;

      Browse_Project
        (Current (Iter.Prj_Iterator), Iter,
         Errors => Errors, Single_File => File);

      return Iter;
   end Generate_LI_For_Source;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   function Generate_LI_For_Project
     (Handler      : access CPP_Handler_Record;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project      : Projects.Project_Type;
      Errors       : Projects.Error_Report;
      Recursive    : Boolean := False) return LI_Handler_Iterator'Class
   is
      Iter : CPP_Handler_Iterator;
   begin
      Iter.Handler      := CPP_Handler (Handler);
      Iter.Project      := Project;
      Iter.Prj_Iterator := Start (Project, Recursive);
      Iter.Lang_Handler := Language_Handler (Lang_Handler);
      Iter.Do_Dbimp     := True;

      if Current (Iter.Prj_Iterator) /= No_Project then
         Browse_Project (Current (Iter.Prj_Iterator), Iter, Errors);
      else
         Iter.State := Done;
      end if;

      return Iter;
   end Generate_LI_For_Project;

   --------------
   -- Continue --
   --------------

   procedure Continue
     (Iterator : in out CPP_Handler_Iterator;
      Errors   : Projects.Error_Report;
      Finished : out Boolean)
   is
      Process_Alive : Boolean := False;
      Success       : Boolean;
      DB_Dirs       : GNAT.Strings.String_List_Access;

      procedure Next_Project;
      --  Parse the source files for the next project

      ------------------
      -- Next_Project --
      ------------------

      procedure Next_Project is
      begin
         Next (Iterator.Prj_Iterator);

         if Current (Iterator.Prj_Iterator) = No_Project then
            Iterator.State := Done;
         else
            Browse_Project (Current (Iterator.Prj_Iterator), Iterator, Errors);
         end if;
      end Next_Project;

   begin
      Finished := False;

      case Iterator.State is
         when Done =>
            null;

         when Skip_Project =>
            Next_Project;

         when Analyze_Files =>
            if Iterator.Process_Running then
               SN.Browse.Is_Alive (Iterator.PD, Process_Alive);
               if Process_Alive then
                  return;
               end if;
            end if;

            if Iterator.Do_Dbimp then
               Trace (Me, "Starting dbimp on "
                      & Project_Name (Current (Iterator.Prj_Iterator)));
               Iterator.State := Process_Xrefs;

               DB_Dirs := Get_DB_Dirs (Current (Iterator.Prj_Iterator));
               SN.Browse.Generate_Xrefs
                 (DB_Directories => DB_Dirs,
                  DBIMP_Path     => Iterator.Handler.DBIMP_Path.all,
                  Temp_Name      => Iterator.Tmp_Filename,
                  PD             => Iterator.PD);
               Iterator.Process_Running := True;
               GNAT.Strings.Free (DB_Dirs);

            else
               Iterator.Process_Running := False;
               Iterator.State := Done;
            end if;

         when Process_Xrefs =>
            if Iterator.Process_Running then
               SN.Browse.Is_Alive (Iterator.PD, Process_Alive);
               if Process_Alive then
                  return;
               end if;
            end if;

            Iterator.Process_Running := False;
            Trace (Me, "dbimp finished, switching to next project");

            GNAT.OS_Lib.Delete_File (Iterator.Tmp_Filename, Success);
            Next_Project;
      end case;

      if Iterator.State = Done then
         Trace (Me, "Processing is finished");
         Finished := True;
      else
         Finished := False;
      end if;
   end Continue;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out CPP_Handler_Iterator) is
      Success : Boolean;
   begin
      if Iterator.List_Filename /= null then
         Delete_File (Iterator.List_Filename.all, Success);
         GNAT.Strings.Free (Iterator.List_Filename);
      end if;

      Unchecked_Free (Iterator.Current_Files);
   end Destroy;

   ---------------------
   -- Set_Executables --
   ---------------------

   function Set_Executables
     (System_Dir : String;
      Handler    : access Entities.LI_Handler_Record'Class) return String
   is
      H            : constant CPP_Handler := CPP_Handler (Handler);
      Exec_Suffix  : GNAT.Strings.String_Access := Get_Executable_Suffix;
      DBIMP_Dir    : GNAT.Strings.String_Access := Getenv ("DBIMP_PATH");
      CBrowser_Dir : GNAT.Strings.String_Access := Getenv ("CBROWSER_PATH");

   begin
      GNAT.Strings.Free (H.DBIMP_Path);
      GNAT.Strings.Free (H.CBrowser_Path);

      if DBIMP_Dir.all = "" then
         H.DBIMP_Path := new String'
           (Name_As_Directory (System_Dir) & "bin" &
            GNAT.OS_Lib.Directory_Separator & DBIMP & Exec_Suffix.all);
      else
         H.DBIMP_Path := new String'
           (DBIMP_Dir.all & Directory_Separator & DBIMP & Exec_Suffix.all);
      end if;

      if CBrowser_Dir.all = "" then
         H.CBrowser_Path := new String'
           (Name_As_Directory (System_Dir) & "bin"
            & Directory_Separator & CBrowser & Exec_Suffix.all);
      else
         H.CBrowser_Path := new String'
           (CBrowser_Dir.all
            & Directory_Separator & CBrowser & Exec_Suffix.all);
      end if;

      GNAT.Strings.Free (Exec_Suffix);
      GNAT.Strings.Free (DBIMP_Dir);
      GNAT.Strings.Free (CBrowser_Dir);

      if not GNAT.OS_Lib.Is_Regular_File (H.DBIMP_Path.all) then
         GNAT.Strings.Free (H.DBIMP_Path);
         GNAT.Strings.Free (H.CBrowser_Path);
         return DBIMP
           & " not found on the path. C/C++ browsing is not available";
      end if;

      if not GNAT.OS_Lib.Is_Regular_File (H.CBrowser_Path.all) then
         GNAT.Strings.Free (H.DBIMP_Path);
         GNAT.Strings.Free (H.CBrowser_Path);
         return CBrowser
           & " not found on the path. C/C++ browsing is not available";
      end if;

      return "";
   end Set_Executables;

   ------------------------
   -- Time_Stamp_From_DB --
   ------------------------

   function Time_Stamp_From_DB
     (Project   : Project_Type;
      File_Name : VFS.Virtual_File) return Ada.Calendar.Time
   is
      Files   : chars_ptr_array (1 .. 1);
      Table   : DB_File;
      F_Pair  : Pair;
      F_Data  : F_Table;
      DB_Dir  : constant String := Get_DB_Dir (Project);
      Success : Boolean;

      Dur : Duration;

   begin
      Files (1) := New_String
        (DB_Dir & SN.Browse.DB_File_Name & Table_Extension (F));
      DB_API.Open (Table, Files, Success);
      Free (Files (1));

      if Success then
         Success := False;
         Set_Cursor_At (Table, Filename => Full_Name (File_Name).all);

         loop
            Get_Pair (Table, Next_By_Key, Result => F_Pair);

            exit when F_Pair = No_Pair;

            Parse_Pair (F_Pair, F_Data);

            --  In the database, this is created by a call to stat() in
            --  dbutils.c::put_file_db()
            Dur := Duration'Value
              (F_Data.Data
                 (F_Data.Timestamp.First .. F_Data.Timestamp.Last));

            DB_API.Close (Table, Success);
            Release_Cursor (Table);

            --  What we just read is a local time, convert it to GMT for
            --  proper comparison

            return Base_Time + Dur - Duration (UTC_Time_Offset) * 60.0;
         end loop;

         Release_Cursor (Table);
      end if;

      DB_API.Close (Table, Success);
      return VFS.No_Time;
   end Time_Stamp_From_DB;

   ---------------------------
   -- Parse_File_Constructs --
   ---------------------------

   procedure Parse_File_Constructs
     (Handler   : access CPP_Handler_Record;
      Languages : access Abstract_Language_Handler_Record'Class;
      File_Name : VFS.Virtual_File;
      Result    : out Language.Construct_List)
   is
      Project    : constant Project_Type :=
                     Get_Project_From_File (Handler.Registry, File_Name);
      Constructs : Language.Construct_List;
      P          : Pair;
      Sym        : FIL_Table;
      Info       : Construct_Access;
      C          : Construct_Access;
      Finished   : Boolean;
      Source     : Source_File;

   begin
      --  This call is just to check whether we know anything of the file from
      --  the on-disk database. If not loaded into memory yet, the info will
      --  be loaded

      Source := Get_Source_Info
        (Handler         => Handler,
         Source_Filename => File_Name);

      if Source = null
        or else not Is_Up_To_Date (Source)
      then
         declare
            --  This will only call cbrowser, not dbimp. Therefore, we have
            --  partial info in memory. This is not a problem: to regenerate
            --  the info, the user has to select the menu Build/Compute xref
            --  anyway, which will systematically rerun cbrowser and dbimp, and
            --  the info will be complete from then on.
            Iter : LI_Handler_Iterator'Class := Generate_LI_For_Source
              (Handler,
               Lang_Handler => Languages,
               Project     => Project,
               Errors      => null,
               File        => File_Name);
         begin
            loop
               Continue (Iter, null, Finished);
               exit when Finished;
            end loop;
            Destroy (Iter);
         end;

         Source := Get_Source_Info
           (Handler         => Handler,
            Source_Filename => File_Name);
      end if;

      if not Is_Open (Handler.SN_Table (FIL)) then
         Open_DB_Files (Handler);
      end if;

      Set_Cursor
        (Handler.SN_Table (FIL),
         Position    => By_Key,
         Key         => Full_Name (File_Name).all & Field_Sep,
         Exact_Match => False);

      loop
         Get_Pair (Handler.SN_Table (FIL), Next_By_Key, Result => P);
         exit when P = No_Pair;

         Info := null;
         C    := null;

         Parse_Pair (P, Sym);

         --  Build the next construct

         case Sym.Symbol is
            when CL | CON | E | IU | T | TA | UN | GV | IV | LV
               | FD | FU | MA | MD | MI =>
               --  Build the constructs
               --  Use subtype instead ???

               Info := Constructs.Current;
               Constructs.Current := new Construct_Information;
               C := Constructs.Current;
               C.Is_Declaration := False;

               --  Link

               if Constructs.First = null then
                  Constructs.First := Constructs.Current;
               else
                  Constructs.Current.Prev := Info;
                  Constructs.Current.Next := Info.Next;
                  Info.Next               := Constructs.Current;
               end if;

               Constructs.Last := Constructs.Current;

               --  Set name and location, common to all categories

               C.Name := new String'
                 (String (Sym.Key
                            (Sym.Identifier.First .. Sym.Identifier.Last)));

               --  ??? For now, do not set the third field (absolute source
               --  location), since the explorer does not use it and
               --  computing it is not simple.

               C.Sloc_Start := (Sym.Start_Position.Line,
                                Integer (Sym.Start_Position.Column),
                                0);
               C.Sloc_End := (Sym.End_Position.Line,
                              Integer (Sym.End_Position.Column),
                              0);
               C.Sloc_Entity := (Sym.Start_Position.Line,
                                 Integer (Sym.Start_Position.Column),
                                 0);
            when others =>
               null;
         end case;

         --  Set the category

         case Sym.Symbol is
            when CL | TA =>
               --  ??? make the distinction between struct and classes
               --  which category for templates ???

               C.Category := Cat_Structure;
            when CON | GV =>
               C.Category := Cat_Variable;
            when IV | LV =>
               C.Category := Cat_Local_Variable;
            when E | T =>
               C.Category := Cat_Type;
            when IU =>
               C.Category := Cat_Include;
            when UN =>
               C.Category := Cat_Union;
            when FD =>
               C.Category := Cat_Function;
               C.Is_Declaration := True;
            when FU =>
               C.Category := Cat_Function;
            when MA =>
               --  Macros can either be "constants" (#define a 0)
               --  or "functions" (#define f(a) ((a) == 0))
               --  For now, only consider macros with arguments as
               --  pseudo functions.

               if Length (Sym.Types_Of_Arguments) > 0 then
                  C.Category := Cat_Function;
               else
                  C.Category := Cat_Unknown;
               end if;

            when MD =>
               C.Category := Cat_Method;
               C.Is_Declaration := True;
            when MI =>
               C.Category := Cat_Method;
            when others =>
               null;
         end case;

         --  For functions and methods, get the profile
         case Sym.Symbol is
            when FD | FU | MA | MD | MI =>
               --  Generate the profile

               if Length (Sym.Types_Of_Arguments) > 0 then
                  C.Profile := new String'
                    ('('
                     & String (Sym.Data (Sym.Types_Of_Arguments.First ..
                                           Sym.Types_Of_Arguments.Last))
                     & ')');
               end if;

            when others =>
               null;
         end case;
      end loop;

      Result := Constructs;
      Release_Cursor (Handler.SN_Table (FIL));

   exception
      when DB_Error   =>
         Result := Constructs;
      when E : others => Trace (Exception_Handle, E);
   end Parse_File_Constructs;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (LI : access CPP_Handler_Record) return String is
      pragma Unreferenced (LI);
   begin
      return CPP_LI_Handler_Name;
   end Get_Name;

end CPP_Parser;
