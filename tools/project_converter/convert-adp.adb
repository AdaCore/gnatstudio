with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with HTables;
with Ada.Unchecked_Deallocation;
with Ada.Command_Line;          use Ada.Command_Line;
with Traces;                    use Traces;

package body Convert.Adp is

   Me : constant Debug_Handle := Create ("Convert");

   Gpr_Extension  : constant String := ".gpr";

   type File_Info_Record is record
      Src_Dir_Index : Integer;
      --  Index of the source directory to which the file belongs

      Obj_Dir_Index : Integer;
      --  Index of the object directory that contains the object files for the
      --  file
   end record;
   type File_Info is access File_Info_Record;
   No_File_Info : constant File_Info := null;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Info_Record, File_Info);

   type Header_Num is range 1 .. 5000;

   function Hash is new HTables.Hash (Header_Num);
   --  Subprograms used for the hash table

   package File_Htables is new HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => File_Info,
      Free_Element => Free,
      No_Element => No_File_Info,
      Key        => String,
      Hash       => Hash,
      Equal      => "=");
   use File_Htables;

   type Is_Related_To is array (Natural range <>, Natural range <>) of Boolean;
   --  Whether two directories are related (source dir has object in object
   --  dir). The first index is for source dirs, the second for obj dir.

   type Object_Directory_Info is record
      Project_Num  : Integer := -1;
      --  The number of the project file associated with that directory

      Files        : String_List_Access;
      --  List of object files in that directory
   end record;

   type Object_Directory_Info_Array
     is array (Natural range <>) of Object_Directory_Info;

   procedure Free (X : in out Object_Directory_Info_Array);
   procedure Free (X : in out Object_Directory_Info);
   --  Free the information in X

   procedure Process_List
     (File         : File_Type;
      Gpr_Variable : String;
      List         : String_List);
   --  Create a new GPR attribute in File, with List as its value.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   procedure Process_Switches
     (File          : File_Type;
      Gpr_Package   : String;
      Gpr_Attribute : String;
      Value         : String);
   --  Process a switches attribute, which needs to be splitted in the .gpr
   --  file

   function Get_Attribute_Value
     (Buffer : String_Access; Attr : String) return String;
   --  Return the first value found in Buffer for Attr

   type Source_Line is record
      First, Last : Integer := 1;
   end record;

   function First_Line_Of (Buffer : String) return Source_Line;
   --  Return a pointer to the beginning of Buffer.

   function At_End (Line : Source_Line) return Boolean;
   --  Return True if Line is past the end of the buffer

   procedure Move_To_Next_Line
     (Buffer : String;
      Line   : in out Source_Line;
      Starting_With : String := "");
   --  Move Iter to the next line starting with Starting_With.
   --  On exit, Line.Start points to the first significant character of
   --  the value, not at the beginning of the line.
   --  Last is left on the last character of the line, before ASCII.LF

   function Read_File (Filename : String) return String_Access;
   --  Return the contents of Filename

   function Project_Name
     (Project_Index : Natural; Root_Project_Name : String) return String;
   --  Return the name of the project to use for the Project_Index-th project.
   --  0 is the root project

   function Parse_Main_Units
     (Buffer : String_Access) return String_List_Access;
   --  Get the list of main units for the project

   function Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String) return String_List_Access;
   --  Get all the source directories in a .adp file

   procedure Parse_Source_Dirs
     (Source_Dirs : GNAT.OS_Lib.String_List;
      Files       : in out File_Htables.HTable);
   --  Process all source directories, and find the list of source files.
   --  Result is stored in the Directories/Files parameters

   function Parse_Object_Dirs
     (Buffer    : String_Access;
      Build_Dir : String) return String_List_Access;
   --  Get all the object directories in a .adp file

   procedure Parse_Object_Dirs
     (Object_Dirs     : GNAT.OS_Lib.String_List;
      Related_To      : in out Is_Related_To;
      Directories     : in out Object_Directory_Info_Array;
      Src_Files       : in out File_Htables.HTable;
      Obj_Files_Count : out Natural;
      Spec_Extension  : String;
      Body_Extension  : String);
   --  Process all the object directories, and compute the dependencies between
   --  source dirs and object dirs.

   procedure Process_Obj_File
     (Obj_Dir_Index  : Integer;
      Obj_Filename   : String;
      Related_To     : in out Is_Related_To;
      Src_Files      : in out File_Htables.HTable;
      Spec_Extension : String;
      Body_Extension : String);
   --  Process the object file, linking the object directory and source
   --  directories together

   procedure Generate_Project_Attributes
     (File              : File_Type;
      Is_Root_Project   : Boolean;
      Main_Units        : String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "";
      Spec_Extension    : String := ".ads";
      Body_Extension    : String := ".adb");
   --  Generate all attributes and packages for a project.
   --  If the project is not a root project, some attributes are ignored.

   function Src_Dirs_Have_Unique_Obj_Dir
     (Related_To  : Is_Related_To) return Boolean;
   --  Return true if each source directory has object files in a single
   --  directory.

   procedure Generate_Withs
     (File              : File_Type;
      Related_To        : Is_Related_To;
      Source_Dirs       : String_List;
      Obj_Dirs          : String_List;
      Src_Files         : File_Htables.HTable;
      Object_Dirs       : in out Object_Directory_Info_Array;
      Root_Project_Name : String;
      Current_Project   : Integer;
      All_Source_Dirs   : Boolean := False);
   --  Generate all the withs statements.
   --  All projects import all other projects, since we do not know how
   --  to test the source dependencies.
   --  No statement is added for the project Omit_Project (or Root_Project
   --  if Omit_Project is 0).
   --  If All_Source_Dirs is True, then all the source dirs are the same for
   --  all the projects, and a Source_Files attribute is generated

   procedure Generate_Source_Files_List
     (Output         : File_Type;
      Src_Files      : File_Htables.HTable;
      Obj_Dir_Index  : Integer);
   --  Generate the list of source files for the given Obj_Dir

   function Image (Num : Integer) return String;
   --  Return the image of an integer, with no leading whitespace

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Object_Directory_Info_Array) is
   begin
      for D in X'Range loop
         Free (X (D));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Object_Directory_Info) is
   begin
      Free (X.Files);
   end Free;

   -----------
   -- Image --
   -----------

   function Image (Num : Integer) return String is
      N : constant String := Integer'Image (Num);
   begin
      if N (N'First) = ' ' then
         return N (N'First + 1 .. N'Last);
      else
         return N;
      end if;
   end Image;

   -----------------------
   -- Parse_Source_Dirs --
   -----------------------

   procedure Parse_Source_Dirs
     (Source_Dirs : GNAT.OS_Lib.String_List;
      Files       : in out File_Htables.HTable)
   is
      Dir  : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
   begin
      for Src in Source_Dirs'Range loop
         begin
            Open (Dir, Source_Dirs (Src).all);

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               Trace (Me, "   Push source file: " & File (1 .. Last));
               File_Htables.Set
                 (Files,
                  File (1 .. Last),
                  new File_Info_Record'(Src_Dir_Index => Src,
                                        Obj_Dir_Index => -1));
            end loop;

            Close (Dir);

         exception
            when Directory_Error =>
               --  Ignored
               null;
         end;
      end loop;
   end Parse_Source_Dirs;

   -----------------------
   -- Parse_Source_Dirs --
   -----------------------

   function Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String) return String_List_Access
   is
      List, List2 : String_List_Access;
      Adp_Prefix  : constant String := "src_dir";
      Line        : Source_Line := First_Line_Of (Buffer.all);
   begin
      loop
         Move_To_Next_Line (Buffer.all, Line, Adp_Prefix);
         exit when At_End (Line);

         if List = null then
            List := new String_List (1 .. 1);
         else
            List2 := List;
            List := new String_List (List2'First .. List2'Last + 1);
            List (List2'Range) := List2.all;
            Unchecked_Free (List2);
         end if;

         List (List'Last) := new String'
           (Normalize_Pathname
              (Buffer (Line.First .. Line.Last), Build_Dir));
      end loop;
      return List;
   end Parse_Source_Dirs;

   ----------------------
   -- Process_Obj_File --
   ----------------------

   procedure Process_Obj_File
     (Obj_Dir_Index  : Integer;
      Obj_Filename   : String;
      Related_To     : in out Is_Related_To;
      Src_Files      : in out File_Htables.HTable;
      Spec_Extension : String;
      Body_Extension : String)
   is
      Extension : constant String := File_Extension (Obj_Filename);
      Base      : constant String := Obj_Filename
        (Obj_Filename'First  .. Obj_Filename'Last - Extension'Length);
      Spec_Name : constant String := Base & Spec_Extension;
      Body_Name : constant String := Base & Body_Extension;
      File      : File_Info;
   begin
      Trace (Me, "  Process obj file: " & Obj_Filename);

      File := File_Htables.Get (Src_Files, Spec_Name);
      if File /= No_File_Info then
         File.Obj_Dir_Index := Obj_Dir_Index;
         Related_To (File.Src_Dir_Index, Obj_Dir_Index) := True;
      end if;

      File := File_Htables.Get (Src_Files, Body_Name);
      if File /= No_File_Info then
         File.Obj_Dir_Index := Obj_Dir_Index;
         Related_To (File.Src_Dir_Index, Obj_Dir_Index) := True;
      end if;
   end Process_Obj_File;

   ----------------------
   -- Parse_Main_Units --
   ----------------------

   function Parse_Main_Units
     (Buffer : String_Access) return String_List_Access
   is
      List, List2 : String_List_Access;
      Line : Source_Line := First_Line_Of (Buffer.all);
   begin
      loop
         Move_To_Next_Line (Buffer.all, Line, "main_unit");
         exit when At_End (Line);

         if List = null then
            List := new String_List (1 .. 1);
         else
            List2 := List;
            List := new String_List (List2'First .. List2'Last + 1);
            List (List2'Range) := List2.all;
            Unchecked_Free (List2);
         end if;

         List (List'Last) := new String'(Buffer (Line.First .. Line.Last));
      end loop;
      return List;
   end Parse_Main_Units;

   -----------------------
   -- Parse_Object_Dirs --
   -----------------------

   function Parse_Object_Dirs
     (Buffer    : String_Access;
      Build_Dir : String) return String_List_Access
   is
      List, List2 : String_List_Access;
      Adp_Prefix : constant String := "obj_dir";
      Line : Source_Line := First_Line_Of (Buffer.all);
      Found : Boolean;
   begin
      List := new String_List (1 .. 1);
      List (1) := new String'(Build_Dir);

      loop
         Move_To_Next_Line (Buffer.all, Line, Adp_Prefix);
         exit when At_End (Line);

         declare
            Name : constant String := Normalize_Pathname
              (Buffer (Line.First .. Line.Last), Build_Dir);
         begin
            Found := False;
            for L in List'Range loop
               if List (L).all = Name then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               List2 := List;
               List := new String_List (List2'First .. List2'Last + 1);
               List (List2'Range) := List2.all;
               Unchecked_Free (List2);

               List (List'Last) := new String'(Name);
            end if;
         end;
      end loop;

      return List;
   end Parse_Object_Dirs;

   -----------------------
   -- Parse_Object_Dirs --
   -----------------------

   procedure Parse_Object_Dirs
     (Object_Dirs     : GNAT.OS_Lib.String_List;
      Related_To      : in out Is_Related_To;
      Directories     : in out Object_Directory_Info_Array;
      Src_Files       : in out File_Htables.HTable;
      Obj_Files_Count : out Natural;
      Spec_Extension  : String;
      Body_Extension  : String)
   is
      Tmp  : String_List_Access;
      Info : Object_Directory_Info;
      Dir  : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
   begin
      Obj_Files_Count := 0;

      for Obj in Object_Dirs'Range loop
         begin
            Trace (Me, "Push object directory: " & Object_Dirs (Obj).all);
            Open (Dir, Object_Dirs (Obj).all);

            --  Put this after the open, so that we only count directories
            --  which actually exist.

            Info := (Obj - Object_Dirs'First, null);

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               if (Last > 1 and then File (Last - 1 .. Last) = ".o")
                 or else (Last > 4 and then File (Last - 3 .. Last) = ".ali")
               then
                  if Info.Files = null then
                     Info.Files := new String_List (1 .. 1);
                  else
                     Tmp := Info.Files;
                     Info.Files := new String_List (1 .. Info.Files'Last + 1);
                     Info.Files (Tmp'Range) := Tmp.all;
                     Unchecked_Free (Tmp);
                  end if;

                  Info.Files (Info.Files'Last) :=
                    new String'(File (1 .. Last));

                  Obj_Files_Count := Obj_Files_Count + 1;
                  Process_Obj_File
                    (Obj_Dir_Index  => Obj,
                     Obj_Filename   => File (1 .. Last),
                     Related_To     => Related_To,
                     Src_Files      => Src_Files,
                     Spec_Extension => Spec_Extension,
                     Body_Extension => Body_Extension);
               end if;
            end loop;

            Directories (Obj) := Info;

            Close (Dir);

         exception
            when Directory_Error =>
               --  Ignored, this was legal with .adp files
               null;
         end;
      end loop;
   end Parse_Object_Dirs;

   --------------------------------
   -- Generate_Source_Files_List --
   --------------------------------

   procedure Generate_Source_Files_List
     (Output         : File_Type;
      Src_Files      : File_Htables.HTable;
      Obj_Dir_Index  : Integer)
   is
      Iter  : File_Htables.Iterator;
      Info  : File_Info;
      First : Boolean := True;
   begin
      Put (Output, "   for Source_Files use (");

      Get_First (Src_Files, Iter);
      loop
         Info := Get_Element (Iter);
         exit when Info = No_File_Info;

         if Info.Obj_Dir_Index = Obj_Dir_Index then
            if First then
               First := False;
            else
               Put_Line (Output, ",");
               Put (Output, "      ");
            end if;

            Put (Output, '"' & Get_Key (Iter) & '"');
         end if;

         Get_Next (Src_Files, Iter);
      end loop;

      Put_Line (Output, ");");
   end Generate_Source_Files_List;

   -------------------
   -- First_Line_Of --
   -------------------

   function First_Line_Of (Buffer : String) return Source_Line is
   begin
      return (First => Buffer'First, Last => Buffer'First - 1);
   end First_Line_Of;

   -----------------------
   -- Move_To_Next_Line --
   -----------------------

   procedure Move_To_Next_Line
     (Buffer : String;
      Line   : in out Source_Line;
      Starting_With : String := "")
   is
      Prefix : constant String := Starting_With & '=';
   begin
      --  Move to next character, since Last was left on the first character
      --  before the ASCII.LF
      if Line.Last >= Buffer'First then
         Line.Last := Line.Last + 1;
      end if;

      loop
         Line.First := Line.Last + 1;
         exit when Line.First > Buffer'Last;

         Line.Last := Line.First;
         while Line.Last <= Buffer'Last
           and then Buffer (Line.Last) /= ASCII.LF
         loop
            Line.Last := Line.Last + 1;
         end loop;

         exit when Line.First + Starting_With'Length - 1 < Line.Last
           and then Prefix =
             Buffer (Line.First .. Line.First + Starting_With'Length);
      end loop;

      --  Point to first significant character of the value
      Line.First := Line.First + Starting_With'Length + 1;

      while Line.First <= Line.Last
        and then Buffer (Line.First) = ' '
      loop
         Line.First := Line.First + 1;
      end loop;

      if Line.Last <= Buffer'Last
        and then Buffer (Line.Last) = ASCII.LF
      then
         Line.Last := Line.Last - 1;
      end if;
   end Move_To_Next_Line;

   ------------
   -- At_End --
   ------------

   function At_End (Line : Source_Line) return Boolean is
   begin
      return Line.First > Line.Last;
   end At_End;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Buffer : String_Access; Attr : String) return String
   is
      Line : Source_Line := First_Line_Of (Buffer.all);
   begin
      Move_To_Next_Line (Buffer.all, Line, Attr);
      if At_End (Line) then
         return "";
      else
         return Buffer (Line.First .. Line.Last);
      end if;
   end Get_Attribute_Value;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Filename : String) return String_Access is
      F : File_Descriptor;
      Length : Long_Integer;
      Name_Zero : aliased constant String := Filename & ASCII.NUL;
      Buffer : String_Access;
   begin
      F := Open_Read (Name_Zero'Address, Text);
      if F = Invalid_FD then
         return null;
      end if;

      Length := File_Length (F);
      Buffer := new String (1 .. Natural (Length));
      Length := Long_Integer (Read (F, Buffer.all'Address, Integer (Length)));
      Close (F);
      return Buffer;
   end Read_File;

   ------------------
   -- Process_List --
   ------------------

   procedure Process_List
     (File         : File_Type;
      Gpr_Variable : String;
      List         : String_List) is
   begin
      Put (File, "   for " & Gpr_Variable & " use (");
      for L in List'Range loop
         if L /= List'First then
            Put_Line (File, ",");
            Put (File, "      """);
         else
            Put (File, """");
         end if;

         Put (File, List (L).all & """");
      end loop;

      Put_Line (File, ");");
   end Process_List;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches
     (File          : File_Type;
      Gpr_Package   : String;
      Gpr_Attribute : String;
      Value         : String)
   is
      List  : Argument_List_Access;
   begin
      if Value /= "" then
         New_Line (File);
         Put_Line (File, "   package " & Gpr_Package & " is");
         Put      (File, "      for " & Gpr_Attribute & " use (");

         List := Argument_String_To_List (Value);

         for L in List'Range loop
            if L /= List'First then
               Put_Line (File, ",");
               Put (File, "         """);
            else
               Put (File, """");
            end if;

            Put (File, List (L).all & '"');
         end loop;

         Free (List);

         Put_Line (File, ");");
         Put_Line (File, "   end " & Gpr_Package & ";");
      end if;
   end Process_Switches;

   ---------------------------------
   -- Generate_Project_Attributes --
   ---------------------------------

   procedure Generate_Project_Attributes
     (File              : File_Type;
      Is_Root_Project   : Boolean;
      Main_Units        : String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "";
      Spec_Extension    : String := ".ads";
      Body_Extension    : String := ".adb") is
   begin
      --  ??? Exact test should be whether the file belongs to the project or
      --  not.
      if Main_Units /= null
        and then Main_Units'Length /= 0
      then
         Process_List (File, "Main", Main_Units.all);
      end if;

      if Is_Root_Project then
         Process_Switches
           (File, "Builder", "Default_Switches (""Ada"")", Builder_Switches);
      end if;

      Process_Switches
        (File, "Compiler", "Default_Switches (""Ada"")", Compiler_Switches);

      if Is_Root_Project then
         Process_Switches
           (File, "Binder", "Default_Switches (""Ada"")", Binder_Switches);
         Process_Switches
           (File, "Linker", "Default_Switches (""Ada"")", Linker_Switches);

         if Cross_Prefix /= "" then
            New_Line (File);
            Put_Line (File, "   package Ide is");
            Put_Line (File, "      for Compiler_Command (""Ada"") use """
                      & Cross_Prefix & "gnatmake"";");
            Put_Line (File, "      for Debugger_Command use """
                      & Cross_Prefix & "gdb"";");
            Put_Line (File, "   end Ide;");
         end if;
      end if;

      if Spec_Extension /= ".ads"
        or else Body_Extension /= ".adb"
      then
         New_Line (File);
         Put_Line (File, "   package Naming is");
         Put_Line (File, "      for Specification_Suffix (""Ada"") use """
                   & Spec_Extension & """;");
         Put_Line (File, "      for Implementation_Suffix (""Ada"") use """
                   & Body_Extension & """;");
         Put_Line (File, "   end Naming;");
      end if;
   end Generate_Project_Attributes;

   ----------------------------------
   -- Src_Dirs_Have_Unique_Obj_Dir --
   ----------------------------------

   function Src_Dirs_Have_Unique_Obj_Dir
     (Related_To  : Is_Related_To) return Boolean
   is
      Count : Natural;
   begin
      for Src in Related_To'Range (1) loop
         Count := 0;
         for Obj in Related_To'Range (2) loop
            if Related_To (Src, Obj) then
               Count := Count + 1;
               if Count > 1 then
                  return False;
               end if;
            end if;
         end loop;
      end loop;
      return True;
   end Src_Dirs_Have_Unique_Obj_Dir;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name
     (Project_Index : Natural; Root_Project_Name : String) return String is
   begin
      if Project_Index = 0 then
         return Root_Project_Name;
      else
         return "project" & Image (Project_Index);
      end if;
   end Project_Name;

   --------------------
   -- Generate_Withs --
   --------------------

   procedure Generate_Withs
     (File              : File_Type;
      Related_To        : Is_Related_To;
      Source_Dirs       : String_List;
      Obj_Dirs          : String_List;
      Src_Files         : File_Htables.HTable;
      Object_Dirs       : in out Object_Directory_Info_Array;
      Root_Project_Name : String;
      Current_Project   : Integer;
      All_Source_Dirs   : Boolean := False)
   is
      Current_Dir : Natural;
      First : Boolean;
   begin
      for D in Object_Dirs'Range loop
         --  Have we found the object directory matching our current project ?
         if Object_Dirs (D).Project_Num = Current_Project  then
            Current_Dir := D;
         else
            Put_Line
              (File,
               "limited with """ & Project_Name
                 (Object_Dirs (D).Project_Num, Root_Project_Name) & """;");
         end if;
      end loop;

      Put_Line (File, "project "
                & Project_Name (Current_Project, Root_Project_Name) & " is");
      Put_Line (File,
                "   for Object_Dir use """
                & Obj_Dirs (Current_Dir).all & """;");

      if not All_Source_Dirs then
         Put (File, "   for Source_Dirs use (");
         First := True;
         for Src in Related_To'Range (1) loop
            if Related_To (Src, Current_Dir) then
               if First then
                  Put (File, """");
                  First := False;
               else
                  Put_Line (File, ",");
                  Put (File, "      """);
               end if;

               Put (File, Source_Dirs (Src).all & """");
            end if;
         end loop;

         Put_Line (File, ");");
      else
         Process_List (File, "Source_Dirs", Source_Dirs);
         Generate_Source_Files_List
           (Output        => File,
            Src_Files     => Src_Files,
            Obj_Dir_Index => Current_Dir);
      end if;
   end Generate_Withs;

   ----------------------
   -- Create_Gpr_Files --
   ----------------------

   function Create_Gpr_Files
     (Root_Project_Name : String;
      Output_Dir        : String;
      Source_Dirs       : GNAT.OS_Lib.String_List;
      Object_Dirs       : GNAT.OS_Lib.String_List;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "") return String
   is
      Obj_Dirs        : Object_Directory_Info_Array (Object_Dirs'Range);
      Src_Files       : File_Htables.HTable;
      Obj_Files_Count : Natural;
      File            : File_Type;
      Single_Obj_Dir  : Boolean;
      Related_To      : Is_Related_To (Source_Dirs'Range, Object_Dirs'Range) :=
        (others => (others => False));
   begin
      Parse_Source_Dirs (Source_Dirs, Src_Files);
      Parse_Object_Dirs
        (Object_Dirs,
         Related_To      => Related_To,
         Directories     => Obj_Dirs,
         Src_Files       => Src_Files,
         Obj_Files_Count => Obj_Files_Count,
         Spec_Extension  => Spec_Extension,
         Body_Extension  => Body_Extension);

      if Obj_Files_Count = 0
        or else Object_Dirs'Length = 1
      then
         Trace (Me, "Simple case: obj_files_count=" & Obj_Files_Count'Img);

         --  Ignore all other object directories but the first one.
         --  The other directories do not contain any object file anyway

         Trace
           (Me, "Creating " & Output_Dir & Root_Project_Name & Gpr_Extension);
         Create (File, Out_File,
                 Output_Dir & Root_Project_Name & Gpr_Extension);
         Put_Line (File, "project " & Root_Project_Name & " is");
         Process_List (File, "Source_Dirs", Source_Dirs);
         Put_Line (File, "   for Object_Dir use """
                   & Object_Dirs (Object_Dirs'First).all & """;");
         Generate_Project_Attributes
           (File              => File,
            Is_Root_Project   => True,
            Main_Units        => Main_Units,
            Builder_Switches  => Builder_Switches,
            Compiler_Switches => Compiler_Switches,
            Binder_Switches   => Binder_Switches,
            Linker_Switches   => Linker_Switches,
            Cross_Prefix      => Cross_Prefix,
            Spec_Extension    => Spec_Extension,
            Body_Extension    => Body_Extension);
         Put_Line (File, "end " & Root_Project_Name & ";");
         Close (File);

      else
         Single_Obj_Dir := Src_Dirs_Have_Unique_Obj_Dir (Related_To);
         Trace (Me, "Case: complex case. Source dirs associated with single"
                & " obj dir: " & Boolean'Image (Single_Obj_Dir));

         for P in 0 .. Object_Dirs'Length - 1 loop
            Trace
              (Me, "Creating " & Output_Dir
               & Project_Name (P, Root_Project_Name)
               & Gpr_Extension);
            Create (File, Out_File,
                    Output_Dir
                    & Project_Name (P, Root_Project_Name)
                    & Gpr_Extension);
            Generate_Withs
              (File              => File,
               Related_To        => Related_To,
               Source_Dirs       => Source_Dirs,
               Obj_Dirs          => Object_Dirs,
               Src_Files         => Src_Files,
               Object_Dirs       => Obj_Dirs,
               Root_Project_Name => Root_Project_Name,
               Current_Project      => P,
               All_Source_Dirs   => not Single_Obj_Dir);

            Generate_Project_Attributes
              (File              => File,
               Is_Root_Project   => P = 0,
               Main_Units        => Main_Units,
               Builder_Switches  => Builder_Switches,
               Compiler_Switches => Compiler_Switches,
               Binder_Switches   => Binder_Switches,
               Linker_Switches   => Linker_Switches,
               Cross_Prefix      => Cross_Prefix,
               Spec_Extension    => Spec_Extension,
               Body_Extension    => Body_Extension);

            Put_Line
              (File, "end " & Project_Name (P, Root_Project_Name) & ";");

            Close (File);
         end loop;
      end if;

      Free (Obj_Dirs);
      File_Htables.Reset (Src_Files);
      return Output_Dir & Root_Project_Name & Gpr_Extension;
   end Create_Gpr_Files;

   -----------------------------
   -- Convert_From_Adp_To_Gpr --
   -----------------------------

   procedure Convert_From_Adp_To_Gpr
     (Adp_Filename : String;
      Spec_Extension, Body_Extension : GNAT.OS_Lib.String_Access)
   is
      Buffer : String_Access := Read_File (Adp_Filename);
      Build_Dir : constant String :=
        Normalize_Pathname
          (Get_Attribute_Value (Buffer, "build_dir"),
           Get_Current_Dir);
      Source_Dirs, Object_Dirs, Main_Units : String_List_Access;

   begin
      if Build_Dir = "" then
         Put_Line ("Cannot convert project: build_dir must be defined in");
         Put_Line ("the .adp file");
         Set_Exit_Status (Failure);
         return;
      end if;

      Source_Dirs := Parse_Source_Dirs (Buffer, Build_Dir);
      Object_Dirs := Parse_Object_Dirs (Buffer, Build_Dir);
      Main_Units  := Parse_Main_Units  (Buffer);

      declare
         Tmp : constant String := Create_Gpr_Files
           (Root_Project_Name => Base_Name (Adp_Filename, ".adp"),
            Output_Dir        => Dir_Name (Adp_Filename),
            Source_Dirs       => Source_Dirs.all,
            Object_Dirs       => Object_Dirs.all,
            Spec_Extension    => Spec_Extension.all,
            Body_Extension    => Body_Extension.all,
            Main_Units        => Main_Units,
            Builder_Switches  => Get_Attribute_Value (Buffer, "gnatmake_opt"),
            Compiler_Switches => Get_Attribute_Value (Buffer, "comp_opt"),
            Binder_Switches   => Get_Attribute_Value (Buffer, "bind_opt"),
            Linker_Switches   => Get_Attribute_Value (Buffer, "link_opt"),
            Cross_Prefix      => Get_Attribute_Value (Buffer, "cross_prefix"));
         pragma Unreferenced (Tmp);
      begin
         null;
      end;

      Free (Main_Units);
      Free (Source_Dirs);
      Free (Object_Dirs);
      Free (Buffer);
   end Convert_From_Adp_To_Gpr;
end Convert.Adp;


