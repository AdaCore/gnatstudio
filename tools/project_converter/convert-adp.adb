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

   type File_Info is record
      Src_Dir : String_Access;
   end record;
   No_File_Info : constant File_Info := (Src_Dir => null);

   type Header_Num is range 1 .. 5000;

   function Equal (F1, F2 : String_Access) return Boolean;
   function Hash (F1 : String_Access) return Header_Num;
   --  Subprograms used for the hash table

   procedure Do_Nothing (X : in out File_Info);
   --  Does nothing

   package File_Htables is new HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => File_Info,
      Free_Element => Do_Nothing,
      No_Element => No_File_Info,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);
   use File_Htables;
   --  Don't care about memory deallocation in this script.

   type Directory_Array is array (Natural range <>) of String_Access;
   type Directory_Array_Access is access all Directory_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Directory_Array, Directory_Array_Access);

   type Directory_Info is record
      Name         : String_Access;
      Related_Dirs : Directory_Array_Access;
      --  For an object directory, this is a list of source directories the
      --  source files of which were compiled in that directory.
      --  For a source directory, this is a list of object directories where
      --  the source files are put.

      Project_Num  : Integer := -1;
      --  The number of the project file associated with that object
      --  directory
   end record;

   No_Directory_Info : constant Directory_Info := (null, null, -1);

   procedure Free (X : in out Directory_Info);
   --  Free the information in X

   package Directory_Htables is new HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Directory_Info,
      Free_Element => Free,
      No_Element => No_Directory_Info,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);
   use Directory_Htables;

   procedure Process_List
     (Gpr_Variable : String;
      List         : String_List);
   --  Check in Buffer all the lines starting with Adp_Prefix, and concatenate
   --  them into the single list variable Gpr_Variable.
   --  If Base_Directory is not "", then the strings read will be converted to
   --  absolute path.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   procedure Process_Switches
     (Gpr_Package, Gpr_Attribute : String; Value : String);
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

   function Parse_Main_Units
     (Buffer : String_Access) return String_List_Access;
   --  Get the list of main units for the project

   function Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String) return String_List_Access;
   --  Get all the source directories in a .adp file

   procedure Parse_Source_Dirs
     (Source_Dirs : GNAT.OS_Lib.String_List;
      Directories : in out Directory_Htables.HTable;
      Files       : in out File_Htables.HTable);
   --  Process all source directories, and find the list of source files.
   --  Result is stored in the Directories/Files parameters

   function Parse_Object_Dirs
     (Buffer    : String_Access;
      Build_Dir : String) return String_List_Access;
   --  Get all the object directories in a .adp file

   procedure Parse_Object_Dirs
     (Object_Dirs     : GNAT.OS_Lib.String_List;
      Directories     : in out Directory_Htables.HTable;
      Source_Dirs     : in out Directory_Htables.HTable;
      Src_Files       : File_Htables.HTable;
      Obj_Files_Count : out Natural;
      Spec_Extension  : String;
      Body_Extension  : String);
   --  Process all the object directories, and compute the dependencies between
   --  source dirs and object dirs.

   procedure Process_Obj_File
     (Obj_Dir        : String_Access;
      Obj_Filename   : String;
      Src_Files      : File_Htables.HTable;
      Source_Dirs,
      Object_Dirs    : in out Directory_Htables.HTable;
      Spec_Extension : String;
      Body_Extension : String);
   --  Process the object file, linking the object directory and source
   --  directories together

   procedure Add_Dir_Link
     (Object_Dirs, Source_Dirs : in out Directory_Htables.HTable;
      Obj_Dir, Src_Dir : String_Access);
   --  Add a link between the two dependencies.
   --  This means that an object file from Obj_Dir has at least one of its
   --  sources in Src_Dir.

   procedure Generate_Root_Project_Attributes
     (Main_Units        : String_List_Access := null;
      Builder_Switches    : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "");
   --  Generate all attributes and packages for the root project

   function Src_Dirs_Have_Unique_Obj_Dir
     (Source_Dirs : Directory_Htables.HTable)
      return Boolean;
   --  Return true if each source directory has object files in a single
   --  directory.

   procedure Generate_Withs
     (Source_Dirs       : String_List;
      Src_Files         : File_Htables.HTable;
      Object_Dirs       : in out Directory_Htables.HTable;
      Root_Project_Name : String;
      Omit_Project      : Integer;
      All_Source_Dirs   : Boolean := False;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Spec_Extension    : String;
      Body_Extension    : String;
      Cross_Prefix      : String := "");
   --  Generate all the withs statements.
   --  All projects import all other projects, since we do not know how
   --  to test the source dependencies.
   --  No statement is added for the project Omit_Project (or Root_Project
   --  if Omit_Project is 0).
   --  If All_Source_Dirs is True, then all the source dirs are the same for
   --  all the projects, and a Source_Files attribute is generated

   procedure Generate_Source_Files_List
     (Src_Files      : File_Htables.HTable;
      Obj_Dir        : String_Access;
      Spec_Extension : String;
      Body_Extension : String);
   --  Generate the list of source files for the given Obj_Dir

   function Image (Num : Integer) return String;
   --  Return the image of an integer, with no leading whitespace

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (X : in out File_Info) is
      pragma Unreferenced (X);
   begin
      null;
   end Do_Nothing;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Directory_Info) is
      pragma Unreferenced (X);
   begin
      --  Trace (Me, "MANU Free " & X.Name.all);
      --  Free (X.Name);
      null;
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

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ----------
   -- Hash --
   ----------

   function Hash (F1 : String_Access) return Header_Num is
      function Hash is new HTables.Hash (Header_Num);
   begin
      return Hash (F1.all);
   end Hash;

   -----------------------
   -- Parse_Source_Dirs --
   -----------------------

   procedure Parse_Source_Dirs
     (Source_Dirs : GNAT.OS_Lib.String_List;
      Directories : in out Directory_Htables.HTable;
      Files       : in out File_Htables.HTable)
   is
      Dir  : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
   begin
      for Src in Source_Dirs'Range loop
         declare
            Info : constant File_Info :=
              (Src_Dir => Source_Dirs (Src));
         begin
            Trace (Me, "Push source dir: " & Info.Src_Dir.all);
            Open (Dir, Source_Dirs (Src).all);
            Directory_Htables.Set
              (Directories, Source_Dirs (Src), (Info.Src_Dir, null, -1));

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               Trace (Me, "   Push source file: " & File (1 .. Last));
               File_Htables.Set
                 (Files, new String'(File (1 .. Last)), Info);
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
      Adp_Prefix : constant String := "src_dir";
      Line : Source_Line := First_Line_Of (Buffer.all);
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

   ------------------
   -- Add_Dir_Link --
   ------------------

   procedure Add_Dir_Link
     (Object_Dirs, Source_Dirs : in out Directory_Htables.HTable;
      Obj_Dir, Src_Dir : String_Access)
   is
      procedure Add_Dependency
        (Dir1 : in out Directory_Info; Dir2 : String_Access);
      --  Add a dependency on Dir2 in Dir1

      --------------------
      -- Add_Dependency --
      --------------------

      procedure Add_Dependency
        (Dir1 : in out Directory_Info; Dir2 : String_Access)
      is
         Found : Boolean := False;
         Old   : Directory_Array_Access;
      begin
         if Dir1.Related_Dirs /= null then
            for D in Dir1.Related_Dirs'Range loop
               if Dir1.Related_Dirs (D).all = Dir2.all then
                  Found := True;
                  exit;
               end if;
            end loop;
         end if;

         if not Found then
            if Dir1.Related_Dirs = null then
               Dir1.Related_Dirs := new Directory_Array'(1 => Dir2);
            else
               Old := Dir1.Related_Dirs;
               Dir1.Related_Dirs := new Directory_Array'(Old.all & Dir2);
               Unchecked_Free (Old);
            end if;
         end if;
      end Add_Dependency;

      Obj_Dir_Info : Directory_Info :=
        Directory_Htables.Get (Object_Dirs, Obj_Dir);
      Src_Dir_Info : Directory_Info :=
        Directory_Htables.Get (Source_Dirs, Src_Dir);
   begin
      Trace (Me, "Add directory dependency: " & Obj_Dir_Info.Name.all
               & " and " & Src_Dir_Info.Name.all);
      Add_Dependency (Obj_Dir_Info, Src_Dir);
      Directory_Htables.Set (Object_Dirs, Obj_Dir_Info.Name, Obj_Dir_Info);
      Add_Dependency (Src_Dir_Info, Obj_Dir);
      Directory_Htables.Set (Source_Dirs, Src_Dir_Info.Name, Src_Dir_Info);
   end Add_Dir_Link;

   ----------------------
   -- Process_Obj_File --
   ----------------------

   procedure Process_Obj_File
     (Obj_Dir      : String_Access;
      Obj_Filename : String;
      Src_Files    : File_Htables.HTable;
      Source_Dirs,
      Object_Dirs  : in out Directory_Htables.HTable;
      Spec_Extension : String;
      Body_Extension : String)
   is
      Extension : constant String := File_Extension (Obj_Filename);
      Base      : constant String := Obj_Filename
        (Obj_Filename'First  .. Obj_Filename'Last - Extension'Length);
      Spec_Name : aliased String := Base & Spec_Extension;
      Body_Name : aliased String := Base & Body_Extension;
      File      : File_Info;
   begin
      Trace (Me, "  Process obj file: " & Obj_Filename);

      File := File_Htables.Get (Src_Files, Spec_Name'Unchecked_Access);
      if File /= No_File_Info then
         Trace (Me, "     Found spec in " & File.Src_Dir.all);
         Add_Dir_Link (Source_Dirs => Source_Dirs,
                       Object_Dirs => Object_Dirs,
                       Obj_Dir     => Obj_Dir,
                       Src_Dir     => File.Src_Dir);
      end if;

      File := File_Htables.Get (Src_Files, Body_Name'Unchecked_Access);
      if File /= No_File_Info then
         Trace (Me, "     Found body in " & File.Src_Dir.all);
         Add_Dir_Link (Source_Dirs => Source_Dirs,
                       Object_Dirs => Object_Dirs,
                       Obj_Dir     => Obj_Dir,
                       Src_Dir     => File.Src_Dir);
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
      Directories     : in out Directory_Htables.HTable;
      Source_Dirs     : in out Directory_Htables.HTable;
      Src_Files       : File_Htables.HTable;
      Obj_Files_Count : out Natural;
      Spec_Extension  : String;
      Body_Extension  : String) is
   begin
      Obj_Files_Count := 0;

      for Obj in Object_Dirs'Range loop
         declare
            Obj_Dir : constant String_Access :=
              new String'(Object_Dirs (Obj).all);
            --  Obj_Dir mustn't be freed since it is stored in a htable
            Dir  : Dir_Type;
            File : String (1 .. 1024);
            Last : Natural;
         begin
            Trace (Me, "Push object directory: " & Obj_Dir.all);
            Open (Dir, Obj_Dir.all);

            --  Put this after the open, so that we only count directories
            --  which actually exist
            Directory_Htables.Set
              (Directories, Obj_Dir, (Obj_Dir, null, Obj - Object_Dirs'First));

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               if (Last > 1 and then File (Last - 1 .. Last) = ".o")
                 or else (Last > 4 and then File (Last - 3 .. Last) = ".ali")
               then
                  Obj_Files_Count := Obj_Files_Count + 1;
                  Process_Obj_File
                    (Obj_Dir        => Obj_Dir,
                     Obj_Filename   => File (1 .. Last),
                     Src_Files      => Src_Files,
                     Source_Dirs    => Source_Dirs,
                     Object_Dirs    => Directories,
                     Spec_Extension => Spec_Extension,
                     Body_Extension => Body_Extension);
               end if;
            end loop;

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
     (Src_Files      : File_Htables.HTable;
      Obj_Dir        : String_Access;
      Spec_Extension : String;
      Body_Extension : String)
   is
      Dir   : Dir_Type;
      File  : String (1 .. 1024);
      Last  : Natural;
      First : Boolean := True;
   begin
      Put ("   for Source_Files use (");

      --  ??? Should avoid rereading the directory, and avoid duplicates when
      --  both .o and .ali are found.
      Open (Dir, Obj_Dir.all);

      loop
         Read (Dir, File, Last);
         exit when Last = 0;

         if (Last > 1 and then File (Last - 1 .. Last) = ".o")
           or else (Last > 4 and then File (Last - 3 .. Last) = ".ali")
         then
            declare
               Extension : constant String :=
                 File_Extension (File (1 .. Last));
               Base      : constant String := File
                 (File'First  .. Last - Extension'Length);
               Spec_Name : aliased String := Base & Spec_Extension;
               Body_Name : aliased String := Base & Body_Extension;
            begin
               if File_Htables.Get
                 (Src_Files, Spec_Name'Unchecked_Access) /= No_File_Info
               then
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                     Put ("      ");
                  end if;

                  Put ('"' & Spec_Name & '"');
               end if;

               if File_Htables.Get
                 (Src_Files, Body_Name'Unchecked_Access) /= No_File_Info
               then
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                     Put ("      ");
                  end if;

                  Put ('"' & Body_Name & '"');
               end if;
            end;
         end if;
      end loop;

      Put_Line (");");

      Close (Dir);

   exception
      when Directory_Error =>
         --  Ignored, this was legal with .adp files
         null;
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
     (Gpr_Variable : String;
      List         : String_List) is
   begin
      Put ("   for " & Gpr_Variable & " use (");
      for L in List'Range loop
         if L /= List'First then
            Put_Line (",");
            Put ("      """);
         else
            Put ("""");
         end if;

         Put (List (L).all & """");
      end loop;

      Put_Line (");");
   end Process_List;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches
     (Gpr_Package, Gpr_Attribute : String; Value : String)
   is
      List  : Argument_List_Access;
   begin
      if Value /= "" then
         New_Line;
         Put_Line ("   package " & Gpr_Package & " is");
         Put      ("      for " & Gpr_Attribute & " use (");

         List := Argument_String_To_List (Value);

         for L in List'Range loop
            if L /= List'First then
               Put_Line (",");
               Put ("         """);
            else
               Put ("""");
            end if;

            Put (List (L).all & '"');
         end loop;

         Free (List);

         Put_Line (");");
         Put_Line ("   end " & Gpr_Package & ";");
      end if;
   end Process_Switches;

   --------------------------------------
   -- Generate_Root_Project_Attributes --
   --------------------------------------

   procedure Generate_Root_Project_Attributes
     (Main_Units        : String_List_Access := null;
      Builder_Switches   : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "")
   is
   begin
      if Main_Units /= null
        and then Main_Units'Length /= 0
      then
         Process_List ("Main", Main_Units.all);
      end if;

      Process_Switches
        ("Builder", "Default_Switches (""Ada"")", Builder_Switches);
      Process_Switches
        ("Compiler", "Default_Switches (""Ada"")", Compiler_Switches);
      Process_Switches
        ("Binder", "Default_Switches (""Ada"")", Binder_Switches);
      Process_Switches
        ("Linker", "Default_Switches (""Ada"")", Linker_Switches);

      if Cross_Prefix /= "" then
         New_Line;
         Put_Line ("   package Ide is");
         Put_Line ("      for Compiler_Command (""Ada"") use """
                   & Cross_Prefix & "gnatmake"";");
         Put_Line ("      for Debugger_Command use """
                   & Cross_Prefix & "gdb"";");
         Put_Line ("   end Ide;");
      end if;
   end Generate_Root_Project_Attributes;

   ----------------------------------
   -- Src_Dirs_Have_Unique_Obj_Dir --
   ----------------------------------

   function Src_Dirs_Have_Unique_Obj_Dir
     (Source_Dirs : Directory_Htables.HTable)
      return Boolean
   is
      Iter : Directory_Htables.Iterator;
      Dir  : Directory_Info;
   begin
      Directory_Htables.Get_First (Source_Dirs, Iter);

      loop
         Dir := Get_Element (Iter);
         exit when Dir = No_Directory_Info;

         if Dir.Related_Dirs /= null
           and then Dir.Related_Dirs'Length > 1
         then
            return False;
         end if;

         Directory_Htables.Get_Next (Source_Dirs, Iter);
      end loop;

      return True;
   end Src_Dirs_Have_Unique_Obj_Dir;

   --------------------
   -- Generate_Withs --
   --------------------

   procedure Generate_Withs
     (Source_Dirs       : String_List;
      Src_Files         : File_Htables.HTable;
      Object_Dirs       : in out Directory_Htables.HTable;
      Root_Project_Name : String;
      Omit_Project      : Integer;
      All_Source_Dirs   : Boolean := False;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Spec_Extension    : String;
      Body_Extension    : String;
      Cross_Prefix      : String := "")
   is
      Dir : Directory_Info;
      Iter : Directory_Htables.Iterator;
      Current_Dir : Directory_Info := No_Directory_Info;
   begin
      Get_First (Object_Dirs, Iter);
      loop
         Dir := Get_Element (Iter);
         exit when Dir = No_Directory_Info;

         Trace (Me, "MANU Dir.Name=" & Dir.Name.all
                & " Project_Num=" & Dir.Project_Num'Img
                & " Omit_Project=" & Omit_Project'Img);

         --  Have we found the object directory matching our current project ?
         if Dir.Project_Num = Omit_Project  then
            Current_Dir := Dir;
         else
            --  Are we generating a with for the root project or another one ?
            if Dir.Project_Num /= 0 then
               Put_Line
                 ("limited with ""project" & Image (Dir.Project_Num) & """;");
            else
               Put_Line ("limited with """ & Root_Project_Name & """;");
            end if;
         end if;

         Get_Next (Object_Dirs, Iter);
      end loop;

      if Omit_Project = 0 then
         Put_Line ("project " & Root_Project_Name & " is");
      else
         Put_Line ("project project" & Image (Omit_Project) & " is");
      end if;

      if not All_Source_Dirs then
         Put ("   for Source_Dirs use (");

         Trace (Me, "Directory has related dirs "
                & Boolean'Image (Current_Dir.Related_Dirs /= null));

         if Current_Dir.Related_Dirs /= null then
            for S in Current_Dir.Related_Dirs'Range loop
               if S /= Current_Dir.Related_Dirs'First then
                  Put_Line (",");
                  Put_Line ("      ");
               end if;

               Put ('"' & Current_Dir.Related_Dirs (S).all & '"');
            end loop;
         end if;

         Put_Line (");");

      else
         Process_List ("Source_Dirs", Source_Dirs);
      end if;

      Put_Line ("   for Object_Dir use """
                & Current_Dir.Name.all & """;");

      if All_Source_Dirs then
         Generate_Source_Files_List
           (Src_Files, Current_Dir.Name,
            Spec_Extension => Spec_Extension,
            Body_Extension => Body_Extension);
      end if;

      if Omit_Project = 0 then
         Generate_Root_Project_Attributes
           (Main_Units        => Main_Units,
            Builder_Switches  => Builder_Switches,
            Compiler_Switches => Compiler_Switches,
            Binder_Switches   => Binder_Switches,
            Linker_Switches   => Linker_Switches,
            Cross_Prefix      => Cross_Prefix);
      end if;

      if Spec_Extension /= ".ads"
        or else Body_Extension /= ".adb"
      then
         New_Line;
         Put_Line ("   package Naming is");
         Put_Line ("      for Specification_Suffix (""Ada"") use """
                   & Spec_Extension & """;");
         Put_Line ("      for Implementation_Suffix (""Ada"") use """
                   & Body_Extension & """;");
         Put_Line ("   end Naming;");
      end if;

      if Omit_Project = 0 then
         Put_Line ("end " & Root_Project_Name & ";");
      else
         Put_Line ("end project" & Image (Omit_Project) & ";");
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
      Src_Dirs, Obj_Dirs : Directory_Htables.HTable;
      Src_Files          : File_Htables.HTable;
      Obj_Files_Count    : Natural;
      File               : File_Type;
   begin
      Parse_Source_Dirs (Source_Dirs, Src_Dirs, Src_Files);
      Parse_Object_Dirs
        (Object_Dirs,
         Directories     => Obj_Dirs,
         Source_Dirs     => Src_Dirs,
         Src_Files       => Src_Files,
         Obj_Files_Count => Obj_Files_Count,
         Spec_Extension  => Spec_Extension,
         Body_Extension  => Body_Extension);


      if Obj_Files_Count = 0 then
         Trace (Me, "Simple case: no object file found");
         --  Ignore all other object directories but the first one.
         --  The other directories do not contain any object file anyway

         Trace
           (Me, "Creating " & Output_Dir & Root_Project_Name & Gpr_Extension);
         Create (File, Out_File,
                 Output_Dir & Root_Project_Name & Gpr_Extension);
         Set_Output (File);
         Put_Line ("project " & Root_Project_Name & " is");
         Process_List ("Source_Dirs", Source_Dirs);
         Put_Line ("   for Object_Dir use """
                   & Object_Dirs (Object_Dirs'First).all & """;");
         Generate_Root_Project_Attributes
           (Main_Units        => Main_Units,
            Builder_Switches  => Builder_Switches,
            Compiler_Switches => Compiler_Switches,
            Binder_Switches   => Binder_Switches,
            Linker_Switches   => Linker_Switches,
            Cross_Prefix      => Cross_Prefix);
         Put_Line ("end " & Root_Project_Name & ";");
         Close (File);

      elsif Object_Dirs'Length = 1 then
         Trace (Me, "Simple case: A single object directory");
         --  No other object directory other than Build_Dir

         Trace
           (Me, "Creating " & Output_Dir & Root_Project_Name & Gpr_Extension);
         Create (File, Out_File,
                 Output_Dir & Root_Project_Name & Gpr_Extension);
         Set_Output (File);
         Put_Line ("project " & Root_Project_Name & " is");
         Process_List ("Source_Dirs", Source_Dirs);
         Put_Line ("   for Object_Dir use """
                   & Object_Dirs (Object_Dirs'First).all & """;");
         Generate_Root_Project_Attributes
           (Main_Units        => Main_Units,
            Builder_Switches  => Builder_Switches,
            Compiler_Switches => Compiler_Switches,
            Binder_Switches   => Binder_Switches,
            Linker_Switches   => Linker_Switches,
            Cross_Prefix      => Cross_Prefix);
         Put_Line ("end " & Root_Project_Name & ";");
         Close (File);

      elsif Src_Dirs_Have_Unique_Obj_Dir (Src_Dirs) then
         Trace (Me, "Case: Source dirs associated with a single object dir");
         --  Associations are unique. Thus, we can generate a set of
         --  independent project files

         for P in 0 .. Object_Dirs'Length - 1 loop
            if P = 0 then
               Trace
                 (Me, "Creating " & Output_Dir & Root_Project_Name &
                  Gpr_Extension);
               Create (File, Out_File,
                       Output_Dir & Root_Project_Name & Gpr_Extension);
            else
               Trace
                 (Me, "Creating " & Output_Dir & "project" & Image (P) &
                  Gpr_Extension);
               Create (File, Out_File,
                       Output_Dir & "project" & Image (P) & Gpr_Extension);
            end if;
            Set_Output (File);
            Generate_Withs
              (Source_Dirs       => Source_Dirs,
               Src_Files         => Src_Files,
               Object_Dirs       => Obj_Dirs,
               Root_Project_Name => Root_Project_Name,
               Omit_Project      => P,
               All_Source_Dirs   => False,
               Main_Units        => Main_Units,
               Builder_Switches  => Builder_Switches,
               Compiler_Switches => Compiler_Switches,
               Binder_Switches   => Binder_Switches,
               Linker_Switches   => Linker_Switches,
               Spec_Extension    => Spec_Extension,
               Body_Extension    => Body_Extension,
               Cross_Prefix      => Cross_Prefix);
            Close (File);
         end loop;

      else
         Trace (Me, "Case: complex case");
         for P in 0 .. Object_Dirs'Length - 1 loop
            if P = 0 then
               Trace
                 (Me, "Creating " & Output_Dir & Root_Project_Name &
                  Gpr_Extension);
               Create (File, Out_File,
                       Output_Dir & Root_Project_Name & Gpr_Extension);
            else
               Trace
                 (Me, "Creating " & Output_Dir & "project" & Image (P) &
                  Gpr_Extension);
               Create (File, Out_File,
                       Output_Dir & "project" & Image (P) & Gpr_Extension);
            end if;
            Set_Output (File);
            Generate_Withs
              (Source_Dirs       => Source_Dirs,
               Src_Files         => Src_Files,
               Object_Dirs       => Obj_Dirs,
               Root_Project_Name => Root_Project_Name,

               Omit_Project      => P,
               All_Source_Dirs   => True,
               Main_Units        => Main_Units,
               Builder_Switches  => Builder_Switches,
               Compiler_Switches => Compiler_Switches,
               Binder_Switches   => Binder_Switches,
               Linker_Switches   => Linker_Switches,
               Spec_Extension    => Spec_Extension,
               Body_Extension    => Body_Extension,
               Cross_Prefix      => Cross_Prefix);
            Close (File);
         end loop;
      end if;

      Directory_Htables.Reset (Src_Dirs);
      Directory_Htables.Reset (Obj_Dirs);
      File_Htables.Reset (Src_Files);
      return "";
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


