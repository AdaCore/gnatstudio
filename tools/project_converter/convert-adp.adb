with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.HTable;               use GNAT.HTable;
with Ada.Unchecked_Deallocation;

package body Convert.Adp is

   Spec_Extension, Body_Extension : String_Access;
   Gpr_Extension  : constant String := ".gpr";

   type File_Info is record
      Src_Dir : String_Access;
   end record;
   No_File_Info : constant File_Info := (Src_Dir => null);

   type Header_Num is range 1 .. 5000;

   function Equal (F1, F2 : String_Access) return Boolean;
   function Hash (F1 : String_Access) return Header_Num;
   --  Subprograms used for the hash table

   package File_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => File_Info,
      No_Element => No_File_Info,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);
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

      Project_Num  : Natural := 0;
      --  The number of the project file associated with that object
      --  directory
   end record;

   No_Directory_Info : constant Directory_Info := (null, null, 0);

   package Source_Dirs_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Directory_Info,
      No_Element => No_Directory_Info,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);
   package Object_Dirs_Htable is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Directory_Info,
      No_Element => No_Directory_Info,
      Key        => String_Access,
      Hash       => Hash,
      Equal      => Equal);

   procedure Process_List
     (Buffer, Adp_Prefix, Gpr_Variable : String;
      Base_Directory : String := "");
   --  Check in Buffer all the lines starting with Adp_Prefix, and concatenate
   --  them into the single list variable Gpr_Variable.
   --  If Base_Directory is not "", then the strings read will be converted to
   --  absolute path.

   procedure Convert_Simple_Attribute
     (Adp_Value, Gpr_Attribute : String;
      Base_Directory : String := "");
   --  Convert an attribute with a string value.
   --  If Base_Directory is not "", then the strings read will be converted to
   --  absolute path.
   --  Adp_Value is the value of the attribute, not its name.

   procedure Process_Switches
     (Buffer : String_Access;
      Adp_Prefix, Gpr_Package, Gpr_Attribute : String);
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

   procedure Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String);
   --  Parse all source directories specified in Buffer, and store the
   --  file information in the File_Htable

   procedure Parse_Object_Dirs
     (Buffer          : String_Access;
      Build_Dir       : String;
     Obj_Files_Count : out Integer);
   --  Process all object directories, and return the number of object
   --  files found.

   function Count_Object_Directories (Build_Dir : String) return Integer;
   --  return the number of non-empty object directories that are
   --  different from Build_Dir

   procedure Process_Obj_File
     (Obj_Dir : String_Access; Obj_Filename : String);
   --  Process the object file, linking the object directory and source
   --  directories together

   procedure Add_Dir_Link (Obj_Dir, Src_Dir : String_Access);
   --  Add a link between the two dependencies.
   --  This means that an object file from Obj_Dir has at least one of its
   --  sources in Src_Dir.

   procedure Generate_Root_Project_Attributes (Buffer : String_Access);
   --  Generate all attributes and packages for the root project

   function Src_Dirs_Have_Unique_Obj_Dir return Boolean;
   --  Return true if each source directory has object files in a single
   --  directory.

   procedure Generate_Withs
     (Buffer            : String_Access;
      Root_Project_Name : String;
      Build_Dir         : String;
      Omit_Project      : Integer;
      All_Source_Dirs   : Boolean := False);
   --  Generate all the withs statements.
   --  All projects import all other projects, since we do not know how
   --  to test the source dependencies.
   --  No statement is added for the project Omit_Project (or Root_Project
   --  if Omit_Project is 0).
   --  If All_Source_Dirs is True, then all the source dirs are the same for
   --  all the projects, and a Source_Files attribute is generated

   procedure Generate_Source_Files_List
     (Obj_Dir : String_Access);
   --  Generate the list of source files for the given Obj_Dir

   function Image (Num : Integer) return String;
   --  Return the image of an integer, with no leading whitespace

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
      function Hash is new GNAT.HTable.Hash (Header_Num);
   begin
      return Hash (F1.all);
   end Hash;

   -----------------------
   -- Parse_Source_Dirs --
   -----------------------

   procedure Parse_Source_Dirs
     (Buffer : String_Access; Build_Dir : String)
   is
      Adp_Prefix : constant String := "src_dir";
      Line : Source_Line := First_Line_Of (Buffer.all);
      Dir  : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
   begin
      loop
         Move_To_Next_Line (Buffer.all, Line, Adp_Prefix);
         exit when At_End (Line);

         declare
            Dir_Name : constant String_Access := new String'
              (Normalize_Pathname
                 (Buffer (Line.First .. Line.Last), Build_Dir));
            Info : constant File_Info := (Src_Dir => Dir_Name);
            --  ??? Src_Dir is never freed, we don't really care

         begin
            Open (Dir, Dir_Name.all);

            Source_Dirs_Htable.Set (Dir_Name, (Dir_Name, null, 0));

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               File_Htable.Set (new String'(File (1 .. Last)), Info);
            end loop;

            Close (Dir);

         exception
            when Directory_Error =>
               --  Ignored, this was legal with .adp files
               null;
         end;
      end loop;
   end Parse_Source_Dirs;

   ------------------------------
   -- Count_Object_Directories --
   ------------------------------

   function Count_Object_Directories (Build_Dir : String) return Integer is
      Count : Natural := 0;
      Dir   : Directory_Info := Object_Dirs_Htable.Get_First;
   begin
      while Dir /= No_Directory_Info loop
         pragma Debug
           (Put_Line ("Dir= " & Dir.Name.all & " Build=" & Build_Dir));
         if Dir.Name.all /= Build_Dir then
            Count := Count + 1;
         end if;

         Dir := Object_Dirs_Htable.Get_Next;
      end loop;

      return Count;
   end Count_Object_Directories;

   ------------------
   -- Add_Dir_Link --
   ------------------

   procedure Add_Dir_Link (Obj_Dir, Src_Dir : String_Access) is
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


      Obj_Dir_Info : Directory_Info := Object_Dirs_Htable.Get (Obj_Dir);
      Src_Dir_Info : Directory_Info := Source_Dirs_Htable.Get (Src_Dir);
   begin
      Add_Dependency (Obj_Dir_Info, Src_Dir);
      Object_Dirs_Htable.Set (Obj_Dir_Info.Name, Obj_Dir_Info);
      Add_Dependency (Src_Dir_Info, Obj_Dir);
      Source_Dirs_Htable.Set (Src_Dir_Info.Name, Src_Dir_Info);
   end Add_Dir_Link;

   ----------------------
   -- Process_Obj_File --
   ----------------------

   procedure Process_Obj_File
     (Obj_Dir : String_Access; Obj_Filename : String)
   is
      Extension : constant String := File_Extension (Obj_Filename);
      Base      : constant String := Obj_Filename
        (Obj_Filename'First  .. Obj_Filename'Last - Extension'Length);
      Spec_Name : String_Access := new String'(Base & Spec_Extension.all);
      Body_Name : String_Access := new String'(Base & Body_Extension.all);
      File      : File_Info;
   begin
      File := File_Htable.Get (Spec_Name);
      if File /= No_File_Info then
         Add_Dir_Link (Obj_Dir, File.Src_Dir);
      end if;

      File := File_Htable.Get (Body_Name);
      if File /= No_File_Info then
         Add_Dir_Link (Obj_Dir, File.Src_Dir);
      end if;

      Free (Spec_Name);
      Free (Body_Name);
   end Process_Obj_File;

   -----------------------
   -- Parse_Object_Dirs --
   -----------------------

   procedure Parse_Object_Dirs
     (Buffer          : String_Access;
      Build_Dir       : String;
     Obj_Files_Count : out Integer)
   is
      procedure Process_Directory (Directory_Name : String);
      --  Process a single directory

      -----------------------
      -- Process_Directory --
      -----------------------

      procedure Process_Directory (Directory_Name : String) is
         Dir_Name : constant String := Normalize_Pathname
           (Directory_Name, Build_Dir);
         Obj_Dir : constant String_Access := new String'(Dir_Name);
         --  Obj_Dir mustn't be freed since it is stored in a htable
         Dir  : Dir_Type;
         File : String (1 .. 1024);
         Last : Natural;
      begin
         Open (Dir, Dir_Name);

         --  Put this after the open, so that we only count directories
         --  which actually exist
         Object_Dirs_Htable.Set (Obj_Dir, (Obj_Dir, null, 0));

         loop
            Read (Dir, File, Last);
            exit when Last = 0;

            if (Last > 1 and then File (Last - 1 .. Last) = ".o")
              or else (Last > 4 and then File (Last - 3 .. Last) = ".ali")
            then
               Obj_Files_Count := Obj_Files_Count + 1;
               Process_Obj_File (Obj_Dir, File (1 .. Last));
            end if;
         end loop;

         Close (Dir);

      exception
         when Directory_Error =>
            --  Ignored, this was legal with .adp files
            null;
      end Process_Directory;

      Adp_Prefix : constant String := "obj_dir";
      Line : Source_Line := First_Line_Of (Buffer.all);
   begin
      Obj_Files_Count := 0;

      Process_Directory (Build_Dir);

      loop
         Move_To_Next_Line (Buffer.all, Line, Adp_Prefix);
         exit when At_End (Line);

         Process_Directory (Buffer (Line.First .. Line.Last));
      end loop;
   end Parse_Object_Dirs;

   --------------------------------
   -- Generate_Source_Files_List --
   --------------------------------

   procedure Generate_Source_Files_List
     (Obj_Dir : String_Access)
   is
      Dir   : Dir_Type;
      File  : String (1 .. 1024);
      Last  : Natural;
      First : Boolean := True;
   begin
      Put ("   for Source_Files use (");

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
               Spec_Name : String_Access :=
                 new String'(Base & Spec_Extension.all);
               Body_Name : String_Access :=
                 new String'(Base & Body_Extension.all);
            begin
               if File_Htable.Get (Spec_Name) /= No_File_Info then
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                     Put ("      ");
                  end if;

                  Put ('"' & Spec_Name.all & '"');
               end if;

               if File_Htable.Get (Body_Name) /= No_File_Info then
                  if First then
                     First := False;
                  else
                     Put_Line (",");
                     Put ("      ");
                  end if;

                  Put ('"' & Body_Name.all & '"');
               end if;

               Free (Spec_Name);
               Free (Body_Name);
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
     (Buffer, Adp_Prefix, Gpr_Variable : String;
      Base_Directory : String := "")
   is
      Line : Source_Line := First_Line_Of (Buffer);
      Is_First : Boolean := True;
   begin
      Put ("   for " & Gpr_Variable & " use (");
      loop
         Move_To_Next_Line (Buffer, Line, Adp_Prefix);
         exit when At_End (Line);

         if not Is_First then
            Put_Line (",");
            Put ("      """);
         else
            Is_First := False;
            Put ("""");
         end if;

         declare
            Value : constant String := Buffer (Line.First .. Line.Last);
         begin
            if Base_Directory = "" then
               Put (Value);
            else
               Put (Format_Pathname
                      (Normalize_Pathname (Value, Base_Directory)));
            end if;
         end;
         Put ("""");
      end loop;

      Put_Line (");");
   end Process_List;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches
     (Buffer : String_Access;
      Adp_Prefix, Gpr_Package, Gpr_Attribute : String)
   is
      Value : constant String := Get_Attribute_Value (Buffer, Adp_Prefix);
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

   ------------------------------
   -- Convert_Simple_Attribute --
   ------------------------------

   procedure Convert_Simple_Attribute
     (Adp_Value, Gpr_Attribute : String;
      Base_Directory : String := "") is
   begin
      if Adp_Value /= "" then
         Put ("   for " & Gpr_Attribute & " use """);

         if Base_Directory = "" then
            Put (Adp_Value);
         else
            Put (Format_Pathname
                   (Normalize_Pathname (Adp_Value, Base_Directory)));
         end if;

         Put_Line (""";");
      end if;
   end Convert_Simple_Attribute;

   --------------------------------------
   -- Generate_Root_Project_Attributes --
   --------------------------------------

   procedure Generate_Root_Project_Attributes (Buffer : String_Access) is
      Cross_Prefix : constant String :=
        Get_Attribute_Value (Buffer, "cross_prefix");
   begin
      Process_List (Buffer.all, "main_unit", "Main");
      Process_Switches
        (Buffer, "gnatmake_opt", "Builder", "Default_Switches (""Ada"")");
      Process_Switches
        (Buffer, "comp_opt", "Compiler", "Default_Switches (""Ada"")");
      Process_Switches
        (Buffer, "bind_opt", "Binder", "Default_Switches (""Ada"")");
      Process_Switches
        (Buffer, "link_opt", "Linker", "Default_Switches (""Ada"")");

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

   function Src_Dirs_Have_Unique_Obj_Dir return Boolean is
      Dir : Directory_Info := Source_Dirs_Htable.Get_First;
   begin
      while Dir /= No_Directory_Info loop
         if Dir.Related_Dirs /= null
           and then Dir.Related_Dirs'Length > 1
         then
            return False;
         end if;

         Dir := Source_Dirs_Htable.Get_Next;
      end loop;

      return True;
   end Src_Dirs_Have_Unique_Obj_Dir;

   --------------------
   -- Generate_Withs --
   --------------------

   procedure Generate_Withs
     (Buffer            : String_Access;
      Root_Project_Name : String;
      Build_Dir         : String;
      Omit_Project      : Integer;
      All_Source_Dirs   : Boolean := False)
   is
      Dir : Directory_Info := Object_Dirs_Htable.Get_First;
      Current_Dir : Directory_Info;
      Project_Num : Natural := 1;
   begin
      while Dir /= No_Directory_Info loop
         if Dir.Project_Num = 0
           and then Dir.Name.all /= Build_Dir
         then
            Dir.Project_Num := Project_Num;
            Project_Num := Project_Num + 1;

            Object_Dirs_Htable.Set (Dir.Name, Dir);
         end if;

         if Dir.Project_Num = Omit_Project then
            Current_Dir := Dir;
         end if;

         if Dir.Project_Num /= Omit_Project then
            if Dir.Project_Num /= 0 then
               Put_Line
                 ("limited with ""project" & Image (Dir.Project_Num) & """;");
            else
               Put_Line ("limited with """ & Root_Project_Name & """;");
            end if;
         end if;

         Dir := Object_Dirs_Htable.Get_Next;
      end loop;

      if Omit_Project = 0 then
         Put_Line ("project " & Root_Project_Name & " is");
      else
         Put_Line ("project project" & Image (Omit_Project) & " is");
      end if;

      if not All_Source_Dirs then
         Put ("   for Source_Dirs use (");

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
         Process_List (Buffer.all, "src_dir", "Source_Dirs", Build_Dir);
      end if;

      Put_Line ("   for Object_Dir use """
                & Current_Dir.Name.all & """;");

      if All_Source_Dirs then
         Generate_Source_Files_List (Current_Dir.Name);
      end if;

      if Omit_Project = 0 then
         Generate_Root_Project_Attributes (Buffer);
         Put_Line ("end " & Root_Project_Name & ";");
      else
         Put_Line ("end project" & Image (Omit_Project) & ";");
      end if;
   end Generate_Withs;

   -----------------------------
   -- Convert_From_Adp_To_Gpr --
   -----------------------------

   procedure Convert_From_Adp_To_Gpr
     (Adp_Filename : String;
      Spec_Extension, Body_Extension : GNAT.OS_Lib.String_Access)
   is
      Buffer : String_Access := Read_File (Adp_Filename);
      Short_Name : constant String := Base_Name (Adp_Filename, ".adp");
      Build_Dir : constant String :=
        Normalize_Pathname
          (Get_Attribute_Value (Buffer, "build_dir"),
           Get_Current_Dir);
      Object_Files_Count, Object_Dirs_Count : Integer;
      File : File_Type;

   begin
      Convert.Adp.Spec_Extension := Spec_Extension;
      Convert.Adp.Body_Extension := Body_Extension;

      if Build_Dir = "" then
         Put_Line ("Cannot convert project: build_dir must be defined in");
         Put_Line ("the .adp file");
         return;
      end if;

      Parse_Source_Dirs (Buffer, Build_Dir);
      Parse_Object_Dirs (Buffer, Build_Dir, Object_Files_Count);
      Object_Dirs_Count := Count_Object_Directories (Build_Dir);

      if Object_Files_Count = 0 then
         --  Ignore all other object directories but Build_Dir
         --  The other directories do not contain any object file anyway

         Create (File, Out_File, Short_Name & Gpr_Extension);
         Set_Output (File);
         Put_Line ("project " & Short_Name & " is");
         Process_List (Buffer.all, "src_dir", "Source_Dirs", Build_Dir);
         Convert_Simple_Attribute (Build_Dir, "Object_Dir");
         Generate_Root_Project_Attributes (Buffer);
         Put_Line ("end " & Short_Name & ";");
         Close (File);

      elsif Object_Dirs_Count = 0 then
         --  No other object directory other than Build_Dir

         Create (File, Out_File, Short_Name & Gpr_Extension);
         Set_Output (File);
         Put_Line ("project " & Short_Name & " is");
         Process_List (Buffer.all, "src_dir", "Source_Dirs", Build_Dir);
         Convert_Simple_Attribute (Build_Dir, "Object_Dir");
         Generate_Root_Project_Attributes (Buffer);
         Put_Line ("end " & Short_Name & ";");
         Close (File);

      elsif Src_Dirs_Have_Unique_Obj_Dir then
         --  Associations are unique. Thus, we can generate a set of
         --  independent project files

         for P in 0 .. Object_Dirs_Count loop
            if P = 0 then
               Create (File, Out_File, Short_Name & Gpr_Extension);
            else
               Create (File, Out_File, "project" & Image (P) & Gpr_Extension);
            end if;
            Set_Output (File);
            Generate_Withs (Buffer, Short_Name, Build_Dir, Omit_Project => P);
            Close (File);
         end loop;

      else
         for P in 0 .. Object_Dirs_Count loop
            if P = 0 then
               Create (File, Out_File, Short_Name & Gpr_Extension);
            else
               Create (File, Out_File, "project" & Image (P) & Gpr_Extension);
            end if;
            Set_Output (File);
            Generate_Withs
              (Buffer, Short_Name, Build_Dir, Omit_Project => P,
               All_Source_Dirs => True);
            Close (File);
         end loop;
      end if;

      Free (Buffer);
   end Convert_From_Adp_To_Gpr;
end Convert.Adp;


