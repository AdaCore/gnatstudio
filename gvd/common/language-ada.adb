-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;  use GNAT.Regpat;

with Pixmaps_IDE;  use Pixmaps_IDE;
with String_Utils; use String_Utils;
with Ada_Analyzer; use Ada_Analyzer;
with Basic_Types;  use Basic_Types;

package body Language.Ada is

   Keywords_List : Pattern_Matcher (1292);
   --  The size is hard-coded to save a little bit on the compilation time
   --  for the regular expression (we need to compile the regexp only once).

   --  Make_Entry functions for the explorer.

   function Make_Entry_Subprogram
     (Str      : String;
      Matched  : Match_Array) return String;
   --  Function used to create an entry in the explorer, for subprograms.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Task
     (Str      : String;
      Matched  : Match_Array) return String;
   --  Function used to create an entry in the explorer, for tasks.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Protected
     (Str      : String;
      Matched  : Match_Array) return String;
   --  Function used to create an entry in the explorer, for protected objects
   --  and types.
   --  See the description of Explorer_Categories for more information.

   function Remove_Ada_Comments (Str : String) return String;
   --  Remove all Ada comments from the string (ie from -- to the next end of
   --  line)

   Comment_RE : constant String := "([ \t]*--[^\n]*)?";

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*(procedure|function)\s+" &
        "(\w+)(" & Comment_RE & "\s*|\s*\([^\)]+\)" & Comment_RE & ")\s*" &
        "(return\s+(\w|\.)+\s*)?is\s", Multiple_Lines or Case_Insensitive);

   Package_RE    : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*package[ \t]+((body[ \t]+)?((\w|\.)+))",
        Multiple_Lines or Case_Insensitive);

   Type_Def_RE   : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*(sub)?type[ \t]+(\w+)", Multiple_Lines or Case_Insensitive);

   Task_RE       : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*task[ \t]+((body|type)[ \t]+)?(\w+)",
        Multiple_Lines or Case_Insensitive);

   Protected_RE : aliased Pattern_Matcher :=
     Compile ("^[ \t]*protected[ \t]+((type|body)[ \t]+)?(\w+)",
              Multiple_Lines or Case_Insensitive);

   --  The Specs are not parsed specifically. Instead, all the work is done
   --  while parsing for subprograms, and the function Make_Entry_Subprogram
   --  distinguishes between the two cases.

   Ada_Explorer_Categories : constant Explorer_Categories :=
     ((Category       => Cat_Procedure,
       Regexp         => Subprogram_RE'Access,
       Position_Index => 2,
       Icon           => subprogram_xpm'Access,
       Make_Entry     => Make_Entry_Subprogram'Access),

      (Category       => Cat_Package,
       Regexp         => Package_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => null),

      (Category       => Cat_Type,
       Regexp         => Type_Def_RE'Access,
       Position_Index => 2,
       Icon           => var_xpm'Access,
       Make_Entry     => null),

      (Category       => Cat_Task,
       Regexp         => Task_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Task'Access),

      (Category       => Cat_Protected,
       Regexp         => Protected_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Protected'Access));


   --  ??? Would be nice to specify the list of available cross compilers
   --  using a configuration file

   Ada_Project_Fields : constant Project_Field_Array :=
     (1 => (Attribute_Name  => new String'("compiler_command"),
            Attribute_Index => new String'("ada"),
            Description     => new String'("Ada compiler"),
            Values          => new String_Array'
              (1  => new String'("gnatmake"),
               2  => new String'("powerpc-wrs-vxworks-gnatmake"),
               3  => new String'("powerpc-wrs-vxworksae-gnatmake"),
               4  => new String'("powerpc-elf-gnatmake"),
               5  => new String'("i386-wrs-vxworks-gnatmake"),
               6  => new String'("m68k-wrs-vxworks-gnatmake"),
               7  => new String'("mips-wrs-vxworks-gnatmake"),
               8  => new String'("sparc-wrs-vxworks-gnatmake"),
               9  => new String'("sparc64-wrs-vxworks-gnatmake"),
               10 => new String'("xscale-wrs-vxworks-gnatmake"),
               11 => new String'("powerpc-xcoff-lynxos-gnatmake"),
               12 => new String'("gnaampmake")),
            Editable       => True),
      2 => (Attribute_Name  => new String'("gnatlist"),
            attribute_Index => null,
            Description     => new String'("Gnatls"),
            Values          => new String_Array'
              (1  => new String'("gnatls"),
               2  => new String'("powerpc-wrs-vxworks-gnatls"),
               3  => new String'("powerpc-wrs-vxworksae-gnatls"),
               4  => new String'("powerpc-elf-gnatls"),
               5  => new String'("i386-wrs-vxworks-gnatls"),
               6  => new String'("m68k-wrs-vxworks-gnatls"),
               7  => new String'("mips-wrs-vxworks-gnatls"),
               8  => new String'("sparc-wrs-vxworks-gnatls"),
               9  => new String'("sparc64-wrs-vxworks-gnatls"),
               10 => new String'("xscale-wrs-vxworks-gnatls"),
               11 => new String'("powerpc-xcoff-lynxos-gnatls"),
               12 => new String'("gnaampls")),
            Editable        => True));


   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean
   is
      pragma Unreferenced (Lang);
   begin
      return Str = "boolean"
        or else Str = "integer"
        or else Str = "natural"
        or else Str = "system.address"
        or else Str = "character";
   end Is_Simple_Type;

   ----------------------
   -- Dereference_Name --
   ----------------------

   function Dereference_Name
     (Lang : access Ada_Language; Name : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Name & ".all";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Index : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      --  Simplify the expression by getting rid of unnecessary ".all"

      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".all"
      then
         return Name (Name'First .. Name'Last - 4) & '(' & Index & ')';
      else
         return Name & '(' & Index & ')';
      end if;
   end Array_Item_Name;

   -----------------------
   -- Record_Field_Name --
   -----------------------

   function Record_Field_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Field : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      --  Simplify the expression by getting rid of unnecessary ".all"

      if Name'Length > 4
        and then Name (Name'Last - 3 .. Name'Last) = ".all"
      then
         return Name (Name'First .. Name'Last - 4) & '.' & Field;
      else
         return Name & '.' & Field;
      end if;
   end Record_Field_Name;

   ----------------------
   -- Explorer_Regexps --
   ----------------------

   function Explorer_Regexps
     (Lang : access Ada_Language) return Explorer_Categories
   is
      pragma Unreferenced (Lang);
   begin
      return Ada_Explorer_Categories;
   end Explorer_Regexps;

   -------------------------
   -- Remove_Ada_Comments --
   -------------------------

   function Remove_Ada_Comments (Str : String) return String is
      Result : String (Str'Range);
      Result_Index : Natural := Result'First;
      J : Natural := Str'First;
   begin
      while J <= Str'Last loop
         if Str (J) = '-' and then Str (J + 1) = '-' then
            Skip_To_Char (Str, J, ASCII.LF);
         else
            Result (Result_Index) := Str (J);
            Result_Index := Result_Index + 1;
         end if;

         J := J + 1;
      end loop;

      return Result (Result'First .. Result_Index - 1);
   end Remove_Ada_Comments;

   ---------------------------
   -- Make_Entry_Subprogram --
   ---------------------------

   function Make_Entry_Subprogram
     (Str      : String;
      Matched  : Match_Array) return String is
   begin
      if Matched (3) = No_Match then
         if Matched (4) = No_Match then
            return Str (Matched (2).First .. Matched (2).Last);
         else
            return Str (Matched (2).First .. Matched (2).Last)
              & " "
              & Reduce (Remove_Ada_Comments
                        (Str (Matched (4).First .. Matched (4).Last)));
         end if;

      elsif Matched (4) = No_Match then
         return
           Str (Matched (2).First .. Matched (2).Last) & ' ' & Reduce
           (Remove_Ada_Comments (Str (Matched (3).First .. Matched (3).Last)));

      else
         return
           Str (Matched (2).First .. Matched (2).Last) & ' ' & Reduce
           (Remove_Ada_Comments
            (Str (Matched (3).First .. Matched (3).Last)
             & " "
             & Str (Matched (4).First .. Matched (4).Last)));
      end if;
   end Make_Entry_Subprogram;

   --------------------------
   -- Make_Entry_Protected --
   --------------------------

   function Make_Entry_Protected
     (Str      : String;
      Matched  : Match_Array) return String
   is
      First, Last : Natural;
   begin
      First := Matched (2).First;
      Last := Matched (2).Last;

      if First < Str'First then
         First := Str'First;
      end if;

      return Str (Matched (3).First .. Matched (3).Last) & " (" &
        Reduce (Str (First .. Last)) & ")";
   end Make_Entry_Protected;

   ---------------------
   -- Make_Entry_Task --
   ---------------------

   function Make_Entry_Task
     (Str     : String;
      Matched : Match_Array) return String
   is
      First, Last : Natural;
   begin
      First := Matched (2).First;
      Last := Matched (2).Last;

      if First < Str'First then
         First := Str'First;
      end if;

      return Str (Matched (3).First .. Matched (3).Last) & " (" &
        Reduce (Str (First .. Last)) & ")";
   end Make_Entry_Task;

   --------------------
   -- Is_System_File --
   --------------------

   function Is_System_File
     (Lang      : access Ada_Language;
      File_Name : String) return Boolean
   is
      pragma Unreferenced (Lang);
      Name : constant String := Base_Name (File_Name);
   begin
      return
        (Name'Length > 2
         and then Name (Name'First + 1) = '-'
         and then (Name (Name'First) = 'a'
                   or else Name (Name'First) = 'g'
                   or else Name (Name'First) = 's'
                   or else Name (Name'First) = 'i'))
        or else Name = "gnat.ads"
        or else Name = "ada.ads"
        or else Name = "interfac.ads"
        or else Name = "system.ads";
   end Is_System_File;

   --------------
   -- Keywords --
   --------------

   function Keywords
     (Lang : access Ada_Language) return GNAT.Regpat.Pattern_Matcher
   is
      pragma Unreferenced (Lang);
   begin
      return Keywords_List;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   Ada_Context : aliased Language_Context :=
     (Comment_Start_Length          => 0,
      Comment_End_Length            => 0,
      New_Line_Comment_Start_Length => 2,
      Comment_Start                 => "",
      Comment_End                   => "",
      New_Line_Comment_Start        => "--",
      String_Delimiter              => '"',
      Quote_Character               => ASCII.NUL,
      Constant_Character            => ''',
      Can_Indent                    => True,
      Syntax_Highlighting           => True,
      Case_Sensitive                => False);

   function Get_Language_Context
     (Lang : access Ada_Language) return Language_Context_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Ada_Context'Access;
   end Get_Language_Context;

   ------------------
   -- Comment_Line --
   ------------------

   function Comment_Line
     (Lang : access Ada_Language;
      Line : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      return "--  " & Line;
   end Comment_Line;

   --------------------
   -- Uncomment_Line --
   --------------------

   function Uncomment_Line
     (Lang : access Ada_Language;
      Line : String) return String
   is
      pragma Unreferenced (Lang);
   begin
      --  Skip all the spaces and HT up to the first occurrence of "--",
      --  then do the following transformations
      --  1  |  --  blabla
      --  => |  blabla
      --
      --  2  |  -- blabla
      --  => |  blabla
      --
      --  3  |  --blabla
      --  => |  blabla
      --
      --  We do the first transformation in priority, so that
      --  Uncomment_Line un-does the effects of Comment_Line, and to keep
      --  close to the GNAT coding style.

      for Index in Line'First .. Line'Last - 1 loop
         if Line (Index .. Index + 1) = "--" then
            if Index + 3 <= Line'Last and then
              Line (Index .. Index + 3) = "--  "
            then
               return Line (Line'First .. Index - 1) &
                 Line (Index + 4 .. Line'Last);
            elsif Index + 2 <= Line'Last and then
              Line (Index .. Index + 2) = "-- "
            then
               return Line (Line'First .. Index - 1) &
                 Line (Index + 3 .. Line'Last);
            else
               return Line (Line'First .. Index - 1) &
                 Line (Index + 2 .. Line'Last);
            end if;
         end if;

         exit when not (Line (Index) = ' ' or else Line (Index) = ASCII.HT);
      end loop;

      return Line;
   end Uncomment_Line;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   procedure Parse_Constructs
     (Lang   : access Ada_Language;
      Buffer : String;
      Result : out Construct_List)
   is
      pragma Unreferenced (Lang);
      Constructs : aliased Construct_List;
   begin
      Analyze_Ada_Source
        (Buffer,
         Default_Indent_Parameters,
         Format     => False,
         Constructs => Constructs'Unchecked_Access);
      Result := Constructs;
   end Parse_Constructs;

   --------------------
   -- Parse_Entities --
   --------------------

   procedure Parse_Entities
     (Lang     : access Ada_Language;
      Buffer   : String;
      Callback : Entity_Callback)
   is
      pragma Unreferenced (Lang);
      pragma Suppress (All_Checks);
      --  See comment in Language.C.Parse_Entities
      --  We've never got an exception for an Ada file, but on the other hand,
      --  no checks are required in this procedure anyway.

   begin
      Analyze_Ada_Source
        (Buffer,
         Default_Indent_Parameters,
         Format   => False,
         Callback => Callback);
   end Parse_Entities;

   -------------------
   -- Format_Buffer --
   -------------------

   procedure Format_Buffer
     (Lang          : access Ada_Language;
      Buffer        : String;
      Replace       : Replace_Text_Callback;
      From, To      : Natural := 0;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters)
   is
      pragma Unreferenced (Lang);
   begin
      Analyze_Ada_Source (Buffer, Indent_Params, True, From, To, Replace);
   end Format_Buffer;

   ------------------------
   -- Get_Project_Fields --
   ------------------------

   function Get_Project_Fields
     (Lang : access Ada_Language) return Project_Field_Array
   is
      pragma Unreferenced (Lang);
   begin
      return Ada_Project_Fields;
   end Get_Project_Fields;

begin
   Compile (Keywords_List,
            "^(a(b(ort|s(tract)?)|cce(pt|ss)|l(iased|l)|nd|rray|t)|b"
            & "(egin|ody)|c(ase|onstant)|d(e(clare|l(ay|ta))|igits|o)|"
            & "e(ls(e|if)|n(d|try)|x(ception|it))|f(or|unction)|g(eneric|"
            & "oto)|i[fns]|l(imited|oop)|mod|n(ew|ot|ull)|o(thers|ut|[fr]"
            & ")|p(ackage|r(agma|ivate|o(cedure|tected)))|r(a(ise|nge)|e("
            & "cord|m|names|queue|turn|verse))|s(e(lect|parate)|ubtype)|t"
            & "(a(gged|sk)|erminate|hen|ype)|u(ntil|se)|w(h(en|ile)|ith)|"
            & "xor)\b",
            Case_Insensitive);
end Language.Ada;
