-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for subprograms.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Package
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for packages.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Type
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for types.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Task
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for tasks.
   --  See the description of Explorer_Categories for more information.

   function Make_Entry_Protected
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String;
   --  Function used to create an entry in the explorer, for protected objects
   --  and types.
   --  See the description of Explorer_Categories for more information.

   Subprogram_RE : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*(procedure|function)\s+" &
        "(\w+)(\s*|\s*\([^\)]+\))\s*" &
        "(return\s+(\w|\.)+\s*)?(is\s|;)", Multiple_Lines);

   Package_RE    : aliased Pattern_Matcher :=
     Compile
       ("^[ \t]*package[ \t]+((body[ \t]+)?((\w|\.)+))", Multiple_Lines);

   Type_Def_RE   : aliased Pattern_Matcher :=
     Compile ("^[ \t]*(sub)?type[ \t]+(\w+)", Multiple_Lines);

   Task_RE       : aliased Pattern_Matcher :=
     Compile ("^[ \t]*task[ \t]+((body|type)[ \t]+)?(\w+)", Multiple_Lines);

   Protected_RE : aliased Pattern_Matcher :=
     Compile ("^[ \t]*protected[ \t]+((type|body)[ \t]+)?(\w+)",
              Multiple_Lines);

   --  The Specs are not parsed specifically. Instead, all the work is done
   --  while parsing for subprograms, and the function Make_Entry_Subprogram
   --  distinguishes between the two cases.

   Ada_Explorer_Categories : constant Explorer_Categories :=
     ((Name           => new String' ("Subprograms"),
       Regexp         => Subprogram_RE'Access,
       Position_Index => 2,
       Icon           => subprogram_xpm'Access,
       Make_Entry     => Make_Entry_Subprogram'Access),

      (Name           => new String' ("Specs"),
       Regexp         => Subprogram_RE'Access,
       Position_Index => 2,
       Icon           => subprogram_xpm'Access,
       Make_Entry     => null),

      (Name           => new String' ("Packages"),
       Regexp         => Package_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Package'Access),

      (Name           => new String' ("Types"),
       Regexp         => Type_Def_RE'Access,
       Position_Index => 2,
       Icon           => var_xpm'Access,
       Make_Entry     => Make_Entry_Type'Access),

      (Name           => new String' ("Tasks"),
       Regexp         => Task_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Task'Access),

      (Name           => new String' ("Protected"),
       Regexp         => Protected_RE'Access,
       Position_Index => 3,
       Icon           => package_xpm'Access,
       Make_Entry     => Make_Entry_Protected'Access));

   --------------------
   -- Is_Simple_Type --
   --------------------

   function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean is
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
     (Lang : access Ada_Language; Name : String) return String is
   begin
      return Name & ".all";
   end Dereference_Name;

   ---------------------
   -- Array_Item_Name --
   ---------------------

   function Array_Item_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Index : String) return String is
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
      Field : String) return String is
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
     (Lang : access Ada_Language) return Explorer_Categories is
   begin
      return Ada_Explorer_Categories;
   end Explorer_Regexps;

   ---------------------------
   -- Make_Entry_Subprogram --
   ---------------------------

   function Make_Entry_Subprogram
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String is
   begin
      if Str (Matched (6).First) = ';' then
         Category.all := 2;  --  specs
      end if;

      if Matched (4) = No_Match then
         return Str (Matched (2).First .. Matched (2).Last);
      else
         return
           Str (Matched (2).First .. Matched (2).Last) & ' ' &
                Reduce (Str (Matched (3).First .. Matched (3).Last));
      end if;
   end Make_Entry_Subprogram;

   ------------------------
   -- Make_Entry_Package --
   ------------------------

   function Make_Entry_Package
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String is
   begin
      return Str (Matched (3).First .. Matched (3).Last);
   end Make_Entry_Package;

   ---------------------
   -- Make_Entry_Type --
   ---------------------

   function Make_Entry_Type
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String is
   begin
      return Str (Matched (2).First .. Matched (2).Last);
   end Make_Entry_Type;

   --------------------------
   -- Make_Entry_Protected --
   --------------------------

   function Make_Entry_Protected
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String
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
     (Str      : String;
      Matched  : Match_Array;
      Category : access Category_Index) return String
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
     (Lang : access Ada_Language;
      File_Name : String) return Boolean
   is
      Name : constant String := Base_File_Name (File_Name);
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
     (Lang : access Ada_Language) return GNAT.Regpat.Pattern_Matcher is
   begin
      return Keywords_List;
   end Keywords;

   --------------------------
   -- Get_Language_Context --
   --------------------------

   function Get_Language_Context
     (Lang : access Ada_Language) return Language_Context is
   begin
      return (Comment_Start_Length          => 0,
              Comment_End_Length            => 0,
              New_Line_Comment_Start_Length => 2,
              Comment_Start                 => "",
              Comment_End                   => "",
              New_Line_Comment_Start        => "--",
              String_Delimiter              => '"',
              Quote_Character               => ASCII.NUL,
              Constant_Character            => ''');
   end Get_Language_Context;

   -------------------
   -- Format_Source --
   -------------------

   procedure Format_Source
     (Lang             : access Ada_Language;
      Buffer           : String;
      Indent_Params    : Indent_Parameters := Default_Indent_Parameters;
      Reserved_Casing  : Casing_Type       := Lower;
      Ident_Casing     : Casing_Type       := Mixed;
      Format_Operators : Boolean           := True)
   is
      New_Buffer : Extended_Line_Buffer;
      Ignore     : Natural;

   begin
      New_Buffer := To_Line_Buffer (Buffer);
      Analyze_Ada_Source
        (To_Unchecked_String (Buffer'Address), Buffer'Length,
         New_Buffer, Indent_Params,
         Reserved_Casing, Ident_Casing, Format_Operators,
         Current_Indent => Ignore,
         Prev_Indent   => Ignore);
      Print (New_Buffer);
      Free (New_Buffer);
   end Format_Source;

   ----------------------
   -- Parse_Constructs --
   ----------------------

   procedure Parse_Constructs
     (Lang            : access Ada_Language;
      Buffer          : Interfaces.C.Strings.chars_ptr;
      Buffer_Length   : Natural;
      Result          : out Construct_List;
      Indent          : out Natural;
      Next_Indent     : out Natural;
      Indent_Params   : Indent_Parameters := Default_Indent_Parameters)
   is
      New_Buffer : Extended_Line_Buffer;
      Constructs : aliased Construct_List;

   begin
      Analyze_Ada_Source
        (To_Unchecked_String (Buffer), Buffer_Length,
         New_Buffer, Indent_Params,
         Reserved_Casing  => Unchanged,
         Ident_Casing     => Unchanged,
         Format_Operators => False,
         Indent           => False,
         Constructs       => Constructs'Unchecked_Access,
         Current_Indent   => Next_Indent,
         Prev_Indent      => Indent);
      Result := Constructs;
   end Parse_Constructs;

   ----------------------
   -- Next_Indentation --
   ----------------------

   procedure Next_Indentation
     (Lang          : access Ada_Language;
      Buffer        : Interfaces.C.Strings.chars_ptr;
      Buffer_Length : Natural;
      Indent        : out Natural;
      Next_Indent   : out Natural;
      Indent_Params : Indent_Parameters := Default_Indent_Parameters)
   is
      New_Buffer : Extended_Line_Buffer;
   begin
      Analyze_Ada_Source
        (To_Unchecked_String (Buffer), Buffer_Length,
         New_Buffer, Indent_Params,
         Reserved_Casing  => Unchanged,
         Ident_Casing     => Unchanged,
         Format_Operators => False,
         Indent           => False,
         Current_Indent   => Next_Indent,
         Prev_Indent      => Indent);
   end Next_Indentation;

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
