-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                  Copyright (C) 2000-2007, AdaCore                 --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This is the general Ada (non debugger specific) support package.
--  See language.ads for a complete spec.

with Ada_Analyzer;

package Language.Ada is

   type Ada_Language is new Language_Root with private;

   Ada_Lang : constant Language_Access;
   --  Class constant for the Ada language

   -------------
   -- Parsing --
   -------------

   function Is_Simple_Type
     (Lang : access Ada_Language; Str : String) return Boolean;

   function Keywords
     (Lang : access Ada_Language) return Strings.String_Access;

   function Keywords
     (Lang : access Ada_Language) return Pattern_Matcher_Access;

   function Get_Language_Context
     (Lang : access Ada_Language) return Language_Context_Access;

   --------------
   -- Explorer --
   --------------

   function Explorer_Regexps
     (Lang : access Ada_Language) return Explorer_Categories;

   function Is_System_File
     (Lang : access Ada_Language; File_Name : String) return Boolean;

   ------------------------
   -- Naming conventions --
   ------------------------

   function Dereference_Name
     (Lang : access Ada_Language;
      Name : String) return String;

   function Array_Item_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Index : String) return String;

   function Record_Field_Name
     (Lang  : access Ada_Language;
      Name  : String;
      Field : String) return String;

   ---------------------
   -- Project support --
   ---------------------

   function Get_Project_Fields
     (Lang : access Ada_Language) return Project_Field_Array;

   ----------------------
   -- Source Analyzing --
   ----------------------

   procedure Parse_Constructs
     (Lang   : access Ada_Language;
      Buffer : String;
      Result : out Construct_List);

   procedure Format_Buffer
     (Lang            : access Ada_Language;
      Buffer          : String;
      Replace         : Replace_Text_Callback;
      From, To        : Natural := 0;
      Indent_Params   : Indent_Parameters := Default_Indent_Parameters;
      Indent_Offset   : Natural := 0;
      Case_Exceptions : Case_Handling.Casing_Exceptions :=
        Case_Handling.No_Casing_Exception);

   procedure Parse_Entities
     (Lang     : access Ada_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   overriding
   procedure Get_Referenced_Entity
     (Lang       : access Ada_Language;
      Buffer     : String;
      Construct  : Simple_Construct_Information;
      Sloc_Start : out Source_Location;
      Sloc_End   : out Source_Location;
      Success    : out Boolean;
      From_Index : Natural := 0);
   --  See inherited documentation

   Ada_Abstract_Attribute  : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Abstract_Attribute;
   Ada_Access_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Access_Attribute;
   Ada_Aliased_Attribute   : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Aliased_Attribute;
   Ada_Array_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Array_Attribute;
   Ada_Assign_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Assign_Attribute;
   Ada_Constant_Attribute  : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Constant_Attribute;
   Ada_Delta_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Delta_Attribute;
   Ada_Digits_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Digits_Attribute;
   Ada_In_Attribute        : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_In_Attribute;
   Ada_Interface_Attribute : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Interface_Attribute;
   Ada_Mod_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Mod_Attribute;
   Ada_New_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_New_Attribute;
   Ada_Not_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Not_Attribute;
   Ada_Null_Attribute      : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Null_Attribute;
   Ada_Out_Attribute       : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Out_Attribute;
   Ada_Range_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Range_Attribute;
   Ada_Record_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Record_Attribute;
   Ada_Tagged_Attribute    : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Tagged_Attribute;
   Ada_Class_Attribute     : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Class_Attribute;
   Ada_Renames_Attribute   : constant Construct_Att_Key :=
                               Ada_Analyzer.Ada_Renames_Attribute;

   Ada_Library_Visibility_Attribute : constant Construct_Att_Key :=
                                        Language.Ada.Ada_Renames_Attribute + 1;
   --  This attribute is not set by the parser itself - but external
   --  process will need to compute & store this value

   function Get_Name (Lang : access Ada_Language) return String;

private
   type Ada_Language is new Language_Root with null record;
   function Comment_Line
     (Lang    : access Ada_Language;
      Line    : String;
      Comment : Boolean := True;
      Clean   : Boolean := False) return String;

   Ada_Lang : constant Language_Access := new Ada_Language;

end Language.Ada;
