------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with GNATCOLL.Scripts; use GNATCOLL.Scripts;
with GPS.Kernel;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Language.Shell is
   type Shell_Language is new Language_Root with private;
   type Shell_Language_Access is access all Shell_Language;

   procedure Register_Shell_Language
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Instance    : Class_Instance;
      Lang_Name   : String;
      Body_Suffix : String;
      Spec_Suffix : String := "";
      Obj_Suffix  : String := "";
      Indent      : Indentation_Kind := Simple);

   overriding function Keywords
     (Lang : access Shell_Language) return Strings.String_Access;

   overriding function Keywords
     (Lang : access Shell_Language) return GNAT.Expect.Pattern_Matcher_Access;

   overriding function Get_Name (Lang : access Shell_Language) return String;
   --  Return the name of the language

   overriding function Is_Simple_Type
     (Lang : access Shell_Language; Str : String) return Boolean;

   overriding function Keywords
     (Lang : access Shell_Language) return GNAT.Strings.String_List;

   overriding function Dereference_Name
     (Lang : access Shell_Language;
      Name : String) return String;

   overriding function Array_Item_Name
     (Lang  : access Shell_Language;
      Name  : String;
      Index : String) return String;

   overriding function Record_Field_Name
     (Lang  : access Shell_Language;
      Name  : String;
      Field : String) return String;

   overriding function Get_Language_Context
     (Lang : access Shell_Language) return Language_Context_Access;

   overriding
   procedure Parse_Constructs
     (Lang    : access Shell_Language;
      File    : GNATCOLL.VFS.Virtual_File;
      Buffer  : UTF8_String;
      Result  : out Construct_List);

   overriding
   procedure Parse_Entities
     (Lang     : access Shell_Language;
      Buffer   : String;
      Callback : Entity_Callback);

   procedure Setup
     (Kernel : GPS.Kernel.Kernel_Handle);

private
   type Shell_Language is new Language_Root with record
      Object : Class_Instance;
      Name   : Unbounded_String;
   end record;
end Language.Shell;
