-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

--  See documentation in the parent package Language_Handlers.

with Language;
with Basic_Types;
with GNAT.OS_Lib;

package Language_Handlers.GVD is

   type GVD_Language_Handler_Record is
     new Language_Handlers.Language_Handler_Record with private;
   type GVD_Language_Handler is access all GVD_Language_Handler_Record'Class;

   procedure Gtk_New (Handler : out GVD_Language_Handler);
   --  Create a new language handler

   function Get_Language_From_File
     (Handler : access GVD_Language_Handler_Record;
      Source_Filename : VFS.Virtual_File) return Language.Language_Access;

   function Get_Language_From_File
     (Handler : access GVD_Language_Handler_Record;
      Source_Filename : VFS.Virtual_File) return String;

   function Get_Language_By_Name
     (Handler : access GVD_Language_Handler_Record;
      Name    : String) return Language.Language_Access;

   function Known_Languages
     (Handler : access GVD_Language_Handler_Record)
      return GNAT.OS_Lib.Argument_List;

   procedure Register_Language
     (Handler : access GVD_Language_Handler_Record;
      Name    : String;
      Lang    : Language.Language_Access);
   --  Register a new language. There are no associated extension, so this
   --  language will never be detected, until you add some extensions with
   --  Add_File_Extension.

   procedure Add_File_Extension
     (Handler       : access GVD_Language_Handler_Record;
      Language_Name : String;
      Pattern       : String);
   --  Register a new pattern for the specific language. The pattern is added
   --  to the previously registered pattern.
   --  Any file whose name matches Pattern will be associated with the
   --  language..
   --  Pattern follows regular expressions as defined in GNAT.Regpat and do not
   --  have to match the entire file, e.g "\.ads$" to match a file ending with
   --  ".ads".
   --  Nothing is done if the language hasn't been registered first.

   procedure Add_File_Extensions
     (Handler       : access GVD_Language_Handler_Record;
      Language_Name : String;
      Extensions    : String);
   --  Register some new extensions for the language.
   --  Add all the extensions contained in Extensions (separated by semicolons)
   --  for the language.
   --  Extensions do not contain any regexp and are of the form:
   --  ".ads;.adb"

   procedure Reset_File_Extensions
     (Handler : access GVD_Language_Handler_Record);
   --  Remove all registered file extensions. This means that no language will
   --  be automatically detected anymore

private
   type Language_Info is record
      Language_Name : Basic_Types.String_Access;
      Pattern       : Basic_Types.String_Access;
      Lang          : Language.Language_Access;
   end record;

   type Language_Info_Array is array (Positive range <>) of Language_Info;
   type Language_Info_Access is access Language_Info_Array;

   type GVD_Language_Handler_Record is
     new Language_Handlers.Language_Handler_Record with
   record
      Languages : Language_Info_Access;
   end record;

end Language_Handlers.GVD;
