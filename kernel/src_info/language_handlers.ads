------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

--  This package provides the registry for all supported languages in GPS.
--
--  When adding new languages, the following needs to be done:
--    - Register the new language with Register_Language. This is associated
--      with basic syntactic information for highlighting in the source editor
--      and manipulation through the debugger. It is also associated with the
--      LI handler that is responsible for generating the xref information for
--      that language
--
--  The naming scheme for a language is defined through either the project
--  itself, or through a default naming scheme registered for custom languages
--  in GNATCOLL.Projects.Register_Default_Language_Extension.

with GNAT.OS_Lib;

with Entities;
with Language;
with Language.Tree.Database;
with Projects;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;

package Language_Handlers is

   type Language_Handler_Record
     is new Language.Tree.Database.Abstract_Language_Handler_Record
   with private;
   type Language_Handler is access all Language_Handler_Record'Class;

   procedure Create_Handler
     (Handler : out Language_Handler;
      Symbols : not null access GNATCOLL.Symbols.Symbol_Table_Record'Class);
   --  Create a new language handler

   procedure Destroy (Handler : in out Language_Handler);
   --  Free the memory occupied by Handler, and removes all the registered LI
   --  handlers. They are destroyed individually.

   procedure Set_Registry
     (Handler  : access Language_Handler_Record;
      Registry : access Projects.Project_Registry'Class);
   --  Set the top-level project for Handler

   -----------------
   -- LI handlers --
   -----------------
   --  These are the types responsible for generating the xref database for all
   --  the supported language. It is possible that a given handler is
   --  associated with multiple languages.

   function Get_LI_Handler_By_Name
     (Handler : access Language_Handler_Record;
      Name    : String) return Entities.LI_Handler;
   --  Return the LI handler which name is Name.
   --  The name of a LI_Handler is the one returned by Get_Name.
   --  Name is case-sensitive.

   function Get_LI_Handler_From_File
     (Handler         : access Language_Handler_Record;
      Source_Filename : GNATCOLL.VFS.Virtual_File) return Entities.LI_Handler;
   --  Return the LI handler to use for a specific file name.
   --  null is returned if the language is unknown.
   --  Project is the project that contains Source_Filename, or No_Project if
   --  it is unknown.

   function LI_Handlers_Count
     (Handler : access Language_Handler_Record) return Natural;
   --  Return the number of LI handlers known. This count will generally be
   --  different from the number of supported languages, since some LI handlers
   --  will handle multiple languages.

   function Get_Nth_Handler
     (Handler : access Language_Handler_Record;
      Num     : Positive) return Entities.LI_Handler;
   --  Return the handler for the Num-th language.
   --  The first handler is number 1.

   ---------------
   -- Languages --
   ---------------

   overriding function Get_Language_From_File
     (Handler           : access Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return Language.Language_Access;
   --  Find the language of a given file.
   --  The language is guessed either from a specific setup the user has done
   --  for instance through the properties dialog for source editors, or from
   --  the project. If From_Project_Only is True, then only the project
   --  setting is returned.
   --  Return Unknown_Lang if no other language could be found.

   overriding function Get_Tree_Language_From_File
     (Handler           : access Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False)
      return Language.Tree.Database.Tree_Language_Access;
   --  Same as above but returns the tree language

   function Get_Language_From_File
     (Handler           : access Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return String;
   --  Return "" if the language is unknown.
   --  The language is guessed either from a specific setup the user has done
   --  for instance through the properties dialog for source editors, or from
   --  the project.
   --  See the function GPS.Kernel.Properties.Set_Language_From_File for more
   --  info on how to override this language

   function Language_Is_Overriden
     (Handler  : access Language_Handler_Record;
      Filename : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return True if the language for Filename doesn't come from the project,
   --  but from a user setting.

   function Get_Language_By_Name
     (Handler : access Language_Handler_Record;
      Name    : String) return Language.Language_Access;

   procedure Register_Language
     (Handler   : access Language_Handler_Record;
      Lang      : access Language.Language_Root'Class;
      Tree_Lang : access Language.Tree.Database.Tree_Language'Class;
      LI        : Entities.LI_Handler);
   --  Register a language and additional information about it.
   --  LI is the parser that should be used for cross references for this
   --  language, and can be left to null if no cross-reference is available. It
   --  can also be shared among languages.
   --  See also Register_Default_Language_Extension and
   --  Add_Language_Extension.

   function Known_Languages
     (Handler : access Language_Handler_Record;
      Sorted  : Boolean := False) return GNAT.OS_Lib.Argument_List;
   --  Return the (sorted) list of all known languages.
   --  Return value must be freed by the caller.

   function Languages_Count (Handler : access Language_Handler_Record)
      return Natural;
   --  Return the number of languages declared in Handler

   function Get_Nth_Language
     (Handler : access Language_Handler_Record;
      Num     : Positive) return String;
   --  Return the name of the Num-th language.
   --  The first handler is number 1.

private
   type Language_Info is record
      Lang      : Language.Language_Access;
      Tree_Lang : Language.Tree.Database.Tree_Language_Access;
      Handler   : Entities.LI_Handler;
   end record;

   type Language_Info_Array is array (Positive range <>) of Language_Info;
   type Language_Info_Access is access Language_Info_Array;

   type Handler_Info_Array is array (Positive range <>) of Entities.LI_Handler;
   type Handler_Info_Access is access Handler_Info_Array;

   type Language_Handler_Record
     is new Language.Tree.Database.Abstract_Language_Handler_Record
   with record
      Symbols   : GNATCOLL.Symbols.Symbol_Table_Access;
      Languages : Language_Info_Access;
      Handlers  : Handler_Info_Access;
      Registry  : Projects.Project_Registry_Access;
   end record;

end Language_Handlers;
