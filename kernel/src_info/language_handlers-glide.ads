-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

--  See documentation in parent package Language_Handlers.
--
--  When adding new languages, the following needs to be done:
--    - Register a LI handler (for generating the xref database for this
--      language). Since such handlers can be shared by multiple languages,
--      this registration is optional when it has already been done.
--
--    - Register the new language with Register_Language. This is associated
--      with basic syntactic information for highlighting in the source editor
--      and manipulation through the debugger.
--
--    - Register extra information for the language with
--      Add_Language_Info. This registers default extensions for this language,
--      and provide a mapping from this language to the matching LI handler.

with Language;
with Basic_Types;
with Src_Info;
with Projects;
with GNAT.OS_Lib;

package Language_Handlers.Glide is

   type Glide_Language_Handler_Record is new Language_Handler_Record
     with private;
   type Glide_Language_Handler is access all
     Glide_Language_Handler_Record'Class;

   procedure Gtk_New (Handler : out Glide_Language_Handler);
   --  Create a new language handler

   procedure Destroy (Handler : in out Glide_Language_Handler);
   --  Free the memory occupied by Handler, and removes all the registered LI
   --  handlers. They are destroyed individually.

   procedure Set_Registry
     (Handler  : access Glide_Language_Handler_Record;
      Registry : access Projects.Abstract_Registry'Class);
   --  Set the top-level project for Handler.

   -----------------
   -- LI handlers --
   -----------------
   --  These are the types responsible for generating the xref database for all
   --  the supported language. It is possible that a given handler is
   --  associated with multiple languages.

   procedure Register_LI_Handler
     (Handler : access Glide_Language_Handler_Record;
      Name    : String;
      LI      : Src_Info.LI_Handler);
   --  Register a new LI handler that can generate xref.
   --  The Name is used both for retrieval of the handler by
   --  Get_LI_Handler_By_Name, and to print in the console when recomputing the
   --  xref database.

   function Get_LI_Handler_By_Name
     (Handler : access Glide_Language_Handler_Record;
      Name    : String) return Src_Info.LI_Handler;
   --  Return the LI handler which name is Name.
   --  Name is case-sensitive.

   function Get_LI_Name
     (Handler : access Glide_Language_Handler_Record;
      Nth     : Natural) return String;
   --  Return the name of LI.

   function Get_LI_Handler_From_File
     (Handler         : access Glide_Language_Handler_Record;
      Source_Filename : String)
      return Src_Info.LI_Handler;
   --  Return the LI handler to use for a specific file name.
   --  null is returned if the language is unknown.
   --  Project is the project that contains Source_Filename, or No_Project if
   --  it is unknown.

   function LI_Handlers_Count
     (Handler : access Glide_Language_Handler_Record) return Natural;
   --  Return the number of LI handlers known. This count will generally be
   --  different from the number of supported languages, since some LI handlers
   --  will handle multiple languages.

   function Get_Nth_Handler
     (Handler : access Glide_Language_Handler_Record;
      Num     : Positive) return Src_Info.LI_Handler;
   --  Return the handler for the Num-th language.
   --  The first handler is number 1.

   ---------------
   -- Languages --
   ---------------

   function Get_Language_From_File
     (Handler : access Glide_Language_Handler_Record;
      Source_Filename : String) return Language.Language_Access;
   --  Find the language of a given file.
   --  Return Unknown_Lang if no other language could be found.

   function Get_Language_From_File
     (Handler : access Glide_Language_Handler_Record;
      Source_Filename : String) return String;
   --  Return "" if the language is unknown.

   procedure Register_Language
     (Handler : access Glide_Language_Handler_Record;
      Name    : String;
      Lang    : Language.Language_Access);

   function Known_Languages
     (Handler : access Glide_Language_Handler_Record)
      return GNAT.OS_Lib.Argument_List;

   procedure Add_Language_Info
     (Handler             : access Glide_Language_Handler_Record;
      Language_Name       : String;
      LI                  : Src_Info.LI_Handler;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String);
   --  Register some extra information for a specific language.
   --  Nothing is done if Language_Name hasn't been registered first.

   function Languages_Count (Handler : access Glide_Language_Handler_Record)
      return Natural;
   --  Return the number of languages declared in Handler

   function Get_Nth_Language
     (Handler : access Glide_Language_Handler_Record;
      Num     : Positive) return String;
   --  Return the name of the Num-th language.
   --  The first handler is number 1.

private
   type Language_Info is record
      Language_Name : Basic_Types.String_Access;
      Lang          : Language.Language_Access;
      Handler       : Src_Info.LI_Handler;
   end record;

   type Language_Info_Array is array (Positive range <>) of Language_Info;
   type Language_Info_Access is access Language_Info_Array;

   type Handler_Info is record
      Name    : Basic_Types.String_Access;
      Handler : Src_Info.LI_Handler;
   end record;

   type Handler_Info_Array is array (Positive range <>) of Handler_Info;
   type Handler_Info_Access is access Handler_Info_Array;

   type Glide_Language_Handler_Record is new Language_Handler_Record
   with record
      Languages : Language_Info_Access;
      Handlers  : Handler_Info_Access;
      Registry  : Projects.Abstract_Registry_Access;
   end record;

end Language_Handlers.Glide;
