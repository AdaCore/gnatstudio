-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

--  See documentation in parent package Language_Handlers

with Language;
with Basic_Types;
with Src_Info;
with Prj;

package Language_Handlers.Glide is

   type Glide_Language_Handler_Record is new Language_Handler_Record
     with private;
   type Glide_Language_Handler is access all
     Glide_Language_Handler_Record'Class;

   procedure Gtk_New (Handler : out Glide_Language_Handler);
   --  Create a new language handler

   procedure Set_Project_View
     (Handler : access Glide_Language_Handler_Record;
      Project_View : Prj.Project_Id);
   --  Set the top-level project for Handler.

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
      return Basic_Types.String_Array;

   procedure Add_Language_Info
     (Handler             : access Glide_Language_Handler_Record;
      Language_Name       : String;
      LI                  : Src_Info.LI_Handler;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String);
   --  Register some extra information for a specific language.
   --  Nothing is done if Language_Name hasn't been registered first.

   function Get_LI_Handler_From_File
     (Handler         : access Glide_Language_Handler_Record;
      Source_Filename : String;
      Project         : Prj.Project_Id := Prj.No_Project)
      return Src_Info.LI_Handler;
   --  Return the LI handler to use for a specific file name.
   --  null is returned if the language is unknown
   --  Raises Unsupported_Language if the language is unknown
   --  Project is the project that contains Source_Filename, or No_Project if
   --  it is unknown.

   function Languages_Count (Handler : access Glide_Language_Handler_Record)
      return Natural;
   --  Return the number of languages declared in Handler

   function Get_Nth_Handler
     (Handler : access Glide_Language_Handler_Record;
      Num     : Positive) return Src_Info.LI_Handler;
   --  Return the handler for the Num-th language.
   --  The first handler is number 1.

   Unsupported_Language : exception;
   --  Raised when a file name can not be associated with one of the handlers.

private
   type Language_Info is record
      Language_Name : Basic_Types.String_Access;
      Lang          : Language.Language_Access;
      Handler       : Src_Info.LI_Handler;
   end record;

   type Language_Info_Array is array (Positive range <>) of Language_Info;
   type Language_Info_Access is access Language_Info_Array;

   type Glide_Language_Handler_Record is new Language_Handler_Record
   with record
      Languages : Language_Info_Access;
      Project_View : Prj.Project_Id;
   end record;

end Language_Handlers.Glide;
