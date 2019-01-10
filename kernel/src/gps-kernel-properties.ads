------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

--  This package provides file-specific properties, optionaly persistent
--
--  Example of use:
--     declare
--        Int   : Integer_Property;
--        Found : Boolean;
--        Prop  : Property_Access;
--     begin
--        Get_Property (Int, File, "dummy", Found);
--        if Found then
--           Put_Line (Int.Value'Img);
--        end if;
--
--        Prop := new Integer_Property'(Value => 232);
--        Set_Property (File, "dummy", Prop, Persistent => True);
--     end;

with GNATCOLL.Projects;
with GPS.Properties;     use GPS.Properties;

package GPS.Kernel.Properties is

   -------------------------------------------
   -- Associating properties with any index --
   -------------------------------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key        : String;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False);
   --  Associate a given property with Key/Name, so that it can be queried
   --  later through Get_File_Property.
   --  If Persistent is True, the property will be preserved from one
   --  session of GPS to the next.
   --  Property names are case sensitive.

   procedure Remove_Property
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Key    : String;
      Name   : String);
   --  Remove the named property (persistent or not) associated with Key/Name.

   ---------------------------------------
   -- Associating properties with files --
   ---------------------------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      File       : GNATCOLL.VFS.Virtual_File;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False);
   --  Associate a given property with File, so that it can be queries later
   --  through Get_File_Property.
   --  If Persistent is True, the property will be preserved from one session
   --  of GPS to the next.
   --  Property names are case sensitive.

   procedure Remove_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      File     : GNATCOLL.VFS.Virtual_File;
      Name     : String);
   --  Remove the named property (persistent or not) from the file.

   ------------------------------------------
   -- Associating properties with projects --
   ------------------------------------------

   procedure Set_Property
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project    : GNATCOLL.Projects.Project_Type;
      Name       : String;
      Property   : access Property_Record'Class;
      Persistent : Boolean := False);
   --  Associate a given property with File, so that it can be queries later
   --  through Get_File_Property.
   --  If Persistent is True, the property will be preserved from one session
   --  of GPS to the next.
   --  Property names are case sensitive.

   procedure Remove_Property
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project  : GNATCOLL.Projects.Project_Type;
      Name     : String);
   --  Remove the named property (persistent or not) from the file.

   procedure Set_Language_From_File
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Filename : GNATCOLL.VFS.Virtual_File;
      Language : String := "");
   --  Override the language to be used for the specific filename. This doesn't
   --  change the project itself, just the properties associated with the file.
   --  If Language is set to "", then the language will be guessed from the
   --  project.

   -----------------------------------------
   -- Saving and restoring all properties --
   -----------------------------------------

   procedure Close_Persistent_Properties_DB
     (Kernel : access Kernel_Handle_Record'Class);
   --  Close DB. This subprogram should only be called by the kernel itself.

   function Open_Persistent_Properties_DB
     (Kernel : access Kernel_Handle_Record'Class)
     return Writer;
   --  Open DB. This subprogram should only be called by the kernel itself.

   procedure Reset_Properties
     (Kernel : access Kernel_Handle_Record'Class);
   --  Clear the properties cache. No property will be available after this
   --  call.
   --  This subprogram should only be called by the kernel itself.

   -------------
   -- Scripts --
   -------------

   procedure Register_Script_Commands
     (Kernel : access Kernel_Handle_Record'Class);
   --  Register the script commands associated with this module

private

   -----------------
   -- Writer_Type --
   -----------------

   type Writer_Record is
     abstract new GPS.Properties.Writer_Record with null record;
   type Writer is access all Writer_Record'Class;

   procedure Insert
     (Self     : not null access Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description) is abstract;

   procedure Update
     (Self     : not null access Writer_Record;
      Key      : String;
      Name     : String;
      Property : Property_Description) is abstract;

   procedure Dump_Database
     (Self : not null access Writer_Record) is abstract;
   --  Dump database's data to test file

end GPS.Kernel.Properties;
