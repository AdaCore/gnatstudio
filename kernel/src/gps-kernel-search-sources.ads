------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

--  A search provider that matches the contents of source files

with GPS.Search;
with GNATCOLL.VFS;
with GNAT.Strings;

package GPS.Kernel.Search.Sources is

   type Sources_Search_Provider is new Kernel_Search_Provider
     with private;
   overriding procedure Free (Self : in out Sources_Search_Provider);
   overriding function Documentation
     (Self    : not null access Sources_Search_Provider) return String;
   overriding procedure Set_Pattern
     (Self    : not null access Sources_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Sources_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Sources_Search_Provider) return String
   is
     (Provider_Sources);
   overriding function Get_Total_Progress
     (Self : not null access Sources_Search_Provider) return Integer;
   --  Searches in all source files of the project

   type Single_Source_Search_Provider is new Kernel_Search_Provider
     with private;
   overriding procedure Free (Self : in out Single_Source_Search_Provider);
   overriding function Documentation
     (Self    : not null access Single_Source_Search_Provider) return String;
   overriding procedure Set_Pattern
     (Self    : not null access Single_Source_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Single_Source_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Single_Source_Search_Provider) return String
     is ("Specific file");
   --  Searches in a specific source file

   procedure Set_File
     (Self    : in out Single_Source_Search_Provider;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type);
   --  Set the file to search

   type Current_File_Search_Provider is new Single_Source_Search_Provider
      with private;
   overriding procedure Set_Pattern
     (Self    : not null access Current_File_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding function Display_Name
     (Self     : not null access Current_File_Search_Provider) return String
     is ("Current file");
   overriding function Documentation
     (Self    : not null access Current_File_Search_Provider) return String
     is ("Search for references in the current editor");
   --  Search in the current editor, if there is one.

private
   type Single_Source_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      Pattern_Needs_Free : Boolean := False;

      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Text    : GNAT.Strings.String_Access;
      Restart : Boolean := False;
      Context : GPS.Search.Search_Context;
   end record;

   type Sources_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      Pattern_Needs_Free : Boolean := False;

      Files   : GNATCOLL.Projects.File_And_Project_Array_Access;
      Index   : Integer;  --  next to process
      Current : aliased Single_Source_Search_Provider;
   end record;

   type Current_File_Search_Provider is new Single_Source_Search_Provider
      with null record;
end GPS.Kernel.Search.Sources;
