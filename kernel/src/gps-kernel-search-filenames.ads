------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

--  A search provider that matches name of files from the current project.

with GPS.Search;
with GNATCOLL.VFS;

package GPS.Kernel.Search.Filenames is

   type Filenames_Search_Provider is new Kernel_Search_Provider
     with private;

   overriding procedure Free (Self : in out Filenames_Search_Provider);
   overriding function Documentation
     (Self    : not null access Filenames_Search_Provider) return String;
   overriding procedure Set_Pattern
     (Self    : not null access Filenames_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Filenames_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Filenames_Search_Provider) return String
     is ("File names");

   type Filenames_Search_Result is new Kernel_Search_Result with private;

private
   type Filenames_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      Pattern_Needs_Free : Boolean := False;

      Line, Column : Natural := 0;  --  from pattern
      Match_Directory : Boolean;  --  whether to match directory part
      Files   : GNATCOLL.VFS.File_Array_Access;
      Index   : Integer;  --  last file tested

      Seen : GPS.Kernel.File_Sets.Set;
      --  Files already returned, to avoid duplicates (in particular the
      --  list of runtime files could include duplicates)

      Runtime : GNATCOLL.VFS.File_Array_Access;
      Runtime_Index : Natural;  --  last runtime file tested
   end record;

   type Filenames_Search_Result is new Kernel_Search_Result with record
      File : GNATCOLL.VFS.Virtual_File;
      Line, Column : Natural := 0;
   end record;

   overriding procedure Execute
     (Self       : not null access Filenames_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access Filenames_Search_Result)
     return Gtk.Widget.Gtk_Widget;

end GPS.Kernel.Search.Filenames;
