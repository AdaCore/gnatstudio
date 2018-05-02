------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2018, AdaCore                     --
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
with Gtk.Box;
with Glib.Object;

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
     is (Provider_Filenames);
   overriding function Complete_Suffix
     (Self      : not null access Filenames_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;
   overriding procedure Edit_Settings
     (Self : not null access Filenames_Search_Provider;
      Box  : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback);
   overriding function Get_Total_Progress
     (Self : not null access Filenames_Search_Provider) return Integer;

   type Filenames_Search_Result is new Kernel_Search_Result with private;

private
   type Search_Step is
     (User_File,
      Project_Sources,
      Runtime_Sources,
      Project_Files,
      Other_Files);

   type Search_Data (Step : Search_Step := Project_Sources) is record
      case Step is
         when User_File =>
            null;
         when Project_Sources =>
            Index : Integer;  --  Last file tested in .Files
         when Runtime_Sources =>
            Runtime_Index : Integer; --  Last runtime file tested
         when Project_Files =>
            Iter  : GNATCOLL.Projects.Project_Iterator;
         when Other_Files =>
            Files_In_Dir : GNATCOLL.VFS.File_Array_Access;
            Dirs_Index : Integer;  --  Current dir being tested
            File_Index : Integer;  --  Last file tested
      end case;
   end record;

   type Filenames_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      Pattern_Needs_Free : Boolean := False;

      Line, Column : Natural := 0;  --  from pattern
      Match_Directory : Boolean;  --  whether to match directory part
      Files       : GNATCOLL.Projects.File_And_Project_Array_Access;
      Runtime     : GNATCOLL.VFS.File_Array_Access;
      Source_Dirs : GNATCOLL.VFS.File_Array_Access;

      Data : Search_Data;

      Seen : GPS.Kernel.File_Sets.Set;
      --  Files already returned, to avoid duplicates (in particular the
      --  list of runtime files could include duplicates)
   end record;

   type Filenames_Search_Result is new Kernel_Search_Result with record
      File         : GNATCOLL.VFS.Virtual_File;
      Project      : GNATCOLL.Projects.Project_Type;
      Line, Column : Natural := 0;
   end record;
   overriding procedure Execute
     (Self       : not null access Filenames_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access Filenames_Search_Result)
     return Gtk.Widget.Gtk_Widget;

end GPS.Kernel.Search.Filenames;
