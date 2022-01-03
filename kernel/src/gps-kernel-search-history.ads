------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2013-2022, AdaCore                     --
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

--  A provider that provides searching history information.

with GPS.Search;
with GNATCOLL.VFS;

private with Ada.Containers.Vectors;

package GPS.Kernel.Search.History is

   type History_Search_Provider is new Kernel_Search_Provider
     with private;

   overriding procedure Free (Self : in out History_Search_Provider);
   overriding function Documentation
     (Self    : not null access History_Search_Provider) return String;
   overriding procedure Set_Pattern
     (Self    : not null access History_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access History_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access History_Search_Provider) return String
     is (Provider_History);
   overriding function Get_Total_Progress
     (Self : not null access History_Search_Provider) return Integer;

   procedure Add_File_To_History
     (Pattern : String;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Natural := 0;
      Column  : Natural := 0);
   --  Stores a file in the history

private

   type History_Search_Result is new Kernel_Search_Result with record
      File         : GNATCOLL.VFS.Virtual_File;
      Project      : GNATCOLL.Projects.Project_Type;
      Line, Column : Natural := 0;
   end record;
   overriding procedure Execute
     (Self       : not null access History_Search_Result;
      Give_Focus : Boolean);
   overriding function Full
     (Self       : not null access History_Search_Result)
     return Gtk.Widget.Gtk_Widget;

   type History is record
      Pattern : Unbounded_String := Null_Unbounded_String;
      File    : GNATCOLL.VFS.Virtual_File := No_File;
      Project : GNATCOLL.Projects.Project_Type := GNATCOLL.Projects.No_Project;
      Line    : Natural := 0;
      Column  : Natural := 0;
   end record;

   package History_Vectors is new Ada.Containers.Vectors (Positive, History);
   use History_Vectors;

   type History_Search_Provider is new Kernel_Search_Provider with record
      Pattern : GPS.Search.Search_Pattern_Access;
      Current : History_Vectors.Cursor;
   end record;

end GPS.Kernel.Search.History;
