------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2016, AdaCore                     --
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

--  This package handles the lifecycle of the database:
--  initialization, finalization, refresh on project changes, etc.

package Xref.Lifecycle is

   procedure Project_Changed (Self : General_Xref_Database);
   --  The project has changed, we need to reset the xref database. This is
   --  called at least once prior to calls to Project_View_Changed.
   --  At this stage, the view of the project hasn't been computed, so you can
   --  not do any query on the project itself.

   procedure Project_View_Changed
     (Self   : General_Xref_Database;
      Tree   : GNATCOLL.Projects.Project_Tree_Access);
   --  The view of the project has changed, we need to refresh the xref
   --  databases.

   procedure Close_Database (Self   : General_Xref_Database);
   --  Close the database connection (and perhaps remove the sqlite database
   --  if we were using a temporary project).

   function Xref_Database_Location
     (Self    : not null access General_Xref_Database_Record'Class)
      return GNATCOLL.VFS.Virtual_File;
   --  Location of the sqlite file that contains the xref database on which
   --  GPS is currently working.

end Xref.Lifecycle;
