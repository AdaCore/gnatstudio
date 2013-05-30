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

--  The root for all the search providers in GPS

with GPS.Search;
with Gtk.Widget;

package GPS.Kernel.Search is

   type Kernel_Search_Result is abstract new GPS.Search.Search_Result
   with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

   function Full
      (Self : not null access Kernel_Search_Result)
      return Gtk.Widget.Gtk_Widget is (null);
   --  Returns the full description for the result. This description might be
   --  displayed in a separate pane in the search popup. In most cases, GPS
   --  will not query or display this information at all.

   type Kernel_Search_Provider is abstract new GPS.Search.Search_Provider
   with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

   procedure Adjust_Score
      (Self   : not null access Kernel_Search_Provider;
       Result : not null access GPS.Search.Search_Result'Class);
   --  Adjust the score of Result, using various criteria. Among other
   --  things, this uses the list of most recent items selected for this
   --  provider so that they appear first

   overriding procedure On_Result_Executed
      (Self   : not null access Kernel_Search_Provider;
       Result : not null access GPS.Search.Search_Result'Class);
   --  Change the list of recent items, after Result has been selected
   --  by the user.

   type Kernel_Provider_Registry
     is new GPS.Search.Search_Provider_Registry with
      record
         Kernel : GPS.Kernel.Kernel_Handle;
      end record;

   overriding function Get
     (Self : Kernel_Provider_Registry;
      Name : String) return GPS.Search.Search_Provider_Access;
   --  Same as inherited Get, but also initializes the Kernel field in the
   --  provider, when needed

   Provider_Filenames : constant String := "file names";
   Provider_Actions   : constant String := "actions";

   Registry : Kernel_Provider_Registry;
   --  ??? Will be moved to the kernel

end GPS.Kernel.Search;
