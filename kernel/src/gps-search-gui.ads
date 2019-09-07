------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

--  This package provides the GUI support for the global search entry
--  in the GPS toolbar.

with Gtk.Box;                    use Gtk.Box;
with Glib.Object;                use Glib.Object;

with Commands.Interactive;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Search;          use GPS.Kernel.Search;
with Histories;

package GPS.Search.GUI is

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Creates the global search entry, and all GPS actions to access it.

   -----------------------------
   -- Overall_Search_Provider --
   -----------------------------

   type Result_Array is array (Natural range <>) of Search_Result_Access;
   type Result_Array_Access is access all Result_Array;

   type Overall_Search_Provider is new Kernel_Search_Provider with private;
   type Overall_Search_Provider_Access
   is access all Overall_Search_Provider'Class;

   procedure Initialize
     (Self     : not null access Overall_Search_Provider;
      Registry : not null Search_Provider_Registry_Access);
   --  Initialize Self, giving it a search registry containing all the
   --  sub-providers that will be used when searching.

   overriding procedure Free (Self : in out Overall_Search_Provider);
   overriding procedure Set_Pattern
     (Self    : not null access Overall_Search_Provider;
      Pattern : not null access GPS.Search.Search_Pattern'Class;
      Limit   : Natural := Natural'Last);
   overriding procedure Next
     (Self     : not null access Overall_Search_Provider;
      Result   : out GPS.Search.Search_Result_Access;
      Has_Next : out Boolean);
   overriding function Display_Name
     (Self     : not null access Overall_Search_Provider) return String;
   overriding function Documentation
     (Self : not null access Overall_Search_Provider) return String;
   overriding procedure Edit_Settings
     (Self      : not null access Overall_Search_Provider;
      Box       : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data      : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback);
   overriding function Complete_Suffix
     (Self      : not null access Overall_Search_Provider;
      Pattern   : not null access GPS.Search.Search_Pattern'Class)
      return String;

   --------------
   -- Commands --
   --------------

   type History_Key_Access is access all Histories.History_Key;

   type Global_Search_Command is new Commands.Interactive.Interactive_Command
   with record
      Provider : GPS.Search.Search_Provider_Access;
      History  : History_Key_Access;
   end record;
   type Global_Search_Command_Access
      is access all Global_Search_Command'Class;
   overriding function Execute
      (Self    : access Global_Search_Command;
       Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Activate the global search field

   procedure Register_Provider_And_Action
     (Kernel     : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Provider   : not null access Kernel_Search_Provider'Class;
      Icon_Name  : String := "");
   --  Register a provider for the Global Search, set its Kernel attribute,
   --  and create an action fo it.

private

   type Overall_Search_Provider is new Kernel_Search_Provider with record
      Registry         : Search_Provider_Registry_Access;
      --  List of all the providers in which we search.

      Pattern          : Search_Pattern_Access;
      Provider         : Search_Provider_Access;  --  the current one
      Current_Provider : Integer := -1;   --  index of current one

      Current          : Result_Array_Access;
      Current_Returned : Natural;
      Current_Index    : Natural;
      --  The best proposals for the current provider.
   end record;

   overriding function Get_Current_Progress
     (Self : not null access Overall_Search_Provider) return Natural;
   overriding function Get_Total_Progress
     (Self : not null access Overall_Search_Provider) return Integer;
   overriding procedure Reset_Progress
     (Self : not null access Overall_Search_Provider);

end GPS.Search.GUI;
