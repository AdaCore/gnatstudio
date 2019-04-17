------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

--  This package provides utilities for creating and maintaining
--  a GNATCOLL Xref database for the kernel.

with GNATCOLL.VFS;  use GNATCOLL.VFS;
with GNATCOLL.Xref; use GNATCOLL.Xref;
with GNATCOLL.Projects; use GNATCOLL.Projects;
with Gtk.Widget;
with Xref;              use Xref;
with Language.Profile_Formaters; use Language.Profile_Formaters;

package GPS.Kernel.Xref is

   -----------------------
   -- GPS_Xref_Database --
   -----------------------

   type GPS_Xref_Database is new Extended_Xref_Database with private;
   type GPS_Xref_Database_Access is access all GPS_Xref_Database'Class;

   overriding procedure On_Error
     (Self  : GPS_Xref_Database;
      Error : String);
   --  Handler for an error

   -------------------------------
   -- GPS_General_Xref_Database --
   -------------------------------

   type GPS_General_Xref_Database_Record is new General_Xref_Database_Record
       with private;

   overriding function Select_Entity_Declaration
     (Self    : access GPS_General_Xref_Database_Record;
      File    : Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Entity  : Root_Entity'Class) return Root_Entity'Class;
   --  see inherited documentation

   procedure Create_Database
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Result : out Standard.Xref.General_Xref_Database);
   --  Create and initialize the xref databases

   ------------------------
   -- Background queries --
   ------------------------
   --  This package contains various subprograms to do highlevel xref queries
   --  in background mode. In particular, it is used for the call-graph related
   --  queries.

   type Commands_User_Data_Record is abstract tagged null record;
   type Commands_User_Data is access all Commands_User_Data_Record'Class;

   procedure Destroy
     (Data : in out Commands_User_Data_Record; Cancelled : Boolean);
   --  Called when the user data is no longer needed (ie at the end of the
   --  search or when the search has been cancelled).
   --  Cancelled is set to True if the search didn't complete. It is possible
   --  that the "Watch" widget passed to Examine_Ancestors_Call_Graph was
   --  destroyed and is no longer valid

   function On_Entity_Found
     (Data                : access Commands_User_Data_Record;
      Entity              : Root_Entity'Class;
      Parent              : Root_Entity'Class;
      Ref                 : Root_Entity_Reference'Class;
      Through_Dispatching : Boolean;
      Is_Renaming         : Boolean) return Boolean is abstract;
   --  If Parent is a renaming of the original Entity, Is_Renaming is set to
   --  true, and Ref is set to No_Entity_Reference.
   --  Entity is the entity that was searched initially.
   --  If False is returned, the search is stopped.
   --  If Through_Dispatching is true, then the call occurs through dispatching

   procedure Examine_Ancestors_Call_Graph
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Entity            : Root_Entity'Class;
      User_Data         : access Commands_User_Data_Record'Class;
      Background_Mode   : Boolean := True;
      Dispatching_Calls : Boolean := False;
      Watch             : Gtk.Widget.Gtk_Widget := null);
   --  Search for all entities calling Entity.
   --  For each of this entity, Callback is called. In this case, its Parent
   --  parameter is the caller, and Ref is the occurrence of Entity within its
   --  Parent.
   --  Is_Renaming will be set to True if Parent is a renaming of Entity.
   --
   --  By default, the search is done asynchronously in background mode.
   --  User_Data will be deallocated automatically when the search is finished.
   --
   --  If Watch is destroyed during the search, then the latter is cancelled.
   --
   --  If Dispatching_Calls is true, then any caller that might call the
   --  Entity indirectly through a dispatching call is also listed.

   procedure Examine_Entity_Call_Graph
     (Entity            : Root_Entity'Class;
      User_Data         : access Commands_User_Data_Record'Class;
      Get_All_Refs      : Boolean;
      Dispatching_Calls : Boolean);
   --  Search for all entities called by Entity.
   --  If Get_All_Refs is true, then all occurrences of the called entities
   --  within Entity will result in a call to On_Entity_Found. Otherwise, a
   --  single call it made for each entity, passing Ref as No_Entity_Reference.
   --  This call is synchronous.
   --  If Dispatching_Calls is true, then a dispatching call within Entity will
   --  be expanded to the full list of subprograms that might possibly be
   --  called at that place.

   ----------------------
   -- Context handling --
   ----------------------

   function Get_Entity_Information_Type return Glib.GType;
   --  Return the type associated with an entity. This is the type that should
   --  be used when creating the tree model.

   function Documentation
     (Self             : General_Xref_Database;
      Handler          : Language_Handlers.Language_Handler;
      Entity           : Root_Entity'Class;
      Color_For_Optional_Param : String := "#555555";
      Raw_Format       : Boolean := False;
      Check_Constructs : Boolean := True) return String;
   --  Return the documentation (tooltips,...) for the entity.
   --  If Raw_Format is False, the documentation is formated in HTML (using
   --  Color_For_Optional_Param to highlight optional parameters).
   --  Check_Constructs should be False to disable the use of the constructs
   --  database.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the Xref actions and hooks.

   function Get_HTML_Profile_Formater return Profile_Formater'Class;

private

   type GPS_General_Xref_Database_Record is new General_Xref_Database_Record
   with record
      Kernel : Kernel_Handle;
   end record;

   type GPS_Xref_Database is new Extended_Xref_Database with record
      Kernel : Kernel_Handle;
   end record;

   type HTML_Profile_Formater is new Profile_Formater with record
      Text                     : Ada.Strings.Unbounded.Unbounded_String;
      Has_Generic_Parameter    : Boolean := False;
      Has_Parameter            : Boolean := False;
      Color_For_Optional_Param : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   overriding procedure Add_Parameter
     (Self    : access HTML_Profile_Formater;
      Name    : String;
      Mode    : String;
      Of_Type : String;
      Default : String);
   overriding procedure Add_Result
     (Self    : access HTML_Profile_Formater;
      Mode    : String;
      Of_Type : String);
   overriding procedure Add_Variable
     (Self    : access HTML_Profile_Formater;
      Mode    : String;
      Of_Type : String);
   overriding procedure Add_Aspects
     (Self : access HTML_Profile_Formater;
      Text : String);
   overriding procedure Add_Comments
     (Self : access HTML_Profile_Formater;
      Text : String);
   overriding procedure Add_Generic_Parameter
     (Self    : access HTML_Profile_Formater;
      Name    : String;
      Mode    : String;
      Of_Type : String;
      Default : String);
   overriding function Get_Text
     (Self : access HTML_Profile_Formater) return String;

end GPS.Kernel.Xref;
