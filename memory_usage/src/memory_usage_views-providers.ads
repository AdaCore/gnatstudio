------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

private package Memory_Usage_Views.Providers is

   ----------------------------------
   -- Memory Usage Views Providers --
   ----------------------------------

   type Memory_Usage_Provider_Type is abstract tagged private;
   type Memory_Usage_Provider is access all Memory_Usage_Provider_Type'Class;
   --  Memory usage providers are used to fecth data regarding memory usage
   --  (e.g: Memory regions, sections, synbols etc.) of the last built
   --  executable.

   procedure Register_Provider
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String;
      Provider : not null access Memory_Usage_Provider_Type'Class);
   --  Register the given memory usage view provider, associating it with the
   --  given Name.

   ----------------------------
   -- Provider Task Visitors --
   ----------------------------

   type Provider_Task_Visitor_Type is tagged private;
   type Provider_Task_Visitor is access all Provider_Task_Visitor_Type'Class;
   --  Most of the memory usage provider primitives should be asynchronous:
   --  calling them with a visitor and its attached primitives allow us to know
   --  when a given task has been completed.

   procedure Free (Self : in out Provider_Task_Visitor_Type) is null;
   procedure Free (Self : in out Provider_Task_Visitor);
   --  Called when the visitor is no longer needed.

   procedure On_Memory_Usage_Data_Fetched
     (Self           : not null access Provider_Task_Visitor_Type;
      Memory_Regions : Memory_Region_Description_Maps.Map);
   --  Called when all the memory usage data has been fetched by the given
   --  memory usage provider.

   ----------------------------
   -- Overridable Operations --
   ----------------------------

   function Is_Enabled
     (Self : not null access Memory_Usage_Provider_Type) return Boolean
      is abstract;
   --  Return True if the given memory usage provider is enabled and False
   --  otherwise.
   --
   --  This is used to know whether we should query memory usage data from
   --  this provider once an executable is built.

   procedure Async_Fetch_Memory_Usage_Data
     (Self    : not null access Memory_Usage_Provider_Type;
      Visitor : access Provider_Task_Visitor_Type'Class) is abstract;
   --  Ask the given memory usage provider to fetch the memory usage data

   ------------
   -- Module --
   ------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);

private

   type Memory_Usage_Provider_Type is abstract tagged record
      Name : Unbounded_String;
   end record;

   type Provider_Task_Visitor_Type is tagged record
      Kernel : Kernel_Handle;
   end record;

end Memory_Usage_Views.Providers;
