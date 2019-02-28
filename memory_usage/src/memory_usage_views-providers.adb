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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Unchecked_Deallocation;
with GNATCOLL.Traces;                       use GNATCOLL.Traces;

with GPS.Kernel.Hooks;                      use GPS.Kernel.Hooks;

package body Memory_Usage_Views.Providers is

   Me : constant Trace_Handle := Create ("GPS.MEMORY_USAGE.PROVIDERS");

   package Memory_Usage_Provider_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Memory_Usage_Provider,
        Hash            => Ada.Strings.Hash_Case_Insensitive,
        Equivalent_Keys => "=",
        "="             => "=");

   type Kernel_Data is record
      Providers : Memory_Usage_Provider_Maps.Map;
   end record;
   Global_Data : Kernel_Data;
   --  Used to store the registered providers

   type On_Compilation_Finished is new Compilation_Finished_Hooks_Function
      with null record;
   overriding procedure Execute
     (Self   : On_Compilation_Finished;
      Kernel : not null access Kernel_Handle_Record'Class;
      Category, Target, Mode : String;
      Shadow, Background     : Boolean;
      Status                 : Integer);
   --  Called when a compilation occured.
   --  Ask the currently selected memory usage provider to fetch the data
   --  that needs to be displayed in the memory usage view.

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Compilation_Finished;
      Kernel : not null access Kernel_Handle_Record'Class;
      Category, Target, Mode : String;
      Shadow, Background     : Boolean;
      Status                 : Integer) is
      pragma Unreferenced (Self, Mode, Category, Background, Shadow, Status);
   begin
      --  ??? use the currently selected provider once we have the
      --  possibility to choose the memory usage provider to use.
      if Target = "Build All" or else Target = "Build Main" then
         declare
            Provider : constant Memory_Usage_Provider :=
                         Global_Data.Providers ("LD");
         begin
            if Provider.Is_Enabled then
               Provider.Async_Fetch_Memory_Usage_Data
                 (Visitor => new Provider_Task_Visitor_Type'
                    (Kernel => Kernel_Handle (Kernel)));
            end if;
         end;
      end if;
   end Execute;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Provider_Task_Visitor) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Provider_Task_Visitor_Type'Class, Provider_Task_Visitor);
   begin
      if Self /= null then
         Self.all.Free;
         Unchecked_Free (Self);
      end if;
   end Free;

   ----------------------------------
   -- On_Memory_Usage_Data_Fetched --
   ----------------------------------

   procedure On_Memory_Usage_Data_Fetched
     (Self           : not null access Provider_Task_Visitor_Type;
      Memory_Regions : Memory_Region_Description_Maps.Map)
   is

      function Has_Memory_Overflow return Boolean
      is
        (for some Region of Memory_Regions =>
            Region.Used_Size > Region.Length);

   begin
      if not Memory_Regions.Is_Empty then
         declare
            use Memory_Usage_MDI_Views;

            View       : Memory_Usage_MDI_Views.View_Access :=
                           Memory_Usage_MDI_Views.Retrieve_View
                             (Kernel       => Self.Kernel,
                              Visible_Only => True);
            Give_Focus : constant Boolean :=
                           (View = null or else Has_Memory_Overflow);
         begin
            --  Raise the view only if it was not there before or if a memory
            --  overflow occured.
            View := Memory_Usage_MDI_Views.Get_Or_Create_View
              (Self.Kernel,
               Focus => Give_Focus,
               Init  => Memory_Usage_Views.On_Init'Access);

            --  Highlight the view when it does not gain the focus to warn
            --  users that the view has been refreshed.
            if not Give_Focus then
               Highlight_Child
                 (Memory_Usage_MDI_Views.Child_From_View (View));
            end if;

            View.Refresh (Memory_Regions => Memory_Regions);
         end;
      end if;
   end On_Memory_Usage_Data_Fetched;

   -----------------------
   -- Register_Provider --
   -----------------------

   procedure Register_Provider
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String;
      Provider : not null access Memory_Usage_Provider_Type'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Trace (Me, Name & " memory usage provider registered");
      Provider.Name := To_Unbounded_String (Name);
      Global_Data.Providers.Include
        (Key      => Name,
         New_Item => Provider);
   end Register_Provider;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Compilation_Finished_Hook.Add (new On_Compilation_Finished);
   end Register_Module;

end Memory_Usage_Views.Providers;
