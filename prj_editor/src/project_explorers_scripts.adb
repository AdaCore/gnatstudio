-----------------------------------------------------------------------
--                               GNAT Studio                         --
--                                                                   --
--                      Copyright (C) 2024, AdaCore                  --
--                                                                   --
-- GNAT Studio is free  software;  you can redistribute it and/or    --
-- modify it under the terms of the GNU General Public License as    --
-- published by the Free Software Foundation; either version 2 of    --
-- the License, or (at your option) any later version.               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNATCOLL.Scripts;   use GNATCOLL.Scripts;
with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;

package body Project_Explorers_Scripts is

   package Python_Filters_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Subprogram_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Project_View_Filters : Python_Filters_Maps.Map;

   Add_Filter_Method_Cst    : constant String := "add_filter";
   Remove_Filter_Method_Cst : constant String := "remove_filter";
   List_Filters_Method_Cst  : constant String := "list_filters";

   procedure Project_View_Filter_Handler
     (Data    : in out Callback_Data'Class;
      Command : String);

   procedure Add_Python_Filter
     (Filters : in out Python_Filters_Maps.Map;
      Id      : String;
      Filter  : Subprogram_Type);
   --  Add extra filter which will be called during Is_Visible
   --  and store it using its Id

   procedure Remove_Python_Filter
     (Filters : in out Python_Filters_Maps.Map;
      Id      : String);
   --  Remove the filter related to Id

   -----------------------
   -- Add_Python_Filter --
   -----------------------

   procedure Add_Python_Filter
     (Filters : in out Python_Filters_Maps.Map;
      Id      : String;
      Filter  : Subprogram_Type) is
   begin
      Remove_Python_Filter (Filters, Id);
      Filters.Insert (Id, Filter);
   end Add_Python_Filter;

   --------------------------
   -- Remove_Python_Filter --
   --------------------------

   procedure Remove_Python_Filter
     (Filters : in out Python_Filters_Maps.Map;
      Id      : String) is
   begin
      if Filters.Contains (Id) then
         declare
            Subp : Subprogram_Type := Filters.Element (Id);
         begin
            Filters.Delete (Id);
            Free (Subp);
         end;
      end if;
   end Remove_Python_Filter;

   -------------------------
   -- Has_Project_Filters --
   -------------------------

   function Has_Project_Filters return Boolean is
   begin
      return not Project_View_Filters.Is_Empty;
   end Has_Project_Filters;

   ---------------------------
   -- Is_Visible_In_Project --
   ---------------------------

   function Is_Visible_In_Project
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      Script : constant Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language ("Python");
   begin
      for Subp of Project_View_Filters loop
         declare
            Args   : Callback_Data'Class := Create
              (Script, Arguments_Count => 1);
            Result : Boolean;
         begin
            Set_Nth_Arg (Args, 1, File);
            Result := Subp.Execute (Args);
            Free (Args);

            if not Result then
               return False;
            end if;
         exception
            when others =>
               Free (Args);
         end;
      end loop;
      return True;
   end Is_Visible_In_Project;

   ---------------------------------
   -- Project_View_Filter_Handler --
   ---------------------------------

   procedure Project_View_Filter_Handler
     (Data    : in out Callback_Data'Class;
      Command : String) is
   begin
      if Command = Add_Filter_Method_Cst then
         declare
            Subp : constant Subprogram_Type := Nth_Arg (Data, 2, null);
         begin
            if Subp /= null then
               Add_Python_Filter
                 (Project_View_Filters, Nth_Arg (Data, 1, ""), Subp);
            end if;
         end;

      elsif Command = Remove_Filter_Method_Cst then
         Remove_Python_Filter (Project_View_Filters, Nth_Arg (Data, 1, ""));

      elsif Command = List_Filters_Method_Cst then
         Set_Return_Value_As_List (Data);
         declare
            Cursor : Python_Filters_Maps.Cursor := Project_View_Filters.First;
         begin
            while Cursor.Has_Element loop
               Set_Return_Value (Data, Cursor.Key);
               Cursor.Next;
            end loop;
         end;
      end if;
   end Project_View_Filter_Handler;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Project_View_Filters_Class : constant Class_Type :=
        New_Class (Kernel, "ProjectViewFilters");
   begin
      Project_View_Filters.Clear;

      Register_Command
        (Kernel,
         Command       => Add_Filter_Method_Cst,
         Minimum_Args  => 2,
         Maximum_Args  => 2,
         Class         => Project_View_Filters_Class,
         Static_Method => True,
         Handler       => Project_View_Filter_Handler'Access);
      Register_Command
        (Kernel,
         Command       => Remove_Filter_Method_Cst,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
         Class         => Project_View_Filters_Class,
         Static_Method => True,
         Handler       => Project_View_Filter_Handler'Access);
      Register_Command
        (Kernel,
         Command       => List_Filters_Method_Cst,
         Minimum_Args  => 0,
         Maximum_Args  => 0,
         Class         => Project_View_Filters_Class,
         Static_Method => True,
         Handler       => Project_View_Filter_Handler'Access);
   end Register_Module;

end Project_Explorers_Scripts;
