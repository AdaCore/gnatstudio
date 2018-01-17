------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

with GPS.Core_Kernels;        use GPS.Core_Kernels;
with GPS.Scripts.Files;
with Basic_Types;             use Basic_Types;

package body GPS.Scripts.File_Locations is

   type Location_Properties_Record is new Instance_Property_Record with record
      Location : File_Location_Info;
   end record;

   File_Location_Class_Name : constant String := "FileLocation";

   Filename_Cst   : aliased constant String := "filename";
   Line_Cst       : aliased constant String := "line";
   Col_Cst        : aliased constant String := "column";

   Location_Cmd_Parameters  : constant Cst_Argument_List :=
                                (1 => Filename_Cst'Access,
                                 2 => Line_Cst'Access,
                                 3 => Col_Cst'Access);

   procedure Set_Data
     (Instance : Class_Instance; Location : File_Location_Info);

   procedure Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "FileLocation" commands

   --------------------------
   -- Create_File_Location --
   --------------------------

   function Create_File_Location
     (Script : access Scripting_Language_Record'Class;
      File   : Class_Instance;
      Line   : Natural;
      Column : Basic_Types.Visible_Column_Type)
      return Class_Instance
   is
      Instance : constant Class_Instance := New_Instance
        (Script,
         New_Class (Get_Repository (Script), File_Location_Class_Name));
      Info     : constant File_Location_Info := (File, Line, Column);

   begin
      Set_Data (Instance, Info);
      return Instance;
   end Create_File_Location;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Location : File_Location_Info)
      return Basic_Types.Visible_Column_Type is
   begin
      return Location.Column;
   end Get_Column;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class;
      N : Positive)
      return File_Location_Info
   is
      Class : constant Class_Type :=
                Get_File_Location_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance :=
                Nth_Arg (Data, N, Class);
      D     : Instance_Property;
   begin
      if Inst /= No_Class_Instance then
         D := Get_Data (Inst, File_Location_Class_Name);
      end if;

      if D = null then
         return No_File_Location;
      else
         return Location_Properties_Record (D.all).Location;
      end if;
   end Get_Data;

   --------------
   -- Get_File --
   --------------

   function Get_File (Location : File_Location_Info) return Class_Instance is
   begin
      return Location.File;
   end Get_File;

   -----------------------------
   -- Get_File_Location_Class --
   -----------------------------

   function Get_File_Location_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, File_Location_Class_Name);
   end Get_File_Location_Class;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Location : File_Location_Info) return Integer is
   begin
      return Location.Line;
   end Get_Line;

   ------------------------------
   -- Location_Command_Handler --
   ------------------------------

   procedure Location_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel   : constant Core_Kernel := Get_Kernel (Data);
      Location : File_Location_Info;

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Location_Cmd_Parameters);

         declare
            File     : constant Class_Instance  :=
                         Nth_Arg (Data, 2, Files.Get_File_Class (Kernel));
            L        : constant Integer := Nth_Arg (Data, 3);
            C        : constant Visible_Column_Type :=
                         Visible_Column_Type (Nth_Arg (Data, 4, Default => 1));
            Instance : constant Class_Instance :=
                         Nth_Arg (Data, 1, Get_File_Location_Class (Kernel));
         begin
            Set_Data (Instance, File_Location_Info'(File, L, C));
         end;

      elsif Command = "line" then
         Location := Get_Data (Data, 1);
         Set_Return_Value (Data, Get_Line (Location));

      elsif Command = "column" then
         Location := Get_Data (Data, 1);
         Set_Return_Value (Data, Natural (Get_Column (Location)));

      elsif Command = "file" then
         Location := Get_Data (Data, 1);

         declare
            File : constant Class_Instance := Get_File (Location);
         begin
            Set_Return_Value (Data, File);
         end;
      end if;
   end Location_Command_Handler;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class) is
   begin
      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Minimum_Args => 3,
         Maximum_Args => 3,
         Class        => Get_File_Location_Class (Kernel),
         Handler      => Location_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "line",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Location_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "column",
         Class         => Get_File_Location_Class (Kernel),
         Handler       => Location_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "file",
         Class        => Get_File_Location_Class (Kernel),
         Handler      => Location_Command_Handler'Access);
   end Register_Commands;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Location : File_Location_Info) is
   begin
      if not Is_Subclass (Instance, File_Location_Class_Name) then
         raise Invalid_Data;
      end if;

      Set_Data
        (Instance, File_Location_Class_Name,
         Location_Properties_Record'(Location => Location));
   end Set_Data;

end GPS.Scripts.File_Locations;
