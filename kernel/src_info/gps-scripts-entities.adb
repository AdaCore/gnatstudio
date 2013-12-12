------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with GPS.Core_Kernels;                 use GPS.Core_Kernels;
with GPS.Intl;                         use GPS.Intl;
with GPS.Scripts.Files;
with GPS.Scripts.File_Locations;
with Xref;                             use Xref;
with Basic_Types;                      use Basic_Types;

package body GPS.Scripts.Entities is

   type Entity_Properties_Record is new Instance_Property_Record with record
      Entity  : General_Entity;
   end record;

   Entity_Class_Name        : constant String := "Entity";

   Name_Cst       : aliased constant String := "name";
   File_Cst       : aliased constant String := "file";
   Line_Cst       : aliased constant String := "line";
   Col_Cst        : aliased constant String := "column";
   Approx_Search_Cst : aliased constant String
     := "approximate_search_fallback";
   Nth_Cst        : aliased constant String := "nth";

   Body_Cmd_Parameters      : constant Cst_Argument_List :=
                                (1 => Nth_Cst'Access);
   Entity_Cmd_Parameters    : constant Cst_Argument_List :=
                                (Name_Cst'Access, File_Cst'Access,
                                 Line_Cst'Access, Col_Cst'Access,
                                 Approx_Search_Cst'Access);

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the "Entity" commands

   -------------------
   -- Create_Entity --
   -------------------

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Xref.General_Entity)
      return Class_Instance
   is
      Instance : Class_Instance;
   begin
      if Entity = No_General_Entity then
         return No_Class_Instance;
      else
         Instance := New_Instance
           (Script, New_Class (Get_Repository (Script), Entity_Class_Name));
         Set_Data (Instance, Entity);
         return Instance;
      end if;
   end Create_Entity;

   ----------------------------
   -- Entity_Command_Handler --
   ----------------------------

   procedure Entity_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel  : constant Core_Kernel := Get_Kernel (Data);
      Entity : General_Entity;
      Ref    : General_Entity_Reference;

   begin
      if Command = Constructor_Method then
         Name_Parameters (Data, Entity_Cmd_Parameters);

         declare
            Name   : constant String  := Nth_Arg (Data, 2);
            File   : constant Class_Instance  :=
              Nth_Arg (Data, 3,
                       GPS.Scripts.Files.Get_File_Class (Kernel),
                       Default    => No_Class_Instance,
                       Allow_Null => True);
            Loc    : General_Location;
            Approx_Search : constant Boolean := Nth_Arg (Data, 6, True);

         begin
            if File = No_Class_Instance then
               --  Looking for a predefined entity
               Loc := No_Location;
            else
               Loc := (File => GPS.Scripts.Files.Get_Data (File),
                       Line => Nth_Arg (Data, 4, Default => 1),
                       Column => Visible_Column_Type
                         (Nth_Arg (Data, 5, Default => 1)));
            end if;

            Kernel.Databases.Find_Declaration_Or_Overloaded
              (Loc               => Loc,
               Entity_Name       => Name,
               Ask_If_Overloaded => False,
               Entity            => Entity,
               Closest_Ref       => Ref,
               Approximate_Search_Fallback => Approx_Search);

            if Entity = No_General_Entity then
               Set_Error_Msg (Data, -"Entity not found");
            else
               declare
                  Instance : constant Class_Instance :=
                    Nth_Arg (Data, 1, Get_Entity_Class (Kernel));
               begin
                  Set_Data (Instance, Entity);
               end;
            end if;
         end;

      elsif Command = "full_name" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value
           (Data, Kernel.Databases.Qualified_Name (Entity));

      elsif Command = "name" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Get_Name (Entity));

      elsif Command = "attributes" then
         --  ??? Should be made obsolete and replaced by separate functions.
         Entity := Get_Data (Data, 1);

         Set_Return_Value (Data, Kernel.Databases.Is_Global (Entity));
         Set_Return_Value_Key (Data, "global");

         Set_Return_Value (Data, Kernel.Databases.Is_Static_Local (Entity));
         Set_Return_Value_Key (Data, "static");

      elsif Command = "is_subprogram" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Subprogram (Entity));

      elsif Command = "is_generic" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Generic (Entity));

      elsif Command = "is_global" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Global (Entity));

      elsif Command = "is_access" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Access (Entity));

      elsif Command = "is_array" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Array (Entity));

      elsif Command = "is_type" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Type (Entity));

      elsif Command = "is_container" then
         Entity := Get_Data (Data, 1);
         Set_Return_Value (Data, Kernel.Databases.Is_Container (Entity));

      elsif Command = "declaration" then
         declare
            Location : General_Location;
         begin
            Entity := Get_Data (Data, 1);
            Location := Kernel.Databases.Get_Declaration (Entity).Loc;

            Set_Return_Value
              (Data, GPS.Scripts.File_Locations.Create_File_Location
                 (Get_Script (Data),
                  File   => GPS.Scripts.Files.Create_File
                              (Get_Script (Data), Location.File),
                  Line   => Location.Line,
                  Column => Location.Column));
         end;

      elsif Command = "body" then
         Name_Parameters (Data, Body_Cmd_Parameters);
         declare
            Location     : General_Location := No_Location;
            Cur_Location : General_Location := No_Location;
            Count        : Integer := Nth_Arg (Data, 2, 1);
         begin
            Entity := Get_Data (Data, 1);
            while Count > 0 loop
               Location := Kernel.Databases.Get_Body
                 (Entity, After => Cur_Location);
               Count := Count - 1;
               Cur_Location := Location;
            end loop;

            if Location /= No_Location then
               Set_Return_Value
                 (Data, GPS.Scripts.File_Locations.Create_File_Location
                    (Get_Script (Data),
                     File   => GPS.Scripts.Files.Create_File
                                 (Get_Script (Data), Location.File),
                     Line   => Location.Line,
                     Column => Location.Column));

            else
               Set_Error_Msg (Data, -"Body not found for the entity");
            end if;
         end;

      elsif Command = "end_of_scope" then
         declare
            Location : General_Location := No_Location;
         begin
            Entity := Get_Data (Data, 1);
            Location := Kernel.Databases.End_Of_Scope (Entity);
            if Location /= No_Location then
               Set_Return_Value
                 (Data, GPS.Scripts.File_Locations.Create_File_Location
                    (Get_Script (Data),
                     File   => GPS.Scripts.Files.Create_File
                                 (Get_Script (Data), Location.File),
                     Line   => Location.Line,
                     Column => Location.Column));
            else
               Set_Error_Msg (Data, -"end-of-scope not found for the entity");
            end if;
         end;

      end if;
   end Entity_Command_Handler;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Data : Callback_Data'Class;
      N : Positive)
      return Xref.General_Entity
   is
      Class : constant Class_Type := Get_Entity_Class (Get_Kernel (Data));
      Inst  : constant Class_Instance := Nth_Arg
        (Data, N, Class, Allow_Null => True);
      Props : Instance_Property;
   begin
      if Inst = No_Class_Instance then
         return No_General_Entity;
      end if;

      Props := Get_Data (Inst, Entity_Class_Name);
      if Props = null then
         return No_General_Entity;
      else
         return Entity_Properties_Record (Props.all).Entity;
      end if;
   end Get_Data;

   ----------------------
   -- Get_Entity_Class --
   ----------------------

   function Get_Entity_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type is
   begin
      return New_Class (Kernel.Scripts, Entity_Class_Name);
   end Get_Entity_Class;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is
   begin
      Register_Command
        (Kernel.Scripts, Constructor_Method,
         Minimum_Args => 1,
         Maximum_Args => 5,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "name",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "full_name",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "attributes",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_subprogram",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_generic",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_global",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_access",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_array",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_type",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "is_container",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "declaration",
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "body",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
      Register_Command
        (Kernel.Scripts, "end_of_scope",
         Minimum_Args => 0,
         Maximum_Args => 1,
         Class        => Get_Entity_Class (Kernel),
         Handler      => Entity_Command_Handler'Access);
   end Register_Commands;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Entity : Xref.General_Entity) is
   begin
      if not Is_Subclass (Instance, Entity_Class_Name) then
         raise Invalid_Data;
      end if;

      Ref (Entity);
      Set_Data
        (Instance, Entity_Class_Name,
         Entity_Properties_Record'(Entity => Entity));
   end Set_Data;

end GPS.Scripts.Entities;
