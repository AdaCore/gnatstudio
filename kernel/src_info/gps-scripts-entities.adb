------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2014, AdaCore                  --
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
with GNATCOLL.Projects;
with GNATCOLL.Xref;                    use GNATCOLL.Xref;
with Xref;                             use Xref;
with Basic_Types;                      use Basic_Types;

package body GPS.Scripts.Entities is

   type Entity_Properties_Record is new Instance_Property_Record with record
      H : Root_Entity_Ref;
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
      Entity : Xref.Root_Entity'Class)
      return Class_Instance
   is
      Instance : Class_Instance;
   begin
      if Entity = No_Root_Entity then
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
               Loc :=
                 (File    => GPS.Scripts.Files.Get_Data (File),
                  Project => GNATCOLL.Projects.No_Project,  --  ??? unknown
                  Line    => Nth_Arg (Data, 4, Default => -1),
                  Column  => Visible_Column_Type
                    (Nth_Arg (Data, 5, Default => -1)));
            end if;

            declare
               Ref : Root_Entity_Reference_Ref;
               Entity : constant Root_Entity'Class :=
                 Kernel.Databases.Find_Declaration_Or_Overloaded
                   (Loc               => Loc,
                    Entity_Name       => Name,
                    Ask_If_Overloaded => False,
                    Closest_Ref       => Ref,
                    Approximate_Search_Fallback => Approx_Search);
            begin
               if Entity = No_Root_Entity then
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
         end;

      elsif Command = "full_name" then
         Set_Return_Value (Data, Get_Data (Data, 1).Qualified_Name);

      elsif Command = "name" then
         Set_Return_Value (Data, Get_Data (Data, 1).Get_Name);

      elsif Command = "attributes" then
         --  ??? Should be made obsolete and replaced by separate functions.
         declare
            Entity : constant Root_Entity'Class := Get_Data (Data, 1);
            Subp   : constant Root_Entity'Class := Entity.Is_Parameter_Of;
         begin
            Set_Return_Value (Data, Entity.Is_Global);
            Set_Return_Value_Key (Data, "global");

            Set_Return_Value (Data, Entity.Is_Static_Local);
            Set_Return_Value_Key (Data, "static");

            if Subp /= No_Root_Entity then
               declare
                  Params : constant Parameter_Array := Subp.Parameters;
               begin
                  for P in Params'Range loop
                     if Root_Entity'Class (Params (P).Parameter) = Entity then
                        case Params (P).Kind is
                           when In_Parameter =>
                              Set_Return_Value (Data, True);
                              Set_Return_Value_Key (Data, "in");

                           when Out_Parameter =>
                              Set_Return_Value (Data, True);
                              Set_Return_Value_Key (Data, "out");

                           when In_Out_Parameter =>
                              Set_Return_Value (Data, True);
                              Set_Return_Value_Key (Data, "inout");

                           when Access_Parameter =>
                              Set_Return_Value (Data, True);
                              Set_Return_Value_Key (Data, "access");
                        end case;
                        exit;
                     end if;
                  end loop;
               end;
            end if;
         end;

      elsif Command = "is_subprogram" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Subprogram);

      elsif Command = "is_generic" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Generic);

      elsif Command = "is_global" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Global);

      elsif Command = "is_access" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Access);

      elsif Command = "is_array" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Array);

      elsif Command = "is_type" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Type);

      elsif Command = "is_container" then
         Set_Return_Value (Data, Get_Data (Data, 1).Is_Container);

      elsif Command = "declaration" then
         declare
            Location : General_Location;
         begin
            Location := Get_Data (Data, 1).Get_Declaration.Loc;

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
            Entity       : constant Root_Entity'Class := Get_Data (Data, 1);
         begin
            while Count > 0 loop
               Location := Entity.Get_Body (After => Cur_Location);
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
            Location := Get_Data (Data, 1).End_Of_Scope;
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
      return Xref.Root_Entity'Class
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
         return Entity_Properties_Record (Props.all).H.Element;
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
      Entity : Xref.Root_Entity'Class)
   is
      R : Entity_Properties_Record;
   begin
      if not Is_Subclass (Instance, Entity_Class_Name) then
         raise Invalid_Data;
      end if;

      Ref (Entity);

      R.H.Replace_Element (Entity);
      Set_Data (Instance, Entity_Class_Name, R);
   end Set_Data;

end GPS.Scripts.Entities;
