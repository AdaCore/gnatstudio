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

with Basic_Types;                      use Basic_Types;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with GNATCOLL.Xref;                    use GNATCOLL.Xref;
with GPS.Core_Kernels;                 use GPS.Core_Kernels;
with GPS.Intl;                         use GPS.Intl;
with GPS.Scripts.File_Locations;
with GPS.Scripts.Files;

package body GPS.Scripts.Entities is

   type Entity_Properties_Record is new Instance_Property_Record with record
      H : Root_Entity_Ref;
   end record;

   Entity_Class_Name        : constant String := "Entity";

   --  Name of the class for shell commands associated with this package

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
      function Get_Body_N (Entity : Root_Entity'Class) return General_Location;
      --  Get body for an entity

      --------------
      -- Get_Body --
      --------------

      function Get_Body_N
        (Entity : Root_Entity'Class) return General_Location
      is
         Location     : General_Location := No_Location;
         Cur_Location : General_Location := No_Location;
         Count        : Integer := Nth_Arg (Data, 2, 1);
      begin
         while Count > 0 loop
            Location := Entity.Get_Body (After => Cur_Location);
            Count := Count - 1;
            Cur_Location := Location;
         end loop;

         return Location;
      end Get_Body_N;

      Kernel    : constant Core_Kernel := Get_Kernel (Data);
      Entity    : constant Root_Entity'Class := Get_Data (Data, 1);
   begin
      if Command = Constructor_Method then
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
                  Project_Path => GNATCOLL.VFS.No_File,  --  ??? unknown
                  Line    => Nth_Arg (Data, 4, Default => -1),
                  Column  => Visible_Column_Type
                    (Nth_Arg (Data, 5, Default => -1)));
            end if;

            declare
               Ref : Root_Entity_Reference_Ref;
               Entity : constant Root_Entity'Class :=
                 Kernel.Databases.Get_Entity
                   (Loc               => Loc,
                    Name              => Name,
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
         Set_Return_Value (Data, Entity.Qualified_Name);

      elsif Command = "name" then
         Set_Return_Value (Data, Entity.Get_Name);

      elsif Command = "category" then
         Set_Return_Value (Data, Entity.Get_Display_Kind);

      elsif Command = "attributes" then
         --  ??? Should be made obsolete and replaced by separate functions.
         declare
            Subp   : constant Root_Entity'Class := Entity.Is_Parameter_Of;
         begin
            Set_Return_Value (Data, Entity.Is_Global);
            Set_Return_Value_Key (Data, "global");

            Set_Return_Value (Data, Entity.Is_Static_Local);
            Set_Return_Value_Key (Data, "static");

            if Subp /= No_Root_Entity then
               declare
                  Params : Parameter_Array := Subp.Parameters;
               begin
                  for P in Params'Range loop
                     if Params (P).Parameter.all = Entity then
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

                  Free (Params);
               end;
            end if;
         end;

      elsif Command = "is_subprogram" then
         Set_Return_Value (Data, Entity.Is_Subprogram);

      elsif Command = "is_generic" then
         Set_Return_Value (Data, Entity.Is_Generic);

      elsif Command = "is_global" then
         Set_Return_Value (Data, Entity.Is_Global);

      elsif Command = "is_access" then
         Set_Return_Value (Data, Entity.Is_Access);

      elsif Command = "is_array" then
         Set_Return_Value (Data, Entity.Is_Array);

      elsif Command = "is_type" then
         Set_Return_Value (Data, Entity.Is_Type);

      elsif Command = "overrides" then
         Set_Return_Value
           (Data, Create_Entity (Get_Script (Data), Entity.Overrides));

      elsif Command = "is_container" then
         Set_Return_Value (Data, Entity.Is_Container);

      elsif Command = "declaration" then
         declare
            Location : constant General_Location :=
              Entity.Get_Declaration.Loc;
         begin
            Set_Return_Value
              (Data, GPS.Scripts.File_Locations.Create_File_Location
                 (Get_Script (Data),
                  File   => GPS.Scripts.Files.Create_File
                              (Get_Script (Data), Location.File),
                  Line   => Location.Line,
                  Column => Location.Column));
         end;

      elsif Command = "body" then
         declare
            Location : constant General_Location := Get_Body_N (Entity);
         begin
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
            Location := Entity.End_Of_Scope;
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

      elsif Command = "discriminants" then
         declare
            Discrs : Xref.Entity_Array := Discriminants (Entity);
         begin
            Set_Return_Value_As_List (Data);

            for D in Discrs'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), Discrs (D).all));
            end loop;

            Free (Discrs);
         end;

      elsif Command = "has_body" then
         declare
            Location : constant General_Location := Get_Body_N (Entity);
         begin
            Set_Return_Value (Data, Location /= No_Location);
         end;

      elsif Command = "parameters" then
         declare
            Params : Parameter_Array := Parameters (Entity);
         begin
            Set_Return_Value_As_List (Data);
            for P in Params'Range loop
               Set_Return_Value
                 (Data, Create_Entity
                    (Get_Script (Data), Params (P).Parameter.all));
            end loop;
            Free (Params);
         end;

      elsif Command = "methods" then
         declare
            Methods : Xref.Entity_Array :=
              Entity.Methods
                (Include_Inherited => Nth_Arg (Data, 2, False));
         begin
            Set_Return_Value_As_List (Data);

            for M in Methods'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), Methods (M).all));
            end loop;

            Free (Methods);
         end;

      elsif Command = "return_type" then
         Set_Return_Value
           (Data, Create_Entity (Get_Script (Data), Entity.Returned_Type));

      elsif Command = "primitive_of" then
         declare
            Arr : Xref.Entity_Array := Entity.Is_Primitive_Of;
         begin
            Set_Return_Value_As_List (Data);
            for A in Arr'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), Arr (A).all));
            end loop;

            Free (Arr);
         end;

      elsif Command = "pointed_type" then
         declare
            Result : constant Root_Entity'Class := Entity.Pointed_Type;
         begin
            if Result = No_Root_Entity then
               declare
                  Res_2 : constant Root_Entity'Class :=
                    Entity.Get_Type_Of.Pointed_Type;
               begin
                  Set_Return_Value
                    (Data, Create_Entity (Get_Script (Data),
                     Res_2));
               end;
            else
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data),
                  Result));
            end if;
         end;

      elsif Command = "type" then
         Set_Return_Value
           (Data, Create_Entity (Get_Script (Data), Entity.Get_Type_Of));

      elsif Command = "instance_of" then
         Set_Return_Value
           (Data, Create_Entity (Get_Script (Data), Entity.Instance_Of));

      elsif Command = "is_predefined" then
         Set_Return_Value
           (Data, Entity.Is_Predefined_Entity);

      elsif Command = "fields" then
         declare
            F : Xref.Entity_Array := Entity.Fields;
         begin
            Set_Return_Value_As_List (Data);

            for F2 in F'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), F (F2).all));
            end loop;

            Free (F);
         end;

      elsif Command = "literals" then
         declare
            F : Xref.Entity_Array := Entity.Literals;
         begin
            Set_Return_Value_As_List (Data);

            for F2 in F'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), F (F2).all));
            end loop;

            Free (F);
         end;

      elsif Command = "derived_types" then
         declare
            Children : Xref.Entity_Array :=
              Entity.Child_Types (Recursive => False);
         begin
            Set_Return_Value_As_List (Data);

            for C in Children'Range loop
               Set_Return_Value
                 (Data, Create_Entity
                    (Get_Script (Data), Children (C).all));
            end loop;

            Free (Children);
         end;

      elsif Command = "get_called_entities" then
         declare
            Called_Entities : Abstract_Entities_Cursor'Class :=
              Entity.Get_All_Called_Entities;
         begin
            Set_Return_Value_As_List (Data);
            while not Called_Entities.At_End loop
               Set_Return_Value
                 (Data,
                  Create_Entity (Get_Script (Data), Called_Entities.Get));
               Called_Entities.Next;
            end loop;

            Called_Entities.Destroy;
         end;

      elsif Command = "parent_types" then
         declare
            Parents : Xref.Entity_Array :=
              Entity.Parent_Types (Recursive => Nth_Arg (Data, 2, False));
         begin
            Set_Return_Value_As_List (Data);

            for C in Parents'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), Parents (C).all));
            end loop;

            Free (Parents);
         end;
      elsif Command = "child_types" then
         declare
            Parents : Xref.Entity_Array :=
              Entity.Child_Types (Recursive => Nth_Arg (Data, 2, False));
         begin
            Set_Return_Value_As_List (Data);

            for C in Parents'Range loop
               Set_Return_Value
                 (Data, Create_Entity (Get_Script (Data), Parents (C).all));
            end loop;

            Free (Parents);
         end;
      elsif Command = "requires_body" then
         Set_Return_Value (Data, Entity.Requires_Body);

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
      Kernel : constant Core_Kernel := Get_Kernel (Data);
      Class : constant Class_Type := Get_Entity_Class (Kernel);
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
      C : constant Class_Type := Get_Entity_Class (Kernel);
   begin
      Kernel.Scripts.Register_Command
        (Constructor_Method,
         Class        => C,
         Params       => (Param ("name"),
                          Param ("file",   Optional => True),
                          Param ("line",   Optional => True),
                          Param ("column", Optional => True),
                          Param ("approximate_search_fallback", True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("name",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("full_name",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("attributes",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_subprogram",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_generic",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_global",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_access",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_array",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_type",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("overrides",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_container",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("declaration",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("body",
         Class        => C,
         Params       => (1 => Param ("nth", Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("end_of_scope",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("category",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("discriminants",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("fields",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("literals",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("is_predefined",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("parameters",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("methods",
         Class        => C,
         Params       => (2 => Param ("include_inherited", Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("return_type",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("pointed_type",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("derived_types",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("parent_types",
         Class        => C,
         Params       => (2 => Param ("recursive", Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("child_types",
         Class        => C,
         Params       => (2 => Param ("recursive", Optional => True)),
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("get_called_entities",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("instance_of",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("primitive_of",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("type",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("requires_body",
         Class        => C,
         Handler      => Entity_Command_Handler'Access);
      Kernel.Scripts.Register_Command
        ("has_body",
         Class        => C,
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

      R.H.Replace_Element (Entity);
      Set_Data (Instance, Entity_Class_Name, R);
   end Set_Data;

end GPS.Scripts.Entities;
