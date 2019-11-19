------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with GNATCOLL.Scripts;             use GNATCOLL.Scripts;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;

with GPS.Kernel.Scripts;           use GPS.Kernel.Scripts;
with GPS.Scripts;                  use GPS.Scripts;
with Memory_Usage_Views.Providers; use Memory_Usage_Views.Providers;

package body Memory_Usage_Views.Scripts is

   Me : constant Trace_Handle := Create ("GPS.MEMORY_USAGE.SCRIPTS");

   Memory_Usage_Provider_Class_Name : constant String := "MemoryUsageProvider";

   Provider_Task_Visitor_Class_Name : constant String :=
                                        "MemoryUsageProviderVisitor";

   -----------------------------------------
   -- Script Memory Usage Views Providers --
   -----------------------------------------

   type Memory_Usage_Provider_Proxy is new Script_Proxy with null record;
   overriding function Class_Name
     (Self : Memory_Usage_Provider_Proxy) return String
   is
     (Memory_Usage_Provider_Class_Name);

   type Script_Memory_Usage_Provider_Type is
     new Memory_Usage_Provider_Type with record
      Script    : Scripting_Language;
      Instances : Memory_Usage_Provider_Proxy;
   end record;
   type Script_Memory_Usage_Provider is
     access all Script_Memory_Usage_Provider_Type'Class;

   overriding function Is_Enabled
     (Self : not null access Script_Memory_Usage_Provider_Type) return Boolean;
   overriding procedure Async_Fetch_Memory_Usage_Data
     (Self    : not null access Script_Memory_Usage_Provider_Type;
      Visitor : Provider_Task_Visitor);

   --  Type representing a memory usage provider created from Python

   package Memory_Usage_Provider_Proxies is new Script_Proxies
     (Element_Type => Script_Memory_Usage_Provider,
      Proxy        => Memory_Usage_Provider_Proxy);

   function Create_Provider_Instance
     (Script   : access Scripting_Language_Record'Class;
      Provider : not null access Script_Memory_Usage_Provider_Type'Class)
      return Class_Instance;
   function Get_Provider
     (Inst   : Class_Instance)
      return not null access Script_Memory_Usage_Provider_Type'Class
     with Unreferenced;
   procedure Set_Provider_Instance
     (Provider : not null access Script_Memory_Usage_Provider_Type'Class;
      Inst     : Class_Instance);
   --  Bindings between Ada and Python types

   -----------------------
   -- Provider Visitors --
   -----------------------

   type Provider_Task_Properties_Record is new Instance_Property_Record
     with record
      Visitor : Provider_Task_Visitor;
   end record;

   procedure Call_Method
     (Self   : not null access Script_Memory_Usage_Provider_Type'Class;
      Method : String;
      Data   : in out Callback_Data'Class);
   --  Call Method on the python instance binded with Self, creating one if
   --  needed.
   --  Data is freed once the method is called.

   procedure Add_Visitor
     (Data    : in out Callback_Data'Class;
      Nth     : Integer;
      Visitor : not null access Provider_Task_Visitor_Type'Class);
   --  Create a new python instance for the given Visitor and set it as the
   --  Nth argument of Data.

   -----------------------------
   -- Script Command Handlers --
   -----------------------------

   procedure Static_Memory_Usage_Provider_Handler
     (Data : in out Callback_Data'Class; Command : String);

   procedure Provider_Task_Visitor_Handler
     (Data : in out Callback_Data'Class; Command : String);

   -----------------
   -- Call_Method --
   -----------------

   procedure Call_Method
     (Self   : not null access Script_Memory_Usage_Provider_Type'Class;
      Method : String;
      Data   : in out Callback_Data'Class)
   is
      Inst  : constant Class_Instance :=
                Create_Provider_Instance (Data.Get_Script, Self);
      F     : Subprogram_Type := Get_Method (Inst, Method);
      Dummy : Boolean;
   begin
      if F /= null then
         Trace (Me, "Executing method: " & Method);
         Dummy := F.Execute (Data);
         Free (F);
      end if;
      Free (Data);
   end Call_Method;

   -----------------
   -- Add_Visitor --
   -----------------

   procedure Add_Visitor
     (Data    : in out Callback_Data'Class;
      Nth     : Integer;
      Visitor : not null access Provider_Task_Visitor_Type'Class)
   is
      Script : constant Scripting_Language := Data.Get_Script;
      Inst : Class_Instance;
   begin
      --  First arg is the visitor
      Inst := Script.New_Instance
        (Script.Get_Repository.New_Class (Provider_Task_Visitor_Class_Name));
      Set_Data
        (Inst,
         Provider_Task_Visitor_Class_Name,
         Provider_Task_Properties_Record'
           (Visitor => Visitor.all'Unchecked_Access));
      Data.Set_Nth_Arg (Nth, Inst);
   end Add_Visitor;

   ----------------
   -- Is_Enabled --
   ----------------

   overriding function Is_Enabled
     (Self : not null access Script_Memory_Usage_Provider_Type) return Boolean
   is
      Inst   : constant Class_Instance :=
                 Create_Provider_Instance (Self.Script, Self);
      Data   : Callback_Data'Class := Create (Self.Script, 0);
      F      : constant Subprogram_Type := Get_Method (Inst, "is_enabled");
      Result : Boolean;
   begin
      if F /= null then
         Result := F.Execute (Data);
      end if;
      Free (Data);

      return Result;
   end Is_Enabled;

   -----------------------------------
   -- Async_Fetch_Memory_Usage_Data --
   -----------------------------------

   overriding procedure Async_Fetch_Memory_Usage_Data
     (Self    : not null access Script_Memory_Usage_Provider_Type;
      Visitor : Provider_Task_Visitor)
   is
      Data : Callback_Data'Class := Create (Self.Script, 1);
   begin
      Add_Visitor
        (Data    => Data,
         Nth     => 1,
         Visitor => Visitor);

      Call_Method
        (Self,
         Method => "async_fetch_memory_usage_data",
         Data   => Data);
   end Async_Fetch_Memory_Usage_Data;

   ------------------------------
   -- Create_Provider_Instance --
   ------------------------------

   function Create_Provider_Instance
     (Script   : access Scripting_Language_Record'Class;
      Provider : not null access Script_Memory_Usage_Provider_Type'Class)
      return Class_Instance is
   begin
      return Memory_Usage_Provider_Proxies.Get_Or_Create_Instance
        (Provider.Instances,
         Obj    => Script_Memory_Usage_Provider (Provider),
         Script => Script);
   end Create_Provider_Instance;

   ------------------
   -- Get_Provider --
   ------------------

   function Get_Provider
     (Inst   : Class_Instance)
      return not null access Script_Memory_Usage_Provider_Type'Class is
   begin
      return Memory_Usage_Provider_Proxies.From_Instance (Inst);
   end Get_Provider;

   ---------------------------
   -- Set_Provider_Instance --
   ---------------------------

   procedure Set_Provider_Instance
     (Provider : not null access Script_Memory_Usage_Provider_Type'Class;
      Inst     : Class_Instance) is
   begin
      Memory_Usage_Provider_Proxies.Store_In_Instance
        (Provider.Instances,
         Inst => Inst,
         Obj  => Script_Memory_Usage_Provider (Provider));
   end Set_Provider_Instance;

   ------------------------------------------
   -- Static_Memory_Usage_Provider_Handler --
   ------------------------------------------

   procedure Static_Memory_Usage_Provider_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "_register" then
         declare
            Name      : constant String := Data.Nth_Arg (1);
            Construct : Subprogram_Type := Data.Nth_Arg (2);
            Provider  : constant not null Script_Memory_Usage_Provider :=
                          new Script_Memory_Usage_Provider_Type;
            Inst      : Class_Instance;
            Args      : Callback_Data'Class := Data.Get_Script.Create (0);

         begin
            Inst := Construct.Execute (Args);  --  create the python instance
            Free (Construct);
            Free (Args);

            Provider.Script := Data.Get_Script;

            Set_Provider_Instance (Provider, Inst);
            Register_Provider (Kernel, Name, Provider);
         end;
      end if;
   end Static_Memory_Usage_Provider_Handler;

   -----------------------------------
   -- Provider_Task_Visitor_Handler --
   -----------------------------------

   procedure Provider_Task_Visitor_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Inst      : constant Class_Instance := Data.Nth_Arg (1);
      Prop_Inst : Instance_Property;
      Prop      : Provider_Task_Properties_Record;
   begin
      Prop_Inst := Get_Data (Inst, Provider_Task_Visitor_Class_Name);
      if Prop_Inst = null then
         return;
      end if;

      Prop := Provider_Task_Properties_Record (Prop_Inst.all);

      if Command = "on_memory_usage_data_fetched" then
         declare
            Regions_List  : constant List_Instance'Class := Data.Nth_Arg (2);
            Sections_List : constant List_Instance'Class := Data.Nth_Arg (3);
            Modules_List  : constant List_Instance'Class := Data.Nth_Arg (4);
            Regions       : Memory_Region_Description_Maps.Map;
         begin
            Trace (Me, "on_memory_usage_data_fetched has been called");

            for J in 1 .. Regions_List.Number_Of_Arguments loop
               declare
                  Current : constant List_Instance'Class :=
                              Regions_List.Nth_Arg (J);
                  Name    : constant String := Current.Nth_Arg (1);
               begin
                  Regions.Include
                    (Key      => Name,
                     New_Item => Memory_Region_Description'
                       (Name            => To_Unbounded_String (Name),
                        Origin          => Current.Nth_Arg (2),
                        Length          => Current.Nth_Arg (3),
                        Used_Size       => 0,
                        Sections        => <>));
               end;
            end loop;

            for J in 1 .. Sections_List.Number_Of_Arguments loop
               declare
                  Current     : constant List_Instance'Class :=
                                  Sections_List.Nth_Arg (J);
                  Name        : constant String := Current.Nth_Arg (1);
                  Region_Name : constant String := Current.Nth_Arg (4);
                  Length      : constant Integer := Current.Nth_Arg (3);
               begin
                  Regions (Region_Name).Sections.Include
                    (Key => Name,
                     New_Item => Memory_Section_Description'
                       (Name        => To_Unbounded_String (Name),
                        Origin      => Current.Nth_Arg (2),
                        Length      => Length,
                        Modules     => <>));

                  --  Calculate the used size of the memory region from the
                  --  contained memory sections.
                  Regions (Region_Name).Used_Size :=
                    Regions (Region_Name).Used_Size + Length;
               end;
            end loop;

            for J in 1 .. Modules_List.Number_Of_Arguments loop
               declare
                  Current      : constant List_Instance'Class :=
                                   Modules_List.Nth_Arg (J);
                  Region_Name  : constant String := Current.Nth_Arg (5);
                  Section_Name : constant String := Current.Nth_Arg (6);
               begin
                  Regions (Region_Name).Sections (Section_Name).Modules.Append
                    (Module_Description'
                       (Obj_File => Create
                           (Current.Nth_Arg (1), Normalize => True),
                        Lib_File => Create
                           (Current.Nth_Arg (2), Normalize => True),
                        Origin   => Current.Nth_Arg (3),
                        Size     => Current.Nth_Arg (4)));
               end;
            end loop;

            if Prop.Visitor /= null then
               Prop.Visitor.On_Memory_Usage_Data_Fetched (Regions);
               Free (Prop.Visitor);
            end if;
         end;
      end if;
   end Provider_Task_Visitor_Handler;

   ----------------------
   -- Register_Scripts --
   ----------------------

   procedure Register_Scripts
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Memory_Usage_Provider_Class : constant Class_Type :=
                                      Kernel.Scripts.New_Class
                                        (Memory_Usage_Provider_Class_Name);
      Provider_Task_Visitor_Class : constant Class_Type :=
                                      Kernel.Scripts.New_Class
                                        (Provider_Task_Visitor_Class_Name);
   begin
      Kernel.Scripts.Register_Command
        ("_register",
         Params        => (1 => Param ("name"),
                           2 => Param ("construct")),
         Static_Method => True,
         Class         => Memory_Usage_Provider_Class,
         Handler       => Static_Memory_Usage_Provider_Handler'Access);

      Kernel.Scripts.Register_Command
        ("on_memory_usage_data_fetched",
         Params        => (2 => Param ("regions"),
                           3 => Param ("sections"),
                           4 => Param ("modules")),
         Class         => Provider_Task_Visitor_Class,
         Handler       => Provider_Task_Visitor_Handler'Access);
   end Register_Scripts;

end Memory_Usage_Views.Scripts;
