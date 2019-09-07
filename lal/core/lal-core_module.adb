------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

with System.Storage_Elements;
with Interfaces.C;
with LAL.Switching_Tree_Providers; use LAL.Switching_Tree_Providers;
with Language.Ada;
with Libadalang.C;
with GNATCOLL.Python;
with GNATCOLL.Scripts.Python;      use GNATCOLL.Scripts.Python;
with GNATCOLL.Scripts;             use GNATCOLL.Scripts;
with GPS.Editors;
with GPS.Scripts;

package body LAL.Core_Module is

   Module : LAL_Module_Id;

   procedure Get_Analysis_Unit_Shell
     (Data    : in out Callback_Data'Class;
      Command : String);
   --  Execute 'get_analysis_unit' script command

   -----------------------------
   -- Get_Analysis_Unit_Shell --
   -----------------------------

   procedure Get_Analysis_Unit_Shell
     (Data    : in out Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);

      Kernel : constant GPS.Core_Kernels.Core_Kernel :=
        GPS.Scripts.Get_Kernel (Data);

      Python : constant Scripting_Language :=
        Kernel.Scripts.Lookup_Scripting_Language ("Python");

      Editor_Buffer_Class : constant Class_Type :=
        Kernel.Scripts.New_Class ("EditorBuffer");

      Instance : constant Class_Instance :=
        Nth_Arg (Data, 1, Editor_Buffer_Class);

      Buffer : constant GPS.Editors.Editor_Buffer'Class :=
        Kernel.Get_Buffer_Factory.Buffer_From_Instance (Instance);

      Unit   : Libadalang.Analysis.Analysis_Unit;
      Unit_C : System.Address;
      Int    : System.Storage_Elements.Integer_Address;
      Value  : GNATCOLL.Python.PyObject;
      Args   : Callback_Data'Class := Python.Create (1);
   begin
      Unit := Libadalang.Analysis.Get_From_Buffer
        (Context     => Module.Context,
         Filename    => Buffer.File.Display_Full_Name,
         Buffer      => Buffer.Get_Chars_U,
         Charset     => "UTF-8");

      Unit_C := Libadalang.C.C_Unit (Unit);
      Int := System.Storage_Elements.To_Integer (Unit_C);
      Value := GNATCOLL.Python.PyInt_FromSize_t (Interfaces.C.size_t (Int));
      Python_Callback_Data'Class (Args).Set_Nth_Arg (1, Value);

      Args.Execute_Command ("lal_utils._wrap_analysis_unit");
      Data.Set_Return_Value (Class_Instance'(Args.Return_Value));
   end Get_Analysis_Unit_Shell;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel     : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Config     : Use_LAL_Configuration;
      Doc_Before : Boolean;
      Legacy     : Language.Tree.Database.Tree_Language_Access;
      Charset    : String;
      Formater   : LAL.Semantic_Trees.Profile_Formater_Factory;
      Result     : out LAL_Module_Id)
   is
      Editor_Buffer_Class : constant Class_Type :=
        Kernel.Scripts.New_Class ("EditorBuffer");
   begin
      Module         := new LAL_Module_Id_Record;
      Module.Kernel  := GPS.Core_Kernels.Core_Kernel (Kernel);
      Module.Unit_Provider.Initialize (GPS.Core_Kernels.Core_Kernel (Kernel));

      Module.Reset_Context (Charset);

      Kernel.Scripts.Register_Command
        (Command => "get_analysis_unit",
         Class   => Editor_Buffer_Class,
         Handler => Get_Analysis_Unit_Shell'Access);

      if Config (Use_LAL_In_Indent) then
         Module.Lang.Initialize (Module.Kernel, Module.Context);
         Kernel.Lang_Handler.Register_Language (Module.Lang'Access, Legacy);
      end if;

      Kernel.Register_Tree_Provider
        (Language.Ada.Ada_Lang,
         new Provider'(Config => Config,
                       Nested => (Module.Kernel,
                                  Module.Context,
                                  Formater,
                                  Doc_Before)));

      Kernel.Register_Module (GPS.Core_Kernels.Abstract_Module (Module));
      Result := Module;
   end Register_Module;

   -------------------
   -- Reset_Context --
   -------------------

   not overriding procedure Reset_Context
     (Self    : in out LAL_Module_Id_Record;
      Charset : String) is
   begin
      Self.Context := Libadalang.Analysis.Create_Context
        (Unit_Provider => Libadalang.Analysis.Create_Unit_Provider_Reference
          (Self.Unit_Provider),
         With_Trivia   => True,
         Charset       => Charset);
   end Reset_Context;

   ----------------------------------
   -- Get_Current_Analysis_Context --
   ----------------------------------

   function Get_Current_Analysis_Context
     (Self : in out LAL_Module_Id_Record)
      return Libadalang.Analysis.Analysis_Context is
   begin
      return Self.Context;
   end Get_Current_Analysis_Context;

end LAL.Core_Module;
