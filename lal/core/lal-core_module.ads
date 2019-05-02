------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2019, AdaCore                   --
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
--  Main entry point for libAdaLang integration module (GUI independent part)

with GPS.Core_Kernels;
with Libadalang.Analysis;
with LAL.Unit_Providers;
with LAL.Ada_Languages;
with LAL.Semantic_Trees;
with Language.Tree.Database;

package LAL.Core_Module is

   type LAL_Module_Id_Record is
     new GPS.Core_Kernels.Abstract_Module_Record with record
      Kernel        : GPS.Core_Kernels.Core_Kernel;
      Context       : Libadalang.Analysis.Analysis_Context;
      Unit_Provider : aliased LAL.Unit_Providers.Unit_Provider;
      Lang          : aliased LAL.Ada_Languages.Ada_Language;
   end record;

   type LAL_Module_Id is access all LAL_Module_Id_Record'Class;

   not overriding procedure Reset_Context
     (Self    : in out LAL_Module_Id_Record;
      Charset : String);
   --  Recreate LAL context, use Charset by default

   function Get_Current_Analysis_Context
     (Self : in out LAL_Module_Id_Record)
      return Libadalang.Analysis.Analysis_Context;
   --  Return the current LAL context

   procedure Register_Module
     (Kernel     : access GPS.Core_Kernels.Core_Kernel_Record'Class;
      Config     : Use_LAL_Configuration;
      Doc_Before : Boolean;
      Legacy     : Language.Tree.Database.Tree_Language_Access;
      Charset    : String;
      Formater   : LAL.Semantic_Trees.Profile_Formater_Factory;
      Result     : out LAL_Module_Id);
   --  Register module. Charset is default charset for reading files.

end LAL.Core_Module;
