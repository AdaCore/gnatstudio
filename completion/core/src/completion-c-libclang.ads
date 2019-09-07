------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2014-2019, AdaCore                     --
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

--  A completion resolver based on libclang

with GPS.Kernel;     use GPS.Kernel;
with Libclang.Index; use Libclang.Index;
with Array_Utils;

package Completion.C.Libclang is

   type Libclang_Resolver is new Completion_Resolver with private;

   function New_Libclang_Completion_Resolver
     (Kernel       : Kernel_Handle;
      Current_File : Virtual_File) return Completion_Resolver_Access;
   --  Create a new resolver based on the analyzed file

   overriding
   procedure Get_Completion_Root
     (Resolver : access Libclang_Resolver;
      Offset   : String_Index_Type;
      Context  : Completion_Context;
      Result   : in out Completion_List);
   --  See inherited documentation

   overriding
   function Get_Id (Resolver : Libclang_Resolver) return String;
   --  See inherited documentation

   overriding procedure Free (This : in out Libclang_Resolver);
   --  Free the data associated to a construct completion resolver

private

   package Completion_Results_Arrays
   is new Array_Utils (Clang_Completion_Result);
   subtype Completion_Results_Array is Completion_Results_Arrays.Array_Type;
   subtype Completion_Results_Array_Access
     is Completion_Results_Arrays.Array_Type_Access;

   type Libclang_Resolver is new Completion_Resolver with record
      Kernel             : Kernel_Handle;
      TU                 : Clang_Translation_Unit := No_Translation_Unit;
      Completions        : Clang_Complete_Results := No_Complete_Results;
      Completions_Array  : Completion_Results_Array_Access;
      Prefix             : String_Access;
      Unsaved_File_Inst  : Unsaved_File;
   end record;

end Completion.C.Libclang;
