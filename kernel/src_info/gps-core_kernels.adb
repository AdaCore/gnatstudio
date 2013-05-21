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

with Ada.Strings.Hash;

with GNAT.OS_Lib;
with GNAT.Strings;                     use GNAT.Strings;

with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;               use GNATCOLL.VFS_Utils;

with GPS.Scripts;

with Language_Handlers;                use Language_Handlers;

package body GPS.Core_Kernels is

   ----------------------
   -- Create_From_Base --
   ----------------------

   function Create_From_Base
     (Kernel : access Core_Kernel_Record'Class;
      Name   : Filesystem_String) return Virtual_File
   is
      File : constant Virtual_File :=
        Kernel.Registry.Tree.Create (Base_Name (Name));
   begin
      if File = No_File then
         return Create (Full_Filename => Name);
      end if;

      return File;
   end Create_From_Base;

   -------------------------------
   -- Create_Scripts_Repository --
   -------------------------------

   procedure Create_Scripts_Repository
     (Self   : not null access Core_Kernel_Record;
      Result : out GNATCOLL.Scripts.Scripts_Repository) is
   begin
      Result := new GPS.Scripts.Kernel_Scripts_Repository'
        (GPS.Scripts.Create (Core_Kernel (Self)));
   end Create_Scripts_Repository;

   ---------------
   -- Databases --
   ---------------

   function Databases
     (Kernel : access Core_Kernel_Record'Class)
      return Xref.General_Xref_Database is
   begin
      return Kernel.Database;
   end Databases;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Core_Kernel_Record'Class) is
      Valgrind : String_Access := GNAT.OS_Lib.Getenv ("VALGRIND");
   begin
      --  Most of the rest if for the sake of memory leaks checkin, and since
      --  it can take a while for big projects we do not do this in normal
      --  times.
      if Valgrind.all /= ""
        and then Valgrind.all /= "no"
      then

         GNATCOLL.Scripts.Destroy (Self.Scripts);

         --  Destroy the entities database after we have finalized the
         --  scripting languages, in case some class instances were still
         --  owning references to entities

         Standard.Xref.Destroy (Self.Database);
      end if;

      Free (Valgrind);

      Projects.Destroy (Self.Registry);

      --  Remove the language handlers (used for xref). This needs to be done
      --  after finalizing the xref database, source_files contain a pointer
      --  to their language handler.

      Destroy (Self.Lang_Handler);

      GNATCOLL.Symbols.Free (Self.Symbols);
   end Destroy;

   --------------------
   -- Get_Build_Mode --
   --------------------

   function Get_Build_Mode
     (Self : not null access Core_Kernel_Record) return String
   is
      pragma Unreferenced (Self);
   begin
      return "default";
   end Get_Build_Mode;

   ----------
   -- Hash --
   ----------

   function Hash (Tag : Ada.Tags.Tag) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (Ada.Tags.External_Tag (Tag));
   end Hash;

   ------------------
   -- Lang_Handler --
   ------------------

   function Lang_Handler
     (Kernel : access Core_Kernel_Record'Class)
      return Language_Handlers.Language_Handler is
   begin
      return Kernel.Lang_Handler;
   end Lang_Handler;

   ------------
   -- Module --
   ------------

   function Module
     (Kernel : access Core_Kernel_Record'Class;
      Tag    : Ada.Tags.Tag) return Abstract_Module
   is
      use Abstract_Module_List;
      Sequence : constant List := Kernel.Module_List (Tag);
   begin
      if Is_Empty (Sequence) then
         return null;
      else
         return Data (Last (Sequence));
      end if;
   end Module;

   -----------------
   -- Module_List --
   -----------------

   function Module_List
     (Kernel : access Core_Kernel_Record'Class;
      Tag    : Ada.Tags.Tag) return Abstract_Module_List.List
   is
      Pos : constant Module_Maps.Cursor := Kernel.Modules.Find (Tag);
   begin
      if Module_Maps.Has_Element (Pos) then
         return Module_Maps.Element (Pos);
      else
         return Abstract_Module_List.Null_List;
      end if;
   end Module_List;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Core_Kernel_Record'Class;
      Module : not null Abstract_Module)
   is
      use Ada.Tags;

      Item : Ada.Tags.Tag := Module'Tag;
   begin
      while Item /= No_Tag loop
         declare
            Pos  : constant Module_Maps.Cursor := Kernel.Modules.Find (Item);
            List : Abstract_Module_List.List;
         begin
            if Module_Maps.Has_Element (Pos) then
               List := Module_Maps.Element (Pos);
            end if;

            Abstract_Module_List.Append (List, Module);
            Kernel.Modules.Include (Item, List);

            Item := Parent_Tag (Item);
         end;
      end loop;
   end Register_Module;

   --------------
   -- Registry --
   --------------

   function Registry
     (Kernel : access Core_Kernel_Record'Class)
      return Projects.Project_Registry_Access is
   begin
      return Kernel.Registry;
   end Registry;

   -------------
   -- Scripts --
   -------------

   function Scripts
     (Kernel : access Core_Kernel_Record'Class)
      return GNATCOLL.Scripts.Scripts_Repository is
   begin
      return Kernel.Scripts;
   end Scripts;

   -------------
   -- Symbols --
   -------------

   function Symbols
     (Kernel : access Core_Kernel_Record'Class)
      return GNATCOLL.Symbols.Symbol_Table_Access is
   begin
      return Kernel.Symbols;
   end Symbols;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : not null access Core_Kernel_Record'Class) is
      Handler : Language_Handler;
   begin
      Self.Symbols := GNATCOLL.Symbols.Allocate;
      Create_Handler (Handler, Self.Symbols);
      Self.Lang_Handler := Handler;
      Self.Create_Registry (Self.Registry);
      Self.Registry.Tree.Load_Empty_Project (Env => Self.Registry.Environment);

      Set_Registry (Self.Lang_Handler, Self.Registry);
      Self.Create_Database (Self.Database);
      Self.Create_Scripts_Repository (Self.Scripts);
   end Initialize;

   ----------------------------
   -- Get_Toolchains_Manager --
   ----------------------------

   function Get_Toolchains_Manager
     (Self : not null access Core_Kernel_Record)
      return Toolchains.Toolchain_Manager is
   begin
      return Self.Toolchains_Manager;
   end Get_Toolchains_Manager;

   ----------------------------
   -- Set_Toolchains_Manager --
   ----------------------------

   procedure Set_Toolchains_Manager
     (Self    : not null access Core_Kernel_Record;
      Manager : Toolchains.Toolchain_Manager) is
   begin
      Self.Toolchains_Manager := Manager;
   end Set_Toolchains_Manager;
end GPS.Core_Kernels;
