------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Symbols;                 use GNATCOLL.Symbols;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;               use GNATCOLL.VFS_Utils;

with GPS.Scripts;

with Language_Handlers;                use Language_Handlers;
with Language.Unknown; use Language.Unknown;

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
   begin
      GNATCOLL.Scripts.Destroy (Self.Scripts);

      --  Destroy the entities database after we have finalized the
      --  scripting languages, in case some class instances were still
      --  owning references to entities

      Standard.Xref.Destroy (Self.Database);

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

   ----------------------
   -- Get_Project_Tree --
   ----------------------

   overriding function Get_Project_Tree
     (Self : Core_Kernel_Record)
      return GNATCOLL.Projects.Project_Tree_Access
   is
      use type Projects.Project_Registry_Access;
   begin
      if Self.Registry = null then
         return null;
      else
         return Self.Registry.Tree;
      end if;
   end Get_Project_Tree;

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
         return Sequence.Last_Element;
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
         return Abstract_Module_List.Empty_List;
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

   ----------------------------
   -- Get_Construct_Database --
   ----------------------------

   function Get_Construct_Database
     (Kernel : not null access Core_Kernel_Record)
      return Language.Tree.Database.Construct_Database_Access
   is
   begin
      return Kernel.Databases.Constructs;
   end Get_Construct_Database;

   ----------------------------
   -- Register_Tree_Provider --
   ----------------------------

   procedure Register_Tree_Provider
     (Kernel   : not null access Core_Kernel_Record;
      Lang     : Language_Access;
      Provider : Semantic_Tree_Provider_Access)
   is
   begin
      Kernel.Semantic_Tree_Providers.Include (Lang, Provider);
   end Register_Tree_Provider;

   ------------------------------------
   -- Default_Language_Tree_Provider --
   ------------------------------------

   function Default_Language_Tree_Provider
     (Kernel : not null access Core_Kernel_Record)
      return Semantic_Tree_Provider_Access is (null);

   --------------------------------
   -- Get_Abstract_Tree_For_File --
   --------------------------------

   function Get_Abstract_Tree_For_File
     (Kernel  : not null access Core_Kernel_Record;
      Context : String;
      File    : GNATCOLL.VFS.Virtual_File) return Semantic_Tree'Class
   is
      Language : constant Language_Access :=
        Kernel.Lang_Handler.Get_Language_From_File (File);
   begin
      if Language = null or else Language = Unknown_Lang then

         --  Return nothing for common cases where the request has no meaning

         return No_Semantic_Tree;

      elsif not Kernel.Semantic_Tree_Providers.Contains (Language) then

         --  If there is no specific provider for Language, then use the
         --  default provider

         if Core_Kernel (Kernel).Default_Language_Tree_Provider /= null then
            return Core_Kernel (Kernel).Default_Language_Tree_Provider
              .Get_Tree_For_File (Context, File);
         else
            return No_Semantic_Tree;
         end if;

      else

         --  If there is a specific provider for Language, use that to return
         --  the file
         return Kernel.Semantic_Tree_Providers
           .Element (Language).Get_Tree_For_File (Context, File);
      end if;
   end Get_Abstract_Tree_For_File;

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

   procedure Initialize
     (Self    : not null access Core_Kernel_Record'Class;
      Symbols : GNATCOLL.Symbols.Symbol_Table_Access := null)
   is
      Handler : Language_Handler;

   begin
      if Symbols /= null then
         Self.Symbols := Symbols;
      else
         Self.Symbols := GNATCOLL.Symbols.Allocate;
      end if;
      Create_Handler (Handler, Self.Symbols);
      Self.Lang_Handler := Handler;
      Self.Create_Registry (Self.Registry);
      Set_Registry (Self.Lang_Handler, Self.Registry);
      Self.Registry.Tree.Load_Empty_Project (Env => Self.Registry.Environment);

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
