-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Toolchains.Known is

   --  ??? Move all this hard coded knowledge in XML files

   Tool_AAMP                    : aliased constant String := "aamp";
   Tool_E500V2_WRS_VXWORKS      : aliased constant String :=
     "e500v2-wrs-vxworks";
   Tool_E500V2_WRS_VXWORKSMILS  : aliased constant String :=
     "e500v2-wrs-vxworksmils";
   Tool_I586_WRS_VXWORKS        : aliased constant String :=
     "i586-wrs-vxworks";
   Tool_JVM                     : aliased constant String := "jvm";
   Tool_Dotnet                  : aliased constant String := "dotnet";
   Tool_POWERPC_WRS_VXWORKS     : aliased constant String :=
     "powerpc-wrs-vxworks";
   Tool_POWERPC_WRS_VXWORKSAE   : aliased constant String :=
     "powerpc-wrs-vxworksae";
   Tool_POWERPC_WRS_VXWORKSMILS : aliased constant String :=
     "powerpc-wrs-vxworksmils";

   Known_Toolchains : aliased constant String_List :=
     ( --  Bareboards
      new String'("erc32-elf"),
      new String'("leon-elf"),
      new String'("powerpc-eabispe"),
      new String'("powerpc-elf"),
      new String'("avr"),

      --  VxWorks platforms
      new String'(Tool_E500V2_WRS_VXWORKS),
      new String'(Tool_E500V2_WRS_VXWORKSMILS),
      new String'(Tool_I586_WRS_VXWORKS),
      new String'(Tool_POWERPC_WRS_VXWORKS),
      new String'(Tool_POWERPC_WRS_VXWORKSAE),
      new String'(Tool_POWERPC_WRS_VXWORKSMILS),

      --  Other cross
      new String'(Tool_AAMP),
      new String'(Tool_JVM),
      new String'(Tool_Dotnet),
      new String'("arm-mentor-nucleus"),
      new String'("powerpc-elf-lynxos"),
      new String'("powerpc-elf-pikeos"),
      new String'("powerpc-xcoff-lynxos"));

   -----------------------------
   -- Is_Known_Toolchain_Name --
   -----------------------------

   function Is_Known_Toolchain_Name (Name    : String) return Boolean is
   begin
      for J in Known_Toolchains'Range loop
         if Known_Toolchains (J).all = Name then
            return True;
         end if;
      end loop;

      return False;
   end Is_Known_Toolchain_Name;

   -------------------------------
   -- Get_Known_Toolchain_Names --
   -------------------------------

   function Get_Known_Toolchain_Names return String_List is
   begin
      return Known_Toolchains;
   end Get_Known_Toolchain_Names;

   --------------------------
   -- Has_Naming_Exception --
   --------------------------

   function Has_Naming_Exception (Name : String) return Boolean is
   begin
      return Name /= Tool_AAMP
        and then Name /= Tool_E500V2_WRS_VXWORKS
        and then Name /= Tool_POWERPC_WRS_VXWORKS
        and then Name /= Tool_POWERPC_WRS_VXWORKSAE
        and then Name /= Tool_POWERPC_WRS_VXWORKSMILS
        and then Name /= Tool_I586_WRS_VXWORKS;
   end Has_Naming_Exception;

   ------------------
   -- Tool_Command --
   ------------------

   function Tool_Command (Tc : String; Name : Valid_Tools) return String is
   begin
      --  ??? Remove this hard coded knowledge and use xml atrributes instead
      case Name is
         when CPP_Filt =>
            if Tc = Tool_AAMP
              or else Tc = Tool_JVM
              or else Tc = Tool_Dotnet
            then

               return "";

            elsif Tc = Tool_E500V2_WRS_VXWORKS
              or else Tc = Tool_POWERPC_WRS_VXWORKS
              or else Tc = Tool_POWERPC_WRS_VXWORKSAE
              or else Tc = Tool_POWERPC_WRS_VXWORKSMILS
            then

               return "c++filtppc";

            elsif Tc = Tool_I586_WRS_VXWORKS then

               return "c++filtpentium";

            else

               return Tc & "-c++filt";
            end if;

         when GNAT_Driver =>
            if Tc = Tool_AAMP then
               return "gnaampcmd";
            else
               return Tc & "-gnat";
            end if;

         when GNAT_List =>
            if Tc = Tool_AAMP then
               return "gnaampls";
            else
               return Tc & "-gnatls";
            end if;

         when Debugger =>
            if Tc = Tool_AAMP
              or else Tc = Tool_Dotnet
              or else Tc = Tool_JVM
            then
               return "";

            elsif Tc = Tool_POWERPC_WRS_VXWORKSAE then
               return "powerpc-wrs-vxworksae-gdb_wtx4";

            elsif Tc = Tool_POWERPC_WRS_VXWORKSMILS then
               return "powerpc-elf-gdb";

            else
               return Tc & "-gdb";
            end if;
      end case;
   end Tool_Command;

   -------------------------
   -- Is_Compiler_Defined --
   -------------------------

   function Is_Compiler_Defined (Tc : String; Lang : String) return Boolean is
      L : constant String := To_Lower (Lang);
   begin
      --  ??? Remove this hard coded knowledge and use XML attributes instead
      if L = "ada" then
         return True;
      elsif L = "c" then
         return Tc /= Tool_AAMP
           and then Tc /= Tool_Dotnet
           and then Tc /= Tool_JVM;
      else
         return False;
      end if;
   end Is_Compiler_Defined;

   ----------------------
   -- Compiler_Command --
   ----------------------

   function Compiler_Command (Tc : String; Lang : String) return String is
      L : constant String := To_Lower (Lang);
   begin
      --  ??? Remove this hard coded knowledge and use XML attributes instead

      if not Is_Compiler_Defined (Tc, L) then
         return "";
      else
         if L = "ada" then
            if Tc = Tool_AAMP then
               return "gnaampmake";
            else
               return Tc & "-gnatmake";
            end if;

         elsif L = "c" then
            if Tc = Tool_E500V2_WRS_VXWORKS
              or else Tc = Tool_POWERPC_WRS_VXWORKS
              or else Tc = Tool_POWERPC_WRS_VXWORKSAE
              or else Tc = Tool_POWERPC_WRS_VXWORKSMILS
            then
               return "ccppc";

            elsif Tc = Tool_I586_WRS_VXWORKS then
               return "ccpentium";

            else
               return Tc & "-gcc";

            end if;

         else
            return "";
         end if;
      end if;
   end Compiler_Command;

end Toolchains.Known;
