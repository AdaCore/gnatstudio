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

with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Regpat;       use GNAT.Regpat;
with GNATCOLL.Utils;    use GNATCOLL.Utils;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions; use Ada.Exceptions;

package body Toolchains is

   procedure Free (This : in out Ada_Library_Info);
   --  Free the memory associated to this library info. This should only be
   --  called by the manager, as we store the result of this information during
   --  the session.

   procedure Fire_Change_Event (This : Toolchain_Manager);
   --  Calls the Toolchain_Changed event on all listeners.

   ---------------------
   -- Get_Source_Path --
   ---------------------

   function Get_Source_Path (This : Ada_Library_Info) return File_Array is
   begin
      if This.Source_Path /= null then
         return This.Source_Path.all;
      else
         return (1 .. 0 => <>);
      end if;
   end Get_Source_Path;

   ----------------------
   -- Get_Objects_Path --
   ----------------------

   function Get_Objects_Path (This : Ada_Library_Info) return File_Array is
   begin
      if This.Objects_Path /= null then
         return This.Objects_Path.all;
      else
         return (1 .. 0 => <>);
      end if;
   end Get_Objects_Path;

   ----------------------
   -- Get_Project_Path --
   ----------------------

   function Get_Project_Path (This : Ada_Library_Info) return File_Array is
   begin
      if This.Project_Path /= null then
         return This.Project_Path.all;
      else
         return (1 .. 0 => <>);
      end if;
   end Get_Project_Path;

   -----------------
   -- Get_Version --
   -----------------

   function Get_Version (This : Ada_Library_Info) return String is
   begin
      if This.Version /= null then
         return This.Version.all;
      else
         return "";
      end if;
   end Get_Version;

   ---------------
   -- Get_Error --
   ---------------

   function Get_Error (This : Ada_Library_Info) return String is
   begin
      if This.Version /= null then
         return This.Version.all;
      else
         return "";
      end if;
   end Get_Error;

   ----------------------
   -- Get_Install_Path --
   ----------------------

   function Get_Install_Path (This : Ada_Library_Info) return Virtual_File is
   begin
      return This.Install_Path;
   end Get_Install_Path;

   ----------------
   -- Has_Errors --
   ----------------

   function Has_Errors (This : Ada_Library_Info) return Boolean is
   begin
      return This.Error /= null;
   end Has_Errors;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ada_Library_Info) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ada_Library_Info_Record, Ada_Library_Info);
   begin
      Unchecked_Free (This.Source_Path);
      Unchecked_Free (This.Objects_Path);
      Unchecked_Free (This.Project_Path);
      Free (This.Version);
      Free (This.Error);
      Free (This);
   end Free;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (This : Toolchain; Manager : Toolchain_Manager)
   is
   begin
      if This.Is_Computed then
         return;
      end if;

      This.Is_Computed := True;

      This.Library :=
        Get_Library_Information (Manager, Get_Command (This, GNAT_List));

      if This.Library.Error /= null then
         This.Is_Valid := False;
      else
         This.Is_Valid := True;
      end if;
   end Compute_Predefined_Paths;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (This : Toolchain; Name : Tool_Names) return String
   is
      function Base_Tool_Name return String;

      function Base_Tool_Name return String is
      begin
         case Name is
            when Ada_Compiler =>
               return "gnatmake";

            when C_Compiler =>
               return "gcc";

            when GNAT_List =>
               return "gnatls";

            when GNAT_Driver =>
               return "gnat";

            when Debugger =>
               return "gdb";

            when CPP_Filt =>
               return "c++filt";

            when Unknown =>
               return "";
         end case;

      end Base_Tool_Name;

   begin
      if This.Tool_Commands (Name) = null then

         if Name = Unknown then
            return "";
         end if;

         if This.Is_Native then
            return Base_Tool_Name;
         else
            return This.Name.all & "-" & Base_Tool_Name;
         end if;
      else
         return This.Tool_Commands (Name).all;
      end if;
   end Get_Command;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (This : Toolchain; Name : Tool_Names; Value : String)
   is
   begin
      Free (This.Tool_Commands (Name));
      This.Tool_Commands (Name) := new String'(Value);
   end Set_Command;

   ---------------------
   -- Is_Simple_Cross --
   ---------------------

   function Is_Simple_Cross (This : Toolchain) return Boolean is
   begin
      return not
        (This.Is_Native
         or else This.Is_Custom
         or else This.Name.all = Tool_AAMP
         or else This.Name.all = Tool_POWERPC_WRS_VXWORKSAE
         or else This.Name.all = Tool_POWERPC_WRS_VXWORKSMILS);
   end Is_Simple_Cross;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : Toolchain) return String is
   begin
      return This.Name.all;
   end Get_Name;

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name (This : Toolchain; Name : String) is
   begin
      Free (This.Name);
      This.Name := new String'(Name);
   end Set_Name;

   ----------
   -- Copy --
   ----------

   function Copy (This : Toolchain) return Toolchain is
      Result : Toolchain;
   begin
      Result := new Toolchain_Record'(This.all);

      if Result.Name /= null then
         Result.Name := new String'(Result.Name.all);
      end if;

      if Result.Label /= null then
         Result.Label := new String'(Result.Label.all);
      end if;

      for J in Result.Tool_Commands'Range loop
         if Result.Tool_Commands (J) /= null then
            Result.Tool_Commands (J) :=
              new String'(Result.Tool_Commands (J).all);
         end if;
      end loop;

      return Result;
   end Copy;

   ---------------
   -- Is_Custom --
   ---------------

   function Is_Custom (This : Toolchain) return Boolean is
   begin
      return This.Is_Custom;
   end Is_Custom;

   ---------------
   -- Is_Native --
   ---------------

   function Is_Native (This : Toolchain) return Boolean is
   begin
      return This.Is_Native;
   end Is_Native;

   -----------------
   -- Is_Computed --
   -----------------

   function Is_Computed (This : Toolchain) return Boolean is
   begin
      return This.Is_Computed;
   end Is_Computed;

   ----------------
   -- Set_Custom --
   ----------------

   procedure Set_Custom (This : Toolchain; Value : Boolean) is
   begin
      This.Is_Custom := Value;
   end Set_Custom;

   ----------------
   -- Set_Native --
   ----------------

   procedure Set_Native (This : Toolchain; Value : Boolean) is
   begin
      This.Is_Native := Value;
   end Set_Native;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Toolchain) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Toolchain_Record, Toolchain);
   begin
      Free (This.Label);
      Free (This.Name);

      for J in This.Tool_Commands'Range loop
         Free (This.Tool_Commands (J));
      end loop;

      Free (This);
   end Free;

   -----------------------------
   -- Get_Library_Information --
   -----------------------------

   function Get_Library_Information
     (This : Toolchain) return Ada_Library_Info
   is
   begin
      --  There intentionally no call to compute here - should use whatever
      --  information is available without long process.

      return This.Library;
   end Get_Library_Information;

   ----------------------------
   -- Create_Known_Toolchain --
   ----------------------------

   function Create_Known_Toolchain (Name : String) return Toolchain is
      Result : constant Toolchain := new Toolchain_Record;
   begin
      Result.Name := new String'(Name);

      --  Set c++filt

      if Name = Tool_AAMP then
         Result.Tool_Commands (CPP_Filt) := new String'("");
      elsif Name = Tool_E500V2_WRS_VXWORKS
        or else Name = Tool_POWERPC_WRS_VXWORKS
        or else Name = Tool_POWERPC_WRS_VXWORKSAE
        or else Name = Tool_POWERPC_WRS_VXWORKSMILS
      then
         Result.Tool_Commands (CPP_Filt) := new String'("c++filtppc");
      elsif Name = Tool_I586_WRS_VXWORKS then
         Result.Tool_Commands (CPP_Filt) := new String'("c++filtpentium");
      end if;

      --  Set c compiler

      if Name = Tool_E500V2_WRS_VXWORKS
        or else Name = Tool_POWERPC_WRS_VXWORKS
        or else Name = Tool_POWERPC_WRS_VXWORKSAE
        or else Name = Tool_POWERPC_WRS_VXWORKSMILS
      then
         Result.Tool_Commands (C_Compiler) := new String'("ccppc");
      elsif Name = Tool_I586_WRS_VXWORKS then
         Result.Tool_Commands (C_Compiler) := new String'("ccpentium");
      end if;

      --  Set other tools

      if Name = Tool_AAMP then
         Result.Tool_Commands (GNAT_List) := new String'("gnaampls");
         Result.Tool_Commands (Ada_Compiler) := new String'("gnaampmake");
         Result.Tool_Commands (GNAT_Driver) := new String'("gnaampcmd");
         Result.Tool_Commands (C_Compiler) := new String'("");
         Result.Tool_Commands (Debugger) := new String'("");
      elsif Name = Tool_POWERPC_WRS_VXWORKSAE then
         Result.Tool_Commands (Debugger) := new String'
           ("powerpc-wrs-vxworksae-gdb_wtx4");
      elsif Name = Tool_POWERPC_WRS_VXWORKSMILS then
         Result.Tool_Commands (Debugger) := new String'("powerpc-elf-gdb");
      end if;

      return Result;
   end Create_Known_Toolchain;

   -----------------------------
   -- Is_Known_Toolchain_Name --
   -----------------------------

   function Is_Known_Toolchain_Name (Name : String) return Boolean is
   begin
      for J in Known_Toolchains'Range loop
         if Known_Toolchains (J).all = Name then
            return True;
         end if;
      end loop;

      return False;
   end Is_Known_Toolchain_Name;

   --------------------------
   -- Get_Native_Toolchain --
   --------------------------

   function Get_Native_Toolchain (This : Toolchain_Manager) return Toolchain is
      use Toolchain_Maps;

      Cur : Toolchain_Maps.Cursor;
      Native_Toolchain : Toolchain;
   begin
      if This.No_Native_Toolchain then
         return null;
      end if;

      Cur := This.Toolchains.First;

      while Cur /= Toolchain_Maps.No_Element loop
         if Element (Cur).Is_Native then
            return Element (Cur);
         end if;

         Cur := Next (Cur);
      end loop;

      --  If no native toolchain has been found, then create one.

      Native_Toolchain := new Toolchain_Record'
        (Name          => new String'("native"),
         Label         => new String'("native"),
         Is_Native     => True,
         Is_Custom     => False,
         Tool_Commands => (others => null),
         Is_Computed   => False,
         Is_Valid      => False,
         Library       => null);

      Compute_Predefined_Paths (Native_Toolchain, This);

      if Native_Toolchain.Is_Valid then
         Add_Toolchain (This, Native_Toolchain);

         return Native_Toolchain;
      else
         --  set the flag, so that we don't try to analyze the native
         --  toolchain later on

         This.No_Native_Toolchain := True;

         return null;
      end if;
   end Get_Native_Toolchain;

   -------------------
   -- Get_Toolchain --
   -------------------

   function Get_Toolchain
     (This : Toolchain_Manager; Name : String) return Toolchain is
   begin
      --  Case 1, the toolchain is already live in the manager

      if This.Toolchains.Contains (Name) then
         return This.Toolchains.Element (Name);
      end if;

      --  Case 2, the toolchain is known. Create a known one

      if Is_Known_Toolchain_Name (Name) then
         declare
            Result : constant Toolchain := Create_Known_Toolchain (Name);
         begin
            Compute_Predefined_Paths (Result, This);
            Add_Toolchain (This, Result);
            return Result;
         end;
      end if;

      --  Case 3, the toolchain contains the string "native", return the
      --  native one

      if Index (Name, "native") in Name'Range then
         return Get_Native_Toolchain (This);
      end if;

      --  Otherwise, the toolchain can't be retreived, return null

      return null;
   end Get_Toolchain;

   -----------------------
   -- Compute_Toolchain --
   -----------------------

   function Compute_Toolchain
     (This : Toolchain_Manager; Project : Project_Type) return Toolchain
   is
      GNAT_List_Str    : constant String :=
        Attribute_Value (Project, Build ("ide", "gnatlist"), "");
      GNAT_Driver_Str  : constant String :=
        Attribute_Value (Project, Build ("ide", "gnat"), "");
      Ada_Compiler_Str : constant String :=
        Attribute_Value (Project, Build ("ide", "compiler_command"), "ada");
      C_Compiler_Str   : constant String :=
        Attribute_Value (Project, Build ("ide", "compiler_command"), "c");
      Debugger_Str     : constant String :=
        Attribute_Value (Project, Build ("ide", "debugger_command"), "");

      The_Toolchain : Toolchain;
   begin
      if GNAT_List_Str /= "" then
         The_Toolchain := Compute_Toolchain_From_Tool
           (This, GNAT_List_Str, GNAT_List);
      end if;

      if The_Toolchain = null and then Ada_Compiler_Str /= "" then
         The_Toolchain := Compute_Toolchain_From_Tool
           (This, Ada_Compiler_Str, Ada_Compiler);
      end if;

      if The_Toolchain = null then
         The_Toolchain := new Toolchain_Record;
      else
         The_Toolchain := Copy (The_Toolchain);
      end if;

      if GNAT_List_Str /= "" then
         Free (The_Toolchain.Tool_Commands (GNAT_List));
         The_Toolchain.Tool_Commands (GNAT_List) :=
           new String'(GNAT_List_Str);
      end if;

      if GNAT_Driver_Str /= "" then
         Free (The_Toolchain.Tool_Commands (GNAT_Driver));
         The_Toolchain.Tool_Commands (GNAT_Driver) :=
           new String'(GNAT_Driver_Str);
      end if;

      if Ada_Compiler_Str /= "" then
         Free (The_Toolchain.Tool_Commands (Ada_Compiler));
         The_Toolchain.Tool_Commands (Ada_Compiler) :=
           new String'(Ada_Compiler_Str);
      end if;

      if C_Compiler_Str /= "" then
         Free (The_Toolchain.Tool_Commands (C_Compiler));
         The_Toolchain.Tool_Commands (C_Compiler) :=
           new String'(C_Compiler_Str);
      end if;

      if Debugger_Str /= "" then
         Free (The_Toolchain.Tool_Commands (Debugger));
         The_Toolchain.Tool_Commands (Debugger) :=
           new String'(Debugger_Str);
      end if;

      return The_Toolchain;
   end Compute_Toolchain;

   ---------------------------------
   -- Compute_Toolchain_From_Tool --
   ---------------------------------

   function Compute_Toolchain_From_Tool
     (This : Toolchain_Manager;
      Name : String;
      Tool : Tool_Names) return Toolchain
   is
      Name_Index    : Integer := Name'Last;
      Subname_Index : Integer;
      Has_Prefix    : Boolean := False;
   begin
      for J in Name'Range loop
         if Name (J) = ' ' then
            Name_Index := J - 1;
            exit;
         end if;
      end loop;

      Subname_Index := Name_Index;

      for J in Name'First .. Name_Index loop
         if Name (J) = '-' then
            Subname_Index := J - 1;
            Has_Prefix := True;
            exit;
         end if;
      end loop;

      declare
         use Toolchain_Maps;

         Name_Before_Params : constant String := Name
           (Name'First .. Name_Index);
         Subname            : constant String := Name
           (Name'First .. Subname_Index);

         Cur : Toolchain_Maps.Cursor;
      begin
         if Has_Prefix then
            return Get_Toolchain (This, Subname);
         elsif Name_Before_Params = ""
           or else Name_Before_Params = "gnatls"
           or else Name_Before_Params = "gnatmake"
         then
            return Get_Native_Toolchain (This);
         elsif Name_Before_Params = "gnaampls"
           or else Name_Before_Params = "gnaampmake"
         then
            return Get_Toolchain (This, Tool_AAMP);
         end if;

         --  If we couln't find the toolchain with a know pattern, look at
         --  custom toolchains

         Cur := This.Toolchains.First;

         while Cur /= Toolchain_Maps.No_Element loop
            if Element (Cur).Is_Custom then
               if Element (Cur).Tool_Commands (Tool).all
                 = Name_Before_Params
               then
                  return Element (Cur);
               end if;
            end if;
         end loop;
      end;

      --  No toolchain can be found, return null

      return null;
   end Compute_Toolchain_From_Tool;

   -------------------
   -- Add_Toolchain --
   -------------------

   procedure Add_Toolchain
     (This          : Toolchain_Manager;
      Ada_Toolchain : Toolchain)
   is
   begin
      if This.Toolchains.Contains (Ada_Toolchain.Name.all) then
         raise Toolchain_Exception with "Toolchain "
           & Ada_Toolchain.Name.all & " already registered";
      end if;

      Compute_Predefined_Paths (Ada_Toolchain, This);
      This.Toolchains.Insert (Ada_Toolchain.Name.all, Ada_Toolchain);

      Fire_Change_Event (This);
   end Add_Toolchain;

   ----------------------
   -- Modify_Toolchain --
   ----------------------

   procedure Modify_Toolchain
     (This          : Toolchain_Manager;
      Ada_Toolchain : Toolchain)
   is
      Existing : Toolchain;
   begin
      if not This.Toolchains.Contains (Ada_Toolchain.Name.all) then
         raise Toolchain_Exception with "toolchain " & Ada_Toolchain.Name.all
           & " not found.";
      end if;

      Existing := This.Toolchains.Element (Ada_Toolchain.Name.all);

      Free (Existing.Name);
      Free (Existing.Label);
      Existing.Library := null;

      for J in Existing.Tool_Commands'Range loop
         Free (Existing.Tool_Commands (J));
      end loop;

      if Ada_Toolchain.Name /= null then
         Existing.Name := new String'(Ada_Toolchain.Name.all);
      end if;

      if Ada_Toolchain.Label /= null then
         Existing.Label := new String'(Ada_Toolchain.Label.all);
      end if;

      for J in Ada_Toolchain.Tool_Commands'Range loop
         if Ada_Toolchain.Tool_Commands (J) /= null then
            Existing.Tool_Commands (J) :=
              new String'(Ada_Toolchain.Tool_Commands (J).all);
         end if;
      end loop;

      Existing.Is_Native := Ada_Toolchain.Is_Native;
      Existing.Is_Custom := Ada_Toolchain.Is_Custom;
      Existing.Is_Computed := False;
      Existing.Is_Valid := False;

      Compute_Predefined_Paths (Existing, This);
      Fire_Change_Event (This);
   end Modify_Toolchain;

   ----------------------
   -- Remove_Toolchain --
   ----------------------

   procedure Remove_Toolchain
     (This          : Toolchain_Manager;
      Ada_Toolchain : Toolchain)
   is
      Existing : Toolchain;
   begin
      if not This.Toolchains.Contains (Ada_Toolchain.Name.all) then
         raise Toolchain_Exception with "toolchain " & Ada_Toolchain.Name.all
           & " not found.";
      end if;

      Existing := This.Toolchains.Element (Ada_Toolchain.Name.all);

      This.Toolchains.Delete (Ada_Toolchain.Name.all);
      Free (Existing);
      Fire_Change_Event (This);
   end Remove_Toolchain;

   ------------------------
   -- Get_Anonymous_Name --
   ------------------------

   function Create_Anonymous_Name (This : Toolchain_Manager) return String is
      Number : Integer := 1;
   begin
      loop
         declare
            Tentative_Name : constant String :=
              "custom_" & Trim (Number'Img, Both);
         begin
            if not This.Toolchains.Contains (Tentative_Name) then
               return Tentative_Name;
            end if;

            Number := Number + 1;
         end;
      end loop;
   end Create_Anonymous_Name;

   -----------------------------
   -- Get_Library_Information --
   -----------------------------

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   use String_Lists;

   function Get_Library_Information
     (This           : Toolchain_Manager;
      GNATls_Command : String) return Ada_Library_Info
   is
      Result : Ada_Library_Info;

      Source_Search_Path  : String_Lists.List;
      Object_Search_Path  : String_Lists.List;
      Project_Search_Path : String_Lists.List;

      function To_Path_Array
        (List : String_Lists.List) return File_Array_Access;

      -------------------
      -- To_Path_Array --
      -------------------

      function To_Path_Array
        (List : String_Lists.List) return File_Array_Access
      is
         Result : constant File_Array_Access := new File_Array
           (1 .. Integer (List.Length));
         Cur : String_Lists.Cursor;
      begin
         Cur := List.First;

         for J in Result'Range loop
            Result (J) := Create (+Element (Cur));
            Cur := Next (Cur);
         end loop;

         return Result;
      end To_Path_Array;

   begin
      if This.Computed_Libraries.Contains (GNATls_Command) then
         return This.Computed_Libraries.Element (GNATls_Command);
      end if;

      declare
         Output : constant String := This.Execute
           (GNATls_Command & " -v", 5_000);
         Lines           : String_List_Access := Split (Output, ASCII.LF);
         Garbage         : String_Access;
         Current_Line    : Integer;
      begin
         Result := new Ada_Library_Info_Record;

         This.Computed_Libraries.Insert (GNATls_Command, Result);

         for J in Lines'Range loop
            for K in Lines (J)'Range loop
               if Lines (J)(K) = ASCII.LF or else Lines (J)(K) = ASCII.CR then
                  Garbage := Lines (J);
                  Lines (J) := new String'
                    (Lines (J) (Lines (J)'First .. K - 1));
                  Free (Garbage);

                  exit;
               end if;
            end loop;
         end loop;

         Current_Line := Lines'First;

         --  Retreive the version number

         declare
            Version_Matcher : constant Pattern_Matcher :=
              Compile ("^GNATLS (.*)$");
            Version_Matches : Match_Array (0 .. 1);
         begin
            while Current_Line <= Lines'Last loop
               Match
                 (Version_Matcher, Lines (Current_Line).all, Version_Matches);

               if Version_Matches (1) /= No_Match then
                  Result.Version := new String'
                    (Lines (Current_Line)
                     (Version_Matches (1).First .. Version_Matches (1).Last));

                  Current_Line := Current_Line + 1;
                  exit;
               end if;

               Current_Line := Current_Line + 1;
            end loop;
         end;

         --  Retreive the source search path

         while Current_Line <= Lines'Last loop
            if Lines (Current_Line).all = "Source Search Path:" then
               Current_Line := Current_Line + 1;
               exit;
            else
               Current_Line := Current_Line + 1;
            end if;
         end loop;

         while Current_Line <= Lines'Last loop
            if Lines (Current_Line).all = "Object Search Path:" then
               Current_Line := Current_Line + 1;
               exit;
            else
               declare
                  Name : constant String :=
                    Trim (Lines (Current_Line).all, Both);
               begin
                  if Name = "" or else Name = "<Current_Directory>" then
                     --  Do nothing for empty lines or current directory
                     null;
                  else
                     Source_Search_Path.Append (Name);
                  end if;
               end;

               Current_Line := Current_Line + 1;
            end if;
         end loop;

         --  Retreive object search path

         while Current_Line <= Lines'Last loop
            if Lines (Current_Line).all = "Project Search Path:" then
               Current_Line := Current_Line + 1;
               exit;
            else
               declare
                  Name : constant String :=
                    Trim (Lines (Current_Line).all, Both);
               begin
                  if Name = "" or else Name = "<Current_Directory>" then
                     --  Do nothing for empty lines or current directory
                     null;
                  else
                     Object_Search_Path.Append (Name);
                  end if;
               end;

               Current_Line := Current_Line + 1;
            end if;
         end loop;

         --  Retreive the project search path

         while Current_Line <= Lines'Last loop
            declare
               Name : constant String :=
                 Trim (Lines (Current_Line).all, Both);
            begin
               if Name = "" or else Name = "<Current_Directory>" then
                  --  Do nothing for empty lines or current directory
                  null;
               else
                  Project_Search_Path.Append (Name);
               end if;
            end;

            Current_Line := Current_Line + 1;
         end loop;

         --  Copy the lists in the result arrays

         Result.Source_Path := To_Path_Array (Source_Search_Path);
         Result.Objects_Path := To_Path_Array (Object_Search_Path);
         Result.Project_Path := To_Path_Array (Project_Search_Path);

         --  Deduce the install path from the adalib directory in the object
         --  path

         for J in Result.Objects_Path'Range loop
            if Result.Objects_Path (J).Base_Dir_Name = "adalib" then
               declare
                  Cur_Path : Virtual_File := Result.Objects_Path (J);
               begin
                  while Cur_Path /= No_File
                    and then Cur_Path.Base_Dir_Name /= "lib"
                  loop
                     Cur_Path := Cur_Path.Get_Parent;
                  end loop;

                  if Cur_Path.Base_Dir_Name = "lib" then
                     Result.Install_Path := Cur_Path.Get_Parent;

                     exit;
                  end if;
               end;
            end if;
         end loop;

         Free (Lines);

         return Result;
      end;
   exception
      when E : others =>
         --  This happens typically if the GNATLS process didn't go through.

         declare
            Lib : Ada_Library_Info;
         begin
            --  If there's already an element added, removed it.

            if This.Computed_Libraries.Contains (GNATls_Command) then
               Lib := This.Computed_Libraries.Element (GNATls_Command);
               Free (Lib);
               This.Computed_Libraries.Delete (GNATls_Command);
            end if;

            Result := new Ada_Library_Info_Record;

            This.Computed_Libraries.Insert (GNATls_Command, Result);
            Result.Error := new String'(Exception_Message (E));

            return Result;
         end;
   end Get_Library_Information;

   --------------------
   -- Get_Toolchains --
   --------------------

   function Get_Toolchains (This : Toolchain_Manager) return Toolchain_Array is
      use Toolchain_Maps;

      Result : Toolchain_Array (1 .. Integer (This.Toolchains.Length));
      Cur    : Toolchain_Maps.Cursor := This.Toolchains.First;
   begin
      for J in Result'Range loop
         Result (J) := Element (Cur);
         Cur := Next (Cur);
      end loop;

      return Result;
   end Get_Toolchains;

   ---------------------
   -- Scan_Toolchains --
   ---------------------

   procedure Scan_Toolchains
     (This     : Toolchain_Manager;
      Progress : access procedure
        (Name    : String;
         Current : Integer;
         Total   : Integer))
   is
      Output : constant String := This.Execute
        ("gprconfig --show-targets", 50_000);
      Lines  : String_List_Access := Split (Output, ASCII.LF);
      Garbage         : String_Access;
      Toolchain_Matcher : constant Pattern_Matcher :=
        Compile ("([^ ]+-[^ ]+).*");
      Toolchain_Matches : Match_Array (0 .. 1);

      Nb_Toolchains : Integer := 0;
      Cur_Progress : Integer := 1;
   begin
      for J in Lines'Range loop
         for K in Lines (J)'Range loop
            if Lines (J)(K) = ASCII.LF or else Lines (J)(K) = ASCII.CR then
               Garbage := Lines (J);
               Lines (J) := new String'
                 (Trim (Lines (J) (Lines (J)'First .. K - 1), Both));
               Free (Garbage);

               exit;
            end if;
         end loop;
      end loop;

      for J in Lines'Range loop
         Match (Toolchain_Matcher, Lines (J).all, Toolchain_Matches);

         if Toolchain_Matches (0) /= No_Match then
            Nb_Toolchains := Nb_Toolchains + 1;
         else
            Free (Lines (J));
         end if;
      end loop;

      for J in Lines'Range loop
         declare
            Ada_Toolchain : Toolchain;
            Is_Native     : Boolean;
         begin
            if Lines (J) /= null then
               Is_Native := Index (Lines (J).all, "native") in Lines (J)'Range;

               if Progress /= null then
                  Progress
                    ("Compute " & Lines (J).all,
                     Cur_Progress,
                     Nb_Toolchains);
                  Cur_Progress := Cur_Progress + 1;
               end if;

               if Is_Native then
                  Ada_Toolchain := Get_Native_Toolchain (This);
               else
                  Ada_Toolchain := Get_Toolchain (This, Lines (J).all);
               end if;

               if Ada_Toolchain = null then
                  Ada_Toolchain := new Toolchain_Record;
                  Ada_Toolchain.Name := new String'(Lines (J).all);
                  Ada_Toolchain.Is_Native := Is_Native;
                  Compute_Predefined_Paths (Ada_Toolchain, This);
                  Add_Toolchain (This, Ada_Toolchain);
               end if;
            end if;
         end;
      end loop;

      Free (Lines);
   end Scan_Toolchains;

   ------------------
   -- Add_Listener --
   ------------------

   procedure Add_Listener
     (This     : Toolchain_Manager;
      Listener : Toolchain_Change_Listener)
   is
      use Listener_List;

      Cur : Listener_List.Cursor := This.Listeners.First;
   begin
      while Cur /= Listener_List.No_Element loop
         if Element (Cur) = Listener then
            return;
         end if;

         Cur := Next (Cur);
      end loop;

      This.Listeners.Append (Listener);
   end Add_Listener;

   ---------------------
   -- Remove_Listener --
   ---------------------

   procedure Remove_Listener
     (This     : Toolchain_Manager;
      Listener : Toolchain_Change_Listener)
   is
      use Listener_List;

      Cur : Listener_List.Cursor := This.Listeners.First;
   begin
      while Cur /= Listener_List.No_Element loop
         if Element (Cur) = Listener then
            This.Listeners.Delete (Cur);
            return;
         end if;

         Cur := Next (Cur);
      end loop;
   end Remove_Listener;

   -----------------------
   -- Fire_Change_Event --
   -----------------------

   procedure Fire_Change_Event (This : Toolchain_Manager)
   is
      use Listener_List;

      Cur : Listener_List.Cursor := This.Listeners.First;
   begin
      while Cur /= Listener_List.No_Element loop
         Element (Cur).Toolchain_Changed (This);

         Cur := Next (Cur);
      end loop;
   end Fire_Change_Event;

end Toolchains;
