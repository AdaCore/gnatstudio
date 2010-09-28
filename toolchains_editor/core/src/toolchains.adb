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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Containers.Indefinite_Doubly_Linked_Lists; use Ada.Containers;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;           use GNAT.Regpat;

with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with Remote;                use Remote;
with Toolchains.Known;      use Toolchains.Known;

package body Toolchains is

   Me : constant Trace_Handle := Create ("TOOLCHAINS");

   procedure Free (This : in out Ada_Library_Info_Access);
   pragma Unreferenced (Free);
   --  Free the memory associated to this library info. This should only be
   --  called by the manager, as we store the result of this information during
   --  the session.
   --  ??? Use this to free the library info when needed

   procedure Compute_Gprconfig_Compilers (Tc : Toolchain);
   --  Retrieve the default compilers for the specified toolchain, and sets the
   --  values.

   function Compilers_Match
     (Comp1, Comp2 : Compiler) return Boolean;
   --  Tells if 2 compilers are equal

   ---------------------------------
   -- Compute_Gprconfig_Compilers --
   ---------------------------------

   procedure Compute_Gprconfig_Compilers (Tc : Toolchain)
   is
      function Target_Param return String;
      --  Returns gprconfig --target parameter

      function Get_Value
        (Num       : Natural;
         Token     : String;
         From      : String;
         Strip_Exe : Boolean) return String;
      --  Parses From to retrieve the value of Token for compiler Num

      ------------------
      -- Target_Param --
      ------------------

      function Target_Param return String is
      begin
         if Get_Name (Tc) = "native" then
            return "";
         else
            return "--target=" & Get_Name (Tc);
         end if;
      end Target_Param;

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value
        (Num       : Natural;
         Token     : String;
         From      : String;
         Strip_Exe : Boolean) return String
      is
         Idx1, Idx2 : Natural;
         Search     : constant String := Num'Img & " " & Token & ":";
      begin
         Idx1 := Index (From, Search);

         if Idx1 in From'Range then
            Idx1 := Idx1 + Search'Length;
            Idx2 := Index (From (Idx1 .. From'Last), "" & ASCII.CR);
            if Idx2 not in From'Range then
               Idx2 := Index (From (Idx1 .. From'Last), "" & ASCII.LF);
            end if;
            Idx2 := Idx2 - 1;

            if Strip_Exe
              and then Idx2 - Idx1 > 3
              and then From (Idx2 - 3 .. Idx2) = ".exe"
            then
               Idx2 := Idx2 - 4;
            end if;

            return From (Idx1 .. Idx2);

         else
            Trace (Me, "could not find '" & Search & "'");
            return "";
         end if;
      end Get_Value;

      Comp_Num   : Natural := 1;

   begin
      if Tc.Compilers_Scanned then
         return;
      end if;

      declare
         Output     : constant String :=
                        Tc.Manager.Execute
                          ("gprconfig --mi-show-compilers " &
                           Target_Param,
                           5_000);
      begin
         loop
            exit when Ada.Strings.Fixed.Index (Output, Comp_Num'Img & " ")
              not in Output'Range;

            declare
               Lang       : constant String :=
                              Get_Value (Comp_Num, "lang", Output, False);
               Path       : constant String :=
                              Get_Value (Comp_Num, "path", Output, False);
               Exe        : constant String :=
                              Get_Value (Comp_Num, "executable", Output, True);
               Full       : Virtual_File;
               F          : Virtual_File;
               Is_Visible : Boolean;

            begin
               Full :=
                 Locate_On_Path (+Exe, Remote.Get_Nickname (Build_Server));

               if Full = No_File then
                  Is_Visible := False;
               else
                  F := GNATCOLL.VFS.Create
                    (+(Path & Exe), Remote.Get_Nickname (Build_Server));

                  if F = Full then
                     Is_Visible := True;
                  else
                     Is_Visible := False;
                  end if;
               end if;

               if not Tc.Compiler_Commands.Contains (Lang) then
                  if Is_Visible then
                     Set_Compiler (Tc, Lang, Exe, True);
                  else
                     Set_Compiler (Tc, Lang, Path & Exe, True);
                  end if;
               end if;

               if Active (Me) then
                  Trace
                    (Me,
                     "Found compiler for lang " & Lang &
                     " installed in " & Path & Exe);
               end if;
            end;

            Comp_Num := Comp_Num + 1;
         end loop;
      end;

      Tc.Compilers_Scanned := True;
   exception
      when others =>
         Trace (Me, "Exception when executing gprconfig. Let's skip.");
         Tc.Compilers_Scanned := True;
   end Compute_Gprconfig_Compilers;

   ---------------------
   -- Compilers_Match --
   ---------------------

   function Compilers_Match
     (Comp1, Comp2 : Compiler) return Boolean is
   begin
      return Comp1.Exe = Comp2.Exe;
   end Compilers_Match;

   -------------
   -- Get_Exe --
   -------------

   function Get_Exe (C : Compiler) return String is
   begin
      return C.Exe;
   end Get_Exe;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (C : Compiler) return Boolean is
   begin
      return C.Is_Valid;
   end Is_Valid;

   -------------
   -- Is_Used --
   -------------

   function Is_Used (C : Compiler) return Boolean is
   begin
      return not C.Unused;
   end Is_Used;

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

   --------------
   -- Get_Date --
   --------------

   function Get_Date (This : Ada_Library_Info) return Date_Type is
      Version       : constant String := Get_Version (This);
      Open_Index    : constant Natural := Index (Version, "(");
      Close_Index   : Natural;
   begin
      if Open_Index = 0 then
         return Null_Date;
      else
         Close_Index := Index (Version (Open_Index + 1 .. Version'Last), "-");

         if Close_Index = 0 then
            Close_Index :=
              Index (Version (Open_Index + 1 .. Version'Last), ")");
         end if;

         if Close_Index = 0 then
            return Null_Date;
         else
            return
              (Year  => Integer'Value
                 (Version (Open_Index + 1 .. Open_Index + 4)),
               Month => Integer'Value
                 (Version (Open_Index + 5 .. Open_Index + 6)),
               Day   => Integer'Value
                 (Version (Open_Index + 7 .. Open_Index + 8)));
         end if;
      end if;
   exception
      when E : Constraint_Error =>
         --  There has been an error in the date recovery, return false

         Trace (Me, E);

         return Null_Date;
   end Get_Date;

   ---------------
   -- Get_Error --
   ---------------

   function Get_Error (This : Ada_Library_Info) return String is
   begin
      if This.Error /= null then
         return This.Error.all;
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

   ---------------------
   -- Set_Source_Path --
   ---------------------

   procedure Set_Source_Path
     (This : in out Ada_Library_Info; Val : File_Array)
   is
   begin
      Unchecked_Free (This.Source_Path);
      This.Source_Path := new File_Array'(Val);
   end Set_Source_Path;

   ----------------------
   -- Set_Objects_Path --
   ----------------------

   procedure Set_Objects_Path
     (This : in out Ada_Library_Info; Val : File_Array)
   is
   begin
      Unchecked_Free (This.Objects_Path);
      This.Objects_Path := new File_Array'(Val);
   end Set_Objects_Path;

   ----------------------
   -- Set_Project_Path --
   ----------------------

   procedure Set_Project_Path
     (This : in out Ada_Library_Info; Val : File_Array)
   is
   begin
      Unchecked_Free (This.Project_Path);
      This.Project_Path := new File_Array'(Val);
   end Set_Project_Path;

   -----------------
   -- Set_Version --
   -----------------

   procedure Set_Version
     (This : in out Ada_Library_Info; Val : String)
   is
   begin
      Free (This.Version);
      This.Version := new String'(Val);
   end Set_Version;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (This : in out Ada_Library_Info; Val : String)
   is
   begin
      Free (This.Error);

      if Val /= "" then
         This.Error := new String'(Val);
      end if;
   end Set_Error;

   ----------------------
   -- Set_Install_Path --
   ----------------------

   procedure Set_Install_Path
     (This : in out Ada_Library_Info; Val : Virtual_File)
   is
   begin
      This.Install_Path := Val;
   end Set_Install_Path;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ada_Library_Info_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Ada_Library_Info, Ada_Library_Info_Access);
   begin
      Unchecked_Free (This.Source_Path);
      Unchecked_Free (This.Objects_Path);
      Unchecked_Free (This.Project_Path);
      Free (This.Version);
      Free (This.Error);
      Free (This);
   end Free;

   ---------
   -- Ref --
   ---------

   procedure Ref (This : Toolchain) is
   begin
      This.Refs := This.Refs + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (This : in out Toolchain) is
   begin
      This.Refs := This.Refs - 1;

      if This.Refs <= 0 then
         Free (This);
      end if;
   end Unref;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths (This : Toolchain)
   is
   begin
      if This.Library /= null and then This.Library.Is_Computed then
         return;
      end if;

      This.Library :=
        This.Manager.Get_Or_Create_Library_Information
          (Get_Command (This, GNAT_List));

      This.Manager.Compute_If_Needed (This.Library.all);

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
     (This : Toolchain; Name : Tools) return String
   is
      function Base_Tool_Name return String;

      function Base_Tool_Name return String is
      begin
         case Name is
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
      if This.Tool_Commands (Name) = null
        and then This.Default_Tools (Name) = null
      then

         if Name = Unknown then
            return "";
         end if;

         if This.Is_Native then
            Set_Command (This, Name, Base_Tool_Name, True);
         else
            Set_Command
              (This, Name, This.Name.all & "-" & Base_Tool_Name, True);
         end if;
      end if;

      if This.Tool_Commands (Name) /= null then
         return This.Tool_Commands (Name).all;
      else
         return This.Default_Tools (Name).all;
      end if;
   end Get_Command;

   ------------------
   -- Get_Compiler --
   ------------------

   function Get_Compiler (This : Toolchain; Lang : String) return Compiler is
   begin
      if This.Compiler_Commands.Contains (Lang) then
         return This.Compiler_Commands.Element (Lang);
      else
         return No_Compiler;
      end if;
   end Get_Compiler;

   ------------------
   -- Set_Compiler --
   ------------------

   procedure Set_Compiler
     (This    : Toolchain;
      Lang    : String;
      Value   : String;
      Default : Boolean := False)
   is
      Comp : Compiler :=
               (Exe_Length => Value'Length,
                Exe        => Value,
                Is_Valid   => False,
                Is_Default => Default,
                Unused     => False);

   begin
      if Locate_On_Path (+Value, Get_Nickname (Build_Server)) /= No_File then
         Comp.Is_Valid := True;
      end if;

      if This.Compiler_Commands.Contains (Lang) then
         --  Do not override default compilers accidently
         declare
            Old : constant Compiler := This.Compiler_Commands.Element (Lang);
         begin
            if Old.Is_Default then
               if Old.Exe = Comp.Exe then
                  --  Same exe, so we can keep the one with the 'default' flag
                  --  set.
                  return;
               else
                  This.Compiler_Commands.Replace (Lang, Comp);
               end if;
            else
               This.Compiler_Commands.Replace (Lang, Comp);
            end if;
         end;
      else
         This.Compiler_Commands.Insert (Lang, Comp);
      end if;
   end Set_Compiler;

   ----------------------
   -- Set_Use_Compiler --
   ----------------------

   procedure Set_Use_Compiler
     (This    : Toolchain;
      Lang    : String;
      Value   : Boolean)
   is
   begin
      if not This.Compiler_Commands.Contains (Lang) then
         declare
            C : Compiler := No_Compiler;
         begin
            C.Unused := not Value;
            This.Compiler_Commands.Insert (Lang, C);
         end;

      else
         declare
            C : Compiler := This.Compiler_Commands.Element (Lang);
         begin
            C.Unused := not Value;
            This.Compiler_Commands.Replace (Lang, C);
         end;
      end if;
   end Set_Use_Compiler;

   -----------------
   -- Set_Command --
   -----------------

   procedure Set_Command
     (This       : Toolchain;
      Name       : Tools;
      Value      : String;
      Is_Default : Boolean := False)
   is
   begin
      if This.Default_Tools (Name) /= null
        and then This.Default_Tools (Name).all = Value
      then
         --  Resetting the default tool, free the custom value
         Free (This.Tool_Commands (Name));

      else
         if Active (Me) then
            Trace (Me, Name'Img & " default state set to " &
                   Is_Default'Img);
         end if;

         if Is_Default then
            Free (This.Default_Tools (Name));
            This.Default_Tools (Name) := new String'(Value);
         else
            Free (This.Tool_Commands (Name));
            This.Tool_Commands (Name) := new String'(Value);
         end if;
      end if;

      if Value /= ""
        and then
         Locate_On_Path (+Value, Remote.Get_Nickname (Build_Server)) /= No_File
      then
         This.Is_Valid_Tool (Name) := True;
      else
         This.Is_Valid_Tool (Name) := False;
      end if;
   end Set_Command;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Toolchain; Name : Tools) return Boolean is
   begin
      return This.Is_Valid_Tool (Name);
   end Is_Valid;

   ----------------
   -- Is_Default --
   ----------------

   function Is_Default (This : Toolchain; Name : Tools) return Boolean is
   begin
      return This.Tool_Commands (Name) = null
        and then This.Default_Tools (Name) /= null;
   end Is_Default;

   ----------------
   -- Is_Default --
   ----------------

   function Is_Default (This : Toolchain; Lang : String) return Boolean is
   begin
      return This.Compiler_Commands.Contains (Lang)
        and then This.Compiler_Commands.Element (Lang).Is_Default;
   end Is_Default;

   ----------------------
   -- Reset_To_Default --
   ----------------------

   procedure Reset_To_Default (This : Toolchain; Name : Tools) is
   begin
      Set_Command (This, Name, This.Default_Tools (Name).all);
   end Reset_To_Default;

   ----------------------
   -- Reset_To_Default --
   ----------------------

   procedure Reset_To_Default (This : Toolchain; Lang : String) is
   begin
      if This.Compiler_Commands.Contains (Lang) then
         This.Compiler_Commands.Delete (Lang);
      end if;
   end Reset_To_Default;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (This : Toolchain) return String is
   begin
      if This.Name = null then
         return "";
      else
         return This.Name.all;
      end if;
   end Get_Name;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (This : Toolchain) return String is
   begin
      if This.Label = null then
         return Get_Name (This);
      else
         return This.Label.all;
      end if;
   end Get_Label;

   ---------------
   -- Set_Label --
   ---------------

   procedure Set_Label (This : Toolchain; Label : String) is
   begin
      if This.Label /= null
        and then This.Manager.Toolchains.Contains (This.Label.all)
        and then This.Manager.Toolchains.Element (This.Label.all) = This
      then
         This.Manager.Toolchains.Delete (This.Label.all);
         This.Manager.Toolchains.Insert (Label, This);
      end if;

      Free (This.Label);
      This.Label := new String'(Label);
   end Set_Label;

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
         if Result.Default_Tools (J) /= null then
            Result.Default_Tools (J) :=
              new String'(Result.Default_Tools (J).all);
         end if;
      end loop;

      declare
         Map  : constant Compiler_Maps.Map := Result.Compiler_Commands;
         Iter : Compiler_Maps.Cursor;
      begin
         Result.Compiler_Commands := Compiler_Maps.Empty_Map;
         Iter := Map.First;

         while Compiler_Maps.Has_Element (Iter) loop
            Result.Compiler_Commands.Insert
              (Compiler_Maps.Key (Iter), Compiler_Maps.Element (Iter));
            Compiler_Maps.Next (Iter);
         end loop;
      end;

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
         Free (This.Default_Tools (J));
      end loop;

      Free (This);
   end Free;

   -----------------------------
   -- Get_Library_Information --
   -----------------------------

   function Get_Library_Information
     (This : Toolchain) return Ada_Library_Info_Access
   is
   begin
      --  There intentionally no call to compute here - should use whatever
      --  information is available without long process.

      return This.Library;
   end Get_Library_Information;

   -----------------------------
   -- Set_Library_Information --
   -----------------------------

   procedure Set_Library_Information
     (This : Toolchain;
      Info : Ada_Library_Info_Access)
   is
   begin
      This.Library := Info;
   end Set_Library_Information;

   ------------------
   -- Add_Language --
   ------------------

   procedure Add_Language
     (Manager : access Toolchain_Manager_Record;
      Lang    : String;
      Project : Project_Type)
   is
      pragma Unreferenced (Project);
   begin
      if Manager.Languages.Contains (Lang) then
         return;
      end if;

      Manager.Languages.Insert (Lang);
   end Add_Language;

   ----------------------------
   -- Get_Or_Create_Language --
   ----------------------------

   function Get_Or_Create_Language
     (Manager : access Toolchain_Manager_Record;
      Lang    : String) return Language_Id
   is
   begin
      if not Manager.Languages.Contains (Lang) then
         Manager.Languages.Insert (Lang);
      end if;

      return Language_Id (Manager.Languages.Find (Lang));
   end Get_Or_Create_Language;

   ----------------------------
   -- Create_Empty_Toolchain --
   ----------------------------

   function Create_Empty_Toolchain
     (Manager : access Toolchain_Manager_Record) return Toolchain
   is
      Ret : constant Toolchain := new Toolchain_Record;
   begin
      Ret.Manager := Toolchain_Manager (Manager);
      return Ret;
   end Create_Empty_Toolchain;

   -------------------
   -- Get_Toolchain --
   -------------------

   function Get_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Label   : String) return Toolchain
   is
   begin
      --  Case 1: the toolchain is already computed
      if Manager.Toolchains.Contains (Label) then
         return Manager.Toolchains.Element (Label);

      --  Case 2, the toolchain is known. Create a known one

      elsif Is_Known_Toolchain_Name (Label) then
         return Manager.Get_Known_Toolchain (Label);

      --  Case 3, the toolchain contains the string "native", return the
      --  native one

      elsif Index (Label, "native") in Label'Range then
         return Manager.Get_Native_Toolchain;

      end if;

      --  Otherwise, the toolchain can't be retreived, return null
      return null;

   end Get_Toolchain;

   -------------------
   -- Get_Toolchain --
   -------------------

   function Get_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Project : Project_Type) return Toolchain
   is
      GNAT_List_Str    : constant String :=
        Attribute_Value (Project, Build ("ide", "gnatlist"), "");
      GNAT_Driver_Str  : constant String :=
        Attribute_Value (Project, Build ("ide", "gnat"), "");
      Gnatmake_Str : constant String :=
        Attribute_Value (Project, Build ("ide", "compiler_command"), "ada");
      Debugger_Str     : constant String :=
        Attribute_Value (Project, Build ("ide", "debugger_command"), "");

      Compilers : Compiler_Maps.Map;

      function Toolchain_Matches
        (TC : Toolchain; Drivers : Compiler_Maps.Map) return Boolean;
      --  Compares the toolchain values against the above attributes, and
      --  return true if the values match.

      function Get_Prefix return String;
      --  Gets the toolchain prefix from the above attributes

      function Get_Prefix (Attr : String) return String;
      --  Gets the toolchain prefix from an attribute

      procedure Set_Compilers_From_Attribute
        (Package_Name, Attribute_Name : String);
      --  Looks at all the indexes for the attribute describe, and assign
      --  compilers according to the values extracted.

      -----------------------
      -- Toolchain_Matches --
      -----------------------

      function Toolchain_Matches
        (TC : Toolchain; Drivers : Compiler_Maps.Map) return Boolean
      is
         Cursor : Compiler_Maps.Cursor;
      begin
         if (GNAT_List_Str = ""
           or else GNAT_List_Str = Get_Command (TC, GNAT_List))
           and then
             (GNAT_Driver_Str = ""
              or else GNAT_Driver_Str = Get_Command (TC, GNAT_Driver))
           and then
             (Debugger_Str = ""
              or else Debugger_Str = Get_Command (TC, Debugger))
         then
            declare
               use Compiler_Maps;
               Tmp : Compiler_Maps.Cursor := First (TC.Compiler_Commands);
            begin
               while Tmp /= Compiler_Maps.No_Element loop
                  Tmp := Next (Tmp);
               end loop;
            end;

            Cursor := Drivers.First;
            while Compiler_Maps.Has_Element (Cursor) loop
               declare
                  Lang : constant String := Compiler_Maps.Key (Cursor);
                  Toolchain_Compiler : constant Compiler :=
                    Get_Compiler (TC, Lang);
               begin
                  if Toolchain_Compiler /= No_Compiler
                    and then not Compilers_Match
                      (Toolchain_Compiler,
                       Compiler_Maps.Element (Cursor))
                  then
                     return False;
                  end if;
               end;

               Compiler_Maps.Next (Cursor);
            end loop;

            return True;
         end if;

         return False;
      end Toolchain_Matches;

      ----------------
      -- Get_Prefix --
      ----------------

      function Get_Prefix return String is
      begin
         declare
            S : constant String := Get_Prefix (Gnatmake_Str);
         begin
            if S /= "" then
               return S;
            end if;
         end;

         declare
            S : constant String := Get_Prefix (GNAT_List_Str);
         begin
            if S /= "" then
               return S;
            end if;
         end;

         declare
            S : constant String := Get_Prefix (GNAT_Driver_Str);
         begin
            if S /= "" then
               return S;
            end if;
         end;

         declare
            S : constant String := Get_Prefix (Debugger_Str);
         begin
            if S /= "" then
               return S;
            end if;
         end;

         return "";
      end Get_Prefix;

      ----------------
      -- Get_Prefix --
      ----------------

      function Get_Prefix (Attr : String) return String is
      begin
         --  Remove the path if indicated in the attribute
         for J in reverse Attr'Range loop
            if Attr (J) = '/' or else Attr (J) = '\' then
               return Get_Prefix (Attr (J + 1 .. Attr'Last));
            end if;
         end loop;

         --  No path, so it's safe to actually look at the prefix
         for J in reverse Attr'Range loop
            if Attr (J) = '-' then
               return Attr (Attr'First .. J - 1);
            end if;
         end loop;

         --  Special case for gnaamp compiler that have special names for its
         --  tools
         if Attr'Length > 6
           and then Attr (Attr'First .. Attr'First + 5) = "gnaamp"
         then
            return "gnaamp";
         end if;

         return "";
      end Get_Prefix;

      ----------------------------------
      -- Set_Compilers_From_Attribute --
      ----------------------------------

      procedure Set_Compilers_From_Attribute
        (Package_Name, Attribute_Name : String)
      is
         Attr : constant Attribute_Pkg_String :=
           Build (Package_Name, Attribute_Name);
         Indexes : String_List := Attribute_Indexes (Project, Attr);
      begin

         for J in Indexes'Range loop
            declare
               Driver : constant String := Attribute_Value
                 (Project, Attr, Indexes (J).all,
                  Default => "gps-dummy-default");
            begin
               if Driver /= "gps-dummy-default" then
                  if Compilers.Contains (Indexes (J).all) then
                     Compilers.Delete (Indexes (J).all);
                  end if;

                  --  We don't care about Is_Valid and Is_Default here, since
                  --  only the name is retrieved later on.
                  Compilers.Insert
                    (Indexes (J).all,
                     Compiler'(Exe_Length => Driver'Length,
                               Exe        => Driver,
                               Is_Valid   => False,
                               Is_Default => False,
                               Unused     => Driver = ""));
               end if;
            end;
         end loop;

         Free (Indexes);
      end Set_Compilers_From_Attribute;

      Cursor    : Toolchain_Maps.Cursor;
      Ret       : Toolchain := null;
      Modified  : Boolean := False;
      --  Whether the toolchain returned has been modified from the one stored
      --  in the manager

      Iter      : Compiler_Maps.Cursor;

      Is_Empty : constant Boolean := GNAT_List_Str = ""
        and then GNAT_Driver_Str = ""
        and then Gnatmake_Str = ""
        and then Debugger_Str = "";

   begin
      --  First of all, we need to retrieve all potential explicitely defined
      --  compilers from the project

      --  First, look at all compiler commands

      Set_Compilers_From_Attribute ("ide", "compiler_command");

      --  Then, look at all drivers, possibly overriding compiler commands

      Set_Compilers_From_Attribute ("compiler", "driver");

      --  1 step: look through the current toolchains list to verify if this
      --  toolchain already exists

      if not Is_Empty then
         Cursor := Manager.Toolchains.First;

         while Toolchain_Maps.Has_Element (Cursor) loop
            Ret := Toolchain_Maps.Element (Cursor);

            if Toolchain_Matches (Ret, Compilers) then
               return Ret;
            end if;

            Toolchain_Maps.Next (Cursor);
         end loop;

         Ret := null;
      end if;

      --  2 step: no such toolchain exists, try to retrieve it from a known
      --  configuration

      --  First known configuration: the native toolchain
      if Is_Empty then
         --  No need for further modifications, just return the native
         --  toolchain
         Ret := Manager.Get_Native_Toolchain;
         Modified := False;
      else
         --  Second case: we retrieve the toolchain from the prefix
         declare
            Prefix : constant String := Get_Prefix;
         begin
            if Prefix /= ""
              and then Is_Known_Toolchain_Name (Prefix)
            then
               Ret := Manager.Get_Known_Toolchain (Prefix);
               Modified := False;
            end if;
         end;
      end if;

      --  Third case: the toolchain is not known, we need to start a brand new
      --  one

      if Ret = null then
         if Get_Prefix = "" then
            Ret := Manager.Get_Native_Toolchain;
         else
            Ret := Create_Empty_Toolchain (Manager);
            Modified := True;
            Set_Name (Ret, Get_Prefix);
            Set_Label (Ret, Get_Prefix);
         end if;
      end if;

      --  At this stage, we have either a toolchain created from a known
      --  configuration and that we potentially need to adjust, or a new one
      --  created for the occasion for which we need to fill the values.

      if GNAT_List_Str /= Get_Command (Ret, GNAT_List)
        and then GNAT_List_Str /= ""
      then
         if not Modified then
            Ret := Copy (Ret);
            Modified := True;
         end if;

         Set_Command (Ret, GNAT_List, GNAT_List_Str);

         --  Reset the library as gnatls changed
         Ret.Library := null;
      end if;

      if GNAT_Driver_Str /= Get_Command (Ret, GNAT_Driver)
        and then GNAT_Driver_Str /= ""
      then
         if not Modified then
            Ret := Copy (Ret);
            Modified := True;
         end if;

         Set_Command (Ret, GNAT_Driver, GNAT_Driver_Str);
      end if;

      if Debugger_Str /= Get_Command (Ret, Debugger)
        and then Debugger_Str /= ""
      then
         if not Modified then
            Ret := Copy (Ret);
            Modified := True;
         end if;

         Set_Command (Ret, Debugger, Debugger_Str);
      end if;

      --  Init the explicitely defined compilers
      Iter := Compilers.First;
      while Compiler_Maps.Has_Element (Iter) loop
         declare
            Lang : constant String := Compiler_Maps.Key (Iter);
            Comp : constant String := Compiler_Maps.Element (Iter).Exe;
         begin
            if Ret.Compiler_Commands.Contains (Lang)
              and then Ret.Compiler_Commands.Element (Lang).Exe /= Comp
            then
               Ret := Copy (Ret);
               Modified := True;
            end if;

            Set_Compiler (Ret, Lang, Comp);
            Set_Use_Compiler
              (Ret, Lang, not Compiler_Maps.Element (Iter).Unused);
         end;

         Compiler_Maps.Next (Iter);
      end loop;

      --  If the toolchain has been modified, then we now need to find a new
      --  name
      if Modified then
         --  Take care of duplicated labels
         if Manager.Toolchains.Contains (Get_Label (Ret)) then
            Ret.Label :=
              new String'(Manager.Create_Anonymous_Name (Get_Prefix));
         end if;

         Ret.Is_Custom := True;
         Manager.Add_Toolchain (Ret);
      end if;

      Compute_Predefined_Paths (Ret);

      return Ret;
   end Get_Toolchain;

   -------------------
   -- Add_Toolchain --
   -------------------

   procedure Add_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Tc      : Toolchain)
   is
   begin
      if Manager.Toolchains.Contains (Get_Label (Tc)) then
         raise Toolchain_Exception with "Toolchain "
           & Get_Label (Tc) & " already registered";
      end if;

      if Tc.Library = null then
         Compute_Gprconfig_Compilers (Tc);
         Compute_Predefined_Paths (Tc);
      end if;

      Ref (Tc);
      Manager.Toolchains.Insert (Get_Label (Tc), Tc);
      Tc.Manager := Toolchain_Manager (Manager);

      Fire_Change_Event (Manager);
   end Add_Toolchain;

   ----------------------
   -- Remove_Toolchain --
   ----------------------

   procedure Remove_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Tc_Name : String)
   is
      Existing : Toolchain;
   begin
      if not Manager.Toolchains.Contains (Tc_Name) then
         raise Toolchain_Exception with "toolchain " & Tc_Name
           & " not found.";
      end if;

      Existing := Manager.Toolchains.Element (Tc_Name);

      Manager.Toolchains.Delete (Tc_Name);
      Unref (Existing);
      Fire_Change_Event (Manager);
   end Remove_Toolchain;

   --------------------
   -- Get_Toolchains --
   --------------------

   function Get_Toolchains
     (Manager : access Toolchain_Manager_Record) return Toolchain_Array
   is
      use Toolchain_Maps;

      Result : Toolchain_Array (1 .. Integer (Manager.Toolchains.Length));
      Cur    : Toolchain_Maps.Cursor := Manager.Toolchains.First;
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

   procedure Scan_Toolchains (Manager : access Toolchain_Manager_Record) is
      procedure Dummy_Progress
        (Name : String; Current : Integer; Total : Integer) is null;
   begin
      Scan_Toolchains (Manager, Dummy_Progress'Access);
   end Scan_Toolchains;

   ---------------------
   -- Scan_Toolchains --
   ---------------------

   procedure Scan_Toolchains
     (Manager : access Toolchain_Manager_Record;
      Progress : access procedure
        (Name    : String;
         Current : Integer;
         Total   : Integer))
   is
      Output : constant String := Toolchain_Manager (Manager).Execute
        ("gprconfig --show-targets", 50_000);
      Lines  : String_List_Access := Split (Output, ASCII.LF);
      Garbage         : String_Access;
      Toolchain_Matcher : constant Pattern_Matcher :=
                            Compile ("([^ ]+).*[^:]$");
      Toolchain_Matches : Match_Array (0 .. 1);

      Nb_Toolchains : Integer := 0;
      Cur_Progress  : Integer := 1;

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
                    (Lines (J).all,
                     Cur_Progress,
                     Nb_Toolchains);
                  Cur_Progress := Cur_Progress + 1;
               end if;

               Trace (Me, "Scan toolchain: " & Lines (J).all);

               if Is_Native then
                  Ada_Toolchain := Manager.Get_Native_Toolchain;
               else
                  Ada_Toolchain := Manager.Get_Toolchain (Lines (J).all);
               end if;

               if Ada_Toolchain = null then
                  Ada_Toolchain := new Toolchain_Record;
                  Ada_Toolchain.Name := new String'(Lines (J).all);
                  Ada_Toolchain.Is_Native := Is_Native;
                  Ada_Toolchain.Manager := Toolchain_Manager (Manager);
                  Manager.Add_Toolchain (Ada_Toolchain);
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
     (Manager  : access Toolchain_Manager_Record;
      Listener : Toolchain_Change_Listener)
   is
      use Listener_List;

      Cur : Listener_List.Cursor := Manager.Listeners.First;
   begin
      while Cur /= Listener_List.No_Element loop
         if Element (Cur) = Listener then
            return;
         end if;

         Cur := Next (Cur);
      end loop;

      Manager.Listeners.Append (Listener);
   end Add_Listener;

   ---------------------
   -- Remove_Listener --
   ---------------------

   procedure Remove_Listener
     (Manager  : access Toolchain_Manager_Record;
      Listener : Toolchain_Change_Listener)
   is
      use Listener_List;

      Cur : Listener_List.Cursor := Manager.Listeners.First;
   begin
      while Cur /= Listener_List.No_Element loop
         if Element (Cur) = Listener then
            Manager.Listeners.Delete (Cur);
            return;
         end if;

         Cur := Next (Cur);
      end loop;
   end Remove_Listener;

   --------------------------
   -- Get_Native_Toolchain --
   --------------------------

   function Get_Native_Toolchain
     (Manager : access Toolchain_Manager_Record) return Toolchain
   is
      use Toolchain_Maps;

      Cur : Toolchain_Maps.Cursor;
      Native_Toolchain : Toolchain;
   begin
      if Manager.No_Native_Toolchain then
         return null;
      end if;

      Cur := Manager.Toolchains.First;

      while Cur /= Toolchain_Maps.No_Element loop
         if Element (Cur).Is_Native then
            return Element (Cur);
         end if;

         Cur := Next (Cur);
      end loop;

      --  If no native toolchain has been found, then create one.

      Native_Toolchain := new Toolchain_Record'
        (Name              => new String'("native"),
         Label             => null,
         Is_Native         => True,
         Is_Custom         => False,
         Tool_Commands     => (others => null),
         Default_Tools     => (others => null),
         Is_Valid_Tool     => (others => False),
         Compiler_Commands => <>,
         Compilers_Scanned => False,
         Is_Valid          => False,
         Library           => null,
         Manager           => Toolchain_Manager (Manager),
         Refs              => 0);

      Set_Command (Native_Toolchain, GNAT_Driver, "gnat", True);
      Set_Command (Native_Toolchain, GNAT_List, "gnatls", True);
      Set_Command (Native_Toolchain, Debugger, "gdb", True);
      Set_Command (Native_Toolchain, CPP_Filt, "c++filt", True);

      Compute_Gprconfig_Compilers (Native_Toolchain);
      Compute_Predefined_Paths (Native_Toolchain);

      if not Native_Toolchain.Compiler_Commands.Contains ("Ada") then
         Set_Compiler (Native_Toolchain, "Ada", "gnatmake", True);
      end if;

      if not Native_Toolchain.Compiler_Commands.Contains ("C") then
         Set_Compiler (Native_Toolchain, "C", "gcc", True);
      end if;

      if Native_Toolchain.Is_Valid then
         Manager.Add_Toolchain (Native_Toolchain);

         return Native_Toolchain;

      else
         --  set the flag, so that we don't try to analyze the native
         --  toolchain later on
         Unref (Native_Toolchain);
         Manager.No_Native_Toolchain := True;

         return null;
      end if;
   end Get_Native_Toolchain;

   --------------------------------
   -- Initialize_Known_Toolchain --
   --------------------------------

   procedure Initialize_Known_Toolchain (This : Toolchain; Name : String) is
   begin
      if not Is_Known_Toolchain_Name (Name) then
         return;
      end if;

      Trace (Me, "Creating known toolchain for target " & Name);

      Set_Name (This, Name);

      This.Compilers_Scanned := False;
      This.Compiler_Commands.Clear;

      --  Set tools

      for T in Valid_Tools'Range loop
         Set_Command
           (This, T,
            Toolchains.Known.Tool_Command (Name, T),
            True);
      end loop;

      Compute_Gprconfig_Compilers (This);

      --  Force compilers if needed

      if not This.Compiler_Commands.Contains ("Ada") then
         Trace (Me, "Create_Known_Toolchain: Force ada compiler command");
         if Is_Compiler_Defined (Name, "Ada") then
            Set_Compiler (This, "Ada", Compiler_Command (Name, "Ada"), True);
         end if;
      end if;

      if not This.Compiler_Commands.Contains ("C") then
         Trace (Me, "Create_Known_Toolchain: Force C compiler command");
         if Is_Compiler_Defined (Name, "C") then
            Set_Compiler (This, "C", Compiler_Command (Name, "C"), True);
         end if;
      end if;
   end Initialize_Known_Toolchain;

   -------------------------
   -- Get_Known_Toolchain --
   -------------------------

   function Get_Known_Toolchain
     (Manager : access Toolchain_Manager_Record;
      Name    : String) return Toolchain
   is
      Result : Toolchain;
   begin
      if not Is_Known_Toolchain_Name (Name) then
         return null;
      end if;

      if Manager.Toolchains.Contains (Name) then
         return Manager.Toolchains.Element (Name);
      end if;

      Result := new Toolchain_Record;
      --  Make sure that the manager part is initialized before initializing
      --  the toolchain.
      Result.Manager := Toolchain_Manager (Manager);
      Initialize_Known_Toolchain (Result, Name);

      Manager.Add_Toolchain (Result);

      return Result;
   end Get_Known_Toolchain;

   ------------------------
   -- Get_Anonymous_Name --
   ------------------------

   function Create_Anonymous_Name
     (Manager : access Toolchain_Manager_Record;
      Prefix  : String) return String
   is
      Number : Integer := 1;
   begin
      if Prefix = "" then
         return Manager.Create_Anonymous_Name ("native");
      end if;

      loop
         declare
            Tentative_Name : constant String :=
              Prefix & " (" & Trim (Number'Img, Both) & ")";
         begin
            if not Manager.Toolchains.Contains (Tentative_Name) then
               return Tentative_Name;
            end if;

            Number := Number + 1;
         end;
      end loop;
   end Create_Anonymous_Name;

   -----------------------
   -- Compute_If_Needed --
   -----------------------

   procedure Compute_If_Needed
     (Manager : access Toolchain_Manager_Record;
      This    : in out Ada_Library_Info)
   is
      package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
        (String);

      use String_Lists;

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
      if This.Is_Computed then
         return;
      else
         This.Is_Computed := True;
      end if;

      declare
         Output : constant String := Toolchain_Manager (Manager).Execute
           (This.GNATls_Command.all & " -v", 5_000);
         Lines           : String_List_Access := Split (Output, ASCII.LF);
         Garbage         : String_Access;
         Current_Line    : Integer;
      begin
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
                  This.Version := new String'
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

         This.Source_Path := To_Path_Array (Source_Search_Path);
         This.Objects_Path := To_Path_Array (Object_Search_Path);
         This.Project_Path := To_Path_Array (Project_Search_Path);

         --  Deduce the install path from the adalib directory in the object
         --  path

         for J in This.Objects_Path'Range loop
            if This.Objects_Path (J).Base_Dir_Name = "adalib" then
               declare
                  Cur_Path : Virtual_File := This.Objects_Path (J);
               begin
                  while Cur_Path /= No_File
                    and then Cur_Path.Base_Dir_Name /= "lib"
                  loop
                     Cur_Path := Cur_Path.Get_Parent;
                  end loop;

                  if Cur_Path.Base_Dir_Name = "lib" then
                     This.Install_Path := Cur_Path.Get_Parent;

                     exit;
                  end if;
               end;
            end if;
         end loop;

         Free (Lines);
      end;
   exception
      when E : others =>
         --  This happens typically if the GNATLS process didn't go through.

         This.Error := new String'(Exception_Message (E));
   end Compute_If_Needed;

   ----------------------
   -- Clear_Toolchains --
   ----------------------

   procedure Clear_Toolchains (Manager : in out Toolchain_Manager_Record) is
      use Toolchain_Maps;

      Cur : Toolchain_Maps.Cursor := First (Manager.Toolchains);
      Tmp : Toolchain;
   begin
      while Cur /= Toolchain_Maps.No_Element loop
         Tmp := Element (Cur);
         Unref (Tmp);

         Cur := Next (Cur);
      end loop;

      Manager.Toolchains.Clear;
   end Clear_Toolchains;

   ---------------------------------------
   -- Get_Or_Create_Library_Information --
   ---------------------------------------

   function Get_Or_Create_Library_Information
     (Manager        : access Toolchain_Manager_Record;
      GNATls_Command : String) return Ada_Library_Info_Access
   is
      Result : Ada_Library_Info_Access;
   begin
      if not Manager.Computed_Libraries.Contains (GNATls_Command) then
         Result := new Ada_Library_Info;
         Manager.Computed_Libraries.Insert (GNATls_Command, Result);
         Result.GNATls_Command := new String'(GNATls_Command);
      else
         Result := Manager.Computed_Libraries.Element (GNATls_Command);
      end if;

      return Result;
   end Get_Or_Create_Library_Information;

   -----------------------
   -- Fire_Change_Event --
   -----------------------

   procedure Fire_Change_Event (This : access Toolchain_Manager_Record)
   is
      use Listener_List;

      Cur : Listener_List.Cursor := This.Listeners.First;
   begin
      while Cur /= Listener_List.No_Element loop
         Element (Cur).Toolchain_Changed (Toolchain_Manager (This));

         Cur := Next (Cur);
      end loop;
   end Fire_Change_Event;

end Toolchains;
