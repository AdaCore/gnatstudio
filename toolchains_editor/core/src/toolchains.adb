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
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;           use GNAT.Regpat;

with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with Remote;                use Remote;
with Toolchains.Known;      use Toolchains.Known;

package body Toolchains is

   Me : constant Trace_Handle := Create ("TOOLCHAINS");

   procedure Free (This : in out Ada_Library_Info_Access);
   --  Free the memory associated to this library info. This should only be
   --  called by the manager, as we store the result of this information during
   --  the session.
   --  ??? Use this to free the library info when needed

   function Compilers_Match
     (Comp1, Comp2 : Compiler) return Boolean;
   --  Tells if 2 compilers are equal

   procedure Compute_Gprconfig_Compilers
     (Mgr : access Toolchain_Manager_Record)
   is
      function Get_Value
        (Num   : Natural;
         Token : String;
         From  : String) return String;
      --  Parses From to retrieve the value of Token for compiler Num

      function Strip_Exe (Name : String) return String;
      --  Strips the .exe extension if needed

      ---------------
      -- Get_Value --
      ---------------

      function Get_Value
        (Num   : Natural;
         Token : String;
         From  : String) return String
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

            return From (Idx1 .. Idx2);

         else
            Trace (Me, "could not find '" & Search & "'");
            return "";
         end if;
      end Get_Value;

      ---------------
      -- Strip_Exe --
      ---------------

      function Strip_Exe (Name : String) return String is
      begin
         if Name'Length > 4
           and then Name (Name'Last - 3 .. Name'Last) = ".exe"
         then
            return Name (Name'First .. Name'Last - 4);
         end if;

         return Name;
      end Strip_Exe;

      Comp_Num   : Natural := 1;
      Glob_List  : Compiler_Vector.Vector;

   begin
      if Mgr.Compilers_Scanned then
         return;
      end if;

      Mgr.Compilers_Scanned := True;

      declare
         Output  : constant String :=
                        Toolchain_Manager (Mgr).Execute
                          ("gprconfig --mi-show-compilers --target=all",
                           5_000);
         package TC_Set is new Ada.Containers.Indefinite_Vectors
           (Positive, String);
         Toolchains : TC_Set.Vector;

      begin
         loop
            exit when Ada.Strings.Fixed.Index (Output, Comp_Num'Img & " ")
              not in Output'Range;

            declare
               Lang       : constant String :=
                              Get_Value (Comp_Num, "lang", Output);
               Path       : constant String :=
                              Get_Value (Comp_Num, "path", Output);
               Exe        : constant String :=
                              Get_Value (Comp_Num, "executable", Output);
               Target     : constant String :=
                              Get_Value (Comp_Num, "target", Output);
               Is_Native  : constant Boolean :=
                              Boolean'Value
                                (Get_Value (Comp_Num, "native", Output));
               Stripped   : constant String := Strip_Exe (Exe);
               Tc_Name    : Unbounded_String;
               Full       : Virtual_File;
               F          : Virtual_File;
               Is_Visible : Boolean;
               New_Comp   : Compiler;

            begin
               if Is_Native then
                  Tc_Name := To_Unbounded_String (Target & " (native)");
               else
                  Tc_Name := To_Unbounded_String (Target);
               end if;

               if not Toolchains.Contains (To_String (Tc_Name)) then
                  Trace
                    (Me, "Append target " & Target &
                     " to the list of scanned toolchains");
                  Toolchains.Append (To_String (Tc_Name));
               end if;

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

               if Is_Visible then
                  New_Comp :=
                    (Exe        => To_Unbounded_String (Stripped),
                     Is_Valid   => True,
                     Origin     => From_Gprconfig,
                     Toolchain  => Tc_Name,
                     Lang       => To_Unbounded_String (Lang));
               else
                  New_Comp :=
                    (Exe        => To_Unbounded_String (Path & Stripped),
                     Is_Valid   => True,
                     Origin     => From_Gprconfig,
                     Toolchain  => Tc_Name,
                     Lang       => To_Unbounded_String (Lang));
               end if;

               if not Glob_List.Contains (New_Comp) then
                  Glob_List.Append (New_Comp);
               end if;
            end;

            Comp_Num := Comp_Num + 1;
         end loop;

         Mgr.Gprconfig_Compilers := Glob_List;

         for J in Toolchains.First_Index .. Toolchains.Last_Index loop
            declare
               Target : constant String := Toolchains.Element (J);
               Tc     : Toolchain := Mgr.Get_Toolchain (Target);
            begin
               if Tc = null then
                  Tc := Create_Empty_Toolchain (Mgr);
                  Set_Name (Tc, Target);
                  Tc.Is_Native := Index (Target, "native") in Target'Range;

                  --  not in the known toolchains database: let's use the
                  --  default gnat scheme for commands.

                  if not Tc.Is_Native then
                     --  Use general scheme for gnat toolchains
                     Set_Command
                       (Tc, GNAT_Driver, Target & "-gnat", True);
                     Set_Command
                       (Tc, GNAT_List, Target & "-gnatls", True);
                     Set_Command
                       (Tc, Debugger, Target & "-gdb", True);
                     Set_Command
                       (Tc, CPP_Filt, Target & "-c++filt", True);
                  else
                     Set_Command
                       (Tc, GNAT_Driver, "gnat", True);
                     Set_Command
                       (Tc, GNAT_List, "gnatls", True);
                     Set_Command
                       (Tc, Debugger, "gdb", True);
                     Set_Command
                       (Tc, CPP_Filt, "c++filt", True);
                  end if;

                  Mgr.Add_Toolchain (Tc);
               end if;

               for J in Glob_List.First_Index .. Glob_List.Last_Index loop
                  if To_String (Glob_List.Element (J).Toolchain) =
                    Target
                  then
                     Add_Compiler
                       (Tc,
                        To_String (Glob_List.Element (J).Lang),
                        To_String (Glob_List.Element (J).Exe),
                        Origin => From_Gprconfig);
                  end if;
               end loop;

               Set_Label (Tc, Target);
            end;
         end loop;

      exception
         when others =>
            Trace (Me, "Exception when executing gprconfig. Let's skip.");
            Mgr.Compilers_Scanned := True;
      end;
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
      return To_String (C.Exe);
   end Get_Exe;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (C : Compiler) return Boolean is
   begin
      return C.Is_Valid;
   end Is_Valid;

   ----------------
   -- Get_Origin --
   ----------------

   function Get_Origin (C : Compiler) return Compiler_Origin is
   begin
      return C.Origin;
   end Get_Origin;

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
      Version     : constant String := Get_Version (This);
      Open_Index  : constant Natural := Index (Version, "(");
      Close_Index : Natural;
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

   procedure Compute_Predefined_Paths (This : Toolchain) is
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

      --------------------
      -- Base_Tool_Name --
      --------------------

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
   -- Add_Compiler --
   ------------------

   procedure Add_Compiler
     (This   : Toolchain;
      Lang   : String;
      Value  : String;
      Origin : Compiler_Origin)
   is
      New_Comp : Compiler :=
                   (Exe       => To_Unbounded_String (Value),
                    Is_Valid  => False,
                    Origin    => Origin,
                    Toolchain => To_Unbounded_String (Get_Name (This)),
                    Lang      => To_Unbounded_String (Lang));

   begin
      if Locate_On_Path (+Value, Get_Nickname (Build_Server)) /= No_File then
         New_Comp.Is_Valid := True;
      end if;

      --  If a compiler exists for the same language and comes from default
      --  (e.g. xml definition file), the we replace it if we have more
      --  accurate information (coming from gprconfig)
      if Origin = From_Gprconfig then
         for J in This.Full_Compiler_List.First_Index ..
           This.Full_Compiler_List.Last_Index
         loop
            declare
               Comp : constant Compiler := This.Full_Compiler_List.Element (J);

            begin
               if Comp.Lang = New_Comp.Lang
                 and then Comp.Origin = From_Default
               then
                  This.Full_Compiler_List.Replace_Element (J, New_Comp);

                  return;
               end if;
            end;
         end loop;
      end if;

      This.Full_Compiler_List.Append (New_Comp);

      if not This.Used_Compiler_List.Contains (Lang) then
         This.Used_Compiler_List.Insert
           (Lang, Natural (This.Full_Compiler_List.Last_Index));
      end if;
   end Add_Compiler;

   ------------------
   -- Set_Compiler --
   ------------------

   procedure Set_Compiler
     (This  : Toolchain;
      Lang  : String;
      Value : String)
   is
      Full     : Compiler_Vector.Vector renames This.Full_Compiler_List;
      User_Def : Natural := 0;
      New_Comp : Compiler;

   begin
      for J in Full.First_Index .. Full.Last_Index loop
         if Ada.Strings.Equal_Case_Insensitive
              (To_String (Full.Element (J).Lang), Lang)
         then
            if Get_Exe (Full.Element (J)) = Value then
               This.Used_Compiler_List.Replace (Lang, J);

               return;
            end if;

            --  Save position of the user-defined compiler
            if Full.Element (J).Origin = From_User then
               User_Def := J;
            end if;
         end if;
      end loop;

      --  No reuseable compiler found, let's insert one
      New_Comp :=
        (Exe       => To_Unbounded_String (Value),
         Is_Valid  => False,
         Origin    => From_User,
         Toolchain => To_Unbounded_String (Get_Name (This)),
         Lang      => To_Unbounded_String (Lang));

      if Locate_On_Path (+Value, Get_Nickname (Build_Server)) /= No_File then
         New_Comp.Is_Valid := True;
      end if;

      if User_Def /= 0 then
         --  Replace the previously user-defined compiler
         This.Full_Compiler_List.Replace_Element (User_Def, New_Comp);
         This.Used_Compiler_List.Replace (Lang, User_Def);
      else
         This.Full_Compiler_List.Append (New_Comp);

         if This.Used_Compiler_List.Contains (Lang) then
            This.Used_Compiler_List.Replace
              (Lang, This.Full_Compiler_List.Last_Index);
         else
            This.Used_Compiler_List.Insert
              (Lang, This.Full_Compiler_List.Last_Index);
         end if;
      end if;
   end Set_Compiler;

   ------------------
   -- Get_Compiler --
   ------------------

   function Get_Compiler (This : Toolchain; Lang : String) return Compiler is
   begin
      --  Make sure this is properly initialized
      if not This.Used_Compiler_List.Contains (Lang) then
         Reset_To_Default (This, Lang);
      end if;

      if This.Used_Compiler_List.Contains (Lang) then
         declare
            Idx : constant Natural :=
                    This.Used_Compiler_List.Element (Lang);
         begin
            if Idx = 0 then
               return No_Compiler;
            else
               return This.Full_Compiler_List.Element (Idx);
            end if;
         end;
      else
         return No_Compiler;
      end if;
   end Get_Compiler;

   -------------------
   -- Get_Compilers --
   -------------------

   function Get_Compilers
     (This : Toolchain; Lang : String) return Compiler_Array
   is
      Vect : Compiler_Vector.Vector renames This.Full_Compiler_List;
      N    : Natural := 0;
   begin
      for J in Vect.First_Index .. Vect.Last_Index loop
         if Ada.Strings.Equal_Case_Insensitive
           (Lang, To_String (Vect.Element (J).Lang))
         then
            N := N + 1;
         end if;
      end loop;

      declare
         Ret : Compiler_Array (1 .. N);
         Idx : Natural := 0;
      begin
         for J in Vect.First_Index .. Vect.Last_Index loop
            if Ada.Strings.Equal_Case_Insensitive
              (Lang, To_String (Vect.Element (J).Lang))
            then
               Idx := Idx + 1;
               Ret (Idx) := Vect.Element (J);
            end if;
         end loop;

         return Ret;
      end;
   end Get_Compilers;

   --------------------------
   -- Get_Compiler_Is_Used --
   --------------------------

   function Get_Compiler_Is_Used
     (This : Toolchain; Lang : String) return Boolean is
   begin
      if This.Used_Compiler_List.Contains (Lang)
        and then This.Used_Compiler_List.Element (Lang) = 0
      then
         return False;
      else
         return True;
      end if;
   end Get_Compiler_Is_Used;

   --------------------------
   -- Set_Compiler_Is_Used --
   --------------------------

   procedure Set_Compiler_Is_Used
     (This : Toolchain; Lang : String; Value : Boolean) is
   begin
      if not Value then
         if This.Used_Compiler_List.Contains (Lang) then
            --  use index 0 to indicate that we force no compiler
            This.Used_Compiler_List.Replace (Lang, 0);
         else
            This.Used_Compiler_List.Insert (Lang, 0);
         end if;

      else
         Reset_To_Default (This, Lang);
      end if;
   end Set_Compiler_Is_Used;

   function Get_Default_Compiler_Index
     (This : Toolchain; Lang : String) return Natural;

   --------------------------------
   -- Get_Default_Compiler_Index --
   --------------------------------

   function Get_Default_Compiler_Index
     (This : Toolchain; Lang : String) return Natural
   is
      Full_List : Compiler_Vector.Vector renames This.Full_Compiler_List;
   begin
      for J in Full_List.First_Index .. Full_List.Last_Index loop
         if Ada.Strings.Equal_Case_Insensitive
              (To_String (Full_List.Element (J).Lang), Lang)
           and then Full_List.Element (J).Origin in Default_Compiler_Origin
         then
            return J;
         end if;
      end loop;

      return 0;
   end Get_Default_Compiler_Index;

   ----------------
   -- Is_Default --
   ----------------

   function Is_Default (This : Toolchain; Lang : String) return Boolean is
      Default : constant Natural :=
                  Get_Default_Compiler_Index (This, Lang);
   begin
      if Default = 0 then
         --  no compiler exist for this language, so no compiler is the default
         return False;

      else
         return Default = This.Used_Compiler_List.Element (Lang);
      end if;
   end Is_Default;

   ----------------------
   -- Reset_To_Default --
   ----------------------

   procedure Reset_To_Default (This : Toolchain; Lang : String) is
      Default : constant Natural :=
                  Get_Default_Compiler_Index (This, Lang);
   begin
      if Default /= 0 then
         if This.Used_Compiler_List.Contains (Lang) then
            This.Used_Compiler_List.Replace (Lang, Default);
         else
            This.Used_Compiler_List.Insert (Lang, Default);
         end if;

         return;

      else
         --  No compiler is defined
         if This.Used_Compiler_List.Contains (Lang) then
            This.Used_Compiler_List.Delete (Lang);
         end if;
      end if;
   end Reset_To_Default;

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

   ----------------------
   -- Reset_To_Default --
   ----------------------

   procedure Reset_To_Default (This : Toolchain; Name : Tools) is
   begin
      Set_Command (This, Name, This.Default_Tools (Name).all);
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

      --  Deep copy of the containers

      declare
         Map  : constant Compiler_Ref_Maps.Map := Result.Used_Compiler_List;
         Iter : Compiler_Ref_Maps.Cursor;
      begin
         Result.Used_Compiler_List := Compiler_Ref_Maps.Empty_Map;
         Iter := Map.First;

         while Compiler_Ref_Maps.Has_Element (Iter) loop
            Result.Used_Compiler_List.Insert
              (Compiler_Ref_Maps.Key (Iter), Compiler_Ref_Maps.Element (Iter));
            Compiler_Ref_Maps.Next (Iter);
         end loop;
      end;

      declare
         Vect : constant Compiler_Vector.Vector := Result.Full_Compiler_List;
      begin
         Result.Full_Compiler_List := Compiler_Vector.Empty_Vector;
         for J in Vect.First_Index .. Vect.Last_Index loop
            Result.Full_Compiler_List.Append (Vect.Element (J));
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
      GNAT_List_Str   : constant String :=
                          Attribute_Value
                            (Project, Build ("ide", "gnatlist"), "");
      GNAT_Driver_Str : constant String :=
                          Attribute_Value
                            (Project, Build ("ide", "gnat"), "");
      Gnatmake_Str    : constant String :=
                          Attribute_Value
                            (Project,
                             Build ("ide", "compiler_command"), "ada");
      Debugger_Str    : constant String :=
                          Attribute_Value
                            (Project, Build ("ide", "debugger_command"), "");

      Compilers : Compiler_Vector.Vector;

      function Toolchain_Matches
        (TC : Toolchain; Drivers : Compiler_Vector.Vector) return Boolean;
      --  Compares the toolchain values against the above attributes, and
      --  return true if the values match.

      function Get_Prefix return String;
      --  Gets the toolchain prefix from the above attributes

      function Get_Prefix (Attr : String) return String;
      --  Gets the toolchain prefix from an attribute

      procedure Set_Compilers_From_Attribute (Attr : Attribute_Pkg_String);
      --  Looks at all the indexes for the attribute describe, and assign
      --  compilers according to the values extracted.

      -----------------------
      -- Toolchain_Matches --
      -----------------------

      function Toolchain_Matches
        (TC : Toolchain; Drivers : Compiler_Vector.Vector) return Boolean
      is
         Cursor : Compiler_Vector.Cursor;
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
            Cursor := Drivers.First;

            while Compiler_Vector.Has_Element (Cursor) loop
               declare
                  Lang        : constant String :=
                                  To_String
                                    (Compiler_Vector.Element (Cursor).Lang);
                  Tc_Compiler : constant Compiler :=
                                  Get_Compiler (TC, Lang);
               begin
                  if Tc_Compiler /= No_Compiler
                    and then not Compilers_Match
                      (Tc_Compiler,
                       Compiler_Vector.Element (Cursor))
                  then
                     return False;
                  end if;
               end;

               Compiler_Vector.Next (Cursor);
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
         --  tools.

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

      procedure Set_Compilers_From_Attribute  (Attr : Attribute_Pkg_String)
      is
         Indexes : String_List := Attribute_Indexes (Project, Attr);
         Origin  : Compiler_Origin;
      begin

         if Attr = Compiler_Command_Attribute then
            Origin := From_Project;
         else
            Origin := From_Project_Driver;
         end if;

         for J in Indexes'Range loop
            declare
               Driver : constant String :=
                          Attribute_Value
                            (Project, Attr, Indexes (J).all,
                             Default => "gps-dummy-default");
            begin
               if Driver /= "gps-dummy-default" then
                  --  We don't care about Is_Valid and Toolchain, not used
                  --  later on.
                  Compilers.Append
                    (Compiler'
                       (Exe       => To_Unbounded_String (Driver),
                        Is_Valid  => False,
                        Origin    => Origin,
                        Toolchain => Null_Unbounded_String,
                        Lang      => To_Unbounded_String (Indexes (J).all)));
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

      Is_Empty  : constant Boolean :=
                    GNAT_List_Str = ""
                        and then GNAT_Driver_Str = ""
                        and then Gnatmake_Str = ""
                        and then Debugger_Str = "";

   begin
      --  We read the compilers defined directly in the project first.
      Set_Compilers_From_Attribute (Compiler_Command_Attribute);
      Set_Compilers_From_Attribute (Compiler_Driver_Attribute);

      --  1 step: look through the current toolchains list to verify if this
      --  toolchain already exists.

      if not Is_Empty then
         Cursor := Manager.Toolchains.First;

         while Toolchain_Maps.Has_Element (Cursor) loop
            Ret := Toolchain_Maps.Element (Cursor);

            if Toolchain_Matches (Ret, Compilers) then
               --  Exact match, we can return this toolchain
               return Ret;
            end if;

            Toolchain_Maps.Next (Cursor);
         end loop;

         Ret := null;
      end if;

      --  2 step: no such toolchain exists, try to retrieve it from a known
      --  configuration.

      --  First known configuration: the native toolchain

      if Is_Empty then
         --  No need for further modifications, just return the native
         --  toolchain.
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
      --  one.

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
      for J in Compilers.First_Index .. Compilers.Last_Index loop
         declare
            Lang : constant String := To_String (Compilers.Element (J).Lang);
            Comp : constant String := To_String (Compilers.Element (J).Exe);
            Orig : constant Compiler_Origin := Compilers.Element (J).Origin;
            Arr  : constant Compiler_Array := Get_Compilers (Ret, Lang);
            Found : Boolean;

         begin
            Found := False;

            for K in Arr'Range loop
               if Arr (K).Exe = Comp
                 and then Arr (K).Origin = Orig
               then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               if not Modified then
                  Ret := Copy (Ret);
                  Modified := True;
               end if;

               if Comp /= "" then
                  Add_Compiler (Ret, Lang, Comp, Orig);
                  Set_Compiler (Ret, Lang, Comp);
               else
                  Set_Compiler_Is_Used (Ret, Lang, False);
               end if;
            end if;
         end;
      end loop;

      --  If the toolchain has been modified, then we now need to find a new
      --  name.

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

      Cur              : Toolchain_Maps.Cursor;
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

      --  If no native toolchain has been found, then create one

      Native_Toolchain := new Toolchain_Record'
        (Name               => new String'("native"),
         Label              => null,
         Is_Native          => True,
         Is_Custom          => False,
         Tool_Commands      => (others => null),
         Default_Tools      => (others => null),
         Is_Valid_Tool      => (others => False),
         Full_Compiler_List => Compiler_Vector.Empty_Vector,
         Used_Compiler_List => Compiler_Ref_Maps.Empty_Map,
         Compilers_Scanned  => False,
         Is_Valid           => False,
         Library            => null,
         Manager            => Toolchain_Manager (Manager),
         Refs               => 0);

      Set_Command (Native_Toolchain, GNAT_Driver, "gnat", True);
      Set_Command (Native_Toolchain, GNAT_List, "gnatls", True);
      Set_Command (Native_Toolchain, Debugger, "gdb", True);
      Set_Command (Native_Toolchain, CPP_Filt, "c++filt", True);

      Compute_Predefined_Paths (Native_Toolchain);

      if Get_Compiler (Native_Toolchain, "Ada") = No_Compiler then
         Add_Compiler (Native_Toolchain, "Ada", "gnatmake", From_Default);
      end if;

      if Get_Compiler (Native_Toolchain, "C") = No_Compiler then
         Add_Compiler (Native_Toolchain, "C", "gcc", From_Default);
      end if;

      if Get_Compiler (Native_Toolchain, "C++") = No_Compiler then
         Add_Compiler (Native_Toolchain, "C++", "g++", From_Default);
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
      This.Used_Compiler_List.Clear;
      This.Full_Compiler_List.Clear;

      --  Set tools

      for T in Valid_Tools'Range loop
         Set_Command
           (This, T,
            Toolchains.Known.Tool_Command (Name, T),
            True);
      end loop;

      --  Force compilers if needed

      declare
         Langs : String_List_Access :=
                   Toolchains.Known.Langs (Name);
      begin
         for J in Langs'Range loop
            if Get_Compiler (This, Langs (J).all) = No_Compiler then
               Add_Compiler
                 (This, Langs (J).all,
                  Compiler_Command (Name, Langs (J).all),
                  From_Default);
            end if;
         end loop;

         Free (Langs);
      end;
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
         Result : constant File_Array_Access :=
                    new File_Array (1 .. Integer (List.Length));
         Cur    : String_Lists.Cursor;
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
         Output : constant String :=
                    Toolchain_Manager (Manager).Execute
                      (This.GNATls_Command.all & " -v", 5_000);
         Lines           : String_List_Access := Split (Output, ASCII.LF);
         Garbage         : GNAT.Strings.String_Access;
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
         --  This happens typically if the GNATLS process didn't go through

         This.Error := new String'(Exception_Message (E));
   end Compute_If_Needed;

   ----------------------
   -- Clear_Toolchains --
   ----------------------

   procedure Clear_Toolchains (Manager : in out Toolchain_Manager_Record) is
      use Toolchain_Maps;

      Tmp : Toolchain;
   begin
      while not Manager.Toolchains.Is_Empty loop
         Tmp := Manager.Toolchains.First_Element;
         Unref (Tmp);

         Manager.Toolchains.Delete_First;
      end loop;

      while not Manager.Saved_Toolchains.Is_Empty loop
         Tmp := Manager.Saved_Toolchains.First_Element;
         Unref (Tmp);

         Manager.Saved_Toolchains.Delete_First;
      end loop;
   end Clear_Toolchains;

   -----------------
   -- Do_Snapshot --
   -----------------

   procedure Do_Snapshot (Manager : in out Toolchain_Manager_Record) is
      use Toolchain_Maps;

      Cur : Toolchain_Maps.Cursor;
      Tc : Toolchain;

   begin
      while not Manager.Saved_Toolchains.Is_Empty loop
         Tc := Manager.Saved_Toolchains.First_Element;
         Free (Tc);
         Manager.Saved_Toolchains.Delete_First;
      end loop;

      Cur := First (Manager.Toolchains);
      while Has_Element (Cur) loop
         Manager.Saved_Toolchains.Insert (Key (Cur), Copy (Element (Cur)));
         Next (Cur);
      end loop;
   end Do_Snapshot;

   ---------------
   -- Do_Commit --
   ---------------

   procedure Do_Commit (Manager : in out Toolchain_Manager_Record) is
      Tc : Toolchain;
   begin
      while not Manager.Saved_Toolchains.Is_Empty loop
         Tc := Manager.Saved_Toolchains.First_Element;
         Free (Tc);
         Manager.Saved_Toolchains.Delete_First;
      end loop;
   end Do_Commit;

   -----------------
   -- Do_Rollback --
   -----------------

   procedure Do_Rollback (Manager : in out Toolchain_Manager_Record) is
      Tc : Toolchain;
   begin
      if Manager.Saved_Toolchains.Is_Empty then
         return;
      end if;

      while not Manager.Toolchains.Is_Empty loop
         Tc := Manager.Toolchains.First_Element;
         Free (Tc);
         Manager.Toolchains.Delete_First;
      end loop;

      while not Manager.Saved_Toolchains.Is_Empty loop
         Manager.Toolchains.Insert
           (Manager.Saved_Toolchains.First_Key,
            Manager.Saved_Toolchains.First_Element);
         Manager.Saved_Toolchains.Delete_First;
      end loop;
   end Do_Rollback;

   ----------
   -- Free --
   ----------

   procedure Free (Manager : in out Toolchain_Manager) is
      Lib : Ada_Library_Info_Access;
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Toolchain_Manager_Record'Class, Toolchain_Manager);
   begin
      Manager.Clear_Toolchains;
      while not Manager.Computed_Libraries.Is_Empty loop
         Lib := Manager.Computed_Libraries.First_Element;
         Free (Lib);
         Manager.Computed_Libraries.Delete_First;
      end loop;
      Manager.Listeners.Clear;
      Manager.Languages.Clear;
      Manager.Gprconfig_Compilers.Clear;
      Internal_Free (Manager);
   end Free;

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
