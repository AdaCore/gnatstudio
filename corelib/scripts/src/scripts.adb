-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2007, AdaCore             --
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

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Scripts.Impl;            use Scripts.Impl;
with System;                  use System;
with System.Address_Image;

package body Scripts is

   Timeout_Threshold : constant Duration := 0.2;   --  in seconds
   --  Timeout between two checks of the gtk+ event queue

   function To_Address is new Ada.Unchecked_Conversion
     (Class_Instance_Record_Access, System.Address);

   -----------------------------------
   -- Data stored in class_instance --
   -----------------------------------

   type User_Data_Type is
     (Strings,
      Integers,
      Consoles);

   type Scalar_Properties_Record (Typ : User_Data_Type) is
     new Instance_Property_Record
   with record
      case Typ is
         when Strings =>
            Str : GNAT.Strings.String_Access;
         when Integers =>
            Int : Integer;
         when Consoles =>
            Console : Virtual_Console;
      end case;
   end record;

   type Scalar_Properties is access all Scalar_Properties_Record'Class;
   overriding procedure Destroy (Prop : in out Scalar_Properties_Record);
   overriding function Get_Instances
     (Prop : Scalar_Properties_Record) return Instance_List_Access;
   --  See inherited documentation

   -----------------
   -- Subprograms --
   -----------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Class_Instance_Record'Class, Class_Instance_Record_Access);

   type Scripting_Language_List is access Scripting_Language_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Scripting_Language_Array, Scripting_Language_List);

   package Classes_Hash is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Class_Type,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");
   use Classes_Hash;

   type Scripting_Data_Record is new Scripts_Repository_Record with
      record
         Scripting_Languages  : Scripting_Language_List :=
           new Scripting_Language_Array'(1 .. 0 => null);
         Classes              : Classes_Hash.Map;
      end record;
   type Scripting_Data is access all Scripting_Data_Record'Class;

   procedure Free_User_Data (Data : in out User_Data_List);
   --  Free the memory used by Data. Data is reset to null, and this doesn't
   --  free other user data in the list.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Prop : in out Instance_Property_Record) is
      pragma Unreferenced (Prop);
   begin
      null;
   end Destroy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Repo : in out Scripts_Repository) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scripts_Repository_Record'Class, Scripts_Repository);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Scripting_Language_Record'Class, Scripting_Language);
      Data : constant Scripting_Data := Scripting_Data (Repo);
      C    : Classes_Hash.Cursor;
      Class : Class_Type;
   begin
      if Repo /= null then
         if Data.Scripting_Languages /= null then
            for L in Data.Scripting_Languages'Range loop
               Destroy (Data.Scripting_Languages (L));
               Unchecked_Free (Data.Scripting_Languages (L));
            end loop;
            Unchecked_Free (Data.Scripting_Languages);
         end if;

         C := First (Data.Classes);
         while Has_Element (C) loop
            Class := Element (C);
            Free (Class.Name);
            Next (C);
         end loop;

         Unchecked_Free (Repo);
      end if;
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Instance_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Instance_Array, Instance_Array_Access);
   begin
      --  Class_Instance are automatically Finalized by the compiler
      Unchecked_Free (List.List);
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (List   : Instance_List;
      Script : access Scripting_Language_Record'Class) return Class_Instance
   is
      Tmp : constant Scripting_Language_Array := Scripting_Data
        (Get_Repository (Script)).Scripting_Languages.all;
   begin
      if List.List /= null then
         for T in Tmp'Range loop
            if Tmp (T) = Scripting_Language (Script) then
               return List.List (T);
            end if;
         end loop;
      end if;
      return No_Class_Instance;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (List   : in out Instance_List;
      Script : access Scripting_Language_Record'Class;
      Inst   : Class_Instance)
   is
      Tmp : constant Scripting_Language_Array := Scripting_Data
        (Get_Repository (Script)).Scripting_Languages.all;
   begin
      if List.List = null then
         List.List := new Instance_Array (Tmp'Range);
         List.List.all := (others => No_Class_Instance);
      end if;

      for T in Tmp'Range loop
         if Tmp (T) = Scripting_Language (Script) then
            List.List (T) := Inst;
            exit;
         end if;
      end loop;
   end Set;

   -------------------
   -- Get_Instances --
   -------------------

   function Get_Instances (List : Instance_List) return Instance_Array is
      Null_List : Instance_Array (1 .. 0);
   begin
      if List.List = null then
         return Null_List;
      else
         return List.List.all;
      end if;
   end Get_Instances;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Callback_Data_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Callback_Data_Array, Callback_Data_List);
   begin
      if List /= null then
         for L in List'Range loop
            if List (L) /= null then
               Free (List (L));
            end if;
         end loop;
         Unchecked_Free (List);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Instance_List_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Instance_List, Instance_List_Access);
   begin
      if List /= null then
         Free (List.all);
         Unchecked_Free (List);
      end if;
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (Repo   : Scripts_Repository;
      List   : Callback_Data_List;
      Script : access Scripting_Language_Record'Class)
      return Callback_Data_Access
   is
      Tmp : constant Scripting_Language_Array :=
              Scripting_Data (Repo).Scripting_Languages.all;
   begin
      if List /= null then
         for T in Tmp'Range loop
            if Tmp (T) = Scripting_Language (Script) then
               return List (T);
            end if;
         end loop;
      end if;
      return null;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Repo   : Scripts_Repository;
      List   : in out Callback_Data_List;
      Script : access Scripting_Language_Record'Class;
      Data   : Callback_Data_Access)
   is
      Tmp : constant Scripting_Language_Array :=
              Scripting_Data (Repo).Scripting_Languages.all;
   begin
      if List = null then
         List := new Callback_Data_Array (Tmp'Range);
      end if;

      for T in Tmp'Range loop
         if Tmp (T) = Scripting_Language (Script) then
            if List (T) /= null
              and then List (T) /= Data
            then
               Free (List (T));
            end if;

            List (T) := Data;
            exit;
         end if;
      end loop;
   end Set;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Callback_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Callback_Data'Class, Callback_Data_Access);
   begin
      if Data /= null then
         Free (Data.all);
         Unchecked_Free (Data);
      end if;
   end Free;

   ---------------------------------
   -- Register_Scripting_Language --
   ---------------------------------

   procedure Register_Scripting_Language
     (Repo   : Scripts_Repository;
      Script : access Scripting_Language_Record'Class)
   is
      Tmp : constant Scripting_Language_Array :=
              Scripting_Data (Repo).Scripting_Languages.all;
   begin
      Unchecked_Free (Scripting_Data (Repo).Scripting_Languages);
      Scripting_Data (Repo).Scripting_Languages :=
        new Scripting_Language_Array'(Tmp & Scripting_Language (Script));
   end Register_Scripting_Language;

   -------------------------------
   -- Lookup_Scripting_Language --
   -------------------------------

   function Lookup_Scripting_Language
     (Repo   : Scripts_Repository;
      Name   : String) return Scripting_Language
   is
      Tmp : constant Scripting_Language_List :=
        Scripting_Data (Repo).Scripting_Languages;
      N   : constant String := To_Lower (Name);
   begin
      for T in Tmp'Range loop
         if To_Lower (Get_Name (Tmp (T))) = N then
            return Tmp (T);
         end if;
      end loop;

      return null;
   end Lookup_Scripting_Language;

   -----------------------------
   -- Get_Scripting_Languages --
   -----------------------------

   function Get_Scripting_Languages
     (Repo : Scripts_Repository)
      return Scripting_Language_Array is
   begin
      return Scripting_Data (Repo).Scripting_Languages.all;
   end Get_Scripting_Languages;

   --------------------
   -- Block_Commands --
   --------------------

   procedure Block_Commands
     (Repo   : Scripts_Repository;
      Block  : Boolean)
   is
      Tmp : constant Scripting_Language_List :=
              Scripting_Data (Repo).Scripting_Languages;
   begin
      for T in Tmp'Range loop
         Block_Commands (Tmp (T), Block);
      end loop;
   end Block_Commands;

   ----------------------
   -- Register_Command --
   ----------------------

   procedure Register_Command
     (Repo          : Scripts_Repository;
      Command       : String;
      Minimum_Args  : Natural := 0;
      Maximum_Args  : Natural := 0;
      Handler       : Module_Command_Function;
      Class         : Class_Type := No_Class;
      Static_Method : Boolean := False)
   is
      Tmp : constant Scripting_Language_List :=
              Scripting_Data (Repo).Scripting_Languages;
   begin
      if Command = Constructor_Method and then Class = No_Class then
         raise Program_Error
           with "Constructors can only be specified for classes";
      end if;

      if Static_Method and then Class = No_Class then
         raise Program_Error
           with "Static method can only be created for classes";
      end if;

      for T in Tmp'Range loop
         Register_Command
           (Tmp (T), Command,
            Minimum_Args, Maximum_Args, Handler, Class, Static_Method);
      end loop;
   end Register_Command;

   ---------------
   -- New_Class --
   ---------------

   function New_Class
     (Repo   : Scripts_Repository;
      Name   : String;
      Base   : Class_Type := No_Class) return Class_Type
   is
      Tmp   : constant Scripting_Language_List :=
                Scripting_Data (Repo).Scripting_Languages;
      Class : Class_Type;
      C     : Classes_Hash.Cursor;

   begin
      if Tmp = null then
         return No_Class;

      else
         C := Find (Scripting_Data (Repo).Classes, Name);
         if Has_Element (C) then
            Class := Element (C);
         else
            if Class = No_Class and then Tmp /= null then
               for T in Tmp'Range loop
                  Register_Class (Tmp (T), Name, Base);
               end loop;

               Class := Class_Type'(Name => new String'(Name));
               Include (Scripting_Data (Repo).Classes, Name, Class);
            end if;
         end if;
      end if;

      return Class;
   end New_Class;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Class : Class_Type) return String is
   begin
      if Class.Name = null then
         return "";
      else
         return Class.Name.all;
      end if;
   end Get_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Repo : in out Scripts_Repository) is
   begin
      Repo := new Scripting_Data_Record;
   end Initialize;

   -------------------------------
   -- Register_Standard_Classes --
   -------------------------------

   procedure Register_Standard_Classes
     (Repo               : Scripts_Repository;
      Console_Class_Name : String) is
   begin
      Repo.Console_Class := New_Class (Repo, Console_Class_Name);
      Register_Console_Class (Repo, Repo.Console_Class);
   end Register_Standard_Classes;

   -------------------------------
   -- Execute_Command_With_Args --
   -------------------------------

   function Execute_Command_With_Args
     (Script  : access Scripting_Language_Record;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List) return String
   is
      pragma Unreferenced (Script, Command, Args);
   begin
      raise Program_Error;
      return "";
   end Execute_Command_With_Args;

   ---------------------
   -- Execute_Command --
   ---------------------

   function Execute_Command
     (Script       : access Scripting_Language_Record;
      Command      : String;
      Console      : Virtual_Console := null;
      Hide_Output  : Boolean := False;
      Show_Command : Boolean := True;
      Errors       : access Boolean) return String is
   begin
      Execute_Command
        (Scripting_Language (Script),
         Command, Console, Hide_Output, Show_Command, Errors.all);
      return "";
   end Execute_Command;

   ---------------
   -- Interrupt --
   ---------------

   function Interrupt
     (Script : access Scripting_Language_Record) return Boolean
   is
      pragma Unreferenced (Script);
   begin
      return False;
   end Interrupt;

   --------------
   -- Complete --
   --------------

   procedure Complete
     (Script      : access Scripting_Language_Record;
      Input       : String;
      Completions : out String_Lists.List)
   is
      pragma Unreferenced (Script, Input);
   begin
      Completions := String_Lists.Empty_List;
   end Complete;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data    : Callback_Data;
      N       : Positive;
      Default : Subprogram_Type) return Subprogram_Type is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : String)
      return String is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Integer)
      return Integer is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data : Callback_Data; N : Positive; Default : Boolean)
      return Boolean is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg
     (Data       : Callback_Data;
      N          : Positive;
      Class      : Class_Type := Any_Class;
      Default    : Class_Instance;
      Allow_Null : Boolean := False) return Class_Instance is
   begin
      return Nth_Arg (Callback_Data'Class (Data), N, Class, Allow_Null);
   exception
      when No_Such_Parameter =>
         return Default;
   end Nth_Arg;

   --------------------
   -- Get_Repository --
   --------------------

   function Get_Repository (Data : Callback_Data)
      return Scripts_Repository is
   begin
      return Get_Repository (Get_Script (Callback_Data'Class (Data)));
   end Get_Repository;

   ----------
   -- Free --
   ----------

   procedure Free (Subprogram : in out Subprogram_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Subprogram_Record'Class, Subprogram_Type);
   begin
      if Subprogram /= null then
         Free (Subprogram.all);
         Unchecked_Free (Subprogram);
      end if;
   end Free;

   --------------------
   -- Free_User_Data --
   --------------------

   procedure Free_User_Data (Data : in out User_Data_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (User_Data, User_Data_List);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Instance_Property_Record'Class, Instance_Property);
   begin
      if Data /= null then
         if Data.Prop /= null then
            Destroy (Data.Prop.all);
            Unchecked_Free (Data.Prop);
         end if;
         Unchecked_Free (Data);
      end if;
   end Free_User_Data;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Prop : in out Scalar_Properties_Record) is
   begin
      case Prop.Typ is
         when Strings =>
            Free (Prop.Str);

         when Integers | Consoles =>
            null;
      end case;
   end Destroy;

   -------------------
   -- Get_Instances --
   -------------------

   function Get_Instances
     (Prop : Instance_Property_Record) return Instance_List_Access
   is
      pragma Unreferenced (Prop);
   begin
      return null;
   end Get_Instances;

   -------------------
   -- Get_Instances --
   -------------------

   function Get_Instances
     (Prop : Scalar_Properties_Record) return Instance_List_Access
   is
   begin
      case Prop.Typ is
         when Strings | Integers | Consoles =>
            null;
      end case;

      return null;
   end Get_Instances;

   ------------
   -- Decref --
   ------------

   procedure Decref (Inst : access Class_Instance_Record) is
      Data : User_Data_List;
      Ptr  : Class_Instance_Record_Access;
      L    : Instance_Array_Access;
      Instances : Instance_List_Access;
      Refs_In_User_Data : Natural := 0;
   begin
      --  We are already in the process of destroying the class instance, so
      --  nothing else to do

      if Inst.Refcount = Natural'Last then
         return;
      end if;

      --  If the instance has only one reference left, and that is owned by
      --  the user data (for instance when it is a selection), we will be able
      --  to kill the CI instances anyway.

      Data := Inst.User_Data;

      while Data /= null loop
         Instances := Get_Instances (Data.Prop.all);

         if Instances /= null and then Instances.List /= null then
            L := Instances.List;

            for C in L'Range loop
               if L (C).Initialized
                 and then L (C).Data.Data = Class_Instance_Record_Access (Inst)
               then
                  Refs_In_User_Data := Refs_In_User_Data + 1;
               end if;
            end loop;
         end if;

         Data := Data.Next;
      end loop;

      Inst.Refcount := Inst.Refcount - 1;

      if Inst.Refcount = Refs_In_User_Data then
         --  Reset the references to the CI in the instances of all its user
         --  data. This might result in a recursive call to Decref, which will
         --  set Refcount to 0. To prevent this, and for more efficiency,
         --  we simulate a different refcount, and finalize the CI ourselves

         Inst.Refcount := Natural'Last;

         Data := Inst.User_Data;
         while Data /= null loop
            Instances := Get_Instances (Data.Prop.all);
            if Instances /= null and then Instances.List /= null then
               L := Instances.List;
               for C in L'Range loop
                  if L (C).Initialized
                    and then L (C).Data.Data =
                       Class_Instance_Record_Access (Inst)
                  then
                     L (C) := No_Class_Instance;
                     exit;
                  end if;
               end loop;
            end if;
            Data := Data.Next;
         end loop;

         Inst.Refcount := 0;
      end if;

      if Inst.Refcount = 0 then
         Inst.Refcount := Natural'Last;

         while Inst.User_Data /= null loop
            Data := Inst.User_Data.Next;
            Free_User_Data (Inst.User_Data);
            Inst.User_Data := Data;
         end loop;

         Inst.Refcount := 0;

         Ptr := Inst.all'Access;
         --  The above isn't dangerous: We know there are no more
         --  Class_Instance referencing this pointer, or we wouldn't be
         --  destroying it. As a result, it is safe to free the memory here

         Unchecked_Free (Ptr);
      end if;
   end Decref;

   ------------
   -- Incref --
   ------------

   procedure Incref (Inst : access Class_Instance_Record) is
   begin
      Inst.Refcount := Inst.Refcount + 1;
   end Incref;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (CI : in out Class_Instance_Data) is
   begin
      Incref (CI.Data);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (CI : in out Class_Instance_Data) is
   begin
      --  Data might be null in some rare cases. Most notably, it happens when
      --  GPS is being destroyed: the python module has already been destroyed,
      --  but we still have remaining CI finalized when GNAT finalizes
      --  everything before exit.

      if CI.Data /= null then
         Decref (CI.Data);
      end if;
   end Finalize;

   ---------
   -- "=" --
   ---------

   function "=" (CI1, CI2 : Class_Instance_Data) return Boolean is
   begin
      return CI1.Data = CI2.Data;
   end "=";

   -------------
   -- Get_CIR --
   -------------

   function Get_CIR
     (Inst : Class_Instance) return Class_Instance_Record_Access is
   begin
      if Inst.Initialized then
         return Inst.Data.Data;
      else
         return null;
      end if;
   end Get_CIR;

   --------------------
   -- Print_Refcount --
   --------------------

   function Print_Refcount
     (Instance : access Class_Instance_Record) return String is
   begin
      return "CI=(" & System.Address_Image
        (To_Address (Class_Instance_Record_Access (Instance)))
        & Integer'Image (Instance.Refcount) & ")";
   end Print_Refcount;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : String) return User_Data_List
   is
      D : User_Data_List;
   begin
      if Instance.Initialized then
         D := Instance.Data.Data.User_Data;
         while D /= null loop
            if D.Name = Name then
               return D;
            end if;
            D := D.Next;
         end loop;
      end if;
      return null;
   end Get_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Name     : String;
      Property : Instance_Property_Record'Class)
   is
      D        : User_Data_List := Instance.Data.Data.User_Data;
      Previous : User_Data_List;
   begin
      while D /= null loop
         if D.Name = Name then
            if Previous = null then
               Instance.Data.Data.User_Data := D.Next;
            else
               Previous.Next := D.Next;
            end if;
            Free_User_Data (D);
            exit;
         end if;
         Previous := D;
         D := D.Next;
      end loop;

      Instance.Data.Data.User_Data := new User_Data'
        (Length => Name'Length,
         Name   => Name,
         Next   => Instance.Data.Data.User_Data,
         Prop   => new Instance_Property_Record'Class'(Property));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : String)
   is
   begin
      Set_Data
        (Instance, Get_Name (Name),
         Scalar_Properties_Record'(Typ => Strings, Str => new String'(Value)));
   end Set_Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance; Name : Class_Type; Value : Integer)
   is
   begin
      Set_Data
        (Instance, Get_Name (Name),
         Scalar_Properties_Record'(Typ => Integers, Int => Value));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance;
      Name     : String) return Instance_Property_Record'Class
   is
      D : constant User_Data_List := Get_Data (Instance, Name);
   begin
      if D = null then
         raise Invalid_Data;
      else
         return D.Prop.all;
      end if;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return Integer
   is
      Prop : constant Instance_Property_Record'Class :=
        Get_Data (Instance, Get_Name (Name));
   begin
      return Scalar_Properties_Record (Prop).Int;
   end Get_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Instance : Class_Instance; Name : Class_Type) return String
   is
      Prop : constant Instance_Property_Record'Class :=
        Get_Data (Instance, Get_Name (Name));
   begin
      return Scalar_Properties_Record (Prop).Str.all;
   end Get_Data;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script (Instance : Class_Instance) return Scripting_Language is
   begin
      return Instance.Data.Data.Script;
   end Get_Script;

   -----------------
   -- Is_Subclass --
   -----------------

   function Is_Subclass
     (Instance : Class_Instance; Base : Class_Type) return Boolean is
   begin
      return Is_Subclass (Get_CIR (Instance), Base);
   end Is_Subclass;

   ---------------------
   -- Set_Hide_Output --
   ---------------------

   procedure Set_Hide_Output
     (Console     : access Virtual_Console_Record;
      Hide_Output : Boolean) is
   begin
      Console.Hide_Output := Hide_Output;
   end Set_Hide_Output;

   -------------------------
   -- Set_Default_Console --
   -------------------------

   procedure Set_Default_Console
     (Script       : access Scripting_Language_Record;
      Console      : Virtual_Console) is
   begin
      if Console /= null then
         Set_As_Default_Console (Console, Script.Console, Script);
      end if;
      Script.Console := Console;
      Display_Prompt (Scripting_Language (Script));
   end Set_Default_Console;

   -------------------------
   -- Get_Default_Console --
   -------------------------

   function Get_Default_Console
     (Script : access Scripting_Language_Record) return Virtual_Console is
   begin
      return Script.Console;
   end Get_Default_Console;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Console  : access Virtual_Console_Record'Class) is
   begin
      --  Note: even if Console is a widget, the call to Set_Data_Primitive
      --  below will automatically take care of proper reference counting, so
      --  that no additional work is needed

      Set_Data_Primitive (Instance, Console);
      Set_Data
        (Instance, "virtualconsole",
         Scalar_Properties_Record'
           (Typ => Consoles, Console => Virtual_Console (Console)));
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Instance : Class_Instance) return Virtual_Console is
      D : constant User_Data_List := Get_Data (Instance, "virtualconsole");
   begin
      if D = null
        or else D.Prop.all not in Scalar_Properties_Record'Class
        or else Scalar_Properties (D.Prop).Typ /= Consoles
      then
         return null;
      else
         return Scalar_Properties (D.Prop).Console;
      end if;
   end Get_Data;

   -----------------------
   -- Get_Console_Class --
   -----------------------

   function Get_Console_Class (Repo : Scripts_Repository) return Class_Type is
   begin
      return Repo.Console_Class;
   end Get_Console_Class;

   ----------
   -- Read --
   ----------

   function Read
     (Console    : access Virtual_Console_Record;
      Size       : Integer;
      Whole_Line : Boolean) return String
   is
      pragma Unreferenced (Console, Size, Whole_Line);
   begin
      return "";
   end Read;

   ----------------------------
   -- Process_Pending_Events --
   ----------------------------

   procedure Process_Pending_Events
     (Console : access Virtual_Console_Record'Class) is
   begin
      --  We mustn't do that if the commands are hidden, for some obscur
      --  reason found in GPS (python-gui.adb:1.25)

      if not Console.Hide_Output then
         if Clock - Console.Refresh_Timeout > Timeout_Threshold then
            Process_Pending_Events_Primitive (Console);
            Console.Refresh_Timeout := Clock;
         end if;
      end if;
   end Process_Pending_Events;

end Scripts;
