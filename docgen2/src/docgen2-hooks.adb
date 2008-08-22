-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

with Ada.Text_IO;
with GPS.Kernel; use GPS.Kernel;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;

package body Docgen2.Hooks is

   Docgen_Start_Hook_Type    : constant Hook_Name := "docgen_start_hook";
   Docgen_Finished_Hook_Type : constant Hook_Name := "docgen_finished_hook";
   User_Tag_Action_Args_Type : constant Hook_Type := "docgen_user_tag_args";
   User_Tag_Action_Type      : constant Hook_Name :=
                                 "docgen_user_tag_action_hook";
   type User_Tags_Args
     (Tag_Length   : Natural;
      Attrs_Length : Natural;
      Value_Length : Natural;
      Name_Length  : Natural;
      Href_Length  : Natural)
     is new Hooks_Data with record
      Tag    : String (1 .. Tag_Length);
      Attrs  : String (1 .. Attrs_Length);
      Value  : String (1 .. Value_Length);
      E_Name : String (1 .. Name_Length);
      Href   : String (1 .. Href_Length);
   end record;

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access User_Tags_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  Create Callback_Data from User_Tags_Args

   function From_Callback_Data
     (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return Hooks_Data'Class;
   --  Create User_Tags_Args from Callback_Data

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access User_Tags_Args)
      return GNATCOLL.Scripts.Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 6));
   begin
      Set_Nth_Arg (D.all, 1, String (Hook));
      Set_Nth_Arg (D.all, 2, Data.Tag);
      Set_Nth_Arg (D.all, 3, Data.Attrs);
      Set_Nth_Arg (D.all, 4, Data.Value);
      Set_Nth_Arg (D.all, 5, Data.E_Name);
      Set_Nth_Arg (D.all, 6, Data.Href);
      return D;
   end Create_Callback_Data;

   ---------------------------------
   -- From_Callback_Data_Function --
   ---------------------------------

   function From_Callback_Data
     (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return Hooks_Data'Class
   is
      Tag    : constant String := Nth_Arg (Data, 2);
      Attrs  : constant String := Nth_Arg (Data, 3);
      Value  : constant String := Nth_Arg (Data, 4);
      E_Name : constant String := Nth_Arg (Data, 5);
      Href   : constant String := Nth_Arg (Data, 6);

      Args  : constant User_Tags_Args :=
                (Hooks_Data with
                 Tag_Length   => Tag'Length,
                 Tag          => Tag,
                 Attrs_Length => Attrs'Length,
                 Attrs        => Attrs,
                 Value_Length => Value'Length,
                 Value        => Value,
                 Name_Length  => E_Name'Length,
                 E_Name       => E_Name,
                 Href_Length  => Href'Length,
                 Href         => Href);
   begin
      return Args;
   end From_Callback_Data;

   -------------------
   -- Register_Hook --
   -------------------

   procedure Register_Hook
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Register_Hook_Data_Type
        (Kernel,
         Data_Type_Name => User_Tag_Action_Args_Type,
         Args_Creator   => From_Callback_Data'Access);
      Register_Hook_Return_String
        (Kernel,
         User_Tag_Action_Type,
         User_Tag_Action_Args_Type);
      Register_Hook_No_Return
        (Kernel,
         Docgen_Start_Hook_Type,
         String_Hook_Type);
      Register_Hook_No_Return
        (Kernel,
         Docgen_Finished_Hook_Type,
         String_Hook_Type);
   end Register_Hook;

   -----------------------------------------
   -- Documentation_Generation_Start_Hook --
   -----------------------------------------

   procedure Documentation_Generation_Start_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Doc_Path : String)
   is
      Data : aliased String_Hooks_Args :=
               (Hooks_Data with
                Length => Doc_Path'Length,
                Value  => Doc_Path);
   begin
      Run_Hook (Kernel, Docgen_Start_Hook_Type, Data'Unchecked_Access, True);
   end Documentation_Generation_Start_Hook;

   ------------------------------------------
   -- Documentation_Generation_Finish_Hook --
   ------------------------------------------

   procedure Documentation_Generation_Finish_Hook
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Doc_Path : String)
   is
      Data : aliased String_Hooks_Args :=
               (Hooks_Data with
                Length => Doc_Path'Length,
                Value  => Doc_Path);
   begin
      Run_Hook
        (Kernel, Docgen_Finished_Hook_Type, Data'Unchecked_Access, True);
   end Documentation_Generation_Finish_Hook;

   ---------------------
   -- User_Tag_Action --
   ---------------------

   function User_Tag_Action
     (Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Tag, Attrs, Value : String;
      Entity_Name, Href : String) return String
   is
      Data : aliased User_Tags_Args :=
               (Hooks_Data with
                Tag_Length   => Tag'Length,
                Tag          => Tag,
                Attrs_Length => Attrs'Length,
                Attrs        => Attrs,
                Value_Length => Value'Length,
                Value        => Value,
                Name_Length  => Entity_Name'Length,
                E_Name       => Entity_Name,
                Href_Length  => Href'Length,
                Href         => Href);
   begin
      Ada.Text_IO.Put_Line ("(dbg) running hook for " & Entity_Name & " tag:" &
                            Tag);
      return Run_Hook_Until_Not_Empty
        (Kernel, User_Tag_Action_Type, Data'Unchecked_Access, True);
   end User_Tag_Action;

end Docgen2.Hooks;
