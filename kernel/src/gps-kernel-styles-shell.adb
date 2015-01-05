------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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

with GPS.Kernel.Scripts; use GPS.Kernel.Scripts;

package body GPS.Kernel.Styles.Shell is

   use Style_Htable.String_Hash_Table;

   Class       : constant String := "Style";
   Style_Class : Class_Type;

   Name_Cst       : aliased constant String := "name";
   Create_Cst     : aliased constant String := "create";

   type Style_Property_Record is new Instance_Property_Record with record
      Style : Style_Access;
   end record;
   type Style_Property_Access is access all Style_Property_Record'Class;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Set_Data
     (Instance : Class_Instance;
      Style    : Style_Access);
   --  Set data in Instance to Style

   procedure Style_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the Style commands

   procedure Accessors
     (Data : in out Callback_Data'Class; Command : String);
   --  Handler for the simple Style commands which simply access the fields
   --  of a Style or run parameterless commands

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Instance : Class_Instance;
      Style    : Style_Access) is
   begin
      Set_Data (Instance, Class,
                Style_Property_Record'
                  (Style => Style));
   end Set_Data;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style (Instance : Class_Instance) return Style_Access is
      Prop : Style_Property_Access;
   begin
      if Instance /= No_Class_Instance then
         Prop := Style_Property_Access
           (Instance_Property'(Get_Data (Instance, Class)));

         if Prop /= null then
            return Prop.Style;
         end if;
      end if;

      return null;
   end Get_Style;

   ---------------------------
   -- Style_Command_Handler --
   ---------------------------

   procedure Style_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Style_Inst : Class_Instance;
      Kernel     : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = Constructor_Method then
         Name_Parameters
           (Data,
            (1 => Name_Cst'Access,
             2 => Create_Cst'Access));

         declare
            Name   : constant String := Nth_Arg (Data, 2);
            Create : constant Boolean := Nth_Arg (Data, 3, True);
            Style  : Style_Access;
         begin
            Style := Get_Or_Create_Style (Kernel, Name, Create);
            Style_Inst := Nth_Arg (Data, 1, Style_Class);
            Set_Data (Style_Inst, Style);
         end;

      elsif Command = "list" then
         Set_Return_Value_As_List (Data);

         declare

            Iter    : Style_Htable.String_Hash_Table.Cursor;
            Info    : Style_Access;
         begin

            Get_First (Style_Htable_Access (Kernel.Styles).Table, Iter);
            loop
               Info := Get_Element (Iter);
               exit when Info = null;
               Style_Inst := New_Instance (Get_Script (Data), Style_Class);
               Set_Data (Style_Inst, Info);
               Set_Return_Value (Data, Style_Inst);

               Get_Next (Style_Htable_Access (Kernel.Styles).Table, Iter);
            end loop;
         end;
      end if;
   end Style_Command_Handler;

   ---------------
   -- Accessors --
   ---------------

   procedure Accessors
     (Data : in out Callback_Data'Class; Command : String)
   is
      Style : constant Style_Access := Get_Style
        (Nth_Arg (Data, 1, Style_Class));
   begin
      if Command = "set_foreground" then
         Set_Foreground (Style, Nth_Arg (Data, 2));
      elsif Command = "set_background" then
         Set_Background (Style, Nth_Arg (Data, 2));
      elsif Command = "set_in_speedbar" then
         Set_In_Speedbar (Style, Nth_Arg (Data, 2));
      elsif Command = "get_name" then
         Set_Return_Value (Data, Get_Name (Style));
      elsif Command = "get_foreground" then
         Set_Return_Value (Data, Get_Foreground (Style));
      elsif Command = "get_background" then
         Set_Return_Value (Data, Get_Background (Style));
      elsif Command = "get_in_speedbar" then
         Set_Return_Value (Data, In_Speedbar (Style));
      end if;
   end Accessors;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Style_Class := New_Class (Kernel, Class);

      Register_Command
        (Kernel, Constructor_Method, 1, 2, Style_Command_Handler'Access,
         Style_Class, False);

      Register_Command
        (Kernel, "list", 0, 0, Style_Command_Handler'Access,
         Style_Class, True);

      Register_Command
        (Kernel, "get_foreground", 0, 0, Accessors'Access, Style_Class);
      Register_Command
        (Kernel, "get_background", 0, 0, Accessors'Access, Style_Class);
      Register_Command
        (Kernel, "get_in_speedbar", 0, 0, Accessors'Access, Style_Class);
      Register_Command
        (Kernel, "get_name", 0, 0, Accessors'Access, Style_Class);
      Register_Command
        (Kernel, "set_foreground", 1, 1, Accessors'Access, Style_Class);
      Register_Command
        (Kernel, "set_background", 1, 1, Accessors'Access, Style_Class);
      Register_Command
        (Kernel, "set_in_speedbar", 1, 1, Accessors'Access, Style_Class);
   end Register_Commands;

end GPS.Kernel.Styles.Shell;
