-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Expect;     use GNAT.Expect;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Generic_Values;  use Generic_Values;
with Process_Proxies; use Process_Proxies;

package body Debugger is

   Remote_Protocol : constant String := "rsh";
   --  How to run a process on a remote machine ?

   ----------------
   -- Parse_Type --
   ----------------

   function Parse_Type
     (Debugger : access Debugger_Root'Class;
      Entity   : String) return Generic_Values.Generic_Type_Access
   is
      Result : Generic_Type_Access;
      Type_Str : String := Type_Of (Debugger, Entity);
      Index  : Natural := Type_Str'First;

   begin
      if Type_Str'Length /= 0 then
         Language.Parse_Type
           (Debugger.The_Language, Type_Str, Entity, Index, Result);
      end if;

      return Result;
   end Parse_Type;

   -----------------
   -- Parse_Value --
   -----------------

   procedure Parse_Value
     (Debugger  : access Debugger_Root'Class;
      Entity    : String;
      Value     : in out Generic_Values.Generic_Type_Access)
   is
      Type_Str   : String := Value_Of (Debugger, Entity);
      Index      : Natural := Type_Str'First;
      Repeat_Num : Positive;

   begin
      --  Clear the value previously parsed.
      Clear_Value (Value.all);
      Language.Parse_Value
        (Debugger.The_Language, Type_Str, Index, Value, Repeat_Num);
   end Parse_Value;

   ------------------
   -- Set_Language --
   ------------------

   procedure Set_Language
     (Debugger     : access Debugger_Root;
      The_Language : Language.Language_Access) is
   begin
      Language.Free (Debugger.The_Language);
      Debugger.The_Language := The_Language;
   end Set_Language;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Language
     (Debugger : access Debugger_Root) return Language.Language_Access is
   begin
      return Debugger.The_Language;
   end Get_Language;

   -----------------
   -- Get_Process --
   -----------------

   function Get_Process
     (Debugger : access Debugger_Root) return Process_Proxy_Access is
   begin
      return Debugger.Process;
   end Get_Process;

   -------------------
   -- General_Spawn --
   -------------------

   procedure General_Spawn
     (Debugger       : access Debugger_Root'Class;
      Arguments      : GNAT.OS_Lib.Argument_List;
      Debugger_Name  : String;
      Proxy          : Process_Proxies.Process_Proxy_Access;
      Remote_Machine : String := "")
   is
      Descriptor : Process_Descriptor_Access;
   begin
      --  Start the external debugger.
      --  Note that there is no limitation on the buffer size, since we can
      --  not control the length of what gdb will return...

      Debugger.Process := Proxy;

      if Remote_Machine = "" then
         Descriptor := new Process_Descriptor'
           (Non_Blocking_Spawn
             (Debugger_Name, Arguments,
              Buffer_Size => 0,
              Err_To_Out => True));

      else
         declare
            Real_Arguments : Argument_List (1 .. Arguments'Length + 2);
         begin
            Real_Arguments (1) := new String'(Remote_Machine);
            Real_Arguments (2) := new String'(Debugger_Name);
            Real_Arguments (3 .. Real_Arguments'Last) := Arguments;

            Descriptor := new Process_Descriptor'
              (Non_Blocking_Spawn
                (Remote_Protocol,
                 Real_Arguments,
                 Buffer_Size => 0,
                 Err_To_Out => True));
            Free (Real_Arguments (1));
            Free (Real_Arguments (2));
         end;
      end if;

      Set_Descriptor (Debugger.Process, Descriptor);
   end General_Spawn;

   ---------------------
   -- Found_File_Name --
   ---------------------

   procedure Found_File_Name (Debugger   : access Debugger_Root;
                              Str        : String;
                              Name_First : out Natural;
                              Name_Last  : out Positive;
                              Line       : out Natural)
   is
   begin
      Name_First := 0;
      Name_Last  := 1;
      Line       := 0;
   end Found_File_Name;

end Debugger;
