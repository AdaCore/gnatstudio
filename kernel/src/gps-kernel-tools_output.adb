------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Ada.Characters.Latin_1;           use Ada.Characters.Latin_1;
with Ada.Strings;                      use Ada.Strings;
with Ada.Strings.Fixed;                use Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Indefinite_Ordered_Maps;

with String_List_Utils;

package body GPS.Kernel.Tools_Output is
   use String_List_Utils.String_List;

   type Output_Parser_Fabric_Access is access all Output_Parser_Fabric'Class;

   package Fabric_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Output_Parser_Fabric_Access);

   package Target_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (String, List);

   function To_Parser_List (Parser_List : String) return List;
   --  Convert string with parser_names to list of parser

   Map : Fabric_Maps.Map;
   --  Map of registered parser sorted by unique parser name
   --  ??? Should be moved to a module or the kernel

   Target_Parsers : Target_Maps.Map;
   --  Map from target name to list of output parser names
   --  ??? Should be moved to a module or the kernel

   Default_Macros : constant String := "[default]";

   Default_Parser_Names : constant String :=
     "output_chopper"   & " " &
     "utf_converter"    & " " &
     "progress_parser"  & " " &
     "console_writer"   & " " &
     "location_parser"  & " " &
     "text_splitter"    & " " &
     "output_collector" & " " &
     "elaboration_cycles";

   Space : constant Ada.Strings.Maps.Character_Set :=
     Ada.Strings.Maps.To_Set (" " & CR & LF & HT);
   --  Character set to separate parser names in a list

   External_Parsers_Fabric : External_Parser_Fabric_Access;

   --------------------
   -- To_Parser_List --
   --------------------

   function To_Parser_List (Parser_List : String) return List is
      Macros : constant Natural := Index (Parser_List, Default_Macros);
      First  : Positive;
      Last   : Natural := 0;
      Result : List;
   begin
      if Macros > 0 then
         return To_Parser_List
           (Replace_Slice
              (Parser_List,
               Low  => Macros,
               High => Macros + Default_Macros'Length - 1,
               By   => Default_Parser_Names));
      end if;
      loop
         Find_Token (Parser_List, Space, Last + 1, Outside, First, Last);
         exit when First > Last;
         Prepend (Result, Parser_List (First .. Last));
      end loop;

      return Result;
   end To_Parser_List;

   Default_Parsers : constant List := To_Parser_List (Default_Parser_Names);

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : not null access Tools_Output_Parser) is
      Child : Tools_Output_Parser_Access := Self.Child;
   begin
      if Child /= null then
         Free (Child);
      end if;
   end Destroy;

   -------------------
   -- End_Of_Stream --
   -------------------

   procedure End_Of_Stream
     (Self    : not null access Tools_Output_Parser;
      Command : Command_Access) is
   begin
      if Self.Child /= null then
         Self.Child.End_Of_Stream (Command);
      end if;
   end End_Of_Stream;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Tools_Output_Parser_Access) is
      procedure Free_Instance is
        new Ada.Unchecked_Deallocation
          (Tools_Output_Parser'Class, Tools_Output_Parser_Access);
   begin
      Self.Destroy;
      Free_Instance (Self);
   end Free;

   -----------------
   -- Set_Parsers --
   -----------------

   procedure Set_Parsers
     (Target_Name : String;
      Parser_List : String) is
   begin
      if Parser_List = "" then
         if Target_Parsers.Contains (Target_Name) then
            Target_Parsers.Delete (Target_Name);
         end if;
      else
         Target_Parsers.Include (Target_Name, To_Parser_List (Parser_List));
      end if;
   end Set_Parsers;

   ----------------------
   -- New_Parser_Chain --
   ----------------------

   function New_Parser_Chain
     (Target_Name : String) return Tools_Output_Parser_Access
   is
      function Get_Parser_List (Target_Name : String) return List;
      --  Return list of parser names for given target

      ---------------------
      -- Get_Parser_List --
      ---------------------

      function Get_Parser_List (Target_Name : String) return List is
      begin
         if Target_Parsers.Contains (Target_Name) then
            return Target_Parsers (Target_Name);
         else
            return Default_Parsers;
         end if;
      end Get_Parser_List;

      Node    : List_Node := First (Get_Parser_List (Target_Name));
      Result  : Tools_Output_Parser_Access;
      Found   : Boolean;
   begin
      while Node /= Null_Node loop
         if Map.Contains (Data (Node)) then
            declare
               Fabric : constant Output_Parser_Fabric_Access :=
                 Map (Data (Node));
            begin
               Result := Fabric.Create (Result);
            end;

            Node := Next (Node);
         elsif External_Parsers_Fabric = null then
            Node := Next (Node);
         else
            External_Parsers_Fabric.Create_External_Parsers
              (Node, Result, Found);

            if not Found then
               --  Both Ada and python parsers not found
               --  Report error and skip to next parser
               Node := Next (Node);
            end if;
         end if;
      end loop;

      return Result;
   end New_Parser_Chain;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   procedure Parse_Standard_Output
     (Self    : not null access Tools_Output_Parser;
      Item    : String;
      Command : Command_Access) is
   begin
      if Self.Child /= null then
         Self.Child.Parse_Standard_Output (Item, Command);
      end if;
   end Parse_Standard_Output;

   --------------------------
   -- Parse_Standard_Error --
   --------------------------

   procedure Parse_Standard_Error
     (Self    : not null access Tools_Output_Parser;
      Item    : String;
      Command : Command_Access) is
   begin
      if Self.Child /= null then
         Self.Child.Parse_Standard_Error (Item, Command);
      end if;
   end Parse_Standard_Error;

   ----------------------------
   -- Register_Output_Parser --
   ----------------------------

   procedure Register_Output_Parser
     (Fabric   : access Output_Parser_Fabric'Class;
      Name     : String) is
   begin
      Map.Insert (Name, Output_Parser_Fabric_Access (Fabric));
   end Register_Output_Parser;

   --------------------------------
   -- Set_External_Parser_Fabric --
   --------------------------------

   procedure Set_External_Parser_Fabric
     (Value : External_Parser_Fabric_Access) is
   begin
      External_Parsers_Fabric := Value;
   end Set_External_Parser_Fabric;

end GPS.Kernel.Tools_Output;
