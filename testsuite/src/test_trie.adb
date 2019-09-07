------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO; use Ada.Text_IO;
with Tries;
with Ada.Numerics.Discrete_Random;
with HTables; use HTables;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with String_Utils;
with GNAT.Strings; use GNAT.Strings;
with GNATCOLL.Traces;  use GNATCOLL.Traces;
with GNATCOLL.Utils;   use GNATCOLL.Utils;
with GNATCOLL.VFS;     use GNATCOLL.VFS;

procedure Test_Trie is
   Mesure_Timing : constant Boolean := False;
   --  Set to true to mesure the timing

   Verbose : constant Boolean := False;

   package Char_Random is new Ada.Numerics.Discrete_Random (Character);
   use Char_Random;

   type My_Data is record
      Index : GNAT.Strings.String_Access;
      Value : Integer;
   end record;
   No_Data : constant My_Data := (null, Integer'First);

   procedure Free (Data : in out My_Data);
   function Get_Index (Data : My_Data) return Cst_String_Access;
   package Integer_Tries is new Tries (My_Data, No_Data, Get_Index);
   use Integer_Tries;

   type Header_Num is range 1 .. 20_000;
   function Hash is new String_Utils.Hash (Header_Num);
   package Integer_Hash is new Simple_HTable
     (Header_Num, My_Data, No_Data, String, Hash, "=", Free);
   use Integer_Hash;

   Length_Inserted : Natural := 0;
   Random_Items_Count : constant := 30_000;
   Tree : aliased Trie_Tree := Empty_Case_Sensitive_Trie_Tree;
   Str : Unbounded_String;

   procedure Str_Put (S : String);
   procedure Str_Put (Item : My_Data);
   procedure Dump is new Integer_Tries.Dump (Str_Put, Str_Put);

   procedure Str_Put_No_Data (Item : My_Data);
   procedure Dump_No_Data is new Integer_Tries.Dump (Str_Put, Str_Put_No_Data);

   procedure Assert (S1, S2 : Integer; Comment : String := "");
   procedure Assert (S1, S2 : String; Comment : String := "");

   procedure Insert_And_Dump
     (Index : String; Value : Integer; Display_Data : Boolean := True);
   --  Insert a new entry in the tree, and dump it

   procedure Remove_And_Dump (Index : String; Display_Data : Boolean := True);
   --  Remove an item from the tree and dump the tree

   procedure Check_Iter (Prefix : String);
   --  Prints all items starting with Prefix

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out My_Data) is
   begin
      if Data.Index /= null then
         Str := Str & " (free" & Data.Index.all & ")";
      else
         Str := Str & " (free no_data)";
      end if;
      Free (Data.Index);
   end Free;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index (Data : My_Data) return Cst_String_Access is
   begin
      return Cst_String_Access (Data.Index);
   end Get_Index;

   ------------
   -- Assert --
   ------------

   procedure Assert (S1, S2 : String; Comment : String := "") is
   begin
      if S1 /= S2 then
         Put_Line ("--- Fail: " & Comment);
         Put_Line ("Str1=" & S1 & "--");
         Put_Line ("Str2=" & S2 & "--");
      elsif Verbose then
         Put_Line ("+++ Success: " & Comment);
         Put_Line ("Str1=" & S1 & "--");
         Put_Line ("Str2=" & S2 & "--");
      end if;
   end Assert;

   procedure Assert (S1, S2 : Integer; Comment : String := "") is
   begin
      if S1 /= S2 then
         Put_Line ("--- Fail: " & Comment);
         Put_Line ("Str1=" & Integer'Image (S1) & "--");
         Put_Line ("Str2=" & Integer'Image (S2) & "--");
      elsif Verbose then
         Put_Line ("+++ Success: " & Comment);
         Put_Line ("Str1=" & Integer'Image (S1) & "--");
         Put_Line ("Str2=" & Integer'Image (S2) & "--");
      end if;
   end Assert;

   -------------
   -- Str_Put --
   -------------

   procedure Str_Put (S : String) is
   begin
      Str := Str & S;
   end Str_Put;

   -------------
   -- Str_Put --
   -------------

   procedure Str_Put (Item : My_Data) is
   begin
      Str := Str & Integer'Image (Item.Value);
   end Str_Put;

   ---------------------
   -- Str_Put_No_Data --
   ---------------------

   procedure Str_Put_No_Data (Item : My_Data) is
      pragma Unreferenced (Item);
   begin
      null;
   end Str_Put_No_Data;

   ---------------------
   -- Insert_And_Dump --
   ---------------------

   procedure Insert_And_Dump
     (Index : String; Value : Integer; Display_Data : Boolean := True) is
   begin
      Str := To_Unbounded_String ("");
      Insert (Tree,
              My_Data'(new String'(Index),
                       Value => Value));
      Length_Inserted := Length_Inserted + Index'Length;

      if Display_Data then
         Dump (Tree);
      else
         Dump_No_Data (Tree);
      end if;
   end Insert_And_Dump;

   ---------------------
   -- Remove_And_Dump --
   ---------------------

   procedure Remove_And_Dump
     (Index : String; Display_Data : Boolean := True) is
   begin
      Str := To_Unbounded_String ("");
      Remove (Tree, Index);

      if Display_Data then
         Dump (Tree);
      else
         Dump_No_Data (Tree);
      end if;
   end Remove_And_Dump;

   ----------------
   -- Check_Iter --
   ----------------

   procedure Check_Iter (Prefix : String) is
      Iter : Integer_Tries.Iterator;
   begin
      Str := To_Unbounded_String ("");
      Iter := Start (Tree'Access, Prefix);
      loop
         exit when Get (Iter) = No_Data;
         Str := Str & ' ' & Get (Iter).Index.all & Get (Iter).Value'Img;
         Next (Iter);
      end loop;
   end Check_Iter;

begin
   GNATCOLL.Traces.Parse_Config_File (Create_From_Base (".gnatdebug"));

   --  Scenario=5
   Insert_And_Dump ("MANU", 1);
   Assert (To_String (Str),
           "(''a -2147483648 ('MANU'M  1 ))",
           "Inserting MANU 1");

   --  Scenario=4
   Insert_And_Dump ("FOO", 2);
   Assert (To_String (Str),
           "(''a -2147483648 ('FOO'F  2 )('MANU'M  1 ))",
           "Inserting FOO 2");

   --  Scenario=2
   Insert_And_Dump ("MAN", 3);
   Assert (To_String (Str),
           "(''a -2147483648 ('FOO'F  2 )('MAN'M  3 ('U'U  1 )))",
           "Inserting MAN 3");

   --  Scenario=3
   Insert_And_Dump ("MAN", 4);
   Assert (To_String (Str),
           " (freeMAN)(''a -2147483648 ('FOO'F  2 )('MAN'M  4 ('U'U  1 )))",
           "Inserting MAN 4");
   Length_Inserted := Length_Inserted - 3;

   --  Scenario=4
   Insert_And_Dump ("MANUEL", 5);
   Assert (To_String (Str),
           "(''a -2147483648 ('FOO'F  2 )('MAN'M  4 ('U'U  1 ('EL'E  5 ))))",
           "Inserting MANUEL 5");

   --  Scenario=4
   Insert_And_Dump ("MANUTENTION", 6);
   Assert (To_String (Str),
           "(''a -2147483648 ('FOO'F  2 )('MAN'M  4 ('U'U  1 ('EL'E  5 )"
           & "('TENTION'T  6 ))))",
           "Inserting MANUTENTION 6");

   --  Scenario=4
   Insert_And_Dump ("MANUELAA", 7);
   Assert (To_String (Str),
           "(''a -2147483648 ('FOO'F  2 )"
           & "('MAN'M  4 ('U'U  1 ('EL'E  5 ('AA'A  7 ))"
           & "('TENTION'T  6 ))))",
           "Inserting MANUELAA 7");

   --  Scenario=4
   Insert_And_Dump ("MANUTENTAL", 8);
   Assert (To_String (Str),
           "(''a -2147483648 ('FOO'F  2 )"
           & "('MAN'M  4 ('U'U  1 ('EL'E  5 ('AA'A  7 ))"
           & "('TENT'T -2147483648 ('AL'A  8 )('ION'I  6 ))))"
           & ")",
           "Inserting MANUTENTAL 8");

   Assert (Length_Inserted, 45, "Total length inserted");

   Assert (Get (Tree'Access, "MANU").Value,       1, "Value for MANU");
   Assert (Get (Tree'Access, "MAN").Value,        4, "Value for MAN");
   Assert (Get (Tree'Access, "MANUTENTAL").Value, 8, "Value for MANUTENTAL");
   Assert (Get (Tree'Access, "QWRE").Value,       Integer'First,
           "Value for unknown item");

   Check_Iter ("MANU");
   Assert (To_String (Str),
           " MANU 1 MANUEL 5 MANUELAA 7 MANUTENTAL 8 MANUTENTION 6",
           "All items starting with MANU");

   Check_Iter ("MANUE");
   Assert (To_String (Str),
           " MANUEL 5 MANUELAA 7",
           "All items starting with MANUE");

   Check_Iter ("F");
   Assert (To_String (Str),
           " FOO 2",
           "All items starting with FOO");

   Check_Iter ("");
   Assert (To_String (Str),
          " FOO 2 MAN 4 MANU 1 MANUEL 5 MANUELAA 7 MANUTENTAL 8 MANUTENTION 6",
          "All items starting with null prefix");

   Check_Iter ("MANUWW");
   Assert (To_String (Str),
           "",
           "All items starting with MANUWW");

   Check_Iter ("TI");
   Assert (To_String (Str),
           "",
           "All items starting with TI");

   Clear (Tree);
   Assert
     (Get (Start (Tree'Access, "")).Value, Integer'First, "Clear the tree");

   Dump (Tree);
   Assert (To_String (Str),
           " (free no_data) (freeFOO) (freeMAN) (freeMANU) (freeMANUEL) "
           & "(freeMANUELAA) (free no_data) (freeMANUTENTAL) "
           & "(freeMANUTENTION)('')",
           "Tree after clear");

   if Mesure_Timing then

      New_Line;
      Put_Line ("Inserting" & Random_Items_Count'Img & " items at random");
      declare
         Gen : Generator;
         Len : Integer;
         Start : constant Time := Clock;
         S   : GNAT.Strings.String_Access;
      begin
         Reset (Gen);
         Length_Inserted := 0;

         for Count in 1 .. Random_Items_Count loop
            Len := Character'Pos (Random (Gen)) + 5;
            Length_Inserted := Length_Inserted + Len;
            S := new String (1 .. Len);
            for L in S'Range loop
                  S (L) := Random (Gen);
            end loop;
            Insert (Tree, (S, 1));
         end loop;

         Put_Line ("Time=" & Duration'Image (Clock - Start));

         Put_Line ("Length_Inserted=" & Length_Inserted'Img);
      end;

      New_Line;
      Put_Line ("Inserting" & Random_Items_Count'Img & " items in hash table");
      declare
         Table : Instance;
         Gen : Generator;
         Len : Integer;
         Start : constant Time := Clock;
         S   : GNAT.Strings.String_Access;
      begin
         Reset (Gen);
         Length_Inserted := 0;

         for Count in 1 .. Random_Items_Count loop
            Len := Character'Pos (Random (Gen)) + 5;
            Length_Inserted := Length_Inserted + Len;
            S := new String (1 .. Len);
            for L in S'Range loop
               S (L) := Random (Gen);
            end loop;
            Set (Table, S.all, (S, 1));
         end loop;

         Put_Line ("Time=" & Duration'Image (Clock - Start));
      end;
   end if;

   --  Check that we do not insert empty nodes
   Clear (Tree);
   Insert_And_Dump ("bar", 1);
   Insert_And_Dump ("b", 2);
   Assert (To_String (Str),
           "(''a -2147483648 ('b'b  2 ('ar'a  1 )))",
           "Inserting b after bar");

   --  Check that we do not match if only part of the index matches
   Clear (Tree);
   Insert_And_Dump ("interactive_canvas_record", 1);
   Assert (Get (Tree'Access, "int").Value, Integer'First, "Value for int");

   --  Check removing and cached index
   Clear (Tree);
   Insert_And_Dump ("s", 1);
   Insert_And_Dump ("sdep", 2);
   Insert_And_Dump ("sdep_id", 3);
   Assert (To_String (Str),
           "(''a -2147483648 ('s's  1 ('dep'd  2 ('_id'_  3 ))))",
           "Inserting sdep_id");
   Remove_And_Dump ("sdep");
   Assert (To_String (Str),
           " (freesdep)(''a -2147483648 ('s's  1 ('dep_id'd  3 )))",
           "Removing sdep");

   Clear (Tree);
   Insert_And_Dump ("load_default", 1);
   Insert_And_Dump ("log_domain", 2);
   Insert_And_Dump ("log", 3);
   Assert (To_String (Str),
           "(''a -2147483648 ('lo'l -2147483648 ('ad_default'a  1 )"
           & "('g'g  3 ('_domain'_  2 ))))", "Invalid cache");

   Clear (Tree);
   Insert_And_Dump ("combo", 1);
   Insert_And_Dump ("gtkada_combo", 2);
   Insert_And_Dump ("gtkada_combo_record", 3);
   Insert_And_Dump ("gtk_new", 4);
   Assert (To_String (Str),
           "(''a -2147483648 ('combo'c  1 )('gtk'g -2147483648"
             & " ('_new'_  4 )('ada_combo'a  2 ('_record'_  3 ))))");
   Remove_And_Dump ("combo", Display_Data => False);
   Assert (To_String (Str),
           " (freecombo)(''a  ('gtk'g  ('_new'_  )('ada_combo'a  "
           & "('_record'_  ))))");

   Clear (Tree);
   Insert_And_Dump ("combo", 1);
   Insert_And_Dump ("gtkada_combo", 2);
   Insert_And_Dump ("gtkada_combo_record", 3);
   Insert_And_Dump ("gtk_new", 4, Display_Data => False);
   Assert (To_String (Str),
           "(''a  ('combo'c  )('gtk'g  ('_new'_  )('ada_combo'a"
           & "  ('_record'_  ))))");
   Remove_And_Dump ("combo", Display_Data => False);
   Assert (To_String (Str),
           " (freecombo)(''a  ('gtk'g  ('_new'_  )('ada_combo'a"
           & "  ('_record'_  ))))");

   GNATCOLL.Traces.Finalize;
end Test_Trie;
