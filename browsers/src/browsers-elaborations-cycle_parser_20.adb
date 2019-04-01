------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2019, AdaCore                          --
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

with Ada.Strings.Fixed;
with GNAT.Regpat;                      use GNAT.Regpat;

with GNATCOLL.VFS;
with Basic_Types;
with GPS.Kernel.Messages.Simple;
with String_Utils;

package body Browsers.Elaborations.Cycle_Parser_20 is

--  Binder circularities messages:
--
--  error: Elaboration circularity detected
--  info:
--  info:    Reason:
--  info:
--  info:      unit UNIT depends on its own elaboration
--  info:
--  info:    Circularity:
--  info:
--  info:      CYCLE
--  info:
--  info:    Suggestions:
--  info:
--  info:      TACTICS
--  info:
--
--  Example:
--
--      error: Elaboration circularity detected
--      info:
--      info:    Reason:
--      info:
--      info:      unit "a (spec)" depends on its own elaboration
--      info:
--      info:    Circularity:
--      info:
--      info:      unit "a (spec)" has with clause and pragma Elaborate for
--  unit "b (spec)"
--      info:      unit "b (body)" is in the closure of pragma Elaborate
--      info:      unit "b (body)" invokes a construct of unit "c (body)" at
--  elaboration time
--      info:      unit "c (body)" has with clause for unit "a (spec)"
--      info:
--      info:    Suggestions:
--      info:
--      info:      remove pragma Elaborate for unit "b (body)" in unit "a
--  (spec)"
--      info:      use the dynamic elaboration model (compiler switch -gnatE)
--      info:

--  All units in all messages have the following structure
--
--      UNIT ::= "UNIT_NAME (UNIT_KIND)"
--
--      UNIT_NAME ::= <a legal unit name>
--
--      UNIT_KIND ::= spec | body
--
--  where <a legal unit name> is a name of a unit that satisfies Ada
--  semantics.
--
--  Example:
--
--      "a (spec)"
--      "my_unit (body)"

--  The cycle messages are quite diverse, especially when compiler switch
--  -gnatd_F is in effect (more on this later). The general cycle message
--  format is:
--
--      CYCLE ::= {CYCLE_MESSAGE}?
--
--      CYCLE_MESSAGE ::=
--          unit UNIT has with clause and pragma Elaborate_All for unit UNIT
--        | unit UNIT is in the closure of pragma Elaborate_All
--        | unit UNIT has with clause and pragma Elaborate for unit UNIT
--        | unit UNIT is in the closure of pragma Elaborate
--        | unit UNIT is subject to pragma Elaborate_Body
--        | unit UNIT is in the closure of pragma Elaborate_Body
--        | unit UNIT has with clause for unit UNIT
--        | unit UNIT has a dependency on unit UNIT forced by -f switch
--        | unit UNIT invokes a construct of unit UNIT at elaboration time
--
--  If compiler switch -gnatd_F is in effect, cycle message
--
--      unit UNIT invokes a construct of unit UNIT at elaboration time
--
--  is further supplemented by additional messages describing the flow of
--  elaboration code. The general format of this augmented message is:
--
--      unit UNIT invokes a construct of unit UNIT at elaboration time
--        INVOCATION_PATHS
--
--  where
--
--      INVOCATION_PATHS ::= {INVOCATION_PATH} INVOCATION_PATH
--
--      INVOCATION_PATH ::=
--        path PATH_ID:<new line>
--          elaboration of unit UNIT
--          INVOCATIONS
--
--      INVOCATIONS ::= {INVOCATION}?
--
--      INVOCATION ::=
--          selection of entry TARGET declared at UNIT:LINE:COLUMN
--        | aliasing of subprogram TARGET declared at UNIT:LINE:COLUMN
--        | call to subprogram TARGET declared at UNIT:LINE:COLUMN
--        | adjustment actions for type TARGET declared at UNIT:LINE:COLUMN
--        | finalization actions for type TARGET declared at UNIT:LINE:COLUMN
--        | initialization actions for type TARGET declared at UNIT:LINE:COLUMN
--        | verification of Default_Initial_Condition for type TARGET
--  declared at UNIT:LINE:COLUMN
--        | verification of Initial_Condition declared at UNIT:LINE:COLUMN
--        | instantiation TARGET declared at UNIT:LINE:COLUMN
--        | verification of invariant for type TARGET declared at
--  UNIT:LINE:COLUMN
--        | verification of postcondition for subprogram TARGET declared at
--  UNIT:LINE:COLUMN
--        | call to protected entry TARGET declared at UNIT:LINE:COLUMN
--        | call to protected subprogram TARGET declared at UNIT:LINE:COLUMN
--        | activation of local task declared at UNIT:LINE:COLUMN
--        | call to task entry TARGET declared at UNIT:LINE:COLUMN
--
--      TARGET ::= "<a legal target name>"
--
--      LINE ::= positive
--
--      COLUMN ::= positive
--
--  where <a legal target name> is the name of an entry, subprogram, type,
--    instantiation that satisfies Ada semantics.
--
--  Example:
--
--      info:      unit "a (spec)" invokes a construct of unit "b (body)" at
--  elaboration time
--      info:        path 1:
--      info:          elaboration of unit "a (spec)"
--      info:          call to protected subprogram "prot_func" declared at
--  "b.ads":27:16
--      info:        path 2:
--      info:          elaboration of unit "a (spec)"
--      info:          activation of local task declared at "b.ads":30:14
--      info:        path 3:
--      info:          elaboration of unit "a (spec)"
--      info:          initialization actions for type "ctrl" declared at
--  "b.ads":10:14
--      info:        path 4:
--      info:          elaboration of unit "a (spec)"
--      info:          call to subprogram "func" declared at "b.ads":5:13
--      info:        path 5:
--      info:          elaboration of unit "a (spec)"
--      info:          adjustment actions for type "ctrl" declared at
--  "b.ads":8:14

--  The tactics messages are also quite diverse. The general tactics
--  message format is:
--
--      TACTICS ::= {TACTIC}?
--
--      TACTIC ::=
--          diagnose all circularities (binder switch -d_C)
--        | use the dynamic elaboration model (compiler switch -gnatE)
--        | change pragma Elaborate_All for unit UNIT to Elaborate in unit UNIT
--        | remove pragma Elaborate_All for unit UNIT in unit UNIT
--        | remove pragma Elaborate_Body in unit UNIT
--        | remove pragma Elaborate for unit UNIT in unit UNIT
--        | remove the dependency of unit UNIT on unit UNIT from argument of
--  switch -f
--        | remove switch -f
--        | use detailed invocation information (compiler switch -gnatd_F)

--  All these messages can be found in GNAT file Bindo.Diagnostics,
--  scattered in various routines.

   Info : constant String := "info:\s+";
   --  Common binder prefix

   Unit : constant String := """([^""]+)""";
   --  Placeholder for unit name (unit kind) in quotes. Example: "aaa (body)"

   --  Compiled matchers:

   Info_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "$");

   Reason_Header_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "Reason:");

   Reason_Info_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "unit " & Unit & " depends on its own elaboration");

   Circularity_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "Circularity:");

   Cycle_Prefix_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "unit " & Unit & " ");

   Cycle_Unit_Suffix_Pattern : constant Pattern_Matcher :=
     Compile (".+" & "unit " & Unit);

   Path_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "path (\d+):");

   Unit_Line_Column : constant Pattern_Matcher :=
     Compile (Unit & ":(\d+):(\d+)");

   Spaces_In_Path_Pattern : constant Pattern_Matcher :=
     Compile ("^info:(\s+)\S");

   Suggestions_Pattern : constant Pattern_Matcher :=
     Compile ("^" & Info & "Suggestions:");

   Messages_Category : constant String := "Elaboration circularity detected";

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      Empty : Elaboration_Cycles.Cycle;
   begin
      GPS.Kernel.Get_Messages_Container
        (Self.Kernel).Remove_Category
        (Messages_Category, GPS.Kernel.Messages.Side_And_Locations);

      Set_Elaboration_Cycle (Empty);

      return new Circularity_Parser'
        (Kernel      => Self.Kernel,
         Child       => Child,
         Last_Error  => <>,
         State       => Expect_Error,
         Path_Lenght => 0,
         Path_Id     => 0);
   end Create;

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Circularity_Parser;
      Item    : String;
      Command : access Root_Command'Class)
   is
      use Elaboration_Cycles;
      use GNATCOLL.VFS;

      Fallback : Boolean := True;
      --  Whether we need to iterate over loop once more

      Matched  : Match_Array (0 .. 3);
      Before   : Unbounded_String;
      After    : Unbounded_String;
      Dep      : Elaboration_Cycles.Dependency;
   begin
      --  Skip empty lines
      if Item = "" or Item = (1 => ASCII.LF) then
         Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
         return;
      end if;

      while Fallback loop
         Fallback := False;

         if Match (Info_Pattern, Item) then
            --  Empty "info:" line used as a separator, skip it
            if Self.State = Invocations then
               Self.State := Cycle;
            end if;
            exit;
         end if;

         case Self.State is
            when Expect_Error =>
               if Dependencies_Count (Self.Last_Error) /= 0 then
                  declare
                     Empty : Elaboration_Cycles.Cycle;
                  begin
                     Set_Elaboration_Cycle (Self.Last_Error);
                     Self.Last_Error := Empty;
                  end;
               end if;

               if String_Utils.Looking_At
                 (Item, Item'First, "error: Elaboration circularity detected")
               then
                  Self.State := Reason_Header;
               end if;

            when Reason_Header =>
               if not Match (Reason_Header_Pattern, Item) then
                  Self.State := Expect_Error;
                  Fallback   := True;
               else
                  Self.State := Reason_Info;
               end if;

            when Reason_Info =>
               if not Match (Reason_Info_Pattern, Item) then
                  Self.State := Expect_Error;
                  Fallback   := True;
               else
                  Self.State := Circularity;
               end if;

            when Circularity =>
               if not Match (Circularity_Pattern, Item) then
                  Self.State := Expect_Error;
                  Fallback   := True;
               else
                  Self.State := Cycle;
               end if;

            when Cycle  =>
               if Match (Suggestions_Pattern, Item) then
                  --  All cycles has been parsed, skip suggestions
                  Self.State := Expect_Error;
                  exit;
               end if;

               Match (Path_Pattern, Item, Matched);
               if Matched (0) /= No_Match then
                  Self.Path_Id := Integer'Value
                    (Item (Matched (1).First .. Matched (1).Last));
                  Match (Spaces_In_Path_Pattern, Item, Matched);
                  Self.Path_Lenght := Matched (1).Last;
                  Self.State := Invocations;
                  exit;
               end if;

               if not Match (Cycle_Prefix_Pattern, Item) then
                  --  Not a cycle
                  Self.State := Expect_Error;
                  exit;
               end if;

               Before := To_Unbounded_String
                 (Item (Matched (1).First .. Matched (1).Last));

               declare
                  Rest_Of_Item : constant String :=
                    Item (Matched (0).Last + 1 .. Item'Last);
               begin
                  if String_Utils.Looking_At
                    (Rest_Of_Item,
                     Rest_Of_Item'First,
                     "has with clause and pragma Elaborate_All for")
                  then
                     Match (Cycle_Unit_Suffix_Pattern, Rest_Of_Item, Matched);
                     if Matched (0) /= No_Match then
                        After := To_Unbounded_String
                          (Rest_Of_Item (Matched (1).First ..
                             Matched (1).Last));
                     end if;

                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (After),
                        Pragma_Elaborate_All);
                     Append (Self.Last_Error, Dep);

                  elsif Rest_Of_Item =
                    "is in the closure of pragma Elaborate_All"
                  then
                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (Before),
                        Pragma_Elaborate_All_Closure);
                     Append (Self.Last_Error, Dep);

                  elsif String_Utils.Looking_At
                    (Rest_Of_Item,
                     Rest_Of_Item'First,
                     "has with clause and pragma Elaborate for")
                  then
                     Match (Cycle_Unit_Suffix_Pattern, Rest_Of_Item, Matched);
                     if Matched (0) /= No_Match then
                        After := To_Unbounded_String
                          (Rest_Of_Item (Matched (1).First ..
                             Matched (1).Last));
                     end if;

                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (After),
                        Pragma_Elaborate);
                     Append (Self.Last_Error, Dep);

                  elsif Rest_Of_Item =
                    "is in the closure of pragma Elaborate"
                  then
                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (Before),
                        Pragma_Elaborate_Closure);
                     Append (Self.Last_Error, Dep);

                  elsif Rest_Of_Item =
                    "is subject to pragma Elaborate_Body"
                  then
                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (Before),
                        Elaborate_Body_Subject);
                     Append (Self.Last_Error, Dep);

                  elsif Rest_Of_Item =
                    "is in the closure of pragma Elaborate_Body"
                  then
                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (Before),
                        Elaborate_Body_Closure);
                     Append (Self.Last_Error, Dep);

                  elsif String_Utils.Looking_At
                    (Rest_Of_Item,
                     Rest_Of_Item'First,
                     "has with clause for")
                  then
                     Match (Cycle_Unit_Suffix_Pattern, Rest_Of_Item, Matched);
                     if Matched (0) /= No_Match then
                        After := To_Unbounded_String
                          (Rest_Of_Item (Matched (1).First ..
                             Matched (1).Last));
                     end if;

                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (After),
                        Withed);
                     Append (Self.Last_Error, Dep);

                  elsif String_Utils.Looking_At
                    (Rest_Of_Item,
                     Rest_Of_Item'First,
                     "has a dependency on")
                  then
                     Match (Cycle_Unit_Suffix_Pattern, Rest_Of_Item, Matched);
                     if Matched (0) /= No_Match then
                        After := To_Unbounded_String
                          (Rest_Of_Item (Matched (1).First ..
                             Matched (1).Last));
                     end if;

                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (After),
                        F_Switch_Forced);
                     Append (Self.Last_Error, Dep);

                  elsif String_Utils.Looking_At
                    (Rest_Of_Item,
                     Rest_Of_Item'First,
                     "invokes a construct of")
                  then
                     Match (Cycle_Unit_Suffix_Pattern, Rest_Of_Item, Matched);
                     if Matched (0) /= No_Match then
                        After := To_Unbounded_String
                          (Rest_Of_Item (Matched (1).First ..
                             Matched (1).Last));
                     end if;

                     Dep := Create_Dependency
                       (To_String (Before),
                        To_String (After),
                        Invokes_Construct);
                     Append (Self.Last_Error, Dep);
                  end if;
               end;

            when Invocations =>
               --  Check whether it is a path string
               Match (Spaces_In_Path_Pattern, Item, Matched);
               if Matched (0) = No_Match then
                  Self.State := Expect_Error;
               end if;

               if Matched (1).Last < Self.Path_Lenght then
                  --  not a path string, an indent is too small
                  Self.State := Cycle;
                  Fallback   := True;
               end if;

               --  Parse path
               Match (Path_Pattern, Item, Matched);
               if Matched (0) /= No_Match then
                  Self.Path_Id := Integer'Value
                    (Item (Matched (1).First .. Matched (1).Last));
               else
                  Match (Unit_Line_Column, Item, Matched);
                  if Matched (0) /= No_Match then

                     ---  Fill message for showing in the location view
                     GPS.Kernel.Messages.Simple.Create_Simple_Message
                       (Container  => GPS.Kernel.Get_Messages_Container
                          (Self.Kernel),
                        Category   => Messages_Category,
                        File       => Create
                          (+Item (Matched (1).First .. Matched (1).Last)),
                        Line       => Natural'Value
                          (Item (Matched (2).First .. Matched (2).Last)),
                        Column     => Basic_Types.Visible_Column_Type'Value
                          (Item (Matched (3).First .. Matched (3).Last)),
                        Text       => "Path" & Integer'Image (Self.Path_Id) &
                          --  Add Item without "info:" prefix
                          ": " & Ada.Strings.Fixed.Trim
                          (Item (Item'First + 5 .. Item'Last),
                           Ada.Strings.Left),
                        Importance => GPS.Kernel.Messages.High,
                        Flags      => GPS.Kernel.Messages.Side_And_Locations);

                  end if;
               end if;
         end case;
      end loop;

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

end Browsers.Elaborations.Cycle_Parser_20;
