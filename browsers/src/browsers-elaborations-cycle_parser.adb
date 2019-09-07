------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with GNAT.Regpat;                      use GNAT.Regpat;

package body Browsers.Elaborations.Cycle_Parser is

   --  Binder circularities messages:
   --
   --  "error: elaboration circularity detected" Then dependencies, each like
   --  "info:   $ must be elaborated before $"  Then one of reasons:
   --   "info:      reason: with clause"
   --   "info:      reason: pragma Elaborate in unit $"
   --   "info:      reason: pragma Elaborate_All in unit $"
   --   "info:      reason: implicit Elaborate_All in unit $"
   --  +"info:      recompile $ with -gnatwl for full details"
   --   "info:      reason: implicit Elaborate in unit $"
   --  +"info:      recompile $ with -gnatwl for full details"
   --   "info:      reason: spec always elaborated before body"
   --
   --  when reason is pragma Elaborate_All or  implicit Elaborate_All
   --  then links each in form:
   --  "info:         $"   unit name, then one of link reasonn
   --  "info:            must be elaborated along with its spec:"
   --  "info:            which must be elaborated along with its spec:"
   --  "info:            is withed by:"
   --  "info:            which is withed by:"
   --  "info:         $"
   --
   --  Dependency optionally ended with
   --  "info:   $ must therefore be elaborated before $",
   --  +"info:      (because $ has a pragma Elaborate_Body)"

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self  : access Output_Parser_Fabric;
      Child : Tools_Output_Parser_Access)
      return Tools_Output_Parser_Access
   is
      pragma Unreferenced (Self);
      Empty : Elaboration_Cycles.Cycle;
   begin
      Set_Elaboration_Cycle (Empty);

      return new Circularity_Parser'
        (Child       => Child,
         Last_Cycle  => <>,
         Has_Cycle   => False,
         Last_Dep    => <>,
         Last_Link   => <>,
         Last_Before => <>,
         Last_After  => <>,
         State       => New_Cycle);
   end Create;

   Info : constant String := "info:  ";
   --  Common binder prefix

   Unit : constant String := """([^""]+)""";
   --  Placeholder for unit name (unit kind) in quotes. Example: "aaa (body)"

   --  Compiled matchers:

   Must_Be_Before : constant Pattern_Matcher :=
     Compile ("^" & Info & "  " & Unit
              & " must be elaborated before " & Unit);

   Must_Therefore_Be_Before : constant Pattern_Matcher :=
     Compile ("^" & Info & "  " & Unit
              & " must therefore be elaborated before "
              & Unit);

   Info_Unit : constant Pattern_Matcher :=
     Compile ("^" & Info & "        " & Unit);

   ---------------------------
   -- Parse_Standard_Output --
   ---------------------------

   overriding procedure Parse_Standard_Output
     (Self    : not null access Circularity_Parser;
      Item    : String;
      Command : access Root_Command'Class)
   is
      use Elaboration_Cycles;

      function Start_With (Text : String) return Boolean;

      ----------------
      -- Start_With --
      ----------------

      function Start_With (Text : String) return Boolean is
      begin
         return Item'Length >= Text'Length
           and then Item (Item'First .. Item'First + Text'Length - 1) = Text;
      end Start_With;

      Fallback : Boolean := True;
      Matched  : Match_Array (0 .. 3);
   begin
      --  Skip empty lines
      if Item = "" or Item = (1 => ASCII.LF) then
         Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
         return;
      end if;

      while Fallback loop
         Fallback := False;

         case Self.State is
         when New_Cycle =>
            if Self.Has_Cycle then
               declare
                  Empty : Elaboration_Cycles.Cycle;
               begin
                  Set_Elaboration_Cycle (Self.Last_Cycle);
                  Self.Has_Cycle := False;
                  Self.Last_Cycle := Empty;
               end;
            end if;

            if Start_With ("error: elaboration circularity detected") then
               Self.State := New_Dependency;
            end if;

         when New_Dependency =>
            Match (Must_Be_Before, Item, Matched);

            if Matched (0) = No_Match then
               Self.State := New_Cycle;
               Fallback := True;
            else
               Self.State := New_Reason;
               Self.Last_Before := To_Unbounded_String
                 (Item (Matched (1).First .. Matched (1).Last));
               Self.Last_After := To_Unbounded_String
                 (Item (Matched (2).First .. Matched (2).Last));
            end if;

         when New_Reason  =>
            if Start_With (Info & "     reason: with clause") then
               Self.Last_Dep := Create_Dependency
                 (To_String (Self.Last_Before),
                  To_String (Self.Last_After),
                  Withed);
               Self.State := Skip_Therefore;
            elsif Start_With
              (Info & "     reason: pragma Elaborate in unit ")
            then
               Self.Last_Dep := Create_Dependency
                 (To_String (Self.Last_Before),
                  To_String (Self.Last_After),
                  Pragma_Elaborate);
               Self.State := Skip_Therefore;
            elsif Start_With
              (Info & "     reason: pragma Elaborate_All in unit ")
            then
               Self.Last_Dep := Create_Dependency
                 (To_String (Self.Last_Before),
                  To_String (Self.Last_After),
                  Pragma_Elaborate_All);
               Self.State := New_Link;
            elsif Start_With
              (Info & "     reason: implicit Elaborate_All in unit ")
            then
               Self.Last_Dep := Create_Dependency
                 (To_String (Self.Last_Before),
                  To_String (Self.Last_After),
                  Elaborate_All_Desirable);
               Self.State  := Skip_Recompile;
            elsif Start_With
              (Info & "     reason: implicit Elaborate in unit ")
            then
               Self.Last_Dep := Create_Dependency
                 (To_String (Self.Last_Before),
                  To_String (Self.Last_After),
                  Elaborate_Desirable);
               Self.State := Skip_Recompile;
            elsif Start_With
              (Info & "     reason: spec always elaborated before body")
            then
               Self.Last_Dep := Create_Dependency
                 (To_String (Self.Last_Before),
                  To_String (Self.Last_After),
                  Specification_First);
               Self.State := Skip_Therefore;
            else
               Self.State := New_Cycle;
            end if;

         when Skip_Recompile =>
            if not Start_With (Info & "     recompile ") then
               Self.State := New_Cycle;
            elsif Reason (Self.Last_Dep) = Elaborate_All_Desirable then
               Self.State := New_Link;
            else
               Self.State := Skip_Therefore;
            end if;

         when New_Link =>
            Match (Info_Unit, Item, Matched);

            if Matched (0) = No_Match then
               Self.State := New_Cycle;
            else
               Self.State := Link_Reason;
               Self.Last_Link := To_Unbounded_String
                 (Item (Matched (1).First .. Matched (1).Last));
            end if;

         when Link_Reason =>
            if Start_With (Info & "           must be elaborated"
                           & " along with its spec:")
              or else Start_With (Info & "           which must be elaborated"
                                  & " along with its spec:")
            then
               Self.State := New_Link;
               Append (Self.Last_Dep,
                       Create_Link
                         (To_String (Self.Last_Link),
                          Body_With_Specification));
            elsif Start_With (Info & "           is withed by:")
              or else Start_With (Info & "           which is withed by:")
            then
               Self.State := New_Link;
               Append (Self.Last_Dep,
                       Create_Link
                         (To_String (Self.Last_Link),
                          Withed));
            else
               Self.State := Skip_Therefore;
               Fallback := True;
            end if;

         when Skip_Therefore =>
            Match (Must_Therefore_Be_Before, Item, Matched);

            if Matched (0) = No_Match then
               Fallback := True;
               Self.State := New_Dependency;
               Append (Self.Last_Cycle, Self.Last_Dep);
            else
               Self.State := Skip_Because;
               Set_Elaborate_Body (Self.Last_Dep);
               Append (Self.Last_Cycle, Self.Last_Dep);
            end if;

            Self.Has_Cycle := True;

         when Skip_Because =>
            if Start_With (Info & "     (because """) then
               Self.State := New_Dependency;
            else
               Self.State := New_Cycle;
            end if;
         end case;
      end loop;

      Tools_Output_Parser (Self.all).Parse_Standard_Output (Item, Command);
   end Parse_Standard_Output;

end Browsers.Elaborations.Cycle_Parser;
