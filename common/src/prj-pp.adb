------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               P R J . P P                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Namet;    use Namet;
with Output;   use Output;
with Stringt;   use Stringt;

package body Prj.PP is

   Not_Tested : array (Project_Node_Kind) of Boolean := (others => True);

   Increment : Positive := 3;
   --  How many spaces for each indentation.
   --  Can be modified by calling Initialize.

   Current_Project : Project_Node_Id := Empty_Node;

   Last_Line_Is_Empty : Boolean := False;
   --  Used to avoid two consecutive empty lines.

   procedure Indicate_Tested (Kind : Project_Node_Kind);

   --  The following Default_* procedure are those
   --  that are used for output if Initialize is never called.

   procedure Default_Write_Empty_Line;
   --  Output an empty line, only if previous line is not empty.

   procedure Default_Write_Line (S : in String);
   --  Output a string, then an End of Line.

   procedure Default_Start_Line (Indent : Natural);
   --  Output an Indent number of spaces.

   procedure Default_Output_String (S : String_Id);
   --  Output the specified string, between double quotes,
   --  doubling internal double quotes.

   procedure Default_Output_Name
     (Name       : Name_Id;
      Capitalize : Boolean := True);
   --  Output the specified Name. Capitilize it if specified
   --  (as in standard Ada 95 identifier capitalization).

   procedure Print (Node : Project_Node_Id; Indent : Natural);
   --  A recursive procedure that traverses a project file tree
   --  and outputs its source.

   --  The following access to procedure values are used
   --  for the output.
   --  They can be modified by calling Initialize.

   Write_Empty_Line : Write_Empty_Line_Ap := Default_Write_Empty_Line'Access;
   Write_Char       : Write_Char_Ap       := Output.Write_Char'Access;
   Write_Str        : Write_Str_Ap        := Output.Write_Str'Access;
   Write_Line       : Write_Line_Ap       := Default_Write_Line'Access;
   Start_Line       : Start_Line_Ap       := Default_Start_Line'Access;
   Output_String    : Output_String_Ap    := Default_Output_String'Access;
   Output_Name      : Output_Name_Ap      := Default_Output_Name'Access;

   -------------------------
   -- Default_Output_Name --
   -------------------------

   procedure Default_Output_Name
     (Name       : Name_Id;
      Capitalize : Boolean := True)
   is
      Capital : Boolean := Capitalize;

   begin
      Get_Name_String (Name);

      for J in 1 .. Name_Len loop
         if Capital then
            Output.Write_Char (To_Upper (Name_Buffer (J)));
         else
            Output.Write_Char (Name_Buffer (J));
         end if;

         if Capitalize then
            Capital := Name_Buffer (J) = '_'
              or else Is_Digit (Name_Buffer (J));
         end if;
      end loop;
   end Default_Output_Name;

   ---------------------------
   -- Default_Output_String --
   ---------------------------

   procedure Default_Output_String (S : String_Id) is
   begin
      Write_Char ('"');
      String_To_Name_Buffer (S);

      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '"' then
            Output.Write_Str ("""");
         else
            Output.Write_Char (Name_Buffer (J));
         end if;
      end loop;

      Output.Write_Char ('"');
   end Default_Output_String;

   ------------------------
   -- Default_Start_Line --
   ------------------------

   procedure Default_Start_Line (Indent : Natural) is
   begin
      for J in 1 .. Indent loop
         Write_Char (' ');
      end loop;
   end Default_Start_Line;

   ------------------------------
   -- Default_Write_Empty_Line --
   ------------------------------

   procedure Default_Write_Empty_Line is
   begin
      if not Last_Line_Is_Empty then
         Output.Write_Eol;
         Last_Line_Is_Empty := True;
      end if;
   end Default_Write_Empty_Line;

   ------------------------
   -- Default_Write_Line --
   ------------------------

   procedure Default_Write_Line (S : in String) is
   begin
      Last_Line_Is_Empty := False;
      Output.Write_Line (S);
   end Default_Write_Line;

   ---------------------
   -- Indicate_Tested --
   ---------------------

   procedure Indicate_Tested (Kind : Project_Node_Kind) is
   begin
      Not_Tested (Kind) := False;
   end Indicate_Tested;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (W_Empty_Line : Write_Empty_Line_Ap;
      W_Char       : Write_Char_Ap;
      W_Str        : Write_Str_Ap;
      W_Line       : Write_Line_Ap;
      S_Line       : Start_Line_Ap;
      O_String     : Output_String_Ap;
      O_Name       : Output_Name_Ap;
      Inc          : Positive)
   is
   begin
      Write_Empty_Line := W_Empty_Line;
      Write_Char       := W_Char;
      Write_Str        := W_Str;
      Write_Line       := W_Line;
      Start_Line       := S_Line;
      Output_String    := O_String;
      Output_Name      := O_Name;
      Increment        := Inc;
   end Initialize;

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print (Project : Project_Node_Id) is
   begin
      Current_Project := Project;
      Print (Project, 0);
   end Pretty_Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node   : Project_Node_Id;
      Indent : Natural)
   is
   begin
      if Node /= Empty_Node then

         case Kind_Of (Node) is

            when N_Project  =>
               pragma Debug (Indicate_Tested (N_Project));
               if First_With_Clause_Of (Node) /= Empty_Node then

                  --  with clause(s)

                  Print (First_With_Clause_Of (Node), Indent);
                  Write_Empty_Line.all;
               end if;

               Start_Line (Indent);
               Write_Str ("project ");
               Output_Name (Name_Of (Node));

               --  Check if this project modifies another project

               if Modified_Project_Path_Of (Node) /= No_String then
                  Write_Str (" modifying ");
                  Output_String (Modified_Project_Path_Of (Node));
               end if;

               Write_Line (" is");
               Write_Empty_Line.all;

               --  Output all of the declarations in the project

               Print (Project_Declaration_Of (Node), Indent);
               Start_Line (Indent);
               Write_Str ("end ");
               Output_Name (Name_Of (Node));
               Write_Line (";");

            when N_With_Clause =>
               pragma Debug (Indicate_Tested (N_With_Clause));
               if Name_Of (Node) /= No_Name then
                  Start_Line (Indent);
                  Write_Str ("with ");
                  Output_String (String_Value_Of (Node));
                  Write_Line (";");
               end if;

               Print (Next_With_Clause_Of (Node), Indent);

            when N_Project_Declaration =>
               pragma Debug (Indicate_Tested (N_Project_Declaration));
               if First_Declarative_Item_Of (Node) /= Empty_Node then
                  Print (First_Declarative_Item_Of (Node), Indent + Increment);
                  Write_Empty_Line.all;
               end if;

            when N_Declarative_Item =>
               pragma Debug (Indicate_Tested (N_Declarative_Item));
               Print (Current_Item_Node (Node), Indent);
               Print (Next_Declarative_Item (Node), Indent);

            when N_Package_Declaration =>
               pragma Debug (Indicate_Tested (N_Package_Declaration));
               Write_Empty_Line.all;
               Start_Line (Indent);
               Write_Str ("package ");
               Output_Name (Name_Of (Node));

               if Project_Of_Renamed_Package_Of (Node) /= Empty_Node then
                  Write_Str (" renames ");
                  Output_Name
                    (Name_Of (Project_Of_Renamed_Package_Of (Node)));
                  Write_Char ('.');
                  Output_Name (Name_Of (Node));
                  Write_Line (";");

               else
                  Write_Line (" is");
                  Write_Empty_Line.all;

                  if First_Declarative_Item_Of (Node) /= Empty_Node then
                     Print
                       (First_Declarative_Item_Of (Node), Indent + Increment);
                     Write_Empty_Line.all;
                  end if;

                  Start_Line (Indent);
                  Write_Str ("end ");
                  Output_Name (Name_Of (Node));
                  Write_Line (";");
                  Write_Empty_Line.all;
               end if;

            when N_String_Type_Declaration =>
               pragma Debug (Indicate_Tested (N_String_Type_Declaration));
               Start_Line (Indent);
               Write_Str ("type ");
               Output_Name (Name_Of (Node));
               Write_Line (" is");
               Start_Line (Indent + Increment);
               Write_Char ('(');

               declare
                  String_Node : Project_Node_Id := First_Literal_String (Node);

               begin
                  while String_Node /= Empty_Node loop
                     Output_String (String_Value_Of (String_Node));
                     String_Node := Next_Literal_String (String_Node);
                     if String_Node /= Empty_Node then
                        Write_Str (", ");
                     end if;
                  end loop;
               end;

               Write_Line (");");

            when N_Literal_String =>
               pragma Debug (Indicate_Tested (N_Literal_String));
               Output_String (String_Value_Of (Node));

            when N_Attribute_Declaration =>
               pragma Debug (Indicate_Tested (N_Attribute_Declaration));
               Start_Line (Indent);
               Write_Str ("for ");
               Output_Name (Name_Of (Node));

               if Associative_Array_Index_Of (Node) /= No_String then
                  Write_Str (" (");
                  Output_String (Associative_Array_Index_Of (Node));
                  Write_Char (')');
               end if;

               Write_Str (" use ");
               Print (Expression_Of (Node), Indent);
               Write_Line (";");

            when N_Typed_Variable_Declaration =>
               pragma Debug (Indicate_Tested (N_Typed_Variable_Declaration));
               Start_Line (Indent);
               Output_Name (Name_Of (Node));
               Write_Str (" : ");
               Output_Name (Name_Of (String_Type_Of (Node)));
               Write_Str (" := ");
               Print (Expression_Of (Node), Indent);
               Write_Line (";");

            when N_Variable_Declaration =>
               pragma Debug (Indicate_Tested (N_Variable_Declaration));
               Start_Line (Indent);
               Output_Name (Name_Of (Node));
               Write_Str (" := ");
               Print (Expression_Of (Node), Indent);
               Write_Line (";");

            when N_Expression =>
               pragma Debug (Indicate_Tested (N_Expression));
               declare
                  Term : Project_Node_Id := First_Term (Node);

               begin
                  while Term /= Empty_Node loop
                     Print (Term, Indent);
                     Term := Next_Term (Term);

                     if Term /= Empty_Node then
                        Write_Str (" & ");
                     end if;
                  end loop;
               end;

            when N_Term =>
               pragma Debug (Indicate_Tested (N_Term));
               Print (Current_Term (Node), Indent);

            when N_Literal_String_List =>
               pragma Debug (Indicate_Tested (N_Literal_String_List));
               Write_Char ('(');

               declare
                  Expression : Project_Node_Id :=
                    First_Expression_In_List (Node);

               begin
                  while Expression /= Empty_Node loop
                     Print (Expression, Indent);
                     Expression := Next_Expression_In_List (Expression);

                     if Expression /= Empty_Node then
                        Write_Str (", ");
                     end if;

                  end loop;
               end;

               Write_Char (')');

            when N_Variable_Reference =>
               pragma Debug (Indicate_Tested (N_Variable_Reference));
               if Project_Node_Of (Node) /= Empty_Node then
                  Output_Name (Name_Of (Project_Node_Of (Node)));
                  Write_Char ('.');
               end if;

               if Package_Node_Of (Node) /= Empty_Node then
                  Output_Name (Name_Of (Package_Node_Of (Node)));
                  Write_Char ('.');
               end if;

               Output_Name (Name_Of (Node));

            when N_External_Value =>
               pragma Debug (Indicate_Tested (N_External_Value));
               Write_Str ("external (");
               Print (External_Reference_Of (Node), Indent);

               if External_Default_Of (Node) /= Empty_Node then
                  Write_Str (", ");
                  Print (External_Default_Of (Node), Indent);
               end if;

               Write_Char (')');

            when N_Attribute_Reference =>
               pragma Debug (Indicate_Tested (N_Attribute_Reference));
               if Project_Node_Of (Node) /= Empty_Node
                 and then Project_Node_Of (Node) /= Current_Project
               then
                  Output_Name (Name_Of (Project_Node_Of (Node)));

                  if Package_Node_Of (Node) /= Empty_Node then
                     Write_Char ('.');
                     Output_Name (Name_Of (Package_Node_Of (Node)));
                  end if;

               elsif Package_Node_Of (Node) /= Empty_Node then
                  Output_Name (Name_Of (Package_Node_Of (Node)));

               else
                  Write_Str ("project");
               end if;

               Write_Char (''');
               Output_Name (Name_Of (Node));

            when N_Case_Construction =>
               pragma Debug (Indicate_Tested (N_Case_Construction));
               Write_Empty_Line.all;
               Start_Line (Indent);
               Write_Str ("case ");
               Print (Case_Variable_Reference_Of (Node), Indent);
               Write_Line (" is");

               declare
                  Case_Item : Project_Node_Id := First_Case_Item_Of (Node);

               begin
                  while Case_Item /= Empty_Node loop
                     Print (Case_Item, Indent + Increment);
                     Case_Item := Next_Case_Item (Case_Item);
                  end loop;
               end;

               Write_Empty_Line.all;
               Start_Line (Indent);
               Write_Line ("end case;");
               Write_Empty_Line.all;

            when N_Case_Item =>
               pragma Debug (Indicate_Tested (N_Case_Item));
               Write_Empty_Line.all;
               Start_Line (Indent);
               Write_Str ("when ");

               if First_Choice_Of (Node) = Empty_Node then
                  Write_Str ("others");

               else
                  declare
                     Label : Project_Node_Id := First_Choice_Of (Node);

                  begin
                     while Label /= Empty_Node loop
                        Print (Label, Indent);
                        Label := Next_Literal_String (Label);

                        if Label /= Empty_Node then
                           Write_Str (" | ");
                        end if;
                     end loop;
                  end;
               end if;

               Write_Line (" =>");
               Print (First_Declarative_Item_Of (Node), Indent + Increment);
         end case;
      end if;

   end Print;

   procedure Output_Statistics is
   begin
      Write_Line ("Project_Node_Kinds not tested:");
      for Kind in Project_Node_Kind loop
         if Not_Tested (Kind) then
            Write_Str ("   ");
            Write_Line (Project_Node_Kind'Image (Kind));
         end if;
      end loop;
      Write_Empty_Line.all;
   end Output_Statistics;

end Prj.PP;
