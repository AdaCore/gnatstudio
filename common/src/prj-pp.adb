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

with Prj.Tree;  use Prj.Tree;
with Namet;     use Namet;
with Output;    use Output;
with Stringt;   use Stringt;

package body Prj.PP is

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print
     (Project : Project_Node_Id;
      Increment : Positive := 3;
      W_Char : Write_Char_Ap := null;
      W_Str  : Write_Str_Ap := null)
   is
      procedure Print (Node : Project_Node_Id; Indent : Natural);
      --  A recursive procedure that traverses a project file tree
      --  and outputs its source.
      --  Current_Prj is the project that we are printing. This
      --  is used when printing attributes, since in nested packages they need
      --  to use a fully qualified name.

      procedure Output_Name (Name : Name_Id);
      --  Outputs a name

      procedure Start_Line (Indent : Natural);
      --  Outputs the indentation at the beginning of the line.

      procedure Output_String (S : String_Id);
      --  Outputs a string using the default output procedures

      procedure Write_Empty_Line;
      --  Outputs an empty line, only if the previous line was not
      --  empty already.

      procedure Write_Line (S : in String);
      --  Outputs S followed by a new line

      Write_Char : Write_Char_Ap := Output.Write_Char'Access;
      Write_Str  : Write_Str_Ap  := Output.Write_Str'Access;
      --  These two access to procedure values are used for the output.

      Last_Line_Is_Empty : Boolean := False;
      --  Used to avoid two consecutive empty lines.

      -------------------
      -- Output_String --
      -------------------

      procedure Output_String (S : String_Id) is
      begin
         Write_Char ('"');
         String_To_Name_Buffer (S);

         for J in 1 .. Name_Len loop
            if Name_Buffer (J) = '"' then
               Write_Str ("""");
            else
               Write_Char (Name_Buffer (J));
            end if;
         end loop;

         Write_Char ('"');
      end Output_String;

      ----------------------
      -- Write_Empty_Line --
      ----------------------

      procedure Write_Empty_Line is
      begin
         if not Last_Line_Is_Empty then
            Write_Char (ASCII.LF);
            Last_Line_Is_Empty := True;
         end if;
      end Write_Empty_Line;

      ----------------
      -- Write_Line --
      ----------------

      procedure Write_Line (S : in String) is
      begin
         Last_Line_Is_Empty := False;
         Write_Str (S);
         Write_Char (ASCII.LF);
      end Write_Line;

      -----------------
      -- Output_Name --
      -----------------

      procedure Output_Name (Name : Name_Id) is
      begin
         Write_Str (Get_Name_String (Name));
      end Output_Name;

      ----------------
      -- Start_Line --
      ----------------

      procedure Start_Line (Indent : Natural) is
      begin
         Write_Str ((1 .. Indent => ' '));
      end Start_Line;

      -----------
      -- Print --
      -----------

      procedure Print (Node : Project_Node_Id; Indent : Natural) is
      begin
         if Node /= Empty_Node then

            case Kind_Of (Node) is

               when N_Project  =>
                  if First_With_Clause_Of (Node) /= Empty_Node then

                     --  with clause(s)

                     Print (First_With_Clause_Of (Node), Indent);
                     Write_Empty_Line;
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
                  Write_Empty_Line;

                  --  Output all of the declarations in the project

                  Print (Project_Declaration_Of (Node), Indent);
                  Start_Line (Indent);
                  Write_Str ("end ");
                  Output_Name (Name_Of (Node));
                  Write_Line (";");

               when N_With_Clause =>
                  if Name_Of (Node) /= No_Name then
                     Start_Line (Indent);
                     Write_Str ("with ");
                     Output_String (String_Value_Of (Node));
                     Write_Line (";");
                  end if;

                  if Next_With_Clause_Of (Node) /= Empty_Node then
                     Print (Next_With_Clause_Of (Node), Indent);
                  end if;

               when N_Project_Declaration =>
                  if First_Declarative_Item_Of (Node) /= Empty_Node then
                     Print
                       (First_Declarative_Item_Of (Node), Indent + Increment);
                     Write_Empty_Line;
                  end if;

               when N_Declarative_Item =>
                  Print (Current_Item_Node (Node), Indent);
                  if Next_Declarative_Item (Node) /= Empty_Node then
                     Print (Next_Declarative_Item (Node), Indent);
                  end if;

               when N_Package_Declaration =>
                  Write_Empty_Line;
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
                     --  Write_Empty_Line.all;

                     if First_Declarative_Item_Of (Node) /= Empty_Node then
                        Print (First_Declarative_Item_Of (Node),
                               Indent + Increment);
                        --  Write_Empty_Line.all;
                     end if;

                     Start_Line (Indent);
                     Write_Str ("end ");
                     Output_Name (Name_Of (Node));
                     Write_Line (";");
                     --  Write_Empty_Line.all;
                  end if;

               when N_String_Type_Declaration =>
                  Start_Line (Indent);
                  Write_Str ("type ");
                  Output_Name (Name_Of (Node));
                  Write_Line (" is");
                  Start_Line (Indent + Increment);
                  Write_Char ('(');

                  declare
                     String_Node : Project_Node_Id :=
                       First_Literal_String (Node);
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
                  Output_String (String_Value_Of (Node));

               when N_Attribute_Declaration =>
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
                  Start_Line (Indent);
                  Output_Name (Name_Of (Node));
                  Write_Str (" : ");
                  Output_Name (Name_Of (String_Type_Of (Node)));
                  Write_Str (" := ");
                  Print (Expression_Of (Node), Indent);
                  Write_Line (";");

               when N_Variable_Declaration =>
                  Start_Line (Indent);
                  Output_Name (Name_Of (Node));
                  Write_Str (" := ");
                  Print (Expression_Of (Node), Indent);
                  Write_Line (";");

               when N_Expression =>
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
                  Print (Current_Term (Node), Indent);

               when N_Literal_String_List =>
                  Write_Char ('(');

                  declare
                     Expression : Project_Node_Id :=
                       First_Expression_In_List (Node);
                  begin
                     while Expression /= Empty_Node loop
                        Print (Expression, Indent);
                        Expression := Next_Expression_In_List (Expression);

                        if Expression /= Empty_Node then
                           Write_Line (", ");
                           Start_Line (Indent);
                        end if;

                     end loop;
                  end;

                  Write_Char (')');

               when N_Variable_Reference =>
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
                  Write_Str ("external (");
                  Print (External_Reference_Of (Node), Indent);

                  if External_Default_Of (Node) /= Empty_Node then
                     Write_Str (", ");
                     Print (External_Default_Of (Node), Indent);
                  end if;

                  Write_Char (')');

               when N_Attribute_Reference =>
                  if Project_Node_Of (Node) /= Empty_Node
                    and then Project_Node_Of (Node) /= Project
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
                  --  Write_Empty_Line.all;
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

                  --  Write_Empty_Line.all;
                  Start_Line (Indent);
                  Write_Line ("end case;");
                  --  Write_Empty_Line.all;

               when N_Case_Item =>
                  --  Write_Empty_Line.all;
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

   begin
      if W_Char = null then
         Write_Char := Output.Write_Char'Access;
      else
         Write_Char := W_Char;
      end if;

      if W_Str = null then
         Write_Str := Output.Write_Str'Access;
      else
         Write_Str := W_Str;
      end if;

      Print (Project, 0);

      if W_Char = null or else W_Str = null then
         Output.Write_Eol;
      end if;
   end Pretty_Print;

end Prj.PP;
