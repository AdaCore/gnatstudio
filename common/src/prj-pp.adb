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

   Increment : Positive := 3;

   Current_Project : Project_Node_Id := Empty_Node;

   Last_Line_Is_Empty : Boolean := False;

   procedure Default_Write_Eol;

   procedure Default_Write_Line (S : in String);

   procedure Default_Start_Line (Indent : Natural);

   procedure Default_Output_String (S : String_Id);

   procedure Default_Output_Name (Name       : Name_Id;
                                  Capitalize : Boolean := True);

   procedure Print (Node   : Project_Node_Id;
                    Indent : Natural);

   -------------------------
   -- Default_Output_Name --
   -------------------------

   procedure Default_Output_Name (Name       : Name_Id;
                                  Capitalize : Boolean := True) is
      Capital : Boolean := Capitalize;
   begin
      Get_Name_String (Name);
      for I in 1 .. Name_Len loop
         if Capital then
            Output.Write_Char (To_Upper (Name_Buffer (I)));
         else
            Output.Write_Char (Name_Buffer (I));
         end if;
         if Capitalize then
            Capital := Name_Buffer (I) = '_'
              or else Is_Digit (Name_Buffer (I));
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
      for I in 1 .. Name_Len loop
         if Name_Buffer (I) = '"' then
            Output.Write_Str ("""");
         else
            Output.Write_Char (Name_Buffer (I));
         end if;
      end loop;
      Output.Write_Char ('"');
   end Default_Output_String;

   ------------------------
   -- Default_Start_Line --
   ------------------------

   procedure Default_Start_Line (Indent : Natural) is
   begin
      for I in 1 .. Indent loop
         Write_Char (' ');
      end loop;
   end Default_Start_Line;

   -----------------------
   -- Default_Write_Eol --
   -----------------------

   procedure Default_Write_Eol is
   begin
      if not Last_Line_Is_Empty then
         Output.Write_Eol;
         Last_Line_Is_Empty := True;
      end if;
   end Default_Write_Eol;

   ------------------------
   -- Default_Write_Line --
   ------------------------

   procedure Default_Write_Line (S : in String) is
   begin
      Last_Line_Is_Empty := False;
      Output.Write_Line (S);
   end Default_Write_Line;

   Write_Eol     : Write_Eol_Ap     := Default_Write_Eol'Access;
   Write_Char    : Write_Char_Ap    := Output.Write_Char'Access;
   Write_Str     : Write_Str_Ap     := Output.Write_Str'Access;
   Write_Line    : Write_Line_Ap    := Default_Write_Line'Access;
   Start_Line    : Start_Line_Ap    := Default_Start_Line'Access;
   Output_String : Output_String_Ap := Default_Output_String'Access;
   Output_Name   : Output_Name_Ap   := Default_Output_Name'Access;

   procedure Initialize (W_Eol     : Write_Eol_Ap;
                         W_Char    : Write_Char_Ap;
                         W_Str     : Write_Str_Ap;
                         W_Line    : Write_Line_Ap;
                         S_Line    : Start_Line_Ap;
                         O_String  : Output_String_Ap;
                         O_Name    : Output_Name_Ap;
                         Inc       : Positive) is
   begin
      Write_Eol        := W_Eol;
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

   procedure Print (Node   : Project_Node_Id;
                    Indent : Natural) is
      Data : Project_Node_Record;
   begin
      if Node /= Empty_Node then
         Data := Project_Nodes.Table (Node);

         case Data.Kind is

            when N_Project  =>
               if Data.Field1 /= Empty_Node then
                  --  with clause
                  Print (Data.Field1, Indent);
                  Write_Eol.all;
               end if;
               Start_Line (Indent);
               Write_Str ("project ");
               Output_Name (Data.Name);
               if Data.Value /= No_String then
                  Write_Str (" modifying ");
                  Output_String (Data.Value);
               end if;
               Write_Line (" is");
               Write_Eol.all;
               Print (Data.Field2, Indent);
               Start_Line (Indent);
               Write_Str ("end ");
               Output_Name (Data.Name);
               Write_Line (";");
            when N_With_Clause =>
               if Data.Path_Name /= No_Name then
                  Start_Line (Indent);
                  Write_Str ("with ");
                  Output_String (Data.Value);
                  Write_Line (";");
               end if;
               Print (Data.Field2, Indent);
            when N_Project_Declaration =>
               if Data.Field1 /= Empty_Node then
                  Print (Data.Field1, Indent + Increment);
                  Write_Eol.all;
               end if;
            when N_Declarative_Item =>
               Print (Data.Field1, Indent);
               Print (Data.Field2, Indent);
            when N_Package_Declaration =>
               Write_Eol.all;
               Start_Line (Indent);
               Write_Str ("package ");
               Output_Name (Data.Name);
               if Data.Field1 /= Empty_Node then
                  Write_Str (" renames ");
                  Output_Name (Project_Nodes.Table (Data.Field1).Name);
                  Write_Char ('.');
                  Output_Name (Data.Name);
                  Write_Line (";");
               else
                  Write_Line (" is");
                  if Data.Field2 /= Empty_Node then
                     Print (Data.Field2, Indent + Increment);
                     Write_Eol.all;
                  end if;
                  Start_Line (Indent);
                  Write_Str ("end ");
                  Output_Name (Data.Name);
                  Write_Line (";");
               end if;
            when N_String_Type_Declaration =>
               Start_Line (Indent);
               Write_Str ("type ");
               Output_Name (Data.Name);
               Write_Line (" is");
               Start_Line (Indent + Increment);
               Write_Char ('(');
               declare
                  String_Node : Project_Node_Id := Data.Field1;
               begin
                  while String_Node /= Empty_Node loop
                     Output_String (Project_Nodes.Table (String_Node).Value);
                     String_Node := Project_Nodes.Table (String_Node).Field1;
                     if String_Node /= Empty_Node then
                        Write_Str (", ");
                     end if;
                  end loop;
               end;
               Write_Line (");");
            when N_Literal_String =>
               Output_String (Data.Value);
            when N_Attribute_Declaration =>
               Start_Line (Indent);
               Write_Str ("for ");
               Output_Name (Data.Name);
               if Data.Value /= No_String then
                  Write_Str (" (");
                  Output_String (Data.Value);
                  Write_Char (')');
               end if;
               Write_Str (" use ");
               Print (Data.Field1, Indent);
               Write_Line (";");
            when N_Typed_Variable_Declaration =>
               Start_Line (Indent);
               Output_Name (Data.Name);
               Write_Str (" : ");
               Output_Name (Project_Nodes.Table (Data.Field2).Name);
               Write_Str (" := ");
               Print (Data.Field1, Indent);
               Write_Line (";");
            when N_Variable_Declaration =>
               Start_Line (Indent);
               Output_Name (Data.Name);
               Write_Str (" := ");
               Print (Data.Field1, Indent);
               Write_Line (";");
            when N_Expression =>
               declare
                  Term : Project_Node_Id := Data.Field1;
               begin
                  while Term /= Empty_Node loop
                     Print (Term, Indent);
                     Term := Project_Nodes.Table (Term).Field2;
                     if Term /= Empty_Node then
                        Write_Str (" & ");
                     end if;
                  end loop;
               end;
            when N_Term =>
               Print (Data.Field1, Indent);
            when N_Literal_String_List =>
               Write_Char ('(');
               declare
                  Expression : Project_Node_Id := Data.Field1;
               begin
                  while Expression /= Empty_Node loop
                     Print (Expression, Indent);
                     Expression := Project_Nodes.Table (Expression).Field2;
                     if Expression /= Empty_Node then
                        Write_Str (", ");
                     end if;
                  end loop;
               end;
               Write_Char (')');
            when N_Variable_Reference =>
               if Data.Field1 /= Empty_Node then
                  Output_Name (Project_Nodes.Table (Data.Field1).Name);
                  Write_Char ('.');
               end if;
               if Data.Field2 /= Empty_Node then
                  Output_Name (Project_Nodes.Table (Data.Field2).Name);
                  Write_Char ('.');
               end if;
               Output_Name (Data.Name);
            when N_External_Value =>
               Write_Str ("external (");
               Print (Data.Field1, Indent);
               if Data.Field2 /= Empty_Node then
                  Write_Str (", ");
                  Print (Data.Field2, Indent);
               end if;
               Write_Char (')');
            when N_Attribute_Reference =>
               if Data.Field2 /= Empty_Node
                 and then
                 Data.Field2 /= Current_Project
               then
                  Output_Name (Project_Nodes.Table (Data.Field2).Name);
                  if Data.Field1 /= Empty_Node then
                     Write_Char ('.');
                     Output_Name (Project_Nodes.Table (Data.Field1).Name);
                  end if;
               elsif Data.Field1 /= Empty_Node then
                  Output_Name (Project_Nodes.Table (Data.Field1).Name);
               else
                  Write_Str ("project");
               end if;
               Write_Char (''');
               Output_Name (Data.Name);
            when N_Case_Construction =>
               Write_Eol.all;
               Start_Line (Indent);
               Write_Str ("case ");
               Print (Data.Field1, Indent);
               Write_Line (" is");
               declare
                  Case_Item : Project_Node_Id := Data.Field2;
               begin
                  while Case_Item /= Empty_Node loop
                     Print (Case_Item, Indent + Increment);
                     Case_Item := Project_Nodes.Table (Case_Item).Field3;
                  end loop;
               end;
               Write_Eol.all;
               Start_Line (Indent);
               Write_Line ("end case;");
            when N_Case_Item =>
               Write_Eol.all;
               Start_Line (Indent);
               Write_Str ("when ");
               if Data.Field1 = Empty_Node then
                  Write_Str ("others");
               else
                  declare
                     Label : Project_Node_Id := Data.Field1;
                  begin
                     while Label /= Empty_Node loop
                        Print (Label, Indent);
                        Label := Project_Nodes.Table (Label).Field1;
                        if Label /= Empty_Node then
                           Write_Str (" | ");
                        end if;
                     end loop;
                  end;
               end if;
               Write_Line (" =>");
               Print (Data.Field2, Indent + Increment);
         end case;
      end if;

   end Print;

end Prj.PP;
