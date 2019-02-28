------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 1999-2019, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;

separate (Templates_Parser)

procedure Print_Tree (T : Tree; Level : Natural := 0) is

   use type Expr.Tree;

   procedure Print_Indent (L : Natural);
   --  Output proper number of spaces for identation

   ------------------
   -- Print_Indent --
   ------------------

   procedure Print_Indent (L : Natural) is
      use Ada.Strings.Fixed;
   begin
      Text_IO.Put ((L * 2) * ' ');
   end Print_Indent;

begin
   if T = null then
      return;
   end if;

   Print_Indent (Level);

   case T.Kind is
      when Info =>
         Text_IO.Put_Line ("[INFO] " & To_String (T.Filename));
         declare
            I : Tree := T.I_File;
         begin
            while I /= null loop
               Text_IO.Put (" -> ");
               if I.File = Null_Static_Tree then
                  Data.Print_Tree (I.I_Filename);
               else
                  Text_IO.Put_Line (To_String (I.File.Info.Filename));
               end if;
               I := I.Next;
            end loop;
         end;

         Print_Tree (T.Next, Level);

      when C_Info =>
         Text_IO.Put_Line ("[C_INFO] "
                           & Natural'Image (T.Used)
                           & ' ' & Boolean'Image (T.Obsolete));

         Print_Tree (T.Next, Level);

      when Text =>
         Text_IO.Put ("[TEXT] ");
         Data.Print_Tree (T.Text);
         Print_Tree (T.Next, Level);

      when Set_Stmt =>
         Text_IO.Put ("[SET] ");
         Definitions.Print_Tree (T.Def);
         Text_IO.New_Line;
         Print_Tree (T.Next, Level);

      when If_Stmt  =>
         Text_IO.Put ("[IF] ");
         Expr.Print_Tree (T.Cond);
         Text_IO.New_Line;
         Print_Tree (T.N_True, Level + 1);
         if T.N_False /= null then
            Print_Indent (Level);
            Text_IO.Put_Line ("[ELSE]");
            Print_Tree (T.N_False, Level + 1);
         end if;
         Print_Indent (Level);
         Text_IO.Put_Line ("[END_IF]");
         Print_Tree (T.Next, Level);

      when Table_Stmt =>
         Text_IO.Put ("[TABLE]");

         if T.Terminate_Sections then
            Text_IO.Put (" TERMINATE_SECTIONS");
         end if;

         if T.Reverse_Index then
            Text_IO.Put (" REVERSE");
         end if;

         if T.Terse then
            Text_IO.Put (" TERSE");
         end if;

         Text_IO.New_Line;
         Print_Tree (T.Blocks, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[END_TABLE]");
         Print_Tree (T.Next, Level);

      when Section_Block =>
         Text_IO.Put_Line ("[BLOCK]");

         if T.Common /= null then
            Print_Indent (Level + 1);
            Text_IO.Put_Line ("[COMMON]");
            Print_Tree (T.Common, Level + 2);
         end if;

         if T.Sections /= null then
            Print_Tree (T.Sections, Level + 1);
         end if;

         Print_Indent (Level);
         Text_IO.Put_Line ("[END_BLOCK]");
         Print_Tree (T.Next, Level);

      when Section_Stmt =>
         Text_IO.Put_Line ("[SECTION]");
         Print_Tree (T.Next, Level + 1);
         Print_Tree (T.N_Section, Level);

      when Include_Stmt =>
         Text_IO.Put ("[INCLUDE] ");

         if T.File = Null_Static_Tree then
            Data.Print_Tree (T.I_Filename);
         else
            Text_IO.Put_Line (To_String (T.File.Info.Filename));
         end if;

         declare
            use type Data.Tree;
         begin
            for K in T.I_Params'Range loop
               if T.I_Params (K) /= null then
                  Print_Indent (Level + 2);
                  Text_IO.Put ("$" & Utils.Image (K) & " = ");
                  Data.Print_Tree (T.I_Params (K));
               end if;
            end loop;
         end;

         Print_Tree (T.File.Info, Level + 1);
         Print_Tree (T.Next, Level);

      when Inline_Stmt =>
         Text_IO.Put_Line ("[INLINE] (" & To_String (T.Sep) & ')');
         Print_Tree (T.I_Block, Level + 1);
         Text_IO.Put_Line ("[END_INLINE]");
         Print_Tree (T.Next, Level);
   end case;

end Print_Tree;
