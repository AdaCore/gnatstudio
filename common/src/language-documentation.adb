-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2006                        --
--                              AdaCore                              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with String_Utils;          use String_Utils;

package body Language.Documentation is

   ------------------------------
   -- Get_Documentation_Before --
   ------------------------------

   procedure Get_Documentation_Before
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Debug         : Debug_Handle := null)
   is
      Current : Integer;
   begin
      Current := Decl_Index;
      Comment_Start := Current;
      Skip_To_Previous_Comment_Start (Context, Buffer, Comment_Start);

      if Comment_Start /= 0 then
         Comment_End := Comment_Start;
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);

         if Active (Debug) then
            Trace (Debug,
                   "Get_Documentation: Found a comment before the entity,"
                   & " from" & Comment_Start'Img & " to" & Comment_End'Img);
         end if;
      end if;
   end Get_Documentation_Before;

   -----------------------------
   -- Get_Documentation_After --
   -----------------------------

   procedure Get_Documentation_After
     (Context       : Language_Context;
      Buffer        : String;
      Decl_Index    : Natural;
      Comment_Start : out Natural;
      Comment_End   : out Natural;
      Debug         : Debug_Handle := null) is
   begin
      --  Else look after the comment after the declaration (which is the
      --  first block of comments after the declaration line, and not
      --  separated by a blank line)
      Comment_Start := Decl_Index;
      Skip_To_Next_Comment_Start (Context, Buffer, Comment_Start);

      if Comment_Start /= 0 then
         Comment_End := Comment_Start;
         Skip_To_Current_Comment_Block_End (Context, Buffer, Comment_End);
         Comment_End := Line_End (Buffer, Comment_End);

         if Active (Debug) then
            Trace (Debug,
                   "Get_Documentation: Found a comment after the entity,"
                   & " from" & Comment_Start'Img & " to" & Comment_End'Img);
         end if;
      end if;
   end Get_Documentation_After;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Language  : access Language_Root'Class;
      Buffer    : String;
      Tree      : Construct_Tree;
      Node      : Construct_Tree_Iterator) return String
   is
      Beginning, Current   : Natural;
      Result               : Unbounded_String;

      Type_Start, Type_End : Source_Location;
      Success              : Boolean;
   begin
      --  ??? There is again a lot of duplication of code here, but
      --  factorization is not obvious, since the data source / iterators are
      --  completely different. See if we can "convert" an entity into a
      --  construct or the other way around for the documentation purpose.
      --  An other solution could be to use a generic procedure, providing a
      --  standart interface implemented by both, the construct and the entity
      --  engine.

      Get_Documentation_Before
        (Context       => Get_Language_Context (Language).all,
         Buffer        => Buffer,
         Decl_Index    => Get_Construct (Node).Sloc_Start.Index,
         Comment_Start => Beginning,
         Comment_End   => Current);

      if Beginning = 0 then
         Get_Documentation_After
           (Context       => Get_Language_Context (Language).all,
            Buffer        => Buffer,
            Decl_Index    => Get_Construct (Node).Sloc_End.Index,
            Comment_Start => Beginning,
            Comment_End   => Current);
      end if;

      if Beginning /= 0 then
         Append
           (Result,
            Comment_Block
              (Language,
               Buffer (Beginning .. Current) & ASCII.LF,
               Comment => False,
               Clean   => True));
      end if;

      if Get_Construct (Node).Category in Subprogram_Category then

         declare
            Sub_Iter               : Construct_Tree_Iterator :=
                                       Next (Tree, Node, Jump_Into);
            Has_Parameter          : Boolean := False;
            Biggest_Parameter_Name : Integer := 0;
         begin
            while Get_Parent_Scope (Tree, Sub_Iter) = Node loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if Get_Construct (Sub_Iter).Name'Length >
                    Biggest_Parameter_Name
                  then
                     Biggest_Parameter_Name :=
                       Get_Construct (Sub_Iter).Name'Length;
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;

            Sub_Iter := Next (Tree, Node, Jump_Into);

            while Get_Parent_Scope (Tree, Sub_Iter) = Node loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if not Has_Parameter then
                     Append (Result, ASCII.LF & "<b>Parameters:</b>"
                             & ASCII.LF);
                     Has_Parameter := True;
                  end if;

                  Get_Referenced_Entity
                    (Language,
                     Buffer,
                     Get_Construct (Sub_Iter),
                     Type_Start,
                     Type_End,
                     Success);

                  Append
                    (Result,
                     Get_Construct (Sub_Iter).Name.all);

                  for J in Get_Construct (Sub_Iter).Name'Length + 1
                    .. Biggest_Parameter_Name
                  loop
                     Append (Result, " ");
                  end loop;

                  if Success then
                     Append
                       (Result,
                        " : " & Buffer (Type_Start.Index .. Type_End.Index)
                        & ASCII.LF);
                  else
                     Append (Result, " : ???" & ASCII.LF);
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;
         end;

         Get_Referenced_Entity
           (Language,
            Buffer,
            Get_Construct (Node),
            Type_Start,
            Type_End,
            Success);

         if Success then
            Append
              (Result,
               ASCII.LF & "<b>Return:</b>"
               & ASCII.LF & Buffer (Type_Start.Index .. Type_End.Index));
         end if;

      end if;

      return To_String (Result);
   end Get_Documentation;

end Language.Documentation;
