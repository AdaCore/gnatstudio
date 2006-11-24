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

end Language.Documentation;
